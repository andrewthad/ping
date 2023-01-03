{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language EmptyCase #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}

module Network.Icmp.Ping.Multihosts
  ( multihosts
  , multirange
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadWaitReadSTM,threadWaitWrite)
import Control.Concurrent.STM.TVar (readTVar,registerDelay)
import Control.Exception (onException,mask)
import Control.Monad.Trans.Except (ExceptT(..),runExceptT)
import Data.Functor (($>))
import Data.Primitive (PrimArray,MutableByteArray,MutablePrimArray)
import Data.Word (Word64,Word8,Word16,Word32)
import Foreign.C.Error (Errno(..),eAGAIN,eWOULDBLOCK,eACCES)
import Foreign.C.Types (CSize(..))
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Exts (RealWorld)
import Net.Types (IPv4(..),IPv4Range)
import Network.Icmp.Common (IcmpException(..))
import Network.Icmp.Marshal (peekIcmpHeaderPayload)
import Network.Icmp.Marshal (peekIcmpHeaderSequenceNumber)
import Network.Icmp.Marshal (sizeOfIcmpHeader,pokeIcmpHeader)
import Network.Icmp.Ping.Debug (debug)
import Posix.Socket (SocketAddressInternet(..))
import System.Endian (toBE32)
import System.Posix.Types (Fd(..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Control.Monad.STM as STM
import qualified Data.Map.Unboxed.Unlifted as MUN
import qualified Data.Primitive as PM
import qualified Data.Set.Unboxed as SU
import qualified Linux.Socket as SCK
import qualified Net.IPv4 as IPv4
import qualified Posix.Socket as SCK

-- TODO: The repeated reallocation for the SockAddr seems
-- a little wasteful. Two possible options are:
--
-- * Use a mutable buffer instead (posix-api doesn't currently support this)
-- * Cache the sockaddrs on a per-host basis.
--
-- I lean toward the first option since it would also
-- reduce allocations in Network.Icmp.Ping.Hosts.

-- Why plus 4? We have 4 extra bytes for the IPv4 address.
fullPacketSize :: Int
fullPacketSize = sizeOfIcmpHeader + 4

-- Wait up to a specified maximum number of microseconds
-- for a socket to have data ready to be read. Returns
-- True if there is something on the buffer to be read
-- and False if nothing showed up in time.
waitForRead ::
     Bool -- Should we attempt to read?
  -> Int -- Maximum number of microseconds to wait.
  -> Fd -- Socket
  -> IO Bool
waitForRead !shouldRead !maxWaitTime !sock = if maxWaitTime > 0 && shouldRead
  then do
    debug ("About to wait for " ++ show maxWaitTime ++ " microseconds")
    (isReadyAction,deregister) <- threadWaitReadSTM sock
    delay <- registerDelay maxWaitTime
    isContentReady <- STM.atomically $
      (isReadyAction $> True)
      <|>
      (do isDone <- readTVar delay 
          STM.check isDone
          pure False
      )
    deregister
    pure isContentReady
  else pure False

-- | Ping a group of hosts simultaneously. Performs a configurable
--   number of pings for each host and reports the elapsed nanoseconds
--   for each response. If the array of durations is smaller than the
--   total number of pings, it indicates that some ICMP requests for
--   that host were lost or corrupted.
--
--   The function also accepts an cutoff for unresponsive hosts. If
--   a host does not respond to the initial number of pings equal to
--   the cutoff, this function does not attempt further pings to the
--   host. Consider the case in which this function performs 20 pings
--   per host with a 5e6 microsecond timeout. Without the unresponsive
--   cutoff, a single nonresponsive host would cause this function to
--   always run for 100 seconds. However, with the cutoff set to 3,
--   this function would stop trying pinging the host after there
--   was no response to any of the first 3 pings. However if there
--   were a response to any of the first 3 pings, then all 20 pings
--   would continue to be sent. This does not necessarily guarantee
--   that this function would run for less than 100 seconds. A host
--   might respond to the initial ping and then go offline. Or a host
--   might take just under 5 seconds to respond to each ping. However,
--   both of these situations are uncommon. What is much more common
--   is that someone includes a bad IP address in the list of hosts,
--   and a low cutoff can considerably reduce the amount of time wasted
--   on such pings. To prevent the cutoff behavior, set it to the number
--   of pings per host. 
multihosts ::
     Int -- ^ Microseconds to wait for response
  -> Int -- ^ Microsecond delay between pings to same host 
  -> Int -- ^ Number of pings per host 
  -> Int -- ^ Nonresponsive cutoff
  -> SU.Set IPv4 -- ^ Hosts
  -> IO (Either IcmpException (MUN.Map IPv4 (PrimArray Word64)))
-- Implementation notes: We have a prim array of durations. Each of these
-- has enough space to hold all the timestamps for each ping. Additionally,
-- they have 4 extra slots at the end: attempted pings, successful pings,
-- last send timestamp, and state (pending send/recv). These are removed
-- at the end by resizeMutablePrimArray, but they are the per-host state
-- over the course of the loop's execution.
multihosts !pause !successPause' !totalPings !cutoff !theHosts
  | pause <= 0 || totalPings <= 0 || cutoff <= 0 || SU.null theHosts = pure (Right mempty)
  | otherwise = let !successPause = max successPause' 0 in mask $ \restore -> SCK.uninterruptibleSocket SCK.Internet SCK.datagram SCK.icmp >>= \case
      Left (Errno e) -> pure (Left (IcmpExceptionSocket e))
      Right sock -> do
        !now0 <- getMonotonicTimeNSec
        !buffer <- PM.newByteArray fullPacketSize
        !durations <- restore
          ( do let nanoPause = intToWord64 pause * 1000
               let nanoSuccessPause = intToWord64 successPause * 1000
               eworking <- runExceptT $ MUN.fromSetP
                 (\theHost -> ExceptT $ do
                   m <- PM.newPrimArray (totalPings + 4)
                   PM.setPrimArray m 0 (totalPings + 4) (0 :: Word64)
                   debug ("Sending initial to " ++ show theHost) 
                   -- We intentionally discard the time that performSend tells
                   -- us to wait until since we can easily calculate this number.
                   performSend 0 now0 nanoPause sock totalPings theHost buffer m >>= \case
                     Left err -> pure (Left err)
                     Right _ -> pure (Right m)
                 ) theHosts
               case eworking of
                 Left err -> pure (Left err)
                 Right working -> do
                   let go :: Word64 -> Word64 -> IO (Either IcmpException ())
                       go !currentPause !nextTime = do
                         -- Since currentPause is represent an an unsigned type,
                         -- it jumps up to near the max bound when we end up
                         -- exceeding the amount of time we are supposed to take.
                         -- The identifier shouldRead becomes False when this happens,
                         -- and we move on to the next round of updates.
                         let shouldRead = currentPause <= nanoPause
                         let microPause = div currentPause 1000
                         waitForRead shouldRead (word64ToInt microPause) sock >>= \case
                           True -> do
                             debug "Receiving in poll loop"
                             r <- SCK.uninterruptibleReceiveFromMutableByteArray_ sock buffer 0 (intToCSize fullPacketSize) SCK.dontWait
                             case r of
                               Left (Errno e) -> pure (Left (IcmpExceptionReceive e))
                               Right receivedBytes -> if receivedBytes == intToCSize fullPacketSize
                                 then do
                                   payload' <- peekIcmpHeaderPayload buffer
                                   end <- getMonotonicTimeNSec
                                   case MUN.lookup (IPv4 payload') working of
                                     Nothing -> go (end - nextTime) nextTime
                                     Just durations -> do
                                       sequenceNumber' <- peekIcmpHeaderSequenceNumber buffer
                                       sequenceNumber <- PM.readPrimArray durations (totalPings + 0)
                                       if word16ToWord64 sequenceNumber' == sequenceNumber
                                         then do
                                           sentTime <- PM.readPrimArray durations (totalPings + 2)
                                           successes <- PM.readPrimArray durations (totalPings + 1)
                                           PM.writePrimArray durations (word64ToInt successes) (end - sentTime)
                                           PM.writePrimArray durations (totalPings + 1) (successes + 1)
                                           PM.writePrimArray durations (totalPings + 2) end
                                           PM.writePrimArray durations (totalPings + 3) pendingSend
                                           let possibleNextTime = end + nanoSuccessPause
                                           if possibleNextTime < nextTime
                                             then go nanoSuccessPause possibleNextTime
                                             else go (nextTime - end) nextTime
                                         else go (nextTime - end) nextTime
                                 else do
                                   end <- getMonotonicTimeNSec
                                   go (nextTime - end) nextTime
                           False -> do
                             debug "Updating in poll loop"
                             currentTime <- getMonotonicTimeNSec
                             r <- runExceptT $ MUN.foldlMapWithKeyM'
                               (step sock nanoPause nanoSuccessPause totalPings cutoff buffer currentTime)
                               working
                             case r of
                               Left e -> pure (Left e)
                               Right (Time futureTime) -> if futureTime == maxBound
                                 then pure (Right ())
                                 else do
                                   debug ("Waiting for " ++ show (futureTime - currentTime) ++ " nanoseconds before spanning for expirations")
                                   go (futureTime - currentTime) futureTime
                   now1 <- getMonotonicTimeNSec
                   go nanoPause (now1 + nanoPause) >>= \case
                     Left e -> pure (Left e)
                     Right _ -> fmap Right
                       ( MUN.mapMaybeP
                         (\durations -> do
                           successes <- PM.readPrimArray durations (totalPings + 1)
                           if successes == 0
                             then pure Nothing
                             else fmap Just (PM.resizeMutablePrimArray durations (word64ToInt successes) >>= PM.unsafeFreezePrimArray)
                         ) working
                       )
          )
          `onException`
          (SCK.uninterruptibleClose sock)
        SCK.uninterruptibleClose sock >>= \case
          Left (Errno e) -> pure (Left (IcmpExceptionClose e))
          Right _ -> pure durations

newtype Time = Time Word64

instance Semigroup Time where
  Time a <> Time b = Time (min a b)

instance Monoid Time where
  mempty = Time maxBound

step ::
     Fd -- socket 
  -> Word64 -- Nanoseconds to wait for response
  -> Word64 -- Nanosecond delay between pings to same host
  -> Int -- Number of pings per host
  -> Int -- Nonresponsive cutoff
  -> MutableByteArray RealWorld -- buffer
  -> Word64 -- current time
  -> IPv4 -- destination address
  -> MutablePrimArray RealWorld Word64 -- durations and metadata
  -> ExceptT IcmpException IO Time
step !sock !pause !successPause !totalPings !cutoff !buffer !now !theHost !durations = ExceptT $ do
  attemptedPings <- PM.readPrimArray durations (totalPings + 0)
  if word64ToInt attemptedPings < totalPings
    then do
      successPings <- PM.readPrimArray durations (totalPings + 1)
      debug ("Detected " ++ show attemptedPings ++ " attempted pings and " ++ show successPings ++ " successes")
      if word64ToInt attemptedPings >= cutoff && successPings == 0
        then pure (Right mempty)
        else do
          -- The time metadata may refer to either the last time a packet
          -- was sent or the last time a packet was received. We
          -- can figure out which one by using theState.
          theState <- PM.readPrimArray durations (totalPings + 3)
          if theState == pendingReceive
            then do
              sendTime <- PM.readPrimArray durations (totalPings + 2)
              if sendTime + pause < now
                then performSend attemptedPings now pause sock totalPings theHost buffer durations
                else pure (Right (Time (sendTime + pause)))
            else do
              receiveTime <- PM.readPrimArray durations (totalPings + 2)
              if receiveTime + successPause < now
                then performSend attemptedPings now pause sock totalPings theHost buffer durations
                else pure (Right (Time (receiveTime + successPause)))
    else pure (Right mempty)

performSend :: Word64 -> Word64 -> Word64 -> Fd -> Int -> IPv4 -> MutableByteArray RealWorld -> MutablePrimArray RealWorld Word64 -> IO (Either IcmpException Time)
performSend attemptedPings now pause sock totalPings theHost buffer durations = do
  PM.writePrimArray durations (totalPings + 2) now
  PM.writePrimArray durations (totalPings + 0) (attemptedPings + 1)
  PM.setByteArray buffer 0 sizeOfIcmpHeader (0 :: Word8)
  pokeIcmpHeader buffer (word64ToWord16 (attemptedPings + 1)) (getIPv4 theHost)
  let sockaddr = SCK.encodeSocketAddressInternet
        (SocketAddressInternet { port = 0, address = toBE32 (getIPv4 theHost) })
  mwriteError <- writeWhenReady
    (SCK.uninterruptibleSendToMutableByteArray sock buffer 0 (intToCSize fullPacketSize) SCK.dontWait sockaddr)
    (threadWaitWrite sock)
  case mwriteError of
    Left (Errno e)
        -- When you try to send a packet to a broadcast address, the kernel
        -- gives you an EACCES failure. Including a broadcast address in a
      | Errno e == eACCES -> do
          PM.writePrimArray durations (totalPings + 0) (intToWord64 totalPings)
          PM.writePrimArray durations (totalPings + 3) pendingSend
          pure (Right mempty)
      | otherwise -> pure (Left (IcmpExceptionSend e))
    Right sentBytes -> if sentBytes == intToCSize fullPacketSize
      then do
        PM.writePrimArray durations (totalPings + 3) pendingReceive
        pure (Right (Time (now + pause)))
      else pure (Left (IcmpExceptionSendBytes sentBytes))

pendingReceive :: Word64
pendingReceive = 0

pendingSend :: Word64
pendingSend = 1

word64ToWord16 :: Word64 -> Word16
word64ToWord16 = fromIntegral

word16ToWord64 :: Word16 -> Word64
word16ToWord64 = fromIntegral

intToWord64 :: Int -> Word64
intToWord64 = fromIntegral

word64ToInt :: Word64 -> Int
word64ToInt = fromIntegral

intToCSize :: Int -> CSize
intToCSize = fromIntegral

-- This is heavily adapted from throwErrnoIfRetryMayBlock in Foreign.C.Error.
-- It only attempts the write twice. If it does not work after the wait
-- function (always threadWaitWrite) returns, it reports an error. Also,
-- this one does not recover from EINTR. I am not sure when that is present
-- in throwErrnoIfRetryMayBlock, but I suspect it is for Windows. This
-- code is not expected to run on Windows.
writeWhenReady
  :: IO (Either Errno CSize) -- the 'IO' operation to be executed
  -> IO () -- action to execute before retrying if immediate retry would block
  -> IO (Either Errno CSize)
writeWhenReady f wait = f >>= \case
  Left e1 -> if e1 == eWOULDBLOCK || e1 == eAGAIN
    then wait *> f 
    else pure (Left e1)
  Right i -> pure (Right i)

-- | Send multiple pings to each host in a range of hosts simultaneously.
multirange ::
     Int -- ^ Microseconds to wait for response
  -> Int -- ^ Microsecond delay between pings to same host 
  -> Int -- ^ Number of pings per host 
  -> Int -- ^ Nonresponsive cutoff
  -> IPv4Range -- ^ Range
  -> IO (Either IcmpException (MUN.Map IPv4 (PrimArray Word64))) -- ^ Elapsed nanoseconds for responsive hosts
multirange !pause !successPause !totalPings !cutoff !r =
  multihosts pause successPause totalPings cutoff $ coerceIPv4Set
    (SU.enumFromTo
      (getIPv4 (IPv4.lowerInclusive r))
      (getIPv4 (IPv4.upperInclusive r))
    )

-- The existence of this function is a little disappointing. I suspect that
-- there is a better way to do this (probably by writing version of
-- Data.Set.Unboxed.enumFromTo that works without a Num constraint),
-- but I am choosing the easiest path for now.
--
-- TODO: There is a better way. I need to rewrite
-- Data.Primitive.Contiguous.fromList to be compatible with
-- list fusion. Then Data.Set.Unboxed.enumFromTo can use
-- that, and everything should work out alright. Well, we
-- still must perform an extra check to ensure that the
-- enum instance is compatible with the Ord instance,
-- but that's not too bad.
coerceIPv4Set :: SU.Set Word32 -> SU.Set IPv4
coerceIPv4Set = unsafeCoerce
