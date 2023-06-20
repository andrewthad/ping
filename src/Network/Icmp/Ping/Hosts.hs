{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language EmptyCase #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}

module DISABLED Network.Icmp.Ping.Hosts
  ( hosts
  , range
  ) where

-- TODO: The functions in this module currently do not finish
-- promptly when they have the opportunity to. If all hosts
-- respond, it would be better to go ahead and finish rather
-- than waiting around. The use of adjustManyInline makes this
-- a little trickier than it would otherwise be.

import Control.Applicative ((<|>))
import Control.Concurrent (threadWaitReadSTM,threadWaitWriteSTM)
import Control.Concurrent.STM.TVar (readTVar,registerDelay)
import Control.Exception (onException,mask)
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.), (.|.), testBit)
import Data.Functor (($>))
import Data.Primitive (PrimArray,MutableByteArray)
import Data.Word (Word64,Word8,Word16,Word32)
import Foreign.C.Error (Errno(..),eACCES)
import Foreign.C.Types (CSize(..))
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Exts (RealWorld)
import Net.Types (IPv4(..),IPv4Range)
import Network.Icmp.Common (IcmpException(..))
import Network.Icmp.Marshal (peekIcmpHeaderPayload,peekIcmpHeaderType)
import Network.Icmp.Marshal (peekIcmpHeaderSequenceNumber)
import Network.Icmp.Marshal (sizeOfIcmpHeader,pokeIcmpHeader)
import Network.Icmp.Ping.Debug (debug)
import Posix.Socket (SocketAddressInternet(..))
import System.Endian (toBE32)
import System.Posix.Types (Fd(..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Control.Monad.STM as STM
import qualified Data.Map.Unboxed.Unboxed as MUU
import qualified Data.Primitive as PM
import qualified Data.Set.Unboxed as SU
import qualified Linux.Socket as SCK
import qualified Net.IPv4 as IPv4
import qualified Posix.Socket as SCK

fullPacketSize :: Int
fullPacketSize = sizeOfIcmpHeader + 4

-- Wait up to a specified maximum number of microseconds
-- for a socket to have data ready to be read. Returns
-- True if there is something on the buffer to be read
-- and False if nothing showed up in time.
waitForRead ::
     Int -- Maximum number of microseconds to wait.
  -> Fd -- Socket
  -> IO Bool
waitForRead !maxWaitTime !sock = do
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

-- Wait for read or write to be available on the socket.
-- Returns True if read became available and False if
-- write became available.
waitForReadWrite :: Fd -> IO Bool
waitForReadWrite sock = do
  (isReadyRead,deregisterRead) <- threadWaitReadSTM sock
  (isReadyWrite,deregisterWrite) <- threadWaitWriteSTM sock
  r <- STM.atomically ((isReadyRead $> True) <|> (isReadyWrite $> False))
  deregisterRead
  deregisterWrite
  pure r

-- | Ping a range of hosts simultaneously.
range ::
     Int -- ^ Microseconds to wait for response
  -> IPv4Range -- ^ Range
  -> IO (Either IcmpException (MUU.Map IPv4 Word64)) -- ^ Elapsed nanoseconds for responding hosts
range !pause !r = hosts pause $ coerceIPv4Set
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

-- | Ping a set of hosts simultaneously. Performs one ping
--   for each host and reports the elapsed nanoseconds for the
--   response. If a key is missing from the resulting map, it
--   indicates that a response was not received from that host.
hosts ::
     Int -- ^ Microseconds to wait for response
  -> SU.Set IPv4 -- ^ Hosts
  -> IO (Either IcmpException (MUU.Map IPv4 Word64)) -- ^ Elapsed nanoseconds for responding hosts
hosts !pause !theHosts = do
  mask $ \restore -> SCK.uninterruptibleSocket SCK.Internet SCK.datagram SCK.icmp >>= \case
    Left (Errno e) -> pure (Left (IcmpExceptionSocket e))
    Right sock -> do
      durations <- restore
        ( do let hostsArr = SU.toArray theHosts
             !buffer <- PM.newByteArray fullPacketSize
             (m,r) <- MUU.adjustManyInline
               (\adjust -> hostsStepA buffer sock pause hostsArr (PM.sizeofPrimArray hostsArr) adjust
               ) (MUU.fromSet (const initialStatus) theHosts)
             pure $ case r of
               Left pair -> Left pair
               Right _ -> Right
                 ( MUU.mapMaybe
                   (\w -> case testBit w 47 of
                     True -> Just (extractTimestamp w)
                     False -> Nothing
                   ) m
                 )
        )
        `onException`
        (SCK.uninterruptibleClose sock)
      SCK.uninterruptibleClose sock >>= \case
        Left (Errno e) -> pure (Left (IcmpExceptionClose e))
        Right _ -> pure durations

hostsStepA :: MutableByteArray RealWorld -> Fd -> Int -> PrimArray IPv4 -> Int -> (IPv4 -> (Word64 -> IO Word64) -> IO ()) -> IO (Either IcmpException ())
hostsStepA !buffer !sock !pause !hostsArr !hostsLen adjust = go 0 where
  go !ix = if ix < hostsLen
    then do
      debug "waiting for read-write"
      waitForReadWrite sock >>= \case
        True -> do
          debug "ready for read"
          r <- SCK.uninterruptibleReceiveFromMutableByteArray_ sock buffer 0 (intToCSize fullPacketSize) SCK.dontWait
          case r of
            Left (Errno e) -> pure (Left (IcmpExceptionReceive e))
            Right receivedBytes -> if receivedBytes == intToCSize fullPacketSize
              then do
                payload' <- peekIcmpHeaderPayload buffer
                adjust (IPv4 payload') $ \w -> case extractStatus w of
                  0b01 -> do
                    sequenceNumber' <- peekIcmpHeaderSequenceNumber buffer
                    if sequenceNumber' == extractSequenceNumber w
                      then do
                        end <- getMonotonicTimeNSec
                        pure (completeStatus ((end .&. 0x3FFFFFFFFFFF) - extractTimestamp w))
                      else pure w
                  _ ->
                    -- In this case, we did not send an icmp echo request
                    -- but we received a response (00). Or we've already received
                    -- a response (1x). Pretty weird, and it
                    -- suggests foul play, but we simply ignore the response.
                    pure w
                go ix
              else do
                -- If the repsonse is malformed, we leave the result in
                -- the map alone. It will be purged at the end.
                go ix
        False -> do
          debug "ready for write"
          let host = PM.indexPrimArray hostsArr ix
          PM.setByteArray buffer 0 sizeOfIcmpHeader (0 :: Word8)
          pokeIcmpHeader buffer (intToWord16 ix) (getIPv4 host)
          let sockaddr = SCK.encodeSocketAddressInternet
                (SocketAddressInternet { port = 0, address = toBE32 (getIPv4 host) })
          mwriteError <- SCK.uninterruptibleSendToMutableByteArray sock buffer 0 (intToCSize fullPacketSize) SCK.dontWait sockaddr
          case mwriteError of
            Left (Errno e)
                -- When you try to send a packet to a broadcast address, the kernel
                -- gives you an EACCES failure. Including a broadcast address in a
                -- range is actually somewhat common though. For example, if we try
                -- to ping everything on our local network 192.168.1.0/24, we are
                -- going to send an ICMP echo request to 192.168.1.0, which the kernel
                -- does not like. This is fine though. We just ignore these failures
                -- rather than having them abort the entire function.
              | Errno e == eACCES -> go (ix + 1)
              | otherwise -> pure (Left (IcmpExceptionSend e))
            Right sentBytes -> if sentBytes == intToCSize fullPacketSize
              then do
                start <- getMonotonicTimeNSec
                adjust host (\_ -> pure (pendingStatus (intToWord16 ix) start))
                go (ix + 1)
              else do
                -- could not send out the full packet, should not happen
                pure (Left (IcmpExceptionSendBytes sentBytes))
    else hostsStepB buffer sock pause adjust =<< getMonotonicTimeNSec

-- We start calling this once we run out of hosts to send to. At this point,
-- all responses need to come in soon. We get the current time and count down
-- from this, requiring any outstanding replies to show up within that time
-- frame.
hostsStepB :: MutableByteArray RealWorld -> Fd -> Int -> (IPv4 -> (Word64 -> IO Word64) -> IO ()) -> Word64 -> IO (Either IcmpException ())
hostsStepB !buffer !sock !pause !adjust !initialTime = go initialTime where
  go !currentTime = do
    debug "Step B iteration"
    let remainingMicroseconds = pause - word64ToInt (div (currentTime - initialTime) 1000)
    if remainingMicroseconds > 0
      then do
        isReady <- waitForRead remainingMicroseconds sock
        if isReady
          then do
            r <- SCK.uninterruptibleReceiveFromMutableByteArray_ sock buffer 0 (intToCSize fullPacketSize) SCK.dontWait
            case r of
              Left (Errno e) -> pure (Left (IcmpExceptionReceive e))
              Right receivedBytes -> if receivedBytes == intToCSize fullPacketSize
                then do
                  payload' <- peekIcmpHeaderPayload buffer
                  end <- getMonotonicTimeNSec
                  peekIcmpHeaderType buffer >>= \case
                    0 -> do
                      adjust (IPv4 payload') $ \w -> case extractStatus w of
                        0b01 -> do
                          sequenceNumber' <- peekIcmpHeaderSequenceNumber buffer
                          if sequenceNumber' == extractSequenceNumber w
                            then pure (completeStatus ((end .&. 0x3FFFFFFFFFFF) - extractTimestamp w))
                            else pure w
                        _ -> pure w
                      go end
                    _ -> go end
                else go =<< getMonotonicTimeNSec -- response was wrong size
          else pure (Right ())
    else pure (Right ())

-- We use the lower 46 bits for the timestamp. We use the upper 16
-- for the sequence number. Bits 46 and 47 are set to 1x when we have have
-- received a response successfully. It is 01 when we are awaiting a response. It
-- is 00 when nothing has been sent.
pendingStatus :: Word16 -> Word64 -> Word64
pendingStatus seqNum timestamp =
  0x400000000000 .|. (timestamp .&. 0x3FFFFFFFFFFF) .|. (unsafeShiftL (word16ToWord64 seqNum) 48)

completeStatus :: Word64 -> Word64
completeStatus timestamp = 0xC00000000000 .|. timestamp

initialStatus :: Word64
initialStatus = 0

extractStatus :: Word64 -> Word64
extractStatus w =  
  unsafeShiftR (0xC00000000000 .&. w) 46

extractSequenceNumber :: Word64 -> Word16
extractSequenceNumber w = word64ToWord16 (unsafeShiftR w 48)

extractTimestamp :: Word64 -> Word64
extractTimestamp w = (w .&. 0x3FFFFFFFFFFF)
    
word64ToWord16 :: Word64 -> Word16
word64ToWord16 = fromIntegral

word16ToWord64 :: Word16 -> Word64
word16ToWord64 = fromIntegral

intToWord16 :: Int -> Word16
intToWord16 = fromIntegral

word64ToInt :: Word64 -> Int
word64ToInt = fromIntegral

intToCSize :: Int -> CSize
intToCSize = fromIntegral

