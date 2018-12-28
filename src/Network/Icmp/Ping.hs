{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}
{-# language EmptyCase #-}

module Network.Icmp.Ping
  ( ping
  , hosts
  , range
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadWaitWrite,threadWaitReadSTM,threadWaitWriteSTM)
import Control.Concurrent.STM.TVar (readTVar,registerDelay)
import Control.Exception (onException,mask)
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.), (.|.), testBit)
import Data.Functor (($>))
import Data.Primitive (PrimArray,MutableByteArray)
import Data.Word (Word64,Word8,Word16,Word32)
import Foreign.C.Error (Errno(..),errnoToIOError,eAGAIN,eWOULDBLOCK)
import Foreign.C.Types (CSize(..))
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Exts (RealWorld,inline)
import GHC.IO (IO(..))
import Net.Types (IPv4(..),IPv4Range)
import Network.Icmp.Marshal (peekIcmpHeaderPayload)
import Network.Icmp.Marshal (peekIcmpHeaderSequenceNumber)
import Network.Icmp.Marshal (sizeOfIcmpHeader,pokeIcmpHeader)
import Posix.Socket (SocketAddressInternet(..))
import System.Endian (toBE32)
import System.Posix.Types (Fd(..))
import Unsafe.Coerce (unsafeCoerce)

import qualified Control.Monad.STM as STM
import qualified Data.Map.Unboxed.Unboxed as MUU
import qualified Data.Primitive as PM
import qualified Data.Set.Unboxed as SU
import qualified Linux.Socket as SCK
import qualified Posix.Socket as SCK
import qualified Net.IPv4 as IPv4

debug :: String -> IO ()
debug _ = pure ()

fullPacketSize :: Int
fullPacketSize = sizeOfIcmpHeader + 4

-- | Ping an IPv4 address. Blocks until a response is received.
ping ::
     Int -- ^ Microseconds to wait for response
  -> IPv4 -- ^ Host
  -> IO (Maybe Word64)
ping !maxWaitTime (IPv4 !w) = do
  r <- mask $ \restore -> SCK.socket SCK.internet SCK.datagram SCK.icmp >>= \case
    Left e -> pure (Left ("socket",e))
    Right sock -> do
      elapsed <- restore
        ( do let sockaddr = SCK.encodeSocketAddressInternet
                   (SocketAddressInternet { port = 0, address = toBE32 w })
             buffer <- PM.newByteArray fullPacketSize
             -- We only zero out the header, not the payload.
             PM.setByteArray buffer 0 sizeOfIcmpHeader (0 :: Word8)
             pokeIcmpHeader buffer 0 w 
             start <- getMonotonicTimeNSec
             mwriteError <- writeWhenReady
               (SCK.unsafeSendToMutableByteArray sock buffer 0 (intToCSize fullPacketSize) SCK.dontWait sockaddr)
               (threadWaitWrite sock)
             case mwriteError of
               Left e -> pure (Left ("send",e))
               Right sentBytes -> do
                 if sentBytes == intToCSize fullPacketSize
                   then do
                     isReady <- waitForRead maxWaitTime sock
                     if isReady
                       then do
                         r <- SCK.unsafeReceiveFromMutableByteArray_ sock buffer 0 (intToCSize fullPacketSize) SCK.dontWait
                         case r of
                           Left e -> pure (Left ("recv",e))
                           Right receivedBytes -> if receivedBytes == intToCSize fullPacketSize
                             then do
                               sequenceNumber' <- peekIcmpHeaderSequenceNumber buffer
                               payload' <- peekIcmpHeaderPayload buffer
                               if sequenceNumber' == 0 && payload' == w
                                 then do
                                   end <- getMonotonicTimeNSec
                                   let !delta = end - start
                                   pure (Right (Just delta))
                                 else pure (Right Nothing) -- response was for a different request
                             else pure (Right Nothing) -- response was the wrong size
                       else pure (Right Nothing) -- did not receive a reply in time 
                   else pure (Left ("send.bytes",(Errno (-1)))) -- could not send out the full packet, should not happen
        )
        `onException`
        -- In the exceptional case, we throw away any errors returned
        -- by unsafeClose (not that we expect to see any). We do this
        -- because there is no other sensible behavior. We would much
        -- rather preserve the original exception.
        (SCK.unsafeClose sock)
      -- It is not neccessary to use closeFdWith here since the socket
      -- cannot possibly be seen by more than one thread.
      SCK.unsafeClose sock >>= \case
        Left e -> pure (Left ("close",e))
        Right _ -> pure elapsed
  case r of
    -- If a socket cannot be opened, there is not a sensible way for the
    -- caller to recover. Conequently, we throw an error rather than
    -- returning an Either. The user will need to rerun the program
    -- with CAP_NET_RAW or as root or after adjusting
    -- net.ipv4.ping_group_range with sysctl.
    Left (action,e) -> throwPingError action e
    Right v -> pure v

-- Since GHC throws out the original error code when printing an IOException,
-- we stick the error code on the location description.
throwPingError :: String -> Errno -> IO a
throwPingError action e@(Errno i) = ioError (errnoToIOError (pingName ++ "." ++ action ++ "(" ++ show i ++ ")") e Nothing (Just moduleName)) 

throwMultipingError :: String -> Errno -> IO a
throwMultipingError action e@(Errno i) = ioError (errnoToIOError (multipingName ++ "." ++ action ++ "(" ++ show i ++ ")") e Nothing (Just moduleName)) 

-- throwMultipingsError :: String -> Errno -> IO a
-- throwMultipingsError action e@(Errno i) = ioError (errnoToIOError (multipingsName ++ "." ++ action ++ "(" ++ show i ++ ")") e Nothing (Just moduleName)) 

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
  -> IO (MUU.Map IPv4 Word64)
range !pause !r = hosts pause $ coerceIPv4Set
  (SU.enumFromTo
    (getIPv4 (IPv4.lowerInclusive r))
    (getIPv4 (IPv4.upperInclusive r))
  )

-- The existence of this function is a little disappointing. I suspect that
-- there is a better way to do this (probably by writing version of
-- Data.Set.Unboxed.enumFromTo that works without a Num constraint),
-- but I am choosing the easiest path for now.
coerceIPv4Set :: SU.Set Word32 -> SU.Set IPv4
coerceIPv4Set = unsafeCoerce

-- | Ping a set of hosts simultaneously. Performs one ping
--   for each host and reports the elapsed nanoseconds for the
--   response. If a key is missing from the resulting map, it
--   indicates that a response was not received from that host.
hosts ::
     Int -- ^ Microseconds to wait for response
  -> SU.Set IPv4 -- ^ Hosts
  -> IO (MUU.Map IPv4 Word64)
hosts !pause !theHosts = do
  r <- mask $ \restore -> SCK.socket SCK.internet SCK.datagram SCK.icmp >>= \case
    Left e -> pure (Left ("socket",e))
    Right sock -> do
      durations <- restore
        ( do let hostsArr = SU.toArray theHosts
             !buffer <- PM.newByteArray fullPacketSize
             (m,r) <- 
               ( MUU.adjustManyInline
                 (\adjust -> multipingStepA buffer sock pause hostsArr (PM.sizeofPrimArray hostsArr) (inline adjust)
                 ) (MUU.fromSet (const initialStatus) theHosts)
               )
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
        (SCK.unsafeClose sock)
      SCK.unsafeClose sock >>= \case
        Left e -> pure (Left ("close",e))
        Right _ -> pure durations
  case r of
    Left (action,e) -> throwMultipingError action e
    Right v -> pure v

multipingStepA :: MutableByteArray RealWorld -> Fd -> Int -> PrimArray IPv4 -> Int -> (IPv4 -> (Word64 -> IO Word64) -> IO ()) -> IO (Either (String,Errno) ())
multipingStepA !buffer !sock !pause !hostsArr !hostsLen adjust = go 0 where
  go !ix = if ix < hostsLen
    then do
      debug "waiting for read-write"
      waitForReadWrite sock >>= \case
        True -> do
          debug "ready for read"
          r <- SCK.unsafeReceiveFromMutableByteArray_ sock buffer 0 (intToCSize fullPacketSize) SCK.dontWait
          case r of
            Left e -> pure (Left ("recv",e))
            Right receivedBytes -> if receivedBytes == intToCSize fullPacketSize
              then do
                payload' <- peekIcmpHeaderPayload buffer
                inline adjust (IPv4 payload') $ \w -> case extractStatus w of
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
          mwriteError <- SCK.unsafeSendToMutableByteArray sock buffer 0 (intToCSize fullPacketSize) SCK.dontWait sockaddr
          case mwriteError of
            Left e -> pure (Left ("send",e))
            Right sentBytes -> if sentBytes == intToCSize fullPacketSize
              then do
                start <- getMonotonicTimeNSec
                inline adjust host (\_ -> pure (pendingStatus (intToWord16 ix) start))
                go (ix + 1)
              else do
                -- could not send out the full packet, should not happen
                pure (Left ("send.bytes",Errno (-1)))
    else multipingStepB buffer sock pause (inline adjust) =<< getMonotonicTimeNSec

-- We start calling this once we run out of hosts to send to. At this point,
-- all responses need to come in soon. We get the current time and count down
-- from this, requiring any outstanding replies to show up within that time
-- frame.
multipingStepB :: MutableByteArray RealWorld -> Fd -> Int -> (IPv4 -> (Word64 -> IO Word64) -> IO ()) -> Word64 -> IO (Either (String,Errno) ())
multipingStepB !buffer !sock !pause !adjust !initialTime = go =<< getMonotonicTimeNSec where
  go !currentTime = do
    let remainingMicroseconds = pause - word64ToInt (div (currentTime - initialTime) 1000)
    if remainingMicroseconds > 0
      then do
        isReady <- waitForRead remainingMicroseconds sock
        if isReady
          then do
            r <- SCK.unsafeReceiveFromMutableByteArray_ sock buffer 0 (intToCSize fullPacketSize) SCK.dontWait
            case r of
              Left e -> pure (Left ("recv",e))
              Right receivedBytes -> if receivedBytes == intToCSize fullPacketSize
                then do
                  payload' <- peekIcmpHeaderPayload buffer
                  end <- getMonotonicTimeNSec
                  inline adjust (IPv4 payload') $ \w -> case extractStatus w of
                    0b01 -> do
                      sequenceNumber' <- peekIcmpHeaderSequenceNumber buffer
                      if sequenceNumber' == extractSequenceNumber w
                        then pure (completeStatus ((end .&. 0x3FFFFFFFFFFF) - extractTimestamp w))
                        else pure w
                    _ -> pure w
                  go end
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

-- | Ping a group of hosts simultaneously. Performs a configurable
--   number of pings for each host and reports the elapsed nanoseconds
--   for each response. If the array of durations is smaller than the
--   total number of pings, it indicates that some ICMP requests for
--   that host were lost or corrupted.
--
-- multipings ::
--      Int -- ^ Microseconds to wait for response
--   -> Int -- ^ Number of pings per host 
--   -> SU.Set IPv4 -- ^ Hosts
--   -> IO (MUN.Map IPv4 (PrimArray Word64))
-- -- Implementation notes: We have a prim array of durations. Each of these
-- -- has enough space to hold all the timestamps for each ping. Additionally,
-- -- they have 3 extra slots at the end: attempted pings, successful pings,
-- -- last send timestamp. These are removed at the end by resizeMutablePrimArray, but
-- -- they are the per-host state over the course of the loop's execution.
-- multipings !pause !pingCount !hosts = do
--   r <- mask $ \restore -> SCK.socket SCK.internet SCK.datagram SCK.icmp >>= \case
--     Left e -> pure (Left ("socket",e))
--     Right sock -> do
--       durations <- restore
--         ( do working <- MUN.fromSetP (\_ -> PM.newPrimArray ((pingCount + 3) * 8)) hosts
--              multiloop working 0 0 0 sock >>= \case
--                -- Nothing -> fmap Right $ MUN.traverse
--                --   (\d -> do
--                --     successes <- PM.readPrimArray d pingCount
--                --     PM.unsafeFreezePrimArray =<< PM.resizeMutablePrimArray d successes
--                --   )
--                -- Just e -> Left e
--         )
--         `onException`
--         (SCK.unsafeClose sock)
--       SCK.unsafeClose sock >>= \case
--         Left e -> pure (Left ("close",e))
--         Right _ -> pure (Right durations)
--   case r of
--     Left (action,e) -> throwMultipingsError action e
--     Right v -> pure v
-- 
-- multiloop ::
--      MUN.Map IPv4 (MutablePrimArray RealWorld Word64) -- durations
--   -> Int -- timeout microseconds
--   -> Int -- total number of pings per host
--   -> Int -- total pings (hosts * pings per host)
--   -> Fd -- socket
--   -> IO ()
-- multiloop !durations !pause !pingCount !total !sock = go 0 where
--   -- Why bother with total? It allows us to stop immidiately
--   -- when we know we will not be receiving anything else. This
--   -- is important when all of the hosts are reachable.
--   go :: Int -> IO ()
--   go ix = error "uhotenuh" -- if ix < total _ <- waitForRead pause sock

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

moduleName :: String
moduleName = "Network.Icmp.Ping"

pingName :: String
pingName = "ping"

multipingName :: String
multipingName = "multiping"

-- multipingsName :: String
-- multipingsName = "multiping"

intToCSize :: Int -> CSize
intToCSize = fromIntegral

-- touchMutableByteArray :: MutableByteArray RealWorld -> IO ()
-- touchMutableByteArray (MutableByteArray x) = touchMutableByteArray# x

-- touchMutableByteArray# :: MutableByteArray# RealWorld -> IO ()
-- touchMutableByteArray# x = IO $ \s -> case touch# x s of s' -> (# s', () #)

-- intToCInt :: Int -> CInt
-- intToCInt = fromIntegral

-- addrToPtr :: Addr -> Ptr Void
-- addrToPtr (Addr x) = Ptr x
