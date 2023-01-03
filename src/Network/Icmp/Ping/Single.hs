{-# language BangPatterns #-}
{-# language EmptyCase #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language UnboxedTuples #-}

module Network.Icmp.Ping.Single
  ( host
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadWaitWrite,threadWaitReadSTM)
import Control.Concurrent.STM.TVar (readTVar,registerDelay)
import Control.Exception (onException,mask)
import Data.Functor (($>))
import Data.Word (Word64,Word8)
import Foreign.C.Error (Errno(..),eAGAIN,eWOULDBLOCK,eACCES)
import Foreign.C.Types (CSize(..))
import GHC.Clock (getMonotonicTimeNSec)
import Net.Types (IPv4(..))
import Network.Icmp.Common (IcmpException(..))
import Network.Icmp.Marshal (peekIcmpHeaderPayload,peekIcmpHeaderType)
import Network.Icmp.Marshal (peekIcmpHeaderSequenceNumber)
import Network.Icmp.Marshal (sizeOfIcmpHeader,pokeIcmpHeader)
import Posix.Socket (SocketAddressInternet(..))
import System.Endian (toBE32)
import System.Posix.Types (Fd(..))

import qualified Control.Monad.STM as STM
import qualified Data.Primitive as PM
import qualified Linux.Socket as SCK
import qualified Posix.Socket as SCK

fullPacketSize :: Int
fullPacketSize = sizeOfIcmpHeader + 4

-- | Ping an IPv4 address. Blocks until a response is received.
host ::
     Int -- ^ Microseconds to wait for response
  -> IPv4 -- ^ Host
  -> IO (Either IcmpException (Maybe Word64)) -- ^ Elapsed nanoseconds
host !maxWaitTime (IPv4 !w) = if maxWaitTime <= 0
  then pure (Right Nothing)
  else do
    -- If a socket cannot be opened, there is not a sensible way for the
    -- caller to recover. We return a structured error in case the user
    -- has a fancy way to display the failure. The user will likely need to
    -- rerun the program with CAP_NET_RAW or as root or after adjusting
    -- net.ipv4.ping_group_range with sysctl.
    mask $ \restore -> SCK.uninterruptibleSocket SCK.Internet SCK.datagram SCK.icmp >>= \case
      Left (Errno e) -> pure (Left (IcmpExceptionSocket e))
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
                 (SCK.uninterruptibleSendToMutableByteArray sock buffer 0 (intToCSize fullPacketSize) SCK.dontWait sockaddr)
                 (threadWaitWrite sock)
               case mwriteError of
                 Left (Errno e)
                     -- When you try to send a packet to a broadcast address, the kernel
                     -- gives you an EACCES failure.
                   | Errno e == eACCES -> pure (Right Nothing)
                   | otherwise -> pure (Left (IcmpExceptionSend e))
                 Right sentBytes -> do
                   if sentBytes == intToCSize fullPacketSize
                     then do
                       isReady <- waitForRead maxWaitTime sock
                       if isReady
                         then do
                           r <- SCK.uninterruptibleReceiveFromMutableByteArray_ sock buffer 0 (intToCSize fullPacketSize) SCK.dontWait
                           case r of
                             Left (Errno e) -> pure (Left (IcmpExceptionReceive e))
                             Right receivedBytes -> if receivedBytes == intToCSize fullPacketSize
                               then do
                                 sequenceNumber' <- peekIcmpHeaderSequenceNumber buffer
                                 payload' <- peekIcmpHeaderPayload buffer
                                 typ <- peekIcmpHeaderType buffer
                                 if sequenceNumber' == 0 && payload' == w && typ == 0
                                   then do
                                     end <- getMonotonicTimeNSec
                                     let !delta = end - start
                                     pure (Right (Just delta))
                                   else pure (Right Nothing) -- response was for a different request
                               else pure (Right Nothing) -- response was the wrong size
                         else pure (Right Nothing) -- did not receive a reply in time 
                     else pure (Left (IcmpExceptionSendBytes sentBytes)) -- could not send out the full packet, should not happen
          )
          `onException`
          -- In the exceptional case, we throw away any errors returned
          -- by unsafeClose (not that we expect to see any). We do this
          -- because there is no other sensible behavior. We would much
          -- rather preserve the original exception.
          (SCK.uninterruptibleClose sock)
        -- It is not neccessary to use closeFdWith here since the socket
        -- cannot possibly be seen by more than one thread.
        SCK.uninterruptibleClose sock >>= \case
          Left (Errno e) -> pure (Left (IcmpExceptionClose e))
          Right _ -> pure elapsed

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

intToCSize :: Int -> CSize
intToCSize = fromIntegral

