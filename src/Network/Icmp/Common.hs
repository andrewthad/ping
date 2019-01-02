module Network.Icmp.Common
  ( IcmpException(..)
  ) where

import Foreign.C.Types (CSize,CInt)
import Control.Exception (Exception)

data IcmpException
  = IcmpExceptionSocket !CInt
    -- ^ Could not create the socket
  | IcmpExceptionSend !CInt
    -- ^ Unable to send when the event manager indicated that the socket
    --   was ready for writes. 
  | IcmpExceptionSendBytes !CSize
    -- ^ Unable to send the entirity on an ICMP request. The field is
    --   the number of bytes actually sent.
  | IcmpExceptionReceive !CInt
    -- ^ Unable to receive when the event manager indicated that the socket
    --   was ready for reads. 
  | IcmpExceptionClose !CInt
    -- ^ Could not close the socket.
  deriving (Show,Eq)

instance Exception IcmpException


