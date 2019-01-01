{-# language DataKinds #-}

#include <netinet/ip_icmp.h>
#include "custom.h"

module Network.Icmp.Marshal
  ( pokeIcmpHeader
  , peekIcmpHeaderSequenceNumber
  , peekIcmpHeaderPayload
  , peekIcmpHeaderType
  , sizeOfIcmpHeader
  ) where

import Data.Word (Word32,Word16,Word8)
import GHC.Exts (RealWorld)

import Data.Primitive (MutableByteArray)
import Data.Primitive (readByteArray,writeByteArray)

-- The linux kernel (version 4.19.9) defines icmphdr as:
--
-- struct icmphdr {
--   __u8		type;
--   __u8		code;
--   __sum16	checksum;
--   union {
-- 	struct {
-- 		__be16	id;
-- 		__be16	sequence;
-- 	} echo;
-- 	__be32	gateway;
-- 	struct {
-- 		__be16	__unused;
-- 		__be16	mtu;
-- 	} frag;
-- 	__u8	reserved[4];
--   } un;
-- };

sizeOfIcmpHeader :: Int
sizeOfIcmpHeader = #{size struct icmphdr}

-- This sets the type to ICMP_ECHO and the sequence number to
-- user-specified values. The sequence number is supposed to be
-- in network byte order, but this does not actually matter.
-- If it is mangled on the way out, it will also be
-- mangled when we receive it.
--
-- Why is the identifier (un.echo.id) missing? Linux overwrites
-- the identifier regardless of what the user puts there. This
-- makes sense since the operating system wants to make it as
-- easy as possible to hand the reply to the right process.
pokeIcmpHeader ::
     MutableByteArray RealWorld
  -> Word16 -- sequence number
  -> Word32 -- payload, we use this as a bigger sequence number
  -> IO ()
pokeIcmpHeader ptr sequenceNumber payload = do
  #{write struct icmphdr, type} ptr (#{const ICMP_ECHO} :: Word8)
  #{write struct icmphdr, un.echo.sequence} ptr sequenceNumber
  writeByteArray ptr #{elementize sizeof (struct icmphdr), 4} payload

peekIcmpHeaderType :: MutableByteArray RealWorld -> IO Word8
peekIcmpHeaderType ptr = do
  #{read struct icmphdr, type} ptr

peekIcmpHeaderSequenceNumber :: MutableByteArray RealWorld -> IO Word16
peekIcmpHeaderSequenceNumber ptr = do
  #{read struct icmphdr, un.echo.sequence} ptr

peekIcmpHeaderPayload :: MutableByteArray RealWorld -> IO Word32
peekIcmpHeaderPayload ptr = do
  readByteArray ptr #{elementize sizeof (struct icmphdr), 4}

