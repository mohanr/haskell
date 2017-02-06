-- | FFI wrappers of CRC32C digest.  Import qualified.

module TensorFlow.CRC32C
  ( value, extend
  , mask, unmask
  , valueMasked
  ) where

import Data.Bits (rotateL, rotateR)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Digest.CRC32C (crc32c_update)
import Data.List (foldl')
import Data.Word (Word32)

-- | Compute the CRC32C checksum of the concatenation of the bytes checksummed
-- by the given CRC32C value and the bytes in the given ByteString.
extend :: Word32 -> B.ByteString -> Word32
extend = crc32c_update

-- | Compute the CRC32C checksum of the given bytes.
value :: BL.ByteString -> Word32
value = foldl' extend 0 . BL.toChunks

-- | Scramble a CRC32C value so that the result can be safely stored in a
-- bytestream that may itself be CRC'd.
--
-- This masking is the algorithm specified by TensorFlow's TFRecords format.
mask :: Word32 -> Word32
mask x = rotateR x 15 + maskDelta

-- | Inverse of 'mask'.
unmask :: Word32 -> Word32
unmask x = rotateL (x - maskDelta) 15

-- | Convenience function combining 'value' and 'mask'.
valueMasked :: BL.ByteString -> Word32
valueMasked = mask . value

maskDelta :: Word32
maskDelta = 0xa282ead8
