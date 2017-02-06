-- | Conduit wrappers for TensorFlow.Records.

{-# LANGUAGE Rank2Types #-}
module TensorFlow.Records.Conduit
  (
  -- * Encode/Decode
    encodeTFRecords
  , decodeTFRecords

  -- * Source/Sink
  , sinkTFRecords
  , sourceTFRecords
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit ((=$=), Conduit, Consumer, Producer)
import Data.Conduit.Binary (sinkFile, sourceFile)
import Data.Conduit.Cereal (conduitGet2, conduitPut)

import TensorFlow.Records (getTFRecord, putTFRecord)

-- | Decode TFRecords from a stream of bytes.
decodeTFRecords :: MonadThrow m => Conduit B.ByteString m BL.ByteString
decodeTFRecords = conduitGet2 getTFRecord

-- | Read TFRecords from a file.
sourceTFRecords :: (MonadResource m, MonadThrow m) => FilePath -> Producer m BL.ByteString
sourceTFRecords path = sourceFile path =$= decodeTFRecords

-- | Encode TFRecords to a stream of bytes.
encodeTFRecords :: Monad m => Conduit BL.ByteString m B.ByteString
encodeTFRecords = conduitPut putTFRecord

-- | Write TFRecords to a file.
sinkTFRecords :: (MonadResource m) => FilePath -> Consumer BL.ByteString m ()
sinkTFRecords path = encodeTFRecords =$= sinkFile path
