{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid ((<>))
import Data.Word (Word8)
import Data.Serialize (runGet, runPut)
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import TensorFlow.Records (getTFRecord, putTFRecord)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testProperty "Inverse" propEncodeDecodeInverse
    , testProperty "FixedRecord" propFixedRecord
    ]

-- There's no (Arbitrary BL.ByteString), so pack it from a list of chunks.
propEncodeDecodeInverse :: [[Word8]] -> Bool
propEncodeDecodeInverse s =
    let bs = BL.fromChunks . fmap B.pack $ s
    in  runGet getTFRecord (runPut (putTFRecord bs)) == Right bs

propFixedRecord :: Bool
propFixedRecord =
    ("\x42" == case runGet getTFRecord record of
        Left err -> error err  -- Make the error appear in the test failure.
        Right x -> x) &&
    (runPut (putTFRecord "\x42") == record)
  where
    record = "\x01\x00\x00\x00\x00\x00\x00\x00" <> "\x01\x75\xde\x41" <>
             "\x42" <> "\x52\xcf\xb8\x1e"
