-- | Tests for EventsFFI.
module Main where

import Data.Default (def)
import Data.List (isPrefixOf)
import Google.Test (googleTestWithFlags, testTmpdir)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import TensorFlow.Internal.EventsFFI (newEventsWriter, logEvent)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertBool)

testLogWrite dir = testCase testName $ do
    assertBool "No file before" . not =<< hasMatchingFiles
    ev <- newEventsWriter (dir </> testName)
    logEvent ev def
    assertBool "File exists after" =<< hasMatchingFiles
  where
    testName = "LogWrite"
    hasMatchingFiles =
        any (testName `isPrefixOf`) <$> getDirectoryContents dir

main :: IO ()
main = googleTestWithFlags $ \flags ->
    let tmpDir = testTmpdir flags in
    [ testLogWrite tmpDir
    ]
