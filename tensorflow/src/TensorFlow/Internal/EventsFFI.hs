{-# LANGUAGE OverloadedStrings #-}

module TensorFlow.Internal.EventsFFI
    ( EventsWriter
    , newEventsWriter
    , logEvent
    )
    where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (throwIO, mask_)
import Control.Monad (when)
import Foreign (nullPtr)
import Foreign.C.String (withCString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Proto.ThirdParty.Tensorflow.Core.Util.Event (Event)
import TensorFlow.Internal.FFI (TensorFlowException(..), useProtoAsVoidPtrLen)

import qualified TensorFlow.Internal.EventsRaw as Raw
import qualified TensorFlow.Internal.Raw as Raw

-- | Handle for logging TensorBoard events safely from multiple threads.
newtype EventsWriter = EventsWriter (MVar (ForeignPtr Raw.EventsWriter))

-- | Creates a new event writer object or throws a TensorFlowException
-- on failure.
newEventsWriter :: FilePath
                   -- ^ File system prefix for creating log files.
                -> IO EventsWriter
newEventsWriter filePrefix = do
    foreignPtr <-
        mask_ (newForeignPtr Raw.deleteEventsWriter =<< allocateWriter)
    EventsWriter <$> newMVar foreignPtr
  where
    allocateWriter = withCString filePrefix $ \cFilePrefix -> do
        Raw.EventsWriter ew <- Raw.newEventsWriter cFilePrefix
        when (ew == nullPtr) (throwIO exception)
        return ew
    exception = TensorFlowException
                Raw.TF_UNKNOWN "EventsWriter failure, check logs"

-- | Logs the given Event protocol buffer.
logEvent :: EventsWriter -> Event -> IO ()
logEvent (EventsWriter mew) pb =
    useProtoAsVoidPtrLen pb $ \ptr len ->
        withMVar mew $
        \pew -> withForeignPtr pew $
        \ew -> Raw.writeSerializedEvent (Raw.EventsWriter ew) ptr len
