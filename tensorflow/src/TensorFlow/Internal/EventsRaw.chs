{-# LANGUAGE ForeignFunctionInterface #-}

module TensorFlow.Internal.EventsRaw where

import Foreign
import Foreign.C

#include "haskell/tensorflow/TensorFlow/Internal/events_writer.h"

{#pointer *TF_EventsWriter as EventsWriter newtype #}

newEventsWriter :: CString -> IO EventsWriter
newEventsWriter = {# call TF_NewEventsWriter as ^ #}

foreign import ccall "&TF_DeleteEventsWriter"
  deleteEventsWriter :: FunPtr (Ptr EventsWriter -> IO ())

writeSerializedEvent :: EventsWriter -> Ptr () -> CULong -> IO ()
writeSerializedEvent = {# call TF_WriteSerializedEvent as ^ #}
