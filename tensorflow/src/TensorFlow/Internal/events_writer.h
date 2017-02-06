#ifndef HASKELL_TENSORFLOW_TENSORFLOW_INTERNAL_EVENTS_WRITER_H_
#define HASKELL_TENSORFLOW_TENSORFLOW_INTERNAL_EVENTS_WRITER_H_

#include <stddef.h>
// --------------------------------------------------------------------------
// C API for access to TensorFlow EventsWriter.

#ifdef __cplusplus
extern "C" {
#endif

// TF_EventsWriter is a handle to the object for saving TensorFlow
// events consumed by TensorBoard. The object is not thread safe.
typedef struct TF_EventsWriter TF_EventsWriter;

// Creates a new event writer object. It must be released with
// TF_DeleteEventsWriter. The returned value may be a nullptr on
// failure and the errors are reported to the log (limitation of
// EventsWriter).
TF_EventsWriter* TF_NewEventsWriter(const char* file_prefix);

// Deletes a previously created TF_NewEventsWriter object.
void TF_DeleteEventsWriter(TF_EventsWriter* w);

// Writes the given serialized tensorboard::Event proto into the
// writer.
void TF_WriteSerializedEvent(TF_EventsWriter* writer, const void* proto,
                             size_t proto_len);

#ifdef __cplusplus
} /* end extern "C" */
#endif

#endif  // HASKELL_TENSORFLOW_TENSORFLOW_INTERNAL_EVENTS_WRITER_H_
