#include "haskell/tensorflow/TensorFlow/Internal/events_writer.h"

#include "third_party/tensorflow/core/util/events_writer.h"

using tensorflow::EventsWriter;

#ifdef __cplusplus
extern "C" {
#endif

struct TF_EventsWriter {
  explicit TF_EventsWriter(const string& s) : writer(s) {}
  EventsWriter writer;
};

TF_EventsWriter* TF_NewEventsWriter(const char* file_prefix) {
  CHECK(file_prefix != nullptr);
  auto *result = new TF_EventsWriter(file_prefix);
  if (!result->writer.Init()) {
    delete result;
    return nullptr;
  } else {
    return result;
  }
}

void TF_DeleteEventsWriter(TF_EventsWriter* w) { delete w; }

void TF_WriteSerializedEvent(TF_EventsWriter* w, const void* proto,
                             size_t proto_len) {
  w->writer.WriteSerializedEvent(
      tensorflow::StringPiece(reinterpret_cast<const char*>(proto), proto_len));
}

#ifdef __cplusplus
} /* end extern "C" */
#endif
