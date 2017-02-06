#include "third_party/tensorflow/core/lib/hash/crc32c.h"

extern "C" {

uint32 HaskellGoogleTensorflowTFRecords_CRC32C_Extend(uint32 init_crc,
                                                      const char* data,
                                                      size_t n) {
  return tensorflow::crc32c::Extend(init_crc, data, n);
}

}  // extern "C"
