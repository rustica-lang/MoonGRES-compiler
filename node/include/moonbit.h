// =====================================
// WARNING: very unstable API, for internal use only
// =====================================

#ifndef moonbit_h_INCLUDED
#define moonbit_h_INCLUDED

#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
#include "moonbit-fundamental.h"
#else
#include <stddef.h>
#include <stdint.h>
#include <math.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if defined (_WIN32) || defined (_WIN64)
#ifdef MOONBIT_BUILD_RUNTIME
#define MOONBIT_EXPORT __declspec(dllexport)
#else
#ifdef MOONBIT_USE_SHARED_RUNTIME
#define MOONBIT_EXPORT __declspec(dllimport)
#else
#define MOONBIT_EXPORT
#endif
#endif
#define MOONBIT_FFI_EXPORT __declspec(dllexport)
#else
#define MOONBIT_EXPORT // __attribute__ ((visibility("default")))
#define MOONBIT_FFI_EXPORT
#endif

enum moonbit_block_kind {
  // 0 => regular block
  moonbit_BLOCK_KIND_REGULAR = 0,
  // 1 => array of pointers
  moonbit_BLOCK_KIND_REF_ARRAY = 1,
  // 2 => array of immediate value/string/bytes
  moonbit_BLOCK_KIND_VAL_ARRAY = 2,
  // 3 => external object with custom deallocator
  //   Note: if we run out of block kind in the future,
  //   we may turn [3] into [moonbit_BLOCK_KIND_EXTENDED],
  //   and store the actual kind & other meta information in the first field
  moonbit_BLOCK_KIND_EXTERNAL = 3
};

struct moonbit_object {
  int32_t rc;
  /* The layout of 32bit meta data (starting from most significant bit):

     enum moonbit_block_kind kind : 2;
     union {
       // when [kind = BLOCK_KIND_REGULAR]
       struct {
         // The 22 length bits are separated into two 11 bit parts:
         //
         // - [ptr_field_offset] is the offset of the first pointer field,
         //   counted by the number of 32-bit words.
         //   The object header itself is also included.
         //
         // - [n_ptr_fields] is the number of pointer fields
         //
         // We rearrange the layout of all pointer fields
         // so that all pointer fields are placed at the end.
         // This make it easy for the runtime to enumerate all pointer fields in an object,
         // without any static type information.
         //
         // The total length of the object can be reconstructed via:
         //
         //   ptr_field_offset * 4 + n_ptr_fields * sizeof(void*)
         unsigned int ptr_field_offset : 11;
         unsigned int n_ptr_fields : 11;
         // For blocks, we steal 8 bits from the length to represent enum tag
         unsigned int tag : 8;
       };
       // when [kind = BLOCK_KIND_REF_ARRAY] or [kind = BLOCK_KIND_VAL_ARRAY]
       struct {
         // The size of array element is [2^object_size_shift] bytes
         unsigned int object_size_shift : 2;
         // The number of elements
         unsigned int len : 28;
       } array_header;
       // when [kind = BLOCK_KIND_REF_EXTERN]
       uint32_t size : 30;
     };
  */
  uint32_t meta;
};

#define Moonbit_object_header(obj) ((struct moonbit_object*)(obj) - 1)
#define Moonbit_object_kind(obj) (Moonbit_object_header(obj)->meta >> 30)
#define Moonbit_object_tag(obj) (Moonbit_object_header(obj)->meta & 0xFF)
#define Moonbit_array_length(obj) (Moonbit_object_header(obj)->meta & (((uint32_t)1 << 28) - 1))
#define Moonbit_array_elem_size_shift(obj) ((Moonbit_object_header(obj)->meta >> 28) & 3)
#define Moonbit_make_array_header(kind, elem_size_shift, length)\
  (((uint32_t)kind << 30)\
   | (((uint32_t)(elem_size_shift) & 3) << 28)\
   | ((length) & (((uint32_t)1 << 28) - 1)))

MOONBIT_EXPORT void *libc_malloc(size_t size);
MOONBIT_EXPORT void libc_free(void *ptr);
MOONBIT_EXPORT void *moonbit_malloc(size_t size);
MOONBIT_EXPORT void moonbit_incref(void *obj);
MOONBIT_EXPORT void moonbit_decref(void *obj);

typedef uint16_t *moonbit_string_t;
typedef uint8_t *moonbit_bytes_t;

struct moonbit_view_t {
  int32_t start;
  int32_t len;
  void *buf;
};

MOONBIT_EXPORT moonbit_string_t moonbit_make_string(int32_t size, uint16_t value);
MOONBIT_EXPORT moonbit_bytes_t moonbit_make_bytes(int32_t size, int value);
MOONBIT_EXPORT int32_t *moonbit_make_int32_array(int32_t len, int32_t value);
MOONBIT_EXPORT void **moonbit_make_ref_array(int32_t len, void *value);
MOONBIT_EXPORT int64_t *moonbit_make_int64_array(int32_t len, int64_t value);
MOONBIT_EXPORT double *moonbit_make_double_array(int32_t len, double value);
MOONBIT_EXPORT float *moonbit_make_float_array(int32_t len, float value);
MOONBIT_EXPORT void **moonbit_make_extern_ref_array(int32_t len, void *value);
MOONBIT_EXPORT void *moonbit_make_view_array(int32_t len, void *ptr);

/* `finalize` should drop the payload of the external object.
   `finalize` MUST NOT drop the [moonbit_external_object] container itself.

   `payload_size` is the size of payload, excluding [drop].

   The returned pointer points directly to the start of user payload.
   The finalizer pointer would be stored at the end of the object, after user payload.
*/
MOONBIT_EXPORT void *moonbit_make_external_object(
  void (*finalize)(void *self),
  uint32_t payload_size
);

MOONBIT_EXPORT extern uint8_t* const moonbit_empty_int8_array;
MOONBIT_EXPORT extern uint16_t* const moonbit_empty_int16_array;
MOONBIT_EXPORT extern int32_t* const moonbit_empty_int32_array;
MOONBIT_EXPORT extern int64_t* const moonbit_empty_int64_array;
MOONBIT_EXPORT extern float*   const moonbit_empty_float_array;
MOONBIT_EXPORT extern double*  const moonbit_empty_double_array;
MOONBIT_EXPORT extern void**   const moonbit_empty_ref_array;
MOONBIT_EXPORT extern void**   const moonbit_empty_extern_ref_array;
MOONBIT_EXPORT extern struct moonbit_view_t* const moonbit_empty_view_array;

#ifdef __cplusplus
}
#endif

#endif // moonbit_h_INCLUDED
