#define INCLUDE_WITHOUT_PATHS

#ifdef __MWERKS__
#if defined(__powerc)
#include <MacHeadersPPC>
#else
#include <MacHeaders68K>
#endif
#endif

// these are defined again in gc_priv.h.
#undef TRUE
#undef FALSE

//#define ALL_INTERIOR_POINTERS     // follows interior pointers.
#define SILENT                    // no collection messages.
//#define DONT_ADD_BYTE_AT_END      // no padding.
//#define SMALL_CONFIG              // use a smaller heap.
#define USE_TEMPORARY_MEMORY      // use Macintosh temporary memory.
#define OLD_BLOCK_ALLOC

// EEXIST --- the error in errno.h returned when a file cannot be
// created because the file already exists --- is not defined on the mac.

#define EEXIST dupFNErr

