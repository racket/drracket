
/* We have several experimental GC implementations, listed roughly in
   best-to-worst order.

   The copying version is mainly for debugging, since it can move data
   on every collection. */

#include "compact.c"
/* #include "copy.c" */
/* #include "noncopy.c" */
