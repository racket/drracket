/*************************************************************************
Copyright (c) 1994 by Xerox Corporation.  All rights reserved.
 
THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 
    Last modified on Sat Nov 19 19:31:14 PST 1994 by ellis
                  on Sat Jun  8 15:10:00 PST 1994 by boehm

Permission is hereby granted to copy this code for any purpose,
provided the above notices are retained on all copies.

This implementation module for gc_c++.h provides an implementation of
the global operators "new" and "delete" that calls the Boehm
allocator.  All objects allocated by this implementation will be
non-collectable but part of the root set of the collector.

You should ensure (using implementation-dependent techniques) that the
linker finds this module before the library that defines the default
built-in "new" and "delete".

Authors: John R. Ellis and Jesse Hull

MATTHEW: This file is greatly modified from the distributed version
for MrEd

**************************************************************************/
/* Boehm, December 20, 1994 7:26 pm PST */

#include "gc_cpp.h"

/* Zeroing object doesn't seem to make a difference: */
#if 0
void GC_cpp_delete( void* obj ) 
{
  if (obj) {
    void *b = (void *)GC_base(obj);
    
    if (b == obj) {
      *(long *)b = 0x23051423;

      size_t l = GC_size(b);
      
      memset(b, 0, l);
    }
  }
}
#endif

void* operator new( size_t size ) {
  /* MATTHEW: Collect everything.*/
  return GC_MALLOC /* _UNCOLLECTABLE */ ( size );
}
  
void operator delete( void* obj ) {
  /* MATTHEW: No GC_FREE */
#if 0
  GC_cpp_delete(obj);
#endif
}
  

#ifdef OPERATOR_NEW_ARRAY

void* operator new[]( size_t size ) {
  /* MATTHEW: Collect everything.*/
  return GC_MALLOC /* _UNCOLLECTABLE */( size );
}
  
void operator delete[]( void* obj ) {
  /* MATTHEW: No GC_FREE */
#if 0
  GC_cpp_delete(obj);
#endif
}

#endif /* OPERATOR_NEW_ARRAY */


