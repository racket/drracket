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

#include <stdlib.h>
#include "gc_cpp.h"

struct GC_Set *wx_objects, *cpp_objects;

#ifdef USE_WXOBJECT_TRACE_COUNTER
extern void wxTraceCount(void *, int);
extern void wxTracePath(void *, unsigned long, void *);
extern void wxTraceInit(void);
extern void wxTraceDone(void);
extern void wxObjectFinalize(void *);
#endif

void *GC_cpp_malloc(size_t size)
{
  if (!wx_objects)
    wx_objects = GC_new_set("wxObjects", 
#ifdef USE_WXOBJECT_TRACE_COUNTER
			    wxTraceInit,
			    wxTraceDone,
			    wxTraceCount,
			    wxTracePath,
			    wxObjectFinalize,
#else
			    NULL, NULL, NULL, NULL, NULL,
#endif
			    0);

  return GC_malloc_specific(size, wx_objects);
}

void GC_cpp_delete(void*) 
{
}

void* operator new(size_t size) 
{
  if (!cpp_objects)
    cpp_objects = GC_new_set("C++", NULL, NULL, NULL, NULL, NULL, 0);

  return GC_malloc_specific(size, cpp_objects);
}
  
void operator delete( void* obj ) {
}
  

#ifdef OPERATOR_NEW_ARRAY

void* operator new[](size_t size)
{
  if (!cpp_objects)
    cpp_objects = GC_new_set("C++", NULL, NULL, NULL, NULL, NULL, 0);

  return GC_malloc_specific(size, cpp_objects);
}
  
void operator delete[](void*) {
}

#endif /* OPERATOR_NEW_ARRAY */


void GC_cpp_for_each(void (*f)(void *, int, void *), void *data)
{
  if (wx_objects)
    GC_for_each_element(wx_objects, f, data);
}

int GC_is_wx_object(void *v)
{
  return wx_objects && (GC_set(v) == wx_objects);
}
