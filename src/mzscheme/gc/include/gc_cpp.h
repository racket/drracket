#ifndef GC_CPP_H
#define GC_CPP_H
#if 0
/****************************************************************************
Copyright (c) 1994 by Xerox Corporation.  All rights reserved.
 
THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 
Permission is hereby granted to use or copy this program for any
purpose, provided the above notices are retained on all copies.
Permission to modify the code and to distribute modified code is
granted, provided the above notices are retained, and a notice that
the code was modified is included with the above copyright notice.
****************************************************************************

C++ Interface to the Boehm Collector

    John R. Ellis and Jesse Hull 
    Last modified on Wed Jan  4 16:30:20 PST 1995 by ellis

MATTHEW: This file is greatly modified from the distributed version
for MrEd

This interface provides access to the Boehm collector.  It provides
basic facilities similar to those described in "Safe, Efficient
Garbage Collection for C++", by John R. Elis and David L. Detlefs
(ftp.parc.xerox.com:/pub/ellis/gc).

All heap-allocated objects are either "collectable" or
"uncollectable".  Programs must explicitly delete uncollectable
objects, whereas the garbage collector will automatically delete
collectable objects when it discovers them to be inaccessible.
Collectable objects may freely point at uncollectable objects and vice
versa.

Objects allocated with the built-in "::operator new" are uncollectable.

Objects derived from class "gc" are collectable.  For example:

    class A: gc {...};
    A* a = new A;       // a is collectable. 

Collectable instances of non-class types can be allocated using the GC
placement:

    typedef int A[ 10 ];
    A* a = new (GC) A;

Uncollectable instances of classes derived from "gc" can be allocated
using the NoGC placement:

    class A: gc {...};
    A* a = new (NoGC) A;   // a is uncollectable.

Both uncollectable and collectable objects can be explicitly deleted
with "delete", which invokes an object's destructors and frees its
storage immediately.

A collectable object may have a clean-up function, which will be
invoked when the collector discovers the object to be inaccessible.
An object derived from "gc_cleanup" or containing a member derived
from "gc_cleanup" has a default clean-up function that invokes the
object's destructors.  Explicit clean-up functions may be specified as
an additional placement argument:

    A* a = ::new (GC, MyCleanup) A;

An object is considered "accessible" by the collector if it can be
reached by a path of pointers from static variables, automatic
variables of active functions, or from some object with clean-up
enabled; pointers from an object to itself are ignored.

Thus, if objects A and B both have clean-up functions, and A points at
B, B is considered accessible.  After A's clean-up is invoked and its
storage released, B will then become inaccessible and will have its
clean-up invoked.  If A points at B and B points to A, forming a
cycle, then that's considered a storage leak, and neither will be
collectable.  See the interface gc.h for low-level facilities for
handling such cycles of objects with clean-up.

The collector cannot guarrantee that it will find all inaccessible
objects.  In practice, it finds almost all of them.


Cautions:

1. Be sure the collector has been augmented with "make c++".

2.  If your compiler supports the new "operator new[]" syntax, then
add -DOPERATOR_NEW_ARRAY to the Makefile.

If your compiler doesn't support "operator new[]", beware that an
array of type T, where T is derived from "gc", will by default be
allocated as an uncollectable object.  Use the explicit GC placement
to make the array collectable.  For example:

    class A: gc {...};
    A* a1 = new A[ 10 ];        // uncollectable
    A* a2 = new (GC) A[ 10 ];   // collectable

3. Arrays of objects derived from "gc_cleanup" do not have default
clean-up functions.  For example:

    class A: gc_cleanup {...};
    A* a = new (GC) A[ 10 ];

The elements of "a" will not have their destructors invoked when the
collector frees "a".  You must supply an explicit clean-up function
for that to occur.

4. Compiler bugs:

    Solaris 2's CC (SC3.0) doesn't implement t->~T() correctly, so the
    destructors of classes derived from gc_cleanup won't be invoked.
    You'll have to explicitly register a clean-up function with
    new-placement syntax.

    Evidently cfront 3.0 does not allow destructors to be explicitly
    invoked using the ANSI-conforming syntax t->~T().  If you're using
    cfront 3.0, you'll have to comment out the class gc_cleanup, which
    uses explicit invocation.

****************************************************************************/
#endif

#include "gc.h"

#ifndef THINK_CPLUS
#define _cdecl
#endif

#if __BORLANDC__ >= 0x450 && !defined(OPERATOR_NEW_ARRAY)
#   define OPERATOR_NEW_ARRAY
#endif

/* MATTHEW: GC => UseGC, Added AtomicGC */
enum GCPlacement {UseGC, AtomicGC, NoGC};

class gc {public:
    inline void* operator new( size_t size );
    inline void* operator new( size_t size, GCPlacement gcp );
    inline void operator delete( void* obj );

#ifdef OPERATOR_NEW_ARRAY
    inline void* operator new[]( size_t size );
    inline void* operator new[]( size_t size, GCPlacement gcp );
    inline void operator delete[]( void* obj );
#endif /* OPERATOR_NEW_ARRAY */
    };    
    /*
    Instances of classes derived from "gc" will be allocated in the 
    collected heap by default, unless an explicit NoGC placement is
    specified. */

/* MATTHEW: gcc 2.5.8 doesn't like "virtual" here: */
class gc_cleanup: /* virtual */ public gc {public:
    void *__gc_external; /* MATTHEW: Need a pointer for MzScheme/MrEd */					    
    inline gc_cleanup();
    inline gc_cleanup(int cleanup);
    inline virtual ~gc_cleanup();
    inline void install_cleanup();
private:
    inline static void _cdecl cleanup( void* obj, void* clientData );
};
    /*
    Instances of classes derived from "gc_cleanup" will be allocated
    in the collected heap by default.  When the collector discovers an
    inaccessible object derived from "gc_cleanup" or containing a
    member derived from "gc_cleanup", its destructors will be
    invoked. */

extern "C" {typedef void (*GCCleanUpFunc)( void* obj, void* clientData );}

inline void* operator new( 
    size_t size, 
    GCPlacement gcp,
    GCCleanUpFunc cleanup = 0,
    void* clientData = 0 );
    /*
    Allocates a collectable or uncollected object, according to the
    value of "gcp".

    For collectable objects, if "cleanup" is non-null, then when the
    allocated object "obj" becomes inaccessible, the collector will
    invoke the function "cleanup( obj, clientData )" but will not
    invoke the object's destructors.  It is an error to explicitly
    delete an object allocated with a non-null "cleanup".

    It is an error to specify a non-null "cleanup" with NoGC or for
    classes derived from "gc_cleanup" or containing members derived
    from "gc_cleanup". */

#ifdef OPERATOR_NEW_ARRAY

inline void* operator new[](
    size_t size, 
    GCPlacement gcp,
    GCCleanUpFunc cleanup = 0,
    void* clientData = 0 );
    /*
    The operator new for arrays, identical to the above. */

#endif /* OPERATOR_NEW_ARRAY */

/****************************************************************************

Inline implementation

****************************************************************************/
inline void* gc::operator new( size_t size ) {
    return GC_MALLOC( size );
}
    
inline void* gc::operator new( size_t size, GCPlacement gcp ) {
  /* MATTHEW: Added AtomicGC */
  if (gcp == AtomicGC) 
    return GC_MALLOC_ATOMIC( size );
  else if (gcp == UseGC) 
    return GC_MALLOC( size );
  else
    return GC_MALLOC_UNCOLLECTABLE( size );
}

#if 0
/* MATTHEW */
extern void GC_cpp_delete(void *);
#endif

inline void gc::operator delete( void * /* obj */) {
  /* MATTHEW: Don't GC_free */
#if 0
  GC_cpp_delete(obj); /* zero it */
#endif
}
    

#ifdef OPERATOR_NEW_ARRAY

inline void* gc::operator new[]( size_t size ) {
    return gc::operator new(size);
}
    
inline void* gc::operator new[]( size_t size, GCPlacement gcp ) {
    return gc::operator new(size, gcp);
}

inline void gc::operator delete[]( void* obj ) {
    gc::operator delete( obj );
}
    
#endif /* OPERATOR_NEW_ARRAY */

extern "C" {
  extern void gc_mark_external_invalid(void *);
};

inline gc_cleanup::~gc_cleanup(void) {
  GC_REGISTER_FINALIZER_IGNORE_SELF(this, 0, 0, 0, 0);
  if (__gc_external) gc_mark_external_invalid(__gc_external);
}

inline void gc_cleanup::cleanup( void* obj, void* displ ) {
  gc_cleanup *clean = ((gc_cleanup*) ((char *)obj + (ptrdiff_t)displ));
  clean->~gc_cleanup();
#if 0
  GC_cpp_delete((void *)obj); /* zero it */
#endif
}

inline void gc_cleanup::install_cleanup(void) {
  register void *base = GC_base((void *)this);

  if (base) {
    GC_finalization_proc old_fn;
    void *old_data;
    GC_REGISTER_FINALIZER_IGNORE_SELF(base, cleanup,
				      (void *)((char *)this - (char *)base), 
				      &old_fn, &old_data);
    if (old_fn)
      /* MATTHEW: Put the old one back. We don't need to register. */
      GC_REGISTER_FINALIZER_IGNORE_SELF(base, old_fn, old_data, 
					&old_fn, &old_data);
  }
}

inline gc_cleanup::gc_cleanup(void) {
  __gc_external = NULL; /* MATTHEW: Initialize my hack */
  install_cleanup();
}

inline gc_cleanup::gc_cleanup(int cleanup) {
  __gc_external = NULL; /* MATTHEW: Initialize my hack */
  if (cleanup)
    install_cleanup();
}

inline void* operator new( 
    size_t size, 
    GCPlacement gcp,
    GCCleanUpFunc cleanup,
    void* clientData )
{
  void* obj;
  
  if (gcp == UseGC) { /* MATTHEW: GC => UseGC */
    obj = GC_MALLOC(size);
    if (cleanup != 0) 
      GC_REGISTER_FINALIZER_IGNORE_SELF(obj, cleanup, clientData, 0, 0);
  } if (gcp == AtomicGC) /* MATTHEW: Added AtomicGC */
    obj = GC_MALLOC_ATOMIC( size );
  else
    obj = GC_MALLOC_UNCOLLECTABLE( size );

  return obj;
}
        

#ifdef OPERATOR_NEW_ARRAY

inline void* operator new[]( 
    size_t size, 
    GCPlacement gcp,
    GCCleanUpFunc cleanup,
    void* clientData )
{
    return ::operator new( size, gcp, cleanup, clientData );
  }

#endif /* OPERATOR_NEW_ARRAY */


#endif /* GC_CPP_H */

