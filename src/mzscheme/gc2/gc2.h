
#ifndef __mzscheme_gc_2__
#define __mzscheme_gc_2__

#ifndef GC2_JUST_MACROS

# ifdef __cplusplus
extern "C" {
# endif

/*
   General architecture:

   Each allocation is for either tagged or untagged memory. Untagged
   memory must be either atomic or an array of pointers. Heterogenous
   memory is always tagged. After alocating a tagged object, MzScheme
   is responsible for setting the tag before a collection occurs. The
   tag is always a `short' value at the beginning of the

   MzScheme supplies a traversal procedure for every tag value. The
   traversal takes two arguments, an allocated object and a procedure
   pointer, and returns the size of the object in words (not bytes!),
   which is not necessarily fixed for a particular tag. If the
   procedure pointer supplied to the traverser is not NULL, it must
   also call the given procedure on every pointer stored in the
   object, via the gcMARK_HERE macro described below.

   The value of a pointer can refer to a GCable object, to the middle
   of a GCable object, or to some other point in memory. It might also
   be a fixnum value, which has 1 in the least-significant bit,
   whereas actual pointers are always 2-byte-aligned. Thus, when
   moving GCable objects in a copying collector, the collector must
   modify pointers to objects or to the middle of objects, but leave
   other pointers and fixnums alone.

   At any point when the allocator/collector is invoked, MzScheme will
   have set the GC_variable_stack variable to indicate a chain of
   local pointer variables on the stack (i.e., both the chain of
   record and the pointer variables themselves are on the stack). The
   GC_variable_stack global points to a value of the form

     struct {
        void *next_frame;
        long frame_size;
        void **pointers[...];
     }

   where the size of `pointers' is indicated by `frame_size'. Each
   element of `pointers' is the address of a pointer of the stack.
   The `next_frame' field in the structure gives the address of the
   next frame on the stack, and so on. The garbage collector should
   follow the chain of frames, adjusting pointers for copying
   collection, until it reaches a frame that is deeper than the value
   returned by GC_get_thread_stack_base() (which is supplied by
   MzScheme).

   More generally, GC_mark_variable_stack() can be used to GC a stack
   that has been copied into the heap. See below for more details.  */

/***************************************************************************/
/* Administration                                                          */
/***************************************************************************/

extern unsigned long (*GC_get_thread_stack_base)(void);
/* 
   Called by GC to get the base for stack traversal in the current
   thread. The returned address must not be in the middle of
   a variable-stack record. */

void GC_set_stack_base(void *base);
unsigned long GC_get_stack_base(void);
/*
   Called by MzScheme to set/get value used for stack base when
   GC_get_thread_stack_base is null. This is mainly useful for getting
   MzScheme started, before it has multiple threads. */

void GC_add_roots(void *start, void *end);
/*
   Called by MzScheme to install roots. The memory between `start'
   (includive) and `end' (exclusive) contains pointers. */

void GC_init_type_tags(int count, int weakbox);
/*
   Called by MzScheme to indicate the number of different type tags it
   uses, starting from 0. `count' is always less than 256. The weakbox
   argument is the value to be used for tagging weak box. (The GC has
   some freedom in the layout of a weak box, so it performs weak bo
   traversals itself, but MzScheme gets to choose the tag.) */

extern void (*GC_collect_start_callback)(void);
extern void (*GC_collect_end_callback)(void);
/*
   Called by GC before/after performing a collection. Used by MzScheme
   to zero out some data and record collection times. */

extern void (*GC_out_of_memory)(void);
/*
   Called by GC when it can't satify a memory request. GC_out_of_memory
   might perform a longjmp. */

void GC_dump(void);
/*
   Dumps memory state info to stderr. */

long GC_get_memory_use();
/*
   Returns the number of currently-allocated bytes. */

void GC_gcollect(void);
/*
   Performs an immediate collection. */

/***************************************************************************/
/* Allocation                                                              */
/***************************************************************************/

/* Note: All alocated memory must be longword-aligned. For architectures
   where `double' values must be 8-byte aligned, the GC must provide
   8-byte aligned memory in response to an allocation request for a
   memory size divisible by 8. */

void *GC_malloc(size_t size_in_bytes);
/*
   Alloc an array of pointers, initially zeroed. */

void *GC_malloc_one_tagged(size_t);
/* 
   Alloc a tagged item, initially zeroed.  MzScheme sets the tag
   before a collection. */

void *GC_malloc_array_tagged(size_t);
/* 
   Alloc an array of tagged items. MzScheme sets the tag in the first
   item before a collection, by maybe not all items. When traversing,
   use the first one for size, and traverse only those with the tag
   set. */

void *GC_malloc_atomic(size_t size_in_bytes);
/*
   Alloc pointerless memory (not necessarily zeroed). */

#define GC_malloc_atomic_tagged GC_malloc_one_tagged
/*
   Alloc pointer-free tagged memory (not necessarily zeroed).
   MzScheme sets the tag before a collection. */

void *GC_malloc_atomic_uncollectable(size_t size_in_bytes);
/*
   Like plain malloc: pointer-free, never collected. */

void *GC_malloc_weak_array(size_t size_in_bytes, void *replace_val);
/* 
   Alloc an array of weak pointers, initially zeroed.  When a value in
   the array is collected, it's replaced by `replace-val'. The
   precense of a pointer in the array doesn't keep the referenced
   memory from being collected. */

void GC_free(void *);
/* 
   Lets the collector optionally reverse an allocation immediately.
   [Generally a noop.] */

void *GC_malloc_weak_box(void *p, void **secondary);
/* 
   Allocate a weak box. A weak box must have the following initial
   structure:

     struct {
       short tag;
       short filler_used_for_hashing;
       void *val;
     }

   but the remainder of the structure is up to the GC. The GC recives
   from MzScheme a tag value to be used for weak boxes (see the
   GC_init_type_tags() function above), but the GC is responsible for
   traversing weak boxes.

   MzScheme can change `value' at any time; when a collection happens,
   if the object in `val' is collectable and is collected, then `val'
   is zeroed.  The `val' pointer must be updated by the GC if the
   object it refers to is moved by the GC, but it does not otherwise
   keep an object from being collected.

   The `secondary' argument points to an auxilliary address (probably
   in the middle of a collectable object) that should be zeroed
   whenever `val' is zeroed. The memory referenced by `secondary' is
   kept live as long as it isn't zeroed by its registration in the
   weak box, but when the content of `secondary' is zeroed, the
   `secondary' pointer itself should be dropped. */

/***************************************************************************/
/* Finalization                                                            */
/***************************************************************************/

void GC_register_eager_finalizer(void *p, int level, void (*f)(void *p, void *data), 
				 void *data, void (**oldf)(void *p, void *data), 
				 void **olddata);
/*
   Installs a finalizer to be queued for invocation when `p' would
   otherwise be collected. `p' isn't actually collected when a
   finalizer is queued, since the finalizer will receive `p' as an
   argument. (Hence, weak references aren't zeroed, either.)

   `level' refers to an ordering of finalizers. It can be 1 or
   2. During a collection, level 2 finalizers are queued first, then
   all objects queued for finalization are marked as live and
   traversed. Then, level 1 finalizers are queued in the same
   way. Thus, if a level 2 object refers to a level 1 object, the
   level 2 object will be queued for finalization, and only sometime
   after the finalizer is run and the object is again no longer
   refermced can the level 1 object be finalized.

   The `f' and `data' arguments are the finalizer clsoure to be
   called. If a finalizer is already installed for `p', it is
   replaced, and `oldf' and `olddata' are filled with the old
   closure. If `f' is NULL, any existing finalizer is removed and no
   new one is installed. */

void GC_register_finalizer(void *p, void (*f)(void *p, void *data), 
			   void *data, void (**oldf)(void *p, void *data), 
			   void **olddata);
/* 
   Eventally to be used for non-eager finalizers (which will be
   defined at that point). Currently, it's only used to clear
   finalizers (i.e., `f' is NULL). */

void GC_finalization_weak_ptr(void **);

/***************************************************************************/
/* Cooperative GC                                                          */
/***************************************************************************/

extern void **GC_variable_stack;
/*
   See the general overview at the top of the file: */

typedef void *(*Mark_Proc)(void *);
/*
   Type of a marking procedure (supplied by GC) provided to a
   traversal proc; use gcMARK_HERE instead of calling it directly. */

typedef int (*Traverse_Proc)(void *obj, Mark_Proc);
/* 
   Type of a traversal proc (supplied by MzScheme); see overview above
   for information about traversals. The return value is the same of
   the object in words. */

void GC_register_traverser(short tag, Traverse_Proc proc);
/*
   Registers a traversal procedure for a tag. Obviously, a traversal
   procedure must be installed for each tag before a collection
   happens where an instance of the tag as been allocated. */

/* #define gcMARK_HERE(mark, t, x) ... see below ... */
/* A macro that, given the mark procedure supplied to a traverser, a
   type, and an l-value containing a pointer, marks the referenced
   memory as live and updates the pointer as necessary (i.e., if it's
   GCable memory that is moving). The `x' argument can appear in the
   macro's output multiple times, and it can be a statement rather
   than a expression. */

/* #define gcMARK_TYPED(t, x) gcMARK_HERE(mark, t, x) */
/*
   Handy macro that assumes the marking procedure is bound to the
   local variable `mark'. */

/* #define gcMARK(x) gcMARK_TYPED(void*, x) */
/*
   Handy macro that assumes that `void*' is a good type. */

/* #define gcBYTES_TO_WORDS(x) ((x + 3) >> 2) */
/*
   Helpful macro for computing the return value in a traversal proc,
   which must be in words. */

void *GC_resolve(void *p);
/*
   Can be called by a traversal proc to get the current address of a
   object that might have been moved already. This is necessary, for
   example, if the size or structure of an object depends on the
   content of an object it references. For example, the size of a
   class instance usually depends on a field count that is stored in
   the class. */

void GC_mark_variable_stack(void **var_stack,
			    long delta,
			    void *limit);
/*
   Can be called by a traversal proc to traverse and update a chunk of
   (atomically-allocated) memory containing an image of the stack. It
   should only be called when the traversal proc is called with a
   non-NULL mark procedure.

   The `var_stack' argument corresponds to the value of GC_var_stack
   for the copied stack (see the overview at the top of this
   file). The `var_stack' pointer refers to the address of the chain
   in the original stack, not in the heap copy. The `delta' argument
   specifies the difference heap_copy_address - stack_address (where
   stack_address is the numerically lower bound for the copied stack
   region, regardless of which direction the stack grows). The `limit'
   argument corresponds to the value that would have been returned by
   GC_get_thread_stack_base() at the time the stack was copied. */

extern void *GC_alloc_space, *GC_alloc_top;
/*
   Used by macro the implementations above; not to be considered part
   of the spec. */

# ifdef __cplusplus
};
# endif

#endif

/* Macros: */
#define gcMARK_HERE(mark, t, x) if (!((long)(x) & 0x1) \
                                   && ((unsigned long)(x) >= (unsigned long)GC_alloc_space) \
                                   && ((unsigned long)(x) <= (unsigned long)GC_alloc_top)) \
                                 x = (t)mark(x)
#define gcMARK_TYPED(t, x) gcMARK_HERE(mark, t, x)
#define gcMARK(x) gcMARK_TYPED(void*, x)
#define gcBYTES_TO_WORDS(x) ((x + 3) >> 2)

#endif /* __mzscheme_gc_2__ */
