/*
  Precise SenoraGC
  Copyright (c) 2000 Matthew Flatt
  All rights reserved.

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  After Boehm et al.

  Note: this implementation is probably still a little hardwired for
  32-bit addresses.

 */

#include <stdlib.h>
#include <setjmp.h>
#include <stdio.h>
#include <memory.h>
#include <string.h>
#include "../sconfig.h"
#include "gc2.h"

/****************************************************************************/
/* Option bundles                                                           */
/****************************************************************************/

#ifndef SGC_STD_DEBUGGING
# define SGC_STD_DEBUGGING 0
#endif

#ifdef WIN32
# define SGC_STD_DEBUGGING_UNIX 0
# define SGC_STD_DEBUGGING_WINDOWS SGC_STD_DEBUGGING
#else
# define SGC_STD_DEBUGGING_UNIX SGC_STD_DEBUGGING
# define SGC_STD_DEBUGGING_WINDOWS 0
#endif

#ifndef SGC_AUTO_ROOTS
# define SGC_AUTO_ROOTS 1
#endif

/****************************************************************************/
/* Options and debugging flags                                              */
/****************************************************************************/

#define NO_COLLECTIONS 0
/* Disable all collections */

#define NO_DISAPPEARING 0
/* Never perform disappearing link */

#define NO_FINALIZING 0
/* Never invoke queued finalizers */

#define NO_ATOMIC 0
/* Never treat allocated data as atomic */

#define KEEP_BLOCKS_FOREVER 0
/* Number of block sector assignments to keep indefinitely, rather
   than recycling them immediately. This speeds up collection
   and allocation, but it also costs memory via fragmentation. */

#define NO_STACK_OFFBYONE 0
/* Don't notice a stack-based pointer just past the end of an object */

#define WATCH_FOR_FINALIZATION_CYCLES 0
/* Report cycles in finalizable chunks */

#define USE_GC_FREE_SPACE_DIVISOR 1
/* Grow stack in a way similar to Boehm's GC using the global
   GC_free_space_divisor. 0 => GC after allocating twice the
   active size from the last GC. */

#define PROVIDE_GC_FREE 0
/* Provide GC_free; automatically implies DISTINGUISH_FREE_FROM_UNMARKED */

#define PROVIDE_CHUNK_GC_FREE 1
/* Provide GC_free that only works on chunks (i.e., large allocation
   blocks); frees on small blacks are ignored */

#define GET_MEM_VIA_SBRK 0
/* Instead of calling malloc() to get low-level memory, use
   sbrk() directly. (Unix) */

#define GET_MEM_VIA_MMAP 0
/* Instead of calling malloc() to get low-level memory, use
   mmap() directly. (Unix) */

#define GET_MEM_VIA_VIRTUAL_ALLOC SGC_STD_DEBUGGING_WINDOWS
/* Instead of calling malloc() to get low-level memory, use
   VirtualAlloc() directly. (Win32) */

#define RELEASE_UNUSED_SECTORS 1
/* Instead of managing a list of unused sectors, they are
   given back to the OS. This only works with mmap(). */

#define DISTINGUISH_FREE_FROM_UNMARKED 0
/* Don't let conservatism resurrect a previously-collected block */

#define STD_MEMORY_TRACING SGC_STD_DEBUGGING
/* Automatically implies TERSE_MEMORY_TRACING, DUMP_BLOCK_COUNTS,
   and DUMP_SECTOR_MAP */

#define DETAIL_MEMORY_TRACING 0
/* Automatically implies STD_MEMORY_TRACING, DUMP_BLOCK_MAPS, 
   and KEEP_DETAIL_PATH */

#define KEEP_DETAIL_PATH SGC_STD_DEBUGGING
/* Keep source offsets for path traces */

#define ALLOW_SET_FINALIZER 0
/* Support the per-set custom "de-allocation" callback. */

#define CHECK_WATCH_FOR_PTR_ALLOC SGC_STD_DEBUGGING
/* Set GC_watch_for_ptr to be ~(ptr value);
   there are 3 places where the ptr is checked, 
   unless USE_WATCH_FOUND_FUNC is on */

#define USE_WATCH_FOUND_FUNC SGC_STD_DEBUGGING
/* Calls GC_found_watch when the watch-for ptr is found. */

#define CHECK_SIMPLE_INTERIOR_POINTERS 0
/* Recognize pointers into the middle of an allocated
   block, (but do not recognize pointers just past the
   end of an allocated block, as is generally performed
   for stack-based pointers). */

#define DUMP_BLOCK_COUNTS 1
/* GC_dump prints detail information about block and
   set size contents. */

#define DUMP_SECTOR_MAP 1
/* GC_dump prints detail information about existing
   sectors. */

#define DUMP_BLOCK_MAPS 1 /* 0 */
/* GC_dump prints detail information about block and
   set address contents. Automatically implies
   DUMP_BLOCK_COUNTS. */

#define CHECK_FREES SGC_STD_DEBUGGING
/* Print an error for redundant frees by calling free_error */

#define PRIM_STRINGOUT_AS_FWRITE 0
/* Implements GC_prim_stringout using fwrite. Not likely to
   solve any problems, but useful for debugging FPRINTF. */

#define PRIM_STRINGOUT_AS_WINDOWS_CONSOLE SGC_STD_DEBUGGING_WINDOWS
/* Implements GC_prim_stringout using Windows console
   functions. */

#define AUTO_STATIC_ROOTS_IF_POSSIBLE SGC_AUTO_ROOTS
/* Automatically registers static C variables as roots if
   platform-specific code is porvided */

#define PRINT_INFO_PER_GC SGC_STD_DEBUGGING
/* Writes to stderr before an after each GC, summarizing
   the state of memory and current system time at each point. */

#define SHOW_SECTOR_MAPS_AT_GC 0
/* Write a sector map before and after each GC. This is helpful for
   noticing unusual memory pattens, such as allocations of large
   blocks or unusually repetitive allocations. Only works with
   PRINT_INFO_PER_GC. */

/****************************************************************************/
/* Parameters and platform-specific settings                                */
/****************************************************************************/

/* GC frequency: MEM_USE_FACTOR is max factor between current
   allocated bytes and alocated bytes after last GC. */
#ifdef SMALL_HASH_TABLES
# define FIRST_GC_LIMIT 20000
# define MEM_USE_FACTOR 1.40
#else
# define FIRST_GC_LIMIT 100000
# define MEM_USE_FACTOR 3
#endif

# define PTR_TO_INT(v) ((unsigned long)(v))
# define INT_TO_PTR(v) ((void *)(v))

#if GET_MEM_VIA_SBRK
# include <unistd.h>
#endif
#if GET_MEM_VIA_MMAP
# include <unistd.h>
# include <sys/mman.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <fcntl.h>
#endif
#ifdef WIN32
# include <windows.h>
#endif

#if !GET_MEM_VIA_MMAP
# undef RELEASE_UNUSED_SECTORS
# define RELEASE_UNUSED_SECTORS 0
#endif

/* System-specific alignment of pointers. */
#define PTR_ALIGNMENT 4
#define LOG_PTR_SIZE 2
#define PTR_SIZE (1 << LOG_PTR_SIZE)

#define DOUBLE_SIZE sizeof(double)

/* SECTOR_SEGMENT_SIZE determines the alignment of collector blocks.
   Since it should be a power of 2, LOG_SECTOR_SEGMENT_SIZE is
   specified directly. A larger block size speeds up GC, but wastes
   more unallocated bytes in same-size buckets. */
# define LOG_SECTOR_SEGMENT_SIZE 12
#define SECTOR_SEGMENT_SIZE (1 << LOG_SECTOR_SEGMENT_SIZE)
#define SECTOR_SEGMENT_MASK (~(SECTOR_SEGMENT_SIZE-1))

/* MAX_COMMON_SIZE is maximum size of allocated blocks
   using relatively efficient memory layouts. */
#define MAX_COMMON_SIZE (SECTOR_SEGMENT_SIZE >> 2)

#define NUM_COMMON_SIZE ((2 * LOG_SECTOR_SEGMENT_SIZE) + 8)

/* Number of sector segments to be allocated at once with
   malloc() to avoid waste when obtaining the proper alignment. */
#define SECTOR_SEGMENT_GROUP_SIZE 32

/* Number of bits used in first level table for checking existance of
   a sector. Creates a table of (1 << SECTOR_LOOKUP_SHIFT) pointers
   to individual page tables of size SECTOR_LOOKUP_PAGESIZE. */
#define SECTOR_LOOKUP_PAGESETBITS 12

#define SECTOR_LOOKUP_SHIFT ((PTR_SIZE*8) - SECTOR_LOOKUP_PAGESETBITS)
#define LOG_SECTOR_LOOKUP_PAGESIZE ((PTR_SIZE*8) - SECTOR_LOOKUP_PAGESETBITS - LOG_SECTOR_SEGMENT_SIZE)
#define SECTOR_LOOKUP_PAGESIZE (1 << LOG_SECTOR_LOOKUP_PAGESIZE)
#define SECTOR_LOOKUP_PAGEMASK (SECTOR_LOOKUP_PAGESIZE - 1)

#define SECTOR_LOOKUP_PAGETABLE(x) (x >> SECTOR_LOOKUP_SHIFT)
#define SECTOR_LOOKUP_PAGEPOS(x) ((x >> LOG_SECTOR_SEGMENT_SIZE) & SECTOR_LOOKUP_PAGEMASK)

#define LOG_SECTOR_PAGEREC_SIZE (LOG_PTR_SIZE + 1)

/***************************************************************************/

/* Implementation Terminology:
    "sector" - A low-level block of memory. Given an arbitrary
               pointer value, whether it is contained in a sector and
	       the starting point of the sector can be determined in
	       constant time.
    "segment" - A portion of a sector, aligned on SECTOR_SEGMENT_SIZE 
               boundaries.
    "page" - part of a table for a (partial) mapping from addresses to 
               segments
    "block" or "common block" - A block for small memory allocations. Blocks
               provide allocation by partitioning a sector.
    "chunk" - A block of memory too large to be a common block. Each chunk
               is allocated in its own sector.
    "set" - A collection of blocks & chunks asscoaited with a particular
               name, "deallocation" function, trace function, etc.
*/

/***************************************************************************/

/* Debugging the collector: */
#define CHECK 0
#define PRINT 0
#define TIME 0
#define CHECK_COLLECTING 0
#define MARK_STATS 0
#define ALLOC_STATS 0
#define FINISH_STATS 0

#if DETAIL_MEMORY_TRACING
# undef STD_MEMORY_TRACING
# undef DUMP_BLOCK_MAPS
# undef KEEP_DETAIL_PATH
# define STD_MEMORY_TRACING 1
# define DUMP_BLOCK_MAPS 1
# define KEEP_DETAIL_PATH 1
#endif

#if STD_MEMORY_TRACING
# undef TERSE_MEMORY_TRACING
# undef DUMP_BLOCK_COUNTS
# define TERSE_MEMORY_TRACING 1
# define DUMP_BLOCK_COUNTS 1
#endif

#if DUMP_BLOCK_MAPS
# undef DUMP_BLOCK_COUNTS
# define DUMP_BLOCK_COUNTS 1
#endif

#if PROVIDE_GC_FREE
# undef DISTINGUISH_FREE_FROM_UNMARKED
# define DISTINGUISH_FREE_FROM_UNMARKED 1
# undef PROVIDE_CHUNK_GC_FREE
# define PROVIDE_CHUNK_GC_FREE 1
#endif

#if PROVIDE_CHUNK_GC_FREE
# define KEEP_PREV_PTR 1
#else
# define KEEP_PREV_PTR 0
#endif

#ifndef NULL
# define NULL 0L
#endif

# define PAD_FORWARD(p) (p)
# define PAD_BACKWARD(p) (p)


void (*GC_out_of_memory)(void);

/************************************************************/

typedef short Type_Tag;

Type_Tag weak_box_tag;

#define gc_finalization_tag 256
#define gc_finalization_weak_link_tag 257
#define gc_weak_array_tag 258

#define _num_tags_ 259

Size_Proc size_table[_num_tags_];
Mark_Proc mark_table[_num_tags_];
Fixup_Proc fixup_table[_num_tags_];

unsigned long (*GC_get_thread_stack_base)(void);

static void *park[2];

void **GC_variable_stack;

void GC_register_traversers(Type_Tag tag, Size_Proc size, Mark_Proc mark, Fixup_Proc fixup)
{
  size_table[tag] = size;
  mark_table[tag] = mark;
  fixup_table[tag] = fixup;
}

void *GC_resolve(void *p)
{
  return p;
}

void GC_init_type_tags(int count, int weakbox)
{
  weak_box_tag = weakbox;
}

/************************************************************/

typedef struct ImmobileBox {
  void *p;
  struct ImmobileBox *next, *prev;
} ImmobileBox;

static ImmobileBox *immobile;

void *GC_malloc_immobile_box(void *p)
{
  ImmobileBox *ib;

  ib = (ImmobileBox *)malloc(sizeof(ImmobileBox));
  ib->p = p;
  ib->next = immobile;
  if (immobile)
    immobile->prev = ib;
  ib->prev = NULL;

  immobile = ib;

  return ib;
}

void GC_free_immobile_box(void *b)
{
  ImmobileBox *ib = (ImmobileBox *)b;

  if (!ib)
    return;

  if (ib->prev)
    ib->prev->next = ib->next;
  else
    immobile = ib->next;
  if (ib->next)
    ib->next->prev = ib->prev;

  free(ib);
}

/************************************************************/

typedef struct GC_Weak_Array {
  Type_Tag type;
  short keyex;
  long count;
  void *replace_val;
  struct GC_Weak_Array *next;
  void *data[1];
} GC_Weak_Array;

static GC_Weak_Array *weak_arrays;

static int size_weak_array(void *p)
{
  GC_Weak_Array *a = (GC_Weak_Array *)p;

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Array) 
			  + ((a->count - 1) * sizeof(void *)));
}

static int mark_weak_array(void *p)
{
  GC_Weak_Array *a = (GC_Weak_Array *)p;

  gcMARK(a->replace_val);

  a->next = weak_arrays;
  weak_arrays = a;

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Array) 
			  + ((a->count - 1) * sizeof(void *)));
}

static int fixup_weak_array(void *p)
{
  /* Not used */
  return 0;
}

void *GC_malloc_weak_array(size_t size_in_bytes, void *replace_val)
{
  GC_Weak_Array *w;

  /* Allcation might trigger GC, so we use park: */
  park[0] = replace_val;

  w = (GC_Weak_Array *)GC_malloc_one_tagged(size_in_bytes 
					    + sizeof(GC_Weak_Array) 
					    - sizeof(void *));

  replace_val = park[0];
  park[0] = NULL;

  w->type = gc_weak_array_tag;
  w->replace_val = replace_val;
  w->count = (size_in_bytes >> 2);
  
  return &(w->data[0]);
}

typedef struct GC_Weak_Box {
  /* The first three fields are mandated by the GC spec: */
  Type_Tag type;
  short keyex;
  void *val;
  /* The rest is up to us: */
  void **secondary_erase;
  struct GC_Weak_Box *next;
} GC_Weak_Box;

static GC_Weak_Box *weak_boxes;

static int size_weak_box(void *p)
{
  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Box));
}

static int mark_weak_box(void *p)
{
  GC_Weak_Box *wb = (GC_Weak_Box *)p;
    
  gcMARK(wb->secondary_erase);

  if (wb->val) {
    wb->next = weak_boxes;
    weak_boxes = wb;
  }

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Box));
}

static int fixup_weak_box(void *p)
{
  /* Not used */
  return 0;
}

void *GC_malloc_weak_box(void *p, void **secondary)
{
  GC_Weak_Box *w;

  /* Allcation might trigger GC, so we use park: */
  park[0] = p;
  park[1] = secondary;

  w = (GC_Weak_Box *)GC_malloc_one_tagged(sizeof(GC_Weak_Box));

  p = park[0];
  park[0] = NULL;
  secondary = (void **)park[1];
  park[1] = NULL;
  
  w->type = weak_box_tag;
  w->val = p;
  w->secondary_erase = secondary;

  return w;
}

void *GC_weak_box_val(void *wb)
{
  return ((GC_Weak_Box *)wb)->val;
}

void GC_set_weak_box_val(void *wb, void *v)
{
  ((GC_Weak_Box *)wb)->val = v;
}

/******************************************************************************/

typedef struct Fnl {
  Type_Tag type;
  short eager_level;
  void *p;
  void (*f)(void *p, void *data);
  void *data;
  struct Fnl *next;
} Fnl;

static Fnl *fnls, *run_queue, *last_in_queue;

static int size_finalizer(void *p)
{
  return gcBYTES_TO_WORDS(sizeof(Fnl));
}

static int mark_finalizer(void *p)
{
  Fnl *fnl = (Fnl *)p;
    
  gcMARK(fnl->next);
  gcMARK(fnl->data);
  /* !eager_level => queued for run: */
  if (!fnl->eager_level) {
    gcMARK(fnl->p);
  }

  return gcBYTES_TO_WORDS(sizeof(Fnl));
}

static int fixup_finalizer(void *p)
{
  /* Not used. */
  return 0;
}

void GC_register_finalizer(void *p, void (*f)(void *p, void *data), 
			   void *data, void (**oldf)(void *p, void *data), 
			   void **olddata)
{
  GC_register_eager_finalizer(p, 3, f, data, oldf, olddata);
}

static int is_marked(void *d);

void GC_register_eager_finalizer(void *p, int level, void (*f)(void *p, void *data), 
				 void *data, void (**oldf)(void *p, void *data), 
				 void **olddata)
{
  Fnl *fnl, *prev;

  {
    if (!is_marked(p)) {
      /* Never collected. Don't finalize it. */
      if (oldf) *oldf = NULL;
      if (olddata) *olddata = NULL;
      return;
    }
  }

  fnl = fnls;
  prev = NULL;
  while (fnl) {
    if (fnl->p == p) {
      if (oldf) *oldf = fnl->f;
      if (olddata) *olddata = fnl->data;
      if (f) {
	fnl->f = f;
	fnl->data = data;
	fnl->eager_level = level;
      } else {
	if (prev)
	  prev->next = fnl->next;
	else
	  fnls = fnl->next;
	return;
      }
      return;
    } else {
      prev = fnl;
      fnl = fnl->next;
    }
  }
  
  if (oldf) *oldf = NULL;
  if (olddata) *olddata = NULL;

  if (!f)
    return;

  /* Allcation might trigger GC, so we use park: */
  park[0] = p;
  park[1] = data;

  fnl = GC_malloc_one_tagged(sizeof(Fnl));

  p = park[0];
  park[0] = NULL;
  data = park[1];
  park[1] = NULL;

  fnl->type = gc_finalization_tag;
  fnl->next = fnls;
  fnl->p = p;
  fnl->f = f;
  fnl->data = data;
  fnl->eager_level = level;

  fnls = fnl;
}

typedef struct Fnl_Weak_Link {
  Type_Tag type;
  void *p;
  void *saved;
  struct Fnl_Weak_Link *next;
} Fnl_Weak_Link;

static Fnl_Weak_Link *fnl_weaks;

static int size_finalizer_weak_link(void *p)
{
  return gcBYTES_TO_WORDS(sizeof(Fnl_Weak_Link));
}

static int mark_finalizer_weak_link(void *p)
{
  Fnl_Weak_Link *wl = (Fnl_Weak_Link *)p;
  
  gcMARK(wl->next);

  return gcBYTES_TO_WORDS(sizeof(Fnl_Weak_Link));
}

static int fixup_finalizer_weak_link(void *p)
{
  /* Not used. */
  return 0;
}

void GC_finalization_weak_ptr(void **p)
{
  Fnl_Weak_Link *wl;

  /* Allcation might trigger GC, so we use park: */
  park[0] = p;

  wl = (Fnl_Weak_Link *)GC_malloc_one_tagged(sizeof(Fnl_Weak_Link));

  p = park[0];
  park[0] = NULL;

  wl->type = gc_finalization_weak_link_tag;
  wl->p = p;
  wl->next = fnl_weaks;

  fnl_weaks = wl;
}

/******************************************************************************/

/* Sector types: */
enum {
  sector_kind_block,
  sector_kind_free,
#if !RELEASE_UNUSED_SECTORS
  sector_kind_freed,
#else
# define sector_kind_freed sector_kind_free
#endif
  sector_kind_chunk,
  sector_kind_managed,
  sector_kind_other
};

#define mtype_ATOMIC 0
#define mtype_TAGGED 1
#define mtype_ARRAY 2
#define mtype_TAGGED_ARRAY 3

typedef struct MemoryBlock {
  struct Finalizer *finalizers;
  unsigned long start;
  unsigned long end;
  unsigned long top;
  short size;
  short mtype;
  short elem_per_block;
  short free_search_start, free_search_bit, free_search_offset;
#if KEEP_SET_NO
  short set_no;
#endif
  int *positions; /* maps displacement in ptrs => position in objects */
  struct MemoryBlock *next;
  unsigned char free[1];
} MemoryBlock;

#if DISTINGUISH_FREE_FROM_UNMARKED

# define FREE_BIT_PER_ELEM 4
# define LOG_FREE_BIT_PER_ELEM 2
# define FREE_BIT_SIZE (8 >> LOG_FREE_BIT_PER_ELEM)
# define FREE_BIT_START 0x2 
# define UNMARK_BIT_START 0x1

# define POS_TO_FREE_INDEX(p) (p >> LOG_FREE_BIT_PER_ELEM)
# define POS_TO_UNMARK_INDEX(p) (p >> LOG_FREE_BIT_PER_ELEM)
# define POS_TO_FREE_BIT(p) (FREE_BIT_START << ((p & (FREE_BIT_PER_ELEM - 1)) << 1))
# define POS_TO_UNMARK_BIT(p) (UNMARK_BIT_START << ((p & (FREE_BIT_PER_ELEM - 1)) << 1))

# define ALL_UNMARKED 0x55
# define ALL_FREE 0xAA

# define _NOT_FREE(x) NOT_FREE(x)

# define SHIFT_UNMARK_TO_FREE(x) ((x & ALL_UNMARKED) << 1)
# define SHIFT_COPY_FREE_TO_UNMARKED(x) ((x & ALL_FREE) | ((x & ALL_FREE) >> 1))

#else /* !DISTINGUISH_FREE_FROM_UNMARKED */

# define FREE_BIT_PER_ELEM 8
# define LOG_FREE_BIT_PER_ELEM 3
# define FREE_BIT_SIZE (8 >> LOG_FREE_BIT_PER_ELEM)
# define FREE_BIT_START 0x1
# define UNMARK_BIT_START 0x1

# define POS_TO_FREE_INDEX(p) (p >> LOG_FREE_BIT_PER_ELEM)
# define POS_TO_UNMARK_INDEX(p) (p >> LOG_FREE_BIT_PER_ELEM)
# define POS_TO_FREE_BIT(p) (FREE_BIT_START << (p & (FREE_BIT_PER_ELEM - 1)))
# define POS_TO_UNMARK_BIT(p) (UNMARK_BIT_START << (p & (FREE_BIT_PER_ELEM - 1)))

# define ALL_UNMARKED 0xFF

# define _NOT_FREE(x) 1

#endif /* DISTINGUISH_FREE_FROM_UNMARKED */

#define NOT_FREE(x) (!(x))
#define IS_FREE(x) (x)
#define NOT_MARKED(x) (x)
#define IS_MARKED(x) (!(x))

#define ELEM_PER_BLOCK(b) b->elem_per_block

typedef struct MemoryChunk {
  struct Finalizer *finalizers;
  unsigned long start;
  unsigned long end;
  struct MemoryChunk *next;
#if KEEP_PREV_PTR
  struct MemoryChunk **prev_ptr;
#endif
  short mtype;
  short marked;
  char data[1];
} MemoryChunk;

/* If this changes size from 2 ptrs, change LOG_SECTOR_PAGEREC_SIZE */
typedef struct {
  long kind;  /* sector_kind_other, etc. */
  unsigned long start; /* Sector start; may not be accurate if the segment
                          is deallocated, but 0 => not in any sector */
} SectorPage;

static SectorPage **sector_pagetables;

#if !RELEASE_UNUSED_SECTORS
# include "../sgc/splay.c"

typedef struct SectorFreepage {
  long size; 
  unsigned long start; /* Sector start */
  unsigned long end; /* start of next */
  Tree by_start;
  Tree by_end;
  Tree by_start_per_size;
  Tree by_size;
  Tree *same_size;
} SectorFreepage;

static Tree *sector_freepage_by_start;
static Tree *sector_freepage_by_end;
static Tree *sector_freepage_by_size;

#define TREE_FP(t) ((SectorFreepage *)(t->data))

static Tree *next(Tree *node)
{
  node = node->right;
  if (node) {
    while (node->left) {
      node = node->left;
    }
    return node;
  } else
    return NULL;
}

static void remove_freepage(SectorFreepage *fp)
{
  /* Remove fp from freelists: */
  sector_freepage_by_start = delete(fp->start, sector_freepage_by_start);
  sector_freepage_by_end = delete(fp->end, sector_freepage_by_end);
  sector_freepage_by_size = splay(fp->size, sector_freepage_by_size);
  if (TREE_FP(sector_freepage_by_size) == fp) {
    /* This was the representative for its size; remove it. */
    sector_freepage_by_size = delete(fp->size, sector_freepage_by_size);
    if (fp->same_size) {
      SectorFreepage *same;
      same = TREE_FP(fp->same_size);
      same->same_size = delete(same->start, fp->same_size);
      sector_freepage_by_size = insert(same->size, &same->by_size, sector_freepage_by_size);
    }
  } else {
    /* Not the top-level representative; remove it from the representative's
       same_size tree */
    SectorFreepage *same;
    same = TREE_FP(sector_freepage_by_size);
    same->same_size = delete(fp->start, same->same_size);
  }
}

static void add_freepage(SectorFreepage *naya)
{
  naya->by_start.data = (void *)naya;
  sector_freepage_by_start = insert(naya->start, &naya->by_start, sector_freepage_by_start);
  naya->by_end.data = (void *)naya;
  sector_freepage_by_end = insert(naya->end, &naya->by_end, sector_freepage_by_end);
  naya->by_size.data = (void *)naya;
  sector_freepage_by_size = insert(naya->size, &naya->by_size, sector_freepage_by_size);
  if (TREE_FP(sector_freepage_by_size) != naya) {
    /* This size was already in the tree; add it to the next_size list, instead */
    SectorFreepage *already = TREE_FP(sector_freepage_by_size);
    naya->by_start_per_size.data = (void *)naya;
    already->same_size = insert(naya->start, &naya->by_start_per_size, already->same_size);
  } else
    naya->same_size = NULL;
}
#endif /* !RELEASE_UNUSED_SECTORS */

#define TABLE_HI_SHIFT LOG_SECTOR_SEGMENT_SIZE
#define TABLE_LO_MASK (SECTOR_SEGMENT_SIZE-1)
#define EACH_TABLE_COUNT (1 << (LOG_SECTOR_SEGMENT_SIZE - LOG_PTR_SIZE))

typedef struct GC_Set {
  short atomic, tagged, array;
  char *name;
  MemoryBlock **blocks;
  MemoryBlock **block_ends;
  MemoryChunk **othersptr;
#if DUMP_BLOCK_COUNTS
  unsigned long total;
#endif
#if ALLOW_SET_FINALIZER
  GC_set_elem_finalizer finalizer;
#endif
} GC_Set;

typedef struct GC_SetWithOthers {
  GC_Set c;
  MemoryChunk *others;
} GC_SetWithOthers;

static GC_Set **common_sets;
static int num_common_sets;

static MemoryBlock *tagged_common[2 * NUM_COMMON_SIZE]; /* second half is `ends' array */
static MemoryBlock *atomic_common[2 * NUM_COMMON_SIZE];
static MemoryBlock *array_common[2 * NUM_COMMON_SIZE];
static MemoryBlock *tagged_array_common[2 * NUM_COMMON_SIZE];
static MemoryChunk *tagged_others, *atomic_others, *array_others, *tagged_array_others;

static int *common_positionses[NUM_COMMON_SIZE];

#define do_malloc_ATOMIC 0x1

void (*GC_push_last_roots)(void);
void (*GC_push_last_roots_again)(void);

static unsigned long sector_low_plausible, sector_high_plausible;
static unsigned long low_plausible, high_plausible;

void *GC_stackbottom;

static long mem_use, mem_limit = FIRST_GC_LIMIT;
#if USE_GC_FREE_SPACE_DIVISOR
int GC_free_space_divisor = 4;
#endif

static long mem_real_use;

static long sector_mem_use, sector_admin_mem_use, sector_free_mem_use;
static long manage_mem_use, manage_real_mem_use;

static long collect_mem_use;

static long num_sector_allocs, num_sector_frees;

static long num_chunks;
static long num_blocks;

void (*GC_collect_start_callback)(void);
void (*GC_collect_end_callback)(void);
void (*GC_custom_finalize)(void);

static long roots_count;
static long roots_size;
static unsigned long *roots;

static long *size_index_map; /* (1/PTR_SIZE)th of requested size to alloc index */
static long *size_map; /* alloc index to alloc size */

#if CHECK_COLLECTING
static int collecting_now;
#endif

# define FPRINTF fprintf
# define STDERR stderr

#if CHECK_FREES
static void free_error(const char *msg)
{
  FPRINTF(STDERR, msg);
}
#endif

/*************************************************************/

/* 
   The kinds of allocation:
   
     malloc_sector = returns new SECTOR_SEGMENT_SIZE-aligned memory;
                     relies on nothing else; the memeory blocks must
		     be explicitly freed with free_sector; all GC
		     allocation is perfomed via sectors
     
     malloc_managed = malloc "atomic" block used by GC implementation
                      itself; no GCing should occur during the malloc;
		      the block is freed with free_managed

     realloc_collect_temp = temporary structures used during gc;
                            no other allocation can take place
			    during gc, and all memory will be freed
                            when GC is done with free_collect_temp
*/

#if GET_MEM_VIA_SBRK
static void *platform_plain_sector(int count)
{
  caddr_t cur_brk = (caddr_t)sbrk(0);
  long lsbs = (unsigned long)cur_brk & TABLE_LO_MASK;
  void *result;
    
  if (lsbs != 0) {
    if ((caddr_t)sbrk(SECTOR_SEGMENT_SIZE - lsbs) == (caddr_t)(-1)) 
      return 0;
  }

  result = (caddr_t)sbrk((count << LOG_SECTOR_SEGMENT_SIZE));

  if (result == (caddr_t)(-1)) 
    return 0;

  return result;
}
#endif
#if GET_MEM_VIA_MMAP
static void *platform_plain_sector(int count)
{
  static int fd;

  if (!fd) {
    fd = open("/dev/zero", O_RDONLY);
  }
  
  return mmap(0, count << LOG_SECTOR_SEGMENT_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
}

static void free_plain_sector(void *p, int count)
{
  munmap(p, count << LOG_SECTOR_SEGMENT_SIZE);
}
#endif
#if GET_MEM_VIA_VIRTUAL_ALLOC
static void *platform_plain_sector(int count)
{
  /* Since 64k blocks are used up by each call to VirtualAlloc,
     use roughly the same trick as in the malloc-based alloc to
     avoid wasting ther address space. */

  static int prealloced;
  static void *preallocptr;
  
  if (!prealloced && (count < SECTOR_SEGMENT_GROUP_SIZE)) {
    prealloced = SECTOR_SEGMENT_GROUP_SIZE;
    preallocptr = VirtualAlloc(NULL, prealloced << LOG_SECTOR_SEGMENT_SIZE,
			       MEM_COMMIT | MEM_RESERVE,
			       PAGE_READWRITE);		
  }
  
  if (count <= prealloced) {
    void *result = preallocptr;
    preallocptr = ((char *)preallocptr) + (count << LOG_SECTOR_SEGMENT_SIZE);
    prealloced -= count;
    return result;
  }
  
  return VirtualAlloc(NULL, count << LOG_SECTOR_SEGMENT_SIZE,
		      MEM_COMMIT | MEM_RESERVE,
		      PAGE_READWRITE);
}
#endif

#if !GET_MEM_VIA_SBRK && !GET_MEM_VIA_MMAP && !GET_MEM_VIA_VIRTUAL_ALLOC

static void *platform_plain_sector(int count)
{
  static int prealloced;
  static void *preallocptr;

  if (!prealloced) {
    unsigned long d;

    if (count <= (SECTOR_SEGMENT_GROUP_SIZE-1))
      prealloced = SECTOR_SEGMENT_GROUP_SIZE-1;
    else
      prealloced = count;

    preallocptr = malloc((prealloced + 1) << LOG_SECTOR_SEGMENT_SIZE);

    d = ((unsigned long)preallocptr) & TABLE_LO_MASK;
    if (d)
      preallocptr = ((char *)preallocptr) + (SECTOR_SEGMENT_SIZE - d);
  }

  if (prealloced >= count) {
    void *r = preallocptr;

    prealloced -= count;
    preallocptr = ((char *)preallocptr) + (count << LOG_SECTOR_SEGMENT_SIZE);
    
    return r;
  }

  {
    unsigned long d;
    void *r;

    r = malloc((count + 1) << LOG_SECTOR_SEGMENT_SIZE);

    d = ((unsigned long)r) & TABLE_LO_MASK;
    if (d)
      r = ((char *)r) + (SECTOR_SEGMENT_SIZE - d);

    return r;
  }
}
#endif

static void *malloc_plain_sector(int count)
{
  void *m;

  m = platform_plain_sector(count);

  if (!m) {
    if (GC_out_of_memory)
      GC_out_of_memory();
    FPRINTF(STDERR, "out of memory\n");	
    exit(-1);
  }

  return m;
}

static void register_sector(void *naya, int need, long kind)
{
  unsigned long ns, orig_ns;
  int pagetableindex, pageindex, i;
  SectorPage *pagetable;

  orig_ns = ns = PTR_TO_INT(naya);
  if (!sector_low_plausible || (ns < sector_low_plausible))
    sector_low_plausible = ns;
  if (!sector_high_plausible 
      || (ns + (need << LOG_SECTOR_SEGMENT_SIZE) > sector_high_plausible))
    sector_high_plausible = ns + (need << LOG_SECTOR_SEGMENT_SIZE);

  /* Register pages as existing: */
  for (i = need; i--; ns += SECTOR_SEGMENT_SIZE) {
    pagetableindex = SECTOR_LOOKUP_PAGETABLE(ns);
    pagetable = sector_pagetables[pagetableindex];
    if (!pagetable) {
      int c = (LOG_SECTOR_LOOKUP_PAGESIZE + LOG_SECTOR_PAGEREC_SIZE) - LOG_SECTOR_SEGMENT_SIZE;
      int j;
      
      if (c < 0)
	c = 0;
      c = 1 << c;
      pagetable = (SectorPage *)malloc_plain_sector(c);
      sector_pagetables[pagetableindex] = pagetable;
      sector_admin_mem_use += (c << LOG_SECTOR_SEGMENT_SIZE);
      for (j = 0; j < SECTOR_LOOKUP_PAGESIZE; j++) {
	pagetable[j].start = 0; 
	pagetable[j].kind = sector_kind_free;
      }
    }

    pageindex = SECTOR_LOOKUP_PAGEPOS(ns);
    pagetable[pageindex].kind = kind;
    pagetable[pageindex].start = orig_ns;
  }
}

static void *malloc_sector(long size, long kind, int no_new)
{
  long need, i;
  void *naya;
#if !RELEASE_UNUSED_SECTORS
  SectorFreepage *fp;
#endif

#if CHECK_COLLECTING
  if (collecting_now) {
    free_error("alloc while collecting\n");
    return NULL;
  }
#endif

  num_sector_allocs++;

  if (!sector_pagetables) {
    int c = (SECTOR_LOOKUP_PAGESETBITS + LOG_PTR_SIZE) - LOG_SECTOR_SEGMENT_SIZE;
    if (c < 0)
      c = 0;
    c = 1 << c;
    sector_pagetables = (SectorPage **)malloc_plain_sector(c);
    sector_admin_mem_use += (c << LOG_SECTOR_SEGMENT_SIZE);
    for (i = 0; i < (1 << SECTOR_LOOKUP_PAGESETBITS); i++)
      sector_pagetables[i] = NULL;
  }

  need = (size + SECTOR_SEGMENT_SIZE - 1) >> LOG_SECTOR_SEGMENT_SIZE;

#if !RELEASE_UNUSED_SECTORS
  if (sector_freepage_by_size) {
    sector_freepage_by_size = splay(need, sector_freepage_by_size);
    if (TREE_FP(sector_freepage_by_size)->size < need) {
      /* No exact match, so find the next size up */
      Tree *node;
      node = next(sector_freepage_by_size);
      if (node)
	fp = TREE_FP(node);
      else
	fp = NULL;
    } else 
      fp = TREE_FP(sector_freepage_by_size);
  } else
    fp = NULL;
  
  if (fp) {
    remove_freepage(fp);

    naya = INT_TO_PTR(fp->start);
    register_sector(naya, need, kind);
    if (fp->size > need) {
      /* Move freepage info and shrink */
      SectorFreepage *naya;
      unsigned long nfp;
      nfp = fp->start + (need << LOG_SECTOR_SEGMENT_SIZE);
      naya = (SectorFreepage *)INT_TO_PTR(nfp);
      naya->size = fp->size - need;
      naya->start = nfp;
      naya->end = fp->end;

      add_freepage(naya);
    }

    sector_free_mem_use -= (need << LOG_SECTOR_SEGMENT_SIZE);
    return naya;
  }
#endif

  if (no_new)
    return NULL;

  naya = malloc_plain_sector(need);
  sector_mem_use += (need << LOG_SECTOR_SEGMENT_SIZE);
  register_sector(naya, need, kind);

  return naya;
}

static void free_sector(void *p)
{
  unsigned long s = PTR_TO_INT(p), t;
  int c = 0;
#if !RELEASE_UNUSED_SECTORS
  SectorFreepage *fp, *ifp;
#endif

  num_sector_frees++;
  
  /* Determine the size: */
  t = s;
  while(1) {
    long pagetableindex = SECTOR_LOOKUP_PAGETABLE(t);
    long pageindex = SECTOR_LOOKUP_PAGEPOS(t);
    if (sector_pagetables[pagetableindex]
	&& (sector_pagetables[pagetableindex][pageindex].start == s)) {
      sector_pagetables[pagetableindex][pageindex].kind = sector_kind_freed;
      sector_pagetables[pagetableindex][pageindex].start = 0;
      c++;
      t += SECTOR_SEGMENT_SIZE;
    } else
      break;
  }

#if CHECK_FREES
  if (!c) {
    free_error("bad sector free!\n");
    return;
  }
#endif

#if RELEASE_UNUSED_SECTORS
  free_plain_sector(p, c);
  sector_mem_use -= (c << LOG_SECTOR_SEGMENT_SIZE);
#else
  sector_free_mem_use += (c << LOG_SECTOR_SEGMENT_SIZE);
  if (sector_freepage_by_end) {
    /* Try merge with a predecessor: */
    sector_freepage_by_end = splay(s, sector_freepage_by_end);
    ifp = TREE_FP(sector_freepage_by_end);
    if (ifp->end == s) {
      remove_freepage(ifp);
      c += ifp->size;
      s = ifp->start;
    }
    
    if (sector_freepage_by_start) {
      /* Try merge with a successor: */
      sector_freepage_by_start = splay(t, sector_freepage_by_start);
      ifp = TREE_FP(sector_freepage_by_start);
      if (ifp->start == t) {
	remove_freepage(ifp);
	c += ifp->size;
	t = ifp->end;
      }
    }
  }
  
  fp = (SectorFreepage *)p;
  fp->start = s;
  fp->end = t;
  fp->size = c;
  add_freepage(fp);
#endif
}

#ifdef WIN32
static int is_sector_segment(void *p)
{
  unsigned long s = PTR_TO_INT(p);
  long pagetableindex = SECTOR_LOOKUP_PAGETABLE(s);
  long pageindex = SECTOR_LOOKUP_PAGEPOS(s);

  return (sector_pagetables[pagetableindex]
          && sector_pagetables[pagetableindex][pageindex].start);
}
#endif

#if GET_MEM_VIA_SBRK
static int c_refcount;
static char *save_brk;
#endif

static void prepare_collect_temp()
{
#if GET_MEM_VIA_SBRK
  save_brk = (char *)sbrk(0);
#else
  collect_mem_use = 0;
#endif
}

static void *realloc_collect_temp(void *v, long oldsize, long newsize)
{
#if GET_MEM_VIA_SBRK
  void *naya;

  naya = (void *)sbrk(newsize);
  memcpy(naya, v, oldsize);
  if (!v)
    c_refcount++;
  return naya;
#elif GET_MEM_VIA_MMAP
  void *naya;
  
  naya = platform_plain_sector((newsize + SECTOR_SEGMENT_SIZE - 1) >> LOG_SECTOR_SEGMENT_SIZE);
  memcpy(naya, v, oldsize);
  if (v)
    munmap(v, (oldsize + SECTOR_SEGMENT_SIZE - 1) >> LOG_SECTOR_SEGMENT_SIZE);

  return naya;
#elif GET_MEM_VIA_VIRTUAL_ALLOC
  void *naya;

  naya = VirtualAlloc(NULL, newsize, 
		      MEM_COMMIT | MEM_RESERVE,
		      PAGE_READWRITE);
  memcpy(naya, v, oldsize);
  if (v)
    VirtualFree(v, 0, MEM_RELEASE);

  return naya;
#else
  void *naya;

  naya = malloc(newsize);
  memcpy(naya, v, oldsize);
  free(v);
  collect_mem_use += newsize;
  return naya;
#endif
}

static void free_collect_temp(void *v, long oldsize)
{
#if GET_MEM_VIA_SBRK
  if (!(--c_refcount)) {
    collect_mem_use = (unsigned long)(sbrk(0)) - (unsigned long)save_brk;
    brk(save_brk);
  }
#elif GET_MEM_VIA_MMAP
  munmap(v, (oldsize + SECTOR_SEGMENT_SIZE - 1) >> LOG_SECTOR_SEGMENT_SIZE);
#elif GET_MEM_VIA_VIRTUAL_ALLOC
  VirtualFree(v, 0, MEM_RELEASE);
#else
  free(v);
#endif
}

typedef struct {
  struct ManagedBlock *next;
  struct ManagedBlock *prev;
  long count;
  long size; /* Use size to find bucket */
  unsigned long end;
} ManagedBlockHeader;

typedef struct ManagedBlock {
  ManagedBlockHeader head;
  char free[1];
} ManagedBlock;

typedef struct {
  long size;
  long perblock;
  long offset;
  ManagedBlock *block;
} ManagedBucket;

typedef struct {
  int num_buckets;
  ManagedBucket buckets[1];
} Managed;

static Managed *managed;

static void *malloc_managed(long size)
{
  /* A naive strategy is sufficient here.
     There will be many disappearing links, many
     finalizations, and very little of anything else. */
  int i, j;
  long perblock, offset;
  ManagedBlock *mb;
  
  if (size & PTR_SIZE)
    size += PTR_SIZE - (size & PTR_SIZE);

  if (!managed) {
    managed = (Managed *)malloc_sector(SECTOR_SEGMENT_SIZE, sector_kind_other, 0);
    managed->num_buckets = 0;
    manage_real_mem_use += SECTOR_SEGMENT_SIZE;
  }

  for (i = 0; i < managed->num_buckets; i++) {
    if (managed->buckets[i].size == size)
      break;
  }

  if (i >= managed->num_buckets) {
    managed->num_buckets++;
    managed->buckets[i].size = size;
    if (size < MAX_COMMON_SIZE) {
      int c;

      mb = (ManagedBlock *)malloc_sector(SECTOR_SEGMENT_SIZE, sector_kind_managed, 0);
      manage_real_mem_use += SECTOR_SEGMENT_SIZE;
      managed->buckets[i].block = mb;

      c = (SECTOR_SEGMENT_SIZE - sizeof(ManagedBlockHeader)) / size;
      if (c & (PTR_SIZE - 1))
	c += (PTR_SIZE - (c & (PTR_SIZE - 1)));
      managed->buckets[i].perblock = (SECTOR_SEGMENT_SIZE - sizeof(ManagedBlockHeader) - c) / size;
      managed->buckets[i].offset = c + sizeof(ManagedBlockHeader);
    } else {
      long l = size + sizeof(ManagedBlockHeader) + PTR_SIZE;
      mb = (ManagedBlock *)malloc_sector(l, sector_kind_managed, 0);
      manage_real_mem_use += l;
      managed->buckets[i].block = mb;
      managed->buckets[i].perblock = 1;
      managed->buckets[i].offset = sizeof(ManagedBlockHeader) + PTR_SIZE;
    }
    mb->head.count = 0;
    mb->head.size = size;
    mb->head.next = NULL;
    mb->head.prev = NULL;
    perblock = managed->buckets[i].perblock;
    for (j = perblock; j--; )
      mb->free[j] = 1;
    mb->head.end = PTR_TO_INT(mb) + managed->buckets[i].offset + size * perblock;
  }

  perblock = managed->buckets[i].perblock;
  offset = managed->buckets[i].offset;
  mb = managed->buckets[i].block;
  while ((mb->head.count == perblock) && mb->head.next)
    mb = mb->head.next;
  if (mb->head.count == perblock) {
    long l = offset + size * perblock;
    mb->head.next = (ManagedBlock *)malloc_sector(l, sector_kind_managed, 0);
    manage_real_mem_use += l;
    mb->head.next->head.prev = mb;
    mb = mb->head.next;
    mb->head.count = 0;
    mb->head.size = size;
    mb->head.next = NULL;
    for (j = perblock; j--; )
      mb->free[j] = 1;
    mb->head.end = PTR_TO_INT(mb) + offset + size * perblock;
  }

  manage_mem_use += size;

  mb->head.count++;
  for (j = perblock; j--; )
    if (mb->free[j]) {
      mb->free[j] = 0;
      return (((char *)mb) + offset) + size * j;
    }

  FPRINTF(STDERR, "error allocating managed\n");
  return NULL;
}

void free_managed(void *s)
{
  int i;
  unsigned long p;
  ManagedBucket *bucket;
  ManagedBlock *mb;

  p = PTR_TO_INT(s);

  /* Assume that s really is an allocated managed pointer: */
  mb = (ManagedBlock *)INT_TO_PTR((p & SECTOR_SEGMENT_MASK));
  
  for (i = 0; i < managed->num_buckets; i++) {
    bucket = managed->buckets + i;
    if (bucket->size == mb->head.size) {
      /* Found bucket */
      int which;
      which = (p - PTR_TO_INT(mb) - bucket->offset) / bucket->size;
      if ((which >= 0) && (which < bucket->perblock)) {
	if (mb->free[which]) {
	  FPRINTF(STDERR, "error freeing managed\n");
	  return;
	}
	mb->free[which] = 1;
	--mb->head.count;
	manage_mem_use -= bucket->size;
	if (!mb->head.count) {
	  if (mb->head.prev) {
	    if (mb->head.next)
	      mb->head.next->head.prev = mb->head.prev;
	    mb->head.prev->head.next = mb->head.next;
	  } else {
	    if (mb->head.next) {
	      bucket->block = mb->head.next;
	      bucket->block->head.prev = NULL;
	    } else {
	      /* Empty bucket */
	      int j;
	      --managed->num_buckets;
	      for (j = i; j < managed->num_buckets; j++)
		memcpy(&(managed->buckets[j]), &(managed->buckets[j + 1]), sizeof(ManagedBucket));
	    }
	  }

	  manage_real_mem_use -= (bucket->offset + bucket->size * bucket->perblock);

	  free_sector(mb);
	}
	return;
      }
    }
  }
  
  FPRINTF(STDERR, "error freeing managed\n");
}

/*************************************************************/

static void init_size_map()
{
  int i, j, find_half;
  long k, next;

  size_index_map = (long *)malloc_sector(MAX_COMMON_SIZE, sector_kind_other, 0);
  size_map = (long *)malloc_sector(NUM_COMMON_SIZE * sizeof(long), sector_kind_other, 0);

  i = 0;
  while (i < 8) {
    size_index_map[i] = i;
    size_map[i] = (i + 1) * PTR_SIZE;
    i++;
  }

  k = 8;
  next = 12;
  j = i;
  find_half = 1;
  while (j < (MAX_COMMON_SIZE >> 2)) {
    size_index_map[j] = i;
    if ((j + 1) == next) {
      size_map[i] = next * PTR_SIZE;
      i++;
      if (find_half) {
	next = 2 * k;
      } else {
	next = 3 * k;
	k = 2 * k;
      }
      find_half = !find_half;
    }
    j++;
  }
  if (i < NUM_COMMON_SIZE)
    size_map[i] = next * PTR_SIZE;

#if 0
  FPRINTF(STDERR, "max: %d  num: %d\n", MAX_COMMON_SIZE, NUM_COMMON_SIZE);
  for (i = 0; i < (MAX_COMMON_SIZE >> 2); i++) {
    FPRINTF(STDERR, "%d->%d=%d;", i, 
	    size_index_map[i], 
	    size_map[size_index_map[i]]);
  }
  FPRINTF(STDERR, "\n");
#endif
}

/*************************************************************/

void GC_add_roots(void *start, void *end)
{
  if (roots_count >= roots_size) {
    unsigned long *naya;

    mem_real_use -= (sizeof(unsigned long) * roots_size);

    roots_size = roots_size ? 2 * roots_size : 500;
    naya = (unsigned long *)malloc_managed(sizeof(unsigned long) * (roots_size + 1));

    mem_real_use += (sizeof(unsigned long) * roots_size);

    memcpy((void *)naya, (void *)roots, 
	   sizeof(unsigned long) * roots_count);

    if (roots)
      free_managed(roots);

    roots = naya;
  }

  roots[roots_count++] = PTR_TO_INT(start);
  roots[roots_count++] = PTR_TO_INT(end) - PTR_ALIGNMENT;
}

static int initialized = 0;

void GC_initialize(void)
{
  int i;

  num_common_sets = 4;
  common_sets = (GC_Set **)malloc_managed(sizeof(GC_Set*) * num_common_sets);

  common_sets[0] = (GC_Set *)malloc_managed(sizeof(GC_Set));
  common_sets[0]->atomic = 0;
  common_sets[0]->tagged = 1;
  common_sets[0]->array = 0;
  common_sets[0]->blocks = tagged_common;
  common_sets[0]->block_ends = tagged_common + NUM_COMMON_SIZE;
  common_sets[0]->othersptr = &tagged_others;

  common_sets[1] = (GC_Set *)malloc_managed(sizeof(GC_Set));
  common_sets[1]->atomic = 1;
  common_sets[1]->tagged = 0;
  common_sets[1]->array = 0;
  common_sets[1]->blocks = atomic_common;
  common_sets[1]->block_ends = atomic_common + NUM_COMMON_SIZE;
  common_sets[1]->othersptr = &atomic_others;

  common_sets[2] = (GC_Set *)malloc_managed(sizeof(GC_Set));
  common_sets[2]->atomic = 0;
  common_sets[2]->tagged = 0;
  common_sets[2]->array = 1;
  common_sets[2]->blocks = array_common;
  common_sets[2]->block_ends = array_common + NUM_COMMON_SIZE;
  common_sets[2]->othersptr = &array_others;

  common_sets[3] = (GC_Set *)malloc_managed(sizeof(GC_Set));
  common_sets[3]->atomic = 0;
  common_sets[3]->tagged = 1;
  common_sets[3]->array = 1;
  common_sets[3]->blocks = tagged_array_common;
  common_sets[3]->block_ends = tagged_array_common + NUM_COMMON_SIZE;
  common_sets[3]->othersptr = &tagged_array_others;

  for (i = 0; i < num_common_sets; i++) {
    common_sets[i]->name = "Basic";
  }

  initialized = 1;
}

void GC_set_stack_base(void *base)
{
  GC_stackbottom = base;
}

unsigned long GC_get_stack_base(void)
{
  return (unsigned long)GC_stackbottom;
}

void *find_ptr(void *d, int *_size,
	       MemoryBlock **_block, int *_pos,
	       MemoryChunk **_chunk,
	       int find_anyway)
{
  unsigned long p = PTR_TO_INT(d);

  if (!sector_pagetables)
    return NULL;

  if (p >= low_plausible && p < high_plausible) {
    SectorPage *pagetable = sector_pagetables[SECTOR_LOOKUP_PAGETABLE(p)];
    if (pagetable) {
      SectorPage *page = pagetable + SECTOR_LOOKUP_PAGEPOS(p);
      long kind = page->kind;

      if (kind == sector_kind_block) {
	/* Found common block: */
	MemoryBlock *block = (MemoryBlock *)INT_TO_PTR(page->start);
	if (p >= block->start && p < block->top) {
	  int size = block->size;
	  int diff = p - block->start;
	  int pos = (diff / size), apos;
	  int bit;
	  unsigned long result;
	  
	  apos = POS_TO_UNMARK_INDEX(pos);
	  bit = POS_TO_UNMARK_BIT(pos);
	  
	  if (_size)
	    *_size = size;
	  
	  if (NOT_MARKED(block->free[apos] & bit) && !find_anyway)
	    return NULL;
	  
	  result = block->start + (pos * size);
	  
	  if (_block)
	    *_block = block;
	  if (_pos)
	    *_pos = pos;
	  
	  return INT_TO_PTR(result);
	}
      } else if (kind == sector_kind_chunk) {
	MemoryChunk *c = (MemoryChunk *)INT_TO_PTR(page->start);
	if ((p >= c->start) && (p < c->end)) {
	  if (_size)
	    *_size = (c->end - c->start);
	  if (c->marked || find_anyway) {
	    if (_chunk)
	      *_chunk = c;
	    return INT_TO_PTR(c->start);
	  } else
	    return NULL;
	}
      }
    }
  }

  return NULL;
}

static int is_marked(void *d)
{
  return (int)find_ptr(d, NULL, NULL, NULL, NULL, 0);
}

#if DUMP_BLOCK_MAPS
static unsigned long trace_stack_start, trace_stack_end, trace_reg_start, trace_reg_end;
#endif

#if DUMP_SECTOR_MAP
static void dump_sector_map(char *prefix)
{
  FPRINTF(STDERR, "%sBegin Sectors\n"
	  "%sO0:free; ,.:block; =-:chunk; mn:other; \"':other; %d each\n%s",
	  prefix, prefix, SECTOR_SEGMENT_SIZE, prefix);
  {
    int i, j;
    int c = 0;
    unsigned long was_sec = 0;
    int was_kind = 0;

    for (i = 0; i < (1 << SECTOR_LOOKUP_PAGESETBITS); i++) {
      SectorPage *pagetable;
      pagetable = sector_pagetables[i];
      if (pagetable) {
	for (j = 0; j < SECTOR_LOOKUP_PAGESIZE; j++) {
	  long kind;
	  kind = pagetable[j].kind;
	  if (kind != sector_kind_free) {
	    char *same_sec, *diff_sec;

	    if (c++ > 40) {
	      FPRINTF(STDERR, "\n%s", prefix);
	      c = 1;
	    }

	    switch(kind) {
#if !RELEASE_UNUSED_SECTORS
	    case sector_kind_freed:
	      same_sec = "0";
	      diff_sec = "O";
	      break;
#endif
	    case sector_kind_block:
	      same_sec = ".";
	      diff_sec = ",";
	      break;
	    case sector_kind_chunk:
	      same_sec = "-";
	      diff_sec = "=";
	      break;
	    case sector_kind_managed:
	      same_sec = "n";
	      diff_sec = "m";
	      break;
	    case sector_kind_other:
	      same_sec = "'";
	      diff_sec = "\"";
	      break;
	    default:
	      same_sec = "?";
	      diff_sec = "?";
	      break;
	    }

	    if ((was_kind != kind) || (was_sec != pagetable[j].start))
	      same_sec = diff_sec;

	    FPRINTF(STDERR, same_sec);
	    
	    was_kind = kind;
	    was_sec = pagetable[j].start;
	  }
	}
      }
    }
  }
  FPRINTF(STDERR, "\n%sEnd Sectors\n", prefix);
}
#endif

void GC_dump(void)
{
  FPRINTF(STDERR, "Begin Map\n");

  FPRINTF(STDERR,
	  "allocated: %ld\n"
	  "including known overhead: %ld  scheduled gc: %ld  last collect depth: %ld\n"
	  "managed: %ld  managed including overhead: %ld\n"
	  "sector used: %ld  sector free: %ld  sector total: %ld\n"
	  "sector range: %ld  sector administration: %ld\n"
	  "num sector allocs: %ld  num sector frees: %ld\n"
	  , mem_use,
	  mem_real_use, mem_limit, collect_mem_use,
	  manage_mem_use, manage_real_mem_use,
	  sector_mem_use - sector_free_mem_use, sector_free_mem_use, sector_mem_use,
	  sector_high_plausible - sector_low_plausible,
	  sector_admin_mem_use,
	  num_sector_allocs, num_sector_frees
	  );

#if DUMP_SECTOR_MAP
  dump_sector_map("");
#endif

#if DUMP_BLOCK_COUNTS
  {
    int i, j;
    unsigned long total;
    
#if DUMP_BLOCK_MAPS
    FPRINTF(STDERR, "roots: ======================================\n");
    for (i = 0; i < roots_count; i += 2)
      FPRINTF(STDERR, ">%lx-%lx", roots[i], roots[i + 1]);
    FPRINTF(STDERR, "\n");

    FPRINTF(STDERR, "stack: ======================================\n");
    FPRINTF(STDERR, ">%lx-%lx>%lx-%lx\n",
	    trace_stack_start, trace_stack_end, trace_reg_start, trace_reg_end);
#endif

    for (j = 0; j < num_common_sets; j++) {
      GC_Set *cs = common_sets[j];

      total = 0;

      FPRINTF(STDERR,
	      "Set: %s [%s/%s/%s]: ======================================\n", 
	      cs->name,
	      cs->tagged ? "tagged" : "plain",
	      cs->atomic ? "atomic" : "pointerful",
	      cs->array ? "array" : "item");

      for (i = 0; i < NUM_COMMON_SIZE; i++) {
	MemoryBlock *block;
	int counter = 0;

	block = (cs)->blocks[i];

	if (block) {
	  FPRINTF(STDERR, "%d:", block->size);

#if DUMP_BLOCK_MAPS
	  FPRINTF(STDERR, "[%lx]", block->start - (unsigned long)block);
#endif

	  while (block) {
	    int k, size = block->size;

#if DUMP_BLOCK_MAPS
	    counter = 0;
#endif

	    for (k = (block->top - block->start) / block->size; k-- ; ) {
	      int bit = POS_TO_UNMARK_BIT(k);
	      int pos = POS_TO_UNMARK_INDEX(k);
	      
	      if (IS_MARKED(block->free[pos] & bit)) {
		total += size;
		counter++;
	      }
	    }

#if DUMP_BLOCK_MAPS
	    FPRINTF(STDERR,
		    ">%lxx%d"
		    , (unsigned long)block, counter
		    );
#endif
	    block = block->next;
	  }
#if DUMP_BLOCK_MAPS
	  FPRINTF(STDERR, "\n");
#else
	  FPRINTF(STDERR, "%d;", counter);
#endif
	}
      }

      /* Print chunks, "sorting" so that same size are printed together: */
      {
	MemoryChunk *c, *cnext, *first = NULL, *last = NULL, *t, *next, *prev;
	int counter = 0;
	
	for (c = *(cs->othersptr); c; c = cnext) {
	  unsigned long size = c->end - c->start;
	  FPRINTF(STDERR, "%ld:", size);

#if DUMP_BLOCK_MAPS
	  FPRINTF(STDERR, "[%lx]", c->start - (unsigned long)c);
#endif
	  
	  cnext = c->next;
	  
	  prev = NULL;
	  for (t = c; t; t = next) {
	    next = t->next;
	    
	    if (size == (t->end - t->start)) {
#if DUMP_BLOCK_MAPS
	      FPRINTF(STDERR,
		      ">%lx"
		      , (unsigned long)t
		      );
#endif
	      
	      counter++;

	      if (last)
		last->next = t;
	      else
		first = t;
	      last = t;
	      if (prev)
		prev->next = t->next;
	      if (t == cnext)
		cnext = t->next;

	      total += size;
	    } else
	      prev = t;
	  }
#if DUMP_BLOCK_MAPS
	  FPRINTF(STDERR, "\n");
#else
	  FPRINTF(STDERR, "%d;", counter);
	  counter = 0;
#endif
	}
	
	if (last)
	  last->next = NULL;
	*(cs->othersptr) = first;
      }
      cs->total = total;

#if KEEP_PREV_PTR
      /* reset prev pointers: */
      {
	MemoryChunk *c, **prev_ptr = (cs->othersptr);
	for (c = *(cs->othersptr); c; c = c->next) {
	  c->prev_ptr = prev_ptr;
	  prev_ptr = &c->next;
	}
      }
#endif
      
      FPRINTF(STDERR, "total size: %ld\n", total);
    }

    FPRINTF(STDERR, "summary: ======================================\n");
    total = 0;
    for (j = 0; j < num_common_sets; j++) {
      GC_Set *cs = common_sets[j];
      FPRINTF(STDERR,
	      "%12s: %10ld  [%s/%s/%s]\n",
	      cs->name, cs->total,
	      cs->tagged ? "tagged" : "plain",
	      cs->atomic ? "atomic" : "pointerful",
	      cs->array ? "array" : "item");
      total += cs->total;
    }
    FPRINTF(STDERR, "%12s: %10ld\n", "total", total);
  }
#endif
  FPRINTF(STDERR, "End Map\n");
}

long GC_get_memory_use()
{
  return mem_real_use;
}

void GC_end_stubborn_change(void *p)
{
  /* stubborness is not exploited */
}

static void *zero_ptr;

#if CHECK_WATCH_FOR_PTR_ALLOC
void *GC_watch_for_ptr = NULL;
#define UNHIDE_WATCH(p) ((void *)~((unsigned long)p))
static int findings;

#if USE_WATCH_FOUND_FUNC
void GC_found_watch()
{
  FPRINTF(STDERR, "found\n");
  findings++;
}
#endif
#endif

static void init_positions(int cpos, int size, int num_elems)
{
  int num_positions = num_elems << LOG_FREE_BIT_PER_ELEM;
  int block_size = size * num_positions;
  int num_offsets = block_size >> LOG_PTR_SIZE;
  int size_in_ptrs = size >> LOG_PTR_SIZE;
  int i, j, pos;
  int *positions;

  positions = (int *)malloc_sector(num_offsets * sizeof(int), sector_kind_other, 0);

  for (i = 0, pos = 0, j = 0; i < num_offsets; ) {
    positions[i++] = pos;
    if (++j == size_in_ptrs) {
      pos++;
      j = 0;
    }
  }

  common_positionses[cpos] = positions;
}

#if ALLOC_STATS
# define ALLOC_STATISTIC(x) x
static int num_allocs_stat;
static int num_nonzero_allocs_stat;
static int num_common_allocs_stat;
static int num_block_alloc_checks_stat;
static int num_block_alloc_nexts_stat;
static int num_block_alloc_second_checks_stat;
static int num_chunk_allocs_stat;
static int num_newblock_allocs_stat;
#else
# define ALLOC_STATISTIC(x) /* empty */
#endif

#if KEEP_SET_NO || KEEP_CHUNK_SET_NO
#define SET_NO_BACKINFO int set_no,
#define KEEP_SET_INFO_ARG(x) x, 
#else
#define SET_NO_BACKINFO /* empty */
#define KEEP_SET_INFO_ARG(x) /* empty */
#endif

void *do_malloc(SET_NO_BACKINFO
		unsigned long size, 
		MemoryBlock **common,
		MemoryChunk **othersptr,
		int flags)
{
  MemoryBlock **find, *block;
  MemoryBlock **common_ends;
  void *s;
  long c;
  unsigned long p;
  long sizeElemBit;
  int i, cpos, elem_per_block, extra_alignment;

#if CHECK_COLLECTING
  if (collecting_now) {
    exit(-1);
  }
#endif

  ALLOC_STATISTIC(num_allocs_stat++);

  if (!size)
    return (void *)&zero_ptr;

  ALLOC_STATISTIC(num_nonzero_allocs_stat++);

  if (size < (MAX_COMMON_SIZE - PTR_SIZE + 1)) {
    ALLOC_STATISTIC(num_common_allocs_stat++);

    if (!size_map)
      init_size_map();

    cpos = size_index_map[((size + PTR_SIZE - 1) >> LOG_PTR_SIZE) - 1];
#if 0
    if (size > size_map[cpos]) {
      FPRINTF(STDERR, "map error: %d < %d\n", size_map[cpos], size);
    }
#endif
    size = size_map[cpos];

    block = common[cpos + NUM_COMMON_SIZE];
    find = NULL;

    while (block) {
      int search_bit, search_offset;

      if (block->top < block->end)
	goto block_top;

      ALLOC_STATISTIC(num_block_alloc_checks_stat++);

      search_bit = block->free_search_bit;
      search_offset = block->free_search_offset;

      for (i = block->free_search_start; i >= 0; i--)
	if (block->free[i]) {
	  char *zp;
	  int v = block->free[i];
	  
	  while (IS_MARKED(v & search_bit)) {
	    search_bit = search_bit << FREE_BIT_SIZE;
	    search_offset++;
	  }
	  block->free[i] -= search_bit;
	  block->free_search_start = i;
	  block->free_search_bit = search_bit << FREE_BIT_SIZE;
	  block->free_search_offset = search_offset + 1;

	  c = (i << LOG_FREE_BIT_PER_ELEM) + search_offset;
	  
	  mem_use += size;
	  
	  p = block->start + c * size;

	  zp = INT_TO_PTR(p);

	  if (!(flags & mtype_ATOMIC)) {
	    void **p = (void **)zp;
	    unsigned long sz = size >> LOG_PTR_SIZE;
	    for (; sz--; p++)
	      *p = 0;
	  }

	  return zp;
	} else {
	  search_bit = (FREE_BIT_START | UNMARK_BIT_START);
	  search_offset = 0;
	}

      find = &block->next;

      block = block->next;
      common[cpos + NUM_COMMON_SIZE] = block;

      ALLOC_STATISTIC(num_block_alloc_nexts_stat++);
      ALLOC_STATISTIC(if (block) num_block_alloc_second_checks_stat++);
    }

  } else {
    void *a;
    MemoryChunk *c;

    /* Round up to ptr-aligned size: */
    if (size & (PTR_SIZE-1))
      size += PTR_SIZE - (size & (PTR_SIZE-1));

    ALLOC_STATISTIC(num_chunk_allocs_stat++);

    cpos = 0;

    a = malloc_sector(size + sizeof(MemoryChunk), sector_kind_chunk, 1);
    if (!a) {
      if (mem_use >= mem_limit)
	GC_gcollect();
      
      a = malloc_sector(size + sizeof(MemoryChunk), sector_kind_chunk, 0);
    }

    c = (MemoryChunk *)a;
    
    c->finalizers = NULL;
    c->marked = 1;

    c->next = *othersptr;
#if CHECK_FREES
    if (PTR_TO_INT(c->next) & (SECTOR_SEGMENT_SIZE - 1))
      free_error("bad next\n");
#endif
    *othersptr = c;
#if KEEP_PREV_PTR
    c->prev_ptr = othersptr;
    if (c->next)
      c->next->prev_ptr = &c->next;
#endif
    
    c->start = PTR_TO_INT(&c->data);
    c->end = c->start + size;
    c->mtype = flags;

    mem_use += size;
    mem_real_use += (size + sizeof(MemoryChunk));
    num_chunks++;

    if (!low_plausible || (c->start < low_plausible))
      low_plausible = c->start;
    if (!high_plausible || (c->end > high_plausible))
      high_plausible = c->end;	

    if (!(flags & mtype_ATOMIC)) {
      void **p = (void **)&c->data;
      unsigned long sz = size >> LOG_PTR_SIZE;
      for (; sz--; p++)
	*p = 0;
    }

#if CHECK_WATCH_FOR_PTR_ALLOC
    if ((&c->data) == UNHIDE_WATCH(GC_watch_for_ptr)) {
#if USE_WATCH_FOUND_FUNC
      GC_found_watch();
#else
      findings++;
#endif
    }
#endif

    s = (void *)&c->data;

    return s;
  }

  ALLOC_STATISTIC(num_newblock_allocs_stat++);

  sizeElemBit = size << LOG_FREE_BIT_PER_ELEM;
  
  extra_alignment = (size & (DOUBLE_SIZE - 1)) ? 0 : (DOUBLE_SIZE - PTR_SIZE);

  /* upper bound: */
  elem_per_block = (SECTOR_SEGMENT_SIZE - sizeof(MemoryBlock)) / sizeElemBit;
  /*                ^- mem area size      ^- block record */
  /* use this one: */
  elem_per_block = ((SECTOR_SEGMENT_SIZE - sizeof(MemoryBlock) - elem_per_block
  /*                ^- mem area size      ^- block record       ^- elems     */
		     - (extra_alignment + PTR_SIZE - 2)) / sizeElemBit);
  /*                     ^- possible elem padding, -2 since MemoryBlock has free[1] */
  if (elem_per_block) {
    /* Small enough to fit into one segment */
    c = SECTOR_SEGMENT_SIZE;
  } else {
    elem_per_block = 1;
    /* Add (PTR_SIZE - 1) to ensure enough room after alignment: */
    c = sizeof(MemoryBlock) + (PTR_SIZE - 1) + sizeElemBit;
  }

  block = (MemoryBlock *)malloc_sector(c, sector_kind_block, 1);
  if (!block) {
    if (mem_use >= mem_limit) {
      GC_gcollect();
      return do_malloc(KEEP_SET_INFO_ARG(set_no)
		       size, common, othersptr, flags);
    } else
      block = (MemoryBlock *)malloc_sector(c, sector_kind_block, 0);
  }

  
  block->elem_per_block = elem_per_block;

  block->finalizers = NULL;

  /* offset for data (ptr aligned): */
  c = sizeof(MemoryBlock) + (elem_per_block - 1);
  if (c & (PTR_SIZE - 1))
    c += (PTR_SIZE - (c & (PTR_SIZE - 1)));
  if (!(size & (DOUBLE_SIZE - 1))) /* Even more alignment for doubles: */
    if (c & (DOUBLE_SIZE - 1))
      c += (DOUBLE_SIZE - (c & (DOUBLE_SIZE - 1)));
  p = PTR_TO_INT(block) + c;

  common_ends = common + NUM_COMMON_SIZE;

  if (common_ends[cpos] || (find && !common[cpos])) {
    /* hey! - GC happened and reset stuff. find may not be alive anymore,
       so find it again. */
    find = &common_ends[cpos];
    while (*find)
      find = &(*find)->next;
  }

  if (find)
    *find = block;
  else if (!common[cpos])
    common[cpos] = block;

  if (!common_ends[cpos])
    common_ends[cpos] = block;

  num_blocks++;

  for (i = ELEM_PER_BLOCK(block); i-- ; )
    block->free[i] = 0;
  block->free_search_start = -1; /* a free search will not yield results until a GC */

  block->start = block->top = p;
  block->end = block->start + (elem_per_block * sizeElemBit);
  block->size = (short)size;
  block->next = NULL;
  block->mtype = flags;
  if (!common_positionses[cpos])
    init_positions(cpos, size, elem_per_block);
  block->positions = common_positionses[cpos];

  if (!low_plausible || (block->start < low_plausible))
    low_plausible = block->start;
  if (!high_plausible || (block->end > high_plausible))
    high_plausible = block->end;	

  mem_real_use += SECTOR_SEGMENT_SIZE;

 block_top:

#if CHECK
  if (block->end < block->start
      || block->top < block->start
      || block->top > block->end)
    FPRINTF(STDERR,
	    "bad block: %ld %ld %ld %ld\n",
	    size, block->start, block->top, block->end);
#endif      

  s = INT_TO_PTR(block->top);
  block->top = block->top + size;

  mem_use += size;

  if (!(flags & mtype_ATOMIC)) {
    void **p = (void **)s;
    unsigned long sz = size >> LOG_PTR_SIZE;
    for (; sz--; p++)
      *p = 0;
  }

#if CHECK_WATCH_FOR_PTR_ALLOC
  if (s == UNHIDE_WATCH(GC_watch_for_ptr)) {
#if USE_WATCH_FOUND_FUNC
    GC_found_watch();
#else
    findings++;
#endif
  }
#endif

  return s;
}

void *GC_malloc(size_t size)
{
  return do_malloc(size, array_common, &array_others,
		   mtype_ARRAY);
}

void *GC_malloc_one_tagged(size_t size)
{
  return do_malloc(size, tagged_common, &tagged_others,
		   mtype_TAGGED);
}

void *GC_malloc_atomic(size_t size)
{
  return do_malloc(size, atomic_common, &atomic_others, 
		   mtype_ATOMIC);
}

void *GC_malloc_array_tagged(size_t size)
{
  return do_malloc(size, tagged_array_common, &tagged_array_others, 
		   mtype_TAGGED_ARRAY);
}

void *GC_malloc_atomic_uncollectable(size_t size)
{
  return malloc(size);
}

/******************************************************************/

static void free_chunk(MemoryChunk *k, MemoryChunk **prev, struct GC_Set *set)
{
  MemoryChunk *next;
  
#if ALLOW_SET_FINALIZER
  if (set->finalizer) {
    void *s = INT_TO_PTR(k->start);
    set->finalizer(s);
  }
#endif
  
  mem_real_use -= (k->end - k->start + sizeof(MemoryChunk));
  
#if PRINT && 0
  FPRINTF(STDERR, "free chunk: %ld (%ld) %d %d\n", 
	  (unsigned long)k, k->end - k->start,
	  set->atomic, set->uncollectable);
#endif
  
  next = k->next;

#if KEEP_PREV_PTR
  if (next)
    next->prev_ptr = k->prev_ptr;
#endif

#if CHECK_FREES
  if (PTR_TO_INT(next) & (SECTOR_SEGMENT_SIZE - 1))
    free_error("bad next\n");
#endif

  *prev = next;

  free_sector(k);
  --num_chunks;
}

void GC_free(void *p) 
{
}

/******************************************************************/

#if CHECK
static long cmn_count, chk_count;
#endif

static void collect_init_chunk(MemoryChunk *c)
{
  for (; c; c = c->next) {
    c->marked = 0;

#if CHECK
    chk_count++;
    if ((!low_plausible || (c->start < low_plausible))
	|| (!high_plausible || (c->end > high_plausible)))
      FPRINTF(STDERR, "implausible chunk!\n");
#endif
  }
}

#if FINISH_STATS
# define FINISH_STATISTIC(x) x
static int num_finish_chunk_stat;
static int num_finish_chunkkeep_stat;
static int num_finish_chunkfree_stat;
static int num_finish_block_stat;
static int num_finish_blockkeep_stat;
static int num_finish_blockfree_stat;
static int num_finish_blockadjust_stat;
static int num_finish_blockfiltercycles_stat;
static int num_finishes_stat;
#else
# define FINISH_STATISTIC(x)
#endif

static void collect_finish_chunk(MemoryChunk **c, struct GC_Set *set)
{
  unsigned long local_low_plausible;
  unsigned long local_high_plausible;

  local_low_plausible = low_plausible;
  local_high_plausible = high_plausible;

  while (*c) {
    MemoryChunk *k = *c;

    FINISH_STATISTIC(num_finish_chunk_stat++);

    if (k->marked) {
      c = &k->next;

      FINISH_STATISTIC(num_finish_chunkkeep_stat++);

      if (!local_low_plausible || (k->start < local_low_plausible))
	local_low_plausible = k->start;
      if (!local_high_plausible || (k->end > local_high_plausible))
	local_high_plausible = k->end;	
    } else {
      FINISH_STATISTIC(num_finish_chunkfree_stat++);

      free_chunk(k, c, set);
    }
  }

  low_plausible = local_low_plausible;
  high_plausible = local_high_plausible;
}

static void collect_init_common(MemoryBlock **blocks)
{
  int i, j;
  int boundary, boundary_val = 0;

  for (i = 0; i < NUM_COMMON_SIZE; i++) {
    MemoryBlock *block = blocks[i];

    while (block) {
#if CHECK
      cmn_count++;
      if ((!low_plausible || (block->start < low_plausible))
	  || (!high_plausible || (block->end > high_plausible)))
	FPRINTF(STDERR, "implausible block!\n");
#endif

      if (block->top < block->end) {
	int pos = block->positions[(block->top - block->start) >> LOG_PTR_SIZE];
	boundary = POS_TO_UNMARK_INDEX(pos);
	boundary_val = (POS_TO_UNMARK_BIT(pos) - 1) & ALL_UNMARKED;
      } else {
	boundary = ELEM_PER_BLOCK(block);
      }
      
      for (j = ELEM_PER_BLOCK(block); j-- ; ) {
	if (j < boundary)
	  block->free[j] |= ALL_UNMARKED;
	else if (j == boundary)
	  block->free[j] = boundary_val;
	else
	  block->free[j] = 0;
      }

      block = block->next;
    }
  }
}

static void collect_finish_common(MemoryBlock **blocks, 
				  MemoryBlock **block_ends, 
				  struct GC_Set *set)
{
  int i;
#if KEEP_BLOCKS_FOREVER
  int kept;
#endif
  unsigned long local_low_plausible;
  unsigned long local_high_plausible;

  local_low_plausible = low_plausible;
  local_high_plausible = high_plausible;

  for (i = 0; i < NUM_COMMON_SIZE; i++) {
    MemoryBlock **prev = &blocks[i];
    MemoryBlock *block = *prev;
#if CHECK
    long size = size_map[i];
#endif

#if KEEP_BLOCKS_FOREVER
    kept = 0;
#endif

    while (block) {
      int unfree;

      FINISH_STATISTIC(num_finish_block_stat++);
      
#if CHECK
      if (block->end < block->start
	  || block->top < block->start
	  || block->top > block->end)
	FPRINTF(STDERR,
		"bad block: %ld %ld %ld %ld\n",
		size, block->start, block->top, block->end);
#endif

#if ALLOW_SET_FINALIZER
      if (set->finalizer) {
	unsigned long s;
	int j;
	for (j = 0, s = block->start; s < block->top; s += block->size, j++) {
	  int pos = POS_TO_UNMARK_INDEX(j);
	  int bit = POS_TO_UNMARK_BIT(j);

	  if (NOT_MARKED(block->free[pos] & bit)) {
	    void *p = INT_TO_PTR(s);
	    set->finalizer(p);
	  }
	}
      }
#endif

      unfree = 0;
      {
	int j;
	for (j = ELEM_PER_BLOCK(block); j-- ; ) {
	  FINISH_STATISTIC(num_finish_blockfiltercycles_stat++);
	  if ((block->free[j] & ALL_UNMARKED) != ALL_UNMARKED) {
	    unfree = j + 1;
	    break;
	  }
	}
      }

#if KEEP_BLOCKS_FOREVER
      if (!unfree && (kept < KEEP_BLOCKS_FOREVER)) {
	int j;
	block->top = block->start;
	for (j = ELEM_PER_BLOCK(block); j-- ; )
	  block->free[j] = 0;
	kept++;
	unfree = 1;
      }
#endif

      if (!unfree) {
	FINISH_STATISTIC(num_finish_blockfree_stat++);

	--num_blocks;

	*prev = block->next;
	free_sector(block);
	mem_real_use -= SECTOR_SEGMENT_SIZE;
	block = *prev;
      } else {
#if DISTINGUISH_FREE_FROM_UNMARKED
	/* If it's unmarked, free it: */
	int j;

	for (j = ELEM_PER_BLOCK(block); j-- ; )
	  block->free[j] |= SHIFT_UNMARK_TO_FREE(block->free[j]);
#endif

	/* Push down block->top if it's easy */
	{
	  unsigned long dt = (unfree << LOG_FREE_BIT_PER_ELEM) * (unsigned long)block->size;
	  if (block->top > block->start + dt) {
	    int k;
	    FINISH_STATISTIC(num_finish_blockadjust_stat++);
	    block->top = block->start + dt;
	    for (k = ELEM_PER_BLOCK(block); --k >= unfree; ) {
	      block->free[k] = 0;
	    }
	  }
	}
	
	block->free_search_start = unfree - 1;
	block->free_search_bit = (FREE_BIT_START | UNMARK_BIT_START);
	block->free_search_offset = 0;

	FINISH_STATISTIC(num_finish_blockkeep_stat++);

	if (!local_low_plausible || (block->start < local_low_plausible))
	  local_low_plausible = block->start;
	if (!local_high_plausible || (block->end > local_high_plausible))
	  local_high_plausible = block->end;

	prev = &block->next;
	block = block->next;
      }
    }

    block_ends[i] = blocks[i];
  }

  low_plausible = local_low_plausible;
  high_plausible = local_high_plausible;
}

static int collect_stack_count;
static int collect_stack_size;
static unsigned long *collect_stack;

#define INITIAL_COLLECT_STACK_SIZE 8192

static void push_collect(const void *start)
{
  if (collect_stack_count >= collect_stack_size) {
    long oldsize;

    if (collect_stack)
      oldsize = sizeof(unsigned long) * collect_stack_size;
    else
      oldsize = 0;

    collect_stack_size = collect_stack_size ? 2 * collect_stack_size : 500;
    collect_stack = (unsigned long *)realloc_collect_temp(collect_stack, 
							  oldsize, 
							  sizeof(unsigned long) 
							  * collect_stack_size);
    /* fprintf(stderr, "grow push stack: %d\n", collect_stack_size); */
  }

  collect_stack[collect_stack_count++] = PTR_TO_INT(start);
}

#define PUSH(s) \
  if (collect_stack_count < collect_stack_size) { \
    collect_stack[collect_stack_count++] = PTR_TO_INT(s); \
  } else \
    push_collect(s);

#if MARK_STATS
static int num_pairs_stat;
static int num_checks_stat;
static int num_interior_checks_stat;
static int num_plausibles_stat;
static int num_pages_stat;
static int num_blocks_stat;
static int num_blockallocs_stat;
static int num_blockaligns_stat;
static int num_blockmarks_stat;
static int num_blockpushes_stat;
static int num_blockpushes_tail_stat;
static int num_chunks_stat;
static int num_chunkmarks_stat;
#endif

#if MARK_STATS
# define MARK_STATISTIC(x) x
#else
# define MARK_STATISTIC(x)
#endif

#ifdef FOLLOW_INTERIOR
# define ALIGNED(x) 1
#else
# define ALIGNED(x) !(x & 0x1)
#endif

static void mark_from_stack()
{
  void *pp = NULL;
  int pp_mtype = mtype_ATOMIC;
  int pp_size = 0;

  while (collect_stack_count) {
    unsigned long p;

    p = collect_stack[--collect_stack_count];
    
    MARK_STATISTIC(num_pairs_stat++);

    {
      pp_mtype = mtype_ATOMIC;

      MARK_STATISTIC(num_checks_stat++);
      if (p >= low_plausible && p < high_plausible && ALIGNED(p)) {
	SectorPage *pagetable = sector_pagetables[SECTOR_LOOKUP_PAGETABLE(p)];

	MARK_STATISTIC(num_plausibles_stat++);

	if (pagetable) {
	  SectorPage *page = pagetable + SECTOR_LOOKUP_PAGEPOS(p);
	  long kind = page->kind;

	  MARK_STATISTIC(num_pages_stat++);

	  if (kind == sector_kind_block) {
	    /* Found common block: */
	    MemoryBlock *block = (MemoryBlock *)INT_TO_PTR(page->start);
	    unsigned long bstart = block->start;

	    MARK_STATISTIC(num_blocks_stat++);

	    if ((p >= bstart) && (p < block->top)) {
	      int size = block->size;
	      int pos = block->positions[(p - bstart) >> LOG_PTR_SIZE];
	      int start = bstart + pos * size;
	    
	      MARK_STATISTIC(num_blockallocs_stat++);

	      {
		int bpos;
		unsigned char bit;
#if DISTINGUISH_FREE_FROM_UNMARKED
		unsigned char fbit;
#endif
		unsigned char freebyte;
	
		MARK_STATISTIC(num_blockaligns_stat++);

		bpos = POS_TO_UNMARK_INDEX(pos);
		bit = POS_TO_UNMARK_BIT(pos);
#if DISTINGUISH_FREE_FROM_UNMARKED
		fbit = POS_TO_FREE_BIT(pos);
#endif

		freebyte = block->free[bpos];
		
		if (NOT_MARKED(freebyte & bit) && _NOT_FREE(freebyte & fbit)) {
		  MARK_STATISTIC(num_blockmarks_stat++);
		
		  
		  block->free[bpos] = freebyte ^ bit;
		  
		  mem_use += size;
		  
		  pp = INT_TO_PTR(start);
		  pp_mtype = block->mtype;
		  pp_size = size;
		}
	      }
	    }
	  } else if (kind == sector_kind_chunk) {
	    MemoryChunk *c = (MemoryChunk *)INT_TO_PTR(page->start);
	    
	    MARK_STATISTIC(num_chunks_stat++);

	    if (!c->marked) {
	      MARK_STATISTIC(num_chunkmarks_stat++);

	      c->marked = 1;
	      pp_size = (c->end - c->start);
	      mem_use += pp_size;
	      pp = INT_TO_PTR(c->start);
	      pp_mtype = c->mtype;
	    }
	  }
	}
      }

      switch (pp_mtype) {
      case mtype_ATOMIC:
	break;
      case mtype_TAGGED:
	{
	  mark_table[*(Type_Tag *)pp](pp);
	  break;
	}
      case mtype_ARRAY:
	{
	  int i = pp_size >> LOG_PTR_SIZE;
	  void **ppp = (void **)pp;
	  while (i--) {
	    PUSH(ppp[i]);
	  }
	  break;
	}
      case mtype_TAGGED_ARRAY:
	{
	  int i = pp_size, onesize;
	  Type_Tag tag = *(Type_Tag *)pp;
	  Mark_Proc mark = mark_table[tag];
	  onesize = size_table[tag](pp);
	  onesize = onesize << LOG_PTR_SIZE;
	  while (i > 0) {
	    mark(pp);
	    pp += onesize;
	    i -= onesize;
	  }
	}
      }
    }
  }
}

void GC_mark(const void *s)
{
  PUSH(s);
}

void GC_fixup(void *s)
{
  /* Not used */
}

void GC_mark_variable_stack(void **var_stack,
			    long delta,
			    void *limit)
{
  while (var_stack) {
    long size;
    void ***p;

    var_stack = (void **)((char *)var_stack + delta);
    if (var_stack == limit)
      return;

    size = *(long *)(var_stack + 1);

    p = (void ***)(var_stack + 2);
    
    while (size--) {
      if (!*p) {
	/* Array */
	long count = ((long *)p)[2];
	void **a = ((void ***)p)[1];
	p += 2;
	size -= 2;
	a = (void **)((char *)a + delta);
	while (count--) {
	  PUSH(*a);
	  a++;
	}
      } else {
	void **a = *p;
	a = (void **)((char *)a + delta);
	PUSH(*a);
      }
      p++;
    }

    var_stack = *var_stack;
  }
}

void GC_fixup_variable_stack(void **var_stack,
			    long delta,
			     void *limit)
{
  /* Not used */
}

static int compare_roots(const void *a, const void *b)
{
  if (*(unsigned long *)a < *(unsigned long *)b)
    return -1;
  else
    return 1;
}

static void sort_and_merge_roots()
{
  static int counter = 0;
  int i, offset, top;

  if (roots_count < 4)
    return;

  /* Only try this every 5 collections or so: */
  if (counter--)
    return;
  counter = 5;

  qsort(roots, roots_count >> 1, 2 * sizeof(unsigned long), compare_roots);
  offset = 0;
  top = roots_count;
  for (i = 2; i < top; i += 2) {
    if ((roots[i - 2 - offset] <= roots[i])
	&& ((roots[i - 1 - offset] + (PTR_ALIGNMENT - 1)) >= roots[i])) {
      /* merge: */
      if (roots[i + 1] > roots[i - 1 - offset])
	roots[i - 1 - offset] = roots[i + 1];
      offset += 2;
      roots_count -= 2;
    } else if (offset) {
      /* compact: */
      roots[i - offset] = roots[i];
      roots[i + 1 - offset] = roots[i + 1];
    }
  }
}

static void do_roots()
{
  ImmobileBox *ib;
  int i;

  for (i = 0; i < roots_count; i += 2) {
    void **s = (void **)roots[i];
    void **e = (void **)roots[i + 1];
    
    while (s < e) {
      PUSH(*s);
      s++;
    }
  }

  GC_mark_variable_stack(GC_variable_stack,
			 0,
			 (void *)(GC_get_thread_stack_base
				  ? GC_get_thread_stack_base()
				  : (unsigned long)GC_stackbottom));
  
  /* Do immobiles: */
  for (ib = immobile; ib; ib = ib->next) {
    PUSH(ib->p);
  }
}

#if 0
# define GETTIME() ((long)scheme_get_milliseconds())
#else
# define GETTIME() ((long)scheme_get_process_milliseconds())
#endif

#if TIME
# define PRINTTIME(x) FPRINTF x
static long started, rightnow, old;
# define INITTIME() (started = GETTIME())
# define GETTIMEREL() (rightnow = GETTIME(), old = started, started = rightnow, rightnow - old)
#else
# define INITTIME() /* empty */
# define PRINTTIME(x) /* empty */
#endif

#if PRINT_INFO_PER_GC
static long last_gc_end;
#endif

static void gcollect()
{
  int did_fnls;
  GC_Weak_Box *wb;
  GC_Weak_Array *wa;
#if TIME
  struct rusage pre, post;
#endif

  INITTIME();
  PRINTTIME((STDERR, "gc: start with %ld [%d]: %ld\n", 
	     memory_in_use, cycle_count + 1, GETTIMEREL()));

  if (!initialized) {
    GC_register_traversers(weak_box_tag, size_weak_box, mark_weak_box, fixup_weak_box);
    GC_register_traversers(gc_weak_array_tag, size_weak_array, mark_weak_array, fixup_weak_array);
    GC_register_traversers(gc_finalization_tag, size_finalizer, mark_finalizer, fixup_finalizer);
    GC_register_traversers(gc_finalization_weak_link_tag, size_finalizer_weak_link, mark_finalizer_weak_link, fixup_finalizer_weak_link);
    GC_add_roots(&fnls, (char *)&fnls + sizeof(fnls) + 1);
    GC_add_roots(&fnl_weaks, (char *)&fnl_weaks + sizeof(fnl_weaks) + 1);
    GC_add_roots(&run_queue, (char *)&run_queue + sizeof(run_queue) + 1);
    GC_add_roots(&last_in_queue, (char *)&last_in_queue + sizeof(last_in_queue) + 1);
    GC_add_roots(&park, (char *)&park + sizeof(park) + 1);

    GC_initialize();
  }
  
  weak_boxes = NULL;
  weak_arrays = NULL;
  did_fnls = 0;

  if (GC_collect_start_callback)
    GC_collect_start_callback();

  sort_and_merge_roots();

  /******************** Init ****************************/

  mem_use = 0;

  if (!initialized)
    GC_initialize();

  prepare_collect_temp();

  collect_stack_size = roots_count ? roots_count : 10;
  if (collect_stack_size < INITIAL_COLLECT_STACK_SIZE)
    collect_stack_size = INITIAL_COLLECT_STACK_SIZE;
  collect_stack_count = 0;
  collect_stack = (unsigned long *)realloc_collect_temp(NULL,
							0,
							sizeof(unsigned long) 
							* collect_stack_size);
  {
    int j;
    
    for (j = 0; j < num_common_sets; j++) {
      collect_init_chunk(*(common_sets[j]->othersptr));
      collect_init_common(common_sets[j]->blocks);
    }
  }

  /************* Mark and Propagate *********************/

  do_roots(0);

#if TIME
  getrusage(RUSAGE_SELF, &post);
#endif

  PRINTTIME((STDERR, "gc: roots (i:%d d:%d) [%ld faults]: %ld\n", 
	     inited_pages, stack_depth, post.ru_minflt - pre.ru_minflt,
	     GETTIMEREL()));

  /* Propagate, loop to do finalization */
  while (1) { 

    /* Mark everything on the stack: */
    mark_from_stack();
    
    if ((did_fnls >= 3) || !fnls) {
      if (did_fnls == 3) {
	/* Finish up ordered finalization */
	Fnl *f, *next, *prev;
	Fnl_Weak_Link *wl;

	/* Enqueue and mark level 3 finalizers that still haven't been marked. */
	/* (Recursive marking is already done, though.) */
	prev = NULL;
	for (f = fnls; f; f = next) {
	  next = f->next;
	  if (f->eager_level == 3) {
	    if (!is_marked(f->p)) {
	      /* Not yet marked. Enqueue it for marking. */
	      PUSH(f->p);

	      if (prev)
		prev->next = next;
	      else
		fnls = next;
	      
	      f->eager_level = 0; /* indicates queued */
	      
	      f->next = NULL;
	      if (last_in_queue) {
		last_in_queue->next = f;
		last_in_queue = f;
	      } else {
		run_queue = last_in_queue = f;
	      }
	    } else {
	      prev = f;
	    }
	  }
	}

	/* Restore zeroed out weak links, marking as we go: */	
	for (wl = fnl_weaks; wl; wl = wl->next) {
	  void *wp = (void *)wl->p;
	  int markit;
	  markit = is_marked(wp);
	  if (markit) {
	    PUSH(wl->saved);
	  }
	  *(void **)wp = wl->saved;
	}
	
	/* We have to mark one more time, because restoring a weak
           link may have made something reachable. */

	did_fnls++;
      } else
	break;
    } else {
      int eager_level = did_fnls + 1;
      
      if (eager_level == 3) {
	/* Ordered finalization */
	Fnl *f;
	Fnl_Weak_Link *wl;

	/* Zero out weak links for ordered finalization */
	for (wl = fnl_weaks; wl; wl = wl->next) {
	  void *wp = (void *)wl->p;
	  wl->saved = *(void **)wp;
	  *(void **)(wp) = NULL;
	}

	/* Mark content of not-yet-marked finalized objects,
	   but don't mark the finalized objects themselves. */	
	for (f = fnls; f; f = f->next) {
	  if (f->eager_level == 3) {
	    if (!is_marked(f->p)) {
	      /* Not yet marked. Mark content. */
	      Type_Tag tag = *(Type_Tag *)f->p;
#if SAFETY
	      if ((tag < 0) || (tag >= _num_tags_) || !mark_table[tag]) {
		CRASH();
	      }
#endif
	      mark_table[tag](f->p);
	    }
	  }
	}
      } else {
	/* Unordered finalization */
	Fnl *f, *prev, *queue;

	f = fnls;
	prev = NULL;
	queue = NULL;
	
	while (f) {
	  if (f->eager_level == eager_level) {
	    if (!is_marked(f->p)) {
	      /* Not yet marked. Move finalization to run queue. */
	      Fnl *next = f->next;

	      if (prev)
		prev->next = next;
	      else
		fnls = next;
	      
	      f->eager_level = 0; /* indicates queued */
	      
	      f->next = NULL;
	      if (last_in_queue) {
		last_in_queue->next = f;
		last_in_queue = f;
	      } else {
		run_queue = last_in_queue = f;
	      }
	      if (!queue)
		queue = f;

	      f = next;
	    } else {
	      prev = f;
	      f = f->next;
	    }
	  } else {
	    prev = f;
	    f = f->next;
	  }
	}
	
	/* Mark items added to run queue: */
	f = queue;
	while (f) {
	  PUSH(f->p);
	  f = f->next;
	}
      }
	
      did_fnls++;
    }

  }

#if MARK_STATS
# define STATS_FORMAT " {c=%ld h=%ld c=%ld r=%ld o=%ld m=%ld s=%ld}"
# define STATS_ARGS mark_calls, mark_hits, mark_colors, mark_recalls, mark_one, mark_many, mark_slow,
#else
# define STATS_FORMAT
# define STATS_ARGS
#endif

#if TIME
  getrusage(RUSAGE_SELF, &post);
#endif

  PRINTTIME((STDERR, "gc: mark (i:%d c:%ld s:%ld d:%ld)"
	     STATS_FORMAT
	     " [%ld faults]: %ld\n", 
	     inited_pages, iterations, 
	     mark_stackcount, mark_maxdepth,
	     STATS_ARGS
	     post.ru_minflt - pre.ru_minflt,
	     GETTIMEREL()));

  /******************************************************/

  /* Do weak boxes: */
  wb = weak_boxes;
  while (wb) {
    if (!is_marked(wb->val)) {
      wb->val = NULL;
      if (wb->secondary_erase) {
	*(wb->secondary_erase) = NULL;
	wb->secondary_erase = NULL;
      }
    }
    wb = wb->next;
  }

  /* Do weak arrays: */
  wa = weak_arrays;
  while (wa) {
    int i;
    void **data;
    
    data = wa->data;
    for (i = wa->count; i--; ) {
      void *p = data[i];
      if (!is_marked(p))
	data[i] = wa->replace_val;
    }
    
    wa = wa->next;
  }

  /* Cleanup weak finalization links: */
  {
    Fnl_Weak_Link *wl, *prev, *next;

    prev = NULL;
    for (wl = fnl_weaks; wl; wl = next) {
      next = wl->next;
      if (!is_marked(wl->p)) {
	/* Will be collected. Removed this link. */
	if (prev)
	  prev->next = next;
	else
	  fnl_weaks = next;
      } else {
	prev = wl;
      }
    }
  }

  PRINTTIME((STDERR, "gc: weak: %ld\n", GETTIMEREL()));

  /**********************************************************************/

  low_plausible = high_plausible = 0;

  {
    int i;
    for (i = 0; i < num_common_sets; i++) {
      FINISH_STATISTIC(num_finishes_stat++);
      collect_finish_chunk(common_sets[i]->othersptr, common_sets[i]);
      collect_finish_common(common_sets[i]->blocks, 
			    common_sets[i]->block_ends,
			    common_sets[i]);
    }
  }

  if (mem_use) {
# if USE_GC_FREE_SPACE_DIVISOR
    long root_size;

    if (roots_count)
      root_size = roots[1] - roots[0];
    else
      root_size = 0;

    mem_limit = mem_use + ((sector_mem_use + root_size) / GC_free_space_divisor);
# else
    mem_limit = MEM_USE_FACTOR * mem_use;
# endif
  }

  free_collect_temp(collect_stack, sizeof(unsigned long) * collect_stack_size);

  /**********************************************************************/

  if (GC_collect_end_callback)
    GC_collect_end_callback();

#if TIME
  getrusage(RUSAGE_SELF, &post);
#endif

  PRINTTIME((STDERR, "gc: finish [%ld faults]: %ld\n", 
	     post.ru_minflt - pre.ru_minflt,
	     GETTIMEREL()));

  /**********************************************************************/

  /* Run Finalizations. Collections may happen */

  while (run_queue) {
    Fnl *f;
    void **gcs;

    f = run_queue;
    run_queue = run_queue->next;
    if (!run_queue)
      last_in_queue = NULL;

    gcs = GC_variable_stack;
    f->f(f->p, f->data);
    GC_variable_stack = gcs;
  }
}

void GC_gcollect(void)
{
  if (!sector_pagetables)
    return;

  gcollect();
}
