/*
  Precise GC for MzScheme
  Copyright (c) 1999 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.
*/

/* This implementation is currently hard-wired for 4-byte words */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define USE_MMAP 1
#define GROW_FACTOR 2
#define COMPACT_THRESHOLD 0.2

#define GENERATIONS 1
#define USE_FREELIST 1
#define COMPACTING SELECTIVELY_COMPACT

#define ALWAYS_COMPACT 2
#define SELECTIVELY_COMPACT 1
#define NEVER_COMPACT 0

#if defined(sparc) || defined(__sparc) || defined(__sparc__)
# define ALIGN_DOUBLES 1
#else
# define ALIGN_DOUBLES 0
#endif

#if USE_MMAP
/* For mmap: */
# include <unistd.h>
# include <fcntl.h>
# include <sys/types.h>
# include <sys/mman.h>
# include <errno.h>
#endif

typedef short Type_Tag;

#include "gc2.h"

#define TIME 0
#define SEARCH 0
#define SAFETY 0
#define RECYCLE_HEAP 0
#define NOISY 0
#define MARK_STATS 0

#define ALLOC_GC_PHASE 0
#define SKIP_FORCED_GC 0

#define MARK_STACK_MAX 4096

void *mark_stack[MARK_STACK_MAX];
unsigned short mark_stack_type[MARK_STACK_MAX];
long mark_stack_pos = 0;

#if TIME
# include <sys/time.h>
# include <sys/resource.h>
# include <unistd.h>
#endif

void (*GC_collect_start_callback)(void);
void (*GC_collect_end_callback)(void);
void (*GC_out_of_memory)(void);
unsigned long (*GC_get_thread_stack_base)(void);

void (*GC_mark_xtagged)(void *obj);
void (*GC_fixup_xtagged)(void *obj);

void **GC_variable_stack;

static long page_allocations = 0;

static long memory_in_use, gc_threshold = 32000;
#if USE_FREELIST
static long on_free_list;
# define FREE_LIST_DELTA on_free_list
#else
# define FREE_LIST_DELTA 0
#endif

static long num_seg_faults;

Type_Tag weak_box_tag;

#define gc_finalization_tag 256
#define gc_finalization_weak_link_tag 257
#define gc_weak_array_tag 258
#define gc_on_free_list_tag 259

#define _num_tags_ 260

Size_Proc size_table[_num_tags_];
Mark_Proc mark_table[_num_tags_];
Fixup_Proc fixup_table[_num_tags_];

#define STARTING_PLACE ((void **)0)

/* Unless ALIGN_DOUBLES, offsets must fit into 14 bits, saving 2 bits
   for tags.  But since the minimum size of an allocation is two
   words, we only need half as much memory. So we store the offset as
   a pair of bytes. */
#if ALIGN_DOUBLES
# define SQUASH_OFFSETS 0
#else
# define SQUASH_OFFSETS 1
#endif

typedef unsigned short OffsetTy;
#if SQUASH_OFFSETS
typedef unsigned char OffsetArrTy;
#else
typedef unsigned short OffsetArrTy;
#endif

typedef struct MPage {
  unsigned short type;
  short alloc_boundary;
  short compact_boundary;
  short age, refs_age, compact_to_age;
  union {
    OffsetArrTy *offsets;
    long size;
  } u;
  union {
    void **compact_to;
    void *bigblock_start;
  } o;
  void *block_start;
  struct MPage *next, *prev;

  OffsetTy gray_start, gray_end;
  struct MPage *gray_next;
} MPage;

static MPage *first, *last;

static MPage *gray_first;

MPage **mpage_maps;

/* MPAGE_SIZE is the size of one chunk of memory used for allocating
   small objects. It's also the granluarity of memory-mapping. */

#define LOG_MPAGE_SIZE 14
#define MPAGE_SIZE (1 << LOG_MPAGE_SIZE)
#define MPAGE_WORDS (1 << (LOG_MPAGE_SIZE - 2))
#define MPAGE_MASK ((1 << LOG_MPAGE_SIZE) - 1)
#define MPAGE_START ~MPAGE_MASK

#define LOG_MAP_SIZE 9
#define LOG_MAPS_SIZE (32 - LOG_MAP_SIZE - LOG_MPAGE_SIZE)
#define MAP_SIZE (1 << LOG_MAP_SIZE)
#define MAPS_SIZE (1 << LOG_MAPS_SIZE)

#define MAPS_SHIFT (32 - LOG_MAPS_SIZE)
#define MAP_MASK ((1 << (LOG_MAP_SIZE + LOG_MPAGE_SIZE)) - 1)
#define MAP_SHIFT LOG_MPAGE_SIZE

#define LOG_OPAGE_SIZE (LOG_MPAGE_SIZE - 2 - SQUASH_OFFSETS)
#define OPAGE_SIZE (sizeof(OffsetTy) << LOG_OPAGE_SIZE)

#define BIGBLOCK_MIN_SIZE (1 << (LOG_MPAGE_SIZE - 2))

#define FREE_LIST_ARRAY_SIZE (BIGBLOCK_MIN_SIZE >> 2)

# define COLOR_MASK 0x3
# define GRAY_BIT 0x1
# define BLACK_BIT 0x2

#define TYPE_MASK 0xFFFC

#define MTYPE_TAGGED       0x4
#define MTYPE_ATOMIC       0x8
#define MTYPE_TAGGED_ARRAY 0x10
#define MTYPE_ARRAY        0x20
#define MTYPE_XTAGGED      0x40
#define MTYPE_BIGBLOCK     0x80
#define MTYPE_CONTINUED    0x100

#define MTYPE_OLD          0x200
#define MTYPE_MODIFIED     0x400
#define MTYPE_INITED       0x800

#define OFFSET_COLOR_UNMASKED(offsets, pos) (offsets[pos])
#define OFFSET_COLOR(offsets, pos) (offsets[pos] & COLOR_MASK)
#define OFFSET_SET_COLOR_UNMASKED(offsets, pos, c) (offsets[pos] = c)

#if SQUASH_OFFSETS
# define OFFSET_HI_MASK 0xFC
# define OFFSET_LO_MASK 0xFF
# define OFFSET_HI_SHIFT 6
# define OFFSET_SIZE(offsets, pos) (((OffsetTy)(offsets[pos] & OFFSET_HI_MASK) << OFFSET_HI_SHIFT) | (offsets[(pos)+1]))
# define OFFSET_SET_SIZE_UNMASKED(offsets, pos, s) (offsets[pos] = (((s) >> OFFSET_HI_SHIFT) & OFFSET_HI_MASK), offsets[(pos)+1] = ((s) & OFFSET_LO_MASK))
#else
# define OFFSET_SHIFT 2
# define OFFSET_SIZE(offsets, pos) ((offsets[pos]) >> OFFSET_SHIFT)
# define OFFSET_SET_SIZE_UNMASKED(offsets, pos, s) (offsets[pos] = ((s) << OFFSET_SHIFT))
#endif

typedef unsigned short mtype_t;

#define UNTAGGED_EOM   (MPAGE_SIZE + 1)

typedef struct {
  void **low, **high;
  MPage *malloc_page, *compact_page;
  void **compact_to;
  OffsetTy compact_to_offset;
#if USE_FREELIST
  void *free_lists[FREE_LIST_ARRAY_SIZE];
#endif
} MSet;

#define NUM_SETS 5
static MSet tagged, atomic, array, tagged_array, xtagged;
static MSet *sets[NUM_SETS]; /* First one is tagged, last one is atomic */

/* The answer for 0-byte requests: */
static char zero_sized[4];

static void *park[2];

static int cycle_count = 0;
static int skipped_pages, scanned_pages, young_pages, inited_pages;

static long iterations;

static long mark_maxdepth, mark_stackcount;

static int during_gc;

static MPage *find_page(void *p);

#if SAFETY
static void CRASH()
{
  fprintf(stderr, "crash\n");
  abort();
}
#endif

/******************************************************************************/

#if USE_MMAP

int fd, fd_created;

static int page_size;

void *malloc_pages(size_t len, size_t alignment)
{
  void *r;
  size_t extra = 0;

  if (!page_size)
    page_size = getpagesize();

  if (!fd_created) {
    fd_created = 1;
    fd = open("/dev/zero", O_RDWR);
  }

  /* Round up to nearest page: */
  if (len & (page_size - 1))
    len += page_size - (len & (page_size - 1));

 try_again:

  r = mmap(NULL, len + extra, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);

  if (r  == (void *)-1) {
    printf("mmap failed: %s\n", strerror(errno));
    exit(-1);
  }

  if (extra) {
    /* We allocated too large so we can choose the alignment. */
    void *real_r;
    long pre_extra;

    real_r = (void *)(((unsigned long)r + (alignment - 1)) & (~(alignment - 1)));
    
    pre_extra = real_r - r;
    if (pre_extra)
      munmap(r, pre_extra);
    if (pre_extra < extra)
      munmap(real_r + len, extra - pre_extra);
    r = real_r;
  }

  if (alignment && ((unsigned long)r & (alignment - 1))) {
    /* Need better alignment. Try harder. */
    munmap(r, len);
    extra = alignment;
    goto try_again;
  }

  page_allocations += len;

  return r;
}

void free_pages(void *p, size_t len)
{
  /* Round up to nearest page: */
  if (len & (page_size - 1))
    len += page_size - (len & (page_size - 1));

  munmap(p, len);
  page_allocations -= len;
}

void protect_pages(void *p, size_t len, int writeable)
{
  if (len & (page_size - 1)) {
    len += page_size - (len & (page_size - 1));
  }

  mprotect(p, len, (writeable ? (PROT_READ | PROT_WRITE) : PROT_READ));
}

#endif

/******************************************************************************/

#if !USE_MMAP

void *malloc_pages(size_t len, size_t alignment)
{
  return malloc(len);
}

void free_pages(void *p, size_t len)
{
  free(p);
}

#endif

/******************************************************************************/

#define PTR_ALIGNMENT 4
#define PTR_TO_INT(x) ((unsigned long)x)
#define INT_TO_PTR(x) ((void *)x)

static long roots_count;
static long roots_size;
static unsigned long *roots;
static int nothing_new = 0;

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

  if (nothing_new)
    return;

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
    } else if (roots[i] == roots[i + 1]) {
      /* Remove empty range: */
      offset += 2;
      roots_count -= 2;
    } else if (offset) {
      /* compact: */
      roots[i - offset] = roots[i];
      roots[i + 1 - offset] = roots[i + 1];
    }
  }

  nothing_new = 1;
}

void GC_add_roots(void *start, void *end)
{
  if (roots_count >= roots_size) {
    unsigned long *naya;

    roots_size = roots_size ? 2 * roots_size : 500;
    naya = (unsigned long *)malloc(sizeof(unsigned long) * (roots_size + 1));

    memcpy((void *)naya, (void *)roots, 
	   sizeof(unsigned long) * roots_count);

    if (roots)
      free(roots);

    roots = naya;
  }

  roots[roots_count++] = PTR_TO_INT(start);
  roots[roots_count++] = PTR_TO_INT(end) - PTR_ALIGNMENT;
  nothing_new = 0;
}

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

/******************************************************************************/

#if USE_FREELIST

static int size_on_free_list(void *p)
{
  return ((OffsetTy *)p)[1];
}

#endif

/******************************************************************************/

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

#if SAFETY
  /* For now, weak arrays only used for symbols and falses: */
  {
    void **data;
    int i;
    data = a->data;
    for (i = a->count; i--; ) {
      if (data[i] 
	  && (*(short *)(data[i]) != 35)
	  && (*(short *)(data[i]) != 44))
	CRASH();
    }
  }
#endif

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Array) 
			  + ((a->count - 1) * sizeof(void *)));
}

static int fixup_weak_array(void *p)
{
  GC_Weak_Array *a = (GC_Weak_Array *)p;
  int i;
  void **data;

  gcFIXUP(a->replace_val);

  data = a->data;
  for (i = a->count; i--; ) {
    gcFIXUP(data[i]);
  }

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Array) 
			  + ((a->count - 1) * sizeof(void *)));
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
  
  return w;
}

typedef struct GC_Weak_Box {
  /* The first three fields are mandated by the GC spec: */
  Type_Tag type;
  short keyex;
  void *val;
  /* The rest is up to us: */
  void **secondary_erase;
  int soffset;
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
  GC_Weak_Box *wb = (GC_Weak_Box *)p;
    
  gcFIXUP(wb->secondary_erase);
  gcFIXUP(wb->val);

  return gcBYTES_TO_WORDS(sizeof(GC_Weak_Box));
}

void *GC_malloc_weak_box(void *p, void **secondary, int soffset)
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
  w->soffset = soffset;

  return w;
}

/******************************************************************************/

typedef struct Fnl {
  Type_Tag type;
  char eager_level;
  char tagged;
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
  Fnl *fnl = (Fnl *)p;
  
  gcFIXUP(fnl->next);
  gcFIXUP(fnl->data);
  gcFIXUP(fnl->p);
  
  return gcBYTES_TO_WORDS(sizeof(Fnl));
}

void GC_set_finalizer(void *p, int tagged, int level, void (*f)(void *p, void *data), 
		      void *data, void (**oldf)(void *p, void *data), 
		      void **olddata)
{
  Fnl *fnl, *prev;

  {
    MPage *page;
    page = find_page(p);
    if (!page || !page->type) {
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
  fnl->tagged = tagged;

  fnls = fnl;
}

typedef struct Fnl_Weak_Link {
  Type_Tag type;
  void *p;
  int offset;
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
  Fnl_Weak_Link *wl = (Fnl_Weak_Link *)p;
    
  gcFIXUP(wl->next);
  gcFIXUP(wl->p);

  return gcBYTES_TO_WORDS(sizeof(Fnl_Weak_Link));
}

void GC_finalization_weak_ptr(void **p, int offset)
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
  wl->offset = offset * sizeof(void*);

  fnl_weaks = wl;
}

/******************************************************************************/

static unsigned long stack_base;

void GC_set_stack_base(void *base)
{
  stack_base = (unsigned long)base;
}

unsigned long GC_get_stack_base(void)
{
  return stack_base;
}

void GC_dump(void)
{
  int i;

  fprintf(stderr, "t=tagged a=atomic v=array x=xtagged g=tagarray\n");
  fprintf(stderr, "pagesize=%d  mpagesize=%d  opagesize=%d\n", page_size, MPAGE_SIZE, OPAGE_SIZE);
  fprintf(stderr, "[");
  for (i = 0; i < MAPS_SIZE; i++) {
    if (i && !(i & 63))
      fprintf(stderr, "\n ");

    if (mpage_maps[i])
      fprintf(stderr, "*");
    else
      fprintf(stderr, "-");
  }
  fprintf(stderr, "]\n");
  for (i = 0; i < MAPS_SIZE; i++) {
    MPage *maps = mpage_maps[i];
    if (maps) {
      int j;
      fprintf(stderr, "%.2x:\n ", i);
      for (j = 0; j < MAP_SIZE; j++) {
	if (j && !(j & 63))
	  fprintf(stderr, "\n ");

	if (maps[j].type) {
	  int c;

	  if (maps[j].type & MTYPE_CONTINUED) 
	    c = '.';
	  else {
	    if (maps[j].type & MTYPE_TAGGED)
	      c = 't';
	    else if (maps[j].type & MTYPE_TAGGED_ARRAY)
	      c = 'g';
	    else if (maps[j].type & MTYPE_ATOMIC)
	      c = 'a';
	    else if (maps[j].type & MTYPE_XTAGGED)
	      c = 'x';
	    else
	      c = 'v';
	    
	    if (maps[j].type & MTYPE_BIGBLOCK)
	      c = c - ('a' - 'A');
	  }

	  fprintf(stderr, "%c", c);
	} else {
	  fprintf(stderr, "-");
	}
      }
      fprintf(stderr, "\n");
    }
  }

  {
    MPage *page;

    fprintf(stderr, "Block info: [type][modified?][age][refs-age]\n");
    for (page = first, i = 0; page; page = page->next, i++) {
       int c;

       if (page->type & MTYPE_CONTINUED) 
	 c = '.';
       else {
	 if (page->type & MTYPE_TAGGED)
	   c = 't';
	 else if (page->type & MTYPE_TAGGED_ARRAY)
	   c = 'g';
	 else if (page->type & MTYPE_ATOMIC)
	   c = 'a';
	 else if (page->type & MTYPE_XTAGGED)
	   c = 'x';
	 else
	   c = 'v';
	 
	 if (page->type & MTYPE_BIGBLOCK)
	   c = c - ('a' - 'A');
       }
       
       fprintf(stderr, " %c%c%c%c",
	       c,
	       ((page->type & MTYPE_MODIFIED)
		? 'M'
		: '_'),
	       ((page->age < 10)
		? (page->age + '0')
		: (page->age + 'a' - 10)),
	       ((page->type & MTYPE_ATOMIC)
		? '-'
		: ((page->refs_age < 10)
		   ? (page->refs_age + '0')
		   : (page->refs_age + 'a' - 10))));
      if ((i % 10) == 9)
	fprintf(stderr, "\n");
    }
    fprintf(stderr, "\n");
  }
  


  fprintf(stderr, "Memory use: %ld\n", memory_in_use - FREE_LIST_DELTA);
  fprintf(stderr, "Memory overhead: %ld (%f%%)   %ld on free list\n", 
	  page_allocations - memory_in_use + FREE_LIST_DELTA,
	  (100.0 * ((double)page_allocations - memory_in_use)) / memory_in_use,
	  (long)FREE_LIST_DELTA);
}

long GC_get_memory_use()
{
  return memory_in_use;
}

void GC_init_type_tags(int count, int weakbox)
{
  weak_box_tag = weakbox;
}

#define SKIP ((Type_Tag)0x7000)
#define TAGGED_EOM ((Type_Tag)0x6000)

#if SEARCH
void *search_for, *search_mark;
long search_size;
int detail_cycle;
int atomic_detail_cycle;
#endif

#if SEARCH
void stop()
{
  printf("stopped\n");
}
#endif

/******************************************************************************/

/* Works anytime: */

static MPage *find_page(void *p)
{
  unsigned long g = ((unsigned long)p >> MAPS_SHIFT);
  MPage *map;

  map = mpage_maps[g];
  if (map) {
    unsigned long addr = (((unsigned long)p & MAP_MASK) >> MAP_SHIFT);
    MPage *page;

    page = map + addr;
    return page;
  }
  
  return NULL;  
}

/* Works only during GC: */

static int is_marked(void *p)
{
  unsigned long g = ((unsigned long)p >> MAPS_SHIFT);
  MPage *map;

  map = mpage_maps[g];
  if (map) {
    unsigned long addr = (((unsigned long)p & MAP_MASK) >> MAP_SHIFT);
    MPage *page;

    page = map + addr;
    if (page->type & MTYPE_BIGBLOCK) {
      if (page->type & MTYPE_CONTINUED)
	return is_marked(page->o.bigblock_start);
      else
	return (page->type & (COLOR_MASK | MTYPE_OLD));
    } else {
      if (page->type & MTYPE_OLD)
	return 1;
      else if (page->type & COLOR_MASK) {
	long offset = ((long)p & MPAGE_MASK) >> 2;

	if (!(page->type & MTYPE_TAGGED))
	  offset -= 1;

	return OFFSET_COLOR(page->u.offsets, offset);
      } else
	return 0;
    }
  }
  
  return 1;
}

/******************************************************************************/

/* Init: set color to white and install offsets (to indicate the
   offset to the start of and allocation block) for marking. */

#if SAFETY
static void **prev_ptr;
static void **prev_var_stack;
#endif

static void init_tagged_mpage(void **p, MPage *page)
{
  OffsetTy offset = 0;
  OffsetArrTy *offsets;
  void **top;

  page->type = (page->type & TYPE_MASK);
  offsets = page->u.offsets;
  top = p + MPAGE_WORDS;

  page->alloc_boundary = MPAGE_WORDS;
  
  while (p < top) {
    Type_Tag tag;
    long size;

    tag = *(Type_Tag *)p;

    if (tag == TAGGED_EOM) {
      /* Remember empty space for prop and compact:  */
      page->alloc_boundary = offset;
      break;
    }

#if ALIGN_DOUBLES
    if (tag == SKIP) {
      OFFSET_SET_SIZE_UNMASKED(offsets, offset, 1);
      offset++;
      p++;
    } else {
#endif

#if SAFETY
      if ((tag < 0) || (tag >= _num_tags_) || !size_table[tag]) {
	fflush(NULL);
	CRASH();
      }
      prev_ptr = p;
      prev_var_stack = GC_variable_stack;
#endif

      size = size_table[tag](p);

      OFFSET_SET_SIZE_UNMASKED(offsets, offset, size);
      offset += size;

#if SAFETY
      if (prev_var_stack != GC_variable_stack) {
	CRASH();
      }
#endif
      
      p += size;
#if ALIGN_DOUBLES
    }
#endif
  }

  inited_pages++;
}

static long the_size;

static void init_untagged_mpage(void **p, MPage *page)
{
  OffsetTy offset = 0;
  OffsetArrTy *offsets;
  void **top;

  page->type = (page->type & TYPE_MASK);
  offsets = page->u.offsets;
  page->alloc_boundary = MPAGE_WORDS;

  top = p + MPAGE_WORDS;

  while (p < top) {
    long size;

    size = *(long *)p + 1;

    if (size == UNTAGGED_EOM) {
      /* Remember empty space for prop:  */
      page->alloc_boundary = offset;
      
      break;
    }

    the_size = size;

    OFFSET_SET_SIZE_UNMASKED(offsets, offset, 0);
    offset += size;

    p += size;
  } 

  inited_pages++;
}

static void init_all_mpages(int young)
{
  MPage *page;

  for (page = first; page; page = page->next) {
    int is_old = (page->age > young);
    void *p = page->block_start;
	
    if (!is_old && !(page->type & MTYPE_MODIFIED)) {
#if GENERATIONS
      if (page->type & MTYPE_BIGBLOCK)
	protect_pages((void *)p, page->u.size, 1);
      else
	protect_pages((void *)p, MPAGE_SIZE, 1);
#endif
      page->type |= MTYPE_MODIFIED;
    }

    if (is_old)
      page->type |= MTYPE_OLD;
    else{
      page->type -= (page->type & MTYPE_OLD);
      young_pages++;
    }
      
    if (!(page->type & MTYPE_INITED)) {
      void *p = page->block_start;

      if (page->type & MTYPE_BIGBLOCK) {
	page->type = (page->type & TYPE_MASK);
	page->type |= MTYPE_INITED;
      } else {
	if (is_old) {
	  if (page->type & MTYPE_TAGGED)
	    init_tagged_mpage((void **)p, page);
	  else
	    init_untagged_mpage((void **)p, page);
	  page->type |= MTYPE_INITED;
	} else {
	  /* Young pages: initialize lazily as needed by `mark'.
	     Not initialized means full page is garbage. */
	  page->type = (page->type & TYPE_MASK);
	}

	if (is_old) {
	  skipped_pages++;
	}
      }
    } else {
       if (is_old) 
	 skipped_pages++;
      /* Clear color flags: */
      page->type = (page->type & TYPE_MASK);
    }

    if (is_old
	&& ((page->refs_age <= young)
	    || (page->type & MTYPE_MODIFIED))
	&& !(page->type & MTYPE_ATOMIC)) {
      /* Offsets inited; need to set gray flag */
      page->type |= GRAY_BIT;
      
      page->gray_next = gray_first;
      gray_first = page;
      
      page->gray_start = 0;
      page->gray_end = page->alloc_boundary - 2;

      if (!(page->type & MTYPE_MODIFIED)) {
#if GENERATIONS
	if (page->type & MTYPE_BIGBLOCK)
	  protect_pages((void *)p, page->u.size, 1);
	else
	  protect_pages((void *)p, MPAGE_SIZE, 1);
#endif
	page->type |= MTYPE_MODIFIED;
      }

      scanned_pages++;
    }
  }
}

/**********************************************************************/

/* Mark: mark a block as reachable. */

#if MARK_STATS
long mark_calls, mark_hits, mark_recalls, mark_colors, mark_one, mark_many, mark_slow;
#endif

void GC_mark(const void *p)
{
  unsigned long g;
  MPage *map;

#if MARK_STATS
  mark_calls++;
#endif

  if ((long)p & 0x1) return;

  g = ((unsigned long)p >> MAPS_SHIFT);

  map = mpage_maps[g];
  if (map) {
    unsigned long addr = (((unsigned long)p & MAP_MASK) >> MAP_SHIFT);
    MPage *page;
    mtype_t type;

    page = map + addr;
    type = page->type;
    if (type && !(type & MTYPE_OLD)) {
#if MARK_STATS
      mark_hits++;
#endif

      if (type & MTYPE_BIGBLOCK) {
	if (type & MTYPE_CONTINUED) {
#if MARK_STATS
	  mark_recalls++;
#endif
	  GC_mark(page->o.bigblock_start);
	} else {
	  if (!(type & COLOR_MASK)) {
#if MARK_STATS
	    mark_colors++;
#endif
	    page->type |= GRAY_BIT;
	    
	    if (!(type & MTYPE_ATOMIC)) {
	      page->gray_next = gray_first;
	      gray_first = page;
	    }
	  }
	}
      } else {
	long offset;
	OffsetArrTy v;
	
	/* Check for lazy initialization: */
	if (!(type & MTYPE_INITED)) {
	  if (type & MTYPE_TAGGED)
	    init_tagged_mpage((void **)page->block_start, page);
	  else
	    init_untagged_mpage((void **)page->block_start, page);
	  page->type |= MTYPE_INITED;
	}

	if (!(type & MTYPE_TAGGED)) {
#if SAFETY
	  if (!((long)p & MPAGE_MASK)) {
	    /* Can't point to beginning of non-tagged block! */
	    CRASH();
	  }
#endif
	  p -= sizeof(void *);
	}

	offset = ((long)p & MPAGE_MASK) >> 2;

#if SAFETY
	if (offset >= page->alloc_boundary) {
	  /* Past allocation region. */
	  CRASH();
	  return;
	}
#endif

	v = OFFSET_COLOR_UNMASKED(page->u.offsets, offset);
	if (!(v & COLOR_MASK)) {
#if MARK_STATS
	  mark_colors++;
#endif

	  if (!(type & (MTYPE_ATOMIC | MTYPE_TAGGED_ARRAY))
	      && (mark_stack_pos < MARK_STACK_MAX)) {
	    page->type |= BLACK_BIT;
	    OFFSET_SET_COLOR_UNMASKED(page->u.offsets, offset, v | BLACK_BIT); /* black can mean on stack */
	    mark_stack[mark_stack_pos] = (void *)p;
	    mark_stack_type[mark_stack_pos++] = type;
	  } else {
	    OFFSET_SET_COLOR_UNMASKED(page->u.offsets, offset, v | GRAY_BIT);

	    if (!(type & GRAY_BIT)) {
	      page->type |= GRAY_BIT;
	      
	      if (!(type & MTYPE_ATOMIC)) {
		page->gray_next = gray_first;
		gray_first = page;
		
		page->gray_start = offset;
		page->gray_end = offset;
	      }
	    } else {
	      if (!(type & MTYPE_ATOMIC)) {
		if (page->gray_start > offset)
		  page->gray_start = offset;
		if (page->gray_end < offset)
		  page->gray_end = offset;
	      }
	    }
	  }
	} else {
#if SAFETY
	  if (!(type & COLOR_MASK)) {
	    CRASH();
	  }
#endif
	}
      }
    }
  }
}

/******************************************************************************/

/* Propoagate: for each marked object, mark objects it
   reaches... until fixpoint. */

static void propagate_tagged_mpage(void **bottom, MPage *page)
{
  OffsetTy offset;
  OffsetArrTy *offsets;
  void **p, **graytop;

  offsets = page->u.offsets;

  offset = page->gray_start;
  p = bottom + offset;
  graytop = bottom + page->gray_end;
  
  while (p <= graytop) {
    OffsetArrTy v;
    Type_Tag tag;
    long size;
    
    tag = *(Type_Tag *)p;

#if ALIGN_DOUBLES
    if (tag != SKIP) {
#endif
      
      v = OFFSET_COLOR_UNMASKED(offsets, offset);
      size = OFFSET_SIZE(offsets, offset);
      if (v & GRAY_BIT) {
	v -= GRAY_BIT;
	v |= BLACK_BIT;
	OFFSET_SET_COLOR_UNMASKED(offsets, offset, v);
	mark_table[tag](p);
      }
	
#if ALIGN_DOUBLES
    } else
      size = 1;
#endif

    p += size;
    offset += size;
  }

#if MARK_STATS
  mark_many++;
#endif

#if MARK_STATS
  if (page->type & GRAY_BIT) {
    mark_slow++;
  }
#endif
}

static void propagate_tagged_whole_mpage(void **p, MPage *page)
{
  void **top;

  top = p + MPAGE_WORDS;
  
  while (p < top) {
    Type_Tag tag;
    long size;

    tag = *(Type_Tag *)p;

    if (tag == TAGGED_EOM) {
      break;
    }

#if ALIGN_DOUBLES
    if (tag == SKIP) {
      p++;
    } else {
#endif

      size = mark_table[tag](p);

      p += size;

#if ALIGN_DOUBLES
    }
#endif
  }
}

static void propagate_array_mpage(void **bottom, MPage *page)
{
  OffsetTy offset = 0;
  OffsetArrTy *offsets;
  void **p, **top;

  offset = page->gray_start;
  p = bottom + offset;
  top = bottom + page->gray_end;
  offsets = page->u.offsets;

  while (p <= top) {
    OffsetArrTy v;
    long size;

    size = *(long *)p + 1;
	
    v = OFFSET_COLOR_UNMASKED(offsets, offset);
    if (v & GRAY_BIT) {
      int i;

      v -= GRAY_BIT;
      v |= BLACK_BIT;
      OFFSET_SET_COLOR_UNMASKED(offsets, offset, v);
      
      for (i = 1; i <= size; i++) {
	gcMARK(p[i]);
      }
    }
    
    p += size;
    offset += size;
  }
}

static void propagate_array_whole_mpage(void **p, MPage *page)
{
  void **top;

  top = p + MPAGE_WORDS;

  while (p < top) {
    long size, i;

    size = *(long *)p + 1;

    if (size == UNTAGGED_EOM) {
      break;
    }

    for (i = 1; i < size; i++) {
      gcMARK(p[i]);
    }

    p += size;
  } 
}

static void propagate_tagged_array_mpage(void **bottom, MPage *page)
{
  OffsetTy offset = 0;
  OffsetArrTy *offsets;
  void **p, **top;

  offset = page->gray_start;
  p = bottom + offset;
  top = bottom + page->gray_end;
  offsets = page->u.offsets;

  while (p <= top) {
    OffsetArrTy v;
    int size;
    
    size = *(long *)p + 1;

    v = OFFSET_COLOR_UNMASKED(offsets, offset);
    if (v & GRAY_BIT) {
      v -= GRAY_BIT;
      v |= BLACK_BIT;
      OFFSET_SET_COLOR_UNMASKED(offsets, offset, v);

      {
	int i, elem_size;
	void **mp = p + 1;
	Type_Tag tag;
	Mark_Proc traverse;
	
	size--;
	tag = *(Type_Tag *)mp;

	traverse = mark_table[tag];
	elem_size = traverse(mp);
	mp += elem_size;
	for (i = elem_size; i < size; i += elem_size, mp += elem_size)
	  traverse(mp);

	size++;
      }
    }
    
    p += size;
    offset += size;
  }
}

static void propagate_tagged_array_whole_mpage(void **p, MPage *page)
{
  void **top;

  top = p + MPAGE_WORDS;

  while (p < top) {
    int i, elem_size, size;
    void **mp;
    Type_Tag tag;
    Mark_Proc traverse;
    
    size = *(long *)p + 1;

    if (size == UNTAGGED_EOM)
      break;
      
    mp = p + 1;
    p += size;
    size--;

    tag = *(Type_Tag *)mp;
      
    traverse = mark_table[tag];
    elem_size = traverse(mp);
    mp += elem_size;
    for (i = elem_size; i < size; i += elem_size, mp += elem_size)
      traverse(mp);
  }
}

static void propagate_xtagged_mpage(void **bottom, MPage *page)
{
  OffsetTy offset = 0;
  OffsetArrTy *offsets;
  void **p, **top;

  offset = page->gray_start;
  p = bottom + offset;
  top = bottom + page->gray_end;
  offsets = page->u.offsets;

  while (p <= top) {
    OffsetArrTy v;
    long size;

    size = *(long *)p + 1;
	
    v = OFFSET_COLOR_UNMASKED(offsets, offset);
    if (v & GRAY_BIT) {
      v -= GRAY_BIT;
      v |= BLACK_BIT;
      OFFSET_SET_COLOR_UNMASKED(offsets, offset, v);
      
      GC_mark_xtagged(p + 1);
    }
    
    p += size;
    offset += size;
  }
}

static void propagate_xtagged_whole_mpage(void **p, MPage *page)
{
  void **top;

  top = p + MPAGE_WORDS;

  while (p < top) {
    long size;

    size = *(long *)p + 1;

    if (size == UNTAGGED_EOM) {
      break;
    }

    GC_mark_xtagged(p + 1);

    p += size;
  } 
}

static void do_bigblock(void **p, MPage *page, int fixup)
{
  if (page->type & MTYPE_ATOMIC)
    return;

  if (page->type & MTYPE_TAGGED) {
    Type_Tag tag;

    tag = *(Type_Tag *)p;

#if SAFETY
    if ((tag < 0) || (tag >= _num_tags_) || !size_table[tag]) {
      CRASH();
    }
    prev_var_stack = GC_variable_stack;
#endif

    if (fixup)
      fixup_table[tag](p);
    else
      mark_table[tag](p);

#if SAFETY
    if (prev_var_stack != GC_variable_stack) {
      CRASH();
    }
#endif
    
    return;
  }

  if (page->type & MTYPE_TAGGED_ARRAY) {
    int i, elem_size, size;
    void **mp = p;
    Type_Tag tag;
    Mark_Proc mark;

    size = page->u.size >> 2;
    tag = *(Type_Tag *)mp;

    if (fixup)
      mark = fixup_table[tag];
    else
      mark = mark_table[tag];
    elem_size = mark(mp);      
    mp += elem_size;
    for (i = elem_size; i < size; i += elem_size, mp += elem_size)
      mark(mp);

    return;
  }

  if (page->type & MTYPE_ARRAY) {
    int i;
    long size = page->u.size >> 2;
    
    if (fixup) {
      for (i = 0; i < size; i++, p++) {
	gcFIXUP(*p);
      }
    } else {
      for (i = 0; i < size; i++, p++) {
	gcMARK(*p);
      }
    }
  }

  if (page->type & MTYPE_XTAGGED) {
    if (fixup)
      GC_fixup_xtagged(p);
    else
      GC_mark_xtagged(p);
  }
}

static void propagate_all_mpages()
{
  MPage *page;
  void *p;

  while (gray_first || mark_stack_pos) {
    iterations++;

    if (mark_stack_pos > mark_maxdepth)
      mark_maxdepth = mark_stack_pos;
    mark_stackcount += mark_stack_pos;

    while (mark_stack_pos) {
      unsigned short type;
      
      p = mark_stack[--mark_stack_pos];
      type = mark_stack_type[mark_stack_pos];
      if (type & MTYPE_TAGGED) {
	Type_Tag tag;
	tag = *(Type_Tag *)p;
	
#if ALIGN_DOUBLES
	if (tag != SKIP) {
#endif
	  
#if SAFETY
	  if ((tag < 0) || (tag >= _num_tags_) || !mark_table[tag]) {
	    CRASH();
	  }
#endif
	  
	  mark_table[tag](p);
	  
#if ALIGN_DOUBLES
	}
#endif
      } else if (type & MTYPE_XTAGGED) {
	GC_mark_xtagged((void **)p + 1);
      } else {
	long size, i;

	size = *(long *)p;
	
	for (i = 1; i <= size; i++) {
	  gcMARK(((void **)p)[i]);
	}
      }
    }

    if (gray_first) {
      page = gray_first;
      gray_first = page->gray_next;
      
      page->type = ((page->type & TYPE_MASK) | BLACK_BIT);
      if (!(page->type & MTYPE_ATOMIC)) {
	p = page->block_start;
	
	if (page->type & MTYPE_BIGBLOCK) {
	  if (!(page->type & MTYPE_CONTINUED))
	    do_bigblock((void **)p, page, 0);
	} else if (page->type & MTYPE_TAGGED) {
	  if (page->type & MTYPE_OLD)
	    propagate_tagged_whole_mpage((void **)p, page);
	  else
	    propagate_tagged_mpage((void **)p, page);
	} else if (page->type & MTYPE_TAGGED_ARRAY) {
	  if (page->type & MTYPE_OLD)
	    propagate_tagged_array_whole_mpage((void **)p, page);
	  else
	    propagate_tagged_array_mpage((void **)p, page);
	} else if (page->type & MTYPE_XTAGGED) {
	  if (page->type & MTYPE_OLD)
	    propagate_xtagged_whole_mpage((void **)p, page);
	  else
	    propagate_xtagged_mpage((void **)p, page);
	} else {
	  if (page->type & MTYPE_OLD)
	    propagate_array_whole_mpage((void **)p, page);
	  else
	    propagate_array_mpage((void **)p, page);
	}
      }
    }      
  }
}

/**********************************************************************/

/* Compact: compact objects, setting page color to white if all
   objects are moved elsewhere */

static void compact_tagged_mpage(void **p, MPage *page)
{
  int to_near = 0, set_age = 0;
  OffsetTy offset, dest_offset, dest_start_offset;
  OffsetArrTy *offsets;
  void **dest, **startp;
  void **top;

  offsets = page->u.offsets;

  top = p + page->alloc_boundary;

  startp = p;
  dest = tagged.compact_to;
  dest_offset = dest_start_offset = tagged.compact_to_offset;
  offset = 0;

  page->o.compact_to = dest;
  page->compact_boundary = MPAGE_WORDS;
  
  while (p < top) {
    long size;

    size = OFFSET_SIZE(offsets, offset);

    if (OFFSET_COLOR(offsets, offset)) {
#if ALIGN_DOUBLES
#define PLUS_ALIGNMENT + alignment
      long alignment;
      if (!(size & 0x1) && (dest_offset & 0x1))
	alignment = 1;
      else
	alignment = 0;
#else
# define PLUS_ALIGNMENT /**/      
#endif
      
      if (dest_offset + size PLUS_ALIGNMENT > MPAGE_WORDS) {
	/* Set end of allocation area in previous page: */
	if (dest_offset < MPAGE_WORDS)
	  *(Type_Tag *)(dest + dest_offset) = TAGGED_EOM;
	
#if NOISY
	printf("t: %lx [0,%d] -> %lx [%d,%d]\n", 
	       (long)startp, offset,
	       (long)dest, dest_start_offset, dest_offset);
#endif

	dest_offset = 0;
	dest = startp;
	to_near = 1;
	if (set_age) {
	  page->compact_boundary = offset;
	  tagged.compact_page->age = page->age;
	  tagged.compact_page->refs_age = page->age;
	} else
	  /* Haven't moved anything; set boundary to 0 to indicate this */
	  page->compact_boundary = 0;
      } else {
	set_age = 1;
#if ALIGN_DOUBLES
	if (alignment) {
	  *(Type_Tag *)(dest + dest_offset) = SKIP;
	  dest_offset++;
	}
#endif
      }
      
      if (!to_near || (dest_offset != offset)) {
	if (to_near)
	  memmove(dest + dest_offset, p, size << 2);
	else
	  memcpy(dest + dest_offset, p, size << 2);
      }
      
      OFFSET_SET_SIZE_UNMASKED(offsets, offset, dest_offset);
      offset += size;
      dest_offset += size;
 
      p += size;
    } else {
      p += size;
      offset += size;
    }
  }

  if (to_near)
    tagged.compact_page = page;
  tagged.compact_to = dest;
  tagged.compact_to_offset = dest_offset;

  if (!to_near) {
    /* Nothing left in here. Reset color to white: */
    page->type = (page->type & TYPE_MASK);
#if NOISY
    printf("t: %lx [all=%d] -> %lx [%d,%d]\n", 
	   (long)startp, offset,
	   (long)dest, dest_start_offset, dest_offset);
#endif
  }
}

static void compact_untagged_mpage(void **p, MPage *page)
{
  int to_near = 0, set_age = 0;
  OffsetTy offset = 0, dest_offset;
  OffsetArrTy *offsets;
  void **dest, **startp, **top;
  MSet *set;

  offsets = page->u.offsets;

  startp = p;
  if (page->type & MTYPE_TAGGED_ARRAY) { 
    set = &tagged_array;
  } else if (page->type & MTYPE_ATOMIC) {
    set = &atomic;
  } else if (page->type & MTYPE_XTAGGED) {
    set = &xtagged;
  } else {
    set = &array;
  }

  dest = set->compact_to;
  dest_offset = set->compact_to_offset;

  page->o.compact_to = dest;
  page->compact_boundary = MPAGE_WORDS;

  top = p + MPAGE_WORDS;

  while (p < top) {
    long size;
      
    size = *(long *)p + 1;
    
    if (size == UNTAGGED_EOM)
      break;

#if SAFETY
    if (size >= BIGBLOCK_MIN_SIZE) {
      CRASH();
    }
#endif

    if (OFFSET_COLOR(offsets, offset)) {
#if ALIGN_DOUBLES
      long alignment;
      if ((size & 0x1) && !(dest_offset & 0x1))
	alignment = 1;
      else
	alignment = 0;
#endif
	
      if ((long)dest_offset + size PLUS_ALIGNMENT > MPAGE_WORDS) {
	/* Set end of allocation area in previous page: */
	if (dest_offset < MPAGE_WORDS)
	  *(long *)(dest + dest_offset) = UNTAGGED_EOM - 1;
	
#if NOISY
	printf("u: %lx -> %lx [%d]\n", (long)startp, (long)dest, offset);
#endif

	dest_offset = 0;
	dest = startp;
	to_near = 1;
#if ALIGN_DOUBLES
	if (size & 0x1) {
	  dest[0] = 0;
	  dest_offset++;
	}
#endif

	if (set_age) {
	  page->compact_boundary = offset;
	  set->compact_page->age = page->age;
	  set->compact_page->refs_age = page->age;
	} else
	  /* Haven't moved anything; set boundary to 0 to indicate this */
	  page->compact_boundary = 0;
      } else {
	set_age = 1;
#if ALIGN_DOUBLES
	if (alignment) {
	  dest[dest_offset] = 0;
	  dest_offset++;
	}
#endif
      }

      if (!to_near || (dest_offset != offset)) {
	if (to_near)
	  memmove(dest + dest_offset, p, size << 2);
	else
	  memcpy(dest + dest_offset, p, size << 2);
      }
      
      OFFSET_SET_SIZE_UNMASKED(offsets, offset, dest_offset + 1);
      offset += size;
      dest_offset += size;
 
      p += size;
    } else {
      p += size;
      offset += size;
    }
  }

  set->compact_to = dest;
  set->compact_to_offset = dest_offset;
  if (to_near)
    set->compact_page = page;

  if (!to_near) {
    /* Nothing left in here. Reset color to white: */
    page->type = (page->type & TYPE_MASK);
#if NOISY
    printf("u: %lx -> %lx [all]\n", (long)startp, (long)dest);
#endif
  }
}

static void compact_all_mpages()
{
  MPage *page;
  int i;

  for (i = 0; i < NUM_SETS; i++)
    sets[i]->compact_to_offset = MPAGE_WORDS;

  for (page = first; page; page = page->next) {
    if (!(page->type & (MTYPE_BIGBLOCK | MTYPE_OLD))) {
      if (page->type & COLOR_MASK) {
	void *p;
	
	page->type -= (page->type & MTYPE_INITED);
	p = page->block_start;
	
	if (page->type & MTYPE_TAGGED)
	  compact_tagged_mpage((void **)p, page);
	else
	  compact_untagged_mpage((void **)p, page);
      } else {
	/* Set compact_boundar to 0 to indicate no moves: */
	page->compact_boundary = 0;
#if NOISY
	printf("x: %lx\n", (long)page->block_start);
#endif
      }
    }
  }

  if (tagged.compact_to_offset < MPAGE_WORDS)
    *(Type_Tag *)(tagged.compact_to + tagged.compact_to_offset) = TAGGED_EOM;
  for (i = 1; i < NUM_SETS; i++) {
    if (sets[i]->compact_to_offset < MPAGE_WORDS)
      *(long *)(sets[i]->compact_to + sets[i]->compact_to_offset) = UNTAGGED_EOM - 1;
  }
}

/**********************************************************************/

#if USE_FREELIST

static void freelist_tagged_mpage(void **p, MPage *page)
{
  OffsetTy offset;
  OffsetArrTy *offsets;
  void **top;
  long on_at_start = on_free_list;

  offsets = page->u.offsets;

  top = p + page->alloc_boundary;

  offset = 0;

  while (p < top) {
    long size;
    OffsetArrTy v;

    size = OFFSET_SIZE(offsets, offset);

    v = OFFSET_COLOR_UNMASKED(offsets, offset);
    if (!(v & COLOR_MASK)) {
#if ALIGN_DOUBLES
      if (size > 1) {
#endif
	/* HACK! This relies on both Type_Tag and OffsetTy being `short' */
	((Type_Tag *)p)[0] = gc_on_free_list_tag;
	((Type_Tag *)p)[1] = size;
	p[1] = tagged.free_lists[size];
	tagged.free_lists[size] = (void *)p;
	on_free_list += size;
#if ALIGN_DOUBLES
      }
#endif
    } else {
      /* Remove color: */
      v -= (v & (GRAY_BIT | BLACK_BIT));
      OFFSET_SET_COLOR_UNMASKED(offsets, offset, v);
    }

    p += size;
    offset += size;
  }

  if (on_at_start != on_free_list)
    page->age = page->refs_age = -1;  /* will be promoted to 0 */
}

static void freelist_untagged_mpage(void **p, MPage *page)
{
  OffsetTy offset = 0;
  OffsetArrTy *offsets;
  void **free_lists, **top;
  long on_at_start = on_free_list;

  if (page->type & MTYPE_TAGGED_ARRAY) { 
    free_lists = tagged_array.free_lists;
  } else if (page->type & MTYPE_ATOMIC) {
    free_lists = atomic.free_lists;
  } else if (page->type & MTYPE_XTAGGED) {
    free_lists = xtagged.free_lists;
  } else {
    free_lists = array.free_lists;
  }

  offsets = page->u.offsets;
  top = p + MPAGE_WORDS;

  while (p < top) {
    long size;
    OffsetArrTy v;

    size = *(long *)p + 1;
    
    if (size == UNTAGGED_EOM)
      break;

#if SAFETY
    if (size >= BIGBLOCK_MIN_SIZE) {
      CRASH();
    }
#endif

    v = OFFSET_COLOR_UNMASKED(offsets, offset);
    if (!(v & COLOR_MASK)) {
#if ALIGN_DOUBLES
      if (size > 1) {
#endif
	p[1] = free_lists[size-1];
	free_lists[size-1] = (void *)(p + 1);
	on_free_list += (size-1);
#if ALIGN_DOUBLES
      }
#endif
    } else {
      /* Remove color: */
      v -= (v & (GRAY_BIT | BLACK_BIT));
      OFFSET_SET_COLOR_UNMASKED(offsets, offset, v);
    }
     
    p += size;
    offset += size;
  }

  if (on_at_start != on_free_list)
    page->age = page->refs_age = -1; /* will be promoted to 0 */
}

static void freelist_all_mpages(int young)
{
  MPage *page;

  for (page = first; page; page = page->next) {
    if (page->type & COLOR_MASK) {
      if (page->refs_age <= young)
	page->refs_age = -1; /* best we can assume */
      if (!(page->type & (MTYPE_BIGBLOCK | MTYPE_OLD))) {
	void *p;
	
	p = page->block_start;
	
	if (page->type & MTYPE_TAGGED)
	  freelist_tagged_mpage((void **)p, page);
	else
	  freelist_untagged_mpage((void **)p, page);
      }
    }
  }
}

#endif

/**********************************************************************/

/* Fixup: translate an old address to a new one, and note age of
   youngest referenced page */

static int min_referenced_page_age;

void GC_fixup(void *pp)
{
  void *p = *(void **)pp;
  unsigned long g;
  MPage *map;

  if ((long)p & 0x1) return;
  g = ((unsigned long)p >> MAPS_SHIFT);

  map = mpage_maps[g];
  if (map) {
    unsigned long addr = (((unsigned long)p & MAP_MASK) >> MAP_SHIFT);
    MPage *page;

    page = map + addr;
    if (page->type) {
      if (page->compact_to_age < min_referenced_page_age)
	min_referenced_page_age = page->compact_to_age;

      if (!(page->type & (MTYPE_OLD | MTYPE_BIGBLOCK))) {
	long offset = ((long)p & MPAGE_MASK) >> 2;
	OffsetTy v;
	void *r;

	if (!(page->type & MTYPE_TAGGED)) {
#if SAFETY
	  if (!offset) {
	    /* Can't point to beginning of non-tagged block! */
	    CRASH();
	  }
#endif
	  offset--;
	}


	v = OFFSET_SIZE(page->u.offsets, offset);
	if (offset < page->compact_boundary)
	  r = (void *)(page->o.compact_to + v);
	else
	  r = (void *)(((long)p & MPAGE_START) + ((long)v << 2));

#if SEARCH
	if (r == search_for)
	  stop();
#endif

#if SAFETY
	if (!(find_page(r)->type & COLOR_MASK)) {
	  CRASH();
	}
#endif

	if (r != p)
	  *(void **)pp = r;
      }
    }
  }
}

/**********************************************************************/

/* set compact_to_age field of a page: */

void reverse_propagate_new_age(void)
{
  MPage *page;

  for (page = first; page; page = page->next) {
    if (!(page->type & (MTYPE_BIGBLOCK | MTYPE_OLD))) {
      if (page->compact_boundary > 0) {
	MPage *page_to;
	page_to = find_page(page->o.compact_to);
	if (page_to->age < page->age)
	  page->compact_to_age = page_to->age;
	else
	  page->compact_to_age = page->age;
      } else
	page->compact_to_age = page->age;
    } else
      page->compact_to_age = page->age;
  }
}

/**********************************************************************/

/* Fixup: fixup addresses in all readable objects */

static void fixup_tagged_mpage(void **p, MPage *page)
{
  void **top;

  top = p + MPAGE_WORDS;

  while (p < top) {
    Type_Tag tag;
    long size;

    tag = *(Type_Tag *)p;

    if (tag == TAGGED_EOM)
      break;

#if ALIGN_DOUBLES
    if (tag == SKIP) {
      p++;
    } else {
#endif

#if SAFETY
      if ((tag < 0) || (tag >= _num_tags_) || !fixup_table[tag]) {
	fflush(NULL);
	CRASH();
      }
      prev_var_stack = prev_ptr;
      prev_ptr = p;
#endif

      size = fixup_table[tag](p);

      p += size;

#if ALIGN_DOUBLES
    }
#endif
  }
}

static void fixup_array_mpage(void **p, MPage *page)
{
  void **top;

  top = p + MPAGE_WORDS;

  while (p < top) {
    long size;

    size = *(long *)p + 1;

    if (size == UNTAGGED_EOM)
      break;

#if SAFETY
    if (size >= BIGBLOCK_MIN_SIZE) {
      CRASH();
    }
#endif

    for (p++; --size; p++) {
      gcFIXUP(*p);
    }
  }
}

static void fixup_tagged_array_mpage(void **p, MPage *page)
{
  void **top;

  top = p + MPAGE_WORDS;

  while (p < top) {
    long size;
    int i, elem_size;
    void **mp;
    Type_Tag tag;
    Fixup_Proc traverse;

    size = *(long *)p + 1;

    if (size == UNTAGGED_EOM)
      break;

    mp = p + 1;
    p += size;
    size--;

#if ALIGN_DOUBLES
    if (size) {
#endif
      tag = *(Type_Tag *)mp;

      traverse = fixup_table[tag];
      elem_size = traverse(mp);
      mp += elem_size;
      for (i = elem_size; i < size; i += elem_size, mp += elem_size)
	traverse(mp);
#if ALIGN_DOUBLES
    }
#endif
  }
}

static void fixup_xtagged_mpage(void **p, MPage *page)
{
  void **top;

  top = p + MPAGE_WORDS;

  while (p < top) {
    long size;

    size = *(long *)p + 1;

    if (size == UNTAGGED_EOM)
      break;

#if SAFETY
    if (size >= BIGBLOCK_MIN_SIZE) {
      CRASH();
    }
#endif

    GC_fixup_xtagged(p + 1);

    p += size;
  }
}

static void fixup_all_mpages()
{
  MPage *page;

  for (page = first; page; page = page->next) {
    if (page->type & COLOR_MASK) {
      if (!(page->type & MTYPE_ATOMIC)) {
	void *p;

	scanned_pages++;
	min_referenced_page_age = page->refs_age;
	p = page->block_start;

#if NOISY
	fprintf(stderr, "Fixup %lx\n", (long)p);
#endif

	if (page->type & MTYPE_BIGBLOCK) {
	  do_bigblock((void **)p, page, 1);
	} else if (page->type & MTYPE_TAGGED)
	  fixup_tagged_mpage((void **)p, page);
	else if (page->type & MTYPE_TAGGED_ARRAY)
	  fixup_tagged_array_mpage((void **)p, page);
	else if (page->type & MTYPE_XTAGGED)
	  fixup_xtagged_mpage((void **)p, page);
	else 
	  fixup_array_mpage((void **)p, page);

	page->refs_age = min_referenced_page_age;
      }
    }
  }
}

/**********************************************************************/

/* Free: release unused pages. */

static void free_unused_mpages()
{
  MPage *page, *next;
  memory_in_use = 0;

  for (page = first; page; page = next) {
    next = page->next;
    if (!(page->type & COLOR_MASK)
	&& !(page->type & MTYPE_OLD)) {
      void *p;
      p = page->block_start;

      if (page->prev)
	page->prev->next = page->next;
      else
	first = page->next;
      if (page->next)
	page->next->prev = page->prev;
      else
	last = page->prev;

      if (page->type & MTYPE_BIGBLOCK) {
#if NOISY
	fprintf(stderr, "Free %lx - %lx\n", (long)p,
		(long)p + page->u.size);
#endif

	free_pages((void *)p, page->u.size);
	
	{
	 long s = page->u.size;
	 unsigned long i = ((unsigned long)p >> MAPS_SHIFT);
	 unsigned long j = (((unsigned long)p & MAP_MASK) >> MAP_SHIFT);
	 while (s > MPAGE_SIZE) {
	   s -= MPAGE_SIZE;
	   j++;
	   if (j == MAP_SIZE) {
	     j = 0;
	     i++;
	   }
	   mpage_maps[i][j].type = 0;
	 }
	}
      } else {
#if NOISY
	fprintf(stderr, "Free %lx\n", (long)p);
#endif
	free_pages((void *)p, MPAGE_SIZE);
	free_pages(page->u.offsets, OPAGE_SIZE);
      }
      
      if (page->type & MTYPE_INITED)
	scanned_pages++;
      
      page->type = 0;
      skipped_pages++;
    } else {
      if (page->type & MTYPE_BIGBLOCK) {
	if (!(page->type & MTYPE_CONTINUED))
	  memory_in_use += page->u.size;
      } else
	memory_in_use += MPAGE_SIZE;
    }
  }
}

void promote_all_ages()
{
  MPage *page;

  for (page = first; page; page = page->next) {
    if (page->age < 15)
      page->age++;
    if (page->refs_age < 15)
      page->refs_age++;
  }
}


void protect_old_mpages()
{
  MPage *page;

#if GENERATIONS
  for (page = first; page; page = page->next) {
    if (page->age && !(page->type & MTYPE_ATOMIC)) {
      void *p;
      
      if (page->type & MTYPE_MODIFIED) {
	page->type -= MTYPE_MODIFIED;
      
	p = page->block_start;
	if (page->type & MTYPE_BIGBLOCK)
	  protect_pages((void *)p, page->u.size, 0);
	else 
	  protect_pages((void *)p, MPAGE_SIZE, 0);
      }
    }
  }
#endif
}


static void designate_modified(void *p)
{
  unsigned long g = ((unsigned long)p >> MAPS_SHIFT);
  MPage *map;

  map = mpage_maps[g];
  if (map) {
    unsigned long addr = (((unsigned long)p & MAP_MASK) >> MAP_SHIFT);
    MPage *page;

    page = map + addr;
    if (page->type) {
      if (page->type & MTYPE_CONTINUED) {
	designate_modified(page->o.bigblock_start);
	num_seg_faults++;
	return;
      } else if (page->age) {
	page->type |= MTYPE_MODIFIED;
	p = (void *)((long)p & MPAGE_START);
	if (page->type & MTYPE_BIGBLOCK)
	  protect_pages(p, page->u.size, 1);
	else
	  protect_pages(p, MPAGE_SIZE, 1);
	num_seg_faults++;
	return;
      }

      fprintf(stderr, "Seg fault (internal error) at %lx [%ld]\n", 
	      (long)p, num_seg_faults);
      _exit(-1);
    }
  }

  
  fprintf(stderr, "Access on unmapped page at %lx [%ld]\n", 
	  (long)p, num_seg_faults);
  _exit(-1);
}

/**********************************************************************/

/* Linux signal handler: */
#if defined(linux)
# include <signal.h>
void fault_handler(int sn, struct sigcontext sc)
{
  designate_modified((void *)sc.cr2);
  signal(SIGSEGV, (void (*)(int))fault_handler);
}
# define NEED_SIGSEGV
#endif

/* FreeBSD signal handler: */
#if defined(__FreeBSD__)
# include <signal.h>
void fault_handler(int sn, int code, struct sigcontext *sc, char *addr)
{
  designate_modified(addr);
}
# define NEED_SIGBUS
#endif

/* Solaris signal handler: */
#if defined(sun)
# include <signal.h>
void fault_handler(int sn, struct siginfo *si, void *ctx)
{
  designate_modified(si->si_addr);
}
# define NEED_SIGACTION
#endif

/**********************************************************************/

#if SAFETY
static void **o_var_stack, **oo_var_stack;
#endif
static int stack_depth;

void GC_mark_variable_stack(void **var_stack,
			    long delta,
			    void *limit)
{
  stack_depth = 0;
  while (var_stack) {
    long size;
    void ***p;

    var_stack = (void **)((char *)var_stack + delta);
    if (var_stack == limit)
      return;

    size = *(long *)(var_stack + 1);

#if SAFETY
    oo_var_stack = o_var_stack;
    o_var_stack = var_stack;
#endif

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
	  gcMARK(*a);
	  a++;
	}
      } else {
	void **a = *p;
	a = (void **)((char *)a + delta);
	gcMARK(*a);
      }
      p++;
    }

#if 0
    if (*var_stack && ((unsigned long)*var_stack < (unsigned long)var_stack)) {
      printf("bad %d\n", stack_depth);
      CRASH();
    }
#endif

    var_stack = *var_stack;
    stack_depth++;
  }
}

void GC_fixup_variable_stack(void **var_stack,
			     long delta,
			     void *limit)
{
  stack_depth = 0;
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
	  gcFIXUP(*a);
	  a++;
	}
      } else {
	void **a = *p;
	a = (void **)((char *)a + delta);
	gcFIXUP(*a);
      }
      p++;
    }

    var_stack = *var_stack;
    stack_depth++;
  }
}

#if SAFETY
void check_variable_stack()
{
  void **limit, **var_stack;

  limit = (void **)(GC_get_thread_stack_base
		    ? GC_get_thread_stack_base()
		    : stack_base);

  var_stack = GC_variable_stack;

  while (var_stack) {
    if (var_stack == limit)
      return;

    if (*var_stack && ((unsigned long)*var_stack <= (unsigned long)var_stack))
      CRASH();

    var_stack = *var_stack;
  }
}
#endif

#if 0
# define GETTIME() ((long)scheme_get_milliseconds())
#else
extern long scheme_get_process_milliseconds();
# define GETTIME() ((long)scheme_get_process_milliseconds())
#endif

#if TIME
# define PRINTTIME(x) fprintf x
# define STDERR stderr
static long started, rightnow, old;
# define INITTIME() (started = GETTIME())
# define GETTIMEREL() (rightnow = GETTIME(), old = started, started = rightnow, rightnow - old)
#else
# define INITTIME() /* empty */
# define PRINTTIME(x) /* empty */
#endif

static int initialized;

static void do_roots(int fixup)
{
  ImmobileBox *ib;
  int i;

  for (i = 0; i < roots_count; i += 2) {
    void **s = (void **)roots[i];
    void **e = (void **)roots[i + 1];
    
    while (s < e) {
      if (fixup) {
	gcFIXUP(*s);
      } else {
	gcMARK(*s);
      }
      s++;
    }
  }

  if (fixup)
    GC_fixup_variable_stack(GC_variable_stack,
			    0,
			    (void *)(GC_get_thread_stack_base
				     ? GC_get_thread_stack_base()
				     : stack_base));
  else
    GC_mark_variable_stack(GC_variable_stack,
			   0,
			   (void *)(GC_get_thread_stack_base
				    ? GC_get_thread_stack_base()
				    : stack_base));

  /* Do immobiles: */
  for (ib = immobile; ib; ib = ib->next) {
    if (fixup) {
      gcFIXUP(ib->p);
    } else {
      gcMARK(ib->p);
    }
  }
}

static void gcollect(int full)
{
  int did_fnls;
  GC_Weak_Box *wb;
  GC_Weak_Array *wa;
#if TIME
  struct rusage pre, post;
#endif
  int young;
  int compact;
  int i;

  INITTIME();
  PRINTTIME((STDERR, "gc: << start with %ld [%d]: %ld\n", 
	     memory_in_use, cycle_count + 1, GETTIMEREL()));

  cycle_count++;

  if (!initialized) {
    GC_register_traversers(weak_box_tag, size_weak_box, mark_weak_box, fixup_weak_box);
    GC_register_traversers(gc_weak_array_tag, size_weak_array, mark_weak_array, fixup_weak_array);
    GC_register_traversers(gc_finalization_tag, size_finalizer, mark_finalizer, fixup_finalizer);
    GC_register_traversers(gc_finalization_weak_link_tag, size_finalizer_weak_link, mark_finalizer_weak_link, fixup_finalizer_weak_link);
#if USE_FREELIST
    GC_register_traversers(gc_on_free_list_tag, size_on_free_list, size_on_free_list, size_on_free_list);
#endif
    GC_add_roots(&fnls, (char *)&fnls + sizeof(fnls) + 1);
    GC_add_roots(&fnl_weaks, (char *)&fnl_weaks + sizeof(fnl_weaks) + 1);
    GC_add_roots(&run_queue, (char *)&run_queue + sizeof(run_queue) + 1);
    GC_add_roots(&last_in_queue, (char *)&last_in_queue + sizeof(last_in_queue) + 1);
    GC_add_roots(&park, (char *)&park + sizeof(park) + 1);

    sets[0] = &tagged;
    sets[1] = &array;
    sets[2] = &tagged_array;
    sets[3] = &xtagged;
    sets[4] = &atomic;

    initialized = 1;

#if GENERATIONS
# ifdef NEED_SIGSEGV
    signal(SIGSEGV, (void (*)(int))fault_handler);
# endif
# ifdef NEED_SIGBUS
    signal(SIGBUS, (void (*)(int))fault_handler);
# endif
# ifdef NEED_SIGACTION
    {
      struct sigaction act, oact;
      act.sa_sigaction = fault_handler;
      sigemptyset(&act.sa_mask);
      act.sa_flags = SA_SIGINFO;
      sigaction(SIGSEGV, &act, &oact);
    }
# endif
#endif
  }

  if (tagged.low < tagged.high)
    *(Type_Tag *)tagged.low = TAGGED_EOM;
  for (i = 1; i < NUM_SETS; i++) {
    if (sets[i]->low < sets[i]->high)
      *(long *)sets[i]->low = UNTAGGED_EOM - 1;
  }

  weak_boxes = NULL;
  weak_arrays = NULL;
  did_fnls = 0;

  gray_first = NULL;

  if (GC_collect_start_callback)
    GC_collect_start_callback();

#if TIME
  getrusage(RUSAGE_SELF, &pre);
#endif

  sort_and_merge_roots();

  during_gc = 1;

  /******************** Init ****************************/

  skipped_pages = 0;
  scanned_pages = 0;
  young_pages = 0;
  inited_pages = 0;

  if (full)
    young = 15;
  else if (!(cycle_count & 0xF))
    young = 15;
  else if (!(cycle_count & 0x7))
    young = 7;
  else if (!(cycle_count & 0x3))
    young = 3;
  else if (!(cycle_count & 0x1))
    young = 1;
  else
    young = 0;

#if !GENERATIONS
  young = 15;
#endif

#if USE_FREELIST && (COMPACTING == SELECTIVELY_COMPACT)
  if (full)
    compact = 1;
  else {
    /* Remaining free list items few enough? */
    if (((float)(on_free_list << 2) / memory_in_use) < COMPACT_THRESHOLD)
      compact = 0;
    else
      compact = 1;
  }
#else
# if (COMPACTING == ALWAYS_COMPACT) || !USE_FREELIST
  compact = 1;
# endif
# if (COMPACTING == NEVER_COMPACT)
  compact = 0;
# endif
#endif

  init_all_mpages(young);

  PRINTTIME((STDERR, "gc: init %s [freelist=%f] (young:%d skip:%d scan:%d init:%d): %ld\n", 
	     compact ? "cmpct" : "frlst", (double)(FREE_LIST_DELTA << 2) / memory_in_use,
	     young_pages, skipped_pages, scanned_pages, inited_pages,
	     GETTIMEREL()));

  /************* Mark and Propagate *********************/

  inited_pages = 0;

  do_roots(0);

#if TIME
  getrusage(RUSAGE_SELF, &post);
#endif

  PRINTTIME((STDERR, "gc: roots (init:%d deep:%d) [%ld faults]: %ld\n", 
	     inited_pages, stack_depth, post.ru_minflt - pre.ru_minflt,
	     GETTIMEREL()));

  iterations = 0;
  mark_stackcount = 0;
  mark_maxdepth = 0;

#if MARK_STATS
  mark_calls = mark_hits = mark_recalls = mark_colors = mark_one = mark_many = mark_slow = 0;
#endif

  /* Propagate, loop to do finalization */
  while (1) { 

    /* Propagate all marks. */
    propagate_all_mpages();
    
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
	      /* Not yet marked. Mark it and enqueue it. */
	      gcMARK(f->p);

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
	    gcMARK(wl->saved);
	  }
	  *(void **)(wp + wl->offset) = wl->saved;
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
	  wl->saved = *(void **)(wp + wl->offset);
	  *(void **)(wp + wl->offset) = NULL;
	}

	/* Mark content of not-yet-marked finalized objects,
	   but don't mark the finalized objects themselves. */	
	for (f = fnls; f; f = f->next) {
	  if (f->eager_level == 3) {
	    if (!is_marked(f->p)) {
	      /* Not yet marked. Mark content. */
	      if (f->tagged) {
		Type_Tag tag = *(Type_Tag *)f->p;
#if SAFETY
		if ((tag < 0) || (tag >= _num_tags_) || !mark_table[tag]) {
		  CRASH();
		}
#endif
		mark_table[tag](f->p);
	      } else {
		GC_mark_xtagged(f->p);
	      }
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
	  gcMARK(f->p);
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

  PRINTTIME((STDERR, "gc: mark (init:%d cycle:%ld stkcnt:%ld stkdp:%ld)"
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
	*(wb->secondary_erase + wb->soffset) = NULL;
	wb->secondary_erase = NULL;
      }
    }
    wb = wb->next;
  }

  /* Do weak arrays: */
  wa = weak_arrays;
  while (wa) {
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

  /******************************************************/

#if USE_FREELIST
  {
    int j;

    for (j = 0; j < NUM_SETS; j++) {
      void **free_lists = sets[j]->free_lists;
      for (i = 0; i < FREE_LIST_ARRAY_SIZE; i++)
	free_lists[i] = NULL;
    }

    on_free_list = 0;
  }
#endif

  if (compact)
    compact_all_mpages();
#if USE_FREELIST
  else
    freelist_all_mpages(young);
#endif

#if TIME
  getrusage(RUSAGE_SELF, &post);
#endif

  PRINTTIME((STDERR, "gc: %s [%ld faults]: %ld\n", 
	     compact ? "compact" : "freelist",
	     post.ru_minflt - pre.ru_minflt,
	     GETTIMEREL()));

  /******************************************************/

  promote_all_ages();

  if (compact) {
    for (i = 0; i < NUM_SETS; i++) {
      sets[i]->malloc_page = sets[i]->compact_page;
      sets[i]->low = sets[i]->compact_to + sets[i]->compact_to_offset;
      sets[i]->high = sets[i]->compact_to + MPAGE_WORDS;
      if (sets[i]->compact_to_offset < MPAGE_WORDS) {
	sets[i]->compact_page->age = 0;
	sets[i]->compact_page->refs_age = 0;
	sets[i]->compact_page->type |= MTYPE_MODIFIED;
      }
    }

    reverse_propagate_new_age();
  } else {
    for (i = 0; i < NUM_SETS; i++) {
      if (sets[i]->malloc_page) {
	if (!(sets[i]->malloc_page->type & COLOR_MASK)) {
	  sets[i]->malloc_page= NULL;
	  sets[i]->low = sets[i]->high = STARTING_PLACE;
	} else
	  sets[i]->malloc_page->type -= (sets[i]->malloc_page->type & MTYPE_INITED);
      }
    }
  }

  /******************************************************/

  if (compact) {
    scanned_pages = 0;
    
    do_roots(1);
    fixup_all_mpages();
    
#if TIME
    getrusage(RUSAGE_SELF, &post);
#endif
    
    PRINTTIME((STDERR, "gc: fixup (%d) [%ld faults]: %ld\n", 
	       scanned_pages,  post.ru_minflt - pre.ru_minflt,
	       GETTIMEREL()));
  }

  /******************************************************/
  
  skipped_pages = scanned_pages = 0;

  free_unused_mpages();

  protect_old_mpages();
  
  gc_threshold = (long)(GROW_FACTOR * (memory_in_use - FREE_LIST_DELTA));

  if (compact) {
    for (i = 0; i < (NUM_SETS - 1); i++) {
      if (sets[i]->compact_to_offset < MPAGE_WORDS)
	memset(sets[i]->low, 0, (sets[i]->high - sets[i]->low) << 2);
    }
  }

#if TIME
  getrusage(RUSAGE_SELF, &post);
#endif

  PRINTTIME((STDERR, "gc: done with %ld (%d/%d) [%ld faults]: %ld >>\n",
	     memory_in_use, scanned_pages, skipped_pages, post.ru_minflt - pre.ru_minflt,
	     GETTIMEREL()));

  during_gc = 0;

  if (GC_collect_start_callback)
    GC_collect_end_callback();

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

void *GC_resolve(void *p)
{
  return p;
}

/**********************************************************************/

static MPage *get_page_rec(void *p, mtype_t mtype)
{
  unsigned long g, addr;
  MPage *map;

  g = ((unsigned long)p >> MAPS_SHIFT);
  
  if (!mpage_maps) {
    int i;
    mpage_maps = (MPage **)malloc_pages(sizeof(MPage *) * MAPS_SIZE, 0);
    for (i = 0; i < MAPS_SIZE; i++)
      mpage_maps[i] = NULL;
  }

  map = mpage_maps[g];
  if (!map) {
    int i;

    map = (MPage *)malloc_pages(sizeof(MPage) * MAP_SIZE, 0);
    for (i = 0; i < MAP_SIZE; i++)
      map[i].type = 0;

    mpage_maps[g] = map;
  }

  addr = (((unsigned long)p & MAP_MASK) >> MAP_SHIFT);

#if NOISY
  {
    int c;
    if (!mtype)
      c = '.';
    else {
      if (mtype & MTYPE_TAGGED)
	c = 't';
      else if (mtype & MTYPE_ATOMIC)
	c = 'a';
      else if (mtype & MTYPE_TAGGED_ARRAY)
	c = 'g';
      else
	c = 'v';

      if (mtype & MTYPE_BIGBLOCK)
	c = c - ('a' - 'A');
    }
    printf("%c p = %lx, g = %lx, addr = %lx\n", c, (long)p, g, addr);
  }
#endif

  return map + addr;
}

static void new_page(mtype_t mtype, MSet *set)
{
  void *p;
  MPage *map;
  OffsetArrTy *offsets;

  if (memory_in_use > gc_threshold) {
    gcollect(0);
    return;
  }
  
  p = (void *)malloc_pages(MPAGE_SIZE, MPAGE_SIZE);
  offsets = (OffsetArrTy *)malloc_pages(OPAGE_SIZE, 0);

  memory_in_use += MPAGE_SIZE;

  map = get_page_rec(p, mtype);

  map->type = (mtype | MTYPE_MODIFIED);
  map->u.offsets = offsets;
  map->block_start = p;
  map->age = 0;
  map->refs_age = 0;

  map->next = NULL;
  map->prev = last;
  if (last)
    last->next = map;
  else
    first = map;
  last = map;

  set->malloc_page = map;

  set->low = (void **)p;
  set->high = (void **)(p + MPAGE_SIZE);
}

static void * malloc_bigblock(long size_in_bytes, mtype_t mtype)
{
  void *p, *mp;
  MPage *map;
  long s;

  if (memory_in_use > gc_threshold) {
    gcollect(0);
    return malloc_bigblock(size_in_bytes, mtype);
  }
  
  mtype |= MTYPE_BIGBLOCK;

  p = (void *)malloc_pages(size_in_bytes, MPAGE_SIZE);

  memory_in_use += size_in_bytes;

  map = get_page_rec(p, mtype);
  
  map->type = (mtype | MTYPE_MODIFIED);
  map->u.size = size_in_bytes;
  map->block_start = p;
  map->age = 0;
  map->refs_age = 0;

  map->next = NULL;
  map->prev = last;
  if (last)
    last->next = map;
  else
    first = map;
  last = map;

  s = size_in_bytes;
  mp = p;
  while (s > MPAGE_SIZE) {
    mp = mp + MPAGE_SIZE;
    s -= MPAGE_SIZE;
    map = get_page_rec(mp, 0);
    map->type = MTYPE_CONTINUED | MTYPE_BIGBLOCK;
    map->o.bigblock_start = p;
  }

#if SEARCH
  if (p == search_for) {
    stop();
  }
#endif

  return p;
}

void *GC_malloc_one_tagged(size_t size_in_bytes)
{
  size_t size_in_words;
  void **m, **naya;

#if SAFETY
  check_variable_stack();
#endif

  size_in_words = ((size_in_bytes + 3) >> 2);

#if SAFETY
  if (size_in_words < 2)
    CRASH();
#endif

  if (size_in_words >= (BIGBLOCK_MIN_SIZE >> 2)) {
    return malloc_bigblock(size_in_words << 2, MTYPE_TAGGED);
  }

#if USE_FREELIST
  m = (void *)tagged.free_lists[size_in_words];
  if (m) {
    int i;

    tagged.free_lists[size_in_words] = m[1];

    for (i = 0; i < size_in_words; i++)
      m[i] = NULL;

    on_free_list -= size_in_words;
    
    return m;
  }
#endif

#if ALIGN_DOUBLES
  if (!(size_in_words & 0x1)) {
    /* Make sure memory is 8-aligned */
    if (((long)tagged.low & 0x4)) {
      if (tagged.low == tagged.high) {
	new_page(MTYPE_TAGGED, &tagged);
	return GC_malloc_one_tagged(size_in_words << 2);
      }
      ((Type_Tag *)tagged.low)[0] = SKIP;
      tagged.low += 1;
    }
  }
#endif

#if SEARCH
  if (size_in_bytes == search_size)
    stop();
#endif

  m = tagged.low;
  naya = tagged.low + size_in_words;
  if (naya >= tagged.high) {
    if (tagged.low < tagged.high)
      *(Type_Tag *)tagged.low = TAGGED_EOM;
    new_page(MTYPE_TAGGED, &tagged);
    return GC_malloc_one_tagged(size_in_words << 2);
  }
  tagged.low = naya;

#if SEARCH
  if (m == search_for) {
    stop();
  }
#endif

  return m;
}

static void *malloc_untagged(size_t size_in_bytes, mtype_t mtype, MSet *set)
{
  size_t size_in_words;
  void **m, **naya;

#if SAFETY
  check_variable_stack();
#endif

  if (!size_in_bytes)
    return zero_sized;

  size_in_words = ((size_in_bytes + 3) >> 2);

  if (size_in_words >= (BIGBLOCK_MIN_SIZE >> 2)) {
    return malloc_bigblock(size_in_words << 2, mtype);
  }

#if USE_FREELIST
  m = (void *)set->free_lists[size_in_words];
  if (m) {
    int i;

    set->free_lists[size_in_words] = m[0];

    if (mtype != MTYPE_ATOMIC)
      for (i = 0; i < size_in_words; i++)
	m[i] = NULL;

    on_free_list -= size_in_words;
    
    return m;
  }
#endif

#if ALIGN_DOUBLES
  if (!(size_in_words & 0x1)) {
    /* Make sure memory is 8-aligned */
    if (!((long)set->low & 0x4)) {
      if (set->low == set->high) {
	new_page(mtype, set);
	return malloc_untagged(size_in_words << 2, mtype, set);
      }
      (set->low)[0] = 0;
      set->low += 1;
    }
  }
#endif

#if SEARCH
  if (size_in_bytes == search_size)
    stop();
#endif

  m = set->low;
  naya = set->low + size_in_words + 1;
  if (naya >= set->high) {
    if (set->low < set->high)
      *(long *)set->low = UNTAGGED_EOM - 1;
    new_page(mtype, set);
    return malloc_untagged(size_in_words << 2, mtype, set);
  }
  set->low = naya;

#if SEARCH
  if ((m + 1) == search_for) {
    stop();
  }
#endif

  *(long *)m = size_in_words;

  return m + 1;
}

/* Array of pointers: */
void *GC_malloc(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, MTYPE_ARRAY, &array);
}

void *GC_malloc_allow_interior(size_t size_in_bytes)
{
  return malloc_bigblock(size_in_bytes, MTYPE_ARRAY);
}

void *GC_malloc_array_tagged(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, MTYPE_TAGGED_ARRAY, &tagged_array);
}

void *GC_malloc_one_xtagged(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, MTYPE_XTAGGED, &xtagged);
}

/* Pointerless */
void *GC_malloc_atomic(size_t size_in_bytes)
{
  return malloc_untagged(size_in_bytes, MTYPE_ATOMIC, &atomic);
}

/* Plain malloc: */
void *GC_malloc_atomic_uncollectable(size_t size_in_bytes)
{
  return malloc(size_in_bytes);
}

void GC_free(void *p)
{
  MPage *page;

  page = find_page(p);

  if ((page->type & MTYPE_BIGBLOCK)
      && !(page->type & MTYPE_CONTINUED)
      && (p == page->block_start)) {
    long s;
    unsigned long i, j;

    memory_in_use -= page->u.size;

    if (page->prev)
      page->prev->next = page->next;
    else
      first = page->next;
    if (page->next)
      page->next->prev = page->prev;
    else
      last = page->prev;
    page->type = 0;

    free_pages((void *)p, page->u.size);
	
    s = page->u.size;
    i = ((unsigned long)p >> MAPS_SHIFT);
    j = (((unsigned long)p & MAP_MASK) >> MAP_SHIFT);
    while (s > MPAGE_SIZE) {
      s -= MPAGE_SIZE;
      j++;
      if (j == MAP_SIZE) {
	j = 0;
	i++;
      }
      mpage_maps[i][j].type = 0;
    }
  }
}

void GC_register_traversers(Type_Tag tag, Size_Proc size, Mark_Proc mark, Fixup_Proc fixup)
{
  size_table[tag] = size;
  mark_table[tag] = mark;
  fixup_table[tag] = fixup;
}

void GC_gcollect()
{
  gcollect(1);
}
