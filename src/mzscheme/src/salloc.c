/*
  MzScheme
  Copyright (c) 1995 Matthew Flatt
 
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

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* Some copilers don't like re-def of GC_malloc in schemef.h: */
#define SCHEME_NO_GC_PROTO

#include "schpriv.h"
#include <string.h>
#include <memory.h>
#include "schgc.h"

#ifdef DOS_FAR_POINTERS
# include <alloc.h>
# define MALLOC farmalloc
#else
# define MALLOC malloc
#endif

#ifndef MZ_REAL_THREADS
# define GET_FIN_LOCK() /* empty */
# define RELEASE_FIN_LOCK() /* empty */
#else
static void *fin_mutex = NULL;
# define GET_FIN_LOCK() (fin_mutex = (fin_mutex ? fin_mutex : SCHEME_MAKE_MUTEX())); SCHEME_LOCK_MUTEX(fin_mutex)
# define RELEASE_FIN_LOCK()  SCHEME_UNLOCK_MUTEX(fin_mutex)
#endif

static void **dgc_array;
static int *dgc_count;
static int dgc_size;

extern void (*GC_out_of_memory)(void);

void scheme_dont_gc_ptr(void *p)
{
  int i, oldsize;
  void **naya;
  int *nayac;

  /* look for existing: */
  for (i = 0; i < dgc_size; i++)
    if (dgc_array[i] == p) {
      dgc_count[i]++;
      return;
    }

  /* look for empty slot: */
  for (i = 0; i < dgc_size; i++)
    if (!dgc_array[i]) {
      dgc_array[i] = p;
      dgc_count[i] = 1;
      return;
    }

  /* Make more room: */
  oldsize = dgc_size;
  if (!dgc_array) {
    REGISTER_SO(dgc_array);
    REGISTER_SO(dgc_count);
    dgc_size = 50;
  } else
    dgc_size *= 2;

  naya = MALLOC_N(void*, dgc_size);
  nayac = MALLOC_N(int, dgc_size);

  for (i = 0; i < oldsize; i++) {
    naya[i] = dgc_array[i];
    nayac[i] = dgc_count[i];
  }

  for (; i < dgc_size; i++) {
    naya[i] = NULL;
    dgc_count[i] = 0;
  }

  dgc_array = naya;

  dgc_array[oldsize] = p;
  dgc_count[oldsize] = 1;
}

void scheme_gc_ptr_ok(void *p)
{
  int i;
  
  for (i = 0; i < dgc_size; i++)
    if (dgc_array[i] == p) {
      if (!(--dgc_count[i]))
	dgc_array[i] = NULL;
      break;
    }
}

#ifdef NO_GC
void *
scheme_malloc (size_t size)
{
  void *space;

  space = MALLOC(size);
  if (!space)
    scheme_raise_out_of_memory(NULL, NULL);

  return (space);
}
#endif

void *
scheme_calloc (size_t num, size_t size)
{
  void *space;
  
  space = MALLOC (num*size);
#ifdef NO_GC
  memset(space, 0, (num*size));
#endif
  if (!space)
    scheme_raise_out_of_memory(NULL, NULL);

  return (space);
}

char *
scheme_strdup(const char *str)
{
  char *naya;
  long len;

  len = strlen (str) + 1;
  naya = (char *)scheme_malloc_atomic (len * sizeof (char));
  memcpy (naya, str, len);

  return naya;
}

char *
scheme_strdup_eternal(const char *str)
{
  char *naya;
  long len;

  len = strlen (str) + 1;
  naya = (char *)scheme_malloc_eternal(len * sizeof (char));
  memcpy (naya, str, len);

  return naya;
}

static void (*save_oom)(void);

static void raise_out_of_memory()
{
  GC_out_of_memory = save_oom;
  scheme_raise_out_of_memory(NULL, NULL);
}

void *scheme_malloc_fail_ok(void *(*f)(size_t), size_t s)
{
  void *v;

  save_oom = GC_out_of_memory;
  GC_out_of_memory = raise_out_of_memory;
  v = f(s);
  GC_out_of_memory = save_oom;

  return v;
}

void scheme_end_stubborn_change(void *p)
{
  GC_end_stubborn_change(p);
}

void *scheme_malloc_eternal(size_t n)
{
#ifdef USE_SENORA_GC
  return GC_malloc_atomic_uncollectable(n);
#else
  void *s;

  s = MALLOC(n);
  if (!s) {
    if (GC_out_of_memory)
      GC_out_of_memory();
    else {
      if (scheme_console_printf)
	scheme_console_printf("out of memory\n");
      else
	printf("out of memory\n");
      exit(-1);
    }
  }
	

  memset(s, 0, n);

  return s;
#endif
}


#ifdef USE_TAGGED_ALLOCATION

struct GC_Set *tagged, *real_tagged, *tagged_atomic, *tagged_eternal, *tagged_uncollectable, *envunbox;
struct GC_Set *tagged_while_counting;

static void trace_count(void *, int);
static void trace_path(void *, unsigned long, void *);
static void trace_init(void);
static void trace_done(void);
static void finalize_object(void *);

#define TRACE_FUNCTIONS trace_init, trace_done, trace_count, trace_path

static void init_tagged_counting(void)
{
  if (!tagged_while_counting)
    tagged_while_counting = GC_new_set("counting", 
				       NULL, NULL, NULL, NULL, NULL,
				       0);
}

void *scheme_malloc_tagged(size_t s)
{
  if (!tagged) {
    init_tagged_counting();
    real_tagged = tagged = GC_new_set("tagged", TRACE_FUNCTIONS, 
				      finalize_object, 
				      0);
  }

  return GC_malloc_specific(s, tagged);
}

void *scheme_malloc_atomic_tagged(size_t s)
{
  if (!tagged_atomic) {
    init_tagged_counting();
    tagged_atomic = GC_new_set("tagged", TRACE_FUNCTIONS, 
			       finalize_object, 
			       SGC_ATOMIC_SET);
  }

  return GC_malloc_specific(s, tagged_atomic);
}

void *scheme_malloc_stubborn_tagged(size_t s)
{
  return scheme_malloc_tagged(s);
}

void *scheme_malloc_envunbox(size_t s)
{
  if (!envunbox)
    envunbox = GC_new_set("envunbox", 
			  NULL, NULL, NULL, NULL, NULL,
			  0);

  return GC_malloc_specific(s, envunbox);
}

void *scheme_malloc_eternal_tagged(size_t s)
{
  if (!tagged_eternal) {
    init_tagged_counting();
    tagged_eternal = GC_new_set("tagged", TRACE_FUNCTIONS,
				finalize_object,
				SGC_UNCOLLECTABLE_SET | SGC_ATOMIC_SET);
  }

  return GC_malloc_specific(s, tagged_eternal);
}

void *scheme_malloc_uncollectable_tagged(size_t s)
{
  if (!tagged_uncollectable) {
    init_tagged_counting();
    tagged_uncollectable = GC_new_set("tagged", TRACE_FUNCTIONS, 
				      finalize_object,
				      SGC_UNCOLLECTABLE_SET);
  }

  return GC_malloc_specific(s, tagged_uncollectable);
}

#endif

typedef struct Finalization {
  void (*f)(void *o, void *data);
  void *data;
  struct Finalization *next;
} Finalization;

typedef struct {
  Finalization *scheme;
  void (*ext_f)(void *o, void *data);
  void *ext_data;
  Finalization *prim;
} Finalizations;

typedef struct {
  void **p; /* place to zero out */
  Finalizations *src_fns, *dest_fns; /* Mutual finalizations to adjust */
  Finalization *src_fn, *dest_fn;
} ZeroingRecord;

static void do_next_finalization(void *o, void *data)
{
  Finalizations *fns = *(Finalizations **)data;
  Finalization *fn;

  if (fns->scheme) {
    if (fns->scheme->next || fns->ext_f || fns->prim) {
      /* Re-install low-level finalizer and run a scheme finalizer */
      GC_register_eager_finalizer(o, do_next_finalization, data, NULL, NULL);
    }

    fn = fns->scheme;
    fns->scheme = fn->next;

    fn->f(o, fn->data);
    return;
  }

  if (fns->ext_f)
    fns->ext_f(o, fns->ext_data);

  for (fn = fns->prim; fn; fn = fn->next)
    fn->f(o, fn->data);
}

static void add_finalizer(void *v, void (*f)(void*,void*), void *data, 
			  int prim, int ext,
			  void (**ext_oldf)(void *p, void *data),
			  void **ext_olddata,
			  Finalizations **_fns,
			  Finalization **_fn)
{
  void (*oldf)(void *p, void *data);
  void *olddata;
  Finalizations *fns, **fns_ptr, *prealloced;
  Finalization *fn;

  if (v != GC_base(v))
    return;

  /* Allocate everything first so that we're not changing
     finalizations when finalizations could run: */

  fns_ptr = MALLOC_ONE(Finalizations*);

  if (!ext) {
    fn = MALLOC_ONE(Finalization);
    fn->f = f;
    fn->data = data;
  } else
    fn = NULL;

  prealloced = MALLOC_ONE(Finalizations); /* may not need this... */

  GET_FIN_LOCK();

  GC_register_eager_finalizer(v, do_next_finalization, fns_ptr, &oldf, &olddata);

  if (oldf) {
    if (oldf != do_next_finalization) {
      /* This happens if an extenal use of GC_ routines conflicts with us. */
      scheme_warning("warning: non-MzScheme finalization on object dropped!");
    } else {
      *fns_ptr = *(Finalizations **)olddata;
    }
  }

  if (!(*fns_ptr))
    *fns_ptr = prealloced;
  fns = *fns_ptr;

  if (ext) {
    if (ext_oldf)
      *ext_oldf = fns->ext_f;
    fns->ext_f = f;
    if (ext_olddata)
      *ext_olddata = fns->ext_data;
    fns->ext_data = data;
  } else {
    if (prim) {
      fn->next = fns->prim;
      fns->prim = fn;
    } else {
      fn->next = fns->scheme;
      fns->scheme = fn;
    }
  }

  if (_fns)
    *_fns = fns;
  if (_fn)
    *_fn = fn;

  RELEASE_FIN_LOCK();
}

static void remove_finalizer(Finalizations *fns, Finalization *rfn, int prim)
{
  Finalization *fn, *prev;

  GET_FIN_LOCK();

  prev = NULL;
  for (fn = (prim ? fns->prim : fns->scheme); fn; fn = fn->next) {
    if (fn == rfn) {
      if (!prev) {
	if (prim)
	  fns->prim = fn->next;
	else
	  fns->scheme = fn->next;
      } else
	prev->next = fn->next;
    }
    prev = fn;
  }

  RELEASE_FIN_LOCK();
}

static void z_rmve_finalizers(void *ignore, void *p)
{
  ZeroingRecord *z = (ZeroingRecord *)p;

  if (z->src_fn)
    remove_finalizer(z->src_fns, z->src_fn, 1);
  if (z->dest_fn)
    remove_finalizer(z->dest_fns, z->dest_fn, 1);
}

static void zero_out(void *ignore, void *p)
{
  ZeroingRecord *z = (ZeroingRecord *)p;

  *z->p = NULL;

  z_rmve_finalizers(ignore, p);
}

void scheme_weak_reference(void **p)
{
  scheme_weak_reference_indirect(p, *p);
}

void scheme_weak_reference_indirect(void **p, void *v)
{
  if (GC_base(v) == v) {
    ZeroingRecord *z;
    void *base;

    z = MALLOC_ONE_ATOMIC(ZeroingRecord);

    z->p = p;
    add_finalizer(v, zero_out, z, 1, 0, NULL, NULL, &z->src_fns, &z->src_fn);

    if ((base = GC_base(p)))
      add_finalizer(base, z_rmve_finalizers, z, 1, 0, NULL, NULL, &z->dest_fns, &z->dest_fn);
    else {
      z->dest_fn = NULL;
      z->dest_fns = NULL;
    }
  }
}

void scheme_add_finalizer(void *p, void (*f)(void *p, void *data), void *data)
{
  add_finalizer(p, f, data, 1, 0, NULL, NULL, NULL, NULL);
}

void scheme_add_scheme_finalizer(void *p, void (*f)(void *p, void *data), void *data)
{
  add_finalizer(p, f, data, 0, 0, NULL, NULL, NULL, NULL);
}

void scheme_register_finalizer(void *p, void (*f)(void *p, void *data), 
			       void *data, void (**oldf)(void *p, void *data), 
			       void **olddata)
{
  add_finalizer(p, f, data, 0, 1, oldf, olddata, NULL, NULL);
}

void scheme_collect_garbage(void)
{
  GC_gcollect();
}

unsigned long scheme_get_deeper_address(void)
{
  int v, *vp;
  vp = &v;
  return (unsigned long)vp;
}



#ifdef DOS_MEMORY

int scheme_same_pointer(void *a, void *b)
{
  long as, ao, bs, bo, areal, breal;
  
  as = FP_SEG(a);
  ao = FP_OFF(a);
  bs = FP_SEG(b);
  bo = FP_SEG(b);

  areal = (as << 4) + ao;
  breal = (bs << 4) + bo;

  return areal == breal;
}

int scheme_diff_pointer(void *a, void *b)
{
  return !scheme_same_pointer(a, b);
}

#endif

#ifdef __cplusplus
extern "C" 
{
#endif
  extern void GC_dump(void);
#ifdef __cplusplus
};
#endif
 

#ifdef USE_TAGGED_ALLOCATION
#define NUM_TYPE_SLOTS (_scheme_last_type_ + 1)

static long scheme_memory_count[NUM_TYPE_SLOTS];
static long scheme_memory_actual_count[NUM_TYPE_SLOTS];
static long scheme_memory_size[NUM_TYPE_SLOTS];
static long scheme_memory_actual_size[NUM_TYPE_SLOTS];
static unsigned long scheme_memory_hi[NUM_TYPE_SLOTS];
static unsigned long scheme_memory_lo[NUM_TYPE_SLOTS];
static long scheme_envunbox_count, scheme_envunbox_size;
static long bad_seeds;
static Scheme_Hash_Table *smc_ht;
static int trace_path_type;

void count_tagged(void *p, int size, void *data)
{
  int which = SCHEME_TYPE((Scheme_Object *)p);
  if ((which >= 0) && (which < _scheme_last_type_)) {
    scheme_count_memory((Scheme_Object *)p, smc_ht);
  } else if (which >= scheme_num_types())
    bad_seeds++;
  else {
    scheme_memory_count[_scheme_last_type_]++;
    scheme_memory_size[_scheme_last_type_] += size;
  }
}

void count_envunbox(void *p, int size, void *data)
{
  scheme_envunbox_count++;
  scheme_envunbox_size += size;
}

static void trace_count(void *p, int size)
{
  int which = SCHEME_TYPE((Scheme_Object *)p);
  if ((which >= 0) && (which <= _scheme_last_type_)) {
    unsigned long s = (unsigned long)p;
    scheme_memory_actual_count[which]++;
    scheme_memory_actual_size[which] += size;
    if (!scheme_memory_lo[which] || (s < scheme_memory_lo[which]))
      scheme_memory_lo[which] = s;
    if (!scheme_memory_hi[which] || (s > scheme_memory_hi[which]))
      scheme_memory_hi[which] = s;
  } else if (which >= scheme_num_types())
    bad_seeds++;
}

static void trace_path(void *p, unsigned long src, void *path_data)
{
  if ((trace_path_type > -1)
      && ((int)SCHEME_TYPE((Scheme_Object *)p) == trace_path_type))
    GC_store_path(p, src, path_data);
}

static void trace_init(void)
{
  /* do nothing */
}

static void trace_done(void)
{
  /* do nothing */
}

static void finalize_object(void *p)
{
  ((Scheme_Object *)p)->type = _scheme_values_types_;
}

#endif

void (*scheme_external_dump_info)(void);
void (*scheme_external_dump_arg)(Scheme_Object *arg);
char *(*scheme_external_dump_type)(void *v);

static void count_managed(Scheme_Manager *m, int *c, int *a, int *u, int *t,
			  int *ipt, int *opt, int *th)
{
  int i;

  *t += 1;
  *c += m->count;
  *a += m->alloc;
  for (i = m->count; i--; )
    if (m->boxes[i]) {
      Scheme_Object *o = (*(m->boxes[i]));
      (*u)++;
      if (SCHEME_PROCESSP(o))
	(*th)++;
      else if (SCHEME_INPORTP(o))
	(*ipt)++;
      else if (SCHEME_OUTPORTP(o))
	(*opt)++;
    }

  if (*m->sibling)
    count_managed(*m->sibling, c, a, u, t, ipt, opt, th);
  if (*m->children)
    count_managed(*m->children, c, a, u, t, ipt, opt, th);
}

Scheme_Object *scheme_dump_gc_stats(int c, Scheme_Object *p[])
{
  scheme_console_printf("Begin Dump\n");

  if (scheme_external_dump_arg)
    scheme_external_dump_arg(c ? p[0] : NULL);

#ifdef USE_TAGGED_ALLOCATION
  trace_path_type = -1;
  if (c && SCHEME_SYMBOLP(p[0])) {
    char *s = SCHEME_SYM_VAL(p[0]);
    int i;

    for (i = 0; i < _scheme_last_type_; i++) {
      void *tn = scheme_get_type_name(i);
      if (tn && !strcmp(tn, s)) {
	trace_path_type = i;
	break;
      }
    }
  }

  {
    int i;
    int stack_c, roots_c, uncollectable_c, final_c;
    long total_count = 0, total_size = 0;
    long total_actual_count = 0, total_actual_size = 0;
    long traced;

    for (i = 0; i < NUM_TYPE_SLOTS; i++) {
      scheme_memory_count[i] = scheme_memory_size[i] = 0;
      scheme_memory_actual_size[i] = scheme_memory_actual_count[i] = 0;
      scheme_memory_hi[i] = scheme_memory_lo[i] = 0;
    }
    scheme_envunbox_count = scheme_envunbox_size = 0;
    bad_seeds = 0;

    traced = GC_trace_count(&stack_c, &roots_c, &uncollectable_c, &final_c);
    GC_dump();

    scheme_console_printf("\ntraced: %ld\n", traced);

    tagged = tagged_while_counting;
    
    smc_ht = scheme_hash_table(1000, SCHEME_hash_ptr, 0, 0);
    
    if (tagged) 
      GC_for_each_element(real_tagged, count_tagged, NULL);
    if (tagged_eternal) 
      GC_for_each_element(tagged_eternal, count_tagged, NULL);
    if (tagged_uncollectable) 
      GC_for_each_element(tagged_uncollectable, count_tagged, NULL);
    if (tagged_atomic)
      GC_for_each_element(tagged_atomic, count_tagged, NULL);
    if (envunbox)
      GC_for_each_element(envunbox, count_envunbox, NULL);

    tagged = real_tagged;

    scheme_console_printf("Begin MzScheme\n");
    scheme_console_printf("%30.30s %10s %10s %10s %8s - %8s\n",
			  "TYPE", "COUNT", "ESTM-SIZE", "TRACE-SIZE", 
			  "LO-LOC", "HI-LOC");
    for (i = 0; i < NUM_TYPE_SLOTS; i++) {
      if (scheme_memory_count[i] || scheme_memory_actual_count[i]) {
	scheme_console_printf("%30.30s %10ld %10ld %10ld %8lx - %8lx\n",
			      (i < _scheme_last_type_)
			      ? scheme_get_type_name(i)
			      : "other",
			      scheme_memory_count[i],
			      scheme_memory_size[i],
			      scheme_memory_actual_size[i],
			      scheme_memory_lo[i],
			      scheme_memory_hi[i]);
	if (scheme_memory_actual_count[i] != scheme_memory_count[i]) {
	  scheme_console_printf("%30.30s actual count: %10ld\n",
				"", scheme_memory_actual_count[i]);
	}
	total_count += scheme_memory_count[i];
	total_size += scheme_memory_size[i];
	total_actual_count += scheme_memory_actual_count[i];
	total_actual_size += scheme_memory_actual_size[i];
      }
    }

    scheme_console_printf("%30.30s %10ld %10ld          -\n",
			  "envunbox", scheme_envunbox_count, scheme_envunbox_size);
    total_count += scheme_envunbox_count;
    total_size += scheme_envunbox_size;

    {
      int c;

      c = scheme_count_sema_callbacks(0);
      scheme_console_printf("%30.30s %10ld          -          -\n",
			    "semaphore-callback", c);
      total_count += c;

      c = scheme_count_sema_callbacks(1);
      scheme_console_printf("%30.30s %10ld          -          -\n",
			    "input-port-post-semaphore", c);
      total_count += c;
    }

    scheme_console_printf("%30.30s          - %10ld          -\n",
			  "miscellaneous", 
			  scheme_misc_count + scheme_type_table_count);
    total_size += scheme_misc_count + scheme_type_table_count;

    scheme_console_printf("%30.30s          -          - %10ld\n",
			  "roots", roots_c);
    total_actual_size += roots_c;

    scheme_console_printf("%30.30s          -          - %10ld\n",
			  "stack", stack_c);
    total_actual_size += stack_c;

    scheme_console_printf("%30.30s          -          - %10ld\n",
			  "unreached-uncollectable", uncollectable_c);
    total_actual_size += uncollectable_c;

    scheme_console_printf("%30.30s          -          - %10ld\n",
			  "finalization", final_c);
    total_actual_size += final_c;

    scheme_console_printf("%30.30s %10ld %10ld %10ld\n",
			  "total", total_count, total_size, 
			  total_actual_size);
    scheme_console_printf("End MzScheme\n");

    {
      Scheme_Manager *m = (Scheme_Manager *)scheme_get_param(scheme_config, MZCONFIG_MANAGER);
      int c = 0, a = 0, u = 0, t = 0, ipt = 0, opt = 0, th = 0;

      while (*m->parent)
	m = *m->parent;

      count_managed(m, &c, &a, &u, &t, &ipt, &opt, &th);

      scheme_console_printf("custodians: %d  managed: actual: %d   breadth: %d   room: %d\n"
			    "                        input-ports: %d  output-ports: %d  threads: %d\n", 
			    t, u, c, a, ipt, opt, th);
    }

    if (bad_seeds)
      scheme_console_printf("ERROR: %ld illegal tags found\n", bad_seeds);

    smc_ht = NULL;
  }
#else
  GC_dump();
#endif

  if (scheme_external_dump_info)
    scheme_external_dump_info();

#ifdef USE_TAGGED_ALLOCATION
  {
    void **ps = NULL;
    int l;
    int max_w;
    Scheme_Object *w;

    GC_trace_path();
    
    w = scheme_get_param(scheme_config, MZCONFIG_ERROR_PRINT_WIDTH);
    if (SCHEME_INTP(w))
      max_w = SCHEME_INT_VAL(w);
    else
      max_w = 10000;

    scheme_console_printf("Begin Paths\n");

    while ((ps = GC_get_next_path(ps, &l))) {
      int i, j;
      if (l)
	scheme_console_printf("$%s", ps[0]);
      for (i = 1, j = 2; i < l; i++, j += 2) {
	void *v = ps[j];
	unsigned long diff = (unsigned long)ps[j + 1];
	char *type, *sep, diffstr[30];
	struct GC_Set *home;

	sep = "";

	home = GC_set(v);
	if ((home == real_tagged)
	    || (home == tagged_atomic)
	    || (home == tagged_uncollectable)
	    || (home == tagged_eternal)) {
#if 0
	  type = scheme_get_type_name(SCHEME_TYPE((Scheme_Object *)v));
	  if (*type)
	    sep = ":";
#else
	  long len;
	  type = scheme_write_to_string_w_max((Scheme_Object *)v, &len, max_w);
	  sep = "=";
#endif
	} else if (scheme_external_dump_type) {
	  type = scheme_external_dump_type(v);
	  if (*type)
	    sep = ":";
	} else
	  type = "";

	if (diff)
	  sprintf(diffstr, "%lx", diff);

	scheme_console_printf("%s->%lx%s%s%s%s", 
			      "\n  ",
			      v, 
			      sep,
			      type,
			      diff ? "+" : "",
			      diff ? diffstr : "");
      }
      scheme_console_printf("\n");
    }

    GC_clear_paths();

    scheme_console_printf("End Paths\n");
  }
#endif

  scheme_console_printf("End Dump\n");

  return scheme_void;
}



#ifdef MEMORY_COUNTING_ON

long scheme_count_closure(Scheme_Object **o, short len, Scheme_Hash_Table *ht)
{
#if 0
  int i;
  int s = 0;

  for (i = 0; i < len; i++) {
    if (!scheme_lookup_in_table(ht, (const char *)o[i])) {
      scheme_add_to_table(ht, (const char *)o[i], scheme_true, 0);
      if (GC_size(o[i]) == sizeof(Scheme_Object *)) {
	/* May be an environment box */
	Scheme_Object *d = *(Scheme_Object **)o[i];
	if (GC_size(d) >= sizeof(Scheme_Type)) {
	  /* Ok - probably it is a box. */
	  s += sizeof(Scheme_Object *);
	  s += scheme_count_memory(d, ht);
	} else {
	  /* Not an environment box. */
	  s += scheme_count_memory(o[i], ht);
	}
      } else {
	s += scheme_count_memory(o[i], ht);
      }
    }
  }

  return s;
#endif
  return 0;
}


#if 0
void scheme_check_home(Scheme_Object *root)
{
  struct GC_Set *home;
  home = GC_set(root);
  if ((home != real_tagged)
      && (home != tagged_atomic)
      && (home != tagged_uncollectable)
      && (home != tagged_eternal)) {
    scheme_console_printf("Check: bad Scheme object: %lx\n", (unsigned long)root);
  }
}
#endif

#define FORCE_SUBPARTS 0
#define FORCE_KNOWN_SUBPARTS 1
#define CAN_TRACE_HOME 1

long scheme_count_memory(Scheme_Object *root, Scheme_Hash_Table *ht)
{
  Scheme_Type type;
  long s = sizeof(Scheme_Object), e = 0;
  int need_align = 0;
  struct GC_Set *home;

  if (!root || SCHEME_INTP(root))
    return 0;

  type = SCHEME_TYPE(root);

  if (type >= _scheme_last_type_)
    return 0;

  if (scheme_lookup_in_table(ht, (const char *)root))
    return 0;

  home = GC_set(root);
#if CAN_TRACE_HOME
  if ((home != real_tagged)
      && (home != tagged_atomic)
      && (home != tagged_uncollectable)
      && (home != tagged_eternal)) {
    scheme_console_printf("Bad Scheme object: %lx\n", (unsigned long)root);
    return 0;
  }
#endif

  scheme_add_to_table(ht, (const char *)root, scheme_true, 0);

#define COUNT(x) scheme_count_memory((Scheme_Object *)x, ht)

  switch (type) {
  case scheme_variable_type:
    s = sizeof(Scheme_Bucket);
#if FORCE_SUBPARTS
    e = COUNT(((Scheme_Bucket *)root)->key)
      + COUNT(((Scheme_Bucket *)root)->val);
#endif
    break;
  case scheme_local_type: 
  case scheme_local_unbox_type:
    s = sizeof(Scheme_Local);
    break;
  case scheme_syntax_type:
#if FORCE_KNOWN_SUBPARTS
    e = COUNT(SCHEME_PTR2_VAL(root));
#endif
    break;
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)root;
      int i;

      s = sizeof(Scheme_App_Rec) + (app->num_args * sizeof(Scheme_Object *))
	+ (app->num_args + 1);
      need_align = 1;
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(app->args[0]);
      for (i = 1; i <= app->num_args; i++)
	e += COUNT(app->args[i]);
#endif
    }
    break;
  case scheme_sequence_type:
  case scheme_case_lambda_sequence_type:
  case scheme_begin0_sequence_type:
  case scheme_case_closure_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)root;
      int i;

      s = sizeof(Scheme_Sequence) + (seq->count - 1) * sizeof(Scheme_Object *);

#if FORCE_KNOWN_SUBPARTS
      for (i = e = 0; i < seq->count; i++)
	e += COUNT(seq->array[i]);
#endif
    }
    break;
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *rec = (Scheme_Branch_Rec *)root;
      
      s = sizeof(Scheme_Branch_Rec);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(rec->test) + COUNT(rec->tbranch) + COUNT(rec->fbranch);
#endif
    }
    break;
  case scheme_unclosed_procedure_type:
  case scheme_compiled_unclosed_procedure_type:
    {
      Scheme_Closure_Compilation_Data *data = 
	(Scheme_Closure_Compilation_Data *)root;

      s = sizeof(Scheme_Closure_Compilation_Data);
      s += data->closure_size * sizeof(short);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(data->code);
#endif
    }
    break;
  case scheme_let_value_type:
    {
      Scheme_Let_Value *let = (Scheme_Let_Value *)root;

      s = sizeof(Scheme_Let_Value);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(let->value) + COUNT(let->body);
#endif
    }
    break;
  case scheme_compiled_let_value_type:
    {
      Scheme_Compiled_Let_Value *let = (Scheme_Compiled_Let_Value *)root;

      s = sizeof(Scheme_Compiled_Let_Value);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(let->value) + COUNT(let->body);
#endif
    }
    break;
  case scheme_let_void_type:
    {
      Scheme_Let_Void *let = (Scheme_Let_Void *)root;

      s = sizeof(Scheme_Let_Void);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(let->body);
#endif
    }
    break;
  case scheme_compiled_let_void_type:
    {
      Scheme_Let_Header *let = (Scheme_Let_Header *)root;

      s = sizeof(Scheme_Let_Header);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(let->body);
#endif
    }
    break;
  case scheme_letrec_type:
    {
      Scheme_Letrec *let = (Scheme_Letrec *)root;
      int i;

      s = sizeof(Scheme_Letrec);
      s += let->count * sizeof(Scheme_Object *);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(let->body);
      for (i = 0; i < let->count; i++)
	e += COUNT(let->procs[i]);
#endif
    }
    break;
  case  scheme_quote_compilation_type:
    s = sizeof(Scheme_Small_Object);
#if FORCE_KNOWN_SUBPARTS
    e = COUNT(SCHEME_PTR_VAL(root));
#endif
    break;
  case scheme_char_type:
    s = sizeof(Scheme_Small_Object);
    break;
  case scheme_integer_type:
    s = 0;
    break;
  case scheme_double_type:
    s = sizeof(Scheme_Double);
    break;
  case scheme_float_type:
    break;
  case scheme_string_type:
    s += (SCHEME_STRTAG_VAL(root) + 1);
    need_align = 1;
    break;
  case scheme_symbol_type:
    s = sizeof(Scheme_Symbol) + SCHEME_SYM_LEN(root) - 1;
    need_align = 1;
    break;
  case scheme_null_type: 
    break;
  case scheme_pair_type:
#if FORCE_KNOWN_SUBPARTS
    e = COUNT(SCHEME_CAR(root)) + COUNT(SCHEME_CDR(root));
#endif
    break;
  case scheme_vector_type:
    {
      int count = SCHEME_VEC_SIZE(root), i;
      Scheme_Object **array = SCHEME_VEC_ELS(root);

      s += count * sizeof(Scheme_Object*);

#if FORCE_KNOWN_SUBPARTS
      for (i = e = 0; i < count; i++)
	e += COUNT(array[i]);
#endif
    }
    break;
  case scheme_prim_type:
    {
      if (((Scheme_Primitive_Proc *)root)->flags & SCHEME_PRIM_IS_MULTI_RESULT)
	s = sizeof(Scheme_Prim_W_Result_Arity);
      else
	s = sizeof(Scheme_Primitive_Proc);
    }	
    break;
  case scheme_closure_type:
#if FORCE_KNOWN_SUBPARTS
    e = (COUNT(SCHEME_CLOS_CODE(root))
	 + COUNT(SCHEME_CLOS_ENV(root)));
#endif
    break;
  case scheme_linked_closure_type:
    {
      Scheme_Closure_Compilation_Data *data;
      Scheme_Object **vals;
      
      data = (Scheme_Closure_Compilation_Data *)SCHEME_COMPILED_CLOS_CODE(root);
      vals = SCHEME_COMPILED_CLOS_ENV(root);

      s += (data->closure_size * sizeof(Scheme_Object *));
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(data) + scheme_count_closure(vals, data->closure_size, ht);
#endif
    }
    break;
  case scheme_closed_prim_type:
    {
      if (((Scheme_Closed_Primitive_Proc *)root)->flags & SCHEME_PRIM_IS_MULTI_RESULT)
	s = sizeof(Scheme_Closed_Prim_W_Result_Arity);
      else
	s = sizeof(Scheme_Closed_Primitive_Proc);
    }	
    break;
  case scheme_cont_type:
    {
      Scheme_Cont *c = (Scheme_Cont *)root;
      Scheme_Saved_Stack *rs;

      s = sizeof(Scheme_Cont);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(c->home);
#endif

      for (rs = c->runstack_copied; rs; rs = rs->prev) {
	s += sizeof(Scheme_Saved_Stack);
	scheme_count_closure(rs->runstack,
			     rs->runstack_size
			     - (rs->runstack
				- rs->runstack_start),
			     ht);
      }
    }
    break;
  case scheme_input_port_type: 
    scheme_count_input_port(root, &s, &e, ht);
    break;
  case scheme_output_port_type:
    scheme_count_output_port(root, &s, &e, ht);
    break;
  case scheme_eof_type:
  case scheme_true_type: 
  case scheme_false_type:
  case scheme_void_type:
  case scheme_undefined_type:
    /* Only one */
    break;
  case scheme_syntax_compiler_type:
    break;
  case scheme_macro_type:
  case scheme_id_macro_type:
  case scheme_exp_time_type:
    s = sizeof(Scheme_Small_Object);
#if FORCE_KNOWN_SUBPARTS
    e = COUNT(SCHEME_PTR_VAL(root));
#endif
    break;
  case scheme_promise_type: 
    s = sizeof(Scheme_Promise);
#if FORCE_KNOWN_SUBPARTS
    e = COUNT(((Scheme_Promise *)root)->val);
#endif
    break;
  case scheme_box_type:
    s = sizeof(Scheme_Small_Object);
#if FORCE_KNOWN_SUBPARTS
    e = COUNT(SCHEME_BOX_VAL(root));
#endif
    break;
  case scheme_will_executor_type:
    s = sizeof(Scheme_Object);
    break;
  case scheme_manager_type: 
    {
      Scheme_Manager *m = (Scheme_Manager *)root;

      s = sizeof(Scheme_Manager);
      e = m->alloc * (sizeof(Scheme_Object **)
		      + sizeof(Scheme_Manager_Reference *)
		      + sizeof(void *)
		      + sizeof(void *));
    }
    break;
  case scheme_process_type:
    {
      Scheme_Process *p = (Scheme_Process *)root;
      Scheme_Saved_Stack *saved;

      s = sizeof(Scheme_Process)
	+ ((p->runstack_size + p->tail_buffer_size) * sizeof(Scheme_Object *));

#if FORCE_KNOWN_SUBPARTS
      e = COUNT(p->config);
#endif

      /* Check stack: */
      
      scheme_count_closure(p->runstack,
			   p->runstack_size
			   - (p->runstack
			      - p->runstack_start),
			   ht);
      for (saved = p->runstack_saved; saved; saved = saved->prev) {
	s += (saved->runstack_size * sizeof(Scheme_Object *));
	scheme_count_closure(saved->runstack,
			     saved->runstack_size
			     - (saved->runstack
				- saved->runstack_start),
			     ht);
      }
    }
    break;
  case scheme_namespace_type:
    {
      Scheme_Env *env = (Scheme_Env *)root;

      s = sizeof(Scheme_Env);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(env->globals) + COUNT(env->nonempty_cond);
#endif
    }
    break;
  case scheme_config_type:
    {
      s = sizeof(Scheme_Config) + (sizeof(Scheme_Object *) * __MZCONFIG_BUILTIN_COUNT__);
#if FORCE_SUBPARTS
      {
	Scheme_Config *c = (Scheme_Config *)root;
	int i;

	e = COUNT(c->extensions) + COUNT(c->base);

	for (i = 0; i < __MZCONFIG_BUILTIN_COUNT__; i++)
	  e += COUNT(*c->configs[i]);
      }
#endif
    }
    break;
  case scheme_defaulting_config_type:
#if FORCE_SUBPARTS
    e += COUNT(SCHEME_LPTR_VAL(root));
#endif
    break;
#ifndef NO_UNIT_SYSTEM
  case scheme_unit_type:
  case scheme_compiled_unit_type:
  case scheme_unit_body_data_type:
  case scheme_unit_compound_data_type:
  case scheme_invoke_unit_data_type:
    scheme_count_unit(type, root, &s, &e, ht);    
    break;
#endif
#ifndef NO_OBJECT_SYSTEM
  case scheme_object_type:
    scheme_count_object(root, &s, &e, ht);
    break;
  case scheme_class_type:
    scheme_count_class(root, &s, &e, ht);
    break;
#endif
  case scheme_structure_type:
    {
      Scheme_Object **slots = ((Scheme_Structure *)root)->slots;
      int i, count = SCHEME_STRUCT_NUM_SLOTS(root);

      s = sizeof(Scheme_Structure) + (count - 1) * sizeof(Scheme_Object *);
#if FORCE_KNOWN_SUBPARTS
      for (i = e = 0; i < count; i++)
	e += COUNT(slots[i]);
      e += COUNT(((Scheme_Structure *)root)->stype);
#endif
    }
    break;
  case scheme_generic_type:
    s = 0; /* Unknown */
    break;
  case scheme_type_symbol_type:
    s = sizeof(Scheme_Small_Object);
#if FORCE_KNOWN_SUBPARTS
    e = COUNT(SCHEME_PTR_VAL(root));
#endif
    break;
  case scheme_bignum_type:
    {
      int count = SCHEME_BIGLEN(root);

      if (count < 0)
	count = -count;

      s = sizeof(Small_Bignum) + (count - 1) * sizeof(bigdig);
    }
    break;
  case scheme_escaping_cont_type:
    s = sizeof(Scheme_Escaping_Cont);
    break;
  case scheme_sema_type:
    s = sizeof(Scheme_Sema);
    break;
  case scheme_compilation_top_type:
    s = sizeof(Scheme_Compilation_Top);
    break;
  case scheme_hash_table_type:
    {
      Scheme_Hash_Table *ht = (Scheme_Hash_Table *)root;

      s = sizeof(Scheme_Hash_Table)
	+ ht->size * sizeof(Scheme_Object *);
      
#if FORCE_SUBPARTS
      {
	int i;
	for (i = e = 0; i < ht->size; i++)
	  if (ht->buckets[i]) {
	    if (ht->by_address)
	      e += COUNT(ht->buckets[i]);
	    else
	      e += COUNT(ht->buckets[i]->val);
	  }
      }
#endif
    }
    break;
#ifndef NO_OBJECT_SYSTEM
  case scheme_generic_data_type:
    scheme_count_generic(root, &s, &e, ht);
    break;
#endif
  case scheme_weak_box_type:
    s = sizeof(Scheme_Small_Object);
    e = COUNT(SCHEME_BOX_VAL(root));
    break;
  case scheme_complex_type:
    s = sizeof(Scheme_Complex);
    e = COUNT(((Scheme_Complex *)root)->r) + COUNT(((Scheme_Complex *)root)->i);
    break;
  case scheme_rational_type:
    s = sizeof(Scheme_Rational);
#if FORCE_KNOWN_SUBPARTS
    e = COUNT(((Scheme_Rational *)root)->num) 
      + COUNT(((Scheme_Rational *)root)->denom);
#endif
    break;
  case scheme_struct_type_type:
    {
      Scheme_Struct_Type *st = (Scheme_Struct_Type *)root;
      s = sizeof(Scheme_Struct_Type) + st->name_pos * sizeof(Scheme_Object*);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(st->type_name);
      if (st->name_pos)
	e += COUNT(st->parent_types[st->name_pos - 1]);
#endif
    }
    break;
  case scheme_listener_type:
    s = sizeof(Scheme_Small_Object);
    break;
  case scheme_reserved_4_type:
    s = 0; /* Not yet used */
    break;
  case scheme_eval_waiting_type:
  case scheme_tail_call_waiting_type:
    /* Only one */
    break;
#ifndef NO_OBJECT_SYSTEM
  case scheme_class_data_type:
    scheme_count_class_data(root, &s, &e, ht);
    break;
#endif
  case scheme_struct_info_type:
    scheme_count_struct_info(root, &s, &e, ht);
    break;
  case scheme_multiple_values_type:
    /* Only one */
    break;
  case scheme_placeholder_type:
    s = 0; /* Infrequent */
    break;
  default:
    s = 0;
    break;
  }

  if (need_align) {
    /* Round up to sizeof(void*) boundary: */
    if (s & (sizeof(void*) - 1))
      s += sizeof(void*) - (s & (sizeof(void*) - 1));
  }

  scheme_memory_count[type]++;
  scheme_memory_size[type] += s;

  return s;
}

long scheme_count_envbox(Scheme_Object *root, Scheme_Hash_Table *ht)
{
#if CAN_TRACE_HOME
  if (GC_set(root) != envunbox) {
    scheme_console_printf("Bad envunbox object: %lx\n", (unsigned long)root);
    return 0;
  }
#endif

  return scheme_count_memory(SCHEME_ENVBOX_VAL(root), ht) + 4;
}

#if 0
Scheme_Object *scheme_dump_memory_count(int c, Scheme_Object **a)
{
  int i, special = 0;
  Scheme_Object *root;
  Scheme_Hash_Table *ht;
  long total_count = 0, total_local = 0, total_extended = 0;

  ht = scheme_hash_table(1000, SCHEME_hash_ptr, 0, 0);

  if (c) {
    if (SCHEME_FALSEP(a[0])) {
      special = 1;
      root = (Scheme_Object *)scheme_symbol_table;
    } else
      root = a[0];
  } else {
    root = (Scheme_Object *)scheme_first_process;
  }

  scheme_count_memory(root, ht);

  for (i = 0; i < _scheme_last_type_; i++) {
    if (scheme_memory_count[i]) {
      scheme_console_printf("%30.30s %10ld %10ld %10ld\n",
			    scheme_get_type_name(i),
			    scheme_memory_count[i],
			    scheme_memory_local[i],
			    scheme_memory_extended[i]);
      total_count += scheme_memory_count[i];
      total_local += scheme_memory_local[i];
      total_extended += scheme_memory_extended[i];
    }
  }

  scheme_console_printf("%30.30s %10ld %10ld %10ld\n",
			"total", 
			total_count,
			total_local,
			total_extended);
  if (special) {
    scheme_console_printf("%30.30s %10ld\n",
			  "type tables", 
			  scheme_type_table_count);
    scheme_console_printf("%30.30s %10ld\n",
			  "miscellaneous", 
			  scheme_misc_count);
  }

  return scheme_void;
}
#endif

#endif
