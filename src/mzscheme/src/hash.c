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

#include "schpriv.h"
#include <string.h>
#include <ctype.h>
#include <memory.h>

#ifdef MZ_PRECISE_GC
# define PTR_TO_LONG(p) scheme_hash_key(p)
#else
# ifdef DOS_MEMORY
#  include <dos.h>
#  define PTR_TO_LONG(p) ((FP_SEG(p) << 4) + FP_OFF(p))
# else
#  define PTR_TO_LONG(p) ((long)(p))
# endif
#endif

#ifdef SMALL_HASH_TABLES
#define FILL_FACTOR 1.30
#else
#define FILL_FACTOR 2
#endif

#ifdef MZ_PRECISE_GC
# define USE_FOREVER 0
#else
# define USE_FOREVER 1
#endif

int scheme_hash_primes[] = 
{7, 31, 127, 257, 521, 1031, 2053, 4099, 8209, 16411, 
   32779, 65543, 131101, 262147, 425329, 1048583, 2097169,
   4194319, 8388617, 16777259, 33554467, 67108879, 134217757,
   268435459, 536870923, 1073741827};

typedef int (*Compare_Proc)(void*, void*);

static void string_hash_indices(void *_key, int *_h, int *_h2)
{
  const char *key = (char *)_key;
  int i, h, h2;

  h2 = h = i = 0;
  while (key[i]) {
    int c = key[i++];
    if (!scheme_case_sensitive)
      if (isupper(c))
	c = tolower(c);
    h += (h << 5) + h + c;
    h2 += c;
  }

  *_h = h;
  *_h2 = h2;
}

Scheme_Hash_Table *
scheme_hash_table (int size, int type, int has_const, int forever)
{
  Scheme_Hash_Table *table;
  size_t asize;

  table = MALLOC_ONE_TAGGED(Scheme_Hash_Table);

  table->step = 0;
  while (scheme_hash_primes[table->step] < size) {
    table->step++;
  }
  table->size = scheme_hash_primes[table->step];

  table->count = 0;

  table->type = scheme_hash_table_type;

  asize = (size_t)table->size * sizeof(Scheme_Bucket *);
#if USE_FOREVER
  if (forever) {
    table->buckets = (Scheme_Bucket **)scheme_malloc_atomic(asize);
    memset((char *)table->buckets, 0, asize);
  } else
#endif
    {
      Scheme_Bucket **ba;
      ba = (Scheme_Bucket **)scheme_malloc(asize);
      table->buckets = ba;
    }

  table->has_constants = has_const;
  table->forever = forever;
  table->weak = (type == SCHEME_hash_weak_ptr);

  if (type == SCHEME_hash_string) {
    table->make_hash_indices = string_hash_indices;
    table->compare = (Compare_Proc)strcmp;
  }

#ifdef MZ_REAL_THREADS
  table->mutex = SCHEME_MAKE_MUTEX();
#endif

  return table;
}

typedef int hash_v_t;

static Scheme_Bucket *
get_bucket (Scheme_Hash_Table *table, const char *key, int add, Scheme_Bucket *b)
{
  hash_v_t h, h2;
  Scheme_Bucket *bucket;

 rehash_key:

  if (table->make_hash_indices) {
    table->make_hash_indices((void *)key, &h, &h2);
    h = h % table->size;
    h2 = h2 % table->size;
  } else {
    long lkey;
    lkey = PTR_TO_LONG((Scheme_Object *)key);
    h = (lkey >> 2) % table->size;
    h2 = (lkey >> 3) % table->size;
  }

  if (h < 0) h = -h;
  if (h2 < 0) h2 = -h2;
  
  if (!h2)
    h2 = 2;
  else if (h2 & 0x1)
    h2++;

  if (table->weak) {
    while ((bucket = table->buckets[h])) {
      if (bucket->key) {
	void *hk = (void *)HT_EXTRACT_WEAK(bucket->key);
	if (!hk) {
	  if (add) {
	    /* Re-use a bucket slot whose key is collected: */
	    /* DON'T increment counter overall... */
	    --table->count;
	    break;
	  }
	} else if (SAME_PTR(hk, key))
	  return bucket;
      } else if (add)
	break;
      h = (h + h2) % table->size;
    }
  } else if (table->compare) {
    while ((bucket = table->buckets[h])) {
      if (!table->compare(bucket->key, (char *)key))
	return bucket;
      h = (h + h2) % table->size;
    }
  } else {
    while ((bucket = table->buckets[h])) {
      if (SAME_PTR(bucket->key, key))
	return bucket;
      h = (h + h2) % table->size;
    }
  }

  if (!add)
    return NULL;

  if (table->count * FILL_FACTOR >= table->size) {
    /* Rehash */
    int i, oldsize = table->size;
    size_t asize;
    Scheme_Bucket **old = table->buckets;

    table->size = scheme_hash_primes[++table->step];
    
    asize = (size_t)table->size * sizeof(Scheme_Bucket *);
#if USE_FOREVER
    if (table->forever) {
      table->buckets = (Scheme_Bucket **)scheme_malloc_atomic(asize);
      memset((char *)table->buckets, 0, asize);
    } else
#endif
      {
	Scheme_Bucket **ba;
	ba = (Scheme_Bucket **)scheme_malloc(asize);
	table->buckets = ba;
      }

    table->count = 0;
    if (table->weak) {
      for (i = 0; i < oldsize; i++) {
	if (old[i] && old[i]->key && HT_EXTRACT_WEAK(old[i]->key))
	  get_bucket(table, (char *)HT_EXTRACT_WEAK(old[i]->key), 1, old[i]);
      }
    } else {
      for (i = 0; i < oldsize; i++) {
	if (old[i] && old[i]->key)
	  get_bucket(table, old[i]->key, 1, old[i]);
      }
    }

    goto rehash_key;
  }

  if (b) {
    bucket = b;
  } else {
    size_t bsize;

#ifndef MZ_PRECISE_GC
    if (!table->has_constants)
      bsize = sizeof(Scheme_Bucket);
    else if (table->has_constants != 2)
      bsize = sizeof(Scheme_Bucket_With_Const_Flag);
    else
#endif
      bsize = sizeof(Scheme_Bucket_With_Ref_Id);

#if USE_FOREVER
    if (table->forever)
      bucket = (Scheme_Bucket *)scheme_malloc_uncollectable_tagged(bsize);
    else
#endif
      bucket = (Scheme_Bucket *)scheme_malloc_tagged(bsize);

    bucket->type = scheme_variable_type;

    if (table->has_constants)
      ((Scheme_Bucket_With_Const_Flag *)bucket)->flags = 0;

    if (table->weak) {
#ifdef MZ_PRECISE_GC
      void *kb;
      kb = GC_malloc_weak_box((void *)key, (void **)&bucket->key);
      bucket->key = (char *)kb;
#else
      char *kb;
      kb = (char *)MALLOC_ONE_WEAK(void *);
      bucket->key = kb;
      *(void **)bucket->key = (void *)key;
      scheme_weak_reference_indirect((void **)bucket->key, (void *)key);
      scheme_weak_reference_indirect((void **)&bucket->val, (void *)key);
#endif
    } else
      bucket->key = (char *)key;
    bucket->val = NULL;
  }

  table->buckets[h] = bucket;

  table->count++;

  return bucket;
}

Scheme_Bucket *
scheme_bucket_or_null_from_table (Scheme_Hash_Table *table, const char *key, int add)
{
  Scheme_Bucket *b;

#ifdef MZ_REAL_THREADS
  SCHEME_LOCK_MUTEX(table->mutex);
#endif

  b = get_bucket(table, key, add, NULL);

#ifdef MZ_REAL_THREADS
  SCHEME_UNLOCK_MUTEX(table->mutex);
#endif

  return b;
}

Scheme_Bucket *
scheme_bucket_from_table (Scheme_Hash_Table *table, const char *key)
{
  return scheme_bucket_or_null_from_table(table, key, 1);
}

void 
scheme_add_to_table (Scheme_Hash_Table *table, const char *key, void *val, 
		     int constant)
{
  Scheme_Bucket *b;

#ifdef MZ_REAL_THREADS
  SCHEME_LOCK_MUTEX(table->mutex);
#endif

  b = get_bucket(table, key, 1, NULL);

#ifdef MZ_REAL_THREADS
  SCHEME_UNLOCK_MUTEX(table->mutex);
#endif

  if (table->has_constants
      && (((Scheme_Bucket_With_Const_Flag *)b)->flags & GLOB_IS_CONST)
      && b->val)
    scheme_raise_exn(MZEXN_VARIABLE_KEYWORD, key,
		     "define: cannot redefine constant %s", 
		     scheme_symbol_name((Scheme_Object *)key));

  if (val)
    b->val = val;
  if (constant && table->has_constants)
    ((Scheme_Bucket_With_Const_Flag *)b)->flags |= GLOB_IS_CONST;
}

void scheme_add_bucket_to_table(Scheme_Hash_Table *table, Scheme_Bucket *b)
{
  get_bucket(table, table->weak ? (char *)HT_EXTRACT_WEAK(b->key) : b->key, 1, b);
}

void *
scheme_lookup_in_table (Scheme_Hash_Table *table, const char *key)
{
  Scheme_Bucket *bucket;

#ifdef MZ_REAL_THREADS
  SCHEME_LOCK_MUTEX(table->mutex);
#endif

  bucket = get_bucket(table, key, 0, NULL);

#ifdef MZ_REAL_THREADS
  SCHEME_UNLOCK_MUTEX(table->mutex);
#endif

  if (bucket)
    return bucket->val;
  else
    return NULL;
}

void
scheme_change_in_table (Scheme_Hash_Table *table, const char *key, void *naya)
{
  Scheme_Bucket *bucket;

#ifdef MZ_REAL_THREADS
  SCHEME_LOCK_MUTEX(table->mutex);
#endif

  bucket = get_bucket(table, key, 0, NULL);

#ifdef MZ_REAL_THREADS
  SCHEME_UNLOCK_MUTEX(table->mutex);
#endif

  if (bucket)
    bucket->val = naya;
}

/***********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

typedef long (*Hash_Key_Proc)(Scheme_Object *o);
Hash_Key_Proc hash_key_procs[_scheme_last_normal_type_];
static short keygen;

static long hash_addr(Scheme_Object *o)
{
  return (long)o;
}

static long hash_general(Scheme_Object *o)
{
  if (!(((short *)o)[1] & 0xFFFC)) {
    if (!keygen)
      keygen += 4;
    ((short *)o)[1] |= keygen;
    keygen += 4;
  }

  return *(long *)o;
}

static long hash_prim(Scheme_Object *o)
{
  return (long)((Scheme_Primitive_Proc *)o)->prim_val;
}

static long hash_case(Scheme_Object *o)
{
  Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)o;

  if (cl->count)
    return scheme_hash_key(cl->array[0]);
  else
    return scheme_case_closure_type << 2;
}

static long hash_bignum(Scheme_Object *o)
{
  int i = SCHEME_BIGLEN(o);
  bigdig *d = SCHEME_BIGDIG(o);
  long k = 0;
  
  while (i--) {
    k += d[i];
  }
  
  return k;
}

void scheme_init_hash_key_procs(void)
{
#define PROC(t,f) hash_key_procs[t] = f
  PROC(scheme_prim_type, hash_prim);
  PROC(scheme_closed_prim_type, hash_prim);
  PROC(scheme_linked_closure_type, hash_general);
  PROC(scheme_case_closure_type, hash_case);
  PROC(scheme_cont_type, hash_general);
  PROC(scheme_escaping_cont_type, hash_general);
  PROC(scheme_char_type, hash_addr);
  PROC(scheme_bignum_type, hash_bignum);
  PROC(scheme_rational_type, hash_general);
  PROC(scheme_float_type, hash_general);
  PROC(scheme_double_type, hash_general);
  PROC(scheme_complex_izi_type, hash_general);
  PROC(scheme_complex_type, hash_general);
  PROC(scheme_string_type, hash_general);
  PROC(scheme_symbol_type, hash_general);
  PROC(scheme_null_type, hash_addr);
  PROC(scheme_pair_type, hash_general);
  PROC(scheme_vector_type, hash_general);
  PROC(scheme_closure_type, hash_general);
  PROC(scheme_input_port_type, hash_general);
  PROC(scheme_output_port_type, hash_general);
  PROC(scheme_eof_type, hash_addr);
  PROC(scheme_true_type, hash_addr);
  PROC(scheme_false_type, hash_addr);
  PROC(scheme_void_type, hash_addr);
  PROC(scheme_syntax_compiler_type, hash_general);
  PROC(scheme_macro_type, hash_general);
  PROC(scheme_promise_type, hash_general);
  PROC(scheme_box_type, hash_general);
  PROC(scheme_process_type, hash_general);
  PROC(scheme_object_type, hash_general);
  PROC(scheme_class_type, hash_general);
  PROC(scheme_structure_type, hash_general);
  PROC(scheme_cont_mark_set_type, hash_general);
  PROC(scheme_sema_type, hash_general);
  PROC(scheme_hash_table_type, hash_general);
  PROC(scheme_weak_box_type, hash_general);
  PROC(scheme_struct_type_type, hash_general);
  PROC(scheme_id_macro_type, hash_general);
  PROC(scheme_unit_type, hash_general);
  PROC(scheme_exp_time_type, hash_general);
  PROC(scheme_listener_type, hash_general);
  PROC(scheme_namespace_type, hash_general);
  PROC(scheme_config_type, hash_general);
  PROC(scheme_will_executor_type, hash_general);
  PROC(scheme_interface_type, hash_general);
  PROC(scheme_manager_type, hash_general);
  PROC(scheme_random_state_type, hash_general);
  PROC(scheme_regexp_type, hash_general);
  PROC(scheme_compilation_top_type, hash_general);
  PROC(scheme_placeholder_type, hash_general);
#undef PROC
}

long scheme_hash_key(Scheme_Object *o)
{
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return (long)o;

  t = SCHEME_TYPE(o);

  if (!hash_key_procs[t]) {
    printf("Can't hash %d\n", t);
    exit(0);
  }

  return hash_key_procs[t](o);
}

END_XFORM_SKIP;

#endif
