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

#ifdef DOS_MEMORY
#include <dos.h>

#define PTR_TO_LONG(p) ((FP_SEG(p) << 4) + FP_OFF(p))
#else
#define PTR_TO_LONG(p) ((long)(p))
#endif

#ifdef SMALL_HASH_TABLES
#define FILL_FACTOR 1.30
#else
#define FILL_FACTOR 2
#endif

int scheme_hash_primes[] = 
{7, 31, 127, 257, 521, 1031, 2053, 4099, 8209, 16411, 
   32779, 65543, 131101, 262147, 425329, 1048583, 2097169,
   4194319, 8388617, 16777259, 33554467, 67108879, 134217757,
   268435459, 536870923, 1073741827};

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
  while (scheme_hash_primes[table->step] < size)
    table->step++;
  table->size = scheme_hash_primes[table->step];

  table->count = 0;

  table->type = scheme_hash_table_type;

  asize = (size_t)table->size * sizeof(Scheme_Bucket *);
  if (forever) {
    table->buckets = (Scheme_Bucket **)scheme_malloc_atomic(asize);
    memset((char *)table->buckets, 0, asize);
  } else
    table->buckets = (Scheme_Bucket **)scheme_malloc(asize);

  table->has_constants = has_const;
  table->forever = forever;
  table->weak = (type == SCHEME_hash_weak_ptr);

  if (type == SCHEME_hash_string) {
    table->make_hash_indices = string_hash_indices;
    table->compare = (int (*)(void*, void*))strcmp;
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
    h = (PTR_TO_LONG(key) >> 2) % table->size;
    h2 = (PTR_TO_LONG(key) >> 3) % table->size;
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
	void *hk = *(void **)bucket->key;
	if (!hk) {
	  /* Re-use a bucket slot whose key is collected: */
	  /* DON'T increment counter overall... */
	  --table->count;
	  break;
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
    if (table->forever) {
      table->buckets = (Scheme_Bucket **)scheme_malloc_atomic(asize);
      memset((char *)table->buckets, 0, asize);
    } else
      table->buckets = (Scheme_Bucket **)scheme_malloc(asize);

    table->count = 0;
    if (table->weak) {
      for (i = 0; i < oldsize; i++)
	if (old[i] && old[i]->key && *(void **)old[i]->key)
	  get_bucket(table, *(char **)old[i]->key, 1, old[i]);
    } else {
      for (i = 0; i < oldsize; i++)
	if (old[i] && old[i]->key)
	  get_bucket(table, 
		     table->weak ? *(char **)old[i]->key : old[i]->key, 
		     1, old[i]);
    }

    goto rehash_key;
  }

  if (b) {
    bucket = b;
  } else {
    size_t bsize;

    if (!table->has_constants)
      bsize = sizeof(Scheme_Bucket);
    else if (table->has_constants == 2)
      bsize = sizeof(Scheme_Bucket_With_Ref_Id);
    else
      bsize = sizeof(Scheme_Bucket_With_Const_Flag);

    if (table->forever)
      bucket = (Scheme_Bucket *)scheme_malloc_uncollectable_tagged(bsize);
    else
      bucket = (Scheme_Bucket *)scheme_malloc_tagged(bsize);

    bucket->type = scheme_variable_type;

    if (table->has_constants)
      ((Scheme_Bucket_With_Const_Flag *)bucket)->flags = 0;

    if (table->weak) {
      bucket->key = (char *)scheme_malloc_atomic(sizeof(void *));
      *(void **)bucket->key = (void *)key;
      scheme_weak_reference_indirect((void **)bucket->key, (void *)key);
      scheme_weak_reference_indirect((void **)&bucket->val, (void *)key);
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
    scheme_raise_exn(MZEXN_MISC_CONSTANT, key,
		     "define: cannot redefine constant %s", 
		     scheme_symbol_name((Scheme_Object *)key));

  if (val)
    b->val = val;
  if (constant && table->has_constants)
    ((Scheme_Bucket_With_Const_Flag *)b)->flags |= GLOB_IS_CONST;
}

void scheme_add_bucket_to_table(Scheme_Hash_Table *table, Scheme_Bucket *b)
{
  get_bucket(table, table->weak ? *(char **)b->key : b->key, 1, b);
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
