/*
 * File:		wb_hash.cc
 * Purpose:	Hash table implementation
 * Author:		Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#if defined(_MSC_VER)
# include "wx.h"
#else
# ifdef wx_xt
#  define  Uses_wxHashTable
#  include "wx.h"
# else
#  include "common.h"
#  include "wx_list.h"
#  include "wx_hash.h"
#  include "wx_types.h"
# endif
#endif

#include <string.h>
#include <stdarg.h>

wxHashTable::wxHashTable (int, int size)
{
  int i;
  wxList **ll;

  __type = wxTYPE_HASH_TABLE;
  n = size;
  current_position = -1;
  current_node = NULL;

  ll = new wxList *[size];
  hash_table = ll;
  for (i = 0; i < size; i++) {
    hash_table[i] = NULL;
  }
}

wxHashTable::~wxHashTable (void)
{
  int i;
  for (i = 0; i < n; i++) {
    if (hash_table[i]) {
      wxList *l;
      l = hash_table[i];
      DELETE_OBJ l;
    }
  }
}

wxList *wxHashTable::GetList(int position, KeyType ktype, Bool makeit)
{
  wxList *l;

  l = hash_table[position];

  if (!l) {
    if (makeit) {
      l = new wxList(ktype, FALSE);
      hash_table[position] = l;
    }
  }
  
  return l;
}

void wxHashTable::Put(long key, wxObject * object)
{
  wxList *l;

  l = GetList(MakeKey(key));

  l->Append(key, object);
}

void wxHashTable::Put(const char *key, wxObject * object)
{
  wxList *l;

  l = GetList(MakeKey(key), wxKEY_STRING);

  l->Append(key, object);
}

wxObject *wxHashTable::Get(long key)
{
  wxList *l;

  l = GetList(MakeKey(key), wxKEY_INTEGER, FALSE);

  if (l) {
    wxNode *node;
    node = l->Find(key);
    if (node)
      return node->Data();
  }

  return NULL;
}

wxObject *wxHashTable::Get(const char *key)
{
  wxList *l;

  l = GetList(MakeKey(key), wxKEY_STRING, FALSE);

  if (l) {
    wxNode *node;
    node = l->Find (key);
    if (node)
      return node->Data();
  }

  return NULL;
}

wxObject *wxHashTable::Delete(long key)
{
  wxList *l;

  l = GetList(MakeKey(key), wxKEY_INTEGER, FALSE);

  if (l) {
    wxNode *node;
    node = l->Find(key);
    if (node) {
      wxObject *data;
      data = node->Data();
      l->DeleteNode(node);
      return data;
    }
  }
  return NULL;
}

wxObject *wxHashTable::Delete(const char *key)
{
  wxList *l;

  l = GetList(MakeKey(key), wxKEY_STRING, FALSE);

  if (l) {
    wxNode *node;
    node = l->Find(key);
    if (node) {
      wxObject *data;
      data = node->Data();
      l->DeleteNode(node);
      return data;
    }
  }

  return NULL;
}

int wxHashTable::MakeKey(const char *string)
{
  long int_key = 0;

  while (*string) {
    int_key += (unsigned char) *string++;
  }

  if (int_key < 0)
    int_key = -int_key;

  return int_key % n;
}

int wxHashTable::MakeKey(long int_key)
{
  if (int_key < 0)
    int_key = -int_key;

  return int_key % n;
}

void wxHashTable::BeginFind (void)
{
  current_position = -1;
  current_node = NULL;
}

wxNode *wxHashTable::Next (void)
{
  wxNode *found = NULL;
  Bool end = FALSE;
  while (!end && !found) {
    if (!current_node) {
      current_position++;
      if (current_position >= n) {
	current_position = -1;
	current_node = NULL;
	end = TRUE;
      } else {
	wxList *l;
	l = hash_table[current_position];
	if (l) {
	  current_node = l->First();
	  found = current_node;
	}
      }
    } else {
      current_node = current_node->Next ();
      found = current_node;
    }
  }
  return found;
}

void wxHashTable::DeleteContents (Bool flag)
{
  int i;
  for (i = 0; i < n; i++) {
    if (hash_table[i]) {
      wxList *l;
      l = hash_table[i];
      l->DeleteContents(flag);
    }
  }
}

void wxHashTable::Clear (void)
{
  int i;
  for (i = 0; i < n; i++) {
    if (hash_table[i])  {
      wxList *l;
      l = hash_table[i];
      l->Clear();
    }
  }
}



/* This is a hash table implementation which does not lock the objects
   from garbage collection. */
/* FIXME: doesn't work for precise GC */

typedef struct Bucket {
  long widget;
  wxObject *object;
} Bucket;

/* because widgets are likely to be word-aligned */
#define HASH(w) ((((unsigned long)w) >> 2) % numbuckets)

#define FILL_FACTOR 2 /* inverted max fraction of hash table implying reash */

wxNonlockingHashTable::wxNonlockingHashTable()
{
  long i;

  numbuckets = 1001;
  buckets = (Bucket *)GC_malloc_atomic(sizeof(Bucket) * numbuckets);
  for (i = 0; i < numbuckets; i++) {
    buckets[i].widget = 0;
  }
  numwidgets = 0;
}

wxNonlockingHashTable::~wxNonlockingHashTable()
{
}

void wxNonlockingHashTable::Put(long widget, wxObject *object)
{
  long i;

  if (FILL_FACTOR * numwidgets >= numbuckets) {
    /* Rehash */
    Bucket *oldbuckets = buckets;
    long oldnumbuckets = numbuckets;

    numbuckets = (numbuckets * FILL_FACTOR) + 1;
    buckets = (Bucket *)GC_malloc_atomic(sizeof(Bucket) * numbuckets);

    numwidgets = 0;
    for (i = 0; i < oldnumbuckets; i++) {
      if (oldbuckets[i].widget && oldbuckets[i].object)
	Put(oldbuckets[i].widget, oldbuckets[i].object);
    }
  }

  i = HASH(widget);
  /* MATTHEW: [10] Added equality check (shouldn't happen, though). */
  while (buckets[i].widget && buckets[i].object
	 && (buckets[i].widget != widget)) {
    i = (i + 1) % numbuckets;
  }
  buckets[i].widget = widget;
  buckets[i].object = object;
  numwidgets++; /* Fix counter */
}

wxObject *wxNonlockingHashTable::Get(long widget)
{
  long i;

  i = HASH(widget);
  while ((buckets[i].widget != widget) && buckets[i].widget) {
    i = (i + 1) % numbuckets;
  }

  if (buckets[i].widget == widget)
    return buckets[i].object;

  return NULL;
}

void wxNonlockingHashTable::Delete(long widget)
{
  long i;

  i = HASH(widget);
  while ((buckets[i].widget != widget) && buckets[i].widget) {
    i = (i + 1) % numbuckets;
  }

  if (buckets[i].widget == widget)
  {
    buckets[i].object = NULL;
    --numwidgets; /* Fix counter */
  }
}

/* new method (not very fast) */
void wxNonlockingHashTable::DeleteObject(wxObject *o)
{
  long i;
  
  for (i = 0; i < numbuckets; i++) {
    if (buckets[i].widget && buckets[i].object == o)
      Delete(buckets[i].widget);
  }
}

