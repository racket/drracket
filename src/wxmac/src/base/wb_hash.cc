/*
 * File:      wb_hash.cc
 * Purpose:     Hash table implementation
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_hash.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "@(#)wb_hash.cc	1.2 5/9/94"; */

#ifdef __GNUG__
#pragma implementation "wx_hash.h"
#endif

#include "common.h"
#include "wx_list.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#include "wx_hash.h"
#include "wx_types.h"

#include <string.h>
#include <stdarg.h>

wxHashTable::wxHashTable (unsigned int the_key_type, size_t size)
{
  __type = wxTYPE_HASH_TABLE;
  n = size;
  current_position = -1;
  current_node = NULL;

  key_type = wxKEY_NONE;
  hash_table = new wxList *[size];
  for (size_t i = 0; i < size; i++)
    hash_table[i] = NULL;
}

wxHashTable::~wxHashTable (void)
{
  for (size_t i = 0; i < n; i++)
    if (hash_table[i])
      delete hash_table[i];
  delete hash_table;
}

void wxHashTable::Put (long key, long value, wxObject * object)
{
  // Should NEVER be
  if (key < 0)
    key = -key;

  size_t position = (size_t) (key % n);
  if (!hash_table[position])
    hash_table[position] = new wxList (wxKEY_INTEGER, FALSE);

  hash_table[position]->Append (value, object);
}

void wxHashTable::Put (long key, char *value, wxObject * object)
{
  // Should NEVER be
  if (key < 0)
    key = -key;

  size_t position = (size_t) (key % n);
  if (!hash_table[position])
    hash_table[position] = new wxList (wxKEY_INTEGER, FALSE);

  hash_table[position]->Append (value, object);
}

void wxHashTable::Put (long key, wxObject * object)
{
  // Should NEVER be
  if (key < 0)
    key = -key;

  size_t position = (size_t) (key % n);
  if (!hash_table[position])
    hash_table[position] = new wxList (wxKEY_INTEGER, FALSE);

  hash_table[position]->Append (key, object);
}

void wxHashTable::Put (const char *key, wxObject * object)
{
  size_t position = (size_t) (MakeKey (key) % n);

  if (!hash_table[position])
    hash_table[position] = new wxList (wxKEY_STRING, FALSE);

  hash_table[position]->Append (key, object);
}

wxObject *wxHashTable::Get (long key, long value)
{
  // Should NEVER be
  if (key < 0)
    key = -key;

  size_t position = (size_t) (key % n);
  if (!hash_table[position])
    return NULL;
  else
    {
      wxNode *node = hash_table[position]->Find (value);
      if (node)
	return node->Data ();
      else
	return NULL;
    }
}

wxObject *wxHashTable::Get (long key, char *value)
{
  // Should NEVER be
  if (key < 0)
    key = -key;

  size_t position = (size_t) (key % n);
  if (!hash_table[position])
    return NULL;
  else
    {
      wxNode *node = hash_table[position]->Find (value);
      if (node)
	return node->Data ();
      else
	return NULL;
    }
}

wxObject *wxHashTable::Get (long key)
{
  // Should NEVER be
  if (key < 0)
    key = -key;

  size_t position = (size_t) (key % n);
  if (!hash_table[position])
    return NULL;
  else
    {
      wxNode *node = hash_table[position]->Find (key);
      return node ? node->Data () : NULL;
    }
}

wxObject *wxHashTable::Get (const char *key)
{
  size_t position = (size_t) (MakeKey (key) % n);

  if (!hash_table[position])
    return NULL;
  else
    {
      wxNode *node = hash_table[position]->Find (key);
      return node ? node->Data () : NULL;
    }
}

wxObject *wxHashTable::Delete (long key)
{
  // Should NEVER be
  if (key < 0)
    key = -key;

  size_t position = (size_t) (key % n);
  if (!hash_table[position])
    return NULL;
  else
    {
      wxNode *node = hash_table[position]->Find (key);
      if (node)
	{
	  wxObject *data = node->Data ();
	  delete node;
	  return data;
	}
      else
	return NULL;
    }
}

wxObject *wxHashTable::Delete (const char *key)
{
  size_t position = (size_t) (MakeKey (key) % n);
  if (!hash_table[position])
    return NULL;
  else
    {
      wxNode *node = hash_table[position]->Find (key);
      if (node)
	{
	  wxObject *data = node->Data ();
	  delete node;
	  return data;
	}
      else
	return NULL;
    }
}

wxObject *wxHashTable::Delete (long key, int value)
{
  // Should NEVER be
  if (key < 0)
    key = -key;

  size_t position = (size_t) (key % n);
  if (!hash_table[position])
    return NULL;
  else
    {
      wxNode *node = hash_table[position]->Find (value);
      if (node)
	{
	  wxObject *data = node->Data ();
	  delete node;
	  return data;
	}
      else
	return NULL;
    }
}

wxObject *wxHashTable::Delete (long key, char *value)
{
/*
  // Should NEVER be
  if (key < 0)
    key = -key;
*/

  size_t position = (size_t) (key % n);
  if (!hash_table[position])
    return NULL;
  else
    {
      wxNode *node = hash_table[position]->Find (value);
      if (node)
	{
	  wxObject *data = node->Data ();
	  delete node;
	  return data;
	}
      else
	return NULL;
    }
}

long wxHashTable::MakeKey (const char *string)
{
  long int_key = 0;

  while (*string)
    int_key += (unsigned char) *string++;

/* // Don't need this since int_key >= 0) 
  if (int_key < 0)
    int_key = -int_key;
*/
  return int_key;
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
  while (!end && !found)
    {
      if (!current_node)
	{
	  current_position++;
	  if (current_position >= n)
	    {
	      current_position = -1;
	      current_node = NULL;
	      end = TRUE;
	    }
	  else
	    {
	      if (hash_table[current_position])
		{
		  current_node = hash_table[current_position]->First ();
		  found = current_node;
		}
	    }
	}
      else
	{
	  current_node = current_node->Next ();
	  found = current_node;
	}
    }
  return found;
}

void wxHashTable::DeleteContents (Bool flag)
{
  for (int i = 0; i < n; i++)
    {
      if (hash_table[i])
	hash_table[i]->DeleteContents (flag);
    }
}

void wxHashTable::Clear (void)
{
  for (int i = 0; i < n; i++)
    {
      if (hash_table[i])
	hash_table[i]->Clear ();
    }
}
