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
#include "schgc.h"

#ifdef SMALL_HASH_TABLES
# define HASH_TABLE_SIZE 2500
# define FILL_FACTOR 1.30
#else
# define HASH_TABLE_SIZE 10313
# define FILL_FACTOR 2
#endif

extern void (*GC_custom_finalize)(void);
#ifndef USE_SENORA_GC
extern int GC_is_marked(void *);
#endif

Scheme_Hash_Table *scheme_symbol_table = NULL;

long scheme_max_found_symbol_name;

/* globals */
int scheme_case_sensitive;

/* locals */
static Scheme_Object *symbol_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_symbol_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_uninterned_symbol_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *symbol_to_string_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *gensym(int argc, Scheme_Object *argv[]);

static int gensym_counter;

/**************************************************************************/

typedef int hash_v_t;

extern int scheme_hash_primes[];

/* Special hashing for symbols: */
static Scheme_Object *symbol_bucket(Scheme_Hash_Table *table, 
				    const char *key, int length,
				    Scheme_Object *naya)
{
  hash_v_t h, h2;
  Scheme_Bucket *bucket;

 rehash_key:

  {
    int i;
    h2 = h = i = 0;
    while (i < length) {
      int c = key[i++];
      h += (h << 5) + h + c;
      h2 += c;
    }
    h = h % table->size;
    h2 = h2 % table->size;
  }

  if (h < 0) h = -h;

  if (!h2)
    h2 = 2;
  else if (h2 & 0x1)
    h2++;

  if (h2 < 0)
    h2 = -h2;

  while ((bucket = table->buckets[h])) {
    if (SAME_OBJ((Scheme_Object *)bucket, scheme_false)) {
      if (naya)
	break;
    } else if ((length == SCHEME_SYM_LEN(bucket))
	&& !memcmp(key, SCHEME_SYM_VAL(bucket), length))
      return (Scheme_Object *)bucket;
    h = (h + h2) % table->size;
  }

  if (!naya)
    return NULL;

  if (table->count * FILL_FACTOR >= table->size) {
    /* Rehash */
    int i, oldsize = table->size, newsize;
    size_t asize;
    Scheme_Bucket **old = table->buckets;

    newsize = scheme_hash_primes[++table->step];
    
    asize = (size_t)newsize * sizeof(Scheme_Bucket *);
    table->buckets = (Scheme_Bucket **)scheme_malloc_atomic(asize);
    table->size = newsize;
    memset((char *)table->buckets, 0, asize);

    table->count = 0;
    for (i = 0; i < oldsize; i++)
      if (old[i] && !SAME_OBJ((Scheme_Object *)old[i], scheme_false))
	symbol_bucket(table, SCHEME_SYM_VAL(old[i]), SCHEME_SYM_LEN(old[i]),
		      (Scheme_Object *)old[i]);
    
    goto rehash_key;
  }

  table->buckets[h] = (Scheme_Bucket *)naya;

  table->count++;

  return naya;
}

static void clean_symbol_table(void)
{
  /* Clean the symbol table by removing pointers to collected
     symbols. The correct way to do this is to install a GC
     finalizer on symbol pointers, but that would be expensive. */
     
  if (scheme_symbol_table) {
    Scheme_Object **buckets = (Scheme_Object **)scheme_symbol_table->buckets;
    int i = scheme_symbol_table->size;
    void *b;
    
    while (i--) {
      if (buckets[i] && !SAME_OBJ(buckets[i], scheme_false)
	  && (!(b = GC_base(buckets[i]))
#ifndef USE_SENORA_GC
	      || !GC_is_marked(b)
#endif
	      ))
	buckets[i] = scheme_false;
    }
  }
}

/**************************************************************************/

void
scheme_init_symbol_table ()
{
  if (scheme_starting_up) {
    REGISTER_SO(scheme_symbol_table);

    scheme_symbol_table = scheme_hash_table(HASH_TABLE_SIZE, 
					    SCHEME_hash_ptr, 0, 1);

    GC_custom_finalize = clean_symbol_table;
  }
}

void
scheme_init_symbol_type (Scheme_Env *env)
{
}

void
scheme_init_symbol (Scheme_Env *env)
{
  if (scheme_starting_up) {
  }

  scheme_add_global_constant("symbol?", 
			     scheme_make_folding_prim(symbol_p_prim, 
						      "symbol?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("string->symbol", 
			     scheme_make_prim_w_arity(string_to_symbol_prim,
						      "string->symbol",
						      1, 1), env);
  scheme_add_global_constant("string->uninterned-symbol", 
			     scheme_make_prim_w_arity(string_to_uninterned_symbol_prim,
						      "string->uninterned-symbol",
						      1, 1), 
			     env);
  scheme_add_global_constant("symbol->string", 
			     scheme_make_prim_w_arity(symbol_to_string_prim,
						      "symbol->string", 
						      1, 1), 
			     env);
  
  scheme_add_global_constant("gensym", 
			     scheme_make_prim_w_arity(gensym,
						      "gensym",
						      0, 1), 
			     env);
}

static Scheme_Object *
make_a_symbol(const char *name, int len)
{
  Scheme_Symbol *sym;
  
  sym = (Scheme_Symbol *)scheme_malloc_atomic_tagged(sizeof(Scheme_Type) + 
						     sizeof(short) +
						     len + 1);
  
  sym->type = scheme_symbol_type;
  sym->len = len;
  memcpy(sym->s, name, len);
  sym->s[len] = 0;
  
  return (Scheme_Object *)(sym);
}

Scheme_Object *
scheme_make_symbol(const char *name)
{
  return make_a_symbol(name, strlen(name));
}

Scheme_Object *
scheme_make_exact_symbol(const char *name, int len)
{
  return make_a_symbol(name, len);
}

Scheme_Object *
scheme_intern_exact_symbol(const char *name, int len)
{
  Scheme_Object *sym;

#ifdef MZ_REAL_THREADS
  SCHEME_LOCK_MUTEX(scheme_symbol_table->mutex);
#endif

  sym = symbol_bucket(scheme_symbol_table, name, len, NULL);

  if (!sym) {
    sym = make_a_symbol(name, len);
    symbol_bucket(scheme_symbol_table, name, len, sym);
  }

#ifdef MZ_REAL_THREADS
  SCHEME_UNLOCK_MUTEX(scheme_symbol_table->mutex);
#endif
   
  return sym;
}

#define MAX_SYMBOL_SIZE 256

Scheme_Object *
scheme_intern_symbol(const char *name)
{
  if (!scheme_case_sensitive) {
    long i, len;
    char *naya;
    char on_stack[MAX_SYMBOL_SIZE];
    
    len = strlen(name);
    if (len >= MAX_SYMBOL_SIZE)
      naya = (char *)scheme_malloc_atomic(len + 1);
    else
      naya = on_stack;

    for (i = 0; i < len; i++) {
      int c = name[i];

      if (isupper(c))
	c = tolower(c);

      naya[i] = c;
    }

    naya[len] = 0;

    return scheme_intern_exact_symbol(naya, len);
  }

  return scheme_intern_exact_symbol(name, strlen(name));
}

const char *scheme_symbol_name_and_size(Scheme_Object *sym, int *length, int flags)
{
  int has_space = 0, has_special = 0, has_pipe = 0, digit_start;
  int i, len = SCHEME_SYM_LEN(sym), dz;
  int total_length;
  int pipe_quote;
  char *s = SCHEME_SYM_VAL(sym), *result;
  
  if ((flags & SNF_PIPE_QUOTE) || (flags & SNF_FOR_TS))
    pipe_quote = 1;
  else if (flags & SNF_NO_PIPE_QUOTE)
    pipe_quote = 0;
  else {
    pipe_quote = SCHEME_TRUEP(scheme_get_param(scheme_config, MZCONFIG_CAN_READ_PIPE_QUOTE));
  }

#define isSpecial(ch) ((ch == '(') || (ch == '[') || (ch == '{')       \
		       || (ch == ')') || (ch == ']') || (ch == '}')    \
		       || (ch == ')') || (ch == '\\')   \
		       || (ch == '"') || (ch == '\'')   \
		       || (ch == '`') || (ch == ',')    \
                       || (ch == ';')                   \
                       || (((ch == '>') || (ch == '<')) \
			   && (flags & SNF_FOR_TS)))

  if (len) {
    digit_start = (isdigit(s[0]) || (s[0] == '.')
		   || (s[0] == '+') || (s[0] == '-'));
    if (s[0] == '#' && (len == 1 || s[1] != '%'))
      has_special = 1;
    if (s[0] == '.' && len == 1)
      has_special = 1;
  } else {
    digit_start = 0;
    has_space = 1;
  }

  for (i = 0; i < len; i++) {
    if (isspace(s[i]) || !isprint(s[i])) {
      if ((flags & SNF_FOR_TS) && (s[i] == ' ')) {
	/* space is OK in type symbols */
      } else
	has_space = 1;
    } else if (isSpecial(s[i]))
      has_special = 1;
    else if (s[i] == '|')
      has_pipe = 1;
  }

  result = NULL;
  total_length = 0;

  if (!has_space && !has_special && (!pipe_quote || !has_pipe)) {
    dz = 0;
    if (digit_start
	&& !(flags & SNF_FOR_TS)
	&& (SCHEME_TRUEP(scheme_read_number(s, len, 0, 0, 10, 0, 0, &dz))
	    || dz)) {
      /* Need quoting: */
      if (pipe_quote)
	has_space = 1; /* Use normal */
      else {
	/* Just need a leading backslash: */
	result = (char *)scheme_malloc_atomic(len + 2);
	total_length = len + 1;
	memcpy(result + 1, s, len);
	result[0] = '\\';
	result[len + 1] = 0;
      }
    } else {
      total_length = len;
      result = s;
    }
  }

  if (!result) {
    if (!has_pipe && pipe_quote) {
      result = (char *)scheme_malloc_atomic(len + 3);
      total_length = len + 2;
      memcpy(result + 1, s, len);
      result[0] = '|';
      result[len + 1] = '|';
      result[len + 2] = 0;
    } else {
      int i, p = 0;
      
      result = (char *)scheme_malloc_atomic((2 * len) + 1);

      for (i = 0; i < len; i++) {
	if (isspace(s[i]) 
	    || isSpecial(s[i]) 
	    || ((s[i] == '|') && pipe_quote)
	    || (!i && s[0] == '#'))
	  result[p++] = '\\';
	result[p++] = s[i];
      }
      
      result[p] = 0;
      total_length = p;
    }
  }

  if (total_length > scheme_max_found_symbol_name)
    scheme_max_found_symbol_name = total_length;

  if (length)
    *length = total_length;

  return result;
}

const char *scheme_symbol_name(Scheme_Object *sym)
{
  return scheme_symbol_name_and_size(sym, NULL, 0);
}

/* locals */

static Scheme_Object *
symbol_p_prim (int argc, Scheme_Object *argv[])
{
  return SCHEME_SYMBOLP(argv[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *
string_to_symbol_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string->symbol", "string", 0, argc, argv);
  return scheme_intern_exact_symbol(SCHEME_STR_VAL(argv[0]),
				    SCHEME_STRTAG_VAL(argv[0]));
}

static Scheme_Object *
string_to_uninterned_symbol_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string->uninterned-symbol", "string", 0, argc, argv);
  return scheme_make_exact_symbol(SCHEME_STR_VAL(argv[0]),
				  SCHEME_STRTAG_VAL(argv[0]));
}

static Scheme_Object *
symbol_to_string_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("symbol->string", "symbol", 0, argc, argv);
  
  return scheme_make_sized_string(SCHEME_SYM_VAL(argv[0]),
				  SCHEME_SYM_LEN(argv[0]),
				  1);
}

static Scheme_Object *gensym(int argc, Scheme_Object *argv[])
{
  char buffer[100], *str;

  if (argc && !SCHEME_SYMBOLP(argv[0]) && !SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("gensym", "symbol/string", 0, argc, argv);
  
  if (argc) {
    if (SCHEME_STRINGP(argv[0]))
      str = SCHEME_STR_VAL(argv[0]);
    else
      str = SCHEME_SYM_VAL(argv[0]);      
    sprintf(buffer, "%.80s%d", str, gensym_counter++);
  } else
    sprintf(buffer, "g%d", gensym_counter++);

  return scheme_make_symbol(buffer);
}
