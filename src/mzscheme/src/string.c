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

#include "schvers.h"

#ifndef SCHEME_PLATFORM_LIBRARY_SUBPATH
# include "schsys.h"
#endif

/* globals */

/* locals */
static Scheme_Object *make_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_set (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *substring (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_append (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_list (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_to_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_copy (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_fill (int argc, Scheme_Object *argv[]);
static Scheme_Object *version(int argc, Scheme_Object *argv[]);
static Scheme_Object *format(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_printf(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_fprintf(int argc, Scheme_Object *argv[]);
static Scheme_Object *banner(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_getenv(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_putenv(int argc, Scheme_Object *argv[]);
static Scheme_Object *system_type(int argc, Scheme_Object *argv[]);
static Scheme_Object *system_library_subpath(int argc, Scheme_Object *argv[]);

static int mz_strcmp(unsigned char *str1, int l1, unsigned char *str2, int l2);
static int mz_strcmp_ci(unsigned char *str1, int l1, unsigned char *str2, int l2);

static Scheme_Object *sys_symbol;
static Scheme_Object *platform_path;
static Scheme_Object *zero_length_string;

static Scheme_Hash_Table *putenv_str_table;

static char *embedding_banner;

void
scheme_init_string (Scheme_Env *env)
{
  if (scheme_starting_up) {
    REGISTER_SO(sys_symbol);
    sys_symbol = scheme_intern_symbol(SYSTEM_TYPE_NAME);

    REGISTER_SO(zero_length_string);
    zero_length_string = scheme_alloc_string(0, 0); 

    REGISTER_SO(platform_path);
    platform_path = scheme_make_string(SCHEME_PLATFORM_LIBRARY_SUBPATH);

    REGISTER_SO(putenv_str_table);

    REGISTER_SO(embedding_banner);
  }

  scheme_add_global_constant("string?", 
			     scheme_make_folding_prim(string_p,
						      "string?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("make-string", 
			     scheme_make_prim_w_arity(make_string,
						      "make-string",
						      1, 2),
			     env);
  scheme_add_global_constant("string", 
			     scheme_make_prim_w_arity(string,
						      "string", 
						      0, -1),
			     env);
  scheme_add_global_constant("string-length", 
			     scheme_make_folding_prim(string_length,
						      "string-length",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("string-ref", 
			     scheme_make_prim_w_arity(string_ref,
						      "string-ref", 
						      2, 2),
			     env);
  scheme_add_global_constant("string-set!", 
			     scheme_make_prim_w_arity(string_set,
						      "string-set!", 
						      3, 3),
			     env);
  scheme_add_global_constant("string=?", 
			     scheme_make_prim_w_arity(string_eq,
						      "string=?",
						      1, -1),
			     env);
  scheme_add_global_constant("string-ci=?", 
			     scheme_make_prim_w_arity(string_ci_eq,
						      "string-ci=?",
						      1, -1),
			     env);
  scheme_add_global_constant("string<?", 
			     scheme_make_prim_w_arity(string_lt,
						      "string<?",
						      1, -1),
			     env);
  scheme_add_global_constant("string>?", 
			     scheme_make_prim_w_arity(string_gt,
						      "string>?",
						      1, -1),
			     env);
  scheme_add_global_constant("string<=?", 
			     scheme_make_prim_w_arity(string_lt_eq,
						      "string<=?",
						      1, -1),
			     env);
  scheme_add_global_constant("string>=?", 
			     scheme_make_prim_w_arity(string_gt_eq,
						      "string>=?",
						      1, -1),
			     env);
  scheme_add_global_constant("string-ci<?", 
			     scheme_make_prim_w_arity(string_ci_lt,
						      "string-ci<?",
						      1, -1),
			     env);
  scheme_add_global_constant("string-ci>?", 
			     scheme_make_prim_w_arity(string_ci_gt,
						      "string-ci>?",
						      1, -1),
			     env);
  scheme_add_global_constant("string-ci<=?", 
			     scheme_make_prim_w_arity(string_ci_lt_eq,
						      "string-ci<=?",
						      1, -1),
			     env);
  scheme_add_global_constant("string-ci>=?", 
			     scheme_make_prim_w_arity(string_ci_gt_eq,
						      "string-ci>=?",
						      1, -1),
			     env);
  scheme_add_global_constant("substring", 
			     scheme_make_prim_w_arity(substring,
						      "substring", 
						      3, 3),
			     env);
  scheme_add_global_constant("string-append", 
			     scheme_make_prim_w_arity(string_append,
						      "string-append", 
						      0, -1),
			     env);
  scheme_add_global_constant("string->list", 
			     scheme_make_prim_w_arity(string_to_list,
						      "string->list",
						      1, 1),
			     env);
  scheme_add_global_constant("list->string", 
			     scheme_make_prim_w_arity(list_to_string,
						      "list->string",
						      1, 1),
			     env);
  scheme_add_global_constant("string-copy", 
			     scheme_make_prim_w_arity(string_copy,
						      "string-copy",
						      1, 1),
			     env);
  scheme_add_global_constant("string-fill!", 
			     scheme_make_prim_w_arity(string_fill,
						      "string-fill!", 
						      2, 2),
			     env);
  

  scheme_add_global_constant("format", 
			     scheme_make_folding_prim(format,
						      "format", 
						      1, -1, 1),
			     env);
  scheme_add_global_constant("printf", 
			     scheme_make_prim_w_arity(sch_printf,
						      "printf", 
						      1, -1),
			     env);
  scheme_add_global_constant("fprintf", 
			     scheme_make_prim_w_arity(sch_fprintf,
						      "fprintf", 
						      2, -1),
			     env);
  

  scheme_add_global_constant("version", 
			     scheme_make_folding_prim(version,
						      "version", 
						      0, 0, 1),
			     env);
  scheme_add_global_constant("banner", 
			     scheme_make_folding_prim(banner,
						      "banner", 
						      0, 0, 1),
			     env);
  
  scheme_add_global_constant("getenv", 
			     scheme_make_prim_w_arity(sch_getenv,
						      "getenv",
						      1, 1),
			     env);
  scheme_add_global_constant("putenv", 
			     scheme_make_prim_w_arity(sch_putenv,
						      "putenv", 
						      2, 2),
			     env);
  
  scheme_add_global_constant("system-type", 
			     scheme_make_folding_prim(system_type,
						      "system-type", 
						      0, 0, 1),
			     env);
  scheme_add_global_constant("system-library-subpath",
			     scheme_make_folding_prim(system_library_subpath,
						      "system-library-subpath",
						      0, 0, 1),
			     env);
}

void
scheme_init_getenv(void)
{
#ifndef GETENV_FUNCTION
  FILE *f = fopen("Environment", "r");
  if (f) {
    Scheme_Object *p = scheme_make_file_input_port(f);
    mz_jmp_buf savebuf;
    memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf));
    if (!scheme_setjmp(scheme_error_buf)) {
      while (1) {
	Scheme_Object *v = scheme_read(p);
	if (SCHEME_EOFP(v))
	  break;

	if (SCHEME_PAIRP(v) && SCHEME_PAIRP(SCHEME_CDR(v))
	    && SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(v)))) {
	  Scheme_Object *key = SCHEME_CAR(v);
	  Scheme_Object *val = SCHEME_CADR(v);
	  if (SCHEME_STRINGP(key) && SCHEME_STRINGP(val)) {
	    Scheme_Object *a[2];
	    a[0] = key;
	    a[1] = val;
	    sch_putenv(2, a);
	    v = NULL;
	  }
	}

	if (v)
	  scheme_signal_error("bad environment specification: %s",
			      scheme_make_provided_string(v, 1, NULL));
      }
    }
    memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf));
    scheme_close_input_port(p);
  }
#endif
}

Scheme_Object *
scheme_make_sized_string(char *chars, long len, int copy)
{
  Scheme_Object *str;
  int moved = 0;

#ifdef MZ_PRECISE_GC
  /* Special precise-GC behavior: allow unaligned char pointer as arg */
  if ((long)chars & 0x1) {
    chars++;
    moved = 1;
    copy = 1;
  }
#endif

  str = scheme_alloc_stubborn_object();
  str->type = scheme_string_type;

  if (len < 0)
    len = strlen(chars - moved);
  if (copy) {
    char *naya;

    naya = (char *)scheme_malloc_fail_ok(scheme_malloc_atomic, len + 1);
    SCHEME_STR_VAL(str) = naya;
    memcpy(naya, chars - moved, len);
    naya[len] = 0;
  } else
    SCHEME_STR_VAL(str) = chars;
  SCHEME_STRTAG_VAL(str) = len;
  scheme_end_stubborn_change((void *)str);
  return (str);
}

Scheme_Object *
scheme_make_string_without_copying(char *chars)
{
  return scheme_make_sized_string(chars, -1, 0);
}

Scheme_Object *
scheme_make_string(const char *chars)
{
  return scheme_make_string_without_copying(scheme_strdup(chars));
}

Scheme_Object *
scheme_alloc_string(int size, char fill)
{
  Scheme_Object *str;
  char *s;
  int i;
  
  if (size < 0) {
    str = scheme_make_integer(size);
    scheme_wrong_type("make-string", "non-negative exact integer",
		      -1, 0, &str);
  }

  str = scheme_alloc_stubborn_object();
  str->type = scheme_string_type;
  s = (char *)scheme_malloc_fail_ok(scheme_malloc_atomic, sizeof(char)*(size + 1));
  for (i = size; i--; ) {
    s[i] = fill;
  }
  s[size] = '\0';
  SCHEME_STR_VAL(str) = s;
  SCHEME_STRTAG_VAL(str) = size;

  scheme_end_stubborn_change((void *)str);

  return str;
}

void scheme_out_of_string_range(const char *name, const char *which, 
				Scheme_Object *i, Scheme_Object *s, 
				long start, long len)
{
  if (SCHEME_STRTAG_VAL(s)) {
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		     scheme_make_integer(i),
		     "%s: %sindex %s out of range [%d, %d] for string: %s",
		     name, which,
		     scheme_make_provided_string(i, 2, NULL), 
		     start, len,
		     scheme_make_provided_string(s, 2, NULL));
  } else {
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		     scheme_make_integer(i),
		     "%s: %sindex %s out of range for empty string",
		     name, which,
		     scheme_make_provided_string(i, 0, NULL));
  }
}

long scheme_extract_index(const char *name, int pos, int argc, Scheme_Object **argv, long top)
{
  long i;
  int is_top = 0;

  if (SCHEME_INTP(argv[pos])) {
    i = SCHEME_INT_VAL(argv[pos]);
  } else if (SCHEME_BIGNUMP(argv[pos])) {
    if (SCHEME_BIGPOS(argv[pos])) {
      i = top; /* out-of-bounds */
      is_top = 1;
    } else
      i = -1; /* negative */
  } else
    i = -1;

  if (!is_top && (i < 0))
    scheme_wrong_type(name, "non-negative exact integer", pos, argc, argv);
  
  return i;
}

/* locals */

static Scheme_Object *
string_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_STRINGP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
make_string (int argc, Scheme_Object *argv[])
{
  long len;
  char fill;
  Scheme_Object *str;

  len = scheme_extract_index("make-string", 0, argc, argv, -1);

  if (len == -1) {
    scheme_raise_out_of_memory("make-string", "making string of length %s",
			       scheme_make_provided_string(argv[0], 0, NULL));
  }

  if (argc == 2) {
    if (!SCHEME_CHARP(argv[1]))
      scheme_wrong_type("make-string", "character", 1, argc, argv);
    fill = SCHEME_CHAR_VAL(argv[1]);
  } else
    fill = 0;

  str = scheme_alloc_string(len, fill);
  return (str);
}

static Scheme_Object *
string (int argc, Scheme_Object *argv[])
{
  Scheme_Object *str;
  int i;

  str = scheme_alloc_string(argc, 0);

  for ( i=0 ; i<argc ; ++i ) {
    if (!SCHEME_CHARP (argv[i]))
      scheme_wrong_type("string", "character", i, argc, argv);
    SCHEME_STR_VAL(str)[i] = SCHEME_CHAR_VAL(argv[i]);
  }

  return (str);
}

static Scheme_Object *
string_length (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string-length", "string", 0, argc, argv);

  return scheme_make_integer(SCHEME_STRTAG_VAL(argv[0]));
}

static Scheme_Object *
string_ref (int argc, Scheme_Object *argv[])
{
  long i, len;
  char *str;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string-ref", "string", 0, argc, argv);

  str = SCHEME_STR_VAL(argv[0]);
  len = SCHEME_STRTAG_VAL(argv[0]);

  i = scheme_extract_index("string-ref", 1, argc, argv, len);

  if (i >= len) {
    scheme_out_of_string_range("string-ref", "", argv[1], argv[0], 0, len - 1);
    return NULL;
  }

  return scheme_make_char(str[i]);
}

static Scheme_Object *
string_set (int argc, Scheme_Object *argv[])
{
  long i, len;
  char *str;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string-set!", "string", 0, argc, argv);

  str = SCHEME_STR_VAL(argv[0]);
  len = SCHEME_STRTAG_VAL(argv[0]);

  i = scheme_extract_index("string-set!", 1, argc, argv, len);

  if (!SCHEME_CHARP(argv[2]))
    scheme_wrong_type("string-set!", "character", 2, argc, argv);

  if (i >= len) {
    scheme_out_of_string_range("string-set!", "", argv[1], argv[0], 0, len - 1);
    return NULL;
  }

  str[i] = SCHEME_CHAR_VAL(argv[2]);

  return scheme_void;
}

/* comparisons */

#define GEN_STRING_COMP(name, scheme_name, comp, op) \
static Scheme_Object * name (int argc, Scheme_Object *argv[]) \
{  char *s, *prev; int i, sl, pl; int falz = 0;\
   if (!SCHEME_STRINGP(argv[0])) \
    scheme_wrong_type(scheme_name, "string", 0, argc, argv); \
   prev = SCHEME_STR_VAL(argv[0]); pl = SCHEME_STRTAG_VAL(argv[0]); \
   for (i = 1; i < argc; i++) { \
     if (!SCHEME_STRINGP(argv[i])) \
      scheme_wrong_type(scheme_name, "string", i, argc, argv); \
     s = SCHEME_STR_VAL(argv[i]); sl = SCHEME_STRTAG_VAL(argv[i]); \
     if (!falz) if (!(comp((unsigned char *)prev, pl, \
                           (unsigned char *)s, sl) op 0)) falz = 1; \
     prev = s; pl = sl; \
  } \
  return falz ? scheme_false : scheme_true; \
}

GEN_STRING_COMP(string_eq, "string=?", mz_strcmp, ==)
GEN_STRING_COMP(string_lt, "string<?", mz_strcmp, <)
GEN_STRING_COMP(string_gt, "string>?", mz_strcmp, >)
GEN_STRING_COMP(string_lt_eq, "string<=?", mz_strcmp, <=)
GEN_STRING_COMP(string_gt_eq, "string>=?", mz_strcmp, >=)

GEN_STRING_COMP(string_ci_eq, "string-ci=?", mz_strcmp_ci, ==)
GEN_STRING_COMP(string_ci_lt, "string-ci<?", mz_strcmp_ci, <)
GEN_STRING_COMP(string_ci_gt, "string-ci>?", mz_strcmp_ci, >)
GEN_STRING_COMP(string_ci_lt_eq, "string-ci<=?", mz_strcmp_ci, <=)
GEN_STRING_COMP(string_ci_gt_eq, "string-ci>=?", mz_strcmp_ci, >=)

void scheme_get_substring_indices(const char *name, Scheme_Object *str, 
				  int argc, Scheme_Object **argv, 
				  int spos, int fpos, long *_start, long *_finish)
{
  long len = SCHEME_STRTAG_VAL(str);
  long start, finish;

  if (argc > spos)
    start = scheme_extract_index(name, spos, argc, argv, len + 1);
  else
    start = 0;
  if (argc > fpos)
    finish = scheme_extract_index(name, fpos, argc, argv, len + 1);
  else
    finish = len;

  if (!(start <= len)) {
    scheme_out_of_string_range(name, "first ", argv[spos], str, 0, len);
  }
  if (!(finish >= start && finish <= len)) {
    scheme_out_of_string_range(name, "second ", argv[fpos], str, start, len);
  }

  *_start = start;
  *_finish = finish;
}

static Scheme_Object *
substring (int argc, Scheme_Object *argv[])
{
  long start, finish, _start, _finish, i;
  char *chars, *dchars;
  Scheme_Object *str;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("substring", "string", 0, argc, argv);

  chars = SCHEME_STR_VAL(argv[0]);

  scheme_get_substring_indices("substring", argv[0], argc, argv, 1, 2,
			       &_start, &_finish);
  start = _start;
  finish = _finish;

  str = scheme_alloc_string(finish-start, 0);
  dchars = SCHEME_STR_VAL(str);
  for (i = 0; i < finish-start; i++) {
    dchars[i] = chars[i+start];
  }

  return (str);
}

static Scheme_Object *
string_append (int argc, Scheme_Object *argv[])
{
  Scheme_Object *naya, *s;
  char *chars;
  int i;
  long len;

  if (argc == 2) {
    Scheme_Object *s1 = argv[0], *s2 = argv[1];
    if (!SCHEME_STRINGP(s1))
      scheme_wrong_type("string-append", "string", 0, argc, argv);
    if (!SCHEME_STRINGP(s2))
      scheme_wrong_type("string-append", "string", 1, argc, argv);
    return scheme_append_string(s1, s2);
  }

  if (!argc)
    return zero_length_string;
  else if (argc == 1)
    return scheme_append_string(zero_length_string, argv[0]);
  
  len = 0;
  for (i = 0; i < argc; i++) {
    s = argv[i];
    if (!SCHEME_STRINGP(s))
      scheme_wrong_type("string-append", "string", i, argc, argv);
    len += SCHEME_STRLEN_VAL(s);
  }

  naya = scheme_alloc_string(len, 0);
  chars = SCHEME_STR_VAL(naya);

  for (i = 0; i < argc; i++) {
    s = argv[i];
    len = SCHEME_STRLEN_VAL(s);
    memcpy(chars, SCHEME_STR_VAL(s), len);
    chars += len;
  }
  
  return naya;
}

Scheme_Object *
scheme_append_string(Scheme_Object *str1, Scheme_Object *str2)
{
  int len1, len2, i;
  char *chars1, *chars2, *r;
  Scheme_Object *naya;

  if (!SCHEME_STRINGP(str1))
    scheme_wrong_type("string-append", "string", -1, 0, &str1);
  if (!SCHEME_STRINGP(str2))
    scheme_wrong_type("string-append", "string", -1, 0, &str2);

  chars1 = SCHEME_STR_VAL(str1);
  chars2 = SCHEME_STR_VAL(str2);
  len1 = SCHEME_STRTAG_VAL(str1);
  len2 = SCHEME_STRTAG_VAL(str2);
  naya = scheme_alloc_string(len1 + len2, 0);

  r = SCHEME_STR_VAL(naya);
  for (i = 0; i < len1; i++, r++) {
    *r = chars1[i];
  }

  for (i = 0; i < len2; i++, r++) {
    *r = chars2[i];
  }

  *r = '\0';

  return naya;
}


static Scheme_Object *
string_to_list (int argc, Scheme_Object *argv[])
{
  int len, i;
  char *chars;
  Scheme_Object *first, *last, *pair;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string->list", "string", 0, argc, argv);

  chars = SCHEME_STR_VAL(argv[0]);
  len = SCHEME_STRTAG_VAL(argv[0]);
  first = last = scheme_null;
  for ( i=0 ; i<len ; ++i ) {
    pair = scheme_make_pair (scheme_make_char (chars[i]), scheme_null);
    if (SCHEME_NULLP(first))
      first = pair;
    else
      SCHEME_CDR (last) = pair;
    last = pair;
  }

  return first;
}

static Scheme_Object *
list_to_string (int argc, Scheme_Object *argv[])
{
  int len, i;
  Scheme_Object *list, *str, *ch;

  list = argv[0];
  len = scheme_list_length (list);
  str = scheme_alloc_string (len, 0);
  i = 0;
  while (SCHEME_PAIRP (list))
    {
      ch = SCHEME_CAR (list);

      if (!SCHEME_CHARP(ch))
	scheme_wrong_type("list->string", "proper character list", 0, 
			  argc, argv);

      SCHEME_STR_VAL(str)[i] = SCHEME_CHAR_VAL(ch);
      i++;
      list = SCHEME_CDR (list);
    }  

  if (!SCHEME_NULLP(list))
    scheme_wrong_type("list->string", "proper character list", 0, argc, argv);

  return (str);
}

static Scheme_Object *
string_copy (int argc, Scheme_Object *argv[])
{
  Scheme_Object *naya;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string-copy", "string", 0, argc, argv);

  naya = scheme_make_sized_string(SCHEME_STR_VAL(argv[0]), 
				  SCHEME_STRTAG_VAL(argv[0]), 1);
  return naya;
}

static Scheme_Object *
string_fill (int argc, Scheme_Object *argv[])
{
  int len, i;
  char *chars, ch;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string-fill!", "string", 0, argc, argv);
  if (!SCHEME_CHARP(argv[1]))
    scheme_wrong_type("string-fill!", "character", 1, argc, argv);
  
  chars = SCHEME_STR_VAL (argv[0]);
  ch = SCHEME_CHAR_VAL (argv[1]);
  len = SCHEME_STRTAG_VAL (argv[0]);
  for (i = 0; i < len; i++) {
    chars[i] = ch;
  }

  return scheme_void;
}

static int mz_strcmp(unsigned char *str1, int l1, unsigned char *str2, int l2)
{
#ifdef USE_LOCALE
  /* USE_LOCALE contributed by Boris Tobotras, tobotras@jet.msk.su. */
  /* Caveat emptor. */
  /* The problem with using strcoll is that MzScheme strings may
     contain a null character. In that case, if the strings match up
     to the first null character but differ afterwards, strcoll will
     return the wrong result. (This is why we use mz_strcmp instead of
     strcmp in the first place.) */
  return strcoll(str1, str2);
#else  
  int endres;

  if (l1 > l2) {
    l1 = l2;
    endres = 1;
  } else {
    if (l2 > l1)
      endres = -1;
    else
      endres = 0;
  }

  while (l1--) {
    unsigned int a, b;
    
    a = *(str1++);
    b = *(str2++);

    a = a - b;
    if (a)
      return a;
  }

  return endres;
#endif
}

static int mz_strcmp_ci(unsigned char *str1, int l1, unsigned char *str2, int l2)
{
  int endres;

  if (l1 > l2) {
    l1 = l2;
    endres = 1;
  } else {
    if (l2 > l1)
      endres = -1;
    else
      endres = 0;
  }

#ifdef USE_LOCALE

  {
# define USE_ALLOCA 1
# if USE_ALLOCA
#  define LALLOC alloca
# else
#  define LALLOC scheme_malloc_atomic
# endif
    unsigned char *cstr1 = LALLOC(l1 + 1);
    unsigned char *cstr2 = LALLOC(l1 + 1);
    int retCode, i;

    strncpy(cstr1, str1, l1);
    strncpy(cstr2, str2, l1);
    cstr1[l1] = cstr2[l1] = '\0';

    for (i = 0; i < l1; ++i) {
      cstr1[i] = toupper(cstr1[i]);
      cstr2[i] = toupper(cstr2[i]);
    }
    retCode = strcoll(cstr1, cstr2);
  
    if (retCode)
      return retCode;
  }

#else /* not USE_LOCALE */

  while (l1--) {
    unsigned int a, b;
    
    a = *(str1++);
    b = *(str2++);
    a = toupper(a);
    b = toupper(b);

    a = a - b;
    if (a)
      return a;
  }

#endif /* USE_LOCALE */

  return endres;
}

void scheme_do_format(const char *procname, Scheme_Object *port, 
		      const unsigned char *format, int flen, 
		      int fpos, int offset, int argc, Scheme_Object **argv)
{
  int i, start, end;
  int used = offset;
  int num_err = 0, char_err = 0, end_ok = 0;
  Scheme_Object *a[2];

  if (!format) {
    if (!SCHEME_STRINGP(argv[fpos])) {
      scheme_wrong_type(procname, "format-string", fpos, argc, argv);
      return;
    }
    format = (unsigned char *)SCHEME_STR_VAL(argv[fpos]);
    flen = SCHEME_STRTAG_VAL(argv[fpos]);
  } else if (flen == -1)
    flen = strlen((char *)format);

  /* Check string first: */
  end = flen - 1;
  for (i = 0; i < end; i++) {
    if (format[i] == '~') {
      i++;
      if (isspace(format[i])) {
	/* skip spaces... */
      } else switch (format[i]) {
      case '~':
	if (i == end)
	  end_ok = 1;
	break;
      case '%':
      case 'n':
      case 'N':
	break;
      case 'a':
      case 'A':
      case 's':
      case 'S':
      case 'v':
      case 'V':
      case 'e':
      case 'E':
	used++;
	break;
      case 'x':
      case 'X':
      case 'o':
      case 'O':
      case 'b':
      case 'B':
	if (!num_err && !char_err && (used < argc)) {
	  Scheme_Object *o = argv[used];
	  if (!SCHEME_EXACT_REALP(o)
	      && (!SCHEME_COMPLEXP(o)
		  || !SCHEME_EXACT_REALP(scheme_complex_real_part(o))))
	    num_err = used + 1;
	}
	used++;
	break;
      case 'c':
      case 'C':
	if (!num_err && !char_err && (used < argc)) {
	  if (!SCHEME_CHARP(argv[used]))
	    char_err = used + 1;
	}
	used++;
	break;
      default:
	{
	  char buffer[64];
	  sprintf(buffer, "pattern-string (tag ~%c not allowed)", format[i]);
	  scheme_wrong_type(procname, buffer, fpos, argc, argv);
	  return;
	}
      }
    }
  }
  if ((format[end] == '~') && !end_ok) {
    scheme_wrong_type(procname, "pattern-string (cannot end in ~)", fpos, argc, argv);
    return;
  }
  if (used != argc) {
    char *args;

    args = scheme_make_args_string("", -1, argc, argv);

    if (used > argc) {
      scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		       argv[fpos],
		       "%s: format string requires %d arguments, given %d%s",
		       procname, used - offset, argc - offset, args);
    } else {
      scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		       argv[fpos],
		       "%s: format string requires %d arguments, given %d%s",
		       procname, used - offset, argc - offset, args);
    }
    return;
  }
  if (num_err || char_err) {
    int pos = (num_err ? num_err : char_err) - 1;
    char *args;
    char *type = (num_err ? "exact-number" : "character");
    Scheme_Object *bad = argv[pos];

    args = scheme_make_args_string("other ", pos, argc, argv);

    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		     bad,
		     "%s: format string requires argument of type <%s>, given %s%s",
		     procname, type, 
		     scheme_make_provided_string(bad, 1, NULL),
		     args);
    return;
  }

  for (used = offset, i = start = 0; i < flen; i++) {
    if (format[i] == '~') {
      if (start < i) {
	char *s;
#ifdef MZ_PRECISE_GC
	/* Can't pass unaligned pointer to scheme_write_string: */
	if (start & 0x1) {
	  s = (char *)scheme_malloc_atomic(i - start);
	  memcpy(s, format + start, i - start);
	} else
#endif
	  s = (char *)format + start;
	scheme_write_string((char *)s, i - start, port);
      }
      i++;
      if (isspace(format[i])) {
	/* skip spaces (at most one newline) */
	do {
	  if ((format[i] == '\n') || (format[i] == '\r')) {
	    /* got one */
	    if ((format[i] == '\r') && (format[i + 1] == '\n'))
	      i++; /* Windows-style CR-NL */
	    i++;
	    while (isspace(format[i]) 
		   && !((format[i] == '\n') || (format[i] == '\r'))) {
	      i++;
	    }
	    break;
	  } else
	    i++;
	} while (isspace(format[i]));
	--i; /* back up over something */
      } else switch (format[i]) {
      case '~':
	scheme_write_string("~", 1, port);
	break;
      case '%':
      case 'n':
      case 'N':
	scheme_write_string("\n", 1, port);
	break;
      case 'c':
      case 'C':
      case 'a':
      case 'A':
	a[0] = argv[used++];
	a[1] = port;
	_scheme_apply(scheme_display_proc, 2, a);
	break;
      case 's':
      case 'S':
	a[0] = argv[used++];
	a[1] = port;
	_scheme_apply(scheme_write_proc, 2, a);
	break;
      case 'v':
      case 'V':
	a[0] = argv[used++];
	a[1] = port;
	_scheme_apply(scheme_print_proc, 2, a);
	break;
      case 'e':
      case 'E':
	{
	  int len;
	  char *s;
	  s = scheme_make_provided_string(argv[used++], 0, &len);
	  scheme_write_string(s, len, port);
	}
	break;
      case 'x':
      case 'X':
      case 'o':
      case 'O':
      case 'b':
      case 'B':
	{
	  char *s;
	  int radix;

	  switch(format[i]) {
	  case 'x':
	  case 'X':
	    radix = 16;
	    break;
	  case 'o':
	  case 'O':
	    radix = 8;
	    break;
	  default:
	  case 'b':
	  case 'B':
	    radix = 2;
	    break;
	  }
	  s = scheme_number_to_string(radix, argv[used++]);
	  
	  scheme_write_string(s, strlen(s), port);
	}
	break;
      }
      SCHEME_USE_FUEL(1);
      start = i + 1;
    }
  }

  SCHEME_USE_FUEL(flen);

  if (start < i) {
    char *s;
#ifdef MZ_PRECISE_GC
    /* Can't pass unaligned pointer to scheme_write_string: */
    if (start & 0x1) {
      s = (char *)scheme_malloc_atomic(i - start);
      memcpy(s, format + start, i - start);
    } else
#endif
      s = (char *)format + start;
    scheme_write_string((char *)s, i - start, port);
  }
}

char *scheme_format(char *format, int flen, int argc, Scheme_Object **argv, int *rlen)
{
  Scheme_Object *port;
  port = scheme_make_string_output_port();
  scheme_do_format("format", port, (unsigned char *)format, flen, 0, 0, argc, argv);
  return scheme_get_sized_string_output(port, rlen);
}

void scheme_printf(char *format, int flen, int argc, Scheme_Object **argv)
{
  scheme_do_format("printf", scheme_get_param(scheme_config, MZCONFIG_OUTPUT_PORT), 
	    (unsigned char *)format, flen, 0, 0, argc, argv);
}

static Scheme_Object *
format(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  char *s;
  int len;

  port = scheme_make_string_output_port();

  scheme_do_format("format", port, NULL, 0, 0, 1, argc, argv);

  s = scheme_get_sized_string_output(port, &len);
  return scheme_make_sized_string(s, len, 0);
}

static Scheme_Object *
sch_printf(int argc, Scheme_Object *argv[])
{
  scheme_do_format("printf", scheme_get_param(scheme_config, MZCONFIG_OUTPUT_PORT), 
		   NULL, 0, 0, 1, argc, argv);
  return scheme_void;
}

static Scheme_Object *
sch_fprintf(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("fprintf", "output-port", 0, argc, argv);

  scheme_do_format("fprintf", argv[0], NULL, 0, 1, 2, argc, argv);
  return scheme_void;
}

static Scheme_Object *
version(int argc, Scheme_Object *argv[])
{
  return scheme_make_string(scheme_version());
}

static Scheme_Object *
banner(int argc, Scheme_Object *argv[])
{
  return scheme_make_string(scheme_banner());
}

char *scheme_version(void)
{
  return VERSION;
}

#ifdef USE_SENORA_GC
# define VERSION_SUFFIX " (sgc)"
#else
# ifdef USE_LOCALE
#  define VERSION_SUFFIX " (using locale)"
# else
#  define VERSION_SUFFIX /* empty */
# endif
#endif

char *scheme_banner(void)
{
  if (embedding_banner)
    return embedding_banner;
  else
    return "Welcome to MzScheme" 
#ifdef MZ_PRECISE_GC
      "2k"
#endif      
      " version " VERSION VERSION_SUFFIX
      ", Copyright (c) 1995-99 PLT (Matthew Flatt)\n";
}

void scheme_set_banner(char *s)
{
  embedding_banner = s;
}

int scheme_string_has_null(Scheme_Object *o)
{
  const char *s = SCHEME_STR_VAL(o);
  int i = SCHEME_STRTAG_VAL(o);
  while (i--) {
    if (!s[i])
      return 1;
  }
  return 0;
}

static Scheme_Object *sch_getenv(int argc, Scheme_Object *argv[])
{
  char *s;

  if (!SCHEME_STRINGP(argv[0]) || scheme_string_has_null(argv[0]))
    scheme_wrong_type("getenv", STRING_W_NO_NULLS, 0, argc, argv);

#ifdef GETENV_FUNCTION
  s = getenv(SCHEME_STR_VAL(argv[0]));
#else
  if (putenv_str_table) {
    s = (char *)scheme_lookup_in_table(putenv_str_table, SCHEME_STR_VAL(argv[0]));
    /* If found, skip over the `=' in the table: */
    if (s)
      s += SCHEME_STRTAG_VAL(argv[0]) + 1;
  } else
    s = NULL;
#endif

  if (s)
    return scheme_make_string(s);

  return scheme_false;
}

static Scheme_Object *sch_putenv(int argc, Scheme_Object *argv[])
{
  char *s, *var, *val;
  long varlen, vallen;

  if (!SCHEME_STRINGP(argv[0]) || scheme_string_has_null(argv[0]))
    scheme_wrong_type("putenv", STRING_W_NO_NULLS, 0, argc, argv);
  if (!SCHEME_STRINGP(argv[1]) || scheme_string_has_null(argv[1]))
    scheme_wrong_type("putenv", STRING_W_NO_NULLS, 1, argc, argv);

  var = SCHEME_STR_VAL(argv[0]);
  val = SCHEME_STR_VAL(argv[1]);

  varlen = strlen(var);
  vallen = strlen(val);

  s = (char *)scheme_malloc_atomic(varlen + vallen + 2);
  memcpy(s, var, varlen);
  memcpy(s + varlen + 1, val, vallen + 1);
  s[varlen] = '=';

  SCHEME_GET_LOCK();

  if (!putenv_str_table)
    putenv_str_table = scheme_hash_table(7, SCHEME_hash_string, 0, 0);

  scheme_add_to_table(putenv_str_table, var, s, 0);

  SCHEME_RELEASE_LOCK();

#ifdef GETENV_FUNCTION
  return putenv(s) ? scheme_false : scheme_true;
#else
  return scheme_true;
#endif
}

static Scheme_Object *system_type(int argc, Scheme_Object *argv[])
{
  return sys_symbol;
}

static Scheme_Object *system_library_subpath(int argc, Scheme_Object *argv[])
{
  return platform_path;
}

const char *scheme_system_library_subpath()
{
  return SCHEME_PLATFORM_LIBRARY_SUBPATH;
}

/* Our own strncpy - which would be really stupid, except the one for
   the implementation in Solaris 2.6 is broken (it doesn't always stop
   at the null terminator). */
int scheme_strncmp(const char *a, const char *b, int len)
{
  while (len-- && (*a == *b) && *a) {
    a++;
    b++;
  }

  if (len < 0)
    return 0;
  else
    return *a - *b;
}
