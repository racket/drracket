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
#include <ctype.h>

/* globals */

/* All characters */
Scheme_Object **scheme_char_constants;

/* locals */
static Scheme_Object *char_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_eq_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lt_eq_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_gt_eq_ci (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_alphabetic (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_numeric (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_whitespace (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_upper_case (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_lower_case (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_to_integer (int argc, Scheme_Object *argv[]);
static Scheme_Object *integer_to_char (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_upcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_downcase (int argc, Scheme_Object *argv[]);

void scheme_init_char (Scheme_Env *env)
{
  if (scheme_starting_up) {
    int i;

    REGISTER_SO(scheme_char_constants);

    scheme_char_constants = 
      (Scheme_Object **)scheme_malloc_eternal(256 * sizeof(Scheme_Object*));
    
    for (i = 0; i < 256; i++) {
      Scheme_Object *sc = scheme_alloc_eternal_small_object ();
      sc->type = scheme_char_type;
      SCHEME_CHAR_VAL(sc) = i;
      
      scheme_char_constants[i] = sc;
    }
  }

  scheme_add_global_constant("char?", 
			     scheme_make_folding_prim(char_p, 
						      "char?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char=?", 
			     scheme_make_folding_prim(char_eq, 
						      "char=?", 
						      1, -1, 1), 
			     env);
  scheme_add_global_constant("char<?", 
			     scheme_make_folding_prim(char_lt, 
						      "char<?", 
						      1, -1, 1), 
			     env);
  scheme_add_global_constant("char>?", 
			     scheme_make_folding_prim(char_gt, 
						      "char>?", 
						      1, -1, 1), 
			     env);
  scheme_add_global_constant("char<=?", 
			     scheme_make_folding_prim(char_lt_eq, 
						      "char<=?", 
						      1, -1, 1), 
			     env);
  scheme_add_global_constant("char>=?", 
			     scheme_make_folding_prim(char_gt_eq, 
						      "char>=?", 
						      1, -1, 1), 
			     env);
  scheme_add_global_constant("char-ci=?", 
			     scheme_make_folding_prim(char_eq_ci, 
						      "char-ci=?", 
						      1, -1, 1), 
			     env);
  scheme_add_global_constant("char-ci<?", 
			     scheme_make_folding_prim(char_lt_ci, 
						      "char-ci<?", 
						      1, -1, 1), 
			     env);
  scheme_add_global_constant("char-ci>?", 
			     scheme_make_folding_prim(char_gt_ci, 
						      "char-ci>?", 
						      1, -1, 1), 
			     env);
  scheme_add_global_constant("char-ci<=?", 
			     scheme_make_folding_prim(char_lt_eq_ci, 
						      "char-ci<=?", 
						      1, -1, 1), 
			     env);
  scheme_add_global_constant("char-ci>=?", 
			     scheme_make_folding_prim(char_gt_eq_ci, 
						      "char-ci>=?", 
						      1, -1, 1), 
			     env);
  scheme_add_global_constant("char-alphabetic?", 
			     scheme_make_folding_prim(char_alphabetic, 
						      "char-alphabetic?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-numeric?", 
			     scheme_make_folding_prim(char_numeric, 
						      "char-numeric?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-whitespace?", 
			     scheme_make_folding_prim(char_whitespace, 
						      "char-whitespace?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-upper-case?", 
			     scheme_make_folding_prim(char_upper_case, 
						      "char-upper-case?", 
						      1, 1, 1),
			     env);
  scheme_add_global_constant("char-lower-case?", 
			     scheme_make_folding_prim(char_lower_case, 
						      "char-lower-case?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char->integer", 
			     scheme_make_folding_prim(char_to_integer, 
						      "char->integer", 
						      1, 1, 1),
			     env);
  scheme_add_global_constant("integer->char",
			     scheme_make_folding_prim(integer_to_char, 
						      "integer->char",
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-upcase", 
			     scheme_make_folding_prim(char_upcase, 
						      "char-upcase", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-downcase", 
			     scheme_make_folding_prim(char_downcase, 
						      "char-downcase", 
						      1, 1, 1),
			     env);
}

Scheme_Object *scheme_make_char(char ch)
{
  return _scheme_make_char(ch);
}

/* locals */

static Scheme_Object *
char_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_CHARP(argv[0]) ? scheme_true : scheme_false);
}

#define CHAR_UN_CHECK(name) \
 if (!SCHEME_CHARP(argv[0])) \
   char_un_error(name, argc, argv)

static void char_un_error(char *name, int argc, Scheme_Object *argv[])
{
  if (!SCHEME_CHARP(argv[0]))
    scheme_wrong_type(name, "character", 0, argc, argv);
}

#define GEN_CHAR_COMP(func_name, scheme_name, comp, uc)          \
 static Scheme_Object *func_name(int argc, Scheme_Object *argv[])     \
 { int c, prev, i; Scheme_Object *rv = scheme_true; \
   if (!SCHEME_CHARP(argv[0]))      \
     scheme_wrong_type(#scheme_name, "character", 0, argc, argv);     \
   prev = ((unsigned char)SCHEME_CHAR_VAL(argv[0]));     \
   if (uc) { prev = toupper(prev); }     \
   for (i = 1; i < argc; i++) {     \
     if (!SCHEME_CHARP(argv[i]))      \
       scheme_wrong_type(#scheme_name, "character", i, argc, argv);     \
     c = ((unsigned char)SCHEME_CHAR_VAL(argv[i]));     \
     if (uc) { c = toupper(c); }     \
     if (!(prev comp c)) rv = scheme_false;     \
     prev = c;     \
   }     \
   return rv;     \
 }

GEN_CHAR_COMP(char_eq, char=?, ==, 0)
GEN_CHAR_COMP(char_lt, char<?, <, 0)
GEN_CHAR_COMP(char_gt, char>?, >, 0)
GEN_CHAR_COMP(char_lt_eq, char<=?, <=, 0)
GEN_CHAR_COMP(char_gt_eq, char>=?, >=, 0)

GEN_CHAR_COMP(char_eq_ci, char-ci=?, ==, 1)
GEN_CHAR_COMP(char_lt_ci, char-ci<?, <, 1)
GEN_CHAR_COMP(char_gt_ci, char-ci>?, >, 1)
GEN_CHAR_COMP(char_lt_eq_ci, char-ci<=?, <=, 1)
GEN_CHAR_COMP(char_gt_eq_ci, char-ci>=?, >=, 1)

static Scheme_Object *
char_alphabetic (int argc, Scheme_Object *argv[])
{
  signed char c;

  CHAR_UN_CHECK("char-alphabetic?");

  c = SCHEME_CHAR_VAL(argv[0]);
  if (c <= 0)
    return scheme_false;

  return isalpha(c) ? scheme_true : scheme_false;
}

static Scheme_Object *
char_numeric (int argc, Scheme_Object *argv[])
{
  signed char c;

  CHAR_UN_CHECK("char-numeric?");

  c = SCHEME_CHAR_VAL(argv[0]);
  if (c <= 0)
    return scheme_false;

  return isdigit(c) ? scheme_true : scheme_false;
}

static Scheme_Object *
char_whitespace (int argc, Scheme_Object *argv[])
{
  signed char c;

  CHAR_UN_CHECK("char-whitespace?");

  c = SCHEME_CHAR_VAL(argv[0]);
  if (c <= 0)
    return scheme_false;

  return isspace(c) ? scheme_true : scheme_false;
}

static Scheme_Object *
char_upper_case (int argc, Scheme_Object *argv[])
{
  signed char c;

  CHAR_UN_CHECK("char-upper-case?");

  c = SCHEME_CHAR_VAL(argv[0]);
  if (c <= 0)
    return scheme_false;

  return isupper(c) ? scheme_true : scheme_false;
}

static Scheme_Object *
char_lower_case (int argc, Scheme_Object *argv[])
{
  signed char c;

  CHAR_UN_CHECK("char-lower-case?");

  c = SCHEME_CHAR_VAL(argv[0]);
  if (c <= 0)
    return scheme_false;

  return islower(c) ? scheme_true : scheme_false;
}

static Scheme_Object *
char_to_integer (int argc, Scheme_Object *argv[])
{
  unsigned char c;

  CHAR_UN_CHECK("char->integer");

  c = SCHEME_CHAR_VAL(argv[0]);

  return scheme_make_integer(c);
}

static Scheme_Object *
integer_to_char (int argc, Scheme_Object *argv[])
{
  long v;

  if (!SCHEME_INTP(argv[0]))
    scheme_wrong_type("integer->char", "exact in [0, 255]", 0, argc, argv);

  v = SCHEME_INT_VAL(argv[0]);
  if ((v < 0) || (v > 255))
    scheme_wrong_type("integer->char", "exact in [0, 255]", 0, argc, argv);

  return _scheme_make_char(v);
}

static Scheme_Object *
char_upcase (int argc, Scheme_Object *argv[])
{
  signed char c;

  CHAR_UN_CHECK("char-upcase");

  c = SCHEME_CHAR_VAL(argv[0]);
  if (c <= 0)
    return argv[0];

  return _scheme_make_char(toupper(c));
}

static Scheme_Object *
char_downcase (int argc, Scheme_Object *argv[])
{
  signed char c;

  CHAR_UN_CHECK("char-downcase");

  c = SCHEME_CHAR_VAL(argv[0]);
  if (c <= 0)
    return argv[0];

  return _scheme_make_char(tolower(c));
}

