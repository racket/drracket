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
#include "schmach.h"
#include <string.h>
#ifdef USE_STACKAVAIL
# include <malloc.c>
#endif
#ifdef USE_IEEE_FP_PREDS
# include <math.h>
#endif

/* global_constants */
Scheme_Object *scheme_true;
Scheme_Object *scheme_false;
Scheme_Object *scheme_not_prim;

/* locals */
static Scheme_Object *not_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *boolean_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *eq_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *eqv_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *equal_prim (int argc, Scheme_Object *argv[]);

static int vector_equal (Scheme_Object *vec1, Scheme_Object *vec2);

void scheme_init_true_false(void)
{
  REGISTER_SO(scheme_true);
  REGISTER_SO(scheme_false);
  
  scheme_true = scheme_alloc_eternal_object();
  scheme_true->type = scheme_true_type;

  scheme_false = scheme_alloc_eternal_object();
  scheme_false->type = scheme_false_type;
}

void scheme_init_bool (Scheme_Env *env)
{
  if (scheme_starting_up) {
    scheme_not_prim = scheme_make_folding_prim(not_prim, "not", 1, 1, 1);

    REGISTER_SO(scheme_not_prim);
  }

  scheme_add_global_constant("not", scheme_not_prim, env);
  scheme_add_global_constant("boolean?", 
			     scheme_make_folding_prim(boolean_p_prim, 
						      "boolean?",
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("eq?", 
			     scheme_make_folding_prim(eq_prim, 
						      "eq?",
						      2, 2, 1), 
			     env);
  scheme_add_global_constant("eqv?", 
			     scheme_make_folding_prim(eqv_prim, 
						      "eqv?",
						      2, 2, 1), 
			     env);
  scheme_add_global_constant("equal?", 
			     scheme_make_prim_w_arity(equal_prim, 
						      "equal?",
						      2, 2),
			     env);
}

static Scheme_Object *
not_prim (int argc, Scheme_Object *argv[])
{
  return (SAME_OBJ(argv[0], scheme_false) ? scheme_true : scheme_false);
}

static Scheme_Object *
boolean_p_prim (int argc, Scheme_Object *argv[])
{
  return (SCHEME_BOOLP(argv[0]) ? scheme_true : scheme_false);  
}

static Scheme_Object *
eq_prim (int argc, Scheme_Object *argv[])
{
  return (SAME_OBJ(argv[0], argv[1]) ? scheme_true : scheme_false);
}

static Scheme_Object *
eqv_prim (int argc, Scheme_Object *argv[])
{
  return (scheme_eqv(argv[0], argv[1]) ? scheme_true : scheme_false);
}

static Scheme_Object *
equal_prim (int argc, Scheme_Object *argv[])
{
  return (scheme_equal(argv[0], argv[1]) ? scheme_true : scheme_false);
}

int scheme_eq (Scheme_Object *obj1, Scheme_Object *obj2)
{
  return SAME_OBJ(obj1, obj2);
}

int scheme_eqv (Scheme_Object *obj1, Scheme_Object *obj2)
{
  Scheme_Type t1, t2;

  if (SAME_OBJ(obj1, obj2))
    return 1;

  t1 = SCHEME_TYPE(obj1);
  t2 = SCHEME_TYPE(obj2);

  if (NOT_SAME_TYPE(t1, t2)) {
#ifdef MZ_USE_SINGLE_FLOATS
    /* One case where different types can be eqv: float & doubles */
    if (((t1 == scheme_float_type) || (t1 == scheme_double_type))
	&& ((t2 == scheme_float_type) || (t2 == scheme_double_type))) {
      double d1, d2;
      d1 = SCHEME_FLOAT_VAL(obj1);
      d2 = SCHEME_FLOAT_VAL(obj2);
      if (MZ_IS_NAN(d1)) {
	if (MZ_IS_NAN(d2))
	  return 1;
	else
	  return 0;
      }
#ifdef NAN_EQUALS_ANYTHING
      if (MZ_IS_NAN(d2))
	return 0;
#endif
      return d1 == d2;
    }
#endif
    return 0;
  }

#ifndef FAST_NUMBERS
  else if (t1 == scheme_integer_type)
    return SCHEME_INT_VAL(obj1) == SCHEME_INT_VAL(obj2);
#endif
#ifdef MZ_USE_SINGLE_FLOATS
  else if (t1 == scheme_float_type) {
    float d1, d2;
    d1 = SCHEME_FLT_VAL(obj1);
    d2 = SCHEME_FLT_VAL(obj2);
    if (MZ_IS_NAN(d1)) {
      if (MZ_IS_NAN(d2))
	return 1;
      else
	return 0;
    }
#ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(d2))
      return 0;
#endif
    return d1 == d2;
  }
#endif
  else if (t1 == scheme_double_type) {
    double d1, d2;
    d1 = SCHEME_DBL_VAL(obj1);
    d2 = SCHEME_DBL_VAL(obj2);
    if (MZ_IS_NAN(d1)) {
      if (MZ_IS_NAN(d2))
	return 1;
      else
	return 0;
    }
#ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(d2))
      return 0;
#endif
    return d1 == d2;
  } else if (t1 == scheme_bignum_type)
    return scheme_bignum_eq(obj1, obj2);
  else if (t1 == scheme_rational_type)
    return scheme_rational_eq(obj1, obj2);
  else if (t1 == scheme_complex_type)
    return scheme_complex_eq(obj1, obj2);
  else
    return 0;
}

static Scheme_Object *equal_k(void)
{
#ifndef ERROR_ON_OVERFLOW
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *v1 = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object *v2 = (Scheme_Object *)p->ku.k.p2;

  p->ku.k.p1 = p->ku.k.p2 = NULL;
  
  return scheme_equal(v1, v2) ? scheme_true : scheme_false;
#else
  return scheme_void;
#endif
}

int scheme_equal (Scheme_Object *obj1, Scheme_Object *obj2)
{
 top:
  if (scheme_eqv (obj1, obj2))
    return 1;
  else if (NOT_SAME_TYPE(SCHEME_TYPE(obj1), SCHEME_TYPE(obj2)))
    return 0;
  else if (SCHEME_PAIRP(obj1)) {
#define EQUAL_COUNT_START 20
    static int equal_counter = EQUAL_COUNT_START;

    if (!--equal_counter) {
      equal_counter = EQUAL_COUNT_START;
      SCHEME_USE_FUEL(EQUAL_COUNT_START);
    
#ifdef DO_STACK_CHECK
      {
#include "mzstkchk.h"
	{
#ifndef ERROR_ON_OVERFLOW
	  Scheme_Process *p = scheme_current_process;
	  p->ku.k.p1 = (void *)obj1;
	  p->ku.k.p2 = (void *)obj2;
#endif
	  return SCHEME_TRUEP(scheme_handle_stack_overflow(equal_k));
	}
      }
#endif
    }

    if (SCHEME_NULLP(obj1) && SCHEME_NULLP(obj2))
      return 1;
    if (scheme_equal(SCHEME_CAR(obj1), SCHEME_CAR(obj2))) {
      obj1 = SCHEME_CDR(obj1);
      obj2 = SCHEME_CDR(obj2);
      goto top;
    } else
      return 0;
  } else if (SCHEME_VECTORP(obj1))
    return vector_equal(obj1, obj2);
  else if (SCHEME_STRINGP(obj1)) {
    int l1, l2;
    l1 = SCHEME_STRTAG_VAL(obj1);
    l2 = SCHEME_STRTAG_VAL(obj2);
    return ((l1 == l2) 
	    && !memcmp(SCHEME_STR_VAL(obj1), SCHEME_STR_VAL(obj2), l1));
  } else  if (SCHEME_STRUCTP(obj1))
    return scheme_equal_structs(obj1, obj2);
  else if (SCHEME_BOXP(obj1)) {
    SCHEME_USE_FUEL(1);
    obj1 = SCHEME_BOX_VAL(obj1);
    obj2 = SCHEME_BOX_VAL(obj2);
    goto top;
  } else
    return 0;
}

static int vector_equal(Scheme_Object *vec1, Scheme_Object *vec2)
{
  int i, len;
  Scheme_Object **vl1, **vl2;

  len = SCHEME_VEC_SIZE(vec1);
  if (len != SCHEME_VEC_SIZE(vec2))
    return 0;

  SCHEME_USE_FUEL(len);

  vl1 = SCHEME_VEC_ELS(vec1);
  vl2 = SCHEME_VEC_ELS(vec2);  

  for (i = 0; i < len; i++)
    if (!scheme_equal(vl1[i], vl2[i]))
      return 0;

  return 1;
}



