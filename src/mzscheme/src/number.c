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
#include "nummacs.h"
#include <math.h>
#include <string.h>
#include <ctype.h>
#ifndef DONT_IGNORE_FPE_SIGNAL
#include <signal.h>
#endif
#ifdef IGNORE_BY_CONTROL_387
#include <float.h>
#endif

#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
# ifndef MZ_USE_SINGLE_FLOATS
#  undef USE_SINGLE_FLOATS_AS_DEFAULT
# endif
#endif

#ifdef SIXTY_FOUR_BIT_INTEGERS
# define MAX_SHIFT_TRY 61
# define MAX_SHIFT_EVER 64
#else
# define MAX_SHIFT_TRY 29
# define MAX_SHIFT_EVER 32
#endif

#define REAL_NUMBER_STR "real number"

#define IZI_REAL_PART(n) (((Scheme_Complex *)(n))->r)

/* globals */
double scheme_infinity_val, scheme_minus_infinity_val;

/* locals */
static Scheme_Object *number_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *complex_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *real_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *rational_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *integer_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *exact_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *inexact_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *zero_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *positive_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *negative_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *even_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_max (int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_min (int argc, Scheme_Object *argv[]);
static Scheme_Object *plus (int argc, Scheme_Object *argv[]);
static Scheme_Object *minus (int argc, Scheme_Object *argv[]);
static Scheme_Object *mult (int argc, Scheme_Object *argv[]);
static Scheme_Object *div_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *abs_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *quotient (int argc, Scheme_Object *argv[]);
static Scheme_Object *rem_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *bitwise_and (int argc, Scheme_Object *argv[]);
static Scheme_Object *bitwise_or (int argc, Scheme_Object *argv[]);
static Scheme_Object *bitwise_xor (int argc, Scheme_Object *argv[]);
static Scheme_Object *bitwise_not (int argc, Scheme_Object *argv[]);
static Scheme_Object *bitwise_shift (int argc, Scheme_Object *argv[]);
static Scheme_Object *gcd (int argc, Scheme_Object *argv[]);
static Scheme_Object *lcm (int argc, Scheme_Object *argv[]);
static Scheme_Object *floor_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *ceiling (int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_truncate (int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_round (int argc, Scheme_Object *argv[]);
static Scheme_Object *numerator (int argc, Scheme_Object *argv[]);
static Scheme_Object *denominator (int argc, Scheme_Object *argv[]);
static Scheme_Object *exp_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *log_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *sin_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cos_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *tan_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *asin_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *acos_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *atan_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *make_rectangular (int argc, Scheme_Object *argv[]);
static Scheme_Object *make_polar (int argc, Scheme_Object *argv[]);
static Scheme_Object *real_part (int argc, Scheme_Object *argv[]);
static Scheme_Object *imag_part (int argc, Scheme_Object *argv[]);
static Scheme_Object *magnitude (int argc, Scheme_Object *argv[]);
static Scheme_Object *angle (int argc, Scheme_Object *argv[]);
static Scheme_Object *inexact_to_exact (int argc, Scheme_Object *argv[]);
static Scheme_Object *exact_to_inexact (int argc, Scheme_Object *argv[]);
static Scheme_Object *number_to_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_number (int argc, Scheme_Object *argv[]);

static Scheme_Object *random_seed(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_random(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_pseudo_random_generator(int argc, Scheme_Object **argv);
static Scheme_Object *current_pseudo_random_generator(int argc, Scheme_Object **argv);
static Scheme_Object *pseudo_random_generator_p(int argc, Scheme_Object **argv);

static int double_to_int(const char *where, double d, long *v);

static double not_a_number_val;

static char *infinity_str = "+inf.0";
static char *minus_infinity_str = "-inf.0";
static char *not_a_number_str = "+nan.0";
static char *other_not_a_number_str = "-nan.0";

static Scheme_Object *inf_object, *minus_inf_object, *nan_object;

#define zeroi scheme_make_integer(0)

static Scheme_Object *zerod, *nzerod, *scheme_pi, *scheme_half_pi, *plus_i, *minus_i;
#ifdef MZ_USE_SINGLE_FLOATS
static Scheme_Object *zerof, *nzerof, *single_scheme_pi;
static Scheme_Object *single_inf_object, *single_minus_inf_object, *single_nan_object;
#endif

double scheme_floating_point_zero = 0.0;
double scheme_floating_point_nzero = 0.0; /* negated below; many compilers treat -0.0 as 0.0, 
					     but otherwise correctly implement fp negation */

#ifdef FREEBSD_CONTROL_387
#include <machine/floatingpoint.h>
#endif
#ifdef LINUX_CONTROL_387
#include <fpu_control.h>
#endif
#ifdef ALPHA_CONTROL_FP
#include <machine/fpu.h>
#endif

void
scheme_init_number (Scheme_Env *env)
{
  if (scheme_starting_up) {
    REGISTER_SO(scheme_pi);
    REGISTER_SO(scheme_half_pi);
    REGISTER_SO(zerod);
    REGISTER_SO(nzerod);
#ifdef MZ_USE_SINGLE_FLOATS
    REGISTER_SO(single_scheme_pi);
    REGISTER_SO(zerof);
    REGISTER_SO(nzerof);
#endif
    REGISTER_SO(plus_i);
    REGISTER_SO(minus_i);
    REGISTER_SO(inf_object);
    REGISTER_SO(minus_inf_object);
    REGISTER_SO(nan_object);
#ifdef MZ_USE_SINGLE_FLOATS
    REGISTER_SO(single_inf_object);
    REGISTER_SO(single_minus_inf_object);
    REGISTER_SO(single_nan_object);
#endif
    
    START_XFORM_SKIP;
#ifndef DONT_IGNORE_FPE_SIGNAL
    MZ_SIGSET(SIGFPE, SIG_IGN);
#endif
#ifdef FREEBSD_CONTROL_387
    __fpsetreg(FP_MSKS_FLD, FP_MSKS_REG, FP_MSKS_FLD, FP_MSKS_OFF);
#endif
#ifdef LINUX_CONTROL_387
    __setfpucw(_FPU_EXTENDED + _FPU_RC_NEAREST + 0x3F);
#endif
#ifdef IGNORE_BY_CONTROL_387
    {
      int bits = 0x3F + _RC_NEAR + _PC_64;
      _control87(bits, 0xFFFF);
    }
#endif
#ifdef ALPHA_CONTROL_FP
    {
      long flags = ieee_get_fp_control();
      flags |= IEEE_TRAP_ENABLE_MASK;
      ieee_set_fp_control(flags);
    }
#endif
    END_XFORM_SKIP;

#if defined(HUGE_VAL) && !defined(USE_DIVIDE_MAKE_INFINITY)
    scheme_infinity_val = HUGE_VAL;
#else
#ifndef USE_INFINITY_FUNC
    scheme_infinity_val = 1.0 / scheme_floating_point_zero;
#else
    scheme_infinity_val = infinity();
#endif
#endif

#ifdef ZERO_MINUS_ZERO_IS_POS_ZERO
    scheme_floating_point_nzero = -1.0 / scheme_infinity_val;
#else
    scheme_floating_point_nzero = - scheme_floating_point_nzero;
#endif

    scheme_minus_infinity_val = -scheme_infinity_val;
    not_a_number_val = scheme_infinity_val + scheme_minus_infinity_val;

    zerod = scheme_make_double(1.0);
    SCHEME_DBL_VAL(zerod) = 0.0;
    nzerod = scheme_make_double(-1.0);
    SCHEME_DBL_VAL(nzerod) = scheme_floating_point_nzero;

    scheme_pi = scheme_make_double(atan2(0, -1));
    scheme_half_pi = scheme_make_double(atan2(0, -1)/2);
#ifdef MZ_USE_SINGLE_FLOATS
    zerof = scheme_make_float(0.0f);
    nzerof = scheme_make_float(-0.0f);
    single_scheme_pi = scheme_make_float((float)atan2(0, -1));
#endif
    plus_i = scheme_make_complex(scheme_make_integer(0), scheme_make_integer(1));
    minus_i = scheme_make_complex(scheme_make_integer(0), scheme_make_integer(-1));

    inf_object = scheme_make_double(scheme_infinity_val);
    minus_inf_object = scheme_make_double(scheme_minus_infinity_val);
#ifdef NAN_EQUALS_ANYTHING
    nan_object = scheme_make_double(1);
    SCHEME_DBL_VAL(nan_object) = not_a_number_val;
#else
    nan_object = scheme_make_double(not_a_number_val);
#endif
#ifdef MZ_USE_SINGLE_FLOATS
    single_inf_object = scheme_make_float((float)scheme_infinity_val);
    single_minus_inf_object = scheme_make_float((float)scheme_minus_infinity_val);
    single_nan_object = scheme_make_float((float)not_a_number_val);
#endif
  }

  scheme_add_global_constant("number?", 
			     scheme_make_folding_prim(number_p,
						      "number?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("complex?", 
			     scheme_make_folding_prim(complex_p,
						      "complex?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("real?", 
			     scheme_make_folding_prim(real_p,
						      "real?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("rational?", 
			     scheme_make_folding_prim(rational_p,
						      "rational?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("integer?", 
			     scheme_make_folding_prim(integer_p,
						      "integer?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("exact?", 
			     scheme_make_folding_prim(exact_p,
						      "exact?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("inexact?", 
			     scheme_make_folding_prim(inexact_p,
						      "inexact?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("=", 
			     scheme_make_folding_prim(eq,
						      "=",
						      1, -1, 1),
			     env);
  scheme_add_global_constant("<", 
			     scheme_make_folding_prim(lt,
						      "<",
						      1, -1, 1),
			     env);
  scheme_add_global_constant(">", 
			     scheme_make_folding_prim(gt,
						      ">",
						      1, -1, 1),
			     env);
  scheme_add_global_constant("<=", 
			     scheme_make_folding_prim(lt_eq,
						      "<=",
						      1, -1, 1),
			     env);
  scheme_add_global_constant(">=", 
			     scheme_make_folding_prim(gt_eq,
						      ">=",
						      1, -1, 1),
			     env);
  scheme_add_global_constant("zero?", 
			     scheme_make_folding_prim(zero_p,
						      "zero?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("positive?", 
			     scheme_make_folding_prim(positive_p,
						      "positive?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("negative?", 
			     scheme_make_folding_prim(negative_p,
						      "negative?",
						      1, 1, 1),
			     env);

  scheme_add_global_constant("add1", 
			     scheme_make_folding_prim(scheme_add1,
						      "add1",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("sub1", 
			     scheme_make_folding_prim(scheme_sub1,
						      "sub1",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("odd?", 
			     scheme_make_folding_prim(scheme_odd_p,
						      "odd?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("even?", 
			     scheme_make_folding_prim(even_p,
						      "even?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("max", 
			     scheme_make_folding_prim(sch_max,
						      "max",
						      1, -1, 1),
			     env);
  scheme_add_global_constant("min", 
			     scheme_make_folding_prim(sch_min,
						      "min",
						      1, -1, 1),
			     env);
  scheme_add_global_constant("+", 
			     scheme_make_folding_prim(plus,
						      "+", 
						      0, -1, 1),
			     env);
  scheme_add_global_constant("-", 
			     scheme_make_folding_prim(minus,
						      "-",
						      1, -1, 1),
			     env);
  scheme_add_global_constant("*", 
			     scheme_make_folding_prim(mult,
						      "*", 
						      0, -1, 1),
			     env);
  scheme_add_global_constant("/", 
			     scheme_make_folding_prim(div_prim,
						      "/",
						      1, -1, 1),
			     env);
  scheme_add_global_constant("abs", 
			     scheme_make_folding_prim(abs_prim,
						      "abs",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("quotient", 
			     scheme_make_folding_prim(quotient,
						      "quotient", 
						      2, 2, 1),
			     env);
  scheme_add_global_constant("remainder", 
			     scheme_make_folding_prim(rem_prim,
						      "remainder", 
						      2, 2, 1),
			     env);
  scheme_add_global_constant("modulo", 
			     scheme_make_folding_prim(scheme_modulo,
						      "modulo", 
						      2, 2, 1),
			     env);
  scheme_add_global_constant("bitwise-and", 
			     scheme_make_folding_prim(bitwise_and,
						      "bitwise-and",
						      1, -1, 1),
			     env);
  scheme_add_global_constant("bitwise-ior", 
			     scheme_make_folding_prim(bitwise_or,
						      "bitwise-ior",
						      1, -1, 1),
			     env);
  scheme_add_global_constant("bitwise-xor", 
			     scheme_make_folding_prim(bitwise_xor,
						      "bitwise-xor",
						      1, -1, 1),
			     env);
  scheme_add_global_constant("bitwise-not", 
			     scheme_make_folding_prim(bitwise_not,
						      "bitwise-not",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("arithmetic-shift", 
			     scheme_make_folding_prim(bitwise_shift,
						      "arithmetic-shift",
						      2, 2, 1),
			     env);
  scheme_add_global_constant("gcd", 
			     scheme_make_folding_prim(gcd,
						      "gcd", 
						      0, -1, 1),
			     env);
  scheme_add_global_constant("lcm", 
			     scheme_make_folding_prim(lcm,
						      "lcm", 
						      0, -1, 1),
			     env);
  scheme_add_global_constant("floor", 
			     scheme_make_folding_prim(floor_prim,
						      "floor",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("ceiling", 
			     scheme_make_folding_prim(ceiling,
						      "ceiling",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("truncate", 
			     scheme_make_folding_prim(sch_truncate,
						      "truncate",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("round", 
			     scheme_make_folding_prim(sch_round,
						      "round",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("numerator", 
			     scheme_make_folding_prim(numerator,
						      "numerator",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("denominator", 
			     scheme_make_folding_prim(denominator,
						      "denominator",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("exp", 
			     scheme_make_folding_prim(exp_prim,
						      "exp",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("log", 
			     scheme_make_folding_prim(log_prim,
						      "log",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("sin", 
			     scheme_make_folding_prim(sin_prim,
						      "sin",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("cos", 
			     scheme_make_folding_prim(cos_prim,
						      "cos",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("tan", 
			     scheme_make_folding_prim(tan_prim,
						      "tan",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("asin", 
			     scheme_make_folding_prim(asin_prim,
						      "asin",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("acos", 
			     scheme_make_folding_prim(acos_prim,
						      "acos",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("atan", 
			     scheme_make_folding_prim(atan_prim,
						      "atan",
						      1, 2, 1),
			     env);
  scheme_add_global_constant("sqrt", 
			     scheme_make_folding_prim(scheme_sqrt,
						      "sqrt",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("expt", 
			     scheme_make_folding_prim(scheme_expt,
						      "expt", 
						      2, 2, 1),
			     env);
  scheme_add_global_constant("make-rectangular", 
			     scheme_make_folding_prim(make_rectangular,
						      "make-rectangular", 
						      2, 2, 1),
			     env);
  scheme_add_global_constant("make-polar", 
			     scheme_make_folding_prim(make_polar,
						      "make-polar", 
						      2, 2, 1),
			     env);
  scheme_add_global_constant("real-part", 
			     scheme_make_folding_prim(real_part,
						      "real-part",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("imag-part", 
			     scheme_make_folding_prim(imag_part,
						      "imag-part",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("angle", 
			     scheme_make_folding_prim(angle,
						      "angle",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("magnitude", 
			     scheme_make_folding_prim(magnitude,
						      "magnitude",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("exact->inexact", 
			     scheme_make_folding_prim(exact_to_inexact,
						      "exact->inexact",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("inexact->exact", 
			     scheme_make_folding_prim(inexact_to_exact,
						      "inexact->exact",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("number->string", 
			     scheme_make_prim_w_arity(number_to_string,
						      "number->string",
						      1, 2),
			     env);
  scheme_add_global_constant("string->number", 
			     scheme_make_folding_prim(string_to_number,
						      "string->number", 
						      1, 2, 1),
			     env);

  scheme_add_global_constant("random", 
			     scheme_make_prim_w_arity(sch_random,
						      "random",
						      1, 1),
			     env);
  scheme_add_global_constant("random-seed", 
			     scheme_make_prim_w_arity(random_seed,
						      "random-seed",
						      1, 1),
			     env);
  scheme_add_global_constant("make-pseudo-random-generator", 
			     scheme_make_prim_w_arity(make_pseudo_random_generator,
						      "make-pseudo-random-generator", 
						      0, 0), 
			     env);
  scheme_add_global_constant("pseudo-random-generator?", 
			     scheme_make_prim_w_arity(pseudo_random_generator_p,
						      "pseudo-random-generator?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("current-pseudo-random-generator", 
			     scheme_register_parameter(current_pseudo_random_generator,
						       "current-pseudo-random-generator",
						       MZCONFIG_RANDOM_STATE),
			     env);
}


Scheme_Object *
scheme_make_integer_value(long i)
{
  Scheme_Object *o = scheme_make_integer(i);
  
  if (SCHEME_INT_VAL(o) == i)
    return o;
  else
    return scheme_make_bignum(i);
}

Scheme_Object *
scheme_make_integer_value_from_unsigned(unsigned long i)
{
  Scheme_Object *o = scheme_make_integer(i);
  
  if ((SCHEME_INT_VAL(o) >= 0)
      && ((unsigned long)SCHEME_INT_VAL(o)) == i)
    return o;
  else
    return scheme_make_bignum_from_unsigned(i);
}

int scheme_get_int_val(Scheme_Object *o, long *v)
{
  if (SCHEME_INTP(o)) {
    *v = SCHEME_INT_VAL(o);
    return 1;
  } else if (SCHEME_BIGNUMP(o))
    return scheme_bignum_get_int_val(o, v);
  else
    return 0;
}

int scheme_get_unsigned_int_val(Scheme_Object *o, unsigned long *v)
{
  if (SCHEME_INTP(o)) {
    long i = SCHEME_INT_VAL(o);
    if (i < 0)
      return 0;
    *v = i;
    return 1;
  } else if (SCHEME_BIGNUMP(o))
    return scheme_bignum_get_unsigned_int_val(o, v);
  else
    return 0;
}

double scheme_real_to_double(Scheme_Object *r)
{
  if (SCHEME_INTP(r))
    return (double)SCHEME_INT_VAL(r);
  else if (SCHEME_DBLP(r))
    return SCHEME_DBL_VAL(r);
#ifdef MZ_USE_SINGLE_FLOATS
  else if (SCHEME_FLTP(r))
    return SCHEME_FLT_VAL(r);
#endif
  else if (SCHEME_BIGNUMP(r))
    return scheme_bignum_to_double(r);
  else if (SCHEME_RATIONALP(r))
    return scheme_rational_to_double(r);
  else if (SCHEME_COMPLEX_IZIP(r))
    return scheme_real_to_double(IZI_REAL_PART(r));
  else
    return 0.0;
}

static
#ifndef NO_INLINE_KEYWORD
MSC_IZE(inline)
#endif
int minus_zero_p(double d)
{
  double a[2];
  long *f, *s;

  a[0] = d;
  a[1] = scheme_floating_point_nzero;

  f = (long *)a;
  s = (long *)(a + 1);

  if (f[0] == s[0] && f[1] == s[1])
    return 1;

  return 0;
}

#ifdef DEFEAT_FP_COMP_OPTIMIZATION
int scheme_both_nan(double a, double b)
{
  /* Called by the MZ_IS_NAN() macro for certain compilers.
     A and B are actually the same FP number, but the compiler
     optimizes (A == A) to TRUE, so we use a function call to
     hide the fact that A and B are the same. */
  return a != b;
}
#endif

Scheme_Object *scheme_make_double(double d)
{
  Scheme_Double *sd;

  if (d == 0.0) {
    if (minus_zero_p(d))
      return nzerod;
#ifdef NAN_EQUALS_ANYTHING
    else if (MZ_IS_NAN(d))
      return nan_object;
#endif
    else
      return zerod;
  }

  sd = (Scheme_Double *)scheme_malloc_atomic_tagged(sizeof(Scheme_Double));
  sd->type = scheme_double_type;
  SCHEME_DBL_VAL(sd) = d;
  return (Scheme_Object *)sd;
}

#ifdef MZ_USE_SINGLE_FLOATS
Scheme_Object *scheme_make_float(float f)
{
  Scheme_Float *sf;

  sf = (Scheme_Float *)scheme_malloc_atomic_tagged(sizeof(Scheme_Float));
  sf->type = scheme_float_type;
  SCHEME_FLT_VAL(sf) = f;
  return (Scheme_Object *)sf;
}
#endif

/* locals */

#define NEED_NUMBER(name) \
  scheme_wrong_type(#name, "number", 0, argc, argv)
#define NEED_REAL(name) \
  scheme_wrong_type(#name, REAL_NUMBER_STR, 0, argc, argv)
#define NEED_INTEGER(name) \
  scheme_wrong_type(#name, "integer", 0, argc, argv)
#define WRONG_TYPE(name, expected, value) \
  scheme_wrong_type(name, expected, -1, 0, (Scheme_Object **)&value)

static Scheme_Object *
number_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  return (SCHEME_NUMBERP(o) ? scheme_true : scheme_false);
}

static Scheme_Object *
complex_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  return (SCHEME_NUMBERP(o) ? scheme_true : scheme_false);
}

static Scheme_Object *
real_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  return (SCHEME_REALP(o) ? scheme_true : scheme_false);
}

static Scheme_Object *
rational_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  return (SCHEME_REALP(o) ? scheme_true : scheme_false);
}
	  
static int is_integer(const Scheme_Object *o)
{
  if (SCHEME_INTP(o) || SCHEME_BIGNUMP(o))
    return 1;

  if (SCHEME_FLOATP(o)) {
    double d;
    d = SCHEME_FLOAT_VAL(o);
# ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(d))
      return 0;
# endif
    if (floor(d) == d)
      return 1;
  }

  if (SCHEME_COMPLEX_IZIP(o))
    return is_integer(IZI_REAL_PART(o));

  return 0;
}


static Scheme_Object *
integer_p (int argc, Scheme_Object *argv[])
{
  return is_integer(argv[0]) ? scheme_true : scheme_false;
}

int scheme_is_exact(Scheme_Object *n)
{
  if (SCHEME_INTP(n)) {
    return 1;
  } else {
    Scheme_Type type = _SCHEME_TYPE(n);
    if ((type == scheme_bignum_type)
	|| (type == scheme_rational_type))
      return 1;
    else if (type == scheme_complex_type) {
      return scheme_is_complex_exact(n);
    } else if (type == scheme_double_type)
      return 0;
#ifdef MZ_USE_SINGLE_FLOATS
    else if (type == scheme_float_type)
      return 0;
#endif
    else if (type == scheme_complex_izi_type)
      return 0;
    else {
      scheme_wrong_type("exact?", "number", 0, 1, &n);
      return 0;
    }
  }
}

static Scheme_Object *
exact_p (int argc, Scheme_Object *argv[])
{
  return (scheme_is_exact(argv[0])
	  ? scheme_true 
	  : scheme_false);
}

int scheme_is_inexact(Scheme_Object *n)
{
  if (SCHEME_INTP(n)) {
    return 0;
  } else {
    Scheme_Type type = _SCHEME_TYPE(n);
    if ((type == scheme_bignum_type)
	|| (type == scheme_rational_type))
      return 0;
    else if (type == scheme_complex_type) {
      return !scheme_is_complex_exact(n);
    } else if (type == scheme_double_type)
      return 1;
#ifdef MZ_USE_SINGLE_FLOATS
    else if (type == scheme_float_type)
      return 1;
#endif
    else if (type == scheme_complex_izi_type)
      return 1;
    else {
      scheme_wrong_type("inexact?", "number", 0, 1, &n);
      return 0;
    }
  }
}

static Scheme_Object *
inexact_p (int argc, Scheme_Object *argv[])
{
  return (scheme_is_inexact(argv[0])
	  ? scheme_true 
	  : scheme_false);
}

GEN_NARY_COMP(eq, "=", scheme_bin_eq, SCHEME_NUMBERP, "number")
GEN_NARY_COMP(lt, "<", scheme_bin_lt, SCHEME_REALP, REAL_NUMBER_STR)
GEN_NARY_COMP(gt, ">", scheme_bin_gt, SCHEME_REALP, REAL_NUMBER_STR)
GEN_NARY_COMP(lt_eq, "<=", scheme_bin_lt_eq, SCHEME_REALP, REAL_NUMBER_STR)
GEN_NARY_COMP(gt_eq, ">=", scheme_bin_gt_eq, SCHEME_REALP, REAL_NUMBER_STR)

#define EQUAL(x, y) (x == y)
#define LESS_THAN(x, y) (x < y)
#define GREATER_THAN(x, y) (x > y)
#define LESS_OR_EQUAL(x, y) (x <= y)
#define GREATER_OR_EQUAL(x, y) (x >= y)

#ifdef NAN_LT_COMPARISON_WRONG
# define fLESS_THAN(x, y) (!(x >= y) && (x == x) && (y == y))
# define fLESS_OR_EQUAL(x, y) (!(x > y) && (x == x) && (y == y))
#else
# define fLESS_THAN LESS_THAN
# define fLESS_OR_EQUAL LESS_OR_EQUAL
#endif

#define COMP_IZI_LT(a, b) scheme_bin_lt(IZI_REAL_PART(a), IZI_REAL_PART(b))
#define COMP_IZI_GT(a, b) scheme_bin_gt(IZI_REAL_PART(a), IZI_REAL_PART(b))
#define COMP_IZI_LT_EQ(a, b) scheme_bin_lt_eq(IZI_REAL_PART(a), IZI_REAL_PART(b))
#define COMP_IZI_GT_EQ(a, b) scheme_bin_gt_eq(IZI_REAL_PART(a), IZI_REAL_PART(b))

#define GEN_IDENT_FOR_IZI GEN_IDENT

GEN_BIN_COMP(scheme_bin_eq, "=", EQUAL, EQUAL, scheme_bignum_eq, scheme_rational_eq, scheme_complex_eq, 0, 0, inexact_p, inexact_p, GEN_IDENT, GEN_IDENT, "number")
GEN_BIN_COMP(scheme_bin_lt, "<", LESS_THAN, fLESS_THAN, scheme_bignum_lt, scheme_rational_lt, COMP_IZI_LT, 0, 1, positive_p, negative_p, GEN_IDENT_FOR_IZI, GEN_OMIT, REAL_NUMBER_STR)
GEN_BIN_COMP(scheme_bin_gt, ">", GREATER_THAN, GREATER_THAN, scheme_bignum_gt, scheme_rational_gt, COMP_IZI_GT, 1, 0, negative_p, positive_p, GEN_IDENT_FOR_IZI, GEN_OMIT, REAL_NUMBER_STR)
GEN_BIN_COMP(scheme_bin_lt_eq, "<=", LESS_OR_EQUAL, fLESS_OR_EQUAL, scheme_bignum_le, scheme_rational_le, COMP_IZI_LT_EQ, 0, 1, positive_p, negative_p, GEN_IDENT_FOR_IZI, GEN_OMIT, REAL_NUMBER_STR)
GEN_BIN_COMP(scheme_bin_gt_eq, ">=", GREATER_OR_EQUAL, GREATER_OR_EQUAL, scheme_bignum_ge, scheme_rational_ge, COMP_IZI_GT_EQ, 1, 0, negative_p, positive_p, GEN_IDENT_FOR_IZI, GEN_OMIT, REAL_NUMBER_STR)

static Scheme_Object *
zero_p (int argc, Scheme_Object *argv[])
{
  Scheme_Type t;
  Scheme_Object *o = argv[0];

  if (SCHEME_INTP(o))
#ifdef FAST_NUMBERS
    return (o == zeroi) ? scheme_true : scheme_false;
#else
    return SCHEME_INT_VAL(o) ? scheme_false : scheme_true;
#endif
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type) {
# ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(SCHEME_FLT_VAL(o)))
      return scheme_false;
# endif
    return (SCHEME_FLT_VAL(o) == 0.0f) ? scheme_true : scheme_false;
  }
#endif
  if (t == scheme_double_type) {
#ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(SCHEME_DBL_VAL(o)))
      return scheme_false;
#endif
    return (SCHEME_DBL_VAL(o) == 0.0) ? scheme_true : scheme_false;
  }

  if (t == scheme_complex_izi_type) {
    Scheme_Object *r = IZI_REAL_PART(o);
    return zero_p(1, &r);
  }

  if ((t >= scheme_bignum_type) && (t <= scheme_complex_type))
    return scheme_false;
 
  NEED_NUMBER(zero?);

  return NULL;
}

static Scheme_Object *
positive_p (int argc, Scheme_Object *argv[])
{
  Scheme_Type t;
  Scheme_Object *o = argv[0];

  if (SCHEME_INTP(o))
    return (SCHEME_INT_VAL(o) > 0 ? scheme_true : scheme_false);
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type) {
    float d = SCHEME_FLT_VAL(o);
# ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(d))
      return scheme_false;
# endif
    return (d > 0 ? scheme_true : scheme_false);
  }
#endif
  if (t == scheme_double_type) {
    double d = SCHEME_DBL_VAL(o);
#ifdef NAN_EQUALS_ANYTHING
    if (MZ_IS_NAN(d))
      return scheme_false;
#endif
    return (d > 0 ? scheme_true : scheme_false);
  }
  if (t == scheme_bignum_type)
    return (SCHEME_BIGPOS(o) ? scheme_true : scheme_false);
  if (t == scheme_rational_type)
    return (scheme_is_rational_positive(o)  ? scheme_true : scheme_false);
  if (t == scheme_complex_izi_type) {
    Scheme_Object *r = IZI_REAL_PART(o);
    return positive_p(1, &r);
  }


  NEED_REAL(positive?);

  return NULL;
}

static Scheme_Object *
negative_p (int argc, Scheme_Object *argv[])
{
  Scheme_Type t;
  Scheme_Object *o = argv[0];

  if (SCHEME_INTP(o))
    return (SCHEME_INT_VAL(o) < 0 ? scheme_true : scheme_false);
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type) {
    float d = SCHEME_FLT_VAL(o);
# if defined(NAN_EQUALS_ANYTHING) || defined(NAN_LT_COMPARISON_WRONG)
    if (MZ_IS_NAN(d))
      return scheme_false;
# endif
    return (d < 0 ? scheme_true : scheme_false);
  }
#endif
  if (t == scheme_double_type) {
    double d = SCHEME_DBL_VAL(o);
# if defined(NAN_EQUALS_ANYTHING) || defined(NAN_LT_COMPARISON_WRONG)
    if (MZ_IS_NAN(d))
      return scheme_false;
#endif
    return (d < 0 ? scheme_true : scheme_false);
  }
  if (t == scheme_bignum_type)
    return (!SCHEME_BIGPOS(o) ? scheme_true : scheme_false);
  if (t == scheme_rational_type)
    return (!scheme_is_rational_positive(o) ? scheme_true : scheme_false);
  if (t == scheme_complex_izi_type) {
    Scheme_Object *r = IZI_REAL_PART(o);
    return negative_p(1, &r);
  }

  NEED_REAL(negative?);

  return NULL;
}

Scheme_Object *
scheme_odd_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];

  if (SCHEME_INTP(v))
    return (SCHEME_INT_VAL(v) & 0x1) ? scheme_true : scheme_false;
  if (SCHEME_BIGNUMP(v))
    return (SCHEME_BIGDIG(v)[0] & 0x1) ? scheme_true : scheme_false;
  if (SCHEME_COMPLEX_IZIP(v)) {
    Scheme_Object *r = IZI_REAL_PART(v);
    return scheme_odd_p(1, &r);
  }
  
  if (is_integer(v)) {
    double d = SCHEME_FLOAT_VAL(v);
    if (MZ_IS_POS_INFINITY(d) || MZ_IS_NEG_INFINITY(d))
      return scheme_true;
    return (fmod(d, 2.0) == 0.0) ? scheme_false : scheme_true;
  }

  NEED_INTEGER(odd?);

  return scheme_void;
}

static Scheme_Object *
even_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];

  if (SCHEME_INTP(v))
    return (SCHEME_INT_VAL(v) & 0x1) ? scheme_false : scheme_true;
  if (SCHEME_BIGNUMP(v))
    return (SCHEME_BIGDIG(v)[0] & 0x1) ? scheme_false : scheme_true;
  if (SCHEME_COMPLEX_IZIP(v)) {
    Scheme_Object *r = IZI_REAL_PART(v);
    return even_p(1, &r);
  }

  if (is_integer(v)) {
    double d = SCHEME_FLOAT_VAL(v);
    if (MZ_IS_POS_INFINITY(d) || MZ_IS_NEG_INFINITY(d))
      return scheme_true;
    return (fmod(d, 2.0) == 0.0) ? scheme_true : scheme_false;
  }

  NEED_INTEGER(even?);

  return scheme_void;
}

Scheme_Object *
scheme_add1 (int argc, Scheme_Object *argv[])
{
  Scheme_Type t;
  Scheme_Object *o = argv[0];

  if (SCHEME_INTP(o)) {
    long v;
    v = SCHEME_INT_VAL(o);
    if (v < 0x3FFFFFFF)
      return scheme_make_integer(v + 1);
    else
      return scheme_bignum_add1(scheme_make_bignum(v));
  }
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type)
    return scheme_make_float(SCHEME_FLT_VAL(o) + 1.0f);
#endif
  if (t == scheme_double_type)
    return scheme_make_double(SCHEME_DBL_VAL(o) + 1.0);
  if (t == scheme_bignum_type)
    return scheme_bignum_add1(o);
  if (t == scheme_rational_type)
    return scheme_rational_add1(o);
  if ((t == scheme_complex_type) || (t == scheme_complex_izi_type))
    return scheme_complex_add1(o);

  NEED_NUMBER(add1);

  return scheme_void;
}

Scheme_Object *
scheme_sub1 (int argc, Scheme_Object *argv[])
{
  Scheme_Type t;
  Scheme_Object *o = argv[0];

  if (SCHEME_INTP(o)) {
    long v;
    v = SCHEME_INT_VAL(o);
    if (v > -(0x3FFFFFFF))
      return scheme_make_integer(SCHEME_INT_VAL(o) - 1);
    else
      return scheme_bignum_sub1(scheme_make_bignum(v));
  }
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type)
    return scheme_make_float(SCHEME_FLT_VAL(o) - 1.0f);
#endif
  if (t == scheme_double_type)
    return scheme_make_double(SCHEME_DBL_VAL(o) - 1.0);
  if (t == scheme_bignum_type)
    return scheme_bignum_sub1(o);
  if (t == scheme_rational_type)
    return scheme_rational_sub1(o);
  if ((t == scheme_complex_type) || (t == scheme_complex_izi_type))
    return scheme_complex_sub1(o);
  
  NEED_NUMBER(sub1);

  return scheme_void;
}

GEN_BIN_PROT(bin_max);
GEN_BIN_PROT(bin_min);

#define F_ADD(x,y) scheme_make_double(x + y)
#define F_SUBTRACT(x,y) scheme_make_double(x - y)
#define F_MULTIPLY(x,y) scheme_make_double(x * y)
#define DIVIDE(x,y) scheme_make_fixnum_rational(x, y)
#define F_DIVIDE(x,y) scheme_make_double((double)x / (double)y)
#define MAX(n1,n2) scheme_make_integer((n1>n2) ? n1 : n2)
#define MIN(n1,n2) scheme_make_integer((n1<n2) ? n1 : n2)
#define F_MAX(n1,n2) scheme_make_double((n1>n2) ? n1 : n2)
#define F_MIN(n1,n2) scheme_make_double((n1<n2) ? n1 : n2)

#define FS_ADD(x,y) scheme_make_float(x + y)
#define FS_SUBTRACT(x,y) scheme_make_float(x - y)
#define FS_MULTIPLY(x,y) scheme_make_float(x * y)
#define FS_DIVIDE(x,y) scheme_make_float((float)x / (float)y)
#define FS_MAX(n1,n2) scheme_make_float((n1>n2) ? n1 : n2)
#define FS_MIN(n1,n2) scheme_make_float((n1<n2) ? n1 : n2)

static Scheme_Object *ADD(long a, long b)
{
  long r;
  Scheme_Object *o;

  r = a + b;

  o = scheme_make_integer(r);
  r = SCHEME_INT_VAL(o);

  if (b == r - a)
    return o;
  else {
    Small_Bignum sa, sb;
    return scheme_bignum_add(scheme_make_small_bignum(a, &sa),
			     scheme_make_small_bignum(b, &sb));
  }
}

static Scheme_Object *SUBTRACT(long a, long b)
{
  long r;
  Scheme_Object *o;

  r = a - b;

  o = scheme_make_integer(r);
  r = SCHEME_INT_VAL(o);

  if (a == r + b)
    return o;
  else {
    Small_Bignum sa, sb;
    return scheme_bignum_subtract(scheme_make_small_bignum(a, &sa),
				  scheme_make_small_bignum(b, &sb));
  }
}

static Scheme_Object *MULTIPLY(long a, long b)
{
  long r;
  Scheme_Object *o;

  if (!b)
    return zeroi;

  r = a * b;

  o = scheme_make_integer(r);
  r = SCHEME_INT_VAL(o);

  if (a == r / b)
    return o;
  else {
    Small_Bignum sa, sb;
    return scheme_bignum_multiply(scheme_make_small_bignum(a, &sa),
				  scheme_make_small_bignum(b, &sb));
  }
}

#define MAX_IZI(a, b) bin_max(IZI_REAL_PART(a), IZI_REAL_PART(b))
#define MIN_IZI(a, b) bin_min(IZI_REAL_PART(a), IZI_REAL_PART(b))

GEN_BIN_OP(scheme_bin_plus, "+", ADD, F_ADD, FS_ADD, scheme_bignum_add, scheme_rational_add, scheme_complex_add, GEN_RETURN_N2, GEN_RETURN_N1, NO_NAN_CHECK, NO_NAN_CHECK)
GEN_BIN_OP(scheme_bin_minus, "-", SUBTRACT, F_SUBTRACT, FS_SUBTRACT, scheme_bignum_subtract, scheme_rational_subtract, scheme_complex_subtract, GEN_SINGLE_SUBTRACT_N2, GEN_RETURN_N1, NO_NAN_CHECK, NO_NAN_CHECK)
GEN_BIN_OP(scheme_bin_mult, "*", MULTIPLY, F_MULTIPLY, FS_MULTIPLY, scheme_bignum_multiply, scheme_rational_multiply, scheme_complex_multiply, GEN_RETURN_0, GEN_RETURN_0, NO_NAN_CHECK, NO_NAN_CHECK)
GEN_BIN_DIV_OP(scheme_bin_div, "/", DIVIDE, F_DIVIDE, FS_DIVIDE, scheme_make_rational, scheme_rational_divide, scheme_complex_divide)

static GEN_BIN_OP(bin_max, "max", MAX, F_MAX, FS_MAX, scheme_bignum_max, scheme_rational_max, MAX_IZI, GEN_OMIT, GEN_OMIT, NAN_RETURNS_NAN, NAN_RETURNS_SNAN)
static GEN_BIN_OP(bin_min, "min", MIN, F_MIN, FS_MIN, scheme_bignum_min, scheme_rational_min, MIN_IZI, GEN_OMIT, GEN_OMIT, NAN_RETURNS_NAN, NAN_RETURNS_SNAN)

GEN_BIN_PROT(bin_bitwise_and);
GEN_BIN_PROT(bin_bitwise_or);
GEN_BIN_PROT(bin_bitwise_xor);

GEN_BIN_INT_OP(bin_bitwise_and, "bitwise-and", &, scheme_bignum_and)
GEN_BIN_INT_OP(bin_bitwise_or, "bitwise-ior", |, scheme_bignum_or)
GEN_BIN_INT_OP(bin_bitwise_xor, "bitwise-xor", ^, scheme_bignum_xor)

GEN_TWOARY_OP(bitwise_and, "bitwise-and", bin_bitwise_and, SCHEME_EXACT_INTEGERP, "exact integer")
GEN_TWOARY_OP(bitwise_or, "bitwise-ior", bin_bitwise_or, SCHEME_EXACT_INTEGERP, "exact integer")
GEN_TWOARY_OP(bitwise_xor, "bitwise-xor", bin_bitwise_xor, SCHEME_EXACT_INTEGERP, "exact integer")

GEN_TWOARY_OP(sch_max, "max", bin_max, SCHEME_REALP, REAL_NUMBER_STR)
GEN_TWOARY_OP(sch_min, "min", bin_min, SCHEME_REALP, REAL_NUMBER_STR)
GEN_NARY_OP(plus, "+", scheme_bin_plus, 0, SCHEME_NUMBERP, "number")
GEN_NARY_OP(mult, "*", scheme_bin_mult, 1, SCHEME_NUMBERP, "number")

static Scheme_Object *
minus (int argc, Scheme_Object *argv[])
{
  Scheme_Object *ret;
  int i;

  ret = argv[0];
  if (argc == 1) {
    if (SCHEME_FLOATP(ret)) {
#ifdef MZ_USE_SINGLE_FLOATS
      if (SCHEME_FLTP(ret))
	return scheme_make_float(-SCHEME_FLT_VAL(ret));
#endif
      return scheme_make_double(-SCHEME_DBL_VAL(ret));
    }
    if (!SCHEME_NUMBERP(ret)) {
      scheme_wrong_type("-", "number", 0, argc, argv);
      return NULL;
    }
    return scheme_bin_minus(zeroi, ret);
  }
  for (i = 1; i < argc; i++) {
    Scheme_Object *o = argv[i];
    if (!SCHEME_NUMBERP(o)) {
      scheme_wrong_type("-", "number", i, argc, argv);
      return NULL;
    }
    ret = scheme_bin_minus(ret, o);
  }
  return ret;
}

static Scheme_Object *
div_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *ret;
  int i;

  ret = argv[0];
  if (!SCHEME_NUMBERP(ret)) {
    scheme_wrong_type("/", "number", 0, argc, argv);
    return NULL;
  }
  if (argc == 1) {
    if (ret != zeroi)
      return scheme_bin_div(scheme_make_integer(1), ret);
    else {
      scheme_raise_exn(MZEXN_APPLICATION_DIVIDE_BY_ZERO, ret,
		       "/: division by zero");
      return NULL;
    }
  }
  for (i = 1; i < argc; i++) {
    Scheme_Object *o = argv[i];

    if (!SCHEME_NUMBERP(o)) {
      scheme_wrong_type("/", "number", i, argc, argv);
      return NULL;
    }

    if (o != zeroi)
      ret = scheme_bin_div(ret, o);
    else {
      scheme_raise_exn(MZEXN_APPLICATION_DIVIDE_BY_ZERO, o,
		       "/: division by zero");
      return NULL;
    }
  }
  return ret;
}

#define ABS(n)  ((n>0) ? n : -n)

static Scheme_Object *
abs_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Type t;
  Scheme_Object *o;

  o = argv[0];

  if (SCHEME_INTP(o)) {
    int n = SCHEME_INT_VAL(o);
    return scheme_make_integer(ABS(n));
  } 
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type)
    return scheme_make_float(fabs(SCHEME_FLT_VAL(o)));
#endif
  if (t == scheme_double_type)
    return scheme_make_double(fabs(SCHEME_DBL_VAL(o)));
  if (t == scheme_bignum_type) {
    if (SCHEME_BIGPOS(o))
      return o;
    return scheme_bignum_negate(o);
  }
  if (t == scheme_rational_type) {
    if (scheme_is_rational_positive(o))
      return o;
    else
      return scheme_rational_negate(o);
  }
  if (t == scheme_complex_izi_type) {
    Scheme_Object *r = IZI_REAL_PART(o);
    return abs_prim(1, &r);
  }

  NEED_REAL(abs);

  return scheme_void;
}

static Scheme_Object *to_bignum(const Scheme_Object *o)
{
  if (SCHEME_INTP(o))
    return scheme_make_bignum(SCHEME_INT_VAL(o));
  else
    return (Scheme_Object *)o;
}

Scheme_Object *
scheme_bin_quotient (const Scheme_Object *n1, const Scheme_Object *n2)
{
  Scheme_Object *q;

  if (!is_integer(n1)) {
    Scheme_Object *a[2];
    a[0] = (Scheme_Object *)n1;
    a[1] = (Scheme_Object *)n2;
    scheme_wrong_type("quotient", "integer", 0, 2, a);
  }
  if (!is_integer(n2)) {
    Scheme_Object *a[2];
    a[0] = (Scheme_Object *)n1;
    a[1] = (Scheme_Object *)n2;
    scheme_wrong_type("quotient", "integer", 1, 2, a);
  }

  if (SCHEME_COMPLEX_IZIP(n1)) n1 = IZI_REAL_PART(n1);
  if (SCHEME_COMPLEX_IZIP(n2)) n2 = IZI_REAL_PART(n2);

  if (SCHEME_INTP(n2) && !SCHEME_INT_VAL(n2))
    scheme_raise_exn(MZEXN_APPLICATION_DIVIDE_BY_ZERO, n2,
		     "quotient: undefined for 0");
  if (
#ifdef MZ_USE_SINGLE_FLOATS
      (SCHEME_FLTP(n2) && (SCHEME_FLT_VAL(n2) == 0.0f)) ||
#endif
      (SCHEME_DBLP(n2) && (SCHEME_DBL_VAL(n2) == 0.0)))
    scheme_raise_exn(MZEXN_APPLICATION_DIVIDE_BY_ZERO, n2,
		     "quotient: undefined for 0.0");

  if (SCHEME_INTP(n1) && SCHEME_INTP(n2)) {
    return (scheme_make_integer (SCHEME_INT_VAL(n1) / SCHEME_INT_VAL(n2)));
  }
  if (SCHEME_DBLP(n1) || SCHEME_DBLP(n2)) {
    Scheme_Object *r;
    double d, d2;

    r = scheme_bin_div(n1, n2);
    d = SCHEME_DBL_VAL(r);

    if (d > 0)
      d2 = floor(d);
    else
      d2 = ceil(d);

    if (d2 == d)
      return r;
    else
      return scheme_make_double(d2);
  }
#ifdef MZ_USE_SINGLE_FLOATS
  if (SCHEME_FLTP(n1) || SCHEME_FLTP(n2)) {
    Scheme_Object *r = scheme_bin_div(n1, n2);
    float d = SCHEME_FLT_VAL(r), d2;

    if (d > 0)
      d2 = floor(d);
    else
      d2 = ceil(d);

    if (d2 == d)
      return r;
    else
      return scheme_make_float(d2);
  }
#endif

  if (SCHEME_RATIONALP(n1))
    WRONG_TYPE("quotient", "integer", n1);
  if (SCHEME_RATIONALP(n2))
    WRONG_TYPE("quotient", "integer", n2);
  
  n1 = to_bignum(n1);
  n2 = to_bignum(n2);

  scheme_bignum_divide(n1, n2, &q, NULL, 1);
  return q;
}

static Scheme_Object *
quotient (int argc, Scheme_Object *argv[])
{
  return scheme_bin_quotient(argv[0], argv[1]);
}

static Scheme_Object *
rem_mod (int argc, Scheme_Object *argv[], char *name, int first_sign)
{
  Scheme_Object *n1, *n2, *r;
  int negate;

  n1 = argv[0];
  n2 = argv[1];

  if (!is_integer(n1))
    scheme_wrong_type(name, "integer", 0, argc, argv);
  if (!is_integer(n2))
    scheme_wrong_type(name, "integer", 1, argc, argv);

  if (SCHEME_COMPLEX_IZIP(n1)) n1 = IZI_REAL_PART(n1);
  if (SCHEME_COMPLEX_IZIP(n2)) n2 = IZI_REAL_PART(n2);

  if (SCHEME_INTP(n2) && !SCHEME_INT_VAL(n2))
    scheme_raise_exn(MZEXN_APPLICATION_DIVIDE_BY_ZERO, n2,
		     "%s: undefined for 0", name);
  if (
#ifdef MZ_USE_SINGLE_FLOATS
      (SCHEME_FLTP(n2) && (SCHEME_FLT_VAL(n2) == 0.0f)) ||
#endif
      (SCHEME_DBLP(n2) && (SCHEME_DBL_VAL(n2) == 0.0))) {
    int neg;
    neg = minus_zero_p(SCHEME_FLOAT_VAL(n2));
    scheme_raise_exn(MZEXN_APPLICATION_DIVIDE_BY_ZERO, n2,
		     "%s: undefined for %s0.0",
		     name,
		     neg ? "-" : "");
  }

  if (SCHEME_INTP(n1) && SCHEME_INTP(n2)) {
    long a, b, na, nb, v;
    int neg1, neg2;

    a = SCHEME_INT_VAL(n1);
    b = SCHEME_INT_VAL(n2);
    na =  (a < 0) ? -a : a;
    nb =  (b < 0) ? -b : b;

    v = na % nb;

    if (v) {
      if (first_sign) {
	if (a < 0)
	  v = -v;
      } else {
	neg1 = (a < 0);
	neg2 = (b < 0);
	
	if (neg1 != neg2)
	  v = nb - v;
	
	if (neg2)
	  v = -v;
      }
    }

    return scheme_make_integer(v);
  }

  if (SCHEME_FLOATP(n1) || SCHEME_FLOATP(n2)) {
    double a, b, na, nb, v;
#ifdef MZ_USE_SINGLE_FLOATS
    int was_single = !(SCHEME_DBLP(n1) || SCHEME_DBLP(n2));
#endif

    if (SCHEME_INTP(n1))
      a = SCHEME_INT_VAL(n1);
#ifdef MZ_USE_SINGLE_FLOATS
    else if (SCHEME_FLTP(n1))
      a = SCHEME_FLT_VAL(n1);
#endif
    else if (SCHEME_DBLP(n1))
      a = SCHEME_DBL_VAL(n1);
    else
      a = scheme_bignum_to_double(n1);

    if (SCHEME_INTP(n2))
      b = SCHEME_INT_VAL(n2);
#ifdef MZ_USE_SINGLE_FLOATS
    else if (SCHEME_FLTP(n2))
      b = SCHEME_FLT_VAL(n2);
#endif
    else if (SCHEME_DBLP(n2))
      b = SCHEME_DBL_VAL(n2);
    else
      b = scheme_bignum_to_double(n2);
    na =  (a < 0) ? -a : a;
    nb =  (b < 0) ? -b : b;

    if (MZ_IS_POS_INFINITY(nb))
      v = na;
    else if (MZ_IS_POS_INFINITY(na))
      return zerod;
    else
      v = fmod(na, nb);

    if (v) {
      if (first_sign) {
	if (a < 0)
	  v = -v;
      } else {
	int neg1, neg2;
	
	neg1 = (a < 0);
	neg2 = (b < 0);
	
	if (neg1 != neg2)
	  v = nb - v;
	
	if (neg2)
	  v = -v;
      }
    }

#ifdef MZ_USE_SINGLE_FLOATS
    if (was_single)
      return scheme_make_float((float)v);
#endif

    return scheme_make_double(v);
  }

  n1 = to_bignum(n1);
  n2 = to_bignum(n2);

  scheme_bignum_divide(n1, n2, NULL, &r, 1);

  negate = 0;

  if (!SCHEME_INTP(r) || SCHEME_INT_VAL(r)) {
    /* Easier if we can assume 'r' is positive: */
    if (SCHEME_INTP(r)) {
      if (SCHEME_INT_VAL(r) < 0)
	r = scheme_make_integer(-SCHEME_INT_VAL(r));
    } else if (!SCHEME_BIGPOS(r))
      r = scheme_bignum_negate(r);

    if (first_sign) {
      if (!SCHEME_BIGPOS(n1))
	negate = 1;
    } else {
      int neg1, neg2;
      
      neg1 = !SCHEME_BIGPOS(n1);
      neg2 = !SCHEME_BIGPOS(n2);
      
      if (neg1 != neg2) {
	if (neg2)
	  r = scheme_bin_plus(n2, r);
	else
	  r = scheme_bin_minus(n2, r);
      } else if (neg2)
	negate = 1;
    }
    
    if (negate) {
      if (SCHEME_INTP(r))
	r = scheme_make_integer(-SCHEME_INT_VAL(r));
      else
	r = scheme_bignum_negate(r);
    }
  }

  return r;
}

static Scheme_Object *
rem_prim (int argc, Scheme_Object *argv[])
{
  return rem_mod(argc, argv, "remainder", 1);
}

Scheme_Object *
scheme_modulo(int argc, Scheme_Object *argv[])
{
  return rem_mod(argc, argv, "modulo", 0);
}

static Scheme_Object *bin_lcm (Scheme_Object *n1, Scheme_Object *n2);

GEN_NARY_OP(gcd, "gcd", scheme_bin_gcd, 0, is_integer, "integer")
GEN_NARY_OP(lcm, "lcm", bin_lcm, 1, is_integer, "integer")

Scheme_Object *
scheme_bin_gcd (const Scheme_Object *n1, const Scheme_Object *n2)
{
  if (SCHEME_COMPLEX_IZIP(n1)) n1 = IZI_REAL_PART(n1);
  if (SCHEME_COMPLEX_IZIP(n2)) n2 = IZI_REAL_PART(n2);

  if (SCHEME_INTP(n1) && SCHEME_INTP(n2)) {
    long i1, i2, a, b, r;

    i1 = SCHEME_INT_VAL(n1);
    i2 = SCHEME_INT_VAL(n2);
    if (i1 < 0)
      i1 = -i1;
    if (i2 < 0)
      i2 = -i2;
    if (i1 > i2) {
      a = i1;
      b = i2;
    } else {
      a = i2;
      b = i1;
    }
    
    r = 1;
    while ((b > 0) && (r > 0)) {
      r = a % b;
      a = b;
      b = r;
    }
    return (scheme_make_integer(a));
  } else if (SCHEME_FLOATP(n1) || SCHEME_FLOATP(n2)) {
    double i1, i2, a, b, r;
#ifdef MZ_USE_SINGLE_FLOATS
    int was_single = !(SCHEME_DBLP(n1) || SCHEME_DBLP(n2));
#endif

    if (SCHEME_INTP(n1))
      i1 = SCHEME_INT_VAL(n1);
    else if (SCHEME_FLOATP(n1))
      i1 = SCHEME_FLOAT_VAL(n1);
    else
      i1 = scheme_bignum_to_double(n1);

    if (SCHEME_INTP(n2))
      i2 = SCHEME_INT_VAL(n2);
    else if (SCHEME_FLOATP(n2))
      i2 = SCHEME_FLOAT_VAL(n2);
    else
      i2 = scheme_bignum_to_double(n2);

    if (i1 < 0)
      i1 = -1;
    if (i2 < 0)
      i2 = -i2;
    if (i1 > i2) {
      a = i1;
      b = i2;
    } else {
      a = i2;
      b = i1;
    }

    if (MZ_IS_NAN(a) || MZ_IS_NAN(b))
      return nan_object;
    if (MZ_IS_POS_INFINITY(a))
      return scheme_make_double(b);
    
    r = 1;
    while ((b > 0) && (r > 0)) {
      r = fmod(a, b);
      a = b;
      b = r;
    }

#ifdef MZ_USE_SINGLE_FLOATS
    if (was_single)
      return scheme_make_float((float)a);
#endif

    return scheme_make_double(a);
  } else {
    Scheme_Object *r;

    n1 = to_bignum(n1);
    n2 = to_bignum(n2);

    if (!SCHEME_BIGPOS(n1))
      n1 = scheme_bignum_negate(n1);
    if (!SCHEME_BIGPOS(n2))
      n2 = scheme_bignum_negate(n2);

    if (scheme_bignum_lt(n1, n2)) {
      const Scheme_Object *save;
      save = n1;
      n1 = n2;
      n2 = save;
    }
    
    r = scheme_make_bignum(1);
    while (SCHEME_BIGLEN(n2) && SCHEME_BIGLEN(r)) {
      scheme_bignum_divide(n1, n2, NULL, &r, 0);
      n1 = n2;
      n2 = r;
    }

    return scheme_bignum_normalize(n1);
  }
}

static Scheme_Object *
bin_lcm (Scheme_Object *n1, Scheme_Object *n2)
{
  Scheme_Object *d, *ret;

  d = scheme_bin_gcd(n1, n2);
  
  if (NOT_SAME_OBJ(zero_p(1, &d), scheme_false))
    return d;

  ret = scheme_bin_mult(n1, scheme_bin_quotient(n2, d));

  return abs_prim(1, &ret);
}

static Scheme_Object *
floor_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return o;
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type)
    return scheme_make_float(floor(SCHEME_FLT_VAL(o)));
#endif
  if (t == scheme_double_type)
    return scheme_make_double(floor(SCHEME_DBL_VAL(o)));
  if (t == scheme_bignum_type)
    return o;
  if (t == scheme_rational_type)
    return scheme_rational_floor(o);
  if (t == scheme_complex_izi_type) {
    Scheme_Object *r = IZI_REAL_PART(o);
    return floor_prim(1, &r);
  }

  NEED_REAL(floor);

  return scheme_void;
}

static Scheme_Object *
ceiling (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return o;
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type)
    return scheme_make_float(ceil(SCHEME_FLT_VAL(o)));
#endif
  if (t == scheme_double_type)
    return scheme_make_double(ceil(SCHEME_DBL_VAL(o)));
  if (t == scheme_bignum_type)
    return o;
  if (t == scheme_rational_type)
    return scheme_rational_ceiling(o);
  if (t == scheme_complex_izi_type) {
    Scheme_Object *r = IZI_REAL_PART(o);
    return ceiling(1, &r);
  }

  NEED_REAL(ceiling);

  return scheme_void;
}

static Scheme_Object *
sch_truncate (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return o;
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type) {
    float v = SCHEME_FLT_VAL(o);
    if (v > 0)
      v = floor(v);
    else
      v = ceil(v);
    return scheme_make_float(v);
  }
#endif
  if (t == scheme_double_type) {
    double v = SCHEME_DBL_VAL(o);
    if (v > 0)
      v = floor(v);
    else
      v = ceil(v);
    return scheme_make_double(v);
  }
  if (t == scheme_bignum_type)
    return o;
  if (t == scheme_rational_type)
    return scheme_rational_truncate(o);
  if (t == scheme_complex_izi_type) {
    Scheme_Object *r = IZI_REAL_PART(o);
    return sch_truncate(1, &r);
  }

  NEED_REAL(truncate);

  return scheme_void;
}

static Scheme_Object *
sch_round (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return o;
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type) {
    double d = SCHEME_FLT_VAL(o);
    double i, frac;
    int invert;

    if (d < 0) {
      d = -d;
      invert = 1;
    } else
      invert = 0;

    frac = modf(d, &i);
    if (frac < 0.5)
      d = i;
    else if (frac > 0.5)
      d = i + 1;
    else if (fmod(i, 2.0))
	d = i + 1;
    else
      d = i;

    if (invert)
      d = -d;

    return scheme_make_float((float)d);
  }
#endif
  if (t == scheme_double_type) {
    double d = SCHEME_DBL_VAL(o);
    double i, frac;
    int invert;

    if (d < 0) {
      d = -d;
      invert = 1;
    } else
      invert = 0;

    frac = modf(d, &i);
    if (frac < 0.5)
      d = i;
    else if (frac > 0.5)
      d = i + 1;
    else if (fmod(i, 2.0))
	d = i + 1;
    else
      d = i;

    if (invert)
      d = -d;

    return scheme_make_double(d);
  }
  if (t == scheme_bignum_type)
    return o;
  if (t == scheme_rational_type)
    return scheme_rational_round(o);
  if (t == scheme_complex_izi_type) {
    Scheme_Object *r = IZI_REAL_PART(o);
    return sch_round(1, &r);
  }

  NEED_REAL(round);

  return scheme_void;
}

#ifdef MZ_USE_SINGLE_FLOATS
static float TO_FLOAT_VAL(const Scheme_Object *n)
{
  Scheme_Type t;

  if (SCHEME_INTP(n))
    return (float)SCHEME_INT_VAL(n);
  t = _SCHEME_TYPE(n);
  if (t == scheme_float_type)
    return SCHEME_FLT_VAL(n);
  if (t == scheme_double_type)
    return SCHEME_DBL_VAL(n);
  if (t == scheme_bignum_type)
    return scheme_bignum_to_float(n);
  if (t == scheme_rational_type)
    return scheme_rational_to_float(n);
  if (t == scheme_complex_izi_type)
    return TO_FLOAT_VAL(IZI_REAL_PART(n));
  return 0.0f;
}

static Scheme_Object *TO_FLOAT(const Scheme_Object *n)
{
  if (SCHEME_FLTP(n))
    return (Scheme_Object *)n;
  else
    return scheme_make_float(TO_FLOAT_VAL(n));
}
#endif

#define TO_DOUBLE_VAL scheme_get_val_as_double

#ifdef USE_SINGLE_FLOATS_AS_DEFAULT

double TO_DOUBLE_VAL(const Scheme_Object *n)
{
  Scheme_Type t;

  if (SCHEME_INTP(n))
    return (double)SCHEME_INT_VAL(n);
  t = _SCHEME_TYPE(n);
  if (t == scheme_float_type)
    return SCHEME_FLT_VAL(n);
  if (t == scheme_double_type)
    return SCHEME_DBL_VAL(n);
  if (t == scheme_bignum_type)
    return scheme_bignum_to_double(n);
  if (t == scheme_rational_type)
    return scheme_rational_to_double(n);
  if (t == scheme_complex_izi_type)
    return TO_DOUBLE_VAL(IZI_REAL_PART(n));
}

static Scheme_Object *TO_DOUBLE(const Scheme_Object *n)
{
  if (SCHEME_DBLP(n))
    return n;
  else
    return scheme_make_double(TO_DOUBLE_VAL(n));
}

#else

static Scheme_Object *TO_DOUBLE(const Scheme_Object *n)
{
  return exact_to_inexact(1, (Scheme_Object **)&n);
}
double TO_DOUBLE_VAL(const Scheme_Object *n)
{
  return SCHEME_DBL_VAL(TO_DOUBLE(n));
}

#endif

static Scheme_Object *get_frac(char *name, int low_p, 
			       int argc, Scheme_Object *argv[])
{
  Scheme_Object *n = argv[0], *orig;

  if (SCHEME_COMPLEX_IZIP(n)) n = IZI_REAL_PART(n);

  orig = n;

  if (SCHEME_FLOATP(n)) {
    double d = SCHEME_FLOAT_VAL(n);
    
    if (MZ_IS_NAN(d))
      return n;
    else if (MZ_IS_POS_INFINITY(d))
      return low_p ? scheme_make_double(1.0) : n;
    else if (MZ_IS_NEG_INFINITY(d))
      return low_p ? scheme_make_double(1.0) : n;

#ifdef MZ_USE_SINGLE_FLOATS
    if (SCHEME_FLTP(n))
      n = scheme_rational_from_float((float)d);
    else
#endif
      n = scheme_rational_from_double(d);
  }
  
  if (SCHEME_INTP(n) || SCHEME_BIGNUMP(n))
    n = low_p ? scheme_make_integer(1) : n;
  else if (SCHEME_RATIONALP(n)) {
    if (low_p)
      n = scheme_rational_denominator(n);
    else
      n = scheme_rational_numerator(n);
  } else {
    scheme_wrong_type(name, REAL_NUMBER_STR, 0, argc, argv);
    return scheme_void;   
  }
  
  if (SCHEME_DBLP(orig))
    return TO_DOUBLE(n);
#ifdef MZ_USE_SINGLE_FLOATS
  if (SCHEME_FLTP(orig))
    return TO_FLOAT(n);
#endif
  else
    return n;
}

static Scheme_Object *un_exp(Scheme_Object *o)
{
  return exp_prim(1, &o);
}

static Scheme_Object *un_log(Scheme_Object *o)
{
  return log_prim(1, &o);
}

static Scheme_Object *numerator(int argc, Scheme_Object *argv[])
{
  return get_frac("numerator", 0, argc, argv);
}

static Scheme_Object *denominator(int argc, Scheme_Object *argv[])
{
  return get_frac("denominator", 1, argc, argv);
}

static Scheme_Object *complex_exp(Scheme_Object *c)
{
  Scheme_Object *r = _scheme_complex_real_part(c);
  Scheme_Object *i = _scheme_complex_imaginary_part(c);
  Scheme_Object *cos_a, *sin_a;

  r = exp_prim(1, &r);
  cos_a = cos_prim(1, &i);
  sin_a = sin_prim(1, &i);

  return scheme_bin_mult(r, scheme_bin_plus(cos_a, scheme_bin_mult(sin_a, plus_i)));
}

static Scheme_Object *complex_log(Scheme_Object *c)
{
  Scheme_Object *m, *theta;

  m = magnitude(1, &c);
  theta = angle(1, &c);

  return scheme_bin_plus(log_prim(1, &m), scheme_bin_mult(plus_i, theta));
}

static Scheme_Object *complex_sin(Scheme_Object *c)
{
  Scheme_Object *i_c;

  i_c = scheme_bin_mult(c, plus_i);
  
  return scheme_bin_div(scheme_bin_minus(un_exp(i_c),
					 un_exp(scheme_bin_minus(zeroi, i_c))),
			scheme_bin_mult(scheme_make_integer(2), plus_i));
}

static Scheme_Object *complex_cos(Scheme_Object *c)
{
  Scheme_Object *i_c;

  i_c = scheme_bin_mult(c, plus_i);
  
  return scheme_bin_div(scheme_bin_plus(un_exp(i_c),
					un_exp(scheme_bin_minus(zeroi, i_c))),
			scheme_make_integer(2));
}

static Scheme_Object *complex_tan(Scheme_Object *c)
{
  return scheme_bin_div(complex_sin(c), complex_cos(c));
}

static Scheme_Object *complex_asin(Scheme_Object *c)
{
  Scheme_Object *one_minus_c_sq, *sqrt_1_minus_c_sq;

  one_minus_c_sq = scheme_bin_minus(scheme_make_integer(1),
				    scheme_bin_mult(c, c));
  sqrt_1_minus_c_sq = scheme_sqrt(1, &one_minus_c_sq);

  return scheme_bin_mult(minus_i,
			 un_log(scheme_bin_plus(scheme_bin_mult(c, plus_i), 
						sqrt_1_minus_c_sq)));
}

static Scheme_Object *complex_acos(Scheme_Object *c)
{
  Scheme_Object *one_minus_c_sq, *sqrt_1_minus_c_sq;

  one_minus_c_sq = scheme_bin_minus(scheme_make_integer(1),
				    scheme_bin_mult(c, c));
  sqrt_1_minus_c_sq = scheme_sqrt(1, &one_minus_c_sq);

  return scheme_bin_mult(minus_i,
			 un_log(scheme_bin_plus(c,
						scheme_bin_mult(plus_i,
								sqrt_1_minus_c_sq))));
}

static Scheme_Object *complex_atan(Scheme_Object *c)
{
  if (scheme_complex_eq(c, plus_i) || scheme_complex_eq(c, minus_i))
    return minus_inf_object;

  return scheme_bin_mult(plus_i,
			 scheme_bin_mult(scheme_make_double(0.5),
					 un_log(scheme_bin_div(scheme_bin_plus(plus_i, c),
							       scheme_bin_plus(plus_i, 
									       scheme_bin_minus(zeroi, c))))));
}

#define GEN_ZERO_IS_ZERO() if (o == zeroi) return zeroi;
#define GEN_ZERO_IS_ONE() if (o == zeroi) return scheme_make_integer(1);
#define GEN_ONE_IS_ZERO() if (o == scheme_make_integer(1)) return zeroi;
#define GEN_ONE_IS_ZERO_AND_ZERO_IS_ERR() if (o == scheme_make_integer(1)) return zeroi; else if (o == zeroi) scheme_raise_exn(MZEXN_APPLICATION_DIVIDE_BY_ZERO, zeroi, "log: undefined for 0");
#define GEN_ZERO_IS_HALF_PI() if (o == zeroi) return scheme_half_pi;

#define NEVER_RESORT_TO_COMPLEX(d) 0
#define NEGATIVE_USES_COMPLEX(d) d < 0.0
#define OVER_ONE_MAG_USES_COMPLEX(d) (d > 1.0) || (d < -1.0)

GEN_UNARY_OP(exp_prim, exp, exp, inf_object, zerod, nan_object, complex_exp, GEN_ZERO_IS_ONE, NEVER_RESORT_TO_COMPLEX)
GEN_UNARY_OP(log_prim, log, log, inf_object, nan_object, nan_object, complex_log, GEN_ONE_IS_ZERO_AND_ZERO_IS_ERR, NEGATIVE_USES_COMPLEX)
GEN_UNARY_OP(sin_prim, sin, sin, nan_object, nan_object, nan_object, complex_sin, GEN_ZERO_IS_ZERO, NEVER_RESORT_TO_COMPLEX)
GEN_UNARY_OP(cos_prim, cos, cos, nan_object, nan_object, nan_object, complex_cos, GEN_ZERO_IS_ONE, NEVER_RESORT_TO_COMPLEX)
GEN_UNARY_OP(tan_prim, tan, tan, nan_object, nan_object, nan_object, complex_tan, GEN_ZERO_IS_ZERO, NEVER_RESORT_TO_COMPLEX)
GEN_UNARY_OP(asin_prim, asin, asin, nan_object, nan_object, nan_object, complex_asin, GEN_ZERO_IS_ZERO, OVER_ONE_MAG_USES_COMPLEX)
GEN_UNARY_OP(acos_prim, acos, acos, nan_object, nan_object, nan_object, complex_acos, GEN_ONE_IS_ZERO, OVER_ONE_MAG_USES_COMPLEX)

static Scheme_Object *
atan_prim (int argc, Scheme_Object *argv[])
{
  double v;
  Scheme_Object *n1;
#ifdef MZ_USE_SINGLE_FLOATS
  int single = 0;
#endif

  n1 = argv[0];

  if (SCHEME_COMPLEX_IZIP(n1)) n1 = IZI_REAL_PART(n1);

  if (SCHEME_INTP(n1))
    v = SCHEME_INT_VAL(n1);
#ifdef MZ_USE_SINGLE_FLOATS
  else if (SCHEME_FLTP(n1)) {
    v = SCHEME_FLT_VAL(n1);
    single++;
  }
#endif
  else if (SCHEME_DBLP(n1))
    v = SCHEME_DBL_VAL(n1);
  else if (SCHEME_BIGNUMP(n1))
    v = scheme_bignum_to_double(n1);
  else if (SCHEME_RATIONALP(n1))
    v = scheme_rational_to_double(n1);
  else if (SCHEME_COMPLEXP(n1)) {
    if (argc > 1) {
      scheme_wrong_type("atan (with two arguments)", REAL_NUMBER_STR, 0, argc, argv);
      return NULL;
    } else
      return complex_atan(n1);
  } else {
    NEED_NUMBER(atan);
    v = 0.0;
  }

  if (argc == 2) {
    double v2;
    Scheme_Object *n2;
    
    n2 = argv[1];

    if (SCHEME_COMPLEX_IZIP(n2)) n2 = IZI_REAL_PART(n2);

    if (SCHEME_INTP(n2))
      v2 = SCHEME_INT_VAL(n2);
#ifdef MZ_USE_SINGLE_FLOATS
    else if (SCHEME_FLTP(n2)) {
      v2 = SCHEME_FLT_VAL(n2);
      single++;
    }
#endif
    else if (SCHEME_DBLP(n2))
      v2 = SCHEME_DBL_VAL(n2);
    else if (SCHEME_BIGNUMP(n2))
      v2 = scheme_bignum_to_double(n2);
    else if (SCHEME_RATIONALP(n2))
      v2 = scheme_rational_to_double(n2);
    else {
      scheme_wrong_type("atan", REAL_NUMBER_STR, 1, argc, argv);
      return NULL;
    }

    if ((v == 0.0) && (v2 == 0.0)) {
#ifdef MZ_USE_SINGLE_FLOATS
      if (single == 2)
	return zerof;
#endif      
      return zerod;
    }

#ifdef ATAN2_DOESNT_WORK_WITH_INFINITIES
	if ((MZ_IS_POS_INFINITY(v) || MZ_IS_NEG_INFINITY(v))
		&& (MZ_IS_POS_INFINITY(v2) || MZ_IS_NEG_INFINITY(v2))) {
	  v = MZ_IS_POS_INFINITY(v) ? 1.0 : -1.0;
	  v2 = MZ_IS_POS_INFINITY(v2) ? 1.0 : -1.0;
	}
#endif

    v = atan2(v, v2);
  } else {
    if (argv[0] == zeroi)
      return zeroi;

    v = atan(v);
#ifdef MZ_USE_SINGLE_FLOATS
    single++;
#endif    
  }

#ifdef MZ_USE_SINGLE_FLOATS
  if (single == 2)
    return scheme_make_float((float)v);
#endif

  return scheme_make_double(v);
}

Scheme_Object *scheme_sqrt (int argc, Scheme_Object *argv[])
{
  int imaginary = 0;
  Scheme_Object *n;
  
  n = argv[0];

  /* Special case for x+0.0i: */
  if (SCHEME_COMPLEX_IZIP(n)) {
    Scheme_Object *r = IZI_REAL_PART(n), *v;
    v = scheme_sqrt(1, &r);
    if (!SCHEME_COMPLEXP(v))
      return scheme_make_complex(v, scheme_complex_imaginary_part(n));
    else
      return v;
  }

  if (SCHEME_COMPLEXP(n))
    return scheme_complex_sqrt(n);

  if (!SCHEME_REALP(n))
    scheme_wrong_type("sqrt", "number", 0, argc, argv);

  if (SCHEME_TRUEP(negative_p(1, &n))) {
    n = scheme_bin_minus(zeroi, n);
    imaginary = 1;
  }

  if (SCHEME_INTP(n) || SCHEME_BIGNUMP(n))
    n = scheme_integer_sqrt(n);
#ifdef MZ_USE_SINGLE_FLOATS
  else if (SCHEME_FLTP(n))
    n = scheme_make_float((float)sqrt(SCHEME_FLT_VAL(n)));
#endif
  else if (SCHEME_DBLP(n)) {
    double d = SCHEME_DBL_VAL(n);
#ifdef SQRT_NAN_IS_WRONG
    if (MZ_IS_NAN(d))
      return nan_object;
#endif
    n = scheme_make_double(sqrt(d));
  } else if (SCHEME_RATIONALP(n))
    n = scheme_rational_sqrt(n);

  if (imaginary)
    return scheme_make_complex(zeroi, n);
  else
    return n;
}

static Scheme_Object *fixnum_expt(int x, int y)
{
  if ((x == 2) && (y <= MAX_SHIFT_TRY))
    return scheme_make_integer(1 << y);
  else
    return scheme_bignum_power(scheme_make_bignum(x), scheme_make_bignum(y));
}

#ifdef POW_HANDLES_INF_CORRECTLY
# define sch_pow pow
#else
static double sch_pow(double x, double y)
{
  if (MZ_IS_POS_INFINITY(y)) {
    return scheme_infinity_val;
  } else if (MZ_IS_NEG_INFINITY(y)) {
    return 0.0;
  } else if (MZ_IS_POS_INFINITY(x)) {
    if (y == 0.0)
      return 1.0;
    else
      return scheme_infinity_val;
  } else if (MZ_IS_NEG_INFINITY(x)) {
    if (y == 0.0)
      return 1.0;
    else {
      if (fmod(y, 2.0) == 1.0)
	return scheme_minus_infinity_val;
      else
	return scheme_infinity_val;
    }
  } else
    return pow(x, y);
}
#endif

# define F_EXPT(x, y) (((x < 0.0) && (y != floor(y))) \
                       ? scheme_complex_power(scheme_real_to_complex(scheme_make_double(x)), \
				              scheme_real_to_complex(scheme_make_double(y))) \
                       : scheme_make_double(sch_pow((double)x, (double)y)))
# define FS_EXPT(x, y) (((x < 0.0) && (y != floor(y))) \
                       ? scheme_complex_power(scheme_real_to_complex(scheme_make_float(x)), \
				              scheme_real_to_complex(scheme_make_float(y))) \
                        : scheme_make_float(sch_pow((double)x, (double)y)))

static GEN_BIN_OP(bin_expt, "expt", fixnum_expt, F_EXPT, FS_EXPT, scheme_bignum_power, scheme_rational_power, scheme_complex_power, GEN_RETURN_0_USUALLY, GEN_RETURN_1, NAN_RETURNS_NAN, NAN_RETURNS_SNAN)

Scheme_Object *
scheme_expt(int argc, Scheme_Object *argv[])
{
  int invert = 0;
  Scheme_Object *e = argv[1], *r, *n;

  n = argv[0];

  if (!SCHEME_NUMBERP(n))
    scheme_wrong_type("expt", "number", 0, argc, argv);

  if (argv[1] == scheme_make_integer(1))
    return n;

  if (!SCHEME_FLOATP(n)) {
    if (SCHEME_INTP(e) || SCHEME_BIGNUMP(e)) {
      if (SCHEME_FALSEP(positive_p(1, &e))) {
	e = scheme_bin_minus(zeroi, e);
	invert = 1;
      }
    }
  }

  r = bin_expt(argv[0], e);
  if (invert)
    r = scheme_bin_div(scheme_make_integer(1), r);

  return r;
}


static Scheme_Object *make_rectangular (int argc, Scheme_Object *argv[])
{
  Scheme_Object *a, *b;
  int af, bf;

  a = argv[0];
  b = argv[1];
  if (!SCHEME_REALP(a))
    scheme_wrong_type("make-rectangular", REAL_NUMBER_STR, 0, argc, argv);
  if (!SCHEME_REALP(b))
    scheme_wrong_type("make-rectangular", REAL_NUMBER_STR, 1, argc, argv);

  if (SCHEME_COMPLEX_IZIP(a)) a = IZI_REAL_PART(a);
  if (SCHEME_COMPLEX_IZIP(b)) b = IZI_REAL_PART(b);

  af = SCHEME_FLOATP(a);
  bf = SCHEME_FLOATP(b);

  if (af && !bf) {
    if (b != zeroi)
      b = exact_to_inexact(1, &b);
  }
  if (bf && !af) {
    if (a != zeroi)
      a = exact_to_inexact(1, &a);
  }

  return scheme_make_complex(a, b);
}

static Scheme_Object *make_polar (int argc, Scheme_Object *argv[])
{
  Scheme_Object *a, *b, *r, *i, *v;

  a = argv[0];
  b = argv[1];
  if (!SCHEME_REALP(a))
    scheme_wrong_type("make-polar", REAL_NUMBER_STR, 0, argc, argv);
  if (!SCHEME_REALP(b))
    scheme_wrong_type("make-polar", REAL_NUMBER_STR, 1, argc, argv);

  if (b == zeroi)
    return a;

  if (SCHEME_COMPLEX_IZIP(a)) a = IZI_REAL_PART(a);
  if (SCHEME_COMPLEX_IZIP(b)) b = IZI_REAL_PART(b);

  v = b;

  r = scheme_bin_mult(a, cos_prim(1, &v));
  i = scheme_bin_mult(a, sin_prim(1, &v));

  return scheme_make_complex(r, i);
}

static Scheme_Object *real_part (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (!SCHEME_NUMBERP(o))
    scheme_wrong_type("real-part", "number", 0, argc, argv);

  if (SCHEME_COMPLEXP(o))
    return _scheme_complex_real_part(o);
  else
    return argv[0];
}

static Scheme_Object *imag_part (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (!SCHEME_NUMBERP(o))
    scheme_wrong_type("imag-part", "number", 0, argc, argv);

  if (SCHEME_COMPLEXP(o))
    return scheme_complex_imaginary_part(o);

  return zeroi;
}

static Scheme_Object *magnitude(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (!SCHEME_NUMBERP(o))
    scheme_wrong_type("magnitude", "number", 0, argc, argv);

  if (SCHEME_COMPLEXP(o)) {
    Scheme_Object *r = _scheme_complex_real_part(o);
    Scheme_Object *i = _scheme_complex_imaginary_part(o);
    Scheme_Object *m2;

    m2 = scheme_bin_plus(scheme_bin_mult(r, r),
			 scheme_bin_mult(i, i));
    
    return scheme_sqrt(1, &m2);
  } else
    return abs_prim(1, argv);
}

static Scheme_Object *angle (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (!SCHEME_NUMBERP(o))
    scheme_wrong_type("angle", "number", 0, argc, argv);

  if (SCHEME_COMPLEXP(o)) {
    Scheme_Object *r = (Scheme_Object *)_scheme_complex_real_part(o);
    Scheme_Object *i = (Scheme_Object *)_scheme_complex_imaginary_part(o);
    double rd, id;
    id = TO_DOUBLE_VAL(i);
    rd = TO_DOUBLE_VAL(r);

    return scheme_make_double(atan2(id, rd));
  } else {
#ifdef MZ_USE_SINGLE_FLOATS
    if (SCHEME_FLTP(o)) {
      float v = SCHEME_FLT_VAL(o);
      if (v == 0.0f)
	scheme_raise_exn(MZEXN_APPLICATION_DIVIDE_BY_ZERO, o,
			 "angle: undefined for %s0.0",
			 minus_zero_p(v) ? "-" : "");
      if (v > 0)
	return zeroi;
      else
	return single_scheme_pi;
    }
#endif
    if (SCHEME_DBLP(o)) {
      double v = SCHEME_DBL_VAL(o);
      if (v == 0.0) {
	int neg;
	neg = minus_zero_p(v);
	scheme_raise_exn(MZEXN_APPLICATION_DIVIDE_BY_ZERO, o,
			 "angle: undefined for %s0.0",
			 neg ? "-" : "");
      }
      if (v > 0)
	return zeroi;
      else
	return scheme_pi;
    } else if (o == zeroi) {
      scheme_raise_exn(MZEXN_APPLICATION_DIVIDE_BY_ZERO, o,
		       "angle: undefined for 0");
      return scheme_void;
    } else if (SCHEME_TRUEP(positive_p(1, argv)))
      return zeroi;
    else
      return scheme_pi;
  }
}

static Scheme_Object *
exact_to_inexact (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return scheme_make_double(SCHEME_INT_VAL(o));
  t = _SCHEME_TYPE(o);
#ifdef MZ_USE_SINGLE_FLOATS
  if (t == scheme_float_type)
    return o;
#endif
  if (t == scheme_double_type)
    return o;
  if (t == scheme_bignum_type) {
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
    return scheme_make_float(scheme_bignum_to_float(o));
#else
    return scheme_make_double(scheme_bignum_to_double(o));
#endif
  }
  if (t == scheme_rational_type) {
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
    return scheme_make_float(scheme_rational_to_float(o));
#else
    return scheme_make_double(scheme_rational_to_double(o));
#endif
  }
  if ((t == scheme_complex_type) || (t == scheme_complex_izi_type)) {
    Scheme_Object *realpart, *imaginarypart;

    realpart = _scheme_complex_real_part(o);
    imaginarypart = _scheme_complex_imaginary_part(o);

    realpart = exact_to_inexact(1, &realpart);
    imaginarypart = exact_to_inexact(1, &imaginarypart);

    return scheme_make_complex(realpart, imaginarypart);
  }

  NEED_NUMBER(exact->inexact);

  return scheme_void;
}

static Scheme_Object *
inexact_to_exact (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  Scheme_Type t;

  if (SCHEME_INTP(o))
    return o;
  t = _SCHEME_TYPE(o);
  if (t == scheme_double_type
#ifdef MZ_USE_SINGLE_FLOATS
      || t == scheme_float_type
#endif
      ) {
    double d = SCHEME_FLOAT_VAL(o);

    /* Try simple case: */
    Scheme_Object *i = scheme_make_integer((int)d);
    if ((double)SCHEME_INT_VAL(i) == d) {
# ifdef NAN_EQUALS_ANYTHING
      if (!MZ_IS_NAN(d))
#endif
	return i;
    }

    return scheme_rational_from_double(d);
  }
  if (t == scheme_bignum_type)
    return o;
  if (t == scheme_rational_type)
    return o;
  if ((t == scheme_complex_type) || (t == scheme_complex_izi_type)) {
    Scheme_Object *realpart, *imaginarypart;

    realpart = _scheme_complex_real_part(o);
    imaginarypart = _scheme_complex_imaginary_part(o);

    realpart = inexact_to_exact(1, &realpart);
    imaginarypart = inexact_to_exact(1, &imaginarypart);

    return scheme_make_complex(realpart, imaginarypart);
  }

  NEED_NUMBER(inexact->exact);

  return scheme_void;
}

static Scheme_Object *
number_to_string (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  long radix;

  if (!SCHEME_NUMBERP(o))
    scheme_wrong_type("number->string", "number", 0, argc, argv);
  
  if (argc == 2) {
    if (!SCHEME_INTP(argv[1]))
      radix = 0;
    else
      radix = SCHEME_INT_VAL(argv[1]);

    if ((radix != 2) && (radix != 8) && (radix != 10)  && (radix != 16)) {
      scheme_wrong_type("number->string", "2, 8, 10, or 16", 1, argc, argv);
      return NULL;
    }
    
    radix = SCHEME_INT_VAL(argv[1]);
  } else
    radix = 10;

  return scheme_make_string_without_copying(scheme_number_to_string(radix, o));
}

static char *double_to_string (double d)
{
  char buffer[100], *s;
  int l, i, digits;

  if (MZ_IS_NAN(d))
    return not_a_number_str;
  else if (MZ_IS_POS_INFINITY(d))
    return infinity_str;
  else if (MZ_IS_NEG_INFINITY(d))
    return minus_infinity_str;

  if (d == 0.0) {
    /* Check for -0.0, since some printers get it wrong. */
    if (minus_zero_p(d))
      return "-0.0";
  }

  /* Initial count for significant digits is 14. That's big enough to
     get most right, small enough to avoid nonsense digits. But we'll
     loop in case it's not precise enough to get read-write invariance: */
  digits = 14;
  while (digits < 30) {
    double check;
    char *ptr;

    sprintf(buffer, "%.*g", digits, d);

    /* Did we get read-write invariance, yet? */
    check = strtod(buffer, &ptr);
    if (check == d)
      break;

    digits++;
  }

  l = strlen(buffer);
  for (i = 0; i < l; i++) {
    if (buffer[i] == '.' || isalpha((unsigned char)buffer[i]))
      break;
  }
  if (i == l) {
    buffer[i] = '.';
    buffer[i + 1] = '0';
    buffer[i + 2] = 0;
    l += 2;
  }
  
  s = (char *)scheme_malloc_atomic(strlen(buffer) + 1);
  strcpy(s, buffer);

  return s;
}

char *scheme_number_to_string(int radix, Scheme_Object *obj)
{
  char *s;

  if (SCHEME_FLOATP(obj)) {
    if (radix != 10)
      scheme_raise_exn(MZEXN_APPLICATION_MISMATCH, 
		       scheme_make_integer(radix),
		       "number->string: "
		       "inexact numbers can only be printed in base 10");
    s = double_to_string(SCHEME_FLOAT_VAL(obj));
  } else if (SCHEME_RATIONALP(obj)) {
    Scheme_Object *n, *d;
    char *ns, *ds;
    int nlen, dlen;

    n = scheme_rational_numerator(obj);
    d = scheme_rational_denominator(obj);

    ns = scheme_number_to_string(radix, n);
    ds = scheme_number_to_string(radix, d);

    nlen = strlen(ns);
    dlen = strlen(ds);

    s = (char *)scheme_malloc_atomic(nlen + dlen + 2);
    memcpy(s, ns, nlen);
    s[nlen] = '/';
    strcpy(s + nlen + 1, ds);
  } else if (SCHEME_COMPLEXP(obj)) {
    Scheme_Object *r, *i;
    char *rs, *is;
    int rlen, ilen, offset = 0;

    r = _scheme_complex_real_part(obj);
    i = _scheme_complex_imaginary_part(obj);

    rs = scheme_number_to_string(radix, r);
    is = scheme_number_to_string(radix, i);

    rlen = strlen(rs);
    ilen = strlen(is);
    s = (char *)scheme_malloc_atomic(rlen + ilen + 3);
    memcpy(s, rs, rlen);
    if ((is[0] != '-') && (is[0] != '+')) {
      offset = 1;
      s[rlen] = '+';
    }
    memcpy(s + rlen + offset, is, ilen);
    s[rlen + offset + ilen] = 'i';
    s[rlen + offset + ilen + 1] = 0;
  } else {
    if (SCHEME_INTP(obj))
      obj = scheme_make_bignum(SCHEME_INT_VAL(obj));

    s = scheme_bignum_to_string(obj, radix);
  }

  return s;
}

int scheme_check_double(const char *where, double d, const char *dest)
{
  if (MZ_IS_POS_INFINITY(d)
      || MZ_IS_NEG_INFINITY(d)
      || MZ_IS_NAN(d)) {
    if (where)
      scheme_raise_exn(MZEXN_APPLICATION_TYPE,
		       scheme_make_double(d),
		       scheme_intern_symbol("small integer"),
		       "%s: no %s representation for %s",
		       where, 
		       dest,
		       double_to_string(d));
    return 0;
  }

  return 1;
}

#ifdef MZ_USE_SINGLE_FLOATS
int scheme_check_float(const char *where, float f, const char *dest)
{
  return scheme_check_double(where, f, dest);
}
#endif

static int double_to_int(const char *where, double d, long *v)
{
  if (scheme_check_double(where, d, "small integer")) {
    if ((d != floor(d))
	|| (d > 0 && ((double)(long)d) + 1 < d)
	|| (d < 0 && ((double)(long)d) - 1 > d)) {
      if (where)
	scheme_raise_exn(MZEXN_APPLICATION_TYPE,
			 scheme_make_double(d),
			 scheme_intern_symbol("small integer"),
			 "%s: no fixnum representation for %f",
			 where, d);
      return 0;
    }

    *v = (long)d;

    return 1;
  } else
    return 0;
}

long scheme_double_to_int(const char *where, double d)
{
  long l;

  double_to_int(where, d, &l);
  
  return l;
}

Scheme_Object *scheme_double_to_integer(const char *where, double d)
{
  long l;

  if (d != floor(d))
    return NULL;

  scheme_check_double(where, d, "integer");

  if (double_to_int(NULL, d, &l))
    return scheme_make_integer_value(l);

  return scheme_bignum_from_double(d);
}

static Scheme_Object *
string_to_number (int argc, Scheme_Object *argv[])
{
  long radix;
  long len;
  char *str;
  int decimal_inexact;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string->number", "string", 0, argc, argv);
  if (argc == 2) {
    if (SCHEME_INTP(argv[1]))
      radix = SCHEME_INT_VAL(argv[1]);
    else
      radix = 0;
    
    if ((radix < 2) || (radix > 16)) {
      scheme_wrong_type("string->number", "exact integer in [2, 16]", 1, argc, argv);
      return NULL;
    }
  } else
    radix = 10;

  str = SCHEME_STR_VAL(argv[0]);
  len = SCHEME_STRTAG_VAL(argv[0]);

  decimal_inexact = SCHEME_TRUEP(scheme_get_param(scheme_config, 
						  MZCONFIG_READ_DECIMAL_INEXACT));
  
  return scheme_read_number(str, len, 
			    0, 0, decimal_inexact,
			    radix, 0, NULL, NULL,
			    0);
}

/* Don't bother reading more than the following number of digits in a
   floating-point mantissa: */
#define MAX_FLOATREAD_PRECISION_DIGITS 50

/* We'd like to use strtod() for the common case, but we don't trust
   it entirely. */
#define MAX_FAST_FLOATREAD_LEN 50

/* Exponent threshold for obvious infinity. Must be at least
   max(MAX_FAST_FLOATREAD_LEN, MAX_FLOATREAD_PRECISION_DIGITS) more
   than the larget possible FP exponent. */
#define CHECK_INF_EXP_THRESHOLD 400

#ifdef USE_EXPLICT_FP_FORM_CHECK

/* Fixes Linux problem of 0e...  => non-number (0 with ptr at e...) */
/* Fixes SunOS problem with numbers like .3e2666666666666 => 0.0 */
/* Fixes HP/UX problem with numbers like .3e2666666666666 => non-number */

double STRTOD(const char *orig_c, char **f)
{
  int neg = 0;
  int found_dot = 0, is_infinity = 0, is_zero = 0;
  const char *c = orig_c;

  *f = (char *)c;

  if (*c == '-') {
    c++;
    neg = 1;
  } else if (*c == '+') {
    c++;
  }

  if (!isdigit((unsigned char)*c)) {
    if (*c == '.') {
      if (!isdigit((unsigned char)c[1]))
	return 0; /* no digits - bad! */
    } else
      return 0; /* no digits - bad! */
  }

  for (; *c; c++) {
    int ch = *c;

    if (isdigit(ch)) {
      /* ok */
    } else if ((ch == 'e') || (ch == 'E')) {
      int e = 0, neg_exp = 0;

      c++;
      if (*c == '-') {
	c++;
	neg_exp = 1;
      } else if (*c == '+') {
	c++;
      }
      if (!isdigit((unsigned char)*c))
	return 0; /* no digits - bad! */

      for (; *c; c++) {
	int ch = *c;
	if (!isdigit(ch))
	  return 0; /* not a digit - bad! */
	else {
	  e = (e * 10) + (ch - '0');
	  if (e > CHECK_INF_EXP_THRESHOLD) {
	    if (neg_exp)
	      is_zero  = 1;
	    else
	      is_infinity  = 1;
	  }
	}
      }

      break;
    } else if (ch == '.') {
      if (found_dot)
	return 0; /* two dots - bad! */
      found_dot = 1;
    } else
      return 0; /* unknown non-digit - bad! */
  }
  
  *f = (char *)c;

  if (is_infinity) {
    if (neg)
      return scheme_minus_infinity_val;
    else
      return scheme_infinity_val;
  }

  if (is_zero) {
    if (neg)
      return scheme_floating_point_nzero;
    else
      return scheme_floating_point_zero;
  }

  /* It's OK if c is ok: */
  return strtod(orig_c, NULL);
}
#else
#define STRTOD(x, y) strtod(x, y)
#endif

#ifdef MZ_USE_SINGLE_FLOATS
static Scheme_Object *CHECK_SINGLE(Scheme_Object *v, int s)
{
  if (s && SCHEME_DBLP(v))
    return scheme_make_float((float)SCHEME_DBL_VAL(v));
  else
    return v;
}
#else
# define CHECK_SINGLE(v, s) v
#endif

Scheme_Object *scheme_read_number(const char *str, long len,
				  int is_float, 
				  int is_not_float,
				  int decimal_means_float,
				  int radix, int radix_set, 
				  Scheme_Object *complain,
				  int *div_by_zero,
				  int test_only)
{
  int i, has_decimal, must_parse, has_slash;
  int report;
  Scheme_Object *next_complain;
  int has_hash, has_expt, has_i, has_sign, has_at, saw_digit, saw_nonzero_digit;
  Scheme_Object *o;
  char *ptr;
  const char *orig;
#ifdef MZ_USE_SINGLE_FLOATS
  int single;
#endif

  if (len < 0)
    len = strlen(str);

  orig = str;

  while (str[0] == '#') {
    if (str[1] != 'E' && str[1] != 'e' && str[1] != 'I' && str[1] != 'i') {
      if (radix_set) {
	if (complain)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad radix specification: %s",
			   str);
	else
	  return scheme_false;
      }
      radix_set = 1;
    } else {
      if (is_float || is_not_float) {
	if (complain)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad exactness specification: %s", 
			   str);
	else
	  return scheme_false;
      }
    }

    switch (str[1]) {
    case 'B':
    case 'b':
      radix = 2;
      break;
    case 'O':
    case 'o':
      radix = 8;
      break;
    case 'D':
    case 'd':
      radix = 10;
      break;
    case 'X':
    case 'x':
      radix = 16;
      break;
    case 'I':
    case 'i':
      is_float = 1;
      break;
    case 'E':
    case 'e':
      is_not_float = 1;
      break;
    default:
      if (complain)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: bad `#' indicator in %s", str);
      return scheme_false;
    }
    str += 2;
    len -= 2;
  }

  must_parse = (radix_set || is_float || is_not_float);

  report = complain && must_parse;
  next_complain = must_parse ? complain : NULL;

  if (!len) {
    if (report)
      scheme_raise_exn(MZEXN_READ, complain, 
		       "read-number: no digits");
    return scheme_false;
  }

  if (len == 6 && (str[0] == '-' || str[0] == '+') && isalpha((unsigned char)str[1])) {
    char s[7];
    int i;

    for (i = 0; i < 6; i++) {
      if (str[i] > 0)
	s[i] = tolower((unsigned char)str[i]);
      else
	s[i] = str[i];
    }
    s[i] = 0;

    if (!strcmp(s, infinity_str)) {
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
      return single_inf_object;
#else
      return inf_object;
#endif
    }
    else if (!strcmp(s, minus_infinity_str)) {
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
      return single_minus_inf_object;
#else
      return minus_inf_object;
#endif
    }
    else if (!strcmp(s, not_a_number_str)
	     || !strcmp(s, other_not_a_number_str)) {
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
      return single_nan_object;
#else      
      return nan_object;
#endif
    }
  }
      
#define isinexactmark(ch) ((ch == 'e') || (ch == 'E') \
			   || (ch == 's') || (ch == 'S') \
			   || (ch == 'f') || (ch == 'F') \
			   || (ch == 'd') || (ch == 'D') \
			   || (ch == 'l') || (ch == 'L'))

#define isbase16digit(ch) (((ch >= 'a') && (ch <= 'f')) \
                           || ((ch >= 'A') && (ch <= 'F')))

  has_i = 0;
  has_at = 0;
  has_sign = -1;
  for (i= 0; i < len; i++) {
    int ch = str[i];
    if (!ch) {
      if (report)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: embedded null character: %s", str);
      return scheme_false;
    } else if (isinexactmark(ch)) {
      /* If a sign follows, don't count it */
      if (str[i+1] == '+' || str[i+1] == '-')
	i++;
    } else if ((ch == '+') || (ch == '-')) {
      if ((has_sign > 0) || ((has_sign == 0) && (i == 1))) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: too many signs: %s", str);
	return scheme_false;
      }
      has_sign = i;
    } else if (((ch == 'I') || (ch == 'i')) && (has_sign >= 0)) {
      if (has_at) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: cannot mix `@' and `i': %s", str);
	return scheme_false;
      }
      if (i + 1 < len) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: `i' must be at the end: %s", str);
	return scheme_false;
      }
      has_i = i;
    } else if (ch == '@') {
      if (has_at) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: too many `@'s: %s", str);
	return scheme_false;
      }
      if (!i) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: `@' cannot be at start: %s", str);
	return scheme_false;
      }
      has_at = i;
      if (has_sign >= 0)
	has_sign = -1;
    }
  }

  if (has_i) {
    Scheme_Object *n1, *n2;
    char *first, *second;
    int fdbz = 0, sdbz = 0;

    if (has_sign) {
      first = (char *)scheme_malloc_atomic(has_sign + 1);
      memcpy(first, str, has_sign);
      first[has_sign] = 0;
    } else
      first = NULL;

    if (has_i - has_sign > 1) {
      second = (char *)scheme_malloc_atomic(has_i - has_sign + 1);
      memcpy(second, str + has_sign, has_i - has_sign);
      second[has_i - has_sign] = 0;
    } else
      second = NULL;

    if (first)
      n1 = scheme_read_number(first, has_sign,
			      is_float, is_not_float, decimal_means_float,
			      radix, 1, next_complain,
			      &fdbz, test_only);
    else
      n1 = zeroi;

    if (SAME_OBJ(n1, scheme_false) && !fdbz)
      return scheme_false;
    else if (SCHEME_FLOATP(n1)) {
      double d = SCHEME_FLOAT_VAL(n1);
      if (MZ_IS_NAN(d))
	return scheme_false;
    }
    
    if (second)
      n2 = scheme_read_number(second, has_i - has_sign,
			      is_float, is_not_float, decimal_means_float,
			      radix, 1, next_complain,
			      &sdbz, test_only);
    else if (str[has_sign] == '-')
      n2 = scheme_make_integer(-1);
    else
      n2 = scheme_make_integer(1);
    
    if (SAME_OBJ(n2, scheme_false) && !sdbz)
      return scheme_false;
    else if (SCHEME_FLOATP(n2)) {
      double d = SCHEME_FLOAT_VAL(n2);
      if (MZ_IS_NAN(d))
	return scheme_false;
    }

    if (fdbz || sdbz) {
      if (div_by_zero)
	*div_by_zero = 1;
      if (complain)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: division by zero in %s", str);
      return scheme_false;
    }

    if (!is_not_float && ((SCHEME_FLOATP(n1) && (n2 != zeroi)) || is_float))
      n2 = exact_to_inexact(1, &n2);  /* uses default conversion: float or double */
    else if (is_not_float)
      n2 = inexact_to_exact(1, &n2);

    if (!is_not_float && ((SCHEME_FLOATP(n2) && (n1 != zeroi)) || is_float))
      n1 = exact_to_inexact(1, &n1); /* uses default conversion: float or double */
    else if (is_not_float)
      n1 = inexact_to_exact(1, &n1);

    return scheme_make_complex(n1, n2);
  }

  if (has_at) {
    Scheme_Object *n1, *n2;
    double d1, d2, r1, r2;
    char *first;
    const char *second;
    int fdbz = 0, sdbz = 0;

    first = (char *)scheme_malloc_atomic(has_at + 1);
    memcpy(first, str, has_at);
    first[has_at] = 0;

#ifdef MZ_PRECISE_GC
    {
      /* Can't pass mis-aligned pointer to scheme_read_number. */
      int slen = len - (has_at + 1) + 1;
      second = (char *)scheme_malloc_atomic(slen);
      memcpy((char *)second, str + has_at + 1, slen);
    }
#else
    second = str + has_at + 1;
#endif

    n2 = scheme_read_number(second, len - has_at - 1,
			    1, 0, 1,
			    radix, 1, next_complain,
			    &fdbz, test_only);

    if (!fdbz) {
      if (SCHEME_FALSEP(n2))
	return scheme_false;

      d2 = SCHEME_FLOAT_VAL(n2);
      
      if (MZ_IS_NAN(d2))
	return scheme_false;

      /* Special case: angle is zero => real number */
      if (d2 == 0.0)
	return scheme_read_number(first, has_at,
				  is_float, is_not_float, decimal_means_float,
				  radix, 1, next_complain,
				  div_by_zero,
				  test_only);
      
      n1 = scheme_read_number(first, has_at, 
			      1, 0, 1,
			      radix, 1, next_complain,
			      &sdbz,
			      test_only);
    } else {
      n1 = NULL;
      d2 = 0;
    }

    if (fdbz || sdbz) {
      if (div_by_zero)
	*div_by_zero = 1;
      if (complain)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: division by zero in %s", str);
      return scheme_false;
    }

    if (SCHEME_FALSEP(n1))
      return scheme_false;

    d1 = SCHEME_FLOAT_VAL(n1);

    if (MZ_IS_NAN(d1))
      return scheme_false;

    r1 = d1 * cos(d2);
    r2 = d1 * sin(d2);

#ifdef MZ_USE_SINGLE_FLOATS
    if (SCHEME_FLTP(n1) && SCHEME_FLTP(n2))
      return scheme_make_complex(scheme_make_float((float)r1),
				 scheme_make_float((float)r2));
#endif

    return scheme_make_complex(scheme_make_double(r1),
			       scheme_make_double(r2));
  }

  has_decimal = has_slash = has_hash = has_expt = saw_digit = saw_nonzero_digit = 0;
  for (i = 0; i < len; i++) {
    int ch = str[i];
    if (ch == '.') {
      if (has_decimal) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: multiple decimal points: %s", str);
	return scheme_false;
      }
      if (has_slash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: decimal points and fractions "
			   "cannot be mixed: %s", str);
	return scheme_false;
      }
      has_decimal = 1;
    } else if (isinexactmark(ch)) {
      if (radix <= 10) {
	if (!i) {
	  if (report)
	    scheme_raise_exn(MZEXN_READ, complain, 
			     "read-number: cannot begin with `%c'", ch);
	  return scheme_false;
	}
	has_expt = i;
      }
      break;
    } else if (ch == '/') {
      if (!i) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: cannot have slash at start: %s", str);
	return scheme_false;
      }
      if (has_slash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: multiple slashes: %s", str);
	return scheme_false;
      }
      if (has_decimal) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: decimal points and fractions "
			   "cannot be mixed: %s", str);
	return scheme_false;
      }
      if (has_hash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: misplaced hash: %s", str);
	return scheme_false;
      }
      has_slash = i;
    } else if ((ch == '-') || (ch == '+')) {
      if (has_slash || has_decimal || has_hash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: misplaced sign: %s", str);
	return scheme_false;
      }
    } else if (ch == '#') {
      if (has_slash || /* has_decimal || (radix > 10) || */ !saw_digit) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: misplaced hash: %s", str);
	return scheme_false;
      }
      has_hash = 1;
    } else if (!isdigit(ch) && !((radix == 16) && isbase16digit(ch))) {
      if (has_decimal) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad decimal number: %s", str);
	return scheme_false;
      }
      if (has_hash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: misplaced hash: %s", str);
	return scheme_false;
      }
      break;
    } else {
      saw_digit = 1;
      if (ch != '0')
	saw_nonzero_digit = 1;
      if (has_hash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: misplaced hash: %s", str);
	return scheme_false;
      }
    }
  }

#ifdef MZ_USE_SINGLE_FLOATS
  if (has_expt && str[has_expt]) {
    single = str[has_expt];
    single = ((single == 'f') || (single == 'F')
	      || (single == 's') || (single == 'S'));
  } else {
# ifdef USE_SINGLE_FLOATS_AS_DEFAULT
    single = 1;
# else
    single = 0;
# endif
  }
#endif


  /* When possible, use the standard floating-point parser */
  if (!is_not_float && (is_float || decimal_means_float) 
      && !has_slash && !has_hash && (radix == 10) 
      && (has_decimal || has_expt)
      && (len <= MAX_FAST_FLOATREAD_LEN)) {
    double d;
    const char *strcpy;
    if (has_expt && (str[has_expt] != 'e' && str[has_expt] != 'E')) {
      char *str2;
      str2 = (char *)scheme_malloc_atomic(len + 1);
      memcpy(str2, str, len + 1);
      str2[has_expt] = 'e';
      strcpy = str2;
    } else
      strcpy = str;
    d = STRTOD(strcpy, &ptr);
    if ((ptr - strcpy) < len) {
      if (report)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: bad decimal number %s", str);
      return scheme_false;
    } 

    if (!saw_nonzero_digit) {
      /* Assert: d = 0.0 or -0.0 */
      if (str[0] == '-') {
	/* Make sure it's -0.0 */
#ifdef MZ_USE_SINGLE_FLOATS
	if (single) return nzerof;
#endif
	return nzerod;
      }
    }

#ifdef MZ_USE_SINGLE_FLOATS
    if (single)
      return scheme_make_float((float)d);
#endif
    return scheme_make_double(d);
  }

  if (has_decimal || has_expt || has_hash) {
    Scheme_Object *mantissa, *exponent, *power, *n;
    Scheme_Object *args[2];
    int result_is_float= (is_float || (!is_not_float && decimal_means_float));

    if (has_expt) {
      char *substr;

#ifdef MZ_PRECISE_GC
      {
	/* Can't pass misaligned pointer to scheme_read_bignum: */
	int slen = len - (has_expt + 1) + 1;
	substr = (char *)scheme_malloc_atomic(slen);
	memcpy(substr, str + has_expt + 1, slen);
      }
#else
      substr = (char *)str + has_expt + 1;
#endif

      exponent = scheme_read_bignum(substr, radix);
      if (SCHEME_FALSEP(exponent)) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad exponent: %s", str);
	return scheme_false;
      }
    } else
      exponent = zeroi;

    if (!has_expt)
      has_expt = len;

    if (has_slash) {
      /* Mantissa is a fraction. */
      char *s;
      int dbz;
      
      s = (char *)scheme_malloc_atomic(has_expt + 1);
      memcpy(s, str, has_expt);
      s[has_expt] = 0;
      
      mantissa = scheme_read_number(s, has_expt, 
				    0, 0, 1,
				    radix, 1, next_complain,
				    &dbz,
				    test_only);

      if (SCHEME_FALSEP(mantissa)) {
	if (dbz) {
	  if (div_by_zero)
	    *div_by_zero = 1;
	  if (complain)
	    scheme_raise_exn(MZEXN_READ, complain, 
			     "read-number: division by zero in %s", str);
	}
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad number %s", str);
	return scheme_false;
      }
    } else {
      /* Mantissa is not a fraction. */
      char *digits;
      int extra_power = 0, dcp = 0, num_ok;

      digits = (char *)scheme_malloc_atomic(has_expt + 1);

      i = 0;
      if (str[i] == '+' || str[i] == '-')
	digits[dcp++] = str[i++];

      for (; isdigit((unsigned char)str[i]) || ((radix == 16) && isbase16digit(str[i])); i++) {
	digits[dcp++] = str[i];
      }

      if (str[i] == '#') {
	for (; str[i] == '#'; i++) {
	  digits[dcp++] = '0';
	}
	num_ok = 0;
      } else
	num_ok = 1;
	
      if (str[i] == '.') {
	i++;
	if (num_ok)
	  for (; isdigit((unsigned char)str[i]) || ((radix == 16) && isbase16digit(str[i])); i++) {
	    digits[dcp++] = str[i];
	    extra_power++;
	  }

	for (; str[i] == '#'; i++) {
	  digits[dcp++] = '0';  
	  extra_power++;
	}
      }

      if ((str[i] && (!has_expt || i != has_expt))
	  || !dcp || (dcp == 1 && !(isdigit((unsigned char)digits[0])
				    || ((radix == 16) && isbase16digit(digits[0]))))) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad decimal number %s", str);
	return scheme_false;
      }

      /* Reduce unnecessary mantissa-reading work for inexact results.
         This is also necessary to make the range check on `exponent'
         correct. */
      if (result_is_float && (dcp > MAX_FLOATREAD_PRECISION_DIGITS)) {
	extra_power -= (dcp - MAX_FLOATREAD_PRECISION_DIGITS);
	dcp = MAX_FLOATREAD_PRECISION_DIGITS;
      }

      digits[dcp] = 0;
      mantissa = scheme_read_bignum(digits, radix);
      if (SCHEME_FALSEP(mantissa)) {
	/* can get here with bad radix */
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad number %s", digits);
	return scheme_false;
      }

      if (extra_power)
	exponent = scheme_bin_minus(exponent, scheme_make_integer(extra_power));
    
      /* Don't calculate a huge exponential if we're returning a float: */
      if (result_is_float) {
	if (scheme_bin_gt(exponent, scheme_make_integer(CHECK_INF_EXP_THRESHOLD))) {
	  if (SCHEME_TRUEP(negative_p(1, &mantissa)))
	    return CHECK_SINGLE(minus_inf_object, single);
	  else
	    return CHECK_SINGLE(inf_object, single);
	} else if (scheme_bin_lt(exponent, scheme_make_integer(-CHECK_INF_EXP_THRESHOLD))) {
	  if (SCHEME_TRUEP(negative_p(1, &mantissa)))
	    return CHECK_SINGLE(nzerod, single);
	  else
	    return CHECK_SINGLE(zerod, single);
	}
      }
    }

    /* This is the important use of test_only, because it's the one
       place where the read calculation is not linear in the input. */
    if (test_only)
      return scheme_make_integer(1);

    args[0] = scheme_make_integer(radix);
    args[1] = exponent;
    power = scheme_expt(2, args);

    n = scheme_bin_mult(mantissa, power);

    if (result_is_float)
      n = CHECK_SINGLE(TO_DOUBLE(n), single);
    else
      n = CHECK_SINGLE(n, single);

    if (SCHEME_FLOATP(n) && str[0] == '-') {
      if (SCHEME_FLOAT_VAL(n) == 0.0) {
	/* 0.0 => -0.0 */
#ifdef MZ_USE_SINGLE_FLOATS
	if (SCHEME_FLTP(n)) {
	  n = scheme_make_float(-SCHEME_FLT_VAL(n));
	} else
#endif
	  n = scheme_make_double(-SCHEME_DBL_VAL(n));
      }
    }

    return n;
  }
  
  if (has_slash) {
    Scheme_Object *n1, *n2;
    char *first;

    first = (char *)scheme_malloc_atomic(has_slash + 1);
    memcpy(first, str, has_slash);
    first[has_slash] = 0;

    n1 = scheme_read_number(first, has_slash,
			    0, 0, 1,
			    radix, 1, next_complain,
			    div_by_zero,
			    test_only);
    if (SAME_OBJ(n1, scheme_false))
      return scheme_false;

    {
      char *substr;

#ifdef MZ_PRECISE_GC
      {
	/* Can't pass misaligned pointer to scheme_read_bignum: */
	int slen = len - (has_slash + 1) + 1;
	substr = (char *)scheme_malloc_atomic(slen);
	memcpy(substr, str + has_slash + 1, slen);
      }
#else
      substr = (char *)str + has_slash + 1;
#endif

      n2 = scheme_read_number(substr, len - has_slash - 1,
			      0, 0, 1,
			      radix, 1, next_complain,
			      div_by_zero,
			      test_only);
    }

    if (SAME_OBJ(n2, scheme_false))
      return scheme_false;

    if (SCHEME_TRUEP(zero_p(1, &n2))) {
      if (complain)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: division by zero in %s", str);
      if (div_by_zero)
	*div_by_zero = 1;
      return scheme_false;
    }

    if (test_only)
      return scheme_make_integer(1);

    n1 = scheme_bin_div(n1, n2);

    if (is_not_float)
      n1 = inexact_to_exact(1, &n1);
    else if (is_float)
      n1 = TO_DOUBLE(n1);

    return CHECK_SINGLE(n1, single);
  }

  o = scheme_read_bignum(str, radix);
  if (SAME_OBJ(o, scheme_false)) {
    if (report)
      scheme_raise_exn(MZEXN_READ, complain, 
		       "read-number: bad number %s", str);
  } else if (is_float) {
    /* Special case: "#i-0" => -0. */
    if ((o == zeroi) && str[0] == '-') {
#ifdef MZ_USE_SINGLE_FLOATS
      if (single) return nzerof;
#endif
      return nzerod;
    }

    return CHECK_SINGLE(TO_DOUBLE(o), single);
  }

  return o;
}

static Scheme_Object *
bitwise_not(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (SCHEME_INTP(o)) {
    long a = SCHEME_INT_VAL(o);

    a = ~a;
    return scheme_make_integer(a);
  } else if (_SCHEME_TYPE(o) == scheme_bignum_type)
    return scheme_bignum_not(o);
   
  scheme_wrong_type("bitwise-not", "exact integer", 0, argc, argv);
  return scheme_void;
}

static Scheme_Object *
bitwise_shift(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v, *so;
  long shift;

  v = argv[0];
  
  if (!SCHEME_EXACT_INTEGERP(v)) {
    scheme_wrong_type("arithmetic-shift", "exact integer", 1, argc, argv);
    return NULL;
  }
  so = argv[1];
  if (!SCHEME_INTP(so)) {
    if (SCHEME_BIGNUMP(so)) {
      if (!SCHEME_BIGPOS(so)) {
	if (SCHEME_TRUEP(negative_p(1, &v)))
	  return scheme_make_integer(-1);
	else
	  return scheme_make_integer(0);
      } else
	scheme_raise_out_of_memory("arithmetic-shift", NULL);
    } else
      scheme_wrong_type("arithmetic-shift", "exact integer", 1, argc, argv);
    return NULL;
  }
  
  shift = SCHEME_INT_VAL(so);
  if (!shift)
    return v;

  if (SCHEME_INTP(v)) {
    long i = SCHEME_INT_VAL(v);

    if (!i)
      return v;

    if (i > 0) {
      if (shift < 0) {
	int shft = -shift;
	if (shft <= MAX_SHIFT_EVER) {
	  i = i >> shft;
	  return scheme_make_integer(i);
	} else
	  return scheme_make_integer(0);
      } else if (shift <= MAX_SHIFT_TRY) {
	long n;
	
	n = i << shift;
	if ((n > 0) && (SCHEME_INT_VAL(scheme_make_integer(n)) >> shift == i))
	  return scheme_make_integer(n);
      }
    }

    v = scheme_make_bignum(i);
  }

  return scheme_bignum_shift(v, shift);
}

#include "random.inc"

static Scheme_Object *
random_seed(int argc, Scheme_Object *argv[])
{
  long i = -1;
  Scheme_Object *o = argv[0];

  if (scheme_get_int_val(o,  &i)) {
    if (i > 2147483647)
      i = -1;
  }

  if (i < 0)
    scheme_wrong_type("random-seed", "exact integer in [0, 2147483647]", 0, argc, argv);

  sch_srand(i, (Scheme_Random_State *)scheme_get_param(scheme_config, MZCONFIG_RANDOM_STATE));

  return scheme_void;
}

static Scheme_Object *
sch_random(int argc, Scheme_Object *argv[])
{
  long i = -1, v;
  Scheme_Object *o = argv[0];

  if (scheme_get_int_val(o,  &i)) {
    if (i > 2147483647)
      i = -1;
  }

  if (i <= 0)
    scheme_wrong_type("random", "exact integer in [1, 2147483647]", 0, argc, argv);
  
  v = sch_rand((Scheme_Random_State *)scheme_get_param(scheme_config, MZCONFIG_RANDOM_STATE)) % i;

  return scheme_make_integer_value(v);
}

static Scheme_Object *current_pseudo_random_generator(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-pseudo-random-generator", 
			     scheme_make_integer(MZCONFIG_RANDOM_STATE),
			     argc, argv,
			     -1, pseudo_random_generator_p, "pseudo-random-generator", 0);
}

static Scheme_Object *make_pseudo_random_generator(int argc, Scheme_Object **argv)
{
  return scheme_make_random_state(scheme_get_milliseconds());
}

static Scheme_Object *pseudo_random_generator_p(int argc, Scheme_Object **argv)
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_random_state_type)) 
	  ? scheme_true 
	  : scheme_false);
}

