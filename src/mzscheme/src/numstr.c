/*
  MzScheme
  Copyright (c) 2000 Matthew Flatt

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
#include <math.h>
#include <string.h>
#include <ctype.h>

static Scheme_Object *number_to_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_number (int argc, Scheme_Object *argv[]);

static Scheme_Object *random_seed(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_random(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_pseudo_random_generator(int argc, Scheme_Object **argv);
static Scheme_Object *current_pseudo_random_generator(int argc, Scheme_Object **argv);
static Scheme_Object *pseudo_random_generator_p(int argc, Scheme_Object **argv);

static char *infinity_str = "+inf.0";
static char *minus_infinity_str = "-inf.0";
static char *not_a_number_str = "+nan.0";
static char *other_not_a_number_str = "-nan.0";

#define TO_DOUBLE scheme_TO_DOUBLE

#define zeroi scheme_make_integer(0)

void scheme_init_numstr(Scheme_Env *env)
{
  if (scheme_starting_up) {
  }

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

/*========================================================================*/
/*                           number parsing                               */
/*========================================================================*/

static Scheme_Object *read_special_number(const char *str, int pos)
{
  if ((str[pos] == '-' || str[pos] == '+') && isalpha((unsigned char)str[pos + 1])) {
    char s[7];
    int i;

    for (i = 0; i < 6; i++) {
      s[i] = tolower((unsigned char)str[i + pos]);
    }
    s[i] = 0;

    if (!strcmp(s, infinity_str)) {
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
      return single_inf_object;
#else
      return scheme_inf_object;
#endif
    }
    else if (!strcmp(s, minus_infinity_str)) {
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
      return single_minus_inf_object;
#else
      return scheme_minus_inf_object;
#endif
    }
    else if (!strcmp(s, not_a_number_str)
	     || !strcmp(s, other_not_a_number_str)) {
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
      return single_nan_object;
#else      
      return scheme_nan_object;
#endif
    }
  }

  return NULL;
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

# ifdef MZ_PRECISE_GC
END_XFORM_ARITH;
# endif

static double STRTOD(const char *orig_c, char **f)
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
# ifdef MZ_PRECISE_GC
START_XFORM_ARITH;
# endif
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
  int report, delta;
  Scheme_Object *next_complain;
  int has_hash, has_expt, has_i, has_sign, has_at, saw_digit, saw_nonzero_digit;
  Scheme_Object *o;
  const char *orig;
#ifdef MZ_USE_SINGLE_FLOATS
  int single;
#endif

  if (len < 0)
    len = strlen(str);

  orig = str;

  delta = 0;

  while (str[delta] == '#') {
    if (str[delta+1] != 'E' && str[delta+1] != 'e' && str[delta+1] != 'I' && str[delta+1] != 'i') {
      if (radix_set) {
	if (complain)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad radix specification: %t",
			   str, len);
	else
	  return scheme_false;
      }
      radix_set = 1;
    } else {
      if (is_float || is_not_float) {
	if (complain)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad exactness specification: %t", 
			   str, len);
	else
	  return scheme_false;
      }
    }

    switch (str[delta+1]) {
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
			 "read-number: bad `#' indicator `%c' in: %t",
			 str[delta+1], str, len);
      return scheme_false;
    }
    delta += 2;
  }

  must_parse = (radix_set || is_float || is_not_float);

  report = complain && must_parse;
  next_complain = must_parse ? complain : NULL;

  if (!(len - delta)) {
    if (report)
      scheme_raise_exn(MZEXN_READ, complain, 
		       "read-number: no digits");
    return scheme_false;
  }

  /* look for +inf.0, etc: */
  if (len -delta == 6) {
    Scheme_Object *special;
    special = read_special_number(str, delta);
    if (special)
      return special;
  }

  /* Look for <special>+...i and ...<special>i */
  if ((len-delta > 7) && str[len-1] == 'i') {
    Scheme_Object *special;
    char *s2;
    
    /* Try <special>+...i */
    special = read_special_number(str, delta);
    if (special) {
      s2 = scheme_malloc_atomic(len - delta - 6 + 4 + 1);
      memcpy(s2, "+0.0", 4);
      memcpy(s2 + 4, str + delta + 6, len - delta - 5);
    } else {
      /* Try ...<special>i: */
      special = read_special_number(str, len - 7);
      if (special) {
	s2 = scheme_malloc_atomic(len - delta - 6 + 4 + 1);
	memcpy(s2, str + delta, len - delta - 7);
	memcpy(s2 + len - delta - 7, "+0.0i", 6);
	special = scheme_bin_mult(special, scheme_plus_i);
      } else
	s2 = NULL;
    }

    if (special) {
      Scheme_Object *other;
      int dbz = 0;

      other = scheme_read_number(s2, len - delta - 6 + 4,
				 is_float, is_not_float, 1,
				 radix, 1, 0,
				 &dbz, test_only);

      if (dbz) {
	if (div_by_zero)
	  *div_by_zero = 1;
	if (complain)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: division by zero in %t",
			   str, len);
	return scheme_false;
      }

      if (!SCHEME_FALSEP(other))
	return scheme_bin_plus(special, other);
      
      if (!complain)
	return scheme_false;
    }
  }

  /* Look for <special>@... and ...@<special> */
  if ((len - delta > 7) && ((str[delta+6] == '@') || (str[len - 7] == '@'))) {
    Scheme_Object *special;
    char *s2;
    int spec_mag = 0;

    /* Try <special>@... */
    if (str[delta+6] == '@')
      special = read_special_number(str, delta);
    else
      special = NULL;
    if (special) {
      s2 = scheme_malloc_atomic(len - delta - 6);
      memcpy(s2, str + delta + 7, len - delta - 6);
      spec_mag = 1;
    } else {
      if (str[len - 7] == '@')
	special = read_special_number(str, len - 6);
      else
	special = NULL;
      
      if (special) {
	s2 = scheme_malloc_atomic(len - delta - 6);
	memcpy(s2, str + delta, len - delta - 7);
	s2[len - delta - 7] = 0;
      } else
	s2 = NULL;
    }

    if (special) {
      Scheme_Object *other;
      int dbz = 0;

      /* s2 can't contain @: */
      for (i = 0; s2[i]; i++) {
	if (s2[i] == '@')
	  break;
      }

      if (s2[i])
	other = scheme_false;
      else
	other = scheme_read_number(s2, len - delta - 7,
				   is_float, is_not_float, 1,
				   radix, 1, 0,
				   &dbz, test_only);

      if (dbz) {
	if (div_by_zero)
	  *div_by_zero = 1;
	if (complain)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: division by zero in %t", 
			   str, len);
	return scheme_false;
      }

      if (!SCHEME_FALSEP(other)) {
	/* If string is complex, not well-formed: */
	if (!SCHEME_COMPLEXP(other)) {
	  Scheme_Object *a[2];
	  if (spec_mag) {
	    a[0] = special;
	    a[1] = other;
	  } else {
	    a[0] = other;
	    a[1] = special;
	  }

	  return scheme_make_polar(2, a);
	}
      }

      if (!complain)
	return scheme_false;
    }
  }
      
#define isinexactmark(ch) ((ch == 'e') || (ch == 'E') \
			   || (ch == 's') || (ch == 'S') \
			   || (ch == 'f') || (ch == 'F') \
			   || (ch == 'd') || (ch == 'D') \
			   || (ch == 'l') || (ch == 'L'))

#define isbaseNdigit(N, ch) (((ch >= 'a') && (ch <= ('a' + N - 11))) \
                             || ((ch >= 'A') && (ch <= ('A' + N - 11))))

  has_i = 0;
  has_at = 0;
  has_sign = delta-1;
  for (i = delta; i < len; i++) {
    int ch = str[i];
    if (!ch) {
      if (report)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: embedded null character: %t",
			 str, len);
      return scheme_false;
    } else if (isinexactmark(ch) && ((radix <= 10) || !isbaseNdigit(radix, ch))) {
      /* If a sign follows, don't count it */
      if (str[i+1] == '+' || str[i+1] == '-')
	i++;
    } else if ((ch == '+') || (ch == '-')) {
      if ((has_sign > delta) || ((has_sign == delta) && (i == delta+1))) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: too many signs: %t", 
			   str, len);
	return scheme_false;
      }
      has_sign = i;
    } else if (((ch == 'I') || (ch == 'i')) && (has_sign >= delta)) {
      if (has_at) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: cannot mix `@' and `i': %t", 
			   str, len);
	return scheme_false;
      }
      if (i + 1 < len) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: `i' must be at the end: %t", 
			   str, len);
	return scheme_false;
      }
      has_i = i;
    } else if (ch == '@') {
      if (has_at) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: too many `@'s: %t", 
			   str, len);
	return scheme_false;
      }
      if (i == delta) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: `@' cannot be at start: %t", 
			   str, len);
	return scheme_false;
      }
      has_at = i;
      if (has_sign >= delta)
	has_sign = delta-1;
    }
  }

  if (has_i) {
    Scheme_Object *n1, *n2;
    char *first, *second;
    int fdbz = 0, sdbz = 0;

    if (has_sign != delta) {
      first = (char *)scheme_malloc_atomic(has_sign - delta + 1);
      memcpy(first, str + delta, has_sign - delta);
      first[has_sign - delta] = 0;
    } else
      first = NULL;

    if (has_i - has_sign > 1) {
      second = (char *)scheme_malloc_atomic(has_i - has_sign + 1);
      memcpy(second, str + has_sign, has_i - has_sign);
      second[has_i - has_sign] = 0;
    } else
      second = NULL;

    if (first)
      n1 = scheme_read_number(first, has_sign - delta,
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
			 "read-number: division by zero in %t", 
			 str, len);
      return scheme_false;
    }

    if (!is_not_float && ((SCHEME_FLOATP(n1) && (n2 != zeroi)) || is_float))
      n2 = scheme_exact_to_inexact(1, &n2);  /* uses default conversion: float or double */
    else if (is_not_float)
      n2 = scheme_inexact_to_exact(1, &n2);

    if (!is_not_float && ((SCHEME_FLOATP(n2) && (n1 != zeroi)) || is_float))
      n1 = scheme_exact_to_inexact(1, &n1); /* uses default conversion: float or double */
    else if (is_not_float)
      n1 = scheme_inexact_to_exact(1, &n1);

    return scheme_make_complex(n1, n2);
  }

  if (has_at) {
    Scheme_Object *n1, *n2;
    double d1, d2, r1, r2;
    char *first;
    const char *second;
    int fdbz = 0, sdbz = 0;

    first = (char *)scheme_malloc_atomic(has_at - delta + 1);
    memcpy(first, str + delta, has_at - delta);
    first[has_at - delta] = 0;

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
			    is_float, is_not_float, decimal_means_float,
			    radix, 1, next_complain,
			    &fdbz, test_only);

    if (!fdbz) {
      if (SCHEME_FALSEP(n2))
	return scheme_false;

      /* Special case: angle is zero => real number */
      if (n2 == zeroi)
	return scheme_read_number(first, has_at - delta,
				  is_float, is_not_float, decimal_means_float,
				  radix, 1, complain,
				  div_by_zero,
				  test_only);
      
      n2 = TO_DOUBLE(n2);

      d2 = SCHEME_FLOAT_VAL(n2);
      
      if (MZ_IS_NAN(d2))
	return scheme_false;

      n1 = scheme_read_number(first, has_at - delta, 
			      is_float, is_not_float, decimal_means_float,
			      radix, 1, next_complain,
			      &sdbz,
			      test_only);

      /* Special case: magnitude is zero => zero */
      if (n1 == zeroi)
	return zeroi;

      if (!SCHEME_FALSEP(n1))
	n1 = TO_DOUBLE(n1);
    } else {
      n1 = NULL;
      d2 = 0;
    }

    if (fdbz || sdbz) {
      if (div_by_zero)
	*div_by_zero = 1;
      if (complain)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: division by zero in %t", 
			 str, len);
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
  for (i = delta; i < len; i++) {
    int ch = str[i];
    if (ch == '.') {
      if (has_decimal) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: multiple decimal points: %t", 
			   str, len);
	return scheme_false;
      }
      if (has_slash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: decimal points and fractions "
			   "cannot be mixed: %t", 
			   str, len);
	return scheme_false;
      }
      has_decimal = 1;
    } else if (isinexactmark(ch)) {
      if ((radix <= 10) || !isbaseNdigit(radix, ch)) {
	if (i == delta) {
	  if (report)
	    scheme_raise_exn(MZEXN_READ, complain, 
			     "read-number: cannot begin with `%c' in %t", 
			     ch, str, len);
	  return scheme_false;
	}
	has_expt = i;
	break;
      }
    } else if (ch == '/') {
      if (i == delta) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: cannot have slash at start: %t", 
			   str, len);
	return scheme_false;
      }
      if (has_slash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: multiple slashes: %t", 
			   str, len);
	return scheme_false;
      }
      if (has_decimal) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: decimal points and fractions "
			   "cannot be mixed: %t", 
			   str, len);
	return scheme_false;
      }
      if (has_hash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: misplaced hash: %t", 
			   str, len);
	return scheme_false;
      }
      has_slash = i;
    } else if ((ch == '-') || (ch == '+')) {
      if (has_slash || has_decimal || has_hash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: misplaced sign: %t", 
			   str, len);
	return scheme_false;
      }
    } else if (ch == '#') {
      if (has_slash || /* has_decimal || (radix > 10) || */ !saw_digit) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: misplaced hash: %t", 
			   str, len);
	return scheme_false;
      }
      has_hash = 1;
    } else if (!isdigit(ch) && !((radix > 10) && isbaseNdigit(radix, ch))) {
      if (has_decimal) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad decimal number: %t", 
			   str, len);
	return scheme_false;
      }
      if (has_hash) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: misplaced hash: %t", 
			   str, len);
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
			   "read-number: misplaced hash: %t", 
			   str, len);
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
    const char *cpy;
    char *ptr;

    if (has_expt && (str[has_expt] != 'e' && str[has_expt] != 'E')) {
      char *str2;
      str2 = (char *)scheme_malloc_atomic(len + 1);
      memcpy(str2, str, len + 1);
      str2[has_expt] = 'e';
      cpy = str2;
    } else
      cpy = str;
    d = STRTOD(cpy + delta, &ptr);
    if ((ptr - cpy) < len) {
      ptr = NULL; /* because not precise-gc aligned */
      if (report)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: bad decimal number %t",
			 str, len);
      return scheme_false;
    } 
    ptr = NULL; /* because not precise-gc aligned */

    if (!saw_nonzero_digit) {
      /* Assert: d = 0.0 or -0.0 */
      if (str[delta] == '-') {
	/* Make sure it's -0.0 */
#ifdef MZ_USE_SINGLE_FLOATS
	if (single) return nzerof;
#endif
	return scheme_nzerod;
      }
    }

    if (!d) {
      if (str[delta] == '-') {
	/* Make sure it's -0.0 */
#ifdef MZ_USE_SINGLE_FLOATS
	if (single) return nzerof;
#endif
	return scheme_nzerod;
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

      exponent = scheme_read_bignum(substr, 0, radix);
      if (SCHEME_FALSEP(exponent)) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad exponent: %t", 
			   str, len);
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
      
      s = (char *)scheme_malloc_atomic(has_expt - delta + 1);
      memcpy(s, str + delta, has_expt - delta);
      s[has_expt - delta] = 0;
      
      mantissa = scheme_read_number(s, has_expt - delta, 
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
			     "read-number: division by zero in %t", 
			     str, len);
	}
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad number %t", 
			   str, len);
	return scheme_false;
      }
    } else {
      /* Mantissa is not a fraction. */
      char *digits;
      int extra_power = 0, dcp = 0, num_ok;

      digits = (char *)scheme_malloc_atomic(has_expt - delta + 1);

      i = delta;
      if (str[i] == '+' || str[i] == '-')
	digits[dcp++] = str[i++];

      for (; isdigit((unsigned char)str[i]) || ((radix > 10) && isbaseNdigit(radix, str[i])); i++) {
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
	  for (; isdigit((unsigned char)str[i]) || ((radix > 10) && isbaseNdigit(radix, str[i])); i++) {
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
				    || ((radix > 10) && isbaseNdigit(radix, digits[0]))))) {
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad decimal number %t", 
			   str, len);
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
      mantissa = scheme_read_bignum(digits, 0, radix);
      if (SCHEME_FALSEP(mantissa)) {
	/* can get here with bad radix */
	if (report)
	  scheme_raise_exn(MZEXN_READ, complain, 
			   "read-number: bad number %t", 
			   str, len);
	return scheme_false;
      }

      if (extra_power)
	exponent = scheme_bin_minus(exponent, scheme_make_integer(extra_power));
    
      /* Don't calculate a huge exponential if we're returning a float: */
      if (result_is_float) {
	if (scheme_bin_gt(exponent, scheme_make_integer(CHECK_INF_EXP_THRESHOLD))) {
	  if (SCHEME_TRUEP(scheme_negative_p(1, &mantissa)))
	    return CHECK_SINGLE(scheme_minus_inf_object, single);
	  else
	    return CHECK_SINGLE(scheme_inf_object, single);
	} else if (scheme_bin_lt(exponent, scheme_make_integer(-CHECK_INF_EXP_THRESHOLD))) {
	  if (SCHEME_TRUEP(scheme_negative_p(1, &mantissa)))
	    return CHECK_SINGLE(scheme_nzerod, single);
	  else
	    return CHECK_SINGLE(scheme_zerod, single);
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

    first = (char *)scheme_malloc_atomic(has_slash - delta + 1);
    memcpy(first, str + delta, has_slash - delta);
    first[has_slash - delta] = 0;

    n1 = scheme_read_number(first, has_slash - delta,
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

    if (SCHEME_TRUEP(scheme_zero_p(1, &n2))) {
      if (complain)
	scheme_raise_exn(MZEXN_READ, complain, 
			 "read-number: division by zero in %t", 
			 str, len);
      if (div_by_zero)
	*div_by_zero = 1;
      return scheme_false;
    }

    if (test_only)
      return scheme_make_integer(1);

    n1 = scheme_bin_div(n1, n2);

    if (is_not_float)
      n1 = scheme_inexact_to_exact(1, &n1);
    else if (is_float)
      n1 = TO_DOUBLE(n1);

    return CHECK_SINGLE(n1, single);
  }

  o = scheme_read_bignum(str, delta, radix);
  if (SAME_OBJ(o, scheme_false)) {
    if (report)
      scheme_raise_exn(MZEXN_READ, complain, 
		       "read-number: bad number %t", 
		       str, len);
  } else if (is_float) {
    /* Special case: "#i-0" => -0. */
    if ((o == zeroi) && str[delta] == '-') {
#ifdef MZ_USE_SINGLE_FLOATS
      if (single) return nzerof;
#endif
      return scheme_nzerod;
    }

    return CHECK_SINGLE(TO_DOUBLE(o), single);
  }

  return o;
}

/*========================================================================*/
/*                           scheme functions                             */
/*========================================================================*/

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
      ESCAPED_BEFORE_HERE;
    }
    
    radix = SCHEME_INT_VAL(argv[1]);
  } else
    radix = 10;

  return scheme_make_string_without_copying(scheme_number_to_string(radix, o));
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
      ESCAPED_BEFORE_HERE;
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
    if (scheme_minus_zero_p(d))
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

/*========================================================================*/
/*                       random number generator                          */
/*========================================================================*/

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

