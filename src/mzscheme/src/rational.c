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
#include <math.h>

static Scheme_Object *one = scheme_make_integer(1);

static Scheme_Object *make_rational(const Scheme_Object *n, const Scheme_Object *d,
				    int normalize)
{
  Scheme_Rational *r;

  r = MALLOC_ONE_TAGGED(Scheme_Rational);
  r->type = scheme_rational_type;
  r->num = (Scheme_Object *)n;
  r->denom = (Scheme_Object *)d;
  
  return normalize ? scheme_rational_normalize((Scheme_Object *)r) 
    : (Scheme_Object *)r;
}

Scheme_Object *scheme_make_rational(const Scheme_Object *n, const Scheme_Object *d)
{
  return make_rational(scheme_bignum_normalize(n), 
		       scheme_bignum_normalize(d), 1);
}

Scheme_Object *scheme_integer_to_rational(const Scheme_Object *n)
{
  return make_rational(n, one, 0);
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

Scheme_Object *scheme_make_small_rational(long n, Small_Rational *s)
{
  s->type = scheme_rational_type;
  s->num = scheme_make_integer(n);
  s->denom = one;

  return (Scheme_Object *)s;
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

int scheme_is_rational_positive(const Scheme_Object *o)
{
  Scheme_Rational *r = (Scheme_Rational *)o;

  if (SCHEME_INTP(r->num))
    return (SCHEME_INT_VAL(r->num) > 0);
  else 
    return SCHEME_BIGPOS(r->num);
}

Scheme_Object *scheme_rational_normalize(const Scheme_Object *o)
{
  Scheme_Rational *r = (Scheme_Rational *)o;
  Scheme_Object *gcd, *tmpn;
  int negate = 0;

  if (r->num == scheme_make_integer(0))
    return scheme_make_integer(0);

  if (SCHEME_INTP(r->denom)) {
    if (SCHEME_INT_VAL(r->denom) < 0) {
      r->denom = scheme_make_integer(-SCHEME_INT_VAL(r->denom));
      negate = 1;
    }
  } else if (!SCHEME_BIGPOS(r->denom)) {
    tmpn = scheme_bignum_negate(r->denom);
    r->denom = tmpn;
    negate = 1;
  }

  if (negate) {
    if (SCHEME_INTP(r->num))
      r->num = scheme_make_integer(-SCHEME_INT_VAL(r->num));
    else {
      tmpn = scheme_bignum_negate(r->num);
      r->num = tmpn;
    }
  }
  
  if (r->denom == one)
    return r->num;

  gcd = scheme_bin_gcd(r->num, r->denom);

  if (gcd == one)
    return (Scheme_Object *)o;

  tmpn = scheme_bin_quotient(r->num, gcd);
  r->num = tmpn;
  tmpn = scheme_bin_quotient(r->denom, gcd);
  r->denom = tmpn;

  if (r->denom == one)
    return r->num;

  return (Scheme_Object *)r;
}

Scheme_Object *scheme_rational_numerator(const Scheme_Object *n)
{
  return ((Scheme_Rational *)n)->num;
}

Scheme_Object *scheme_rational_denominator(const Scheme_Object *n)
{
  return ((Scheme_Rational *)n)->denom;
}

Scheme_Object *scheme_make_fixnum_rational(long n, long d)
{
  return scheme_make_rational(scheme_make_integer(n),
			      scheme_make_integer(d));
}

int scheme_rational_eq(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Rational *ra = (Scheme_Rational *)a;
  Scheme_Rational *rb = (Scheme_Rational *)b;

  if (SCHEME_INTP(ra->num) && SCHEME_INTP(rb->num)) {
    if (ra->num != rb->num)
      return 0;
  } else if (SCHEME_BIGNUMP(ra->num) && SCHEME_BIGNUMP(rb->num)) {
    if (!scheme_bignum_eq(ra->num, rb->num))
      return 0;
  } else
    return 0;

  if (SCHEME_INTP(ra->denom) && SCHEME_INTP(rb->denom)) {
    if (ra->denom != rb->denom)
      return 0;
  } else if (SCHEME_BIGNUMP(ra->denom) && SCHEME_BIGNUMP(rb->denom)) {
    if (!scheme_bignum_eq(ra->denom, rb->denom))
      return 0;
  } else
    return 0;

  return 1;
}

int scheme_rational_lt(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Rational *ra = (Scheme_Rational *)a;
  Scheme_Rational *rb = (Scheme_Rational *)b;
  Scheme_Object *ma, *mb;

  ma = scheme_bin_mult(ra->num, rb->denom);
  mb = scheme_bin_mult(rb->num, ra->denom);

  if (SCHEME_INTP(ma) && SCHEME_INTP(mb)) {
    return (SCHEME_INT_VAL(ma) < SCHEME_INT_VAL(mb));
  } else if (SCHEME_BIGNUMP(ma) && SCHEME_BIGNUMP(mb)) {
    return scheme_bignum_lt(ma, mb);
  } else if (SCHEME_BIGNUMP(mb)) {
    return SCHEME_BIGPOS(mb);
  } else
    return !SCHEME_BIGPOS(ma);
}

int scheme_rational_gt(const Scheme_Object *a, const Scheme_Object *b)
{
  return !scheme_rational_lt(a, b) && !scheme_rational_eq(a, b);
}

int scheme_rational_le(const Scheme_Object *a, const Scheme_Object *b)
{
  return !scheme_rational_gt(a, b);
}

int scheme_rational_ge(const Scheme_Object *a, const Scheme_Object *b)
{
  return !scheme_rational_lt(a, b);
}

Scheme_Object *scheme_rational_negate(const Scheme_Object *o)
{
  Scheme_Rational *r = (Scheme_Rational *)o;

  return make_rational(scheme_bin_minus(scheme_make_integer(0),
					r->num), 
		       r->denom, 0);
}

Scheme_Object *scheme_rational_add(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Rational *ra = (Scheme_Rational *)a;
  Scheme_Rational *rb = (Scheme_Rational *)b;
  Scheme_Object *ac, *bd, *sum, *cd;

  ac = scheme_bin_mult(ra->num, rb->denom);
  bd = scheme_bin_mult(ra->denom, rb->num);
  sum = scheme_bin_plus(ac, bd);
  cd = scheme_bin_mult(ra->denom, rb->denom);

  return scheme_make_rational(sum, cd);
}

Scheme_Object *scheme_rational_subtract(const Scheme_Object *a, const Scheme_Object *b)
{
  return scheme_rational_add(a, scheme_rational_negate(b));
}

Scheme_Object *scheme_rational_add1(const Scheme_Object *n)
{
  Small_Rational s;

  return scheme_rational_add(scheme_make_small_rational(1, &s), n);
}

Scheme_Object *scheme_rational_sub1(const Scheme_Object *n)
{
  Small_Rational s;

  return scheme_rational_add(n, scheme_make_small_rational(-1, &s));
}

Scheme_Object *scheme_rational_multiply(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Rational *ra = (Scheme_Rational *)a;
  Scheme_Rational *rb = (Scheme_Rational *)b;
  Scheme_Object *ab, *cd;

  ab = scheme_bin_mult(ra->num, rb->num);
  cd = scheme_bin_mult(ra->denom, rb->denom);

  return scheme_make_rational(ab, cd);  
}

Scheme_Object *scheme_rational_max(const Scheme_Object *a, const Scheme_Object *b)
{
  int lt;
  lt = scheme_rational_lt(a, b);
  return scheme_rational_normalize(lt ? b : a);
}

Scheme_Object *scheme_rational_min(const Scheme_Object *a, const Scheme_Object *b)
{
  int lt;
  lt = scheme_rational_lt(a, b);
  return scheme_rational_normalize(lt ? a : b);
}

Scheme_Object *scheme_rational_divide(const Scheme_Object *n, const Scheme_Object *d)
{ 
  Scheme_Rational *rd = (Scheme_Rational *)d, d_inv;
  
  d_inv.type = scheme_rational_type;
  d_inv.denom = rd->num;
  d_inv.num = rd->denom;

  return scheme_rational_multiply(n, (Scheme_Object *)&d_inv);
}

Scheme_Object *scheme_generic_integer_power(const Scheme_Object *o, const Scheme_Object *p)
{
  Scheme_Object *r = one, *zero = scheme_make_integer(0);
  Scheme_Object *two = scheme_make_integer(2);
  char *bitstream;
  int bs_size = 0, bs_alloc = 10;

  bitstream = (char *)scheme_malloc_atomic(bs_alloc);

  while (p != zero) {
    if (bs_size >= bs_alloc) {
      char *old = bitstream;

      bs_alloc *= 2;
      bitstream = (char *)scheme_malloc_atomic(bs_alloc);
      memcpy(bitstream, old, bs_size);
    }

    if (SCHEME_TRUEP(scheme_odd_p(1, (Scheme_Object **)&p))) {
      bitstream[bs_size] = 1;
      p = scheme_sub1(1, (Scheme_Object **)&p);
    } else
      bitstream[bs_size] = 0;

    bs_size++;
    p = scheme_bin_quotient(p, two);
  }

  while (bs_size--) {
    r = scheme_bin_mult(r, r);
    if (bitstream[bs_size])
      r = scheme_bin_mult(r, o);
  }

  return r;
}

Scheme_Object *scheme_rational_power(const Scheme_Object *o, const Scheme_Object *p)
{
  double b, e;

  if (((Scheme_Rational *)p)->denom == one)
    return scheme_generic_integer_power(o, ((Scheme_Rational *)p)->num);

  if (scheme_is_rational_positive(o)) {
    b = scheme_rational_to_double(o);
    e = scheme_rational_to_double(p);
    
    return scheme_make_double(pow(b, e));
  } else {
    return scheme_complex_power(scheme_real_to_complex(o),
				scheme_real_to_complex(p));
  }
}

Scheme_Object *scheme_rational_truncate(const Scheme_Object *o)
{
  Scheme_Rational *r = (Scheme_Rational *)o;

  return scheme_bin_quotient(r->num, r->denom);
}

Scheme_Object *scheme_rational_floor(const Scheme_Object *o)
{
  if (scheme_is_rational_positive(o))
    return scheme_rational_truncate(o);
  else {
    Scheme_Object *r;
    r = scheme_rational_truncate(o);
    return scheme_sub1(1, &r);
  }
}

Scheme_Object *scheme_rational_ceiling(const Scheme_Object *o)
{
  if (!scheme_is_rational_positive(o))
    return scheme_rational_truncate(o);
  else {
    Scheme_Object *r;
    r = scheme_rational_truncate(o);
    return scheme_add1(1, &r);
  }  
}

Scheme_Object *scheme_rational_round(const Scheme_Object *o)
{
  Scheme_Rational *r = (Scheme_Rational *)o;
  Scheme_Object *q, *qd, *delta, *half;
  int more = 0, can_eq_half, negative;

  negative = !scheme_is_rational_positive(o);
  
  q = scheme_bin_quotient(r->num, r->denom);

  /* Get remainder absolute value: */
  qd = scheme_bin_mult(q, r->denom);
  if (negative)
    delta = scheme_bin_minus(qd, r->num);
  else
    delta = scheme_bin_minus(r->num, qd);

  half = scheme_bin_quotient(r->denom, scheme_make_integer(2));
  can_eq_half = SCHEME_FALSEP(scheme_odd_p(1, &r->denom));

  if (SCHEME_INTP(half) && SCHEME_INTP(delta)) {
    if (can_eq_half && (SCHEME_INT_VAL(delta) == SCHEME_INT_VAL(half)))
      more = SCHEME_TRUEP(scheme_odd_p(1, &q));
    else
      more = (SCHEME_INT_VAL(delta) > SCHEME_INT_VAL(half));
  } else if (SCHEME_BIGNUMP(delta) && SCHEME_BIGNUMP(half)) {
    if (can_eq_half && (scheme_bignum_eq(delta, half)))
      more = SCHEME_TRUEP(scheme_odd_p(1, &q));      
    else
      more = !scheme_bignum_lt(delta, half);
  } else
    more = SCHEME_BIGNUMP(delta);

  if (more) {
    if (negative)
      q = scheme_sub1(1, &q);
    else
      q = scheme_add1(1, &q);      
  }

  return q;
}


Scheme_Object *scheme_rational_sqrt(const Scheme_Object *o)
{
  Scheme_Rational *r = (Scheme_Rational *)o;
  Scheme_Object *n, *d;

  n = scheme_integer_sqrt(r->num);
  if (!SCHEME_DBLP(n)) {
    d = scheme_integer_sqrt(r->denom);
    if (!SCHEME_DBLP(d))
      return make_rational(n, d, 0);
  }

  return scheme_make_double(sqrt(scheme_rational_to_float(o)));
}

#define FP_TYPE double
#define SCHEME_RATIONAL_TO_FLOAT scheme_rational_to_double
#define SCHEME_RATIONAL_FROM_FLOAT scheme_rational_from_double
#define SCHEME_BIGNUM_TO_FLOAT_INF_INFO scheme_bignum_to_double_inf_info
#define SCHEME_CHECK_FLOAT scheme_check_double
#define SCHEME_BIGNUM_FROM_FLOAT scheme_bignum_from_double
#define DO_FLOAT_DIV do_double_div
#include "ratfloat.inc"

#ifdef MZ_USE_SINGLE_FLOATS
# undef FP_TYPE
# undef SCHEME_RATIONAL_TO_FLOAT
# undef SCHEME_RATIONAL_FROM_FLOAT
# undef SCHEME_BIGNUM_TO_FLOAT_INF_INFO
# undef SCHEME_BIGNUM_FROM_FLOAT
# undef SCHEME_CHECK_FLOAT

#define FP_TYPE float
#define SCHEME_RATIONAL_TO_FLOAT scheme_rational_to_float
#define SCHEME_RATIONAL_FROM_FLOAT scheme_rational_from_float
#define SCHEME_BIGNUM_TO_FLOAT_INF_INFO scheme_bignum_to_float_inf_info
#define SCHEME_CHECK_FLOAT scheme_check_float
#define SCHEME_BIGNUM_FROM_FLOAT scheme_bignum_from_float
#define DO_FLOAT_DIV do_float_div
#include "ratfloat.inc"
#endif

