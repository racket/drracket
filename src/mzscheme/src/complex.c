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

static Scheme_Object *zero = scheme_make_integer(0);

static Scheme_Object *make_complex(const Scheme_Object *r, const Scheme_Object *i,
				   int normalize)
{
  Scheme_Complex *c;

  c = MALLOC_ONE_TAGGED(Scheme_Complex);
  c->type = scheme_complex_type;
  c->r = (Scheme_Object *)r;
  c->i = (Scheme_Object *)i;
  
  return normalize 
    ? scheme_complex_normalize((Scheme_Object *)c) 
      : (Scheme_Object *)c;
}

Scheme_Object *scheme_make_complex(const Scheme_Object *r, const Scheme_Object *i)
{
  return make_complex(r, i, 1);
}

Scheme_Object *scheme_real_to_complex(const Scheme_Object *n)
{
  return make_complex(n, (SCHEME_DBLP(n) 
			  ? scheme_make_double(0.0) 
#ifdef MZ_USE_SINGLE_FLOATS
			  : SCHEME_FLTP(n)
			  ? scheme_make_float(0.0f)
#endif
			  : zero), 
		      0);
}

Scheme_Object *scheme_make_small_complex(const Scheme_Object *n, Small_Complex *s)
{
  s->type = scheme_complex_type;
  s->r = (Scheme_Object *)n;
  s->i = (SCHEME_DBLP(n) 
	  ? scheme_make_double(0.0) 
#ifdef MZ_USE_SINGLE_FLOATS
	  : SCHEME_FLTP(n)
	  ? scheme_make_float(0.0f)
#endif
	  : zero);

  return (Scheme_Object *)s;
}

int scheme_is_complex_exact(const Scheme_Object *o)
{
  Scheme_Complex *c = (Scheme_Complex *)o;

  return !SCHEME_FLOATP(c->r);
}

Scheme_Object *scheme_complex_normalize(const Scheme_Object *o)
{
  Scheme_Complex *c = (Scheme_Complex *)o;

  if (SCHEME_DBLP(c->i)) {
    if (SCHEME_DBL_VAL(c->i) == 0.0)
      return c->r;
#ifdef MZ_USE_SINGLE_FLOATS
  } else if (SCHEME_FLTP(c->i)) {
    if (SCHEME_FLT_VAL(c->i) == 0.0f)
      return c->r;
#endif
  } else if (c->i == zero)
    return c->r;

  return (Scheme_Object *)o;
}

Scheme_Object *scheme_complex_real_part(const Scheme_Object *n)
{
  return ((Scheme_Complex *)n)->r;
}

Scheme_Object *scheme_complex_imaginary_part(const Scheme_Object *n)
{
  return ((Scheme_Complex *)n)->i;
}

int scheme_complex_eq(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Complex *ca = (Scheme_Complex *)a;
  Scheme_Complex *cb = (Scheme_Complex *)b;
  return scheme_bin_eq(ca->r, cb->r) && scheme_bin_eq(ca->i, cb->i);
}

Scheme_Object *scheme_complex_negate(const Scheme_Object *o)
{
  Scheme_Complex *c = (Scheme_Complex *)o;

  return make_complex(scheme_bin_minus(scheme_make_integer(0),
				       c->r), 
		      scheme_bin_minus(scheme_make_integer(0),
				       c->i),
		      0);
}

Scheme_Object *scheme_complex_add(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Complex *ca = (Scheme_Complex *)a;
  Scheme_Complex *cb = (Scheme_Complex *)b;

  return scheme_make_complex(scheme_bin_plus(ca->r, cb->r),
			     scheme_bin_plus(ca->i, cb->i));
}

Scheme_Object *scheme_complex_subtract(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Complex *ca = (Scheme_Complex *)a;
  Scheme_Complex *cb = (Scheme_Complex *)b;

  return scheme_make_complex(scheme_bin_minus(ca->r, cb->r),
			     scheme_bin_minus(ca->i, cb->i));
}

Scheme_Object *scheme_complex_add1(const Scheme_Object *n)
{
  Small_Complex s;

  return scheme_complex_add(scheme_make_small_complex(scheme_make_integer(1), &s), 
			    n);
}

Scheme_Object *scheme_complex_sub1(const Scheme_Object *n)
{
  Small_Complex s;

  return scheme_complex_add(n, scheme_make_small_complex(scheme_make_integer(-1), 
							 &s));
}

Scheme_Object *scheme_complex_multiply(const Scheme_Object *a, const Scheme_Object *b)
{
  Scheme_Complex *ca = (Scheme_Complex *)a;
  Scheme_Complex *cb = (Scheme_Complex *)b;

  return scheme_make_complex(scheme_bin_minus(scheme_bin_mult(ca->r, cb->r),
					      scheme_bin_mult(ca->i, cb->i)),
			     scheme_bin_plus(scheme_bin_mult(ca->r, cb->i),
					     scheme_bin_mult(ca->i, cb->r)));
  
}

static int zero_p(const Scheme_Object *v)
{
  return ((v == zero) 
#ifdef MZ_USE_SINGLE_FLOATS
	  || (SCHEME_FLTP(v) && SCHEME_FLT_VAL(v) == 0.0f)
#endif
	  || (SCHEME_DBLP(v) && SCHEME_DBL_VAL(v) == 0.0));
}

Scheme_Object *scheme_complex_divide(const Scheme_Object *n, const Scheme_Object *d)
{ 
  Scheme_Complex *cn = (Scheme_Complex *)n;
  Scheme_Complex *cd = (Scheme_Complex *)d;
  Scheme_Object *a_sq_p_b_sq, *r, *i;
  
  if (zero_p(cn->r) && zero_p(cn->i))
    return zero;

  a_sq_p_b_sq = scheme_bin_plus(scheme_bin_mult(cd->r, cd->r), 
				scheme_bin_mult(cd->i, cd->i));
  
  r = scheme_bin_div(scheme_bin_plus(scheme_bin_mult(cd->r, cn->r),
				     scheme_bin_mult(cd->i, cn->i)),
		     a_sq_p_b_sq);
  i = scheme_bin_div(scheme_bin_minus(scheme_bin_mult(cd->r, cn->i),
				      scheme_bin_mult(cd->i, cn->r)),
		     a_sq_p_b_sq);
  
  return scheme_make_complex(r, i);
}

Scheme_Object *scheme_complex_power(const Scheme_Object *base, const Scheme_Object *exponent)
{
  Scheme_Complex *cb = (Scheme_Complex *)base;
  Scheme_Complex *ce = (Scheme_Complex *)exponent;
  double a, b, c, d, bm, ba, nm, na, r1, r2;

  if (ce->i == zero)
    return scheme_generic_power(base, ce->r);

  a = scheme_get_val_as_double(cb->r);
  b = scheme_get_val_as_double(cb->i);
  c = scheme_get_val_as_double(ce->r);
  d = scheme_get_val_as_double(ce->i);

  bm = sqrt(a * a + b * b);
  ba = atan2(b, a);

  /* New mag & angle */
  nm = pow(bm, c) * exp(-(ba * d));
  na = log(bm) * d + ba * c;

  r1 = nm * cos(na);
  r2 = nm * sin(na);

#ifdef MZ_USE_SINGLE_FLOATS
  /* Coerce to double or float? */
#ifdef USE_SINGLE_FLOATS_AS_DEFAULT
  if (!SCHEME_DBLP(cb->r) && !SCHEME_DBLP(cb->i)
      && !SCHEME_DBLP(ce->r) && !SCHEME_DBLP(ce->i))
#else
  if (SCHEME_FLTP(cb->r) && SCHEME_FLTP(cb->i)
      && SCHEME_FLTP(ce->r) && SCHEME_FLTP(ce->i))
#endif
    return scheme_make_complex(scheme_make_float((float)r1), 
			       scheme_make_float((float)r2));
#endif

  return scheme_make_complex(scheme_make_double(r1), 
			     scheme_make_double(r2));
}

Scheme_Object *scheme_complex_sqrt(const Scheme_Object *o)
{
  Scheme_Complex *c = (Scheme_Complex *)o;
  Scheme_Object *r, *i, *ssq, *srssq, *nrsq, *nr, *ni;

  r = c->r;
  i = c->i;

  ssq = scheme_bin_plus(scheme_bin_mult(r, r),
			scheme_bin_mult(i, i));

  srssq = scheme_sqrt(1, &ssq);

  nrsq = scheme_bin_div(scheme_bin_minus(srssq, r),
			scheme_make_integer(2));

  nr = scheme_sqrt(1, &nrsq);
  
  ni = scheme_bin_div(i, scheme_bin_mult(nr,
					 scheme_make_integer(2)));

  /* Choose root with positive real part: */
  if (scheme_bin_lt(ni, zero)) {
    return scheme_make_complex(scheme_bin_minus(zero, ni), 
			       scheme_bin_minus(zero, nr));
  } else
    return scheme_make_complex(ni, nr);
}
