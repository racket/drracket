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


  Some algorithms imitate VSCM:

   (C) m.b (Matthias Blume); May 1992, HUB; Jan 1993 PU/CS
           Humboldt-University of Berlin
           Princeton University, Dept. of Computer Science
*/

/* Precise GC warning: SCHEME_BIGDIG() can return an interior pointer,
   if the bignum is a Small_Bignum */

#include "schpriv.h"
#include <ctype.h>
#include <math.h>

#ifdef SIXTY_FOUR_BIT_INTEGERS
# define BIG_MAX 0x3FFFFFFFFFFFFFFF
# define BIG_RADIX 0x4000000000000000
# define LOG_BIG_RADIX 62
# define QUICK_DIV_MAX 0x7FFFFFFF

/* Need to work with halves for mult & div: */
# define BIG_LO_HALF 0x7FFFFFFF
# define LOG_BIG_LO 31

# define BIG_DIG_DEC_DIG 21
# define SMALL_NUM_STR_LEN 20 /* conservatively low is OK */
# define TRANS_DIG_DEC 100000000
# define TRANS_DIG_DEC_DIG 9
#else
# define BIG_MAX 0x3FFFFFFF
# define BIG_RADIX 0x40000000
# define LOG_BIG_RADIX 30
# define QUICK_DIV_MAX 0x7FFF

/* Need to work with halves for mult & div: */
# define BIG_LO_HALF 0x7FFF
# define LOG_BIG_LO 15

# define BIG_DIG_DEC_DIG 10
# define SMALL_NUM_STR_LEN 10 /* conservatively low is OK */
# define TRANS_DIG_DEC 10000
# define TRANS_DIG_DEC_DIG 5
#endif

static Scheme_Object *bignum_none = NULL;
static Scheme_Object *bignum_one = NULL;
static Scheme_Object *decimal_digits[17];

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

Scheme_Object *scheme_make_small_bignum(long v, Small_Bignum *o)
{
  o->o.type = scheme_bignum_type;
#if MZ_PRECISE_GC
  o->o.allocated_inline = 1;
#endif  
  SCHEME_BIGPOS(&o->o) = ((v >= 0) ? 1 : 0);
  if (v < 0)
    v = -v;

  if (!v)
    SCHEME_BIGLEN(&o->o) = 0;
  else
    SCHEME_BIGLEN(&o->o) = 1;

  SCHEME_BIGDIG(&o->o) = o->v;

  o->v[0] = v;

  return (Scheme_Object *)o;
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

Scheme_Object *scheme_make_bignum(long v)
{
  if (v <= BIG_MAX && v >= -BIG_MAX) {
    Small_Bignum *r;
    r = MALLOC_ONE_TAGGED(Small_Bignum);
    return scheme_make_small_bignum(v, r);
  } else {
    Small_Bignum *o;
    int bad_neg = 0;

    /* There's one negative number in twos complement without a
       positive counterpart. In fact, if you take the negative of that
       number, you get back the same number: */
    if (v && (v == -v)) {
      bad_neg = 1;
      v++;
    }

    o = (Small_Bignum *)scheme_malloc_tagged(sizeof(Small_Bignum) + sizeof(bigdig));
    o->o.type = scheme_bignum_type;
#if MZ_PRECISE_GC
    o->o.allocated_inline = 2;
#endif  

    SCHEME_BIGDIG(&o->o) = o->v;

    SCHEME_BIGPOS(&o->o) = ((v >= 0) ? 1 : 0);
    if (v < 0)
      v = -v;
    
    o->v[0] = (v & BIG_MAX);
    o->v[1] = (v >> LOG_BIG_RADIX);

    SCHEME_BIGLEN(&o->o) = 2;

    if (bad_neg) {
      o->v[0]++;
      o->v[1] += (o->v[0] >> LOG_BIG_RADIX);
      o->v[0] = (o->v[0] & BIG_MAX);
    }

    return (Scheme_Object *)o;
  }
}

Scheme_Object *scheme_make_bignum_from_unsigned(unsigned long v)
{
  if (v <= BIG_MAX) {
    Small_Bignum *r;
    r = MALLOC_ONE_TAGGED(Small_Bignum);
    return scheme_make_small_bignum(v, r);
  } else {
    Small_Bignum *o;

    o = (Small_Bignum *)scheme_malloc_tagged(sizeof(Small_Bignum) + sizeof(bigdig));
    o->o.type = scheme_bignum_type;
#if MZ_PRECISE_GC
    o->o.allocated_inline = 2;
#endif  

    SCHEME_BIGDIG(&o->o) = o->v;

    SCHEME_BIGPOS(&o->o) = 1;
    
    o->v[0] = (v & BIG_MAX);
    o->v[1] = (v >> LOG_BIG_RADIX);

    SCHEME_BIGLEN(&o->o) = 2;
    
    return (Scheme_Object *)o;
  }
}

int scheme_bignum_get_int_val(const Scheme_Object *o, long *v)
{
  long n;
  bigdig *a;

  if (SCHEME_BIGLEN(o) > 2)
    return 0;

  a = SCHEME_BIGDIG(o);

  if (a[1] > 0x1) {
    if (!SCHEME_BIGPOS(o)) {
      if ((a[1] == 0x2) && !a[0]) {
	/* Special case: the one negative number whose negation
	   doesn't fit in a long: */
	unsigned long m;
	m = (unsigned long)0x2 << LOG_BIG_RADIX;
	*v = m;
	return 1;
      }
    }
    return 0;
  }

  n = (a[1] << LOG_BIG_RADIX) | a[0];
  if (!SCHEME_BIGPOS(o))
    n = -n;
  *v = n;

  return 1;
}

int scheme_bignum_get_unsigned_int_val(const Scheme_Object *o, unsigned long *v)
{
  bigdig *a;

  if ((SCHEME_BIGLEN(o) > 2) || !SCHEME_BIGPOS(o))
    return 0;

  a = SCHEME_BIGDIG(o);

  if (a[1] > 0x3)
    return 0;

  *v = (a[1] << LOG_BIG_RADIX) | a[0];

  return 1;
}

Scheme_Object *scheme_bignum_normalize(const Scheme_Object *o)
{
  long v;
  
  if (!SCHEME_BIGNUMP(o))
    return (Scheme_Object *)o;

  v = SCHEME_BIGLEN(o);

  if (v > 1)
    return (Scheme_Object *)o;

  if (!v)
    return scheme_make_integer(0);

  v = SCHEME_BIGDIG(o)[0];

  if (!SCHEME_BIGPOS(o))
    v = -v;

  return scheme_make_integer(v);
}

int scheme_bignum_eq(const Scheme_Object *a, const Scheme_Object *b)
{
  int al, bl;
  const bigdig *aa, *ba;

  if (SCHEME_BIGPOS(a) != SCHEME_BIGPOS(b))
    return 0;

  al = SCHEME_BIGLEN(a);
  bl = SCHEME_BIGLEN(b);
  if (al != bl)
    return 0;

  aa = SCHEME_BIGDIG(a);
  ba = SCHEME_BIGDIG(b);
  
  /* Why is this code here? (How can al be negative?)
     03/19/2000 */
  if (al < 0)
    al = -al;

  while (al--) {
    if (*(aa++) != *(ba++))
      return 0;
  }

  return 1;
}

int scheme_bignum_lt(const Scheme_Object *a, const Scheme_Object *b)
{
  int ap, bp, al, bl;
  const bigdig *aa, *ba;
  bigdig av, bv;

  ap = SCHEME_BIGPOS(a);
  bp = SCHEME_BIGPOS(b);
  if (ap && !bp)
    return 0;
  if (!ap && bp)
    return 1;

  if (!ap) {
    const Scheme_Object *save;

    save = a;
    a = b;
    b = save;
  }

  al = SCHEME_BIGLEN(a);
  bl = SCHEME_BIGLEN(b);
  if (al != bl)
    return (al < bl);

  aa = SCHEME_BIGDIG(a) + al;
  ba = SCHEME_BIGDIG(b) + al;
  
  while (al--) {
    av = *(--aa);
    bv = *(--ba);
    if (av != bv)
      return (av < bv);
  }

  return 0;
}

int scheme_bignum_gt(const Scheme_Object *a, const Scheme_Object *b)
{
  return scheme_bignum_lt(b, a);
}

int scheme_bignum_le(const Scheme_Object *a, const Scheme_Object *b)
{
  return !scheme_bignum_gt(a, b);
}

int scheme_bignum_ge(const Scheme_Object *a, const Scheme_Object *b)
{
  return !scheme_bignum_lt(a, b);
}

Scheme_Object *scheme_bignum_negate(const Scheme_Object *n)
{
  Scheme_Object *o;

  o = (Scheme_Object *)MALLOC_ONE_TAGGED(Scheme_Bignum);

  o->type = scheme_bignum_type;
  SCHEME_BIGPOS(o) = !SCHEME_BIGPOS(n);
  SCHEME_BIGLEN(o) = SCHEME_BIGLEN(n);
  SCHEME_BIGDIG(o) = SCHEME_BIGDIG(n);
 
  return o;
}

static void bignum_negate_inplace(Scheme_Object *n)
{
  SCHEME_BIGPOS(n) = !SCHEME_BIGPOS(n);
}

static Scheme_Object *bignum_copy(const Scheme_Object *n, int copy_array)
{
  Scheme_Object *o;
  int c;

  c = SCHEME_BIGLEN(n);

  o = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Bignum));

  o->type = scheme_bignum_type;
  SCHEME_BIGLEN(o) = c;
  SCHEME_BIGPOS(o) = SCHEME_BIGPOS(n);
  if (copy_array) {
    bigdig *a;
    a = (bigdig *)scheme_malloc_atomic(sizeof(bigdig) 
				       * (c + (copy_array - 1)));
    SCHEME_BIGDIG(o) = a;
    memcpy(SCHEME_BIGDIG(o), SCHEME_BIGDIG(n), sizeof(bigdig) * c);
  } else
    SCHEME_BIGDIG(o) = SCHEME_BIGDIG(n);

  return o;
}

static Scheme_Object *bignum_add(Scheme_Object *o, bigdig **buffer, int *size,
				 const Scheme_Object *a, const Scheme_Object *b, int norm)
{
  int ap, bp, vp;
  int al, bl, vl, i, same;
  long s, av, bv, carry;
  bigdig *aa, *ba, *va;

  al = SCHEME_BIGLEN(a);
  bl = SCHEME_BIGLEN(b);
  ap = SCHEME_BIGPOS(a);
  bp = SCHEME_BIGPOS(b);

  /* Because bignum calculations are not bounded: */
  SCHEME_USE_FUEL(2 * al);

  same = (ap == bp);

  vl = ((al > bl) ? al : bl);

  if (same)
    vl++;
 
  if (buffer) {
    if (*size < vl) {
      bigdig *bufa;
      *size = 2 * vl;
      bufa = (bigdig *)scheme_malloc_atomic(*size * sizeof(bigdig));
      *buffer = bufa;
    }
    va = *buffer;
  } else
    va = (bigdig *)scheme_malloc_atomic(vl * sizeof(bigdig));

  if (!o) {
    o = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Bignum));
    o->type = scheme_bignum_type;
  }

  /* All allocation is done... can get aa and ba: */

  aa = SCHEME_BIGDIG(a);
  ba = SCHEME_BIGDIG(b);
  
  if (!same) {
    int reverse, sl, sp;
    bigdig *sa;

    if (al == bl) {
      reverse = 0;
      for (i = al; i--; ) {
	if (aa[i] != ba[i]) {
	  reverse = (aa[i] < ba[i]);
	  break;
	}
      }
    } else
      reverse = (al < bl);
    
    if (reverse) {
      sl = al;
      al = bl;
      bl = sl;
      sa = aa;
      aa = ba;
      ba = sa;
      sp = ap;
      ap = bp;
      bp = sp;
    }
  }
    
  carry = 0;
  for (i = 0; i < vl; i++) {
    av = (i >= al) ? 0 : aa[i];
    bv = (i >= bl) ? 0 : ba[i];
      
    if (same)
      s = (long)av + (long)bv + carry;
    else
      s = (long)av - (long)bv + carry;
    
    if (s < 0) {
      carry = -1;
      s += BIG_RADIX;
    } else {
      carry = (s & BIG_RADIX) ? 1 : 0;
      s = s & BIG_MAX;
    }
    
    va[i] = s;
  }

  /* Get rid of leading zeros: */
  for (i = vl; i--; ) {
    if (va[i])
      break;
  }
  vl = i + 1;

  vp = ap;
  
  SCHEME_BIGPOS(o) = vp;
  SCHEME_BIGLEN(o) = vl;
  SCHEME_BIGDIG(o) = va;

  if (norm)
    return scheme_bignum_normalize(o);
  else
    return o;
}

Scheme_Object *scheme_bignum_add(const Scheme_Object *a, const Scheme_Object *b)
{
  return bignum_add(NULL, NULL, NULL, a, b, 1);
}

Scheme_Object *scheme_bignum_subtract(const Scheme_Object *a, const Scheme_Object *b)
{
  return scheme_bignum_add(a, scheme_bignum_negate(b));
}

Scheme_Object *scheme_bignum_add1(const Scheme_Object *n)
{
  if (!bignum_one) {
    REGISTER_SO(bignum_one);
    bignum_one = scheme_make_bignum(1);
  }

  return scheme_bignum_add(n, bignum_one);
}

Scheme_Object *scheme_bignum_sub1(const Scheme_Object *n)
{
  if (!bignum_none) {
    REGISTER_SO(bignum_none);
    bignum_none = scheme_make_bignum(-1);
  }

  return scheme_bignum_add(n, bignum_none);
}

static void bignum_double_inplace(Scheme_Object *n, int bs)
{
  int nl, i;
  bigdig *na, *naya, carry;

  nl = SCHEME_BIGLEN(n);
  na = SCHEME_BIGDIG(n);
  
  /* Because bignum calculations are not bounded: */
  SCHEME_USE_FUEL(nl);

  carry = 0;
  for (i = 0; i < nl; i++) {
    na[i] = (na[i] << 1) + carry;
    carry = (na[i] & BIG_RADIX) ? 1 : 0;
    if (carry)
      na[i] = na[i] & BIG_MAX;
  }

  if (carry) {
    if (bs < nl + 1) {
      /* expand */
      na = NULL; /* Might be a pointer into the middle of a small bignum */
      naya = (bigdig *)scheme_malloc_atomic(sizeof(bigdig) * (nl + 1));
      na = SCHEME_BIGDIG(n);

      for (i = 0; i < nl; i++) {
	naya[i] = na[i];
      }
      SCHEME_BIGDIG(n) = naya;
    } else
      naya = na;

    naya[nl] = 1;

    SCHEME_BIGLEN(n) = nl + 1;
  }
}

static void bignum_half_inplace(Scheme_Object *n)
{
  int nl, i;
  bigdig *na, carry, next;
  
  nl = SCHEME_BIGLEN(n);
  na = SCHEME_BIGDIG(n);
  
  carry = 0;
  for (i = nl; i--; ) {
    next = (na[i] & 1) ? (BIG_RADIX >> 1) : 0;
    na[i] = (na[i] >> 1) + carry;
    carry = next;
  }

  if (nl && !na[nl - 1]) {
    --nl;
    SCHEME_BIGLEN(n) = nl;
  }
}

static Scheme_Object *bignum_multiply(Small_Bignum *rsmall,
				      const Scheme_Object *a, const Scheme_Object *b, 
				      int norm)
{
  Scheme_Object *r;
  int bl, al, i, k;
  int size;
  bigdig *ba, *aa, *buffer;
  unsigned long carry;

  bl = SCHEME_BIGLEN(b);
  al = SCHEME_BIGLEN(a);

  size = bl + al;
  buffer = (bigdig *)scheme_malloc_atomic(size * sizeof(bigdig));

  if (al < bl) {
    const Scheme_Object *aux;
    int sl;

    aux = a;
    a = b;
    b = aux;
    
    sl = bl;
    bl = al;
    al = sl;
  }

  ba = SCHEME_BIGDIG(b);

  if ((bl == 1) && (ba[0] < BIG_LO_HALF)) {
    /* Fast simplified version */
    unsigned long m = ba[0];

    ba = NULL;
    SCHEME_USE_FUEL(al);
    aa = SCHEME_BIGDIG(a);

    carry = 0;
    for (i = 0; i < al; i++) {
      unsigned long a, loa, mida;

      a = aa[i];
      loa = a & BIG_LO_HALF;
      mida = a >> LOG_BIG_LO;

      loa *= m;
      mida *= m;
      
      loa += carry + ((mida & BIG_LO_HALF) << LOG_BIG_LO);
      carry = mida >> LOG_BIG_LO;

      if (loa > BIG_MAX) {
	carry += (loa >> LOG_BIG_RADIX);
	loa = loa & BIG_MAX;
      }
      
      buffer[i] = loa;
    }
    buffer[al] = carry;
  } else {
    memset(buffer, 0, size * sizeof(bigdig));

    for (i = 0; i < bl; i++) {
      aa = ba = NULL;
      SCHEME_USE_FUEL(al);
      aa = SCHEME_BIGDIG(a);
      ba = SCHEME_BIGDIG(b);

      carry = 0;
      for (k = 0; k < al; k++) {
	long a, b, loa, lob, hia, hib;
	unsigned long los, his, mids;
	
	a = aa[k];
	loa = a & BIG_LO_HALF;
	hia = a >> LOG_BIG_LO;
	b = ba[i];
	lob = b & BIG_LO_HALF;
	hib = b >> LOG_BIG_LO;
	
	los = loa * lob;
	his = hia * hib;
	
	mids = loa * hib + lob * hia;
	
	los += ((mids & BIG_LO_HALF) << LOG_BIG_LO) + carry + buffer[i + k];
	his += (mids >> LOG_BIG_LO);
	
	if (los > BIG_MAX) {
	  his += (los >> LOG_BIG_RADIX);
	  los = los & BIG_MAX;
	}
	carry = his;
	
	buffer[i + k] = los;
      }
      buffer[i + k] = carry;
    }
  }

  while (size && !buffer[size - 1]) {
    --size;
  }

  if (rsmall)
    r = (Scheme_Object *)rsmall;
  else
    r = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Bignum));
  r->type = scheme_bignum_type;
  SCHEME_BIGPOS(r) = ((SCHEME_BIGPOS(a) && SCHEME_BIGPOS(b))
		      || (!SCHEME_BIGPOS(a) && !SCHEME_BIGPOS(b)));
  SCHEME_BIGLEN(r) = size;
  SCHEME_BIGDIG(r) = buffer;

  if (norm)
    r = scheme_bignum_normalize(r);

  return r;
}

Scheme_Object *scheme_bignum_multiply(const Scheme_Object *a, const Scheme_Object *b)
{
  return bignum_multiply(NULL, a, b, 1);
}

Scheme_Object *scheme_bignum_power(const Scheme_Object *a, const Scheme_Object *b)
{
  Small_Bignum s1, s2;
  Scheme_Object *r;
  int bl, i, toggle = 0;
  bigdig v;

  if (!SCHEME_BIGPOS(b)) {
    scheme_signal_error("positive bignum powers only");
  }
  bl = SCHEME_BIGLEN(b);

  r = scheme_make_small_bignum(1, &s1);
  while (bl--) {
    v = SCHEME_BIGDIG(b)[bl];

    for (i = 0; i < LOG_BIG_RADIX; i++) {
      r = bignum_multiply(toggle ? &s1 : &s2, r, r, 0);
      toggle = !toggle;
      v = v << 1;
      if (v & BIG_RADIX) {
	r = bignum_multiply(toggle ? &s1 : &s2, r, a, 0);
	toggle = !toggle;
      }
    }
  }

  r = scheme_bignum_normalize(r);

  if (SCHEME_BIGNUMP(r))
    r = bignum_copy(r, 0);

  return r;
}

Scheme_Object *scheme_bignum_max(const Scheme_Object *a, const Scheme_Object *b)
{
  int lt;
  lt = scheme_bignum_lt(a, b);
  return scheme_bignum_normalize(lt ? b : a);
}

Scheme_Object *scheme_bignum_min(const Scheme_Object *a, const Scheme_Object *b)
{
  int lt;
  lt = scheme_bignum_lt(a, b);
  return scheme_bignum_normalize(lt ? a : b);
}

static int setup_binop(const Scheme_Object *a, const Scheme_Object *b,
		       bigdig **nr_out, bigdig **no_out, 
		       Scheme_Object **r_out)
{
  int al, bl, i, aneg, bneg, extra;
  Scheme_Object *o;
  bigdig *no;
  bigdig *na;

  al = SCHEME_BIGLEN(a);
  aneg = !SCHEME_BIGPOS(a);
  bl = SCHEME_BIGLEN(b);
  bneg = !SCHEME_BIGPOS(b);
  
  if (al > bl) {
    const Scheme_Object *aux;
    int iaux;

    iaux = al;
    al = bl;
    bl = iaux;
    aux = a;
    a = b;
    b = aux;
    iaux = aneg;
    aneg = bneg;
    bneg = iaux;
  }

  o = bignum_copy(b, 2);

  SCHEME_BIGPOS(o) = 1;
  no = SCHEME_BIGDIG(o);
  na = SCHEME_BIGDIG(a);

  extra = -1;
  for (i = 0; i < al; i++) {
    no[i] = na[i];
    if (aneg) {
      no[i] = ~no[i] & BIG_MAX;
      if (extra) {
	if (no[i] < BIG_MAX) {
	  no[i]++;
	  extra = 0;
	} else
	  no[i] = 0;
      }
    }
  }
  for (; i < bl; i++) {
    if (aneg)
      no[i] = BIG_MAX;
    else
      no[i] = 0;
  }

  *nr_out = no;

  if (bneg) {
    no = na = NULL;

    b = bignum_copy(b, 1);

    no = SCHEME_BIGDIG(b);

    extra = -1;
    for (i = 0; i < bl; i++) {
      no[i] = ~no[i] & BIG_MAX;
      if (extra) {
	if (no[i] < BIG_MAX) {
	  no[i]++;
	  extra = 0;
	} else
	  no[i] = 0;
      }
    }

    *no_out = no;
  } else
    *no_out = SCHEME_BIGDIG(b);

  *r_out = o;

  return bl;
}

static Scheme_Object *done_binop(Scheme_Object *r, bigdig *nr, int l)
{
  int i;

  if (!SCHEME_BIGPOS(r)) {
    int extra = 1;

    /* 2's comp negative: get equiv positive */
    for (i = 0; i < l; i++) {
      nr[i] = ~nr[i] & BIG_MAX;
      if (extra) {
	if (nr[i] < BIG_MAX) {
	  nr[i]++;
	  extra = 0;
	} else
	  nr[i] = 0;
      }
    }
    if (extra) {
      nr[l] = 1;
      l++;
    }  
  }

  for (i = l; i--; ) {
    if (!nr[i])
      l--;
    else
      break;
  }

  SCHEME_BIGLEN(r) = l;

  return scheme_bignum_normalize(r);
}

Scheme_Object *scheme_bignum_and(const Scheme_Object *a, const Scheme_Object *b)
{
  int l, i;
  bigdig *nr, *no;
  Scheme_Object *r;

  l = setup_binop(a, b, &nr, &no, &r);
  SCHEME_BIGPOS(r) = !(!SCHEME_BIGPOS(a) && !SCHEME_BIGPOS(b));
  for (i = l; i--; ) {
    nr[i] &= no[i];
  }

  return done_binop(r, nr, l);
}

Scheme_Object *scheme_bignum_or(const Scheme_Object *a, const Scheme_Object *b)
{
  int l, i;
  bigdig *nr, *no;
  Scheme_Object *r;

  l = setup_binop(a, b, &nr, &no, &r);
  SCHEME_BIGPOS(r) = !(!SCHEME_BIGPOS(a) || !SCHEME_BIGPOS(b));
  for (i = 0; i < l; i++) {
    nr[i] |= no[i];
  }

  return done_binop(r, nr, l);
}

Scheme_Object *scheme_bignum_xor(const Scheme_Object *a, const Scheme_Object *b)
{
  int l, i;
  bigdig *nr, *no;
  Scheme_Object *r;

  l = setup_binop(a, b, &nr, &no, &r);
  SCHEME_BIGPOS(r) = !(!SCHEME_BIGPOS(a) ^ !SCHEME_BIGPOS(b));
  for (i = l; i--; ) {
    nr[i] ^= no[i];
  }

  return done_binop(r, nr, l);
}

Scheme_Object *scheme_bignum_not(const Scheme_Object *a)
{
  int i, l;
  bigdig *na;

  a = bignum_copy(a, 2);

  l = SCHEME_BIGLEN(a);
  na = SCHEME_BIGDIG(a);
  
  if (SCHEME_BIGPOS(a)) {
    SCHEME_BIGPOS(a) = 0;
    for (i = 0; i < l; i++) {
      if (na[i] == BIG_MAX)
	na[i] = 0;
      else {
	na[i]++;
	break;
      }
    }
    if (!na[l - 1]) {
      na[l] = 1;
      SCHEME_BIGLEN(a) = l + 1;
    }
  } else {
    SCHEME_BIGPOS(a) = 1;
    for (i = 0; i < l; i++) {
      if (!na[i])
	na[i] = BIG_MAX;
      else {
	--na[i];
	break;
      }
    }
    if (!na[l - 1])
      SCHEME_BIGLEN(a) = l - 1;
  }

  return scheme_bignum_normalize(a);
}

Scheme_Object *scheme_bignum_shift(const Scheme_Object *n, long shift)
{
  int nl, rl, i, loshift, hishift, offset;
  bigdig *ra, *na;
  Scheme_Object *r;
  int subone = 0;

  nl = SCHEME_BIGLEN(n);

  if (shift < 0) {
    shift = -shift;

    offset = (shift / LOG_BIG_RADIX);

    if (offset >= nl) {
      if (!SCHEME_BIGPOS(n))
	return scheme_make_integer(-1);
      else
	return scheme_make_integer(0);
    }

    loshift = (shift % LOG_BIG_RADIX);
    hishift = (LOG_BIG_RADIX - loshift);

    rl = nl - offset;
    ra = MALLOC_N_ATOMIC(bigdig, rl);
    
    na = SCHEME_BIGDIG(n);

    for (i = rl - 1; i--; ) {
      ra[i] = (na[i + offset] >> loshift) | ((na[i + offset + 1] << hishift) & BIG_MAX);
    }
    ra[rl - 1] = (na[rl - 1 + offset] >> loshift);

    /* If n is negative and we dropped any bits, sub one from result (2's complement!): */
    if (!SCHEME_BIGPOS(n)) {
      for (i = 0; i < offset; i++) {
	if (na[i]) {
	  subone = 1;
	  break;
	}
      }
      if ((i == offset) && ((na[offset] << hishift) & BIG_MAX))
	subone = 1;
    }
  } else {
    int top;

    offset = (shift / LOG_BIG_RADIX);
    hishift = (shift % LOG_BIG_RADIX);
    loshift = (LOG_BIG_RADIX - hishift);

    rl = nl + offset + 1;
    ra = MALLOC_N_ATOMIC(bigdig, rl);
    
    na = SCHEME_BIGDIG(n);

    top = rl - 1;
    for (i = 0; i < offset; i++) {
      ra[i] = 0;
    }
    for (i = offset + 1; i < top; i++) {
      ra[i] = ((na[i - offset] << hishift) & BIG_MAX) | (na[i - offset - 1] >> loshift);
    }
    ra[rl - 1] = (na[rl - offset - 2] >> loshift);
    ra[offset] = ((na[0] << hishift) & BIG_MAX);
  }

  while (rl && !ra[rl - 1]) {
    --rl;
  }

  na = NULL; /* might be pointer to interior of a small bignum */

  r = (Scheme_Object *)scheme_malloc_tagged(sizeof(Scheme_Bignum));
  r->type = scheme_bignum_type;
  SCHEME_BIGPOS(r) = SCHEME_BIGPOS(n);
  SCHEME_BIGLEN(r) = rl;
  SCHEME_BIGDIG(r) = ra;

  if (subone)
    return scheme_bignum_sub1(r);
  else
    return scheme_bignum_normalize(r);
}

static void 
bignum_small_divide_in_place(Scheme_Object *n, bigdig d, bigdig *rp)
/* d > 0, d <= QUICK_DIV_MAX */
{
  int nl, i;
  long carry;
  bigdig *na;

  nl = SCHEME_BIGLEN(n);

  /* Because bignum calculations are not bounded: */
  SCHEME_USE_FUEL(nl);

  na = SCHEME_BIGDIG(n);

  carry = 0;
  for (i = nl; i--; ) {
    long lo, hi;

    lo = na[i] & BIG_LO_HALF;
    hi = na[i] >> LOG_BIG_LO;

    carry = carry << LOG_BIG_LO;
    carry += hi;
    hi = carry / d;
    carry %= d;

    carry = carry << LOG_BIG_LO;
    carry += lo;
    lo = carry / d;
    carry %= d;

    na[i] = (hi << LOG_BIG_LO) + lo;
  }

  while (nl && !na[nl - 1]) {
    --nl;
  }

  SCHEME_BIGLEN(n) = nl;

  *rp = carry;
}

static char *scheme_bignum_to_string_10(const Scheme_Object *b)
{
  Scheme_Object *r;
  int nl, sl, p, i;
  char *s;
  bigdig a;

  nl = SCHEME_BIGLEN(b);
  if (!nl)
    return "0";
  sl = nl * BIG_DIG_DEC_DIG;
  s = (char *)scheme_malloc_atomic(sl + 1);
  p = 0;

  r = bignum_copy(b, 1);
  while (SCHEME_BIGLEN(r)) {
    bignum_small_divide_in_place(r, TRANS_DIG_DEC, &a);
    
    for (i = 0; i < TRANS_DIG_DEC_DIG - 1; i++) {
      s[p++] = (a % 10) + '0';
      a /= 10;
    }
  }

  while (s[p - 1] == '0') {
    --p;
  }

  if (!SCHEME_BIGPOS(r))
    s[p++] = '-';

  s[p] = 0;

  /* reverse the string: */
  --p;
  for (i = 0; i < p; i++, --p) {
    char k = s[i];
    s[i] = s[p];
    s[p] = k;
  }

  return s;
}

char *scheme_bignum_to_string(const Scheme_Object *b, int radix)
{
  char *n, c;
  int size, p, i, j;
  int bl, dig, step, needed;
  bigdig *ba, v;

  if (radix != 10 && radix != 2 && radix != 8 && radix != 16)
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH, scheme_make_integer(radix),
		     "bad bignum radix (%d)", radix);

  if (!SCHEME_BIGLEN(b))
    return "0";

  if (radix == 10)
    return scheme_bignum_to_string_10(b);
  else if (radix == 2)
    step = 1;
  else if (radix == 8)
    step = 3;
  else if (radix == 16)
    step = 4;
  else  {
    return NULL; /* Doesn't get here */
  }

  bl = SCHEME_BIGLEN(b);

  size = (bl * ((LOG_BIG_RADIX + step) / step)) + 2;
  n = (char *)scheme_malloc_atomic(size);
  p = 0;

  needed = step;
  dig = 0;

  ba = SCHEME_BIGDIG(b);

  while (bl--) {
    v = *(ba++);

    for (j = 0; j < LOG_BIG_RADIX; j++) {
      dig += (v & 0x1) << (step - needed);
      v = v >> 1;
 
      if (!(--needed)) {
	if (dig > 9)
	  n[p] = 'a' + (dig - 10);
	else
	  n[p] = '0' + dig;
	p++;
	needed = step;
	dig = 0;
      }
    }
  }

  if (dig > 9)
    n[p] = 'a' + (dig - 10);
  else
    n[p] = '0' + dig;

  for (i = p; i && (n[i] == '0'); --i) {}
  p = i + 1;

  if (!SCHEME_BIGPOS(b))
    n[p++] = '-';

  /* Reverse the string: */
  for (i = 0, j = p - 1; i < j; i++, --j) {
    c = n[i];
    n[i] = n[j];
    n[j] = c;
  }

  n[p] = 0;

  return n;
}

Scheme_Object *scheme_read_bignum(const char *str, int offset, int radix)
{
  Small_Bignum s1, s2;
  Scheme_Object *r;
  int size = 0, read_valid;
  int len, i, d, negate, stri;
  bigdig *buffer;

  if ((radix < 0) || (radix > 16))
    return scheme_false;

  if (!decimal_digits[0]) {
    REGISTER_SO(decimal_digits);
    for (i = 0; i < 17; i++) {
      decimal_digits[i] = scheme_make_bignum(i);
    }
  }

  negate = 0;
  stri = offset;
  while ((str[stri] == '+') || (str[stri] == '-')) {
    if (str[stri] == '-')
      negate = !negate;
    stri++;
  }

  len = strlen(str + stri);

  if (radix == 10 && (len < SMALL_NUM_STR_LEN)) {
    /* try simple fixnum read first */
    long fx;
    if (!str[stri])
      return scheme_false;
    for (fx = 0; str[stri]; stri++) {
      if (str[stri] < '0' || str[stri] > '9')
	return scheme_false;
      fx = (fx * 10) + (str[stri] - '0');
    }
    if (negate)
       fx = -fx;
    return scheme_make_integer(fx);
  }

  r = scheme_make_small_bignum(0, &s2);

  read_valid = 0;

  while (len--) {
    d = str[stri++];
    if (d >= '0' && d <= '9')
      d -= '0';
    else if (d >= 'a' && d <= 'z')
      d -= 'a' - 10;
    else if (d >= 'A' && d <= 'Z')
      d -= 'A' - 10;
    else
      return scheme_false;
    if (d < 0 || d >= radix)
      return scheme_false;
    read_valid = 1;
    r = bignum_multiply(&s1, r, decimal_digits[radix], 0);
    r = bignum_add((Scheme_Object *)&s2, &buffer, &size, 
		   r, decimal_digits[d], 0);
  }

  if (!read_valid)
    /* No digits */
    return scheme_false;

  if (negate)
    bignum_negate_inplace(r);

  r = scheme_bignum_normalize(r);
  if (SCHEME_BIGNUMP(r))
    r = bignum_copy(r, 0);

  return r;
}

#define USE_FLOAT_BITS 51
#define FP_TYPE double
#define IS_FLOAT_INF is_double_inf
#define SCHEME_BIGNUM_TO_FLOAT_INFO scheme_bignum_to_double_inf_info
#define SCHEME_BIGNUM_TO_FLOAT scheme_bignum_to_double
#define SCHEME_CHECK_FLOAT scheme_check_double
#define SCHEME_BIGNUM_FROM_FLOAT scheme_bignum_from_double
#include "bgnfloat.inc"

#ifdef MZ_USE_SINGLE_FLOATS
# undef USE_FLOAT_BITS
# undef FP_TYPE
# undef IS_FLOAT_INF
# undef SCHEME_BIGNUM_TO_FLOAT_INFO
# undef SCHEME_BIGNUM_TO_FLOAT
# undef SCHEME_CHECK_FLOAT
# undef SCHEME_BIGNUM_FROM_FLOAT

# define USE_FLOAT_BITS 22
# define FP_TYPE float
# define IS_FLOAT_INF is_float_inf
# define SCHEME_BIGNUM_TO_FLOAT_INFO scheme_bignum_to_float_inf_info
# define SCHEME_BIGNUM_TO_FLOAT scheme_bignum_to_float
# define SCHEME_CHECK_FLOAT scheme_check_float
# define SCHEME_BIGNUM_FROM_FLOAT scheme_bignum_from_float
# include "bgnfloat.inc"
#endif

void scheme_bignum_divide(const Scheme_Object *n, const Scheme_Object *d,
			  Scheme_Object **qp, Scheme_Object **rp, int norm)
{
  Scheme_Object *m, *q, *r, *one;
  int log, negate_q, rsize = 0, qsize = 0;
  bigdig *rbuffer, *qbuffer;

  if ((SCHEME_BIGLEN(d) == 1) && (SCHEME_BIGDIG(d)[0] <= BIG_LO_HALF)) {
    bigdig remain, div;

    div = SCHEME_BIGDIG(d)[0];

    q = bignum_copy(n, 1);
    bignum_small_divide_in_place(q, div, &remain);

    if (qp) {
      if (!SCHEME_BIGPOS(d))
	SCHEME_BIGPOS(q) = !SCHEME_BIGPOS(q);
    
      if (norm)
	q = scheme_bignum_normalize(q);

      *qp = q;
    }

    if (rp) {
      if (!SCHEME_BIGPOS(d))
	remain = -remain;

      if (norm)
	*rp = scheme_make_integer(remain);
      else {
	r = scheme_make_bignum(remain);
	*rp = r;
      }
    }

    return;
  }

  negate_q = 0;
  if (!SCHEME_BIGPOS(n)) {
    n = scheme_bignum_negate(n);
    negate_q = !negate_q;
  }
  if (!SCHEME_BIGPOS(d)) {
    d = scheme_bignum_negate(d);
    negate_q = !negate_q;
  }

  if (scheme_bignum_gt(d, n)) {
    if (qp) {
      q = norm ? scheme_make_integer(0) : scheme_make_bignum(0);
      *qp = q;
    }
    if (rp) {
      r = norm ? scheme_bignum_normalize(n) : (Scheme_Object *)n;
      *rp = r;
    }
    return;
  }

  log = 0;
  m = bignum_copy(d, 1);
  while (scheme_bignum_lt(m, n)) {
    bignum_double_inplace(m, 0);
    log++;
  }

  one = scheme_make_bignum(1);

  q = scheme_make_bignum(0);
  r = bignum_copy(n, 1);
  log++;
  while (log--) {
    bignum_double_inplace(q, qsize);
    if (!scheme_bignum_lt(r, m)) {
      bignum_negate_inplace(m);
      r = bignum_add(r, &rbuffer, &rsize, r, m, 0);
      bignum_negate_inplace(m);
      q = bignum_add(q, &qbuffer, &qsize, q, one, 0);
    }
    bignum_half_inplace(m);
  }

  if (qp) {
    if (negate_q)
      bignum_negate_inplace(q);
    q = norm ? scheme_bignum_normalize(q) : q;
    *qp = q;
  }
  if (rp) {
    r = norm ? scheme_bignum_normalize(r) : r;
    *rp = r;
  }
}

Scheme_Object *scheme_integer_sqrt(const Scheme_Object *n)
{
  int len;
  Scheme_Object *a, *b, *a_sq, *twice_a;
  Scheme_Object *zero = scheme_make_integer(0);
  Scheme_Object *two = scheme_make_integer(2);

  /* First, find inital guess (too high, if anything):. */

  /* Get length of binary number: */
  {
    bigdig v;

    if (SCHEME_INTP(n)) {
      len = 0;
      v = SCHEME_INT_VAL(n);
    } else {
      Scheme_Bignum *b_n = (Scheme_Bignum *)n;
      len = (b_n->len - 1) * LOG_BIG_RADIX;
      v = b_n->digits[b_n->len - 1];
    }
    
    while (v) {
      v = v >> 1;
      len++;
    }

    if (!len)
      return zero;
  }

  /* We know that the square root is no bigger than
     2^((len+1)/2). Pick that number as a. */
  a = scheme_bignum_shift(scheme_make_bignum(1), (len + 1) >> 1);
  
  /* Now, refine the guess until we reach a fixpoint: */

  while (1) {
    /* Add a*a to x, divide by 2a: */
    a_sq = scheme_bin_mult(a, a);
    twice_a = scheme_bin_mult(two, a);

    b = scheme_bin_quotient(scheme_bin_plus(n, a_sq), twice_a);

    /* If b = a, we might be done: */
    if (scheme_bin_eq(b, a)) {
      if (scheme_bin_eq(a_sq, n))
	return a;
      break; 
    }

    /* If b is bigger, there's no exact root: */
    if (scheme_bin_gt(b, a))
      break;
    
    /* Otherwise, b is a better guess: */
    a = b;
  }

  /* Return inexact root: */
  {
    double v;
    
    if (SCHEME_INTP(n))
      v = (double)SCHEME_INT_VAL(n);
    else {
      v = scheme_bignum_to_float(n);
      
      if (MZ_IS_POS_INFINITY(v))
	return scheme_make_double(v);
    }
    
    return scheme_make_double(sqrt(v));
  }
}
