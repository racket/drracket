
#ifdef _MSC_VER
#pragma optimize("p", on)
#endif

FP_TYPE SCHEME_BIGNUM_TO_FLOAT_INFO(const Scheme_Object *n, int skip, int *_skipped)
{
  int nl, skipped;
  bigdig *na;
  FP_TYPE d;

  nl = SCHEME_BIGLEN(n);
  na = SCHEME_BIGDIG(n) + nl;

  skipped = nl;

  if (skip >= nl)
    return 0.0;
  else
    nl -= skip;

  d = 0;
  while (nl--) {
    d *= (FP_TYPE)BIG_RADIX;
    d += *(--na);
    if (MZ_IS_POS_INFINITY(d))
      break;
    --skipped;
  }

  if (_skipped)
    *_skipped = skipped;
  
  if (!SCHEME_BIGPOS(n))
    d = -d;

  return d;
}

#ifdef _MSC_VER
#pragma optimize("p", off)
#endif

FP_TYPE SCHEME_BIGNUM_TO_FLOAT(const Scheme_Object *n)
{
  return SCHEME_BIGNUM_TO_FLOAT_INFO(n, 0, NULL);
}

Scheme_Object *SCHEME_BIGNUM_FROM_FLOAT(FP_TYPE d)
{
  Small_Bignum s1, s2, onesmall;
  int negate, log, times, i, size = 0;
  FP_TYPE r;
  Scheme_Object *n, *m, *one;
  int toggle = 0;
  bigdig *buffer;

  SCHEME_CHECK_FLOAT("inexact->exact", d, "integer");

  if (d < 0) {
    negate = 1;
    d = -d;
  } else
    negate = 0;

  if (d < 1.0)
    return scheme_make_integer(0);

  r = 1;
  log = 0;
  while (r < d) {
    log++;
    r *= 2.0;
  }

  if (log > USE_FLOAT_BITS) {
    times = log - USE_FLOAT_BITS;
    log = USE_FLOAT_BITS;
    for (i = 0; i < times; i++)
      d /= 2;
  } else
    times = 0;

  r = pow(2.0, (FP_TYPE)log);

  n = scheme_make_small_bignum(0, &s1);
  one = scheme_make_small_bignum(1, &onesmall);

  log++;
  while (log--) {
    bignum_double_inplace(n, size);
    if (d >= r) {
      d -= r;
      n = bignum_add(n, &buffer, &size, n, one, 0);
    }
    r /= 2;
  }

  if (times) {
    m = scheme_make_bignum(1);
    while (times--)
      bignum_double_inplace(m, 0);      
    n = bignum_multiply(toggle ? &s1 : &s2, n, m, 0);
    toggle = !toggle;
  }

  if (negate)
    bignum_negate_inplace(n);

  n = scheme_bignum_normalize(n);
  if (SCHEME_BIGNUMP(n))
    n = bignum_copy(n, 0);

  return n;
}

