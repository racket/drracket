
/* 
 * guile.c
 * libguile implementation for MzScheme
 * Matthew 08/10/96
 */

#ifdef _MzSchemeExtension_
#include "escheme.h"
#else
#include "scheme.h"
#endif

#define scheme_env scheme_get_env(scheme_config)

/* _UsesMzScheme_ equates SCM and SchemeObject* */
#define _UsesMzScheme_
#include "gh.h"

char *strdup_sized(char *s, int len)
{
  char *n;
  n = malloc(len);
  memcpy(n, s, len);
  return n;
}

#include "guileinc.c"

void gh_enter(int argc, char **argv, SCM_MAIN_FN proc)
{
#ifndef _MzSchemeExtension_
  (void)scheme_basic_env();
#endif
  proc(argc, argv);
}

/*************** Procedures *********************/

typedef struct {
  SCM_FN f;
  int n;
} WrappedProc;

typedef struct {
  SCM f;
  SCM a;
} CurriedProc;

Scheme_Object *wrapper(void *wp, int argc, Scheme_Object *argv[])
{
  SCM_FN f = ((WrappedProc *)wp)->f;
  int n = ((WrappedProc *)wp)->n;
  Scheme_Object *v2[11], *rest;
  
  if (argc > n) {
    rest = scheme_build_list(argc - n, argv + n);
  } else if (argc < n) {
    int i;
    for (i = 0; i < n; i++)
      if (i < argc)
	v2[i] = argv[i];
      else
	v2[i] = NULL;
    argv = v2;
    rest = scheme_null;
  } else
    rest = scheme_null;

  switch (n) {
# include "wrap.inc"
  }

  return NULL;
}

SCM gh_make_subr (SCM_FN fn, int req, int opt, int varp, char *doc)
{
  WrappedProc *wp;

  wp = (WrappedProc *)scheme_malloc(sizeof(WrappedProc));

  wp->f = fn;
  wp->n = req + opt;

  return scheme_make_closed_prim_w_arity(wrapper, (void *)wp, NULL,
					 req, varp ? -1 : req + opt);
}

#define CURRY_QUICK_SIZE 5

static Scheme_Object *do_curry(void *c, int argc, Scheme_Object *argv[])
{
  Scheme_Object **argv2, *quick[CURRY_QUICK_SIZE];
  int i;

  if (argc < CURRY_QUICK_SIZE)
    argv2 = quick;
  else
    argv2 = (Scheme_Object **)scheme_malloc((argc + 1) * sizeof(Scheme_Object *));

  argv2[0] = ((CurriedProc *)c)->a;
  for (i = 0; i < argc; i++)
    argv2[i] = argv[i + 1];

  return scheme_apply(((CurriedProc *)c)->f, argc + 1, argv2);
}

SCM gh_curry(SCM f, SCM a)
{
  CurriedProc *c;

  c = (CurriedProc *)scheme_malloc(sizeof(CurriedProc));
  c->f = f;
  c->a = a;

  return scheme_make_closed_prim(do_curry, (void *)c);
}

SCM gh_call1(SCM proc, SCM arg1)
{
  SCM args[1];
  args[0] = arg1;
  return _scheme_apply(proc, 1, args);
}

SCM gh_call2(SCM proc, SCM arg1, SCM arg2)
{
  SCM args[2];
  args[0] = arg1;
  args[1] = arg2;
  return _scheme_apply(proc, 2, args);
}

SCM gh_call3(SCM proc, SCM arg1, SCM arg2, SCM arg3)
{
  SCM args[3];
  args[0] = arg1;
  args[1] = arg2;
  args[2] = arg3;
  return _scheme_apply(proc, 3, args);
}

/*************** Catch/Throw *********************/

SCM gh_catch(SCM key, SCM thunk, SCM handler)
{
  
}

SCM gh_throw (SCM key, SCM args)
{
}

/*************** Numbers *********************/

int gh_fits_C_long_p(SCM x)
{
  long v;

  if (SCHEME_INTP(x) || SCHEME_BIGNUMP(x))
    return scheme_get_int_val(x, &v);
  else if (SCHEME_DBLP(x)) {
    double d = SCHEME_DBL_VAL(x);
    if (d == (long)d)
      return 1;
  } 

  return 0;
}

int gh_fits_C_unsigned_long_p(SCM x)
{
  unsigned long v;

  if (SCHEME_INTP(x) || SCHEME_BIGNUMP(x))
    return scheme_get_unsigned_int_val(x, &v);
  else if (SCHEME_DBLP(x)) {
    double d = SCHEME_DBL_VAL(x);
    if (d == (unsigned long)d)
      return 1;
  } 

  return 0;
}

long gh_scm2long(SCM x)
{
  long v;

  if (SCHEME_DBLP(x))
    return SCHEME_DBL_VAL(x);
  else
    scheme_get_int_val(x, &v);

  return v;
}

unsigned long gh_scm2ulong(SCM x)
{
  unsigned long v;

  if (SCHEME_DBLP(x))
    return SCHEME_DBL_VAL(x);
  else
    scheme_get_unsigned_int_val(x, &v);

  return v;
}

double gh_scm2double(SCM x)
{
  if (SCHEME_DBLP(x))
    return SCHEME_DBL_VAL(x);
  else if (SCHEME_INTP(x))
    return SCHEME_INT_VAL(x);
  else if (SCHEME_BIGNUMP(x))
    return scheme_bignum_to_float(x);
  else if (SCHEME_RATIONALP(x))
    return scheme_rational_to_float(x);
  else
    return 0;
}

SCM gh_list(SCM x, ...)
{
  Scheme_Object *first, *last, *pair;
  va_list args;

  va_start(args, x);

  first = last = scheme_make_pair(x, scheme_null);
  while(1) {
    x = va_arg(args, Scheme_Object*);
    if (x) {
      pair = scheme_make_pair(x, scheme_null);
      SCHEME_CDR(last) = pair;
      last = pair;
    } else
      break;
  }

  va_end(args);

  return first;
}

