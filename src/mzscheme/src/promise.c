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

/* globals */

/* locals */
static Scheme_Object *force(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_promise(int argc, Scheme_Object *argv[]);
static Scheme_Object *promise_p(int argc, Scheme_Object *argv[]);

void
scheme_init_promise (Scheme_Env *env)
{
  scheme_add_global_constant("make-promise", 
			     scheme_make_prim_w_arity(make_promise, 
						      "make-promise",
						      1, 1), 
			     env);
  scheme_add_global_constant("promise?", 
			     scheme_make_folding_prim(promise_p, 
						      "promise?",
						      1, 1, 1), 
			     env);
  scheme_add_global_constant ("force", 
			      scheme_make_prim_w_arity(force, 
						       "force",
						       1, 1), 
			      env);
}

static Scheme_Object *
make_promise_value(Scheme_Object *expr, int is_expr)
{
  Scheme_Promise *promise;

  promise = MALLOC_ONE_TAGGED(Scheme_Promise);
  promise->type = scheme_promise_type;
  promise->forced = 0;
  promise->is_expr = is_expr;
  promise->val = expr;
  promise->multi_array = NULL;

#ifdef MZ_REAL_THREADS
  promise->sema = scheme_make_sema(1);
#endif

  return (Scheme_Object *)promise;
}

Scheme_Object *
scheme_make_promise(Scheme_Object *expr, Scheme_Env *env)
{
  return make_promise_value(scheme_compile(expr, env, 0), 1);
}

Scheme_Object *
scheme_make_promise_value(Scheme_Object *expr)
{
  return make_promise_value(expr, 1);
}

Scheme_Object *
scheme_make_promise_from_thunk(Scheme_Object *expr)
{
  return make_promise_value(expr, 0);
}

static Scheme_Object *
make_promise(int argc, Scheme_Object *argv[])
{
  scheme_check_proc_arity("make-promise", 0,
			  0, argc, argv);

  return scheme_make_promise_from_thunk(argv[0]);
}

static Scheme_Object *
force (int argc, Scheme_Object *argv[])
{
  Scheme_Promise *promise;
  Scheme_Object *v;

  if (!SCHEME_PROMP(argv[0]))
    scheme_wrong_type("force", "promise", 0, argc, argv);

  promise = (Scheme_Promise *)argv[0];

#ifdef MZ_REAL_THREADS
  scheme_wait_sema(promise->sema, 0);
#endif

  if (!promise->forced) {
    int is_expr = promise->is_expr;
    v = promise->val;

#ifdef MZ_REAL_THREADS
    scheme_post_sema(promise->sema);
#endif

    if (is_expr)
      v = _scheme_eval_compiled_expr_multi(v);
    else
      v = _scheme_apply_multi(v, 0, NULL);

#ifdef MZ_REAL_THREADS
    scheme_wait_sema(promise->sema, 0);
#endif

    if (!promise->forced) {
      promise->val = v;
      promise->forced = 1;
      if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
	promise->multi_count = scheme_multiple_count;
	promise->multi_array = scheme_multiple_array;
      }
    }
  }

  v = promise->val;

  if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
    scheme_multiple_count = promise->multi_count;
    scheme_multiple_array = promise->multi_array;
  }

#ifdef MZ_REAL_THREADS
  scheme_post_sema(promise->sema);
#endif
  
  return v;
}

static Scheme_Object *
promise_p(int argc, Scheme_Object *argv[])
{
  return SCHEME_PROMP(argv[0]) ? scheme_true : scheme_false;
}
