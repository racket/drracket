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
*/

#include "schpriv.h"

#ifndef NO_SCHEME_THREADS

static Scheme_Object *make_sema(int n, Scheme_Object **p);
static Scheme_Object *semap(int n, Scheme_Object **p);
static Scheme_Object *hit_sema(int n, Scheme_Object **p);
static Scheme_Object *block_sema_p(int n, Scheme_Object **p);
static Scheme_Object *block_sema(int n, Scheme_Object **p);
static Scheme_Object *block_sema_breakable(int n, Scheme_Object **p);
static Scheme_Object *sema_callback(int n, Scheme_Object **p);
static Scheme_Object *sema_input_port(int n, Scheme_Object **p);

void scheme_init_sema(Scheme_Env *env)
{
  scheme_add_global_constant("make-semaphore", 
			     scheme_make_prim_w_arity(make_sema,
						      "make-semaphore", 
						      0, 1), 
			     env);
  scheme_add_global_constant("semaphore?", 
			     scheme_make_folding_prim(semap,
						      "semaphore?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("semaphore-post", 
			     scheme_make_prim_w_arity(hit_sema, 
						      "semaphore-post", 
						      1, 1), 
			     env);
  scheme_add_global_constant("semaphore-try-wait?", 
			     scheme_make_prim_w_arity(block_sema_p, 
						      "semaphore-try-wait?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("semaphore-wait", 
			     scheme_make_prim_w_arity(block_sema, 
						      "semaphore-wait", 
						      1, 1), 
			     env);
  scheme_add_global_constant("semaphore-wait/enable-break", 
			     scheme_make_prim_w_arity(block_sema_breakable, 
						      "semaphore-wait/enable-break", 
						      1, 1), 
			     env);
  scheme_add_global_constant("semaphore-callback", 
			     scheme_make_prim_w_arity(sema_callback, 
						      "semaphore-callback", 
						      2, 2), 
			     env);
  scheme_add_global_constant("input-port-post-semaphore", 
			     scheme_make_prim_w_arity(sema_input_port, 
						      "input-port-post-semaphore", 
						      2, 2), 
			     env);
}

#ifdef MZ_REAL_THREADS
static void free_sema(void *s, void *ignored)
{
  Scheme_Sema *sema;

  sema = (Scheme_Sema *)s;

  SCHEME_FREE_SEMA(sema->sema);
}
#endif

Scheme_Object *scheme_make_sema(long v)
{
  Scheme_Sema *sema;

#ifdef MZ_REAL_THREADS
  sema = (Scheme_Sema *)scheme_malloc_tagged(sizeof(Scheme_Sema));
  sema->sema = SCHEME_MAKE_SEMA(v);
  scheme_add_finalizer(sema, free_sema, NULL);
#else
  sema = (Scheme_Sema *)scheme_malloc_atomic_tagged(sizeof(Scheme_Sema));
  sema->value = v;
#endif

  sema->type = scheme_sema_type;

  return (Scheme_Object *)sema;
}

static Scheme_Object *make_sema(int n, Scheme_Object **p)
{
  long v;

  if (n) {
    if (!SCHEME_INTP(p[0])) {
      if (!SCHEME_BIGNUMP(p[0]) || !SCHEME_BIGPOS(p[0]))
	scheme_wrong_type("make-semaphore", "non-negative exact integer", 0, n, p);
    }

    if (!scheme_get_int_val(p[0], &v)) {
      scheme_raise_exn(MZEXN_MISC_SEMAPHORE,
		       "make-semaphore: starting value %s is too large",
		       scheme_make_provided_string(p[0], 0, NULL));
    } else if (v < 0)
      scheme_wrong_type("make-semaphore", "non-negative exact integer", 0, n, p);
  } else
    v = 0;

  return scheme_make_sema(v);
}

static Scheme_Object *semap(int n, Scheme_Object **p)
{
  return SCHEME_SEMAP(p[0]) ? scheme_true : scheme_false;
}

void scheme_post_sema(Scheme_Object *o)
{
  Scheme_Sema *t = (Scheme_Sema *)o;

#ifdef MZ_REAL_THREADS
  if (SCHEME_SEMA_UP(t->sema))
    return;
#else
  int v;

  v = t->value + 1;
  if (v > t->value) {
    t->value = v;
    return;
  }
#endif

  scheme_raise_exn(MZEXN_MISC_SEMAPHORE,
		   "semaphore-post: the maximum post count has already been reached");
}

static Scheme_Object *hit_sema(int n, Scheme_Object **p)
{
  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("semaphore-post", "semaphore", 0, n, p);

  scheme_post_sema(p[0]);

  return scheme_void;
}

typedef struct {
  Scheme_Config *config;
  Scheme_Object *orig_param_val;
  Scheme_Object *sema;
} BreakableWait;

static void pre_breakable_wait(void *data)
{
  BreakableWait *bw = (BreakableWait *)data;

  bw->orig_param_val = scheme_get_param(bw->config, MZCONFIG_ENABLE_BREAK);
  scheme_set_param(bw->config, MZCONFIG_ENABLE_BREAK, scheme_true);
}

static Scheme_Object *do_breakable_wait(void *data)
{
  BreakableWait *bw = (BreakableWait *)data;
  scheme_wait_sema(bw->sema, 0);
  return scheme_void;
}

static void post_breakable_wait(void *data)
{
  BreakableWait *bw = (BreakableWait *)data;
  scheme_set_param(bw->config, MZCONFIG_ENABLE_BREAK, bw->orig_param_val);
}

int scheme_wait_sema(Scheme_Object *o, int just_try)
{
  Scheme_Sema *sema = (Scheme_Sema *)o;
  int v;

  if (just_try) {
    if (just_try > 0) {
#ifdef MZ_REAL_THREADS
      v = SCHEME_SEMA_TRY_DOWN(sema->sema);
#else
      if (sema->value) {
	--sema->value;
	v = 1;
      } else
	v = 0;
#endif
    } else {
      BreakableWait *bw = MALLOC_ONE(BreakableWait);

      bw->sema = o;
      bw->config = scheme_config;

      scheme_dynamic_wind(pre_breakable_wait, 
			  do_breakable_wait, 
			  post_breakable_wait, 
			  NULL, bw);

      return 1;
    }
  } else {
#ifdef MZ_REAL_THREADS
    SCHEME_SEMA_DOWN(sema->sema);
#else
    if (sema->value)
      --sema->value;
    else {
      scheme_current_process->block_descriptor = SEMA_BLOCKED;
      scheme_current_process->blocker = (Scheme_Object *)sema;
      
      while (!sema->value)
	scheme_process_block(0);
      --sema->value;
      
      scheme_current_process->block_descriptor = NOT_BLOCKED;
      scheme_current_process->blocker = NULL;
      scheme_current_process->ran_some = 1;
    }
    v = 1;
#endif
  }

  return v;
}

static Scheme_Object *block_sema_p(int n, Scheme_Object **p)
{
  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("semaphore-try-wait?", "sema", 0, n, p);

  return scheme_wait_sema(p[0], 1) ? scheme_true : scheme_false;
}

static Scheme_Object *block_sema(int n, Scheme_Object **p)
{
  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("semaphore-wait", "sema", 0, n, p);

  scheme_wait_sema(p[0], 0);

  return scheme_void;
}

static Scheme_Object *block_sema_breakable(int n, Scheme_Object **p)
{
  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("semaphore-wait/enable-break", "sema", 0, n, p);

  scheme_wait_sema(p[0], -1);

  return scheme_void;
}

static Scheme_Object *sema_callback(int n, Scheme_Object **p)
{
  Scheme_Sema_Callback *cb;

  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("semaphore-callback", "semaphore", 0, n, p);
  scheme_check_proc_arity("semaphore-callback", 0, 1, n, p);

  cb = (Scheme_Sema_Callback*)scheme_malloc(sizeof(Scheme_Sema_Callback));
  cb->sema = (Scheme_Sema *)p[0];
  cb->callback = p[1];
  
  scheme_add_sema_callback(cb);

  return scheme_void;
}

static Scheme_Object *sema_input_port(int n, Scheme_Object **p)
{
  Scheme_Sema_From_Port *cb;

  if (!SCHEME_INPORTP(p[0]))
    scheme_wrong_type("input-port-post-semaphore", "input port", 0, n, p);
  if (!SCHEME_SEMAP(p[1]))
    scheme_wrong_type("input-port-post-semaphore", "semaphore", 1, n, p);

  cb = (Scheme_Sema_From_Port*)scheme_malloc(sizeof(Scheme_Sema_From_Port));
  cb->sema = (Scheme_Sema *)p[1];
  cb->port = p[0];
  
  scheme_add_sema_from_port(cb);

  return scheme_void;
}

#endif
