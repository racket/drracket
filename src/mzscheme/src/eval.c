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

/* This file contains the main evaluation loop, in the
   scheme_do_eval() function. 

   This file also contains the core of the macro-expander and bytecode
   compiler, which is why it's so big. */

#include "schpriv.h"
#include "schrunst.h"

#ifdef USE_STACKAVAIL
#include <malloc.h>
#endif
#ifdef UNIX_FIND_STACK_BOUNDS
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif
#ifdef BEOS_FIND_STACK_BOUNDS
# include <be/kernel/OS.h>
#endif
#ifdef OSKIT_FIXED_STACK_BOUNDS
# include <oskit/machine/base_stack.h>
#endif
#include "schmach.h"
#ifdef MACOS_STACK_LIMIT
#include <Memory.h>
#endif

/* Debugging stuff: */
#define OPT_COMPILE_VISIBLE 0

/* globals */
int scheme_allow_cond_auto_else = 1;

Scheme_Object *scheme_eval_waiting;
Scheme_Object *scheme_multiple_values;

#ifndef MZ_REAL_THREADS
int scheme_fuel_counter;
#endif

int scheme_stack_grows_up;

/* locals */
static Scheme_Object *eval(int argc, Scheme_Object *argv[]);
static Scheme_Object *compile(int argc, Scheme_Object *argv[]);
#if OPT_COMPILE_VISIBLE
static Scheme_Object *compile_x(int argc, Scheme_Object *argv[]);
#endif
static Scheme_Object *expand(int argc, Scheme_Object **argv);
static Scheme_Object *local_expand(int argc, Scheme_Object **argv);
static Scheme_Object *local_expand_body_expression(int argc, Scheme_Object **argv);
static Scheme_Object *expand_once(int argc, Scheme_Object **argv);
static Scheme_Object *enable_break(int, Scheme_Object *[]);
static Scheme_Object *current_eval(int argc, Scheme_Object *[]);

static Scheme_Object *built_in_name(int argc, Scheme_Object **argv);

static Scheme_Object *allow_auto_cond_else(int argc, Scheme_Object **argv);
static Scheme_Object *allow_set_undefined(int argc, Scheme_Object **argv);

static Scheme_Object *write_application(Scheme_Object *obj);
static Scheme_Object *read_application(Scheme_Object *obj);
static Scheme_Object *write_sequence(Scheme_Object *obj);
static Scheme_Object *read_sequence(Scheme_Object *obj);
static Scheme_Object *read_sequence_save_first(Scheme_Object *obj);
static Scheme_Object *write_branch(Scheme_Object *obj);
static Scheme_Object *read_branch(Scheme_Object *obj);
static Scheme_Object *write_with_cont_mark(Scheme_Object *obj);
static Scheme_Object *read_with_cont_mark(Scheme_Object *obj);
static Scheme_Object *write_syntax(Scheme_Object *obj);
static Scheme_Object *read_syntax(Scheme_Object *obj);

static Scheme_Object *define_values_symbol, *letrec_values_symbol, *lambda_symbol;
static Scheme_Object *unknown_symbol, *void_link_symbol, *quote_symbol;
static Scheme_Object *letmacro_symbol, *begin_symbol;
static Scheme_Object *let_id_macro_symbol;
static Scheme_Object *let_exp_time_symbol;
static Scheme_Object *let_symbol;

#define cons(x,y) scheme_make_pair(x,y)

typedef void (*DW_PrePost_Proc)(void *);

#define TAIL_COPY_THRESHOLD 5

#ifndef MZ_REAL_THREADS
# define DO_CHECK_FOR_BREAK(p, e) \
	if ((scheme_fuel_counter--) <= 0) { \
	  e scheme_process_block(0); \
          (p)->ran_some = 1; \
	}
#else
# define DO_CHECK_FOR_BREAK(p, e) \
	if (((p)->fuel_counter--) <= 0) { \
	  e scheme_process_block_w_process(0, p); \
	}
#endif

#if defined(UNIX_FIND_STACK_BOUNDS) || defined(WINDOWS_FIND_STACK_BOUNDS) \
    || defined(MACOS_FIND_STACK_BOUNDS) || defined(ASSUME_FIXED_STACK_SIZE) \
    || defined(BEOS_FIND_STACK_BOUNDS) || defined(OSKIT_FIXED_STACK_BOUNDS) \
    || defined(PALM_FIND_STACK_BOUNDS)
# ifndef MZ_REAL_THREADS
unsigned long scheme_stack_boundary;
# endif
#endif

#define REGISTYPE(f) f ## _type

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

void
scheme_init_eval (Scheme_Env *env)
{
  if (scheme_starting_up) {
#ifdef MZ_PRECISE_GC
    register_traversers();
#endif

#ifdef MZ_EVAL_WAITING_CONSTANT
    scheme_eval_waiting = MZ_EVAL_WAITING_CONSTANT;
#else
    REGISTER_SO(scheme_eval_waiting);
    scheme_eval_waiting = scheme_alloc_eternal_object();
    scheme_eval_waiting->type = scheme_eval_waiting_type;
#endif

#ifdef MZ_EVAL_WAITING_CONSTANT
    scheme_multiple_values = MZ_MULTIPLE_VALUES_CONSTANT;
#else
    REGISTER_SO(scheme_multiple_values);
    scheme_multiple_values = scheme_alloc_eternal_object();
    scheme_multiple_values->type = scheme_multiple_values_type;
#endif

    REGISTER_SO(define_values_symbol);
    REGISTER_SO(letrec_values_symbol);
    REGISTER_SO(lambda_symbol);
    REGISTER_SO(unknown_symbol);
    REGISTER_SO(void_link_symbol);
    REGISTER_SO(quote_symbol);
    REGISTER_SO(letmacro_symbol);
    REGISTER_SO(begin_symbol);
    REGISTER_SO(let_id_macro_symbol);
    REGISTER_SO(let_exp_time_symbol);
    REGISTER_SO(let_symbol);

    define_values_symbol = scheme_intern_symbol("#%define-values");
    letrec_values_symbol = scheme_intern_symbol("#%letrec-values");
    let_symbol = scheme_intern_symbol("#%let");
    lambda_symbol = scheme_intern_symbol("#%lambda");
    unknown_symbol = scheme_intern_symbol("unknown");
    void_link_symbol = scheme_intern_symbol("-v");
    quote_symbol = scheme_intern_symbol("#%quote");
    letmacro_symbol = scheme_intern_symbol("#%let-macro");
    let_id_macro_symbol = scheme_intern_symbol("#%let-id-macro");
    let_exp_time_symbol = scheme_intern_symbol("#%let-expansion-time");
    begin_symbol = scheme_intern_symbol("#%begin");

    scheme_install_type_writer(REGISTYPE(scheme_application), write_application);
    scheme_install_type_reader(REGISTYPE(scheme_application), read_application);
    scheme_install_type_writer(REGISTYPE(scheme_sequence), write_sequence);
    scheme_install_type_reader(REGISTYPE(scheme_sequence), read_sequence);
    scheme_install_type_writer(REGISTYPE(scheme_branch), write_branch);
    scheme_install_type_reader(REGISTYPE(scheme_branch), read_branch);
    scheme_install_type_writer(REGISTYPE(scheme_with_cont_mark), write_with_cont_mark);
    scheme_install_type_reader(REGISTYPE(scheme_with_cont_mark), read_with_cont_mark);
    scheme_install_type_writer(REGISTYPE(scheme_syntax), write_syntax);
    scheme_install_type_reader(REGISTYPE(scheme_syntax), read_syntax);

    scheme_install_type_writer(scheme_begin0_sequence_type, write_sequence);
    scheme_install_type_reader(scheme_begin0_sequence_type, read_sequence_save_first);
  }
    
  scheme_add_global_constant("eval", 
			     scheme_make_prim_w_arity2(eval, 
						       "eval", 
						       1, 2,
						       0, -1), 
			     env);
  scheme_add_global_constant("compile", 
			     scheme_make_prim_w_arity(compile, 
						      "compile", 
						      1, 1), 
			     env);
#if OPT_COMPILE_VISIBLE
  scheme_add_global_constant("compile-x", 
			     scheme_make_prim_w_arity(compile_x, 
						      "compile-x", 
						      1, 1), 
			     env);
#endif
  scheme_add_global_constant("expand-defmacro", 
			     scheme_make_prim_w_arity(expand, 
						      "expand-defmacro",
						      1, 1), 
			     env);
  scheme_add_global_constant("local-expand-defmacro", 
			     scheme_make_prim_w_arity(local_expand, 
						      "local-expand-defmacro",
						      1, 2), 
			     env);
  scheme_add_global_constant("local-expand-body-expression", 
			     scheme_make_prim_w_arity2(local_expand_body_expression, 
						       "local-expand-body-expression",
						       1, 2,
						       2, 2), 
			     env);
  scheme_add_global_constant("expand-defmacro-once", 
			     scheme_make_prim_w_arity(expand_once, 
						      "expand-defmacro-once", 
						      1, 1), 
			     env);
  scheme_add_global_constant("break-enabled", 
			     scheme_register_parameter(enable_break, 
						       "break-enabled",
						       MZCONFIG_ENABLE_BREAK), 
			     env);
  scheme_add_global_constant("current-eval",
			     scheme_register_parameter(current_eval, 
						       "current-eval",
						       MZCONFIG_EVAL_HANDLER),
			     env);

  scheme_add_global_constant("built-in-name",
			     scheme_make_prim_w_arity(built_in_name, 
						      "built-in-name",
						      1, 1),
			     env);

  scheme_add_global_constant("compile-allow-cond-fallthrough", 
			     scheme_register_parameter(allow_auto_cond_else, 
						       "compile-allow-cond-fallthrough",
						       MZCONFIG_COND_AUTO_ELSE), 
			     env);
  scheme_add_global_constant("compile-allow-set!-undefined", 
			     scheme_register_parameter(allow_set_undefined, 
						       "compile-allow-set!-undefined",
						       MZCONFIG_ALLOW_SET_UNDEFINED), 
			     env);
}

Scheme_Object *
scheme_handle_stack_overflow(Scheme_Object *(*k)(void))
{
  scheme_overflow_k = k;
  scheme_init_jmpup_buf(&scheme_overflow_cont);
  scheme_zero_unneeded_rands(scheme_current_process);
  if (scheme_setjmpup(&scheme_overflow_cont, scheme_current_process,
		      scheme_current_process->cc_start)) {
    scheme_init_jmpup_buf(&scheme_overflow_cont);
    if (!scheme_overflow_reply) {
      scheme_longjmp(scheme_error_buf, 1);
    } else
      return scheme_overflow_reply;
  } else
    scheme_longjmp(scheme_current_process->overflow_buf, 1);
  return NULL; /* never gets here */
}

/* 4 cases => magic number for some compilers doing a switch */
enum {
  SCHEME_EVAL_CONSTANT = 0,
  SCHEME_EVAL_GLOBAL,
  SCHEME_EVAL_LOCAL,
  SCHEME_EVAL_LOCAL_UNBOX,
  SCHEME_EVAL_GENERAL
};

int scheme_get_eval_type(Scheme_Object *obj)
{
  Scheme_Type type;

  type = SCHEME_TYPE(obj);

  if (type > _scheme_values_types_)
    return SCHEME_EVAL_CONSTANT;
  else if (SAME_TYPE(type, scheme_local_type))
    return SCHEME_EVAL_LOCAL;
  else if (SAME_TYPE(type, scheme_local_unbox_type))
    return SCHEME_EVAL_LOCAL_UNBOX;
  else if (SAME_TYPE(type, scheme_variable_type))
    return SCHEME_EVAL_GLOBAL;
  else
    return SCHEME_EVAL_GENERAL;
}    

static Scheme_Object *make_application(Scheme_Object *orig_app,
				       Scheme_Object * volatile v,
				       int can_opt_const,
				       int final)
{
  Scheme_Object *o, *tmp[20], ** volatile linked;
  Scheme_App_Rec *app, *orig;
  int i, nv, size, devals;
  volatile int n;

  orig = (Scheme_App_Rec *)orig_app;

  if (orig) {
    n = orig->num_args + 1;
    nv = 0;
    if (n <= 20)
      linked = tmp;
    else
      linked = MALLOC_N(Scheme_Object *, n);

    for (i = 0; i < n; i++) {
      Scheme_Type type;
      linked[i] = orig->args[i];
      type = SCHEME_TYPE(linked[i]);
      if ((type < _scheme_compiled_values_types_)
	  || SAME_TYPE(type, scheme_quote_compilation_type))
	nv = 1;
    }
  } else {
    o = v;
    n = 0;
    nv = 0;
    while (!SCHEME_NULLP(o)) {
      Scheme_Type type;

      n++;
      if (!SCHEME_LISTP(o))
	scheme_wrong_syntax("application", NULL, NULL, NULL);
      type = SCHEME_TYPE(SCHEME_CAR(o));
      if ((type < _scheme_compiled_values_types_)
	  || SAME_TYPE(type, scheme_quote_compilation_type))
	nv = 1;
      o = SCHEME_CDR(o);
    }
    linked = NULL;
  }

  if (!nv && can_opt_const) {
    /* They're all values. Applying folding prim or closure? */
    Scheme_Object * volatile f;

    f = linked ? linked[0] : SCHEME_CAR(v);

    if ((SCHEME_PRIMP(f) && (((Scheme_Primitive_Proc *)f)->flags & SCHEME_PRIM_IS_FOLDING))
	|| (SCHEME_CLSD_PRIMP(f) 
	    && (((Scheme_Closed_Primitive_Proc *)f)->flags & SCHEME_PRIM_IS_FOLDING))
	|| (SAME_TYPE(SCHEME_TYPE(f), scheme_linked_closure_type)
	    && (((Scheme_Closure_Compilation_Data *)SCHEME_COMPILED_CLOS_CODE(f))->flags
		& CLOS_FOLDABLE))) {
      mz_jmp_buf savebuf;

      /* Apply the procedure. */
      scheme_current_process->error_invoked = 5;
      memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf));
      if (scheme_setjmp(scheme_error_buf))
	f = NULL;
      else {
	if (linked) {
	  f = linked[0];
	  if (n > 20) {
	    /* Have to copy to keep ptr at front for GC */
	    for (i = 1; i < n; i++) {
	      linked[i - 1] = linked[i];
	    }
	  } else
	    linked++;
	  f = _scheme_apply(f, n - 1, linked);
	} else
	  f = _scheme_apply_to_list(SCHEME_CAR(v), SCHEME_CDR(v));
      }

      memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf));
      scheme_current_process->error_invoked = 0;

      if (f)
	return f;
    }
  }

  size = (sizeof(Scheme_App_Rec) + ((n - 1) 
				    * sizeof(Scheme_Object *))
	  + n * sizeof(char));
  app = (Scheme_App_Rec *)(final 
			   ? scheme_malloc_stubborn_tagged(size)
			   : scheme_malloc_tagged(size));

  app->type = scheme_application_type;

  app->num_args = n - 1;

  devals = sizeof(Scheme_App_Rec) + (app->num_args * sizeof(Scheme_Object *));

  if (linked) {
    for (i = 0; i < n; i++) {
      app->args[i] = linked[i];
    }
  } else {
    for (i = 0; i < n; i++, v = SCHEME_CDR(v)) {
      app->args[i] = SCHEME_CAR(v);
    }
  }

  if (final) {
    for (i = 0; i < n; i++) {
      char etype;
      etype = scheme_get_eval_type(app->args[i]);
      ((char *)app + devals)[i] = etype;
    }

    scheme_end_stubborn_change((void *)app);
  }

  return (Scheme_Object *)app;
}

static Scheme_Object *link_application(Scheme_Object *o, Link_Info *info)
{
  Scheme_App_Rec *app;
  int i, n, devals;

  app = (Scheme_App_Rec *)o;

  devals = sizeof(Scheme_App_Rec) + (app->num_args * sizeof(Scheme_Object *));
  
  n = app->num_args + 1;

  info = scheme_link_info_extend(info, n - 1, 0, 0);

  for (i = 0; i < n; i++) {
    Scheme_Object *le;
    le = scheme_link_expr(app->args[i], info);
    app->args[i] = le;
  }

  for (i = 0; i < n; i++) {
    char et;
    et = scheme_get_eval_type(app->args[i]);
    ((char *)app + devals)[i] = et;
  }

  return o;
}

static Scheme_Object *link_branch(Scheme_Object *o, Link_Info *info)
{
  Scheme_Branch_Rec *b;
  Scheme_Object *t, *tb, *fb;

  b = (Scheme_Branch_Rec *)o;

  t = scheme_link_expr(b->test, info);
  tb = scheme_link_expr(b->tbranch, info);
  fb = scheme_link_expr(b->fbranch, info);
  b->test = t;
  b->tbranch = tb;
  b->fbranch = fb;

  return o;
}

static Scheme_Sequence *malloc_sequence(int count)
{
  return (Scheme_Sequence *)scheme_malloc_stubborn_tagged(sizeof(Scheme_Sequence)
							  + (count - 1) 
							  * sizeof(Scheme_Object *));
}

static Scheme_Object *look_for_letv_change(Scheme_Sequence *s)
{
  int i;

  for (i = 0; i < s->count - 1; i++) {
    Scheme_Object *v;
    v = s->array[i];
    if (SAME_TYPE(SCHEME_TYPE(v), scheme_let_value_type)) {
      Scheme_Let_Value *lv = (Scheme_Let_Value *)v;
      if (scheme_omittable_expr(lv->body)) {
	int esize = s->count - (i + 1);
	int nsize = i + 1;
	Scheme_Object *nv, *ev;

	if (nsize > 1) {
	  Scheme_Sequence *naya;

	  naya = malloc_sequence(nsize);
	  naya->type = scheme_sequence_type;
	  naya->count = nsize;
	  nv = (Scheme_Object *)naya;

	  for (i = 0; i < nsize; i++) {
	    naya->array[i] = s->array[i];
	  }
	} else
	  nv = (Scheme_Object *)lv;

	if (esize > 1) {
	  Scheme_Sequence *e;
	  e = malloc_sequence(esize);
	  e->type = scheme_sequence_type;
	  e->count = esize;

	  for (i = 0; i < esize; i++) {
	    e->array[i] = s->array[i + nsize];
	  }

	  ev = (Scheme_Object *)look_for_letv_change(e);
	} else
	  ev = s->array[nsize]; 

	lv->body = ev;

	return nv;
      }
    }
  }

  return (Scheme_Object *)s;
}

static Scheme_Object *link_sequence(Scheme_Object *o, Link_Info *info)
{
  Scheme_Sequence *s = (Scheme_Sequence *)o;
  int i;

  for (i = s->count; i--; ) {
    Scheme_Object *le;
    le = scheme_link_expr(s->array[i], info);
    s->array[i] = le;
  }
  
  return look_for_letv_change(s);
}

Scheme_Object *scheme_make_syntax_link(Scheme_Syntax_Executer *prim,
				       Scheme_Object *data)
{
  Scheme_Object *v;

  v = scheme_alloc_stubborn_object();
  v->type = scheme_syntax_type;
  SCHEME_PTR1_VAL(v) = (void *)prim;
  SCHEME_PTR2_VAL(v) = (void *)data;
  scheme_end_stubborn_change((void *)v);

  return v;
}

Scheme_Object *scheme_make_syntax_compile(Scheme_Syntax_Linker *prim,
					  Scheme_Object *data)
{
  Scheme_Object *v;

  v = scheme_alloc_stubborn_object();
  v->type = scheme_compiled_syntax_type;
  SCHEME_PTR1_VAL(v) = (void *)prim;
  SCHEME_PTR2_VAL(v) = (void *)data;
  scheme_end_stubborn_change((void *)v);

  return v;  
}

Scheme_Object *scheme_link_expr(Scheme_Object *expr, Link_Info *info)
{
  Scheme_Type type = SCHEME_TYPE(expr);

  switch (type) {
  case scheme_local_type:
    {
      int pos, flags;
      
      pos = scheme_link_info_lookup(info, SCHEME_LOCAL_POS(expr), &flags);
      return scheme_make_local((flags & SCHEME_INFO_BOXED) 
			       ? scheme_local_unbox_type
			       : scheme_local_type,
			       pos);
    }
  case scheme_compiled_syntax_type:
    {
      Scheme_Syntax_Linker *f;
	  
      f = (Scheme_Syntax_Linker *)SCHEME_PTR1_VAL(expr);
      return f((Scheme_Object *)SCHEME_PTR2_VAL(expr), info);
    }
  case scheme_application_type:
    return link_application(expr, info);
  case scheme_sequence_type:
    return link_sequence(expr, info);
  case scheme_branch_type:
    return link_branch(expr, info);
  case scheme_with_cont_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)expr;
      Scheme_Object *k, *v, *b;
      k = scheme_link_expr(wcm->key, info);
      v = scheme_link_expr(wcm->val, info);
      b = scheme_link_expr(wcm->body, info);
      wcm->key = k;
      wcm->val = v;
      wcm->body = b;
      return (Scheme_Object *)wcm;
    }
  case scheme_compiled_unclosed_procedure_type:
    return scheme_link_closure_compilation(expr, info);
  case scheme_compiled_let_void_type:
    return scheme_link_lets(expr, info);
  default:
    return expr;
  }
}

Scheme_Object *scheme_link_list(Scheme_Object *expr, Link_Info *info)
{
  Scheme_Object *first = scheme_null, *last = NULL;

  while (SCHEME_PAIRP(expr)) {
    Scheme_Object *pr;

    pr = scheme_make_pair(scheme_link_expr(SCHEME_CAR(expr), info),
			  scheme_null);

    if (last)
      SCHEME_CDR(last) = pr;
    else
      first = pr;
    last = pr;

    expr = SCHEME_CDR(expr);
  }

  return first;
}

void scheme_default_compile_rec(Scheme_Compile_Info *rec, int drec)
{
  rec[drec].max_let_depth = 0;
}

void scheme_init_compile_recs(Scheme_Compile_Info *src, int drec, 
			      Scheme_Compile_Info *dest, int n)
{
  int i;

  for (i = 0; i < n; i++) {
#ifdef MZTAG_REQUIRED
    dest[i].type = scheme_rt_compile_info;
#endif
    dest[i].max_let_depth = 0;
    dest[i].can_optimize_constants = src[drec].can_optimize_constants;
    dest[i].keep_unit_debug_info = src[drec].keep_unit_debug_info;
    dest[i].dont_mark_local_use = src[drec].dont_mark_local_use;
    dest[i].value_name = NULL;
  }
}

void scheme_merge_compile_recs(Scheme_Compile_Info *src, int drec, 
			       Scheme_Compile_Info *dest, int n)
{
  int i;

  if (!n) {
    src[drec].max_let_depth = 0;
    return;
  }
  
  src[drec].max_let_depth = dest[0].max_let_depth;

  for (i = 1; i < n; i++) {
    if (dest[i].max_let_depth > src[drec].max_let_depth)
      src[drec].max_let_depth = dest[i].max_let_depth;
  }
}

void scheme_init_lambda_rec(Scheme_Compile_Info *src, int drec,
			    Scheme_Compile_Info *lam, int dlrec)
{
  lam[dlrec].max_let_depth = 0;
  lam[dlrec].can_optimize_constants = src[drec].can_optimize_constants;
  lam[dlrec].keep_unit_debug_info = src[drec].keep_unit_debug_info;
  lam[dlrec].dont_mark_local_use = src[drec].dont_mark_local_use;
  lam[dlrec].value_name = NULL;
}

void scheme_merge_lambda_rec(Scheme_Compile_Info *src, int drec,
			     Scheme_Compile_Info *lam, int dlrec)
{
}

void scheme_compile_rec_done_local(Scheme_Compile_Info *rec, int drec)
{
  rec[drec].value_name = NULL;
}

/* Forward declaration... */
static Scheme_Object *
scheme_compile_expand_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
			   Scheme_Compile_Info *rec, int drec, int depth,
			   int app_position);

static Scheme_Object *
scheme_inner_compile_list(Scheme_Object *form, Scheme_Comp_Env *env, 
			  Scheme_Compile_Info *rec, int drec, int start_app_position)
{
  if (SCHEME_NULLP(form)) {
    scheme_compile_rec_done_local(rec, drec);
    scheme_default_compile_rec(rec, drec);
    return scheme_null;
  } else if (SCHEME_PAIRP(form)) {
    Scheme_Compile_Info recs[2];
    Scheme_Object *c1, *c2, *name, *first, *rest;

    name = rec[drec].value_name;
    scheme_compile_rec_done_local(rec, drec);
    scheme_init_compile_recs(rec, drec, recs, 2);

    first = SCHEME_CAR(form);
    rest = SCHEME_CDR(form);
    if (SCHEME_NULLP(rest))
      recs[0].value_name = name;
    else
      recs[1].value_name = name;

    c1 = scheme_compile_expand_expr(first, env, recs, 0,
				    1, start_app_position);
    /* if (start_app_position)
      recs[0].is_proc_closure = 0; */
    c2 = scheme_inner_compile_list(rest, env, recs, 1, 0);

    scheme_merge_compile_recs(rec, drec, recs, 2);

    return scheme_make_pair(c1, c2);
  } else
    return scheme_compile_expr(form, env, rec, drec);
}

static Scheme_Object *compile_application(Scheme_Object *form, Scheme_Comp_Env *env,
					  Scheme_Compile_Info *rec, int drec)
{
  int len;

  len = scheme_proper_list_length(form);

  if (len < 0)
    scheme_wrong_syntax("application", NULL, form, NULL);
  
  scheme_compile_rec_done_local(rec, drec);
  form = scheme_inner_compile_list(form, scheme_no_defines(env), rec, drec, 1);

  rec[drec].max_let_depth += (len - 1);

  return make_application(NULL, form, rec[drec].can_optimize_constants, 0);
}

Scheme_Object *
scheme_compile_list(Scheme_Object *form, Scheme_Comp_Env *env, 
		    Scheme_Compile_Info *rec, int drec)
{
  return scheme_inner_compile_list(form, env, rec, drec, 0);
}

static void *compile_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *form;
  int writeable;
  Scheme_Comp_Env *env;
  Scheme_Compile_Info rec;
  Scheme_Object *o;
  Scheme_Compilation_Top *top;
  int can_opt;

  form = (Scheme_Object *)p->ku.k.p1;
  env = (Scheme_Comp_Env *)p->ku.k.p2;
  writeable = p->ku.k.i1;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  can_opt = !writeable;

  rec.can_optimize_constants = can_opt;
  rec.keep_unit_debug_info = 1;
  rec.dont_mark_local_use = 0;
  rec.value_name = NULL;

  o = scheme_link_expr(scheme_compile_expr(form, (Scheme_Comp_Env *)env, &rec, 0),
		       scheme_link_info_create(can_opt));

  top = MALLOC_ONE_TAGGED(Scheme_Compilation_Top);
  top->type = scheme_compilation_top_type;
  top->max_let_depth = rec.max_let_depth;
  top->code = o;

  return (void *)top;
}

static Scheme_Object *_compile(Scheme_Object *form, Scheme_Env *env, int writeable, int eb)
{
  Scheme_Process *p = scheme_current_process;

  if (SAME_TYPE(SCHEME_TYPE(form), scheme_compilation_top_type))
    return form;

  p->ku.k.p1 = form;
  p->ku.k.p2 = env->init;
  p->ku.k.i1 = writeable;

  return (Scheme_Object *)scheme_top_level_do(compile_k, eb);
}

Scheme_Object *scheme_compile(Scheme_Object *form, Scheme_Env *env, int writeable)
{
  return _compile(form, env, writeable, 1);
}

Scheme_Object *scheme_check_immediate_macro(Scheme_Object *first, 
					    Scheme_Comp_Env *env, 
					    Scheme_Compile_Info *rec, int drec,
					    int depth,
					    Scheme_Object **current_val)
{
  Scheme_Object *name, *val, *orig;

  orig = first;

 check_top:
  *current_val = NULL;

  name = SCHEME_CAR(first);
  if (!SCHEME_SYMBOLP(name))
    return first;

  val = scheme_static_distance(name, env, 
			       SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
			       + ((rec && rec[drec].dont_mark_local_use) 
				  ? SCHEME_DONT_MARK_USE 
				  : 0));

  *current_val = val;

  if (SAME_TYPE(SCHEME_TYPE(val), scheme_macro_type)) {
    /* Yep, it's a macro; expand once */
    first = scheme_expand_expr(first, env, 1);
  } else if (SAME_TYPE(SCHEME_TYPE(val), scheme_id_macro_type)) {
    /* id macro */
    first = scheme_make_pair(SCHEME_PTR_VAL(val), SCHEME_CDR(first));
  } else {
    if (SAME_OBJ(val, scheme_define_values_syntax)) {
      /* Check the form of the definition: can't shadow syntax bindings. */
      /* Only check identifier if the definition is well-formed. */
      Scheme_Object *binding, *rest;
      rest = SCHEME_CDR(first);
      if (SCHEME_PAIRP(rest)) {
	binding = SCHEME_CAR(rest);
	rest = SCHEME_CDR(rest);
	if (SCHEME_PAIRP(rest) && SCHEME_NULLP(SCHEME_CDR(rest))) {
	  if (SCHEME_SYMBOLP(binding)) {
	    /* Binding part is ok */
	  } else if (SCHEME_PAIRP(binding)) {
	    rest = SCHEME_CDR(binding);
	    binding = SCHEME_CAR(binding);
	    while (SCHEME_PAIRP(rest)) {
	      if (!SCHEME_SYMBOLP(SCHEME_CAR(rest)))
		break;
	      rest = SCHEME_CDR(rest);
	    }
	    if (!SCHEME_NULLP(rest) && !SCHEME_SYMBOLP(rest))
	      binding = NULL;
	  } else
	    binding = NULL;
	  
	  if (binding) {
	    /* Check binding id for shadowing syntax */
	    Scheme_Object *sdval;
	    sdval = scheme_static_distance(binding, env, 
					   SCHEME_DONT_MARK_USE 
					   + SCHEME_ENV_CONSTANTS_OK);
	    if (SAME_TYPE(SCHEME_TYPE(sdval), scheme_macro_type)
		|| SAME_TYPE(SCHEME_TYPE(sdval), scheme_syntax_compiler_type)) {
	      scheme_wrong_syntax("define-values (in unit or internal)",
				  binding, orig,
				  "unit/internal binding cannot shadow syntax or macro names");
	      return NULL;
	    }
	  }
	}
      }
    }

    return first;
  }

  if (SCHEME_PAIRP(first))
    goto check_top;
  
  return first;
}

Scheme_Object *
scheme_compile_expand_macro_app(Scheme_Object *macro,
				Scheme_Object *form, Scheme_Comp_Env *env,
				Scheme_Compile_Info *rec, int drec, int depth)
{
  Scheme_Comp_Env *save_env;
  Scheme_Process *p = scheme_current_process;

  Scheme_Object *rest = SCHEME_CDR(form);

  if (!depth)
    return form; /* We've gone as deep as requested */

  save_env = p->current_local_env;
  p->current_local_env = env;
  form = scheme_apply_macro_to_list((Scheme_Object *)SCHEME_PTR_VAL(macro), 
				    rest, form);
  p->current_local_env = save_env;

  if (rec)
    return scheme_compile_expr(form, env, rec, drec);
  else {
    --depth;
    if (depth)
      return scheme_expand_expr(form, env, depth);
    else
      return form;
  }
}

Scheme_Object *
scheme_get_primitive_global(Scheme_Object *var, Scheme_Env *env, 
			    int bucket_ok, int can_opt, int signal)
{
  Scheme_Object *sym, *orig, *gvar;
  Scheme_Bucket *b;

  gvar = var;

  if (((Scheme_Bucket_With_Const_Flag *)gvar)->flags & GLOB_IS_PRIMITIVE)
    return var;

  /* Not prim. Try #% version: */
  orig = (Scheme_Object *)(SCHEME_VAR_BUCKET(gvar))->key;

  sym = scheme_hash_percent_name(scheme_symbol_val(orig),
				 SCHEME_SYM_LEN(orig));

  b = scheme_global_bucket(sym, env);

  if (b && (((Scheme_Bucket_With_Const_Flag *)b)->flags & GLOB_IS_PRIMITIVE)) {
    if (can_opt)
      if (((Scheme_Bucket_With_Const_Flag *)b)->flags & GLOB_IS_CONST)
	return (Scheme_Object *)b->val;

    return (Scheme_Object *)b;
  }

  /* #% doesn't work either. must not be a real primitive */
  if (signal) {
    scheme_wrong_syntax("unit", NULL,  orig,
			"used an unbound or non-primitive global");
  }

  return NULL;
}

static Scheme_Object *
built_in_name(int argc, Scheme_Object **argv)
{
  Scheme_Object *o;
  Scheme_Env *env;

  env = scheme_get_env(scheme_config);

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("built-in-name", "symbol", 0, argc, argv);

  o = (Scheme_Object *)scheme_global_bucket(argv[0], env);
  if (o) {
    if ((o = scheme_get_primitive_global(o, env, 1, 0, 0)))
      return (Scheme_Object *)((Scheme_Bucket *)o)->key;
  }

  return scheme_false;
}

static Scheme_Object *compile_expand_expr_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *form = (Scheme_Object *)p->ku.k.p1;
  Scheme_Comp_Env *env = (Scheme_Comp_Env *)p->ku.k.p2;
  Scheme_Compile_Info *rec = (Scheme_Compile_Info *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return scheme_compile_expand_expr(form, 
				    env,
				    rec,
				    p->ku.k.i3,
				    p->ku.k.i1,
				    p->ku.k.i2);
}

static Scheme_Object *
scheme_compile_expand_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
			   Scheme_Compile_Info *rec, int drec, int depth,
			   int app_position)
{
  Scheme_Object *name, *var, *rest;

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Process *p = scheme_current_process;
    p->ku.k.p1 = (void *)form;
    p->ku.k.p2 = (void *)env;
    p->ku.k.p3 = (void *)rec;
    p->ku.k.i3 = drec;
    p->ku.k.i1 = depth;
    p->ku.k.i2 = app_position;
    return scheme_handle_stack_overflow(compile_expand_expr_k);
  }
#endif

 top:

  DO_CHECK_FOR_BREAK(scheme_current_process, ;);

  if (rec) {
    scheme_default_compile_rec(rec, drec);

    if (rec[drec].can_optimize_constants) {
      Scheme_Type type = SCHEME_TYPE(form);
      if (type == scheme_quote_compilation_type)
	return SCHEME_PTR_VAL(form);
    }
  }

  if (!SCHEME_PAIRP(form)) {
    if (SCHEME_SYMBOLP(form)) {
      var = scheme_static_distance(form, env, 
				   SCHEME_ENV_CONSTANTS_OK
				   + ((rec && rec[drec].can_optimize_constants
				       && !ENV_PRIM_GLOBALS_ONLY(env))
				      ? SCHEME_ELIM_CONST 
				      : 0)
				   + (app_position 
				      ? SCHEME_APP_POS 
				      : 0)
				   + ((rec && drec[rec].dont_mark_local_use) ? 
				      SCHEME_DONT_MARK_USE 
				      : 0));
      if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type))
	scheme_wrong_syntax("compile", NULL, form, 
			    "illegal use of a macro name");
      else if (SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type))
	scheme_wrong_syntax("compile", NULL, form, 
			    "illegal use of a syntactic form name");
      else if (SAME_TYPE(SCHEME_TYPE(var), scheme_id_macro_type)) {
	form = SCHEME_PTR_VAL(var);
	if (!rec) {
	  --depth;
	  if (!depth)
	    return form;
	}	  
	goto top;
      } else if (SAME_TYPE(SCHEME_TYPE(var), scheme_exp_time_type))
	scheme_wrong_syntax("compile", NULL, form, 
			    "illegal use of an expansion-time value name");


      /* Check for global use within unit: */
      if (ENV_PRIM_GLOBALS_ONLY(env)
	  && (SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type)))
	var = scheme_get_primitive_global(var, scheme_min_env(env), 0, 
					  rec && rec[drec].can_optimize_constants, 1);

      if (rec) {
	scheme_compile_rec_done_local(rec, drec);
	return var;
      }
    } else if (rec && SAME_TYPE(SCHEME_TYPE(form), scheme_closure_type))
      return scheme_make_closure_compilation(SCHEME_CLOS_ENV(form)->init,
					     SCHEME_CLOS_CODE(form), 
					     rec, drec);

    return form;
  }

  name = SCHEME_CAR(form);
  rest = SCHEME_CDR(form);
  if (SCHEME_SYMBOLP(name)) {
    /* Check for macros: */
    var = scheme_static_distance(name, env, 
				 SCHEME_APP_POS
				 + SCHEME_ENV_CONSTANTS_OK
				 + ((rec && rec[drec].can_optimize_constants
				     && !ENV_PRIM_GLOBALS_ONLY(env))
				    ? SCHEME_ELIM_CONST
				    : 0)
				 + ((rec && rec[drec].dont_mark_local_use)
				    ? SCHEME_DONT_MARK_USE 
				    : 0));
    if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)
	|| SAME_TYPE(SCHEME_TYPE(var), scheme_local_unbox_type)) {
      /* apply to local variable: compile it normally */
    } else {
      if (SAME_TYPE(SCHEME_TYPE(var), scheme_macro_type)) {
	return scheme_compile_expand_macro_app(var, form, env, rec, drec, depth);
      } else if (SAME_TYPE(SCHEME_TYPE(var), scheme_id_macro_type)) {
	form = scheme_make_pair(SCHEME_PTR_VAL(var), rest);
	if (!rec) {
	  --depth;
	  if (!depth)
	    return form;
	} 
	goto top;
      } else if (SAME_TYPE(SCHEME_TYPE(var), scheme_syntax_compiler_type)) {
	if (rec) {
	  Scheme_Syntax *f;
	  f = (Scheme_Syntax *)SCHEME_SYNTAX(var);
	  return f(form, env, rec, drec);
	} else {
	  Scheme_Syntax_Expander *f;
	  f = (Scheme_Syntax_Expander *)SCHEME_SYNTAX_EXP(var);
	  return f(form, env, depth);
	}
      } else if (SAME_TYPE(SCHEME_TYPE(var), scheme_exp_time_type))
	scheme_wrong_syntax("compile", NULL, name, 
			    "illegal use of an expansion-time value name");
	
      /* Else: unknown global - must be a function: compile normally */

      /* Check for global use within unit: */
      if (ENV_PRIM_GLOBALS_ONLY(env)
	  && (SAME_TYPE(SCHEME_TYPE(var), scheme_variable_type)))
	var = scheme_get_primitive_global(var, scheme_min_env(env), 0, 
					  rec && rec[drec].can_optimize_constants, 1);

      /* Optimize (void) to just the value void */
      if (rec && rec[drec].can_optimize_constants && SAME_OBJ(var, scheme_void_func)
	  && SCHEME_NULLP(rest)) {
	scheme_compile_rec_done_local(rec, drec);
	return scheme_void;
      }
    }
    
    /* Must be an application */
    if (rec)
      return compile_application(form, env, rec, drec);
    else
      return scheme_expand_list(form, scheme_no_defines(env), depth);
  } else if (rec) {
    /* look for ((lambda (x) ...) ...) */
    if (SCHEME_PAIRP(name) && SCHEME_SYMBOLP(SCHEME_CAR(name))) {
      Scheme_Object *gval, *origname = name;

      name = scheme_check_immediate_macro(name, env, rec, drec, depth, &gval);
      
      if (SAME_OBJ(gval, scheme_lambda_syntax)) {
	Scheme_Object *argsnbody;
	
	argsnbody = SCHEME_CDR(name);
	if (SCHEME_PAIRP(argsnbody)) {
	  Scheme_Object *args = SCHEME_CAR(argsnbody);
	  Scheme_Object *body = SCHEME_CDR(argsnbody);
	  
	  if (SCHEME_PAIRP(body)) {
	    int pl;
	    pl = scheme_proper_list_length(args);
	    if (pl >= 0) {
	      Scheme_Object *bindings = scheme_null, *last = NULL;
	      int al;
	      al = scheme_proper_list_length(rest);
	      
	      if (al == pl) {	      
		while (!SCHEME_NULLP(args)) {
		  Scheme_Object *v, *n;
		  
		  n = SCHEME_CAR(args);
		  scheme_check_identifier("lambda", n, NULL, env, form);
		  
		  v = cons(cons(n, cons(SCHEME_CAR(rest), scheme_null)), scheme_null);
		  if (last)
		    SCHEME_CDR(last) = v;
		  else
		    bindings = v;
		  
		  last = v;
		  args = SCHEME_CDR(args);
		  rest = SCHEME_CDR(rest);
		}
		
		return scheme_compile_expand_expr(cons(let_symbol,
						       cons(bindings,
							    body)),
						  env, rec, drec, depth, 0);
	      } else {
#if 0
 		scheme_wrong_syntax("application", NULL, form, 
				    "procedure application: bad ((lambda (...) ...) ...) syntax");
		return NULL
#endif
	      }
	    }
	  }
	}
      }

      if (NOT_SAME_OBJ(name, origname))
	form = scheme_make_pair(name, rest);
    }
    
    return compile_application(form, env, rec, drec);
  } else
    return scheme_expand_list(form, scheme_no_defines(env), depth);
}

Scheme_Object *scheme_compile_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
				   Scheme_Compile_Info *rec, int drec)
{
  return scheme_compile_expand_expr(form, env, rec, drec, 1, 0);
}

Scheme_Object *scheme_expand_expr(Scheme_Object *form, Scheme_Comp_Env *env, 
				  int depth)
{
  return scheme_compile_expand_expr(form, env, NULL, 0, depth, 0);
}

static Scheme_Object *
scheme_compile_expand_block(Scheme_Object *forms, Scheme_Comp_Env *env, 
			    Scheme_Compile_Info *rec, int drec, int depth)
/* This ugly code parses a block of code, transforming embedded
   define-values and define-macro into letrec and let-macro.
   It is espcailly ugly because we have to expand macros
   before deciding what we have. */
{
  Scheme_Object *first;
  Scheme_Compile_Info recs[2];

  if (rec)
    scheme_default_compile_rec(rec, drec);

  if (SCHEME_NULLP(forms)) {
    if (rec)
      scheme_compile_rec_done_local(rec, drec);
    return scheme_null;
  }

 try_again:

  first = SCHEME_CAR(forms);

  if (SCHEME_PAIRP(first)) {
    Scheme_Object *name, *gval, *result;

    result = forms;

    /* Check for macro expansion, which could mask the real
       define-values, define-macro, etc.: */
    first = scheme_check_immediate_macro(first, env, rec, drec, depth, &gval);

    name = SCHEME_PAIRP(first) ? SCHEME_CAR(first) : scheme_void;
    if (SAME_OBJ(gval, scheme_begin_syntax)) {
      /* Inline content */
      Scheme_Object *content = SCHEME_CDR(first);

      if (scheme_proper_list_length(content) < 0)
	scheme_wrong_syntax("begin", NULL, first, 
			    "bad syntax (" IMPROPER_LIST_FORM ")");

      forms = scheme_append(content, SCHEME_CDR(forms));

      if (SCHEME_NULLP(forms)) {
	scheme_wrong_syntax("begin", NULL, first, 
			    "bad syntax (empty form)");
      }

      goto try_again;
    } else if (SAME_OBJ(gval, scheme_define_values_syntax)) {
      /* Turn defines into a letrec-values: */
      Scheme_Object *var, *vars, *v, *link, *l = scheme_null, *start = NULL;
      
      while (1) {
	v = SCHEME_CDR(first);
	
	if (!SCHEME_PAIRP(v))
	  scheme_wrong_syntax("define-values (internal)", NULL, forms, 
			      "bad syntax (" IMPROPER_LIST_FORM ")");

	var = NULL;
	vars = SCHEME_CAR(v);
	while (SCHEME_PAIRP(vars)) {
	  var = SCHEME_CAR(vars);
	  if (!SCHEME_SYMBOLP(var))
	    scheme_wrong_syntax("define-values (internal)", var, forms, 
				"name must be an identifier");
	  vars = SCHEME_CDR(vars);
	}
	
	link = scheme_make_pair(v, scheme_null);
	if (!start)
	  start = link;
	else
	  SCHEME_CDR(l) = link;
	l = link;
	result = SCHEME_CDR(result);
	if (!SCHEME_LISTP(result))
	  scheme_wrong_syntax("define-values (internal)", NULL, forms, NULL);

	/* Special case: (define-values #%define-values x) */
	if (SAME_OBJ(define_values_symbol, var))
	  break;

      define_try_again:
	if (!SCHEME_NULLP(result)) {
	  first = SCHEME_CAR(result);
	  if (SCHEME_PAIRP(first)) {
	    first = scheme_check_immediate_macro(first, env, rec, drec, depth, &gval);
	    name = SCHEME_CAR(first);
	    if (NOT_SAME_OBJ(gval, scheme_define_values_syntax)) {
	      if (SAME_OBJ(gval, scheme_begin_syntax)) {
		/* Inline content */
		Scheme_Object *content = SCHEME_CDR(first);
		
		if (scheme_proper_list_length(content) < 0)
		  scheme_wrong_syntax("begin", NULL, first, 
				      "bad syntax (" IMPROPER_LIST_FORM ")");
		
		result = scheme_append(content, SCHEME_CDR(result));
		goto define_try_again;
	      } else
		break;
	    }
	  } else
	    break;
	} else
	  break;
      }

      if (SCHEME_PAIRP(result)) {
	result = scheme_make_pair(letrec_values_symbol, scheme_make_pair(start, result));
      
	name = NULL;
      } else {
	/* Empty body: illegal. */
	scheme_wrong_syntax("begin (possibly implicit)", NULL, forms, 
			    "no expression after a sequence of internal definitions");
      }
    } else if (SAME_OBJ(gval, scheme_defmacro_syntax)
	       || SAME_OBJ(gval, scheme_def_id_macro_syntax)
	       || SAME_OBJ(gval, scheme_def_exp_time_syntax)) {
      /* Convert to let-... */
      Scheme_Object *var, *body, *rest, *let;
      char *where;
      
      if (SAME_OBJ(gval, scheme_defmacro_syntax)) {
	where = "define-macro (internal)";
	let = letmacro_symbol;
      } else if (SAME_OBJ(gval, scheme_def_id_macro_syntax)) {
	where = "define-id-macro (internal)";
	let = let_id_macro_symbol;
      } else {
	where = "define-non-value (internal)";
	let = let_exp_time_symbol;
      }
      
      rest = SCHEME_CDR(result);
      
      first = SCHEME_CDR(first);
      if (!SCHEME_PAIRP(first))
	scheme_wrong_syntax(where, first, result, NULL);
      var = SCHEME_CAR(first);
      first = SCHEME_CDR(first);
      if (!SCHEME_PAIRP(first))
	scheme_wrong_syntax(where, first, result, NULL);
      body = SCHEME_CAR(first);
      first = SCHEME_CDR(first);
      if (!SCHEME_NULLP(first))
	scheme_wrong_syntax(where, first, result, NULL);

      if (SCHEME_NULLP(rest))
	scheme_wrong_syntax("begin (possibly implicit)", NULL, forms, 
			    "no expression after a macro or expansion-time definition");

      result = cons(let,
		   cons(var,
			cons(body,
			     rest)));
      
      name = NULL;
    }

    if (!name) {
      if (rec)
	result = scheme_compile_expr(result, env, rec, drec);
      else {
	--depth;
	if (depth)
	  result = scheme_expand_expr(result, env, depth);
      }
      
      return scheme_make_pair(result, scheme_null);
    }
  }

  if (rec) {
    Scheme_Object *vname, *rest;

    vname = rec[drec].value_name;
    scheme_compile_rec_done_local(rec, drec);
    scheme_init_compile_recs(rec, drec, recs, 2);

    rest = SCHEME_CDR(forms);
    if (SCHEME_NULLP(rest))
      recs[0].value_name = vname;
    else
      recs[1].value_name = vname;

    first = scheme_compile_expr(first, env, recs, 0);
#if EMBEDDED_DEFINES_START_ANYWHERE
    forms = scheme_compile_expand_block(rest, env, recs, 1, 1);
#else
    forms = scheme_compile_list(rest, env, recs, 1);
#endif

    scheme_merge_compile_recs(rec, drec, recs, 2);
  } else {
    first = scheme_expand_expr(first, env, depth);
#if EMBEDDED_DEFINES_START_ANYWHERE
    forms = scheme_compile_expand_block(SCHEME_CDR(forms), env, rec, drec, depth);
#else
    forms = scheme_expand_list(SCHEME_CDR(forms), env, depth);
#endif
  }

  return scheme_make_pair(first, forms);
}

Scheme_Object *
scheme_compile_block(Scheme_Object *forms, Scheme_Comp_Env *env, 
		     Scheme_Compile_Info *rec, int drec)
{
  return scheme_compile_expand_block(forms, env, rec, drec, 1);
}

Scheme_Object *
scheme_expand_block(Scheme_Object *forms, Scheme_Comp_Env *env, int depth)
{
  return scheme_compile_expand_block(forms, env, NULL, 0, depth);
}

Scheme_Object *
scheme_expand_list(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  if (SCHEME_NULLP(form))
    return scheme_null;

  if (!SCHEME_PAIRP(form))
    scheme_wrong_syntax("expand", NULL, form, 
			"bad syntax (" IMPROPER_LIST_FORM ")");

  return scheme_make_pair(scheme_expand_expr(SCHEME_CAR(form), env, depth),
			  scheme_expand_list(SCHEME_CDR(form), env, depth));
}

Scheme_Object *
scheme_make_branch(Scheme_Object *test, Scheme_Object *thenp,
		   Scheme_Object *elsep)
{
  Scheme_Branch_Rec *b;

  if (SAME_TYPE(SCHEME_TYPE(test), scheme_quote_compilation_type))
    test = SCHEME_PTR_VAL(test);
  
  if (SCHEME_TYPE(test) > _scheme_compiled_values_types_) {
    if (SCHEME_FALSEP(test))
      return elsep;
    else
      return thenp;
  }

  b = (Scheme_Branch_Rec *)scheme_malloc_stubborn_tagged(sizeof(Scheme_Branch_Rec));
  b->type = scheme_branch_type;

#if 0
  /* Try optimize: (if (not x) y z) => (if x z y) */
  if (SAME_TYPE(SCHEME_TYPE(test), scheme_application_type)) {
    Scheme_App_Rec *app;

    app = (Scheme_App_Rec *)test;
    if ((app->num_args == 1) && SAME_PTR(scheme_not_prim, app->args[0])) {
      test = thenp;
      thenp = elsep;
      elsep = test;
      test = app->args[1];
    }
  }
#endif

  b->test = test;
  b->tbranch = thenp;
  b->fbranch = elsep;

  scheme_end_stubborn_change((void *)b);

  return (Scheme_Object *)b;
}

extern Scheme_Object *scheme_compiled_void_code;

int scheme_omittable_expr(Scheme_Object *o)
{
  Scheme_Type vtype;

  if (SAME_OBJ(o, scheme_compiled_void_code))
    return 1;

  vtype = SCHEME_TYPE(o);

  if ((vtype > _scheme_compiled_values_types_) 
      || (vtype == scheme_local_type)
      || (vtype == scheme_local_unbox_type)
      || (vtype == scheme_unclosed_procedure_type)
      || (vtype == scheme_compiled_unclosed_procedure_type)
      || (vtype == scheme_quote_compilation_type))
    return 1;

  if ((vtype == scheme_branch_type)) {
    Scheme_Branch_Rec *b;
    b = (Scheme_Branch_Rec *)o;
    return (scheme_omittable_expr(b->test)
	    && scheme_omittable_expr(b->tbranch)
	    && scheme_omittable_expr(b->fbranch));
  }

  return 0;
}

int scheme_is_compiled_procedure(Scheme_Object *o, int can_be_closed)
{
  if (SAME_TYPE(SCHEME_TYPE(o), scheme_compiled_unclosed_procedure_type)) {
    if (!can_be_closed) {
      Scheme_Closure_Compilation_Data *data;
      data = (Scheme_Closure_Compilation_Data *)o;
      /* Because == 0 is like a constant */
      return (data->closure_size > 0);
    } else
      return 1;
  } else
    return 0;
}

static Scheme_Object *build_sequence(Scheme_Object *seq, 
				     int to_linked, 
				     int opt)
{
  Scheme_Object **array, *list, *v, *good, **linked;
  Scheme_Sequence *o;
  int count, i, k, total, last, first, setgood, addconst;
  Scheme_Type type;

  type = scheme_sequence_type;

  linked = NULL;

  list = seq;
  count = i = 0;
  good = NULL;
  total = 0;
  first = 1;
  setgood = 1;
  while (!SCHEME_NULLP(list)) {
    v = SCHEME_CAR(list);
    list = SCHEME_CDR(list);
    last = SCHEME_NULLP(list);

    if (((opt > 0) || !first) && SAME_TYPE(SCHEME_TYPE(v), type)) {
      /* "Inline" nested begins */
      count += ((Scheme_Sequence *)v)->count;
      total++;
    } else if (opt 
	       && (((opt > 0) && !last) || ((opt < 0) && !first))
	       && scheme_omittable_expr(v)) {
      /* A value that is not the result. We'll drop it. */
      total++;
    } else {
      if (setgood)
	good = v;
      count++;
      total++;
    }
    i++;
    if (first) {
      if (opt < 0)
	setgood = 0;
      first = 0;
    }
  }

  if (!count)
    return scheme_compiled_void(to_linked);
  
  if (count == 1) {
    if ((opt < 0) && !scheme_omittable_expr(SCHEME_CAR(seq))) {
      /* We can't optimize (begin expr cont) to expr because
	 exp is not in tail position in the original (so we'd mess
	 up continuation marks. */
      addconst = 1;
    } else
      return good;
  } else
    addconst = 0;

  o = malloc_sequence(count + addconst);

  o->type = ((opt < 0) ? scheme_begin0_sequence_type : scheme_sequence_type);
  o->count = count + addconst;
  array = o->array;
  
  --total;
  for (i = k = 0; i < count; k++) {
    v = SCHEME_CAR(seq);
    seq = SCHEME_CDR(seq);

    if (((opt > 0) || k) && SAME_TYPE(SCHEME_TYPE(v), type)) {
      int c, j;
      Scheme_Object **a;

      c = ((Scheme_Sequence *)v)->count;
      a = ((Scheme_Sequence *)v)->array;
      for (j = 0; j < c; j++) {
	array[i++] = a[j];
      }
    } else if (opt 
	       && ((opt > 0 && (k < total))
		   || ((opt < 0) && k))
	       && scheme_omittable_expr(v)) {
      /* Value not the result. Do nothing. */
    } else
      array[i++] = v;
  }

  if (addconst)
    array[i] = scheme_make_integer(0);
  
  scheme_end_stubborn_change(o);

  return (Scheme_Object *)o;
}

Scheme_Object *scheme_make_sequence_compilation(Scheme_Object *seq, 
						int to_linked, int opt)
{
  return build_sequence(seq, to_linked, opt);
}

#if defined(UNIX_FIND_STACK_BOUNDS) || defined(ASSUME_FIXED_STACK_SIZE)
extern unsigned long GC_get_stack_base();
#endif

void scheme_init_stack_check()
{
  int *v;
  unsigned long deeper;
#ifdef UNIX_FIND_STACK_BOUNDS
  struct rlimit rl;
#endif
  
  deeper = scheme_get_deeper_address();
  scheme_stack_grows_up = (deeper > (unsigned long)&v);

#ifdef STACK_GROWS_UP
  if (!scheme_stack_grows_up) {
    if (scheme_console_printf)
      scheme_console_printf("Stack grows DOWN, not UP.\n");
    else
      printf("Stack grows DOWN, not UP.\n");
    exit(1);
  }
#endif
#ifdef STACK_GROWS_DOWN
  if (scheme_stack_grows_up) {
    if (scheme_console_printf)
      scheme_console_printf("Stack grows UP, not DOWN.\n");
    else
      printf("Stack grows UP, not DOWN.\n");
    exit(1);
  }
#endif

#ifdef ASSUME_FIXED_STACK_SIZE
  scheme_stack_boundary = GC_get_stack_base();
  if (scheme_stack_grows_up)
    scheme_stack_boundary += (FIXED_STACK_SIZE - STACK_SAFETY_MARGIN);
  else
    scheme_stack_boundary += (STACK_SAFETY_MARGIN - FIXED_STACK_SIZE);
#endif

#ifdef WINDOWS_FIND_STACK_BOUNDS
  scheme_stack_boundary = GC_get_stack_base();
  scheme_stack_boundary += (STACK_SAFETY_MARGIN - 0x100000);
#endif

#ifdef MACOS_FIND_STACK_BOUNDS
  scheme_stack_boundary = (unsigned long)&v +  STACK_SAFETY_MARGIN - StackSpace();
#endif

#ifdef PALMOS_FIND_STACK_BOUNDS
  {
    Ptr s, e;
    SysGetStackInfo(Ptr &s, &e);
    scheme_stack_boundary = (unsigned long)e + STACK_SAFETY_MARGIN;
  }
#endif

#ifdef BEOS_FIND_STACK_BOUNDS
  {
    thread_info info;
    get_thread_info(find_thread(NULL), &info);
    scheme_stack_boundary = (unsigned long)info.stack_base + STACK_SAFETY_MARGIN;
  }
#endif

#ifdef OSKIT_FIXED_STACK_BOUNDS
  scheme_stack_boundary = (unsigned long)base_stack_start + STACK_SAFETY_MARGIN;
#endif

#ifdef UNIX_FIND_STACK_BOUNDS
  getrlimit(RLIMIT_STACK, &rl);

  {
    unsigned long bnd;
    bnd = (unsigned long)GC_get_stack_base();

    if (scheme_stack_grows_up)
      bnd += ((unsigned long)rl.rlim_cur - STACK_SAFETY_MARGIN);
    else
      bnd += (STACK_SAFETY_MARGIN - (unsigned long)rl.rlim_cur);

# ifndef MZ_REAL_THREADS
    scheme_stack_boundary = bnd;
# else
    scheme_current_process->stack_end = (void *)bnd;
# endif
  }
#endif
}

void scheme_push_continuation_frame(Scheme_Cont_Frame_Data *d)
{
#ifdef MZ_REAL_THREADS
  Scheme_Process *p = scheme_current_process;
#endif
  d->cont_mark_pos = MZ_CONT_MARK_POS;
  d->cont_mark_stack = MZ_CONT_MARK_STACK;

  MZ_CONT_MARK_POS += 2;
}

void scheme_pop_continuation_frame(Scheme_Cont_Frame_Data *d)
{
#ifdef MZ_REAL_THREADS
  Scheme_Process *p = scheme_current_process;
#endif
  MZ_CONT_MARK_POS = d->cont_mark_pos;
  MZ_CONT_MARK_STACK = d->cont_mark_stack;
}


void scheme_set_cont_mark(Scheme_Object *key, Scheme_Object *val)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Cont_Mark *cm = NULL;
  long findpos;
  
  findpos = (long)MZ_CONT_MARK_STACK;
  while (findpos--) {
    Scheme_Cont_Mark *seg = p->cont_mark_stack_segments[findpos >> SCHEME_LOG_MARK_SEGMENT_SIZE];
    long pos = findpos & SCHEME_MARK_SEGMENT_MASK;
    Scheme_Cont_Mark *find = seg + pos;

    if ((long)find->pos < (long)MZ_CONT_MARK_POS) {
      break;
    } else {
      if (find->key == key) {
	cm = find;
	break;
      } else {
	/* Assume that we'll mutate rather than allocate a new mark record. */
	/* This is a bad assumption for a nasty program that repeatedly
	   creates a new key for the same frame, but it's good enough. */
	find->cached_chain = NULL;
      }
    }
  }
  
  if (!cm) {
    /* Allocate a new mark record: */
    long segpos = ((long)MZ_CONT_MARK_STACK) >> SCHEME_LOG_MARK_SEGMENT_SIZE;
    long pos = ((long)MZ_CONT_MARK_STACK) & SCHEME_MARK_SEGMENT_MASK;
    Scheme_Cont_Mark *seg;

    if (segpos >= p->cont_mark_seg_count) {
      /* Need a new segment */
      int c = p->cont_mark_seg_count;
      Scheme_Cont_Mark **segs, *seg;

      /* Note: we perform allocations before changing p to avoid GC trouble,
	 since MzScheme adjusts a thread's cont_mark_stack_segments on GC. */
      segs = MALLOC_N(Scheme_Cont_Mark *, c + 1);
      seg = scheme_malloc_allow_interior(sizeof(Scheme_Cont_Mark) * SCHEME_MARK_SEGMENT_SIZE);
      segs[c] = seg;

      memcpy(segs, p->cont_mark_stack_segments, c * sizeof(Scheme_Cont_Mark *));
	      
      p->cont_mark_seg_count++;
      p->cont_mark_stack_segments = segs;
    }

    seg = p->cont_mark_stack_segments[segpos];
    cm = seg + pos;
    MZ_CONT_MARK_STACK++;
  }

  cm->key = key;
  cm->val = val;
  cm->pos = MZ_CONT_MARK_POS; /* always odd */
  cm->cached_chain = NULL;
}

void scheme_temp_dec_mark_depth()
{
#ifdef MZ_REAL_THREADS
  Scheme_Process *p = scheme_current_process;
#endif
  MZ_CONT_MARK_POS -= 2;
}

void scheme_temp_inc_mark_depth()
{
#ifdef MZ_REAL_THREADS
  Scheme_Process *p = scheme_current_process;
#endif
  MZ_CONT_MARK_POS += 2;
}

static Scheme_Object *do_apply_known_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object **argv = (Scheme_Object **)p->ku.k.p2;

  p->ku.k.p2 = NULL;

  return _scheme_apply_known_closed_prim_multi((Scheme_Object *)p->ku.k.p1, 
					       p->ku.k.i1, 
					       argv);
}

#if 0
# define DEBUG_CHECK_TYPE(v) \
  if ((v != SCHEME_MULTIPLE_VALUES) \
      && (v != SCHEME_TAIL_CALL_WAITING) \
      && (v != SCHEME_EVAL_WAITING) \
      && (SCHEME_TYPE(v) > (_scheme_last_type_ + 5))) \
  { Scheme_Object *o = *(Scheme_Object **)(v); \
    if (SCHEME_TYPE(o) > (_scheme_last_type_ + 5))\
       scheme_signal_error("bad type"); }
#else
# define DEBUG_CHECK_TYPE(v) /**/
#endif

Scheme_Object *_scheme_apply_known_closed_prim_multi(Scheme_Object *rator,
						     int argc,
						     Scheme_Object **argv)
{
#define PRIM_CHECK_ARITY 0
#define PRIM_CHECK_MULTI 0
#include "schapp.inc"
}

Scheme_Object *_scheme_apply_closed_prim_multi(Scheme_Object *rator,
					       int argc,
					       Scheme_Object **argv)
{
#define PRIM_CHECK_ARITY 1
#define PRIM_CHECK_MULTI 0
#include "schapp.inc"
}

Scheme_Object *_scheme_apply_known_closed_prim(Scheme_Object *rator,
					       int argc,
					       Scheme_Object **argv)
{
#define PRIM_CHECK_ARITY 0
#define PRIM_CHECK_MULTI 1
#include "schapp.inc"
}

Scheme_Object *_scheme_apply_closed_prim(Scheme_Object *rator,
					 int argc,
					 Scheme_Object **argv)
{
#define PRIM_CHECK_ARITY 1
#define PRIM_CHECK_MULTI 1
#include "schapp.inc"
}

Scheme_Object *scheme_check_one_value(Scheme_Object *v)
{
  if (v == SCHEME_MULTIPLE_VALUES)
    scheme_wrong_return_arity(NULL, 1, scheme_multiple_count, scheme_multiple_array, NULL);
  return v;
}

static Scheme_Object *zero_rands_ptr;

static Scheme_Object *do_eval_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *obj = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object **argv = (Scheme_Object **)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return scheme_do_eval(obj, 
			p->ku.k.i1, 
			argv,
			p->ku.k.i2);
}

int scheme_check_runstack(long size)
{
#ifndef RUNSTACK_IS_GLOBAL
  Scheme_Process *p = scheme_current_process;
#endif

  return ((MZ_RUNSTACK - MZ_RUNSTACK_START) >= (size + TAIL_COPY_THRESHOLD));
}

void *scheme_enlarge_runstack(long size, void *(*k)())
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Saved_Stack *saved;
  void *v;

  saved = MALLOC_ONE_RT(Scheme_Saved_Stack);

#ifdef MZTAG_REQUIRED
  saved->type = scheme_rt_saved_stack;
#endif
  saved->prev = p->runstack_saved;
  saved->runstack = MZ_RUNSTACK;
  saved->runstack_start = MZ_RUNSTACK_START;
  saved->runstack_size = p->runstack_size;
  
  size += TAIL_COPY_THRESHOLD;
  if (size < SCHEME_STACK_SIZE)
    size = SCHEME_STACK_SIZE;

  p->runstack_saved = saved;
  p->runstack_size = size;
  MZ_RUNSTACK_START = scheme_malloc_allow_interior(sizeof(Scheme_Object*) * size);
  MZ_RUNSTACK = MZ_RUNSTACK_START + size;
  
  v = k();
  
  p->runstack_saved = saved->prev;
  MZ_RUNSTACK = saved->runstack;
  MZ_RUNSTACK_START = saved->runstack_start;
  p->runstack_size = saved->runstack_size;

  return v;
}

#ifdef REGISTER_POOR_MACHINE
# define USE_LOCAL_RUNSTACK 0
# define DELAY_THREAD_RUNSTACK_UPDATE 0
#else
# define USE_LOCAL_RUNSTACK 1
# define DELAY_THREAD_RUNSTACK_UPDATE 1
#endif

#ifdef MZ_REAL_THREADS
Scheme_Object *
scheme_do_eval_w_process(Scheme_Object *obj, int num_rands, Scheme_Object **rands, 
			 int get_value, Scheme_Process *p)
#else
Scheme_Object *
scheme_do_eval(Scheme_Object *obj, int num_rands, Scheme_Object **rands, 
		int get_value)
#endif
{
  Scheme_Type type;
  Scheme_Object *v, **old_runstack;
  MZ_MARK_STACK_TYPE old_cont_mark_stack;
#if USE_LOCAL_RUNSTACK
  Scheme_Object **runstack;
#endif
#ifndef MZ_REAL_THREADS
  Scheme_Process *p = scheme_current_process;
#endif

#ifdef DO_STACK_CHECK
# define SCHEME_CURRENT_PROCESS p
# ifdef MZ_REAL_THREADS
#  define SCHEME_STACK_BOUNDARY ((unsigned long)p->stack_end)
# endif
# include "mzstkchk.h"
  {
    p->ku.k.p1 = (void *)obj;
    p->ku.k.i1 = num_rands;
    if (num_rands >= 0) {
      /* Copy rands: */
      void *ra;
      ra = (void *)MALLOC_N(Scheme_Object*, num_rands);
      p->ku.k.p2 = ra;
      {
	int i;
	for (i = num_rands; i--; ) {
	  ((Scheme_Object **)p->ku.k.p2)[i] = rands[i];
	}
      }
    } else
      p->ku.k.p2 = (void *)rands;
    p->ku.k.i2 = get_value;
    return scheme_handle_stack_overflow(do_eval_k);
  }
#endif

#if USE_LOCAL_RUNSTACK
# define RUNSTACK runstack
# if DELAY_THREAD_RUNSTACK_UPDATE
#  define UPDATE_THREAD_RSPTR() (MZ_RUNSTACK = runstack)
#  define RUNSTACK_CHANGED() /**/
# else
#  define UPDATE_THREAD_RSPTR() /**/
#  define RUNSTACK_CHANGED() (MZ_RUNSTACK = runstack)
# endif
  runstack = MZ_RUNSTACK;
# define RESET_LOCAL_RUNSTACK() (runstack = MZ_RUNSTACK)
#else
# define RUNSTACK MZ_RUNSTACK
# define UPDATE_THREAD_RSPTR() /**/
# define RUNSTACK_CHANGED() /**/
# define RESET_LOCAL_RUNSTACK() /**/
#endif

#define RUNSTACK_START MZ_RUNSTACK_START

#define UPDATE_THREAD_RSPTR_FOR_GC() UPDATE_THREAD_RSPTR()
#define UPDATE_THREAD_RSPTR_FOR_ERROR() UPDATE_THREAD_RSPTR()

  MZ_CONT_MARK_POS += 2;
  old_runstack = RUNSTACK;
  old_cont_mark_stack = MZ_CONT_MARK_STACK;

  if (num_rands >= 0) {

    if ((RUNSTACK - RUNSTACK_START) < TAIL_COPY_THRESHOLD) {
      /* It's possible that a sequence of primitive _scheme_tail_apply()
	 calls will exhaust the Scheme stack. Watch out for that. */
      p->ku.k.p1 = (void *)obj;
      p->ku.k.i1 = num_rands;
      p->ku.k.p2 = (void *)rands;
      p->ku.k.i2 = -1;
      
      UPDATE_THREAD_RSPTR();
      MZ_CONT_MARK_POS -= 2;
      return scheme_enlarge_runstack(100 * TAIL_COPY_THRESHOLD, (void *(*)(void))do_eval_k);
    }

  apply_top:

    if (SCHEME_INTP(obj)) {
      UPDATE_THREAD_RSPTR();
      scheme_wrong_rator(obj, num_rands, rands);
      return NULL; /* doesn't get here */
    }

    type = _SCHEME_TYPE(obj);

    if (type == scheme_prim_type) {
      Scheme_Primitive_Proc *prim;

      if (rands == p->tail_buffer) {
	if (num_rands < TAIL_COPY_THRESHOLD) {
	  int i;
	  Scheme_Object **quick_rands;

	  quick_rands = PUSH_RUNSTACK(p, RUNSTACK, num_rands);
	  RUNSTACK_CHANGED();

	  for (i = num_rands; i--; ) {
	    quick_rands[i] = rands[i];
	  }
	  rands = quick_rands;
	} else {
	  UPDATE_THREAD_RSPTR_FOR_GC();
	  {
	    Scheme_Object **tb;
	    tb = MALLOC_N(Scheme_Object *, p->tail_buffer_size);
	    p->tail_buffer = tb;
	  }
	}
#ifdef AGRESSIVE_ZERO_TB
	p->tail_buffer_set = 0;
#endif
      } 

      UPDATE_THREAD_RSPTR();

      prim = (Scheme_Primitive_Proc *)obj;

      if (num_rands < prim->mina 
	  || (num_rands > prim->maxa && prim->maxa >= 0)) {
	scheme_wrong_count(prim->name, prim->mina, prim->maxa, 
			   num_rands, rands);
	return NULL; /* Shouldn't get here */
      }

      v = prim->prim_val(num_rands, rands);

      DEBUG_CHECK_TYPE(v);
    } else if (type == scheme_linked_closure_type) {
      Scheme_Closure_Compilation_Data *data;
      Scheme_Object **stack, **src;
      int i, has_rest, num_params;
      
      DO_CHECK_FOR_BREAK(p, UPDATE_THREAD_RSPTR_FOR_GC(););
      
      data = (Scheme_Closure_Compilation_Data *)SCHEME_COMPILED_CLOS_CODE(obj);

      if ((RUNSTACK - RUNSTACK_START) < data->max_let_depth) {
	p->ku.k.p1 = (void *)obj;
	p->ku.k.i1 = num_rands;
	p->ku.k.p2 = (void *)rands;
	p->ku.k.i2 = -1;

	UPDATE_THREAD_RSPTR();
	MZ_CONT_MARK_POS -= 2;
	v = (Scheme_Object *)scheme_enlarge_runstack(data->max_let_depth, (void *(*)(void))do_eval_k);
	MZ_CONT_MARK_POS += 2;

	goto returnv;
      }

      num_params = data->num_params;
      has_rest = (data->flags & CLOS_HAS_REST);
      
      if (num_params) {
	if (has_rest) {
	  int extra, n;

	  if (num_rands < (num_params - 1)) {
	    UPDATE_THREAD_RSPTR_FOR_ERROR();
	    scheme_wrong_count(scheme_get_proc_name(obj, NULL, 1), 
			       num_params - 1, -1,
			       num_rands, rands);
	    return NULL; /* Doesn't get here */
	  }

	  n = num_params - has_rest;
	  
	  stack = RUNSTACK = old_runstack - num_params;
	  CHECK_RUNSTACK(p, RUNSTACK);
	  RUNSTACK_CHANGED();

	  extra = num_rands - n;
	  if (extra) {
	    Scheme_Object *rest_vals, *pairs;

	    /* This is a special case: GC may be triggered, but
	       p->runstack does not point at everything that needs
	       to be kept if args are lower on the stack. 
	       That's what runstack_tmp_keep is for. */
	    UPDATE_THREAD_RSPTR_FOR_GC();
	    p->runstack_tmp_keep = rands;

	    rest_vals = scheme_null;
	    for (i = num_rands - 1; i >= n; --i) {
	      pairs = scheme_alloc_object();
	      pairs->type = scheme_pair_type;
	      SCHEME_CDR(pairs) = rest_vals;
	      SCHEME_CAR(pairs) = rands[i];
	      rest_vals = pairs;
	    }

	    p->runstack_tmp_keep = NULL;

	    stack[n] = rest_vals;
	    while (n--) {
	      stack[n] = rands[n];
	    }
	  } else {
	    /* Possibly, but not necessarily, rands > stack: */
	    if ((unsigned long)rands > (unsigned long)stack) {
	      int i;
	      for (i = 0; i < n; i++) {
		stack[i] = rands[i];
	      }
	      stack[n] = scheme_null;
	    } else {
	      stack[n] = scheme_null;
	      while (n--) {
		stack[n] = rands[n];
	      }
	    }
	  }
	} else {
	  if (num_rands != num_params) {
	    if (num_rands < num_params) {
	      UPDATE_THREAD_RSPTR_FOR_ERROR();
	      scheme_wrong_count(scheme_get_proc_name(obj, NULL, 1), 
				 num_params, num_params,
				 num_rands, rands);
	      return NULL; /* Doesn't get here */
	    } else {
	      UPDATE_THREAD_RSPTR_FOR_ERROR();
	      scheme_wrong_count(scheme_get_proc_name(obj, NULL, 1), 
				 num_params, num_params,
				 num_rands, rands);
	      return NULL; /* Doesn't get here */
	    }
	  }
	
	  stack = RUNSTACK = old_runstack - num_params;
	  CHECK_RUNSTACK(p, RUNSTACK);
	  RUNSTACK_CHANGED();

	  if (rands != stack) {
	    int n = num_params; 
	    while (n--) {
	      stack[n] = rands[n];
	    }
	  }
	}
#ifdef AGRESSIVE_ZERO_TB
	if (rands == p->tail_buffer)
	  p->tail_buffer_set = 0;
#endif
      } else {
	if (num_rands) {
	  UPDATE_THREAD_RSPTR_FOR_ERROR();
	  scheme_wrong_count(scheme_get_proc_name(obj, NULL, 1),
			     0, 0, num_rands, rands);
	  return NULL; /* Doesn't get here */
	}
	RUNSTACK = old_runstack;
	RUNSTACK_CHANGED();
      }
      
      {
	int n = data->closure_size;
      
	if (n) {
	  src = SCHEME_COMPILED_CLOS_ENV(obj);
	  stack = PUSH_RUNSTACK(p, RUNSTACK, n);
	  RUNSTACK_CHANGED();

	  while (n--) {
	    stack[n] = src[n];
	  }
	}
      }

      obj = data->code;

      goto eval_top;
    } else if (type == scheme_closed_prim_type) {
      Scheme_Closed_Primitive_Proc *prim;
      
      DO_CHECK_FOR_BREAK(p, UPDATE_THREAD_RSPTR_FOR_GC(););
      
      if (rands == p->tail_buffer) {
	if (num_rands < TAIL_COPY_THRESHOLD) {
	  int i;
	  Scheme_Object **quick_rands;

	  quick_rands = PUSH_RUNSTACK(p, RUNSTACK, num_rands);
	  RUNSTACK_CHANGED();

	  for (i = num_rands; i--; ) {
	    quick_rands[i] = rands[i];
	  }
	  rands = quick_rands;
	} else {
	  UPDATE_THREAD_RSPTR_FOR_GC();
	  {
	    Scheme_Object **tb;
	    tb = MALLOC_N(Scheme_Object *, p->tail_buffer_size);
	    p->tail_buffer = tb;
	  }
	}
#ifdef AGRESSIVE_ZERO_TB
	p->tail_buffer_set = 0;
#endif
      }

      UPDATE_THREAD_RSPTR();

      prim = (Scheme_Closed_Primitive_Proc *)obj;
      
      if (num_rands < prim->mina 
	  || (num_rands > prim->maxa && prim->maxa >= 0)) {
	scheme_wrong_count(prim->name, prim->mina, prim->maxa, 
			   num_rands, rands);
	return NULL; /* Shouldn't get here */
      }
      
      v = prim->prim_val(prim->data, num_rands, rands);

      DEBUG_CHECK_TYPE(v);
    } else if (type == scheme_case_closure_type) {
      Scheme_Case_Lambda *seq;
      Scheme_Closure_Compilation_Data *data;
      
      int i;
      
      seq = (Scheme_Case_Lambda *)obj;
      for (i = 0; i < seq->count; i++) {
	data = (Scheme_Closure_Compilation_Data *)SCHEME_COMPILED_CLOS_CODE(seq->array[i]);
	if ((!(data->flags & CLOS_HAS_REST) 
	     && (data->num_params == num_rands))
	    || ((data->flags & CLOS_HAS_REST)
		&& (data->num_params - 1 <= num_rands))) {
	  obj = seq->array[i];
	  goto apply_top;
	}
      }
      
      UPDATE_THREAD_RSPTR_FOR_ERROR();
      scheme_wrong_count((char *)seq, -1, -1, num_rands, rands);

      return NULL; /* Doesn't get here. */
    } else if (type == scheme_cont_type) {
      Scheme_Cont *c;
      Scheme_Dynamic_Wind *dw, *common;
      Scheme_Object *value;
      
      if (num_rands != 1) {
	Scheme_Object **vals;
	int i;

	UPDATE_THREAD_RSPTR_FOR_GC();
	vals = MALLOC_N(Scheme_Object *, num_rands);
	for (i = num_rands; i--; ) {
	  vals[i] = rands[i];
	}

	value = (Scheme_Object *)vals;
      } else
	value = rands[0];
#ifdef AGRESSIVE_ZERO_TB
      if (rands == p->tail_buffer)
	p->tail_buffer_set = 0;
#endif
      
      c = (Scheme_Cont *)obj;
      
      DO_CHECK_FOR_BREAK(p, ;);

      if (NOT_SAME_OBJ(c->home, p)) {
	UPDATE_THREAD_RSPTR_FOR_ERROR();
	scheme_raise_exn(MZEXN_APPLICATION_CONTINUATION,
			 c,
			 "continuation application: attempted to apply foreign continuation"
			 " (created in another thread)");
      }
      if (c->ok && !*c->ok) {
	UPDATE_THREAD_RSPTR_FOR_ERROR();
	scheme_raise_exn(MZEXN_APPLICATION_CONTINUATION,
			 c,
			 "continuation application: attempted to cross a continuation boundary");
      }
      
      p->suspend_break = 1; /* restored at call/cc destination */

      /* Find `common', then intersection of dynamic-wind chain for 
	 the current continuation and the given continuation */
      common = p->dw;
      while (common) {
	dw = c->dw;
	while (dw && dw != common) {
	  dw = dw->prev;
	}
	if (dw)
	  break;
	common = common->prev;
      }
	
      c->common = common;
      /* For dynamaic-winds after `common' in this
	 continuation, execute the post-thunks */
      for (dw = p->dw; dw != common; dw = dw->prev) {
	if (dw->post) {
	  DW_PrePost_Proc post = dw->post;
	  p->dw = dw->prev;
	  post(dw->data);
	}
      }
      
      if (num_rands == 1)
	c->value = value;
      else {
	Scheme_Object *vals;
	vals = scheme_values(num_rands, (Scheme_Object **)value);
	c->value = vals;
      }
      scheme_longjmpup(&c->buf);
      
      return NULL;
    } else if (type == scheme_escaping_cont_type) {
      Scheme_Object *value;

      if (num_rands != 1) {
	Scheme_Object **vals;
	int i;

	UPDATE_THREAD_RSPTR_FOR_GC();
	vals = MALLOC_N(Scheme_Object *, num_rands);
	for (i = num_rands; i--; ) {
	  vals[i] = rands[i];
	}
	
	value = (Scheme_Object *)vals;
	p->cjs.num_vals = num_rands;
      } else {
	value = rands[0];
	p->cjs.num_vals = 1;
      }

#ifdef AGRESSIVE_ZERO_TB
      if (rands == p->tail_buffer)
	p->tail_buffer_set = 0;
#endif

      if (!SCHEME_CONT_HOME(obj)) {
	UPDATE_THREAD_RSPTR_FOR_ERROR();
	scheme_raise_exn(MZEXN_APPLICATION_CONTINUATION,
			 obj,
			 "continuation application: attempt to jump into an escape continuation");
      }
      if (NOT_SAME_OBJ(SCHEME_CONT_HOME(obj), p)) {
	UPDATE_THREAD_RSPTR_FOR_ERROR();
	scheme_raise_exn(MZEXN_APPLICATION_CONTINUATION,
			 obj,
			 "continuation application: attempted to apply foreign escape continuation"
			 " (created in another thread)");
      }
      if (SCHEME_CONT_OK(obj) && !*SCHEME_CONT_OK(obj)) {
	UPDATE_THREAD_RSPTR_FOR_ERROR();
	scheme_raise_exn(MZEXN_APPLICATION_CONTINUATION,
			 obj,
			 "continuation application: attempted to cross an escape continuation boundary");
      }
      p->cjs.u.val = value;
      p->cjs.jumping_to_continuation = (Scheme_Escaping_Cont *)obj;
      scheme_longjmp(MZTHREADELEM(p, error_buf), 1);
    } else {
      UPDATE_THREAD_RSPTR_FOR_ERROR();
      scheme_wrong_rator(obj, num_rands, rands);
      return NULL; /* Doesn't get here. */
    }
  } else {

  eval_top:

    if (SCHEME_INTP(obj)) {
      v = obj;
      goto returnv;
    }

    type = _SCHEME_TYPE(obj);
    switch (type)
      {
      case scheme_variable_type:
	{
#define global_lookup(prefix, _obj, tmp)                                \
	  tmp = (Scheme_Object *)(SCHEME_VAR_BUCKET(_obj))->val;        \
	  if (!tmp) {                                                   \
            UPDATE_THREAD_RSPTR_FOR_ERROR();                            \
	    scheme_unbound_global((Scheme_Object *)                     \
				  (SCHEME_VAR_BUCKET(_obj))->key);      \
            return NULL;                                                \
	  }                                                             \
	  prefix tmp

	  global_lookup(v = , obj, v);  
	  goto returnv;
	}
      case scheme_local_type:
	{
	  v = RUNSTACK[SCHEME_LOCAL_POS(obj)];
	  goto returnv;
	}
      case scheme_local_unbox_type:
	{
	  v = SCHEME_ENVBOX_VAL(RUNSTACK[SCHEME_LOCAL_POS(obj)]);
	  goto returnv;
	}
      case scheme_syntax_type:
	{
	  Scheme_Syntax_Executer *f;

	  UPDATE_THREAD_RSPTR();
	  f = (Scheme_Syntax_Executer *)SCHEME_PTR1_VAL(obj);
	  v = f((Scheme_Object *)SCHEME_PTR2_VAL(obj));
	  break;
	}
      case scheme_application_type:
	{
	  Scheme_App_Rec *app;
	  Scheme_Object *tmpv, **randsp, **stack;
	  int k;
	  int d_evals;
#ifdef MZ_PRECISE_GC
# define GET_FIRST_EVAL ((char *)app)[d_evals]
#else
	  char *evals;	  
	  Scheme_Object **args;
# define GET_FIRST_EVAL evals[0]
#endif

	  app = (Scheme_App_Rec *)obj;
	  num_rands = app->num_args;
	  
	  d_evals = sizeof(Scheme_App_Rec) + (num_rands * sizeof(Scheme_Object *));
#ifndef MZ_PRECISE_GC
	  evals = ((char *)obj) + d_evals;
#endif

	  obj = app->args[0];
	  
	  stack = PUSH_RUNSTACK(p, RUNSTACK, num_rands);
	  RUNSTACK_CHANGED();
	  UPDATE_THREAD_RSPTR();

	  /* Inline local & global variable lookups for speed */
	  switch (GET_FIRST_EVAL) {
	  case SCHEME_EVAL_CONSTANT:
	    break;
	  case SCHEME_EVAL_GLOBAL:
	    global_lookup(obj =, obj, tmpv);
	    break;
	  case SCHEME_EVAL_LOCAL:
	    obj = stack[SCHEME_LOCAL_POS(obj)];
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
	    obj = SCHEME_ENVBOX_VAL(stack[SCHEME_LOCAL_POS(obj)]);
	    break;
	  default:
	    obj = _scheme_eval_compiled_expr_wp(obj, p);
	    break;
	  }

	  if (num_rands) {
#ifdef MZ_PRECISE_GC
	    int evalpos = 1;
#endif

	    rands = stack;
	
	    /* Inline local & global variable lookups for speed */
#ifdef MZ_PRECISE_GC
# define GET_NEXT_EVAL ((char *)app)[d_evals + evalpos++]	    
# define GET_NEXT_ARG app->args[evalpos]
#else
	    evals++;
	    args = app->args + 1;
# define GET_NEXT_EVAL *(evals++)
# define GET_NEXT_ARG *(args++)
#endif
	    randsp = rands;
	    for (k = num_rands; k--; ) {
	      v = GET_NEXT_ARG;
	      switch (GET_NEXT_EVAL) {
	      case SCHEME_EVAL_CONSTANT:
		*(randsp++) = v;
		break;
	      case SCHEME_EVAL_GLOBAL:
		global_lookup(*(randsp++) =, v, tmpv);
		break;
	      case SCHEME_EVAL_LOCAL:
		*(randsp++) = stack[SCHEME_LOCAL_POS(v)];
		break;
	      case SCHEME_EVAL_LOCAL_UNBOX:
		*(randsp++) = SCHEME_ENVBOX_VAL(stack[SCHEME_LOCAL_POS(v)]);
		break;
	      default:
		{
		  Scheme_Object *er;
		  er = _scheme_eval_compiled_expr_wp(v, p);
		  *(randsp++) = er;
		}
		break;
	      }

	      DEBUG_CHECK_TYPE(randsp[-1]);
	    }
	  } else
	    rands = &zero_rands_ptr;
      
	  goto apply_top;
	}
      
      case scheme_sequence_type:
	{
	  int cnt;
	  int i;
	  
	  cnt = ((Scheme_Sequence *)obj)->count - 1;
	  
	  UPDATE_THREAD_RSPTR();
	  for (i = 0; i < cnt; i++) {
	    (void)_scheme_eval_compiled_expr_multi_wp(((Scheme_Sequence *)obj)->array[i], p);
	  }

	  obj = ((Scheme_Sequence *)obj)->array[cnt];
	  goto eval_top;
	}
      case scheme_branch_type:
	{
	  Scheme_Branch_Rec *b;
	  b = (Scheme_Branch_Rec *)obj;

	  UPDATE_THREAD_RSPTR();
	  obj = NOT_SAME_OBJ(_scheme_eval_compiled_expr_wp(b->test, p), scheme_false)
	    ? b->tbranch : b->fbranch;

	  goto eval_top;
	}
      case scheme_unclosed_procedure_type:
	UPDATE_THREAD_RSPTR();
	v = scheme_make_linked_closure(p, obj, 1);
	goto returnv;

      case scheme_let_value_type:
	{
	  Scheme_Let_Value *lv;
	  Scheme_Object *value, **values;
	  int i, c, ab;

	  lv = (Scheme_Let_Value *)obj;

	  c = lv->count;

	  i = lv->position;
	  ab = lv->autobox;
	  value = lv->value;
	  obj = lv->body;

	  UPDATE_THREAD_RSPTR();

	  if (c == 1) {
	    value = _scheme_eval_compiled_expr_wp(value, p);
	    if (ab)
	      SCHEME_ENVBOX_VAL(RUNSTACK[i]) = value;
	    else
	      RUNSTACK[i] = value;
	  } else {
	    int c2;
	    Scheme_Object **stack;

	    value = _scheme_eval_compiled_expr_multi_wp(value, p);
	    c2 = (SAME_OBJ(value, SCHEME_MULTIPLE_VALUES) ? p->ku.multiple.count : 1);
	    if (c2 != c) {
	      scheme_wrong_return_arity(NULL, c, c2, 
					(c2 == 1) ? (Scheme_Object **)value : p->ku.multiple.array,
					"lexical binding");
	      return NULL;
	    }

	    /* Precise GC: values++ is ok because we exit the block
	       before any GC can happen. */

	    values = p->ku.multiple.array;
#ifdef AGRESSIVE_ZERO_FOR_GC
	    p->ku.multiple.array = NULL;
#endif
	    stack = RUNSTACK;
	    if (ab) {
	      while (c--) {
		SCHEME_ENVBOX_VAL(stack[i]) = *values;
		values++;
		i++;
	      }
	    } else {
	      while (c--) {
		stack[i] = *values;
		values++;
		i++;
	      }
	    }
	  }

	  goto eval_top;
	}

      case scheme_let_void_type:
	{
	  Scheme_Let_Void *lv;
	  int c;

	  lv = (Scheme_Let_Void *)obj;
	  c = lv->count;
	  obj = lv->body;

	  PUSH_RUNSTACK(p, RUNSTACK, c);
	  RUNSTACK_CHANGED();

	  if (lv->autobox) {
	    Scheme_Object **stack = RUNSTACK;

	    UPDATE_THREAD_RSPTR_FOR_GC();

	    while (c--) {
	      Scheme_Object *ub;
	      ub = scheme_make_envunbox(scheme_undefined);
	      stack[c] = ub;
	    }
	  }

	  goto eval_top;
	}

      case scheme_letrec_type:
	{
	  Scheme_Letrec *l = (Scheme_Letrec *)obj;
	  Scheme_Object **a, **dest, **stack;
	  short *map;
	  int i;

	  stack = RUNSTACK;
	  a = l->procs;
	  i = l->count;
	  
	  UPDATE_THREAD_RSPTR_FOR_GC();

	  /* Create unfinished closures */
	  while (i--) {
	    Scheme_Object *uc;
	    uc = scheme_make_linked_closure(p, a[i], 0);
	    stack[i] = uc;
	  }

	  /* Close them: */
	  i = l->count;
	  while (i--) {
	    Scheme_Closed_Compiled_Procedure *closure;
	    Scheme_Closure_Compilation_Data *data;
	    int j;

	    closure = (Scheme_Closed_Compiled_Procedure *)stack[i];
	    data = (Scheme_Closure_Compilation_Data *)a[i];

	    map = data->closure_map;
	    j = data->closure_size;
	    dest = closure->vals;

	    while (j--) {
	      dest[j] = stack[map[j]];
	    }
	  }

	  obj = l->body;
	  goto eval_top;
	}

      case scheme_let_one_type:
	{
	  Scheme_Let_One *lo = (Scheme_Let_One *)obj;

	  PUSH_RUNSTACK(p, RUNSTACK, 1);
	  RUNSTACK_CHANGED();

	  switch (lo->eval_type) {
	  case SCHEME_EVAL_CONSTANT:
	    RUNSTACK[0] = lo->value;
	    break;
	  case SCHEME_EVAL_GLOBAL:
	    {
	      Scheme_Object *tmpv;
	      global_lookup(RUNSTACK[0] =, lo->value, tmpv);
	    }
	    break;
	  case SCHEME_EVAL_LOCAL:
	    RUNSTACK[0] = RUNSTACK[SCHEME_LOCAL_POS(lo->value)];
	    break;
	  case SCHEME_EVAL_LOCAL_UNBOX:
	    RUNSTACK[0] = SCHEME_ENVBOX_VAL(RUNSTACK[SCHEME_LOCAL_POS(lo->value)]);
	    break;
	  default:
	    UPDATE_THREAD_RSPTR();
	    {
	      Scheme_Object *val;
	      val = _scheme_eval_compiled_expr_wp(lo->value, p);
	      RUNSTACK[0] = val;
	    }
	    break;
	  }

	  obj = lo->body;

	  goto eval_top;
	}
      
      case scheme_with_cont_mark_type:
	{
	  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)obj;
	  Scheme_Object *key, *val;
	  
	  UPDATE_THREAD_RSPTR();
	  key = wcm->key;
	  if (SCHEME_TYPE(key) < _scheme_values_types_)
	    key = _scheme_eval_compiled_expr_wp(wcm->key, p);
	  val = wcm->val;
	  if (SCHEME_TYPE(val) < _scheme_values_types_)
	    val = _scheme_eval_compiled_expr_wp(wcm->val, p);

	  scheme_set_cont_mark(key, val);

	  obj = wcm->body;

	  goto eval_top;
	}
      
      default:
	v = obj;
	goto returnv;
      }
  }

  if (SAME_OBJ(v, SCHEME_TAIL_CALL_WAITING)) {
    obj = p->ku.apply.tail_rator;
    num_rands = p->ku.apply.tail_num_rands;
    rands = p->ku.apply.tail_rands;
#ifdef AGRESSIVE_ZERO_FOR_GC
    p->ku.apply.tail_rands = NULL;
#endif
    RUNSTACK = old_runstack;
    RUNSTACK_CHANGED();
    goto apply_top;
  }

  if (SAME_OBJ(v, SCHEME_EVAL_WAITING)) {
    RESET_LOCAL_RUNSTACK();
    obj = p->ku.eval.wait_expr;
#ifdef AGRESSIVE_ZERO_FOR_GC
    p->ku.eval.wait_expr = NULL;
#endif
    goto eval_top;
  }

 returnv:

  if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES))
    if (get_value > 0) {
      scheme_wrong_return_arity(NULL, 1, p->ku.multiple.count, 
				p->ku.multiple.array,
				NULL);
      return NULL;
    }

  MZ_RUNSTACK = old_runstack;
  MZ_CONT_MARK_STACK = old_cont_mark_stack;
  MZ_CONT_MARK_POS -= 2;

  DEBUG_CHECK_TYPE(v);

  return v;
}

Scheme_Object *scheme_eval(Scheme_Object *obj, Scheme_Env *env)
{
  return scheme_eval_compiled(scheme_compile(obj, env, 0));
}

Scheme_Object *scheme_eval_multi(Scheme_Object *obj, Scheme_Env *env)
{
  return scheme_eval_compiled_multi(scheme_compile(obj, env, 0));
}

static void *eval_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *v;
  int isexpr, multi;

  v = (Scheme_Object *)p->ku.k.p1;
  p->ku.k.p1 = NULL;
  isexpr = p->ku.k.i2;
  multi = p->ku.k.i1;
    
  if (isexpr) {
    if (multi)
      v = _scheme_eval_compiled_expr_multi_wp(v, p);
    else
      v = _scheme_eval_compiled_expr_wp(v, p);
  } else if (SAME_TYPE(SCHEME_TYPE(v), scheme_compilation_top_type)) {
    Scheme_Compilation_Top *top = (Scheme_Compilation_Top *)v;

    if (!scheme_check_runstack(top->max_let_depth)) {
      p->ku.k.p1 = top;
      p->ku.k.i1 = multi;
      p->ku.k.i2 = isexpr;
      return (Scheme_Object *)scheme_enlarge_runstack(top->max_let_depth, eval_k);
    }

    v = top->code;    

    if (multi)
      v = _scheme_eval_compiled_expr_multi_wp(v, p);
    else
      v = _scheme_eval_compiled_expr_wp(v, p);
  } else {
    v = scheme_void;
  }

  return (void *)v;
}

static Scheme_Object *_eval(Scheme_Object *obj, int isexpr, int multi, int top)
{
  Scheme_Process *p = scheme_current_process;
  
  p->ku.k.p1 = obj;
  p->ku.k.i1 = multi;
  p->ku.k.i2 = isexpr;

  if (top)
    return (Scheme_Object *)scheme_top_level_do(eval_k, 1);
  else
    return (Scheme_Object *)eval_k();
}

Scheme_Object *scheme_eval_compiled(Scheme_Object *obj)
{
  return _eval(obj, 0, 0, 1);
}

Scheme_Object *scheme_eval_compiled_multi(Scheme_Object *obj)
{
  return _eval(obj, 0, 1, 1);
}

Scheme_Object *_scheme_eval_compiled(Scheme_Object *obj)
{
  return _eval(obj, 0, 0, 0);
}

Scheme_Object *_scheme_eval_compiled_multi(Scheme_Object *obj)
{
  return _eval(obj, 0, 1, 0);
}

Scheme_Object *scheme_eval_compiled_expr(Scheme_Object *obj)
{
  return _eval(obj, 1, 0, 1);
}

static void *expand_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *obj;
  Scheme_Comp_Env *env;

  obj = (Scheme_Object *)p->ku.k.p1;
  env = (Scheme_Comp_Env *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return scheme_expand_expr(obj, env, p->ku.k.i1);
}

static Scheme_Object *_expand(Scheme_Object *obj, Scheme_Comp_Env *env, int depth, int eb)
{
  Scheme_Process *p = scheme_current_process;

  p->ku.k.p1 = obj;
  p->ku.k.p2 = env;
  p->ku.k.i1 = depth;

  return (Scheme_Object *)scheme_top_level_do(expand_k, eb);
}

Scheme_Object *scheme_expand(Scheme_Object *obj, Scheme_Env *env)
{
  return _expand(obj, env->init, 1, -1);
}

Scheme_Object *scheme_tail_eval_expr(Scheme_Object *obj)
{
  return scheme_tail_eval(obj);
}

static Scheme_Object *
do_default_eval_handler(Scheme_Env *env, int argc, Scheme_Object **argv)
{
  Scheme_Object *v;

  v = _compile(argv[0], env, 0, 0);

  return _eval(v, 0, 1, 0);
}

/* local functions */

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Config *config;
  Scheme_Object *e;
  Scheme_Object *namespace;
  Scheme_Object *old;
} Eval_In_Env;

static void pre_eval_in_env(void *e)
{
  Eval_In_Env *ee = (Eval_In_Env *)e;
  ee->old = scheme_get_param(ee->config, MZCONFIG_ENV);
  scheme_set_param(ee->config, MZCONFIG_ENV, ee->namespace);
}

static void post_eval_in_env(void *e)
{
  Eval_In_Env *ee = (Eval_In_Env *)e;
  scheme_set_param(ee->config, MZCONFIG_ENV, ee->old);
}

static Scheme_Object *do_eval_in_env(void *e)
{
  Eval_In_Env *ee = (Eval_In_Env *)e;
  return _scheme_apply_multi(scheme_get_param(ee->config, MZCONFIG_EVAL_HANDLER),
			     1, &ee->e);
}

static Scheme_Object *
eval(int argc, Scheme_Object *argv[])
{
  if (argc == 1) {
    return _scheme_apply_multi(scheme_get_param(scheme_config, MZCONFIG_EVAL_HANDLER),
			       1, argv);
  } else {
    Eval_In_Env *ee;
    
    if (SCHEME_TYPE(argv[1]) != scheme_namespace_type)
      scheme_wrong_type("eval", "namespace", 1, argc, argv);

    ee = MALLOC_ONE_RT(Eval_In_Env);
#ifdef MZTAG_REQUIRED
    ee->type = scheme_rt_eval_in_env;
#endif
    ee->e = argv[0];
    ee->config = scheme_config;
    ee->namespace = argv[1];

    return scheme_dynamic_wind(pre_eval_in_env, do_eval_in_env, post_eval_in_env,
			       NULL, ee);
  }
}

Scheme_Object *
scheme_default_eval_handler(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  env = scheme_get_env(scheme_config);

  return do_default_eval_handler(env, argc, argv);
}

static Scheme_Object *
current_eval(int argc, Scheme_Object **argv)
{
  return scheme_param_config("eval-handler", 
			     scheme_make_integer(MZCONFIG_EVAL_HANDLER),
			     argc, argv,
			     1, NULL, NULL, 0);
}

static Scheme_Object *
compile(int argc, Scheme_Object *argv[])
{
  return _compile(argv[0], scheme_get_env(scheme_config), 1, 0);
}

#if OPT_COMPILE_VISIBLE
static Scheme_Object *
compile_x(int argc, Scheme_Object *argv[])
{
  Scheme_Compile_Info rec;

  rec.can_optimize_constants = 1;
  rec.value_name = NULL;
  return scheme_link_expr(scheme_compile_expr(argv[0], 
					      scheme_get_env(scheme_config)->init, 
					      &rec, 0),
			  scheme_link_info_create(1));
}
#endif



static Scheme_Object *expand(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  env = scheme_get_env(scheme_config);

  return _expand(argv[0], env->init, -1, 0);
}

static Scheme_Comp_Env *local_expand_extend_env(Scheme_Object *locals, 
						Scheme_Comp_Env *env)
{
  Scheme_Object *l;
  
  for (l = locals; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    if (!SCHEME_SYMBOLP(SCHEME_CAR(l)))
      return NULL;
  }
  if (!SCHEME_NULLP(l))
    return NULL;
  
  return scheme_add_compilation_frame(locals, env, 0);
}

static Scheme_Object *
local_expand(int argc, Scheme_Object **argv)
{
  Scheme_Comp_Env *env;

  env = scheme_current_process->current_local_env;
  if (env && (argc > 1)) {
    env = local_expand_extend_env(argv[1], env);
    if (!env)
      scheme_wrong_type("local-expand-defmacro", "list of symbols", 1, argc, argv);
  }
  if (!env)
    scheme_raise_exn(MZEXN_MISC,
		     "local-expand-defmacro: illegal at run-time");

  return _expand(argv[0], env, -1, 0);
}

static Scheme_Object *
local_expand_body_expression(int argc, Scheme_Object **argv)
{
  Scheme_Comp_Env *env;
  Scheme_Object *expr, *a[2], *gval;

  env = scheme_current_process->current_local_env;
  if (env && (argc > 1)) {
    env = local_expand_extend_env(argv[1], env);
    if (!env)
      scheme_wrong_type("local-expand-body-expression", "list of symbols", 1, argc, argv);
  }
  if (!env)
    scheme_raise_exn(MZEXN_MISC,
		     "local-expand-body-expression: illegal at run-time");

  /* Check for macro expansion, which could mask the real define-values */
  expr = argv[0];
  if (SCHEME_PAIRP(expr))
    expr = scheme_check_immediate_macro(argv[0], env, NULL, 0, -1, &gval);
  else
    gval = NULL;

  a[0] = expr;
  if (SAME_OBJ(gval, scheme_begin_syntax)) {
    a[1] = begin_symbol;
  } else if (SAME_OBJ(gval, scheme_define_values_syntax)) {
    a[1] = define_values_symbol;
  } else {
    a[1] = scheme_false;
  }

  return scheme_values(2, a);
}

static Scheme_Object *
expand_once(int argc, Scheme_Object **argv)
{
  Scheme_Env *env;

  env = scheme_get_env(scheme_config);

  return _expand(argv[0], env->init, 1, 0);
}

Scheme_Object *scheme_eval_string_all(const char *str, Scheme_Env *env, int cont)
{
  Scheme_Object *port, *expr, *result = scheme_void;

  port = scheme_make_string_input_port(str);
  do {
    expr = scheme_read(port);
    if (SAME_OBJ(expr, scheme_eof))
      cont = 0;
    else if (cont < 0)
      result = scheme_eval(expr, env);
    else
      result = scheme_eval_multi(expr, env);
  } while (cont > 0);

  return result;
}

Scheme_Object *scheme_eval_string(const char *str, Scheme_Env *env)
{
  return scheme_eval_string_all(str, env, -1);
}

Scheme_Object *scheme_eval_string_multi(const char *str, Scheme_Env *env)
{
  return scheme_eval_string_all(str, env, 0);
}

static Scheme_Object *allow_auto_cond_else(int argc, Scheme_Object **argv)
{
  return scheme_param_config("compile-auto-cond-else", 
			     scheme_make_integer(MZCONFIG_COND_AUTO_ELSE),
			     argc, argv,
			     -1, NULL, NULL, 1);
}

static Scheme_Object *allow_set_undefined(int argc, Scheme_Object **argv)
{
  return scheme_param_config("compile-allow-set!-undefined", 
			     scheme_make_integer(MZCONFIG_ALLOW_SET_UNDEFINED),
			     argc, argv,
			     -1, NULL, NULL, 1);
}

static Scheme_Object *
enable_break(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;
  Scheme_Process *p = scheme_current_process;

  v = scheme_param_config("break-enabled", 
			  scheme_make_integer(MZCONFIG_ENABLE_BREAK),
			  argc, argv,
			  -1, NULL, NULL, 1);

  if (argc == 1) /* might have turned on breaking... */
    if (p->external_break && scheme_can_break(p, p->config))
      scheme_process_block_w_process(0.0, p);

  return v;
}

/****************************************************/

#define BOOL(x) (x ? scheme_true : scheme_false)

#define RD_TM_OPT 1

static Scheme_Object *write_application(Scheme_Object *obj)
{
  Scheme_App_Rec *app;
  Scheme_Object *l;
  int i;

  app = (Scheme_App_Rec *)obj;

  l = scheme_null;
  for (i = app->num_args + 1; i--; ) {
    l = cons(app->args[i], l);
  }
  
  return l;
}

static Scheme_Object *read_application(Scheme_Object *obj)
{
  /* (void) optimization: */
  if (SCHEME_PAIRP(obj) && SCHEME_NULLP(SCHEME_CDR(obj))
      && SAME_OBJ(SCHEME_CAR(obj), scheme_void_func))
    return scheme_void;

  return make_application(NULL, obj, RD_TM_OPT, 1);
}

static Scheme_Object *write_sequence(Scheme_Object *obj)
{
  Scheme_Object *l, **a;
  int i;

  i = ((Scheme_Sequence *)obj)->count;
  a = ((Scheme_Sequence *)obj)->array;

  l = scheme_null;
  for (; i--; ) {
    l = cons(a[i], l);
  }
  
  return l;
}

static Scheme_Object *read_sequence(Scheme_Object *obj)
{
  return scheme_make_sequence_compilation(obj, RD_TM_OPT, RD_TM_OPT);
}

static Scheme_Object *read_sequence_save_first(Scheme_Object *obj)
{
  return scheme_make_sequence_compilation(obj, 1, -1);
}

static Scheme_Object *write_branch(Scheme_Object *obj)
{
  Scheme_Branch_Rec *b;

  b = (Scheme_Branch_Rec *)obj;

  return cons(b->test, cons(b->tbranch, b->fbranch));
}

#define MAKE_BRANCH scheme_make_branch

static Scheme_Object *read_branch(Scheme_Object *obj)
{
#if 0
  if (!SCHEME_PAIRP(obj) || !SCHEME_PAIRP(SCHEME_CDR(obj)))
    scheme_signal_error("bad compiled branch");
#endif

  return MAKE_BRANCH(SCHEME_CAR(obj), 
		     SCHEME_CAR(SCHEME_CDR(obj)),
		     SCHEME_CDR(SCHEME_CDR(obj)));
}

static Scheme_Object *write_with_cont_mark(Scheme_Object *obj)
{
  Scheme_With_Continuation_Mark *wcm;

  wcm = (Scheme_With_Continuation_Mark *)obj;

  return cons(wcm->key, cons(wcm->val, wcm->body));
}

static Scheme_Object *read_with_cont_mark(Scheme_Object *obj)
{
  Scheme_With_Continuation_Mark *wcm;

  wcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
  wcm->type = scheme_with_cont_mark_type;
  wcm->key = SCHEME_CAR(obj);
  wcm->val = SCHEME_CADR(obj);
  wcm->body = SCHEME_CDR(SCHEME_CDR(obj));

  return (Scheme_Object *)wcm;
}

static Scheme_Object *write_syntax(Scheme_Object *obj)
{
  Scheme_Object *sym;
  Scheme_Syntax_Registered *f;

  f = (Scheme_Syntax_Registered *)SCHEME_PTR1_VAL(obj);
  sym = scheme_find_linker_name(f);
  if (!sym)
    sym = unknown_symbol;

  return cons(sym, (Scheme_Object *)SCHEME_PTR2_VAL(obj));
}

static Scheme_Object *read_syntax(Scheme_Object *obj)
{
  Scheme_Object *sym;
  Scheme_Syntax_Registered *f;
  Scheme_Object *first = NULL, *last = NULL;

#if 0
  if (!SCHEME_PAIRP(obj) || !SCHEME_SYMBOLP(SCHEME_CAR(obj)))
    scheme_signal_error("bad compiled syntax");
#endif

  sym = SCHEME_CAR(obj);
  if (SAME_OBJ(unknown_symbol, sym))
    scheme_signal_error("unknown (at write time) compiled syntax");

  if (SAME_OBJ(void_link_symbol, sym))
    return scheme_void;

  f = scheme_find_linker(sym);
  if (!f)
    scheme_signal_error("unknown compiled syntax: %S", sym);

  /* Copy obj: */
  obj = SCHEME_CDR(obj);
  while (SCHEME_PAIRP(obj)) {
    Scheme_Object *p;
    p = scheme_make_pair(SCHEME_CAR(obj), scheme_null);
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;
    obj = SCHEME_CDR(obj);
  }
  
  if (last)
    SCHEME_CDR(last) = obj;
  else
    first = obj;

  return scheme_make_syntax_link(f, first);
}

/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_EVAL_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_compile_info, mark_comp_info);
  GC_REG_TRAV(scheme_rt_saved_stack, mark_saved_stack);
  GC_REG_TRAV(scheme_rt_eval_in_env, mark_eval_in_env);
}

END_XFORM_SKIP;

#endif
