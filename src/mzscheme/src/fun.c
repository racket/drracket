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
#ifdef TIME_SYNTAX
# include <time.h>
# ifdef USE_FTIME
#  include <sys/timeb.h>
# else
#  ifndef USE_DIFFTIME
#   include <sys/time.h>
#  endif /* USE_DIFFTIME */
# endif /* USE_FTIME */
# ifdef USE_GETRUSAGE
#  include <sys/types.h>
#  include <sys/time.h>
#  include <sys/resource.h>
# endif /* USE_GETRUSAGE */
# ifdef USE_SYSCALL_GETRUSAGE
#  include <sys/syscall.h>
#  define getrusage(a, b)  syscall(SYS_GETRUSAGE, a, b)
#  define USE_GETRUSAGE
# endif /* USE_SYSCALL_GETRUSAGE */
#endif /* TIME_SYNTAX */

/* globals */
int scheme_escape_continuations_only; 

int scheme_defining_primitives;

Scheme_Object *scheme_void;
Scheme_Object *scheme_void_func;

Scheme_Object *scheme_tail_call_waiting;

/* locals */
static Scheme_Object *procedure_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *apply (int argc, Scheme_Object *argv[]);
static Scheme_Object *map (int argc, Scheme_Object *argv[]);
static Scheme_Object *for_each (int argc, Scheme_Object *argv[]);
static Scheme_Object *andmap (int argc, Scheme_Object *argv[]);
static Scheme_Object *ormap (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_ec (int argc, Scheme_Object *argv[]);
static Scheme_Object *call_cc (int argc, Scheme_Object *argv[]);
static Scheme_Object *void_func (int argc, Scheme_Object *argv[]);
static Scheme_Object *is_void_func (int argc, Scheme_Object *argv[]);
static Scheme_Object *dynamic_wind (int argc, Scheme_Object *argv[]);
#ifdef TIME_SYNTAX
static Scheme_Object *time_apply(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_milliseconds(int argc, Scheme_Object **argv);
static Scheme_Object *current_process_milliseconds(int argc, Scheme_Object **argv);
static Scheme_Object *current_gc_milliseconds(int argc, Scheme_Object **argv);
static Scheme_Object *current_seconds(int argc, Scheme_Object **argv);
static Scheme_Object *seconds_to_date(int argc, Scheme_Object **argv);
#endif
static Scheme_Object *inferred_name(int argc, Scheme_Object *argv[]);
static Scheme_Object *arity(int argc, Scheme_Object *argv[]);
static Scheme_Object *procedure_arity_includes(int argc, Scheme_Object *argv[]);
static Scheme_Object *primitive_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *primitive_closure_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *primitive_name(int argc, Scheme_Object *argv[]);
static Scheme_Object *primitive_result_arity (int argc, Scheme_Object *argv[]);
static Scheme_Object *syntax_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *macro_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *id_macro_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *exp_time_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *call_with_values(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_values(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_print(int argc, Scheme_Object **argv);
static Scheme_Object *current_prompt_read(int, Scheme_Object **);

static Scheme_Object *default_print_handler(int, Scheme_Object *[]);
static Scheme_Object *default_prompt_read_handler(int, Scheme_Object *[]);

static Scheme_Object *get_or_check_arity(Scheme_Object *p, long a);

static Scheme_Object *write_compiled_closure(Scheme_Object *obj);
static Scheme_Object *read_compiled_closure(Scheme_Object *obj);

static Scheme_Object *rep;

#define CONS(a,b) scheme_make_pair(a,b)

#define DYNAMIC_WIND "dynamic-wind"

#ifdef USE_DIFFTIME
static time_t base_time;
#endif

/* See call_cc: */
typedef struct Scheme_Dynamic_Wind_List {
  Scheme_Dynamic_Wind *dw;
  struct Scheme_Dynamic_Wind_List *next;
} Scheme_Dynamic_Wind_List;

void
scheme_init_fun (Scheme_Env *env)
{
  Scheme_Object *o;

  if (scheme_starting_up) {
    REGISTER_SO(scheme_void);
    REGISTER_SO(scheme_void_func);
    REGISTER_SO(scheme_tail_call_waiting);

#ifdef USE_DIFFTIME
    base_time = time(NULL);
#endif

    scheme_void = scheme_alloc_eternal_object();
    scheme_void->type = scheme_void_type;

#ifdef MZ_APPLY_WAITING_CONSTANT
    scheme_tail_call_waiting = MZ_APPLY_WAITING_CONSTANT;
#else
    scheme_tail_call_waiting = scheme_alloc_eternal_object();
    scheme_tail_call_waiting->type = scheme_tail_call_waiting_type;
#endif

    scheme_void_func = scheme_make_folding_prim(void_func, "void", 0, -1, 1);
  }

  scheme_add_global_constant("procedure?", 
			     scheme_make_folding_prim(procedure_p, 
						      "procedure?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("apply", 
			     scheme_make_prim_w_arity2(apply,  
						       "apply", 
						       2, -1,
						       0, -1), 
			     env);
  scheme_add_global_constant("map", 
			     scheme_make_prim_w_arity(map,  
						      "map", 
						      2, -1), 
			     env);
  scheme_add_global_constant("for-each", 
			     scheme_make_prim_w_arity(for_each,  
						      "for-each", 
						      2, -1), 
			     env);
  scheme_add_global_constant("andmap", 
			     scheme_make_prim_w_arity(andmap,  
						      "andmap", 
						      2, -1), 
			     env);
  scheme_add_global_constant("ormap", 
			     scheme_make_prim_w_arity(ormap,  
						      "ormap", 
						      2, -1), 
			     env);
  scheme_add_global_constant("call-with-values",
			     scheme_make_prim_w_arity2(call_with_values,  
						       "call-with-values",
						       2, 2,
						       0, -1),
			     env);
  scheme_add_global_constant("values",
			     scheme_make_prim_w_arity2(scheme_values,  
						       "values",
						       0, -1,
						       0, -1),
			     env);

  o = scheme_make_prim_w_arity2(call_ec,  
				"call-with-escaping-continuation",
				1, 1,
				0, -1), 
  scheme_add_global_constant("call-with-escaping-continuation", o, env);
  scheme_add_global_constant("call/ec", o, env);  

  if (!scheme_escape_continuations_only)
    o = scheme_make_prim_w_arity2(call_cc,  
				  "call-with-current-continuation", 
				  1, 1,
				  0, -1);
  
  scheme_add_global_constant("call-with-current-continuation", o, env);
  scheme_add_global_constant("call/cc", o, env);  

  scheme_add_global_constant("void", scheme_void_func, env);  
  scheme_add_global_constant("void?", 
			     scheme_make_folding_prim(is_void_func,  
						      "void?", 
						      1, 1, 1), 
			     env);  
#ifdef TIME_SYNTAX
  scheme_add_global_constant("time-apply", 
			     scheme_make_prim_w_arity2(time_apply,  
						       "time-apply", 
						       1, 1,
						       3, 3), 
			     env);
  scheme_add_global_constant("current-milliseconds",
			     scheme_make_prim_w_arity(current_milliseconds,
						      "current-milliseconds",
						      0, 0),
			     env);
  scheme_add_global_constant("current-process-milliseconds",
			     scheme_make_prim_w_arity(current_process_milliseconds,
						      "current-process-milliseconds",
						      0, 0),
			     env);
  scheme_add_global_constant("current-gc-milliseconds",
			     scheme_make_prim_w_arity(current_gc_milliseconds,
						      "current-gc-milliseconds",
						      0, 0),
			     env);
  scheme_add_global_constant("current-seconds",
			     scheme_make_prim_w_arity(current_seconds,
						      "current-seconds",
						      0, 0),
			     env);
  scheme_add_global_constant("seconds->date",
			     scheme_make_prim_w_arity(seconds_to_date,
						      "seconds->date",
						      1, 1),
			     env);
#endif

  scheme_add_global_constant(DYNAMIC_WIND, 
			     scheme_make_prim_w_arity(dynamic_wind,  
						      DYNAMIC_WIND, 
						      3, 3), 
			     env);

  scheme_add_global_constant("inferred-name", 
			     scheme_make_folding_prim(inferred_name,  
						      "inferred-name", 
						      1, 1, 1), 
			     env);

  scheme_add_global_constant("arity", 
			     scheme_make_folding_prim(arity,  
						      "arity", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("procedure-arity-includes?", 
			     scheme_make_folding_prim(procedure_arity_includes,  
						      "procedure-arity-includes?", 
						      2, 2, 1), 
			     env);

  scheme_add_global_constant("primitive?", 
			     scheme_make_folding_prim(primitive_p, 
						      "primitive?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("primitive-closure?", 
			     scheme_make_folding_prim(primitive_closure_p, 
						      "primitive-closure?", 
						      1, 1, 1), 
			     env);

  scheme_add_global_constant("primitive-name", 
			     scheme_make_folding_prim(primitive_name, 
						      "primitive-name", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("primitive-result-arity", 
			     scheme_make_folding_prim(primitive_result_arity, 
						      "primitive-result-arity", 
						      1, 1, 1), 
			     env);

  scheme_add_global_constant("syntax?", 
			     scheme_make_folding_prim(syntax_p, 
						      "syntax?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("macro?", 
			     scheme_make_folding_prim(macro_p, 
						      "macro?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("id-macro?", 
			     scheme_make_folding_prim(id_macro_p, 
						      "id-macro?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("expansion-time-value?", 
			     scheme_make_folding_prim(exp_time_p, 
						      "expansion-time-value?", 
						      1, 1, 1), 
			     env);
  
  scheme_add_global_constant("current-print",
			     scheme_register_parameter(current_print, 
						       "current-print",
						       MZCONFIG_PRINT_HANDLER),
			     env);
  scheme_add_global_constant("current-prompt-read",
			     scheme_register_parameter(current_prompt_read, 
						       "current-prompt-read",
						       MZCONFIG_PROMPT_READ_HANDLER),
			     env);


  if (scheme_starting_up) {
    Scheme_Config *config = scheme_config;
    
    scheme_set_param(config, MZCONFIG_PRINT_HANDLER,
		     scheme_make_prim_w_arity(default_print_handler,
					      "default-print-handler",
					      1, 1));

    scheme_set_param(config, MZCONFIG_PROMPT_READ_HANDLER,
		     scheme_make_prim_w_arity(default_prompt_read_handler,
					      "default-prompt-read-handler",
					      0, 0));
  
    scheme_install_type_writer(scheme_unclosed_procedure_type,
			       write_compiled_closure);
    scheme_install_type_reader(scheme_unclosed_procedure_type,
			       read_compiled_closure);
  }
}

void scheme_init_rep(Scheme_Env *env)
{
  REGISTER_SO(rep);
  rep = scheme_lookup_global(scheme_intern_symbol("#%read-eval-print-loop"), env);
}

Scheme_Object *
scheme_force_value(Scheme_Object *obj)
{
  if (SAME_OBJ(obj, SCHEME_TAIL_CALL_WAITING)) {
    Scheme_Process *p = scheme_current_process;
    Scheme_Object *v;

    v = _scheme_apply_multi(p->ku.apply.tail_rator, 
			    p->ku.apply.tail_num_rands, 
			    p->ku.apply.tail_rands);
    return v;
  } else if (SAME_OBJ(obj, SCHEME_EVAL_WAITING)) {
    Scheme_Process *p = scheme_current_process;
    return _scheme_eval_compiled_expr(p->ku.eval.wait_expr);
  } else if (obj)
    return obj;
  else
    return scheme_void;
}

Scheme_Object *
scheme_make_prim_w_everything(Scheme_Prim *fun, int eternal,
			      const char *name,
			      short mina, short maxa,
			      short folding,
			      short minr, short maxr)
{
  Scheme_Primitive_Proc *prim;
  int hasr, size;

  hasr = ((minr != 1) || (maxr != 1));
  size = hasr ? sizeof(Scheme_Prim_W_Result_Arity) : sizeof(Scheme_Primitive_Proc);

  if (eternal && scheme_starting_up)
    prim = (Scheme_Primitive_Proc *)scheme_malloc_eternal_tagged(size);
  else
    prim = (Scheme_Primitive_Proc *)scheme_malloc_stubborn_tagged(size);
  prim->type = scheme_prim_type;
  SCHEME_PRIM(prim) = fun;
  prim->name = name;
  prim->mina = mina;
  prim->maxa = maxa;
  prim->flags = ((folding ? SCHEME_PRIM_IS_FOLDING : 0)
		 | (scheme_defining_primitives ? SCHEME_PRIM_IS_PRIMITIVE : 0)
		 | (hasr ? SCHEME_PRIM_IS_MULTI_RESULT : 0));

  if (!eternal || !scheme_starting_up)
    scheme_end_stubborn_change(prim);

  if (hasr) {
    ((Scheme_Prim_W_Result_Arity *)prim)->minr = minr;
    ((Scheme_Prim_W_Result_Arity *)prim)->maxr = maxr;
  }

  return (Scheme_Object *)prim;
}

Scheme_Object *scheme_make_prim(Scheme_Prim *fun)
{
  return scheme_make_prim_w_everything(fun, 1, NULL, 0, -1, 0, 1, 1);
}

Scheme_Object *
scheme_make_noneternal_prim (Scheme_Prim *fun)
{
  return scheme_make_prim_w_everything(fun, 0, NULL, 0, -1, 0, 1, 1);
}

Scheme_Object *
scheme_make_prim_w_arity(Scheme_Prim *fun, const char *name, 
			 short mina, short maxa)
{
  return scheme_make_prim_w_everything(fun, 1, name, mina, maxa, 0, 1, 1);
}

Scheme_Object *
scheme_make_folding_prim(Scheme_Prim *fun, const char *name, 
			 short mina, short maxa,
			 short folding)
{
  return scheme_make_prim_w_everything(fun, 1, name, mina, maxa, 
				       folding, 1, 1);
}

Scheme_Object *
scheme_make_noneternal_prim_w_arity(Scheme_Prim *fun, const char *name,
				    short mina, short maxa)
{
  return scheme_make_prim_w_everything(fun, 0, name, mina, maxa, 0, 1, 1);
}

Scheme_Object *
scheme_make_closed_prim_w_everything(Scheme_Closed_Prim *fun, 
				     void *data,
				     const char *name, 
				     short mina, short maxa,
				     short folding,
				     short minr, short maxr)
{
  Scheme_Closed_Primitive_Proc *prim;
  int hasr, size;

  hasr = ((minr != 1) || (maxr != 1));
  size = hasr ? sizeof(Scheme_Closed_Prim_W_Result_Arity) : sizeof(Scheme_Closed_Primitive_Proc);

  prim = (Scheme_Closed_Primitive_Proc *)scheme_malloc_tagged(size);

  prim->type = scheme_closed_prim_type;
  SCHEME_CLSD_PRIM(prim) = fun;
  SCHEME_CLSD_PRIM_DATA(prim) = data;
  prim->name = name;
  prim->mina = mina;
  prim->maxa = maxa;
  prim->flags = ((folding ? SCHEME_PRIM_IS_FOLDING : 0)
		 | (scheme_defining_primitives ? SCHEME_PRIM_IS_PRIMITIVE : 0)
		 | (hasr ? SCHEME_PRIM_IS_MULTI_RESULT : 0));

  if (hasr) {
    ((Scheme_Closed_Prim_W_Result_Arity *)prim)->minr = minr;
    ((Scheme_Closed_Prim_W_Result_Arity *)prim)->maxr = maxr;
  }

  return (Scheme_Object *)prim;
}

Scheme_Object *
scheme_make_folding_closed_prim(Scheme_Closed_Prim *fun, 
				void *data,
				const char *name, 
				short mina, short maxa,
				short folding)
{
  return scheme_make_closed_prim_w_everything(fun, data, name, mina, maxa, folding, 1, 1);
}

Scheme_Object *
scheme_make_closed_prim_w_arity(Scheme_Closed_Prim *fun, void *data,
				const char *name, short mina, short maxa)
{
  return scheme_make_closed_prim_w_everything(fun, data, name, mina, maxa, 0, 1, 1);
}

Scheme_Object *
scheme_make_closed_prim(Scheme_Closed_Prim *fun, void *data)
{
  return scheme_make_closed_prim_w_everything(fun, data, NULL, 0, -1, 0, 1, 1);
}

Scheme_Object *
scheme_make_closure (Scheme_Env *env, Scheme_Object *code)
{
  Scheme_Object *closure;

  closure = scheme_alloc_stubborn_object();

  closure->type = scheme_closure_type;
  SCHEME_CLOS_ENV(closure) = env;
  SCHEME_CLOS_CODE(closure) = code;

  scheme_end_stubborn_change((void *)closure);

  return closure;
}

Scheme_Object *
scheme_make_linked_closure(Scheme_Process *p, 
			   Scheme_Object *linked_code, int close)
{
  Scheme_Closure_Compilation_Data *data;
  Scheme_Closed_Compiled_Procedure *closure;
  Scheme_Object **runstack, **dest;
  short *map;
  int i;

  data = (Scheme_Closure_Compilation_Data *)linked_code;

  i = data->closure_size;

  closure = (Scheme_Closed_Compiled_Procedure *)
    scheme_malloc_tagged(sizeof(Scheme_Closed_Compiled_Procedure)
			 + (i - 1) * sizeof(Scheme_Object *));

  closure->type = scheme_linked_closure_type;
  SCHEME_COMPILED_CLOS_CODE(closure) = linked_code;

  if (!close || !i)
    return (Scheme_Object *)closure;

  runstack = p->runstack;
  dest = closure->vals;
  map = data->closure_map;

  while (i--) 
    dest[i] = runstack[map[i]];

  return (Scheme_Object *)closure;
}

typedef struct {
  int *local_flags;
  short *real_closure_map;
} Closure_Info;

Scheme_Object *
scheme_link_closure_compilation(Scheme_Object *_data, Link_Info *info)
{
  Scheme_Closure_Compilation_Data *data;
  int i;
  short *oldpos;
  Closure_Info *cl;
  Link_Info *new_info;

  data = (Scheme_Closure_Compilation_Data *)_data;
  cl = (Closure_Info *)data->closure_map;
  data->closure_map = cl->real_closure_map;
  data->type = scheme_unclosed_procedure_type;

  /* Set local_flags: */
  for (i = 0; i < data->num_params; i++) {
    if (cl->local_flags[i] & SCHEME_WAS_SET_BANGED)
      cl->local_flags[i] = SCHEME_INFO_BOXED;
    else
      cl->local_flags[i] = 0;
  }

  oldpos = (short *)scheme_malloc_atomic(sizeof(short) * data->closure_size);
  for (i = data->closure_size; i--; ) {
    oldpos[i] = data->closure_map[i];
    data->closure_map[i] = scheme_link_info_lookup(info, oldpos[i], NULL);
  }
  
  new_info = scheme_link_info_extend(info, data->num_params, data->num_params,
				     data->closure_size + data->num_params);
  for (i = 0; i < data->num_params; i++)
    scheme_link_info_add_mapping(new_info, i, i + data->closure_size, 
				 cl->local_flags[i]);
  for (i = 0; i < data->closure_size; i++) {
    int p = oldpos[i];

    if (p < 0)
      p -= data->num_params;
    else
      p += data->num_params;

    scheme_link_info_add_mapping(new_info, p, i,
				 scheme_link_info_flags(info, oldpos[i]));
  }

  data->code = scheme_link_expr(data->code, new_info);

  /* Add code to box set!ed variables: */
  for (i = 0; i < data->num_params; i++) {
    if (cl->local_flags[i] & SCHEME_INFO_BOXED) {
      int j = i + data->closure_size;
      data->code = scheme_make_syntax_link(scheme_bangboxenv_execute, 
					   scheme_make_pair(scheme_make_integer(j),
							    data->code));
    }
  }

  if ((SCHEME_TYPE(data->code) > _scheme_compiled_values_types_)
      || (SCHEME_TYPE(data->code) == scheme_quote_compilation_type))
    data->flags |= CLOS_FOLDABLE;

  if (!data->closure_size && info->can_optimize_constants)
    /* If only global frame is needed, go ahead and finialize closure */
    return scheme_make_linked_closure(NULL, (Scheme_Object *)data, 0);
  else
    return (Scheme_Object *)data;
}

Scheme_Object *
scheme_make_closure_compilation(Scheme_Comp_Env *env, Scheme_Object *code, 
				Scheme_Compile_Info *rec)
{
  Scheme_Object *allparams, *params, *forms, *param, *name, *rest;
  Scheme_Closure_Compilation_Data *data;
  Scheme_Compile_Info lam;
  Scheme_Comp_Env *frame;
  Closure_Info *cl;
  int i;

  data  = MALLOC_ONE_TAGGED(Scheme_Closure_Compilation_Data);

  data->type = scheme_compiled_unclosed_procedure_type;

  params = allparams = SCHEME_CAR(SCHEME_CDR(code));

  data->num_params = 0;
  for (; SCHEME_PAIRP(params); params = SCHEME_CDR(params))
    data->num_params++;
  data->flags = 0;
  if (!SCHEME_NULLP(params)) {
    data->flags |= CLOS_HAS_REST;
    data->num_params++;
  }

  forms = SCHEME_CDR(SCHEME_CDR(code));

  /* Check for duplicate names: */
  for (params = allparams;
       SCHEME_PAIRP(params); 
       params = SCHEME_CDR(params)) {
    name = SCHEME_CAR(params);
    for (rest = SCHEME_CDR(params); 
	 !SCHEME_NULLP(rest); 
	 rest = SCHEME_CDR(rest)) {
      if (!SCHEME_PAIRP(rest))
	param = rest;
      else
	param = SCHEME_CAR(rest);
      if (SCHEME_SYMBOLP(name) && SAME_OBJ(param, name)) {
	scheme_wrong_syntax("lambda", name, code,
			    "duplicate argument name");
      }
      if (!SCHEME_PAIRP(rest))
	break;
    }
  }

  frame = scheme_new_compilation_frame(data->num_params, SCHEME_LAMBDA_FRAME, env);
  params = allparams;
  for (i = 0; i < data->num_params; i++) {
    if (!SCHEME_PAIRP(params))
      param = params;
    else
      param = SCHEME_CAR(params);
    scheme_check_identifier("lambda", param, NULL, env, code);
    scheme_add_compilation_binding(i, param, frame);
    if (SCHEME_PAIRP(params))
      params = SCHEME_CDR (params);
  }

  if (SCHEME_NULLP(forms))
    scheme_wrong_syntax("lambda", NULL, code, "bad syntax (empty body)");

  data->name = rec->value_name;

  scheme_compile_rec_done_local(rec);

  scheme_init_lambda_rec(rec, &lam);

  data->code = scheme_compile_sequence(forms, 
				       scheme_no_defines(frame), 
				       &lam);

  scheme_merge_lambda_rec(rec, &lam);

  cl = MALLOC_ONE(Closure_Info);

  cl->local_flags = scheme_env_get_flags(frame, 0, data->num_params);

  /* Remembers positions of used vars (and unsets usage for this level) */
  scheme_env_make_closure_map(frame, &data->closure_size, &cl->real_closure_map);

  data->closure_map = (short *)cl;

  data->max_let_depth = lam.max_let_depth + data->num_params + data->closure_size;
  
  return (Scheme_Object *)data;
}

void *scheme_top_level_do(void *(*k)(void))
{
  void *v;
  long *old_cc_ok, *ok;
  void *old_cc_start;
  jmp_buf save, oversave;
#ifdef ERROR_ON_OVERFLOW
  int orig_overflow;
#endif
  Scheme_Stack_State envss;
  Scheme_Comp_Env *save_current_local_env;
  Scheme_Process *p = scheme_current_process;

#ifndef MZ_REAL_THREADS
  if (scheme_active_but_sleeping)
    scheme_wake_up();
#endif

  old_cc_ok = p->cc_ok;
  old_cc_start = p->cc_start;

  p->cc_ok = ok = (long *)scheme_malloc_atomic(sizeof(long));
  p->cc_start = &v;
  
  if (old_cc_ok)
    *old_cc_ok = 0;
  *ok = 1;

#ifdef ERROR_ON_OVERFLOW
  orig_overflow = p->stack_overflow;
#endif
  scheme_save_env_stack_w_process(envss, p);

  save_current_local_env = p->current_local_env;

#ifndef ERROR_ON_OVERFLOW
  memcpy(&oversave, &p->overflow_buf, sizeof(jmp_buf));
  if (scheme_setjmp(p->overflow_buf)) {
    while (1) {
      Scheme_Overflow *overflow;

      overflow = MALLOC_ONE(Scheme_Overflow);
      
      memcpy(&overflow->cont, &scheme_overflow_cont, 
	     sizeof(Scheme_Jumpup_Buf));
      overflow->prev = p->overflow;
      p->overflow = overflow;
      
      memcpy(&overflow->savebuf, &scheme_error_buf, sizeof(jmp_buf));
      if (scheme_setjmp(scheme_error_buf)) {
	scheme_overflow_reply = NULL; /* means "continue the error" */
      } else {
	void *p1, *p2, *p3, *p4;
	int i1, i2;
	
	p1 = p->ku.k.p1;
	p2 = p->ku.k.p2;
	p3 = p->ku.k.p3;
	p4 = p->ku.k.p4;
	i1 = p->ku.k.i1;
	i2 = p->ku.k.i2;
	
	/* stack overflow is a lot of work; force a sleep */
	scheme_process_block(0);
	
	p->ku.k.p1 = p1;
	p->ku.k.p2 = p2;
	p->ku.k.p3 = p3;
	p->ku.k.p4 = p4;
	p->ku.k.i1 = i1;
	p->ku.k.i2 = i2;
	
	scheme_overflow_reply = scheme_overflow_k();
      }
      
      overflow = p->overflow;
      memcpy(&scheme_error_buf, &overflow->savebuf, sizeof(jmp_buf));
      p->overflow = overflow->prev;
      memcpy(&scheme_overflow_cont, &overflow->cont, 
	     sizeof(Scheme_Jumpup_Buf));
      overflow = NULL; /* Maybe helps GC */
      /* Reset overflow buffer and continue */
      if (scheme_setjmp(p->overflow_buf)) {
	/* handle again */
      } else
	scheme_longjmpup(&scheme_overflow_cont);
    }
  }
#endif

  memcpy(&save, &p->error_buf, sizeof(jmp_buf));

  if (scheme_setjmp(p->error_buf)) {
    scheme_restore_env_stack_w_process(envss, p);
    *ok = 0;
    if (old_cc_ok)
      *old_cc_ok = 1;
    p->cc_ok = old_cc_ok;
    p->cc_start = old_cc_start;  
#ifdef ERROR_ON_OVERFLOW
    p->stack_overflow = orig_overflow;
#else
    memcpy(&p->overflow_buf, &oversave, sizeof(jmp_buf));
#endif
    scheme_longjmp(save, 1);
  }

  v = k();

  p->current_local_env = save_current_local_env;

  memcpy(&p->error_buf, &save, sizeof(jmp_buf));

#ifdef ERROR_ON_OVERFLOW
  p->stack_overflow = orig_overflow;
#else
  memcpy(&p->overflow_buf, &oversave, sizeof(jmp_buf));
#endif

  *ok = 0;
  if (old_cc_ok)
    *old_cc_ok = 1;
  p->cc_ok = old_cc_ok;
  p->cc_start = old_cc_start;

#ifndef MZ_REAL_THREADS
  if (scheme_active_but_sleeping)
    scheme_wake_up();
#endif

  return v;
}

static void *apply_k()
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *rator;
  int num_rands;
  Scheme_Object **rands;

  rator = (Scheme_Object *)p->ku.k.p1;
  rands = (Scheme_Object **)p->ku.k.p2;
  num_rands = p->ku.k.i1;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  if (p->ku.k.i2)
    return (void *)_scheme_apply_wp(rator, num_rands, rands, p);
  else
    return (void *)_scheme_apply_multi_wp(rator, num_rands, rands, p);
}

static Scheme_Object *
_apply(Scheme_Object *rator, int num_rands, Scheme_Object **rands, int multi
#ifdef MZ_REAL_THREADS
	     , Scheme_Process *p
#endif
	     )
{
#ifndef MZ_REAL_THREADS
  Scheme_Process *p = scheme_current_process;
#endif

  p->ku.k.p1 = rator;
  p->ku.k.p2 = rands;
  p->ku.k.i1 = num_rands;
  p->ku.k.i2 = multi;

  return (Scheme_Object *)scheme_top_level_do(apply_k);
}


#ifndef MZ_REAL_THREADS
Scheme_Object *
scheme_apply(Scheme_Object *rator, int num_rands, Scheme_Object **rands)
#else
Scheme_Object *
scheme_apply_wp(Scheme_Object *rator, int num_rands, Scheme_Object **rands,
		Scheme_Process *p)
#endif
{
  return _apply(rator, num_rands, rands, 0
#ifdef MZ_REAL_THREADS
		, p
#endif
		);
}

#ifndef MZ_REAL_THREADS
Scheme_Object *
scheme_apply_multi(Scheme_Object *rator, int num_rands, Scheme_Object **rands)
#else
Scheme_Object *
scheme_apply_multi_wp(Scheme_Object *rator, int num_rands, Scheme_Object **rands,
		      Scheme_Process *p)
#endif
{
  return _apply(rator, num_rands, rands, 1
#ifdef MZ_REAL_THREADS
		, p
#endif
		);
}

 
Scheme_Object *
scheme_tail_apply (Scheme_Object *rator, int num_rands, Scheme_Object **rands)
{
  int i;
  Scheme_Process *p = scheme_current_process;

  p->ku.apply.tail_rator = rator;
  p->ku.apply.tail_num_rands = num_rands;
  
  if (num_rands) {
    Scheme_Object **a;
    if (num_rands > p->tail_buffer_size) {
      p->tail_buffer_size = num_rands;
      p->tail_buffer = 
	(Scheme_Object **)scheme_malloc(num_rands * sizeof(Scheme_Object *));
    }
    a = p->tail_buffer;
    p->ku.apply.tail_rands = a;
    for (i = num_rands; i--; )
      a[i] = rands[i];

#ifdef AGRESSIVE_ZERO_TB
    p->tail_buffer_set = num_rands;
#endif
  } else
    p->ku.apply.tail_rands = NULL;

  return SCHEME_TAIL_CALL_WAITING;
}

Scheme_Object *
scheme_tail_apply_no_copy (Scheme_Object *rator, int num_rands, 
			   Scheme_Object **rands)
{
  Scheme_Process *p = scheme_current_process;

  p->ku.apply.tail_rator = rator;
  p->ku.apply.tail_num_rands = num_rands;
  p->ku.apply.tail_rands = rands;

  return SCHEME_TAIL_CALL_WAITING;
}

static
Scheme_Object *
X_scheme_apply_to_list(Scheme_Object *rator, Scheme_Object *rands, int force,
		       int top_level, Scheme_Object *macro_apply)
{
  int num_rands, i;
  Scheme_Object **rands_vec;

  num_rands = scheme_list_length(rands);
  rands_vec = (Scheme_Object **)scheme_malloc(num_rands 
					      * sizeof(Scheme_Object *));

  if (macro_apply) {
    /* Check arity now so we can give the right error message: */
    if (SCHEME_FALSEP(get_or_check_arity(rator, num_rands)))
      scheme_wrong_syntax("macro application", NULL, macro_apply, 
			  "bad syntax (wrong number of parts)");
  }

  for (i = 0; i < num_rands ; i++) {
    if (!SCHEME_PAIRP(rands)) {
      if (macro_apply)
	scheme_wrong_syntax("macro application", NULL, macro_apply, 
			    "bad syntax (" IMPROPER_LIST_FORM ")");
      else
	scheme_signal_error("bad application form");
    }
    rands_vec[i] = SCHEME_CAR (rands);
    rands = SCHEME_CDR (rands);
  }
  
  if (top_level)  {
    if (force)
      return scheme_apply(rator, num_rands, rands_vec);
    else
      return scheme_tail_apply(rator, num_rands, rands_vec);
  } else {
    if (force)
      return _scheme_apply(rator, num_rands, rands_vec);
    else
      return _scheme_tail_apply(rator, num_rands, rands_vec);
  }
}

Scheme_Object *
scheme_apply_to_list (Scheme_Object *rator, Scheme_Object *rands)
{
  return X_scheme_apply_to_list(rator, rands, 1, 1, NULL);
}

Scheme_Object *
scheme_tail_apply_to_list (Scheme_Object *rator, Scheme_Object *rands)
{
  return X_scheme_apply_to_list(rator, rands, 0, 1, NULL);
}

Scheme_Object *
_scheme_apply_to_list (Scheme_Object *rator, Scheme_Object *rands)
{
  return X_scheme_apply_to_list(rator, rands, 1, 0, NULL);
}

Scheme_Object *
_scheme_tail_apply_to_list (Scheme_Object *rator, Scheme_Object *rands)
{
  return X_scheme_apply_to_list(rator, rands, 0, 0, NULL);
}

Scheme_Object *
scheme_apply_macro_to_list(Scheme_Object *rator, Scheme_Object *rands,
			   Scheme_Object *code)
{
  return X_scheme_apply_to_list(rator, rands, 1, 1, code);
}

/* locals */

static Scheme_Object *
procedure_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_PROCP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *primitive_p(int argc, Scheme_Object *argv[])
{
  int isprim;

  if (SCHEME_PRIMP(argv[0]))
    isprim = (((Scheme_Primitive_Proc *)argv[0])->flags & SCHEME_PRIM_IS_PRIMITIVE);
  else if (SCHEME_CLSD_PRIMP(argv[0]))
    isprim = (((Scheme_Closed_Primitive_Proc *)argv[0])->flags & SCHEME_PRIM_IS_PRIMITIVE);
  else
    isprim = 0;

  return isprim ? scheme_true : scheme_false;
}

static Scheme_Object *primitive_closure_p(int argc, Scheme_Object *argv[])
{
  int isprim;

  if (SCHEME_CLSD_PRIMP(argv[0]))
    isprim = (((Scheme_Closed_Primitive_Proc *)argv[0])->flags & SCHEME_PRIM_IS_PRIMITIVE);
  else
    isprim = 0;

  return isprim ? scheme_true : scheme_false;
}


static Scheme_Object *primitive_name(int argc, Scheme_Object *argv[])
{
  const char *s;
  Scheme_Object *o;

  o = argv[0];

  if (SCHEME_PRIMP(o)
      && (((Scheme_Primitive_Proc *)o)->flags & SCHEME_PRIM_IS_PRIMITIVE))
    s = ((Scheme_Primitive_Proc *)o)->name;
  else if (SCHEME_CLSD_PRIMP(o)
	   && (((Scheme_Closed_Primitive_Proc *)o)->flags & SCHEME_PRIM_IS_PRIMITIVE))
    s = ((Scheme_Closed_Primitive_Proc *)o)->name;
  else {
    scheme_wrong_type("primitive-name", "primitive", 0, argc, argv);
    s = NULL;
  }

  if (!s) s = "UNKNOWN";

  return scheme_make_string(s);
}

const char *scheme_get_proc_name(Scheme_Object *p, int *len, int for_error)
{
  Scheme_Type type;
  int dummy;
  char *s;

  if (!len)
    len = &dummy;

  type = SCHEME_TYPE(p);
  if (type == scheme_prim_type) {
    *len = strlen(((Scheme_Primitive_Proc *)p)->name);
    return ((Scheme_Primitive_Proc *)p)->name;
  } else if (type == scheme_closed_prim_type) {
    *len = strlen(((Scheme_Closed_Primitive_Proc *)p)->name);
    return ((Scheme_Closed_Primitive_Proc *)p)->name;
  } else if (type == scheme_cont_type || type == scheme_escaping_cont_type) {
    return NULL;
  } else if (type == scheme_case_closure_type) {
    Scheme_Case_Lambda *seq;

    seq = (Scheme_Case_Lambda *)p;
    if (seq->name) {
      *len = SCHEME_SYM_LEN(seq->name);
      s = SCHEME_SYM_VAL(seq->name);
    } else
      return NULL;
  } else {
    Scheme_Closure_Compilation_Data *data;

    data = (Scheme_Closure_Compilation_Data *)SCHEME_COMPILED_CLOS_CODE(p);
    if (data->name) {
      *len = SCHEME_SYM_LEN(data->name);
      s = SCHEME_SYM_VAL(data->name);
    } else
      return NULL;
  }

  if (for_error) {
    char *r;
    
    r = (char *)scheme_malloc_atomic(*len + 11);
    memcpy(r, "procedure ", 10);
    memcpy(r + 10, s, *len + 1);
    *len += 11;

    return r;
  }

  return s;
}

static Scheme_Object *primitive_result_arity(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o;

  o = argv[0];

  if (SCHEME_PRIMP(o)
      && (((Scheme_Primitive_Proc *)o)->flags & SCHEME_PRIM_IS_PRIMITIVE)) {
    if (((Scheme_Primitive_Proc *)o)->flags & SCHEME_PRIM_IS_MULTI_RESULT) {
      Scheme_Prim_W_Result_Arity *p = (Scheme_Prim_W_Result_Arity *)o;
      return scheme_make_arity(p->minr, p->maxr);
    }
  } else if (SCHEME_CLSD_PRIMP(o)
	     && (((Scheme_Closed_Primitive_Proc *)o)->flags & SCHEME_PRIM_IS_PRIMITIVE)) {
    if (((Scheme_Closed_Primitive_Proc *)o)->flags & SCHEME_PRIM_IS_MULTI_RESULT) {
      Scheme_Closed_Prim_W_Result_Arity *p = (Scheme_Closed_Prim_W_Result_Arity *)o;
      return scheme_make_arity(p->minr, p->maxr);
    }
  } else {
    scheme_wrong_type("primitive-result_arity", "primitive", 0, argc, argv);
    return NULL;
  }

  return scheme_make_integer(1);
}

static Scheme_Object *syntax_p(int argc, Scheme_Object *argv[])
{
  return (SCHEME_SYNTAXP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *macro_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_macro_type) 
	  ? scheme_true : scheme_false);
}

static Scheme_Object *id_macro_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_id_macro_type) 
	  ? scheme_true : scheme_false);
}

static Scheme_Object *exp_time_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_exp_time_type) 
	  ? scheme_true : scheme_false);
}

static Scheme_Object *inferred_name(int argc, Scheme_Object **argv)
{
  const char *s;
  int len;
  Scheme_Type type;

  if (SCHEME_PROCP(argv[0])) {
    s = scheme_get_proc_name(argv[0], &len, 0);
  } else {
    type = SCHEME_TYPE(argv[0]);
    
    switch(type) {
    case scheme_class_type:
      s = scheme_get_class_name(argv[0], &len);
      break;
    case scheme_interface_type:
      s = scheme_get_interface_name(argv[0], &len);
      break;
    case scheme_unit_type:
      s = scheme_get_unit_name(argv[0], &len);
      break;
    default:
      return scheme_false;
    }
  }

  if (!s)
    return scheme_false;
  else
    return scheme_intern_exact_symbol(s, len);
}

Scheme_Object *scheme_make_arity(short mina, short maxa)
{
  if (mina == maxa)
    return scheme_make_integer(mina);
  else if (maxa == -1) {
    Scheme_Object *p[1];
    p[0] = scheme_make_integer(mina);
    return scheme_make_struct_instance(scheme_arity_at_least, 1, p);
  } else {
    int i;
    Scheme_Object *l = scheme_null;

    for (i = maxa; i >= mina; --i)
      l = scheme_make_pair(scheme_make_integer(i), l);

    return l;
  }
}

static Scheme_Object *get_or_check_arity(Scheme_Object *p, long a)
/* a == -1 => get arity
   a == -2 => check for allowing varargs */
{
  Scheme_Type type;
  short mina, maxa;

  type = SCHEME_TYPE(p);
  if (type == scheme_prim_type) {
    mina = ((Scheme_Primitive_Proc *)p)->mina;
    maxa = ((Scheme_Primitive_Proc *)p)->maxa;
  } else if (type == scheme_closed_prim_type) {
    mina = ((Scheme_Closed_Primitive_Proc *)p)->mina;
    maxa = ((Scheme_Closed_Primitive_Proc *)p)->maxa;

    if (mina == -2) {
      short *cases = ((Scheme_Closed_Case_Primitive_Proc *)p)->cases;
      int count = -maxa, i;

      if (a == -1) {
	Scheme_Object *arity = scheme_alloc_list(count), *a;
	
	for (i = 0, a = arity; i < count; i++, a = SCHEME_CDR(a))
	  SCHEME_CAR(a) = scheme_make_arity(cases[2 * i], cases[(2 * i) + 1]);

	return arity;
      }

      if (a == -2) {
	for (i = 0; i < count; i++)
	  if (cases[(2 * i) + 1] < 0)
	    return scheme_true;

	return scheme_false;
      }

      for (i = 0; i < count; i++) {
	int na, xa;
	na = cases[2 * i];
	xa = cases[(2 * i) + 1];
	if ((a >= na) && ((xa < 0) || (a <= xa)))
	  return scheme_true;
      }

      return scheme_false;
    }    
  } else if (type == scheme_cont_type || type == scheme_escaping_cont_type) {
    mina = 0;
    maxa = -1;
  } else if (type == scheme_case_closure_type) {
    Scheme_Case_Lambda *seq;
    Scheme_Closure_Compilation_Data *data;
    int i;
    Scheme_Object *first, *last = NULL, *v;

    if (a == -1)
      first = scheme_null;
    else
      first = scheme_false;

    seq = (Scheme_Case_Lambda *)p;
    for (i = 0; i < seq->count; i++) {
      data = (Scheme_Closure_Compilation_Data *)SCHEME_COMPILED_CLOS_CODE(seq->array[i]);
      mina = maxa = data->num_params;
      if (data->flags & CLOS_HAS_REST) {
	--mina;
	maxa = -1;
      } 

      if (a >= 0) {
	if (a >= mina && (maxa < 0 || a <= maxa))
	  return scheme_true;
      } else if (a == -2) {
	if (maxa < 0)
	  return scheme_true;
      } else {
	v = scheme_make_pair(scheme_make_arity(mina, maxa), scheme_null);
	if (!last)
	  first = v;
	else
	  SCHEME_CDR(last) = v;
	last = v;
      }
    }

    return first;
  } else {
    Scheme_Closure_Compilation_Data *data;

    data = (Scheme_Closure_Compilation_Data *)SCHEME_COMPILED_CLOS_CODE(p);
    mina = maxa = data->num_params;
    if (data->flags & CLOS_HAS_REST) {
      --mina;
      maxa = -1;
    }
  }

  if (a == -1)
    return scheme_make_arity(mina, maxa);

  if (a == -2)
    return (maxa < 0) ? scheme_true : scheme_false;

  if (a < mina || (maxa >= 0 && a > maxa))
    return scheme_false;

  return scheme_true;
}

int scheme_check_proc_arity(const char *where, int a,
			     int which, int argc, Scheme_Object **argv)
{
  Scheme_Object *p;

  if (which < 0)
    p = argv[0];
  else
    p = argv[which];

  if (!SCHEME_PROCP(p) || SCHEME_FALSEP(get_or_check_arity(p, a))) {
    if (where) {
      char buffer[50];
      
      sprintf(buffer, "procedure (arity %d)", a);
      
      scheme_wrong_type(where, buffer, which, argc, argv);
    } else
      return 0;
  }

  return 1;
}

Scheme_Object *scheme_arity(Scheme_Object *p)
{
  return get_or_check_arity(p, -1);
}

static Scheme_Object *arity(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_type("arity", "procedure", 0, argc, argv);
  
  return get_or_check_arity(argv[0], -1);
}

static Scheme_Object *procedure_arity_includes(int argc, Scheme_Object *argv[])
{
  long n;

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_type("procedure-arity-includes?", "procedure", 0, argc, argv);

  n = scheme_extract_index("procedure-arity-includes?", 1, argc, argv, -2);
  
  return get_or_check_arity(argv[0], n);
}

static Scheme_Object *
apply(int argc, Scheme_Object *argv[])
{
  Scheme_Object *rands, *r;
  Scheme_Object **rand_vec;
  int i, num_rands;
  Scheme_Process *p = scheme_current_process;

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_type("apply", "procedure", 0, argc, argv);

  rands = argv[argc-1];

  num_rands = (argc - 2);
  r = rands;
  while (!SCHEME_NULLP(r)) {
    if (!SCHEME_PAIRP(r))
      scheme_wrong_type("apply", "proper list", argc - 1, argc, argv);
    r = SCHEME_CDR(r);
    num_rands++;
  }
  rand_vec = (Scheme_Object **)scheme_malloc(num_rands *
					     sizeof(Scheme_Object *));

  for (i = argc - 2; i--; )
    rand_vec[i] = argv[i + 1];
  
  for (i = argc - 2; SCHEME_PAIRP(rands); i++, rands = SCHEME_CDR(rands))
    rand_vec[i] = SCHEME_CAR(rands);

  p->ku.apply.tail_rator = argv[0];
  p->ku.apply.tail_rands = rand_vec;
  p->ku.apply.tail_num_rands = num_rands;

  return SCHEME_TAIL_CALL_WAITING;
}

static Scheme_Object *
do_map(int argc, Scheme_Object *argv[], char *name, int make_result,
       int and_mode, int or_mode)
{
  int i, size = 0, l;
  int can_multi;
  Scheme_Object *quick1[5], *quick2[5], **working;
  Scheme_Object *pair, *v;
  Scheme_Object *retfirst, *retlast = NULL;
  Scheme_Object **args;

  can_multi = (!make_result && !and_mode && !or_mode);

  if (!SCHEME_PROCP(argv[0]))
    scheme_wrong_type(name, "procedure", 0, argc, argv);

  for (i = 1; i < argc; i++) {
    if (!SCHEME_LISTP (argv[i]))
      scheme_wrong_type(name, "list", i, argc, argv);

    l = scheme_proper_list_length(argv[i]);

    if (l < 0)
      scheme_wrong_type(name, "proper list", i, argc, argv);
    
    if (i == 1)
      size = l;
    else if (size != l) {
      char *argstr = scheme_make_args_string("", -1, argc, argv);

      scheme_raise_exn(MZEXN_APPLICATION_LIST_SIZES, argv[i],
		       "%s: all lists must have same size%s", 
		       name, argstr);
      return NULL;
    }
  }

  if (SCHEME_FALSEP(get_or_check_arity(argv[0], argc - 1))) {
    char *s = scheme_make_arity_expect_string(argv[0], argc - 1, NULL);

    scheme_raise_exn(MZEXN_APPLICATION_MAP_ARITY, argv[0],
		     scheme_make_integer(argc - 1),
		     "%s: arity mismatch for %s", name, s);
    return NULL;
  }
  
  if (argc < 6) {
    args = quick1;
    working = quick2;
  } else {
    args = (Scheme_Object **)scheme_malloc((argc - 1) * sizeof(Scheme_Object *));
    working = (Scheme_Object **)scheme_malloc((argc - 1) * sizeof(Scheme_Object *));
  }

  /* Copy argc into working array */
  for (i = 1; i < argc ; i++)
    working[i-1] = argv[i];

  --argc;

  if (make_result)
    retfirst = retlast = scheme_null;
  else if (and_mode)
    retfirst = scheme_true;
  else if (or_mode)
    retfirst = scheme_false;
  else
    retfirst = scheme_void;

  while (!SCHEME_NULLP(working[0])) {
    /* collect args to apply */
    for (i = 0; i < argc ; i++) {
      args[i] = SCHEME_CAR(working[i]);
      working[i] = SCHEME_CDR(working[i]);
    }

    if (can_multi)
      v = _scheme_apply_multi(argv[0], argc, args);
    else
      v = _scheme_apply(argv[0], argc, args);
    
    if (make_result) {
      pair = scheme_make_pair(v, scheme_null);
      if (SCHEME_NULLP (retfirst))
	retfirst = retlast = pair;
      else {
	SCHEME_CDR (retlast) = pair;
	retlast = pair;
      }
    } else if (and_mode) {
      if (SCHEME_FALSEP(v))
	return scheme_false;
      retfirst = v;
    } else if (or_mode) {
      if (SCHEME_TRUEP(v))
	return v;
    }
  }

  return retfirst;
}

static Scheme_Object *
map (int argc, Scheme_Object *argv[])
{
  return do_map(argc, argv, "map", 1, 0, 0);
}

static Scheme_Object *
for_each (int argc, Scheme_Object *argv[])
{
  return do_map(argc, argv, "for-each", 0, 0, 0);
}

static Scheme_Object *
andmap(int argc, Scheme_Object *argv[])
{
  return do_map(argc, argv, "andmap", 0, 1, 0);
}

static Scheme_Object *
ormap(int argc, Scheme_Object *argv[])
{
  return do_map(argc, argv, "ormap", 0, 0, 1);
}

static Scheme_Object *call_with_values(int argc, Scheme_Object *argv[])
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *v;

  scheme_check_proc_arity("call-with-values", 0, 0, argc, argv);
  if (!SCHEME_PROCP(argv[1]))
    scheme_wrong_type("call-with-values", "procedure", 1, argc, argv);

  v = _scheme_apply_multi(argv[0], 0, NULL);
  if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
    /* Careful - get the ordering right! */
    p->ku.apply.tail_num_rands = p->ku.multiple.count;
    p->ku.apply.tail_rands = p->ku.multiple.array;
  } else {
    p->ku.apply.tail_num_rands = 1;
    p->ku.apply.tail_rands = p->tail_buffer;
    p->ku.apply.tail_rands[0] = v;
#ifdef AGRESSIVE_ZERO_TB
    p->tail_buffer_set = 1;
#endif
  }
  
  p->ku.apply.tail_rator = argv[1];

  return SCHEME_TAIL_CALL_WAITING;
}

Scheme_Object *scheme_values(int argc, Scheme_Object *argv[])
{
  Scheme_Process *p = scheme_current_process;
  int i;
  Scheme_Object **a;

  if (argc == 1)
    return argv[0];
  p->ku.multiple.count = argc;
  if (argc)
    a = (Scheme_Object **)scheme_malloc(argc * sizeof(Scheme_Object *));
  else
    a = NULL;
  p->ku.multiple.array = a;
  
  for (i = 0; i < argc; i++)
    a[i] = argv[i];

  return SCHEME_MULTIPLE_VALUES;
}

static void pre_call_ec(void *ec)
{
  SCHEME_CONT_HOME(ec) = scheme_current_process;
}

static void post_call_ec(void *ec)
{
  SCHEME_CONT_HOME(ec) = NULL;
}

static Scheme_Object *do_call_ec(void *ec)
{
  Scheme_Object *p[1], *f;

  p[0] = (Scheme_Object *)ec;
  f = SCHEME_CONT_VAL(ec);
  SCHEME_CONT_VALS(ec) = NULL;

  return _scheme_apply_multi(f, 1, p);
}

static Scheme_Object *handle_call_ec(void *ec)
{
  if (SCHEME_CONT_VAL(ec)) {
    int n;
    scheme_jumping_to_continuation = 0;
#ifdef ERROR_ON_OVERFLOW
    scheme_current_process->stack_overflow = 
      ((Scheme_Escaping_Cont *)ec)->orig_overflow;
#endif
    n = SCHEME_CONT_NUM_VALS(ec);
    if (n == 1)
      return SCHEME_CONT_VAL(ec);
    else
      return scheme_values(n, SCHEME_CONT_VALS(ec));
  } else
    return NULL;
}

static Scheme_Object *
call_ec (int argc, Scheme_Object *argv[])
{
  Scheme_Escaping_Cont *cont;

  scheme_check_proc_arity("call-with-escaping-continuation", 1, 
			  0, argc, argv);

  cont = MALLOC_ONE_TAGGED(Scheme_Escaping_Cont);
  cont->type = scheme_escaping_cont_type;
  SCHEME_CONT_HOME(cont) = scheme_current_process;
  cont->u.val = argv[0];
#ifdef ERROR_ON_OVERFLOW
  cont->orig_overflow = scheme_current_process->stack_overflow;
#endif

  return scheme_dynamic_wind(pre_call_ec, do_call_ec, post_call_ec,
			     handle_call_ec, (void *)cont);
}

#define TOTAL_STACK_SIZE (sizeof(Scheme_Object*) * SCHEME_STACK_SIZE)

static Scheme_Object *
call_cc (int argc, Scheme_Object *argv[])
{
  Scheme_Object *ret;
  Scheme_Cont *cont;
  Scheme_Dynamic_Wind *dw;
  Scheme_Process *p = scheme_current_process;
  Scheme_Saved_Stack *saved, *isaved, *csaved;
  long size;
  
  scheme_check_proc_arity("call-with-current-continuation", 1, 
			  0, argc, argv);

  cont = MALLOC_ONE_TAGGED(Scheme_Cont);
  cont->type = scheme_cont_type;
  scheme_init_jmpup_buf(&cont->buf);
  cont->ok = p->cc_ok;
  cont->dw = p->dw;
  cont->home = p;
#ifdef ERROR_ON_OVERFLOW
  cont->orig_overflow = p->stack_overflow;
#else
  cont->save_overflow = p->overflow;
  memcpy(&cont->save_overflow_buf, &p->overflow_buf, sizeof(jmp_buf));
#endif
  cont->current_local_env = p->current_local_env;
  scheme_save_env_stack_w_process(cont->ss, p);

  /* Hide call/cc's arg off of stack */
  p->ku.k.p1 = argv[0];
  argv[0] = NULL;

  /* Copy out stack: */
  cont->runstack_copied = saved = MALLOC_ONE(Scheme_Saved_Stack);
  size = p->runstack_size - (p->runstack - p->runstack_start);
  saved->runstack_size = size;
  saved->runstack_start = MALLOC_N(Scheme_Object*, size);
  memcpy(saved->runstack_start, p->runstack, size * sizeof(Scheme_Object *));
  isaved = saved;
  for (csaved = p->runstack_saved; csaved; csaved = csaved->prev) {
    isaved->prev = MALLOC_ONE(Scheme_Saved_Stack);
    isaved = isaved->prev;
    size = csaved->runstack_size - (csaved->runstack - csaved->runstack_start);
    isaved->runstack_size = size;
    isaved->runstack_start = MALLOC_N(Scheme_Object*, size);
    memcpy(isaved->runstack_start, csaved->runstack, size * sizeof(Scheme_Object *));
  }
  isaved->prev = NULL;

  memcpy(&cont->savebuf, &p->error_buf, sizeof(jmp_buf));

  scheme_zero_unneeded_rands(p);

  if (scheme_setjmpup(&cont->buf, p->cc_start)) {
    Scheme_Object *result = cont->value;
    cont->value = NULL;

    p->current_local_env = cont->current_local_env;

    /* For dynamic-winds after the "common" intersection
       (see eval.c), execute the pre thunks. Make a list
       of these first because they have to be done in the
       inverse order of `prev' linkage. */
    if (cont->dw) {
      Scheme_Dynamic_Wind_List *dwl = NULL, *last = NULL;
      
      for (dw = cont->dw; dw != cont->common; dw = dw->prev) {
	Scheme_Dynamic_Wind_List *cell;

	cell = MALLOC_ONE(Scheme_Dynamic_Wind_List);
	cell->dw = dw;
	cell->next = NULL;

	if (last)
	  last->next = cell;
	else
	  dwl = cell;
	last = cell;
      }
      for (; dwl; dwl = dwl->next)
	if (dwl->dw->pre) {
	  p->dw = dwl->dw->prev;
	  dwl->dw->pre(dwl->dw->data);
	}
    }
    p->dw = cont->dw;

    memcpy(&p->error_buf, &cont->savebuf, sizeof(jmp_buf));

#ifdef ERROR_ON_OVERFLOW
    p->stack_overflow = cont->orig_overflow;
#else
    memcpy(&p->overflow_buf, &cont->save_overflow_buf, sizeof(jmp_buf));
    p->overflow = cont->save_overflow;
#endif
    scheme_restore_env_stack_w_process(cont->ss, p);

    /* Copy stack back in: (p->runstack and p->runstack_saved arrays
       are already restored, so the shape is certainly the same as 
       when cont->runstack_copied was made) */
    isaved = cont->runstack_copied;
    size = isaved->runstack_size;
    p->runstack = p->runstack_start + (p->runstack_size - size);
    memcpy(p->runstack, isaved->runstack_start, size * sizeof(Scheme_Object *));
    for (csaved = p->runstack_saved; csaved; csaved = csaved->prev) {
      isaved = isaved->prev;
      size = isaved->runstack_size;
      csaved->runstack = csaved->runstack_start + (csaved->runstack_size - size);
      memcpy(csaved->runstack, isaved->runstack_start, size * sizeof(Scheme_Object *));
    }

    return result;
  } else {
    Scheme_Object *argv2[1];

    /* Restore call/cc's arg to stack. */
    /* (We aren't actually allowed to modify argv! :) */
    argv[0] = p->ku.k.p1;
    p->ku.k.p1 = NULL;

    argv2[0] = (Scheme_Object *)cont;
    ret = _scheme_tail_apply(argv[0], 1, argv2);
    return ret;
  }
}

typedef struct {
  Scheme_Object *pre, *act, *post;
} Dyn_Wind;

static void pre_dyn_wind(void *d)
{
  Dyn_Wind *dw;
  dw = (Dyn_Wind *)d;

  (void)_scheme_apply_multi(dw->pre, 0, NULL);
}

static Scheme_Object *do_dyn_wind(void *d)
{
  Dyn_Wind *dw;
  dw = (Dyn_Wind *)d;

  return _scheme_apply_multi(dw->act, 0, NULL);
}

static void post_dyn_wind(void *d)
{
  Dyn_Wind *dw;
  dw = (Dyn_Wind *)d;

  (void)_scheme_apply_multi(dw->post, 0, NULL);
}

static Scheme_Object *dynamic_wind(int c, Scheme_Object *p[])
{
  Dyn_Wind *dw;

  dw = MALLOC_ONE(Dyn_Wind);

  dw->pre = p[0];
  dw->act = p[1];
  dw->post = p[2];

  return scheme_dynamic_wind(pre_dyn_wind, do_dyn_wind, post_dyn_wind, NULL,
			     (void *)dw);
}
 
Scheme_Object *scheme_dynamic_wind(void (*pre)(void *),
				   Scheme_Object *(*act)(void *),
				   void (*post)(void *), 
				   Scheme_Object *(*jmp_handler)(void *),
				   void *data)
{
  Scheme_Object *v, **save_values;
  int err;
  Scheme_Dynamic_Wind *dw;
  int save_count;
  Scheme_Process *p = scheme_current_process;

  dw = MALLOC_ONE(Scheme_Dynamic_Wind);

  dw->data = data;
  dw->pre = pre;
  dw->post = post;
  dw->prev = p->dw;

  if (pre)
    pre(data);

  p->dw = dw;

  memcpy(&dw->saveerr, &scheme_error_buf, sizeof(jmp_buf));

  scheme_save_env_stack_w_process(dw->envss, p);

  dw->current_local_env = p->current_local_env;

  if (scheme_setjmp(p->error_buf)) {
    scheme_restore_env_stack_w_process(dw->envss, p);
    p->current_local_env = dw->current_local_env;
    if (jmp_handler)
      v = jmp_handler(data);
    else
      v = NULL;
    err = !v;
  } else {
    v = act(data);

    err = 0;
  }

  if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
    save_count = p->ku.multiple.count;
    save_values = p->ku.multiple.array;
    p->ku.multiple.array = NULL;
  } else {
    save_count = 0;
    save_values = NULL;
  }

  p->dw = dw->prev;

  if (post) {
    if (scheme_setjmp(p->error_buf)) {
      scheme_restore_env_stack_w_process(dw->envss, p);
      p->current_local_env = dw->current_local_env;
      err = 1;
    } else {
      post(data);
    }
  }

  if (err)
    scheme_longjmp(dw->saveerr, 1);

  memcpy(&p->error_buf, &dw->saveerr, sizeof(jmp_buf));

  if (save_values) {
    p->ku.multiple.count = save_count;
    p->ku.multiple.array = save_values;
    return SCHEME_MULTIPLE_VALUES;
  }

  return v;
}

static Scheme_Object *
void_func (int argc, Scheme_Object *argv[])
{
  return scheme_void;
}

static Scheme_Object *
is_void_func (int argc, Scheme_Object *argv[])
{
  return SAME_OBJ(argv[0], scheme_void) ? scheme_true : scheme_false;
}

#ifdef TIME_SYNTAX

#ifndef CLOCKS_PER_SEC
#define CLOCKS_PER_SEC 1000000
#endif

long scheme_get_milliseconds(void)
{
#ifdef USE_FTIME
  struct MSC_IZE(timeb) now;
  MSC_IZE(ftime)(&now);
  return now.time * 1000 + now.millitm;
#else
#ifdef USE_DIFFTIME
  time_t now;
  now = time(NULL);
  return difftime(now, base_time) * 1000;
#else
  struct timeval now;
  gettimeofday(&now, NULL);
  return now.tv_sec * 1000 + now.tv_usec / 1000;
#endif
#endif
}

long scheme_get_process_milliseconds(void)
{
#ifdef USE_GETRUSAGE
  struct rusage use;
  long s, u;

  getrusage(RUSAGE_SELF, &use);

  s = use.ru_utime.tv_sec + use.ru_stime.tv_sec;
  u = use.ru_utime.tv_usec + use.ru_stime.tv_usec;

  return s * 1000 + u / 1000;
#else
  return clock()  * 1000 / CLOCKS_PER_SEC;
#endif
}

static long get_seconds(void)
{
#ifdef USE_FTIME
  struct MSC_IZE(timeb) now;
  MSC_IZE(ftime)(&now);
  return now.time;
#else
#ifdef USE_DIFFTIME
  time_t now;
  now = time(NULL);
  return now;
#else
  struct timeval now;
  gettimeofday(&now, NULL);
  return now.tv_sec;
#endif
#endif
}

#ifdef TIME_TYPE_IS_UNSIGNED
#define scheme_get_time_val(o, v) scheme_get_unsigned_int_val(o, v)
#define UNBUNDLE_TIME_TYPE unsigned long
#else
#define scheme_get_time_val(o, v) scheme_get_int_val(o, v)
#define UNBUNDLE_TIME_TYPE long
#endif

static Scheme_Object *seconds_to_date(int argc, Scheme_Object **argv)
{
  UNBUNDLE_TIME_TYPE lnow;
  time_t now;
  int hour, min, sec, month, day, year, wday, yday, dst;
  struct tm *localTime;
  Scheme_Object *p[9], *secs;

  secs = argv[0];

  if (!SCHEME_INTP(secs) && !SCHEME_BIGNUMP(secs)) {
    scheme_wrong_type("seconds->date", "exact integer", 0, argc, argv);
    return NULL;
  }

  if (scheme_get_time_val(secs, &lnow) 
      && ((UNBUNDLE_TIME_TYPE)(now = (time_t)lnow)) == lnow) {
    localTime = localtime(&now);
    if (localTime) {
      hour = localTime->tm_hour;
      min = localTime->tm_min;
      sec = localTime->tm_sec;
      
      month = localTime->tm_mon + 1;
      day = localTime->tm_mday;
      year = localTime->tm_year + 1900;
      
      wday = localTime->tm_wday;
      yday = localTime->tm_yday;
      
      dst = localTime->tm_isdst;

      p[0] = scheme_make_integer(sec);
      p[1] = scheme_make_integer(min);
      p[2] = scheme_make_integer(hour);
      p[3] = scheme_make_integer(day);
      p[4] = scheme_make_integer(month);
      p[5] = scheme_make_integer(year);
      p[6] = scheme_make_integer(wday);
      p[7] = scheme_make_integer(yday);
      p[8] = dst ? scheme_true : scheme_false;
      
      return scheme_make_struct_instance(scheme_date, 9, p);
    }
  }
    scheme_wrong_type("seconds->date", "date-integer", 0, argc, argv);

  scheme_raise_exn(MZEXN_APPLICATION_INTEGER,
		   secs,
		   "seconds->date: integer %s is out-of-range",
		   scheme_make_provided_string(secs, 0, NULL));

  return NULL;
}

static Scheme_Object *time_apply(int argc, Scheme_Object *argv[])
{
  long start, end;
  long cpustart, cpuend;
  long dur, cpudur;
  Scheme_Object *v, *p[3];

  scheme_check_proc_arity("time-apply", 0, 0, argc, argv);

  start = scheme_get_milliseconds();
  cpustart = scheme_get_process_milliseconds();
  v = _scheme_apply_multi(argv[0], 0, NULL);
  cpuend = scheme_get_process_milliseconds();
  end = scheme_get_milliseconds();
  
  dur = end - start;
  cpudur = cpuend - cpustart;

  if (v == SCHEME_MULTIPLE_VALUES) {
    Scheme_Process *p = scheme_current_process;
    v = scheme_build_list(p->ku.multiple.count,
			  p->ku.multiple.array);
  } else
    v = scheme_make_pair(v, scheme_null);

  p[0] = v;
  p[1] = scheme_make_integer(cpudur);
  p[2] = scheme_make_integer(dur);

  return scheme_values(3, p);
}

static Scheme_Object *current_milliseconds(int argc, Scheme_Object **argv)
{
  return scheme_make_integer(scheme_get_milliseconds());
}

static Scheme_Object *current_process_milliseconds(int argc, Scheme_Object **argv)
{
  return scheme_make_integer(scheme_get_process_milliseconds());
}

static Scheme_Object *current_gc_milliseconds(int argc, Scheme_Object **argv)
{
  return scheme_make_integer(scheme_total_gc_time);
}

static Scheme_Object *current_seconds(int argc, Scheme_Object **argv)
{
  return scheme_make_integer_value_from_time(get_seconds());
}

#endif


/****************************************************************/

static Scheme_Object *
current_print(int argc, Scheme_Object **argv)
{
  return scheme_param_config("current-print", MZCONFIG_PRINT_HANDLER,
			     argc, argv,
			     1, NULL, NULL, 0);
}

static Scheme_Object *
current_prompt_read(int argc, Scheme_Object **argv)
{
  return scheme_param_config("current-prompt-read", MZCONFIG_PROMPT_READ_HANDLER,
			     argc, argv,
			     0, NULL, NULL, 0);
}

static Scheme_Object *
default_print_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Object *obj = argv[0];

  if (!SAME_OBJ(obj, scheme_void)) {
    Scheme_Config *config = scheme_config;
    Scheme_Object *port = scheme_get_param(config, MZCONFIG_OUTPUT_PORT);
    Scheme_Object *argv[2];
    
    argv[0] = obj;
    argv[1] = port;
    _scheme_apply(scheme_print_proc, 2, argv);
    scheme_write_string("\n", 1, port);
  }

  return scheme_void;
}

static Scheme_Object *
default_prompt_read_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Config *config = scheme_config;
  Scheme_Object *port = scheme_get_param(config, MZCONFIG_OUTPUT_PORT);
  Scheme_Object *inport = scheme_get_param(config, MZCONFIG_INPUT_PORT);

  scheme_write_string("> ", 2, port);
  scheme_flush_output(port);
    
  return scheme_read(inport);
}


/****************************************************************/

void scheme_rep()
{
  jmp_buf save;
  Scheme_Process *p = scheme_current_process;

  memcpy(&save, &p->error_buf, sizeof(jmp_buf));
  if (scheme_setjmp(p->error_buf)) {
    /* done */
  } else
    scheme_apply(rep, 0, NULL);
  memcpy(&p->error_buf, &save, sizeof(jmp_buf));
}

/****************************************************************/

#define BOOL(x) (x ? scheme_true : scheme_false)

static Scheme_Object *write_compiled_closure(Scheme_Object *obj)
{
  Scheme_Closure_Compilation_Data *data;

  data = (Scheme_Closure_Compilation_Data *)obj;
  
  return CONS(scheme_make_integer(data->flags),
	      CONS(scheme_make_integer(data->num_params),
		   CONS(scheme_make_integer(data->max_let_depth),
			CONS(data->name ? data->name : scheme_null,
			     CONS(scheme_make_svector(data->closure_size,
						      data->closure_map),
				  data->code)))));
}

static Scheme_Object *read_compiled_closure(Scheme_Object *obj)
{
  Scheme_Closure_Compilation_Data *data;
  Scheme_Object *v;

#define BAD_CC "bad compiled closure"
#define X_SCHEME_ASSERT(x, y)

  data  = (Scheme_Closure_Compilation_Data *)scheme_malloc_stubborn_tagged(sizeof(Scheme_Closure_Compilation_Data));

  data->type = scheme_unclosed_procedure_type;

  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  data->flags = SCHEME_INT_VAL(v);

  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  data->num_params = SCHEME_INT_VAL(v);

  data->max_let_depth = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  data->name = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  if (SCHEME_NULLP(data->name))
    data->name = NULL;
  
  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  /* v is an svector */

  data->code = obj;

  data->closure_size = SCHEME_SVEC_LEN(v);
  data->closure_map = SCHEME_SVEC_VEC(v);
 
  scheme_end_stubborn_change((void *)data);

  if (data->flags & CLOS_FOLDABLE)
    data->flags -= CLOS_FOLDABLE;

  if (SCHEME_TYPE(data->code) > _scheme_values_types_)
    data->flags |= CLOS_FOLDABLE;

  if (!data->closure_size)
    return scheme_make_linked_closure(NULL, (Scheme_Object *)data, 0);

  return (Scheme_Object *)data;
}
