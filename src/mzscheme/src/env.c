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

#include "schminc.h"

#if defined(UNIX_LIMIT_STACK) || defined(UNIX_LIMIT_FDSET_SIZE)
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif

#ifdef MZ_USE_IRIX_SPROCS
#include "../gc/gc.h"
#endif

#ifdef SMALL_HASH_TABLES
#define GLOBAL_TABLE_SIZE 500
#define REFERENCES_TABLE_SIZE 500
#else
#define GLOBAL_TABLE_SIZE 5313
#define REFERENCES_TABLE_SIZE 5313
#endif

/* #define TIME_STARTUP_PROCESS */

/* globals */
int scheme_constant_builtins;
int scheme_allow_set_undefined;
int scheme_no_keywords;

int scheme_starting_up;

int scheme_hash_percent_syntax_only = 0;

Scheme_Object *scheme_local[MAX_CONST_LOCAL_POS][2];

/* locals */
static Scheme_Env *make_env(void);
static Scheme_Object *constant_prim(int argc, Scheme_Object *argv[]);
static Scheme_Object *constant_all_prim(int argc, Scheme_Object *argv[]);
static Scheme_Object *keyword_prim(int argc, Scheme_Object *argv[]);
static Scheme_Object *keyword_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *undefine(int argc, Scheme_Object *argv[]);
static Scheme_Object *list_globals(int argc, Scheme_Object *argv[]);
static Scheme_Object *defined(int argc, Scheme_Object *argv[]);
static Scheme_Object *global_defined_value(int, Scheme_Object *[]);
static Scheme_Object *local_exp_time_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *global_exp_time_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_exp_time_bound_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *local_exp_top_level_p(int argc, Scheme_Object *argv[]);

static Scheme_Object *current_loaded_library_table(int argc, Scheme_Object *argv[]);

static Scheme_Object *write_variable(Scheme_Object *obj);
static Scheme_Object *read_variable(Scheme_Object *obj);
static Scheme_Object *write_local(Scheme_Object *obj);
static Scheme_Object *read_local(Scheme_Object *obj);
static Scheme_Object *read_local_unbox(Scheme_Object *obj);

static void hash_percent(const char *name, Scheme_Object *obj, 
			 Scheme_Env *, int add);
static void add_primitive_symbol(Scheme_Object *sym, Scheme_Object *obj, 
				 Scheme_Env *env);

static int hash_percent_buflen = 0;
static char *hash_percent_buffer;

static int set_reference_ids = 0;
static int builtin_ref_counter = 0;

typedef struct Constant_Binding {
  Scheme_Object *name;
  Scheme_Object *val;
  short before;
  struct Constant_Binding *next;
} Constant_Binding;

#define ARBITRARY_USE 1
#define CONSTRAINED_USE 2
#define NOT_SETTABLE 4
#define WAS_SET_BANGED 8

typedef struct Compile_Data {
  char **stat_dists; /* (pos, depth) => used? */
  int *sd_depths;
  Constant_Binding *constants;
  int *use;
} Compile_Data;

typedef struct Scheme_Full_Comp_Env {
  Scheme_Comp_Env base;
  Compile_Data data;
} Scheme_Full_Comp_Env;
static void init_compile_data(Scheme_Comp_Env *env);

#define COMPILE_DATA(e) (&((Scheme_Full_Comp_Env *)e)->data)

#ifdef MZ_REAL_THREADS
void *scheme_global_lock;
#endif

#if defined(_IBMR2)
/* Under AIX, we need a hack to set stackbottom. HEURISTIC2
   doesn't really work, and 3.x is different from 4.x. */
extern long GC_stackbottom;
long scheme_stackbottom = 0;
#endif

Scheme_Env *scheme_basic_env ()
{
  Scheme_Process *process;
  Scheme_Env *env;

#ifdef UNIX_LIMIT_STACK
  {
    struct rlimit rl;
    
    getrlimit(RLIMIT_STACK, &rl);
    if (rl.rlim_cur > UNIX_LIMIT_STACK) {
      rl.rlim_cur = UNIX_LIMIT_STACK;
      setrlimit(RLIMIT_STACK, &rl);
    }
  }
#endif
#ifdef UNIX_LIMIT_FDSET_SIZE
  {
    struct rlimit rl;
    
    getrlimit(RLIMIT_NOFILE, &rl);
    if (rl.rlim_cur > FD_SETSIZE) {
      rl.rlim_cur = FD_SETSIZE;
      setrlimit(RLIMIT_NOFILE, &rl);
    }
  }
#endif

#if defined(_IBMR2)
  if (scheme_stackbottom)
    GC_stackbottom = scheme_stackbottom;
#endif

#ifdef MZ_USE_IRIX_SPROCS
  GC_INIT();
#endif

#ifdef TIME_STARTUP_PROCESS
   printf("#if 0\nbasic @ %ld\n", scheme_get_process_milliseconds());
#endif

#ifdef MUST_REGISTER_GLOBALS
  REGISTER_SO(hash_percent_buffer);

  scheme_register_bignum();
#endif

  scheme_starting_up = 1;

#ifdef MZ_REAL_THREADS
  scheme_global_lock = SCHEME_MAKE_MUTEX();
#else
  scheme_init_stack_check();
#endif

  {
    int i, k;

#ifndef USE_TAGGED_ALLOCATION
    Scheme_Local *all;

    all = (Scheme_Local *)scheme_malloc_eternal(sizeof(Scheme_Local) * 2 * MAX_CONST_LOCAL_POS);
# ifdef MEMORY_COUNTING_ON
    scheme_misc_count += sizeof(Scheme_Local) * 2 * MAX_CONST_LOCAL_POS;
# endif    
#endif

    for (i = 0; i < MAX_CONST_LOCAL_POS; i++)
      for (k = 0; k < 2; k++) {
	Scheme_Object *v;
	
#ifndef USE_TAGGED_ALLOCATION
	v = (Scheme_Object *)(all++);
#else
	v = (Scheme_Object *)scheme_malloc_eternal_tagged(sizeof(Scheme_Local));
#endif
	v->type = k + scheme_local_type;
	SCHEME_LOCAL_POS(v) = i;
	
	scheme_local[i][k] = v;
      }
  }

  scheme_init_true_false();

#ifdef TIME_STARTUP_PROCESS
  printf("pre-process @ %ld\n", scheme_get_process_milliseconds());
#endif

  process = scheme_make_process();
  process->stack_start = NULL;

#ifdef TIME_STARTUP_PROCESS
  printf("process @ %ld\n", scheme_get_process_milliseconds());
#endif

  env = scheme_top_level_env();
  
  scheme_init_error_escape_proc(process);

  scheme_starting_up = 0;

#ifdef TIME_STARTUP_PROCESS
  printf("done @ %ld\n#endif\n", scheme_get_process_milliseconds());
#endif

  return env;
}

static void primitive_syntax_through_scheme(const char *name, 
					    Scheme_Env *env)
{
  Scheme_Object *hp;

  if (scheme_no_keywords && scheme_hash_percent_syntax_only)
    return;

  hp = scheme_hash_percent_name(name, -1);

  if (!scheme_no_keywords)
    scheme_set_keyword(hp, env);

  if (!scheme_hash_percent_syntax_only) {
    Scheme_Object *sym = scheme_intern_symbol(name);

    scheme_add_global_symbol(sym, scheme_lookup_global(hp, env), env);
    
    if (scheme_constant_builtins)
      scheme_constant(sym, env);
  }
}

static void primitive_function_through_scheme(const char *name, 
					      Scheme_Env *env)
{
  Scheme_Object *sym, *hp;

  sym = scheme_intern_symbol(name);

  if (scheme_constant_builtins)
    scheme_constant(sym, env);

  hp = scheme_hash_percent_name(name, -1);
  
  add_primitive_symbol(hp, scheme_lookup_global(sym, env), env);

  if (!scheme_no_keywords)
    scheme_set_keyword(hp, env);
}

static void primitive_cond_through_scheme(const char *name, 
					  Scheme_Env *env)
{
  primitive_syntax_through_scheme(name, env);
  scheme_init_empty_cond(env);
}

#if USE_COMPILED_MACROS
Scheme_Object *scheme_eval_compiled_sized_string(const char *str, int len, Scheme_Env *env)
{
  Scheme_Object *port, *expr, *saved;
  Scheme_Process *p = scheme_current_process;
  Scheme_Config *config = p->config;

  port = scheme_make_sized_string_input_port(str, len);

  saved = scheme_get_param(config, MZCONFIG_ENV);
  scheme_set_param(config, MZCONFIG_ENV, (Scheme_Object *)env);
  expr = scheme_internal_read(port, 1, scheme_config
#ifdef MZ_REAL_THREADS
			      , p
#endif
			      );
  scheme_set_param(config, MZCONFIG_ENV, saved);

  return _scheme_eval_compiled(expr);
}
#endif

/* On the Mac, 68K, store the built-in Scheme code as pc-relative */
#if defined(__MWERKS__)
#if !defined(__powerc)
#pragma pcrelstrings on
#endif
#endif

Scheme_Env *scheme_top_level_env(void)
{
  Scheme_Env *env;
  Scheme_Object *q, *qq, *nllt;
#ifdef TIME_STARTUP_PROCESS
  long startt;
#endif

  env = make_env();

  if (scheme_starting_up)
    scheme_set_param(scheme_current_process->config, MZCONFIG_ENV, 
		     (Scheme_Object *)env);

  scheme_defining_primitives = 1;
  set_reference_ids = 1;
  builtin_ref_counter = 0;

#ifdef TIME_STARTUP_PROCESS
   printf("init @ %ld\n", scheme_get_process_milliseconds());
# define MZTIMEIT(n, f) (MARK_START_TIME(), f, DONE_TIME(n))
# define MARK_START_TIME() startt = scheme_get_process_milliseconds()
# define DONE_TIME(n) printf(#n ": %ld\n", (long)(scheme_get_process_milliseconds() - startt))
#else
# define MZTIMEIT(n, f) f
# define MARK_START_TIME() /**/
# define DONE_TIME(n) /**/
#endif

  /* The ordering of the first few init calls is important.
	  Add to the end of the list, not the beginning. */
  MZTIMEIT(symbol-table, scheme_init_symbol_table());
  MZTIMEIT(type-symbol-table, scheme_init_type_symbol_table());
  MZTIMEIT(type, scheme_init_type(env));
  MZTIMEIT(symbol-type, scheme_init_symbol_type(env));
  MZTIMEIT(fun, scheme_init_fun(env));
  MZTIMEIT(symbol, scheme_init_symbol(env));
  MZTIMEIT(type-symbol, scheme_init_type_symbol(env));
  MZTIMEIT(list, scheme_init_list(env));
  MZTIMEIT(number, scheme_init_number(env));
  MZTIMEIT(port, scheme_init_port(env));
  MZTIMEIT(string, scheme_init_string(env));
  MZTIMEIT(vector, scheme_init_vector(env));
  MZTIMEIT(char, scheme_init_char(env));
  MZTIMEIT(bool, scheme_init_bool(env));
  MZTIMEIT(syntax, scheme_init_syntax(env));
  MZTIMEIT(eval, scheme_init_eval(env));
  MZTIMEIT(error, scheme_init_error(env));
  MZTIMEIT(promise, scheme_init_promise(env));
  MZTIMEIT(struct, scheme_init_struct(env));
#ifndef NO_UNIT_SYSTEM
  MZTIMEIT(unit, scheme_init_unit(env));
#endif
#ifndef NO_SCHEME_EXNS
  MZTIMEIT(exn, scheme_init_exn(env));
#endif
  MZTIMEIT(process, scheme_init_process(env));
#ifndef NO_SCHEME_THREADS
  MZTIMEIT(sema, scheme_init_sema(env));
#endif
  MZTIMEIT(read, scheme_init_read(env));
  MZTIMEIT(print, scheme_init_print(env));
  MZTIMEIT(file, scheme_init_file(env));
  MZTIMEIT(dynamic-extension, scheme_init_dynamic_extension(env));
  MZTIMEIT(image, scheme_init_image(env));
#ifndef NO_REGEXP_UTILS
  MZTIMEIT(regexp, scheme_regexp_initialize(env));
#endif

  MARK_START_TIME();

  scheme_add_global_constant("constant-name",
			     scheme_make_prim_w_arity(constant_prim,
						      "constant-name",
						      1, 1),
			     env);
  scheme_add_global_constant("constant-name-all-globals",
			     scheme_make_prim_w_arity(constant_all_prim,
						      "constant-name-all-globals",
						      0, -1),
			     env);
  scheme_add_global_constant("keyword-name",
			     scheme_make_prim_w_arity(keyword_prim,
						      "keyword-name",
						      1, 1),
			     env);
  scheme_add_global_constant("keyword-name?",
			     scheme_make_prim_w_arity(keyword_p,
						      "keyword-name?",
						      1, 1),
			     env);
  scheme_add_global_constant("undefine",
			     scheme_make_prim_w_arity(undefine,
					      "undefine",
					      1, 1),
			     env);
  scheme_add_global_constant("make-global-value-list",
			     scheme_make_prim_w_arity(list_globals,
					      "make-global-value-list",
					      0, 0),
			     env);
  
  scheme_add_global_constant("defined?", 
			     scheme_make_prim_w_arity(defined,
						      "defined?",
						      1, 1),
			     env);
  scheme_add_global_constant("global-defined-value", 
			     scheme_make_prim_w_arity(global_defined_value,
						      "global-defined-value",
						      1, 2),
			     env);
  scheme_add_global_constant("local-expansion-time-value", 
			     scheme_make_prim_w_arity(local_exp_time_value,
						      "local-expansion-time-value",
						      1, 1),
			     env);
  scheme_add_global_constant("global-expansion-time-value", 
			     scheme_make_prim_w_arity(global_exp_time_value,
						      "global-expansion-time-value",
						      1, 1),
			     env);
  scheme_add_global_constant("local-expansion-time-bound?", 
			     scheme_make_prim_w_arity(local_exp_time_bound_p,
						      "local-expansion-time-bound?",
						      1, 1),
			     env);
  scheme_add_global_constant("local-expansion-top-level?", 
			     scheme_make_prim_w_arity(local_exp_top_level_p,
						      "local-expansion-top-level?",
						      0, 0),
			     env);
  

  DONE_TIME(env);

#ifndef NO_OBJECT_SYSTEM
  MZTIMEIT(object, scheme_init_object(env));
#endif

  if (scheme_starting_up) {
    scheme_install_type_writer(scheme_variable_type, write_variable);
    scheme_install_type_reader(scheme_variable_type, read_variable);
    scheme_install_type_writer(scheme_local_type, write_local);
    scheme_install_type_reader(scheme_local_type, read_local);
    scheme_install_type_writer(scheme_local_unbox_type, write_local);
    scheme_install_type_reader(scheme_local_unbox_type, read_local_unbox);
  }

  MARK_START_TIME();

  set_reference_ids = 0;

  /* It's too painful to implement macros without ` and ', so make
     sure they're always defined, and then undefine if necessary. */

  nllt = scheme_intern_symbol("current-loaded-library-table");
  scheme_add_global_symbol(nllt, scheme_make_prim(current_loaded_library_table), env);

  if (scheme_hash_percent_syntax_only) {
    Scheme_Object *q_hp;
    q = scheme_intern_symbol("quote");
    qq = scheme_intern_symbol("quasiquote");
    q_hp = scheme_intern_symbol("#%quote");

    scheme_add_global_symbol(q, scheme_lookup_global(q_hp, env), env);
  } else
    q = qq = NULL;

#define EVAL_ONE_STR(str) scheme_eval_string(str, env)
#define EVAL_ONE_SIZED_STR(str, len) scheme_eval_compiled_sized_string(str, len, env)
#define JUST_DEFINED(name) primitive_syntax_through_scheme(#name, env)
#define JUST_DEFINED_FUNC(name) primitive_function_through_scheme(#name, env)
#define JUST_DEFINED_KEY(name) primitive_syntax_through_scheme(#name, env)
#define JUST_DEFINED_COND(name) primitive_cond_through_scheme(#name, env)
#define JUST_DEFINED_QQ(name) \
   JUST_DEFINED_KEY(name); \
   if (qq) { \
     Scheme_Object *qq_hp = scheme_intern_symbol("#%quasiquote"); \
     scheme_add_global_symbol(qq, scheme_lookup_global(qq_hp, env), env); \
   }

#if USE_COMPILED_MACROS
#include "cmacro.inc"
#else
#include "macro.inc"
#endif

   scheme_remove_global_symbol(nllt, env);

  if (scheme_hash_percent_syntax_only) {
    scheme_remove_global_symbol(q, env);
    scheme_remove_global_symbol(qq, env);
  }

  if (scheme_starting_up) {
    scheme_init_format_procedure(env);
    scheme_init_rep(env);
  }

  if (scheme_secure_primitive_exn)
    scheme_secure_leftover_exceptions(env);

  DONE_TIME(macro);

  scheme_defining_primitives = 0;

  return env;
}

Scheme_Env *scheme_make_empty_env(void)
{
  return make_env();
}

#if defined(__MWERKS__)
#if !defined(__powerc)
#pragma pcrelstrings reset
#endif
#endif

static Scheme_Object *
undefine(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("undefine", "symbol", 0, argc, argv);

  scheme_remove_global_symbol(argv[0], scheme_get_env(scheme_config));
  return scheme_void;
}

static Scheme_Object *
current_loaded_library_table(int argc, Scheme_Object *argv[])
{
  return (Scheme_Object *)scheme_get_env(scheme_config)->loaded_libraries;
}

static Scheme_Env *make_env (void)
{
  Scheme_Hash_Table *globals;
  Scheme_Env *env;

  globals = scheme_hash_table(GLOBAL_TABLE_SIZE, SCHEME_hash_ptr, 
			      1, 0 /* scheme_starting_up */);

  env = MALLOC_ONE_TAGGED(Scheme_Env);

  env->type = scheme_namespace_type;
  env->globals = globals;

  env->loaded_libraries = scheme_hash_table(3, SCHEME_hash_ptr, 0, 0);

  env->init = (Scheme_Comp_Env *)MALLOC_ONE(Scheme_Full_Comp_Env);
  env->init->basic.num_bindings = 0;
  env->init->basic.next = NULL;
  env->init->basic.genv = env;
  init_compile_data(env->init);

  return env;
}

Scheme_Env *scheme_min_env(Scheme_Comp_Env *env)
{
  return env->basic.genv;
}

#define get_globals(env) (env->globals)

static void
do_add_global_symbol(Scheme_Env *env, Scheme_Object *sym, 
		     Scheme_Object *obj, int constant,
		     int primitive)
{
  Scheme_Hash_Table *globals;
  Scheme_Bucket *b;

  globals = get_globals(env);

  scheme_add_to_table(globals, (char *)sym, obj, constant);
  if (primitive) {
    b = scheme_bucket_from_table(globals, (char *)sym);
    
    ((Scheme_Bucket_With_Const_Flag *)b)->flags |= GLOB_IS_PRIMITIVE;
  } else
    b = NULL;

#if 0
  if (scheme_starting_up && !scheme_no_reference_id) {
    if (!b)
      b = scheme_bucket_from_table(globals, (char *)sym);

    ((Scheme_Bucket_With_Ref_Id *)b)->id = (++builtin_ref_counter);
    ((Scheme_Bucket_With_Const_Flag *)b)->flags |= GLOB_HAS_REF_ID;
  }
#endif
}

static void
do_add_global(Scheme_Env *env, const char *name, 
	      Scheme_Object *obj, int constant,
	      int primitive)
{
  Scheme_Object *sym;
  
  sym = scheme_intern_symbol(name);
  do_add_global_symbol(env, sym, obj, constant, primitive);
}

void
scheme_add_global(const char *name, Scheme_Object *obj, Scheme_Env *env)
{
  do_add_global(env, name, obj, 0, 0);
}

void
scheme_add_global_symbol(Scheme_Object *sym, Scheme_Object *obj, 
			 Scheme_Env *env)
{
  do_add_global_symbol(env, sym, obj, 0, 0);
}

static void
add_primitive_symbol(Scheme_Object *sym, Scheme_Object *obj, 
		     Scheme_Env *env)
{
  do_add_global_symbol(env, sym, obj, 1, 1);
}

void
scheme_remove_global_symbol(Scheme_Object *sym, Scheme_Env *env)
{
  Scheme_Bucket *b;
  Scheme_Hash_Table *globals;

  globals = get_globals(env);

  b = scheme_bucket_from_table(globals, (char *)sym);

  if (!b)
    return;

  if (((Scheme_Bucket_With_Const_Flag *)b)->flags & GLOB_IS_CONST)
    scheme_raise_exn(MZEXN_MISC_CONSTANT, sym,
		     "undefine: can't undefine constant %s",
		     scheme_symbol_name(sym));
  if (((Scheme_Bucket_With_Const_Flag *)b)->flags & GLOB_IS_KEYWORD)
    scheme_raise_exn(MZEXN_MISC_CONSTANT, sym,
		     "undefine: can't undefine keyword %s",
		     scheme_symbol_name(sym));
  if (((Scheme_Bucket_With_Const_Flag *)b)->flags & GLOB_IS_PERMANENT)
    scheme_raise_exn(MZEXN_MISC_CONSTANT, sym,
		     "undefine: can't undefine permanent global %s",
		     scheme_symbol_name(sym));

  b->val = NULL;
}

void
scheme_remove_global(const char *name, Scheme_Env *env)
{
  Scheme_Object *sym;

  sym = scheme_intern_symbol(name);

  scheme_remove_global_symbol(sym, env);
}

void
scheme_remove_global_constant(const char *name, Scheme_Env *env)
{
  scheme_remove_global(name, env);
#ifndef NO_SEPARATE_HASH_PRECENT
  hash_percent(name, NULL, env, 0);
#endif
}

void
scheme_add_global_constant(const char *name, Scheme_Object *obj, 
			   Scheme_Env *env)
{
  do_add_global(env, name, obj, scheme_constant_builtins, 0);
#ifndef NO_SEPARATE_HASH_PRECENT
  hash_percent(name, obj, env, 1);
#endif
}

void
scheme_add_global_constant_symbol(Scheme_Object *name, Scheme_Object *obj, 
				  Scheme_Env *env)
{
  do_add_global_symbol(env, name, obj, scheme_constant_builtins, 0);
#ifndef NO_SEPARATE_HASH_PRECENT
  hash_percent(SCHEME_SYM_VAL(name), obj, env, 1);
#endif
}

void 
scheme_set_keyword(Scheme_Object *sym, Scheme_Env *env)
{
  Scheme_Bucket *b;
  Scheme_Hash_Table *globals;
  
  globals = get_globals(env);
  
  b = scheme_bucket_from_table(globals, (char *)sym);
  ((Scheme_Bucket_With_Const_Flag *)b)->flags |= GLOB_IS_KEYWORD;
  if (!b->val)
    b->val = scheme_void;
}

void
scheme_add_global_keyword(const char *name, Scheme_Object *obj, 
			  Scheme_Env *env)
{
  Scheme_Object *hp = scheme_hash_percent_name(name, -1);

  do_add_global_symbol(env, hp, obj, 0, 0);

  if (!scheme_no_keywords)
    scheme_set_keyword(hp, env);

  if (!scheme_hash_percent_syntax_only) {
    Scheme_Object *sym = scheme_intern_symbol(name);

    scheme_add_global_symbol(sym, scheme_lookup_global(hp, env), env);
    
    if (scheme_constant_builtins)
      scheme_constant(sym, env);
  }
}

#ifndef NO_SEPARATE_HASH_PRECENT

Scheme_Object *scheme_hash_percent_name(const char *name, int len)
{
  if (len < 0)
    len = strlen(name);
  
  if (len + 2 > hash_percent_buflen) {
#ifdef MEMORY_COUNTING_ON
  scheme_misc_count -= hash_percent_buflen;
#endif

    hash_percent_buflen = 2 * (hash_percent_buflen + len + 2);
    hash_percent_buffer = (char *)scheme_malloc_atomic(hash_percent_buflen + 1);
    hash_percent_buffer[0] = '#';
    hash_percent_buffer[1] = '%';

#ifdef MEMORY_COUNTING_ON
  scheme_misc_count += hash_percent_buflen;
#endif
  }

  memcpy(hash_percent_buffer + 2, name, len + 1);

  return scheme_intern_exact_symbol(hash_percent_buffer, len + 2);
}

static void hash_percent(const char *name, Scheme_Object *obj,
			 Scheme_Env *env, int add)
{
  Scheme_Object *sym = scheme_hash_percent_name(name, -1);

  if (!add) {
    Scheme_Bucket *b;
    Scheme_Hash_Table *globals = get_globals(env);

    b = scheme_bucket_from_table(globals, (char *)sym);

    if (b)
      b->val = NULL; /* (even if it's constant) */
  } else {
    Scheme_Bucket *b;

    Scheme_Hash_Table *globals = get_globals(env);
    globals->has_constants = 2;

    do_add_global_symbol(env, sym, obj, 1, 1);

    globals->has_constants = 1;
    
    if (!scheme_no_keywords)
      scheme_set_keyword(sym, env);
    
#if 1
    if (set_reference_ids) {
      b = scheme_bucket_from_table(globals, (char *)sym);
      ((Scheme_Bucket_With_Ref_Id *)b)->id = (++builtin_ref_counter);
      ((Scheme_Bucket_With_Const_Flag *)b)->flags |= GLOB_HAS_REF_ID;
    }
#endif
  }
}

#endif

Scheme_Object **scheme_make_builtin_references_table(void)
{
  Scheme_Hash_Table *ht;
  Scheme_Object **t;
  Scheme_Bucket **bs;
  long i;

  t = (Scheme_Object **)scheme_malloc(sizeof(Scheme_Object *)
				      * (builtin_ref_counter + 1));
#ifdef MEMORY_COUNTING_ON
  scheme_misc_count += sizeof(Scheme_Object *) * (builtin_ref_counter + 1);
#endif

  ht = get_globals(scheme_get_env(scheme_config));

  bs = ht->buckets;

  for (i = ht->size; i--; ) {
    Scheme_Bucket *b = bs[i];
    if (b && (((Scheme_Bucket_With_Const_Flag *)b)->flags & GLOB_HAS_REF_ID))
      t[((Scheme_Bucket_With_Ref_Id *)b)->id] = (Scheme_Object *)b;
  }

  return t;
}

void scheme_check_identifier(const char *formname, Scheme_Object *id, 
			     const char *where, Scheme_Comp_Env *env,
			     Scheme_Object *form)
{
  Scheme_Bucket *b;
  Scheme_Hash_Table *globals;

  if (!where)
    where = "";

  if (!SCHEME_SYMBOLP(id))
    scheme_wrong_syntax(formname, form ? id : NULL, 
			form ? form : id, 
			"identifier must be a symbol%s", where);
  
  if (scheme_no_keywords)
    return;
  
  globals = get_globals(scheme_min_env(env));

  if (scheme_lookup_in_table(globals, (char *)id)) {
    b = scheme_bucket_from_table(globals, (char *)id);
    if (((Scheme_Bucket_With_Const_Flag *)b)->flags & GLOB_IS_KEYWORD)
      scheme_wrong_syntax(formname, form ? id : NULL, 
			  form ? form : id, "illegal use of keyword");
  }

  /* Allow #% for whatever if it's not already a keyword */
/*
  s = SCHEME_SYM_VAL(id);
  if (s[0] == '#' && s[1] == '%')
    scheme_signal_error("%s: illegal use of special identifier `%s'%s",
			form, s, where);
*/
}

static void init_compile_data(Scheme_Comp_Env *env)
{
  Compile_Data *data;
  int i, c;

  data = COMPILE_DATA(env);

  data->stat_dists = NULL;
  data->sd_depths = NULL;
  c = env->basic.num_bindings;
  data->use = MALLOC_N(int, c);
  for (i = 0; i < c; i++)
    data->use[i] = 0;

  data->constants = NULL;
}

Scheme_Comp_Env *scheme_new_compilation_frame(int num_bindings, int flags,
					 Scheme_Comp_Env *base)
{
  Scheme_Comp_Env *frame;
  int count;
  
  count = num_bindings;

  frame = (Scheme_Comp_Env *)MALLOC_ONE(Scheme_Full_Comp_Env);

  frame->values = MALLOC_N(Scheme_Object *, count);

  frame->basic.num_bindings = num_bindings;
  frame->basic.flags = flags;
  frame->basic.next = base;
  frame->basic.genv = base->basic.genv;

  init_compile_data(frame);

  return frame;
}

int scheme_used_app_only(Scheme_Comp_Env *env, int which)
{
  Compile_Data *data = COMPILE_DATA(env);

  if (data->use[which] & ARBITRARY_USE)
    return 0;
  else
    return 1;
}

int scheme_used_ever(Scheme_Comp_Env *env, int which)
{
  Compile_Data *data = COMPILE_DATA(env);

  return !!data->use[which];
}

int scheme_is_env_variable_boxed(Scheme_Comp_Env *env, int which)
{
  Compile_Data *data = COMPILE_DATA(env);

  return !!(data->use[which] & WAS_SET_BANGED);
}

void scheme_unsettable_variable(Scheme_Comp_Env *env, int which)
{
  Compile_Data *data = COMPILE_DATA(env);

  data->use[which] |= NOT_SETTABLE;
}

void
scheme_add_compilation_binding(int index, Scheme_Object *val, Scheme_Comp_Env *frame)
{
  if ((index >= frame->basic.num_bindings) || (index < 0))
    scheme_signal_error("internal error: scheme_add_binding: "
			"index out of range: %d", index);
  
  frame->values[index] = val;
}

void scheme_push_constant(Scheme_Object *name, Scheme_Object *val,
			  Scheme_Comp_Env *env)
{
  Compile_Data *data = COMPILE_DATA(env);
  Constant_Binding *b;
  
  b = (Constant_Binding *)scheme_malloc(sizeof(Constant_Binding));

  b->next = data->constants;
  b->name = name;
  b->val = val;
  b->before = env->basic.num_bindings;

  data->constants = b;
}

void scheme_pop_constant(Scheme_Comp_Env *env)
{
  Compile_Data *data = COMPILE_DATA(env);

  if (!data->constants)
    scheme_signal_error("internal error: scheme_pop_constant: empty");

  data->constants = data->constants->next;
}

Scheme_Comp_Env *
scheme_add_compilation_frame(Scheme_Object *vals, Scheme_Comp_Env *env, int flags)
{
  Scheme_Comp_Env *frame;
  int len, i, count;
  

  count = len = scheme_list_length(vals);

  frame = scheme_new_compilation_frame(count, flags, env);

  for (i = 0; i < len ; i++)
    if (SCHEME_SYMBOLP(vals))
      frame->values[i] = vals;
    else {
      frame->values[i] = SCHEME_CAR (vals);
      vals = SCHEME_CDR (vals);
    }
  
  init_compile_data(frame);

  return frame;
}

Scheme_Comp_Env *scheme_no_defines(Scheme_Comp_Env *env)
{
  if (scheme_is_toplevel(env))
    return scheme_new_compilation_frame(0, 0, env);
  else
    return env;
}

int scheme_is_toplevel(Scheme_Comp_Env *env)
{
  return !env->basic.next || (env->basic.flags & SCHEME_TOPLEVEL_FRAME);
}

Scheme_Comp_Env *scheme_extend_as_toplevel(Scheme_Comp_Env *env)
{
  if (scheme_is_toplevel(env))
    return env;
  else
    return scheme_new_compilation_frame(0, SCHEME_TOPLEVEL_FRAME, env);
}


static Scheme_Object *alloc_local(short type, int pos)
{
  Scheme_Object *v;

  v = (Scheme_Object *)scheme_malloc_atomic_tagged(sizeof(Scheme_Local));
  v->type = type;
  SCHEME_LOCAL_POS(v) = pos;

  return (Scheme_Object *)v;
}

Scheme_Object *scheme_make_local(short type, int pos)
{
  int k;

  k = type - scheme_local_type;

  if (pos < MAX_CONST_LOCAL_POS)
    return scheme_local[pos][k];

  return alloc_local(type, pos);
}

static Scheme_Local *get_frame_loc(Scheme_Comp_Env *frame, Compile_Data *data,
				   int i, int j, int p, int flags)
{
  data->use[i] |= (((flags & (SCHEME_APP_POS | SCHEME_SETTING))
		    ? CONSTRAINED_USE
		    : ARBITRARY_USE)
		   | ((flags & (SCHEME_SETTING | SCHEME_LINKING_REF))
		      ? WAS_SET_BANGED
		      : 0));
  
  if (!data->stat_dists) {
    data->stat_dists = MALLOC_N(char*, frame->basic.num_bindings);
    data->sd_depths = MALLOC_N(int, frame->basic.num_bindings);
    /* Rely on GC to zero-out array */
  }
  
  if (data->sd_depths[i] <= j) {
    char *naya, *a;
    int k;
    
    naya = MALLOC_N(char, (j + 1));
    a = data->stat_dists[i];
    for (k = data->sd_depths[i]; k--; )
      naya[k] = a[k];
    
    data->stat_dists[i] = naya;
    data->sd_depths[i] = j + 1;
  }

  data->stat_dists[i][j] = 1;

  return (Scheme_Local *)scheme_make_local(scheme_local_type, p + i);
}

Scheme_Object *
scheme_static_distance(Scheme_Object *symbol, Scheme_Comp_Env *env, int flags)
{
  Scheme_Comp_Env *frame;
  int j = 0, p = 0;
  Scheme_Bucket *b;
  Scheme_Object *val;
  Scheme_Hash_Table *globals;
  
  frame = env;
  for (frame = env; frame->basic.next != NULL; frame = frame->basic.next) {
    int i;
    Compile_Data *data = COMPILE_DATA(frame);
    Constant_Binding *c = data->constants;
    
    if (frame->basic.flags & SCHEME_LAMBDA_FRAME)
      j++;

    for (i = frame->basic.num_bindings; i--; ) {
      while (c && (c->before > i)) {
	if (SAME_OBJ(symbol, c->name)) {
	  val = c->val;
	  goto found_const;
	}
	c = c->next;
      }

      if (SAME_OBJ(symbol, frame->values[i])) {
	if ((flags & SCHEME_SETTING) && (data->use[i] & NOT_SETTABLE))
	  scheme_wrong_syntax("set!", NULL, symbol,
			      "imported/inherited variable cannot be mutated");

	if (flags & SCHEME_DONT_MARK_USE)
	  return scheme_make_local(scheme_local_type, 0);
	else
	  return (Scheme_Object *)get_frame_loc(frame, data, i, j, p, flags);
      }
    }

    while (c) {
      if (SAME_OBJ(symbol, c->name)) {
	val = c->val;
	goto found_const;
      }
      c = c->next;
    }

    p += frame->basic.num_bindings;
  }

  globals = get_globals(scheme_min_env(env));
  b = scheme_bucket_from_table(globals, (char *)symbol);
  if ((flags & SCHEME_ELIM_CONST) && b && b->val 
      && (((Scheme_Bucket_With_Const_Flag *)b)->flags & GLOB_IS_CONST)
      && !(flags & SCHEME_GLOB_ALWAYS_REFERENCE))
    return (Scheme_Object *)b->val;

  /* If the value in this env is syntax, we want the value, not a reference */
  if (b->val && !(flags & SCHEME_GLOB_ALWAYS_REFERENCE)) {
    Scheme_Type type = SCHEME_TYPE((Scheme_Object *)b->val);

    if ((type ==  scheme_syntax_compiler_type)
	|| (type == scheme_macro_type)
	|| (type == scheme_id_macro_type)
	|| (type == scheme_exp_time_type))
      return (Scheme_Object *)b->val;
  }

  return (Scheme_Object *)b;

 found_const:
  if (!(flags & SCHEME_ENV_CONSTANTS_OK)) {
    scheme_wrong_syntax("set!", NULL, symbol,
			"local constant cannot be mutated");
    return NULL;
  }
  return val;
}

void scheme_env_make_closure_map(Scheme_Comp_Env *env, short *_size, short **_map)
{
  Compile_Data *data;
  Scheme_Comp_Env *frame;
  int i, j, pos = 0, lpos = 0;
  short *map, size;

  /* Count vars used by this closure (skip args): */
  j = 1;
  for (frame = env->basic.next; frame; frame = frame->basic.next) {
    data = COMPILE_DATA(frame);

    if (frame->basic.flags & SCHEME_LAMBDA_FRAME)
      j++;

    if (data->stat_dists) {
      for (i = 0; i < frame->basic.num_bindings; i++) {
	if (data->sd_depths[i] > j) {
	  if (data->stat_dists[i][j]) {
	    pos++;
	    if (frame->basic.flags & SCHEME_ANCHORED_FRAME)
	      pos++;
	  }
	}
      }
    }
  }

  size = pos;
  *_size = size;
  map = MALLOC_N(short, size);
  *_map = map;

  /* Build map, unmarking locals and marking deeper in parent prame */
  j = 1; pos = 0;
  for (frame = env->basic.next; frame; frame = frame->basic.next) {
    data = COMPILE_DATA(frame);

    if (frame->basic.flags & SCHEME_LAMBDA_FRAME)
      j++;

    if (data->stat_dists) {
      for (i = 0; i < frame->basic.num_bindings; i++) {
	if (data->sd_depths[i] > j) {
	  if (data->stat_dists[i][j]) {
	    map[pos++] = lpos;
	    if (frame->basic.flags & SCHEME_ANCHORED_FRAME)
	      map[pos++] = -(lpos + 1);
	    data->stat_dists[i][j] = 0; /* This closure's done with these vars... */
	    data->stat_dists[i][j - 1] = 1; /* ... but insure previous keeps */
	  }
	}
	lpos++;
      }
    } else
      lpos += frame->basic.num_bindings;
  }
}

int *scheme_env_get_flags(Scheme_Comp_Env *frame, int start, int count)
{
  Compile_Data *data = COMPILE_DATA(frame);
  int *v, i;
  
  v = MALLOC_N(int, count);
  memcpy(v, data->use + start, sizeof(int) * count);

  for (i = count; i--; ) {
    int old;
    old = v[i];
    v[i] = 0;
    if (old & (ARBITRARY_USE | CONSTRAINED_USE))
      v[i] |= SCHEME_WAS_USED;
    if (old & WAS_SET_BANGED)
      v[i] |= SCHEME_WAS_SET_BANGED;
  }

  return v;
}

Link_Info *scheme_link_info_create(int can_opt)
{
  Link_Info *naya;

  naya = MALLOC_ONE(Link_Info);
  naya->count = 0;
  naya->next = NULL;
  naya->can_optimize_constants = can_opt;

  return naya;
}

Link_Info *scheme_link_info_extend(Link_Info *info, int size, int oldsize, int mapc)
     /* size = number of appended items in run-time frame */
     /* oldisze = number of appended items in original compile-time frame */
     /* mapc = mappings that will be installed */
{
  Link_Info *naya;

  naya = MALLOC_ONE(Link_Info);
  naya->next = info;
  naya->size = size;
  naya->oldsize = oldsize;
  naya->count = mapc;
  naya->pos = 0;
  naya->anchor_offset = 0;
  naya->old_pos = MALLOC_N(short, mapc);
  naya->new_pos = MALLOC_N(short, mapc);
  naya->flags = MALLOC_N(int, mapc);

  naya->can_optimize_constants = info->can_optimize_constants;

  return naya;
}

void scheme_link_info_add_mapping(Link_Info *info, int oldp, int newp, int flags)
{
  if (info->pos == info->count) {
    scheme_signal_error("internal error: add_mapping: "
			"too many: %d", info->pos);
  }

  info->old_pos[info->pos] = oldp;
  info->new_pos[info->pos] = newp;
  info->flags[info->pos] = flags;
  
  info->pos++;
}

void scheme_link_info_set_anchor_offset(Link_Info *info, int offset)
{
  info->anchor_offset = offset;
}

static int link_info_lookup(Link_Info *info, int pos, int *flags)
{
  int i, offset = 0, orig = pos;
  int get_anchor;

  if (pos < 0) {
    get_anchor = pos;
    pos = -(pos + 1);
  } else
    get_anchor = 0;

  while (info) {
    for (i = info->pos; i--; ) {
      int oldp = info->old_pos[i];
      if (pos == oldp) {
	/* not yet mapped anchor */
	if (flags)
	  *flags = info->flags[i];
	return info->new_pos[i] + offset + (get_anchor ? info->anchor_offset : 0);
      }
      if (get_anchor && (get_anchor == oldp)) {
	/* re-mapped anchor */
	if (flags)
	  *flags = info->flags[i];
	return info->new_pos[i] + offset;
      }
    }

    pos -= info->oldsize;
    offset += info->size;
    info = info->next;
  }

  scheme_signal_error("internal error: scheme_link_info_lookup: "
		      "variable %d not found", orig);

  return 0;
}

int scheme_link_info_flags(Link_Info *info, int pos)
{
  int flags;

  link_info_lookup(info, pos, &flags);

  return flags;
}

int scheme_link_info_lookup(Link_Info *info, int pos, int *flags)
{
  return link_info_lookup(info, pos, flags);
}

int scheme_link_info_lookup_anchor(Link_Info *info, int pos)
{
  return link_info_lookup(info, -(pos + 1), NULL);
}


Scheme_Object *
scheme_lookup_global (Scheme_Object *symbol, Scheme_Env *env)
{
  return (Scheme_Object *)scheme_lookup_in_table(get_globals(env), (char *)symbol);
}

Scheme_Bucket *
scheme_global_bucket (Scheme_Object *symbol, Scheme_Env *env)
{
  return scheme_bucket_from_table(get_globals(env), (char *)symbol);
}

Scheme_Object *scheme_make_envunbox(Scheme_Object *value)
{
  Scheme_Object *obj;

  obj = (Scheme_Object *)scheme_malloc_envunbox(sizeof(Scheme_Object*));
  SCHEME_ENVBOX_VAL(obj) = value;

  return obj;
}

void scheme_constant(Scheme_Object *sym, Scheme_Env *env)
{
  if (!SCHEME_SYMBOLP(sym))
    scheme_wrong_type("constant-name", "symbol", 0, 1, &sym);

  do_add_global_symbol(env, sym, NULL, 1, 0);
}

static Scheme_Object *
constant_prim(int argc, Scheme_Object *argv[])
{
  scheme_constant(argv[0], scheme_get_env(scheme_config));
  return scheme_void;
}

static Scheme_Object *
constant_all_prim (int argc, Scheme_Object *argv[])
{
  int i, j;
  Scheme_Hash_Table *globals;
  Scheme_Bucket *b;

  for (i = 0; i < argc; i++)
    if (!SCHEME_SYMBOLP(argv[i]))
      scheme_wrong_type("constant-name-all-globals", 
			"symbol", i, argc, argv);

  globals = get_globals(scheme_get_env(scheme_config));

  for (i = 0; i < globals->size; i++) {
    b = globals->buckets[i];
    if (b && b->val) {
      for (j = 0; j < argc; j++)
	if (b->key == (void *)argv[j])
	  break;
      
      if (j >= argc)
	((Scheme_Bucket_With_Const_Flag *)b)->flags |= GLOB_IS_CONST;
    }
  }

  return scheme_void;
}

static Scheme_Object *keyword_prim(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("keyword-name", "symbol", 0, argc, argv);

  scheme_set_keyword(argv[0], scheme_get_env(scheme_config));
  return scheme_void;
}

static Scheme_Object *keyword_p(int argc, Scheme_Object *argv[])
{
  Scheme_Bucket *b;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("keyword-name?", "symbol", 0, argc, argv);

  b = scheme_global_bucket(argv[0], scheme_get_env(scheme_config));

  return ((((Scheme_Bucket_With_Const_Flag *)b)->flags & GLOB_IS_KEYWORD) 
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *list_globals(int argc, Scheme_Object *argv[])
{
  Scheme_Object *l = scheme_null;
  int i;
  Scheme_Hash_Table *globals;
  Scheme_Bucket *b;

  globals = get_globals(scheme_get_env(scheme_config));

  for (i = 0; i < globals->size; i++) {
    b = globals->buckets[i];
    if (b && b->val)
      l = scheme_make_pair(scheme_make_pair((Scheme_Object *)b->key, 
					    (Scheme_Object *)b->val),
			   l);
  }

  return l;
}

static Scheme_Object *defined(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("defined?", "symbol", 0, argc, argv);

  return scheme_lookup_global(argv[0], scheme_get_env(scheme_config))
    ? scheme_true : scheme_false;
}

static Scheme_Object *
global_defined_value(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;
  Scheme_Env *env;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("global-defined-value", "symbol", 0, argc, argv);

  env = scheme_get_env(scheme_config);

  if (argc > 1) {
    Scheme_Bucket *bucket;

    bucket = scheme_global_bucket(argv[0], env);

    scheme_set_global_bucket("global-defined-value", bucket, argv[1], 1);

    return scheme_void;
  } else {
    v = scheme_lookup_global(argv[0], env);
    
    if (!v)
      scheme_raise_exn(MZEXN_VARIABLE, argv[0],
		       "global-defined-value: %s is not defined",
		       scheme_symbol_name(argv[0]));

    return v;
  }
}

static Scheme_Object *
local_exp_time_value(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;
  Scheme_Comp_Env *env;

  env = scheme_current_process->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_MISC_EXPANSION_TIME,
		     "local-expansion-time-value: illegal at run-time");

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("local-expansion-time-value", "symbol", 0, argc, argv);

  v = scheme_static_distance(argv[0], env,
			     (SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
			      + SCHEME_ELIM_CONST));

  /* Deref globls */
  if (SAME_TYPE(SCHEME_TYPE(v), scheme_variable_type))
    v = (Scheme_Object *)(SCHEME_VAR_BUCKET(v))->val;

  if (!v || NOT_SAME_TYPE(SCHEME_TYPE(v), scheme_exp_time_type))
    scheme_raise_exn(MZEXN_MISC_EXPANSION_TIME,
		     "local-expansion-time-value: %s is not defined "
		     "as an expansion-time value",
		     scheme_symbol_name(argv[0]));
  
  return SCHEME_PTR_VAL(v);
}


static Scheme_Object *
global_exp_time_value(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;
  Scheme_Comp_Env *env;

  env = scheme_current_process->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_MISC_EXPANSION_TIME,
		     "global-expansion-time-value: illegal at run-time");

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("global-expansion-time-value", "symbol", 0, argc, argv);

  v = scheme_lookup_global(argv[0], scheme_min_env(env));

  /* Deref global */
  if (v && (SAME_TYPE(SCHEME_TYPE(v), scheme_variable_type)))
    v = (Scheme_Object *)(SCHEME_VAR_BUCKET(v))->val;

  if (!v || NOT_SAME_TYPE(SCHEME_TYPE(v), scheme_exp_time_type))
    scheme_raise_exn(MZEXN_MISC_EXPANSION_TIME,
		     "global-expansion-time-value: %s is not defined "
		     "as an expansion-time value",
		     scheme_symbol_name(argv[0]));
  
  return SCHEME_PTR_VAL(v);
}


static Scheme_Object *
local_exp_time_bound_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;
  Scheme_Comp_Env *env;

  env = scheme_current_process->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_MISC_EXPANSION_TIME,
		     "local-expansion-time-bound?: illegal at run-time");

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("local-expansion-time-bound?", "symbol", 0, argc, argv);

  v = scheme_static_distance(argv[0], env,
			     (SCHEME_APP_POS + SCHEME_ENV_CONSTANTS_OK
			      + SCHEME_ELIM_CONST));

  /* Deref globls */
  if (SAME_TYPE(SCHEME_TYPE(v), scheme_variable_type))
    return scheme_false;
  else
    return scheme_true;
}

static Scheme_Object *
local_exp_top_level_p(int argc, Scheme_Object *argv[])
{
  Scheme_Comp_Env *env;

  env = scheme_current_process->current_local_env;
  if (!env)
    scheme_raise_exn(MZEXN_MISC_EXPANSION_TIME,
		     "local-expansion-top-level?: illegal at run-time");

  return (scheme_is_toplevel(env)
	  ? scheme_true
	  : scheme_false);
}

/*********************************************************************/

static Scheme_Object *write_variable(Scheme_Object *obj)
{
  Scheme_Object *sym;

  sym = (Scheme_Object *)(SCHEME_VAR_BUCKET(obj))->key;

  return sym;
}

static Scheme_Object *read_variable(Scheme_Object *obj)
{
#if 0
  if (!SCHEME_PAIRP(obj) || !SCHEME_NULLP(SCHEME_CDR(obj))
      || !SCHEME_SYMBOLP(SCHEME_CAR(obj)))
    scheme_signal_error("bad compiled global");
#endif

#define SCHEME_GLOBAL_REFERENCE (Scheme_Object *)scheme_global_bucket

  return SCHEME_GLOBAL_REFERENCE(obj, scheme_get_env(scheme_config));
}

static Scheme_Object *write_local(Scheme_Object *obj)
{
  return scheme_make_integer(SCHEME_LOCAL_POS(obj));
}

static Scheme_Object *read_local(Scheme_Object *obj)
{
  return scheme_make_local(scheme_local_type,
			   SCHEME_INT_VAL(obj));
}

static Scheme_Object *read_local_unbox(Scheme_Object *obj)
{
  return scheme_make_local(scheme_local_unbox_type,
			   SCHEME_INT_VAL(obj));
}

