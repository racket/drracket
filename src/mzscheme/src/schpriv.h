/*
  MzScheme
  Copyright (c) 1995 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* 
   MzScheme prototypes for private functions.
   (The prototypes are in an effectively random order.)
*/

#ifndef __mzscheme_private__
#define __mzscheme_private__

#include "scheme.h"

#ifndef MZ_REAL_THREADS
# define RUNSTACK_IS_GLOBAL
#endif

#ifdef RUNSTACK_IS_GLOBAL
extern Scheme_Object **scheme_current_runstack;
extern Scheme_Object **scheme_current_runstack_start;
extern MZ_MARK_POS_TYPE scheme_current_cont_mark_stack;
extern MZ_MARK_STACK_TYPE scheme_current_cont_mark_pos;
# define MZ_RUNSTACK scheme_current_runstack
# define MZ_RUNSTACK_START scheme_current_runstack_start
# define MZ_CONT_MARK_STACK scheme_current_cont_mark_stack
# define MZ_CONT_MARK_POS scheme_current_cont_mark_pos
#else
# define MZ_RUNSTACK (p->runstack)
# define MZ_RUNSTACK_START (p->runstack_start)
# define MZ_CONT_MARK_STACK (p->cont_mark_stack)
# define MZ_CONT_MARK_POS (p->cont_mark_pos)
#endif

#ifdef MZTAG_REQUIRED
# define MZTAG_IF_REQUIRED  Scheme_Type type;
#else
# define MZTAG_IF_REQUIRED /* empty */
#endif

#define GLOB_IS_CONST 1
#define GLOB_IS_KEYWORD 2
#define GLOB_IS_PRIMITIVE 4
#define GLOB_IS_PERMANENT 8
#define GLOB_HAS_REF_ID 16

typedef struct {
  Scheme_Bucket bucket;
  short flags;
} Scheme_Bucket_With_Const_Flag;

typedef struct {
  Scheme_Bucket_With_Const_Flag flagged;
  short id;
} Scheme_Bucket_With_Ref_Id;

typedef struct Scheme_Comp_Env
{
  MZTAG_IF_REQUIRED
  short num_bindings;
  short flags; /* used for expanding/compiling */
  Scheme_Env *genv;
  struct Scheme_Comp_Env *next;
  struct Scheme_Object **values;
} Scheme_Comp_Env;


#define CLOS_HAS_REST 1
#define CLOS_MUST_ALLOC 2
#define CLOS_ONLY_LOCALS 4
#define CLOS_FOLDABLE 8

typedef struct Scheme_Compile_Info
{
  MZTAG_IF_REQUIRED
  int max_let_depth;
  char can_optimize_constants;
  char keep_unit_debug_info;
  char dont_mark_local_use;
  Scheme_Object *value_name;
} Scheme_Compile_Info;

typedef struct Link_Info
{
  MZTAG_IF_REQUIRED
  int can_optimize_constants;
  int size, oldsize, count, pos, anchor_offset;
  short *old_pos;
  short *new_pos;
  int *flags;
  struct Link_Info *next;
} Link_Info;

typedef struct Scheme_Object *
(Scheme_Syntax)(struct Scheme_Object *form, struct Scheme_Comp_Env *env,
		struct Scheme_Compile_Info *rec);

typedef struct Scheme_Object *
(Scheme_Syntax_Expander)(struct Scheme_Object *form, struct Scheme_Comp_Env *env,
			 int depth);

typedef struct Scheme_Object *
(Scheme_Syntax_Linker)(Scheme_Object *data, Link_Info *link);

typedef struct Scheme_Object *
(Scheme_Syntax_Executer)(struct Scheme_Object *data);

typedef Scheme_Syntax_Executer Scheme_Syntax_Registered;

typedef struct Scheme_Closure_Compilation_Data
{
  Scheme_Type type;
  /* Scheme_Object *src; */
  short flags;
  short num_params; /* includes collecting arg if has_rest */
  short max_let_depth;
  short closure_size;
  short *closure_map;
  Scheme_Object *code;
  Scheme_Object *name;
} Scheme_Closure_Compilation_Data;

typedef struct {
  Scheme_Type type;
#ifdef MZ_PRECISE_GC
  MZ_HASH_KEY_EX
  int closure_size;
#endif
  Scheme_Object *code;
  Scheme_Object *vals[1];
} Scheme_Closed_Compiled_Procedure;

#define SCHEME_COMPILED_CLOS_CODE(c) ((Scheme_Closed_Compiled_Procedure *)c)->code
#define SCHEME_COMPILED_CLOS_ENV(c) ((Scheme_Closed_Compiled_Procedure *)c)->vals

typedef struct Scheme_Struct_Type {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  short num_slots;
  short name_pos;
  Scheme_Object *type_name;
  struct Scheme_Struct_Type *parent_types[1];
} Scheme_Struct_Type;

typedef struct Scheme_Structure
{
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Struct_Type *stype;
  Scheme_Object *slots[1];
} Scheme_Structure;

#define SCHEME_STRUCT_TYPE(o) (((Scheme_Structure *)o)->stype)

#define SCHEME_STRUCT_NUM_SLOTS(o) (SCHEME_STRUCT_TYPE(o)->num_slots)
#define SCHEME_STRUCT_NAME_SYM(o) (SCHEME_STRUCT_TYPE(o)->type_name)

typedef struct {
  Scheme_Type type;
  short num_args;
  Scheme_Object *args[1];
  /* After array of f & args, array of chars for eval type */
} Scheme_App_Rec;

typedef struct {
  Scheme_Type type;
  Scheme_Object *test;
  Scheme_Object *tbranch;
  Scheme_Object *fbranch;
} Scheme_Branch_Rec;

typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  short max_let_depth;
  Scheme_Object *code;
} Scheme_Compilation_Top;

extern int scheme_starting_up;


typedef struct Scheme_Compiled_Let_Value {
  Scheme_Type type;
  short count;
  short position;
  int *flags;
  Scheme_Object *value;
  Scheme_Object *body;
} Scheme_Compiled_Let_Value;

typedef struct Scheme_Let_Header {
  Scheme_Type type;
  short count;
  short recursive;
  short num_clauses;
  Scheme_Object *body;
} Scheme_Let_Header;

typedef struct {
  Scheme_Type type;
  Scheme_Object *key;
  Scheme_Object *val;
  Scheme_Object *body;
} Scheme_With_Continuation_Mark;

/* ininitializarion */
void scheme_init_stack_check(void);
#ifdef MZ_PRECISE_GC
void scheme_register_traversers(void);
void scheme_init_hash_key_procs(void);
#endif
Scheme_Process *scheme_make_process(void);
void scheme_init_true_false(void);
void scheme_init_symbol_table (void);
void scheme_init_symbol_type (Scheme_Env *env);
void scheme_init_type (Scheme_Env *env);
void scheme_init_list (Scheme_Env *env);
void scheme_init_port (Scheme_Env *env);
void scheme_init_file (Scheme_Env *env);
void scheme_init_proc (Scheme_Env *env);
void scheme_init_vector (Scheme_Env *env);
void scheme_init_string (Scheme_Env *env);
void scheme_init_number (Scheme_Env *env);
void scheme_init_eval (Scheme_Env *env);
void scheme_init_promise (Scheme_Env *env);
void scheme_init_struct (Scheme_Env *env);
void scheme_init_fun(Scheme_Env *env);
void scheme_init_symbol(Scheme_Env *env);
void scheme_init_char(Scheme_Env *env);
void scheme_init_bool(Scheme_Env *env);
void scheme_init_syntax(Scheme_Env *env);
void scheme_init_error(Scheme_Env *env);
void scheme_init_exn(Scheme_Env *env);
void scheme_init_debug(Scheme_Env *env);
#ifndef NO_OBJECT_SYSTEM
void scheme_init_object(Scheme_Env *env);
#endif
void scheme_init_unit(Scheme_Env *env);
void scheme_init_process(Scheme_Env *env);
void scheme_init_read(Scheme_Env *env);
void scheme_init_print(Scheme_Env *env);
void scheme_init_image(Scheme_Env *env);
#ifndef NO_SCHEME_THREADS
void scheme_init_sema(Scheme_Env *env);
#endif
void scheme_init_dynamic_extension(Scheme_Env *env);
#ifndef NO_REGEXP_UTILS
extern void scheme_regexp_initialize(Scheme_Env *env);
#endif
void scheme_init_empty_cond(Scheme_Env *env);
void scheme_init_format_procedure(Scheme_Env *env);
void scheme_init_rep(Scheme_Env *env);
void scheme_init_getenv(void);

extern Scheme_Type_Reader *scheme_type_readers;
extern Scheme_Type_Writer *scheme_type_writers;

Scheme_Object *scheme_find_linker_name(Scheme_Syntax_Registered *f);
Scheme_Syntax_Registered *scheme_find_linker(Scheme_Object *sym);

extern Scheme_Object *scheme_void_func;

#ifndef MZ_REAL_THREADS
extern int scheme_fuel_counter;
#endif

void scheme_out_of_fuel(void);
#define SCHEME_USE_FUEL(n) { \
  if ((scheme_fuel_counter -= (n)) <= 0) { scheme_out_of_fuel(); }}

Scheme_Object *scheme_handle_stack_overflow(Scheme_Object *(*k)(void));

Scheme_Object *scheme_read_number(const char *str, long len,
				  int is_float, 
				  int is_not_float,
				  int decimal_means_float,
				  int radix, int radix_set, 
				  Scheme_Object *port,
				  int *div_by_zero,
				  int test_only);

typedef long bigdig;

typedef struct {
  Scheme_Type type;
#if MZ_PRECISE_GC
  char pos;
  char allocated_inline;
#else
  short pos;
#endif
  int len;
  bigdig *digits;
} Scheme_Bignum;

#define SCHEME_BIGPOS(b) (((Scheme_Bignum *)b)->pos)
#define SCHEME_BIGLEN(b) (((Scheme_Bignum *)b)->len)
#define SCHEME_BIGDIG(b) (((Scheme_Bignum *)b)->digits)

typedef struct {
  Scheme_Bignum o;
  bigdig v[1];
} Small_Bignum;
  
typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Object *num;
  Scheme_Object *denom;
} Scheme_Rational;

typedef Scheme_Rational Small_Rational;

typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Object *r;
  Scheme_Object *i;
} Scheme_Complex;

typedef Scheme_Complex Small_Complex;

#define _scheme_complex_real_part(n) (((Scheme_Complex *)(n))->r)
#define _scheme_complex_imaginary_part(n) (((Scheme_Complex *)(n))->i)

Scheme_Object *scheme_make_small_bignum(long v, Small_Bignum *s);
char *scheme_number_to_string(int radix, Scheme_Object *obj);

Scheme_Object *_scheme_apply_to_list (Scheme_Object *rator, Scheme_Object *rands);
Scheme_Object *_scheme_tail_apply_to_list (Scheme_Object *rator, Scheme_Object *rands);

void scheme_ensure_stack_start(Scheme_Process *p, void *d);

void *scheme_top_level_do(void *(*k)(void), int eb);
#define scheme_top_level_do_w_process(k, eb, p) scheme_top_level_do(k, eb)

typedef struct Scheme_Saved_Stack {
  MZTAG_IF_REQUIRED
  Scheme_Object **runstack_start;
  Scheme_Object **runstack;
  long runstack_size;
  struct Scheme_Saved_Stack *prev;
} Scheme_Saved_Stack;

typedef struct Scheme_Cont_Mark {
  MZTAG_IF_REQUIRED
  Scheme_Object *key;
  Scheme_Object *val;
  struct Scheme_Cont_Mark_Chain *cached_chain;
  MZ_MARK_POS_TYPE pos;
} Scheme_Cont_Mark;

typedef struct Scheme_Cont_Mark_Chain {
  MZTAG_IF_REQUIRED
  Scheme_Object *key;
  Scheme_Object *val;
  struct Scheme_Cont_Mark_Chain *next;
} Scheme_Cont_Mark_Chain;

typedef struct Scheme_Cont_Mark_Set {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  struct Scheme_Cont_Mark_Chain *chain;
} Scheme_Cont_Mark_Set;

#define SCHEME_LOG_MARK_SEGMENT_SIZE 8
#define SCHEME_MARK_SEGMENT_SIZE (1 << SCHEME_LOG_MARK_SEGMENT_SIZE)
#define SCHEME_MARK_SEGMENT_MASK (SCHEME_MARK_SEGMENT_SIZE - 1)

typedef struct Scheme_Stack_State {
  Scheme_Object **runstack;
  Scheme_Object **runstack_start;
  long runstack_size;
  Scheme_Saved_Stack *runstack_saved;
  MZ_MARK_POS_TYPE cont_mark_pos;
  MZ_MARK_STACK_TYPE cont_mark_stack;
} Scheme_Stack_State;

typedef struct Scheme_Dynamic_Wind {
  MZTAG_IF_REQUIRED
  void *data;
  void (*pre)(void *);
  void (*post)(void *);
  mz_jmp_buf saveerr;
  Scheme_Comp_Env *current_local_env;
  struct Scheme_Stack_State envss;
  struct Scheme_Cont *cont;
  struct Scheme_Dynamic_Wind *prev;
} Scheme_Dynamic_Wind;

typedef struct Scheme_Cont {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Object *value;
  Scheme_Jumpup_Buf buf;
  long *ok;
  Scheme_Dynamic_Wind *dw, *common;
  Scheme_Process *home;
  Scheme_Continuation_Jump_State cjs;
  mz_jmp_buf save_overflow_buf;
  int suspend_break;
  Scheme_Stack_State ss;
  Scheme_Saved_Stack *runstack_copied;
  Scheme_Cont_Mark *cont_mark_stack_copied;
  struct Scheme_Overflow *save_overflow;
  Scheme_Comp_Env *current_local_env;
  mz_jmp_buf savebuf; /* save old error buffer here */
} Scheme_Cont;

typedef struct Scheme_Escaping_Cont {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Continuation_Jump_State cjs;
  Scheme_Process *home;
  long *ok;  
  Scheme_Object *f;
  int suspend_break;
} Scheme_Escaping_Cont;

#define SCHEME_CONT_HOME(obj)  (((Scheme_Escaping_Cont *)(obj))->home)
#define SCHEME_CONT_OK(obj)  (((Scheme_Escaping_Cont *)(obj))->ok)
#define SCHEME_CONT_F(obj) (((Scheme_Escaping_Cont *)(obj))->f)

#define _scheme_do_eval(obj, env, v) \
  ((SCHEME_INTP(obj) || !SCHEME_STRTAG_VAL(_SCHEME_TYPE(obj))) \
   ? obj : scheme_do_eval(obj, -1, env, v))
#define q_scheme_eval_compiled(obj) _scheme_do_eval(obj, 1)
#define q_scheme_tail_eval(obj) scheme_tail_eval(obj)

#define SEMAPHORE_WAITING_IS_COLLECTABLE 1

#ifndef MZ_REAL_THREADS
# if SEMAPHORE_WAITING_IS_COLLECTABLE
typedef struct Scheme_Sema_Waiter {
  MZTAG_IF_REQUIRED
  Scheme_Process *p;
  int in_line;
  struct Scheme_Sema_Waiter *prev, *next;
} Scheme_Sema_Waiter;
# endif
#endif

typedef struct Scheme_Sema {
  Scheme_Type type;
  MZ_HASH_KEY_EX
#ifdef MZ_REAL_THREADS
  void *sema;
#else
  long value;  
# if SEMAPHORE_WAITING_IS_COLLECTABLE
  Scheme_Sema_Waiter *first, *last;
# endif
#endif
} Scheme_Sema;

typedef struct Scheme_Local {
  Scheme_Type type;
  short position;
#ifdef MZ_PRECISE_GC
  /* Everything has to be at least 2 words in size. */
  int x;
#endif
} Scheme_Local;

typedef struct Scheme_Let_Value {
  Scheme_Type type;
  short count;
  short position;
  short autobox;
  Scheme_Object *value;
  Scheme_Object *body;
} Scheme_Let_Value;

typedef struct Scheme_Let_One {
  Scheme_Type type;
  short eval_type;
  Scheme_Object *value;
  Scheme_Object *body;
} Scheme_Let_One;

typedef struct Scheme_Let_Void {
  Scheme_Type type;
  short count;
  short autobox;
  Scheme_Object *body;
} Scheme_Let_Void;

typedef struct Scheme_Letrec {
  Scheme_Type type;
  short count;
  Scheme_Object **procs;
  Scheme_Object *body;
} Scheme_Letrec;

typedef struct {
  Scheme_Type type;
  short num_bindings;
  Scheme_Object *body;
} Scheme_Let_Frame_Data;

#define SCHEME_LOCAL_POS(obj)    (((Scheme_Local *)(obj))->position)

extern int scheme_active_but_sleeping;

typedef struct {
  Scheme_Type type;
  short count;
  Scheme_Object *array[1];
} Scheme_Sequence;

typedef struct {
  Scheme_Type type;
  short count;
  Scheme_Object *name;
  Scheme_Object *array[1];
} Scheme_Case_Lambda;

typedef struct Scheme_Promise {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  char forced;
  char is_expr;
  Scheme_Object *val;
  int multi_count;
  Scheme_Object **multi_array;
#ifdef MZ_REAL_THREADS
  Scheme_Object *sema;
#endif
} Scheme_Promise;

#define NOT_BLOCKED 0
#define PIPE_BLOCKED 1
#define PORT_BLOCKED 2
#define SEMA_BLOCKED 3
#define EVENTLOOP_BLOCKED 4
#define SLEEP_BLOCKED 10

#ifdef DO_STACK_CHECK
void scheme_init_stack_limit (void);
#endif

extern Scheme_Object *scheme_not_prim;
extern Scheme_Object *scheme_define_values_syntax, *scheme_defmacro_syntax;
extern Scheme_Object *scheme_def_id_macro_syntax;
extern Scheme_Object *scheme_def_exp_time_syntax;
extern Scheme_Object *scheme_lambda_syntax;
extern Scheme_Object *scheme_begin_syntax;

extern Scheme_Object *scheme_def_exit_proc;

Scheme_Object *scheme_named_map_1(char *, Scheme_Object *(*fun)(Scheme_Object*, Scheme_Object *form), Scheme_Object *lst, Scheme_Object *form);

extern Scheme_Object *scheme_orig_stdout_port;

int scheme_bignum_eq(const Scheme_Object *a, const Scheme_Object *b);
int scheme_bignum_lt(const Scheme_Object *a, const Scheme_Object *b);
int scheme_bignum_gt(const Scheme_Object *a, const Scheme_Object *b);
int scheme_bignum_le(const Scheme_Object *a, const Scheme_Object *b);
int scheme_bignum_ge(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_negate(const Scheme_Object *n);
Scheme_Object *scheme_bignum_add(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_subtract(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_add1(const Scheme_Object *n);
Scheme_Object *scheme_bignum_sub1(const Scheme_Object *n);
Scheme_Object *scheme_bignum_multiply(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_max(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_min(const Scheme_Object *a, const Scheme_Object *b);
void scheme_bignum_divide(const Scheme_Object *n, const Scheme_Object *d,
			  Scheme_Object **qp, Scheme_Object **rp, int norm);
Scheme_Object *scheme_bignum_power(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_integer_sqrt(const Scheme_Object *n);
Scheme_Object *scheme_bignum_and(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_or(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_xor(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_bignum_not(const Scheme_Object *a);
Scheme_Object *scheme_bignum_shift(const Scheme_Object *a, long shift);

Scheme_Object *scheme_make_small_rational(long n, Small_Rational *space);
Scheme_Object *scheme_integer_to_rational(const Scheme_Object *n);
Scheme_Object *scheme_make_fixnum_rational(long n, long d);
int scheme_rational_eq(const Scheme_Object *a, const Scheme_Object *b);
int scheme_rational_lt(const Scheme_Object *a, const Scheme_Object *b);
int scheme_rational_gt(const Scheme_Object *a, const Scheme_Object *b);
int scheme_rational_le(const Scheme_Object *a, const Scheme_Object *b);
int scheme_rational_ge(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_negate(const Scheme_Object *n);
Scheme_Object *scheme_rational_add(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_subtract(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_add1(const Scheme_Object *n);
Scheme_Object *scheme_rational_sub1(const Scheme_Object *n);
Scheme_Object *scheme_rational_multiply(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_max(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_min(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_rational_divide(const Scheme_Object *n, const Scheme_Object *d);
Scheme_Object *scheme_rational_power(const Scheme_Object *a, const Scheme_Object *b);
int scheme_is_rational_positive(const Scheme_Object *o);
Scheme_Object *scheme_rational_floor(const Scheme_Object *a);
Scheme_Object *scheme_rational_truncate(const Scheme_Object *a);
Scheme_Object *scheme_rational_ceiling(const Scheme_Object *a);
Scheme_Object *scheme_rational_round(const Scheme_Object *a);
Scheme_Object *scheme_rational_sqrt(const Scheme_Object *n);

Scheme_Object *scheme_make_small_complex(const Scheme_Object *n, Small_Complex *space);
Scheme_Object *scheme_real_to_complex(const Scheme_Object *n);
int scheme_complex_eq(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_complex_negate(const Scheme_Object *n);
Scheme_Object *scheme_complex_add(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_complex_subtract(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_complex_add1(const Scheme_Object *n);
Scheme_Object *scheme_complex_sub1(const Scheme_Object *n);
Scheme_Object *scheme_complex_multiply(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_complex_divide(const Scheme_Object *n, const Scheme_Object *d);
Scheme_Object *scheme_complex_power(const Scheme_Object *a, const Scheme_Object *b);
Scheme_Object *scheme_complex_sqrt(const Scheme_Object *a);
Scheme_Object *scheme_complex_real_part(const Scheme_Object *a);
Scheme_Object *scheme_complex_imaginary_part(const Scheme_Object *a);
int scheme_is_complex_exact(const Scheme_Object *o);

/* General numeric: */
Scheme_Object *scheme_bin_gcd(const Scheme_Object *n1, const Scheme_Object *n2);
Scheme_Object *scheme_bin_quotient(const Scheme_Object *n1, const Scheme_Object *n2);
Scheme_Object *scheme_bin_mult(const Scheme_Object *n1, const Scheme_Object *n2);
Scheme_Object *scheme_bin_div(const Scheme_Object *n1, const Scheme_Object *n2);
Scheme_Object *scheme_bin_plus(const Scheme_Object *n1, const Scheme_Object *n2);
Scheme_Object *scheme_bin_minus(const Scheme_Object *n1, const Scheme_Object *n2);
int scheme_bin_eq(const Scheme_Object *n1, const Scheme_Object *n2);
int scheme_bin_lt(const Scheme_Object *n1, const Scheme_Object *n2);
int scheme_bin_gt(const Scheme_Object *n1, const Scheme_Object *n2);
int scheme_bin_gt_eq(const Scheme_Object *n1, const Scheme_Object *n2);
int scheme_bin_lt_eq(const Scheme_Object *n1, const Scheme_Object *n2);

Scheme_Object *scheme_sub1(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_add1(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_odd_p(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_expt(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_modulo(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_sqrt(int argc, Scheme_Object *argv[]);
double scheme_get_val_as_double(const Scheme_Object *n);

Scheme_Object *scheme_double_to_integer(const char *where, double d);

Scheme_Object *scheme_generic_integer_power(const Scheme_Object *o, const Scheme_Object *p);

#define scheme_add_good_binding(i,v,f) (f->values[i] = v)

int scheme_equal_structs(Scheme_Object *obj1, Scheme_Object *obj2);

Scheme_Object *scheme_compiled_void(int can_be_value);

int scheme_find_type(Scheme_Object *ts);

#define scheme_save_env_stack_w_process(ss, p) \
    (ss.runstack = MZ_RUNSTACK, ss.runstack_start = MZ_RUNSTACK_START, \
     ss.cont_mark_stack = MZ_CONT_MARK_STACK, ss.cont_mark_pos = MZ_CONT_MARK_POS, \
     ss.runstack_size = p->runstack_size, ss.runstack_saved = p->runstack_saved)
#define scheme_restore_env_stack_w_process(ss, p) \
    (MZ_RUNSTACK = ss.runstack, MZ_RUNSTACK_START = ss.runstack_start, \
     MZ_CONT_MARK_STACK = ss.cont_mark_stack, MZ_CONT_MARK_POS = ss.cont_mark_pos, \
     p->runstack_size = ss.runstack_size, p->runstack_saved = ss.runstack_saved)
#define scheme_save_env_stack(ss) \
    scheme_save_env_stack_w_process(ss, scheme_current_process)
#define scheme_restore_env_stack(ss) \
    scheme_restore_env_stack_w_process(ss, scheme_current_process)

typedef struct Scheme_Overflow {
  MZTAG_IF_REQUIRED
  Scheme_Jumpup_Buf cont; /* continuation after value obtained in overflowed */
  struct Scheme_Overflow *prev; /* old overflow info */
  mz_jmp_buf savebuf; /* save old error buffer here */
} Scheme_Overflow;

void scheme_jmpup_free(Scheme_Jumpup_Buf *);

extern Scheme_Object *scheme_arity_at_least;

int scheme_check_double(const char *where, double v, const char *dest);
#ifdef MZ_USE_SINGLE_FLOATS
int scheme_check_float(const char *where, float v, const char *dest);
#else
# define scheme_check_float scheme_check_double
#endif

unsigned long scheme_get_deeper_address(void);

#if defined(UNIX_FIND_STACK_BOUNDS) || defined(WINDOWS_FIND_STACK_BOUNDS) \
    || defined(MACOS_FIND_STACK_BOUNDS) || defined(ASSUME_FIXED_STACK_SIZE) \
    || defined(BEOS_FIND_STACK_BOUNDS) || defined(OSKIT_FIXED_STACK_BOUNDS)
# ifndef MZ_REAL_THREADS
extern unsigned long scheme_stack_boundary;
# endif
#endif

#ifdef MEMORY_COUNTING_ON
extern Scheme_Hash_Table *scheme_symbol_table;
extern long scheme_type_table_count;
extern long scheme_misc_count;

Scheme_Object *scheme_dump_memory_count(int c, Scheme_Object *a[]);

long scheme_count_closure(Scheme_Object **o, short len, Scheme_Hash_Table *ht);

long scheme_count_envbox(Scheme_Object *root, Scheme_Hash_Table *ht);
long scheme_count_memory(Scheme_Object *root, Scheme_Hash_Table *ht);
void scheme_count_input_port(Scheme_Object *port, long *s, long *e, Scheme_Hash_Table *ht);
void scheme_count_output_port(Scheme_Object *port, long *s, long *e, Scheme_Hash_Table *ht);

void scheme_count_struct_info(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);

#ifndef NO_OBJECT_SYSTEM
void scheme_count_object(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);
void scheme_count_class(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);
void scheme_count_class_data(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);
void scheme_count_generic(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);
#endif
void scheme_count_unit(Scheme_Type type, Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht);
#endif

void scheme_init_error_escape_proc(Scheme_Process *p);

int scheme_used_app_only(Scheme_Comp_Env *env, int which);
int scheme_used_ever(Scheme_Comp_Env *env, int which);
char *scheme_expand_filename(char* filename, int ilen, char *errorin, int *ex);

int scheme_omittable_expr(Scheme_Object *o);

#ifndef MZ_REAL_THREADS
Scheme_Object *scheme_internal_read(Scheme_Object *port, int crc, Scheme_Config *);
#else
Scheme_Object *scheme_internal_read(Scheme_Object *port, int crc, Scheme_Config *, Scheme_Process *p);
#endif
void scheme_internal_display(Scheme_Object *obj, Scheme_Object *port, Scheme_Config *);
void scheme_internal_write(Scheme_Object *obj, Scheme_Object *port, Scheme_Config *);

Scheme_Object *scheme_make_arity(short minc, short maxc);
Scheme_Object *scheme_arity(Scheme_Object *p);

Scheme_Object *scheme_apply_macro_to_list(Scheme_Object *f, Scheme_Object *argss,
					  Scheme_Object *code);

void scheme_wrong_syntax(const char *where, 
			 Scheme_Object *local_form, 
			 Scheme_Object *form, 
			 const char *detail, ...);
void scheme_wrong_rator(Scheme_Object *rator, int argc, Scheme_Object **argv);

void scheme_raise_else(const char *where, Scheme_Object *v);
void scheme_raise_out_of_memory(const char *where, const char *msg, ...);

void scheme_raise(Scheme_Object *exn);

extern long scheme_max_found_symbol_name;

Scheme_Object *scheme_compile_expand_macro_app(Scheme_Object *macro,
					       Scheme_Object *form, Scheme_Comp_Env *env,
					       Scheme_Compile_Info *rec, int depth);

#define _scheme_make_char(ch) (scheme_char_constants[(unsigned char)(ch)])

#ifdef TIME_SYNTAX
extern Scheme_Object *scheme_date;
#endif

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Object **scheck_hash;
  long scheck_size, scheck_count, scheck_step;
} DupCheckRecord;

DupCheckRecord *scheme_begin_dup_symbol_check(DupCheckRecord *r);
void scheme_dup_symbol_check(DupCheckRecord *r, const char *where,
			     Scheme_Object *symbol, char *what, 
			     Scheme_Object *form, int inverted);

void scheme_unsettable_variable(Scheme_Comp_Env *env, int which);

Scheme_Object *scheme_hash_percent_name(const char *name, int len);

Scheme_Object *scheme_make_branch(Scheme_Object *test,
				  Scheme_Object *tbranch,
				  Scheme_Object *fbranch);

#ifdef MZ_REAL_THREADS
#define MZTHREADELEM(p, x) p->x
#else
#define MZTHREADELEM(p, x) scheme_ ## x
#endif

extern Scheme_Process *scheme_main_process;


#ifndef MZ_REAL_THREADS
# define SCHEME_GET_LOCK() /* empty */
# define SCHEME_RELEASE_LOCK() /* empty */
#else
# define SCHEME_SEMA_DOWN(sema) scheme_real_sema_down(sema)
void scheme_real_sema_down(void *sema);

/* #define MZ_KEEP_LOCK_INFO */

extern void *scheme_global_lock;
# ifdef MZ_KEEP_LOCK_INFO
extern int scheme_global_lock_c;
#  define _MZ_LOCK_INFO(x) , x
#  define MZ_LOCK_INFO_(x) x,
# else
#  define _MZ_LOCK_INFO(x) /**/
#  define MZ_LOCK_INFO_(x) /**/
# endif

# define SCHEME_GET_LOCK() (SCHEME_LOCK_MUTEX(scheme_global_lock) _MZ_LOCK_INFO(scheme_global_lock_c++))
# define SCHEME_RELEASE_LOCK()  (MZ_LOCK_INFO_(--scheme_global_lock_c) SCHEME_UNLOCK_MUTEX(scheme_global_lock))
#endif

#ifdef MUST_REGISTER_GLOBALS
void scheme_register_bignum();

#define REGISTER_SO(x)  scheme_register_extension_global((void *)&x, sizeof(x));
#else
#define REGISTER_SO(x) 
#endif

int scheme_is_regular_file(char *filename);

extern int scheme_builtin_ref_counter;

Scheme_Object **scheme_make_builtin_references_table(void);
Scheme_Object *scheme_make_local(Scheme_Type type, int pos);

#define MAX_CONST_LOCAL_POS 20

extern Scheme_Object *scheme_local[MAX_CONST_LOCAL_POS][2];

#define MAKE_CLOSED_PRIM(f,v,n,mi,ma) \
  scheme_make_closed_prim_w_arity((Scheme_Closed_Prim *)f, (void *)v, n, mi, ma)

#define _MALLOC_N(x, n, malloc) ((x*)malloc(sizeof(x)*(n)))
#define MALLOC_ONE(x) _MALLOC_N(x, 1, scheme_malloc)
#define MALLOC_ONE_TAGGED(x) _MALLOC_N(x, 1, scheme_malloc_tagged)
#define MALLOC_N_TAGGED(x, n) _MALLOC_N(x, n, scheme_malloc_array_tagged)
#ifdef MZTAG_REQUIRED
# define scheme_malloc_rt(x) scheme_malloc_tagged(x)
# define MALLOC_ONE_RT(x) MALLOC_ONE_TAGGED(x)
# define MALLOC_N_RT(x,c) MALLOC_N_TAGGED(x,c)
# define MALLOC_ONE_WEAK(x) _MALLOC_N(x, 1, scheme_malloc)
# define MALLOC_N_WEAK(x,c) _MALLOC_N(x, c, scheme_malloc)
# define MALLOC_ONE_TAGGED_WEAK(x) _MALLOC_N(x, 1, scheme_malloc_tagged)
# define MALLOC_ONE_WEAK_RT(x) MALLOC_ONE_TAGGED_WEAK(x)
#else
# define scheme_malloc_rt(x) scheme_malloc(x)
# define MALLOC_ONE_RT(x) MALLOC_ONE(x)
# define MALLOC_N_RT(x,c) MALLOC_N(x,c)
# define MALLOC_ONE_WEAK(x) MALLOC_ONE_ATOMIC(x)
# define MALLOC_N_WEAK(x,c) MALLOC_N_ATOMIC(x,c)
# define MALLOC_ONE_WEAK_RT(x) MALLOC_ONE_WEAK(x)
# define MALLOC_ONE_TAGGED_WEAK(x) MALLOC_ONE_WEAK(x)
#endif
#define MALLOC_N(x, n) _MALLOC_N(x, n, scheme_malloc)
#define MALLOC_ONE_ATOMIC(x) _MALLOC_N(x, 1, scheme_malloc_atomic)
#define MALLOC_N_ATOMIC(x, n) _MALLOC_N(x, n, scheme_malloc_atomic)
#define MALLOC_SO_BOX() _MALLOC_ONE(Scheme_Object*, scheme_malloc)
#define MALLOC_N_STUBBORN(x, n) _MALLOC_N(x, n, scheme_malloc_stubborn)

#define CONFIG_DEFAULTING(c, f) (c->base && (c->base->f == c->f))

extern int scheme_no_reference_id;

#ifdef MAC_FILE_SYSTEM
void scheme_file_create_hook(char *filename);
#endif

int scheme_bignum_get_int_val(const Scheme_Object *o, long *v);
int scheme_bignum_get_unsigned_int_val(const Scheme_Object *o, unsigned long *v);

Scheme_Object *scheme_check_immediate_macro(Scheme_Object *first, 
					    Scheme_Comp_Env *env, 
					    Scheme_Compile_Info *rec, 
					    int depth,
					    Scheme_Object **current_val);

#ifdef NO_TCP_SUPPORT
# undef USE_UNIX_SOCKETS_TCP
# undef USE_WINSOCK_TCP
# undef USE_MAC_TCP
#endif
#if defined(USE_UNIX_SOCKETS_TCP) || defined(USE_WINSOCK_TCP) || defined(USE_MAC_TCP)
# define USE_TCP
#endif

#if !defined(USE_IEEE_FP_PREDS) && !defined(USE_SCO_IEEE_PREDS)
extern double scheme_infinity_val, scheme_minus_infinity_val;
# define MZ_IS_POS_INFINITY(d) ((d) == scheme_infinity_val)
# define MZ_IS_NEG_INFINITY(d) ((d) == scheme_minus_infinity_val)
# ifdef NAN_EQUALS_ANYTHING
#  define MZ_IS_NAN(d) (((d) == 1.0) && ((d) == 2.0))
# else
#  ifdef DEFEAT_FP_COMP_OPTIMIZATION
extern int scheme_both_nan(double a, double b);
#   define MZ_IS_NAN(d) (scheme_both_nan(d, d))
#  else
#   define MZ_IS_NAN(d) (!((d) == (d)))
#  endif
# endif
#else
# ifdef USE_SCO_IEEE_PREDS
#  include <ieeefp.h>
#  define MZ_IS_POS_INFINITY(d) (fpclass(d) == FP_PINF)
#  define MZ_IS_NEG_INFINITY(d) (fpclass(d) == FP_NINF)
#  define MZ_IS_NAN(d) isnan(d)
# else
#  define MZ_IS_POS_INFINITY(d) (isinf(d) && (d > 0))
#  define MZ_IS_NEG_INFINITY(d) (isinf(d) && (d < 0))
#  define MZ_IS_NAN(d) isnan(d)
# endif
#endif

double scheme_bignum_to_double_inf_info(const Scheme_Object *n, int just_use, int *only_need);
#ifdef MZ_USE_SINGLE_FLOATS
float scheme_bignum_to_float_inf_info(const Scheme_Object *n, int just_use, int *only_need);
#else
# define scheme_bignum_to_float_inf_info scheme_bignum_to_double_inf_info
#endif

void scheme_zero_unneeded_rands(Scheme_Process *p);

#define IMPROPER_LIST_FORM "illegal use of `.'"

extern long scheme_total_gc_time;

Scheme_Object *
scheme_make_prim_w_everything(Scheme_Prim *fun, int eternal,
			      const char *name,
			      short mina, short maxa,
			      short folding,
			      short minr, short maxr);
Scheme_Object *
scheme_make_closed_prim_w_everything(Scheme_Closed_Prim *fun, 
				     void *data,
				     const char *name, 
				     short mina, short maxa,
				     short folding,
				     short minr, short maxr);

#define scheme_make_prim_w_arity2(f, n, mina, maxa, minr, maxr) \
  scheme_make_prim_w_everything(f, 0, n, mina, maxa, 0, minr, maxr)

int scheme_can_break(Scheme_Process *p, Scheme_Config *config);

int scheme_is_env_variable_boxed(Scheme_Comp_Env *env, int which);

Scheme_Object *scheme_bangboxenv_execute(Scheme_Object *data);


void scheme_check_identifier(const char *formname, Scheme_Object *id, 
			     const char *where,
			     Scheme_Comp_Env *env,
			     Scheme_Object *form);


Scheme_Comp_Env *scheme_new_compilation_frame(int num_bindings, int flags, Scheme_Comp_Env *env);
void scheme_add_compilation_binding(int index, Scheme_Object *val, 
				    Scheme_Comp_Env *frame);
Scheme_Comp_Env *scheme_add_compilation_frame(Scheme_Object *vals, 
					 Scheme_Comp_Env *env, int flags);

Scheme_Object *scheme_static_distance(Scheme_Object *symbol, Scheme_Comp_Env *env,
				      int flags);

void scheme_push_constant(Scheme_Object *name, Scheme_Object *val,
			  Scheme_Comp_Env *env);
void scheme_pop_constant(Scheme_Comp_Env *env);

void scheme_env_make_closure_map(Scheme_Comp_Env *frame, short *size, short **map);

Scheme_Object *scheme_make_linked_closure(Scheme_Process *p, 
					  Scheme_Object *compiled_code,
					  int close);

/* Linking */
void scheme_register_syntax(const char *name, Scheme_Syntax_Registered *f);

Scheme_Object *scheme_make_syntax_link(Scheme_Syntax_Executer *prim,
				       Scheme_Object *data);
Scheme_Object *scheme_make_syntax_compile(Scheme_Syntax_Linker *prim,
					  Scheme_Object *data);

Scheme_Object *scheme_link_expr(Scheme_Object *, Link_Info *);
Scheme_Object *scheme_link_list(Scheme_Object *, Link_Info *);

int scheme_is_compiled_procedure(Scheme_Object *o, int can_be_closed);

Scheme_Object *scheme_link_lets(Scheme_Object *form, Link_Info *info);

Link_Info *scheme_link_info_create(int can_opt);
Link_Info *scheme_link_info_extend(Link_Info *info, int size, int oldsize, int mapcount);
void scheme_link_info_add_mapping(Link_Info *info, int oldp, int newp, int flags);
void scheme_link_info_set_anchor_offset(Link_Info *info, int offset);
int scheme_link_info_flags(Link_Info *info, int pos);
int scheme_link_info_lookup(Link_Info *link, int pos, int *flags);
int scheme_link_info_lookup_anchor(Link_Info *info, int pos);

Scheme_Object *scheme_make_compiled_syntax(Scheme_Syntax *syntax,
					   Scheme_Syntax_Expander *exp);

Scheme_Object *scheme_compile_expr(Scheme_Object *form, Scheme_Comp_Env *env,
				   Scheme_Compile_Info *rec);
Scheme_Object *scheme_compile_sequence(Scheme_Object *forms, Scheme_Comp_Env *env,
			      Scheme_Compile_Info *rec);
Scheme_Object *scheme_compile_block(Scheme_Object *forms, Scheme_Comp_Env *env,
			      Scheme_Compile_Info *rec);
Scheme_Object *scheme_compile_list(Scheme_Object *form, Scheme_Comp_Env *env,
			      Scheme_Compile_Info *rec);

void scheme_default_compile_rec(Scheme_Compile_Info *src);
void scheme_compile_rec_done_local(Scheme_Compile_Info *src);
void scheme_init_compile_recs(Scheme_Compile_Info *src, 
			      Scheme_Compile_Info *dest, int n);
void scheme_merge_compile_recs(Scheme_Compile_Info *src, 
			       Scheme_Compile_Info *dest, int n);
void scheme_init_lambda_rec(Scheme_Compile_Info *src,
			    Scheme_Compile_Info *lam);
void scheme_merge_lambda_rec(Scheme_Compile_Info *src,
			    Scheme_Compile_Info *lam);


Scheme_Object *scheme_make_closure_compilation(Scheme_Comp_Env *env,
					       Scheme_Object *uncompiled_code,
					       Scheme_Compile_Info *rec);
Scheme_Object *scheme_make_sequence_compilation(Scheme_Object *compiled_list,
						int to_linked, int strip_values);


Scheme_Object *scheme_link_closure_compilation(Scheme_Object *_data, Link_Info *info);

#define SCHEME_SYNTAX(obj)   ((obj)->u.two_ptr_val.ptr1)
#define SCHEME_SYNTAX_EXP(obj)   ((obj)->u.two_ptr_val.ptr2)

int *scheme_env_get_flags(Scheme_Comp_Env *frame, int start, int count);


/* flags reported by scheme_env_get_fags */
#define SCHEME_WAS_USED 1
#define SCHEME_WAS_SET_BANGED 2

/* flags reported by scheme_link_info_flags */
#define SCHEME_INFO_BOXED 1
#define SCHEME_INFO_ANCHORED 2

/* flags used with scheme_new_frame */
#define SCHEME_AUTO_UNBOX 1
#define SCHEME_COMPILE_PTR 2
#define SCHEME_LAMBDA_FRAME 8
#define SCHEME_LET_FRAME 16
#define SCHEME_ANCHORED_FRAME 32
#define SCHEME_TOPLEVEL_FRAME 64
#define SCHEME_PRIM_GLOBALS_ONLY 128

#define ENV_PRIM_GLOBALS_ONLY(env) ((env)->flags & SCHEME_PRIM_GLOBALS_ONLY)

/* Flags used with scheme_static_distance */
#define SCHEME_ELIM_CONST 1
#define SCHEME_APP_POS 2
#define SCHEME_SETTING 4
#define SCHEME_ENV_CONSTANTS_OK 8
#define SCHEME_GLOB_ALWAYS_REFERENCE 16
#define SCHEME_MUST_INDRECT 32
#define SCHEME_LINKING_REF 64
#define SCHEME_DONT_MARK_USE 128


Scheme_Env *scheme_min_env(Scheme_Comp_Env *);

void *scheme_enlarge_runstack(long size, void *(*k)());
int scheme_check_runstack(long size);

Scheme_Object *scheme_expand_expr(Scheme_Object *form, Scheme_Comp_Env *env,
				  int depth);
Scheme_Object *scheme_expand_list(Scheme_Object *form, Scheme_Comp_Env *env,
				  int depth);
Scheme_Object *scheme_expand_block(Scheme_Object *form, Scheme_Comp_Env *env,
				   int depth);

Scheme_Object *scheme_make_svector(short v, short *a);

#define SCHEME_SVEC_LEN(obj) ((obj)->u.svector_val.len)
#define SCHEME_SVEC_VEC(obj) ((obj)->u.svector_val.vec)

Scheme_Object *scheme_dump_gc_stats(int c, Scheme_Object *p[]);

#define _scheme_eval_compiled_expr(obj) scheme_do_eval(obj,-1,NULL,1)
#define _scheme_eval_compiled_expr_multi(obj) scheme_do_eval(obj,-1,NULL,-1)
#define _scheme_eval_compiled_expr_wp(obj, p) scheme_do_eval_w_process(obj,-1,NULL,1,p)
#define _scheme_eval_compiled_expr_multi_wp(obj, p) scheme_do_eval_w_process(obj,-1,NULL,-1,p)

Scheme_Object *scheme_eval_compiled_expr(Scheme_Object *obj);

char *scheme_make_args_string(char *s, int which, int argc, Scheme_Object **argv);

Scheme_Object **scheme_make_struct_names_from_array(const char *base, 
						    int fcount,
						    const char **field_names,
						    int flags, int *count_out);
Scheme_Object *scheme_make_struct_type_from_string(const char *base, 
						   Scheme_Object *parent, 
						   int num_fields);
extern Scheme_Object *scheme_write_proc, *scheme_display_proc, *scheme_print_proc;

int scheme_num_types(void);

void scheme_alloc_list_stack(Scheme_Process *process);

int scheme_get_eval_type(Scheme_Object *obj);

Scheme_Comp_Env *scheme_no_defines(Scheme_Comp_Env *env);
int scheme_count_sema_callbacks(int kind);

int scheme_is_toplevel(Scheme_Comp_Env *env);
Scheme_Comp_Env *scheme_extend_as_toplevel(Scheme_Comp_Env *env);

int scheme_is_relative_path(const char *s, long len);
int scheme_is_complete_path(const char *s, long len);

void scheme_do_format(const char *procname, Scheme_Object *port, 
		      const unsigned char *format, int flen, 
		      int fpos, int offset, int argc, Scheme_Object **argv);

char *scheme_make_arity_expect_string(Scheme_Object *proc,
				      int argc, Scheme_Object **argv);

#ifdef TIME_TYPE_IS_UNSIGNED
#define scheme_make_integer_value_from_time(t) scheme_make_integer_value_from_unsigned((unsigned long)t)
#else
#define scheme_make_integer_value_from_time(t) scheme_make_integer_value((long)t)
#endif

long scheme_extract_index(const char *name, int pos, int argc, Scheme_Object **argv, long top);
void scheme_out_of_string_range(const char *name, const char *which, 
				Scheme_Object *i, Scheme_Object *s, 
				long start, long len);

Scheme_Object *scheme_get_file_directory(const char *filename);

char *scheme_normal_path_case(char *s, int len);

#ifdef WIN32_THREADS
void *scheme_win32_get_break_semaphore(void *th);
#endif

Scheme_Env *scheme_make_empty_env(void);

const char *scheme_number_suffix(int);

Scheme_Object *
scheme_get_primitive_global(Scheme_Object *var, Scheme_Env *env, 
			    int bucket_ok, int can_opt, int signal);

void scheme_add_bucket_to_table(Scheme_Hash_Table *table, Scheme_Bucket *b);
Scheme_Bucket *scheme_bucket_or_null_from_table (Scheme_Hash_Table *table, const char *key, int add);

Scheme_Object *scheme_load_with_clrd(int argc, Scheme_Object *argv[], char *who, int handler_param);

int scheme_string_has_null(Scheme_Object *o);
#define STRING_W_NO_NULLS "string (with no null characters)"

#ifdef MACINTOSH_EVENTS
int scheme_mac_start_app(char *name, int find_path, Scheme_Object *s);
int scheme_mac_send_event(char *name, int argc, Scheme_Object **argv, Scheme_Object **result, OSErr *err, char **stage);
#endif

Scheme_Object *scheme_do_exit(int argc, Scheme_Object *argv[]);

void scheme_init_setjumpup(void);

void scheme_get_substring_indices(const char *name, Scheme_Object *str, 
				  int argc, Scheme_Object **argv, 
				  int spos, int fpos, long *_start, long *_finish);

Scheme_Object *scheme_make_random_state(long seed);

void scheme_copy_from_original_env(Scheme_Env *env);

extern int scheme_internal_checking_char;

#define MZTHREAD_RUNNING 0x1
#define MZTHREAD_SUSPENDED 0x2
#define MZTHREAD_KILLED 0x4
#define MZTHREAD_NEED_KILL_CLEANUP 0x8
#define MZTHREAD_STILL_RUNNING(running) ((running) && !((running) & MZTHREAD_KILLED))

#ifdef MZ_PRECISE_GC
long scheme_hash_key(Scheme_Object *o);
#else
# define scheme_hash_key(o) ((long)(o))
#endif

#ifdef WINDOWS_PROCESSES
struct Scheme_Thread_Memory *scheme_remember_thread(void *);
void scheme_remember_subthread(struct Scheme_Thread_Memory *, void *);
void scheme_forget_thread(struct Scheme_Thread_Memory *);
void scheme_forget_subthread(struct Scheme_Thread_Memory *);
void scheme_suspend_remembered_threads(void);
void scheme_resume_remembered_threads(void);
#endif

Scheme_Object *scheme_call_ec(int argc, Scheme_Object *argv[]);

#define	MZ_RANDOM_STATE_DEG 31
typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  short fpos, rpos;
  long state[MZ_RANDOM_STATE_DEG];
} Scheme_Random_State;

# define BEGIN_ESCAPEABLE(onbreak) \
    { mz_jmp_buf savebuf; \
      memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf)); \
      if (scheme_setjmp(scheme_error_buf)) { \
        onbreak; \
        scheme_longjmp(savebuf, 1); \
      } else {
# define END_ESCAPEABLE() \
      memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf)); } }

void scheme_reset_prepared_error_buffer(void);

#ifdef MZ_PRECISE_GC
# define WEAKIFY(x) scheme_make_weak_box(x)
# define WEAKIFIED(x) SCHEME_WEAK_BOX_VAL(x)
# define HT_EXTRACT_WEAK(x) SCHEME_WEAK_BOX_VAL(x)
#else
# define WEAKIFY(x) x
# define WEAKIFIED(x) x
# define HT_EXTRACT_WEAK(x) (*(char **)(x))
#endif

#ifndef MZ_PRECISE_GC
# define START_XFORM_SKIP /**/
# define END_XFORM_SKIP /**/
#endif

int scheme_strncmp(const char *a, const char *b, int len);

#endif /* __mzscheme_private__ */
