/*
  MzScheme
  Copyright (c) 1995 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#ifndef SCHEME_H
#define SCHEME_H

/* The next line is used and set during installation: */
/*III*/

#ifdef INCLUDE_WITHOUT_PATHS
# include "sconfig.h"
#else
# include "../sconfig.h"
#endif

#define AGRESSIVE_ZERO_FOR_GC
#define AGRESSIVE_ZERO_TB

#if SGC_STD_DEBUGGING
# ifndef USE_SENORA_GC
#  define USE_SENORA_GC
# endif
# define USE_MEMORY_TRACING 
#endif

#ifdef USE_SENORA_GC
# define MUST_REGISTER_GLOBALS
# undef UNIX_IMAGE_DUMPS
#endif

#ifdef USE_SINGLE_FLOATS
# define MZ_USE_SINGLE_FLOATS
#endif

#include <stdio.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#ifndef SCHEME_DIRECT_EMBEDDED
#define SCHEME_DIRECT_EMBEDDED 1
#endif

#ifndef MSC_IZE
# define MSC_IZE(x) x
#endif

#ifdef SIGSET_IS_SIGNAL
# define MZ_SIGSET(s, f) signal(s, f)
#else
# define MZ_SIGSET(s, f) sigset(s, f)
#endif

#ifdef __cplusplus
extern "C" 
{
#endif

typedef short Scheme_Type;

typedef struct Scheme_Bucket
{
  Scheme_Type type;
  void *val;
  char *key;
} Scheme_Bucket;

typedef struct Scheme_Hash_Table
{
  Scheme_Type type;
  int size, count, step;
  Scheme_Bucket **buckets;
  char has_constants, forever, weak;
  void (*make_hash_indices)(void *v, int *h1, int *h2);
  int (*compare)(void *v1, void *v2);
#ifdef MZ_REAL_THREADS
  void *mutex;
#endif
} Scheme_Hash_Table;

/* Hash tablekey types, used with scheme_hash_table */
enum {
  SCHEME_hash_string,
  SCHEME_hash_ptr,
  SCHEME_hash_weak_ptr
};

typedef struct Scheme_Env
{
  Scheme_Type type; /* scheme_namespace_type */
  Scheme_Hash_Table *globals;
  Scheme_Hash_Table *loaded_libraries;
  struct Scheme_Object *nonempty_cond; /* hack used when !scheme_allow_cond_auto_else */
  struct Scheme_Comp_Env *init; /* initial compilation environment */
} Scheme_Env;

typedef struct Scheme_Object *
(Scheme_Prim)(int argc, struct Scheme_Object *argv[]);

typedef struct Scheme_Object *
(Scheme_Closed_Prim)(void *d, int argc, struct Scheme_Object *argv[]);

typedef struct Scheme_Object *
(Scheme_Method_Prim)(struct Scheme_Object *o, 
		     int argc, struct Scheme_Object *argv[]);

typedef struct Scheme_Object
{
  Scheme_Type type; /* Anything that starts with a type field
		       can be a Scheme_Object */
  union
    {
      struct { char *string_val; int tag_val; } str_val;
      struct { void *ptr1, *ptr2; } two_ptr_val;
      struct { int int1; int int2; } two_int_val;
      struct { void *ptr; int pint; } ptr_int_val;
      struct { void *ptr; long pint; } ptr_long_val;
      struct {
	Scheme_Closed_Prim *f;
	void *d; } clsd_prim_val;
      struct { struct Scheme_Object *car, *cdr; } pair_val;
      struct { struct Scheme_Env *env; struct Scheme_Object *code; } closure_val;
      struct { short len; short *vec; } svector_val;
      struct Scheme_Debugging_Info *debug_val;
    } u;
} Scheme_Object;

typedef struct {
  Scheme_Type type;
  union {
    char char_val;
    Scheme_Object *ptr_value;
    long int_val;
    Scheme_Object *ptr_val;
    Scheme_Prim *prim_val;
  } u;
} Scheme_Small_Object;  

typedef struct {
  Scheme_Type type;
  double double_val;
} Scheme_Double;

#ifdef MZ_USE_SINGLE_FLOATS
typedef struct {
  Scheme_Type type;
  float float_val;
} Scheme_Float;
#endif

#define SCHEME_PRIM_IS_FOLDING 1
#define SCHEME_PRIM_IS_PRIMITIVE 2
#define SCHEME_PRIM_IS_STRUCT_PROC 4
#define SCHEME_PRIM_IS_STRUCT_SETTER 8
#define SCHEME_PRIM_IS_PARAMETER 16
#define SCHEME_PRIM_IS_STRUCT_GETTER 32
#define SCHEME_PRIM_IS_STRUCT_PRED 64
#define SCHEME_PRIM_IS_STRUCT_CONSTR 128
#define SCHEME_PRIM_IS_MULTI_RESULT 256
#define SCHEME_PRIM_IS_GENERIC 512
#define SCHEME_PRIM_IS_USER_PARAMETER 1024

typedef struct {
  Scheme_Type type;
  short flags; /* keep flags at same place as in closed */
  Scheme_Prim *prim_val;
  const char *name;
  short mina, maxa;
} Scheme_Primitive_Proc;

typedef struct {
  Scheme_Primitive_Proc p;
  short minr, maxr;
} Scheme_Prim_W_Result_Arity;

typedef struct {
  Scheme_Type type;
  short flags; /* keep flags at same place as in unclosed */
  Scheme_Closed_Prim *prim_val;
  void *data;
  const char *name;
  short mina, maxa; /* mina == -2 => maxa is negated case count and
		       record is a Scheme_Closed_Case_Primitive_Proc */
} Scheme_Closed_Primitive_Proc;

typedef struct {
  Scheme_Closed_Primitive_Proc p;
  short minr, maxr;
} Scheme_Closed_Prim_W_Result_Arity;

typedef struct {
  Scheme_Closed_Primitive_Proc p;
  short *cases;
} Scheme_Closed_Case_Primitive_Proc;

#define _scheme_fill_prim_closure(rec, cfunc, dt, nm, amin, amax) \
  ((rec)->type = scheme_closed_prim_type, \
   (rec)->prim_val = cfunc, \
   (rec)->data = (void *)(dt), \
   (rec)->name = nm, \
   (rec)->mina = amin, \
   (rec)->maxa = amax, \
   rec)
   
#define _scheme_fill_prim_case_closure(rec, cfunc, dt, nm, ccount, cses) \
  ((rec)->p.type = scheme_closed_prim_type, \
   (rec)->p.prim_val = cfunc, \
   (rec)->p.data = (void *)(dt), \
   (rec)->p.name = nm, \
   (rec)->p.mina = -2, \
   (rec)->p.maxa = -(ccount), \
   (rec)->cases = cses, \
   rec)

typedef struct Scheme_Debugging_Info {
  Scheme_Object *src;
} Scheme_Debugging_Info;

typedef struct Scheme_Sema {
  Scheme_Type type;
#ifdef MZ_REAL_THREADS
  void *sema;
#else
  long value;  
#endif
} Scheme_Sema;

typedef struct Scheme_Symbol {
  Scheme_Type type;
  short len;
  char s[1];
} Scheme_Symbol;

typedef struct Scheme_Vector {
  Scheme_Type type;
  int size;
  Scheme_Object *els[1];
} Scheme_Vector;

typedef void Scheme_Close_Manager_Client(Scheme_Object *o, void *data);
typedef struct Scheme_Manager *Scheme_Manager_Reference;

typedef struct Scheme_Manager {
  Scheme_Type type;
  short count, alloc;
  Scheme_Object ***boxes;
  Scheme_Manager_Reference **mrefs;
  Scheme_Close_Manager_Client **closers;
  void **data;

  /* atomic indirections: */
  struct Scheme_Manager **parent;
  struct Scheme_Manager **sibling;
  struct Scheme_Manager **children;
} Scheme_Manager;

typedef struct Scheme_Input_Port
{
  Scheme_Type type;
  short closed;
  Scheme_Object *sub_type;
  Scheme_Manager_Reference *mref;
  void *port_data;
  int (*getc_fun) (struct Scheme_Input_Port *port);
  int (*char_ready_fun) (struct Scheme_Input_Port *port);
  void (*close_fun) (struct Scheme_Input_Port *port);
  void (*need_wakeup_fun)(struct Scheme_Input_Port *, void *);
  Scheme_Object *read_handler;
  char *name;
  char *ungotten;
  int ungotten_count, ungotten_allocated;
  long position, lineNumber, charsSinceNewline;
  int eoffound;
#ifdef MZ_REAL_THREADS
  Scheme_Object *sema;
#endif
} Scheme_Input_Port;

typedef struct Scheme_Output_Port
{
  Scheme_Type type;
  short closed;
  Scheme_Object *sub_type;
  Scheme_Manager_Reference *mref;
  void *port_data;
  void (*write_string_fun)(char *str, long len, struct Scheme_Output_Port *);
  void (*close_fun) (struct Scheme_Output_Port *);
  long pos;
  Scheme_Object *display_handler;
  Scheme_Object *write_handler;
  Scheme_Object *print_handler;
#ifdef MZ_REAL_THREADS
  Scheme_Object *sema;
#endif
} Scheme_Output_Port;

typedef struct {
  Scheme_Type type;
  struct Scheme_Object *sclass;
  /* The following fields are only here for instances of classes
     created with scheme_make_class(): */
  void *primdata;
  short primflag;
  short inited;
} Scheme_Class_Object;

typedef struct Scheme_Unit {
  Scheme_Type type;        /* scheme_unit_type */
  short num_imports;       /* num expected import args */
  short num_exports;       /* num exported vars */
  Scheme_Object **exports; /* names of exported */
  Scheme_Object **export_debug_names; /* internal names; NULL => no debugging */
  Scheme_Object *(*init_func)(Scheme_Object **boxes, Scheme_Object **anchors,
			      struct Scheme_Unit *m,
			      void *debug_request);
  Scheme_Object *data;
} Scheme_Unit;

typedef void Scheme_Instance_Init_Proc(Scheme_Object **init_boxes,
				       Scheme_Object **extract_boxes,
				       Scheme_Object *super_init,
				       int argc,
				       Scheme_Object **argv,
				       Scheme_Object *instance,
				       void *data);

/* Like setjmp & longjmp, but you can jmp to a deeper stack position */
/* Intialize a Scheme_Jumpup_Buf record before using it */
typedef struct Scheme_Jumpup_Buf {
  void *stack_from, *stack_copy;
  long stack_size, stack_max_size;
  struct Scheme_Jumpup_Buf *cont;
  jmp_buf buf;
} Scheme_Jumpup_Buf;

enum {
  MZCONFIG_ENV,
  MZCONFIG_INPUT_PORT,
  MZCONFIG_OUTPUT_PORT,
  MZCONFIG_ERROR_PORT,

  MZCONFIG_USER_BREAK_POLL_HANDLER,
  MZCONFIG_ENABLE_BREAK,
  MZCONFIG_ENABLE_EXCEPTION_BREAK,

  MZCONFIG_ERROR_DISPLAY_HANDLER,
  MZCONFIG_ERROR_PRINT_VALUE_HANDLER,

  MZCONFIG_EXIT_HANDLER,

  MZCONFIG_EXN_HANDLER,
  MZCONFIG_DEBUG_INFO_HANDLER,

  MZCONFIG_EVAL_HANDLER,
  MZCONFIG_LOAD_HANDLER,

  MZCONFIG_PRINT_HANDLER,
  MZCONFIG_PROMPT_READ_HANDLER,

  MZCONFIG_CAN_READ_GRAPH,
  MZCONFIG_CAN_READ_COMPILED,
  MZCONFIG_CAN_READ_BOX,
  MZCONFIG_CAN_READ_TYPE_SYMBOL,
  MZCONFIG_CAN_READ_PIPE_QUOTE,

  MZCONFIG_PRINT_GRAPH,
  MZCONFIG_PRINT_STRUCT,
  MZCONFIG_PRINT_BOX,

  MZCONFIG_CASE_SENS,
  MZCONFIG_SQUARE_BRACKETS_ARE_PARENS,
  MZCONFIG_CURLY_BRACES_ARE_PARENS,

  MZCONFIG_ERROR_PRINT_WIDTH,

  MZCONFIG_CONFIG_BRANCH_HANDLER,

  MZCONFIG_WILL_EXECUTOR,

  MZCONFIG_ALLOW_SET_UNDEFINED,
  MZCONFIG_COND_AUTO_ELSE,

  MZCONFIG_MANAGER,

  MZCONFIG_REQ_LIB_USE_COMPILED,

  MZCONFIG_LOAD_DIRECTORY,

  MZCONFIG_COLLECTION_PATHS,

  MZCONFIG_PORT_PRINT_HANDLER,

  MZCONFIG_REQUIRE_COLLECTION,

  MZCONFIG_LOAD_EXTENSION_HANDLER,

  __MZCONFIG_BUILTIN_COUNT__
};


typedef struct Scheme_Config {
  Scheme_Type type;
  Scheme_Hash_Table *extensions;

  /* For sharing as-yet uncreated parameters: */
  struct Scheme_Config **parent;
  struct Scheme_Config **child;
  struct Scheme_Config **sibling;

  Scheme_Object **configs[1];
} Scheme_Config;

#define scheme_set_param(c, pos, o) (*((c)->configs[pos]) = o)
#define scheme_get_param(c, pos) (*((c)->configs[pos]))

typedef struct Scheme_Saved_Stack {
  Scheme_Object **runstack_start;
  Scheme_Object **runstack;
  long runstack_size;
  struct Scheme_Saved_Stack *prev;
} Scheme_Saved_Stack;

typedef struct Scheme_Process {
  Scheme_Type type;

  jmp_buf error_buf;
  int jumping_to_continuation;

  Scheme_Config *config;

  Scheme_Object **runstack;
  Scheme_Object **runstack_start;
  long runstack_size;
  Scheme_Saved_Stack *runstack_saved;
  Scheme_Object **runstack_tmp_keep;

  long engine_weight;

  void *stack_start, *stack_end;
  Scheme_Jumpup_Buf jmpup_buf;
#if defined(USE_WIN32_THREADS) || defined(SPAWN_NEW_STACK)
  void *stack_current;
#ifdef NEW_STACK_VIA_THREAD
  void *threadinfo;
#endif
#endif
#ifdef USE_WIN32_THREADS
  void *thread;
  void *sem;
#endif
#ifdef MZ_REAL_THREADS
  void *thread;
#endif  

  void *cc_start;
  long *cc_ok;
  struct Scheme_Dynamic_Wind *dw;

  struct Scheme_Process *next;

  int running;
#ifdef ERROR_ON_OVERFLOW
  int stack_overflow;
#endif

  float sleep_time; /* blocker has starting sleep time */
  int block_descriptor;
  Scheme_Object *blocker; /* semaphore or port */
  int (*block_check)(Scheme_Object *blocker);
  void (*block_needs_wakeup)(Scheme_Object *blocker, void *fds);
  int ran_some;

#ifndef ERROR_ON_OVERFLOW
  struct Scheme_Overflow *overflow;
  jmp_buf overflow_buf;
#endif

#ifdef USE_MAC_FILE_TOOLBOX
  short wd_inited;
  short vrefnum;
  long dirid;
#else
  char *working_directory;
  int wd_len;
#endif

  struct Scheme_Comp_Env *current_local_env;

  Scheme_Object *error_escape_proc; /* Per-thread paramaterization */

  /* These are used to lock in values during `read': */
  char quick_can_read_type_symbol;
  char quick_can_read_compiled;
  char quick_can_read_pipe_quote;
  char quick_can_read_box;
  char quick_can_read_graph;
  char quick_case_sens;
  char quick_square_brackets_are_parens;
  char quick_curly_braces_are_parens;

  /* Used during `display' and `write': */
  char *print_buffer;
  long print_position;
  long print_allocated;
  long print_maxlen;
  Scheme_Object *print_port;
  jmp_buf print_escape;

  char exn_raised;
  char error_invoked;
  char err_val_str_invoked;

#ifndef ERROR_ON_OVERFLOW
  Scheme_Object *(*overflow_k)(void);
  Scheme_Object *overflow_reply;
  Scheme_Jumpup_Buf overflow_cont;
#endif

  Scheme_Object **tail_buffer;
  int tail_buffer_size;

  union {
    struct {
      Scheme_Object *wait_expr;
    } eval;
    struct {
      Scheme_Object *tail_rator;
      Scheme_Object **tail_rands;
      int tail_num_rands;
    } apply;
    struct {
      Scheme_Object **array;
      int count;
    } multiple;
    struct {
      void *p1, *p2, *p3, *p4;
      long i1, i2;
    } k;
  } ku;

  short checking_break;
  short external_break;

#ifdef MZ_REAL_THREADS
  Scheme_Object *done_sema;
  long fuel_counter;
#define scheme_fuel_counter (scheme_current_process->fuel_counter)
#define scheme_stack_boundary ((unsigned long)scheme_current_process->stack_end)
#endif

  Scheme_Object *list_stack;
  int list_stack_pos;

  long block_start_sleep;

#ifdef AGRESSIVE_ZERO_TB
  int tail_buffer_set;
#endif

  void (*on_kill)(struct Scheme_Process *p);
  void *kill_data;

  void **user_tls;
  int user_tls_size;

  Scheme_Manager_Reference *mref;
} Scheme_Process;

/* Type readers & writers for compiled code data */
typedef Scheme_Object *(*Scheme_Type_Reader)(Scheme_Object *list);
typedef Scheme_Object *(*Scheme_Type_Writer)(Scheme_Object *obj);

/* This file defines all the built-in types */
#ifdef INCLUDE_WITHOUT_PATHS
#include "stypes.h"
#else
#include "../src/stypes.h"
#endif

/* This file includes the MZEXN constants */
#ifdef INCLUDE_WITHOUT_PATHS
#include "schexn.h"
#else
#include "../src/schexn.h"
#endif

#if defined(USE_FAR_MZ_FDCALLS) || defined(DETECT_WIN32_CONSOLE_STDIN) || defined(WINDOWS_PROCESSES)
# define MZ_GET_FDSET(p, n) scheme_get_fdset(p, n)
#else
# define MZ_GET_FDSET(p, n) ((void *)(((fd_set *)p) + n))
#endif

#ifdef USE_FAR_MZ_FDCALLS
# define MZ_FD_ZERO(p) scheme_fdzero(p)
# define MZ_FD_SET(n, p) scheme_fdset(p, n)
# define MZ_FD_CLR(n, p) scheme_fdclr(p, n)
# define MZ_FD_ISSET(n, p) scheme_fdisset(p, n)
#else
# define MZ_FD_ZERO(p) FD_ZERO(p)
# define MZ_FD_SET(n, p) FD_SET(n, p)
# define MZ_FD_CLR(n, p) FD_CLR(n, p)
# define MZ_FD_ISSET(n, p) FD_ISSET(n, p)
#endif

/* Exploit the fact that these should never be dereferenced: */
#ifndef FIRST_TWO_BYTES_ARE_LEGAL_ADDRESSES
# define MZ_EVAL_WAITING_CONSTANT ((Scheme_Object *)0x2)
# define MZ_APPLY_WAITING_CONSTANT ((Scheme_Object *)0x4)
# define MZ_MULTIPLE_VALUES_CONSTANT ((Scheme_Object *)0x6)
#endif

#ifdef MZ_EVAL_WAITING_CONSTANT
# define SCHEME_EVAL_WAITING MZ_EVAL_WAITING_CONSTANT
# define SCHEME_TAIL_CALL_WAITING MZ_APPLY_WAITING_CONSTANT
# define SCHEME_MULTIPLE_VALUES MZ_MULTIPLE_VALUES_CONSTANT
#else
# define SCHEME_TAIL_CALL_WAITING scheme_tail_call_waiting
# define SCHEME_EVAL_WAITING scheme_eval_waiting
# define SCHEME_MULTIPLE_VALUES scheme_multiple_values
#endif

#define FAST_NUMBERS /* Force fast numbers */

/* Value-access macros */
#ifdef FAST_NUMBERS
#define SCHEME_TYPE(obj)     (SCHEME_INTP(obj)?(Scheme_Type)scheme_integer_type:(obj)->type)
#define _SCHEME_TYPE(obj) ((obj)->type) /* unsafe version */
#else
#define SCHEME_TYPE(obj)     ((obj)->type)
#define _SCHEME_TYPE SCHEME_TYPE
#endif


#define SCHEME_CHAR_VAL(obj) (((Scheme_Small_Object *)(obj))->u.char_val)
#ifdef FAST_NUMBERS
#define SCHEME_INT_VAL(obj)  (((long)(obj))>>1)
#else
#define SCHEME_INT_VAL(obj)  (((Scheme_Small_Object *)(obj))->u.int_val)
#endif
#define SCHEME_DBL_VAL(obj)  (((Scheme_Double *)(obj))->double_val)
#ifdef MZ_USE_SINGLE_FLOATS
# define SCHEME_FLT_VAL(obj)  (((Scheme_Float *)(obj))->float_val)
# define SCHEME_FLOAT_VAL(obj) (SCHEME_DBLP(obj) ? SCHEME_DBL_VAL(obj) : SCHEME_FLT_VAL(obj))
#else
# define SCHEME_FLT_VAL SCHEME_DBL_VAL
# define SCHEME_FLOAT_VAL SCHEME_DBL_VAL
#endif
#define SCHEME_STR_VAL(obj)  ((obj)->u.str_val.string_val)
#define SCHEME_STRTAG_VAL(obj)  ((obj)->u.str_val.tag_val)
#define SCHEME_STRLEN_VAL(obj)  ((obj)->u.str_val.tag_val)
#define SCHEME_SYM_VAL(obj)  (((Scheme_Symbol *)(obj))->s)
#define SCHEME_SYM_LEN(obj)  (((Scheme_Symbol *)(obj))->len)
#define SCHEME_TSYM_VAL(obj)  (SCHEME_SYM_VAL(SCHEME_PTR_VAL(obj)))
#define SCHEME_BOX_VAL(obj)  (((Scheme_Small_Object *)(obj))->u.ptr_val)
#define SCHEME_PTR_VAL(obj)  (((Scheme_Small_Object *)(obj))->u.ptr_val)
#define SCHEME_PTR1_VAL(obj) ((obj)->u.two_ptr_val.ptr1)
#define SCHEME_PTR2_VAL(obj) ((obj)->u.two_ptr_val.ptr2)
#define SCHEME_IPTR_VAL(obj) ((obj)->u.ptr_int_val.ptr)
#define SCHEME_LPTR_VAL(obj) ((obj)->u.ptr_long_val.ptr)
#define SCHEME_INT1_VAL(obj) ((obj)->u.two_int_val.int1)
#define SCHEME_INT2_VAL(obj) ((obj)->u.two_int_val.int2)
#define SCHEME_PINT_VAL(obj) ((obj)->u.ptr_int_val.pint)
#define SCHEME_PLONG_VAL(obj) ((obj)->u.ptr_long_val.pint)
#define SCHEME_PRIM(obj)     (((Scheme_Primitive_Proc *)(obj))->prim_val)
#define SCHEME_CLSD_PRIM(obj) (((Scheme_Closed_Primitive_Proc *)(obj))->prim_val)
#define SCHEME_CLSD_PRIM_DATA(obj) (((Scheme_Closed_Primitive_Proc *)(obj))->data)
#define SCHEME_CAR(obj)      ((obj)->u.pair_val.car)
#define SCHEME_CDR(obj)      ((obj)->u.pair_val.cdr)
#define SCHEME_VEC_SIZE(obj) (((Scheme_Vector *)(obj))->size)
#define SCHEME_VEC_ELS(obj)  (((Scheme_Vector *)(obj))->els)
#define SCHEME_VEC_BASE   SCHEME_VEC_ELS
#define SCHEME_CLOS_ENV(obj) ((obj)->u.closure_val.env)
#define SCHEME_CLOS_CODE(obj) ((obj)->u.closure_val.code)
#define SCHEME_DEBUG(obj)    ((obj)->u.debug_val)
#define SCHEME_OBJ_CLASS(obj) ((Scheme_Object *)((Scheme_Class_Object *)(obj))->sclass)
#define SCHEME_OBJ_DATA(obj)  (((Scheme_Class_Object *)(obj))->primdata)
#define SCHEME_OBJ_FLAG(obj)  (((Scheme_Class_Object *)(obj))->primflag)
#define SCHEME_INPORT_VAL(obj) (((Scheme_Input_Port)(obj))->port_data)
#define SCHEME_OUTPORT_VAL(obj) (((Scheme_Output_Port)(obj))->port_data)
#define SCHEME_VAR_BUCKET(obj) ((Scheme_Bucket *)(obj))
#define SCHEME_ENVBOX_VAL(obj)  (*((Scheme_Object **)(obj)))

#define SCHEME_ASSERT(expr,msg) ((expr) ? 1 : (scheme_signal_error(msg), 0))

#if !SCHEME_DIRECT_EMBEDDED
#ifdef MZ_REAL_THREADS
#define scheme_current_process (scheme_get_current_process())
#else
#ifdef LINK_EXTENSIONS_BY_TABLE
#define scheme_current_process (*scheme_current_process_ptr)
#endif
#endif
#endif

#define scheme_eval_wait_expr (scheme_current_process->ku.eval.wait_expr)
#define scheme_tail_rator (scheme_current_process->ku.apply.tail_rator)
#define scheme_tail_num_rands (scheme_current_process->ku.apply.tail_num_rands)
#define scheme_tail_rands (scheme_current_process->ku.apply.tail_rands)
#define scheme_overflow_k (scheme_current_process->overflow_k)
#define scheme_overflow_reply (scheme_current_process->overflow_reply)
#define scheme_overflow_cont (scheme_current_process->overflow_cont)

#define scheme_error_buf (scheme_current_process->error_buf)
#define scheme_jumping_to_continuation (scheme_current_process->jumping_to_continuation)
#define scheme_config (scheme_current_process->config)

#define scheme_multiple_count (scheme_current_process->ku.multiple.count)
#define scheme_multiple_array (scheme_current_process->ku.multiple.array)

#define scheme_setjmpup(b, s) scheme_setjmpup_relative(b, s, NULL)

#ifdef MZ_REAL_THREADS
#define scheme_do_eval(r,n,e,f) scheme_do_eval_w_process(r,n,e,f,scheme_current_process)
#else
#define scheme_do_eval_w_process(r,n,e,f,p) scheme_do_eval(r,n,e,f)
#endif
#ifdef MZ_REAL_THREADS
#define scheme_apply(r,n,a) scheme_apply_wp(r,n,a,scheme_current_process)
#define scheme_apply_multi(r,n,a) scheme_apply_multi_wp(r,n,a,scheme_current_process)
#else
#define scheme_apply_wp(r,n,a,p) scheme_apply(r,n,a)
#define scheme_apply_multi_wp(r,n,a,p) scheme_apply_multi(r,n,a)
#endif

#define _scheme_apply(r,n,rs) scheme_do_eval(r,n,rs,1)
#define _scheme_apply_multi(r,n,rs) scheme_do_eval(r,n,rs,-1)
#define _scheme_apply_wp(r,n,rs,p) scheme_do_eval_w_process(r,n,rs,1,p)
#define _scheme_apply_multi_wp(r,n,rs,p) scheme_do_eval_w_process(r,n,rs,-1,p)
#define _scheme_tail_apply scheme_tail_apply
#define _scheme_tail_apply_wp scheme_tail_apply_wp

#define _scheme_tail_eval scheme_tail_eval
#define _scheme_tail_eval_wp scheme_tail_eval_wp

#define _scheme_direct_apply_primitive_multi(prim, argc, argv) \
    (((Scheme_Primitive_Proc *)prim)->prim_val(argc, argv))
#define _scheme_direct_apply_primitive(prim, argc, argv) \
    scheme_check_one_value(_scheme_direct_apply_primitive_multi(prim, argc, argv))
#define _scheme_direct_apply_closed_primitive_multi(prim, argc, argv) \
    (((Scheme_Closed_Primitive_Proc *)prim)->prim_val(((Scheme_Closed_Primitive_Proc *)prim)->data, argc, argv))
#define _scheme_direct_apply_closed_primitive(prim, argc, argv) \
    scheme_check_one_value(_scheme_direct_apply_closed_primitive_multi(prim, argc, argv))

#define _scheme_force_value(v) ((v == SCHEME_TAIL_CALL_WAITING) ? scheme_force_value(v) : v)

#ifdef AGRESSIVE_ZERO_TB
#define scheme_tail_apply_buffer_wp(n, p) ((p)->tail_buffer_set = n, (p)->tail_buffer)
#else
#define scheme_tail_apply_buffer_wp(n, p) ((p)->tail_buffer)
#endif
#define scheme_tail_apply_buffer(n) scheme_tail_apply_buffer_wp(n, scheme_current_process)

#define _scheme_tail_apply_no_copy_wp_tcw(f, n, args, p, tcw) (p->ku.apply.tail_rator = f, p->ku.apply.tail_rands = args, p->ku.apply.tail_num_rands = n, tcw)
#define _scheme_tail_apply_no_copy_wp(f, n, args, p) _scheme_tail_apply_no_copy_wp_tcw(f, n, args, p, SCHEME_TAIL_CALL_WAITING)
#define _scheme_tail_apply_no_copy(f, n, args) _scheme_tail_apply_no_copy_wp(f, n, args, scheme_current_process)

#ifndef MZ_REAL_THREADS
#define scheme_process_block_w_process(t,p) scheme_process_block(t)
#else
#define scheme_process_block(t) scheme_process_block_w_process(t,scheme_current_process)
#endif

#if !SCHEME_DIRECT_EMBEDDED
#ifndef MZ_REAL_THREADS
#ifdef LINK_EXTENSIONS_BY_TABLE
#define scheme_fuel_counter (*scheme_fuel_counter_ptr)
#endif
#endif
#endif

#ifdef MZ_REAL_THREADS
#define _scheme_check_for_break_wp(penalty, p) \
   { if (((p)->fuel_counter -= penalty) <= 0) scheme_process_block_w_process(0, p); }
#else
#define _scheme_check_for_break_wp(penalty, p) \
   { if ((scheme_fuel_counter -= penalty) <= 0) scheme_process_block_w_process(0, p); }
#endif
#define _scheme_check_for_break(penalty) _scheme_check_for_break_wp(penalty, scheme_current_process)

#if SCHEME_DIRECT_EMBEDDED
extern Scheme_Object *scheme_eval_waiting;
#define scheme_tail_eval(obj) \
 (scheme_eval_wait_expr = obj, SCHEME_EVAL_WAITING)
#endif

#define scheme_break_waiting(p) (p->external_break)

/* Allocation */
#define scheme_alloc_object() \
   ((Scheme_Object *) scheme_malloc_tagged(sizeof(Scheme_Object)))
#define scheme_alloc_small_object() \
   ((Scheme_Object *) scheme_malloc_tagged(sizeof(Scheme_Small_Object)))
#define scheme_alloc_stubborn_object() \
   ((Scheme_Object *) scheme_malloc_stubborn_tagged(sizeof(Scheme_Object)))
#define scheme_alloc_stubborn_small_object() \
   ((Scheme_Object *) scheme_malloc_stubborn_tagged(sizeof(Scheme_Small_Object)))
#define scheme_alloc_eternal_object() \
   ((Scheme_Object *) scheme_malloc_eternal_tagged(sizeof(Scheme_Object)))
#define scheme_alloc_eternal_small_object() \
   ((Scheme_Object *) scheme_malloc_eternal_tagged(sizeof(Scheme_Small_Object)))

#ifdef SCHEME_NO_GC
void *scheme_malloc(size_t size);
#define scheme_malloc_atomic scheme_malloc
#define scheme_malloc_stubborn scheme_malloc
#define scheme_malloc_uncollectable scheme_malloc
#else
#define scheme_malloc GC_malloc
#define scheme_malloc_atomic GC_malloc_atomic
#define scheme_malloc_stubborn GC_malloc_stubborn
#define scheme_malloc_uncollectable GC_malloc_uncollectable
#endif

#ifdef USE_MEMORY_TRACING
#define USE_TAGGED_ALLOCATION
#define MEMORY_COUNTING_ON
#endif

#ifdef USE_TAGGED_ALLOCATION
extern void *scheme_malloc_tagged(size_t);
extern void *scheme_malloc_atomic_tagged(size_t);
extern void *scheme_malloc_stubborn_tagged(size_t);
extern void *scheme_malloc_eternal_tagged(size_t);
extern void *scheme_malloc_uncollectable_tagged(size_t);
extern void *scheme_malloc_envunbox(size_t);
#else
#define scheme_malloc_tagged scheme_malloc
#define scheme_malloc_atomic_tagged scheme_malloc_atomic
#define scheme_malloc_stubborn_tagged scheme_malloc_stubborn
#define scheme_malloc_eternal_tagged scheme_malloc_eternal
#define scheme_malloc_uncollectable_tagged scheme_malloc_uncollectable
#define scheme_malloc_envunbox scheme_malloc
#endif

#ifdef FAST_NUMBERS
#define scheme_make_integer(i) ((Scheme_Object *)((((long)i) << 1) | 0x1))
#else
#define scheme_make_integer scheme_make_integer_value
#endif
#define scheme_make_character(ch) (scheme_char_constants[(unsigned char)(ch)])

#define scheme_new_frame(n) scheme_new_special_frame(n, 0)
#define scheme_extend_env(f, e) (f->basic.next = e, f)
#define scheme_next_frame(e) ((e)->basic.next)
#define scheme_settable_frame(f, s) ((f)->basic.has_set_bang = (s))
#define scheme_get_frame_settable(f) ((f)->basic.has_set_bang)
#define scheme_get_binding(f, n) ((f)->values[n])

#define SNF_FOR_TS 0x1
#define SNF_PIPE_QUOTE 0x2
#define SNF_NO_PIPE_QUOTE 0x4

#if SCHEME_DIRECT_EMBEDDED

#if defined(_IBMR2)
extern long scheme_stackbottom;
#endif

extern int scheme_defining_primitives;

/* These flags must be set before MzScheme is started: */
extern int scheme_case_sensitive; /* Defaults to 0 */
extern int scheme_constant_builtins; /* Defaults to 0 */
extern int scheme_no_keywords; /* Defaults to 0 */
extern int scheme_allow_set_undefined; /* Defaults to 0 */
extern int scheme_escape_continuations_only; /* Defaults to 0 */
extern int scheme_secure_primitive_exn; /* Defaults to 0 */
extern int scheme_allow_cond_auto_else; /* Defaults to 1 */
extern int scheme_square_brackets_are_parens; /* Defaults to 1 */
extern int scheme_curly_braces_are_parens; /* Defaults to 1 */
extern int scheme_hash_percent_syntax_only; /* Defaults to 0 */

#ifdef MZ_REAL_THREADS
Scheme_Process *scheme_get_current_process();
#define scheme_current_process (SCHEME_GET_CURRENT_PROCESS())
#else
extern Scheme_Process *scheme_current_process;
#endif
extern Scheme_Process *scheme_first_process;

/* Set these global hooks: */
extern void (*scheme_exit)(int v);
extern void (*scheme_console_printf)(char *str, ...);
extern void (*scheme_sleep)(float seconds, void *fds);
extern void (*scheme_notify_multithread)(int on);
extern void (*scheme_wakeup_on_input)(void *fds);
extern int (*scheme_check_for_break)(void);
#ifdef USE_WIN32_THREADS
extern void (*scheme_suspend_main_thread)(void);
int scheme_set_in_main_thread(void);
void scheme_restore_nonmain_thread(void);
#endif
#ifdef MAC_FILE_SYSTEM
extern long scheme_creator_id;
#endif
extern void *(*scheme_get_sema_callback_context)(void);

extern Scheme_Object *(*scheme_make_stdin)(void);
extern Scheme_Object *(*scheme_make_stdout)(void);
extern Scheme_Object *(*scheme_make_stderr)(void);

/* Initialization */
Scheme_Env *scheme_basic_env(void);

void scheme_check_threads(void);
void *scheme_check_sema_callbacks(int (*)(void *, void*), void *, int check_only);
void scheme_remove_sema_callbacks(int (*)(void *, void*), void *);
void scheme_wake_up(void);

/* image dump enabling startup: */
int scheme_image_main(int argc, char **argv);
extern int (*scheme_actual_main)(int argc, char **argv);

/* All functions & global constants prototyped here */
#ifdef INCLUDE_WITHOUT_PATHS
#include "schemef.h"
#else
#include "../src/schemef.h"
#endif

#else

#ifdef LINK_EXTENSIONS_BY_TABLE
/* Constants and function prototypes as function pointers in a struct: */
#ifdef INCLUDE_WITHOUT_PATHS
#include "schemex.h"
#else
#include "../src/schemex.h"
#endif

extern Scheme_Extension_Table *scheme_extension_table;

/* Macro mapping names to record access */
#ifdef INCLUDE_WITHOUT_PATHS
#include "schemexm.h"
#else
#include "../src/schemexm.h"
#endif

#else

/* Not LINK_EXTENSIONS_BY_TABLE */
#ifdef INCLUDE_WITHOUT_PATHS
#include "schemef.h"
#else
#include "../src/schemef.h"
#endif

#endif

#endif

#ifndef USE_MZ_SETJMP
#ifdef WIN32_SETJMP_HACK    /* See comment in setjmpup.c */
#define scheme_longjmp(b, v) \
    { jmp_buf hack; setjmp(hack); (b)->j_excep = hack->j_excep; \
      longjmp(b, v); }
#else
#define scheme_longjmp(b, v) longjmp(b, v)
#endif
#define scheme_setjmp(b) setjmp(b)
#endif

#define SAME_PTR(a, b) ((a) == (b))
#define NOT_SAME_PTR(a, b) ((a) != (b))

#define SCHEME_STRUCT_NO_TYPE 0x01
#define SCHEME_STRUCT_NO_CONSTR 0x02
#define SCHEME_STRUCT_NO_PRED 0x04
#define SCHEME_STRUCT_NO_GET 0x08
#define SCHEME_STRUCT_NO_SET 0x10

#define SAME_OBJ SAME_PTR
#define NOT_SAME_OBJ NOT_SAME_PTR

#define SAME_TYPE(a, b) ((Scheme_Type)(a) == (Scheme_Type)(b))
#define NOT_SAME_TYPE(a, b) ((Scheme_Type)(a) != (Scheme_Type)(b))

/* convenience macros */
#define SCHEME_CHARP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_char_type)
#ifdef FAST_NUMBERS
#define SCHEME_INTP(obj)     (((long)obj) & 0x1)
#else
#define SCHEME_INTP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_integer_type)
#endif
#define SCHEME_DBLP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_double_type)
#ifdef MZ_USE_SINGLE_FLOATS
# define SCHEME_FLTP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_float_type)
# define SCHEME_FLOATP(obj)     (SCHEME_FLTP(obj) || SCHEME_DBLP(obj))
#else
# define SCHEME_FLTP SCHEME_DBLP
# define SCHEME_FLOATP SCHEME_DBLP
#endif
#define SCHEME_BIGNUMP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_bignum_type)
#define SCHEME_RATIONALP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_rational_type)
#define SCHEME_COMPLEXP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_complex_type)
#define SCHEME_EXACT_INTEGERP(obj)  (SCHEME_INTP(obj) || (_SCHEME_TYPE(obj) == scheme_bignum_type))
#define SCHEME_EXACT_REALP(obj)  (SCHEME_INTP(obj) || (_SCHEME_TYPE(obj) == scheme_bignum_type) || (_SCHEME_TYPE(obj) == scheme_rational_type))
#define SCHEME_REALP(obj)  (SCHEME_INTP(obj) || ((_SCHEME_TYPE(obj) >= scheme_bignum_type) && (_SCHEME_TYPE(obj) <= scheme_double_type)))
#define SCHEME_NUMBERP(obj)  (SCHEME_INTP(obj) || ((_SCHEME_TYPE(obj) >= scheme_bignum_type) && (_SCHEME_TYPE(obj) <= scheme_complex_type)))
#define SCHEME_STRINGP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_string_type)
#define SCHEME_SYMBOLP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_symbol_type)
#define SCHEME_TSYMBOLP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_type_symbol_type)
#define SCHEME_BOOLP(obj)    (SAME_OBJ(obj, scheme_true) || SAME_OBJ(obj, scheme_false))
#define SCHEME_FALSEP(obj)     SAME_OBJ((obj), scheme_false)
#define SCHEME_TRUEP(obj)     (!SCHEME_FALSEP(obj))
#define SCHEME_SYNTAXP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_syntax_compiler_type)
#define SCHEME_PRIMP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_prim_type)
#define SCHEME_CLSD_PRIMP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_closed_prim_type)
#define SCHEME_CONTP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_cont_type)
#define SCHEME_ECONTP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_escaping_cont_type)
#define SCHEME_NULLP(obj)    SAME_OBJ(obj, scheme_null)
#define SCHEME_PAIRP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_pair_type)
#define SCHEME_LISTP(obj)    (SCHEME_NULLP(obj) || SCHEME_PAIRP(obj))
#define SCHEME_BOXP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_box_type)
#define SCHEME_HASHTP(obj) SAME_TYPE(SCHEME_TYPE(obj),scheme_hash_table_type)
#define SCHEME_VECTORP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_vector_type)
#define SCHEME_STRUCT_PROCP(obj) (SCHEME_CLSD_PRIMP(obj) && (((Scheme_Closed_Primitive_Proc *)obj)->flags & SCHEME_PRIM_IS_STRUCT_PROC))
#define SCHEME_GENERICP(obj) (SCHEME_CLSD_PRIMP(obj) && (((Scheme_Closed_Primitive_Proc *)obj)->flags & SCHEME_PRIM_IS_GENERIC))
#define SCHEME_STRUCTP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_structure_type)
#define SCHEME_STRUCT_TYPEP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_struct_type_type)
#define SCHEME_CLOSUREP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_closure_type) || SAME_TYPE(SCHEME_TYPE(obj), scheme_linked_closure_type) || SAME_TYPE(SCHEME_TYPE(obj), scheme_case_closure_type))
#define SCHEME_PROCP(obj)    (SCHEME_PRIMP(obj) || SCHEME_CLSD_PRIMP(obj) || SCHEME_CLOSUREP(obj) || SCHEME_CONTP(obj) || SCHEME_ECONTP(obj))
#define SCHEME_INPORTP(obj)  SAME_TYPE(SCHEME_TYPE(obj), scheme_input_port_type)
#define SCHEME_OUTPORTP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_output_port_type)
#define SCHEME_EOFP(obj)     SAME_OBJ((obj), scheme_eof)
#define SCHEME_PROMP(obj)    SAME_TYPE(SCHEME_TYPE(obj), scheme_promise_type)
#define SCHEME_DEBUGP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_debug_handle_type)
#define SCHEME_PROCESSP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_process_type)
#define SCHEME_MANAGERP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_manager_type)
#define SCHEME_SEMAP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_sema_type)
#define SCHEME_OBJP(obj)     SAME_TYPE(SCHEME_TYPE(obj), scheme_object_type)
#define SCHEME_CLASSP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_class_type)
#define SCHEME_INTERFACEP(obj)   SAME_TYPE(SCHEME_TYPE(obj), scheme_interface_type)
#define SCHEME_VOIDP(obj)     SAME_OBJ((obj), scheme_void)
#define SCHEME_DIVARP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_delayed_ivar_type)
#define SCHEME_WEAKP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_weak_box_type)
#define SCHEME_GENDATAP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_generic_data_type)
#define SCHEME_UNITP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_unit_type)
#define SCHEME_CONFIGP(obj) SAME_TYPE(SCHEME_TYPE(obj), scheme_config_type)

/* other */
#define SCHEME_CADR(obj)     (SCHEME_CAR (SCHEME_CDR (obj)))
#define SCHEME_CAAR(obj)     (SCHEME_CAR (SCHEME_CAR (obj)))
#define SCHEME_CDDR(obj)     (SCHEME_CDR (SCHEME_CDR (obj)))
#define SCHEME_IPORT_NAME(obj) (((Scheme_Input_Port *)obj)->name)

#ifdef __cplusplus
};
#endif

#endif /* ! SCHEME_H */

