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

/* Irix SPROCS needs to load some files first, so find out if we're SPROCS. */
#ifdef INCLUDE_WITHOUT_PATHS
# include "sconfig.h"
#else
# include "../sconfig.h"
#endif

#ifdef MZ_USE_IRIX_SPROCS
/* Don't include anything else before this */
# include "../gc/gc.h"
# include "../gc/semaphores.h"
# include "../gc/sproc.h"
#endif

#include "schpriv.h"
#include <time.h>
#ifdef FILES_HAVE_FDS
# include <sys/types.h>
# include <sys/time.h>
# ifdef SELECT_INCLUDE
#  include <sys/select.h>
# endif
#endif
#ifdef USE_WINSOCK_TCP
# include <winsock.h>
#endif
#ifdef USE_STACKAVAIL
# include <malloc.h>
#endif
#ifdef UNISTD_INCLUDE
# include <unistd.h>
#endif
#if defined(MZ_USE_WIN32_THREADS)
# include <windows.h>
# include <process.h>
#endif
#ifdef MZ_USE_SOLARIS_THREADS
# include <thread.h>
# include <synch.h>
# include "../gc/gc.h"
#endif
#ifdef WIN32_THREADS
# include <process.h>
#endif

#if defined(WINDOWS_PROCESSES) || defined(DETECT_WIN32_CONSOLE_STDIN)
extern HANDLE scheme_break_semaphore;
#endif

#include "schfd.h"

typedef struct ActiveWill {
  Scheme_Object *o;
  Scheme_Object *proc;
  struct WillExecutor *w;  /* Set to will executor when executed */
  struct ActiveWill *next;
} ActiveWill;

typedef struct WillExecutor {
  Scheme_Type type;
  short running;
  ActiveWill *first, *last;
} WillExecutor;

typedef struct WillRegistration {
  Scheme_Object *proc;
  WillExecutor *w;
} WillRegistration;

#define INIT_TB_SIZE  20

static int buffer_init_size = INIT_TB_SIZE;

#ifndef MZ_REAL_THREADS
Scheme_Process *scheme_current_process = NULL;
#endif
Scheme_Process *scheme_main_process = NULL;
Scheme_Process *scheme_first_process = NULL;
#ifdef LINK_EXTENSIONS_BY_TABLE
Scheme_Process **scheme_current_process_ptr;
int *scheme_fuel_counter_ptr;
#endif

static Scheme_Manager *main_manager;

long scheme_total_gc_time;
static long start_this_gc_time;

#ifdef MANUAL_STACK_PUSH
static Scheme_Process *leaving_process;
#endif

void (*scheme_sleep)(float seconds, void *fds);
void (*scheme_notify_multithread)(int on);
void (*scheme_wakeup_on_input)(void *fds);
int (*scheme_check_for_break)(void);
void *(*scheme_get_sema_callback_context)(void);

#ifndef MZ_REAL_THREADS
static int scheme_do_atomic = 0;
static int have_activity = 0;
int scheme_active_but_sleeping = 0;
static int process_ended_with_activity;
#endif

static int tls_pos = 0;

extern void (*GC_collect_start_callback)(void);
extern void (*GC_collect_end_callback)(void);
#ifndef MZ_REAL_THREADS
extern void (*GC_push_other_roots)(void);
#endif
static void get_ready_for_GC(void);
static void done_with_GC(void);

static Scheme_Object *constants_symbol, *no_constants_symbol;
static Scheme_Object *keywords_symbol, *no_keywords_symbol;
static Scheme_Object *callcc_is_callec_symbol, *callcc_is_not_callec_symbol;
static Scheme_Object *hash_percent_syntax_symbol, *all_syntax_symbol, *empty_symbol;

static Scheme_Sema_Callback *sema_callbacks = NULL;
static Scheme_Sema_From_Port *port_semas = NULL;

static Scheme_Object *collect_garbage(int argc, Scheme_Object *args[]);

#ifndef NO_SCHEME_THREADS
static Scheme_Object *sch_thread(int argc, Scheme_Object *args[]);
static Scheme_Object *sch_sleep(int argc, Scheme_Object *args[]);
static Scheme_Object *process_weight(int argc, Scheme_Object *args[]);
static Scheme_Object *processp(int argc, Scheme_Object *args[]);
static Scheme_Object *process_running_p(int argc, Scheme_Object *args[]);
static Scheme_Object *process_wait(int argc, Scheme_Object *args[]);
static Scheme_Object *sch_current(int argc, Scheme_Object *args[]);
static Scheme_Object *kill_thread(int argc, Scheme_Object *args[]);
static Scheme_Object *break_thread(int argc, Scheme_Object *args[]);

static Scheme_Object *make_manager(int argc, Scheme_Object *argv[]);
static Scheme_Object *manager_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *manager_close_all(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_manager(int argc, Scheme_Object *argv[]);

static Scheme_Object *config_p(int argc, Scheme_Object **argv);
static Scheme_Object *parameter_p(int argc, Scheme_Object **argv);
static Scheme_Object *make_parameter(int argc, Scheme_Object **argv);
static Scheme_Object *parameter_procedure_eq(int argc, Scheme_Object **argv);
static Scheme_Object *namespace_p(int argc, Scheme_Object **argv);

static Scheme_Object *make_config(int argc, Scheme_Object *args[]);
static Scheme_Object *make_config_with_sharing(int argc, Scheme_Object *args[]);
static Scheme_Object *current_config(int argc, Scheme_Object *args[]);
static Scheme_Object *in_config(int argc, Scheme_Object **argv);
static Scheme_Object *current_namespace(int argc, Scheme_Object *args[]);
static Scheme_Object *config_branch_handler(int argc, Scheme_Object *args[]);

static Scheme_Object *make_new_config(Scheme_Config *base, Scheme_Object *defshare, Scheme_Object *clist, Scheme_Object *sharef);

static void adjust_manager_family(void *pr, void *ignored);
#endif

static Scheme_Object *current_will_executor(int argc, Scheme_Object *args[]);
static Scheme_Object *make_will_executor(int argc, Scheme_Object *args[]);
static Scheme_Object *will_executor_p(int argc, Scheme_Object *args[]);
static Scheme_Object *register_will(int argc, Scheme_Object *args[]);
static Scheme_Object *will_executor_try(int argc, Scheme_Object *args[]);

static Scheme_Object *do_next_will(WillExecutor *w, int tail);
static Scheme_Config *make_initial_config(void);
static int do_kill_thread(Scheme_Process *p);
static int check_distinct_params(Scheme_Object *l);

static Scheme_Config *initial_config;

static Scheme_Object **config_map;

typedef struct {
  unsigned long key;
  Scheme_Object *guard;
  Scheme_Object *defval;
} ParamData;

typedef struct ParamExtensionRecData {
  Scheme_Object *p;
  unsigned long key;
} ParamExtensionRecData;

typedef struct ParamExtensionRec {
  ParamExtensionRecData *data; /* atomic */
  struct ParamExtensionRec *next;
} ParamExtensionRec;

static ParamExtensionRec *param_ext_recs;

enum {
  CONFIG_DIRECT,
  CONFIG_INDIRECT
};

typedef struct {
  Scheme_Object *key;
  void (*f)(Scheme_Env *);
} Scheme_NSO;
static int num_nsos = 0;
static Scheme_NSO *namespace_options = NULL;


#ifndef PROCESS_STACK_SIZE
#define PROCESS_STACK_SIZE 300000
#endif

#if defined(MZ_REAL_THREADS)
# define SETJMP(p) 1
# define LONGJMP(p) 0
# define CLEARJMP(p)
#else /* not USE_REAL_THREADS */
# define SETJMP(p) scheme_setjmpup(&p->jmpup_buf, p->stack_start)
# define LONGJMP(p) scheme_longjmpup(&p->jmpup_buf)
# define CLEARJMP(p) scheme_init_jmpup_buf(&p->jmpup_buf)
#endif

#ifdef MZ_REAL_THREADS
void *make_namespace_mutex;
#endif

#ifndef MZ_REAL_THREADS
#define GET_WILL_LOCK() /* empty */
#define RELEASE_WILL_LOCK() /* empty */

#define GET_CUST_LOCK() /* empty */
#define RELEASE_CUST_LOCK() /* empty */
#else
static void *will_mutex;
#define GET_WILL_LOCK() SCHEME_LOCK_MUTEX(will_mutex)
#define RELEASE_WILL_LOCK()  SCHEME_UNLOCK_MUTEX(will_mutex)

static void *cust_mutex;
#define GET_CUST_LOCK() SCHEME_LOCK_MUTEX(cust_mutex)
#define RELEASE_CUST_LOCK()  SCHEME_UNLOCK_MUTEX(cust_mutex)
#endif

#ifndef MZ_REAL_THREADS
static void push_other_stacks(void);
#endif

#ifdef WIN32_THREADS
/* Only set up for Boehm GC that thinks it's a DLL: */
# define GC_THINKS_ITS_A_DLL_BUT_ISNT

# ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
extern BOOL WINAPI DllMain(HINSTANCE inst, ULONG reason, LPVOID reserved);
# endif
#endif

void scheme_init_process(Scheme_Env *env)
{
  scheme_add_global_constant("dump-memory-stats",
			     scheme_make_prim_w_arity(scheme_dump_gc_stats,
						      "dump-memory-stats",
						      0, 1), 
			     env);

#if 0
#ifdef MEMORY_COUNTING_ON
  scheme_add_global_constant("dump-memory-count",
			     scheme_make_prim_w_arity(scheme_dump_memory_count,
						      "dump-memory-count",
						      0, 1),
			     env);
#endif
#endif

#ifndef NO_SCHEME_THREADS
  scheme_add_global_constant("make-namespace",
			     scheme_make_prim_w_arity(scheme_make_namespace,
						      "make-namespace",
						      0, -1),
			     env);
  scheme_add_global_constant("make-parameterization",
			     scheme_make_prim_w_arity(make_config,
						      "make-parameterization",
						      0, 1),
			     env);
  scheme_add_global_constant("make-parameterization-with-sharing",
			     scheme_make_prim_w_arity(make_config_with_sharing,
						      "make-parameterization-with-sharing",
						      4, 4),
			     env);
  scheme_add_global_constant("thread",
			     scheme_make_prim_w_arity(sch_thread,
						      "thread",
						      1, 1),
			     env);
  
  scheme_add_global_constant("sleep",
			     scheme_make_prim_w_arity(sch_sleep,
						      "sleep",
						      0, 1),
			     env);

  scheme_add_global_constant("in-parameterization",
			     scheme_make_prim_w_arity(in_config,
						      "in-parameterization",
						      2, 2),
			     env);

  scheme_add_global_constant("thread-weight",
			     scheme_make_prim_w_arity(process_weight,
						      "thread-weight",
						      1, 2),
			     env);
  scheme_add_global_constant("thread?",
			     scheme_make_folding_prim(processp,
						      "thread?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("thread-running?",
			     scheme_make_prim_w_arity(process_running_p,
						      "thread-running?",
						      1, 1),
			     env);
  scheme_add_global_constant("thread-wait",
			     scheme_make_prim_w_arity(process_wait,
						      "thread-wait",
						      1, 1),
			     env);
  
  scheme_add_global_constant("current-thread", 
			     scheme_make_prim_w_arity(sch_current,
						      "current-thread", 
						      0, 0), 
			     env);

  scheme_add_global_constant("kill-thread", 
			     scheme_make_prim_w_arity(kill_thread,
						      "kill-thread", 
						      1, 1), 
			     env);
  scheme_add_global_constant("break-thread", 
			     scheme_make_prim_w_arity(break_thread,
						      "break-thread", 
						      1, 1), 
			     env);

  scheme_add_global_constant("make-custodian",
			     scheme_make_prim_w_arity(make_manager,
						      "make-custodian",
						      0, 1),
			     env);
  scheme_add_global_constant("custodian?",
			     scheme_make_folding_prim(manager_p,
						      "custodian?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("custodian-shutdown-all",
			     scheme_make_prim_w_arity(manager_close_all,
						      "custodian-shutdown-all",
						      0, 1),
			     env);
  scheme_add_global_constant("current-custodian", 
			     scheme_register_parameter(current_manager,
						       "current-custodian",
						       MZCONFIG_MANAGER),
			     env);

  scheme_add_global_constant("current-parameterization", 
			     scheme_make_prim_w_arity(current_config,
						      "current-parameterization", 
						      0, 1), 
			     env);

  scheme_add_global_constant("current-namespace", 
			     scheme_register_parameter(current_namespace,
						       "current-namespace",
						       MZCONFIG_ENV),
			     env);

  scheme_add_global_constant("parameterization-branch-handler", 
			     scheme_register_parameter(config_branch_handler,
						       "parameterization-branch-handler",
						       MZCONFIG_CONFIG_BRANCH_HANDLER),
			     env);

  scheme_add_global_constant("namespace?", 
			     scheme_make_prim_w_arity(namespace_p,
						      "namespace?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("parameterization?", 
			     scheme_make_prim_w_arity(config_p,
						      "parameterization?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("parameter?", 
			     scheme_make_prim_w_arity(parameter_p,
						      "parameter?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("make-parameter", 
			     scheme_make_prim_w_arity(make_parameter,
						      "make-parameter", 
						      1, 2), 
			     env);
  scheme_add_global_constant("parameter-procedure=?", 
			     scheme_make_prim_w_arity(parameter_procedure_eq,
						      "parameter-procedure=?", 
						      2, 2), 
			     env);
#endif

  scheme_add_global_constant("make-will-executor", 
			     scheme_make_prim_w_arity(make_will_executor,
						      "make-will-executor", 
						      0, 0), 
			     env);
  scheme_add_global_constant("will-executor?", 
			     scheme_make_prim_w_arity(will_executor_p,
						      "will-executor?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("register-will", 
			     scheme_make_prim_w_arity(register_will,
						      "register-will", 
						      2, 3), 
			     env);
  scheme_add_global_constant("will-executor-try", 
			     scheme_make_prim_w_arity(will_executor_try,
						      "will-executor-try", 
						      1, 1), 
			     env);
  scheme_add_global_constant("current-will-executor", 
			     scheme_register_parameter(current_will_executor,
						       "current-will-executor",
						       MZCONFIG_WILL_EXECUTOR),
			     env);

  scheme_add_global_constant("collect-garbage", 
			     scheme_make_prim_w_arity(collect_garbage, 
						      "collect-garbage",
						      0, 0), 
			     env);

  if (scheme_starting_up) {
    REGISTER_SO(config_map);
    REGISTER_SO(namespace_options);
    REGISTER_SO(sema_callbacks);
    REGISTER_SO(port_semas);
    REGISTER_SO(param_ext_recs);

#ifndef NO_SCHEME_THREADS
#ifdef MZ_REAL_THREADS
    make_namespace_mutex = SCHEME_MAKE_MUTEX();
    will_mutex = SCHEME_MAKE_MUTEX();
#endif
#endif

    REGISTER_SO(constants_symbol);
    REGISTER_SO(no_constants_symbol);
    REGISTER_SO(keywords_symbol);
    REGISTER_SO(no_keywords_symbol);
    REGISTER_SO(callcc_is_callec_symbol);
    REGISTER_SO(callcc_is_not_callec_symbol);
    REGISTER_SO(hash_percent_syntax_symbol);
    REGISTER_SO(all_syntax_symbol);
    REGISTER_SO(empty_symbol);

    constants_symbol = scheme_intern_symbol("constants");
    no_constants_symbol = scheme_intern_symbol("no-constants");
    keywords_symbol = scheme_intern_symbol("keywords");
    no_keywords_symbol = scheme_intern_symbol("no-keywords");
    callcc_is_callec_symbol = scheme_intern_symbol("call/cc=call/ec");
    callcc_is_not_callec_symbol = scheme_intern_symbol("call/cc!=call/ec");
    hash_percent_syntax_symbol = scheme_intern_symbol("hash-percent-syntax");
    all_syntax_symbol = scheme_intern_symbol("all-syntax");
    empty_symbol = scheme_intern_symbol("empty");

    GC_collect_start_callback = get_ready_for_GC;
    GC_collect_end_callback = done_with_GC;

#ifndef MZ_REAL_THREADS
    GC_push_other_roots = push_other_stacks;
#endif
  }
}

static Scheme_Object *collect_garbage(int c, Scheme_Object *p[])
{
  scheme_collect_garbage();

  return scheme_void;
}

static Scheme_Process *make_process(Scheme_Config *config, Scheme_Manager *mgr)
{
  Scheme_Process *process;
  int prefix = 0;

#ifdef WIN32_THREADS
# ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
  DllMain(NULL, DLL_PROCESS_ATTACH, NULL);
# endif
#endif

  process = MALLOC_ONE_TAGGED(Scheme_Process);

  process->type = scheme_process_type;

  process->stack_start = 0;

  if (!scheme_main_process) {
#ifndef MZ_REAL_THREADS
    REGISTER_SO(scheme_current_process);
#endif
    REGISTER_SO(scheme_main_process);
    REGISTER_SO(scheme_first_process);
#ifdef MANUAL_STACK_PUSH
    REGISTER_SO(leaving_process);
#endif

#ifdef MZ_REAL_THREADS
    process->thread = SCHEME_INIT_THREADS();
    SCHEME_SET_CURRENT_PROCESS(process);
#else
    scheme_current_process = process;
#endif
    scheme_first_process = scheme_main_process = process;
    process->next = NULL;

#ifndef MZ_REAL_THREADS
#ifdef LINK_EXTENSIONS_BY_TABLE
    scheme_current_process_ptr = &scheme_current_process;
    scheme_fuel_counter_ptr = &scheme_fuel_counter;
#endif
#endif

#ifdef MZ_REAL_THREADS
    scheme_init_stack_check();
#endif
  } else {
    prefix = 1;
  }
  
  process->engine_weight = 10000;

  if (!config) {
    config = process->config = make_initial_config();

    if (scheme_starting_up) {
      REGISTER_SO(initial_config);
      initial_config = process->config;
    }
  } else
    process->config = config;

  scheme_init_jmpup_buf(&process->jmpup_buf);

  process->running = 1;
#ifdef ERROR_ON_OVERFLOW
  process->stack_overflow = 0;
#endif

  process->dw = NULL;

  process->block_descriptor = NOT_BLOCKED;
  process->block_check = NULL;
  process->block_needs_wakeup = NULL;
  process->sleep_time = 0;

  process->current_local_env = NULL;

  process->checking_break = 0;
  process->external_break = 0;

#ifdef MANUAL_STACK_PUSH
  process->stack_current = 0L;
#endif

#ifdef USE_MAC_FILE_TOOLBOX
  process->wd_inited = 0;
#else
  process->working_directory = NULL;
#endif

  process->print_buffer = NULL;
  process->print_allocated = 0;

#ifndef NO_SCHEME_EXN
  process->exn_raised = 0;
#endif
  process->error_invoked = 0;
  process->err_val_str_invoked = 0;

  scheme_alloc_list_stack(process);

  SCHEME_GET_LOCK();
  if (prefix) {
    process->next = scheme_first_process;
    scheme_first_process = process;
  }

  process->tail_buffer = (Scheme_Object **)scheme_malloc(buffer_init_size 
							 * sizeof(Scheme_Object *));
  process->tail_buffer_size = buffer_init_size;
#ifdef AGRESSIVE_ZERO_TB
  process->tail_buffer_set = 0;
#endif
  SCHEME_RELEASE_LOCK();

  process->runstack_size = SCHEME_STACK_SIZE;
  process->runstack_start = MALLOC_N(Scheme_Object*, SCHEME_STACK_SIZE);
  process->runstack = process->runstack_start + SCHEME_STACK_SIZE;
  process->runstack_saved = NULL;

#ifdef MZ_REAL_THREADS
  process->done_sema = scheme_make_sema(0);
#endif

  process->on_kill = NULL;

  process->user_tls = NULL;
  process->user_tls_size = 0;

  process->mref = scheme_add_managed(mgr
				     ? mgr
				     : (Scheme_Manager *)scheme_get_param(scheme_config, MZCONFIG_MANAGER),
				     (Scheme_Object *)process, NULL, NULL, 0);

  return process;
}

static void ensure_manage_space(Scheme_Manager *m, int k)
{
  int i;

  if (m->count + k >= m->alloc) {
    Scheme_Object ***naya_boxes;
    Scheme_Manager_Reference **naya_mrefs;
    Scheme_Close_Manager_Client **naya_closers;
    void **naya_data;

    m->alloc = (m->alloc ? (2 * m->alloc) : 4);
    if (m->alloc < k)
      m->alloc += k;
    
    naya_boxes = MALLOC_N(Scheme_Object**, m->alloc);
    naya_closers = MALLOC_N(Scheme_Close_Manager_Client*, m->alloc);
    naya_data = MALLOC_N(void*, m->alloc);
    naya_mrefs = MALLOC_N(Scheme_Manager_Reference*, m->alloc);

    for (i = m->count; i--; ) {
      naya_boxes[i] = m->boxes[i];
      m->boxes[i] = NULL;
      naya_closers[i] = m->closers[i];
      m->closers[i] = NULL;
      naya_data[i] = m->data[i];
      m->data[i] = NULL;
      naya_mrefs[i] = m->mrefs[i];
      m->mrefs[i] = NULL;
    }

    m->boxes = naya_boxes;
    m->closers = naya_closers;
    m->data = naya_data;
    m->mrefs = naya_mrefs;
  }
}

static void add_managed_box(Scheme_Manager *m, 
			    Scheme_Object **box, Scheme_Manager_Reference *mref,
			    Scheme_Close_Manager_Client *f, void *data)
{
  int i;

  for (i = m->count; i--; ) {
    if (!m->boxes[i]) {
      m->boxes[i] = box;
      m->closers[i] = f;
      m->data[i] = data;
      m->mrefs[i] = mref;

      return;
    }
  }

  ensure_manage_space(m, 1);

  m->boxes[m->count] = box;
  m->closers[m->count] = f;
  m->data[m->count] = data;
  m->mrefs[m->count] = mref;

  m->count++;
}

static void remove_managed(Scheme_Manager_Reference *mr, Scheme_Object *o,
			   Scheme_Close_Manager_Client **old_f, void **old_data)
{
  Scheme_Manager *m;
  int i;

  GET_CUST_LOCK();

  m = *mr;
  if (!m) {
    RELEASE_CUST_LOCK();
    return;
  }

  for (i = m->count; i--; )
    if (m->boxes[i] && SAME_OBJ((*(m->boxes[i])),  o)) {
      *(m->boxes[i]) = 0;
      m->boxes[i] = NULL;
      *(m->mrefs[i]) = 0;
      m->mrefs[i] = NULL;
      if (old_f)
	*old_f = m->closers[i];
      if (old_data)
	*old_data = m->data[i];
      m->data[i] = NULL;
      break;
    }

  while (m->count && !m->boxes[m->count - 1])
    --m->count;

  RELEASE_CUST_LOCK();
}

static void adjust_manager_family(void *mgr, void *ignored)
{
  /* Threads note: because this function is only called as a
     finalization callback, it is automatically syncronized by the GC
     locks. And it is synchronized against all finalizations, so a
     managee can't try to unregister while we're shuffling its
     manager. */
  Scheme_Manager *r = (Scheme_Manager *)mgr, *parent, *m;
  int i;

  parent = *r->parent;

  GET_CUST_LOCK();

  if (parent) {
    /* Remove from parent's list of children: */
    if (*parent->children == r) {
      *parent->children = *(r->sibling);
    } else {
      m = *parent->children;
      while (m && *m->sibling != r)
	m = *m->sibling;
      if (m)
	*m->sibling = *r->sibling;
    }

    /* Add children to parent's list: */
    for (m = *r->children; m; ) {
      Scheme_Manager *next = *m->sibling;
      
      *m->parent = parent;
      *m->sibling = *parent->children;
      *parent->children = m;

      m = next;
    }

    /* Add remaining managed items to parent: */
    for (i = 0; i < r->count; i++) {
      if (r->boxes[i]) {
	*(r->mrefs[i]) = parent;
	add_managed_box(parent, r->boxes[i], r->mrefs[i], r->closers[i], r->data[i]);
      }
    }
  }

  *r->parent = NULL;
  *r->sibling = NULL;
  *r->children = NULL;

  RELEASE_CUST_LOCK();
}

Scheme_Manager *scheme_make_manager(Scheme_Manager *parent) 
{
  Scheme_Manager *m;

  m = MALLOC_ONE_TAGGED(Scheme_Manager);

  m->type = scheme_manager_type;

  m->alloc = m->count = 0;

  m->parent = MALLOC_ONE_ATOMIC(Scheme_Manager*);
  m->children = MALLOC_ONE_ATOMIC(Scheme_Manager*);
  m->sibling = MALLOC_ONE_ATOMIC(Scheme_Manager*);

  *(m->children) = NULL;
  *(m->sibling) = NULL;

  *(m->parent) = parent;
  if (parent) {
    *m->sibling = *parent->children;
    *parent->children = m;
  } else
    *(m->sibling) = NULL;

  scheme_add_finalizer(m, adjust_manager_family, NULL);

  return m;
}

static void rebox_willdone_object(void *o, void *mr)
{
  Scheme_Manager *m = *(Scheme_Manager **)mr;
  Scheme_Close_Manager_Client *f;
  void *data;

  /* Still needs management? */
  if (m) {
    Scheme_Object **b;

    remove_managed(mr, o, &f, &data);

    b = MALLOC_ONE(Scheme_Object*); /* not atomic this time */
    *b = o;
    
    /* Put the manager back: */
    *(Scheme_Manager **)mr = m;

    add_managed_box(m, b, mr, f, data);
  }
}

static void managed_object_gone(void *o, void *mr)
{
  Scheme_Manager *m = *(Scheme_Manager **)mr;

  /* Still has management? */
  if (m)
    remove_managed(mr, o, NULL, NULL);
}


Scheme_Manager_Reference *scheme_add_managed(Scheme_Manager *m, Scheme_Object *o, 
					     Scheme_Close_Manager_Client *f, void *data, int must_close)
{
  Scheme_Object **b;
  Scheme_Manager_Reference *mr;

  b = MALLOC_ONE_ATOMIC(Scheme_Object*);
  *b = o;

  mr = MALLOC_ONE_ATOMIC(Scheme_Manager_Reference);

  if (!m)
    m = (Scheme_Manager *)scheme_get_param(scheme_config, MZCONFIG_MANAGER);

  *mr = m;

  /* The atomic link via the box `b' allows the execution of wills for
     o. After this, we should either drop the object or we have to
     hold on to the object strongly (for when manager-close-all is
     called). */
  if (must_close)
    scheme_add_finalizer(o, rebox_willdone_object, mr);
  else
    scheme_add_finalizer(o, managed_object_gone, mr);

#ifdef MZ_REAL_THREADS
  /* GCing while we have the lock would be bad: */
  ensure_manage_space(m, 1);

  if (!cust_mutex)
    cust_mutex = SCHEME_MAKE_MUTEX();
#endif

  GET_CUST_LOCK();
  add_managed_box(m, b, mr, f, data);
  RELEASE_CUST_LOCK();

  return mr;
}

void scheme_remove_managed(Scheme_Manager_Reference *mr, Scheme_Object *o)
{
  remove_managed(mr, o, NULL, NULL);
}

static Scheme_Process *do_close_managed(Scheme_Manager *m)
{
  Scheme_Process *kill_self = NULL, *ks;
  Scheme_Manager *c, *next;

  /* Kill children first: */
  for (c = *(m->children); c; c = next) {
    next = *(c->sibling);
    ks = do_close_managed(c);
    if (ks)
      kill_self = ks;
  }

  while (m->count) {
    int i = m->count - 1;
    if (m->boxes[i]) {
      Scheme_Object *o;
      Scheme_Close_Manager_Client *f;
      void *data;

      o = *(m->boxes[i]);

      f = m->closers[i];
      data = m->data[i];
      *(m->boxes[i]) = NULL;
      m->boxes[i] = NULL;
      *(m->mrefs[i]) = NULL;
      m->mrefs[i] = NULL;
      m->data[i] = NULL;
      --m->count;

      if (SCHEME_PROCESSP(o)) {
	Scheme_Process *p = (Scheme_Process *)o;

	if (do_kill_thread(p))
	  kill_self = p;
      } else {
	f(o, data);
      }
    } else {
      --m->count;
    }
  }

  return kill_self;
}

void scheme_close_managed(Scheme_Manager *m)
/* The trick is that we may need to kill the thread
   that is running us. If so, delay it to the very
   end. */
{
  Scheme_Process *p;

  if ((p = do_close_managed(m))) {
    /* Kill self */
    scheme_kill_thread(p);
  }
}

void scheme_set_tail_buffer_size(int s)
{
  SCHEME_GET_LOCK();
  if (s > buffer_init_size) {
    Scheme_Process *p;

    buffer_init_size = s;

    for (p = scheme_first_process; p; p = p->next)
      if (p->tail_buffer_size < s) {
	p->tail_buffer = (Scheme_Object **)scheme_malloc(buffer_init_size 
							 * sizeof(Scheme_Object *));
	p->tail_buffer_size = buffer_init_size;
      }
  }
  SCHEME_RELEASE_LOCK();  
}

Scheme_Process *scheme_make_process()
{
  return make_process(NULL, NULL);
}

int scheme_in_main_thread(void)
{
  return !scheme_current_process->next;
}

#ifdef MZ_REAL_THREADS
Scheme_Process *scheme_get_current_process()
{
  return scheme_current_process;
}
#endif

#ifndef NO_SCHEME_THREADS

#ifndef MZ_REAL_THREADS

void scheme_swap_process(Scheme_Process *new_process)
{
#ifdef MANUAL_STACK_PUSH
  int dummy;

  scheme_current_process->stack_current = (void *)&dummy;
#endif

#ifdef DIR_FUNCTION
  scheme_set_process_directory(new_process, scheme_current_process);
#endif

  scheme_zero_unneeded_rands(scheme_current_process);

  if (SETJMP(scheme_current_process)) {
    CLEARJMP(scheme_current_process);
    /* We're back! */
#ifdef MANUAL_STACK_PUSH
    if (leaving_process) {
      leaving_process = NULL;
    }
#endif
  } else {
    /* We're leaving... */
    scheme_current_process = new_process;
    LONGJMP(scheme_current_process);
  }
}

#endif

static void remove_process(Scheme_Process *r)
{
  Scheme_Process *p;
  Scheme_Saved_Stack *saved;

  r->running = 0;

#ifdef MZ_REAL_THREADS
  scheme_post_sema(r->done_sema);
#endif

  if (r == scheme_first_process) {
    scheme_first_process = r->next;
  } else {
    p = scheme_first_process;
    while (p && p->next != r)
      p = p->next;
    if (p)
      p->next = r->next;
  }

  memset(r->runstack_start, 0, r->runstack_size * sizeof(Scheme_Object*));
  r->runstack_start = NULL;
  for (saved = r->runstack_saved; saved; saved = saved->prev) {
    memset(saved->runstack_start, 0, saved->runstack_size * sizeof(Scheme_Object*));
    saved->runstack_start = NULL;
  }

  scheme_remove_managed(r->mref, (Scheme_Object *)r);

  scheme_done_process_directory(r);
}

static void start_child(Scheme_Process *child,
			Scheme_Process *return_to_process,
			Scheme_Object *child_eval)
{
  if (SETJMP(child)) {
#ifndef MZ_REAL_THREADS
    if (return_to_process)
      scheme_swap_process(return_to_process);
#endif    

    if (!scheme_setjmp(scheme_error_buf))
      scheme_apply_multi(child_eval, 0, NULL);
    
    SCHEME_GET_LOCK();
    
#ifdef MANUAL_STACK_PUSH
    leaving_process = scheme_current_process;
#endif

    remove_process(scheme_current_process);
    SCHEME_RELEASE_LOCK();
    
#ifndef MZ_REAL_THREADS
    process_ended_with_activity = 1;
    
    if (scheme_notify_multithread && !scheme_first_process->next
	&& !sema_callbacks && !port_semas) {
      scheme_notify_multithread(0);
      have_activity = 0;
    }
#endif
    
#ifndef MZ_REAL_THREADS
    if (scheme_current_process->next)
      scheme_swap_process(scheme_current_process->next);
    else
      scheme_swap_process(scheme_first_process);
    
    /* Shouldn't get here! */
    scheme_signal_error("bad process switch");
#endif
  }
}

#if defined(MZ_REAL_THREADS)

typedef struct {
  Scheme_Process *sc_child, *sc_rtp;
  Scheme_Object *sc_eval;
} ThreadStartData;

static void really_start_child(void *data)
{
  long dummy;
  ThreadStartData *th = (ThreadStartData *)data;

#ifdef MZ_REAL_THREADS
  SCHEME_SET_CURRENT_PROCESS(th->sc_child);
  th->sc_child->stack_start = (void *)&dummy;
#endif

  start_child(th->sc_child, th->sc_rtp, th->sc_eval);
}

#endif /* defined(MZ_REAL_THREADS) */

#if defined(MZ_REAL_THREADS)

static void do_start_child(Scheme_Process *child, Scheme_Process *rtp,
			   Scheme_Object *child_eval)
{
  Scheme_Process *sc_child, *sc_rtp;
  Scheme_Object *sc_eval;

  sc_child = child;
  sc_rtp = rtp;
  sc_eval = child_eval;

  start_child(sc_child, sc_rtp, sc_eval);
  {
    ThreadStartData *th = scheme_malloc(sizeof(ThreadStartData));
    
    th->sc_child = sc_child;
    th->sc_rtp = sc_rtp;
    th->sc_eval = sc_eval;

    SCHEME_CREATE_THREAD(really_start_child, (void *)th, 
			 (unsigned long *)&sc_child->stack_end,
			 &sc_child->thread);
  }
}

#endif

static Scheme_Object *make_subprocess(Scheme_Object *child_thunk,
				      void *child_start, 
				      Scheme_Config *config,
				      Scheme_Manager *mgr)
{
  Scheme_Process *child, *return_to_process;
  int turn_on_multi;
 
  turn_on_multi = !scheme_first_process->next && !sema_callbacks
    && !port_semas;
  
  scheme_ensure_stack_start(scheme_current_process, child_start);
  
  child = make_process(config, mgr);

  child->stack_start = child_start;

  scheme_init_error_escape_proc(child);
  
#ifndef MZ_REAL_THREADS
  if (scheme_do_atomic)
    return_to_process = scheme_current_process;
  else
#endif
    return_to_process = NULL;

#if defined(MZ_REAL_THREADS)
  do_start_child(child, return_to_process, child_thunk);
#else
  start_child(child, return_to_process, child_thunk);
#endif

#ifndef MZ_REAL_THREADS
  if (scheme_notify_multithread && turn_on_multi) {
    scheme_notify_multithread(1);
    have_activity = 1;
  }

  scheme_swap_process(child);
  
  /* Because that swap just reset our fuel, we'd better check for a break */
  scheme_process_block((float)-1);
#endif
  
  return (Scheme_Object *)child;
}

Scheme_Object *scheme_thread(Scheme_Object *thunk, Scheme_Config *config)
{
  long dummy;

  return make_subprocess(thunk, (void *)&dummy, config, NULL);
}

Scheme_Object *scheme_thread_w_manager(Scheme_Object *thunk, Scheme_Config *config, Scheme_Manager *mgr)
{
  long dummy;

  return make_subprocess(thunk, (void *)&dummy, config, mgr);
}

void scheme_break_thread(Scheme_Process *p)
{
  if (!p)
    p = scheme_current_process;

  p->external_break = 1;

#ifndef MZ_REAL_THREADS
  if (p == scheme_current_process)
    scheme_fuel_counter = 0;
# if defined(WINDOWS_PROCESSES) || defined(DETECT_WIN32_CONSOLE_STDIN)
  if (!p->next)
    ReleaseSemaphore(scheme_break_semaphore, 1, NULL);
# endif
#else
  p->fuel_counter = 0;
  SCHEME_BREAK_THREAD(p->thread);
#endif
}

void scheme_add_namespace_option(Scheme_Object *key, void (*f)(Scheme_Env *))
{
  Scheme_NSO *old = namespace_options;
  
  namespace_options = (Scheme_NSO *)scheme_malloc((num_nsos + 1) 
						  * sizeof(Scheme_NSO));

  memcpy(namespace_options, old, num_nsos * sizeof(Scheme_NSO));

  namespace_options[num_nsos].key = key;
  namespace_options[num_nsos].f = f;
  
  num_nsos++;
}

Scheme_Object *scheme_make_namespace(int argc, Scheme_Object *argv[])
{
  int save_const;
  int save_no_key, save_ec_only, save_hp_syntax;
  int i, with_nso = 0;
  int empty = 0;
  Scheme_Object *v;
  Scheme_Env *env;

#ifdef MZ_REAL_THREADS
  SCHEME_LOCK_MUTEX(make_namespace_mutex);
#endif

  save_const = scheme_constant_builtins;
  save_no_key = scheme_no_keywords;
  save_ec_only = scheme_escape_continuations_only;
  save_hp_syntax = scheme_hash_percent_syntax_only;

  for (i = 0; i < argc; i++) {
    v = argv[i];
    if (v == constants_symbol)
      scheme_constant_builtins = 1;
    else if (v == no_constants_symbol)
      scheme_constant_builtins = 0;
    else if (v == keywords_symbol)
      scheme_no_keywords = 0;
    else if (v == no_keywords_symbol)
      scheme_no_keywords = 1;
    else if (v == callcc_is_callec_symbol)
      scheme_escape_continuations_only = 1;
    else if (v == callcc_is_not_callec_symbol)
      scheme_escape_continuations_only = 0;
    else if (v == hash_percent_syntax_symbol)
      scheme_hash_percent_syntax_only = 1;
    else if (v == all_syntax_symbol)
      scheme_hash_percent_syntax_only = 0;
    else if (v == empty_symbol)
      empty = 1;
    else {
      int j;
      for (j = 0; j < num_nsos; j++)
	if (namespace_options[j].key == v) {
	  with_nso = 1;
	  break;
	}
      
      if (j >= num_nsos)
	scheme_wrong_type("make-namespace", "symbol-flag", i, argc, argv);
    }
  }
  
#ifndef MZ_REAL_THREADS
  scheme_do_atomic++;
#endif
  env = (empty ? scheme_make_empty_env() : scheme_top_level_env());
#ifndef MZ_REAL_THREADS
  --scheme_do_atomic;
#endif

  scheme_constant_builtins = save_const;    
  scheme_no_keywords = save_no_key;   
  scheme_escape_continuations_only = save_ec_only;
  scheme_hash_percent_syntax_only = save_hp_syntax;

  if (with_nso && !empty) {
    int j;

    for (i = 0; i < argc; i++) {
      v = argv[i];
      for (j = 0; j < num_nsos; j++)
	if (namespace_options[j].key == v) {
	  namespace_options[j].f(env);
	  break;
	}
    }
  }

#ifdef MZ_REAL_THREADS
  SCHEME_UNLOCK_MUTEX(make_namespace_mutex);
#endif

  return (Scheme_Object *)env;
}

#endif
/* NO_SCHEME_THREADS */

void scheme_add_sema_callback(Scheme_Sema_Callback *cb)
{
  cb->next = NULL;
  cb->context = (scheme_get_sema_callback_context
		 ? scheme_get_sema_callback_context()
		 : NULL);
  cb->ready = 0;

  SCHEME_GET_LOCK();
  if (!sema_callbacks)
    sema_callbacks = cb;
  else {
    Scheme_Sema_Callback *c;

    for (c = sema_callbacks; c->next; c = c->next);
    c->next = cb;
  }
  SCHEME_RELEASE_LOCK();
#ifndef MZ_REAL_THREADS
  if (!scheme_first_process->next && !port_semas
      && scheme_notify_multithread) {
    scheme_notify_multithread(1);
    have_activity = 1;
  }
#endif
}

void scheme_add_sema_from_port(Scheme_Sema_From_Port *pt)
{
  SCHEME_GET_LOCK();
  pt->next = port_semas;
  port_semas = pt;
  SCHEME_RELEASE_LOCK();
#ifndef MZ_REAL_THREADS
  if (!scheme_first_process->next && !sema_callbacks 
      && scheme_notify_multithread ) {
    scheme_notify_multithread(1);
    have_activity = 1;
  }
#endif
}

int scheme_count_sema_callbacks(int kind)
{
  int count = 0;

  if (kind) {
    Scheme_Sema_From_Port *pt;
    for (pt = port_semas; pt; pt = pt->next)
      count++;
  } else {
    Scheme_Sema_Callback *c;
    for (c = sema_callbacks; c; c = c->next)
      count++;
  }

  return count;
}

static void port_sema_check(void) 
{  
  Scheme_Sema_From_Port *pt, *pprev;
  
  SCHEME_GET_LOCK();
  pt = port_semas;
  pprev = NULL;
  while (pt) {
    if (scheme_char_ready(pt->port)) {
      if (pprev)
	pprev->next = pt->next;
      else {
	port_semas = pt->next;
#ifndef MZ_REAL_THREADS
	if (!port_semas
	    && !scheme_first_process->next
	    && !sema_callbacks
	    && scheme_notify_multithread) {
	  scheme_notify_multithread(0);
	  have_activity = 0;
	}
#endif
      }
      
#ifndef MZ_REAL_THREADS
      pt->sema->value++;
#else
      SCHEME_SEMA_UP(pt->sema->sema);
#endif
    } else
      pprev = pt;
    pt = pt->next;
  }
  SCHEME_RELEASE_LOCK();
}

#ifndef MZ_REAL_THREADS
static int check_sleep(int need_activity, int sleep_now)
{
  Scheme_Process *p, *p2;
  Scheme_Sema_From_Port *pt;
  int end_with_act;
  
#if defined(FILES_HAVE_FDS) || defined(USE_WINSOCK_TCP)
  DECL_FDSET(set, 3);
  fd_set *set1, *set2;
#endif
  void *fds;

  /* Is everything blocked? */
  if (!scheme_do_atomic) {
    p = scheme_first_process;
    while (p) {
      if (p->ran_some || p->block_descriptor == NOT_BLOCKED)
	break;
      p = p->next;
    }
  } else
    p = NULL;
  
  p2 = scheme_first_process;
  while (p2) {
    p2->ran_some = 0;
    p2 = p2->next;
  }
  
  end_with_act = process_ended_with_activity;
  process_ended_with_activity = 0;
  
  if (need_activity && !end_with_act && 
      (scheme_do_atomic 
       || (!p && ((!sleep_now && scheme_wakeup_on_input)
		  || (sleep_now && scheme_sleep))))) {
    float max_sleep_time = 0;

    /* Poll from top-level process, and all subprocesses are blocked. */
    /* So, everything is blocked pending external input. */
    /* Build a list of file descriptors that we're waiting on */
    /* and turn off polling. */
    if (have_activity)
      scheme_active_but_sleeping = 1;
    if (have_activity && scheme_notify_multithread)
      scheme_notify_multithread(0);
    
#if defined(FILES_HAVE_FDS) || defined(USE_WINSOCK_TCP)
    INIT_DECL_FDSET(set, 3);
    set1 = (fd_set *) MZ_GET_FDSET(set, 1);
    set2 = (fd_set *) MZ_GET_FDSET(set, 2);

    fds = (void *)set;
    MZ_FD_ZERO(set);
    MZ_FD_ZERO(set1);
    MZ_FD_ZERO(set2);
#else
    fds = NULL;
#endif
    
    p = scheme_first_process;
    while (p) {
      int merge_time = 0;

      if (p->block_descriptor == -1) {
	if (p->block_needs_wakeup)
	  (p->block_needs_wakeup)(p->blocker, fds);
	merge_time = (p->sleep_time > 0.0);
      } else if (p->block_descriptor == PORT_BLOCKED)
	scheme_need_wakeup(p->blocker, fds);
      else if (p->block_descriptor == SLEEP_BLOCKED) {
	merge_time = 1;
      }

      if (merge_time) {
	long d = (long)p->block_start_sleep;
	float t;

	d = (scheme_get_milliseconds() - d);

	if (d < 0)
	  d = -d;
	
	t = p->sleep_time - (((float)d) / 1000);
	if (t <= 0)
	  t = 0.00001;
	if (!max_sleep_time || (t < max_sleep_time))
	  max_sleep_time = t;
      } 
      p = p->next;
    }
    
    pt = port_semas;
    while (pt) {
      scheme_need_wakeup(pt->port, fds);
      pt = pt->next;
    }
    
    if (sleep_now)
      scheme_sleep(max_sleep_time, fds);
    else
      scheme_wakeup_on_input(fds);
    
    return 1;
  }
  
  return 0;
}
#endif

void *scheme_check_sema_callbacks(int (*test)(void *, void *), void *cdata, int check_only)
{
  Scheme_Sema_Callback *cb, *prev;
  jmp_buf savebuf;
  
  port_sema_check();

  cb = sema_callbacks;
  while (cb) {
    int passed;
#ifdef MZ_REAL_THREADS
    passed = SCHEME_SEMA_TRY_DOWN(cb->sema->sema);
#else
    if (!test || test(cdata, cb->context)) {
      if (cb->ready)
	passed = 1;
      else if (cb->sema->value) {
	passed = 1;
	cb->sema->value--;
	cb->ready = 1;
      } else
	passed = 0;
    } else
      passed = 0;
#endif
    if (passed) {
      if (check_only)
	return cb->context;
      SCHEME_GET_LOCK();
      /* Remove this callback record from the list: */
      if (NOT_SAME_PTR(cb, sema_callbacks)) {
	prev = sema_callbacks;
	while (prev) {
	  if (SAME_PTR(prev->next, cb)) {
	    prev->next = cb->next;
	    break;
	  } else
	    prev = prev->next;
	}
      } else {
	sema_callbacks = cb->next;
	
#ifndef MZ_REAL_THREADS
	if (!sema_callbacks 
	    && !scheme_first_process->next
	    && !port_semas
	    && scheme_notify_multithread) {
	  scheme_notify_multithread(0);
	  have_activity = 0;
	}
#endif
      }
      SCHEME_RELEASE_LOCK();

      memcpy(&savebuf, &scheme_error_buf, sizeof(jmp_buf));
      if (!scheme_setjmp(scheme_error_buf))
	scheme_apply_multi(cb->callback, 0, NULL);
      memcpy(&scheme_error_buf, &savebuf, sizeof(jmp_buf));

      return cb->context;
    }
    cb = cb->next;
  }

  return NULL;
}

void scheme_remove_sema_callbacks(int (*test)(void *, void *), void *cdata)
{
  Scheme_Sema_Callback *cb, *prev;

  prev = NULL;
  for (cb = sema_callbacks; cb; cb = cb->next) {
    if (test(cdata, cb->context)) {
      if (prev)
	prev->next = cb->next;
      else
	sema_callbacks = cb->next;
    } else
      prev = cb;
  }
}

void scheme_check_threads(void)
{
  scheme_check_sema_callbacks(NULL, NULL, 0);

#ifndef MZ_REAL_THREADS
  scheme_current_process->checking_break = 1;
  scheme_process_block((float)0);
  scheme_current_process->checking_break = 0;

  check_sleep(have_activity, 0);
#endif
}

void scheme_wake_up(void)
{
#ifndef MZ_REAL_THREADS
  scheme_active_but_sleeping = 0;
  if (have_activity && scheme_notify_multithread)
    scheme_notify_multithread(1);
#endif
}

void scheme_out_of_fuel(void)
{
#ifndef MZ_REAL_THREADS
  scheme_process_block((float)0);
  scheme_current_process->ran_some = 1;
#endif
}

void scheme_zero_unneeded_rands(Scheme_Process *p)
{
  /* Call this procedure before GC or before copying out
     a thread's stack. */
}

static void get_ready_for_GC()
{
  start_this_gc_time = scheme_get_process_milliseconds();

  scheme_zero_unneeded_rands(scheme_current_process);

#ifndef MZ_REAL_THREADS
  if (scheme_fuel_counter) {
    Scheme_Process *p;

    /* zero ununsed part of env stack in each thread */
    for (p = scheme_first_process; p; p = p->next) {
      Scheme_Object **o, **e, **e2;
      Scheme_Saved_Stack *saved;

      o = p->runstack_start;
      e = p->runstack;
      e2 = p->runstack_tmp_keep;

      while (o < e && (o != e2))
	*(o++) = NULL;

      for (saved = p->runstack_saved; saved; saved = saved->prev) {
	o = saved->runstack_start;
	e = saved->runstack_start;
	while (o < e)
	  *(o++) = NULL;
      }

# ifdef AGRESSIVE_ZERO_TB
      {
	int i;
	for (i = p->tail_buffer_set; i < p->tail_buffer_size; i++)
	  p->tail_buffer[i] = NULL;
      }
# endif
    }
  }
#endif
   
  scheme_fuel_counter = 0;
}

static void done_with_GC()
{
  scheme_total_gc_time += (scheme_get_process_milliseconds() - start_this_gc_time);
}

int scheme_can_break(Scheme_Process *p, Scheme_Config *config)
{
  return (SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_ENABLE_BREAK))
	  && (!p->exn_raised 
	      || SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_ENABLE_EXCEPTION_BREAK))));
}

int scheme_block_until(int (*f)(Scheme_Object *), void (*fdf)(Scheme_Object *,void*), 
		       void *data, float delay)
{
  int result;
  Scheme_Process *p = scheme_current_process;

  p->block_descriptor = -1;
  p->blocker = (Scheme_Object *)data;
  p->block_check = f;
  p->block_needs_wakeup = fdf;
  do
    scheme_process_block(delay);
  while (!(result = f((Scheme_Object *)data)));
  p->block_descriptor = NOT_BLOCKED;
  p->blocker = NULL;
  p->block_check = NULL;
  p->block_needs_wakeup = NULL;
  p->ran_some = 1;

  return result;
}

static void make_unblocked(Scheme_Process *p)
{
  p->block_descriptor = 0;
  p->blocker = NULL;
  p->block_check = NULL;
  p->block_needs_wakeup = NULL;
  p->ran_some = 1;
}

#ifndef MZ_REAL_THREADS
void scheme_process_block(float sleep_time)
#else
void scheme_process_block_w_process(float sleep_time, Scheme_Process *p)
#endif
/* Auto-resets p's blocking info if an escape occurs. */
{
  long start, d;
#ifndef MZ_REAL_THREADS
  Scheme_Process *next, *prev, *p = scheme_current_process;
#endif
  Scheme_Config *config = p->config;

#ifndef MZ_REAL_THREADS
  if (p->running < 0) {
    /* This thread is dead! Give up now. */
    scheme_swap_process(scheme_first_process);
  }

  if (scheme_active_but_sleeping)
    scheme_wake_up();
#else
  if (p->running <= 0) {
    remove_process(p);
    SCHEME_EXIT_THREAD();
  }
#endif

  if (sleep_time > 0)
    start = scheme_get_milliseconds();
  else
    start = 0; /* compiler-friendly */

  if (!p->checking_break) {
    int breaking;
    int allow_break = scheme_can_break(p, config);

    if (p->external_break) {
      if (allow_break) {
	p->external_break = 0;
	breaking = 1;
      } else
	breaking = 0;
    } else {
      Scheme_Object *poll;
      
      poll = scheme_get_param(config, MZCONFIG_USER_BREAK_POLL_HANDLER);
      
      if (poll && NOT_SAME_OBJ(poll, scheme_null_break_poll)) {
	Scheme_Object *v;
	int save_desc = p->block_descriptor;
	Scheme_Object *save_blocker = p->blocker;
	int (*save_check)(Scheme_Object *) = p->block_check;
	void (*save_needs_wakeup)(Scheme_Object *, void *) = p->block_needs_wakeup;
	int ran_some = p->ran_some;

	make_unblocked(p);
	
	p->checking_break = 1;
	v = scheme_apply_wp(poll, 0, NULL, p);
	p->checking_break = 0;

	p->block_descriptor = save_desc;
	p->blocker = save_blocker;
	p->block_check = save_check;
	p->block_needs_wakeup = save_needs_wakeup;
	p->ran_some = ran_some;
	
	breaking = SCHEME_TRUEP(v);
      } else
	breaking = 0;

      if (breaking && !allow_break) {
	p->external_break = 1;
	breaking = 0;
      }
    }
	
    if (breaking
	|| (!p->next && scheme_check_for_break && scheme_check_for_break())) {
      make_unblocked(p);
      scheme_raise_exn(MZEXN_MISC_USER_BREAK, "user break");
    }
  }
  
 swap_or_sleep:
  
  /* Check for active wills: */
  {
    WillExecutor *w;
    
    w = (WillExecutor *)scheme_get_param(config, MZCONFIG_WILL_EXECUTOR);
    while (w->first && !w->running && (p->running >= 0)) {
      p->ran_some = 1;
      do_next_will(w, 0);
      p->ran_some = 1;
    }
  }

  port_sema_check();
  
#ifndef MZ_REAL_THREADS
  if (!scheme_do_atomic && (sleep_time >= 0.0)) {
    /* Find the next process. Skip processes that are definitely
       blocked. */
    
    prev = next = p;
    while (1) {
      next = next->next ? next->next : scheme_first_process;
      if (SAME_PTR(next, p)) {
	next = NULL;
	break;
      }
      
      if (next->running < 0) {
	/* This one has been terminated */
	remove_process(next);
      } else if (next->external_break && scheme_can_break(next, next->config)) {
	break;
      } else {
	prev = next;
	if (next->block_descriptor == -1) {
	  if (next->block_check)
	    if ((next->block_check)(next->blocker))
	      break;
	} else if (next->block_descriptor == EVENTLOOP_BLOCKED) {
	  /* Can't use it. */
	} else if (next->block_descriptor == SEMA_BLOCKED) {
	  Scheme_Sema *sema = (Scheme_Sema *)next->blocker;
	  if (sema->value)
	    break;
	} else if ((next->block_descriptor == PORT_BLOCKED)
		   || (next->block_descriptor == PIPE_BLOCKED)) {
	  if (scheme_char_ready(next->blocker))
	    break;
	} else if (next->block_descriptor == SLEEP_BLOCKED) {
	  d = (scheme_get_milliseconds() - (long)next->block_start_sleep);
	  if (d < 0)
	    d = -d;
	  if (d >= (next->sleep_time * 1000))
	    break;
	} else
	  break;

	/* If the will executor has something, the thread can work: */
	{
	  WillExecutor *w;

	  w = (WillExecutor *)scheme_get_param(next->config, MZCONFIG_WILL_EXECUTOR);
	  if (w->first && !w->running)
	    break;
	}
      }
    }
  } else
    next = NULL;
  
  if ((sleep_time > 0.0) && (p->block_descriptor == NOT_BLOCKED)) {
    p->block_descriptor = SLEEP_BLOCKED;
    p->block_start_sleep = start;
    p->sleep_time = sleep_time;
  }

  if (next) {
    if (!p->next) {
      /* This is the main process */
      scheme_ensure_stack_start(p, (void *)&start);
    }
    
    scheme_swap_process(next);
  } else {
    /* If all processes are blocked, check for total process sleeping: */
    if (p->block_descriptor != NOT_BLOCKED)
      check_sleep(1, 1);
  }

  if (p->block_descriptor == SLEEP_BLOCKED) {
    p->block_descriptor = NOT_BLOCKED;
    p->sleep_time = 0.0;
  }
#else
  if ((sleep_time > 0) && scheme_sleep)
    scheme_sleep(sleep_time, NULL);
#endif

#ifndef MZ_REAL_THREADS
  /* Killed while I was asleep? */
  if (p->running < 0) {
    /* This thread is dead! Give up now. */
    remove_process(p);
    scheme_swap_process(scheme_first_process);
  }
#else
  if (p->running <= 0) {
    remove_process(p);
    SCHEME_EXIT_THREAD();
  }
#endif

  /* Check for external break again after swap or sleep */
  if (!p->checking_break && p->external_break && scheme_can_break(p, config)) {
    p->external_break = 0;
    make_unblocked(p);
    scheme_raise_exn(MZEXN_MISC_USER_BREAK, "user break");
  }
  
  if (sleep_time > 0) {
    d = (scheme_get_milliseconds() - start);
    if (d < 0)
      d = -d;
    if (d < (sleep_time * 1000))
      goto swap_or_sleep;
  }

#if defined(MZ_REAL_THREADS) && (defined(FILES_HAVE_FDS) || defined(USE_WINSOCK_TCP))
  if ((p->block_descriptor == PORT_BLOCKED)
      || (p->block_descriptor == -1)) {
    DECL_FDSET(set, 3);
    fd_set *set1, *set2;
    void *fds;
    float sleep_time = 0;

    INIT_DECL_FDSET(set, 3);
    set1 = (fd_set *) MZ_GET_FDSET(set, 1);
    set2 = (fd_set *) MZ_GET_FDSET(set, 2);

    fds = (void *)set;
    MZ_FD_ZERO(set);
    MZ_FD_ZERO(set1);
    MZ_FD_ZERO(set2);
    
    if (p->block_descriptor == -1) {
	if (p->block_needs_wakeup)
	  (p->block_needs_wakeup)(p->blocker, fds);
	sleep_time = p->sleep_time;
    } else
      scheme_need_wakeup(p->blocker, fds);

    if (scheme_sleep)
      scheme_sleep(sleep_time, fds);
  }
#endif

  MZTHREADELEM(p, fuel_counter) = p->engine_weight;
}

#ifndef NO_SCHEME_THREADS

Scheme_Object *scheme_branch_config(void)
{
  Scheme_Object *o;

  o = _scheme_apply(scheme_get_param(scheme_config,
				     MZCONFIG_CONFIG_BRANCH_HANDLER),
		    0, NULL);

  if (!SCHEME_CONFIGP(o)) {
    scheme_raise_exn(MZEXN_MISC_PARAMETERIZATION,
		     o,
		     "thread: parameterization branch handler returned a non-parameterization: %s",
		     scheme_make_provided_string(o, 1, NULL));
    return NULL;
  }

  return o;
}

static Scheme_Object *sch_thread(int argc, Scheme_Object *args[])
{
  scheme_check_proc_arity("thread", 0, 0, argc, args);

  return scheme_thread(args[0], (Scheme_Config *)scheme_branch_config());
}

static Scheme_Object *
sch_sleep(int argc, Scheme_Object *args[])
{
  float t;

  if (argc && !SCHEME_REALP(args[0]))
    scheme_wrong_type("sleep", "real", 0, argc, args);

  if (argc) {
    if (SCHEME_INTP(args[0]))
      t = SCHEME_INT_VAL(args[0]);
    else if (SCHEME_DBLP(args[0]))
      t = SCHEME_DBL_VAL(args[0]);
    else if (SCHEME_BIGNUMP(args[0]))
      t = scheme_bignum_to_float(args[0]);
    else if (SCHEME_RATIONALP(args[0]))
      t = scheme_rational_to_float(args[0]);
    else
      t = 0;
  } else
    t = 0;

  scheme_process_block(t);

  return scheme_void;
}

static Scheme_Object *sch_current(int argc, Scheme_Object *args[])
{
  return (Scheme_Object *)scheme_current_process;
}

static Scheme_Object *break_thread(int argc, Scheme_Object *args[])
{
  Scheme_Process *p;

  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_process_type))
    scheme_wrong_type("break-thread", "thread", 0, argc, args);

  p = (Scheme_Process *)args[0];

  scheme_break_thread(p);

  return scheme_void;
}

static int do_kill_thread(Scheme_Process *p)
{
  int kill_self = 0;

  if (!p->next) {
    if (scheme_exit)
      scheme_exit(0);

    /* We really have to exit: */
    exit(0);
  }

  if (p->running <= 0)
    return 0;

  if (p->on_kill)
    p->on_kill(p);

  scheme_remove_managed(p->mref, (Scheme_Object *)p);

#ifdef MZ_REAL_THREADS
  SCHEME_GET_LOCK();
  if (p->running) {
    p->running = -1;
    if (p == scheme_current_process)
      kill_self = 1;
    else
      p->fuel_counter = 0;
  }
  SCHEME_RELEASE_LOCK();
#else
  if (p->running)
    p->running = -1;
#endif

  return kill_self;
}

void scheme_kill_thread(Scheme_Process *p)
{
  if (do_kill_thread(p)) {
#ifdef MZ_REAL_THREADS
    /* Kill self: */
    SCHEME_EXIT_THREAD();
#endif
  }

#ifndef MZ_REAL_THREADS
  scheme_process_block(0);
#endif
}

static Scheme_Object *kill_thread(int argc, Scheme_Object *argv[])
{
  Scheme_Manager *m, *current;
  Scheme_Process *p = (Scheme_Process *)argv[0];

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_process_type))
    scheme_wrong_type("kill-thread", "thread", 0, argc, argv);

  if (p->running <= 0)
    return scheme_void;

  /* Check management of the thread: */
  current = (Scheme_Manager *)scheme_get_param(scheme_config, MZCONFIG_MANAGER);
  m = *p->mref;
  while (NOT_SAME_OBJ(m, current)) {
    m = *m->parent;
    if (!m) {
      scheme_raise_exn(MZEXN_MISC_THREAD_KILL,
		       "kill-thread: the current custodian does not "
		       "manage the specified thread");
      return NULL;
    }
  }

  scheme_kill_thread(p);

  return scheme_void;
}

static Scheme_Object *process_weight(int argc, Scheme_Object *args[])
{
  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_process_type))
    scheme_wrong_type("thread-weight", "thread", 0, argc, args);

  if (argc > 1) {
    long v;
    Scheme_Process *p;

    if (!SCHEME_INTP(args[1])) {
      if (!SCHEME_BIGNUMP(args[1]) || !SCHEME_BIGPOS(args[1]))
	scheme_wrong_type("thread-weight", "non-negative exact integer", 1, argc, args);
    }

    if (!scheme_get_int_val(args[1], &v)) {
      scheme_raise_exn(MZEXN_APPLICATION_INTEGER,
		       args[1],
		       "thread-weight: %s is too large",
		       scheme_make_provided_string(args[1], 0, NULL));
    } else if (v < 0) {
      scheme_wrong_type("thread-weight", "non-negative exact integer", 1, argc, args);
    }

    p = (Scheme_Process *)args[0];
    
    p->engine_weight = v;
    if (p == scheme_current_process && scheme_fuel_counter > v)
      scheme_fuel_counter = v;
    
    return scheme_void;
  } else
    return scheme_make_integer(((Scheme_Process *)args[0])->engine_weight);
}

static Scheme_Object *processp(int argc, Scheme_Object *args[])
{
  return SCHEME_PROCESSP(args[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *process_running_p(int argc, Scheme_Object *args[])
{
  if (!SCHEME_PROCESSP(args[0]))
    scheme_wrong_type("thread-running?", "thread", 0, argc, args);

  return ((Scheme_Process *)args[0])->running ? scheme_true : scheme_false;
}

#ifndef MZ_REAL_THREADS
static int thread_wait_done(Scheme_Object *p)
{
  return !((Scheme_Process *)p)->running;
}
#endif

static Scheme_Object *process_wait(int argc, Scheme_Object *args[])
{
  Scheme_Process *p;

  if (!SCHEME_PROCESSP(args[0]))
    scheme_wrong_type("thread-wait", "thread", 0, argc, args);

  p = (Scheme_Process *)args[0];

  if (p->running) {
#ifndef MZ_REAL_THREADS
    scheme_block_until(thread_wait_done, NULL, p, 0);
#else
    scheme_wait_sema(p->done_sema, 0);
    scheme_post_sema(p->done_sema);
#endif
  }

  return scheme_void;
}

int scheme_tls_allocate()
{
  return tls_pos++;
}

void scheme_tls_set(int pos, void *v)
{
  Scheme_Process *p = scheme_current_process;

  if (p->user_tls_size <= pos) {
    int oldc = p->user_tls_size;
    void **old_tls = p->user_tls;

    p->user_tls_size = tls_pos;
    p->user_tls = MALLOC_N(void*, tls_pos);
    while (oldc--)
      p->user_tls[oldc] = old_tls[oldc];
  }

  p->user_tls[pos] = v;
}

void *scheme_tls_get(int pos)
{
  Scheme_Process *p = scheme_current_process;

  if (p->user_tls_size <= pos)
    return NULL;
  else
    return p->user_tls[pos];
}

#endif
/* NO_SCHEME_THREADS */

static Scheme_Object *make_manager(int argc, Scheme_Object *argv[])
{
  Scheme_Manager *m;

  if (argc) {
    if (!SCHEME_MANAGERP(argv[0]))
      scheme_wrong_type("make-custodian", "custodian", 0, argc, argv);
    m = (Scheme_Manager *)argv[0];
  } else
    m = (Scheme_Manager *)scheme_get_param(scheme_config, MZCONFIG_MANAGER);

  return (Scheme_Object *)scheme_make_manager(m);
}

static Scheme_Object *manager_p(int argc, Scheme_Object *argv[])
{
  return SCHEME_MANAGERP(argv[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *manager_close_all(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_MANAGERP(argv[0]))
    scheme_wrong_type("custodian-shutdown-all", "custodian", 0, argc, argv);

  scheme_close_managed((Scheme_Manager *)argv[0]);

  return scheme_void;
}

static Scheme_Object *current_manager(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-custodian", MZCONFIG_MANAGER,
			     argc, argv,
			     -1, manager_p, "custodian", 0);
}

static Scheme_Object *make_config(int argc, Scheme_Object *args[])
{
  Scheme_Config *c;

  if (argc) {
    if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_config_type))
      scheme_wrong_type("make-parameterization", "parameterization", 0, argc, args);
    c = (Scheme_Config *)args[0];
  } else
    c = NULL;
    
  return make_new_config(c, NULL, NULL, NULL);
}

static Scheme_Object *make_config_with_sharing(int argc, Scheme_Object *args[])
{
  Scheme_Config *c;
  Scheme_Object *p, *l, *tl, *s;
  char *badlist;

  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_config_type))
    scheme_wrong_type("make-parameterization-with-sharing", "parameterization", 0, argc, args);
  c = (Scheme_Config *)args[0];

  if (SCHEME_FALSEP(args[1])) {
    s = scheme_false;
  } else {
    if (!SAME_TYPE(SCHEME_TYPE(args[1]), scheme_config_type)) {
      scheme_wrong_type("make-parameterization-with-sharing", "parameterization or #f", 1, argc, args);
      return NULL;
    }
    s = args[1];
  }

  l = args[2];
  badlist = NULL;
  for (tl = l; SCHEME_PAIRP(tl); tl = SCHEME_CDR(tl)) {
    Scheme_Object *v = SCHEME_CAR(tl);
    if (!((SCHEME_PRIMP(v) || SCHEME_CLSD_PRIMP(v))
	  && (((Scheme_Primitive_Proc *)v)->flags & SCHEME_PRIM_IS_PARAMETER)))
      break;
  }
  if (SCHEME_NULLP(tl)) {
    if (!SCHEME_NULLP(l) && !check_distinct_params(l))
      badlist = "list of DISTINCT parameter procedures";
  } else
    badlist = "list of (distinct) parameter procedures";
  
  if (badlist) {
    scheme_wrong_type("make-parameterization-with-sharing", badlist, 2, argc, args);
    return NULL;
  }

  scheme_check_proc_arity("make-parameterization-with-sharing", 1, 3, argc, args);
  p = args[3];
  
  return make_new_config(c, s, l, p);
}

static Scheme_Object *parameter_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *v = argv[0];

  return (((SCHEME_PRIMP(v) || SCHEME_CLSD_PRIMP(v))
	   && (((Scheme_Primitive_Proc *)v)->flags & SCHEME_PRIM_IS_PARAMETER))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *do_param(void *data, int argc, Scheme_Object *argv[])
{
  Scheme_Object *guard, **argv2;

  if (argc && argv[0]) {
    guard = ((ParamData *)data)->guard;
    if (guard) {
      Scheme_Object *v;
      long l;
      
      v = _scheme_apply(guard, 1, argv);
      argv2 = (Scheme_Object **)scheme_malloc(l = (argc * sizeof(Scheme_Object **)));
      memcpy(argv2, argv, l);
      argv2[0] = v;
    } else
      argv2 = argv;
  } else
    argv2 = argv;    

  return scheme_param_config("parameter-procedure", 
			     (long)scheme_make_pair((Scheme_Object *)((ParamData *)data)->key,
						    ((ParamData *)data)->defval),
			     argc, argv2,
			     -2, NULL, NULL, 0);
}

static Scheme_Object *make_parameter(int argc, Scheme_Object **argv)
{
  Scheme_Object *p;
  ParamData *data;
  ParamExtensionRec *erec, *prev;
  void *k = scheme_malloc_atomic(1); /* generates a key */

  if (argc > 1)
    scheme_check_proc_arity("make-parameter", 1, 1, argc, argv);

  data = MALLOC_ONE(ParamData);
  data->key = (unsigned long)k;
  data->defval = argv[0];
  data->guard = ((argc > 1) ? argv[1] : NULL);

  p = scheme_make_closed_prim_w_arity(do_param, (void *)data, 
				      "parameter-procedure", 0, 1);
  ((Scheme_Primitive_Proc *)p)->flags |= SCHEME_PRIM_IS_PARAMETER;

  /* Throw away expired ext recs: */
  prev = NULL;
  for (erec = param_ext_recs; erec; erec = erec->next) {
    if (!erec->data->p) {
      if (prev)
	prev->next = erec->next;
      else
	param_ext_recs = erec->next;
    } else
      prev = erec;
  }

  erec = MALLOC_ONE(ParamExtensionRec);
  erec->next = param_ext_recs;
  param_ext_recs = erec;
  erec->data = MALLOC_ONE_ATOMIC(ParamExtensionRecData);
  erec->data->p = p;
  erec->data->key = (unsigned long)k;

  scheme_weak_reference((void **)&erec->data->p);

  return p;
}

static Scheme_Object *parameter_procedure_eq(int argc, Scheme_Object **argv)
{
  Scheme_Object *a, *b;

  a = argv[0];
  b = argv[1];

  if (!((SCHEME_PRIMP(a) || SCHEME_CLSD_PRIMP(a))
	&& (((Scheme_Primitive_Proc *)a)->flags & SCHEME_PRIM_IS_PARAMETER)))
    scheme_wrong_type("parameter-procedure=?", "parameter-procedure", 0, argc, argv);
  if (!((SCHEME_PRIMP(b) || SCHEME_CLSD_PRIMP(b))
	&& (((Scheme_Primitive_Proc *)b)->flags & SCHEME_PRIM_IS_PARAMETER)))
    scheme_wrong_type("parameter-procedure=?", "parameter-procedure", 1, argc, argv);

  return (SAME_OBJ(a, b)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *do_param_in_config(Scheme_Object *data, int argc, Scheme_Object *args[])
{
  Scheme_Object *args2[2], *p;

  if (argc)
    args2[0] = args[0];
  else
    args2[0] = NULL;
  args2[1] = SCHEME_CDR(data);

  p = SCHEME_CAR(data);

  if (SCHEME_PRIMP(p))
    return ((Scheme_Primitive_Proc *)p)->prim_val(2, args2);
  else {
    Scheme_Closed_Primitive_Proc *c = (Scheme_Closed_Primitive_Proc *)p;

    return c->prim_val(c->data, 2, args2);
  }
}

static Scheme_Object *in_config(int argc, Scheme_Object *args[])
{
  char name[256];
  Scheme_Object *p;

  if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_config_type))
    scheme_wrong_type("in-parameterization", "parameterization", 0, argc, args);
  p = args[1];
  if ((!SCHEME_PRIMP(p) && !SCHEME_CLSD_PRIMP(p))
      || !(((Scheme_Primitive_Proc *)p)->flags & SCHEME_PRIM_IS_PARAMETER))
    scheme_wrong_type("in-parameterization", "parameter procedure", 1, argc, args);
  
  sprintf(name, "%s in parameterization", 
	  SCHEME_PRIMP(p) 
	  ? ((Scheme_Primitive_Proc *)p)->name
	  : ((Scheme_Closed_Primitive_Proc *)p)->name);

  return scheme_make_closed_prim_w_arity((Scheme_Closed_Prim *)do_param_in_config,
					 scheme_make_pair(args[1], args[0]),
					 scheme_strdup(name),
					 0, 1);
}

static Scheme_Object *config_p(int argc, Scheme_Object **argv)
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_config_type)) 
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *current_config(int argc, Scheme_Object *args[])
{
  if (!argc)
    return (Scheme_Object *)scheme_config;
  else {
    if (!SAME_TYPE(SCHEME_TYPE(args[0]), scheme_config_type))
      scheme_wrong_type("current-parameterization", "parameterization", 0, argc, args);
    scheme_config = (Scheme_Config *)args[0];
    return scheme_void;
  }
}

static Scheme_Object *namespace_p(int argc, Scheme_Object **argv)
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_namespace_type)) 
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *current_namespace(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-namespace", MZCONFIG_ENV,
			     argc, argv,
			     -1, namespace_p, "namespace", 0);
}

static Scheme_Object *config_branch_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("parameterization-branch-handler", MZCONFIG_CONFIG_BRANCH_HANDLER,
			     argc, argv,
			     0, NULL, NULL, 0);
}

/****************************************/

static int max_configs = __MZCONFIG_BUILTIN_COUNT__;

static int occurs_in_list(Scheme_Object *v, Scheme_Object *l)
{
  Scheme_Object *tl;
  int count = 0;

  for (tl = l; SCHEME_PAIRP(tl); tl = SCHEME_CDR(tl)) {
    if (SAME_OBJ(SCHEME_CAR(tl), v))
      count++;
  }

  return count;
}

static int check_distinct_params(Scheme_Object *l)
{
  int i;
  ParamExtensionRec *erec;
 
  for (i = 0; i < max_configs; i++) {
    if (occurs_in_list(config_map[i], l) > 1)
      return 0;
  }
  
  for (erec = param_ext_recs; erec; erec = erec->next) {
    if (erec->data->p && (occurs_in_list(erec->data->p, l) > 1))
      return 0;
  }
  
  return 1;
}

int scheme_new_param(void)
{
  return max_configs++;
}

static Scheme_Object *default_config_branch_handler(int argc, Scheme_Object **argv)
{
  return scheme_make_config(scheme_config);
}

static Scheme_Config *make_initial_config(void)
{
  Scheme_Config *config;
  int i;

  config = (Scheme_Config *)scheme_malloc_tagged(sizeof(Scheme_Config) + 
						 (max_configs - 1) * sizeof(Scheme_Object*));
  
  config->type = scheme_config_type;
  config->parent = MALLOC_ONE_ATOMIC(Scheme_Config*);
  *config->parent = NULL;
  config->child = MALLOC_ONE_ATOMIC(Scheme_Config*);
  *config->child = NULL;
  config->sibling = MALLOC_ONE_ATOMIC(Scheme_Config*);
  *config->sibling = NULL;

  for (i = 0; i < max_configs; i++)
    config->configs[i] = MALLOC_ONE(Scheme_Object *);

  scheme_set_param(config, MZCONFIG_ENABLE_BREAK, scheme_true);
  scheme_set_param(config, MZCONFIG_ENABLE_EXCEPTION_BREAK, scheme_false);
  scheme_set_param(config, MZCONFIG_CAN_READ_GRAPH, scheme_true);
  scheme_set_param(config, MZCONFIG_CAN_READ_COMPILED, scheme_false);
  scheme_set_param(config, MZCONFIG_CAN_READ_BOX, scheme_true);
  scheme_set_param(config, MZCONFIG_CAN_READ_TYPE_SYMBOL, scheme_true);
  scheme_set_param(config, MZCONFIG_CAN_READ_PIPE_QUOTE, scheme_true);

  scheme_set_param(config, MZCONFIG_PRINT_GRAPH, scheme_false);
  scheme_set_param(config, MZCONFIG_PRINT_STRUCT, scheme_false);
  scheme_set_param(config, MZCONFIG_PRINT_BOX, scheme_true);

  scheme_set_param(config, MZCONFIG_CASE_SENS, (scheme_case_sensitive ? scheme_true : scheme_false));
  scheme_set_param(config, MZCONFIG_SQUARE_BRACKETS_ARE_PARENS, (scheme_square_brackets_are_parens
								 ? scheme_true : scheme_false));
  scheme_set_param(config, MZCONFIG_CURLY_BRACES_ARE_PARENS, (scheme_curly_braces_are_parens
							      ? scheme_true : scheme_false));

  scheme_set_param(config, MZCONFIG_ERROR_PRINT_WIDTH, scheme_make_integer(40));

  scheme_set_param(config, MZCONFIG_CONFIG_BRANCH_HANDLER, scheme_make_prim_w_arity(default_config_branch_handler,
										    "default-parameterization-branch-handler",
										    0, 0));

  REGISTER_SO(main_manager);
  main_manager = scheme_make_manager(NULL);
  scheme_set_param(config, MZCONFIG_MANAGER, (Scheme_Object *)main_manager);

  scheme_set_param(config, MZCONFIG_WILL_EXECUTOR, make_will_executor(0, NULL));

  scheme_set_param(config, MZCONFIG_ALLOW_SET_UNDEFINED, (scheme_allow_set_undefined
							  ? scheme_true
							  : scheme_false));
  scheme_set_param(config, MZCONFIG_COND_AUTO_ELSE, (scheme_allow_cond_auto_else
						     ? scheme_true
						     : scheme_false));

  scheme_set_param(config, MZCONFIG_REQ_LIB_USE_COMPILED, scheme_true);

  scheme_set_param(config, MZCONFIG_REQUIRE_COLLECTION, scheme_false);

  config->extensions = NULL;

  return config;
}

static void adjust_config_family(void *c, void *ignored)
{
  Scheme_Config *config = (Scheme_Config *)c;
  Scheme_Config **p;

  if (*config->parent) {

    p = (*config->parent)->child;
    while (*p && *p != config)
      p = (*p)->sibling;
    
    if (*p)
      *p = *config->sibling;

    while (*p)
      p = (*p)->sibling;

    *p = *config->child;

  }

  p = config->child;
  while (*p) {
    *(*p)->parent = *config->parent;
    p = (*p)->sibling;
  }
}

static Scheme_Object *make_new_config(Scheme_Config *base, Scheme_Object *defshare,
				      Scheme_Object *share_list, Scheme_Object *sharef)
{
  Scheme_Config *config;
  int i;
  
  if (!base)
    base = scheme_config;

  config = (Scheme_Config *)scheme_malloc_tagged(sizeof(Scheme_Config) + 
						 (max_configs - 1) * sizeof(Scheme_Object**));

  config->type = scheme_config_type;
  config->extensions = NULL;

  for (i = 0; i < max_configs; i++) {
    if (defshare) {
      Scheme_Object *r;

      if (occurs_in_list(config_map[i], share_list))
	r = _scheme_apply(sharef, 1, config_map + i);
      else
	r = defshare;

      if (SCHEME_FALSEP(r)) {
	config->configs[i] = MALLOC_ONE(Scheme_Object *);
	*config->configs[i] = *base->configs[i];
      } else if (SCHEME_CONFIGP(r)) {
	Scheme_Config *s = (Scheme_Config *)r;
	config->configs[i] = s->configs[i];
      } else {
	scheme_raise_exn(MZEXN_MISC_PARAMETERIZATION,
			 r,
			 "make-parameterization: sharing procedure returned a non-parameterization: %s",
			 scheme_make_provided_string(r, 1, NULL));
	return NULL;
      }
    } else {
      config->configs[i] = MALLOC_ONE(Scheme_Object *);
      *config->configs[i] = *base->configs[i];
    }
  }

  if (defshare) {
    ParamExtensionRec *erec;

    if (!config->extensions)
      config->extensions = scheme_hash_table(2, SCHEME_hash_weak_ptr, 0, 0);

    for (erec = param_ext_recs; erec; erec = erec->next) {
      if (erec->data->p) {
	Scheme_Object *r, *defval;
	unsigned long key;
	
	key = erec->data->key;
	defval = ((ParamData *)((Scheme_Closed_Primitive_Proc *)erec->data->p)->data)->defval;

	if (occurs_in_list(erec->data->p, share_list)) {
	  Scheme_Object *p = erec->data->p;
	  r = _scheme_apply(sharef, 1, &p);
	} else
	  r = defshare;
	
	if (!SCHEME_FALSEP(r)) {
	  if (SCHEME_CONFIGP(r)) {
	    Scheme_Config *s = (Scheme_Config *)r;
	    Scheme_Bucket *b;

	    if (!s->extensions)
	      s->extensions = scheme_hash_table(2, SCHEME_hash_weak_ptr, 0, 0);

	    b = scheme_bucket_from_table(s->extensions, (const char *)key);
	    if (!b->val)
	      b->val = defval;

	    scheme_add_bucket_to_table(config->extensions, b);
	  } else {
	    scheme_raise_exn(MZEXN_MISC_PARAMETERIZATION,
			     r,
			     "make-parameterization: "
			     "sharing procedure returned a non-parameterization: %s",
			     scheme_make_provided_string(r, 1, NULL));
	    return NULL;
	  }
	} else if (!SCHEME_FALSEP(defshare))
	  scheme_add_to_table(config->extensions, (const char *)key, defval, 0);
      }
    }
  }

  config->parent = MALLOC_ONE_ATOMIC(Scheme_Config*);
  *config->parent = NULL;
  config->child = MALLOC_ONE_ATOMIC(Scheme_Config*);
  *config->child = NULL;
  config->sibling = MALLOC_ONE_ATOMIC(Scheme_Config*);
  *config->sibling = NULL;

  if (defshare && !SCHEME_FALSEP(defshare)) {
    Scheme_Config *d = (Scheme_Config *)defshare;
    *config->sibling = *d->child;
    *d->child = config;
    *config->parent = d;
  }

  scheme_add_finalizer(config, adjust_config_family, NULL);

  return (Scheme_Object *)config;
}

Scheme_Object *scheme_make_config(Scheme_Config *base)
{
  return make_new_config(base, NULL, NULL, NULL);
}

Scheme_Object *scheme_register_parameter(Scheme_Prim *function, char *name, int which)
{
  Scheme_Object *o;

  if (!config_map)
    config_map = MALLOC_N(Scheme_Object*, max_configs);

  if (config_map[which])
    return config_map[which];

  o = scheme_make_prim_w_arity(function, name, 0, 1);
  ((Scheme_Primitive_Proc *)o)->flags |= SCHEME_PRIM_IS_PARAMETER;

  config_map[which] = o;

  return o;
}

static Scheme_Bucket *get_ext_bucket(Scheme_Config *config, const char *key, Scheme_Object *defval)
{
  Scheme_Config *c = config;
  Scheme_Bucket *b;
  
  while (1) {
    b = scheme_bucket_or_null_from_table(c->extensions, key, 0);
    if (b) {
      if (c != config)
	scheme_add_bucket_to_table(config->extensions, b);
      
      return b;
    } else if (*c->parent)
      c = *c->parent;
    else
      break;
  }

  b = scheme_bucket_from_table(c->extensions, key);
  b->val = (char *)defval;
  if (c != config)
    scheme_add_bucket_to_table(c->extensions, b);

  return b;
}

Scheme_Object *scheme_param_config(char *name, long pos,
				   int argc, Scheme_Object **argv,
				   int arity,
				   /* -2 => user paramter; pos is (cons key defval)
				      -1 => try check, etc. 
				      0+ => check argument for this arity */
				   Scheme_Object *(*check)(int, Scheme_Object **), char *expected,
				   int isboolorfilter)
{
  int set = 0;
  Scheme_Config *config;

  if (argc < 2) {
    config = scheme_config;
    if (argc == 1)
      set = 1;
  } else {
    config = (Scheme_Config *)argv[1];
    if (argv[0]) {
      set = 1;
      argc = 1;
    } else {
      argc = 0;
    }
  }

  if (!set) {
    if (arity == -2) {
      Scheme_Object *defval = SCHEME_CDR((Scheme_Object *)pos);
      if (config->extensions) {
	const char *key = (const char *)SCHEME_CAR((Scheme_Object *)pos);
	Scheme_Bucket *b;
	
	b = get_ext_bucket(config, key, defval);
	return (Scheme_Object *)b->val;
      }
      return defval;
    } else
      return scheme_get_param(config, pos);
  } else {
    Scheme_Object *naya = argv[0];

    if (arity != -2) {
      if (arity < 0) {
	if (check) {
	  Scheme_Object *r = check(1, argv);
	  
	  if (!isboolorfilter && SCHEME_FALSEP(r))
	    r = NULL;
	  
	  if (!r) {
	    scheme_wrong_type(name, expected, 0, argc, argv);
	    return NULL;
	  }
	  
	  if (isboolorfilter)
	    naya = r;
	}
      } else 
	scheme_check_proc_arity(name, arity, 0, argc, argv);

      if (isboolorfilter && !check)
	scheme_set_param(config, pos, ((SCHEME_TRUEP(naya)) ? scheme_true : scheme_false));
      else
	scheme_set_param(config, pos, naya);
    } else {
      const char *key = (const char *)SCHEME_CAR((Scheme_Object *)pos);
      Scheme_Bucket *b;

      if (!config->extensions)
	config->extensions = scheme_hash_table(2, SCHEME_hash_weak_ptr, 0, 0);
      
      b = get_ext_bucket(config, key, NULL);
      b->val = naya;
    }
  
    return scheme_void;
  }
}

Scheme_Env *scheme_get_env(Scheme_Config *c)
{
  Scheme_Object *o = scheme_get_param(c, MZCONFIG_ENV);
  return (Scheme_Env *)o;
}

/****************************************/

static void activate_will(void *o, void *data) 
{
  WillRegistration *r = (WillRegistration *)data;
  ActiveWill *a;
  WillExecutor *w;
    
  a = MALLOC_ONE(ActiveWill);
  a->o = (Scheme_Object *)o;
  a->proc = r->proc;
  
  GET_WILL_LOCK();
  w = r->w;
  if (w->last)
    w->last->next = a;
  else
    w->first = a;
  w->last = a;
  RELEASE_WILL_LOCK();
}

static Scheme_Object *now_do_will(void *v)
{
  ActiveWill *a = (ActiveWill *)v;
  Scheme_Object *o[1];

  o[0] = a->o;
  a->o = NULL;

  return scheme_apply(a->proc, 1, o);
}

static void post_will(void *v)
{
  ActiveWill *a = (ActiveWill *)v;

  a->w->running = 0;
}

static Scheme_Object *do_next_will(WillExecutor *w, int tail)
{
  ActiveWill *a;

  GET_WILL_LOCK();
  if (w->running || !w->first) {
    RELEASE_WILL_LOCK();
    return scheme_false;
  }
  a = w->first;
  w->first = a->next;
  if (!w->first)
    w->last = NULL;
  w->running = 1;
  RELEASE_WILL_LOCK();
  
  a->w = w;

  return scheme_dynamic_wind(NULL, now_do_will, post_will, NULL, a);
}

static Scheme_Object *current_will_executor(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-will-executor", MZCONFIG_WILL_EXECUTOR,
			     argc, argv,
			     -1, will_executor_p, "will-executor", 0);
}

static Scheme_Object *make_will_executor(int argc, Scheme_Object **argv)
{
  WillExecutor *w = MALLOC_ONE_TAGGED(WillExecutor);

  w->type = scheme_will_executor_type;
  w->first = NULL;
  w->last = NULL;
  w->running = 0;

  return (Scheme_Object *)w;
}

static Scheme_Object *will_executor_p(int argc, Scheme_Object **argv)
{
  return ((SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_will_executor_type)) 
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *register_will(int argc, Scheme_Object **argv)
{
  WillRegistration *r;

  scheme_check_proc_arity("register-will", 1, 1, argc, argv);
  if ((argc > 2) && NOT_SAME_TYPE(SCHEME_TYPE(argv[2]), scheme_will_executor_type))
    scheme_wrong_type("register-will", "will-executor", 2, argc, argv);

  r = MALLOC_ONE(WillRegistration);
  r->proc = argv[1];
  r->w = (WillExecutor *)((argc > 2)
			  ? argv[2]
			  : scheme_get_param(scheme_config, MZCONFIG_WILL_EXECUTOR));

  scheme_add_scheme_finalizer(argv[0], activate_will, (void *)r);

  return scheme_void;
}

static Scheme_Object *will_executor_try(int argc, Scheme_Object **argv)
{
  WillExecutor *w;

  if (NOT_SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_will_executor_type))
    scheme_wrong_type("will-executor-try", "will-executor", 0, argc, argv);
  
  w = (WillExecutor *)argv[0];

  if (w->first && !w->running)
    return do_next_will(w, 1);
  else
    return scheme_false;
}

/****************************************/

#ifdef MZ_REAL_THREADS

/* Implementing support for new thread systems:

   MzScheme needs implementations for the following functions (usually
   these are macros mapped to actual function names in sconfig.h):

   void* SCHEME_INIT_THREADS(void) - initializes thread system, returns
     the id of the initial (main) thread

   void SCHEME_CREATE_THREAD(void (*f)(void*), void* data, 
     unsigned long *slimit, void** thp) - starts a new thread, applying `f' 
     to `data' in the new thread. BEFORE `f' is applied to `data', the id 
     for the new thread must be in `*thp'. If Unix-style stack-checking is 
     used, the hottest allowable stack address (with the safely margin 
     already removed) must be put into `slimit' before `f' is applied; 
     `slimit' may be NULL, in which case the stack limit is not needed.

   void SCHEME_EXIT_THREAD() - exits the current thread

   void SCHEME_BREAK_THREAD(void* th) - signals a break in the thread `th'.
     If `th' is waiting on a semaphore or selecting a file descriptor, this
     breaking signal must cause that wait or select to break.

   void SCHEME_SET_CURRENT_PROCESS(Scheme_Process* p) - stores `p' as the
     Scheme thread pointer for the current thread.

   Scheme_Process* SCHEME_GET_CURRENT_PROCESS() - retrieves the Scheme thread
     pointer for the current thread.

   void* SCHEME_MAKE_MUTEX(int init) - creates a new mutex.

   void SCHEME_FREE_MUTEX(void* m) - destroys the mutex `m'.

   void SCHEME_LOCK_MUTEX(void* m) - locks the mutex `m', blocking until
     it is available.

   void SCHEME_UNLOCK_MUTEX(void* m) - unlocks the mutex `m'.

   int SCHEME_SEMA_DOWN_BREAKABLE(void* s) - waits on `s' but allows the
     wait to be terminated by a break, returing 1 if the wait was sucessful
     (i.e., no break occurred) or 0 if unsuccessful.

   void* SCHEME_MAKE_SEMA(int init) - creates a new semaphore with initial
     count `init'.

   void SCHEME_FREE_SEMA(void* s) - destroys the semaphore `s'.

   int SCHEME_SEMA_UP(void* s) - posts to the semaphore `s', returning 0 if
     an error occurred.

   int SCHEME_SEMA_DOWN_BREAKABLE(void* s) - waits on `s' but allows the
     wait to be terminated by a break, returing 1 if the wait was sucessful
     (i.e., no break occurred) or 0 if unsuccessful.

   int SCHEME_SEMA_TRY_DOWN(void* s) - attempts a non-blocking wait on the
     semaphore `s', immediately returning 1 if the wait was successful or
     0 if unsuccessful.
*/

void scheme_real_sema_down(void *sema)
{
  if (!SCHEME_SEMA_DOWN_BREAKABLE(sema)) {
    Scheme_Process *p = scheme_current_process;
    do {
      scheme_process_block_w_process(0, p);
    } while (!SCHEME_SEMA_DOWN_BREAKABLE(sema));
  }
}

#endif

/****************************************/

#ifdef MZ_USE_SOLARIS_THREADS

static thread_key_t cp_key;
typedef struct {
  void (*f)(void *);
  void *data;
  void *stack;
} solaris_closure;

void *scheme_solaris_init_threads(void)
{
  if (thr_keycreate(&cp_key, NULL)) {
    printf("init failed\n");
    exit(-1);
  }

  return (void *)thr_self();
}

static void do_nothing(int ignored)
{
}

static void *start_solaris_thread(void *cl)
{
  sigset(SIGINT, do_nothing);

  ((solaris_closure *)cl)->f(((solaris_closure *)cl)->data);

  return NULL;
}

void scheme_solaris_create_thread(void (*f)(void *), void *data, 
				  unsigned long *stackend, void **thp)
{
  void *stack;
  size_t size;
  thread_t naya;
  solaris_closure *cl = scheme_malloc(sizeof(solaris_closure));

  cl->f = f;
  cl->data = data;

  if (stackend) {
    size = 2 * STACK_SAFETY_MARGIN;
    if (thr_min_stack() > size)
      size = thr_min_stack();
    stack = scheme_malloc_atomic(size);
  
    *stackend = ((unsigned long)stack) + STACK_SAFETY_MARGIN;
  } else {
    stack = NULL;
    size = 0;
  }

  cl->stack = stack; /* weird trick: as long as the stack is needed,
			cl will be on it, so let GC decide whether 
			the stack is active and therefore whether
			"stack" is still active. */

  thr_create(stack, size,
	     start_solaris_thread, (void *)cl, 
	     THR_SUSPENDED, &naya);

  *thp = (void *)naya;
  
  thr_continue(naya);
}

void scheme_solaris_exit_thread()
{
  thr_exit(0);
}

void scheme_solaris_break_thread(void *th)
{
  thr_kill((thread_t)th, SIGINT);
}

Scheme_Process *scheme_solaris_get_current_process()
{
  Scheme_Process *p;

  thr_getspecific(cp_key, (void **)&p);

  return p;
}

void scheme_solaris_set_current_process(Scheme_Process *p)
{
  thr_setspecific(cp_key, (void *)p);
}

void *scheme_solaris_make_mutex(int v)
{
  mutex_t *m;
  m = (mutex_t *)scheme_malloc_atomic(sizeof(mutex_t));
  mutex_init(m, USYNC_THREAD, NULL);

  return (void *)m;
}

void scheme_solaris_free_mutex(void *m)
{
  mutex_destroy(m);
}

void scheme_solaris_lock_mutex(void *m)
{
  mutex_lock(m);
}

void scheme_solaris_unlock_mutex(void *m)
{
  mutex_unlock(m);
}

void *scheme_solaris_make_semaphore(int v)
{
  sema_t *s;
  s = (sema_t *)scheme_malloc_atomic(sizeof(sema_t));
  sema_init(s, v, USYNC_THREAD, NULL);

  return (void *)s;
}

void scheme_solaris_free_semaphore(void *s)
{
  sema_destroy(s);
}

int scheme_solaris_semaphore_up(void *s)
{
  return !sema_post((sema_t *)s);
}

int scheme_solaris_semaphore_down_breakable(void *s)
{
  return !sema_wait((sema_t *)s);
}

int scheme_solaris_semaphore_try_down(void *s)
{
  return !sema_trywait((sema_t *)s);
}

#endif /* MZ_USE_SOLARIS_THREADS */

/****************************************************************/

#ifdef MZ_USE_WIN32_THREADS

typedef struct {
  long th;
  HANDLE break_sema;
} Win32SchemeThread;

typedef struct {
  void (*f)(void *);
  void *data;
  void *stack;
  HANDLE stack_set;
  HANDLE thread_go;
} thread_closure;

static DWORD tls;

void *scheme_win32_init_threads(void)
{
  Win32SchemeThread *th = MALLOC_ONE_ATOMIC(Win32SchemeThread);

  tls = TlsAlloc();

  th->th = 0;
  th->break_sema = NULL; /* use scheme_break_semaphore */

  return th;
}

static unsigned __stdcall run_win32_thread(void *data)
{
  long dummy;
  thread_closure *cl = (thread_closure *)data;

  cl->stack = (void *)&dummy;
  
# ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
  DllMain(NULL, DLL_THREAD_ATTACH, NULL);
# endif

  ReleaseSemaphore(cl->stack_set, 1, NULL);
  WaitForSingleObject(cl->thread_go, INFINITE);

  cl->f(cl->data);

# ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
  DllMain(NULL, DLL_THREAD_DETACH, NULL);
# endif

  return 0;
}

static void free_win32_break_sema(void *th, void *ignored)
{
  CloseHandle(((Win32SchemeThread *)th)->break_sema);
}

void scheme_win32_create_thread(void (*f)(void *), void *data, 
				unsigned long *stackend, void **thp)
{
  Win32SchemeThread *th = MALLOC_ONE_ATOMIC(Win32SchemeThread);
  thread_closure *cl = MALLOC_ONE(thread_closure);
  unsigned id;

  cl->f = f;
  cl->data = data;

  th->break_sema = CreateSemaphore(NULL, 0, 1, NULL);
  cl->thread_go = th->break_sema;
  cl->stack_set = stackend ? CreateSemaphore(NULL, 0, 1, NULL) : NULL;

  th->th = _beginthreadex(NULL, 0, run_win32_thread, cl, 0, &id);

  *thp = (void *)th;

  if (stackend) {
    WaitForSingleObject(cl->stack_set, INFINITE);

# ifdef WINDOWS_FIND_STACK_BOUNDS
    *stackend = ((unsigned long)cl->stack + (STACK_SAFETY_MARGIN - 0x100000));
# else
    >> not implemented <<
# endif

    CloseHandle(cl->stack_set);
  }

  ReleaseSemaphore(cl->thread_go, 1, NULL);

  ResumeThread((HANDLE)th->th);

  scheme_add_finalizer(th, free_win32_break_sema, NULL);

# ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
  DllMain(NULL, DLL_THREAD_DETACH, NULL);
# endif
}

void scheme_win32_exit_thread()
{
# ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
  DllMain(NULL, DLL_THREAD_DETACH, NULL);
# endif

  _endthreadex(0);
}

void *scheme_win32_get_break_semaphore(void *th)
{
  HANDLE s;

  s = ((Win32SchemeThread *)th)->break_sema;
  if (!s)
    s = scheme_break_semaphore;

  return (void *)s;
}

void scheme_win32_break_thread(void *th)
{
  HANDLE s = (HANDLE)scheme_win32_get_break_semaphore(th);

  ReleaseSemaphore(s, 1, NULL);
}

struct Scheme_Process *scheme_win32_get_current_process()
{
  return (Scheme_Process *)TlsGetValue(tls);
}

void scheme_win32_set_current_process(struct Scheme_Process *p)
{
  TlsSetValue(tls, (LPVOID)p);
}

void *scheme_win32_make_semaphore(int init)
{
  return (void *)CreateSemaphore(NULL, init, 256, NULL);
}

void scheme_win32_free_semaphore(void *s)
{
  CloseHandle((HANDLE)s);
}

int scheme_win32_semaphore_up(void *s)
{
  return ReleaseSemaphore((HANDLE)s, 1, NULL);
}

int scheme_win32_semaphore_down_breakable(void *s)
{
  int c;
  HANDLE a[2];

  a[0] = (HANDLE)s;
  a[1] = scheme_win32_get_break_semaphore((Win32SchemeThread *)scheme_current_process->thread);

  c = !a[1] ? 1 : 2;

  return WaitForMultipleObjects(c, a, FALSE, INFINITE) == WAIT_OBJECT_0;
}

int scheme_win32_semaphore_try_down(void *s)
{
  return !WaitForSingleObject((HANDLE)s, 0);
}

#endif /* MZ_USE_WIN32_THREADS */

/****************************************************************/

#ifdef MZ_USE_IRIX_SPROCS

/* Irix sprocs implementation contributed by Fernando D. Mato Mira */
/* 
   NOTE TO IMPLEMENTORS:
   IRIX semaphores are awfully slow to awake a process. 
   It's better to use ulocks or abilocks, if a count is not really needed.
   There're are macros in semaphores.h that homogeneize the interfaces
   for locking, eg:

   SLOW:   slock --> usema_t
   MEDIUM: ulock --> ulock_t   (good response)
   QUICK:  qlock --> abilock_t (eats a bit of CPU time, 'cause it spins/nanosleeps)

   --fdmm
*/

#include <ulocks.h>

static thread_key_t cp_key;
typedef struct {
  void (*f)(void *);
  void *data;
#ifdef MZ_PRIVATE_SPAWN_MUTEX
  mutex_t *mutex;
#endif
  void *stack;
} sproc_closure;

void *scheme_sproc_init_threads(void)
{
  if (thr_keycreate(&cp_key, NULL)) {
    printf("init failed\n");
    exit(-1);
  }

  return (void *)getpid(); /* retrurn the main thread (= current thread) */
}

static void do_nothing(int ignored)
{
}

static void start_sproc_thread(void *cl, size_t ignored)
{
  sproc_closure *scl = (sproc_closure *)cl;

  sigset(SIGINT, do_nothing);

  /* wait until `*stackend' and `*thp' have been set. */
#ifdef MZ_PRIVATE_SPAWN_MUTEX
  qlock_1(scl->mutex, "scheme_sproc_mutex_down");
  freemutex(scl->mutex);
#else
  /* (parent does not release the allocation lock */
  /*  until that is done) */
  sproc_lock();
  sproc_unlock();
#endif

  scl->f(scl->data);
}

void scheme_sproc_create_thread(void (*f)(void *), void *data, 
				unsigned long *stackend, void **thp)
{
  int naya;
  void *stack;
  size_t size;
  sproc_closure *cl = scheme_malloc(sizeof(sproc_closure));

  cl->f = f;
  cl->data = data;

  if (stackend) {
    size = 2 * STACK_SAFETY_MARGIN;
    stack = scheme_malloc(size); /*scheme_malloc_atomic(size);*/
    
    *stackend = ((unsigned long)stack) + STACK_SAFETY_MARGIN;
  } else {
    stack = NULL;
    size = 0;
  }

  cl->stack = stack; /* weird trick: as long as the stack is needed,
			cl will be on it, so let GC decide whether 
			the stack is active and therefore whether
			"stack" is still active. */
#ifdef MZ_PRIVATE_SPAWN_MUTEX
#define SPROCSP sprocsp
  cl->mutex = newmutex();
#else
#define SPROCSP GC_sprocsp_
  /* I'll deadlock if I call GC-safe unblockproc() before   */
  /* GC-safe sproc()  completes its housekeeping.           */
  /* Worse, because of sproc_lock() a process that tries to */
  /* GC-safely blockproc() before it gets completely        */
  /* unblockproc'ed will also cause a deadlock.             */

  /* Solution: we call sprocsp_(), which is GC_safe, but   */
  /*   does not release the allocation lock.               */
  /*   We could use _GC_unblockproc() then, but it's less  */
  /*   redundant to just wait for this lock in the child   */ 
  /*   Hopefully, this will not cause much more contention */
  /*   with other threads than if we used a separate lock  */
  /*   just for this.      */
#endif

  naya = SPROCSP (start_sproc_thread, PR_SALL, (void *)cl, 
		 (caddr_t)(((char *)stack) + size), 0);

  *thp = (void *)naya;

  /* let thread run, now */
#ifdef MZ_PRIVATE_SPAWN_MUTEX
  qunlock_1(cl->mutex, "scheme_sproc_mutex_up");
#else
  sproc_unlock();
#endif

}

void scheme_sproc_exit_thread()
{
  exit(0);
}

void scheme_sproc_break_thread(void *th)
{
  kill((int)th, SIGINT);
}

Scheme_Process *scheme_sproc_get_current_process()
{
  Scheme_Process *p;

  thr_getspecific(cp_key, (void **)&p);

  return p;
}

void scheme_sproc_set_current_process(Scheme_Process *p)
{
  thr_setspecific(cp_key, (void *)p);
}

void *scheme_sproc_make_semaphore(int v)
{
  usema_t *s = usnewsema(semArena, v);

  return (void *)s;
}

void scheme_sproc_free_semaphore(void *s)
{
  usfreesema(s, semArena);
}

#  ifdef MZ_TASKS
--> Semaphore/lock routines should be revised
#  endif

int scheme_sproc_semaphore_up(void *s)
{

  return !seminc((usema_t *)s,"scheme_sproc_semaphore_up");
}

int scheme_sproc_semaphore_down_breakable(void *s)
{
  return (semdec((sema_t *)s,"scheme_sproc_semaphore_down") == 1);
}

int scheme_sproc_semaphore_try_down(void *s)
{
  return (uscpsema((usema_t *)s) == 1);
}

void *scheme_sproc_make_mutex()
{
  mutex_t *s = usnewlock(semArena);

  return (void *)s;
}

void scheme_sproc_free_mutex(void *s)
{
  usfreelock(s, semArena);
}

int scheme_sproc_mutex_up(void *s)
{

  return (!uunlock((mutex_t *)s));
}

int scheme_sproc_mutex_down_breakable(void *s)
{
  return (ulock((mutex_t *)s));
}

int scheme_sproc_mutex_try_down(void *s)
{
  return (utrylock((mutex_t *)s));
}

#endif /* MZ_USE_IRIX_SPROCS */

/****************************************************************/

/* Special stack handling for GC when multiple stacks are used. */
/* Must define NO_AUTO_STACK_PUSH for compiling GC */

#ifdef MANUAL_STACK_PUSH

extern void GC_push_all_stack(unsigned long b, unsigned long t);
extern unsigned long GC_stackbottom;

static void push_process_stack(Scheme_Process *p, unsigned long dummy)
{
  unsigned long b, t;
  
  if (p == scheme_current_process)
    t = dummy;
  else
    t = (unsigned long)p->stack_current;
  
  if (p->next) {
	 b = (unsigned long)p->stack_start;
	 if (!b) return; /* process still being initialized */
  } else
	 b = GC_stackbottom;

  if (b < t)
    GC_push_all_stack(b, t);
  else
    GC_push_all_stack(t, b);
}

void GC_push_all_stacks()
{
  unsigned long dummy;
  Scheme_Process *p = NULL;

  dummy = (unsigned long)&dummy;

  if (!leaving_process &&
		(!scheme_current_process || !scheme_first_process->next)) {
	 /* Not yet running MzScheme or just one thread: */
	 if (dummy < GC_stackbottom)
		GC_push_all_stack(dummy, GC_stackbottom);
	 else
		GC_push_all_stack(GC_stackbottom, dummy);

	 return;
  }

  for (p = scheme_first_process; p; p = p->next)
	 push_process_stack(p, dummy);

  if (leaving_process)
	 push_process_stack(leaving_process, dummy);
}

#endif

#ifndef MZ_REAL_THREADS
extern GC_push_all_stack(void *, void *);
static void push_other_stacks(void)
{
  Scheme_Process *p;
  Scheme_Overflow *o;
  
  /* Explicitly push saved stacks. This is needed
     because the saved stack may contain pointers to the
     interior of collectable objects. */

#define PUSH_JMPUPBUF(b) { if ((b).stack_size) GC_push_all_stack((b).stack_copy, ((char *)(b).stack_copy) + (b).stack_size); }

  p = scheme_first_process;
  while (p) {
    if (p != scheme_current_process)
      PUSH_JMPUPBUF(p->jmpup_buf);
    for (o = p->overflow; o; o = o->prev)
      PUSH_JMPUPBUF(o->cont);

    p = p->next;
  }
}
#endif
