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
#ifdef UNISTD_INCLUDE
# include <unistd.h>
#endif
#ifdef USE_ULIMIT
# include <ulimit.h>
#endif
#ifdef FILES_HAVE_FDS
# include <sys/types.h>
# include <sys/time.h>
# ifdef BSTRING_INCLUDE
#  include <bstring.h>
# endif
# ifdef SELECT_INCLUDE
#  include <sys/select.h>
# endif
# ifdef USE_BEOS_SOCKET_INCLUDE
#  include <be/net/socket.h>
# endif
#endif
#if defined(UNIX_PROCESSES)
# include <signal.h>
# include <sys/types.h>
# include <sys/wait.h>
/* # include <sys/resource.h> */
#endif
#ifdef IO_INCLUDE
# include <io.h>
#endif
#ifdef USE_BEOS_SNOOZE
# include <be/kernel/OS.h>
#endif
#ifdef BEOS_PROCESSES
# include <be/kernel/image.h>
#endif
#ifdef NO_ERRNO_GLOBAL
static int mzerrno = 0;
# define errno mzerrno
#else
# include <errno.h>
#endif
#ifndef DONT_IGNORE_PIPE_SIGNAL
# include <signal.h>
#endif
#ifdef USE_OSKIT_CONSOLE
# ifndef OSKIT_TEST
#  include <x86/pc/direct_cons.h> 
# endif
#endif
#ifdef INCLUDE_OSKIT_SOCKET
# include <oskit/net/socket.h>
#endif
#include <math.h> /* for fmod , used by default_sleep */
#include "schfd.h"

#if defined(WINDOWS_PROCESSES) || defined(DETECT_WIN32_CONSOLE_STDIN)
static void init_thread_memory();
# ifdef NO_STDIO_THREADS 
#  undef DETECT_WIN32_CONSOLE_STDIN
# else
#  define WIN32_FD_HANDLES
#  include <process.h>
#  include <signal.h>
#  include <io.h>
#  define OS_SEMAPHORE_TYPE HANDLE
#  define OS_MUTEX_TYPE CRITICAL_SECTION
#  define OS_THREAD_TYPE HANDLE
# endif
#endif

#ifdef USE_BEOS_PORT_THREADS
# include <be/kernel/OS.h>
# define OS_SEMAPHORE_TYPE sem_id
typedef struct { int32 v; sem_id s; } mutex_id;
# define OS_MUTEX_TYPE mutex_id
# define OS_THREAD_TYPE thread_id
#endif

typedef struct Scheme_Indexed_String {
  MZTAG_IF_REQUIRED
  char *string;
  int size;
  int index;
  union { 
    int hot; /* output port */
    int pos; /* input port */
  } u;
} Scheme_Indexed_String;

typedef struct {
  MZTAG_IF_REQUIRED
  FILE *f;
  int regfile;
} Scheme_Input_File;

typedef struct {
  MZTAG_IF_REQUIRED
  FILE *f;
} Scheme_Output_File;

#if defined(USE_UNIX_SOCKETS_TCP) || defined(USE_WINSOCK_TCP)
# define USE_SOCKETS_TCP
#endif

#ifdef USE_MAC_TCP
# include <MacTCP.h>
# include <dnr.c>
#endif

#ifdef USE_UNIX_SOCKETS_TCP
# include <netinet/in.h>
# include <netdb.h>
# include <sys/socket.h>
# include <fcntl.h>
# define TCP_SOCKSENDBUF_SIZE 32768
# ifdef USE_FCNTL_O_NONBLOCK
#  define TCP_NONBLOCKING O_NONBLOCK
# else
#  define TCP_NONBLOCKING FNDELAY
# endif
#endif

#if defined(WIN32_FD_HANDLES) || defined(WINDOWS_PROCESSES)
# include <windows.h>
#endif
#ifdef WINDOWS_PROCESSES
# include <ctype.h>
#endif

#ifdef USE_WINSOCK_TCP
# include <winsock.h>
struct SOCKADDR_IN {
  short sin_family;
  unsigned short sin_port;
  struct in_addr sin_addr;
  char sin_zero[8];
};
#endif

#ifdef USE_TCP

#ifdef USE_MAC_TCP
# define TCP_BUFFER_SIZE 16384
#else
# define TCP_BUFFER_SIZE 512
#endif

#ifdef USE_UNIX_SOCKETS_TCP
typedef int tcp_t;
# define INVALID_SOCKET (-1)
#define closesocket close
#endif

#ifdef USE_WINSOCK_TCP
typedef SOCKET tcp_t;
#endif

#ifdef USE_SOCKETS_TCP
typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  tcp_t s;
  Scheme_Manager_Reference *mref;
} listener_t;
#endif

#ifdef USE_MAC_TCP
typedef struct {
  void *create_pb;
  void *current_pb; /* prevents GC during async call */
  StreamPtr stream;
  int state;
  int async_errid;
  Scheme_Object *lock; /* read lock */
} tcp_t;

typedef struct {
  Scheme_Type type; 
  MZ_HASH_KEY_EX
  int portid;
  int count;
  struct Scheme_Tcp **datas;
  Scheme_Manager_Reference *mref;
} listener_t;
# define htons(x) x
#endif

typedef struct Scheme_Tcp {
  MZTAG_IF_REQUIRED
  tcp_t tcp;
  int refcount;
  char buffer[TCP_BUFFER_SIZE];
  short bufpos, bufmax;
  short hiteof;
#ifdef USE_MAC_TCP
  TCPiopb *activeRcv;
#endif
} Scheme_Tcp;

#endif

#if defined(UNIX_PROCESSES)
/* For process & system: */
typedef struct System_Child {
  MZTAG_IF_REQUIRED
  pid_t id;
  short done;
  int status;
  struct System_Child *next;
} System_Child;
#endif

#ifdef USE_FD_PORTS
# include <fcntl.h>
# ifdef USE_FCNTL_O_NONBLOCK
#  define FD_NONBLOCKING O_NONBLOCK
# else
#  define FD_NONBLOCKING FNDELAY
# endif
# define MZPORT_FD_BUFFSIZE 2048
typedef struct Scheme_FD {
  MZTAG_IF_REQUIRED
  int fd;
  int bufcount, buffpos;
  unsigned char buffer[MZPORT_FD_BUFFSIZE];
} Scheme_FD;
#endif

/* globals */
Scheme_Object scheme_eof[1];
Scheme_Object *scheme_orig_stdout_port;
Scheme_Object *scheme_orig_stderr_port;
Scheme_Object *scheme_orig_stdin_port;
Scheme_Object *scheme_write_proc, *scheme_display_proc, *scheme_print_proc;

Scheme_Object *(*scheme_make_stdin)(void) = NULL;
Scheme_Object *(*scheme_make_stdout)(void) = NULL;
Scheme_Object *(*scheme_make_stderr)(void) = NULL;

int scheme_file_open_count;

int scheme_internal_checking_char;

/* locals */
#ifdef USE_FD_PORTS
static Scheme_Object *fd_input_port_type;
#endif
#ifdef USE_OSKIT_CONSOLE
static Scheme_Object *oskit_console_input_port_type;
#endif
static Scheme_Object *file_input_port_type;
static Scheme_Object *string_input_port_type;
#ifdef USE_TCP
static Scheme_Object *tcp_input_port_type;
#endif
#ifdef USE_FD_PORTS
static Scheme_Object *fd_output_port_type;
#endif
static Scheme_Object *file_output_port_type;
static Scheme_Object *string_output_port_type;
#ifdef USE_TCP
static Scheme_Object *tcp_output_port_type;
#endif
static Scheme_Object *user_input_port_type;
static Scheme_Object *user_output_port_type;
static Scheme_Object *pipe_read_port_type;
static Scheme_Object *pipe_write_port_type;
#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
static Scheme_Object *tested_file_input_port_type;
#endif

static Scheme_Object *text_symbol, *binary_symbol;
static Scheme_Object *append_symbol, *error_symbol;
static Scheme_Object *replace_symbol, *truncate_symbol, *truncate_replace_symbol;

static Scheme_Object *any_symbol, *any_one_symbol;
static Scheme_Object *cr_symbol, *lf_symbol, *crlf_symbol;

static Scheme_Object *all_symbol, *non_elaboration_symbol, *none_symbol;

#ifdef USE_MAC_TCP
static int num_tcp_send_buffers = 0;
static void **tcp_send_buffers;
#endif

#if defined(UNIX_PROCESSES)
System_Child *scheme_system_children;
#endif

/* generic ports */

static Scheme_Object *input_port_p (int, Scheme_Object *[]);
static Scheme_Object *output_port_p (int, Scheme_Object *[]);
static Scheme_Object *current_input_port (int, Scheme_Object *[]);
static Scheme_Object *current_output_port (int, Scheme_Object *[]);
static Scheme_Object *current_error_port (int, Scheme_Object *[]);
static Scheme_Object *make_input_port (int, Scheme_Object *[]);
static Scheme_Object *make_output_port (int, Scheme_Object *[]);
static Scheme_Object *open_input_file (int, Scheme_Object *[]);
static Scheme_Object *open_output_file (int, Scheme_Object *[]);
static Scheme_Object *close_input_port (int, Scheme_Object *[]);
static Scheme_Object *close_output_port (int, Scheme_Object *[]);
static Scheme_Object *call_with_output_file (int, Scheme_Object *[]);
static Scheme_Object *call_with_input_file (int, Scheme_Object *[]);
static Scheme_Object *with_input_from_file (int, Scheme_Object *[]);
static Scheme_Object *with_output_to_file (int, Scheme_Object *[]);
static Scheme_Object *read_f (int, Scheme_Object *[]);
static Scheme_Object *read_char (int, Scheme_Object *[]);
static Scheme_Object *read_line (int, Scheme_Object *[]);
static Scheme_Object *read_string (int, Scheme_Object *[]);
static Scheme_Object *read_string_bang (int, Scheme_Object *[]);
static Scheme_Object *peek_char (int, Scheme_Object *[]);
static Scheme_Object *eof_object_p (int, Scheme_Object *[]);
static Scheme_Object *char_ready_p (int, Scheme_Object *[]);
static Scheme_Object *sch_write (int, Scheme_Object *[]);
static Scheme_Object *display (int, Scheme_Object *[]);
static Scheme_Object *print (int, Scheme_Object *[]);
static Scheme_Object *newline (int, Scheme_Object *[]);
static Scheme_Object *write_char (int, Scheme_Object *[]);
static Scheme_Object *load (int, Scheme_Object *[]);
static Scheme_Object *current_load (int, Scheme_Object *[]);
static Scheme_Object *current_load_directory(int argc, Scheme_Object *argv[]);
static Scheme_Object *default_load (int, Scheme_Object *[]);
static Scheme_Object *use_compiled_kind(int, Scheme_Object *[]);
static Scheme_Object *current_require_relative_collection(int, Scheme_Object *[]);
static Scheme_Object *transcript_on(int, Scheme_Object *[]);
static Scheme_Object *transcript_off(int, Scheme_Object *[]);
/* non-standard */
static Scheme_Object *flush_output (int, Scheme_Object *[]);
static Scheme_Object *file_position (int, Scheme_Object *[]);
static Scheme_Object *open_input_string (int, Scheme_Object *[]);
static Scheme_Object *open_output_string (int, Scheme_Object *[]);
static Scheme_Object *get_output_string (int, Scheme_Object *[]);
static Scheme_Object *sch_pipe(int, Scheme_Object **args);
static Scheme_Object *port_read_handler(int, Scheme_Object **args);
static Scheme_Object *port_display_handler(int, Scheme_Object **args);
static Scheme_Object *port_write_handler(int, Scheme_Object **args);
static Scheme_Object *port_print_handler(int, Scheme_Object **args);
static Scheme_Object *global_port_print_handler(int, Scheme_Object **args);

static Scheme_Object *sch_process(int c, Scheme_Object *args[]);
static Scheme_Object *sch_system(int c, Scheme_Object *args[]);
static Scheme_Object *sch_execute(int c, Scheme_Object *args[]);
static Scheme_Object *sch_process_star(int c, Scheme_Object *args[]);
static Scheme_Object *sch_system_star(int c, Scheme_Object *args[]);
static Scheme_Object *sch_execute_star(int c, Scheme_Object *args[]);
static Scheme_Object *sch_send_event(int c, Scheme_Object *args[]);

#ifndef NO_TCP_SUPPORT
static Scheme_Object *tcp_connect(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_listen(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_stop(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_accept_ready(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_accept(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_listener_p(int argc, Scheme_Object *argv[]);
#endif

static Scheme_Object *sch_default_read_handler(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_display_handler(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_write_handler(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_print_handler(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_global_port_print_handler(int argc, Scheme_Object *argv[]);

static void default_sleep(float v, void *fds);
#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
static Scheme_Object *make_tested_file_input_port(FILE *fp, char *name, int tested);
OS_SEMAPHORE_TYPE scheme_break_semaphore;
#else
# define make_tested_file_input_port(fp, name, t) scheme_make_named_file_input_port(fp, name)
#endif

#ifdef USE_FD_PORTS
static Scheme_Object *make_fd_input_port(int fd, const char *filename);
static Scheme_Object *make_fd_output_port(int fd);
#endif
#ifdef USE_OSKIT_CONSOLE
static Scheme_Object *make_oskit_console_input_port();
#endif

static Scheme_Object *default_read_handler;
static Scheme_Object *default_display_handler;
static Scheme_Object *default_write_handler;
static Scheme_Object *default_print_handler;

typedef void (*Write_String_Fun)(char *str, long len, struct Scheme_Output_Port *);
typedef void (*Close_Fun_o)(struct Scheme_Output_Port *);

typedef int (*Getc_Fun)(struct Scheme_Input_Port *port);
typedef int (*Peekc_Fun)(struct Scheme_Input_Port *port);
typedef int (*Char_Ready_Fun)(struct Scheme_Input_Port *port);
typedef void (*Close_Fun_i)(struct Scheme_Input_Port *port);
typedef void (*Need_Wakeup_Fun)(struct Scheme_Input_Port *, void *);

#include "schwinfd.h"

#if defined(USE_BEOS_PORT_THREADS) || defined(BEOS_PROCESSES)
static status_t kill_my_team(void *t)
{
  thread_info info;
  status_t v;

  MZ_SIGSET(SIGINT, SIG_IGN);

  get_thread_info((thread_id)t, &info);
  wait_for_thread((thread_id)t, &v);

  kill_team(info.team);

  return 0;
}
#endif

void 
scheme_init_port (Scheme_Env *env)
{
  if (scheme_starting_up) {
    Scheme_Config *config = scheme_config;

#ifdef MZ_PRECISE_GC
    register_traversers();
#endif

#ifdef WINDOWS_PROCESSES
    init_thread_memory();
#endif
    
    REGISTER_SO(scheme_orig_stdout_port);
    REGISTER_SO(scheme_orig_stderr_port);
    REGISTER_SO(scheme_orig_stdin_port);
#ifdef USE_FD_PORTS
    REGISTER_SO(fd_input_port_type);
#endif
#ifdef USE_OSKIT_CONSOLE
    REGISTER_SO(oskit_console_input_port_type);
#endif
    REGISTER_SO(file_input_port_type);
    REGISTER_SO(string_input_port_type);
#ifdef USE_TCP
    REGISTER_SO(tcp_input_port_type);
#endif
#ifdef USE_FD_PORTS
    REGISTER_SO(fd_output_port_type);
#endif
    REGISTER_SO(file_output_port_type);
    REGISTER_SO(string_output_port_type);
#ifdef USE_TCP
    REGISTER_SO(tcp_output_port_type);
#endif
    REGISTER_SO(user_input_port_type);
    REGISTER_SO(user_output_port_type);
    REGISTER_SO(pipe_read_port_type);
    REGISTER_SO(pipe_write_port_type);
#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
    REGISTER_SO(tested_file_input_port_type);
#endif
    REGISTER_SO(default_read_handler);
    REGISTER_SO(default_display_handler);
    REGISTER_SO(default_write_handler);
    REGISTER_SO(default_print_handler);
    REGISTER_SO(scheme_write_proc);
    REGISTER_SO(scheme_display_proc);
    REGISTER_SO(scheme_print_proc);

#ifdef USE_MAC_TCP
    REGISTER_SO(tcp_send_buffers);
#endif

#if defined(UNIX_PROCESSES)
    REGISTER_SO(scheme_system_children);
#endif

#ifndef DONT_IGNORE_PIPE_SIGNAL
    START_XFORM_SKIP;
    MZ_SIGSET(SIGPIPE, SIG_IGN);
    END_XFORM_SKIP;
#endif

#if defined(USE_BEOS_PORT_THREADS) || defined(BEOS_PROCESSES)
    resume_thread(spawn_thread(kill_my_team, "killer",
			       B_NORMAL_PRIORITY, (void*)find_thread(NULL)));
#endif

    if (!scheme_sleep)
      scheme_sleep = default_sleep;

    scheme_eof->type = scheme_eof_type;

    string_input_port_type = scheme_make_port_type("<string-input-port>");
    string_output_port_type = scheme_make_port_type("<string-output-port>");

#ifdef USE_FD_PORTS
    fd_input_port_type = scheme_make_port_type("<file-desc-input-port>");
    fd_output_port_type = scheme_make_port_type("<file-desc-output-port>");
#endif
#ifdef USE_OSKIT_CONSOLE
    oskit_console_input_port_type = scheme_make_port_type("<console-input-port>");
#endif

    file_input_port_type = scheme_make_port_type("<file-input-port>");
    file_output_port_type = scheme_make_port_type("<file-output-port>");

    user_input_port_type = scheme_make_port_type("<user-input-port>");
    user_output_port_type = scheme_make_port_type("<user-output-port>");

    pipe_read_port_type = scheme_make_port_type("<pipe-input-port>");
    pipe_write_port_type = scheme_make_port_type("<pipe-output-port>");

#ifdef USE_TCP
    tcp_input_port_type = scheme_make_port_type("<tcp-input-port>");
    tcp_output_port_type = scheme_make_port_type("<tcp-output-port>");
#endif

#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
    tested_file_input_port_type = scheme_make_port_type("<file-input-port>");
# ifdef WIN32_FD_HANDLES
    scheme_break_semaphore = CreateSemaphore(NULL, 0, 1, NULL);
# endif
# ifdef USE_BEOS_PORT_THREADS
    scheme_break_semaphore = create_sem(0, NULL);
# endif
#endif

    scheme_orig_stdin_port = (scheme_make_stdin
			      ? scheme_make_stdin()
#ifdef USE_FD_PORTS
			      : make_fd_input_port(0, "STDIN")
#else
# ifdef USE_OSKIT_CONSOLE
			      : make_oskit_console_input_port()
# else
			      : make_tested_file_input_port(stdin, "STDIN", 1)
# endif
#endif
			      );

    scheme_set_param(config, MZCONFIG_INPUT_PORT,
		     scheme_orig_stdin_port);
    scheme_orig_stderr_port = (scheme_make_stdout
			       ? scheme_make_stdout()
#ifdef USE_FD_PORTS
			       : make_fd_output_port(1)
#else
			       : scheme_make_file_output_port(stdout)
#endif
			       );
    scheme_set_param(config, MZCONFIG_OUTPUT_PORT,
		     scheme_orig_stderr_port);
    scheme_orig_stdout_port = (scheme_make_stderr
			       ? scheme_make_stderr()
#ifdef USE_FD_PORTS
			       : make_fd_output_port(2)
#else
			       : scheme_make_file_output_port(stderr)
#endif
			       );
    scheme_set_param(config, MZCONFIG_ERROR_PORT,
		     scheme_orig_stdout_port);

    {
      Scheme_Object *dlh;
      dlh = scheme_make_prim_w_arity2(default_load,
				      "default-load-handler",
				      1, 1,
				      0, -1);
      scheme_set_param(config, MZCONFIG_LOAD_HANDLER, dlh);
    }

    scheme_set_param(config, MZCONFIG_LOAD_DIRECTORY, scheme_false);

    scheme_write_proc = scheme_make_prim_w_arity(sch_write, 
						 "write", 
						 1, 2);
    scheme_display_proc = scheme_make_prim_w_arity(display, 
						   "display", 
						   1, 2);
    scheme_print_proc = scheme_make_prim_w_arity(print, 
						 "print", 
						 1, 2);
    
    REGISTER_SO(text_symbol);
    REGISTER_SO(binary_symbol);
    REGISTER_SO(append_symbol); 
    REGISTER_SO(error_symbol);
    REGISTER_SO(replace_symbol);
    REGISTER_SO(truncate_symbol);
    REGISTER_SO(truncate_replace_symbol);

    text_symbol = scheme_intern_symbol("text");
    binary_symbol = scheme_intern_symbol("binary");
    append_symbol = scheme_intern_symbol("append");
    error_symbol = scheme_intern_symbol("error");
    replace_symbol = scheme_intern_symbol("replace");
    truncate_symbol = scheme_intern_symbol("truncate");
    truncate_replace_symbol = scheme_intern_symbol("truncate/replace");

    REGISTER_SO(any_symbol);
    REGISTER_SO(any_one_symbol);
    REGISTER_SO(cr_symbol);
    REGISTER_SO(lf_symbol);
    REGISTER_SO(crlf_symbol);

    any_symbol = scheme_intern_symbol("any");
    any_one_symbol = scheme_intern_symbol("any-one");
    cr_symbol = scheme_intern_symbol("return");
    lf_symbol = scheme_intern_symbol("linefeed");
    crlf_symbol = scheme_intern_symbol("return-linefeed");

    REGISTER_SO(all_symbol);
    REGISTER_SO(non_elaboration_symbol);
    REGISTER_SO(none_symbol);

    all_symbol = scheme_intern_symbol("all");
    non_elaboration_symbol = scheme_intern_symbol("non-elaboration");
    none_symbol = scheme_intern_symbol("none");

    scheme_set_param(config, MZCONFIG_USE_COMPILED_KIND, all_symbol);

    default_read_handler = scheme_make_prim_w_arity(sch_default_read_handler,
						    "default-port-read-handler", 
						    1, 1);
    default_display_handler = scheme_make_prim_w_arity(sch_default_display_handler,
						       "default-port-display-handler", 
						       2, 2);
    default_write_handler = scheme_make_prim_w_arity(sch_default_write_handler,
						     "default-port-write-handler", 
						     2, 2);
    default_print_handler = scheme_make_prim_w_arity(sch_default_print_handler,
						     "default-port-print-handler", 
						     2, 2);

    {
      Scheme_Object *gpph;
      gpph = scheme_make_prim_w_arity(sch_default_global_port_print_handler,
				      "default-global-port-print-handler",
				      2, 2);
      scheme_set_param(config, MZCONFIG_PORT_PRINT_HANDLER, gpph);
    }
  }

  scheme_add_global_constant("eof", scheme_eof, env);

  scheme_add_global_constant("input-port?", 
			     scheme_make_folding_prim(input_port_p, 
						      "input-port?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("output-port?", 
			     scheme_make_folding_prim(output_port_p, 
						      "output-port?", 
						      1, 1, 1), 
			     env);
  
  scheme_add_global_constant("current-input-port", 
			     scheme_register_parameter(current_input_port,
						       "current-input-port",
						       MZCONFIG_INPUT_PORT),
			     env);
  scheme_add_global_constant("current-output-port", 
			     scheme_register_parameter(current_output_port,
						       "current-output-port",
						       MZCONFIG_OUTPUT_PORT),
			     env);
  scheme_add_global_constant("current-error-port", 
			     scheme_register_parameter(current_error_port, 
						       "current-error-port",
						       MZCONFIG_ERROR_PORT),
			     env);
  
  scheme_add_global_constant("open-input-file", 
			     scheme_make_prim_w_arity(open_input_file, 
						      "open-input-file", 
						      1, 2), 
			     env);
  scheme_add_global_constant("open-input-string", 
			     scheme_make_prim_w_arity(open_input_string, 
						      "open-input-string", 
						      1, 1), 
			     env);
  scheme_add_global_constant("open-output-file", 
			     scheme_make_prim_w_arity(open_output_file,
						      "open-output-file",
						      1, 3), 
			     env);
  scheme_add_global_constant("open-output-string", 
			     scheme_make_prim_w_arity(open_output_string,
						      "open-output-string", 
						      0, 0),
			     env);
  scheme_add_global_constant("get-output-string", 
			     scheme_make_prim_w_arity(get_output_string,
						      "get-output-string",
						      1, 1),
			     env);
  scheme_add_global_constant("close-input-port", 
			     scheme_make_prim_w_arity(close_input_port,
						      "close-input-port", 
						      1, 1), 
			     env);
  scheme_add_global_constant("close-output-port", 
			     scheme_make_prim_w_arity(close_output_port, 
						      "close-output-port", 
						      1, 1), 
			     env);
  scheme_add_global_constant("call-with-output-file",
			     scheme_make_prim_w_arity2(call_with_output_file,
						       "call-with-output-file",
						       2, 4,
						       0, -1),
			     env);
  scheme_add_global_constant("call-with-input-file",
			     scheme_make_prim_w_arity2(call_with_input_file,
						       "call-with-input-file",
						       2, 3,
						       0, -1),
			     env);
  scheme_add_global_constant("with-output-to-file",
			     scheme_make_prim_w_arity2(with_output_to_file,
						       "with-output-to-file",
						       2, 4,
						       0, -1),
			     env);
  scheme_add_global_constant("with-input-from-file",
			     scheme_make_prim_w_arity2(with_input_from_file,
						       "with-input-from-file",
						       2, 3,
						       0, -1),
			     env);
  scheme_add_global_constant("make-input-port", 
			     scheme_make_prim_w_arity(make_input_port, 
						      "make-input-port", 
						      3, 4), 
			     env);
  scheme_add_global_constant("make-output-port", 
			     scheme_make_prim_w_arity(make_output_port, 
						      "make-output-port", 
						      2, 2), 
			     env);
  
  scheme_add_global_constant("read", 
			     scheme_make_prim_w_arity(read_f,
						      "read", 
						      0, 1), 
			     env);
  scheme_add_global_constant("read-char", 
			     scheme_make_prim_w_arity(read_char, 
						      "read-char", 
						      0, 1), 
			     env);
  scheme_add_global_constant("read-line", 
			     scheme_make_prim_w_arity(read_line, 
						      "read-line", 
						      0, 2), 
			     env);
  scheme_add_global_constant("read-string", 
			     scheme_make_prim_w_arity(read_string, 
						      "read-string", 
						      1, 2), 
			     env);
  scheme_add_global_constant("read-string!", 
			     scheme_make_prim_w_arity(read_string_bang, 
						      "read-string!", 
						      1, 4), 
			     env);
  scheme_add_global_constant("peek-char", 
			     scheme_make_prim_w_arity(peek_char, 
						      "peek-char", 
						      0, 1), 
			     env);
  scheme_add_global_constant("eof-object?", 
			     scheme_make_folding_prim(eof_object_p, 
						      "eof-object?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-ready?", 
			     scheme_make_prim_w_arity(char_ready_p, 
						      "char-ready?", 
						      0, 1), 
			     env);
  scheme_add_global_constant("write", scheme_write_proc, env);
  scheme_add_global_constant("display", scheme_display_proc, env);
  scheme_add_global_constant("print", scheme_print_proc, env);
  scheme_add_global_constant("newline", 
			     scheme_make_prim_w_arity(newline, 
						      "newline", 
						      0, 1), 
			     env);
  scheme_add_global_constant("write-char", 
			     scheme_make_prim_w_arity(write_char, 
						      "write-char", 
						      1, 2), 
			     env);
  
  scheme_add_global_constant("port-read-handler", 
			     scheme_make_prim_w_arity(port_read_handler, 
						      "port-read-handler", 
						      1, 2), 
			     env);
  scheme_add_global_constant("port-display-handler", 
			     scheme_make_prim_w_arity(port_display_handler, 
						      "port-display-handler", 
						      1, 2), 
			     env);
  scheme_add_global_constant("port-write-handler", 
			     scheme_make_prim_w_arity(port_write_handler, 
						      "port-write-handler", 
						      1, 2), 
			     env);
  scheme_add_global_constant("port-print-handler", 
			     scheme_make_prim_w_arity(port_print_handler, 
						      "port-print-handler", 
						      1, 2), 
			     env);
  scheme_add_global_constant("global-port-print-handler",
			     scheme_register_parameter(global_port_print_handler,
						       "global-port-print-handler",
						       MZCONFIG_PORT_PRINT_HANDLER),
			     env);
  
  scheme_add_global_constant("load", 
			     scheme_make_prim_w_arity2(load, 
						       "load", 
						       1, 1,
						       0, -1),
			      env);
  scheme_add_global_constant("current-load", 
			     scheme_register_parameter(current_load, 
						       "current-load",
						       MZCONFIG_LOAD_HANDLER), 
			     env);
  scheme_add_global_constant("current-load-relative-directory", 
			     scheme_register_parameter(current_load_directory, 
						       "current-load-relative-directory",
						       MZCONFIG_LOAD_DIRECTORY), 
			     env);

  scheme_add_global_constant("use-compiled-file-kinds",
			     scheme_register_parameter(use_compiled_kind,
						       "use-compiled-file-kinds",
						       MZCONFIG_USE_COMPILED_KIND),
			     env);
  scheme_add_global_constant("current-require-relative-collection",
			     scheme_register_parameter(current_require_relative_collection,
						       "current-require-relative-collection",
						       MZCONFIG_REQUIRE_COLLECTION),
			     env);

  scheme_add_global_constant ("transcript-on", 
			      scheme_make_prim_w_arity(transcript_on,
						       "transcript-on", 
						       1, 1),
			      env);
  scheme_add_global_constant ("transcript-off", 
			      scheme_make_prim_w_arity(transcript_off,
						       "transcript-off", 
						       0, 0),
			      env);
  
  scheme_add_global_constant("flush-output", 
			     scheme_make_prim_w_arity(flush_output, 
						      "flush-output", 
						      0, 1), 
			     env);
  scheme_add_global_constant("file-position", 
			     scheme_make_prim_w_arity(file_position, 
						      "file-position", 
						      1, 2), 
			     env);
  
  scheme_add_global_constant("make-pipe", 
			     scheme_make_prim_w_arity2(sch_pipe, 
						       "make-pipe", 
						       0, 0,
						       2, 2), 
			     env);
  
  scheme_add_global_constant("process", 
			     scheme_make_prim_w_arity(sch_process, 
						      "process", 
						      1, 1), 
			     env);
  scheme_add_global_constant("system", 
			     scheme_make_prim_w_arity(sch_system,
						      "system", 
						      1, 1), 
			     env);
  scheme_add_global_constant("execute", 
			     scheme_make_prim_w_arity(sch_execute, 
						      "execute", 
						      1, 1), 
			     env);
  scheme_add_global_constant("process*", 
			     scheme_make_prim_w_arity(sch_process_star,
						      "process*",
						      1, -1), 
			     env);
  scheme_add_global_constant("system*", 
			     scheme_make_prim_w_arity(sch_system_star,
						      "system*",
						      1, -1), 
			     env);
  scheme_add_global_constant("execute*", 
			     scheme_make_prim_w_arity(sch_execute_star,
						      "execute*", 
						      1, -1), 
			     env);
  scheme_add_global_constant("send-event", 
			     scheme_make_prim_w_arity(sch_send_event,
						      "send-event", 
						      3, 5), 
			     env);

#ifndef NO_TCP_SUPPORT
  scheme_add_global_constant("tcp-connect", 
			     scheme_make_prim_w_arity2(tcp_connect,
						       "tcp-connect", 
						       2, 2,
						       2, 2), 
			     env);
  scheme_add_global_constant("tcp-listen", 
			     scheme_make_prim_w_arity(tcp_listen,
						      "tcp-listen", 
						      1, 2), 
			     env);
  scheme_add_global_constant("tcp-close", 
			     scheme_make_prim_w_arity(tcp_stop,
						      "tcp-close", 
						      1, 1), 
			     env);
  scheme_add_global_constant("tcp-accept-ready?", 
			     scheme_make_prim_w_arity(tcp_accept_ready,
						      "tcp-accept-ready?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("tcp-accept", 
			     scheme_make_prim_w_arity2(tcp_accept,
						       "tcp-accept", 
						       1, 1,
						       2, 2), 
			     env);
  scheme_add_global_constant("tcp-listener?", 
			     scheme_make_folding_prim(tcp_listener_p,
						      "tcp-listener?", 
						      1, 1, 1), 
			     env);
#endif
}


#ifdef USE_DYNAMIC_FDSET_SIZE
static int dynamic_fd_size;

void *scheme_alloc_fdset_array(int count, int permanent)
{
  if (!dynamic_fd_size) {
#ifdef USE_ULIMIT
    dynamic_fd_size = ulimit(4, 0);
#else
    dynamic_fd_size = getdtablesize();
#endif
    /* divide by bits-per-byte: */
    dynamic_fd_size = (dynamic_fd_size + 7) >> 3;
    /* word-align: */
    if (dynamic_fd_size % sizeof(void*))
      dynamic_fd_size += sizeof(void*) - (dynamic_fd_size % sizeof(void*));
  }   

  if (permanent)
    return scheme_malloc_eternal(count * dynamic_fd_size);
  else
    return scheme_malloc_atomic(count * dynamic_fd_size);
}

void *scheme_init_fdset_array(void *fdarray, int count)
{
  return fdarray;
}

void *scheme_get_fdset(void *fdarray, int pos)
{
  return ((char *)fdarray) + (pos * dynamic_fd_size);
}

void scheme_fdzero(void *fd)
{
  memset(fd, 0, dynamic_fd_size);
}

#else

#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
# define fdset_type win_extended_fd_set
#else
# define fdset_type fd_set
#endif

void *scheme_alloc_fdset_array(int count, int permanent)
{
#if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP) || defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
  void *fdarray;
  if (permanent)
    fdarray = scheme_malloc_eternal(count * sizeof(fdset_type));
  else
    fdarray = scheme_malloc_atomic(count * sizeof(fdset_type));
# if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
  if (count) {
    ((win_extended_fd_set *)fdarray)->added = 0;
    ((win_extended_fd_set *)fdarray)->num_handles = 0;
    ((win_extended_fd_set *)fdarray)->wait_event_mask = 0;
  }
# endif
  return fdarray;
#else
  return NULL;
#endif
}

void *scheme_init_fdset_array(void *fdarray, int count)
{
#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
  if (count) {
    ((win_extended_fd_set *)fdarray)->added = 0;
    ((win_extended_fd_set *)fdarray)->num_handles = 0;
    ((win_extended_fd_set *)fdarray)->wait_event_mask = 0;
  }
#endif
  return fdarray;
}

void *scheme_get_fdset(void *fdarray, int pos)
{
#if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP) || defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
  return ((fdset_type *)fdarray) + pos;
#else
  return NULL;
#endif
}

void scheme_fdzero(void *fd)
{
#if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP)
  FD_ZERO((fd_set *)fd);
#endif
#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
  ((win_extended_fd_set *)fd)->added = 0;
#endif
}

#endif

void scheme_fdclr(void *fd, int n)
{
#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
  if (FD_ISSET(n, ((fd_set *)fd)))
    --((win_extended_fd_set *)fd)->added;
#endif
#if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP)
  FD_CLR((unsigned)n, ((fd_set *)fd));
#endif
}

void scheme_fdset(void *fd, int n)
{
#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
  if (!FD_ISSET(n, ((fd_set *)fd)))
    ((win_extended_fd_set *)fd)->added++;
#endif
#if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP)
  FD_SET(n, ((fd_set *)fd));
#endif
}

int scheme_fdisset(void *fd, int n)
{
#if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP)
  return FD_ISSET(n, ((fd_set *)fd));
#else
  return 0;
#endif
}

void scheme_add_fd_handle(void *h, void *fds, int repost)
{
#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
  win_extended_fd_set *efd = (win_extended_fd_set *)fds;
  OS_SEMAPHORE_TYPE *hs;
  int i, *rps;
  
  i = efd->num_handles;
  hs = MALLOC_N_ATOMIC(OS_SEMAPHORE_TYPE, i + 3);
  /*                 Leave room for two more -^ */
  rps = MALLOC_N_ATOMIC(int, i + 3);
  hs[i] = (OS_SEMAPHORE_TYPE)h;
  rps[i] = repost;
  while (i--) {
    hs[i] = efd->handles[i];
    rps[i] = efd->repost_sema[i];
  }
  efd->num_handles++;
  efd->handles= hs;
  efd->repost_sema = rps;
#else
  /* Do nothing. */
#endif
}

void scheme_add_fd_eventmask(void *fds, int mask)
{
#if defined(WIN32_FD_HANDLES)
  ((win_extended_fd_set *)fds)->wait_event_mask |= mask;
#endif
}

#ifdef WINDOWS_PROCESSES
typedef struct Scheme_Thread_Memory {
  MZTAG_IF_REQUIRED
  void *handle;
  void *subhandle;
  struct Scheme_Thread_Memory *prev;
  struct Scheme_Thread_Memory *next;
} Scheme_Thread_Memory;

Scheme_Thread_Memory *tm_start, *tm_next;

extern void (*GC_collect_start_callback)(void);
extern void (*GC_collect_end_callback)(void);

static void init_thread_memory()
{
  REGISTER_SO(tm_start);
  REGISTER_SO(tm_next);

  /* We start with a pre-allocated tm because we
     want to register a thread before performing any
     allocations. */
  tm_next = MALLOC_ONE_RT(Scheme_Thread_Memory);
#ifdef MZTAG_REQUIRED
  tm_next->type = scheme_rt_thread_memory;
#endif

  /* scheme_init_process() will replace these: */
  GC_collect_start_callback = scheme_suspend_remembered_threads;
  GC_collect_end_callback = scheme_resume_remembered_threads;
}

struct Scheme_Thread_Memory *scheme_remember_thread(void *t)
{
  Scheme_Thread_Memory *tm = tm_next;

  tm->next = tm_start;
  if (tm->next)
    tm->next->prev = tm;
  tm_start = tm;

  tm_next = MALLOC_ONE_RT(Scheme_Thread_Memory);
#ifdef MZTAG_REQUIRED
  tm_next->type = scheme_rt_thread_memory;
#endif

  return tm;
}

void scheme_remember_subthread(struct Scheme_Thread_Memory *tm, void *t)
{
  tm->subhandle = t;
}

void scheme_forget_thread(struct Scheme_Thread_Memory *tm)
{
  if (tm->prev)
    tm->prev->next = tm->next;
  else
    tm_start = tm->next;

  if (tm->next)
    tm->next = tm->prev;
}

void scheme_forget_subthread(struct Scheme_Thread_Memory *tm)
{
  tm->subhandle = NULL;
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

void scheme_suspend_remembered_threads(void)
{
  Scheme_Thread_Memory *tm;
  
  for (tm = tm_start; tm; tm = tm->next) {
    SuspendThread((HANDLE)tm->handle);
    if (tm->subhandle)
      SuspendThread((HANDLE)tm->subhandle);
  }
}

void scheme_resume_remembered_threads(void)
{
  Scheme_Thread_Memory *tm;
  
  for (tm = tm_start; tm; tm = tm->next) {
    if (tm->subhandle)
      ResumeThread((HANDLE)tm->subhandle);
    ResumeThread((HANDLE)tm->handle);
  }
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

#endif


Scheme_Object *scheme_make_port_type(const char *name)
{
  return scheme_make_symbol(name);
}

Scheme_Input_Port *
_scheme_make_input_port(Scheme_Object *subtype,
			void *data,
			int (*getc_fun) (Scheme_Input_Port*),
			int (*peekc_fun) (Scheme_Input_Port*),
			int (*char_ready_fun) (Scheme_Input_Port*),
			void (*close_fun) (Scheme_Input_Port*),
			void (*need_wakeup_fun)(Scheme_Input_Port*, void *),
			int must_close)
{
  Scheme_Input_Port *ip;

  ip = MALLOC_ONE_TAGGED(Scheme_Input_Port);
  ip->type = scheme_input_port_type;
  ip->sub_type = subtype;
  ip->port_data = data;
  ip->getc_fun = getc_fun;
  ip->peekc_fun = peekc_fun;
  ip->char_ready_fun = char_ready_fun;
  ip->need_wakeup_fun = need_wakeup_fun;
  ip->close_fun = close_fun;
  ip->name = "stdin";
  ip->ungotten = NULL;
  ip->ungotten_count = 0;
  ip->ungotten_allocated = 0;
  ip->position = 0;
  ip->lineNumber = 1;
  ip->charsSinceNewline = 1;
  ip->closed = 0;
  ip->eoffound = 0;
  ip->read_handler = NULL;

#ifdef MZ_REAL_THREADS
  ip->sema = scheme_make_sema(1);
#endif

  if (must_close) {
    Scheme_Manager_Reference *mref;    
    mref = scheme_add_managed(NULL,
			      (Scheme_Object *)ip, 
			      (Scheme_Close_Manager_Client *)scheme_close_input_port, 
			      NULL, must_close);
    ip->mref = mref;
  } else
    ip->mref = NULL;

  return (ip);
}

Scheme_Input_Port *
scheme_make_input_port(Scheme_Object *subtype,
		       void *data,
		       int (*getc_fun) (Scheme_Input_Port*),
		       int (*peekc_fun) (Scheme_Input_Port*),
		       int (*char_ready_fun) (Scheme_Input_Port*),
		       void (*close_fun) (Scheme_Input_Port*),
		       void (*need_wakeup_fun)(Scheme_Input_Port*, void *),
		       int must_close)
{
  return _scheme_make_input_port(subtype, data,
				 getc_fun, peekc_fun, char_ready_fun, close_fun, 
				 need_wakeup_fun, must_close);
}

Scheme_Output_Port *
scheme_make_output_port(Scheme_Object *subtype,
			void *data,
			void (*write_string_fun)(char *str, long,
						 Scheme_Output_Port*),
			void (*close_fun) (Scheme_Output_Port*),
			int must_close)
{
  Scheme_Output_Port *op;

  op = MALLOC_ONE_TAGGED(Scheme_Output_Port);
  op->type = scheme_output_port_type;
  op->sub_type = subtype;
  op->port_data = data;
  op->write_string_fun = write_string_fun;
  op->close_fun = close_fun;
  op->closed = 0;
  op->pos = 0;
  op->display_handler = NULL;
  op->write_handler = NULL;
  op->print_handler = NULL;

#ifdef MZ_REAL_THREADS
  op->sema = scheme_make_sema(1);
#endif

  if (must_close) {
    Scheme_Manager_Reference *mref;
    mref = scheme_add_managed(NULL,
			      (Scheme_Object *)op, 
			      (Scheme_Close_Manager_Client *)scheme_close_output_port, 
			      NULL, must_close);
    op->mref = mref;
  } else
    op->mref = NULL;

  return op;
}

#define check_closed(who, kind, port, closed) if (closed) scheme_raise_exn(MZEXN_I_O_PORT_CLOSED, port, "%s: " kind " port is closed", who);

#ifdef MZ_REAL_THREADS

# define BEGIN_LOCK_PORT(sema) \
    { mz_jmp_buf savebuf; \
      ADD_PORT_LOCK() \
      scheme_wait_sema(sema, 0); \
      memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf)); \
      if (scheme_setjmp(scheme_error_buf)) { \
        scheme_post_sema(sema); \
        scheme_longjmp(savebuf, 1); \
      } else {
# define END_LOCK_PORT(sema) \
      memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf)); \
      SUB_PORT_LOCK() \
      scheme_post_sema(sema); } }

# ifdef MZ_KEEP_LOCK_INFO
int scheme_port_lock_c;
#  define ADD_PORT_LOCK() SCHEME_GET_LOCK(); scheme_port_lock_c++; SCHEME_RELEASE_LOCK();
#  define SUB_PORT_LOCK() SCHEME_GET_LOCK(); --scheme_port_lock_c; SCHEME_RELEASE_LOCK();
# else
#  define ADD_PORT_LOCK() /* empty */
#  define SUB_PORT_LOCK() /* empty */
# endif

#else
# define BEGIN_LOCK_PORT(sema) /* empty */
# define END_LOCK_PORT(sema) /* empty */
#endif

int
scheme_getc (Scheme_Object *port)
{
  int c;
  Scheme_Input_Port *ip;
  
  SCHEME_USE_FUEL(1);

  ip = (Scheme_Input_Port *)port;

  BEGIN_LOCK_PORT(ip->sema);

  if (ip->ungotten_count)
    c = ip->ungotten[--ip->ungotten_count];
  else {
    Getc_Fun f;
    check_closed("#<primitive:get-port-char>", "input", port, ip->closed);
    f = ip->getc_fun;
    c = f(ip);
  }

  if (c != EOF) {
    ip->position++;
    if (c == '\n' || c == '\r') {
      ip->charsSinceNewline = 1;
      ip->lineNumber++;
    } else
      ip->charsSinceNewline++;
  } else
    ip->eoffound = 1;

  END_LOCK_PORT(ip->sema);

  return c;
}

long 
scheme_get_chars(Scheme_Object *port, long size, char *buffer)
{
  Scheme_Input_Port *ip;
  long got = 0, i, c;
  long orig_size = size;
  
  ip = (Scheme_Input_Port *)port;

  BEGIN_LOCK_PORT(ip->sema);

  check_closed("#<primitive:get-port-char>", "input", port, ip->closed);

  if (ip->ungotten_count) {
    long l, i;
    unsigned char *s;

    if (ip->ungotten_count < size)
      l = ip->ungotten_count;
    else
      l = size;
    
    i = ip->ungotten_count;
    s = (unsigned char *)ip->ungotten;
    size -= l;
    while (l--) {
      buffer[got++] = s[--i];
    }
    
    ip->ungotten_count = i;
  }

  if (size) {
    if (SAME_OBJ(ip->sub_type, file_input_port_type)
	&& ((Scheme_Input_File *)ip->port_data)->regfile) {
      FILE *f = ((Scheme_Input_File *)ip->port_data)->f;
      got += fread(buffer + got, 1, size, f);
    } else if (SAME_OBJ(ip->sub_type, string_input_port_type)) {
      Scheme_Indexed_String *is;
      long l;
      
      is = (Scheme_Indexed_String *)ip->port_data;
      
      if (is->index + size <= is->size)
	l = size;
      else
	l = (is->size - is->index);
      
      memcpy(buffer + got, is->string + is->index, l);
      is->index += l;
      
      got += l;
    } else {
      /* use getc: */
      int c;
      
      while (size--) {
	Getc_Fun f = ip->getc_fun;
	c = f(ip);
	if (c != EOF)
	  buffer[got++] = c;
	else
	  break;
      }
    }
  }
  
  /* Adjust position information: */
  ip->position += got;
  for (i = got, c = 0; i--; c++) {
    if (buffer[i] == '\n' || buffer[i] == '\r') {
      break;
    }
  }
  if (i >= 0) {
    int n = 0;
    ip->charsSinceNewline = c + 1;
    while (i--) {
      if (buffer[i] == '\n' || buffer[i] == '\r')
	n++;
    }
    ip->lineNumber += n;
  } else
    ip->charsSinceNewline += c;

  if (got < orig_size)
    ip->eoffound = 1;

  END_LOCK_PORT(ip->sema);

  return got;
}

int scheme_peekc(Scheme_Object *port)
{
  Scheme_Input_Port *ip;

  ip = (Scheme_Input_Port *)port;

  if (!ip->peekc_fun) {
    int ch;
    ch = scheme_getc(port);
    scheme_ungetc(ch, port);
    return ch;
  } else {
    int ch;
    
    BEGIN_LOCK_PORT(ip->sema);
    
    if (ip->ungotten_count)
      ch = ip->ungotten[ip->ungotten_count - 1];
    else {
      Peekc_Fun f;
      check_closed("#<primitive:peek-port-char>", "input", port, ip->closed);
      f = ip->peekc_fun;
      ch = f(ip);
    }

    END_LOCK_PORT(ip->sema);

    return ch;
  }
}

int scheme_peekc_is_ungetc(Scheme_Object *port)
{
  Scheme_Input_Port *ip;

  ip = (Scheme_Input_Port *)port;

  return !ip->peekc_fun;
}

int scheme_are_all_chars_ready(Scheme_Object *port)
{
  if (SCHEME_INPORTP(port)) {
    Scheme_Input_Port *ip;
    ip = (Scheme_Input_Port *)port;
    if (SAME_OBJ(ip->sub_type, file_input_port_type)
	&& ((Scheme_Input_File *)ip->port_data)->regfile)
      return 1;
    else if (SAME_OBJ(ip->sub_type, string_input_port_type))
      return 1;
    else
      return 0;
  } else
    return 0;
}

void
scheme_ungetc (int ch, Scheme_Object *port)
{
  Scheme_Input_Port *ip;

  if (ch == EOF)
    return;

  ip = (Scheme_Input_Port *)port;

  BEGIN_LOCK_PORT(ip->sema);

  check_closed("#<primitive:peek-port-char>", "input", port, ip->closed);

  if (ip->ungotten_count == ip->ungotten_allocated) {
    unsigned char *old;
    int oldc;

    old = ip->ungotten;
    oldc = ip->ungotten_count;
    if (oldc)
      ip->ungotten_allocated = 2 * oldc;
    else
      ip->ungotten_allocated = 5;

    {
      unsigned char *uca;
      uca = (unsigned char *)scheme_malloc_atomic(ip->ungotten_allocated);
      ip->ungotten = uca;
    }
    if (oldc)
      memcpy(ip->ungotten, old, oldc);
  }
  ip->ungotten[ip->ungotten_count++] = ch;

  if (ip->position)
    --ip->position;
  if (!(--ip->charsSinceNewline)) {
    --ip->lineNumber;
    /* If you back up over two lines, then lineNumber will be wrong. */
  }

  END_LOCK_PORT(ip->sema);
}

int
scheme_char_ready (Scheme_Object *port)
{
  Scheme_Input_Port *ip;
  int retval;

  ip = (Scheme_Input_Port *)port;

  BEGIN_LOCK_PORT(ip->sema);

  if (scheme_internal_checking_char && ip->closed) {
    /* never for real threads... */
    return 1;
  }
   
  check_closed("char-ready?", "input", port, ip->closed);

  if (ip->ungotten_count)
    retval = 1;
  else {
    Char_Ready_Fun f = ip->char_ready_fun;
    retval = f(ip);
  }

  END_LOCK_PORT(ip->sema);

  return retval;
}

void
scheme_need_wakeup (Scheme_Object *port, void *fds)
{
  Scheme_Input_Port *ip;

  ip = (Scheme_Input_Port *)port;
  
  if (ip->need_wakeup_fun) {
    Need_Wakeup_Fun f = ip->need_wakeup_fun;
    f(ip, fds);
  }
}

long
scheme_tell (Scheme_Object *port)
{
  Scheme_Input_Port *ip;
  long pos;

  ip = (Scheme_Input_Port *)port;

  BEGIN_LOCK_PORT(ip->sema);

  check_closed("#<primitive:get-file-position>", "input", port, ip->closed);

  pos = ip->position;

  END_LOCK_PORT(ip->sema);

  return pos;
}

long
scheme_tell_line (Scheme_Object *port)
{
  Scheme_Input_Port *ip;
  long line;

  ip = (Scheme_Input_Port *)port;

  BEGIN_LOCK_PORT(ip->sema);

  check_closed("#<primitive:get-file-line>", "input", port, ip->closed);

  line = ip->lineNumber;

  END_LOCK_PORT(ip->sema);

  return line;
}

void
scheme_close_input_port (Scheme_Object *port)
{
  Scheme_Input_Port *ip;

  ip = (Scheme_Input_Port *)port;

  BEGIN_LOCK_PORT(ip->sema);

  if (!ip->closed) {
    if (ip->mref)
      scheme_remove_managed(ip->mref, (Scheme_Object *)ip);

    if (ip->close_fun) {
      Close_Fun_i f = ip->close_fun;
      f(ip);
    }
    ip->closed = 1;
    ip->ungotten_count = 0;
  }

  END_LOCK_PORT(ip->sema);
}

void 
scheme_write_string(const char *str, long len, Scheme_Object *port)
{
  Scheme_Output_Port *op;

  op = (Scheme_Output_Port *)port;

  BEGIN_LOCK_PORT(op->sema);

  check_closed("#<primitive:write-port-string>", "output", port, op->closed);

  {
    Write_String_Fun f = op->write_string_fun;
    f((char *)str, len, op);
  }
  op->pos += len;

  END_LOCK_PORT(op->sema);
}

long
scheme_output_tell(Scheme_Object *port)
{
  Scheme_Output_Port *op;
  long pos;

  op = (Scheme_Output_Port *)port;

  BEGIN_LOCK_PORT(op->sema);

  check_closed("#<primitive:get-file-position>", "output", port, op->closed);

  pos = op->pos;

  END_LOCK_PORT(op->sema);

  return pos;
}

void
scheme_close_output_port (Scheme_Object *port)
{
  Scheme_Output_Port *op;

  op = (Scheme_Output_Port *)port;

  BEGIN_LOCK_PORT(op->sema);

  if (!op->closed) {
    if (op->mref)
      scheme_remove_managed(op->mref, (Scheme_Object *)op);

    if (op->close_fun) {
      Close_Fun_o f = op->close_fun;
      f(op);
    }
    op->closed = 1;
  }

  END_LOCK_PORT(op->sema);
}

/* file input ports */

#ifdef USE_FD_PORTS
static int
fd_write_ready (Scheme_Object *port)
{
  Scheme_FD *fop;

  fop = (Scheme_FD *)((Scheme_Output_Port *)port)->port_data;

  {
    DECL_FDSET(writefds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};

    INIT_DECL_FDSET(writefds, 1);
    INIT_DECL_FDSET(exnfds, 1);

    MZ_FD_ZERO(writefds);
    MZ_FD_ZERO(exnfds);
    MZ_FD_SET(fop->fd, writefds);
    MZ_FD_SET(fop->fd, exnfds);
    
    return select(fop->fd + 1, NULL, writefds, exnfds, &time);
  }
}


static void
fd_write_need_wakeup(Scheme_Object *port, void *fds)
{
  Scheme_FD *fop;
  void *fds2;
  int n;

  fop = (Scheme_FD *)((Scheme_Output_Port *)port)->port_data;

  n = fop->fd;
  fds2 = MZ_GET_FDSET(fds, 1);
  MZ_FD_SET(n, (fd_set *)fds2);
  fds2 = MZ_GET_FDSET(fds, 2);
  MZ_FD_SET(n, (fd_set *)fds2);
}

static void flush_fd(Scheme_Output_Port *op, char *bufstr, int buflen)
{
  Scheme_FD *fop = (Scheme_FD *)op->port_data;
  int offset = 0;

  if (!bufstr) {
    bufstr = fop->buffer;
    buflen = fop->bufcount;
  }

  if (buflen) {
    while (1) {
      long len;
      len = write(fop->fd, bufstr + offset, buflen - offset);

      if (len < 0) {
	if (errno == EAGAIN) {
	  scheme_block_until(fd_write_ready, fd_write_need_wakeup, (Scheme_Object *)op, 0.0);
	} else {
	  scheme_raise_exn(MZEXN_I_O_PORT_WRITE,
			   op,
			   "error writing to file port (%d)",
			   errno);
	  return;
	}
      } else if (len + offset == buflen)
	break;
      else
	offset += len;
    }

    fop->bufcount = 0;
  }
}
#endif

static void flush_orig_outputs()
{
  /* Flush original output ports: */
  Scheme_Output_Port *op;
  
  op = (Scheme_Output_Port *)scheme_orig_stdout_port;
#ifdef USE_FD_PORTS
  if (SAME_OBJ(op->sub_type, fd_output_port_type))
    flush_fd(op, NULL, 0);
  else
#endif
    if (SAME_OBJ(op->sub_type, file_output_port_type))
      fflush(((Scheme_Output_File *)op->port_data)->f);
  
  op = (Scheme_Output_Port *)scheme_orig_stderr_port;
#ifdef USE_FD_PORTS
  if (SAME_OBJ(op->sub_type, fd_output_port_type))
    flush_fd(op, NULL, 0);
  else
#endif
    if (SAME_OBJ(op->sub_type, file_output_port_type))
      fflush(((Scheme_Output_File *)op->port_data)->f);
}

static int
file_char_ready (Scheme_Input_Port *port)
{
#ifndef FILES_HAVE_FDS
  return 1;
#else
  FILE *fp;
  Scheme_Input_File *fip;

  fip = (Scheme_Input_File *)port->port_data;
  fp = fip->f;

  if (fip->regfile || port->closed)
    return 1;

# ifdef HAS_STANDARD_IOB  
  if (fp->_cnt)
    return 1;
# endif
# ifdef HAS_SCO_IOB  
  if (fp->__cnt)
    return 1;
# endif
# ifdef HAS_GNU_IOB
  if (fp->_egptr - fp->_gptr)
    return 1;
# endif
# ifdef HAS_CYGWIN_IOB
  if (fp->_r)
    return 1;
# endif
# ifdef HAS_LINUX_IOB
  if (fp->_IO_read_end - fp->_IO_read_ptr)
    return 1;
# endif
# ifdef HAS_BSD_IOB
  if (fp->_r > 0)
    return 1;
# endif
# ifdef HAS_BEOS_IOB  
  /* Not actually useful since BeOS doesn't have file descriptors... */
  if (fp->buffer_pos)
    return 1;
# endif
# ifdef HAS_OSKIT_IOB  
  if (fp->_r)
    return 1;
# endif

  if (feof(fp) || ferror(fp))
    return 1;

  {
    int fd, r;
    DECL_FDSET(readfds, 1);
    struct timeval time = {0, 0};

    INIT_DECL_FDSET(readfds, 1);

    fd = fileno(fp);
    if (fd < 0)
      return 1; /* No descriptor? Go ahead and read EOF */
    
    MZ_FD_ZERO(readfds);
    MZ_FD_SET(fd, readfds);

    r = select(fd + 1, readfds, NULL, NULL, &time);

    return r;
  }
#endif
}

static int file_getc(Scheme_Input_Port *port)
{
  FILE *fp;
  Scheme_Input_File *fip;
  int c;

  fip = (Scheme_Input_File *)port->port_data;
  fp = fip->f;

  if (!fip->regfile && !file_char_ready(port)) {
#if defined(FILES_HAVE_FDS) || defined(WIN32_FD_HANDLES)
    scheme_current_process->block_descriptor = PORT_BLOCKED;
    scheme_current_process->blocker = (Scheme_Object *)port;
# define FILE_BLOCK_TIME (float)0.0
#else
# define FILE_BLOCK_TIME (float)0.5
#endif
    do {
      scheme_process_block(FILE_BLOCK_TIME);
    } while (!file_char_ready(port));
#if defined(FILES_HAVE_FDS) || defined(WIN32_FD_HANDLES)
    scheme_current_process->block_descriptor = NOT_BLOCKED;
    scheme_current_process->blocker = NULL;
#endif
    scheme_current_process->ran_some = 1;
  }

  if (port->closed) {
    /* Another thread closed the input port while we were waiting. */
    /* Call scheme_getc to signal the error */
    scheme_getc((Scheme_Object *)port);
  }

  c = getc(fp);

  if (c == EOF) {
    if (!feof(fp)) {
      scheme_raise_exn(MZEXN_I_O_PORT_READ,
		       port,
		       "error reading from file port (%d)",
		       errno);
      return 0;
    }
#ifndef DONT_CLEAR_FILE_EOF
    clearerr(fp);
#endif
  }

  return c;
}

static void
file_close_input(Scheme_Input_Port *port)
{
  Scheme_Input_File *fip;

  fip = (Scheme_Input_File *)port->port_data;

  fclose(fip->f);
  --scheme_file_open_count;
}

static void
file_need_wakeup(Scheme_Input_Port *port, void *fds)
{
#ifdef FILES_HAVE_FDS
  Scheme_Input_File *fip;
  void *fds2;
  int n;

  fip = (Scheme_Input_File *)port->port_data;

  n = fileno(fip->f);
  MZ_FD_SET(n, (fd_set *)fds);
  fds2 = MZ_GET_FDSET(fds, 2);
  MZ_FD_SET(n, (fd_set *)fds2);
#endif
}

Scheme_Object *
_scheme_make_named_file_input_port(FILE *fp, const char *filename, 
				   int regfile)
{
  Scheme_Input_Port *ip;
  Scheme_Input_File *fip;

  if (!fp)
    scheme_signal_error("make-file-input-port(internal): "
			"null file pointer");
  
  fip = MALLOC_ONE_RT(Scheme_Input_File);
#ifdef MZTAG_REQUIRED
  fip->type = scheme_rt_input_file;
#endif

  fip->f = fp;
  fip->regfile = regfile;

  ip = _scheme_make_input_port(file_input_port_type,
			       fip,
			       file_getc,
			       NULL,
			       file_char_ready,
			       file_close_input,
			       file_need_wakeup,
			       1);

  {
    char *s;
    s = scheme_strdup(filename);
    ip->name = s;
  }

  return (Scheme_Object *)ip;
}

Scheme_Object *
scheme_make_named_file_input_port(FILE *fp, const char *filename)
{
  return _scheme_make_named_file_input_port(fp, filename, 0);
}

Scheme_Object *
scheme_make_file_input_port(FILE *fp)
{
  return scheme_make_named_file_input_port(fp, "FILE");
}

#ifdef USE_FD_PORTS
/* fd input ports */

static int
fd_char_ready (Scheme_Input_Port *port)
{
  Scheme_FD *fip;

  fip = (Scheme_FD *)port->port_data;

  if (fip->bufcount)
    return 1;
  else {
    DECL_FDSET(readfds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};

    INIT_DECL_FDSET(readfds, 1);
    INIT_DECL_FDSET(exnfds, 1);

    MZ_FD_ZERO(readfds);
    MZ_FD_ZERO(exnfds);
    MZ_FD_SET(fip->fd, readfds);
    MZ_FD_SET(fip->fd, exnfds);
    
    return select(fip->fd + 1, readfds, NULL, exnfds, &time);
  }
}

static int fd_getc(Scheme_Input_Port *port)
{
  Scheme_FD *fip;

  fip = (Scheme_FD *)port->port_data;

  if (fip->bufcount) {
    fip->bufcount--;
    return fip->buffer[fip->buffpos++];
  } else {
    if (!fd_char_ready(port)) {
      scheme_current_process->block_descriptor = PORT_BLOCKED;
      scheme_current_process->blocker = (Scheme_Object *)port;
      do {
	scheme_process_block(0.0);
      } while (!fd_char_ready(port));
      scheme_current_process->block_descriptor = NOT_BLOCKED;
      scheme_current_process->blocker = NULL;
      scheme_current_process->ran_some = 1;
    }

    fip->bufcount = read(fip->fd, fip->buffer, MZPORT_FD_BUFFSIZE);

    if (fip->bufcount < 0) {
      fip->bufcount = 0;
      scheme_raise_exn(MZEXN_I_O_PORT_READ,
		       port,
		       "error reading from fd port (%d)",
		       errno);
    }

    if (!fip->bufcount) {
      fip->buffpos = 0;
      return EOF;
    } else {
      fip->buffpos = 1;
      fip->bufcount--;
      return fip->buffer[0];
    }
  }
}

static void
fd_close_input(Scheme_Input_Port *port)
{
  Scheme_FD *fip;

  fip = (Scheme_FD *)port->port_data;

  close(fip->fd);
}

static void
fd_need_wakeup(Scheme_Input_Port *port, void *fds)
{
  Scheme_FD *fip;
  void *fds2;
  int n;

  fip = (Scheme_FD *)port->port_data;

  n = fip->fd;
  MZ_FD_SET(n, (fd_set *)fds);
  fds2 = MZ_GET_FDSET(fds, 2);
  MZ_FD_SET(n, (fd_set *)fds2);
}

static Scheme_Object *
make_fd_input_port(int fd, const char *filename)
{
  Scheme_Input_Port *ip;
  Scheme_FD *fip;

  fip = MALLOC_ONE_RT(Scheme_FD);
#ifdef MZTAG_REQUIRED
  fip->type = scheme_rt_input_fd;
#endif

  fip->fd = fd;
  fip->bufcount = 0;

  ip = _scheme_make_input_port(fd_input_port_type,
			       fip,
			       fd_getc,
			       NULL,
			       fd_char_ready,
			       fd_close_input,
			       fd_need_wakeup,
			       1);

  {
    char *s;
    s = scheme_strdup(filename);
    ip->name = s;
  }

  return (Scheme_Object *)ip;
}

#endif

#ifdef USE_OSKIT_CONSOLE

# ifdef OSKIT_TEST
static Scheme_Object *normal_stdin;
static int direct_cons_trygetchar() { return scheme_char_ready(normal_stdin) ? scheme_getc(normal_stdin) : -1; }
static void direct_cons_putchar(int c) { }
# define convert_scan_code(x) x
# else
#  include "pc_keys.inc"
# endif

typedef struct osk_console_input {
  MZTAG_IF_REQUIRED
  int count, size, ready;
  unsigned char *buffer;
  struct osk_console_input *next; /* typeahead */
} osk_console_input;

static int
osk_char_ready (Scheme_Input_Port *port)
{
  osk_console_input *osk, *orig;
  int k;

  osk = orig = (osk_console_input *)port->port_data;

  while (osk->ready) {
    if (osk->next)
      osk = osk->next;
    else {
      osk->next = MALLOC_ONE(osk_console_input);
#ifdef MZTAG_REQUIRED
      osk->type = scheme_rt_oskit_console_input;
#endif
      osk = osk->next;
      osk->count = osk->size = osk->ready = 0;
      osk->buffer = NULL;
      osk->next = NULL;
    }
  }

  k = direct_cons_trygetchar();
  k = convert_scan_code(k); /* defined in pc_keys.inc; handles ctl-alt-del */
  if (k > 0) {
    if (k == 3) { /* Ctl-C */
      scheme_break_thread(NULL);
    } else if (k == 4) { /* Ctl-D */
      if (!osk->count)
	/* ready with !count => EOF */
	osk->ready = 1;
    } else if (k == 8) { /* Backspace */
      if (osk->count) {
	direct_cons_putchar(8);
	direct_cons_putchar(' '); /* space erases old letter */
	direct_cons_putchar(8);
	--osk->count;
      }
    } else {
      if (osk->count == osk->size) {
	char *naya;
	osk->size = osk->size ? 2 * osk->size : 256;
	naya = scheme_malloc_atomic(osk->size);
	memcpy(naya, osk->buffer, osk->count);
	osk->buffer = naya;
      }
      osk->buffer[osk->count++] = k;
      if (k == 13 || k == 10) { /* Return/newline */
	direct_cons_putchar(13);
	direct_cons_putchar(10);
	osk->ready = 1;
      } else
	direct_cons_putchar(k);
    }
  }

  if (orig->ready)
    return 1;
  else
    return 0;
}

static int osk_getc(Scheme_Input_Port *port)
{
  int c;
  osk_console_input *osk;

  if (!osk_char_ready(port)) {
    scheme_current_process->block_descriptor = PORT_BLOCKED;
    scheme_current_process->blocker = (Scheme_Object *)port;
    do {
      scheme_process_block(0.0);
    } while (!osk_char_ready(port));
    scheme_current_process->block_descriptor = NOT_BLOCKED;
    scheme_current_process->blocker = NULL;
    scheme_current_process->ran_some = 1;
  }

  osk = (osk_console_input *)port->port_data;

  if (!osk->count) {
    /* EOF */
    osk->ready = 0;
    return EOF;
  }

  c = osk->buffer[osk->ready - 1];
  osk->ready++;
  if (osk->ready > osk->count) {
    if (osk->next) {
      /* Copy typeahead to here */
      osk_console_input *next = osk->next;
      memcpy(osk, next, sizeof(osk_console_input));
    } else
      osk->ready = osk->count = 0;
  }

  return c;
}

static void
osk_close_input(Scheme_Input_Port *port)
{
}

static void
osk_need_wakeup(Scheme_Input_Port *port, void *fds)
{
# ifdef OSKIT_TEST
  /* for testing, write to stdout is almost certainly ready: */
  void *fdw;
  fdw = MZ_GET_FDSET(fds, 1);
  MZ_FD_SET(1, (fd_set *)fdw);
# endif

  /* In OSKit, makes select() return immediately */
  MZ_FD_SET(0, (fd_set *)fds);
}

static Scheme_Object *
make_oskit_console_input_port()
{
  Scheme_Input_Port *ip;
  osk_console_input *osk;
  
  osk = MALLOC_ONE_RT(osk_console_input);
#ifdef MZTAG_REQUIRED
  osk->type = scheme_rt_oskit_console_input;
#endif

  osk->count = osk->size = osk->ready = 0;
  osk->buffer = NULL;
  osk->next = NULL;

# ifdef OSKIT_TEST
  REGISTER_SO(normal_stdin);
  normal_stdin = make_tested_file_input_port(stdin, "STDIN", 1);
# endif

  ip = _scheme_make_input_port(oskit_console_input_port_type,
			       osk,
			       osk_getc,
			       NULL,
			       osk_char_ready,
			       osk_close_input,
			       osk_need_wakeup,
			       1);
  
  ip->name = "STDIN";
  
  return (Scheme_Object *)ip;
}

void scheme_check_keyboard_input(void)
{
  osk_char_ready((Scheme_Input_Port *)scheme_orig_stdin_port);
}

#endif

/* Win32/BeOS input ports that could block on reads. The BeOS
   contortions are resonable, and they'll probably fix select()
   in the future so this code is unneeded. The Win32 contortions
   are insane, inefficient, and apparently unavoidable. */
#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)

#define TIF_BUFFER 500

typedef struct {
  MZTAG_IF_REQUIRED
  FILE *fp;
  OS_THREAD_TYPE th;             /* worker thread */
  OS_SEMAPHORE_TYPE try_sema;    /* hit when a char is wanted */
  OS_SEMAPHORE_TYPE ready_sema;  /* hit when a char is ready */
#ifdef WIN32_FD_HANDLES
  int type; /* console or pipe? */
  OS_SEMAPHORE_TYPE interrupt;  /* hit when a char is ready */
  int stupid_eof_check;
  OS_SEMAPHORE_TYPE stupid_eof_check_going;
  struct Scheme_Thread_Memory *thread_memory;
#endif
  int need_wait;                 /* 1 => use ready_sema */
  OS_MUTEX_TYPE lock_mutex;      /* lock on remaining fields */
  int trying;          /* indicates that it's already trying to read */
  int ready;           /* indicates that a character is ready */
  int err_no;          /* indicates an error */
  int c[TIF_BUFFER];   /* ready character */
} Tested_Input_File;

#ifdef WIN32_FD_HANDLES
# define RELEASE_SEMAPHORE(sem) ReleaseSemaphore(sem, 1, NULL);
# define TRY_WAIT_SEMAPHORE(sem) (WaitForSingleObject(sem, 0) == WAIT_OBJECT_0)
# define WAIT_SEMAPHORE(sem) WaitForSingleObject(sem, INFINITE);
# define WAIT_THREAD(th) WaitForSingleObject(th, INFINITE);
# define MAKE_SEMAPHORE() CreateSemaphore(NULL, 0, TIF_BUFFER, NULL);
# define FREE_SEMAPHORE(sem) CloseHandle(sem)
# define MAKE_MUTEX(m) InitializeCriticalSection(&m)
# define ACQUIRE_MUTEX(m) EnterCriticalSection(&m)
# define RELEASE_MUTEX(m) LeaveCriticalSection(&m)
# define FREE_MUTEX(m) DeleteCriticalSection(&m)
#endif
#ifdef USE_BEOS_PORT_THREADS
# define RELEASE_SEMAPHORE(sem) release_sem(sem)
# define TRY_WAIT_SEMAPHORE(sem) (acquire_sem_etc(sem, 1, B_TIMEOUT, 0) == B_NO_ERROR)
# define WAIT_SEMAPHORE(sem) while (acquire_sem(sem) != B_NO_ERROR) {}
static status_t mz_thread_status;
# define WAIT_THREAD(th) wait_for_thread(th, &mz_thread_status)
# define MAKE_SEMAPHORE() create_sem(0, NULL)
# define FREE_SEMAPHORE(sem) delete_sem(sem)
# define MAKE_MUTEX(m) (m.v = 0, m.s = MAKE_SEMAPHORE())
# define ACQUIRE_MUTEX(m) if (atomic_add(&m.v, 1) >= 1) { WAIT_SEMAPHORE(m.s); }
# define RELEASE_MUTEX(m) if (atomic_add(&m.v, -1) > 1) { RELEASE_SEMAPHORE(m.s); }
# define FREE_MUTEX(m) FREE_SEMAPHORE(m.s)

static sem_id got_started;
#endif

static int tested_file_char_ready(Scheme_Input_Port *p)
{
  Tested_Input_File *tip;

  tip = (Tested_Input_File *)p->port_data;

  if (tip->ready || tip->err_no)
    return 1;
 
  if (!tip->trying) {
    tip->trying = TIF_BUFFER;
    RELEASE_SEMAPHORE(tip->try_sema);
  }

  return 0;
}

static int tested_file_getc(Scheme_Input_Port *p)
{
  Tested_Input_File *tip;

  tip = (Tested_Input_File *)p->port_data;

  if (!tested_file_char_ready(p)) {
    scheme_current_process->block_descriptor = PORT_BLOCKED;
    scheme_current_process->blocker = (Scheme_Object *)p;
    do {
      scheme_process_block((float)0.0);
    } while (!tested_file_char_ready(p));
    scheme_current_process->block_descriptor = NOT_BLOCKED;
    scheme_current_process->blocker = NULL;
    scheme_current_process->ran_some = 1;
  }

  if (tip->ready) {
    int c;
    ACQUIRE_MUTEX(tip->lock_mutex);
    if (tip->need_wait) {
      WAIT_SEMAPHORE(tip->ready_sema);
      tip->need_wait = 0;
    }
    c = tip->c[--tip->trying];
    --tip->ready;
    RELEASE_MUTEX(tip->lock_mutex);
    return c;
  } else {
    /* must be an error */
    scheme_raise_exn(MZEXN_I_O_PORT_READ,
		     p,
		     "error reading from file port (%d)",
		     tip->err_no);
    return EOF;
  }
}

static void tested_file_close_input(Scheme_Input_Port *p)
{
  Tested_Input_File *tip;

  tip = (Tested_Input_File *)p->port_data;

  tip->err_no = 1;

#ifdef WIN32_FD_HANDLES
  /* Tell reader thread to stop */
  RELEASE_SEMAPHORE(tip->interrupt);
#endif

  fclose(tip->fp); /* BeOS: sends eof to reader thread */

  --scheme_file_open_count;

  RELEASE_SEMAPHORE(tip->try_sema);

#ifdef WIN32_FD_HANDLES
  if (WaitForSingleObject(tip->th, 5000) == WAIT_TIMEOUT) {
    /* kill it if it's still there; according to the docs, we
       shouldn't do this, but my experience is that if something
       goes wrong, killing the thread makes my NT machine less
       likely to hang. */
    printf("have to kill reader thread\n");
    TerminateThread(tip->th, -1);
  }
  scheme_forget_thread(tip->thread_memory);
  CloseHandle(tip->th);
  if (tip->stupid_eof_check_going)
    CloseHandle(tip->stupid_eof_check_going);
#else
  WAIT_THREAD(tip->th);
#endif

  FREE_SEMAPHORE(tip->ready_sema);
  FREE_SEMAPHORE(tip->try_sema);
  FREE_MUTEX(tip->lock_mutex);
}

static void tested_file_need_wakeup(Scheme_Input_Port *p, void *fds)
{
  Tested_Input_File *tip;

  tip = (Tested_Input_File *)p->port_data;

  if (!tip->need_wait) {
    ACQUIRE_MUTEX(tip->lock_mutex);
    if (tip->ready) {
      /* A char turned up as we were preparing the wakeup... */
      RELEASE_SEMAPHORE(tip->ready_sema);
      tip->need_wait = -1;
    } else
      tip->need_wait = 1;
    RELEASE_MUTEX(tip->lock_mutex);
  }

  scheme_add_fd_handle((void *)tip->ready_sema, fds, 1);
}

#ifdef WIN32_FD_HANDLES

static int was_break;
static BOOL CALLBACK that_was_a_break(DWORD x)
{
  if (x != CTRL_C_EVENT)
    return FALSE;

  was_break = 1;

  {
    Scheme_Process *p = scheme_main_process;
    if (!p->external_break)
      scheme_break_thread(p);
  }

  /* Unreliabale hack: */
  Sleep(100); /* Give broken port time to notice it's a break */
  was_break = 0;

  return TRUE;
}

static long StupidEofCheck(Tested_Input_File *tip)
{
  DWORD got;

  RELEASE_SEMAPHORE(tip->stupid_eof_check_going);
  
  if (ReadFile((HANDLE)_get_osfhandle(_fileno(tip->fp)), NULL, 0, &got, NULL))
    tip->stupid_eof_check = 1;
  
  return 0;
}

static int stupid_windows_machine;

#endif

static long read_for_tested_file(void *data)
{
  Tested_Input_File *tip;
  int c = 0, i;

#ifdef USE_BEOS_PORT_THREADS
  signal(SIGINT, SIG_IGN);
  RELEASE_SEMAPHORE(got_started);
#endif

#ifdef WIN32_FD_HANDLES
  /* Try to make ctl-c work. */
  SetConsoleCtrlHandler(that_was_a_break, TRUE);
#endif

  tip = (Tested_Input_File *)data;

  while (!tip->err_no) {
    WAIT_SEMAPHORE(tip->try_sema);
    for (i = tip->trying; !tip->err_no && i--; ) {
    try_again:
#ifdef WIN32_FD_HANDLES
      if (!tip->fp->_cnt) {
	HANDLE file = (HANDLE)_get_osfhandle(_fileno(tip->fp));
	if (tip->type == FILE_TYPE_CHAR) {
	  /* Console */
	  HANDLE h[2];
	  h[0] = file;
	  h[1] = tip->interrupt;
	  WaitForMultipleObjectsEx(2, h, FALSE, INFINITE, TRUE);
	} else {
	  /* Pipe */
	  /* Is there really no better solution than polling?! */
	  /* We can't use ReadFileEx because the pipe isn't necessarily
	     created as overlayed. */
	  while (!tip->err_no) {
	    DWORD avail;
	    if (!PeekNamedPipe(file, NULL, 0, NULL, &avail, NULL))
	      break; /* let fgetc handle error */
	    if (avail)
	      break;

	    /* Windows 98: PeekNamedPipe doesn't detect EOF! */
	    if (stupid_windows_machine > 0) {
	      DWORD id;
	      HANDLE th;

	      if (!tip->stupid_eof_check_going)
		tip->stupid_eof_check_going = MAKE_SEMAPHORE();

	      /* Perhaps unlikely: parent thread is memorized, yet: */
	      while (!tip->thread_memory) {
		Sleep(1);
	      }

	      tip->stupid_eof_check = 0;
	      th = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)StupidEofCheck, tip, 0, &id);
	      scheme_remember_subthread(tip->thread_memory, (void *)th);
	      WAIT_SEMAPHORE(tip->stupid_eof_check_going);

	      WaitForSingleObject(th, 100);
	      TerminateThread(th, 0);
	      scheme_forget_subthread(tip->thread_memory);
	      CloseHandle(th);

	      if (tip->stupid_eof_check)
		break;
	    }

	    Sleep(100); /* Yuck Yuck Yuck Yuck */
	  }
	}
	if (tip->err_no)
	  break;
      }
#endif
      c = fgetc(tip->fp);
      tip->c[i] = c;
      if (c == EOF) {
	if (!feof(tip->fp)) {
	  tip->err_no = errno;
	  if (!tip->err_no)
	    tip->err_no = 1;
	} else {
#ifdef WIN32_FD_HANDLES
	  if (tip->type == FILE_TYPE_CHAR) {
	    /* Console */
	    /* Unreliabale hack: */
	    Sleep(100); /* Give the break thread time, if it's there */
	    if (was_break)
	      goto try_again;
	  }
#endif
	}
      }
      if (!tip->err_no) {
	ACQUIRE_MUTEX(tip->lock_mutex);
	tip->ready++;
	if (tip->need_wait > 0) {
	  RELEASE_SEMAPHORE(tip->ready_sema);
	  tip->need_wait = -1;
	}
	RELEASE_MUTEX(tip->lock_mutex);
      } else /* was error */
	RELEASE_SEMAPHORE(tip->ready_sema);
    }
  }

  return 0;
}

static Scheme_Object *make_tested_file_input_port(FILE *fp, char *name, int tested)
{
  Scheme_Input_Port *ip;
  Tested_Input_File *tip;

  if (!tested)
    return scheme_make_named_file_input_port(fp, name);
    
  tip = MALLOC_ONE_RT(Tested_Input_File);
#ifdef MZTAG_REQUIRED
  tip->type = scheme_rt_tested_input_file;
#endif

  tip->fp = fp;

  tip->ready = 0;
  tip->trying = 0;
  tip->err_no = 0;
  tip->ready_sema = MAKE_SEMAPHORE();
  tip->try_sema = MAKE_SEMAPHORE();
#ifdef WIN32_FD_HANDLES
  tip->type = GetFileType((HANDLE)_get_osfhandle(_fileno(tip->fp)));
  tip->interrupt = MAKE_SEMAPHORE();
  tip->stupid_eof_check_going = NULL;
#endif
  MAKE_MUTEX(tip->lock_mutex);

#ifdef WIN32_FD_HANDLES
  if (!stupid_windows_machine) {
    OSVERSIONINFO info;
    info.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&info);
    if (info.dwPlatformId == VER_PLATFORM_WIN32_NT)
      stupid_windows_machine = -1; /* not as stupid */
    else
      stupid_windows_machine = 1;
  }

  /* Remap stdin, so system and process* don't get hung up. */
  if (!_fileno(fp))
    tip->fp = _fdopen(_dup(0), "rt");

# ifdef NO_NEED_FOR_BEGINTHREAD
#  define _beginthreadex CreateThread
# endif
  {
    DWORD id;
    tip->th = (void *)_beginthreadex(NULL, 5000, 
				     (LPTHREAD_START_ROUTINE)read_for_tested_file,
				     tip, 0, &id);
    tip->thread_memory = scheme_remember_thread((void *)tip->th);
  }
#endif

#ifdef USE_BEOS_PORT_THREADS
  {
    sigset_t sigs;

    if (!got_started)
      got_started = create_sem(0, NULL);

    /* Disable SIGINT until the child starts ignoring it */
    sigemptyset(&sigs);
    sigaddset(&sigs, SIGINT);
    sigprocmask(SIG_BLOCK, &sigs, NULL);

    tip->th = spawn_thread(read_for_tested_file, "port reader", 
			   B_NORMAL_PRIORITY, tip);
    if (tip->th < 0)
      tip->th = 0;
    else
      resume_thread(tip->th);

    WAIT_SEMAPHORE(got_started);
    
    sigprocmask(SIG_UNBLOCK, &sigs, NULL);
  }
#endif

  if (!tip->th) {
    /* Thread creation failed; give up niceness: */
    return scheme_make_named_file_input_port(fp, name);
  }

  ip = _scheme_make_input_port(tested_file_input_port_type,
			       tip,
			       tested_file_getc,
			       NULL,
			       tested_file_char_ready,
			       tested_file_close_input,
			       tested_file_need_wakeup,
			       1);

  ip->name = scheme_strdup(name);

  return (Scheme_Object *)ip;
}

#endif

/* string input ports */

static int 
string_getc (Scheme_Input_Port *port)
{
  Scheme_Indexed_String *is;

  is = (Scheme_Indexed_String *) port->port_data;
  if (is->index >= is->size)
    return (EOF);
  else
    return (unsigned char)(is->string[is->index++]);
}

static int
string_char_ready (Scheme_Input_Port *port)
{
  Scheme_Indexed_String *is;

  is = (Scheme_Indexed_String *) port->port_data;
  return (is->index < is->size);
}

static void
string_close_in (Scheme_Input_Port *port)
{
  return;
}

static Scheme_Indexed_String *
make_indexed_string (const char *str, long len)
{
  Scheme_Indexed_String *is;

  is = MALLOC_ONE_RT(Scheme_Indexed_String);
#ifdef MZTAG_REQUIRED
  is->type = scheme_rt_indexed_string;
#endif

  if (str) {
    if (len < 0) {
      is->string = (char *)str;
      is->size = -len;
    } else {
      char *ca;
      ca = (char *)scheme_malloc_atomic(len);
      is->string = ca;
      memcpy(is->string, str, len);
      is->size = len;
    }
  } else {
    char *ca;
    is->size = 100;
    ca = (char *)scheme_malloc_atomic(is->size + 1);
    is->string = ca;
  }
  is->index = 0;
  return (is);
}

Scheme_Object *
scheme_make_sized_string_input_port(const char *str, long len)
{
  Scheme_Input_Port *ip;

  ip = _scheme_make_input_port(string_input_port_type,
			       make_indexed_string(str, len),
			       string_getc,
			       NULL,
			       string_char_ready,
			       string_close_in,
			       NULL, 
			       0);

  ip->name = "STRING";

  return (Scheme_Object *)ip;
}

Scheme_Object *
scheme_make_string_input_port(const char *str)
{
  return scheme_make_sized_string_input_port(str, strlen(str));
}

/* string output ports */

static void
string_write_string(char *str, long len, Scheme_Output_Port *port)
{
  Scheme_Indexed_String *is;

  is = (Scheme_Indexed_String *) port->port_data;

  if (is->index + len >= is->size) {
    char *old;

    old = is->string;

    if (len > is->size)
      is->size += 2 * len;
    else
      is->size *= 2;

    {
      char *ca;
      ca = (char *)scheme_malloc_atomic(is->size + 1);
      is->string = ca;
    }
    memcpy(is->string, old, is->index);
  }
  
  memcpy(is->string + is->index, str, len);
  is->index += len;
}

static void
string_close_out (Scheme_Output_Port *port)
{
  return;
}

Scheme_Object *
scheme_make_string_output_port (void)
{
  Scheme_Output_Port *op;

  op = scheme_make_output_port (string_output_port_type,
				make_indexed_string(NULL, 0),
				string_write_string,
				string_close_out,
				0);

  return (Scheme_Object *)op;
}

char *
scheme_get_sized_string_output(Scheme_Object *port, int *size)
{
  Scheme_Output_Port *op;
  Scheme_Indexed_String *is;
  char *v;
  long len;

  if (!SCHEME_OUTPORTP(port))
    return NULL;

  op = (Scheme_Output_Port *)port;
  if (op->sub_type != string_output_port_type)
    return NULL;

  is = (Scheme_Indexed_String *)op->port_data;

  len = is->index;
  if (is->u.hot > len)
    len = is->u.hot;

  v = (char *)scheme_malloc_atomic(len + 1);
  memcpy(v, is->string, len);
  v[len] = 0;
  
  if (size)
    *size = len;

  return v;
}

char *
scheme_get_string_output(Scheme_Object *port)
{
  return scheme_get_sized_string_output(port, NULL);
}

/* user input ports */

static int 
user_getc (Scheme_Input_Port *port)
{
  Scheme_Object *fun, *val;

  fun = ((Scheme_Object **) port->port_data)[0];
  val = _scheme_apply(fun, 0, NULL);
  if (SCHEME_EOFP(val))
    return EOF;
  else {
    if (!SCHEME_CHARP(val))
      scheme_raise_exn(MZEXN_I_O_PORT_USER,
		       port,
		       "port: user read-char returned a non-character");
    return (unsigned char)SCHEME_CHAR_VAL(val);
  }
}

static int 
user_peekc (Scheme_Input_Port *port)
{
  Scheme_Object *fun, *val;

  fun = ((Scheme_Object **) port->port_data)[3];
  val = _scheme_apply(fun, 0, NULL);
  if (SCHEME_EOFP(val))
    return EOF;
  else {
    if (!SCHEME_CHARP(val))
      scheme_raise_exn(MZEXN_I_O_PORT_USER,
		       port,
		       "port: user peek-char returned a non-character");
    return (unsigned char)SCHEME_CHAR_VAL(val);
  }
}

static int
user_char_ready(Scheme_Input_Port *port)
{
  Scheme_Object *fun, *val;

  fun = ((Scheme_Object **) port->port_data)[1];
  val = _scheme_apply(fun, 0, NULL);
  return SAME_OBJ(val, scheme_true);
}

static void
user_close_input(Scheme_Input_Port *port)
{
  Scheme_Object *fun;

  fun = ((Scheme_Object **) port->port_data)[2];
  _scheme_apply_multi(fun, 0, NULL);
}

/* file output ports */

static void
file_write_string(char *str, long len, Scheme_Output_Port *port)
{
  FILE *fp;

  if (!len)
    return;

  fp = ((Scheme_Output_File *)port->port_data)->f;

  if (fwrite(str, len, 1, fp) != 1) {
    scheme_raise_exn(MZEXN_I_O_PORT_WRITE,
		     port,
		     "error writing to file port (%d)",
		     errno);
    return;
  }

  while (len--) {
    if (*str == '\n' || *str == '\r') {
      fflush(fp);
      break;
    }
    str++;
  }
}

static void
file_close_output(Scheme_Output_Port *port)
{
  Scheme_Output_File *fop = (Scheme_Output_File *)port->port_data;
  FILE *fp = fop->f;

  fclose(fp);
  --scheme_file_open_count;
}

Scheme_Object *
scheme_make_file_output_port(FILE *fp)
{
  Scheme_Output_File *fop;

  if (!fp)
    scheme_signal_error("make-file-out-port(internal): "
			"null file pointer");
  
  fop = MALLOC_ONE_RT(Scheme_Output_File);
#ifdef MZTAG_REQUIRED
  fop->type = scheme_rt_output_file;
#endif

  fop->f = fp;

  return (Scheme_Object *)scheme_make_output_port(file_output_port_type,
						  fop,
						  file_write_string,
						  file_close_output,
						  1);
}

#ifdef USE_FD_PORTS
/* fd output ports */

static void
fd_write_string(char *str, long len, Scheme_Output_Port *port)
{
  Scheme_FD *fop;
  long l;

  if (!len)
    return;

  fop = (Scheme_FD *)port->port_data;

  l = MZPORT_FD_BUFFSIZE - fop->bufcount;
  if (len <= l) {
    memcpy(fop->buffer + fop->bufcount, str, len);
    fop->bufcount += len;
  } else {
    if (fop->bufcount)
      flush_fd(port, NULL, 0);
    if (len <= MZPORT_FD_BUFFSIZE) {
      memcpy(fop->buffer, str, len);
      fop->bufcount = len;
    } else {
      flush_fd(port, str, len);
      return; /* Don't check for flush */
    }
  }

  while (len--) {
    if (*str == '\n' || *str == '\r') {
      flush_fd(port, NULL, 0);
      break;
    }
    str++;
  }
}

static void
fd_close_output(Scheme_Output_Port *port)
{
  Scheme_FD *fop = (Scheme_FD *)port->port_data;

  close(fop->fd);
}

Scheme_Object *
make_fd_output_port(int fd)
{
  Scheme_FD *fop;

  fop = MALLOC_ONE_RT(Scheme_FD);
#ifdef MZTAG_REQUIRED
  fop->type = scheme_rt_input_fd;
#endif

  fop->fd = fd;
  fop->bufcount = 0;

  /* Make output non-blocking: */
  fcntl(fd, F_SETFL, FD_NONBLOCKING);

  return (Scheme_Object *)scheme_make_output_port(fd_output_port_type,
						  fop,
						  fd_write_string,
						  fd_close_output,
						  1);
}
#endif

/* user output ports */

static void
user_write(char *str, long len, Scheme_Output_Port *port)
{
  Scheme_Object *fun, *p[1];
  
  p[0] = scheme_make_sized_string(str, len, 1);

  fun = ((Scheme_Object **) port->port_data)[0];
  _scheme_apply_multi(fun, 1, p);
}

static void
user_close_output (Scheme_Output_Port *port)
{
  Scheme_Object *fun;

  fun = ((Scheme_Object **) port->port_data)[1];
  _scheme_apply_multi(fun, 0, NULL);
}

static Scheme_Object *
input_port_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_INPORTP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
output_port_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_OUTPORTP(argv[0]) ? scheme_true : scheme_false);
}

#define CURRENT_INPUT_PORT(config) scheme_get_param(config, MZCONFIG_INPUT_PORT)
#define CURRENT_OUTPUT_PORT(config) scheme_get_param(config, MZCONFIG_OUTPUT_PORT)

static Scheme_Object *current_input_port(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-input-port", scheme_make_integer(MZCONFIG_INPUT_PORT),
			     argc, argv,
			     -1, input_port_p, "input-port", 0);
}

static Scheme_Object *current_output_port(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-output-port", scheme_make_integer(MZCONFIG_OUTPUT_PORT),
			     argc, argv,
			     -1, output_port_p, "output-port", 0);
}

static Scheme_Object *current_error_port(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-error-port", scheme_make_integer(MZCONFIG_ERROR_PORT),
			     argc, argv,
			     -1, output_port_p, "error port", 0);
}

static Scheme_Object *
make_input_port(int argc, Scheme_Object *argv[])
{
  Scheme_Input_Port *ip;
  Scheme_Object **copy;

  scheme_check_proc_arity("make-input-port", 0, 0, argc, argv);
  scheme_check_proc_arity("make-input-port", 0, 1, argc, argv);
  scheme_check_proc_arity("make-input-port", 0, 2, argc, argv);
  if (argc > 3)
    scheme_check_proc_arity("make-input-port", 0, 3, argc, argv);
  
  copy = MALLOC_N_STUBBORN(Scheme_Object *, argc);
  memcpy(copy, argv, argc * sizeof(Scheme_Object *));
  scheme_end_stubborn_change((void *)copy);

  ip = _scheme_make_input_port(user_input_port_type,
			       copy,
			       user_getc,
			       (argc > 3) ? user_peekc : NULL,
			       user_char_ready,
			       user_close_input,
			       NULL,
			       0);

  ip->name = "USERPORT";

  return (Scheme_Object *)ip;
}

static Scheme_Object *
make_output_port (int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;
  Scheme_Object **copy;

  scheme_check_proc_arity("make-output-port", 1, 0, argc, argv);
  scheme_check_proc_arity("make-output-port", 0, 1, argc, argv);

  copy = MALLOC_N_STUBBORN(Scheme_Object *, 2);
  memcpy(copy, argv, 2 * sizeof(Scheme_Object *));
  scheme_end_stubborn_change((void *)copy);

  op = scheme_make_output_port(user_output_port_type,
			       copy,
			       user_write,
			       user_close_output,
			       0);

  return (Scheme_Object *)op;
}

static void filename_exn(char *name, char *msg, char *filename, int err)
{
  char *dir, *drive;
  int len;
  char *pre, *rel, *post;

  len = strlen(filename);

  if (scheme_is_relative_path(filename, len)) {
    dir = scheme_os_getcwd(NULL, 0, NULL, 1);
    drive = NULL;
  } else if (scheme_is_complete_path(filename, len)) {
    dir = NULL;
    drive = NULL;
  } else {
    dir = NULL;
    drive = scheme_getdrive();
  }
  
  pre = dir ? " in directory \"" : (drive ? " on drive " : "");
  rel = dir ? dir : (drive ? drive : "");
  post = dir ? "\"" : "";

  scheme_raise_exn(MZEXN_I_O_FILESYSTEM,
		   scheme_make_string(filename),
		   "%s: %s: \"%.255s\"%s%.255s%s (%d%s)", 
		   name, msg, filename,
		   pre, rel, post,
		   err,
#ifdef EMFILE
		   ((err == EMFILE) || (err == ENFILE)
		    ? " - too many files open" 
		    : "")
#else
		   ""
#endif
		   );
}

static Scheme_Object *
do_open_input_file(char *name, int offset, int argc, Scheme_Object *argv[])
{
  FILE *fp;
  char *mode = "rb";
  char *filename;
  int regfile;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type(name, "string", 0, argc, argv);
  if (argc > 1 + offset) {
    if (!SCHEME_SYMBOLP(argv[offset + 1]))
      scheme_wrong_type(name, "symbol", offset + 1, argc, argv);
    
    if (SAME_OBJ(argv[offset + 1], text_symbol))
      mode = "rt";
    else if (SAME_OBJ(argv[offset + 1], binary_symbol)) {
      /* This is the default */
    } else
      scheme_raise_exn(MZEXN_APPLICATION_TYPE,
		       argv[offset + 1],
		       scheme_intern_symbol("input file mode"),
		       "%s: bad mode: %s%s", name,
		       scheme_make_provided_string(argv[offset + 1], 1, NULL),
		       scheme_make_args_string("other ", offset + 1, argc, argv));
  }
  
  filename = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
				    SCHEME_STRTAG_VAL(argv[0]),
				    name,
				    NULL);

  if (scheme_directory_exists(filename)) {
    filename_exn(name, "cannot open directory as a file", filename, errno);
    return scheme_void;
  }

  regfile = scheme_is_regular_file(filename);

  fp = fopen(filename, mode);
  if (!fp)
    filename_exn(name, "cannot open input file", filename, errno);
  scheme_file_open_count++;
  
  return _scheme_make_named_file_input_port(fp, filename, regfile);
}

static Scheme_Object *
open_input_file (int argc, Scheme_Object *argv[])
{
  return do_open_input_file("open-input-file", 0, argc, argv);
}

static Scheme_Object *
open_input_string (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("open-input-string", "string", 0, argc, argv);

  return scheme_make_sized_string_input_port(SCHEME_STR_VAL(argv[0]), 
					     SCHEME_STRTAG_VAL(argv[0]));
}

static Scheme_Object *
do_open_output_file (char *name, int offset, int argc, Scheme_Object *argv[])
{
  FILE *fp;
  int e_set = 0, m_set = 0, i;
  int existsok = 0, namelen;
  char *filename;
  char mode[2];
#ifdef MAC_FILE_SYSTEM
  int creating = 1;
#endif

  mode[0] = 'w';
  mode[1] = 'b';
  
  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type(name, "string", 0, argc, argv);
  
  for (i = 1 + offset; argc > i; i++) {
    if (!SCHEME_SYMBOLP(argv[i]))
      scheme_wrong_type(name, "symbol", i, argc, argv);
  
    if (SAME_OBJ(argv[i], append_symbol)) {
      mode[0] = 'a';
      existsok = -1;
      e_set++;
    } else if (SAME_OBJ(argv[i], replace_symbol)) {
      existsok = 1;
      e_set++;
    } else if (SAME_OBJ(argv[i], truncate_symbol)) {
      existsok = -1;
      e_set++;
    } else if (SAME_OBJ(argv[i], truncate_replace_symbol)) {
      existsok = -2;
      e_set++;
    } else if (SAME_OBJ(argv[i], error_symbol)) {
      /* This is the default */
      e_set++;
    } else if (SAME_OBJ(argv[i], text_symbol)) {
      mode[1] = 't';
      m_set++;
    } else if (SAME_OBJ(argv[i], binary_symbol)) {
      /* This is the default */
      m_set++;
    } else
      scheme_raise_exn(MZEXN_APPLICATION_TYPE,
		       argv[i],
		       scheme_intern_symbol("output file mode"),
		       "%s: bad mode: %s%s", name,
		       scheme_make_provided_string(argv[i], 1, NULL),
		       scheme_make_args_string("other ", i, argc, argv));

    if (m_set > 1 || e_set > 1)
      scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		       argv[i],
		       "%s: conflicting or redundant "
		       "file modes given%s", name,
		       scheme_make_args_string("", -1, argc, argv));
  }

  filename = SCHEME_STR_VAL(argv[0]);
  namelen = SCHEME_STRTAG_VAL(argv[0]);

  filename = scheme_expand_filename(filename, namelen, name, NULL);

  if (scheme_directory_exists(filename)) {
    filename_exn(name, "cannot open directory as a file", filename, errno);
    return scheme_void;
  }

#ifndef MAC_FILE_SYSTEM
  if (existsok >= 0) {
#endif
    if (scheme_file_exists(filename)) {
      if (!existsok)
	scheme_raise_exn(MZEXN_I_O_FILESYSTEM,
			 argv[0],
			 "%s: file \"%s\" exists", name, filename);
#ifdef MAC_FILE_SYSTEM
      if (existsok > 0) {
#endif
	if (MSC_IZE(unlink)(filename))
	  scheme_raise_exn(MZEXN_I_O_FILESYSTEM,
			   argv[0],
			   "%s: error deleting \"%s\"", 
			   name, filename);
#ifdef MAC_FILE_SYSTEM
      } else
	creating = 0;
#endif
    }
#ifndef MAC_FILE_SYSTEM
  }
#endif

  fp = fopen(filename, mode);
  if (!fp) {
    if (existsok < -1) {
      /* Can't truncate; try to replace */
      if (scheme_file_exists(filename)) {
	if (MSC_IZE(unlink)(filename))
	  scheme_raise_exn(MZEXN_I_O_FILESYSTEM,
			   argv[0],
			   "%s: error deleting \"%s\"", 
			   name, filename);
	else {
	  fp = fopen(filename, mode);
#ifdef MAC_FILE_SYSTEM
	  creating = 1;
#endif
	}
      }
    }
    if (!fp)
      filename_exn(name, "cannot open output file", filename, errno);
  }
  scheme_file_open_count++;

#ifdef MAC_FILE_SYSTEM
  if (creating)
    scheme_file_create_hook(filename);
#endif

  return scheme_make_file_output_port(fp);
}

static Scheme_Object *
open_output_file (int argc, Scheme_Object *argv[])
{
  return do_open_output_file("open-output-file", 0, argc, argv);
}

static Scheme_Object *
open_output_string (int argc, Scheme_Object *argv[])
{
  return scheme_make_string_output_port();
}

static Scheme_Object *
get_output_string (int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;
  char *s;
  int size;

  op = (Scheme_Output_Port *)argv[0];
  if (!SCHEME_OUTPORTP(argv[0]) 
      || (op->sub_type != string_output_port_type))
    scheme_wrong_type("get-output-string", "string output port", 0, argc, argv);

  s = scheme_get_sized_string_output(argv[0], &size);

  return scheme_make_sized_string(s, size, 1);
}

static Scheme_Object *
close_input_port (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("close-input-port", "input-port", 0, argc, argv);

  scheme_close_input_port (argv[0]);
  return (scheme_void);
}

static Scheme_Object *
close_output_port (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("close-output-port", "output-port", 0, argc, argv);

  scheme_close_output_port (argv[0]);
  return (scheme_void);
}

static Scheme_Object *
call_with_output_file (int argc, Scheme_Object *argv[])
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *port, *v, **m;

  scheme_check_proc_arity("call-with-output-file", 1, 1, argc, argv);

  port = do_open_output_file("call-with-output-file", 1, argc, argv);
  
  v = _scheme_apply_multi(argv[1], 1, &port);

  m = p->ku.multiple.array;
  scheme_close_output_port(port);
  p->ku.multiple.array = m;

  return v;
}

static Scheme_Object *
call_with_input_file(int argc, Scheme_Object *argv[])
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *port, *v, **m;

  scheme_check_proc_arity("call-with-input-file", 1, 1, argc, argv);

  port = do_open_input_file("call-with-input-file", 1, argc, argv);
  
  v = _scheme_apply_multi(argv[1], 1, &port);
  
  m = p->ku.multiple.array;
  scheme_close_input_port(port);
  p->ku.multiple.array = m;

  return v;
}

static Scheme_Object *with_call_thunk(void *d)
{
  return _scheme_apply_multi(SCHEME_CAR((Scheme_Object *)d), 0, NULL);
}

static void with_set_output_port(void *d)
{
  Scheme_Config *config = scheme_config;

  SCHEME_CDR(SCHEME_CDR((Scheme_Object *)d)) = scheme_get_param(config, MZCONFIG_OUTPUT_PORT);
  scheme_set_param(config, MZCONFIG_OUTPUT_PORT, SCHEME_CAR(SCHEME_CDR((Scheme_Object *)d)));
}

static void with_unset_output_port(void *d)
{
  Scheme_Config *config = scheme_config;

  scheme_set_param(config, MZCONFIG_OUTPUT_PORT, SCHEME_CDR(SCHEME_CDR((Scheme_Object *)d)));
}

static Scheme_Object *
with_output_to_file (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *v;

  scheme_check_proc_arity("with-output-to-file", 0, 1, argc, argv);

  port = do_open_output_file("with-output-to-file", 1, argc, argv);
  
  v = scheme_dynamic_wind(with_set_output_port,
			  with_call_thunk,
			  with_unset_output_port,
			  NULL,
			  scheme_make_pair(argv[1], 
					   scheme_make_pair(port,
							    scheme_void)));

  scheme_close_output_port(port);

  return v;
}

static void with_set_input_port(void *d)
{
  Scheme_Config *config = scheme_config;

  SCHEME_CDR(SCHEME_CDR((Scheme_Object *)d)) = scheme_get_param(config, MZCONFIG_INPUT_PORT);
  scheme_set_param(config, MZCONFIG_INPUT_PORT, SCHEME_CAR(SCHEME_CDR((Scheme_Object *)d)));
}

static void with_unset_input_port(void *d)
{
  Scheme_Config *config = scheme_config;

  scheme_set_param(config, MZCONFIG_INPUT_PORT, SCHEME_CDR(SCHEME_CDR((Scheme_Object *)d)));
}

static Scheme_Object *
with_input_from_file(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *v;

  scheme_check_proc_arity("with-input-from-file", 0, 1, argc, argv);

  port = do_open_input_file("with-input-from-file", 1, argc, argv);
  
  v = scheme_dynamic_wind(with_set_input_port,
			  with_call_thunk,
			  with_unset_input_port,
			  NULL,
			  scheme_make_pair(argv[1], 
					   scheme_make_pair(port,
							    scheme_void)));

  scheme_close_input_port(port);

  return v;
}

static Scheme_Object *sch_default_read_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Process *p = scheme_current_process;

  if (!SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("default-port-read-handler", "input-port", 0, argc, argv);

  if ((Scheme_Object *)argv[0] == scheme_orig_stdin_port)
    flush_orig_outputs();

  return scheme_internal_read(argv[0],
			      SCHEME_TRUEP(scheme_get_param(p->config, MZCONFIG_CAN_READ_COMPILED)),
			      p->config
#ifdef MZ_REAL_THREADS
			      , p
#endif
			      );
}

static Scheme_Object *read_f(int argc, Scheme_Object *argv[])
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *port;

  if (argc && !SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("read", "input-port", 0, argc, argv);

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(p->config);
  
  if (((Scheme_Input_Port *)port)->read_handler) {
    Scheme_Object *o[1];
    o[0] = port;
    return _scheme_apply(((Scheme_Input_Port *)port)->read_handler, 1, o);
  } else {
    if (port == scheme_orig_stdin_port)
      flush_orig_outputs();

    return scheme_internal_read(port,
				SCHEME_TRUEP(scheme_get_param(p->config, MZCONFIG_CAN_READ_COMPILED)),
				p->config
#ifdef MZ_REAL_THREADS
				, p
#endif
				);
  }
}

static Scheme_Object *
do_read_char(char *name, int argc, Scheme_Object *argv[], int peek)
{
  Scheme_Object *port;
  int ch;

  if (argc && !SCHEME_INPORTP(argv[0]))
    scheme_wrong_type(name, "input-port", 0, argc, argv);

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(scheme_config);

  if (peek)
    ch = scheme_peekc(port);
  else
    ch = scheme_getc(port);

  if (ch == EOF)
    return (scheme_eof);
  else
    return _scheme_make_char(ch);
}

static Scheme_Object *
read_char (int argc, Scheme_Object *argv[])
{
  return do_read_char("read-char", argc, argv, 0);
}

static Scheme_Object *
peek_char (int argc, Scheme_Object *argv[])
{
  return do_read_char("peek-char", argc, argv, 1);
}

static Scheme_Object *
read_line (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  int ch;
  int crlf = 0, cr = 0, lf = 1;
  char *buf, *oldbuf, onstack[32];
  long size = 31, oldsize, i = 0;

  if (argc && !SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("read-line", "input-port", 0, argc, argv);
  if (argc > 1) {
    Scheme_Object *v = argv[1];
    if (SAME_OBJ(v, any_symbol)) {
      crlf = cr = lf = 1;
    } else if (SAME_OBJ(v, any_one_symbol)) {
      crlf = 0;
      cr = lf = 1;
    } else if (SAME_OBJ(v, cr_symbol)) {
      crlf = lf = 0;
      cr = 1;
    } else if (SAME_OBJ(v, lf_symbol)) {
      crlf = cr = 0;
      lf = 1;
    } else if (SAME_OBJ(v, crlf_symbol)) {
      lf = cr = 0;
      crlf = 1;
    } else
      scheme_wrong_type("read-line", "newline specification symbol", 1, argc, argv);
  }

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(scheme_config);

  if ((Scheme_Object *)port == scheme_orig_stdin_port)
    flush_orig_outputs();

  buf = onstack;

  while (1) {
    ch = scheme_getc(port);
    if (ch == EOF) {
      if (!i)
	return scheme_eof;
      break;
    }
    if (ch == '\r') {
      if (crlf) {
	int ch2;
	ch2 = scheme_getc(port);
	if (ch2 == '\n')
	  break;
	else {
	  scheme_ungetc(ch2, port);
	  if (cr)
	    break;
	}
      } else if (cr)
	break;
    } else if (ch == '\n') {
      if (lf) break;
    }
    
    if (i >= size) {
      oldsize = size;
      oldbuf = buf;

      size *= 2;
      buf = (char *)scheme_malloc_atomic(size + 1);
      memcpy(buf, oldbuf, oldsize);
    }
    buf[i++] = ch;
  }
  buf[i] = '\0';

  return scheme_make_sized_string(buf, i, buf == onstack);
}

static Scheme_Object *
read_string(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *str;
  long size, got;

  if (!SCHEME_INTP(argv[0])) {
    if (SCHEME_BIGNUMP(argv[0])) {
      scheme_raise_out_of_memory("read-string", "making string of length %s",
				 scheme_make_provided_string(argv[0], 0, NULL));
      return NULL;
    } else
      size = -1; /* cause the error message to be printed */
  } else
    size = SCHEME_INT_VAL(argv[0]);

  if (size < 0) {
    scheme_wrong_type("read-string", "non-negative exact integer", 0, argc, argv);
    return NULL;
  }

  if ((argc > 1) && !SCHEME_INPORTP(argv[1]))
    scheme_wrong_type("read-string", "input-port", 1, argc, argv);

  if (argc > 1)
    port = argv[1];
  else
    port = CURRENT_INPUT_PORT(scheme_config);

  if ((Scheme_Object *)port == scheme_orig_stdin_port)
    flush_orig_outputs();

  if (!size)
    return scheme_make_sized_string("", 0, 0);

  str = scheme_alloc_string(size, 0);

  got = scheme_get_chars(port, size, SCHEME_STR_VAL(str));

  if (!got)
    return scheme_eof;

  if (got < size) {
    /* Reallocate in case got << size */
    str = scheme_make_sized_string(SCHEME_STR_VAL(str), got, 1);
  }

  return str;
}

static Scheme_Object *
read_string_bang(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *str;
  long size, start, finish, got;

  if (!SCHEME_STRINGP(argv[0])) {
    scheme_wrong_type("read-string!", "string", 0, argc, argv);
    return NULL;
  } else
    str = argv[0];
  if ((argc > 1) && !SCHEME_INPORTP(argv[1]))
    scheme_wrong_type("read-string!", "input-port", 1, argc, argv);
  
  scheme_get_substring_indices("read-string!", str, 
			       argc, argv,
			       2, 3, &start, &finish);

  size = finish - start;

  if (argc > 1)
    port = argv[1];
  else
    port = CURRENT_INPUT_PORT(scheme_config);

  if ((Scheme_Object *)port == scheme_orig_stdin_port)
    flush_orig_outputs();

  if (!size)
    return scheme_make_integer(0);

  got = scheme_get_chars(port, size, SCHEME_STR_VAL(str) + start);

  if (!got)
    return scheme_eof;

  return scheme_make_integer(got);
}

static Scheme_Object *
eof_object_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_EOFP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
char_ready_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;

  if (argc && !SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("char-ready?", "input-port", 0, argc, argv);

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(scheme_config);
  
  return (scheme_char_ready(port) ? scheme_true : scheme_false);
}

static Scheme_Object *sch_default_display_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[1]))
    scheme_wrong_type("default-port-display-handler", "output-port", 1, argc, argv);

  scheme_internal_display(argv[0], argv[1], scheme_config);

  return scheme_void;
}

static Scheme_Object *sch_default_write_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[1]))
    scheme_wrong_type("default-port-write-handler", "output-port", 1, argc, argv);

  scheme_internal_write(argv[0], argv[1], scheme_config);

  return scheme_void;
}

static Scheme_Object *sch_default_print_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[1]))
    scheme_wrong_type("default-port-print-handler", "output-port", 1, argc, argv);

  return _scheme_apply(scheme_get_param(scheme_config,
					MZCONFIG_PORT_PRINT_HANDLER),
		       argc, argv);
}

static Scheme_Object *sch_default_global_port_print_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[1]))
    scheme_wrong_type("default-global-port-print-handler", "output-port", 1, argc, argv);

  scheme_internal_write(argv[0], argv[1], scheme_config);

  return scheme_void;
}

static Scheme_Object *
display_write(char *name, 
	      int argc, Scheme_Object *argv[], int escape)
{
  Scheme_Object *port;
  Scheme_Config *config = scheme_config;

  if (argc > 1) {
    if (!SCHEME_OUTPORTP(argv[1]))
      scheme_wrong_type(name, "output-port", 1, argc, argv);
    port = argv[1];
  } else
    port = CURRENT_OUTPUT_PORT(config);
  
  if (escape > 0) {
    if (!((Scheme_Output_Port *)port)->display_handler) {
      Scheme_Object *v = argv[0];
#ifndef MZ_REAL_THREADS
      if (SCHEME_STRINGP(v)) {
	Scheme_Output_Port *op = (Scheme_Output_Port *)port;
	int len = SCHEME_STRTAG_VAL(v);
	check_closed(name, "output", port, op->closed);
	{
	  Write_String_Fun f = op->write_string_fun;
	  f(SCHEME_STR_VAL(v), len, op);
	}
	op->pos += len;
      } else
#endif
	scheme_internal_display(v, port, config);
    } else {
      Scheme_Object *a[2];
      a[0] = argv[0];
      a[1] = port;
      _scheme_apply_multi(((Scheme_Output_Port *)port)->display_handler, 2, a);
    }
  } else if (!escape) {
    Scheme_Object *h;

    h = ((Scheme_Output_Port *)port)->write_handler;
    
    if (!h)
      scheme_internal_write(argv[0], port, config);
    else {
      Scheme_Object *a[2];
      a[0] = argv[0];
      a[1] = port;
      _scheme_apply_multi(h, 2, a);      
    }
  } else {
    Scheme_Object *h;
    Scheme_Object *a[2];
    
    a[0] = argv[0];
    a[1] = port;
    
    h = ((Scheme_Output_Port *)port)->print_handler;
						
    if (!h)
      sch_default_print_handler(2, a);
    else
      _scheme_apply_multi(h, 2, a);      
  }

  return scheme_void;
}

static Scheme_Object *
sch_write (int argc, Scheme_Object *argv[])
{
  return display_write("write", argc, argv, 0);
}

static Scheme_Object *
display (int argc, Scheme_Object *argv[])
{
  return display_write("display", argc, argv, 1);
}

static Scheme_Object *
print (int argc, Scheme_Object *argv[])
{
  return display_write("print", argc, argv, -1);
}

static Scheme_Object *
newline (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;

  if (argc && !SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("newline", "output-port", 0, argc, argv);

  if (argc)
    port = argv[0];
  else
    port = CURRENT_OUTPUT_PORT(scheme_config);
  
  scheme_write_string("\n", 1, port);

  return scheme_void;
}

static Scheme_Object *
write_char (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  Scheme_Output_Port *op;
  unsigned char buffer[1];

  if (argc && !SCHEME_CHARP(argv[0]))
    scheme_wrong_type("write-char", "character", 0, argc, argv);
  if (argc > 1) {
    if (!SCHEME_OUTPORTP(argv[1]))
      scheme_wrong_type("write-char", "output-port", 1, argc, argv);
    port = argv[1];
  } else
    port = CURRENT_OUTPUT_PORT(scheme_config);

  buffer[0] = SCHEME_CHAR_VAL(argv[0]);

  op = (Scheme_Output_Port *)port;
#ifdef MZ_REAL_THREADS
  scheme_write_string((char *)buffer, 1, port);
#else
  check_closed("write-char", "output", port, op->closed);
  {
    Write_String_Fun f = op->write_string_fun;
    f((char *)buffer, 1, op);
  }
  op->pos++;
#endif

  return scheme_void;
}

static Scheme_Object *port_read_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Input_Port *ip;

  if (!SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("port-read-handler", "input-port", 0, argc, argv);

  ip = (Scheme_Input_Port *)argv[0];
  if (argc == 1) {
    if (ip->read_handler)
      return ip->read_handler;
    else
      return default_read_handler;
  } else {
    scheme_check_proc_arity("port-read-handler", 1, 1, argc, argv);
    if (argv[1] == default_read_handler)
      ip->read_handler = NULL;
    else
      ip->read_handler = argv[1];

    return scheme_void;
  }
}

static Scheme_Object *port_display_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;

  if (!SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("port-display-handler", "output-port", 0, argc, argv);

  op = (Scheme_Output_Port *)argv[0];
  if (argc == 1) {
    if (op->display_handler)
      return op->display_handler;
    else
      return default_display_handler;
  } else {
    scheme_check_proc_arity("port-display-handler", 2, 1, argc, argv);
    if (argv[1] == default_display_handler)
      op->display_handler = NULL;
    else
      op->display_handler = argv[1];

    return scheme_void;
  }
}

static Scheme_Object *port_write_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;

  if (!SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("port-write-handler", "output-port", 0, argc, argv);

  op = (Scheme_Output_Port *)argv[0];
  if (argc == 1) {
    if (op->write_handler)
      return op->write_handler;
    else
      return default_write_handler;
  } else {
    scheme_check_proc_arity("port-write-handler", 2, 1, argc, argv);
    if (argv[1] == default_write_handler)
      op->write_handler = NULL;
    else
      op->write_handler = argv[1];

    return scheme_void;
  }
}

static Scheme_Object *port_print_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;

  if (!SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("port-print-handler", "output-port", 0, argc, argv);

  op = (Scheme_Output_Port *)argv[0];
  if (argc == 1) {
    if (op->print_handler)
      return op->print_handler;
    else
      return default_print_handler;
  } else {
    scheme_check_proc_arity("port-print-handler", 2, 1, argc, argv);
    if (argv[1] == default_print_handler)
      op->print_handler = NULL;
    else
      op->print_handler = argv[1];

    return scheme_void;
  }
}

static Scheme_Object *global_port_print_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("global-port-print-handler",
			     scheme_make_integer(MZCONFIG_PORT_PRINT_HANDLER),
			     argc, argv,
			     2, NULL, NULL, 0);
}

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Config *config;
  Scheme_Object *port;
  Scheme_Process *p;
} LoadHandlerData;

static void post_load_handler(void *data)
{
  LoadHandlerData *lhd = (LoadHandlerData *)data;

  scheme_close_input_port((Scheme_Object *)lhd->port);
}

static Scheme_Object *do_load_handler(void *data)
{  
  LoadHandlerData *lhd = (LoadHandlerData *)data;
  Scheme_Object *port = lhd->port;
  Scheme_Process *p = lhd->p;
  Scheme_Config *config = lhd->config;
  Scheme_Object *last_val = scheme_void, *obj, **save_array = NULL;
  int save_count = 0;

  while ((obj = scheme_internal_read(port,
				     1,
				     config
#ifdef MZ_REAL_THREADS
				     , p
#endif
				     )) 
	 && !SCHEME_EOFP(obj)) {
    save_array = NULL;

    last_val = _scheme_apply_multi(scheme_get_param(config, MZCONFIG_EVAL_HANDLER),
				   1, &obj);

    /* If multi, we must save then: */
    if (last_val == SCHEME_MULTIPLE_VALUES) {
      save_array = p->ku.multiple.array;
      save_count = p->ku.multiple.count;
    }
  }

  if (save_array) {
    p->ku.multiple.array = save_array;
    p->ku.multiple.count = save_count;
  }

  return last_val;
}

static Scheme_Object *default_load(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  char *filename;
  int ch;
  FILE *fp;
  Scheme_Process *p = scheme_current_process;
  Scheme_Config *config = p->config;
  LoadHandlerData *lhd;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("default-load-handler", "string", 0, argc, argv);

  filename = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
				    SCHEME_STRTAG_VAL(argv[0]),
				    "default-load-handler", 
				    NULL);

  if (scheme_directory_exists(filename)) {
    filename_exn("default-load-handler", "cannot open directory as a file", filename, errno);
    return scheme_void;
  }

  fp = fopen(filename, "rb");
  if (!fp) {
    filename_exn("default-load-handler", "cannot open file for input", filename, errno);
    return NULL;
  }
  scheme_file_open_count++;

  port = _scheme_make_named_file_input_port(fp, filename, 
					    scheme_is_regular_file(filename));

  /* Skip over #! at beginning of file */
  if ((ch = scheme_getc(port)) == '#') {
    if ((ch = scheme_getc(port)) == '!') {
      int oldch;
    eol_loop:
      oldch = 0;
      while (1) {
	ch = scheme_getc(port);
	if (ch == '\n' || ch == '\r')
	  break;
	oldch = ch;
      }
      if (oldch == '\\')
	goto eol_loop;
    } else {
      scheme_ungetc(ch, port);
      scheme_ungetc('#', port);
    }
  } else
    scheme_ungetc(ch, port);

  lhd = MALLOC_ONE_RT(LoadHandlerData);
#ifdef MZTAG_REQUIRED
  lhd->type = scheme_rt_load_handler_data;
#endif
  lhd->p = p;
  lhd->config = config;
  lhd->port = port;

  return scheme_dynamic_wind(NULL, do_load_handler, post_load_handler,
			     NULL, (void *)lhd);
}

typedef struct {
  MZTAG_IF_REQUIRED
  int param;
  Scheme_Object *filename;
  Scheme_Config *config;
  Scheme_Object *load_dir, *old_load_dir;
} LoadData;

static void pre_load(void *data)
{
  LoadData *ld = (LoadData *)data;

  scheme_set_param(ld->config, MZCONFIG_LOAD_DIRECTORY, ld->load_dir);  
}

static void post_load(void *data)
{
  LoadData *ld = (LoadData *)data;

  scheme_set_param(ld->config, MZCONFIG_LOAD_DIRECTORY, ld->old_load_dir);
}

static Scheme_Object *do_load(void *data)
{  
  LoadData *ld = (LoadData *)data;
  Scheme_Object *argv[1];

  argv[0] = ld->filename;
  return _scheme_apply_multi(scheme_get_param(ld->config, ld->param), 1, argv);
}

Scheme_Object *scheme_load_with_clrd(int argc, Scheme_Object *argv[],
				     char *who, int handler_param)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Config *config = p->config;
  LoadData *ld;
  const char *filename;
  Scheme_Object *load_dir;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type(who, "string", 0, argc, argv);

  filename = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
				    SCHEME_STRTAG_VAL(argv[0]),
				    who,
				    NULL);

  /* Calculate load directory */
  load_dir = scheme_get_file_directory(filename);

  ld = MALLOC_ONE_RT(LoadData);
#ifdef MZTAG_REQUIRED
  ld->type = scheme_rt_load_data;
#endif
  ld->param = handler_param;
  {
    Scheme_Object *ss;
    ss = scheme_make_sized_string((char *)filename, -1, 0);
    ld->filename = ss;
  }
  ld->config = config;
  ld->load_dir = load_dir;
  ld->old_load_dir = scheme_get_param(config, MZCONFIG_LOAD_DIRECTORY);

  return scheme_dynamic_wind(pre_load, do_load, post_load,
			     NULL, (void *)ld);
}

static Scheme_Object *load(int argc, Scheme_Object *argv[])
{
  return scheme_load_with_clrd(argc, argv, "load", MZCONFIG_LOAD_HANDLER);
}

static Scheme_Object *
current_load(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-load",
			     scheme_make_integer(MZCONFIG_LOAD_HANDLER),
			     argc, argv,
			     1, NULL, NULL, 0);
}

static Scheme_Object *abs_directory_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *d = argv[0];

  if (!SCHEME_FALSEP(d)) {
    char *expanded;
    Scheme_Object *ed;
    char *s;
    int len;

    if (!SCHEME_STRINGP(d))
      return NULL;

    s = SCHEME_STR_VAL(d);
    len = SCHEME_STRTAG_VAL(d);

    if (!scheme_is_complete_path(s, len))
      scheme_raise_exn(MZEXN_I_O_FILESYSTEM,
		       d,
		       "current-load-relative-directory: not a complete path: \"%.255s\"",
		       s);

    expanded = scheme_expand_filename(s, len, "current-load-relative-directory", NULL);
    ed = scheme_make_sized_string(expanded, strlen(expanded), 1);
    if (!scheme_directory_exists(expanded)) {
      scheme_raise_exn(MZEXN_I_O_FILESYSTEM,
		       ed,
		       "current-load-relative-directory: directory not found or not a directory: \"%.255s\"",
		       expanded);
    }

    return ed;
  }

  return scheme_false;
}

static Scheme_Object *
current_load_directory(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-load-relative-directory", 
			     scheme_make_integer(MZCONFIG_LOAD_DIRECTORY),
			     argc, argv,
			     -1, abs_directory_p, "string or #f", 1);
}

Scheme_Object *scheme_load(const char *file)
{
  Scheme_Object *p[1];
  mz_jmp_buf savebuf;
  Scheme_Object *val;

  p[0] = scheme_make_string(file);
  memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf));
  if (scheme_setjmp(scheme_error_buf)) {
    val = NULL;
  } else {
    val = scheme_apply_multi(scheme_make_prim((Scheme_Prim *)load),
			     1, p);
  }
  memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf));

  return val;
}

static Scheme_Object *compiled_kind_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *o = argv[0];
  
  if (SAME_OBJ(o, all_symbol))
    return o;
  if (SAME_OBJ(o, non_elaboration_symbol))
    return o;
  if (SAME_OBJ(o, none_symbol))
    return o;

  return NULL;
}

static Scheme_Object *use_compiled_kind(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("use-compiled-file-kinds",
			     scheme_make_integer(MZCONFIG_USE_COMPILED_KIND),
			     argc, argv,
			     -1, compiled_kind_p, "compiled file kind symbol", 1);
}

static Scheme_Object *collection_path_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *o;

  if (SCHEME_FALSEP(argv[0]))
    return scheme_false;

  if (scheme_proper_list_length(argv[0]) < 1)
    return NULL;

  o = argv[0];
  while (!SCHEME_NULLP(o)) {
    Scheme_Object *p = SCHEME_CAR(o);
    if (!SCHEME_STRINGP(p) || 
	!scheme_is_relative_path(SCHEME_STR_VAL(p),
				 SCHEME_STRTAG_VAL(p)))
      return NULL;
    o = SCHEME_CDR(o);
  }

  return argv[0];
}

static Scheme_Object *current_require_relative_collection(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-require-relative-collection",
			     scheme_make_integer(MZCONFIG_REQUIRE_COLLECTION),
			     argc, argv,
			     -1, collection_path_p, "non-empty list of relative path strings or #f", 1);
}

static Scheme_Object *
transcript_on(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("transcript-on", "string", 0, argc, argv);

  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "transcript-on: not supported");

  return scheme_void;
}

static Scheme_Object *
transcript_off(int argc, Scheme_Object *argv[])
{
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "transcript-off: not supported");

  return scheme_void;
}

static Scheme_Object *
_flush_output(int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;

  if (argc && !SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("flush-output", "output-port", 0, argc, argv);

  if (argc) 
    op = (Scheme_Output_Port *)argv[0];
  else
    op = (Scheme_Output_Port *)CURRENT_OUTPUT_PORT(scheme_config);

  if (SAME_OBJ(op->sub_type, file_output_port_type))
    fflush (((Scheme_Output_File *)op->port_data)->f);
#ifdef USE_FD_PORTS
  if (SAME_OBJ(op->sub_type, fd_output_port_type))
    flush_fd(op, NULL, 0);
#endif

  return (scheme_void);
}

static Scheme_Object *
flush_output (int argc, Scheme_Object *argv[])
{
  return _flush_output(argc, argv);
}

static Scheme_Object *
file_position(int argc, Scheme_Object *argv[])
{
  FILE *f;
  Scheme_Indexed_String *is;
  int wis;

  if (!SCHEME_OUTPORTP(argv[0]) && !SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("file-position", "port", 0, argc, argv);
  if (argc == 2) {
    int ok = 0;

    if (SCHEME_INTP(argv[1])) {
      ok = (SCHEME_INT_VAL(argv[1]) >= 0);
    }

    if (SCHEME_BIGNUMP(argv[1])) {
      ok = SCHEME_BIGPOS(argv[1]);
    }

    if (!ok)
      scheme_wrong_type("file-position", "non-negative exact integer", 1, argc, argv);
  }

  f = NULL;
  is = NULL;
  wis = 0;

  if (SCHEME_OUTPORTP(argv[0])) {
    Scheme_Output_Port *op;

    op = (Scheme_Output_Port *)argv[0];
    if (SAME_OBJ(op->sub_type, file_output_port_type))
      f = ((Scheme_Output_File *)op->port_data)->f;
    else if (SAME_OBJ(op->sub_type, string_output_port_type)) {
      is = (Scheme_Indexed_String *)op->port_data;
      wis = 1;
    } else if (argc < 2)
      return scheme_make_integer(scheme_output_tell(argv[0]));
  } else if (SCHEME_INPORTP(argv[0])) {
    Scheme_Input_Port *ip;
  
    ip = (Scheme_Input_Port *)argv[0];
    if (SAME_OBJ(ip->sub_type, file_input_port_type))
      f = ((Scheme_Input_File *)ip->port_data)->f;
    else if (SAME_OBJ(ip->sub_type, string_input_port_type))
      is = (Scheme_Indexed_String *)ip->port_data;
    else if (argc < 2)
      return scheme_make_integer(scheme_tell(argv[0]));
  }

  if (!f && !is)
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		     argv[0],
		     "file-position: setting position allowed for file and string ports only;"
		     " given %s and position %s",
		     scheme_make_provided_string(argv[0], 2, NULL),
		     scheme_make_provided_string(argv[1], 2, NULL));

  if ((argc > 1) && SCHEME_BIGNUMP(argv[1]))
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		     argv[1],
		     "file-position: new position is too large: %s for port: %s",
		     scheme_make_provided_string(argv[1], 2, NULL),
		     scheme_make_provided_string(argv[0], 2, NULL));

  if (argc > 1) {
    long n = SCHEME_INT_VAL(argv[1]);
    if (f)
      fseek(f, n, 0);
    else {
      if (wis) {
	if (is->index > is->u.hot)
	  is->u.hot = is->index;
	if (is->size < is->index + n) {
	  /* Expand string up to n: */
	  char *old;
	  
	  old = is->string;
	  is->size = is->index + n;
	  {
	    char *ca;
	    ca = (char *)scheme_malloc_atomic(is->size + 1);
	    is->string = ca;
	  }
	  memcpy(is->string, old, is->index);
	}
	if (n > is->u.hot)
	  memset(is->string + is->u.hot, 0, n - is->u.hot);
      } else {
	/* Can't really move past end of read string, but pretend we do: */
	if (n > is->size) {
	  is->u.pos = n;
	  n = is->size;
	} else
	  is->u.pos = 0;
      }
      is->index = n;
    }
    return scheme_void;
  } else {
    long p;
    if (f)
      p = ftell(f);
    else if (wis)
      p = is->index;
    else {
      /* u.pos > index implies we previously moved past the end with file-position */
      if (is->u.pos > is->index)
	p = is->u.pos;
      else
	p = is->index;
    }
    return scheme_make_integer(p);
  }
}

void scheme_flush_output(Scheme_Object *port)
{
  (void)_flush_output(1, &port);
}

typedef struct {
  MZTAG_IF_REQUIRED
  unsigned char *buf;
  long buflen;
  int bufstart, bufend;
  int eof;
#ifdef MZ_REAL_THREADS
  int num_waiting;
  void *change_mutex;
  Scheme_Object *wait_sem;
#endif
} Scheme_Pipe;

static int pipe_getc(Scheme_Input_Port *p)
{
  Scheme_Pipe *pipe;
  int c;

  pipe = (Scheme_Pipe *)(p->port_data);

#ifdef MZ_REAL_THREADS
  SCHEME_LOCK_MUTEX(pipe->change_mutex);
  c = (pipe->bufstart == pipe->bufend && !pipe->eof) ? 0 : 1;
  if (!c) {
    pipe->num_waiting++;
    SCHEME_UNLOCK_MUTEX(pipe->change_mutex);
    SCHEME_SEMA_DOWN(pipe->wait_sem);
    SCHEME_LOCK_MUTEX(pipe->change_mutex);
    BEGIN_ESCAPEABLE(SCHEME_UNLOCK_MUTEX(pipe->change_mutex));
    scheme_process_block(0);
    END_ESCAPEABLE();
  }
#else
  scheme_current_process->block_descriptor = PIPE_BLOCKED;
  scheme_current_process->blocker = (Scheme_Object *)p;
  while (pipe->bufstart == pipe->bufend && !pipe->eof) {
    scheme_process_block((float)0.0);
  }
  scheme_current_process->block_descriptor = NOT_BLOCKED;
  scheme_current_process->ran_some = 1;
#endif

  if (p->closed) {
    /* Another thread closed the input port while we were waiting. */
    /* Call scheme_getc to signal the error */
    scheme_getc((Scheme_Object *)p);
  }
  
  if (pipe->bufstart == pipe->bufend)
    c = EOF;
  else {
    c = pipe->buf[pipe->bufstart];
    
    pipe->bufstart = (pipe->bufstart + 1) % pipe->buflen;
  }

#ifdef MZ_REAL_THREADS
  SCHEME_UNLOCK_MUTEX(pipe->change_mutex);
#endif

  return c;
}

static void pipe_write(char *str, long len, Scheme_Output_Port *p)
{
  Scheme_Pipe *pipe;
  long avail, firstpos, firstn, secondn, endpos;

  pipe = (Scheme_Pipe *)(p->port_data);
  
  if (pipe->eof)
    return;

#ifdef MZ_REAL_THREADS
  SCHEME_LOCK_MUTEX(pipe->change_mutex);
#endif

  if (pipe->bufstart <= pipe->bufend) {
    firstn = pipe->buflen - pipe->bufend;
    avail = firstn + pipe->bufstart - 1;
    if (!pipe->bufstart)
      --firstn;
  } else {
    firstn = avail = pipe->bufstart - pipe->bufend - 1;
  }
  firstpos = pipe->bufend;

  if (avail < len) {
    unsigned char *old;
    int newlen;

    old = pipe->buf;
    newlen = 2 * (pipe->buflen + len);

    {
      unsigned char *uca;
      uca = (unsigned char *)scheme_malloc_atomic(newlen);
      pipe->buf = uca;
    }

    if (pipe->bufstart <= pipe->bufend) {
      memcpy(pipe->buf, old + pipe->bufstart, pipe->bufend - pipe->bufstart);
      pipe->bufend -= pipe->bufstart;
      pipe->bufstart = 0;
    } else {
      int slen;
      slen = pipe->buflen - pipe->bufstart;
      memcpy(pipe->buf, old + pipe->bufstart, slen);
      memcpy(pipe->buf + slen, old, pipe->bufend);
      pipe->bufstart = 0;
      pipe->bufend += slen;
    }

    pipe->buflen = newlen;

    firstpos = pipe->bufend;
    firstn = len;
    endpos = firstpos + firstn;

    secondn = 0;
  } else {
    if (firstn >= len) {
      firstn = len;
      endpos = (firstpos + len) % pipe->buflen;
      secondn = 0;
    } else {
      secondn = len - firstn;
      endpos = secondn;
    }
  }

  if (firstn)
    memcpy(pipe->buf + firstpos, str, firstn);
  if (secondn)
    memcpy(pipe->buf, str + firstn, secondn);

  pipe->bufend = endpos;

#ifdef MZ_REAL_THREADS
  if (pipe->num_waiting) {
    --pipe->num_waiting;
    SCHEME_SEMA_UP(pipe->wait_sem);
  }
  SCHEME_UNLOCK_MUTEX(pipe->change_mutex);
#endif
}

static int pipe_char_ready(Scheme_Input_Port *p)
{
  Scheme_Pipe *pipe;
  int v;

  pipe = (Scheme_Pipe *)(p->port_data);
  
#ifdef MZ_REAL_THREADS
  SCHEME_LOCK_MUTEX(pipe->change_mutex);
#endif

  v = (pipe->bufstart != pipe->bufend || pipe->eof);

#ifdef MZ_REAL_THREADS
  SCHEME_UNLOCK_MUTEX(pipe->change_mutex);
#endif

  return v;
}

static void pipe_in_close(Scheme_Input_Port *p)
{
  Scheme_Pipe *pipe;

  pipe = (Scheme_Pipe *)(p->port_data);
  
#ifdef MZ_REAL_THREADS
  SCHEME_LOCK_MUTEX(pipe->change_mutex);
#endif

  pipe->eof = 1;

#ifdef MZ_REAL_THREADS
  while (pipe->num_waiting) {
    --pipe->num_waiting;
    SCHEME_SEMA_UP(pipe->wait_sem);
  }

  SCHEME_UNLOCK_MUTEX(pipe->change_mutex);
#endif
}

static void pipe_out_close(Scheme_Output_Port *p)
{
  Scheme_Pipe *pipe;

  pipe = (Scheme_Pipe *)(p->port_data);
  
#ifdef MZ_REAL_THREADS
  SCHEME_LOCK_MUTEX(pipe->change_mutex);
#endif

  pipe->eof = 1;

#ifdef MZ_REAL_THREADS
  while (pipe->num_waiting) {
    --pipe->num_waiting;
    SCHEME_SEMA_UP(pipe->wait_sem);
  }

  SCHEME_UNLOCK_MUTEX(pipe->change_mutex);
#endif
}

#ifdef MZ_REAL_THREADS
static void free_semas(void *p, void *ignored)
{
  Scheme_Pipe *pipe;

  pipe = (Scheme_Pipe *)p;

  SCHEME_FREE_SEMA(pipe->wait_sem);
  SCHEME_FREE_MUTEX(pipe->change_mutex);
}
#endif

void scheme_pipe(Scheme_Object **read, Scheme_Object **write)
{
  Scheme_Pipe *pipe;
  Scheme_Input_Port *readp;
  Scheme_Output_Port *writep;

  pipe = MALLOC_ONE_RT(Scheme_Pipe);
#ifdef MZTAG_REQUIRED
  pipe->type = scheme_rt_pipe;
#endif
  pipe->buflen = 100;
  {
    unsigned char *uca;
    uca = (unsigned char *)scheme_malloc_atomic(pipe->buflen);
    pipe->buf = uca;
  }
  pipe->bufstart = pipe->bufend = 0;
  pipe->eof = 0;
#ifdef MZ_REAL_THREADS
  pipe->num_waiting = 0;
  pipe->change_mutex = SCHEME_MAKE_MUTEX();
  pipe->wait_sem = SCHEME_MAKE_SEMA(0);

  scheme_add_finalizer(pipe, free_semas, NULL);
#endif

  readp = _scheme_make_input_port(pipe_read_port_type,
				  (void *)pipe,
				  pipe_getc,
				  NULL,
				  pipe_char_ready,
				  pipe_in_close,
				  NULL,
				  0);

  readp->name = "PIPE";

  writep = scheme_make_output_port(pipe_write_port_type,
				   (void *)pipe,
				   pipe_write,
				   pipe_out_close,
				   0);

  *read = (Scheme_Object *)readp;
  *write = (Scheme_Object *)writep;
}

static Scheme_Object *sch_pipe(int argc, Scheme_Object **args)
{
  Scheme_Object *v[2];

  scheme_pipe(&v[0], &v[1]);

  return scheme_values(2, v);
}

#ifdef PROCESS_FUNCTION

# define USE_CREATE_PIPE

#ifdef WINDOWS_PROCESSES
# ifdef USE_CREATE_PIPE
#  define _EXTRA_PIPE_ARGS
static int MyPipe(int *ph) {
  HANDLE r, w;
  if (CreatePipe(&r, &w, NULL, 0)) {
    ph[0] = _open_osfhandle((long)r, 0);
    ph[1] = _open_osfhandle((long)w, 0);
    return 0;
  } else
    return 1;
}
#  define PIPE_FUNC MyPipe
# else
#  include <Process.h>
#  include <fcntl.h>
#  define PIPE_FUNC MSC_IZE(pipe)
#  define _EXTRA_PIPE_ARGS , 256, _O_BINARY
# endif
#else
# define _EXTRA_PIPE_ARGS
# define PIPE_FUNC MSC_IZE(pipe)
#endif

#endif

#if defined(UNIX_PROCESSES)

# define WAITANY(s) waitpid((pid_t)-1, s, WNOHANG)

static void child_done(int ingored)
{
  pid_t result;
  int status;
  System_Child *sc, *prev;

  do {
    do {
      result = WAITANY(&status);
    } while ((result == -1) && (errno == EINTR));
    
    if (result > 0) {
      if (WIFEXITED(status)) {
	if (WEXITSTATUS(status))
	  status = 0;
	else
	  status = 1;
      } else
	status = 0;

      prev = NULL;
      for (sc = scheme_system_children; sc; prev = sc, sc = sc->next) {
	if (sc->id == result) {
	  sc->done = 1;
	  sc->status = status;
	  
	  if (prev)
	    prev->next = sc->next;
	  else
	    scheme_system_children = sc->next;
	}
      }
    }
  } while (result > 0);

# ifdef SIGSET_NEEDS_REINSTALL
  START_XFORM_SKIP;
  MZ_SIGSET(SIGCHLD, child_done);
  END_XFORM_SKIP;
# endif
}

#endif

#if defined(UNIX_PROCESSES) || defined(WINDOWS_PROCESSES) || defined(BEOS_PROCESSES)

#ifdef BEOS_PROCESSES
typedef struct {
  MZTAG_IF_REQUIRED
  thread_id t;
  status_t result;
  int done;
  sem_id done_sem;
} BeOSProcess;
#endif

static int subp_done(Scheme_Object *sci)
{
#if defined(UNIX_PROCESSES)
  System_Child *sc = (System_Child *)sci;
  return sc->done;
#endif
#ifdef WINDOWS_PROCESSES
  DWORD w;
  if (sci) {
    if (GetExitCodeProcess((HANDLE)sci, &w))
      return w != STILL_ACTIVE;
    else
      return 1;
  } else
    return 1;
#endif
#ifdef BEOS_PROCESSES
  BeOSProcess *p = (BeOSProcess *)sci;
  if (p->done) {
    if (p->done_sem) {
      delete_sem(p->done_sem);
      p->done_sem = 0;
    }
    return 1;
  } else
    return 0;
#endif
}

static void subp_needs_wakeup(Scheme_Object *sci, void *fds)
{
#ifdef WINDOWS_PROCESSES
# ifndef NO_STDIO_THREADS
  scheme_add_fd_handle((void *)(HANDLE)sci, fds, 0);
# endif
#endif
#ifdef BEOS_PROCESSES
  scheme_add_fd_handle((void *)((BeOSProcess *)sci)->done_sem, fds, 1);
#endif
}

static Scheme_Object *get_process_status(void *sci, int argc, Scheme_Object **argv)
{
  if (SAME_OBJ(argv[0], scheme_intern_symbol("status"))) {
#if defined(UNIX_PROCESSES)
    System_Child *sc = (System_Child *)sci;
    
    if (sc->done) {
      if (sc->status)
	return scheme_intern_symbol("done-ok");
      else
	return scheme_intern_symbol("done-error");	
    } else
      return scheme_intern_symbol("running");
#else
# ifdef WINDOWS_PROCESSES
    DWORD w;
    if (!sci)
      return scheme_intern_symbol("done-error");
    
    if (GetExitCodeProcess((HANDLE)sci, &w)) {
      if (w == STILL_ACTIVE)
	return scheme_intern_symbol("running");
      else if (w)
	return scheme_intern_symbol("done-error");
      else
	return scheme_intern_symbol("done-ok");
    }
# endif
# ifdef BEOS_PROCESSES
    BeOSProcess *p = (BeOSProcess *)sci;
    if (!p)
      return scheme_intern_symbol("done-error");
    if (p->done) {
      if (!p->result)
	return scheme_intern_symbol("done-ok");
      else
	return scheme_intern_symbol("done-error");
    } else
      return scheme_intern_symbol("running");
# endif
    return scheme_intern_symbol("unknown");
#endif
  } else if (SAME_OBJ(argv[0], scheme_intern_symbol("wait")))
    scheme_block_until(subp_done, subp_needs_wakeup, sci, (float)0.0);
  else
    scheme_wrong_type("control-process", "'status or 'wait", 0, argc, argv);
  return scheme_void;
}
#endif

#ifdef WINDOWS_PROCESSES
static char *cmdline_protect(char *s)
{
  char *naya;
  int has_space = 0, has_quote = 0, was_slash = 0;
  
  for (naya = s; *naya; naya++) {
    if (isspace(*naya) || (*naya == '\'')) {
      has_space = 1;
      was_slash = 0;
    } else if (*naya == '"') {
      has_quote += 1 + (2 * was_slash);
      was_slash = 0;
    } else if (*naya == '\\') {
      was_slash++;
    } else
      was_slash = 0;
  }
  
  if (has_space || has_quote) {
    char *p;
    int wrote_slash = 0;
    
    naya = scheme_malloc_atomic(strlen(s) + 3 + 3*has_quote);
    naya[0] = '"';
    for (p = naya + 1; *s; s++) {
      if (*s == '"') {
	while (wrote_slash--) {
	  *(p++) = '\\';
	}
	*(p++) = '"'; /* endquote */
	*(p++) = '\\';
	*(p++) = '"'; /* protected */
	*(p++) = '"'; /* start quote again */
	wrote_slash = 0;
      } else if (*s == '\\') {
	*(p++) = '\\';
	wrote_slash++;
      } else {
	*(p++) = *s;
	wrote_slash = 0;
      }
    }
    *(p++) = '"';
    *p = 0;
    
    return naya;
  }

  return s;
}
#endif /* WINDOWS_PROCESSES */

#ifdef BEOS_PROCESSES
extern char **environ;
static status_t wait_process(void *_p)
{
  BeOSProcess *p = (BeOSProcess *)_p;
  status_t r;

  signal(SIGINT, SIG_IGN);
  RELEASE_SEMAPHORE(got_started);

  if (wait_for_thread(p->t, &r) == B_NO_ERROR)
    p->result = r;
  else
    p->result = -1;

  p->done = 1;

  release_sem(p->done_sem);

  return 0;
}

static void delete_done_sem(void *_p)
{
  BeOSProcess *p = (BeOSProcess *)_p;
  if (p->done_sem)
    delete_sem(p->done_sem);
}

static long spawnv(int type, char *command, const char *  const *argv)
{
  BeOSProcess *p = MALLOC_ONE_RT(BeOSProcess);
  sigset_t sigs;
  int i;

#ifdef MZTAG_REQUIRED
  p->type = scheme_rt_beos_process;
#endif

  for (i = 0; argv[i]; i++);

  p->t = load_image(i, (char **)argv, NULL environ);
  
  if (p->t <= 0)
    return -1;

  p->done = 0;
  p->result = -1;
  p->done_sem = create_sem(0, NULL);

  scheme_register_finalizer(p, delete_done_sem, NULL, NULL, NULL);

  if (!got_started)
    got_started = create_sem(0, NULL);
  
  /* Disable SIGINT until the child starts ignoring it */
  sigemptyset(&sigs);
  sigaddset(&sigs, SIGINT);
  sigprocmask(SIG_BLOCK, &sigs, NULL);
  
  resume_thread(spawn_thread(wait_process, "process waiter",
			     B_NORMAL_PRIORITY, p));

  WAIT_SEMAPHORE(got_started);
    
  sigprocmask(SIG_UNBLOCK, &sigs, NULL);

  return (long)p;
}
#endif

static Scheme_Object *process(int c, Scheme_Object *args[], 
			      char *name, int shell, int synchonous, 
			      int as_child)
{
#ifdef PROCESS_FUNCTION
  char *command;
  int to_subprocess[2], from_subprocess[2], err_subprocess[2];
  int i, pid;
  int def_exit_on;
  char **argv;
  Scheme_Object *in, *out, *subpid, *err, *thunk;
#if defined(UNIX_PROCESSES)
  System_Child *sc;
#else
  void *sc = 0;
#endif
#if defined(WINDOWS_PROCESSES) || defined(BEOS_PROCESSES)
  int spawn_status;

  /* Don't know how to do these, yet */
  if (shell && (!as_child || !synchonous)) {
    scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		     "%s: not supported on this platform",
		     name);
  }
#endif

  if (!SCHEME_STRINGP(args[0]) || scheme_string_has_null(args[0]))
    scheme_wrong_type(name, STRING_W_NO_NULLS, 0, c, args);

  if (shell) {
    argv = NULL;
    command = SCHEME_STR_VAL(args[0]);
  } else  {
    argv = MALLOC_N(char *, c + 1);
    {
      char *ef;
      ef = scheme_expand_filename(SCHEME_STR_VAL(args[0]),
				  SCHEME_STRTAG_VAL(args[0]),
				  name, NULL);
      argv[0] = ef;
    }
    {
      /* This is for Windows: */
      char *np;
      np = scheme_normal_path_case(argv[0], strlen(argv[0]));
      argv[0] = np;
    }
    
    for (i = 1; i < c; i++) { 
      if (!SCHEME_STRINGP(args[i]) || scheme_string_has_null(args[i]))
	scheme_wrong_type(name, STRING_W_NO_NULLS, i, c, args);
      argv[i] = SCHEME_STR_VAL(args[i]);
    }
    argv[c] = NULL;

    command = argv[0];
  }

  def_exit_on = SAME_OBJ(scheme_def_exit_proc,
			 scheme_get_param(scheme_config, MZCONFIG_EXIT_HANDLER));
  
  if (!synchonous) {
    if (PIPE_FUNC(to_subprocess _EXTRA_PIPE_ARGS))
      scheme_raise_exn(MZEXN_MISC,
		       "%s: pipe failed (too many ports open?)", name);
    if (PIPE_FUNC(from_subprocess _EXTRA_PIPE_ARGS)) {
      MSC_IZE(close)(to_subprocess[0]);
      MSC_IZE(close)(to_subprocess[1]);
      scheme_raise_exn(MZEXN_MISC,
		       "%s: pipe failed (too many ports open?)", name);
    }
    if (PIPE_FUNC(err_subprocess _EXTRA_PIPE_ARGS)) {
      MSC_IZE(close)(to_subprocess[0]);
      MSC_IZE(close)(to_subprocess[1]);
      MSC_IZE(close)(from_subprocess[0]);
      MSC_IZE(close)(from_subprocess[1]);
      scheme_raise_exn(MZEXN_MISC,
		       "%s: pipe failed (too many ports open?)", name);
    }
  }

#if defined(WINDOWS_PROCESSES) || defined(BEOS_PROCESSES)
# if defined(BEOS_PROCESSES)
  fflush(NULL);
# else
  /* Windows: quasi-stdin is locked, and we'll say it doesn't matter */
  fflush(stdin);
  fflush(stdout);
  fflush(stderr);
# endif

  if (shell) {
    /* Set real CWD - and hope no other thread changes it! */
    scheme_os_setcwd(SCHEME_STR_VAL(scheme_get_param(scheme_config, 
						     MZCONFIG_CURRENT_DIRECTORY)),
		     0);

    spawn_status = system(command);
  } else {
    int type;
    int save0, save1, save2;

#ifdef BEOS_PROCESSES
# define _P_NOWAIT 0
# define _P_OVERLAY 1
#endif

    if (!synchonous)
      type = _P_NOWAIT;
    else if (!as_child && def_exit_on)
      type = _P_OVERLAY;
    else 
      type = _P_NOWAIT; /* We'll implement waiting ourselves */

    if (!synchonous) {
      /* Save stdin and stdout */
      save0 = MSC_IZE(dup)(0);
      save1 = MSC_IZE(dup)(1);
      save2 = MSC_IZE(dup)(2);

      /* Copy pipe descriptors to stdin and stdout */
      MSC_IZE(dup2)(to_subprocess[0], 0);
      MSC_IZE(dup2)(from_subprocess[1], 1);
      MSC_IZE(dup2)(err_subprocess[1], 2);
    }

#ifdef WINDOWS_PROCESSES
    /* spawnv is too stupid to protect spaces, etc. in the arguments: */
    for (i = 0; i < c; i++)
      argv[i] = cmdline_protect(argv[i]);
#endif

    /* Set real CWD - and hope no other thread changes it! */
    scheme_os_setcwd(SCHEME_STR_VAL(scheme_get_param(scheme_config, 
						     MZCONFIG_CURRENT_DIRECTORY)),
		     0);

    spawn_status = MSC_IZE(spawnv)(type, command, (const char * const *)argv);

    if (!synchonous) {
      /* Restore stdin and stdout */
      MSC_IZE(dup2)(save0, 0);
      MSC_IZE(dup2)(save1, 1);
      MSC_IZE(dup2)(save2, 2);

      pid = spawn_status;
      
      if (spawn_status != -1)
        sc = (void *)pid;
    } else if ((spawn_status != -1) && !as_child) {
      sc = (void *)spawn_status;

      scheme_block_until(subp_done, subp_needs_wakeup, (void *)sc, (float)0.0);

#ifdef WINDOWS_PROCESSES
      {
	DWORD w;
	if (GetExitCodeProcess((HANDLE)sc, &w))
	  spawn_status = w;
	else
	  spawn_status = -1;
      }
#endif
#ifdef BEOS_PROCESSES
      spawn_status = (((BeOSProcess *)sc)->result ? -1 : 0);
#endif

      pid = 0;
    }
  }

  if (!as_child) {
    if (spawn_status != -1)
      scheme_do_exit(0, NULL);
    else
      return scheme_void;
  }
#else
  if (as_child || !def_exit_on) {
    sigset_t sigs;

    {
      static int sigchld_installed = 0;
      if (!sigchld_installed) {
	/* Catch child-done signals */
	START_XFORM_SKIP;
	MZ_SIGSET(SIGCHLD, child_done);
	END_XFORM_SKIP;
	sigchld_installed = 1;
      }
    }

    sc = MALLOC_ONE_RT(System_Child);
#ifdef MZTAG_REQUIRED
    sc->type = scheme_rt_system_child;
#endif
    sc->id = 0;
    sc->done = 0;

    sigemptyset(&sigs);
    sigaddset(&sigs, SIGCHLD);
    sigprocmask(SIG_BLOCK, &sigs, NULL);

    pid = fork();

    if (pid > 0) {
      sc->next = scheme_system_children;
      scheme_system_children = sc;
      sc->id = pid;
    }

    sigprocmask(SIG_UNBLOCK, &sigs, NULL);
  } else {
    sc = NULL;
    pid = 0;
  }

  switch (pid)
    {
    case -1:
      scheme_raise_exn(MZEXN_MISC,
		       "%s: fork failed", name);
      return scheme_false;

    case 0: /* child */

      {
	/* Ignore signals here */
	START_XFORM_SKIP;
	MZ_SIGSET(SIGCHLD, SIG_IGN);
	END_XFORM_SKIP;
      }

      if (!synchonous) {
	/* Copy pipe descriptors to stdin and stdout */
	MSC_IZE(dup2)(to_subprocess[0], 0);
	MSC_IZE(dup2)(from_subprocess[1], 1);
	MSC_IZE(dup2)(err_subprocess[1], 2);
	
	/* Close unwanted descriptors */
	MSC_IZE(close)(to_subprocess[0]);
	MSC_IZE(close)(to_subprocess[1]);
	MSC_IZE(close)(from_subprocess[0]);
	MSC_IZE(close)(from_subprocess[1]);
	MSC_IZE(close)(err_subprocess[0]);
	MSC_IZE(close)(err_subprocess[1]);
      }
      
      /* Set real CWD */
      scheme_os_setcwd(SCHEME_STR_VAL(scheme_get_param(scheme_config, MZCONFIG_CURRENT_DIRECTORY)), 0);

      /* Exec new process */      

      if (shell) {
	int v;

	v = system(command);

	if (!(v & 0xFF))
	  v = v >> 8;

	if (as_child || !def_exit_on || !v)
	  _exit(v);
	else
	  return scheme_void;
      } else {
	int err;

	err = MSC_IZE(execv)(command, argv);

	/* If we get here it failed; give up */

	if (as_child || !def_exit_on)
	  _exit(err ? err : -1);
	else
	  return scheme_void;
      }

    default: /* parent */

      break;
    }
#endif

  /* Close unneeded descriptors */

  if (!synchonous) {
    MSC_IZE(close)(to_subprocess[0]);
    MSC_IZE(close)(from_subprocess[1]);
    MSC_IZE(close)(err_subprocess[1]);

    scheme_file_open_count += 3;

#ifdef USE_FD_PORTS
    in = make_fd_input_port(from_subprocess[0], "subprocess-stdout");
    out = make_fd_output_port(to_subprocess[1]);
    err = make_fd_input_port(err_subprocess[0], "subprocess-stderr");
#else
    in = make_tested_file_input_port(MSC_IZE(fdopen)(from_subprocess[0], "r"), "subprocess-stdout", 1);
    out = scheme_make_file_output_port(MSC_IZE(fdopen)(to_subprocess[1], "w"));
    err = make_tested_file_input_port(MSC_IZE(fdopen)(err_subprocess[0], "r"), "subprocess-stderr", 1);
#endif

    subpid = scheme_make_integer_value(pid);
    thunk = scheme_make_closed_prim_w_arity(get_process_status,
					    sc, "control-process",
					    1, 1);

#define cons scheme_make_pair

    return cons(in,
		cons(out,
		     cons(subpid, 
			  cons(err, 
			       cons(thunk,
				    scheme_null)))));
  } else {
    int status;

#if defined(WINDOWS_PROCESSES) || defined(BEOS_PROCESSES)
    status = spawn_status;
#else
#if defined(UNIX_PROCESSES)
    if (!as_child) {
      /* exec Sucess => (exit) */
      /* exec Failure => (void) */
      /* But how do we know whether it succeeded? */
      scheme_do_exit(0, NULL);
      status = 0; /* Doesn't get here */
    } else {
      scheme_block_until(subp_done, NULL, (void *)sc, 0);
      status = !sc->status;
    }
#else
    -->> Configuration error <<--
#endif
#endif

    return status ? scheme_false : scheme_true;
  }

#else
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "%s: not supported on this platform",
		   name);
  return NULL;
#endif
}


static Scheme_Object *sch_process_star(int c, Scheme_Object *args[])
{
  return process(c, args, "process*", 0, 0, 1);
}

static Scheme_Object *sch_system_star(int c, Scheme_Object *args[])
{
#ifdef MACINTOSH_EVENTS
  if (c != 1) {
    scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		     "system*: extra arguments after the application pathname are "
		     "not supported for this platform");
    return NULL;
  }

  return (scheme_mac_start_app("system*", 0, args[0])
	  ? scheme_true
	  : scheme_false);
#else
  return process(c, args, "system*", 0, 1, 1);
#endif
}

static Scheme_Object *sch_execute_star(int c, Scheme_Object *args[])
{
#ifdef MACINTOSH_EVENTS
  if (c != 1) {
    scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		     "execute*: extra arguments after the application pathname are "
		     "not supported for this platform");
    return NULL;
  }

  if (scheme_mac_start_app("execute*", 0, args[0]))
    scheme_do_exit(0, NULL);

  return scheme_void;
#else
  return process(c, args, "execute*", 0, 1, 0);
#endif
}

static Scheme_Object *sch_process(int c, Scheme_Object *args[])
{
  return process(c, args, "process", 1, 0, 1);
}

static Scheme_Object *sch_system(int c, Scheme_Object *args[])
{
#ifdef MACINTOSH_EVENTS
  return (scheme_mac_start_app("system", 1, args[0])
	  ? scheme_true
	  : scheme_false);
#else
  return process(c, args, "system", 1, 1, 1);
#endif
}

static Scheme_Object *sch_execute(int c, Scheme_Object *args[])
{
#ifdef MACINTOSH_EVENTS
  if (scheme_mac_start_app("execute", 1, args[0]))
    scheme_do_exit(0, NULL);

  return scheme_void;
#else
  return process(c, args, "execute", 1, 1, 0);
#endif
}

static Scheme_Object *sch_send_event(int c, Scheme_Object *args[])
{
#ifdef MACINTOSH_EVENTS
  OSErr err;
  char *stage = "";
  Scheme_Object *result;
  if (scheme_mac_send_event("send-event", c, args, &result, &err, &stage))
    return result;
  else
    scheme_raise_exn(MZEXN_MISC, "send-event: failed (%s%d)", stage, (int)err);
#else
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "send-event: not supported on this platform");
  return NULL;
#endif
}

/**************************  T  C  P  *********************************/

/* These two need o be outside of USE_TCP */
#define PORT_ID_TYPE "exact integer in [1, 65535]"
#define CHECK_PORT_ID(obj) (SCHEME_INTP(obj) && (SCHEME_INT_VAL(obj) >= 1) && (SCHEME_INT_VAL(obj) <= 65535))


#ifdef USE_TCP

#ifdef USE_SOCKETS_TCP
#define MAKE_TCP_ARG tcp_t tcp, 
#else
#define MAKE_TCP_ARG
#endif

#define REGISTER_SOCKET(s) /**/
#define UNREGISTER_SOCKET(s) /**/

#ifdef USE_UNIX_SOCKETS_TCP
typedef struct sockaddr_in tcp_address;
#endif
#ifdef USE_WINSOCK_TCP
typedef struct SOCKADDR_IN tcp_address;
# undef REGISTER_SOCKET
# undef UNREGISTER_SOCKET
# define REGISTER_SOCKET(s) winsock_remember(s)
# define UNREGISTER_SOCKET(s) winsock_forget(s)
#endif

#ifdef USE_WINSOCK_TCP

/******************************* WinSock ***********************************/

static int wsr_size = 0;
static tcp_t *wsr_array;

static void winsock_remember(tcp_t s)
{
  int i, new_size;
  tcp_t *naya;

  for (i = 0; i < wsr_size; i++)
    if (!wsr_array[i]) {
      wsr_array[i] = s;
      return;
    }

  if (!wsr_size) {
    REGISTER_SO(wsr_array);
    new_size = 32;
  } else
    new_size = 2 * wsr_size;

  naya = MALLOC_N_ATOMIC(tcp_t, new_size);
  for (i = 0; i < wsr_size; i++)
    naya[i] = wsr_array[i];

  naya[wsr_size] = s;

  wsr_array = naya;
  wsr_size = new_size;  
}

static void winsock_forget(tcp_t s)
{
  int i;

  for (i = 0; i < wsr_size; i++)
    if (wsr_array[i] == s) {
      wsr_array[i] = (tcp_t)NULL;
      return;
    }
}

static int winsock_done(void)
{
  int i;

  for (i = 0; i < wsr_size; i++)
    if (wsr_array[i]) {
      closesocket(wsr_array[i]);
      wsr_array[i] = (tcp_t)NULL;
    }

  return WSACleanup();
}

static void TCP_INIT(char *name)
{
  static int started = 0;
  
  if (!started) {
    WSADATA data;
    if (!WSAStartup(MAKEWORD(1, 1), &data)) {
      started = 1;
      _onexit(winsock_done);
      return;
    }
  } else
    return;
  
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "%s: not supported on this machine"
		   " (no winsock driver)",
		   name);
}
#else
#ifdef USE_MAC_TCP

/***************************** Mac *******************************************/

/* Much of this is derived from (or at least influenced by) GUSI's TCP
   socket implementation, by Matthias Neeracher, which was derived from
   a library by Charlie Reiman Tom Milligan */

static short tcpDriverId;

#define	SOCK_STATE_NO_STREAM 0 /* Socket doesn't have a MacTCP stream yet */
#define	SOCK_STATE_UNCONNECTED 1 /* Socket is unconnected. */
#define	SOCK_STATE_LISTENING 2 /* Socket is listening for connection. */
#define	SOCK_STATE_CONNECTING 4 /* Socket is initiating a connection. */
#define	SOCK_STATE_CONNECTED 5 /* Socket is connected. */
#define	SOCK_STATE_CLOSING 6 /* Socket is closing */
#define	SOCK_STATE_CLOSED 8 /* Socket closed nicely */

typedef struct TCPiopbX {
  TCPiopb pb;
  Scheme_Tcp *data;
  struct TCPiopbX *next;
} TCPiopbX;

typedef struct {
  MZTAG_IF_REQUIRED
  wdsEntry e[2];
  TCPiopbX *xpb;
} WriteData;

static TCPiopbX *active_pbs;

static pascal void dnr_done(struct hostInfo *, int * done)
{
  *done = true;
}

static ResultUPP u_dnr_done;

static pascal void tcp_notify(StreamPtr stream, unsigned short eventCode,
			      Ptr userDataPtr, unsigned short something,
			      struct ICMPReport *reportPtr)
{
  tcp_t *t = (tcp_t *)userDataPtr;

  switch (eventCode) {
  case TCPClosing:
    t->state = SOCK_STATE_CLOSING;
    break;
    
  case TCPTerminate:
    if (t->state == SOCK_STATE_LISTENING)
      t->state = SOCK_STATE_CLOSED;
    else if (t->state == SOCK_STATE_CLOSING)
      t->state == SOCK_STATE_CLOSED;
    else
      t->state = SOCK_STATE_UNCONNECTED;
    break;
  }
}

static TCPNotifyUPP u_tcp_notify;

static void tcp_connect_done(TCPiopbX *pbx)
{
  if (!pbx->pb.ioResult)
    pbx->data->tcp.state = SOCK_STATE_CONNECTED;
}

static TCPIOCompletionUPP u_tcp_connect_done;

static void tcp_listen_done(TCPiopbX *pbx)
{
  TCPiopb *pb = &pbx->pb;
  Scheme_Tcp *data = pbx->data;

  switch(pb->ioResult) {
  case noErr:
    data->tcp.state = SOCK_STATE_CONNECTED;
    break;
    
  case openFailed:
  case invalidStreamPtr:
  case connectionExists:
  case duplicateSocket:
  case commandTimeout:
  default:
    data->tcp.state = SOCK_STATE_UNCONNECTED;
    data->tcp.async_errid = -pb->ioResult;
    break;
  }
}

static TCPIOCompletionUPP u_tcp_listen_done;

static void tcp_recv_done(TCPiopbX *pbx)
{
  TCPiopb *pb = &pbx->pb;
  Scheme_Tcp *data = pbx->data;
  
  if (!pb->ioResult)
    data->bufmax = pb->csParam.receive.rcvBuffLen;
}

static TCPIOCompletionUPP u_tcp_recv_done;

static void tcp_send_done(TCPiopbX *pbx)
{
  TCPiopb *pb = &pbx->pb;
  Scheme_Tcp *data = pbx->data;

  /* mark it free: */
  ((WriteData *)(pb->csParam.send.wdsPtr))->xpb = NULL;

  switch (pb->ioResult) {
  case noErr:
    break;
  case ipNoFragMemErr:
  case connectionClosing:
  case connectionTerminated:
  case connectionDoesntExist:
  case ipDontFragErr:
  case invalidStreamPtr:
  case invalidLength:
  case invalidWDS:
  default:
    data->tcp.state = SOCK_STATE_UNCONNECTED;
    data->tcp.async_errid = -pb->ioResult;
    break;
  }
}

static TCPIOCompletionUPP u_tcp_send_done;

/************** Mac Set-up *****************/

static void tcp_cleanup(void);

static void TCP_INIT(char *name)
{
  ParamBlockRec pb;
  short errNo;
  struct GetAddrParamBlock pbr;
	
  pb.ioParam.ioCompletion = 0L; 
  pb.ioParam.ioNamePtr = (StringPtr) "\p.IPP"; 
  pb.ioParam.ioPermssn = fsCurPerm;
  
  if ((errNo = PBOpenSync(&pb))
      || (errNo = OpenResolver(NULL))) {
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "%s: TCP initialization error (%d)",
		     name, (int)errNo);
  }
		
  tcpDriverId = pb.ioParam.ioRefNum; 
  
  u_dnr_done = NewResultProc(dnr_done);
  u_tcp_notify = NewTCPNotifyProc(tcp_notify);
  u_tcp_connect_done = NewTCPIOCompletionProc(tcp_connect_done);
  u_tcp_listen_done = NewTCPIOCompletionProc(tcp_listen_done);
  u_tcp_recv_done = NewTCPIOCompletionProc(tcp_recv_done);
  u_tcp_send_done = NewTCPIOCompletionProc(tcp_send_done);
  
  REGISTER_SO(active_pbs);
  
  atexit(tcp_cleanup);
}

static int tcp_addr(char *address, struct hostInfo *info)
{
  int tries = 3;
  long *done = MALLOC_ONE_ATOMIC(long);
  
  /* Check for numerical address: */
  {
     unsigned char *s = (unsigned char *)address, n[4];
     int p = 0, v = 0;
     while (*s) {
       if (isdigit(*s)) {
         if (v < 256)
           v = (v * 10) + (*s - '0');
       } else if (*s == '.') {
         if (p < 4) {
           n[p] = v;
           p++;
         }
         v = 0;
       } else
         break;
       s++;
     }
     
     if (p == 3) {
       n[p] = v;
       p++;
     }
     
     if (!*s && (p == 4)
         && (s[0] < 256) && (s[1] < 256)
         && (s[2] < 256) && (s[3] < 256)) {
       /* Numerical address */
       info->addr[0] = *(unsigned long *)n;
       return 0;
     }
  }
  
 try_again:
  *done = 0;
  info->rtnCode = 0;
  if (StrToAddr(address, info, u_dnr_done, (char *)done) == cacheFault) {
    /* FIXME: If we get a break, it's possible that `info' and `done' will be
              GCed before the async call completes. */
    while (!*done) { scheme_process_block(0.25); }
    scheme_current_process->ran_some = 1;
  }
  if (info->rtnCode == cacheFault) {
    if (--tries)
      goto try_again;
  }
  if (info->rtnCode)
    return info->rtnCode;
  if (info->cname[0] == 0)
    return -42;
  
  return 0;
}

/* Forward prototype: */
static Scheme_Tcp *make_tcp_port_data(MAKE_TCP_ARG int refcount);

#define STREAM_BUFFER_SIZE 131072

static TCPiopbX *mac_make_xpb(Scheme_Tcp *data)
{
  TCPiopbX *xpb;

  /* FIXME, precise GC: no GC tag... */
  xpb = (TCPiopbX *)scheme_malloc(sizeof(TCPiopbX));
  
  memcpy(xpb, data->tcp.create_pb, sizeof(TCPiopb));

  xpb->data = data;
  
  data->tcp.current_pb = xpb;

  return xpb;
}

static int mac_tcp_make(TCPiopbX **_xpb, TCPiopb **_pb, Scheme_Tcp **_data)
{
  TCPiopbX *xpb;
  TCPiopb *pb;
  Scheme_Tcp *data;
  int errid;

  data = make_tcp_port_data(2);
  
  /* FIXME, precise GC: no GC tag... */
  xpb = (TCPiopbX *)scheme_malloc(sizeof(TCPiopbX));
  xpb->next = active_pbs;
  active_pbs = xpb;
  
  pb = (TCPiopb *)xpb;

  pb->ioCRefNum = tcpDriverId;
  pb->csCode = TCPCreate;
  pb->csParam.create.rcvBuff = (char *)scheme_malloc_atomic(STREAM_BUFFER_SIZE);
  pb->csParam.create.rcvBuffLen = STREAM_BUFFER_SIZE;
  pb->csParam.create.notifyProc = u_tcp_notify;
  pb->csParam.create.userDataPtr = (char *)&data->tcp;
  
  xpb->data = data;
  
  if ((errid = PBControlSync((ParamBlockRec*)pb)))
    return errid;
	
  data->tcp.create_pb = (void *)pb;
  data->tcp.stream = pb->tcpStream;
  data->tcp.async_errid = -1;

  *_xpb = xpb;
  *_pb = pb;
  *_data = data;

  return 0;
}

static void mac_tcp_close(Scheme_Tcp *data)
{
  TCPiopb *pb;
  
  pb = (TCPiopb *)mac_make_xpb(data);
  
  pb->ioCompletion = NULL;
  pb->csCode = TCPClose;
  pb->csParam.close.validityFlags = timeoutValue | timeoutAction;
  pb->csParam.close.ulpTimeoutValue = 60 /* seconds */;
  pb->csParam.close.ulpTimeoutAction = 1 /* 1:abort 0:report */;
  PBControlSync((ParamBlockRec*)pb);

  pb->csCode = TCPRelease;
  PBControlSync((ParamBlockRec*)pb);

 {
    TCPiopbX *x, *prev = NULL;
    x = active_pbs;
    while (x) {
      if (x->data->tcp.stream == data->tcp.stream) {
	if (!prev)
	  active_pbs = x->next;
	else
	  prev->next = x->next;
	break;
      } else {
        prev = x;
        x = x->next;
      }
    }
  }
}

static int mac_tcp_listen(int id, Scheme_Tcp **_data)
{
  TCPiopbX *xpb;
  TCPiopb *pb;
  Scheme_Tcp *data;
  int errid;
  
  if (!(errid = mac_tcp_make(&xpb, &pb, &data))) {
    data->tcp.state = SOCK_STATE_LISTENING;
    
    xpb = mac_make_xpb(data);
    pb = (TCPiopb *)xpb;

    pb->ioCompletion = u_tcp_listen_done;
    pb->csCode = TCPPassiveOpen;
    pb->csParam.open.validityFlags = timeoutValue | timeoutAction;
    pb->csParam.open.ulpTimeoutValue = 0 /* seconds; 0 = infinity */;
    pb->csParam.open.ulpTimeoutAction = 0 /* 1:abort 0:report */;
    pb->csParam.open.commandTimeoutValue = 0 /* seconds; 0 = infinity */;
    pb->csParam.open.remoteHost = 0;
    pb->csParam.open.remotePort = 0;
    pb->csParam.open.localHost = 0;
    pb->csParam.open.localPort = id;
    pb->csParam.open.dontFrag = 0;
    pb->csParam.open.timeToLive = 0;
    pb->csParam.open.security = 0;
    pb->csParam.open.optionCnt = 0;

    if ((errid = PBControlAsync((ParmBlkPtr)pb))) {
      data->tcp.state = SOCK_STATE_UNCONNECTED;
      mac_tcp_close(data);
      return errid;
    } else {
      *_data = data;
      return 0;
    }
  } else
    return errid;
}

static void tcp_cleanup(void)
{
  while (active_pbs) {
    TCPiopbX *pb = active_pbs;
    active_pbs = active_pbs->next;
    mac_tcp_close(pb->data);
  }
}

#else
#define TCP_INIT(x) /* nothing */
#endif
#endif

/*************************** Generic **************************************/

#ifdef USE_SOCKETS_TCP
#define LISTENER_WAS_CLOSED(x) (((listener_t *)(x))->s == INVALID_SOCKET)
#endif
#ifdef USE_MAC_TCP
#define LISTENER_WAS_CLOSED(x) !((listener_t *)(x))->datas
#endif
#ifndef LISTENER_WAS_CLOSED
#define LISTENER_WAS_CLOSED(x) 0
#endif

/* Forward declaration */
static int stop_listener(Scheme_Object *o);

static int tcp_check_accept(Scheme_Object *listener)
{
#ifdef USE_SOCKETS_TCP
  tcp_t s;
  DECL_FDSET(readfds, 1);
  DECL_FDSET(exnfds, 1);
  struct timeval time = {0, 0};

  INIT_DECL_FDSET(readfds, 1);
  INIT_DECL_FDSET(exnfds, 1);

  if (LISTENER_WAS_CLOSED(listener))
    return 1;

  s = ((listener_t *)listener)->s;

  MZ_FD_ZERO(readfds);
  MZ_FD_ZERO(exnfds);
  MZ_FD_SET(s, readfds);
  MZ_FD_SET(s, exnfds);
    
  return select(s + 1, readfds, NULL, exnfds, &time);
#endif
#ifdef USE_MAC_TCP
  int i, count;
  Scheme_Tcp **datas;
  count = ((listener_t *)listener)->count;
  datas = ((listener_t *)listener)->datas;

  if (LISTENER_WAS_CLOSED(listener))
    return 1;

  for (i = 0; i < count; i++)
    if (datas[i] && (datas[i]->tcp.state != SOCK_STATE_LISTENING))
      return 1;

  return 0;
#endif
}

static void tcp_accept_needs_wakeup(Scheme_Object *listener, void *fds)
{
#ifdef USE_SOCKETS_TCP
  tcp_t s = ((listener_t *)listener)->s;
  void *fds2;

  fds2 = MZ_GET_FDSET(fds, 2);

  MZ_FD_SET(s, (fd_set *)fds);
  MZ_FD_SET(s, (fd_set *)fds2);
#endif
}

static int tcp_check_connect(Scheme_Object *connector)
{
#ifdef USE_MAC_TCP
  return ((TCPiopb *)connector)->ioResult != inProgress;
#else
#ifdef USE_SOCKETS_TCP
  tcp_t s;
  DECL_FDSET(writefds, 1);
  DECL_FDSET(exnfds, 1);
  struct timeval time = {0, 0};

  INIT_DECL_FDSET(writefds, 1);
  INIT_DECL_FDSET(exnfds, 1);

  s = (tcp_t)connector;

  MZ_FD_ZERO(writefds);
  MZ_FD_SET(s, writefds);
  MZ_FD_ZERO(exnfds);
  MZ_FD_SET(s, exnfds);
    
  if (!select(s + 1, NULL, writefds, exnfds, &time))
    return 0;
  if (FD_ISSET(s, exnfds))
    return -1;
  else
    return 1;
#else
  return 0;
#endif
#endif
}

static void tcp_connect_needs_wakeup(Scheme_Object *connector, void *fds)
{
#ifdef USE_SOCKETS_TCP
  void *fds1, *fds2;
  tcp_t s = (tcp_t)connector;
  
  fds1 = MZ_GET_FDSET(fds, 1);
  fds2 = MZ_GET_FDSET(fds, 2);

  MZ_FD_SET(s, (fd_set *)fds1);
  MZ_FD_SET(s, (fd_set *)fds2);
#endif
}

#ifdef USE_MAC_TCP
static void tcp_read_needs_wakeup(Scheme_Object *connector, void *fds)
{
}

static int tcp_check_read(Scheme_Object *pb)
{
  return (((TCPiopb *)pb)->ioResult != inProgress);
}
#endif

static int tcp_check_write(Scheme_Object *conn)
{
  Scheme_Tcp *data = (Scheme_Tcp *)conn;

#ifdef USE_SOCKETS_TCP
  tcp_t s;
  DECL_FDSET(writefds, 1);
  DECL_FDSET(exnfds, 1);
  struct timeval time = {0, 0};

  INIT_DECL_FDSET(writefds, 1);
  INIT_DECL_FDSET(exnfds, 1);

  s = data->tcp;

  MZ_FD_ZERO(writefds);
  MZ_FD_SET(s, writefds);
  MZ_FD_ZERO(exnfds);
  MZ_FD_SET(s, exnfds);
  
  return select(s + 1, NULL, writefds, exnfds, &time);
#else
  TCPiopbX *xpb;
  TCPiopb *pb;
  int bytes;

  xpb = mac_make_xpb(data);
  pb = (TCPiopb *)xpb;
    
  pb->csCode = TCPStatus;
  if (PBControlSync((ParamBlockRec*)pb))
    bytes = -1;
  else {
    bytes = pb->csParam.status.sendWindow - pb->csParam.status.amtUnackedData;
    if (bytes < 0)
      bytes = 0;
  }
  
  return !!bytes;
#endif
}

static void tcp_write_needs_wakeup(Scheme_Object *conn, void *fds)
{
#ifdef USE_SOCKETS_TCP
  void *fds1, *fds2;
  tcp_t s = ((Scheme_Tcp *)conn)->tcp;
  
  fds1 = MZ_GET_FDSET(fds, 1);
  fds2 = MZ_GET_FDSET(fds, 2);

  MZ_FD_SET(s, (fd_set *)fds1);
  MZ_FD_SET(s, (fd_set *)fds2);
#endif
}


static Scheme_Tcp *make_tcp_port_data(MAKE_TCP_ARG int refcount)
{
  Scheme_Tcp *data;
  
  data = MALLOC_ONE_RT(Scheme_Tcp);
#ifdef MZTAG_REQUIRED
  data->type = scheme_rt_tcp;
#endif
#ifdef USE_SOCKETS_TCP
  data->tcp = tcp;
#endif
  data->bufpos = 0;
  data->bufmax = 0;
  data->hiteof = 0;
  data->refcount = refcount;

#ifndef USE_MAC_TCP
# ifdef USE_WINSOCK_TCP
  {
    unsigned long ioarg = 1;
    ioctlsocket(tcp, FIONBIO, &ioarg);
  }
# else
  fcntl(tcp, F_SETFL, TCP_NONBLOCKING);
# endif
#endif

  return data;
}

static int tcp_char_ready (Scheme_Input_Port *port)
{
  Scheme_Tcp *data;
#ifdef USE_SOCKETS_TCP
  DECL_FDSET(readfds, 1);
  DECL_FDSET(exfds, 1);
  struct timeval time = {0, 0};

  INIT_DECL_FDSET(readfds, 1);
  INIT_DECL_FDSET(exfds, 1);
#endif

  data = (Scheme_Tcp *)port->port_data;

  if (data->hiteof)
    return 1;
  if (data->bufpos < data->bufmax)
    return 1;

#ifdef USE_SOCKETS_TCP
  MZ_FD_ZERO(readfds);
  MZ_FD_ZERO(exfds);
  MZ_FD_SET(data->tcp, readfds);
  MZ_FD_SET(data->tcp, exfds);
    
  return select(data->tcp + 1, readfds, NULL, exfds, &time);
#endif

#ifdef USE_MAC_TCP
  if (data->tcp.state == SOCK_STATE_CONNECTED) {
    /* socket is connected */
    TCPiopbX *xpb;
    TCPiopb *pb;
    
    xpb = mac_make_xpb(data);
    pb = (TCPiopb *)xpb;
    
    pb->csCode = TCPStatus;
    pb->ioCompletion = NULL;

    if (PBControlSync((ParamBlockRec*)pb))
      return 1;
      
    if (pb->csParam.status.amtUnreadData)
      return 1;
 } else
   return 1;
#endif

  return 0;
}

static int tcp_getc(Scheme_Input_Port *port)
{
  int errid;
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

#ifdef USE_MAC_TCP
 top:
#endif

  if (data->hiteof)
    return EOF;

#ifdef USE_MAC_TCP
  if (!data->activeRcv)
#endif
    if (data->bufpos < data->bufmax)
      return (unsigned char)data->buffer[data->bufpos++];

  if (!tcp_char_ready(port)) {
#ifdef USE_SOCKETS_TCP
    scheme_current_process->block_descriptor = PORT_BLOCKED;
    scheme_current_process->blocker = (Scheme_Object *)port;
#endif
    do {
      scheme_process_block((float)0.0);
    } while (!tcp_char_ready(port));
#ifdef USE_SOCKETS_TCP
    scheme_current_process->block_descriptor = NOT_BLOCKED;
    scheme_current_process->blocker = NULL;
#endif
    scheme_current_process->ran_some = 1;
  }

#ifdef USE_SOCKETS_TCP
  data->bufmax = recv(data->tcp, data->buffer, TCP_BUFFER_SIZE, 0);
  errid = errno;
#endif
#ifdef USE_MAC_TCP
  /* Allow only one read at a time: */
  if (!data->tcp.lock)
    data->tcp.lock = scheme_make_sema(0);
  else {
    if (!scheme_wait_sema(data->tcp.lock, 1)) {
      /* Do it the hard way: */
      scheme_wait_sema(data->tcp.lock, 0);
      scheme_post_sema(data->tcp.lock);
      goto top;
    }
  }
  
  if (data->activeRcv || (data->tcp.state == SOCK_STATE_CONNECTED)) {
    /* socket is connected or an old recv is unfinished */
    TCPiopb *pb;    

    if (data->activeRcv) {
      pb = data->activeRcv;
    } else {
      pb = (TCPiopb *)mac_make_xpb(data);
    
      pb->csCode = TCPRcv;
      pb->ioCompletion = u_tcp_recv_done;
      pb->csParam.receive.commandTimeoutValue = 0; /* seconds, 0 = blocking */
      pb->csParam.receive.rcvBuff = data->buffer;
      pb->csParam.receive.rcvBuffLen = TCP_BUFFER_SIZE;
    
      data->activeRcv = pb;

      PBControlAsync((ParamBlockRec*)pb);
    }

    BEGIN_ESCAPEABLE(scheme_post_sema(data->tcp.lock));
    scheme_block_until(tcp_check_read, tcp_read_needs_wakeup, pb, 0);
    END_ESCAPEABLE();

    data->activeRcv = NULL;
    
    switch((errid = pb->ioResult)) {
    case noErr:
    case connectionClosing:
    case connectionTerminated:
      errid = 0;
      break;
    case commandTimeout:
    case connectionDoesntExist:
    case invalidStreamPtr:
    case invalidLength:
    case invalidBufPtr:
    default:
      break;
    }
  } else if (data->tcp.state == SOCK_STATE_CLOSING 
             || data->tcp.state == SOCK_STATE_CLOSED) {
    data->bufmax = 0;
    errid = 0;
  } else
    errid = data->tcp.async_errid;
  
  if (errid)
    data->bufmax = -1;
    
  scheme_post_sema(data->tcp.lock);
#endif
  
  if (data->bufmax == -1) {
    scheme_raise_exn(MZEXN_I_O_PORT_READ,
		     port,
		     "tcp-read: error reading (%d)",
		     errid);
  } else if (!data->bufmax) {
    data->hiteof = 1;
    return EOF;
  }

  data->bufpos = 1;
    
  return (unsigned char)data->buffer[0];
}

static void tcp_need_wakeup(Scheme_Input_Port *port, void *fds)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

#ifdef USE_SOCKETS_TCP
  {
    void *fds2;
  
    fds2 = MZ_GET_FDSET(fds, 2);
    
    MZ_FD_SET(data->tcp, (fd_set *)fds);
    MZ_FD_SET(data->tcp, (fd_set *)fds2);
  }
#endif
}

static void tcp_close_input(Scheme_Input_Port *port)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

  if (--data->refcount)
    return;

#ifdef USE_SOCKETS_TCP
  UNREGISTER_SOCKET(data->tcp);
  closesocket(data->tcp);
#endif
#ifdef USE_MAC_TCP
  mac_tcp_close(data);
#endif

  --scheme_file_open_count;
}

/* forward decls: */
static void tcp_write_string(char *s, long len, Scheme_Output_Port *port);
static void tcp_close_output(Scheme_Output_Port *port);

static void tcp_write_string(char *s, long len, Scheme_Output_Port *port)
{
  Scheme_Tcp *data;
  int errid, would_block = 0;
#ifdef USE_SOCKETS_TCP
  int sent;
#endif

  if (!len)
    return;

  data = (Scheme_Tcp *)port->port_data;

 top:

#ifdef USE_SOCKETS_TCP
  if ((sent = send(data->tcp, s, len, 0)) != len) {
#ifdef USE_WINSOCK_TCP
    errid = WSAGetLastError();
# define SEND_BAD_MSG_SIZE(e) (e == WSAEMSGSIZE)
# define SEND_WOULD_BLOCK(e) \
    ((e == WSAEWOULDBLOCK) || (e == WSAEINPROGRESS))
#else
    errid = errno;
# ifdef SEND_IS_NEVER_TOO_BIG
#  define SEND_BAD_MSG_SIZE(errid) 0
# else
#  define SEND_BAD_MSG_SIZE(errid) (errid == EMSGSIZE)
# endif
# define SEND_WOULD_BLOCK(errid) \
   ((errid == EWOULDBLOCK) || (errid == EAGAIN)\
    || (errid == EINPROGRESS) || (errid == EALREADY))
#endif
    if (sent > 0) {
      /* Some data was sent. Recur to handle the rest */
      tcp_write_string(s + sent, len - sent, port);
      errid = 0;
    } else if ((len > 1) && SEND_BAD_MSG_SIZE(errid)) {
      /* split the message and try again: */
      int half = (len / 2);
      tcp_write_string(s, half, port);
      tcp_write_string(s + half, len - half, port);
      errid = 0;
    } else if (SEND_WOULD_BLOCK(errid)) {
      errid = 0;
      would_block = 1;
    }
  } else
    errid = 0;
#endif
#ifdef USE_MAC_TCP
  errid = 0;
  if (data->tcp.state == SOCK_STATE_CONNECTED) {
    /* socket is connected */
    TCPiopbX *xpb;
    TCPiopb *pb;
    int bytes;

    xpb = mac_make_xpb(data);
    pb = (TCPiopb *)xpb;
    
    pb->csCode = TCPStatus;
    if ((errid = PBControlSync((ParamBlockRec*)pb)))
      bytes = 0;
    else {
      bytes = pb->csParam.status.sendWindow - pb->csParam.status.amtUnackedData;
      if (bytes < 0)
	bytes = 0;
    }
    
    if (bytes >= len) {
      WriteData *wd;
      wdsEntry *e;
      int i;
      
      wd = NULL;
      for (i = 0; i < num_tcp_send_buffers; i++)
	if (!((WriteData *)(tcp_send_buffers[i]))->xpb) {
	  wd = (WriteData *)(tcp_send_buffers[i]);
	  break;
	}
      
      if (!wd) {
	void **naya;
	int nayac;
	
	nayac = (2 * num_tcp_send_buffers) + 1;
	naya = MALLOC_N(void *, nayac);
	memcpy(naya, tcp_send_buffers, sizeof(void *) * num_tcp_send_buffers);
	for (i = num_tcp_send_buffers; i < nayac; i++) {
	  wd = MALLOC_ONE_RT(WriteData);
#ifdef MZTAG_REQUIRED
	  wd->type = scheme_rt_write_data;
#endif
	  wd->xpb = NULL;
	  e = wd->e;
	  e[0].ptr = NULL;
	  e[1].ptr = NULL;
	  e[1].length = 0;
	  naya[i] = (void *)e;
	}

	wd = (WriteData *)naya[num_tcp_send_buffers];
	
	tcp_send_buffers = naya;
	num_tcp_send_buffers = nayac;
      }

      wd->xpb = xpb;
      e = wd->e;

      e[0].ptr = (Ptr)scheme_malloc_atomic(len);
      memcpy(e[0].ptr, s, len);
      e[0].length = len;
      e[1].ptr = NULL;
      e[1].length = 0;

      pb->csCode = TCPSend;
      pb->ioCompletion = u_tcp_send_done;
      pb->csParam.send.validityFlags = timeoutValue | timeoutAction;
      pb->csParam.send.ulpTimeoutValue = 60 /* seconds */;
      pb->csParam.send.ulpTimeoutAction = 1 /* 0:abort 1:report */;
      pb->csParam.send.pushFlag = 1;
      pb->csParam.send.urgentFlag = 0;
      pb->csParam.send.wdsPtr = (Ptr)e;
      pb->csParam.send.sendFree = 0;
      pb->csParam.send.sendLength = 0;
      
      errid = PBControlAsync((ParamBlockRec*)pb);
    } else if (!errid) {
      if (bytes) {
      	/* Do partial write: */
        tcp_write_string(s, bytes, port);
        tcp_write_string(s + bytes, len - bytes, port);
      } else
        would_block = 1;
    }
  } else
    errid = data->tcp.async_errid;
#endif

  if (would_block) {
    /* Block for writing: */
    scheme_block_until(tcp_check_write, tcp_write_needs_wakeup, data, (float)0.0);

    /* Ok - try again! */
    would_block = 0;
    goto top;
  }

  if (errid)
    scheme_raise_exn(MZEXN_I_O_PORT_WRITE,
		     port,
		     "tcp-write: error writing (%d)",
		     errid);
}

static void tcp_close_output(Scheme_Output_Port *port)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

  if (--data->refcount)
    return;

#ifdef USE_UNIX_SOCKETS_TCP
  close(data->tcp);
#endif
#ifdef USE_WINSOCK_TCP
  shutdown(data->tcp, 2);
#endif
#ifdef USE_MAC_TCP
  mac_tcp_close(data);
#endif
}

static Scheme_Object *
make_named_tcp_input_port(void *data, const char *name)
{
  Scheme_Input_Port *ip;

  ip = _scheme_make_input_port(tcp_input_port_type,
			       data,
			       tcp_getc,
			       NULL,
			       tcp_char_ready,
			       tcp_close_input,
			       tcp_need_wakeup,
			       1);

  ip->name = (char *)name;

  return (Scheme_Object *)ip;
}

static Scheme_Object *
make_tcp_output_port(void *data)
{
  return (Scheme_Object *)scheme_make_output_port(tcp_output_port_type,
						  data,
						  tcp_write_string,
						  tcp_close_output,
						  1);
}

#endif /* USE_TCP */

#ifndef NO_TCP_SUPPORT

# ifdef PROTOENT_IS_INT
#  define PROTO_P_PROTO PROTOENT_IS_INT
# else
#  define PROTO_P_PROTO proto->p_proto
# endif

# ifndef MZ_PF_INET
#  define MZ_PF_INET PF_INET
# endif

static Scheme_Object *tcp_connect(int argc, Scheme_Object *argv[])
{
  char * volatile address = "", * volatile errmsg = "";
  unsigned short origid, id;
  int errpart = 0, errid = 0;
#ifdef USE_SOCKETS_TCP
  struct hostent *host;
  tcp_address tcp_connect_dest_addr; /* Use a long name for precise GC's xform.ss */
# ifndef PROTOENT_IS_INT
  struct protoent *proto;
# endif
#endif

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("tcp-connect", "string", 0, argc, argv);
  if (!CHECK_PORT_ID(argv[1]))
    scheme_wrong_type("tcp-connect", PORT_ID_TYPE, 1, argc, argv);

#ifdef USE_TCP
  TCP_INIT("tcp-connect");
#endif

  address = SCHEME_STR_VAL(argv[0]);
  origid = (unsigned short)SCHEME_INT_VAL(argv[1]);

#ifdef USE_TCP
  /* Set id in network order: */
  id = htons(origid);
#endif

#ifdef USE_MAC_TCP
  {
    TCPiopbX *xpb;
    TCPiopb *pb;
    Scheme_Tcp *data;
    int errNo;
    struct hostInfo *dest_host;
    Scheme_Object *v[2];
    
    dest_host = MALLOC_ONE_ATOMIC(struct hostInfo);
    if ((errNo = tcp_addr(address, dest_host))) {
      errpart = 1;
      errmsg = " host not found";
      goto tcp_error;
    }
    
    if ((errNo = mac_tcp_make(&xpb, &pb, &data))) {
      errpart = 2;
      goto tcp_error;
    }

    data->tcp.state = SOCK_STATE_CONNECTING;
    
    pb->ioCompletion = u_tcp_connect_done;
    pb->csCode = TCPActiveOpen;
    pb->csParam.open.validityFlags = timeoutValue | timeoutAction;
    pb->csParam.open.ulpTimeoutValue = 60 /* seconds */;
    pb->csParam.open.ulpTimeoutAction = 1 /* 1:abort 0:report */;
    pb->csParam.open.commandTimeoutValue = 0;
    pb->csParam.open.remoteHost = dest_host->addr[0];
    pb->csParam.open.remotePort = id;
    pb->csParam.open.localHost = 0;
    pb->csParam.open.localPort = 0;
    pb->csParam.open.dontFrag = 0;
    pb->csParam.open.timeToLive = 0;
    pb->csParam.open.security = 0;
    pb->csParam.open.optionCnt = 0;

    if (errNo = PBControlAsync((ParamBlockRec*)pb)) {
      errpart = 3;
      goto tcp_close_and_error;
    }
    
    BEGIN_ESCAPEABLE(mac_tcp_close(data));
    scheme_block_until(tcp_check_connect, tcp_connect_needs_wakeup, pb, 0);
    END_ESCAPEABLE();
    
    if (data->tcp.state != SOCK_STATE_CONNECTED) {
      errpart = 4;
      errNo = pb->ioResult;
      goto tcp_close_and_error;
    }
    
    v[0] = make_named_tcp_input_port(data, address);
    v[1] = make_tcp_output_port(data);
    
    return scheme_values(2, v);
    
  tcp_close_and_error:
    
    mac_tcp_close(data);
    
  tcp_error:
    
    errid = errNo;
  }
#endif

#ifdef USE_SOCKETS_TCP
  host = gethostbyname(address);
  if (host) {
    tcp_connect_dest_addr.sin_family = AF_INET;
    tcp_connect_dest_addr.sin_port = id;
    memset(&tcp_connect_dest_addr.sin_addr, 0, sizeof(tcp_connect_dest_addr.sin_addr));
    memset(&tcp_connect_dest_addr.sin_zero, 0, sizeof(tcp_connect_dest_addr.sin_zero));
    memcpy(&tcp_connect_dest_addr.sin_addr, host->h_addr_list[0], host->h_length); 

#ifndef PROTOENT_IS_INT
    proto = getprotobyname("tcp");
    if (proto)
#endif
    {
      tcp_t s = socket(MZ_PF_INET, SOCK_STREAM, PROTO_P_PROTO);
      if (s != INVALID_SOCKET) {
	int status, inprogress;
#ifdef USE_WINSOCK_TCP
	unsigned long ioarg = 1;
	ioctlsocket(s, FIONBIO, &ioarg);
#else
	int size = TCP_SOCKSENDBUF_SIZE;
	fcntl(s, F_SETFL, TCP_NONBLOCKING);
# ifndef CANT_SET_SOCKET_BUFSIZE
	setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char *)&size, sizeof(int));
# endif
#endif
	status = connect(s, (struct sockaddr *)&tcp_connect_dest_addr, sizeof(tcp_connect_dest_addr));
#ifdef USE_UNIX_SOCKETS_TCP
	if (status)
	  status = errno;
	
	inprogress = (status == EINPROGRESS);
#endif
#ifdef USE_WINSOCK_TCP
	if (status)
	  status = WSAGetLastError();

	inprogress = (status == WSAEWOULDBLOCK);
#endif

	scheme_file_open_count++;
	
	if (inprogress) {
          BEGIN_ESCAPEABLE(closesocket(s); --scheme_file_open_count);
	  status = scheme_block_until(tcp_check_connect, tcp_connect_needs_wakeup, (void *)s, (float)0.0);
	  END_ESCAPEABLE();
	  if (status == 1) {
#ifdef USE_UNIX_SOCKETS_TCP
	    status = recv(s, NULL, 0, 0); /* test input */
	    if (!status)
	      status = send(s, NULL, 0, 0); /* test output */
#else
	    status = send(s, "", 0, 0); /* test output */
	    if (status)
	      errno = WSAGetLastError();
#endif
	  }
	}
	
	if (!status) {
	  Scheme_Object *v[2];
	  Scheme_Tcp *tcp;

	  tcp = make_tcp_port_data(s, 2);
	  
	  v[0] = make_named_tcp_input_port(tcp, address);
	  v[1] = make_tcp_output_port(tcp);
	  
	  REGISTER_SOCKET(s);

	  return scheme_values(2, v);
	} else {
	  closesocket(s);
	  --scheme_file_open_count;
	  errpart = 4;
	}
      } else
	errpart = 3;
      errid = errno;
    }
#ifndef PROTOENT_IS_INT
    else
      (errid = 1, errpart = 2);
#endif
  } else {
    errpart = 1;
#ifdef USE_WINSOCK_TCP
    errid = WSAGetLastError();
#else
    errid = 0;
#endif
    errmsg = " host not found";
  }
#endif

#ifdef USE_TCP
  scheme_raise_exn(MZEXN_I_O_TCP,
		   "tcp-connect: connection to %s, port %d failed (%d%s%d%s)",
		   address, origid, errpart, ":", errid, errmsg);
#else
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "tcp-connect: not supported on this platform");
#endif

  return NULL;
}

static Scheme_Object *
tcp_listen(int argc, Scheme_Object *argv[])
{
  unsigned short id, origid;
  int backlog, errid;
#ifdef USE_SOCKETS_TCP
# ifndef PROTOENT_IS_INT
  struct protoent *proto;
# endif
#endif

  if (!CHECK_PORT_ID(argv[0]))
    scheme_wrong_type("tcp-listen", PORT_ID_TYPE, 0, argc, argv);
  if (argc > 1)
    if (!SCHEME_INTP(argv[1]) || (SCHEME_INT_VAL(argv[1]) < 1))
      scheme_wrong_type("tcp-listen", "small positive integer", 1, argc, argv);
    
#ifdef USE_TCP
  TCP_INIT("tcp-listen");
#endif

  origid = (unsigned short)SCHEME_INT_VAL(argv[0]);
  if (argc > 1)
    backlog = SCHEME_INT_VAL(argv[1]);
  else
    backlog = 4;

#ifdef USE_TCP
  /* Set id in network order: */
  id = htons(origid);
#endif

#ifdef USE_MAC_TCP
  {
    int i;
    Scheme_Tcp **datas, *data;

    datas = MALLOC_N(Scheme_Tcp *, backlog);

    for (i = 0; i < backlog; i++) {
      if ((errid = mac_tcp_listen(id, &data))) {
        /* Close listeners that had succeeded: */
        int j;
        for (j = 0; j < i; j++)
          mac_tcp_close(datas[i]);
	break;
      }
      datas[i] = data;
    }

    if (!errid) {
      listener_t *l = MALLOC_ONE_TAGGED(listener_t);

      l->type = scheme_listener_type;
      l->portid = id;
      l->count = backlog;
      l->datas = datas;
      l->mref = scheme_add_managed(NULL,
				   (Scheme_Object *)l,
				   (Scheme_Close_Manager_Client *)stop_listener,
				   NULL,
				   1);
      
      return (Scheme_Object *)l;
    }
  }
#endif

#ifdef USE_SOCKETS_TCP
# ifndef PROTOENT_IS_INT
  proto = getprotobyname("tcp");
  if (proto)
# endif
  {
    tcp_address tcp_listen_addr; /* Use a long name for precise GC's xform.ss */
    tcp_t s;

    tcp_listen_addr.sin_family = AF_INET;
    tcp_listen_addr.sin_port = id;
    memset(&tcp_listen_addr.sin_addr, 0, sizeof(tcp_listen_addr.sin_addr));
    memset(&tcp_listen_addr.sin_zero, 0, sizeof(tcp_listen_addr.sin_zero));

    s = socket(MZ_PF_INET, SOCK_STREAM, PROTO_P_PROTO);
    if (s != INVALID_SOCKET) {
#ifdef USE_WINSOCK_TCP
      unsigned long ioarg = 1;
      ioctlsocket(s, FIONBIO, &ioarg);
#else
      fcntl(s, F_SETFL, TCP_NONBLOCKING);
#endif
      if (!bind(s, (struct sockaddr *)&tcp_listen_addr, sizeof(tcp_listen_addr)))
	if (!listen(s, backlog)) {
	  listener_t *l;

	  l = MALLOC_ONE_TAGGED(listener_t);
	  l->type = scheme_listener_type;
	  l->s = s;
	  {
	    Scheme_Manager_Reference *mref;
	    mref = scheme_add_managed(NULL,
				      (Scheme_Object *)l,
				      (Scheme_Close_Manager_Client *)stop_listener,
				      NULL,
				      1);
	    l->mref = mref;
	  }

	  scheme_file_open_count++;
	  REGISTER_SOCKET(s);

	  return (Scheme_Object *)l;
	}

      closesocket(s);
    }
  }
  errid = errno;
#endif

#ifdef USE_TCP
  scheme_raise_exn(MZEXN_I_O_TCP,
		   "tcp-listen: listen on %d failed (%d)",
		   origid, errid);
#else
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "tcp-listen: not supported on this platform");
#endif

  return NULL;
}

#ifdef USE_TCP
static int stop_listener(Scheme_Object *o)
{
  int was_closed = 0;

#ifdef USE_MAC_TCP
  { 
    listener_t *l = (listener_t *)o;
    int i, count = l->count;
    Scheme_Tcp **datas = l->datas;
    if (!datas || !l->count)
      was_closed = 1;
    else {
      l->count = 0;
      for (i = 0; i < count; i++)
	if (datas[i])
	  mac_tcp_close(datas[i]);
    }
 }
#endif

#ifdef USE_SOCKETS_TCP
  {
    tcp_t s = ((listener_t *)o)->s;
    if (s == INVALID_SOCKET)
      was_closed = 1;
    else {
      UNREGISTER_SOCKET(s);
      closesocket(s);
      ((listener_t *)o)->s = INVALID_SOCKET;
      --scheme_file_open_count;
    }
  }
#endif

  return was_closed;
}
#endif

static Scheme_Object *
tcp_stop(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  int was_closed;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_type("tcp-close", "tcp-listener", 0, argc, argv);

  TCP_INIT("tcp-close");

  was_closed = stop_listener(argv[0]);

  if (was_closed) {
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "tcp-close: listener was already closed");
    return NULL;
  }

  return scheme_void;
#else
  scheme_wrong_type("tcp-close", "tcp-listener", 0, argc, argv);
  return NULL;
#endif
}

static Scheme_Object *
tcp_accept_ready(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  int ready;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_type("tcp-accept-rady?", "tcp-listener", 0, argc, argv);

  TCP_INIT("tcp-accept-ready?");

  if (LISTENER_WAS_CLOSED(argv[0])) {
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "tcp-accept-ready?: listener is closed");
    return NULL;
  }

  ready = tcp_check_accept(argv[0]);

  return (ready ? scheme_true : scheme_false);
#else
  scheme_wrong_type("tcp-accept-rady?", "tcp-listener", 0, argc, argv);
  return NULL;
#endif
}

static Scheme_Object *
tcp_accept(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  int was_closed = 0, errid;
  Scheme_Object *listener;
# ifdef USE_SOCKETS_TCP
  tcp_t s;
  int l;
  tcp_address tcp_accept_addr; /* Use a long name for precise GC's xform.ss */
# endif
# ifdef USE_MAC_TCP
  listener_t *l;
  int i, count;
  Scheme_Tcp **datas;
# endif

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_type("tcp-accept", "tcp-listener", 0, argc, argv);

  TCP_INIT("tcp-accept?");

  listener = argv[0];

  was_closed = LISTENER_WAS_CLOSED(listener);

  if (!was_closed) {
    if (!tcp_check_accept(listener)) {
      scheme_current_process->block_descriptor = -1;
      scheme_current_process->blocker = listener;
      scheme_current_process->block_check = tcp_check_accept;
      scheme_current_process->block_needs_wakeup = tcp_accept_needs_wakeup;
      do {
	scheme_process_block((float)0.0);
      } while (!tcp_check_accept(listener));
      scheme_current_process->block_descriptor = NOT_BLOCKED;
      scheme_current_process->blocker = NULL;
      scheme_current_process->ran_some = 1;
    }
    was_closed = LISTENER_WAS_CLOSED(listener);
  }

  if (was_closed) {
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "tcp-accept: listener is closed");
    return NULL;
  }
  
# ifdef USE_SOCKETS_TCP
  s = ((listener_t *)listener)->s;

  l = sizeof(tcp_accept_addr);
  s = accept(s, (struct sockaddr *)&tcp_accept_addr, &l);
  if (s != INVALID_SOCKET) {
    Scheme_Object *v[2];
    Scheme_Tcp *tcp;
    
#  ifdef USE_UNIX_SOCKETS_TCP
    int size = TCP_SOCKSENDBUF_SIZE;
#   ifndef CANT_SET_SOCKET_BUFSIZE
    setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char *)&size, sizeof(int));
#   endif
#  endif

    tcp = make_tcp_port_data(s, 2);

    v[0] = make_named_tcp_input_port(tcp, "TCP");
    v[1] = make_tcp_output_port(tcp);

    scheme_file_open_count++;
    REGISTER_SOCKET(s);
    
    return scheme_values(2, v);
  }
  errid = errno;
# endif

# ifdef USE_MAC_TCP
  l = (listener_t *)listener;
  count = l->count;
  datas = l->datas;

  errid = 0;
  for (i = 0; i < count; i++) {
    if (datas[i] && (datas[i]->tcp.state != SOCK_STATE_LISTENING)) {
      Scheme_Object *v[2];
      Scheme_Tcp *data;
      
      v[0] = make_named_tcp_input_port(datas[i], "TCP");
      v[1] = make_tcp_output_port(datas[i]);
      
      if (!(errid = mac_tcp_listen(l->portid, &data))) {
        /* new listener at the end of the queue: */
	memcpy(datas + i, datas + i + 1, sizeof(Scheme_Tcp *) * (count - i - 1));
	datas[count - 1] = data;

	scheme_file_open_count++;
      } else {
      	/* catastophic error; we permanently decrement the listener count */
        datas[i] = NULL;
      }
      return scheme_values(2, v);
    }
  }
# endif

  scheme_raise_exn(MZEXN_I_O_TCP,
		   "tcp-accept: accept from listener failed (%d)", errid);
#else
  scheme_wrong_type("tcp-accept", "tcp-listener", 0, argc, argv);
#endif

  return NULL;
}

static Scheme_Object *tcp_listener_p(int argc, Scheme_Object *argv[])
{
   return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type)
	   ? scheme_true
	   : scheme_false);
}

#endif /* !NO_TCP_SUPPORT */

#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
typedef struct 
{
  MZTAG_IF_REQUIRED
  fd_set *rd, *wr, *ex;
} Tcp_Select_Info;

static long select_for_tcp(void *data)
{
  Tcp_Select_Info *info = (Tcp_Select_Info *)data;
  
  select(0, info->rd, info->wr, info->ex, NULL);  

  return 0;
}
#endif

#ifdef USE_BEOS_PORT_THREADS
# define WAIT_OBJECT_0 1

sem_id siggo, go;
int32 done;
int32 enable_sig_count;
sem_id finished_first;

static void check_enable_signals()
{
  signal(SIGINT, SIG_IGN);

  if (atomic_add(&enable_sig_count, -1) == 1) {
    /* We're the last child; re-enable signals */
    release_sem(siggo);
  }
}

static long wait_one_sema(void *s)
{
  int v;

  check_enable_signals();

  while (acquire_sem((sem_id)s) != B_NO_ERROR) {}

  v = atomic_add(&done, 1);
  if (!v)
    finished_first = (sem_id)s;

  release_sem(go);

  return 0;
}

static long wait_timeout(void *t)
{
  int v;

  check_enable_signals();

  snooze((bigtime_t)(long)t);

  v = atomic_add(&done, 1);
  if (!v)
    finished_first = -1;

  release_sem(go);

  return 0;
}

static int wait_multiple_sema(int count, sem_id *a, float timeout)
{
  int i, got = 0;
  thread_id tot;
  sigset_t sigs;

  go = MAKE_SEMAPHORE();
  siggo = MAKE_SEMAPHORE();
  done = 0;
  finished_first = 0;

  if (!got_started)
    got_started = create_sem(0, NULL);

  /* Disable SIGINT until the children start ignoring it */
  sigemptyset(&sigs);
  sigaddset(&sigs, SIGINT);
  sigprocmask(SIG_BLOCK, &sigs, NULL);

  enable_sig_count = count + (timeout ? 1 : 0);

  for (i = 0; i < count; i++)
    resume_thread(spawn_thread(wait_one_sema, "multi-waiter", 
			       B_NORMAL_PRIORITY, (void *)a[i]));
  
  if (timeout) {
    tot = spawn_thread(wait_timeout, "multi-waiter timeout",
		       B_NORMAL_PRIORITY, 
		       (void *)(long)(timeout * 1000000));
    resume_thread(tot);
  } else
    tot = 0;

  while (acquire_sem(siggo) != B_NO_ERROR) {}

  sigprocmask(SIG_UNBLOCK, &sigs, NULL);

  FREE_SEMAPHORE(siggo);

  /* Might be interrupted by a signal: */
  acquire_sem(go);

  /* Post to still-waiting semaphores. This dislodges the waiting 
     threads. They won't change finished_first. */
  for (i = 0; i < count; i++) {
    if (a[i] != finished_first)
      release_sem(a[i]);
    else
      got = i + WAIT_OBJECT_0;
  }

  /* Wait for everybody */
  for (i = 0; i < count; i++)
    if (a[i] != finished_first)
      while (acquire_sem(go) != B_NO_ERROR) {}

  if (tot && (finished_first != -1)) {
    /* Wait for timeout, too. */
    /* Be Book says to suspend & resume to wake up a snoozer */
    suspend_thread(tot);
    /* Be Book says snooze here - why?! */ snooze(1000);
    resume_thread(tot);
    while (acquire_sem(go) != B_NO_ERROR) {}
  }

  FREE_SEMAPHORE(go);

  return got;
}
#endif

#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
static void clean_up_wait(long result, OS_SEMAPHORE_TYPE *array, 
			  int *rps, int count)
{
  if ((result >= (long)WAIT_OBJECT_0) && (result < (long)WAIT_OBJECT_0 + count)) {
    result -= WAIT_OBJECT_0;
    if (rps[result])
      RELEASE_SEMAPHORE(array[result]);
  }
  
  /* Clear out break semaphore */
  TRY_WAIT_SEMAPHORE(scheme_break_semaphore);
}
#endif

static void default_sleep(float v, void *fds)
{
#ifdef USE_OSKIT_CONSOLE
  /* Don't really sleep; keep polling the keyboard: */
  if (!v || (v > 0.01))
    v = 0.01;
#endif

  if (!fds) {
#ifndef NO_SLEEP
# ifdef USE_BEOS_SNOOZE
    snooze((bigtime_t)(v * 1000000));
# else
#  ifndef NO_USLEEP
    usleep((unsigned)(v * 1000));
#  else
    sleep(v);
#  endif
# endif
#endif
  } else {
#if defined(FILES_HAVE_FDS) || defined(USE_WINSOCK_TCP) || defined(USE_BEOS_PORT_THREADS)
    int limit;
    fd_set *rd, *wr, *ex;
    struct timeval time;

#ifdef SIGCHILD_DOESNT_INTERRUPT_SELECT
    if (scheme_system_children) {
      /* Better poll every second or so... */
      if (!v || (v > 1))
	v = 1;
    }
#endif

    time.tv_sec = (long)v;
    time.tv_usec = (long)(fmod(v, 1.0) * 1000000);

# ifdef USE_WINSOCK_TCP
    limit = 0;
# else
#  ifdef USE_ULIMIT
    limit = ulimit(4, 0);
#  else
#   ifdef FIXED_FD_LIMIT
      limit = FIXED_FD_LIMIT;
#   else
      limit = getdtablesize();
#   endif
#  endif    
#endif

    rd = (fd_set *)fds;
    wr = (fd_set *)MZ_GET_FDSET(fds, 1);
    ex = (fd_set *)MZ_GET_FDSET(fds, 2);

#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
    {
      long result;
      OS_SEMAPHORE_TYPE *array, just_two_array[2], break_sema;
      int count, *rps, just_two_rps[2];
      int fd_added;

      fd_added = (((win_extended_fd_set *)rd)->added
		  || ((win_extended_fd_set *)wr)->added
		  || ((win_extended_fd_set *)ex)->added);
      count = ((win_extended_fd_set *)fds)->num_handles;
      array = ((win_extended_fd_set *)fds)->handles;
      rps = ((win_extended_fd_set *)fds)->repost_sema;
      
      /* add break semaphore: */
      if (!count) {
	array = just_two_array;
	rps = just_two_rps;
      }
      rps[count] = 0;
# ifdef WIN32_THREADS
      break_sema = (HANDLE)scheme_win32_get_break_semaphore(scheme_current_process->thread);
# else
      break_sema = scheme_break_semaphore;
# endif
      array[count++] = break_sema;

      if (count && !fd_added) {
	/* Simple: just wait for HANDLE-based input: */
#if defined(WIN32_FD_HANDLES)
	/* Extensions may handle events */
	result = MsgWaitForMultipleObjects(count, array, FALSE, 
					   v ? (DWORD)(v * 1000) : INFINITE,
					   ((win_extended_fd_set *)fds)->wait_event_mask);
#endif
#if defined(USE_BEOS_PORT_THREADS)
	result = wait_multiple_sema(count, array, v);
#endif
	clean_up_wait(result, array, rps, count);
	return;
      } else if (count) {
	/* What a mess! We must wait for either HANDLE-based input or TCP
	   status. Use a thread to select() for TCP status, and then
	   hit a semaphore if the status changes. Meanwhile, in this
	   thread, wait on both the console input and the semaphore.
	   When either happens, kill the thread. */
	OS_THREAD_TYPE th;
	Tcp_Select_Info *info;
	tcp_t fake;
#if defined(WIN32_FD_HANDLES)
	struct Scheme_Thread_Memory *thread_memory;
#endif

	info = MALLOC_ONE_RT(Tcp_Select_Info);
#ifdef MZTAG_REQUIRED
	info->type = scheme_rt_tcp_select_info;
#endif

	fake = socket(MZ_PF_INET, SOCK_STREAM, 0);
	FD_SET(fake, ex);

	info->rd = rd;
	info->wr = wr;
	info->ex = ex;

#if defined(WIN32_FD_HANDLES)
	{
	  DWORD id;
	  th = CreateThread(NULL, 5000, 
			    (LPTHREAD_START_ROUTINE)select_for_tcp,
			    info, 0, &id);
	  /* Not actually necessary, since GC can't occur during the
	     thread's life, but better safe than sorry if we change the
	     code later. */
	  thread_memory = scheme_remember_thread((void *)th);
	}
#endif	
#if defined(USE_BEOS_PORT_THREADS)
	th = spawn_thread(select_for_tcp, "socket waiter",
			  B_NORMAL_PRIORITY, info);
	resume_thread(th);
#endif

	rps[count] = 0;
	array[count++] = th;

#if defined(WIN32_FD_HANDLES)
	result = MsgWaitForMultipleObjects(count, array, FALSE, 
					   v ? (DWORD)(v * 1000) : INFINITE,
					   ((win_extended_fd_set *)fds)->wait_event_mask),
#endif	
#if defined(USE_BEOS_PORT_THREADS)
	result = wait_multiple_sema(count, array, v);
#endif
	clean_up_wait(result, array, rps, count);

	closesocket(fake); /* cause selector thread to end */

#if defined(WIN32_FD_HANDLES)
	WaitForSingleObject(th, INFINITE);
	scheme_forget_thread(thread_memory);
	CloseHandle(th);
#endif
	
	return;
      }
    }
#endif

#ifdef USE_WINSOCK_TCP
    /* Stupid Windows: give select() empty fd_sets and it ignores the timeout. */
    if (!rd->fd_count && !wr->fd_count && !ex->fd_count) {
      if (v)
        Sleep((DWORD)(v * 1000));
      return;
    }
#endif

    select(limit, rd, wr, ex, v ? &time : NULL);
#endif
  }
}

#ifdef MEMORY_COUNTING_ON
void scheme_count_input_port(Scheme_Object *port, long *s, long *e, 
			     Scheme_Hash_Table *ht)
{
  Scheme_Input_Port *ip = (Scheme_Input_Port *)port;

  *e = scheme_count_memory(ip->read_handler, ht);
  *s = sizeof(Scheme_Input_Port);

  if (ip->sub_type == file_input_port_type)
    *s += sizeof(Scheme_Input_File);    
  else if (ip->sub_type == string_input_port_type) {
    Scheme_Indexed_String *is;
    is = (Scheme_Indexed_String *)ip->port_data;
    *s += (sizeof(Scheme_Indexed_String)
	   + is->size);
  } else if (ip->sub_type == tcp_input_port_type) {
    if (ht && !scheme_lookup_in_table(ht, (const char *)ip->port_data)) {
      scheme_add_to_table(ht, (const char *)ip->port_data, scheme_true, 0);
      *s += sizeof(Scheme_Tcp);
    }
  } else if (ip->sub_type == user_input_port_type) {
    Scheme_Object **d;
    d = (Scheme_Object **)ip->port_data;
    *s += (3 * sizeof(Scheme_Object *));
    *e += (scheme_count_memory(d[0], ht)
	   + scheme_count_memory(d[1], ht)
	   + scheme_count_memory(d[2], ht));
  } else if (ip->sub_type == pipe_read_port_type) {
    if (ht && !scheme_lookup_in_table(ht, (const char *)ip->port_data)) {
      Scheme_Pipe *p = (Scheme_Pipe *)ip->port_data;
      scheme_add_to_table(ht, (const char *)ip->port_data, scheme_true, 0);
      *s += (sizeof(Scheme_Pipe) + p->buflen);
    }
  }
}

void scheme_count_output_port(Scheme_Object *port, long *s, long *e, 
			     Scheme_Hash_Table *ht)
{
  Scheme_Output_Port *op = (Scheme_Output_Port *)port;

  *e = 0;
  *s = sizeof(Scheme_Output_Port);

  if (op->sub_type == file_output_port_type)
    *s += sizeof(Scheme_Output_File);    
  else if (op->sub_type == string_output_port_type) {
    Scheme_Indexed_String *is;
    is = (Scheme_Indexed_String *)op->port_data;
    *s += (sizeof(Scheme_Indexed_String)
	   + is->size);
  } else if (op->sub_type == tcp_output_port_type) {
    if (!scheme_lookup_in_table(ht, (const char *)op->port_data)) {
      scheme_add_to_table(ht, (const char *)op->port_data, scheme_true, 0);
      *s += sizeof(Scheme_Tcp);
    }
  } else if (op->sub_type == user_output_port_type) {
    Scheme_Object **d;
    d = (Scheme_Object **)op->port_data;
    *s += (2 * sizeof(Scheme_Object *));
    *e += (scheme_count_memory(d[0], ht)
	   + scheme_count_memory(d[1], ht));
  } else if (op->sub_type == pipe_read_port_type) {
    if (!scheme_lookup_in_table(ht, (const char *)op->port_data)) {
      Scheme_Pipe *p = (Scheme_Pipe *)op->port_data;
      scheme_add_to_table(ht, (const char *)op->port_data, scheme_true, 0);
      *s += (sizeof(Scheme_Pipe) + p->buflen);
    }
  }
}
#endif

/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

static int mark_listener(void *p, Mark_Proc mark)
{
  if (mark) {
    listener_t *l = (listener_t *)p;

    gcMARK(l->mref);
#ifdef USE_MAC_TCP
    gcMARK(l->datas);
#endif
  }

  return gcBYTES_TO_WORDS(sizeof(listener_t));
}

#ifdef WINDOWS_PROCESSES
static int mark_thread_memory(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Thread_Memory *tm = (Scheme_Thread_Memory *)p;
    gcMARK(tm->prev);
    gcMARK(tm->next);
  }

  return gcBYTES_TO_WORDS(sizeof(Scheme_Thread_Memory));
}
#endif

static int mark_input_file(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Input_File *i = (Scheme_Input_File *)p;

    gcMARK(i->f);
  }

  return gcBYTES_TO_WORDS(sizeof(Scheme_Input_File));
}

#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
static int mark_tested_input_file(void *p, Mark_Proc mark)
{
  if (mark) {
    Tested_Input_File *tip = (Tested_Input_File *)p;

    gcMARK(tip->fp);
#ifdef WIN32_FD_HANDLES
    gcMARK(tip->thread_memory);
#endif
  }

  return gcBYTES_TO_WORDS(sizeof(Tested_Input_File));
}

static int mark_tcp_select_info(void *p, Mark_Proc mark)
{
  if (mark) {
  }

  return gcBYTES_TO_WORDS(sizeof(Tcp_Select_Info));
}
#endif

static int mark_indexed_string(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Indexed_String *is = (Scheme_Indexed_String *)p;
    
    gcMARK(is->string);
  }

  return gcBYTES_TO_WORDS(sizeof(Scheme_Indexed_String));
}

static int mark_output_file(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Output_File *o = (Scheme_Output_File *)p;

    gcMARK(o->f);
  }

  return gcBYTES_TO_WORDS(sizeof(Scheme_Output_File));
}

static int mark_load_handler_data(void *p, Mark_Proc mark)
{
  if (mark) {
    LoadHandlerData *d = (LoadHandlerData *)p;
    
    gcMARK(d->config);
    gcMARK(d->port);
    gcMARK(d->p);
  }

  return gcBYTES_TO_WORDS(sizeof(LoadHandlerData));
}

static int mark_load_data(void *p, Mark_Proc mark)
{
  if (mark) {
    LoadData *d = (LoadData *)p;

    gcMARK(d->filename);
    gcMARK(d->config);
    gcMARK(d->load_dir);
    gcMARK(d->old_load_dir);
  }

  return gcBYTES_TO_WORDS(sizeof(LoadData));
}

static int mark_pipe(void *_p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Pipe *p = (Scheme_Pipe *)_p;
    
    gcMARK(p->buf);
#ifdef MZ_REAL_THREADS
    gcMARK(p->wait_sem);
#endif
  }

  return gcBYTES_TO_WORDS(sizeof(Scheme_Pipe));
}

#ifdef USE_TCP
static int mark_tcp(void *p, Mark_Proc mark)
{
  if (mark) {
# ifdef USE_MAC_TCP
    Scheme_Tcp *tcp = (Scheme_Tcp *)p;

    gcMARK(tcp->tcp);
    gcMARK(tcp->activeRcv);
# endif
  }

  return gcBYTES_TO_WORDS(sizeof(Scheme_Tcp));
}

# ifdef USE_MAC_TCP
static int mark_write_data(void *p, Mark_Proc mark)
{
  if (mark) {
    WriteData *d = (WriteData *)p;
    
    gcMARK(d->xpb);
  }

  return gcBYTES_TO_WORDS(sizeof(WriteData));
}
# endif
#endif

#ifdef USE_FD_PORTS
static int mark_input_fd(void *p, Mark_Proc mark)
{
  if (mark) {
  }

  return gcBYTES_TO_WORDS(sizeof(Scheme_FD));
}
#endif

#if defined(UNIX_PROCESSES)
static int mark_system_child(void *p, Mark_Proc mark)
{
  if (mark) {
    System_Child *sc = (System_Child *)p;

    gcMARK(sc->next);
  }

  return gcBYTES_TO_WORDS(sizeof(System_Child));
}
#endif

#ifdef BEOS_PROCESSES
static int mark_beos_process(void *p, Mark_Proc mark)
{
  if (mark) {
  }

  return gcBYTES_TO_WORDS(sizeof(BeOSProcess));
}
#endif

#ifdef USE_OSKIT_CONSOLE
static int mark_oskit_console_input(void *p, Mark_Proc mark)
{
  if (mark) {
    osk_console_input *c = (osk_console_input *)p;
    
    gcMARK(c->buffer);
    gcMARK(c->next);
  }

  return gcBYTES_TO_WORDS(sizeof(osk_console_input));
}
#endif

static void register_traversers(void)
{
  GC_register_traverser(scheme_listener_type, mark_listener);  
#ifdef WINDOWS_PROCESSES
  GC_register_traverser(scheme_rt_thread_memory, mark_thread_memory);
#endif
  GC_register_traverser(scheme_rt_input_file, mark_input_file);
#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
  GC_register_traverser(scheme_rt_tested_input_file, mark_tested_input_file);
  GC_register_traverser(scheme_rt_tcp_select_info, mark_tcp_select_info);
#endif
  GC_register_traverser(scheme_rt_indexed_string, mark_indexed_string);
  GC_register_traverser(scheme_rt_output_file, mark_output_file);
  GC_register_traverser(scheme_rt_load_handler_data, mark_load_handler_data);
  GC_register_traverser(scheme_rt_load_data, mark_load_data);
  GC_register_traverser(scheme_rt_pipe, mark_pipe);
#ifdef USE_TCP
  GC_register_traverser(scheme_rt_tcp, mark_tcp);
# ifdef USE_MAC_TCP
  GC_register_traverser(scheme_rt_write_data, mark_write_data);
# endif
#endif

#ifdef USE_FD_PORTS
  GC_register_traverser(scheme_rt_input_fd, mark_input_fd);
#endif

#if defined(UNIX_PROCESSES)
  GC_register_traverser(scheme_rt_system_child, mark_system_child);
#endif

#ifdef BEOS_PROCESSES
  GC_register_traverser(scheme_rt_beos_process, mark_beos_process);
#endif

#ifdef USE_OSKIT_CONSOLE
  GC_register_traverser(scheme_rt_oskit_console_input, mark_oskit_console_input);
#endif
}

END_XFORM_SKIP;

#endif
