/*
  MzScheme
  Copyright (c) 2000 Matthew Flatt

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

/* This file implements the least platform-specific aspects of MzScheme
   port types. */

#include "schpriv.h"

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
static Scheme_Object *sch_read_string (int, Scheme_Object *[]);
static Scheme_Object *read_string_bang (int, Scheme_Object *[]);
static Scheme_Object *read_string_bang_break (int, Scheme_Object *[]);
static Scheme_Object *write_string_avail_break(int argc, Scheme_Object *argv[]);
static Scheme_Object *peek_char (int, Scheme_Object *[]);
static Scheme_Object *eof_object_p (int, Scheme_Object *[]);
static Scheme_Object *char_ready_p (int, Scheme_Object *[]);
static Scheme_Object *sch_write (int, Scheme_Object *[]);
static Scheme_Object *display (int, Scheme_Object *[]);
static Scheme_Object *sch_print (int, Scheme_Object *[]);
static Scheme_Object *newline (int, Scheme_Object *[]);
static Scheme_Object *write_char (int, Scheme_Object *[]);
static Scheme_Object *load (int, Scheme_Object *[]);
static Scheme_Object *current_load (int, Scheme_Object *[]);
static Scheme_Object *current_load_directory(int argc, Scheme_Object *argv[]);
static Scheme_Object *default_load (int, Scheme_Object *[]);
static Scheme_Object *use_compiled_kind(int, Scheme_Object *[]);
static Scheme_Object *transcript_on(int, Scheme_Object *[]);
static Scheme_Object *transcript_off(int, Scheme_Object *[]);
static Scheme_Object *flush_output (int, Scheme_Object *[]);
static Scheme_Object *open_input_string (int, Scheme_Object *[]);
static Scheme_Object *open_output_string (int, Scheme_Object *[]);
static Scheme_Object *get_output_string (int, Scheme_Object *[]);
static Scheme_Object *sch_pipe(int, Scheme_Object **args);
static Scheme_Object *port_read_handler(int, Scheme_Object **args);
static Scheme_Object *port_display_handler(int, Scheme_Object **args);
static Scheme_Object *port_write_handler(int, Scheme_Object **args);
static Scheme_Object *port_print_handler(int, Scheme_Object **args);
static Scheme_Object *global_port_print_handler(int, Scheme_Object **args);

static Scheme_Object *sch_default_read_handler(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_display_handler(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_write_handler(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_print_handler(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_global_port_print_handler(int argc, Scheme_Object *argv[]);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

static Scheme_Object *any_symbol, *any_one_symbol;
static Scheme_Object *cr_symbol, *lf_symbol, *crlf_symbol;

static Scheme_Object *all_symbol, *non_elaboration_symbol, *none_symbol;

static Scheme_Object *default_read_handler;
static Scheme_Object *default_display_handler;
static Scheme_Object *default_write_handler;
static Scheme_Object *default_print_handler;

Scheme_Object *scheme_write_proc, *scheme_display_proc, *scheme_print_proc;

#define fail_err_symbol scheme_false

void 
scheme_init_port_fun(Scheme_Env *env)
{
  if (scheme_starting_up) {    
#ifdef MZ_PRECISE_GC
    register_traversers();
#endif

    REGISTER_SO(default_read_handler);
    REGISTER_SO(default_display_handler);
    REGISTER_SO(default_write_handler);
    REGISTER_SO(default_print_handler);

    REGISTER_SO(scheme_write_proc);
    REGISTER_SO(scheme_display_proc);
    REGISTER_SO(scheme_print_proc);

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

    scheme_write_proc = scheme_make_prim_w_arity(sch_write, 
						 "write", 
						 1, 2);
    scheme_display_proc = scheme_make_prim_w_arity(display, 
						   "display", 
						   1, 2);
    scheme_print_proc = scheme_make_prim_w_arity(sch_print, 
						 "print", 
						 1, 2);
    
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

    scheme_init_port_fun_config();
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
  
  scheme_add_global_constant("file-stream-port?", 
			     scheme_make_folding_prim(scheme_file_stream_port_p, 
						      "file-stream-port?", 
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
			     scheme_make_prim_w_arity(sch_read_string, 
						      "read-string", 
						      1, 2), 
			     env);
  scheme_add_global_constant("read-string-avail!", 
			     scheme_make_prim_w_arity(read_string_bang, 
						      "read-string-avail!", 
						      1, 4), 
			     env);
  scheme_add_global_constant("read-string-avail!/enable-break", 
			     scheme_make_prim_w_arity(read_string_bang_break, 
						      "read-string-avail!/enable-break", 
						      1, 4), 
			     env);
  scheme_add_global_constant("write-string-avail", 
			     scheme_make_prim_w_arity(scheme_write_string_avail, 
						      "write-string-avail", 
						      1, 4),
			     env);
  scheme_add_global_constant("write-string-avail/enable-break",
			     scheme_make_prim_w_arity(write_string_avail_break, 
						      "write-string-avail/enable-break", 
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
			     scheme_make_prim_w_arity(scheme_file_position, 
						      "file-position", 
						      1, 2), 
			     env);
  
  scheme_add_global_constant("make-pipe", 
			     scheme_make_prim_w_arity2(sch_pipe, 
						       "make-pipe", 
						       0, 0,
						       2, 2), 
			     env);
}


void scheme_init_port_fun_config(void)
{
  Scheme_Config *config = scheme_config;
  scheme_set_param(config, MZCONFIG_LOAD_DIRECTORY, scheme_false);
  scheme_set_param(config, MZCONFIG_USE_COMPILED_KIND, all_symbol);

  {
    Scheme_Object *dlh;
    dlh = scheme_make_prim_w_arity2(default_load,
				    "default-load-handler",
				    1, 1,
				    0, -1);
    scheme_set_param(config, MZCONFIG_LOAD_HANDLER, dlh);
  }

  {
    Scheme_Object *gpph;
    gpph = scheme_make_prim_w_arity(sch_default_global_port_print_handler,
				    "default-global-port-print-handler",
				    2, 2);
    scheme_set_param(config, MZCONFIG_PORT_PRINT_HANDLER, gpph);
  }
}

/*========================================================================*/
/*                          string input ports                            */
/*========================================================================*/

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

  ip = _scheme_make_input_port(scheme_string_input_port_type,
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

/*========================================================================*/
/*                          string output ports                           */
/*========================================================================*/

static void
string_write_string(char *str, long d, long len, Scheme_Output_Port *port)
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
  
  memcpy(is->string + is->index, str + d, len);
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

  op = scheme_make_output_port (scheme_string_output_port_type,
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
  if (op->sub_type != scheme_string_output_port_type)
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

/*========================================================================*/
/*                 "user" input ports (created from Scheme)               */
/*========================================================================*/

static int 
user_getc (Scheme_Input_Port *port)
{
  Scheme_Object *fun, *val;

  fun = ((Scheme_Object **) port->port_data)[0];

  val = _scheme_apply(fun, 0, NULL);

  if (SCHEME_EOFP(val))
    return EOF;
  else {
    if (!SCHEME_CHARP(val)) {
      if (scheme_return_eof_for_error()) {
	return EOF;
      } else {
	scheme_raise_exn(MZEXN_I_O_PORT_USER,
			 port,
			 "port: user read-char returned a non-character");
      }
    }
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

/*========================================================================*/
/*                 "user" output ports (created from Scheme)              */
/*========================================================================*/

static void
user_write(char *str, long d, long len, Scheme_Output_Port *port)
{
  Scheme_Object *fun, *p[1];
  
  p[0] = scheme_make_sized_offset_string(str, d, len, 1);

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

/*========================================================================*/
/*                               pipe ports                               */
/*========================================================================*/

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

static void pipe_write(char *str, long d, long len, Scheme_Output_Port *p)
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
    memcpy(pipe->buf + firstpos, str + d, firstn);
  if (secondn)
    memcpy(pipe->buf, str + d + firstn, secondn);

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

  readp = _scheme_make_input_port(scheme_pipe_read_port_type,
				  (void *)pipe,
				  pipe_getc,
				  NULL,
				  pipe_char_ready,
				  pipe_in_close,
				  NULL,
				  0);

  readp->name = "PIPE";

  writep = scheme_make_output_port(scheme_pipe_write_port_type,
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

/*========================================================================*/
/*                    Scheme functions and helpers                        */
/*========================================================================*/

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

  ip = _scheme_make_input_port(scheme_user_input_port_type,
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

  op = scheme_make_output_port(scheme_user_output_port_type,
			       copy,
			       user_write,
			       user_close_output,
			       0);

  return (Scheme_Object *)op;
}

static Scheme_Object *
open_input_file (int argc, Scheme_Object *argv[])
{
  return scheme_do_open_input_file("open-input-file", 0, argc, argv);
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
open_output_file (int argc, Scheme_Object *argv[])
{
  return scheme_do_open_output_file("open-output-file", 0, argc, argv);
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
      || (op->sub_type != scheme_string_output_port_type))
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

  port = scheme_do_open_output_file("call-with-output-file", 1, argc, argv);
  
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

  port = scheme_do_open_input_file("call-with-input-file", 1, argc, argv);
  
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
  scheme_close_output_port(SCHEME_CAR(SCHEME_CDR((Scheme_Object *)d)));
}

static Scheme_Object *
with_output_to_file (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *v;

  scheme_check_proc_arity("with-output-to-file", 0, 1, argc, argv);

  port = scheme_do_open_output_file("with-output-to-file", 1, argc, argv);
  
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
  scheme_close_input_port(SCHEME_CAR(SCHEME_CDR((Scheme_Object *)d)));
}

static Scheme_Object *
with_input_from_file(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *v;

  scheme_check_proc_arity("with-input-from-file", 0, 1, argc, argv);

  port = scheme_do_open_input_file("with-input-from-file", 1, argc, argv);
  
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
    scheme_flush_orig_outputs();

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
      scheme_flush_orig_outputs();

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
    scheme_flush_orig_outputs();

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
sch_read_string(int argc, Scheme_Object *argv[])
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
    scheme_flush_orig_outputs();
  
  if (!size)
    return scheme_make_sized_string("", 0, 0);

  str = scheme_alloc_string(size, 0);

  got = scheme_get_chars(port, size, SCHEME_STR_VAL(str), 0);

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

  if (!SCHEME_MUTABLE_STRINGP(argv[0])) {
    scheme_wrong_type("read-string-avail!", "mutable-string", 0, argc, argv);
    return NULL;
  } else
    str = argv[0];
  if ((argc > 1) && !SCHEME_INPORTP(argv[1]))
    scheme_wrong_type("read-string-avail!", "input-port", 1, argc, argv);
  
  scheme_get_substring_indices("read-string-avail!", str, 
			       argc, argv,
			       2, 3, &start, &finish);

  size = finish - start;

  if (argc > 1)
    port = argv[1];
  else
    port = CURRENT_INPUT_PORT(scheme_config);

  if ((Scheme_Object *)port == scheme_orig_stdin_port)
    scheme_flush_orig_outputs();

  if (!size)
    return scheme_make_integer(0);

  got = scheme_get_chars(port, -size, SCHEME_STR_VAL(str), start);

  if (!got)
    return scheme_eof;

  return scheme_make_integer(got);
}

typedef struct {
  MZTAG_IF_REQUIRED
  int argc;
  Scheme_Object **argv;
  Scheme_Config *config;
  Scheme_Object *orig_param_val;
  Scheme_Prim *k;
} Breakable;

static void pre_breakable(void *data)
{
  Breakable *b = (Breakable *)data;

  b->orig_param_val = scheme_get_param(b->config, MZCONFIG_ENABLE_BREAK);
  scheme_set_param(b->config, MZCONFIG_ENABLE_BREAK, scheme_true);
}

static Scheme_Object *do_breakable(void *data)
{
  Breakable *b = (Breakable *)data;
  Scheme_Prim *k;

  /* Need to check for a break, in case one was queued and we just enabled it: */
  {
    Scheme_Process *p = scheme_current_process;
    if (p->external_break)
      if (scheme_can_break(p, p->config))
	scheme_process_block_w_process(0.0, p);
  }

  k = b->k;
  return k(b->argc, b->argv);
}

static void post_breakable(void *data)
{
  Breakable *b = (Breakable *)data;
  scheme_set_param(b->config, MZCONFIG_ENABLE_BREAK, b->orig_param_val);
}

static Scheme_Object *
read_string_bang_break(int argc, Scheme_Object *argv[])
{
  Breakable *b;

  b = MALLOC_ONE_RT(Breakable);
#ifdef MZTAG_REQUIRED
  b->type = scheme_rt_breakable;
#endif
  b->argc = argc;
  b->argv = argv;
  b->k = read_string_bang;
  b->config = scheme_current_process->config;

  return scheme_dynamic_wind(pre_breakable, 
			     do_breakable, 
			     post_breakable, 
			     NULL, b);
}

static Scheme_Object *
write_string_avail_break(int argc, Scheme_Object *argv[])
{
  Breakable *b;

  b = MALLOC_ONE_RT(Breakable);
#ifdef MZTAG_REQUIRED
  b->type = scheme_rt_breakable;
#endif
  b->argc = argc;
  b->argv = argv;
  b->k = scheme_write_string_avail;
  b->config = scheme_current_process->config;

  return scheme_dynamic_wind(pre_breakable, 
			     do_breakable, 
			     post_breakable, 
			     NULL, b);
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
	CHECK_PORT_CLOSED(name, "output", port, op->closed);
	{
	  Write_String_Fun f = op->write_string_fun;
	  f(SCHEME_STR_VAL(v), 0, len, op);
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
sch_print (int argc, Scheme_Object *argv[])
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
  
  scheme_write_offset_string("\n", 0, 1, port);

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
  CHECK_PORT_CLOSED("write-char", "output", port, op->closed);
  {
    Write_String_Fun f = op->write_string_fun;
    f((char *)buffer, 0, 1, op);
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
  int ch;
  Scheme_Process *p = scheme_current_process;
  Scheme_Config *config = p->config;
  LoadHandlerData *lhd;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("default-load-handler", "string", 0, argc, argv);

  port = scheme_do_open_input_file("default-load-handler", 0, 1, argv);

  scheme_count_lines(port);

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
		       scheme_intern_symbol("ill-formed-path"),
		       "current-load-relative-directory: not a complete path: \"%q\"",
		       s);

    expanded = scheme_expand_filename(s, len, "current-load-relative-directory", NULL);
    ed = scheme_make_immutable_sized_string(expanded, strlen(expanded), 1);
    if (!scheme_directory_exists(expanded)) {
      scheme_raise_exn(MZEXN_I_O_FILESYSTEM,
		       ed,
		       fail_err_symbol,
		       "current-load-relative-directory: directory not found or not a directory: \"%q\"",
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
  Scheme_Object * volatile val;

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
flush_output(int argc, Scheme_Object *argv[])
{
  Scheme_Object *op;

  if (argc && !SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("flush-output", "output-port", 0, argc, argv);

  if (argc) 
    op = argv[0];
  else
    op = CURRENT_OUTPUT_PORT(scheme_config);

  scheme_flush_output(op);

  return (scheme_void);
}


/*========================================================================*/
/*                       precise GC traversers                            */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_PORTFUN_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_breakable, mark_breakable);  
  GC_REG_TRAV(scheme_rt_indexed_string, mark_indexed_string);
  GC_REG_TRAV(scheme_rt_load_handler_data, mark_load_handler_data);
  GC_REG_TRAV(scheme_rt_load_data, mark_load_data);
  GC_REG_TRAV(scheme_rt_pipe, mark_pipe);
}

END_XFORM_SKIP;

#endif
