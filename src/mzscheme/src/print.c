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
#include "schmach.h"
#include "schcpt.h"
#include <ctype.h>
#ifdef USE_STACKAVAIL
#include <malloc.h>
#endif

/* Flag for debugging compiled code in printed form: */
#define NO_COMPACT 0

#define USE_BUFFERING_CPORT 1
/* Companion to USE_BUFFERING_CPORT in read.c */

#define PRINT_MAXLEN_MIN 3

/* locals */
#define MAX_PRINT_BUFFER 500

#define quick_print_struct quick_can_read_compiled
#define quick_print_graph quick_can_read_graph
#define quick_print_box quick_can_read_box
#define quick_print_vec_shorthand quick_square_brackets_are_parens
/* Don't use can_read_pipe_quote! */

static void print_to_port(char *name, Scheme_Object *obj, Scheme_Object *port, 
			  int escaped, long maxl, Scheme_Process *p,
			  Scheme_Config *config);
static int print(Scheme_Object *obj, int escaped, int compact, 
		 Scheme_Hash_Table *ht, Scheme_Hash_Table *vht, Scheme_Process *p);
static void print_string(Scheme_Object *string, int escaped, Scheme_Process *p);
static void print_pair(Scheme_Object *pair, int escaped, int compact, 
		       Scheme_Hash_Table *ht, Scheme_Hash_Table *vht, Scheme_Process *p);
static void print_vector(Scheme_Object *vec, int escaped, int compact, 
			 Scheme_Hash_Table *ht, Scheme_Hash_Table *vht, Scheme_Process *p);
static void print_char(Scheme_Object *chobj, int escaped, Scheme_Process *p);
static char *print_to_string(Scheme_Object *obj, long *len, int write,
			     Scheme_Object *port, long maxl,
			     Scheme_Process *p, Scheme_Config *config);

static Scheme_Object *quote_link_symbol = NULL;
static char *quick_buffer = NULL;

static char compacts[_CPT_COUNT_];

#define print_compact(p, v) print_this_string(p, &compacts[v], 1)

#define HAS_SUBSTRUCT(obj) \
   (SCHEME_PAIRP(obj) || SCHEME_VECTORP(obj) \
    || (p->quick_print_box && SCHEME_BOXP(obj)) \
    || (p->quick_print_struct  \
	&& SAME_TYPE(SCHEME_TYPE(obj), scheme_structure_type)))

void scheme_init_print(Scheme_Env *env)
{
  if (scheme_starting_up) {
    int i;

    REGISTER_SO(quick_buffer);

    quick_buffer = (char *)scheme_malloc_atomic(100);

    REGISTER_SO(quote_link_symbol);

    quote_link_symbol = scheme_intern_symbol("-q");

    for (i = 0; i < _CPT_COUNT_; i++) {
      compacts[i] = i;
    }
  }
}

Scheme_Object *scheme_make_svector(short c, short *a)
{
  Scheme_Object *o;
  o = scheme_alloc_object();

  o->type = scheme_svector_type;
  SCHEME_SVEC_LEN(o) = c;
  SCHEME_SVEC_VEC(o) = a;

  return o;
}

static void make_sym_vec_hash_indices(void *v, int *h1, int *h2)
{
  Scheme_Object *vec = (Scheme_Object *)v, **elems;
  int i, key1 = 0, key2 = 0;
  
  elems = SCHEME_VEC_ELS(vec);
  for (i = SCHEME_VEC_SIZE(vec); i--; ) {
    key1 += (int)elems[i];
    key2 += ((int)elems[i]) & 0x555;
  }

  *h1 = key1;
  *h2 = key2;
}

static int compare_sym_vec(void *v1, void *v2)
{
  Scheme_Object *vec1 = (Scheme_Object *)v1, **elems1;
  Scheme_Object *vec2 = (Scheme_Object *)v2, **elems2;
  int s;

  s = SCHEME_VEC_SIZE(vec1);
  if (s != SCHEME_VEC_SIZE(vec2))
    return 1;
  
  elems1 = SCHEME_VEC_ELS(vec1);
  elems2 = SCHEME_VEC_ELS(vec2);

  while (s--) {
    if (!SAME_OBJ(elems1[s], elems2[s]))
      return 1;
  }

  return 0;
}

void
scheme_debug_print (Scheme_Object *obj)
{
  scheme_write(obj, scheme_orig_stdout_port);
  fflush (stdout);
}

static void *print_to_port_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *obj, *port;

  port = (Scheme_Object *)p->ku.k.p1;
  obj = (Scheme_Object *)p->ku.k.p2;

  print_to_port(p->ku.k.i2 ? "write" : "display", 
		obj, port,
		p->ku.k.i2, p->ku.k.i1,
		p, p->config);

  return NULL;
}

static void do_handled_print(Scheme_Object *obj, Scheme_Object *port,
			     Scheme_Object *proc, long maxl)
{
  Scheme_Object *a[2];

  a[0] = obj;
  
  if (maxl > 0) {
    a[1] = scheme_make_string_output_port();
  } else
    a[1] = port;
  
  scheme_apply_multi(scheme_write_proc, 2, a);
  
  if (maxl > 0) {
    char *s;
    int len;

    s = scheme_get_sized_string_output(a[1], &len);
    if (len > maxl)
      len = maxl;

    scheme_write_string(s, len, port);
  }
}

void scheme_write_w_max(Scheme_Object *obj, Scheme_Object *port, long maxl)
{
  if (((Scheme_Output_Port *)port)->write_handler)
    do_handled_print(obj, port, scheme_write_proc, maxl);
  else {
    Scheme_Process *p = scheme_current_process;
    
    p->ku.k.p1 = port;
    p->ku.k.p2 = obj;
    p->ku.k.i1 = maxl;
    p->ku.k.i2 = 1;
    
    (void)scheme_top_level_do(print_to_port_k, 0);
  }
}

void scheme_write(Scheme_Object *obj, Scheme_Object *port)
{
  scheme_write_w_max(obj, port, -1);
}

void scheme_display_w_max(Scheme_Object *obj, Scheme_Object *port, long maxl)
{
  if (((Scheme_Output_Port *)port)->display_handler)
    do_handled_print(obj, port, scheme_display_proc, maxl);
  else {
    Scheme_Process *p = scheme_current_process;
    
    p->ku.k.p1 = port;
    p->ku.k.p2 = obj;
    p->ku.k.i1 = maxl;
    p->ku.k.i2 = 0;
    
    (void)scheme_top_level_do(print_to_port_k, 0);
  }
}

void scheme_display(Scheme_Object *obj, Scheme_Object *port)
{
  scheme_display_w_max(obj, port, -1);
}

static void *print_to_string_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *obj;
  long *len, maxl;
  int iswrite;

  obj = (Scheme_Object *)p->ku.k.p1;
  len = (long *)p->ku.k.p2;
  maxl = p->ku.k.i1;
  iswrite = p->ku.k.i2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return (void *)print_to_string(obj, len, iswrite, 
				 NULL, maxl,
				 p, p->config);
}

char *scheme_write_to_string_w_max(Scheme_Object *obj, long *len, long maxl)
{
  Scheme_Process *p = scheme_current_process;

  p->ku.k.p1 = obj;
  p->ku.k.p2 = len;
  p->ku.k.i1 = maxl;
  p->ku.k.i2 = 1;

  return (char *)scheme_top_level_do(print_to_string_k, 0);
}

char *scheme_write_to_string(Scheme_Object *obj, long *len)
{
  return scheme_write_to_string_w_max(obj, len, -1);
}

char *scheme_display_to_string_w_max(Scheme_Object *obj, long *len, long maxl)
{
  Scheme_Process *p = scheme_current_process;

  p->ku.k.p1 = obj;
  p->ku.k.p2 = len;
  p->ku.k.i1 = maxl;
  p->ku.k.i2 = 0;

  return (char *)scheme_top_level_do(print_to_string_k, 0);
}

char *scheme_display_to_string(Scheme_Object *obj, long *len)
{
  return scheme_display_to_string_w_max(obj, len, -1);
}

void
scheme_internal_write(Scheme_Object *obj, Scheme_Object *port, 
		      Scheme_Config *config)
{
  print_to_port("write", obj, port, 1, -1, scheme_current_process, config);
}

void
scheme_internal_display(Scheme_Object *obj, Scheme_Object *port, 
		      Scheme_Config *config)
{
  print_to_port("display", obj, port, 0, -1, scheme_current_process, config);
}

#ifdef DO_STACK_CHECK
static int check_cycles(Scheme_Object *, Scheme_Process *);

static Scheme_Object *check_cycle_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  return check_cycles(o, p)
    ? scheme_true : scheme_false;
}
#endif

static int check_cycles(Scheme_Object *obj, Scheme_Process *p)
{
  Scheme_Type t;
  int cycle = 0;

  t = SCHEME_TYPE(obj);
  if (t < 0)
    return 1;

#ifdef DO_STACK_CHECK
#define CHECK_COUNT_START 50
  {
    static int check_counter = CHECK_COUNT_START;

    if (!--check_counter) {
      check_counter = CHECK_COUNT_START;
      SCHEME_USE_FUEL(CHECK_COUNT_START);
      {
#include "mzstkchk.h"
	{
	  scheme_current_process->ku.k.p1 = (void *)obj;
	  return SCHEME_TRUEP(scheme_handle_stack_overflow(check_cycle_k));
	}
      }
    }
  }
#else
  SCHEME_USE_FUEL(1);
#endif

  if (SCHEME_PAIRP(obj)) {
    obj->type = -t;
    cycle = check_cycles(SCHEME_CAR(obj), p) || check_cycles(SCHEME_CDR(obj), p);
    obj->type = t;
  } else if (p->quick_print_box && SCHEME_BOXP(obj)) {
    obj->type = -t;
    cycle = check_cycles(SCHEME_BOX_VAL(obj), p);
    obj->type = t;
  } else if (SCHEME_VECTORP(obj)) {
    int i, len;
    Scheme_Object **array;

    obj->type = -t;
    len = SCHEME_VEC_SIZE(obj);
    array = SCHEME_VEC_ELS(obj);
    for (i = 0; i < len; i++) {
      if (check_cycles(array[i], p)) {
	cycle = 1;
	break;
      }
    }
    obj->type = t;
  } else if (p->quick_print_struct && SAME_TYPE(t, scheme_structure_type)) {
    Scheme_Object **slots = ((Scheme_Structure *)obj)->slots;
    int i = SCHEME_STRUCT_NUM_SLOTS(obj);

    obj->type = -t;
    while (i--) {
      if (check_cycles(slots[i], p)) {
	cycle = 1;
	break;
      }
    }
    obj->type = t;
  }  else
    cycle = 0;

  return cycle;
}

static void setup_graph_table(Scheme_Object *obj, Scheme_Hash_Table *ht,
			      int *counter, Scheme_Process *p)
{
  if (HAS_SUBSTRUCT(obj)) {
    Scheme_Bucket *b;

    b = scheme_bucket_from_table(ht, (const char *)obj);

    if (!b->val)
      b->val = (void *)1;
    else {
      if ((long)b->val == 1)
	b->val = (void *)(long)++(*counter);
      return;
    }
  } else
    return;

  if (SCHEME_PAIRP(obj)) {
    setup_graph_table(SCHEME_CAR(obj), ht, counter, p);
    setup_graph_table(SCHEME_CDR(obj), ht, counter, p);
  } else if (p->quick_print_box && SCHEME_BOXP(obj)) {
    setup_graph_table(SCHEME_BOX_VAL(obj), ht, counter, p);
  } else if (SCHEME_VECTORP(obj)) {
    int i, len;
    Scheme_Object **array;

    len = SCHEME_VEC_SIZE(obj);
    array = SCHEME_VEC_ELS(obj);
    for (i = 0; i < len; i++) {
      setup_graph_table(array[i], ht, counter, p);
    }
  } else if (p->quick_print_struct 
	   && SAME_TYPE(SCHEME_TYPE(obj), scheme_structure_type)) {
    Scheme_Object **slots = ((Scheme_Structure *)obj)->slots;
    int i = SCHEME_STRUCT_NUM_SLOTS(obj);

    while (i--) {
      setup_graph_table(slots[i], ht, counter, p);
    }
  }
}

static char *
print_to_string(Scheme_Object *obj, long *len, int write,
		Scheme_Object *port, long maxl, Scheme_Process *p,
		Scheme_Config *config)
{
  Scheme_Hash_Table * volatile ht;
  char *ca;

  p->print_allocated = 50;
  ca = (char *)scheme_malloc_atomic(p->print_allocated);
  p->print_buffer = ca;
  p->print_position = 0;
  p->print_maxlen = maxl;
  p->print_port = port;

  p->quick_print_graph = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_PRINT_GRAPH));
  p->quick_print_box = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_PRINT_BOX));
  p->quick_print_struct = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_PRINT_STRUCT));
  p->quick_print_vec_shorthand = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_PRINT_VEC_SHORTHAND));
  p->quick_can_read_pipe_quote = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_CAN_READ_PIPE_QUOTE));

  if (p->quick_print_graph || check_cycles(obj, p)) {
    int counter = 1;
    ht = scheme_hash_table(101, SCHEME_hash_ptr, 0, 0);
    setup_graph_table(obj, ht, &counter, p);
  } else
    ht = NULL;

  if ((maxl <= PRINT_MAXLEN_MIN) 
      || !scheme_setjmp(p->print_escape))
    print(obj, write, 0, ht, NULL, p);

  p->print_buffer[p->print_position] = '\0';

  if (len)
    *len = p->print_position;

  return p->print_buffer;
}

typedef void (*Write_String_Fun)(char *str, long len, struct Scheme_Output_Port *);

static void 
print_to_port(char *name, Scheme_Object *obj, Scheme_Object *port, int escaped,
	      long maxl, Scheme_Process *p, Scheme_Config *config)
{
  Scheme_Output_Port *op;
  char *str;
  long len;
  
  op = (Scheme_Output_Port *)port;
  if (op->closed)
    scheme_raise_exn(MZEXN_I_O_PORT_CLOSED, port, "%s: output port is closed", name);

  str = print_to_string(obj, &len, escaped, port, maxl, p, config);

  {
    Write_String_Fun f = op->write_string_fun;
    f(str, len, op);
  }
  op->pos += len;
}

static void print_this_string(Scheme_Process *p, const char *str, int autolen)
{
  long len;
  char *oldstr;

  if (!autolen)
    return;
  else if (autolen > 0)
    len = autolen;
  else
    len = strlen(str);

  if (!p->print_buffer) {
    /* Just getting the length */
    p->print_position += len;
    return;
  }


  if (len + p->print_position + 1 > p->print_allocated) {
    if (len + 1 >= p->print_allocated)
      p->print_allocated = 2 * p->print_allocated + len + 1;
    else
      p->print_allocated = 2 * p->print_allocated;

    oldstr = p->print_buffer;
    {
      char *ca;
      ca = (char *)scheme_malloc_atomic(p->print_allocated);
      p->print_buffer = ca;
    }
    memcpy(p->print_buffer, oldstr, p->print_position);
  }

  memcpy(p->print_buffer + p->print_position, str, len);
  p->print_position += len;

  SCHEME_USE_FUEL(len);
  
  if (p->print_maxlen > PRINT_MAXLEN_MIN)  {
    if (p->print_position > p->print_maxlen) {
      long l = p->print_maxlen;

      p->print_buffer[l] = 0;
      p->print_buffer[l - 1] = '.';
      p->print_buffer[l - 2] = '.';
      p->print_buffer[l - 3] = '.';

      p->print_position = l;

      scheme_longjmp(p->print_escape, 1);
    }
  } else if (p->print_position > MAX_PRINT_BUFFER) {
    if (p->print_port) {
      Scheme_Output_Port *op = (Scheme_Output_Port *)p->print_port;

      p->print_buffer[p->print_position] = 0;
      {
	Write_String_Fun f = op->write_string_fun;
	f(p->print_buffer, p->print_position, op);
      }
      op->pos += p->print_position;
      
      p->print_position = 0;
    }
  }
}

static void print_compact_number(Scheme_Process *p, int n)
{
  unsigned char s[5];

  if (n < 0) {
    if (n > -256) {
      s[0] = 254;
      s[1] = -n;
      print_this_string(p, (char *)s, 2);
      return;
    } else {
      n = -n;
      s[0] = 255;
    }
  } else if (n < 253) {
    s[0] = n;
    print_this_string(p, (char *)s, 1);
    return;
  } else
    s[0] = 253;

  s[1] = n & 0xFF;
  s[2] = (n >> 8) & 0xFF;
  s[3] = (n >> 16) & 0xFF;
  s[4] = (n >> 24) & 0xFF;  
  
  print_this_string(p, (char *)s, 5);
}

static void print_string_in_angle(Scheme_Process *p, const char *start, const char *prefix, int slen)
{
  /* Used to do something special for type symbols. No more. */
  print_this_string(p, prefix, -1);
  print_this_string(p, start, slen);
}

#ifdef DO_STACK_CHECK

static Scheme_Object *print_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)p->ku.k.p2;
  Scheme_Hash_Table *vht = (Scheme_Hash_Table *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return print(o, 
	       p->ku.k.i1, 
	       p->ku.k.i2, 
	       ht,
	       vht,
	       p) 
    ? scheme_true : scheme_false;
}
#endif

#define SCHEME_VARIABLE_TYPE scheme_variable_type

static int
print_substring(Scheme_Object *obj, int escaped, int compact, Scheme_Hash_Table *ht,
		Scheme_Hash_Table *vht, Scheme_Process *p, char **result, long *rlen)
{
  int closed;
  long save_alloc, save_pos, save_maxl;
  char *save_buf;
  Scheme_Object *save_port;

  save_alloc = p->print_allocated;
  save_buf = p->print_buffer;
  save_pos = p->print_position;
  save_maxl = p->print_maxlen;
  save_port = p->print_port;
  
  /* If result is NULL, just measure the output. */
  if (result) {
    char *ca;
    p->print_allocated = 50;
    ca = (char *)scheme_malloc_atomic(p->print_allocated);
    p->print_buffer = ca;
  } else {
    p->print_allocated = 0;
    p->print_buffer = NULL;
  }
  p->print_position = 0;
  p->print_port = NULL;

  closed = print(obj, escaped, compact, ht, vht, p);

  if (result)
    *result = p->print_buffer;
  *rlen = p->print_position;

  p->print_allocated = save_alloc;
  p->print_buffer = save_buf;
  p->print_position = save_pos;
  p->print_maxlen = save_maxl;
  p->print_port = save_port;
  
  return closed;
}

static void print_escaped(Scheme_Process *p, int escaped, 
			  Scheme_Object *obj, Scheme_Hash_Table *ht)
{
  char *r;
  long len;

  print_substring(obj, escaped, 0, ht, NULL, p, &r, &len);

  print_compact(p, CPT_ESCAPE);
  print_compact_number(p, len);
  print_this_string(p, r, len);
}

#ifdef SGC_STD_DEBUGGING
static void printaddress(Scheme_Process *p, Scheme_Object *o)
{
  char buf[40];
  sprintf(buf, ":%lx", (long)o);
  print_this_string(p, buf, -1);
}
# define PRINTADDRESS(p, obj) printaddress(p, obj)
#else
# define PRINTADDRESS(p, obj) /* empty */
#endif

static void print_named(Scheme_Object *obj, const char *kind,
			const char *s, int len, Scheme_Process *p)
{
  print_this_string(p, "#<", 2);
  print_this_string(p, kind, -1);

  if (s) {
    print_this_string(p, ":", 1);

    print_this_string(p, s, len);
  }
   
  PRINTADDRESS(p, obj);
  print_this_string(p, ">", 1);
}

static int
print(Scheme_Object *obj, int escaped, int compact, Scheme_Hash_Table *ht,
      Scheme_Hash_Table *vht, Scheme_Process *p)
{
  int closed = 0;

#if NO_COMPACT
  compact = 0;
#endif

#ifdef DO_STACK_CHECK
#define PRINT_COUNT_START 20
  {
    static int check_counter = PRINT_COUNT_START;

    if (!--check_counter) {
      check_counter = PRINT_COUNT_START;
      {
#include "mzstkchk.h"
	{
	  p->ku.k.p1 = (void *)obj;
	  p->ku.k.i1 = escaped;
	  p->ku.k.i2 = compact;
	  p->ku.k.p2 = (void *)ht;
	  return SCHEME_TRUEP(scheme_handle_stack_overflow(print_k));
	}
      }
    }
  }
#endif

  if (ht && HAS_SUBSTRUCT(obj)) {
    Scheme_Bucket *b;
    b = scheme_bucket_from_table(ht, (const char *)obj);
    
    if ((long)b->val != 1) {
      if (compact) {
	print_escaped(p, escaped, obj, ht);
	return 1;
      } else {
	if ((long)b->val > 0) {
	  sprintf(quick_buffer, "#%ld=", ((long)b->val) - 2);
	  print_this_string(p, quick_buffer, -1);
	  b->val = (void *)(-(long)b->val);
	} else {
	  sprintf(quick_buffer, "#%ld#", -((long)b->val + 2));
	  print_this_string(p, quick_buffer, -1);
	  return 0;
	}
      }
    }
  }

  if (SCHEME_SYMBOLP(obj))
    {
      int l;

      if (compact) {
	l = SCHEME_SYM_LEN(obj);
	if (l < CPT_RANGE(SMALL_SYMBOL)) {
	  unsigned char s[1];
	  s[0] = l + CPT_SMALL_SYMBOL_START;
	  print_this_string(p, (char *)s, 1);
	} else {
	  print_compact(p, CPT_SYMBOL);
	  print_compact_number(p, l);
	}
	print_this_string(p, SCHEME_SYM_VAL(obj), l);
      } else if (escaped) {
	const char *s;
	
	s = scheme_symbol_name_and_size(obj, &l, (p->quick_can_read_pipe_quote 
						  ? SNF_PIPE_QUOTE
						  : SNF_NO_PIPE_QUOTE));
	print_this_string(p, s, l);
      } else {
	print_this_string(p, SCHEME_SYM_VAL(obj), SCHEME_SYM_LEN(obj));
      }
    }
  else if (SCHEME_STRINGP(obj))
    {
      if (compact) {
	int l;

	print_compact(p, CPT_STRING);
	l = SCHEME_STRTAG_VAL(obj);
	print_compact_number(p, l);
	print_this_string(p, SCHEME_STR_VAL(obj), l);
      } else {
	print_string(obj, escaped, p);
	closed = 1;
      }
    }
  else if (SCHEME_CHARP(obj))
    {
      if (compact) {
	char s[1];
	print_compact(p, CPT_CHAR);
	s[0] = SCHEME_CHAR_VAL(obj);
	print_this_string(p, s, 1);
      } else
	print_char(obj, escaped, p);
    }
  else if (SCHEME_INTP(obj))
    {
      if (compact) {
	long v = SCHEME_INT_VAL(obj);
	if (v >= 0 && v < CPT_RANGE(SMALL_NUMBER)) {
	  unsigned char s[1];
	  s[0] = v + CPT_SMALL_NUMBER_START;
	  print_this_string(p, (char *)s, 1);
	} else {
	  print_compact(p, CPT_INT);
	  print_compact_number(p, v);
	}
      } else {
	sprintf(quick_buffer, "%ld", SCHEME_INT_VAL(obj));
	print_this_string(p, quick_buffer, -1);
      }
    }
  else if (SCHEME_NUMBERP(obj))
    {
      if (compact) {
	print_escaped(p, escaped, obj, ht);
	closed = 1;
      } else
	print_this_string(p, scheme_number_to_string(10, obj), -1);
    }
  else if (SCHEME_NULLP(obj))
    {
      if (compact) {
	print_compact(p, CPT_NULL);
      } else {
	print_this_string(p, "()", 2);
	closed = 1;
      }
    }
  else if (SCHEME_PAIRP(obj))
    {
      print_pair(obj, escaped, compact, ht, vht, p);
      closed = 1;
    }
  else if (SCHEME_VECTORP(obj))
    {
      print_vector(obj, escaped, compact, ht, vht, p);
      closed = 1;
    }
  else if (p->quick_print_box && SCHEME_BOXP(obj))
    {
      if (compact)
	print_compact(p, CPT_BOX);
      else
	print_this_string(p, "#&", 2);
      closed = print(SCHEME_BOX_VAL(obj), escaped, compact, ht, vht, p);
    }
  else if (SAME_OBJ(obj, scheme_true))
    {
      if (compact)
	print_compact(p, CPT_TRUE);
      else
	print_this_string(p, "#t", 2);
    }
  else if (SAME_OBJ(obj, scheme_false))
    {
      if (compact)
	print_compact(p, CPT_FALSE);
      else
	print_this_string(p, "#f", 2);
    }
  else if (SAME_TYPE(SCHEME_TYPE(obj), scheme_structure_type))
    {
      if (compact)
	print_escaped(p, escaped, obj, ht);
      else {
	Scheme_Object *name = SCHEME_STRUCT_NAME_SYM(obj);

	print_this_string(p, p->quick_print_struct ? "#(" : "#<", 2);
	{
	  int l;
	  const char *s;

	  s = scheme_symbol_name_and_size(name, &l, 
					  (p->quick_print_struct
					   ? SNF_FOR_TS
					   : (p->quick_can_read_pipe_quote 
					      ? SNF_PIPE_QUOTE
					      : SNF_NO_PIPE_QUOTE)));
	  print_this_string(p, s, l);
	}

	if (p->quick_print_struct) {
	  Scheme_Object **slots = ((Scheme_Structure *)obj)->slots;
	  int i, count = SCHEME_STRUCT_NUM_SLOTS(obj), no_sp_ok;
	  
	  if (count)
	    print_this_string(p, " ", 1);
	  
	  for (i = 0; i < count; i++) {
	    no_sp_ok = print(slots[i], escaped, compact, ht, vht, p);
	    if ((i < count - 1) && (!compact || !no_sp_ok))
	      print_this_string(p, " ", 1);
	  }
	  print_this_string(p, ")", 1);
	} else {
	  PRINTADDRESS(p, obj);
	  print_this_string(p, ">", 1);
	}
      }

      closed = 1;
    }
  else if (SCHEME_PRIMP(obj) && ((Scheme_Primitive_Proc *)obj)->name)
    {
      if (compact) {
	print_escaped(p, escaped, obj, ht);
      } else {
	print_this_string(p, "#<", 2);
	print_string_in_angle(p, ((Scheme_Primitive_Proc *)obj)->name, "primitive:", -1);
	PRINTADDRESS(p, obj);
	print_this_string(p, ">", 1);
      }
      closed = 1;
    }
  else if (SCHEME_CLSD_PRIMP(obj) && ((Scheme_Closed_Primitive_Proc *)obj)->name)
    {
      if (compact)
	print_escaped(p, escaped, obj, ht);
      else {
	if (SCHEME_STRUCT_PROCP(obj)) {
	  print_named(obj, "struct-procedure", 
		      ((Scheme_Closed_Primitive_Proc *)obj)->name, 
		      -1, p);
	} else {
	  print_this_string(p, "#<", 2);
	  print_string_in_angle(p, ((Scheme_Closed_Primitive_Proc *)obj)->name, 
				SCHEME_GENERICP(obj) ? "" : "primitive:", -1);
	  PRINTADDRESS(p, obj);
	  print_this_string(p, ">", 1);
	}
      }

      closed = 1;
    }
  else if (SCHEME_CLOSUREP(obj)) 
    {
      if (compact) {
	print_escaped(p, escaped, obj, ht);
      } else {
	int len;
	const char *s;
	s = scheme_get_proc_name(obj, &len, 0);
	
	print_named(obj, "procedure", s, len, p);
      }
      closed = 1;
    }
#ifndef NO_OBJECT_SYSTEM
  else if (SCHEME_OBJP(obj))
    {
      if (compact)
	print_escaped(p, escaped, obj, ht);
      else {
	int len;
	const char *cl;
	cl = scheme_get_class_name(((Scheme_Class_Object *)obj)->sclass, &len);
	
	print_named(obj, "object", cl, len, p);
      }
      closed = 1;
    }
  else if (SCHEME_CLASSP(obj))
    {
      if (compact)
	print_escaped(p, escaped, obj, ht);
      else {
	int len;
	const char *cl;
	cl = scheme_get_class_name(obj, &len);
	
	print_named(obj, "class", cl, len, p);
      }
      closed = 1;
    }
  else if (SCHEME_INTERFACEP(obj))
    {
      if (compact)
	print_escaped(p, escaped, obj, ht);
      else {
	int len;
	const char *cl;
	cl = scheme_get_interface_name(obj, &len);
	
	print_named(obj, "interface", cl, len, p);
      }
      closed = 1;
    }
#endif
  else if (SCHEME_UNITP(obj))
    {
      if (compact)
	print_escaped(p, escaped, obj, ht);
      else {
	int len;
	const char *cl;
	cl = scheme_get_unit_name(obj, &len);
	
	print_named(obj, "unit", cl, len, p);
      }
      closed = 1;
    }
  else if (SCHEME_INPORTP(obj))
    {
      Scheme_Input_Port *ip;
      ip = (Scheme_Input_Port *)obj;
      print_this_string(p, "#", 1);
      print_this_string(p, SCHEME_SYM_VAL(ip->sub_type), SCHEME_SYM_LEN(ip->sub_type));
    }
  else if (SCHEME_OUTPORTP(obj))
    {
      Scheme_Output_Port *op;
      op = (Scheme_Output_Port *)obj;
      print_this_string(p, "#", 1);
      print_this_string(p, SCHEME_SYM_VAL(op->sub_type), SCHEME_SYM_LEN(op->sub_type));
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), SCHEME_VARIABLE_TYPE)
	   && (((Scheme_Bucket_With_Const_Flag *)obj)->flags & GLOB_HAS_REF_ID))
    {
      int pos;
      pos = ((Scheme_Bucket_With_Ref_Id *)obj)->id;
      print_compact(p, CPT_REFERENCE);
      print_compact_number(p, pos);
    }   
  else if (compact 
	   && (SAME_TYPE(SCHEME_TYPE(obj), scheme_local_type)
	       || SAME_TYPE(SCHEME_TYPE(obj), scheme_local_unbox_type)))
    {
      int unbox = SAME_TYPE(SCHEME_TYPE(obj), scheme_local_unbox_type);
      Scheme_Local *loc = (Scheme_Local *)obj;
      if (loc->position < CPT_RANGE(SMALL_LOCAL)) {
	unsigned char s[1];
	s[0] = loc->position + (unbox 
				? CPT_SMALL_LOCAL_UNBOX_START 
				: CPT_SMALL_LOCAL_START);
	print_this_string(p, (char *)s, 1);
      } else {
	print_compact(p, unbox ? CPT_LOCAL_UNBOX : CPT_LOCAL);
	print_compact_number(p, loc->position);
      }
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_application_type))
    {
      Scheme_App_Rec *app;
      int i;

      app = (Scheme_App_Rec *)obj;

      if (app->num_args < CPT_RANGE(SMALL_APPLICATION)) {
	unsigned char s[1];
	s[0] = CPT_SMALL_APPLICATION_START + app->num_args;
	print_this_string(p, (char *)s, 1);
      } else {
	print_compact(p, CPT_APPLICATION);
	print_compact_number(p, app->num_args);
      }

      for (i = 0; i < app->num_args + 1; i++) {
	closed = print(app->args[i], escaped, 1, NULL, vht, p);
      }
    }
  else if (SAME_TYPE(SCHEME_TYPE(obj), scheme_quote_compilation_type))
    {
      Scheme_Hash_Table *q_ht;
      Scheme_Object *v;
      int counter = 1;

      v = SCHEME_PTR_VAL(obj);

      /* A quoted expression may have graph structure. We assume that
	 this structure is local within the quoted expression. */
      
      q_ht = scheme_hash_table(101, SCHEME_hash_ptr, 0, 0);
      setup_graph_table(v, q_ht, &counter, p);

      if (compact)
	print_compact(p, CPT_QUOTE);
      else {
#if !NO_COMPACT
	/* Doesn't happen: */
	scheme_signal_error("internal error: non-compact quote compilation");
	return 0;
#endif
      }

      compact = print(v, escaped, 1, q_ht, vht, p);
    }
  else if (
#if !NO_COMPACT
	   compact && 
#endif
	   SAME_TYPE(SCHEME_TYPE(obj), scheme_svector_type))
    {
      short l, *v;
      l = SCHEME_SVEC_LEN(obj);
      v = (short *)SCHEME_SVEC_VEC(obj);
      
#if NO_COMPACT
      print_this_string(p, "[", 1);
      {
	int i; 
	char s[10];

	for (i = 0; i < l; i++) {
	  if (i)
	    print_this_string(p, " ", 1);
	  sprintf(s, "%d", (int)v[i]);
	  print_this_string(p, s, -1);
	}
      }
      print_this_string(p, "]", 1);
#else
      if (l < CPT_RANGE(SMALL_SVECTOR)) {
	unsigned char s[1];
	s[0] = l + CPT_SMALL_SVECTOR_START;
	print_this_string(p, (char *)s, 1);
      } else {
	print_compact(p, CPT_SVECTOR);
	print_compact_number(p, l);
      }
      while (l--) {
	int n = v[l];
	print_compact_number(p, n);
      }
#endif
    }
  else if (scheme_type_writers[SCHEME_TYPE(obj)]
#if !NO_COMPACT
	   && (compact || SAME_TYPE(SCHEME_TYPE(obj), scheme_compilation_top_type))
#endif
	   )
    {
      Scheme_Type t = SCHEME_TYPE(obj);
      Scheme_Object *v;
#if USE_BUFFERING_CPORT
      long slen;
#endif

      if (t >= _scheme_last_type_) {
	/* Doesn't happen: */
	scheme_signal_error("internal error: bad type with writer");
	return 0;
      }

      if (compact) {
	if (t < CPT_RANGE(SMALL_MARSHALLED)) {
	  unsigned char s[1];
	  s[0] = t + CPT_SMALL_MARSHALLED_START;
	  print_this_string(p, (char *)s, 1);
	} else {
	  print_compact(p, CPT_MARSHALLED);
	  print_compact_number(p, t);
	}
      } else {
	print_this_string(p, "#`", 2);
#if NO_COMPACT
	if (t < _scheme_last_type_) {
	  sprintf (quick_buffer, "%ld", (long)SCHEME_TYPE(obj));
	  print_this_string(p, quick_buffer, -1);
	} else
	  print_this_string(p, scheme_get_type_name(t), -1);
#endif
      }

      {
	Scheme_Type_Writer writer;
	writer = scheme_type_writers[t];
	v = writer(obj);
      }

      if (compact)
	closed = print(v, escaped, 1, NULL, vht, p);
      else {
	vht = scheme_hash_table(10, SCHEME_hash_ptr, 0, 0);
	vht->make_hash_indices = make_sym_vec_hash_indices;
	vht->compare = compare_sym_vec;

#if USE_BUFFERING_CPORT
	/* "print" the string once to get a measurement */
	print_substring(v, escaped, 1, NULL, vht, p, NULL, &slen);
	print_compact_number(p, slen);

	/* Make vht again to ensure the same results */
	vht = scheme_hash_table(10, SCHEME_hash_ptr, 0, 0);
	vht->make_hash_indices = make_sym_vec_hash_indices;
	vht->compare = compare_sym_vec;
#endif

	closed = print(v, escaped, 1, NULL, vht, p);
      }
    } 
  else 
    {
      if (compact)
	print_escaped(p, escaped, obj, ht);
      else {
	char *s;
	long len = -1;
	s = scheme_get_type_name((SCHEME_TYPE(obj)));
	print_this_string(p, "#", 1);
#ifdef SGC_STD_DEBUGGING
	len = strlen(s) - 1;
#endif
	print_this_string(p, s, len);
#ifdef SGC_STD_DEBUGGING
	PRINTADDRESS(p, obj);
	print_this_string(p, ">", 1);
#endif
      }

      closed = 1;
    }

  return (closed || compact);
}

static void
print_string(Scheme_Object *string, int escaped, Scheme_Process *p)
{
  char *str, minibuf[2];
  int simple;
  int len, i;

  if (escaped)
    print_this_string(p, "\"", 1);

  simple = 1;

  len = SCHEME_STRTAG_VAL(string);

  if (len) {
    if (escaped) {
      str = SCHEME_STR_VAL(string);
      for (i = 0; i < len; i++, str++) {
	if ((*str == '"') || (*str == '\\')) {
	  simple = 0;
	  break;
	}
      }
    }
    
    if (simple)
      print_this_string(p, SCHEME_STR_VAL(string), len);
    else {
      minibuf[1] = 0;
      for (str = SCHEME_STR_VAL(string), i = 0; i < len; i++, str++) {
	if ((*str == '"') || (*str == '\\'))
	  print_this_string(p, "\\", 1);
	minibuf[0] = *str;
	print_this_string(p, minibuf, 1);
      }
    }
  }

  if (escaped)
    print_this_string(p, "\"", 1);
}

static void
print_pair(Scheme_Object *pair, int escaped, int compact, 
	   Scheme_Hash_Table *ht, Scheme_Hash_Table *vht, Scheme_Process *p)
{
  Scheme_Object *cdr;
  int no_space_ok;
  int super_compact = 0;

  if (compact) {
    int c = 0;
    Scheme_Object *pr;

    pr = pair;
    while (SCHEME_PAIRP(pr)) {
      if (ht)
	if ((long)scheme_lookup_in_table(ht, (const char *)pr) != 1) {
	  c = -1;
	  break;
	}
      c++;
      pr = SCHEME_CDR(pr);
    }

    if (c > -1) {
      super_compact = 1;
      if (c < CPT_RANGE(SMALL_LIST)) {
	unsigned char s[1];
	s[0] = c + (SCHEME_NULLP(pr) 
		    ? CPT_SMALL_PROPER_LIST_START
		    : CPT_SMALL_LIST_START);
	print_this_string(p, (char *)s, 1);
      } else {
	print_compact(p, CPT_LIST);
	print_compact_number(p, c);
	super_compact = -1;
      }
    }
  }

  if (compact) {
    if (!super_compact)
      print_compact(p, CPT_PAIR);
  } else
    print_this_string(p, "(", 1);

  no_space_ok = print(SCHEME_CAR(pair), escaped, compact, ht, vht, p);

  cdr = SCHEME_CDR (pair);
  while (SCHEME_PAIRP(cdr)) {
    if (ht && !super_compact) {
      if ((long)scheme_lookup_in_table(ht, (const char *)cdr) != 1) {
	/* This needs a tag */
	if (!compact)
	  print_this_string(p, " . ", 3);
	(void)print(cdr, escaped, compact, ht, vht, p);
	if (!compact)
	  print_this_string(p, ")", 1);
	return;
      }
    }
    if (compact && !super_compact)
      print_compact(p, CPT_PAIR);
    if (!compact)
      print_this_string(p, " ", 1);
    no_space_ok = print(SCHEME_CAR(cdr), escaped, compact, ht, vht, p);
    cdr = SCHEME_CDR(cdr);
  }

  if (!SCHEME_NULLP(cdr)) {
    if (!compact)
      print_this_string(p, " . ", 3);
    print(cdr, escaped, compact, ht, vht, p);
  } else if (compact && (super_compact < 1))
    print_compact(p, CPT_NULL);

  if (!compact)
    print_this_string(p, ")", 1);
}

static void
print_vector(Scheme_Object *vec, int escaped, int compact, 
	     Scheme_Hash_Table *ht, Scheme_Hash_Table *vht, Scheme_Process *p)
{
  int i, no_space_ok, size, common = 0;
  Scheme_Object **elems;

  size = SCHEME_VEC_SIZE(vec);
  elems = SCHEME_VEC_ELS(vec);

  if (compact) {
    if (vht) {
      for (i = size; i--; ) {
	if (!SCHEME_INTP(elems[i]) && !SCHEME_SYMBOLP(elems[i]))
	  break;
      }
      if (i < 0) {
	/* It's a symbol vector */
	Scheme_Object *o;
	
	o = (Scheme_Object *)scheme_lookup_in_table(vht, (const char *)vec);
	if (o) {
	  /* This vector has been printed before. `Reuse' it and return. */
	  print_compact(p, CPT_SYM_VECTOR_REUSE);
	  print_compact_number(p, SCHEME_INT_VAL(o));
	  return;
	} else {
	  /* This vector hasn't been printed before. `Remember' it just in case. */
	  print_compact(p, CPT_SYM_VECTOR_REMEMBER);
	  o = scheme_make_integer(vht->count);
	  scheme_add_to_table(vht, (const char *)vec, o, 0);
	}
      }
    }

    print_compact(p, CPT_VECTOR);
    print_compact_number(p, size);
  } else {
    for (i = size; i--; common++) {
      if (!i || (elems[i] != elems[i - 1]))
	break;
    }
    
    if (escaped && p->quick_print_vec_shorthand) {
      char buffer[100];
      sprintf(buffer, "#%d(", size);
      print_this_string(p, buffer, -1);
      size -= common;
    } else
      print_this_string(p, "#(", 2);
  }

  for (i = 0; i < size; i++) {
    no_space_ok = print(elems[i], escaped, compact, ht, vht, p);
    if (i < (size - 1))
      if (!compact)
	print_this_string(p, " ", 1);
  }

  if (!compact)
    print_this_string(p, ")", 1);
}

static void
print_char(Scheme_Object *charobj, int escaped, Scheme_Process *p)
{
  char ch, minibuf[4], *str;
  int len = -1;

  ch = SCHEME_CHAR_VAL (charobj);
  if (escaped) {
    switch ( ch )
      {
      case '\0':
	str = "#\\nul";
	break;
      case '\n':
	str = "#\\newline";
	break;
      case '\t':
	str = "#\\tab";
	break;
      case 0xb:
	str = "#\\vtab";
	break;
      case ' ':
	str = "#\\space";
	break;
      case '\r':
	str = "#\\return";
	break;
      case '\f':
	str = "#\\page";
	break;
      case '\b':
	str = "#\\backspace";
	break;
      case 0x7f:
	str = "#\\rubout";
	break;
      default:
	minibuf[0] = '#';
	minibuf[1] = '\\';
	minibuf[2] = ch;
	minibuf[3] = 0;
	str = minibuf;
	break;
      }
  } else {
    minibuf[0] = ch;
    minibuf[1] = 0;
    str = minibuf;
    len = 1;
  }

  print_this_string(p, str, len);
}
