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
#include <string.h>

Scheme_Type_Reader *scheme_type_readers;
Scheme_Type_Writer *scheme_type_writers;

static char **type_names;
static Scheme_Type maxtype, allocmax;

#ifdef MEMORY_COUNTING_ON
long scheme_type_table_count;
#endif

static void init_type_arrays()
{
  long n;

  REGISTER_SO(type_names);
  REGISTER_SO(scheme_type_readers);
  REGISTER_SO(scheme_type_writers);
  
  maxtype = _scheme_last_type_;
  allocmax = maxtype + 10;

  type_names = MALLOC_N(char *, allocmax);
  scheme_type_readers = MALLOC_N_ATOMIC(Scheme_Type_Reader, allocmax);
  n = allocmax * sizeof(Scheme_Type_Reader);
  memset((char *)scheme_type_readers, 0, n);

#ifdef MEMORY_COUNTING_ON
  scheme_type_table_count += n;
  scheme_misc_count += (allocmax * sizeof(char *));
#endif

  scheme_type_writers = MALLOC_N_ATOMIC(Scheme_Type_Writer, allocmax);
  n = allocmax * sizeof(Scheme_Type_Writer);
  memset((char *)scheme_type_writers, 0, n);

#ifdef MEMORY_COUNTING_ON
  scheme_type_table_count += n;
#endif  
}

void
scheme_init_type (Scheme_Env *env)
{
  if (!scheme_starting_up)
    return;

  if (!type_names)
    init_type_arrays();

#define set_name(t, n) type_names[t] = n

  set_name(scheme_true_type, "<true>");
  set_name(scheme_false_type, "<false>");
  set_name(scheme_char_type, "<char>");
  set_name(scheme_envunbox_type, "<env-auto-unbox>");
  set_name(scheme_local_type, "<local-code>");
  set_name(scheme_local_unbox_type, "<local-unbox-code>");
  set_name(scheme_variable_type, "<variable-code>");
  set_name(scheme_application_type, "<application-code>");
  set_name(scheme_compiled_unclosed_procedure_type, "<procedure-semi-code>");
  set_name(scheme_unclosed_procedure_type, "<procedure-code>");
  set_name(scheme_syntax_type, "<syntax-code>");
  set_name(scheme_compiled_syntax_type, "<syntax-semi-code>");
  set_name(scheme_branch_type, "<branch-code>");
  set_name(scheme_sequence_type, "<sequence-code>");
  set_name(scheme_case_lambda_sequence_type, "<case-lambda-code>");
  set_name(scheme_begin0_sequence_type, "<begin0-code>");
  set_name(scheme_with_cont_mark_type, "<with-continuation-mark-code>");

  set_name(scheme_let_value_type, "<let-value-code>");
  set_name(scheme_let_void_type, "<let-void-code>");
  set_name(scheme_compiled_let_value_type, "<let-value-semi-code>");
  set_name(scheme_compiled_let_void_type, "<let-void-semi-code>");
  set_name(scheme_letrec_type, "<letrec-code>");
  set_name(scheme_let_one_type, "<let-one-code>");
  set_name(scheme_quote_compilation_type, "<quote-code>");

  set_name(scheme_eval_waiting_type, "<eval-waiting>");
  set_name(scheme_void_type, "<void>");
  set_name(scheme_prim_type, "<primitive>");
  set_name(scheme_closed_prim_type, "<primitive-closure>");
  set_name(scheme_closure_type, "<closure-form>");
  set_name(scheme_linked_closure_type, "<procedure>");
  set_name(scheme_cont_type, "<continuation>");
  set_name(scheme_tail_call_waiting_type, "<tail-call-waiting>");
  set_name(scheme_null_type, "<empty-list>");
  set_name(scheme_pair_type, "<pair>");
  set_name(scheme_box_type, "<box>");
  set_name(scheme_integer_type, "<fixnum-integer>");
  set_name(scheme_double_type, "<inexact-number>");
  set_name(scheme_float_type, "<inexact-number*>");
  set_name(scheme_object_type, "<object>");
  set_name(scheme_class_type, "<class>");
  set_name(scheme_class_data_type, "<class-code>");
  set_name(scheme_generic_type, "<unknown-external>");
  set_name(scheme_undefined_type, "<undefined>");
  set_name(scheme_eof_type, "<eof>");
  set_name(scheme_input_port_type, "<input-port>");
  set_name(scheme_output_port_type, "<output-port>");
  set_name(scheme_process_type, "<thread>");
  set_name(scheme_promise_type, "<promise>");
  set_name(scheme_string_type, "<string>");
  set_name(scheme_struct_info_type, "<struct-info>");
  set_name(scheme_structure_type, "<struct>");
  set_name(scheme_symbol_type, "<symbol>");
  set_name(scheme_syntax_compiler_type, "<syntax>");
  set_name(scheme_macro_type, "<macro>");
  set_name(scheme_id_macro_type, "<id-macro>");
  set_name(scheme_vector_type, "<vector>");
  set_name(scheme_bignum_type, "<bignum-integer>");
  set_name(scheme_escaping_cont_type, "<escape-continuation>");
  set_name(scheme_sema_type, "<semaphore>");
  set_name(scheme_hash_table_type, "<hash-table>");
  set_name(scheme_case_closure_type, "<procedure>");
  set_name(scheme_generic_data_type, "<generic-data>");
  set_name(scheme_multiple_values_type, "<multiple-values>");
  set_name(scheme_placeholder_type, "<placeholder>");
  set_name(scheme_weak_box_type, "<weak-box>");
  set_name(scheme_rational_type, "<fractional-number>");
  set_name(scheme_complex_type, "<complex-number>");
  set_name(scheme_complex_izi_type, "<inexactly-real-number>");
  set_name(scheme_struct_type_type, "<struct-type>");
  set_name(scheme_exp_time_type, "<expansion-time-value>");
  set_name(scheme_listener_type, "<tcp-listener>");
  set_name(scheme_namespace_type, "<namespace>");
  set_name(scheme_config_type, "<parameterization>");
  set_name(scheme_will_executor_type, "<will-executor>");
  set_name(scheme_interface_type, "<interface>");
  set_name(scheme_random_state_type, "<pseudo-random-generator>");
  set_name(scheme_regexp_type, "<regexp>");

  set_name(scheme_unit_type, "<unit>");
  set_name(scheme_compiled_unit_type, "<unit-code>");
  set_name(scheme_unit_body_data_type, "<unit-body-code>");
  set_name(scheme_unit_compound_data_type, "<compound-unit-code>");
  set_name(scheme_invoke_unit_data_type, "<invoke-unit-code>");

  set_name(scheme_interface_data_type, "<interface-code>");

  set_name(scheme_compilation_top_type, "<compiled-code>");
  set_name(scheme_svector_type, "<short-vector>");

  set_name(scheme_manager_type, "<custodian>");
  set_name(scheme_cont_mark_set_type, "<continuation-mark-set>");
  
  set_name(scheme_reserved_1_type, "<reserved1>");
  set_name(scheme_reserved_3_type, "<reserved3>");
  set_name(scheme_reserved_5_type, "<reserved5>");

  set_name(_scheme_values_types_, "<resurrected>");
  set_name(_scheme_compiled_values_types_, "<internal>");
}

Scheme_Type scheme_make_type(const char *name)
{
  if (!type_names)
    init_type_arrays();

  if (maxtype == allocmax) {
    /* Expand arrays */
    void *naya;
    long n;
    
    allocmax += 20;

    naya = scheme_malloc(allocmax * sizeof(char *));
    memcpy(naya, type_names, maxtype * sizeof(char *));
    type_names = (char **)naya;

    naya = scheme_malloc_atomic(n = allocmax * sizeof(Scheme_Type_Reader));
    memset((char *)naya, 0, n);
    memcpy(naya, scheme_type_readers, maxtype * sizeof(Scheme_Type_Reader));
    scheme_type_readers = (Scheme_Type_Reader *)naya;

    naya = scheme_malloc_atomic(n = allocmax * sizeof(Scheme_Type_Writer));
    memset((char *)naya, 0, n);
    memcpy(naya, scheme_type_writers, maxtype * sizeof(Scheme_Type_Writer));
    scheme_type_writers = (Scheme_Type_Writer *)naya;

#ifdef MEMORY_COUNTING_ON
  scheme_type_table_count += 20 * (sizeof(Scheme_Type_Reader)
				   + sizeof(Scheme_Type_Writer));
  scheme_misc_count += (20 * sizeof(char *));
#endif
  }

  {
    char *tn;
    tn = scheme_strdup(name);
    type_names[maxtype] = tn;
  }

  return maxtype++;
}

char *scheme_get_type_name(Scheme_Type t)
{
  if (t < 0 || t >= maxtype)
    return "<bad-value>";
  return type_names[t];
}

void scheme_install_type_reader(Scheme_Type t, Scheme_Type_Reader f)
{
  if (t < 0 || t >= maxtype)
    return;

  scheme_type_readers[t] = f;
}

void scheme_install_type_writer(Scheme_Type t, Scheme_Type_Writer f)
{
  if (t < 0 || t >= maxtype)
    return;

  scheme_type_writers[t] = f;
}

int scheme_num_types(void)
{
  return maxtype;
}

/***********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

static int bad_trav_SIZE(void *p)
{
  printf("Shouldn't get here.\n");
  exit(1);
}

static int bad_trav_MARK(void *p)
{
  printf("Shouldn't get here.\n");
  exit(1);
}

static int bad_trav_FIXUP(void *p)
{
  printf("Shouldn't get here.\n");
  exit(1);
}

static void MARK_cjs(Scheme_Continuation_Jump_State *cjs)
{
  gcMARK(cjs->jumping_to_continuation);
  gcMARK(cjs->u.vals);
}

static void FIXUP_cjs(Scheme_Continuation_Jump_State *cjs)
{
  gcFIXUP(cjs->jumping_to_continuation);
  gcFIXUP(cjs->u.vals);
}

static void MARK_stack_state(Scheme_Stack_State *ss)
{

  Scheme_Object **old = ss->runstack_start;

  gcMARK(ss->runstack_saved);
  gcMARK(ss->runstack_start);
  ss->runstack = ss->runstack_start + (ss->runstack - old);
}

static void FIXUP_stack_state(Scheme_Stack_State *ss)
{

  Scheme_Object **old = ss->runstack_start;

  gcFIXUP(ss->runstack_saved);
  gcFIXUP(ss->runstack_start);
  ss->runstack = ss->runstack_start + (ss->runstack - old);
}

static void MARK_jmpup(Scheme_Jumpup_Buf *buf)
{
  gcMARK(buf->stack_copy);
  gcMARK(buf->cont);
  gcMARK(buf->external_stack);

  if (buf->stack_copy)
    GC_mark_variable_stack(buf->gc_var_stack,
			   (long)buf->stack_copy - (long)buf->stack_from,
			   /* FIXME: stack direction */
			   buf->stack_copy + buf->stack_size);
}

static void FIXUP_jmpup(Scheme_Jumpup_Buf *buf)
{
  gcFIXUP(buf->stack_copy);
  gcFIXUP(buf->cont);
  gcFIXUP(buf->external_stack);

  if (buf->stack_copy)
    GC_fixup_variable_stack(buf->gc_var_stack,
			    (long)buf->stack_copy - (long)buf->stack_from,
			    /* FIXME: stack direction */
			    buf->stack_copy + buf->stack_size);
}

#define MARKS_FOR_TYPE_C
#include "mzmark.c"

void scheme_register_traversers(void)
{
  GC_REG_TRAV(scheme_variable_type, variable_obj);
  GC_REG_TRAV(scheme_local_type, local_obj);
  GC_REG_TRAV(scheme_local_unbox_type, local_obj);
  GC_REG_TRAV(scheme_syntax_type, second_of_cons);
  GC_REG_TRAV(scheme_application_type, app_rec);
  GC_REG_TRAV(scheme_sequence_type, seq_rec);
  GC_REG_TRAV(scheme_branch_type, branch_rec);
  GC_REG_TRAV(scheme_unclosed_procedure_type, unclosed_proc);
  GC_REG_TRAV(scheme_let_value_type, let_value);
  GC_REG_TRAV(scheme_let_void_type, let_void);
  GC_REG_TRAV(scheme_letrec_type, letrec);
  GC_REG_TRAV(scheme_let_one_type, let_one);
  GC_REG_TRAV(scheme_with_cont_mark_type, with_cont_mark);

  GC_REG_TRAV(_scheme_values_types_, bad_trav);
  
  GC_REG_TRAV(scheme_compiled_unclosed_procedure_type, comp_unclosed_proc);
  GC_REG_TRAV(scheme_compiled_let_value_type, comp_let_value);
  GC_REG_TRAV(scheme_compiled_let_void_type, let_header);
  GC_REG_TRAV(scheme_compiled_syntax_type, second_of_cons);

  GC_REG_TRAV(scheme_quote_compilation_type, small_object);

  GC_REG_TRAV(_scheme_compiled_values_types_, bad_trav);

  GC_REG_TRAV(scheme_prim_type, prim_proc);
  GC_REG_TRAV(scheme_closed_prim_type, closed_prim_proc);
  GC_REG_TRAV(scheme_linked_closure_type, linked_closure);
  GC_REG_TRAV(scheme_case_closure_type, case_closure);
  GC_REG_TRAV(scheme_cont_type, cont_proc);
  GC_REG_TRAV(scheme_rt_dyn_wind, mark_dyn_wind);
  GC_REG_TRAV(scheme_rt_overflow, mark_overflow);
  GC_REG_TRAV(scheme_escaping_cont_type, escaping_cont_proc);

  GC_REG_TRAV(scheme_char_type, char_obj);
  GC_REG_TRAV(scheme_integer_type, bad_trav);
  GC_REG_TRAV(scheme_bignum_type, bignum_obj);
  GC_REG_TRAV(scheme_rational_type, rational_obj);
  GC_REG_TRAV(scheme_float_type,  float_obj);
  GC_REG_TRAV(scheme_double_type, double_obj);
  GC_REG_TRAV(scheme_complex_izi_type, complex_obj);
  GC_REG_TRAV(scheme_complex_type, complex_obj);
  GC_REG_TRAV(scheme_string_type, string_obj);
  GC_REG_TRAV(scheme_symbol_type, symbol_obj);
  GC_REG_TRAV(scheme_null_type, char_obj); /* small */
  GC_REG_TRAV(scheme_pair_type, cons_cell);
  GC_REG_TRAV(scheme_vector_type, vector_obj);
  GC_REG_TRAV(scheme_closure_type, bad_trav); /* not used anymore */

  GC_REG_TRAV(scheme_input_port_type, input_port);
  GC_REG_TRAV(scheme_output_port_type, output_port);
  GC_REG_TRAV(scheme_eof_type, char_obj); /* small */
  GC_REG_TRAV(scheme_true_type, char_obj); /* small */
  GC_REG_TRAV(scheme_false_type, char_obj); /* small */
  GC_REG_TRAV(scheme_void_type, char_obj);  /* small */
  GC_REG_TRAV(scheme_syntax_compiler_type, syntax_compiler);
  GC_REG_TRAV(scheme_macro_type, small_object);
  GC_REG_TRAV(scheme_promise_type, promise_val);
  GC_REG_TRAV(scheme_box_type, small_object);
  GC_REG_TRAV(scheme_process_type, process_val);
  GC_REG_TRAV(scheme_generic_type, bad_trav); /* generic = proc */
  GC_REG_TRAV(scheme_cont_mark_set_type, cont_mark_set_val);
  GC_REG_TRAV(scheme_sema_type, sema_val);
  GC_REG_TRAV(scheme_hash_table_type, hash_table_val);
  GC_REG_TRAV(scheme_id_macro_type, small_object);
  GC_REG_TRAV(scheme_exp_time_type, small_object);
  GC_REG_TRAV(scheme_namespace_type, namespace_val);
  GC_REG_TRAV(scheme_random_state_type, random_state_val);
  
  GC_REG_TRAV(scheme_compilation_top_type, compilation_top_val);

  GC_REG_TRAV(scheme_envunbox_type, small_object);
  GC_REG_TRAV(scheme_eval_waiting_type, bad_trav);
  GC_REG_TRAV(scheme_tail_call_waiting_type, bad_trav);
  GC_REG_TRAV(scheme_undefined_type, char_obj); /* small */
  GC_REG_TRAV(scheme_multiple_values_type, bad_trav);
  GC_REG_TRAV(scheme_placeholder_type, small_object);
  GC_REG_TRAV(scheme_case_lambda_sequence_type, case_closure);
  GC_REG_TRAV(scheme_begin0_sequence_type, seq_rec);

  GC_REG_TRAV(scheme_svector_type, svector_val);

  GC_REG_TRAV(scheme_reserved_1_type, bad_trav);
  GC_REG_TRAV(scheme_reserved_3_type, bad_trav);
  GC_REG_TRAV(scheme_reserved_5_type, bad_trav);
}

END_XFORM_SKIP;

#endif
