
enum {

  /* compiled object types: (internal) */
  scheme_variable_type,
  scheme_local_type, 
  scheme_local_unbox_type,
  scheme_syntax_type,
  scheme_application_type,
  scheme_sequence_type,
  scheme_branch_type,
  scheme_unclosed_procedure_type,
  scheme_let_value_type,
  scheme_let_void_type,
  scheme_letrec_type, /* 10 */
  scheme_let_one_type,

  _scheme_values_types_, /* All following types are values */
  
  /* intermediate compiled: */
  scheme_compiled_unclosed_procedure_type,
  scheme_compiled_let_value_type,
  scheme_compiled_let_void_type,
  scheme_compiled_syntax_type,

  scheme_quote_compilation_type,

  _scheme_compiled_values_types_,

  /* procedure types */
  scheme_prim_type, /* 19 */
  scheme_closed_prim_type, /* 20 */
  scheme_linked_closure_type,
  scheme_case_closure_type,
  scheme_cont_type,
  scheme_escaping_cont_type,

  /* basic types */
  scheme_char_type, /* 25 */
  scheme_integer_type,
  scheme_bignum_type,
  scheme_rational_type,
  scheme_float_type,
  scheme_double_type, /* 30 */
  scheme_complex_type,
  scheme_string_type,
  scheme_symbol_type,
  scheme_null_type,
  scheme_pair_type,
  scheme_vector_type,
  scheme_closure_type,
  scheme_input_port_type,
  scheme_output_port_type,
  scheme_eof_type,   /* 40 */
  scheme_true_type,
  scheme_false_type, 
  scheme_void_type,
  scheme_syntax_compiler_type,
  scheme_macro_type,
  scheme_promise_type,
  scheme_box_type,
  scheme_process_type,
  scheme_object_type,
  scheme_class_type,  /* 50 */
  scheme_structure_type,
  scheme_generic_type, 
  scheme_type_symbol_type, 
  scheme_sema_type,
  scheme_hash_table_type,
  scheme_generic_data_type,
  scheme_weak_box_type, 
  scheme_struct_type_type,
  scheme_id_macro_type,
  scheme_unit_type,  /* 60 */
  scheme_exp_time_type,
  scheme_listener_type,
  scheme_namespace_type, 
  scheme_config_type,
  scheme_defaulting_config_type, 
  scheme_will_executor_type,
  scheme_interface_type,
  scheme_manager_type,

  /* These reserved types will let us add types
     without forcing recompilation of compiled MzScheme code */
  scheme_reserved_4_type,

  /* more internal types: */
  scheme_compilation_top_type, /* 70 */

  scheme_envunbox_type,
  scheme_eval_waiting_type,
  scheme_tail_call_waiting_type,
  scheme_class_data_type,
  scheme_undefined_type, 
  scheme_struct_info_type,
  scheme_multiple_values_type,
  scheme_reserved_5_type,
  scheme_placeholder_type,
  scheme_case_lambda_sequence_type, /* 80 */
  scheme_begin0_sequence_type,

  scheme_compiled_unit_type,
  scheme_unit_body_data_type,
  scheme_unit_body_closure_data_type,
  scheme_unit_compound_data_type,
  scheme_invoke_unit_data_type,

  scheme_interface_data_type,

  scheme_svector_type,

  _scheme_last_type_
};

extern char *scheme_get_type_name(Scheme_Type type);
