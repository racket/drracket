  scheme_extension_table->scheme_init_jmpup_buf = scheme_init_jmpup_buf;
  scheme_extension_table->scheme_setjmpup_relative = scheme_setjmpup_relative;
  scheme_extension_table->scheme_longjmpup = scheme_longjmpup;
  scheme_extension_table->scheme_reset_jmpup_buf = scheme_reset_jmpup_buf;
#ifdef USE_MZ_SETJMP
  scheme_extension_table->scheme_setjmp = scheme_setjmp;
  scheme_extension_table->scheme_longjmp = scheme_longjmp;
#endif
  scheme_extension_table->scheme_clear_escape = scheme_clear_escape;
  scheme_extension_table->scheme_make_config = scheme_make_config;
  scheme_extension_table->scheme_branch_config = scheme_branch_config;
  scheme_extension_table->scheme_new_param = scheme_new_param;
  scheme_extension_table->scheme_param_config = scheme_param_config;
  scheme_extension_table->scheme_register_parameter = scheme_register_parameter;
  scheme_extension_table->scheme_get_env = scheme_get_env;
#ifdef MZ_REAL_THREADS
  scheme_extension_table->scheme_get_current_process = scheme_get_current_process;
#else
#ifndef LINK_EXTENSIONS_BY_TABLE
  scheme_extension_table->scheme_current_process = scheme_current_process;
  scheme_extension_table->scheme_fuel_counter = scheme_fuel_counter;
#else
  scheme_extension_table->scheme_current_process_ptr = scheme_current_process_ptr;
  scheme_extension_table->scheme_fuel_counter_ptr = scheme_fuel_counter_ptr;
#endif
#endif
#ifndef NO_SCHEME_THREADS
  scheme_extension_table->scheme_thread = scheme_thread;
  scheme_extension_table->scheme_thread_w_manager = scheme_thread_w_manager;
  scheme_extension_table->scheme_kill_thread = scheme_kill_thread;
#endif
  scheme_extension_table->scheme_break_thread = scheme_break_thread;
#ifndef MZ_REAL_THREADS
  scheme_extension_table->scheme_process_block = scheme_process_block;
  scheme_extension_table->scheme_swap_process = scheme_swap_process;
#else
  scheme_extension_table->scheme_process_block_w_process = scheme_process_block_w_process;
#endif
  scheme_extension_table->scheme_weak_suspend_thread = scheme_weak_suspend_thread;
  scheme_extension_table->scheme_weak_resume_thread = scheme_weak_resume_thread;
  scheme_extension_table->scheme_block_until = scheme_block_until;
  scheme_extension_table->scheme_in_main_thread = scheme_in_main_thread;
  scheme_extension_table->scheme_tls_allocate = scheme_tls_allocate;
  scheme_extension_table->scheme_tls_set = scheme_tls_set;
  scheme_extension_table->scheme_tls_get = scheme_tls_get;
  scheme_extension_table->scheme_make_manager = scheme_make_manager;
  scheme_extension_table->scheme_add_managed = scheme_add_managed;
  scheme_extension_table->scheme_remove_managed = scheme_remove_managed;
  scheme_extension_table->scheme_close_managed = scheme_close_managed;
  scheme_extension_table->scheme_signal_error = scheme_signal_error;
  scheme_extension_table->scheme_raise_exn = scheme_raise_exn;
  scheme_extension_table->scheme_warning = scheme_warning;
  scheme_extension_table->scheme_wrong_count = scheme_wrong_count;
  scheme_extension_table->scheme_case_lambda_wrong_count = scheme_case_lambda_wrong_count;
  scheme_extension_table->scheme_wrong_type = scheme_wrong_type;
  scheme_extension_table->scheme_arg_mismatch = scheme_arg_mismatch;
  scheme_extension_table->scheme_wrong_return_arity = scheme_wrong_return_arity;
  scheme_extension_table->scheme_unbound_global = scheme_unbound_global;
  scheme_extension_table->scheme_dynamic_wind = scheme_dynamic_wind;
  scheme_extension_table->scheme_make_type = scheme_make_type;
  scheme_extension_table->scheme_install_type_reader = scheme_install_type_reader;
  scheme_extension_table->scheme_install_type_writer = scheme_install_type_writer;
  scheme_extension_table->scheme_eof = scheme_eof;
  scheme_extension_table->scheme_null = scheme_null;
  scheme_extension_table->scheme_true = scheme_true;
  scheme_extension_table->scheme_false = scheme_false;
  scheme_extension_table->scheme_void = scheme_void;
  scheme_extension_table->scheme_undefined = scheme_undefined;
  scheme_extension_table->scheme_tail_call_waiting = scheme_tail_call_waiting;
  scheme_extension_table->scheme_multiple_values = scheme_multiple_values;
  scheme_extension_table->scheme_eval = scheme_eval;
  scheme_extension_table->scheme_eval_multi = scheme_eval_multi;
  scheme_extension_table->scheme_eval_compiled = scheme_eval_compiled;
  scheme_extension_table->scheme_eval_compiled_multi = scheme_eval_compiled_multi;
  scheme_extension_table->_scheme_eval_compiled = _scheme_eval_compiled;
  scheme_extension_table->_scheme_eval_compiled_multi = _scheme_eval_compiled_multi;
#ifndef MZ_REAL_THREADS
  scheme_extension_table->scheme_apply = scheme_apply;
  scheme_extension_table->scheme_apply_multi = scheme_apply_multi;
  scheme_extension_table->scheme_apply_eb = scheme_apply_eb;
  scheme_extension_table->scheme_apply_multi_eb = scheme_apply_multi_eb;
#else
  scheme_extension_table->scheme_apply_wp = scheme_apply_wp;
  scheme_extension_table->scheme_apply_multi_wp = scheme_apply_multi_wp;
  scheme_extension_table->scheme_apply_eb_wp = scheme_apply_eb_wp;
  scheme_extension_table->scheme_apply_multi_eb_wp = scheme_apply_multi_eb_wp;
#endif
  scheme_extension_table->scheme_apply_to_list = scheme_apply_to_list;
  scheme_extension_table->scheme_eval_string = scheme_eval_string;
  scheme_extension_table->scheme_eval_string_multi = scheme_eval_string_multi;
  scheme_extension_table->scheme_eval_string_all = scheme_eval_string_all;
  scheme_extension_table->_scheme_apply_known_closed_prim = _scheme_apply_known_closed_prim;
  scheme_extension_table->_scheme_apply_known_closed_prim_multi = _scheme_apply_known_closed_prim_multi;
  scheme_extension_table->_scheme_apply_closed_prim = _scheme_apply_closed_prim;
  scheme_extension_table->_scheme_apply_closed_prim_multi = _scheme_apply_closed_prim_multi;
  scheme_extension_table->scheme_values = scheme_values;
  scheme_extension_table->scheme_check_one_value = scheme_check_one_value;
  scheme_extension_table->scheme_tail_apply = scheme_tail_apply;
  scheme_extension_table->scheme_tail_apply_no_copy = scheme_tail_apply_no_copy;
  scheme_extension_table->scheme_tail_apply_to_list = scheme_tail_apply_to_list;
  scheme_extension_table->scheme_tail_eval_expr = scheme_tail_eval_expr;
  scheme_extension_table->scheme_set_tail_buffer_size = scheme_set_tail_buffer_size;
  scheme_extension_table->scheme_force_value = scheme_force_value;
  scheme_extension_table->scheme_set_cont_mark = scheme_set_cont_mark;
  scheme_extension_table->scheme_push_continuation_frame = scheme_push_continuation_frame;
  scheme_extension_table->scheme_pop_continuation_frame = scheme_pop_continuation_frame;
  scheme_extension_table->scheme_temp_dec_mark_depth = scheme_temp_dec_mark_depth;
  scheme_extension_table->scheme_temp_inc_mark_depth = scheme_temp_inc_mark_depth;
  scheme_extension_table->scheme_current_continuation_marks = scheme_current_continuation_marks;
#ifndef MZ_REAL_THREADS
  scheme_extension_table->scheme_do_eval = scheme_do_eval;
#else
  scheme_extension_table->scheme_do_eval_w_process = scheme_do_eval_w_process;
#endif
#ifndef SCHEME_NO_GC
# ifndef SCHEME_NO_GC_PROTO
  scheme_extension_table->GC_malloc = GC_malloc;
  scheme_extension_table->GC_malloc_atomic = GC_malloc_atomic;
#  ifdef MZ_PRECISE_GC
  scheme_extension_table->GC_malloc_one_tagged = GC_malloc_one_tagged;
  scheme_extension_table->GC_malloc_atomic_uncollectable = GC_malloc_atomic_uncollectable;
  scheme_extension_table->GC_malloc_array_tagged = GC_malloc_array_tagged;
#  else
  scheme_extension_table->GC_malloc_stubborn = GC_malloc_stubborn;
  scheme_extension_table->GC_malloc_uncollectable = GC_malloc_uncollectable;
#  endif
# endif
#endif
  scheme_extension_table->scheme_malloc_eternal = scheme_malloc_eternal;
  scheme_extension_table->scheme_end_stubborn_change = scheme_end_stubborn_change;
  scheme_extension_table->scheme_calloc = scheme_calloc;
  scheme_extension_table->scheme_strdup = scheme_strdup;
  scheme_extension_table->scheme_strdup_eternal = scheme_strdup_eternal;
  scheme_extension_table->scheme_malloc_fail_ok = scheme_malloc_fail_ok;
  scheme_extension_table->scheme_weak_reference = scheme_weak_reference;
  scheme_extension_table->scheme_weak_reference_indirect = scheme_weak_reference_indirect;
  scheme_extension_table->scheme_unweak_reference = scheme_unweak_reference;
  scheme_extension_table->scheme_add_finalizer = scheme_add_finalizer;
  scheme_extension_table->scheme_add_finalizer_once = scheme_add_finalizer_once;
  scheme_extension_table->scheme_add_scheme_finalizer = scheme_add_scheme_finalizer;
  scheme_extension_table->scheme_add_scheme_finalizer_once = scheme_add_scheme_finalizer_once;
  scheme_extension_table->scheme_register_finalizer = scheme_register_finalizer;
  scheme_extension_table->scheme_remove_all_finalization = scheme_remove_all_finalization;
  scheme_extension_table->scheme_dont_gc_ptr = scheme_dont_gc_ptr;
  scheme_extension_table->scheme_gc_ptr_ok = scheme_gc_ptr_ok;
  scheme_extension_table->scheme_collect_garbage = scheme_collect_garbage;
  scheme_extension_table->scheme_hash_table = scheme_hash_table;
  scheme_extension_table->scheme_add_to_table = scheme_add_to_table;
  scheme_extension_table->scheme_change_in_table = scheme_change_in_table;
  scheme_extension_table->scheme_lookup_in_table = scheme_lookup_in_table;
  scheme_extension_table->scheme_bucket_from_table = scheme_bucket_from_table;
  scheme_extension_table->scheme_make_prim = scheme_make_prim;
  scheme_extension_table->scheme_make_noneternal_prim = scheme_make_noneternal_prim;
  scheme_extension_table->scheme_make_closed_prim = scheme_make_closed_prim;
  scheme_extension_table->scheme_make_prim_w_arity = scheme_make_prim_w_arity;
  scheme_extension_table->scheme_make_folding_prim = scheme_make_folding_prim;
  scheme_extension_table->scheme_make_noneternal_prim_w_arity = scheme_make_noneternal_prim_w_arity;
  scheme_extension_table->scheme_make_closed_prim_w_arity = scheme_make_closed_prim_w_arity;
  scheme_extension_table->scheme_make_folding_closed_prim = scheme_make_folding_closed_prim;
  scheme_extension_table->scheme_make_closure = scheme_make_closure;
  scheme_extension_table->scheme_make_pair = scheme_make_pair;
  scheme_extension_table->scheme_make_string = scheme_make_string;
  scheme_extension_table->scheme_make_sized_string = scheme_make_sized_string;
  scheme_extension_table->scheme_make_sized_offset_string = scheme_make_sized_offset_string;
  scheme_extension_table->scheme_make_immutable_sized_string = scheme_make_immutable_sized_string;
  scheme_extension_table->scheme_make_string_without_copying = scheme_make_string_without_copying;
  scheme_extension_table->scheme_alloc_string = scheme_alloc_string;
  scheme_extension_table->scheme_append_string = scheme_append_string;
  scheme_extension_table->scheme_make_vector = scheme_make_vector;
  scheme_extension_table->scheme_make_integer_value = scheme_make_integer_value;
  scheme_extension_table->scheme_make_integer_value_from_unsigned = scheme_make_integer_value_from_unsigned;
  scheme_extension_table->scheme_make_double = scheme_make_double;
#ifdef MZ_USE_SINGLE_FLOATS
  scheme_extension_table->scheme_make_float = scheme_make_float;
#endif
  scheme_extension_table->scheme_make_char = scheme_make_char;
  scheme_extension_table->scheme_make_promise = scheme_make_promise;
  scheme_extension_table->scheme_make_promise_from_thunk = scheme_make_promise_from_thunk;
#ifndef NO_SCHEME_THREADS
  scheme_extension_table->scheme_make_sema = scheme_make_sema;
  scheme_extension_table->scheme_post_sema = scheme_post_sema;
  scheme_extension_table->scheme_wait_sema = scheme_wait_sema;
#endif
  scheme_extension_table->scheme_char_constants = scheme_char_constants;
  scheme_extension_table->scheme_get_int_val = scheme_get_int_val;
  scheme_extension_table->scheme_get_unsigned_int_val = scheme_get_unsigned_int_val;
  scheme_extension_table->scheme_real_to_double = scheme_real_to_double;
  scheme_extension_table->scheme_get_proc_name = scheme_get_proc_name;
  scheme_extension_table->scheme_make_bignum = scheme_make_bignum;
  scheme_extension_table->scheme_make_bignum_from_unsigned = scheme_make_bignum_from_unsigned;
  scheme_extension_table->scheme_bignum_to_double = scheme_bignum_to_double;
  scheme_extension_table->scheme_bignum_from_double = scheme_bignum_from_double;
#ifdef MZ_USE_SINGLE_FLOATS
  scheme_extension_table->scheme_bignum_to_float = scheme_bignum_to_float;
  scheme_extension_table->scheme_bignum_from_float = scheme_bignum_from_float;
#else
# define scheme_bignum_to_float scheme_bignum_to_double
# define scheme_bignum_from_float scheme_bignum_from_double
#endif
  scheme_extension_table->scheme_bignum_to_string = scheme_bignum_to_string;
  scheme_extension_table->scheme_read_bignum = scheme_read_bignum;
  scheme_extension_table->scheme_bignum_normalize = scheme_bignum_normalize;
  scheme_extension_table->scheme_double_to_int = scheme_double_to_int;
  scheme_extension_table->scheme_make_rational = scheme_make_rational;
  scheme_extension_table->scheme_rational_to_double = scheme_rational_to_double;
  scheme_extension_table->scheme_rational_from_double = scheme_rational_from_double;
#ifdef MZ_USE_SINGLE_FLOATS
  scheme_extension_table->scheme_rational_to_float = scheme_rational_to_float;
  scheme_extension_table->scheme_rational_from_float = scheme_rational_from_float;
#else
# define scheme_rational_to_float scheme_rational_to_double
# define scheme_rational_from_float scheme_rational_from_double
#endif
  scheme_extension_table->scheme_rational_normalize = scheme_rational_normalize;
  scheme_extension_table->scheme_rational_numerator = scheme_rational_numerator;
  scheme_extension_table->scheme_rational_denominator = scheme_rational_denominator;
  scheme_extension_table->scheme_make_complex = scheme_make_complex;
  scheme_extension_table->scheme_complex_normalize = scheme_complex_normalize;
  scheme_extension_table->scheme_complex_real_part = scheme_complex_real_part;
  scheme_extension_table->scheme_complex_imaginary_part = scheme_complex_imaginary_part;
  scheme_extension_table->scheme_is_exact = scheme_is_exact;
  scheme_extension_table->scheme_is_inexact = scheme_is_inexact;
  scheme_extension_table->scheme_expand = scheme_expand;
  scheme_extension_table->scheme_compile = scheme_compile;
  scheme_extension_table->scheme_make_promise_value = scheme_make_promise_value;
  scheme_extension_table->scheme_read = scheme_read;
  scheme_extension_table->scheme_write = scheme_write;
  scheme_extension_table->scheme_display = scheme_display;
  scheme_extension_table->scheme_write_w_max = scheme_write_w_max;
  scheme_extension_table->scheme_display_w_max = scheme_display_w_max;
  scheme_extension_table->scheme_write_string = scheme_write_string;
  scheme_extension_table->scheme_write_offset_string = scheme_write_offset_string;
  scheme_extension_table->scheme_write_to_string = scheme_write_to_string;
  scheme_extension_table->scheme_display_to_string = scheme_display_to_string;
  scheme_extension_table->scheme_write_to_string_w_max = scheme_write_to_string_w_max;
  scheme_extension_table->scheme_display_to_string_w_max = scheme_display_to_string_w_max;
  scheme_extension_table->scheme_debug_print = scheme_debug_print;
  scheme_extension_table->scheme_flush_output = scheme_flush_output;
  scheme_extension_table->scheme_format = scheme_format;
  scheme_extension_table->scheme_printf = scheme_printf;
  scheme_extension_table->scheme_getc = scheme_getc;
  scheme_extension_table->scheme_peekc = scheme_peekc;
  scheme_extension_table->scheme_ungetc = scheme_ungetc;
  scheme_extension_table->scheme_char_ready = scheme_char_ready;
  scheme_extension_table->scheme_peekc_is_ungetc = scheme_peekc_is_ungetc;
  scheme_extension_table->scheme_need_wakeup = scheme_need_wakeup;
  scheme_extension_table->scheme_get_chars = scheme_get_chars;
  scheme_extension_table->scheme_tell = scheme_tell;
  scheme_extension_table->scheme_output_tell = scheme_output_tell;
  scheme_extension_table->scheme_tell_line = scheme_tell_line;
  scheme_extension_table->scheme_count_lines = scheme_count_lines;
  scheme_extension_table->scheme_close_input_port = scheme_close_input_port;
  scheme_extension_table->scheme_close_output_port = scheme_close_output_port;
  scheme_extension_table->scheme_are_all_chars_ready = scheme_are_all_chars_ready;
  scheme_extension_table->scheme_make_port_type = scheme_make_port_type;
  scheme_extension_table->scheme_make_input_port = scheme_make_input_port;
  scheme_extension_table->scheme_make_output_port = scheme_make_output_port;
  scheme_extension_table->scheme_make_file_input_port = scheme_make_file_input_port;
  scheme_extension_table->scheme_make_named_file_input_port = scheme_make_named_file_input_port;
  scheme_extension_table->scheme_make_file_output_port = scheme_make_file_output_port;
  scheme_extension_table->scheme_make_string_input_port = scheme_make_string_input_port;
  scheme_extension_table->scheme_make_sized_string_input_port = scheme_make_sized_string_input_port;
  scheme_extension_table->scheme_make_string_output_port = scheme_make_string_output_port;
  scheme_extension_table->scheme_get_string_output = scheme_get_string_output;
  scheme_extension_table->scheme_get_sized_string_output = scheme_get_sized_string_output;
  scheme_extension_table->scheme_pipe = scheme_pipe;
  scheme_extension_table->scheme_file_exists = scheme_file_exists;
  scheme_extension_table->scheme_directory_exists = scheme_directory_exists;
  scheme_extension_table->scheme_expand_filename = scheme_expand_filename;
  scheme_extension_table->scheme_os_getcwd = scheme_os_getcwd;
  scheme_extension_table->scheme_os_setcwd = scheme_os_setcwd;
  scheme_extension_table->scheme_getdrive = scheme_getdrive;
  scheme_extension_table->scheme_split_pathname = scheme_split_pathname;
  scheme_extension_table->scheme_build_pathname = scheme_build_pathname;
  scheme_extension_table->scheme_alloc_fdset_array = scheme_alloc_fdset_array;
  scheme_extension_table->scheme_init_fdset_array = scheme_init_fdset_array;
  scheme_extension_table->scheme_get_fdset = scheme_get_fdset;
  scheme_extension_table->scheme_fdzero = scheme_fdzero;
  scheme_extension_table->scheme_fdset = scheme_fdset;
  scheme_extension_table->scheme_fdclr = scheme_fdclr;
  scheme_extension_table->scheme_fdisset = scheme_fdisset;
  scheme_extension_table->scheme_add_fd_handle = scheme_add_fd_handle;
  scheme_extension_table->scheme_add_fd_eventmask = scheme_add_fd_eventmask;
  scheme_extension_table->scheme_return_eof_for_error = scheme_return_eof_for_error;
  scheme_extension_table->scheme_make_namespace = scheme_make_namespace;
  scheme_extension_table->scheme_add_namespace_option = scheme_add_namespace_option;
  scheme_extension_table->scheme_add_global = scheme_add_global;
  scheme_extension_table->scheme_add_global_constant = scheme_add_global_constant;
  scheme_extension_table->scheme_add_global_keyword = scheme_add_global_keyword;
  scheme_extension_table->scheme_remove_global = scheme_remove_global;
  scheme_extension_table->scheme_remove_global_constant = scheme_remove_global_constant;
  scheme_extension_table->scheme_add_global_symbol = scheme_add_global_symbol;
  scheme_extension_table->scheme_remove_global_symbol = scheme_remove_global_symbol;
  scheme_extension_table->scheme_add_global_constant_symbol = scheme_add_global_constant_symbol;
  scheme_extension_table->scheme_set_keyword = scheme_set_keyword;
  scheme_extension_table->scheme_make_envunbox = scheme_make_envunbox;
  scheme_extension_table->scheme_lookup_global = scheme_lookup_global;
  scheme_extension_table->scheme_global_bucket = scheme_global_bucket;
  scheme_extension_table->scheme_set_global_bucket = scheme_set_global_bucket;
  scheme_extension_table->scheme_intern_symbol = scheme_intern_symbol;
  scheme_extension_table->scheme_intern_exact_symbol = scheme_intern_exact_symbol;
  scheme_extension_table->scheme_make_symbol = scheme_make_symbol;
  scheme_extension_table->scheme_make_exact_symbol = scheme_make_exact_symbol;
  scheme_extension_table->scheme_symbol_name = scheme_symbol_name;
  scheme_extension_table->scheme_symbol_name_and_size = scheme_symbol_name_and_size;
  scheme_extension_table->scheme_symbol_val = scheme_symbol_val;
  scheme_extension_table->scheme_make_struct_values = scheme_make_struct_values;
  scheme_extension_table->scheme_make_struct_names = scheme_make_struct_names;
  scheme_extension_table->scheme_make_struct_type = scheme_make_struct_type;
  scheme_extension_table->scheme_make_struct_instance = scheme_make_struct_instance;
  scheme_extension_table->scheme_is_struct_instance = scheme_is_struct_instance;
  scheme_extension_table->scheme_struct_ref = scheme_struct_ref;
  scheme_extension_table->scheme_struct_set = scheme_struct_set;
#ifndef NO_OBJECT_SYSTEM
  scheme_extension_table->scheme_is_subclass = scheme_is_subclass;
  scheme_extension_table->scheme_is_implementation = scheme_is_implementation;
  scheme_extension_table->scheme_is_interface_extension = scheme_is_interface_extension;
  scheme_extension_table->scheme_is_a = scheme_is_a;
  scheme_extension_table->scheme_get_class_name = scheme_get_class_name;
  scheme_extension_table->scheme_get_interface_name = scheme_get_interface_name;
  scheme_extension_table->scheme_make_object = scheme_make_object;
  scheme_extension_table->scheme_make_uninited_object = scheme_make_uninited_object;
  scheme_extension_table->scheme_find_ivar = scheme_find_ivar;
  scheme_extension_table->scheme_make_class = scheme_make_class;
  scheme_extension_table->scheme_add_method = scheme_add_method;
  scheme_extension_table->scheme_add_method_w_arity = scheme_add_method_w_arity;
  scheme_extension_table->scheme_made_class = scheme_made_class;
  scheme_extension_table->scheme_class_to_interface = scheme_class_to_interface;
  scheme_extension_table->scheme_make_class_assembly = scheme_make_class_assembly;
  scheme_extension_table->scheme_create_class = scheme_create_class;
  scheme_extension_table->scheme_make_interface_assembly = scheme_make_interface_assembly;
  scheme_extension_table->scheme_create_interface = scheme_create_interface;
  scheme_extension_table->scheme_apply_generic_data = scheme_apply_generic_data;
  scheme_extension_table->scheme_get_generic_data = scheme_get_generic_data;
#endif
  scheme_extension_table->scheme_invoke_unit = scheme_invoke_unit;
  scheme_extension_table->scheme_assemble_compound_unit = scheme_assemble_compound_unit;
  scheme_extension_table->scheme_make_compound_unit = scheme_make_compound_unit;
  scheme_extension_table->scheme_get_unit_name = scheme_get_unit_name;
  scheme_extension_table->scheme_eq = scheme_eq;
  scheme_extension_table->scheme_eqv = scheme_eqv;
  scheme_extension_table->scheme_equal = scheme_equal;
  scheme_extension_table->scheme_build_list = scheme_build_list;
  scheme_extension_table->scheme_list_length = scheme_list_length;
  scheme_extension_table->scheme_proper_list_length = scheme_proper_list_length;
  scheme_extension_table->scheme_alloc_list = scheme_alloc_list;
  scheme_extension_table->scheme_map_1 = scheme_map_1;
  scheme_extension_table->scheme_car = scheme_car;
  scheme_extension_table->scheme_cdr = scheme_cdr;
  scheme_extension_table->scheme_cadr = scheme_cadr;
  scheme_extension_table->scheme_caddr = scheme_caddr;
  scheme_extension_table->scheme_vector_to_list = scheme_vector_to_list;
  scheme_extension_table->scheme_list_to_vector = scheme_list_to_vector;
  scheme_extension_table->scheme_append = scheme_append;
  scheme_extension_table->scheme_box = scheme_box;
  scheme_extension_table->scheme_unbox = scheme_unbox;
  scheme_extension_table->scheme_set_box = scheme_set_box;
  scheme_extension_table->scheme_make_weak_box = scheme_make_weak_box;
  scheme_extension_table->scheme_load = scheme_load;
  scheme_extension_table->scheme_load_extension = scheme_load_extension;
  scheme_extension_table->scheme_register_extension_global = scheme_register_extension_global;
  scheme_extension_table->scheme_get_milliseconds = scheme_get_milliseconds;
  scheme_extension_table->scheme_get_process_milliseconds = scheme_get_process_milliseconds;
  scheme_extension_table->scheme_rep = scheme_rep;
  scheme_extension_table->scheme_banner = scheme_banner;
  scheme_extension_table->scheme_version = scheme_version;
  scheme_extension_table->scheme_check_proc_arity = scheme_check_proc_arity;
  scheme_extension_table->scheme_make_provided_string = scheme_make_provided_string;
  scheme_extension_table->scheme_make_args_string = scheme_make_args_string;
  scheme_extension_table->scheme_no_dumps = scheme_no_dumps;
  scheme_extension_table->scheme_system_library_subpath = scheme_system_library_subpath;
