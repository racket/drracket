
/* This serves as a vague test of MzScheme's FFI, so it shows how to
   do especially stange things, such as defining Scheme classes and
   interfaces from C. It's somewhat annotated, but it's not good
   example code in general.

   For example extensions, see plt/collects/mzscheme/examples. */

#include "escheme.h"

/* Note that all these are regsitered with the GC in scheme_initialize: */
static Scheme_Object *one_const, *two_const;
static Scheme_Bucket *zerop_g, *minus_g, *plus_g, *le_g;
static Scheme_Bucket *odd_g, *even_g, *fib_g, *mod_g, *eq_g;
static Scheme_Prim *le_f, *minus_f, *plus_f;

#define SCHEME_GLOBAL(b) b->val

Scheme_Object *odd(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v, *p, *array[2];

  if (argc != 1)
    scheme_wrong_count("odd", 1, 1, argc, argv);

  /* The following checks whether the argument is zero. If we accepted
     only exact integeres, a simpler test would be:
     (SCHEME_INTP(argv[0]) && !SCHEME_INT_VAL(argv[0])). */

  /* We could look up `zero?' with
     scheme_lookup_global(scheme_intern_symbol("zero?"), scheme_env)
     but scheme_initialize below has already resolved `zero?' to a
     specific global variable "bucket". We just need to extract the
     procedure from the bucket: */
  p = SCHEME_GLOBAL(zerop_g);
  if (!p) /* unlikely */
    scheme_signal_error("odd: zero? undefined");
  array[0] = argv[0];
  /* Apply `zero?' to the argument. Note the leading "_" in
     "_scheme_apply"; that allows exceptions and full continuations to
     work. It's also a lot faster than "scheme_apply". */
  v = _scheme_apply(p, 1, array);

  if (SCHEME_TRUEP(v)) {
    /* If the argument was zero, then it wasn't odd. */
    return scheme_false;
  } else {
    /* Another application... */
    p = SCHEME_GLOBAL(minus_g);
    if (!p)
      scheme_signal_error("odd: - undefined");
    array[0] = argv[0];
    array[1] = one_const;
    v = _scheme_apply(p, 2, array);

    p = SCHEME_GLOBAL(even_g);
    if (!p)
      scheme_signal_error("odd: even undefined");
    
    array[0] = v;
    return scheme_tail_apply(p, 1, array);
  }
}

/* Practically the same as odd. */
Scheme_Object *even(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v, *p, *array[2];

  if (argc != 1)
    scheme_wrong_count("even", 1, 1, argc, argv);

  p = SCHEME_GLOBAL(zerop_g);
  if (!p)
    scheme_signal_error("even: zero? undefined");
  array[0] = argv[0];
  v = _scheme_apply(p, 1, array);

  if (SCHEME_TRUEP(v)) {
    return scheme_true;
  } else {
    p = SCHEME_GLOBAL(minus_g);
    if (!p)
      scheme_signal_error("even: - undefined");
    array[0] = argv[0];
    array[1] = one_const;
    v = _scheme_apply(p, 2, array);

    p = SCHEME_GLOBAL(odd_g);
    if (!p)
      scheme_signal_error("even: odd undefined");

    array[0] = v;
    return scheme_tail_apply(p, 1, array);
  }
}

Scheme_Object *fib(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v, *v2, *p, *array[2];

  if (argc != 1)
    scheme_wrong_count("fib", 1, 1, argc, argv);

  p = SCHEME_GLOBAL(le_g);
  if (!p)
    scheme_signal_error("fib: <= undefined");
  array[0] = argv[0];
  array[1] = one_const;
  v = _scheme_apply(p, 2, array);

  if (SCHEME_TRUEP(v)) {
    return one_const;
  } else {
    p = SCHEME_GLOBAL(minus_g);
    if (!p)
      scheme_signal_error("fib: - undefined");
    array[0] = argv[0];
    array[1] = one_const;
    v = _scheme_apply(p, 2, array);
    
    p = SCHEME_GLOBAL(fib_g);
    if (!p)
      scheme_signal_error("fib: fib undefined");
    array[0] = v;
    v = _scheme_apply(p, 1, array);

    p = SCHEME_GLOBAL(minus_g);
    if (!p)
      scheme_signal_error("fib: - undefined");
    array[0] = argv[0];
    array[1] = two_const;
    v2 = _scheme_apply(p, 2, array);
    
    p = SCHEME_GLOBAL(fib_g);
    if (!p)
      scheme_signal_error("fib: fib undefined");
    array[0] = v2;
    v2 = _scheme_apply(p, 1, array);

    p = SCHEME_GLOBAL(plus_g);
    if (!p)
      scheme_signal_error("fib: + undefined");
    array[0] = v;
    array[1] = v2;
    return scheme_tail_apply(p, 2, array);
  }
}

Scheme_Object *fib2(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v, *v2, *array[2];

  if (argc != 1)
    scheme_wrong_count("fib", 1, 1, argc, argv);

  /* In this case, scheme_initialize has resolved `<' even further
     than getting a global variable bucket. It has extracted the
     low-level implementation so it can call the function directly.
     You don't usually want to do that, as it may not be clear whether
     `<' is bound to the primitive implementation or not. */
  array[0] = argv[0];
  array[1] = one_const;
  v = le_f(2, array);

  if (SCHEME_TRUEP(v)) {
    return one_const;
  } else {
    array[0] = argv[0];
    array[1] = one_const;
    v = minus_f(2, array);
    
    array[0] = v;
    v = fib2(1, array);

    array[0] = argv[0];
    array[1] = two_const;
    v2 = minus_f(2, array);
    
    array[0] = v2;
    v2 = fib2(1, array);

    array[0] = v;
    array[1] = v2;
    return plus_f(2, array);
  }
}

/*********************************************************************/
/* Example exception-catching code                                   */
/*********************************************************************/

static Scheme_Object *exn_catching_apply, *exn_p, *exn_message;

static void init_exn_catching_apply()
{
  if (!exn_catching_apply) {
    char *e = 
      "(#%lambda (thunk) "
	"(#%with-handlers ([#%void (#%lambda (exn) (#%cons #f exn))]) "
	  "(#%cons #t (thunk))))";
    /* make sure we have a namespace with the standard syntax: */
    Scheme_Env *env = (Scheme_Env *)scheme_make_namespace(0, NULL);

#if !SCHEME_DIRECT_EMBEDDED
    scheme_register_extension_global(&exn_catching_apply, sizeof(Scheme_Object *));
    scheme_register_extension_global(&exn_p, sizeof(Scheme_Object *));
    scheme_register_extension_global(&exn_message, sizeof(Scheme_Object *));
#endif
    
    exn_catching_apply = scheme_eval_string(e, env);
    exn_p = scheme_lookup_global(scheme_intern_symbol("exn?"), env);
    exn_message = scheme_lookup_global(scheme_intern_symbol("exn-message"), env);
  }
}

/* This function applies a thunk, returning the Scheme value if there's no exception, 
   otherwise returning NULL and setting *exn to the raised value (usually an exn 
   structure). */
Scheme_Object *_apply_thunk_catch_exceptions(Scheme_Object *f, Scheme_Object **exn)
{
  Scheme_Object *v;

  init_exn_catching_apply();
  
  v = _scheme_apply(exn_catching_apply, 1, &f);
  /* v is a pair: (cons #t value) or (cons #f exn) */

  if (SCHEME_TRUEP(SCHEME_CAR(v)))
    return SCHEME_CDR(v);
  else {
    *exn = SCHEME_CDR(v);
    return NULL;
  }
}

Scheme_Object *extract_exn_message(Scheme_Object *v)
{
  init_exn_catching_apply();

  if (SCHEME_TRUEP(_scheme_apply(exn_p, 1, &v)))
    return _scheme_apply(exn_message, 1, &v);
  else
    return NULL; /* Not an exn structure */
}

/*********************************************************************/
/* Example use of example exception-catching code                    */
/*********************************************************************/

static Scheme_Object *do_eval(void *s, int noargc, Scheme_Object **noargv)
{
  return scheme_eval_string((char *)s, scheme_get_env(scheme_config));
}

static Scheme_Object *eval_string_or_get_exn_message(char *s)
{
  Scheme_Object *v, *exn;

  v = _apply_thunk_catch_exceptions(scheme_make_closed_prim(do_eval, s), &exn);
  /* Got a value? */
  if (v)
    return v;

  v = extract_exn_message(exn);
  /* Got an exn? */
  if (v)
    return v;

  /* `raise' was called on some arbitrary value */
  return exn;
}

static Scheme_Object *catch_eval_error(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("catch-eval-error", "string", 0, argc, argv);

  return eval_string_or_get_exn_message(SCHEME_STR_VAL(argv[0]));
}

/*********************************************************************/

/* Test Scheme class FFI */

static Scheme_Object *create_tclass(void *a, int argc, Scheme_Object **argv)
{
  return scheme_create_class((struct Scheme_Class_Assembly *)a, NULL, argv[0], argv + 1);
}

static Scheme_Object *create_tinterface(void *a, int argc, Scheme_Object **argv)
{
  return scheme_create_interface((struct Scheme_Interface_Assembly *)a, argv);
}

static Scheme_Object *unboxit(void *v, int argc, Scheme_Object **argv)
{
  return SCHEME_ENVBOX_VAL(v);
}

static Scheme_Object *constfunc(void *v, int argc, Scheme_Object **argv)
{
  return (Scheme_Object *)v;
}

static void init_tclass(Scheme_Object **init_boxes,
			Scheme_Object **extract_boxes,
			Scheme_Object *super_init,
			int argc,
			Scheme_Object **argv,
			Scheme_Object *instance,
			void *ignored_data)
{
  SCHEME_ENVBOX_VAL(init_boxes[0]) = scheme_make_integer(5);
  SCHEME_ENVBOX_VAL(init_boxes[1]) = scheme_make_closed_prim_w_arity(unboxit,
                                                                     extract_boxes[0],
                                                                     "get-y",
                                                                     0, 0);
  SCHEME_ENVBOX_VAL(init_boxes[2]) = scheme_make_closed_prim_w_arity(unboxit,
                                                                     extract_boxes[6],
                                                                     "get-z1",
                                                                     0, 0);
  SCHEME_ENVBOX_VAL(init_boxes[3]) = scheme_make_closed_prim_w_arity(unboxit,
                                                                     extract_boxes[7],
                                                                     "get-z2",
                                                                     0, 0);
  SCHEME_ENVBOX_VAL(init_boxes[4]) = scheme_make_closed_prim_w_arity(constfunc,
                                                                     argv[0],
                                                                     "get-x1",
                                                                     0, 0);
  SCHEME_ENVBOX_VAL(init_boxes[5]) = scheme_make_closed_prim_w_arity(constfunc,
                                                                     argv[1],
                                                                     "get-x2",
                                                                     0, 0);

  _scheme_apply(super_init, 0, NULL);
}

void make_number_indices(void *v, long *h1, long *h2)
{
  Scheme_Object *args[2];

  args[0] = (Scheme_Object *)v;
  args[1] = scheme_make_integer(10000);
  
  *h1 = SCHEME_INT_VAL(_scheme_apply(SCHEME_GLOBAL(mod_g), 2, args));
  *h2 = *h1 % 73;
}

int compare_numbers(void *v1, void *v2)
{
  Scheme_Object *args[2];
  
  args[0] = (Scheme_Object *)v1;
  args[1] = (Scheme_Object *)v2;
  
  return SCHEME_FALSEP(_scheme_apply(SCHEME_GLOBAL(eq_g), 2, args));
}

/*********************************************************************/

Scheme_Object *scheme_initialize(Scheme_Env *global_env)
{
  Scheme_Object *mv[2];
  Scheme_Hash_Table *table;

  /* Conservative GC automatically sees local variables, but it
     doesn't see static variables in an extension. They have to be
     explicitly registered. */
#define REGISTER_GLOBAL(x) scheme_register_extension_global((void *)&x, sizeof(x));
  REGISTER_GLOBAL(one_const);
  REGISTER_GLOBAL(two_const);
  REGISTER_GLOBAL(zerop_g);
  REGISTER_GLOBAL(minus_g);
  REGISTER_GLOBAL(plus_g);
  REGISTER_GLOBAL(le_g);
  REGISTER_GLOBAL(odd_g);
  REGISTER_GLOBAL(even_g);
  REGISTER_GLOBAL(fib_g);
  REGISTER_GLOBAL(mod_g);
  REGISTER_GLOBAL(eq_g);
  REGISTER_GLOBAL(le_f);
  REGISTER_GLOBAL(minus_f);
  REGISTER_GLOBAL(plus_f);

  scheme_eval_string("(printf \"loading from~s~n\" "
		     " (current-load-relative-directory))",
		     global_env);

  one_const = scheme_make_integer(1);
  two_const = scheme_make_integer(2);

  scheme_add_global("odd",
		    scheme_make_prim_w_arity(odd, "odd", 1, 1),
		    global_env);

  scheme_add_global("even",
		    scheme_make_prim_w_arity(even, "even", 1, 1),
		    global_env);

  scheme_add_global("fib",
		    scheme_make_prim_w_arity(fib, "fib", 1, 1),
		    global_env);

  scheme_add_global("fib2",
		    scheme_make_prim_w_arity(fib2, "fib2", 1, 1),
		    global_env);

  scheme_add_global("catch-eval-error",
		    scheme_make_prim_w_arity(catch_eval_error, "catch-eval-error", 1, 1),
		    global_env);

  zerop_g = scheme_global_bucket(scheme_intern_symbol("zero?"), global_env);
  even_g = scheme_global_bucket(scheme_intern_symbol("even"), global_env);
  odd_g = scheme_global_bucket(scheme_intern_symbol("odd"), global_env);
  fib_g = scheme_global_bucket(scheme_intern_symbol("fib"), global_env);
  minus_g = scheme_global_bucket(scheme_intern_symbol("-"), global_env);
  plus_g = scheme_global_bucket(scheme_intern_symbol("+"), global_env);
  le_g = scheme_global_bucket(scheme_intern_symbol("<="), global_env);
  mod_g = scheme_global_bucket(scheme_intern_symbol("modulo"), global_env);
  eq_g = scheme_global_bucket(scheme_intern_symbol("="), global_env);

  le_f = SCHEME_PRIM(SCHEME_GLOBAL(le_g));
  minus_f = SCHEME_PRIM(SCHEME_GLOBAL(minus_g));
  plus_f = SCHEME_PRIM(SCHEME_GLOBAL(plus_g));  

  {
      /* (define mktinterface
           (lambda (i)
             (interface i get-x1 get-x2)))
         (define mktclass
          (lambda (c i)
           (class* c (i) (x1 x2) 
            (inherit z1 z2)
            (public [y 5]
                    [get-y (lambda () y)]
                    [get-z1 (lambda () z1)]
                    [get-z2 (lambda () z2)]
                    [get-x1 (lambda () x1)]
                    [get-x2 (lambda () x2)])
            (sequence (super-init))))) */
      
    void *v;
    Scheme_Object *pnames[6], *inames[1];

    pnames[0] = scheme_intern_symbol("y");
    pnames[1] = scheme_intern_symbol("get-y");
    pnames[2] = scheme_intern_symbol("get-z1");
    pnames[3] = scheme_intern_symbol("get-z2");
    pnames[4] = scheme_intern_symbol("get-x1");
    pnames[5] = scheme_intern_symbol("get-x2");
    
    inames[0] = scheme_intern_symbol("z1");
    inames[1] = scheme_intern_symbol("z2");
    
    v = (void *)scheme_make_class_assembly("tclass", /* name */
					   1, /* 1 interface */
					   6, pnames,
					   0, NULL,
					   2, inames,
					   0, NULL, /* renameds */
					   2, 2,
					   init_tclass);
    
    scheme_add_global("mktclass",
		      scheme_make_closed_prim_w_arity(create_tclass, v, "mktclass", 2, 2),
		      global_env);

    pnames[0] = scheme_intern_symbol("get-x1");
    pnames[1] = scheme_intern_symbol("get-x2");

    v = (void *)scheme_make_interface_assembly("tinterface", 1, 2, pnames);

    scheme_add_global("mktinterface",
		      scheme_make_closed_prim_w_arity(create_tinterface, v, "mktinterface", 1, 1),
		      global_env);
  }

  table = scheme_hash_table(10, SCHEME_hash_ptr, 0, 0);
  table->make_hash_indices = make_number_indices;
  table->compare = compare_numbers;

  scheme_add_global("number-table",
		    (Scheme_Object *)table,
		    global_env);

  mv[0] = scheme_true;
  mv[1] = scheme_false;
  return scheme_values(2, mv);
}

Scheme_Object *scheme_reload(Scheme_Env *global_env)
{
  return scheme_intern_symbol("reload-does-nothing");
}
