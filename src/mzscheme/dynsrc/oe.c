
#include "escheme.h"

/* Perhaps we should register these globals for GC. Turns out
   to not matter since they're guranteed to be registered
   via another path. */
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

  p = SCHEME_GLOBAL(zerop_g);
  if (!p)
    scheme_signal_error("odd: zero? undefined");
  array[0] = argv[0];
  v = _scheme_apply(p, 1, array);

  if (SCHEME_TRUEP(v)) {
    return scheme_false;
  } else {
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

static Scheme_Object *create_tclass(void *a, int argc, Scheme_Object **argv)
{
  return scheme_create_class((struct Scheme_Class_Assembly *)a, argv[0], argv + 1);
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
			Scheme_Object *instance)
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

void make_number_indices(void *v, int *h1, int *h2)
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


Scheme_Object *scheme_initialize(Scheme_Env *global_env)
{
  Scheme_Object *mv[2];
  Scheme_Hash_Table *table;

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
    
    v = (void *)scheme_make_class_assembly(1, /* 1 interface */
					   6, pnames,
					   2, inames,
					   2, 2,
					   init_tclass);
    
    scheme_add_global("mktclass",
		      scheme_make_closed_prim_w_arity(create_tclass, v, "mktclass", 2, 2),
		      global_env);

    pnames[0] = scheme_intern_symbol("get-x1");
    pnames[1] = scheme_intern_symbol("get-x2");

    v = (void *)scheme_make_interface_assembly(1, 2, pnames);

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
