#include "xcglue.h"
#include "gc.h"

#ifdef SUPPORT_ARBITRARY_OBJECTS

typedef struct {
  void *realobj;
  Scheme_Class_Object *obj;
} ObjectHash;

static ObjectHash *hash;
static long hashsize = 100, hashcount = 0;

#endif

typedef struct {
  long id;
  Objscheme_Bundler f;
} BundlerHash;

static BundlerHash *bhash;
static long bhashsize = 201, bhashcount = 0, bhashstep = 17;

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

static long num_objects_allocated = 0;

#if defined(MZ_PRECISE_GC) || defined(USE_SENORA_GC) || defined(GC_MIGHT_USE_REGISTERED_STATICS)
# define wxREGGLOB(x) scheme_register_extension_global((void *)&x, sizeof(x))
#else
# define wxREGGLOB(x) /* empty */
#endif

static Scheme_Object *set_car_prim;

void objscheme_init(Scheme_Env *env)
{
  long i;
  Scheme_Object *set_car_symbol;

  wxREGGLOB(set_car_prim);
  set_car_symbol = scheme_intern_symbol("set-car!");
  set_car_prim = scheme_lookup_global(set_car_symbol, env);
  
#ifdef SUPPORT_ARBITRARY_OBJECTS
  wxREGGLOB(hash);
  hash = (ObjectHash *)scheme_malloc_atomic(sizeof(ObjectHash) * hashsize);
  for (i = 0; i < hashsize; i++) {
    hash[i].realobj = NULL;
  }
#endif
  
  wxREGGLOB(bhash);
  bhash = (BundlerHash *)scheme_malloc_atomic(sizeof(BundlerHash) 
					      * bhashsize);
  for (i = 0; i < bhashsize; i++) {
    bhash[i].id = 0;
  }
}

Scheme_Object *objscheme_def_prim_class(void *global_env, 
					char *name, char *superclass,
					Scheme_Method_Prim *initf,
					int nmethods)
{
  Scheme_Object *obj;
  Scheme_Object *sclass;

  if (superclass)
    obj = scheme_lookup_xc_global(superclass, global_env);
  else
    obj = NULL;

  sclass = scheme_make_class(name, obj, initf, nmethods);

  scheme_install_xc_global(name, sclass, global_env);

  return sclass;
}

void objscheme_add_global_class(Scheme_Object *sclass, char *name, void *env)
{
  scheme_install_xc_global(name, sclass, env);
}

void objscheme_add_global_interface(Scheme_Object *in, char *name, void *env)
{
  scheme_install_xc_global(name, in, env);
}

Scheme_Object *objscheme_find_method(Scheme_Object *obj, Scheme_Object *sclass,
				     char *name, void **cache)
{
  Scheme_Object *gdata;

  if (!obj)
    return NULL;

  if (*cache)
    gdata = (Scheme_Object *)*cache;
  else {
    Scheme_Object *s;
    s = scheme_intern_symbol(name);
    gdata = scheme_get_generic_data(sclass, s);
#if defined(MZ_PRECISE_GC) || defined(USE_SENORA_GC) || defined(GC_MIGHT_USE_REGISTERED_STATICS)
    scheme_register_extension_global(cache, sizeof(void *));
#endif
    *cache = (void *)gdata;
  }

  return scheme_apply_generic_data(gdata, obj, 0);
}


/***************************************************************************/

int objscheme_istype_bool(Scheme_Object *obj, const char *where)
{
  return 1; /* Anything can be a boolean */
}

int objscheme_istype_integer(Scheme_Object *obj, const char *stopifbad)
{
  if (SCHEME_INTP(obj) || SCHEME_BIGNUMP(obj))
    return 1;
  else if (stopifbad) {
    scheme_wrong_type(stopifbad, "exact integer", -1, 0, &obj);
  }
  return 0;
}

int objscheme_istype_ExactLong(Scheme_Object *obj, const char *stopifbad)
{
  return objscheme_istype_integer(obj, stopifbad);
}

int objscheme_istype_number(Scheme_Object *obj, const char *stopifbad)
{
  if (SCHEME_INTP(obj) || SCHEME_DBLP(obj) || SCHEME_BIGNUMP(obj)
      || SCHEME_RATIONALP(obj))
    return 1;
  else if (stopifbad) {
    scheme_wrong_type(stopifbad, "real number", -1, 0, &obj);
  }
  return 0;
}

int objscheme_istype_float(Scheme_Object *obj, const char *stopifbad)
{
  if (SCHEME_DBLP(obj))
    return 1;
  else if (stopifbad)
    scheme_wrong_type(stopifbad, "inexact real number", -1, 0, &obj);
  return 0;
}

int objscheme_istype_pair(Scheme_Object *obj, const char *stopifbad)
{
  if (SCHEME_PAIRP(obj))
    return 1;
  else if (stopifbad)
    scheme_wrong_type(stopifbad, "pair", -1, 0, &obj);
  return 0;
}

int objscheme_istype_string(Scheme_Object *obj, const char *stopifbad)
{
  if (SCHEME_STRINGP(obj))
    return 1;
  else if (stopifbad)
    scheme_wrong_type(stopifbad, "string", -1, 0, &obj);
  return 0;
}

int objscheme_istype_pathname(Scheme_Object *obj, const char *stopifbad)
{
  if (SCHEME_STRINGP(obj))
    return 1;
  else if (stopifbad)
    scheme_wrong_type(stopifbad, "pathname string", -1, 0, &obj);
  return 0;
}

int objscheme_istype_char(Scheme_Object *obj, const char *stopifbad)
{
  if (SCHEME_CHARP(obj))
    return 1;
  else if (stopifbad)
    scheme_wrong_type(stopifbad, "character", -1, 0, &obj);
  return 0;
}

int objscheme_istype_generic(Scheme_Object *obj, const char *stopifbad)
{
  if (SAME_TYPE(SCHEME_TYPE(obj), scheme_generic_type))
    return 1;
  else if (stopifbad)
    scheme_signal_error("expected an unknown type in %s", stopifbad);
  return 0;
}

int objscheme_istype_closed_prim(Scheme_Object *obj, const char *stopifbad)
{
  if (SAME_TYPE(SCHEME_TYPE(obj), scheme_closed_prim_type))
    return 1;
  else if (stopifbad)
    scheme_wrong_type(stopifbad, "procedure", -1, 0, &obj);
  return 0;
}

int objscheme_istype_proc2(Scheme_Object *obj, const char *stopifbad)
{
  return scheme_check_proc_arity(stopifbad, 2, -1, 0, &obj);
}

int objscheme_istype_box(Scheme_Object *obj, const char *stopifbad)
{
  if (SCHEME_BOXP(obj))
    return 1;
  else if (stopifbad)
    scheme_wrong_type(stopifbad, "box", -1, 0, &obj);
  return 0;
}

int objscheme_istype_nonnegative_symbol_integer(Scheme_Object *obj, const char *sym, const char *where)
{
  if (SCHEME_SYMBOLP(obj)) {
    int l;
    l = strlen(sym);
    if (SCHEME_SYM_LEN(obj) == l) {
      if (!strcmp(sym, SCHEME_SYM_VAL(obj))) {
	return 1;
      }
    }
  }

  if (objscheme_istype_integer(obj, NULL)) {
    long v;
    v = objscheme_unbundle_integer(obj, where);
    if (v >= 0)
      return 1;
  }

  if (where) {
    char *b;
    b = (char *)scheme_malloc_atomic(50);
    strcpy(b, "non-negative exact integer or '");
    strcat(b, sym);
    scheme_wrong_type(where, b, -1, 0, &obj);
  }

  return 0;
}

int objscheme_istype_nonnegative_symbol_float(Scheme_Object *obj, const char *sym, const char *where)
{
  if (SCHEME_SYMBOLP(obj)) {
    int l;
    l = strlen(sym);
    if (SCHEME_SYM_LEN(obj) == l) {
      if (!strcmp(sym, SCHEME_SYM_VAL(obj))) {
	return 1;
      }
    }
  }

  if (objscheme_istype_number(obj, NULL)) {
    double v;
    v = objscheme_unbundle_float(obj, where);
    if (v >= 0)
      return 1;
  }

  if (where) {
    char *b;
    b = (char *)scheme_malloc_atomic(50);
    strcpy(b, "non-negative number or '");
    strcat(b, sym);
    scheme_wrong_type(where, b, -1, 0, &obj);
  }

  return 0;
}

/************************************************************************/

Scheme_Object *objscheme_box(Scheme_Object *v)
{
  return scheme_box(v);
}

Scheme_Object *objscheme_bundle_generic(void *p)
{
  Scheme_Object *obj;

  obj = scheme_alloc_small_object();
  obj->type = scheme_generic_type;
  SCHEME_PTR_VAL(obj) = (Scheme_Object *)p;

  return obj;
}

Scheme_Object *objscheme_bundle_string(char *s)
{
  if (!s)
    return XC_SCHEME_NULL;
  else
    return scheme_make_string(s);
}

Scheme_Object *objscheme_bundle_pathname(char *s)
{
  return objscheme_bundle_string(s);
}

Scheme_Object *objscheme_bundle_nonnegative_symbol_float(double d, const char *symname)
{
  if (d < 0)
    return scheme_intern_symbol(symname);
  else
    return scheme_make_double(d);
}

/************************************************************************/

void *objscheme_unbundle_generic(Scheme_Object *obj, const char *where)
{
  (void)objscheme_istype_generic(obj, where);
  return (void *)SCHEME_PTR_VAL(obj);
}

long objscheme_unbundle_integer(Scheme_Object *obj, const char *where)
{
  (void)objscheme_istype_integer(obj, where);
  if (SCHEME_BIGNUMP(obj)) {
    if (SCHEME_PINT_VAL(obj) < 0)
      return -0xfffFFFF;
    else
      return 0xfffFFFF;
  } else
    return SCHEME_INT_VAL(obj);
}

long objscheme_unbundle_nonnegative_integer(Scheme_Object *obj, const char *where)
{
  if (objscheme_istype_integer(obj, NULL)) {
    long v;
    v = objscheme_unbundle_integer(obj, where);
    if (v >= 0)
      return v;
  }

  if (where)
    scheme_wrong_type(where, "non-negative exact integer", -1, 0, &obj);

  return -1;
}

long objscheme_unbundle_integer_in(Scheme_Object *obj, long minv, long maxv, const char *stopifbad)
{
  if (objscheme_istype_integer(obj, NULL)) {
    long v;
    v = objscheme_unbundle_integer(obj, stopifbad);
    if ((v >= minv) && (v <= maxv))
      return v;
  }

  if (stopifbad) {
    char buffer[100];
    sprintf(buffer, "exact integer in [%ld, %ld]", minv, maxv);
    scheme_wrong_type(stopifbad, buffer, -1, 0, &obj);
  }

  return 0;
}


long objscheme_unbundle_nonnegative_symbol_integer(Scheme_Object *obj, const char *sym, const char *where)
{
  if (SCHEME_SYMBOLP(obj)) {
    int l;
    l = strlen(sym);
    if (SCHEME_SYM_LEN(obj) == l) {
      if (!strcmp(sym, SCHEME_SYM_VAL(obj))) {
	return -1;
      }
    }
  }

  if (objscheme_istype_number(obj, NULL)) {
    long v;
    v = objscheme_unbundle_integer(obj, where);
    if (v >= 0)
      return v;
  }

  (void)objscheme_istype_nonnegative_symbol_integer(obj, sym, where);
  return -1;
}

ExactLong objscheme_unbundle_ExactLong(Scheme_Object *obj, const char *where)
{
  long v;

  (void)objscheme_istype_integer(obj, where);
  if (!scheme_get_int_val(obj, &v)) {
    if (where)
      scheme_arg_mismatch(where, "argument integer is out of platform-specific bounds", obj);
  }

  return v;
}


double objscheme_unbundle_float(Scheme_Object *obj, const char *where)
{
  (void)objscheme_istype_number(obj, where);
  if (SCHEME_DBLP(obj))
    return SCHEME_DBL_VAL(obj);
  else if (SCHEME_RATIONALP(obj))
    return scheme_rational_to_float(obj);
  else if (SCHEME_BIGNUMP(obj))
    return scheme_bignum_to_float(obj);
  else
    return (double)SCHEME_INT_VAL(obj);
}

double objscheme_unbundle_nonnegative_symbol_float(Scheme_Object *obj, const char *sym, const char *where)
{
  if (SCHEME_SYMBOLP(obj)) {
    int l;
    l = strlen(sym);
    if (SCHEME_SYM_LEN(obj) == l) {
      if (!strcmp(sym, SCHEME_SYM_VAL(obj))) {
	return -1;
      }
    }
  }

  if (objscheme_istype_number(obj, NULL)) {
    double v;
    v = objscheme_unbundle_float(obj, where);
    if (v >= 0)
      return v;
  }

  (void)objscheme_istype_nonnegative_symbol_float(obj, sym, where);
  return -1;
}

double objscheme_unbundle_float_in(Scheme_Object *obj, double minv, double maxv, const char *stopifbad)
{
  if (objscheme_istype_number(obj, NULL)) {
    double v;
    v = objscheme_unbundle_float(obj, stopifbad);
    if ((v >= minv) && (v <= maxv))
      return v;
  }

  if (stopifbad) {
    char buffer[100];
    sprintf(buffer, "real number in [%f, %f]", minv, maxv);
    scheme_wrong_type(stopifbad, buffer, -1, 0, &obj);
  }

  return 0;
}

double objscheme_unbundle_nonnegative_float(Scheme_Object *obj, const char *where)
{
  if (objscheme_istype_number(obj, NULL)) {
    double v;
    v = objscheme_unbundle_float(obj, where);
    if (v >= 0)
      return v;
  }

  if (where)
    scheme_wrong_type(where, "non-negative number", -1, 0, &obj);

  return -1.0;
}

int objscheme_unbundle_bool(Scheme_Object *obj, const char *where)
{  
  (void)objscheme_istype_bool(obj, where);
  return NOT_SAME_OBJ(obj, scheme_false);
}

char *objscheme_unbundle_string(Scheme_Object *obj, const char *where)
{
  (void)objscheme_istype_string(obj, where);
  return SCHEME_STR_VAL(obj);
}

char *objscheme_unbundle_pathname(Scheme_Object *obj, const char *where)
{
  (void)objscheme_istype_pathname(obj, where);
  return scheme_expand_filename(SCHEME_STR_VAL(obj), 
				SCHEME_STRTAG_VAL(obj),
				(char *)where, NULL);
}

char *objscheme_unbundle_nullable_string(Scheme_Object *obj, const char *where)
{
  if (XC_SCHEME_NULLP(obj))
    return NULL;
  else if (!where || SCHEME_STRINGP(obj))
    return objscheme_unbundle_string(obj, where);
  else {
    scheme_wrong_type(where, "string or "  XC_NULL_STR, -1, 0, &obj);
    return NULL;
  }
}

char *objscheme_unbundle_nullable_pathname(Scheme_Object *obj, const char *where)
{
  if (XC_SCHEME_NULLP(obj))
    return NULL;
  else  if (!where || SCHEME_STRINGP(obj))
    return objscheme_unbundle_pathname(obj, where);
  else  {
    scheme_wrong_type(where, "pathname string or " XC_NULL_STR, -1, 0, &obj);
    return NULL;
  }
    
}

char objscheme_unbundle_char(Scheme_Object *obj, const char *where)
{
  (void)objscheme_istype_char(obj, where);
  return SCHEME_CHAR_VAL(obj);
}

Scheme_Object *objscheme_car(Scheme_Object *obj, const char *where)
{  
  (void)objscheme_istype_pair(obj, where);
  return scheme_car(obj);
}

Scheme_Object *objscheme_unbox(Scheme_Object *obj, const char *where)
{  
  (void)objscheme_istype_box(obj, where);
  return scheme_unbox(obj);
}

Scheme_Object *objscheme_nullable_unbox(Scheme_Object *obj, const char *where)
{  
  if (!SCHEME_BOXP(obj)) {
    if (where)
      scheme_wrong_type(where, "box or " XC_NULL_STR, -1, 0, &obj);
    return NULL;
  } else
    return scheme_unbox(obj);
    
}

/************************************************************************/

void objscheme_set_car(Scheme_Object *l, Scheme_Object *v)
{
  Scheme_Object *p[2];

  p[0] = l;
  p[1] = v;

  (void)scheme_apply(set_car_prim, 2, p);
}

void objscheme_set_box(Scheme_Object *b, Scheme_Object *v)
{
  (void)objscheme_istype_box(b, "set-box!");
  SCHEME_PTR_VAL(b) = v;
}

/************************************************************************/

void objscheme_note_creation(Scheme_Object *obj)
{
  num_objects_allocated++;
}

#ifdef SUPPORT_ARBITRARY_OBJECTS

#define HASH(realobj) (((long)realobj >> 2) % hashsize)

#define GONE ((void *)1)

void objscheme_save_object(void *realobj, Scheme_Object *obj)
{
  int i;

  if (2 * hashcount > hashsize) {
    long oldsize = hashsize;
    ObjectHash *old = hash;

    hashsize *= 2;
    hash = (ObjectHash *)scheme_malloc_atomic(sizeof(ObjectHash) * hashsize);

    for (i = 0; i < hashsize; i++) {
      hash[i].realobj = NULL;
    }

    hashcount = 0;
    for (i = 0; i < oldsize; i++) {
      if (old[i].realobj && NOT_SAME_PTR(old[i].realobj, GONE))
	objscheme_save_object(old[i].realobj, (Scheme_Object *)old[i].obj);
    }
  }

  i = HASH(realobj);
  if (i < 0)
    i = -i;

  while (hash[i].realobj && NOT_SAME_PTR(hash[i].realobj, GONE)) {
    i++;
    if (i >= hashsize)
      i = 0;
  }

  hash[i].realobj = realobj;
  hash[i].obj = (Scheme_Class_Object *)obj;

  hashcount++;
}

Scheme_Class_Object *objscheme_find_object(void *realobj)
{
  int i;

  i = HASH(realobj);
  if (i < 0)
    i = -i;

  while (NOT_SAME_PTR(hash[i].realobj, realobj) || SAME_PTR(hash[i].realobj, GONE)) {
    if (!hash[i].realobj)
      return NULL;
    i++;
    if (i >= hashsize)
      i = 0;
  }

  return hash[i].obj;
}

#endif

void objscheme_check_valid(Scheme_Object *o)
{
  Scheme_Class_Object *obj = (Scheme_Class_Object *)o;

  if (obj->primflag < 0) {
    int len;
    const char *classname;
    classname = scheme_get_class_name(obj->sclass, &len);
    scheme_signal_error("attempt to use an %sobject%s (class %s)",
			(obj->primflag == -2) ? "" : "invalidated ",
			(obj->primflag == -2) ? " shutdown by custodian" : "",
			classname ? classname : "unknown");
  }
  if (!obj->inited) {
    int len;
    const char *classname;
    classname = scheme_get_class_name(obj->sclass, &len);
    scheme_signal_error("attempt to use an uninitialized object (class %s)",
			classname ? classname : "unknown");
  }
}

int objscheme_is_shutdown(Scheme_Object *o)
{
  Scheme_Class_Object *obj = (Scheme_Class_Object *)o;

  return (obj->primflag < 0);
}

void objscheme_destroy(void *realobj, Scheme_Object *obj_in)
{
#ifdef SUPPORT_ARBITRARY_OBJECTS
  int i;
#endif
  Scheme_Class_Object *obj;

  --num_objects_allocated;

  obj = (Scheme_Class_Object *)obj_in;

#ifdef SUPPORT_ARBITRARY_OBJECTS
  if (!obj) {
    i = HASH(realobj);
    if (i < 0)
      i = -i;
    
    while (NOT_SAME_PTR(hash[i].realobj, realobj) 
	   || SAME_PTR(hash[i].realobj, GONE)) {
      if (!hash[i].realobj)
	break;
      i++;
      if (i >= hashsize)
	i = 0;
    }
    
    if (hash[i].realobj) {
      obj = hash[i].obj;
      hash[i].realobj = GONE;
    }
  }
#endif

  if (obj) {
    if (obj->primflag < 0)
      return;

    obj->primflag = -1;
    obj->primdata = NULL;
  }
}

void objscheme_register_primpointer(void *prim_obj, void *prim_ptr_address)
{
#ifdef MZ_PRECISE_GC
  GC_finalization_weak_ptr((void **)prim_obj, (void **)prim_ptr_address - (void **)prim_obj);
#else
  GC_general_register_disappearing_link((void **)prim_ptr_address, NULL);
#endif
}

/***************************************************************/

void objscheme_install_bundler(Objscheme_Bundler f, long id)
{
  long i;

  i = id % bhashsize;
  while(bhash[i].id && bhash[i].id != id) {
    i = (i + bhashstep) % bhashsize;
  }

  bhash[i].id = id;
  bhash[i].f = f;
  bhashcount++;
}

Scheme_Object *objscheme_bundle_by_type(void *realobj, long id)
{
  long i;

  i = id % bhashsize;
  while(bhash[i].id && bhash[i].id != id) {
    i = (i + bhashstep) % bhashsize;
  }

  if (!bhash[i].id)
    return NULL;

  return bhash[i].f(realobj);
}

/************************************************************************/

#ifdef __cplusplus
extern "C" 
{
#endif

void objscheme_mark_external_invalid(void *sobj)
{
  Scheme_Class_Object *obj = (Scheme_Class_Object *)sobj;

  obj->primflag = -1;
  obj->primdata = NULL;  
}

#ifdef __cplusplus
}
#endif

