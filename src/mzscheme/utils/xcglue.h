
#ifndef OBJ_SCHEME_H
#define OBJ_SCHEME_H

#include "scheme.h"

#ifdef __cplusplus
extern "C" 
{
#endif

/******************************************************************/
/*                   Utilites used by xctocc                      */
/******************************************************************/

void objscheme_init(Scheme_Env *env);

void scheme_install_xc_global(char *name, Scheme_Object *val, void *env);
Scheme_Object *scheme_lookup_xc_global(char *name, void *env);

/* Defining a primitive class: */
Scheme_Object *
objscheme_def_prim_class(void *env, char *name, char *superclass,
			 Scheme_Method_Prim *initf, int nmethods);
void objscheme_add_global_class(Scheme_Object *sclass, char *name,
				void *env);

void objscheme_note_creation(Scheme_Object *obj);

/* Maintaining the Scheme - C++ connection */
void objscheme_save_object(void *, Scheme_Object *);
Scheme_Class_Object *objscheme_find_object(void *);
void objscheme_check_valid(Scheme_Object *);

void objscheme_backpointer(void *obj_ptr_address);
void objscheme_register_primpointer(void *prim_ptr_address);

void objscheme_destroy(void *, Scheme_Object *obj);

/* Finding a method: */
Scheme_Object *objscheme_find_method(Scheme_Object *obj, 
				     Scheme_Object *sclass,
				     char *name, 
				     void **cache);

/* Checking a class relationship */
#define objscheme_is_subclass scheme_is_subclass

void objscheme_set_car(Scheme_Object *, Scheme_Object *);
Scheme_Object *objscheme_unbox(Scheme_Object *, const char *where);
Scheme_Object *objscheme_box(Scheme_Object *);
void objscheme_set_box(Scheme_Object *, Scheme_Object *);

int objscheme_istype_bool(Scheme_Object *, const char *stopifbad);
int objscheme_istype_number(Scheme_Object *, const char *stopifbad);
int objscheme_istype_integer(Scheme_Object *, const char *stopifbad);
int objscheme_istype_float(Scheme_Object *, const char *stopifbad);
int objscheme_istype_pair(Scheme_Object *, const char *stopifbad);
int objscheme_istype_string(Scheme_Object *, const char *stopifbad);
int objscheme_istype_pathname(Scheme_Object *, const char *stopifbad);
int objscheme_istype_char(Scheme_Object *, const char *stopifbad);
int objscheme_istype_generic(Scheme_Object *, const char *stopifbad);
int objscheme_istype_closed_prim(Scheme_Object *, const char *stopifbad);
int objscheme_istype_proc2(Scheme_Object *, const char *stopifbad);
int objscheme_istype_box(Scheme_Object *, const char *stopifbad);

Scheme_Object *objscheme_car(Scheme_Object *, const char *where);
Scheme_Object *objscheme_bundle_string(char *);
Scheme_Object *objscheme_bundle_pathname(char *);
Scheme_Object *objscheme_bundle_generic(void *);

void *objscheme_unbundle_generic(Scheme_Object *, const char *);
long objscheme_unbundle_integer(Scheme_Object *, const char *);
long objscheme_unbundle_nonnegative_integer(Scheme_Object *, const char *);
double objscheme_unbundle_float(Scheme_Object *, const char *);
double objscheme_unbundle_nonnegative_float(Scheme_Object *, const char *);
int objscheme_unbundle_bool(Scheme_Object *, const char *);
char *objscheme_unbundle_string(Scheme_Object *, const char *);
char *objscheme_unbundle_nullable_string(Scheme_Object *, const char *);
char *objscheme_unbundle_pathname(Scheme_Object *, const char *);
char *objscheme_unbundle_nullable_pathname(Scheme_Object *, const char *);
char objscheme_unbundle_char(Scheme_Object *, const char *);

#define objscheme_bundle_integer scheme_make_integer
#define objscheme_bundle_long objscheme_bundle_integer
#define objscheme_bundle_int objscheme_bundle_integer
#define objscheme_bundle_float scheme_make_double
#define objscheme_bundle_bool(x) ((x) ? scheme_true : scheme_false)
#define objscheme_bundle_char scheme_make_char
#define objscheme_bundle_pair scheme_make_pair

#define objscheme_bundle_double objscheme_bundle_float
#define objscheme_unbundle_double objscheme_unbundle_float

#define objscheme_unbundle_long objscheme_unbundle_integer
#define objscheme_unbundle_int objscheme_unbundle_integer

/* #define OBJSCHEME_PRIM_METHOD(m) SCHEME_CLSD_PRIMP(m) */
#define OBJSCHEME_PRIM_METHOD(m) 0

#ifndef WXS_CANT_ASSIGN_STRUCTURES
#define COPY_JMPBUF(dest, src) (dest = src)
#else
#define COPY_JMPBUF(dest, src) memcpy(&dest, &src, sizeof(jmp_buf));
#endif


typedef Scheme_Object *(*Objscheme_Bundler)(void *);
void objscheme_install_bundler(Objscheme_Bundler f, long id);
Scheme_Object *objscheme_bundle_by_type(void *realobj, long type);

#ifndef _MSC_VER
typedef char byte;
#endif
typedef unsigned char ubyte;

typedef char *xc_string;
#define string xc_string
typedef const unsigned char *custring, *ncustring; 
typedef const char *cstring, *ncstring; 
typedef char *nstring;
typedef unsigned char *ustring, *nustring;

typedef char *pathname;
typedef char *npathname;
typedef const char *cpathname;
typedef const char *cnpathname;

typedef int nnint;
typedef float nnfloat;

#ifdef __GNUG__
#define WXS_USE_ARGUMENT(x) x = x; /* compiler optimizes it away */
#else
#define WXS_USE_ARGUMENT(x) 
#endif

#ifdef __cplusplus
};
#endif

#endif
