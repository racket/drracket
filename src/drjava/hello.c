/*
 * This stuff will _break_ if called from a different thread than the
 * one that ran scheme_initialize (in theory).  Each thread should
 * grab its own jenv to prevent this.
 */

#include "escheme.h"
#include "jni.h"

#include <stdio.h>
#include <string.h>
#include <limits.h>

#define JFIND_CLASS		"jfind-class"
#define JFIND_METHOD		"jfind-method"
#define JFIND_FIELD		"jfind-field"
#define JFIND_STATIC_METHOD	"jfind-static-method"
#define JFIND_STATIC_FIELD	"jfind-static-field"
#define JGET_FIELD		"jget-field"
#define JSET_FIELD		"jset-field!"
#define JCALL			"jcall"
#define JNEW			"jnew"
#define J2S_STR			"jstring->string"
#define JSTR2BYTES		"string->jbytes"
#define NUM_PRIM		11

JavaVM *jvm;
JNIEnv *jenv;
Scheme_Type jclass_type;
Scheme_Type jmethod_type;
Scheme_Type jfield_type;
Scheme_Type jobject_type;

static jclass StringClass;

#include "mzjvm.h"

static void unref(void *p, void *_) {
  (*jenv)->DeleteGlobalRef(jenv, ((jobject_t *)p)->val); 
}

Scheme_Object *saveClass(jclass toSave) {
  jclass_t *ref = (jclass_t *)scheme_malloc(sizeof(jclass_t));
  ref->tag = jclass_type;
  ref->val = (*jenv)->NewGlobalRef(jenv, toSave);
  scheme_register_finalizer(ref, &unref, NULL, NULL, NULL);
  return (Scheme_Object *)ref;
}

Scheme_Object *saveRef(jobject toSave) {
  jobject_t *ref = (jobject_t *)scheme_malloc(sizeof(jobject_t));
  ref->tag = jobject_type;
  ref->val = (*jenv)->NewGlobalRef(jenv, toSave);
  scheme_register_finalizer(ref, &unref, NULL, NULL, NULL);
  return (Scheme_Object *)ref;
}

static void freeStr(void *p, void *s) {
  (*jenv)->ReleaseStringUTFChars(jenv, (jstring)s, (const char *)p); 
}

Scheme_Object *j2s_str(int argc, Scheme_Object **argv) {
  Scheme_Object *str = argv[0];

  if (SCHEME_TYPE(str) != jobject_type) {
    scheme_signal_error(J2S_STR " not a Java object.");
  } else {
    jobject jstr = ((jobject_t *)str)->val;
    if (!(*jenv)->IsInstanceOf(jenv, jstr, StringClass)) {
      scheme_signal_error(J2S_STR " not a Java string.");
    } else {
      jboolean iscopy;
      char *chars = (char *)((*jenv)->GetStringUTFChars(jenv, jstr, NULL));
      
      scheme_register_finalizer(chars, &freeStr, jstr, NULL, NULL);
      if (iscopy) {
	return scheme_make_string_without_copying(chars);
      } else {
	return scheme_make_string(chars);
      }
    }
  }
}

Scheme_Object *str2jbytes(int argc, Scheme_Object **argv) {
  Scheme_Object *str = argv[0];

  if (SCHEME_STRINGP(str)) {
    jsize len = (jsize)SCHEME_STRLEN_VAL(str);
    jbyteArray array = (*jenv)->NewByteArray(jenv, len);
    Scheme_Object *ret;
    (*jenv)->SetByteArrayRegion(jenv,
				array,
				0,
				len,
				(jbyte *)SCHEME_STR_VAL(str));
    if ((*jenv)->ExceptionOccurred(jenv)) {
      (*jenv)->ExceptionClear(jenv);
      scheme_signal_error(JSTR2BYTES " couldn't convert string to byte[]");
    }
    ret = saveRef(array);
    (*jenv)->DeleteLocalRef(jenv, array);
    return ret;
  } else {
    scheme_signal_error(JSTR2BYTES " expected a string.");
    return scheme_void;
  }
}

// jfind-class : String -> jclass
Scheme_Object *jFindClass(int argc, Scheme_Object **argv) {
  Scheme_Object *name = argv[0]; // fully qualified name, internal jvm notation
  if (SCHEME_STRINGP(name)) {
    jclass theClass = (*jenv)->FindClass(jenv, SCHEME_STR_VAL(name));

    if ((*jenv)->ExceptionOccurred(jenv)) {
      (*jenv)->ExceptionClear(jenv);
      scheme_signal_error(JFIND_CLASS " couldn't load class");
    }
    return saveClass(theClass);
  } else {
    scheme_signal_error(JFIND_CLASS " expected a string.");
  }
}

#include "generated.c"

static Scheme_Object *
Call_Void(jobject obj, jmethodID id, jvalue *args) {
  (*jenv)->CallVoidMethodA(jenv, obj, id, args);
  return scheme_void;
}

static Scheme_Object *
Call_StaticVoid(jclass obj, jmethodID id, jvalue *args) {
  (*jenv)->CallStaticVoidMethodA(jenv, obj, id, args);
  return scheme_void;
}

static Caller_t findStaticReturnType(int len, char *sig) {
  if (sig[len-2] == '[') {
    return &Call_StaticObject;
  }
  switch (sig[len-1]) {
  case ';':
    return &Call_StaticObject;
  case 'Z':
    return &Call_StaticBoolean;
  case 'B':
    return &Call_StaticByte;
  case 'C':
    return &Call_StaticChar;
  case 'S':
    return &Call_StaticShort;
  case 'I':
    return &Call_StaticInt;
  case 'J':
    return &Call_StaticLong;
  case 'F':
    return &Call_StaticFloat;
  case 'D':
    return &Call_StaticDouble;
  case 'V':
    return &Call_StaticVoid;
  }
}

static Caller_t findReturnType(int len, char *sig) {
  if (sig[len-2] == '[') {
    return &Call_Object;
  }
  switch (sig[len-1]) {
  case ';':
    return &Call_Object;
  case 'Z':
    return &Call_Boolean;
  case 'B':
    return &Call_Byte;
  case 'C':
    return &Call_Char;
  case 'S':
    return &Call_Short;
  case 'I':
    return &Call_Int;
  case 'J':
    return &Call_Long;
  case 'F':
    return &Call_Float;
  case 'D':
    return &Call_Double;
  case 'V':
    return &Call_Void;
  }
}

static Scheme_Object *
findMethod(int argc, Scheme_Object **argv, typeFinder_t tf, getMethID_t gmID) {
  Scheme_Object *theClass = argv[0];
  Scheme_Object *methName = argv[1];
  Scheme_Object *sig = argv[2];

  if (SCHEME_TYPE(theClass) == jclass_type &&
      SCHEME_STRINGP(methName) &&
      SCHEME_STRINGP(sig)) {
    char *methodChars = SCHEME_STR_VAL(methName);
    char *sigChars = SCHEME_STR_VAL(sig);
    jmethod_t *ref;
    jmethodID method = (*gmID)(jenv, ((jclass_t *)theClass)->val, methodChars, sigChars); 

    if ((*jenv)->ExceptionOccurred(jenv)) {
      (*jenv)->ExceptionClear(jenv);
      scheme_signal_error(JFIND_METHOD " couldn't find method %s w. "
				       "signature %s.", methodChars, sigChars);
    }
    ref = (jmethod_t *)scheme_malloc(sizeof(jmethod_t));
    ref->tag = jmethod_type;
    ref->val = method;
    ref->mc = (*tf)(SCHEME_STRLEN_VAL(sig), sigChars);
    return (Scheme_Object *)ref;
  }
}

// jfind-method : jclass String String -> jmethod
Scheme_Object *jFindMethod(int argc, Scheme_Object **argv) {
  return findMethod(argc, argv, &findReturnType, (*jenv)->GetMethodID);
}

static jclass SchemeFunction_class;
static jmethodID newSchemeFunction;

// convert a scheme value to a java value without looking at the java type.
static jvalue s2jVal(Scheme_Object *v) {
  Scheme_Type t = SCHEME_TYPE(v);
  if (t == jobject_type) {
    return (jvalue)(((jobject_t *)v)->val);
  }
  switch (t) {
  case scheme_true_type:
    return (jvalue)(jboolean)JNI_TRUE;
  case scheme_false_type:
    return (jvalue)(jchar)JNI_FALSE;
  case scheme_integer_type:
    return (jvalue)(jint)SCHEME_INT_VAL(v);
  case scheme_char_type:
    return (jvalue)(jchar)SCHEME_CHAR_VAL(v);
  case scheme_string_type:
    return (jvalue)(*jenv)->NewStringUTF(jenv, SCHEME_STR_VAL(v));
  default:
    if (SCHEME_PROCP(v)) {
// more here - fix memory leak introduced on purpose:
scheme_dont_gc_ptr((void*)v);
      return (jvalue)((*jenv)->NewObject(jenv, SchemeFunction_class, newSchemeFunction, (jint)v));
    } else {
      // more here - what about float, double, byte, short, long?
      scheme_signal_error("can't convert unsupported scheme type to jvm type");
    }
  }
}

static Scheme_Object *
call_helper(jobject obj, jmethod_t *m, int argc, Scheme_Object **argv) {
    jvalue *vals;
    int i;
    Scheme_Object *result;

    // vals is freed in this function - like it's on the stack.
    vals = (jvalue*)malloc(sizeof(jvalue)*argc);

    for (i = 0; i < argc; i++) {
      vals[i] = s2jVal(argv[i]);
    }
    result = (*(m->mc))(obj, m->val, vals);
    free(vals);
    return result;
}

// jcall : jobject_t jmethod_t . jobj/prim* -> jobj/prim
Scheme_Object *jCallMethod(int argc, Scheme_Object **argv) {
  Scheme_Object *object = argv[0];
  Scheme_Object *method = argv[1];
  Scheme_Object *result;
  Scheme_Type obj_t = SCHEME_TYPE(object);

  if ((obj_t == jobject_type || obj_t == jclass_type) &&
      SCHEME_TYPE(method) == jmethod_type) {
    result = call_helper(((jobject_t *)object)->val,
			 ((jmethod_t *)method), argc-2, argv+2);
  } else {
    scheme_signal_error(JCALL " expected jobject x jmethod . args");
  }

  if ((*jenv)->ExceptionOccurred(jenv)) {
    (*jenv)->ExceptionDescribe(jenv);
    (*jenv)->ExceptionClear(jenv);
    scheme_signal_error(JCALL " method call threw exception.");
  }
  return result;
}

// jnew : jobject_t jmethod_t . jobj/prim* -> jobj/prim
Scheme_Object *jNew(int argc, Scheme_Object **argv) {
  Scheme_Object *theClass = argv[0];
  Scheme_Object *constructor = argv[1];
  jobject object;

  if (SCHEME_TYPE(theClass) == jclass_type &&
      SCHEME_TYPE(constructor) == jmethod_type) {
#ifdef KAFFE_BUG  // fixed 6/10/1999AD by Alexandre Oliva
    //  Ugly lack of abstraction due to a bug in Kaffe-1.0b4
    jvalue *vals;
    int i;
    argc -= 2;
    argv += 2;
    vals = (jvalue*)malloc(sizeof(jvalue)*argc);

    for (i = 0; i < argc; i++) {
      vals[i] = s2jVal(argv[i]);
    }
    object = (*jenv)->NewObjectA(jenv, ((jobject_t *)theClass)->val, ((jmethod_t *)constructor)->val, vals);
    free(vals);
#else // !KAFFE_BUG
    object = (*jenv)->AllocObject(jenv, ((jobject_t *)theClass)->val);
    (void)call_helper(object, (jmethod_t *)constructor, argc-2, argv+2);
#endif // !KAFFE_BUG
  } else {
    scheme_signal_error(JNEW " expected jclass x jmethod . args");
  }

  if ((*jenv)->ExceptionOccurred(jenv)) {
    (*jenv)->ExceptionDescribe(jenv);
    (*jenv)->ExceptionClear(jenv);
    scheme_signal_error(JNEW " constructor call threw exception.");
  }
  return saveRef(object);
}

static void
getset_static(jfield_t *ref, char *sig) {
  if (sig[1] != '\0') {
    ref->get = &Get_StaticObject;
    ref->set = &Set_StaticObject;
  } else {
    switch (sig[0]) {
    case 'Z':
      ref->get = &Get_StaticBoolean;
      ref->set = &Set_StaticBoolean;
      return;
    case 'B':
      ref->get = &Get_StaticByte;
      ref->set = &Set_StaticByte;
      return;
    case 'C':
      ref->get = &Get_StaticChar;
      ref->set = &Set_StaticChar;
      return;
    case 'S':
      ref->get = &Get_StaticShort;
      ref->set = &Set_StaticShort;
      return;
    case 'I':
      ref->get = &Get_StaticInt;
      ref->set = &Set_StaticInt;
      return;
    case 'J':
      ref->get = &Get_StaticLong;
      ref->set = &Set_StaticLong;
      return;
    case 'F':
      ref->get = &Get_StaticFloat;
      ref->set = &Set_StaticFloat;
      return;
    case 'D':
      ref->get = &Get_StaticDouble;
      ref->set = &Set_StaticDouble;
      return;
    }
  }
}

static void
getset(jfield_t *ref, char *sig) {
  if (sig[1] != '\0') {
    ref->get = &Get_Object;
    ref->set = &Set_Object;
  } else {
    switch (sig[0]) {
    case 'Z':
      ref->get = &Get_Boolean;
      ref->set = &Set_Boolean;
      return;
    case 'B':
      ref->get = &Get_Byte;
      ref->set = &Set_Byte;
      return;
    case 'C':
      ref->get = &Get_Char;
      ref->set = &Set_Char;
      return;
    case 'S':
      ref->get = &Get_Short;
      ref->set = &Set_Short;
      return;
    case 'I':
      ref->get = &Get_Int;
      ref->set = &Set_Int;
      return;
    case 'J':
      ref->get = &Get_Long;
      ref->set = &Set_Long;
      return;
    case 'F':
      ref->get = &Get_Float;
      ref->set = &Set_Float;
      return;
    case 'D':
      ref->get = &Get_Double;
      ref->set = &Set_Double;
      return;
    }
  }
}

static Scheme_Object *
findField(int argc, Scheme_Object **argv, getField_t gfID, getSet_t gs) {
  Scheme_Object *theClass = argv[0];
  Scheme_Object *fieldName = argv[1];
  Scheme_Object *sig = argv[2];

  if (SCHEME_TYPE(theClass) == jclass_type &&
      SCHEME_STRINGP(fieldName) &&
      SCHEME_STRINGP(sig)) {
    char *fieldChars = SCHEME_STR_VAL(fieldName);
    char *sigChars = SCHEME_STR_VAL(sig);
    jfield_t *ref;
    jfieldID field = 
      (*gfID)(jenv, ((jclass_t *)theClass)->val, fieldChars, sigChars); 

    if ((*jenv)->ExceptionOccurred(jenv)) {
      (*jenv)->ExceptionClear(jenv);
      scheme_signal_error(JFIND_FIELD " couldn't find field %s of type %s.",
			  fieldChars, sigChars);
    }
    ref = (jfield_t *)scheme_malloc(sizeof(jfield_t));
    ref->tag = jfield_type;
    ref->val = field;
    (*gs)(ref, sigChars);
    return (Scheme_Object *)ref;
  } else {
    scheme_signal_error(JFIND_FIELD " expected jclass String String.");
  }
}

// jfind-field : jclass String String -> jfield
Scheme_Object *jFindField(int argc, Scheme_Object **argv) {
  return findField(argc, argv, (*jenv)->GetFieldID, &getset);
}

Scheme_Object *jFindStaticMethod(int argc, Scheme_Object **argv) {
  return findMethod(argc, argv, &findStaticReturnType, (*jenv)->GetStaticMethodID);
}

Scheme_Object *jFindStaticField(int argc, Scheme_Object **argv) {
  return findField(argc, argv, (*jenv)->GetStaticFieldID, &getset_static);
}

// jget-field : jobject jfield -> (field's type)
Scheme_Object *jGetField(int argc, Scheme_Object **argv) {
  Scheme_Object *obj = argv[0];
  Scheme_Object *field = argv[1];
  Scheme_Type obj_t = SCHEME_TYPE(obj);

  if ((obj_t == jobject_type || obj_t == jclass_type) &&
      SCHEME_TYPE(field) == jfield_type) {
    return (*((jfield_t *)field)->get)(((jobject_t *)obj)->val,
					 ((jfield_t *)field)->val);
  } else {
    scheme_signal_error(JGET_FIELD " expected jobject jfield.");
  }
}

// jset-field : jobject jfield (field's type) -> Void
Scheme_Object *jSet_Field(int argc, Scheme_Object **argv) {
  Scheme_Object *obj = argv[0];
  Scheme_Object *field = argv[1];
  Scheme_Object *rhs = argv[2];
  Scheme_Type obj_t = SCHEME_TYPE(obj);

  if ((obj_t == jobject_type || obj_t == jclass_type) &&
      SCHEME_TYPE(field) == jfield_type) {
    (*((jfield_t *)field)->set)(((jobject_t *)obj)->val,
				  ((jfield_t *)field)->val,
				  rhs);
    return scheme_void;
  } else {
    scheme_signal_error(JSET_FIELD " expected jobject jfield (field's type).");
  }
}

#define CLASSPATH_OPT "-Djava.class.path="
#define LIBPATH_OPT "-Djava.library.path="

#define DEFAULT_LIBPATH_OPT LIBPATH_OPT "."
#define DEFAULT_CLASSPATH_OPT CLASSPATH_OPT "."

#define NO_JIT "-Djava.compiler=NONE"
#define NUM_OPTS 3

static char *my_concat(char *cstr, Scheme_Object *sstr) {
  char *path = SCHEME_STR_VAL(sstr);
  char *opt = (char *)malloc(strlen(cstr) +
			     SCHEME_STRLEN_VAL(sstr));

  strcpy(opt, cstr);
  strcat(opt, path);
  return opt;
}

Scheme_Object *init_unit(Scheme_Object **boxes, Scheme_Object **anchors,
			 Scheme_Unit *m, void *debug_request)
{
  SCHEME_ENVBOX_VAL(boxes[0]) =
    scheme_make_prim_w_arity((Scheme_Prim *)jFindClass, JFIND_CLASS, 1, 1);
  SCHEME_ENVBOX_VAL(boxes[1]) =
    scheme_make_prim_w_arity((Scheme_Prim *)jFindMethod, JFIND_METHOD, 3, 3);
  SCHEME_ENVBOX_VAL(boxes[2]) =
    scheme_make_prim_w_arity((Scheme_Prim *)jFindField, JFIND_FIELD, 3, 3);
  SCHEME_ENVBOX_VAL(boxes[3]) =
    scheme_make_prim_w_arity((Scheme_Prim *)jFindStaticMethod, JFIND_STATIC_METHOD, 3, 3);
  SCHEME_ENVBOX_VAL(boxes[4]) =
    scheme_make_prim_w_arity((Scheme_Prim *)jFindStaticField, JFIND_STATIC_FIELD, 3, 3);
  SCHEME_ENVBOX_VAL(boxes[5]) =
    scheme_make_prim_w_arity((Scheme_Prim *)jGetField, JGET_FIELD, 2, 2);
  SCHEME_ENVBOX_VAL(boxes[6]) =
    scheme_make_prim_w_arity((Scheme_Prim *)jSet_Field, JSET_FIELD, 3, 3);
  SCHEME_ENVBOX_VAL(boxes[7]) =
    scheme_make_prim_w_arity((Scheme_Prim *)jCallMethod, JCALL, 2, -1);
  SCHEME_ENVBOX_VAL(boxes[8]) =
    scheme_make_prim_w_arity((Scheme_Prim *)jNew, JNEW, 2, -1);
  SCHEME_ENVBOX_VAL(boxes[9]) =
    scheme_make_prim_w_arity((Scheme_Prim *)j2s_str, J2S_STR, 1, 1);
  SCHEME_ENVBOX_VAL(boxes[10]) =
    scheme_make_prim_w_arity((Scheme_Prim *)str2jbytes, JSTR2BYTES, 1, 1);
  m->data = (Scheme_Object *)anchors;

  return scheme_void;
}

static Scheme_Unit *jvm_unit = (Scheme_Unit *)scheme_void;

Scheme_Object *scheme_initialize(Scheme_Env *senv) {
  JavaVMInitArgs vm_args;
#ifndef KAFFE
  JavaVMOption options[NUM_OPTS];
#endif // !KAFFE
  Scheme_Object *sclasspath;
  Scheme_Object *libpath;

  sclasspath = scheme_lookup_global(scheme_intern_symbol("classpath"), senv);
  libpath = scheme_lookup_global(scheme_intern_symbol("libpath"), senv);

  if (libpath == NULL || sclasspath == NULL) {
    return scheme_false;
  }

#ifdef KAFFE
  vm_args.version = 0x00010001; // copied from JDK spec.
  JNI_GetDefaultJavaVMInitArgs(&vm_args);

  vm_args.classpath = SCHEME_STR_VAL(sclasspath);
  vm_args.libraryhome = SCHEME_STR_VAL(libpath);
  vm_args.classhome = KAFFE;

  vm_args.maxHeapSize = INT_MAX;

# ifdef HELLO_DEBUG
  vm_args.enableClassGC = 0;
  vm_args.enableVerboseGC = 1;
  vm_args.enableVerboseJIT = 1;
  vm_args.enableVerboseCall = 1;
  vm_args.enableVerboseClassloading = 1;
# endif // HELLO_DEBUG
  if (JNI_CreateJavaVM(&jvm, &jenv, &vm_args) < 0) {
    return scheme_false;
  }
#else // !KAFFE
  options[0].optionString = my_concat(LIBPATH_OPT, libpath);
  options[1].optionString = my_concat(CLASSPATH_OPT, sclasspath);
  options[2].optionString = NO_JIT;

  vm_args.version = JNI_VERSION_1_2;
  vm_args.options = options;
  vm_args.nOptions = NUM_OPTS;
  vm_args.ignoreUnrecognized = 1;

  jclass_type = scheme_make_type("<jclass>");
  jmethod_type = scheme_make_type("<jmethod>");
  jfield_type = scheme_make_type("<jfield>");
  jobject_type = scheme_make_type("<jobject>");

  if (JNI_CreateJavaVM(&jvm, (void **)&jenv, &vm_args) < 0) {
    return scheme_false;
  }
#endif // !KAFFE

  StringClass = (*jenv)->FindClass(jenv, "java/lang/String");
  SchemeFunction_class = (*jenv)->FindClass(jenv, "edu/rice/cs/drj/SchemeFunction");
  if ((*jenv)->ExceptionOccurred(jenv)) {
    (*jenv)->ExceptionClear(jenv);
    (void)fprintf(stderr, "SchemeFunctions not loaded.\n");
  } else {
    newSchemeFunction = (*jenv)->GetMethodID(jenv, SchemeFunction_class, "<init>", "(I)V");
  }

  jvm_unit = (Scheme_Unit *)scheme_malloc(sizeof(Scheme_Unit));
  jvm_unit->type = scheme_unit_type;
  jvm_unit->num_imports = 0;
  jvm_unit->num_exports = NUM_PRIM;
  jvm_unit->exports = (Scheme_Object **)scheme_malloc(NUM_PRIM*sizeof(Scheme_Object *));

  jvm_unit->exports[0] = scheme_intern_symbol(JFIND_CLASS);
  jvm_unit->exports[1] = scheme_intern_symbol(JFIND_METHOD);
  jvm_unit->exports[2] = scheme_intern_symbol(JFIND_FIELD);
  jvm_unit->exports[3] = scheme_intern_symbol(JFIND_STATIC_METHOD);
  jvm_unit->exports[4] = scheme_intern_symbol(JFIND_STATIC_FIELD);
  jvm_unit->exports[5] = scheme_intern_symbol(JGET_FIELD);
  jvm_unit->exports[6] = scheme_intern_symbol(JSET_FIELD);
  jvm_unit->exports[7] = scheme_intern_symbol(JCALL);
  jvm_unit->exports[8] = scheme_intern_symbol(JNEW);
  jvm_unit->exports[9] = scheme_intern_symbol(J2S_STR);
  jvm_unit->exports[10] = scheme_intern_symbol(JSTR2BYTES);
  jvm_unit->export_debug_names = NULL;
  jvm_unit->init_func = &init_unit;
  jvm_unit->data = NULL;

  return (Scheme_Object *)jvm_unit;
}

Scheme_Object *scheme_reload(Scheme_Env *_) {
  return (Scheme_Object *)jvm_unit;
}
