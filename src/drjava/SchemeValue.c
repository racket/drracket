#include "escheme.h"
#include "jni.h"
#include "mzjvm.h"

static jfieldID SchemeValue_ptr;

Scheme_Object *saveRef(jobject);
extern Scheme_Type jobject_type;

void Java_edu_rice_cs_drj_SchemeValue_initNative(JNIEnv *env, jclass me) {
  SchemeValue_ptr = (*env)->GetFieldID(env, me, "ptr", "I"); 
}

void Java_edu_rice_cs_drj_SchemeList_init__(JNIEnv *env, jobject me) {
  // The jvm spec guarantees that a jint is as big as a native pointer.
  (*env)->SetIntField(env, me, SchemeValue_ptr, (jint)scheme_null); 
}

#include "SchemeValue-gen.c"

//#include <stdio.h>
void Java_edu_rice_cs_drj_SchemeFunction_applyVoid(JNIEnv *env, jobject obj, jobject args) {
//(void)fprintf(stderr, "About to applyVoid(). env = %p\n", env);
    scheme_apply_to_list((Scheme_Object*)((*env)->GetIntField(env, obj, SchemeValue_ptr)),
                         (Scheme_Object*)((*env)->GetIntField(env, args, SchemeValue_ptr)));
//(void)fprintf(stderr, "Done with applyVoid(). env = %p\n", env);
}
