typedef Scheme_Object *(*Caller_t)(jobject obj, jmethodID id, jvalue *args);
typedef Scheme_Object *(*Getter_t)(jobject obj, jfieldID id);
typedef void (*Setter_t)(jobject obj, jfieldID id, Scheme_Object *v);

typedef struct {
  Scheme_Type tag;
  jclass val;
} jclass_t;

typedef struct {
  Scheme_Type tag;
  jmethodID val;
  Caller_t mc;
} jmethod_t;

typedef struct {
  Scheme_Type tag;
  jfieldID val;
  Getter_t get;
  Setter_t set;
} jfield_t;

typedef struct {
  Scheme_Type tag;
  jobject val;
} jobject_t;

typedef Caller_t (*typeFinder_t)(int len, char *sig);
typedef jmethodID (*getMethID_t)(JNIEnv *env, jclass clazz, const char *name, const char *sig); 

typedef jfieldID (*getField_t)
  (JNIEnv *env, jclass clazz, const char *name, const char *sig); 
typedef void (*getSet_t)(jfield_t *ref, char *sig);

