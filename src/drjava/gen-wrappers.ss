; gen2-call : String Static -> String String -> String
(define (gen2-call static arg)
  (lambda (type return)
    (let* ([lst (string->list type)]
	   [native (list->string (cons #\j (cons (char-downcase (car lst)) (cdr lst))))])
      (string-append "static Scheme_Object *" nl
		     "Call_" static type "("arg" obj, jmethodID id, jvalue *args) {" nl
		     "  " native " v = (*jenv)->Call" static type "MethodA(jenv, obj, id, args);" nl
		     "  return " return ";" nl
		     "}" nl nl))))

;; gen2-getter : String Static -> String String -> String
(define (gen2-getter static arg)
  (lambda (type return)
    (let* ([lst (string->list type)]
	   [native (list->string (cons #\j (cons (char-downcase (car lst)) (cdr lst))))])
      (string-append "static Scheme_Object *" nl
		     "Get_" static type "("arg" obj, jfieldID id) {" nl
		     "  " native " v = (*jenv)->Get"static type"Field(jenv, obj, id);" nl
		     "  return "return";" nl
		     "}" nl nl))))

;; gen2-setter : String String -> String String -> String
(define (gen2-setter static arg)
  (lambda (type to-jvm)
    (string-append "static void" nl
		   "Set_" static type "("arg" obj, jfieldID id, Scheme_Object *v) {" nl
		   "  (*jenv)->Set"static type"Field(jenv, obj, id, "to-jvm");" nl
		   "}" nl nl)))

;; gen-list : String^3 -> String
(define (gen-list sig to-scheme jni-type)
  (string-append "void Java_edu_rice_cs_drj_SchemeList_init__" sig "Ledu_rice_cs_drj_SchemeList_2(JNIEnv *env, jobject me, " jni-type " v, jobject cdr) {" nl
		 "  Scheme_Object *car =" nl
		 "    scheme_make_pair(" to-scheme "," nl
		 "      (Scheme_Object*)((*env)->GetIntField(env, cdr, SchemeValue_ptr)));" nl
		 "  (*env)->SetIntField(env, me, SchemeValue_ptr, (jint)car);" nl
		 "}" nl nl))

;; gen-apply : ? -> String
(define (gen-apply jvm-type type return)
  (string-append jvm-type " Java_edu_rice_cs_drj_SchemeFunction_apply" type "(JNIEnv *env, jobject obj, jobject args) {" nl
		 "  Scheme_Object *v = " nl
		 "    scheme_apply_to_list((Scheme_Object*)((*env)->GetIntField(env, obj, SchemeValue_ptr))," nl
		 "                         (Scheme_Object*)((*env)->GetIntField(env, args, SchemeValue_ptr)));" nl
		 "    return " return ";" nl
		 "}" nl nl))

(define nl (list->string (list #\newline)))

(define types '("Boolean" "Byte" "Char" "Short" "Int" "Long" "Float" "Double" "Object"))
(define jni-sigs '("Z" "B" "C" "S" "I" "J" "F" "D" "Ljava_lang_Object_2"))
(define jni-types '("jboolean" "jbyte" "jchar" "jshort" "jint" "jlong" "jfloat" "jdouble" "jobject"))

(define java->scheme
  (let ([short "scheme_make_integer(v)"]
	[long "scheme_make_integer_value(v)"]
	[double "scheme_make_double(v)"])
    (list "v ? scheme_true : scheme_false"
	  "scheme_make_character(v)"
	  short
	  short
	  long
	  long
	  double
	  double
	  "saveRef(v)")))

(define scheme->java
  (let ([int "SCHEME_INT_VAL(v)"]
	[float "SCHEME_FLOAT_VAL(v)"])
    (list "(v == scheme_false) ? JNI_FALSE : JNI_TRUE"
	  "SCHEME_CHAR_VAL(v)"
	  int
	  int
	  int
	  int
	  float
	  float
	  ;; replace below with "s2jVal(v)"
	  (string-append "SCHEME_TYPE(v) == jobject_type ?" nl
			 "    ((jobject_t *)v)->val :" nl
			 "    (scheme_signal_error(\"not a Java object\"), obj)"))))

(define out (open-output-file "generated.c" 'truncate))
(define out2 (open-output-file "SchemeValue-gen.c" 'truncate))

; run : (String String -> String) (listof String) -> Void
(define (run f out . lst)
  (apply for-each (lambda x (display (apply f x) out)) lst))

(define (run1 f lst) (run f out types lst))

(run1 (gen2-call "" "jobject") java->scheme)
(run1 (gen2-getter "" "jobject") java->scheme)
(run1 (gen2-setter "" "jobject") scheme->java)

(run1 (gen2-call "Static" "jclass") java->scheme)
(run1 (gen2-getter "Static" "jclass") java->scheme)
(run1 (gen2-setter "Static" "jclass") scheme->java)

(run gen-list out2 jni-sigs java->scheme jni-types)
(run gen-apply out2 jni-types types scheme->java)
