
#define USE_COMPILED_MACROS 1

#if defined(__MWERKS__) && !defined(powerc)
#define MZCOMPILED_STRING_FAR far
#else
#define MZCOMPILED_STRING_FAR /**/
#endif

#if USE_COMPILED_MACROS
extern Scheme_Object *scheme_eval_compiled_sized_string(const char *str, int len, Scheme_Env *env);
#endif
