
/* 
   The cXXX.inc files (where XXX is `macro' or `unitsig') are not, as
   distributed, compatible with omissions from or additions to the set
   of built-in identifiers.  The reason is that the cXXX.inc files are
   .zo versions of the XXX.inc files, and the .zo format changes when
   the set of built-in names changes (because indices assigned to the
   built-in names shift).

   If you make a version with omissions or additions and then run
   `mzmake cmacro' to recreate the cXXX.inc files, set
   EXPECTED_PRIM_COUNT to the new value, and then USE_COMPILED_MACROS
   can be set to 1 again.
*/

#define USE_COMPILED_MACROS 1

#define EXPECTED_PRIM_COUNT 597

#ifdef MZSCHEME_SOMETHING_OMITTED
# undef USE_COMPILED_MACROS
# define USE_COMPILED_MACROS 0
#endif

#if defined(__MWERKS__) && !defined(powerc)
#define MZCOMPILED_STRING_FAR far
#else
#define MZCOMPILED_STRING_FAR /**/
#endif

#if USE_COMPILED_MACROS
extern Scheme_Object *scheme_eval_compiled_sized_string(const char *str, int len, Scheme_Env *env);
#endif
