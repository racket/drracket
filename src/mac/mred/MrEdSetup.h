
#define INCLUDE_WITHOUT_PATHS

#ifdef __MWERKS__
#if defined(__powerc)
#include <MacHeadersPPC>
#else
#include <MacHeaders68K>
#endif
#endif

#define WX_STANDARD_GRAPHICS 1

#define WX_FORCE_APP_CREATION 1
#define WXS_CANT_ASSIGN_STRUCTURES 1

#define WXUNUSED(x)

#define OPERATOR_NEW_ARRAY

#define WXME_FOR_MRED 1

/* code added by JBC because compiler no longer handles keyword 
   'far' for PPC */
   
#if defined(__powerc)
#  define WX_FAR /**/
# else
#  define WX_FAR far
# endif
