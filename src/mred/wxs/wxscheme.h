
#ifdef INCLUDE_WITHOUT_PATHS
# include "xcglue.h"
#else 
# include "../../mzscheme/utils/xcglue.h"
#endif

void wxsScheme_setup(Scheme_Env *env);

#ifndef wx_msw
# undef USE_METAFILE
# define USE_METAFILE 0
#endif
