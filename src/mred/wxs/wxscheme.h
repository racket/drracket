
#ifdef INCLUDE_WITHOUT_PATHS
#include "xcglue.h"
#else 
#include "../../mzscheme/utils/xcglue.h"
#endif

void wxsScheme_setup(Scheme_Env *env);

extern char *WXSCHEME_wrong, *WXSCHEME_too_many, *WXSCHEME_too_few;
extern char *WXSCHEME_expected;

#ifndef wx_msw
#undef USE_METAFILE
#define USE_METAFILE 0
#endif

