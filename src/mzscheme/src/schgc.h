
#ifdef INCLUDE_WITHOUT_PATHS
# ifdef USE_SENORA_GC
#  include "sgc.h"
# else
#  include "gc.h"
# endif
#else
# ifdef USE_SENORA_GC
#  include "../sgc/sgc.h"
# else
#  include "../gc/gc.h"
# endif
#endif
