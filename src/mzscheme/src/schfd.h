
#if defined(DETECT_WIN32_CONSOLE_STDIN) || defined(WINDOWS_PROCESSES)
#define USE_FAR_MZ_FDINIT
#endif
#ifdef USE_DYNAMIC_FDSET_SIZE
#define USE_FAR_MZ_FDINIT
#endif

#if defined(USE_FAR_MZ_FDCALLS) || defined(USE_FAR_MZ_FDINIT)
#define DECL_FDSET(n, c) static fd_set *n
#define INIT_DECL_FDSET(n, c) (n = (n ? (fd_set *)scheme_init_fdset_array(n, c) : (fd_set *)scheme_alloc_fdset_array(c, 1)))
#else
#define DECL_FDSET(n, c) fd_set n[c]
#define INIT_DECL_FDSET(n, c) /* empty */
#endif
