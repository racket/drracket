
/* 
   Configuration for compiling MzScheme

   If you want to set all the flags externally (on the command line
   with -D or some other compiler-dependent way), then define
   FLAGS_ALREADY_SET, and this file will be ignored.

   One flag cannot be set in this file: INCLUDE_WITHOUT_PATHS.
   Define this flag if your compiler doesn't like #include
   statements with relative paths using ".." and "/". (You will
   have to #define this for Macintosh CodeWarrior in the project 
   header.)

   The best flag settings are already provided for some auto-detected
   architecture/system/compilers. Otherwise, the default settings 
   are generic Unix.  Send other architecture/system/compiler-specific 
   info to "plt-bugs@cs.rice.edu".
*/

#ifndef FLAGS_ALREADY_SET

/*************** (BEGIN PLATFORM-INDEPENDENT OPTIONS) *************/

  /***********************/
 /* Language Extensions */
/***********************/

 /* NO_OBJECT_SYSTEM removes MzScheme's object system. */

 /* NO_REGEXP_UTILS removes MzScheme's regular expression utilities. */

 /* NO_TCP_SUPPORT removes MzScheme's TCP utilities */

 /* NO_SCHEME_THREADS removes MzScheme's threads from the Scheme user. 
    In this case, custodian-shutdown-all doesn't kill threads. */

 /* NO_FILE_SYSTEM_UTILS removes most file system utilities. */

#if defined(NO_FILE_SYSTEM_UTILS) \
	|| defined(NO_OBJECT_SYSTEM) \
	|| defined(NO_TCP_SUPPORT) \
	|| defined(NO_REGEXP_UTILS) \
	|| defined(NO_SCHEME_THREADS)
# define MZSCHEME_SOMETHING_OMITTED
#endif

  /*******************************/
 /* Evaluator Tuning Parameters */
/*******************************/

#define SCHEME_STACK_SIZE 5000

 /* SCHEME_STACK_SIZE <X> sets the size of stack segments for Scheme
    variables. */

/**************** (END PLATFORM-INDEPENDENT OPTIONS) **************/



/******** (BEGIN KNOWN ARCHITECTURE/SYSTEM CONFIGURATIONS) ********/
  
  /*************** OSKit with Linux/gcc *****************/

#if defined(OSKIT)

# define SYSTEM_TYPE_NAME "oskit"

# define STACK_GROWS_DOWN
# define DO_STACK_CHECK
# ifndef OSKIT_TEST
#  define OSKIT_FIXED_STACK_BOUNDS
# else
#  define ASSUME_FIXED_STACK_SIZE
#  define FIXED_STACK_SIZE 65500
# endif
# define STACK_SAFETY_MARGIN 10000

# define UNIX_FILE_SYSTEM
# define NO_UNIX_USERS

# define TIME_SYNTAX
# define DIR_FUNCTION
# define DIRENT_NO_NAMLEN
# define GETENV_FUNCTION

# ifndef OSKIT_TEST
#  define HAS_FREEBSD_IOB
# else
#  define HAS_LINUX_IOB
# endif
# define USE_OSKIT_CONSOLE
# define FILES_HAVE_FDS
# define FIXED_FD_LIMIT 256
# define USE_UNIX_SOCKETS_TCP
# define USE_FCNTL_O_NONBLOCK

# ifndef OSKIT_TEST
#  define INCLUDE_OSKIT_SOCKET
#  define MZ_PF_INET OSKIT_PF_INET
#  define PROTOENT_IS_INT IPPROTO_TCP
# endif

# define NO_USLEEP
# define NO_SLEEP
# define USER_TIME_IS_CLOCK

# define SIGSET_IS_SIGNAL

# define UNISTD_INCLUDE

# define USE_TM_GMTOFF_FIELD

# define FLAGS_ALREADY_SET

#else  /* >>>>>>> close after all platforms <<<<<<<<<< */

  /************** SunOS/Solaris with gcc ****************/

#if defined(sun)

# include "uconfig.h"

# define STACK_GROWS_DOWN

# define USE_EXPLICT_FP_FORM_CHECK
# define POW_HANDLES_INF_CORRECTLY

# include <errno.h>
# ifdef ECHRNG
/* Solaris */
#  if  defined(i386)
#   define SCHEME_PLATFORM_LIBRARY_SUBPATH "i386-solaris"
#  else
#   define SCHEME_PLATFORM_LIBRARY_SUBPATH "sparc-solaris"
#  endif
#  define DIRENT_NO_NAMLEN
#  define NO_USLEEP
#  define USE_ULIMIT
#  define USE_FCNTL_O_NONBLOCK
#  define USE_TIMEZONE_AND_ALTZONE_VAR

#  ifdef SOLARIS_THREADS
#   define MZ_REAL_THREADS
#   define MZ_USE_SOLARIS_THREADS

#ifdef __cplusplus
extern "C" {
#endif
void *scheme_solaris_init_threads(void);
void scheme_solaris_create_thread(void (*f)(void *), void *data, unsigned long *stackend, void **thp);
void scheme_solaris_exit_thread();
void scheme_solaris_break_thread(void *th);
struct Scheme_Process *scheme_solaris_get_current_process();
void scheme_solaris_set_current_process(struct Scheme_Process *);
void *scheme_solaris_make_mutex();
void scheme_solaris_free_mutex(void *);
void scheme_solaris_lock_mutex(void *);
void scheme_solaris_unlock_mutex(void *);
void *scheme_solaris_make_semaphore(int init);
void scheme_solaris_free_semaphore(void *);
int scheme_solaris_semaphore_up(void *);
int scheme_solaris_semaphore_down_breakable(void *);
int scheme_solaris_semaphore_try_down(void *);
#ifdef __cplusplus
}
#endif

#define SCHEME_INIT_THREADS() scheme_solaris_init_threads()
#define SCHEME_CREATE_THREAD(f, data, slimit, thp) scheme_solaris_create_thread(f, data, slimit, thp)
#define SCHEME_EXIT_THREAD() scheme_solaris_exit_thread()
#define SCHEME_BREAK_THREAD(th) scheme_solaris_break_thread(th)
#define SCHEME_GET_CURRENT_PROCESS() scheme_solaris_get_current_process()
#define SCHEME_SET_CURRENT_PROCESS(p) scheme_solaris_set_current_process(p)
#define SCHEME_MAKE_MUTEX() scheme_solaris_make_mutex()
#define SCHEME_FREE_MUTEX(m) scheme_solaris_free_mutex(m)
#define SCHEME_LOCK_MUTEX(m) scheme_solaris_lock_mutex(m)
#define SCHEME_UNLOCK_MUTEX(m) scheme_solaris_unlock_mutex(m)
#define SCHEME_MAKE_SEMA(init) scheme_solaris_make_semaphore(init)
#define SCHEME_FREE_SEMA(s) scheme_solaris_free_semaphore(s)
#define SCHEME_SEMA_UP(s) scheme_solaris_semaphore_up(s)
#define SCHEME_SEMA_DOWN_BREAKABLE(s) scheme_solaris_semaphore_down_breakable(s)
#define SCHEME_SEMA_TRY_DOWN(s) scheme_solaris_semaphore_try_down(s)
#  endif
# else
/* SunOS4 */
# define SCHEME_PLATFORM_LIBRARY_SUBPATH "sparc-sunos4"
# define SIGSET_IS_SIGNAL
# define USE_TM_GMTOFF_FIELD
# endif

# define FLAGS_ALREADY_SET

#endif

  /************** RS6000/AIX with gcc or xlc ****************/

# if defined(_IBMR2)

# define SCHEME_PLATFORM_LIBRARY_SUBPATH "rs6k-aix"

# include "uconfig.h"
# undef UNIX_DYNAMIC_LOAD
# undef USE_FCHDIR

# define STACK_GROWS_DOWN
# define UNIX_LIMIT_STACK 33554944

# define AIX_DYNAMIC_LOAD

# define SELECT_INCLUDE

# define POW_HANDLES_INF_CORRECTLY

# define USE_TIMEZONE_VAR_W_DLS

# define FLAGS_ALREADY_SET

#endif

  /************** x86/Linux with gcc ****************/

#if defined(linux) && defined(i386)

# define SCHEME_PLATFORM_LIBRARY_SUBPATH "i386-linux"

# include "uconfig.h"
# undef HAS_STANDARD_IOB
#ifndef __ELF__
# undef UNIX_DYNAMIC_LOAD
#endif

# define DIRENT_NO_NAMLEN

# define HAS_LINUX_IOB

# define STACK_GROWS_DOWN

# define USE_IEEE_FP_PREDS
# define USE_EXPLICT_FP_FORM_CHECK

# define SIGSET_IS_SIGNAL
# define SIGSET_NEEDS_REINSTALL

# define USE_DYNAMIC_FDSET_SIZE

# define USE_TIMEZONE_VAR

# define REGISTER_POOR_MACHINE

# ifdef LINUX_THREADS
#  define MZ_USE_PTHREADS
#  define MZ_USE_LINUX_PTHREADS
   /* More configuration below for pthreads */
# endif

# define FLAGS_ALREADY_SET

#endif

  /************** PowerPC/Linux with gcc ****************/

#if defined(linux) && defined(powerpc)

# define SCHEME_PLATFORM_LIBRARY_SUBPATH "ppc-linux"

# include "uconfig.h"
# undef HAS_STANDARD_IOB

# define DIRENT_NO_NAMLEN

# define HAS_LINUX_IOB

# define STACK_GROWS_DOWN

# define USE_IEEE_FP_PREDS
# define USE_EXPLICT_FP_FORM_CHECK

# define SIGSET_IS_SIGNAL
# define SIGSET_NEEDS_REINSTALL

# define USE_TIMEZONE_VAR

# define FLAGS_ALREADY_SET

#endif

  /************** x86/OpenBSD with gcc ****************/
              /* Thanks to Bengt Kleberg */

# if defined(__OpenBSD__) && defined(i386)

# define SCHEME_PLATFORM_LIBRARY_SUBPATH "i386-openbsd"

# include "uconfig.h"
# undef HAS_STANDARD_IOB

# define HAS_BSD_IOB

# define STACK_GROWS_DOWN

# define UNDERSCORE_DYNLOAD_SYMBOL_PREFIX

# define USE_IEEE_FP_PREDS
# define POW_HANDLES_INF_CORRECTLY

# define USE_DYNAMIC_FDSET_SIZE

# define SIGSET_IS_SIGNAL

# define REGISTER_POOR_MACHINE

# define USE_TM_GMTOFF_FIELD

# define FLAGS_ALREADY_SET

#endif

  /************** x86/FreeBSD with gcc ****************/

# if defined(__FreeBSD__) && defined(i386)

# define SCHEME_PLATFORM_LIBRARY_SUBPATH "i386-freebsd"

# include "uconfig.h"
# undef HAS_STANDARD_IOB

# define HAS_BSD_IOB

# define STACK_GROWS_DOWN

# ifdef FREEBSD_VERSION_2x
#  define UNDERSCORE_DYNLOAD_SYMBOL_PREFIX
# endif

# define USE_IEEE_FP_PREDS
# define FREEBSD_CONTROL_387
# define POW_HANDLES_INF_CORRECTLY

# define USE_DYNAMIC_FDSET_SIZE

# define SIGSET_IS_SIGNAL

# define USE_TM_GMTOFF_FIELD

# define REGISTER_POOR_MACHINE

# define FLAGS_ALREADY_SET

#endif

  /************** BeOS with egcs (and CodeWarrior?) ****************/

#if defined(__BEOS__)

# ifdef __INTEL__
#  define SCHEME_PLATFORM_LIBRARY_SUBPATH "i586-beos"
# else
#  define SCHEME_PLATFORM_LIBRARY_SUBPATH "ppc-beos"
#endif

# include "uconfig.h"
# undef UNIX_DYNAMIC_LOAD
# undef UNIX_FIND_STACK_BOUNDS
# undef USE_GETRUSAGE
# undef SYSTEM_TYPE_NAME

# define SYSTEM_TYPE_NAME "beos"

# define BEOS_IMAGE_DYNAMIC_LOAD
# define LINK_EXTENSIONS_BY_TABLE

# undef HAS_STANDARD_IOB
# undef FILES_HAVE_FDS
# define USE_BEOS_PORT_THREADS

# undef UNIX_PROCESSES
# define BEOS_PROCESSES

# define USE_FCNTL_O_NONBLOCK
# define MZ_PF_INET AF_INET
# define PROTOENT_IS_INT IPPROTO_TCP
# define CANT_SET_SOCKET_BUFSIZE
# define SEND_IS_NEVER_TOO_BIG

# define USE_BEOS_SNOOZE

# define SIGSET_IS_SIGNAL

# define DIRENT_NO_NAMLEN

# define BEOS_FIND_STACK_BOUNDS
# define STACK_GROWS_DOWN

# define USE_TM_GMTOFF_FIELD

# ifdef __INTEL__
#  define REGISTER_POOR_MACHINE
# endif

# define FLAGS_ALREADY_SET

#endif

  /************** SGI/IRIX with SGI cc ****************/

#if  (defined(mips) || defined(__mips)) \
     && !(defined(ultrix) || defined(__ultrix))

# define SCHEME_PLATFORM_LIBRARY_SUBPATH "mips-irix"

# include "uconfig.h"

# define STACK_GROWS_DOWN

# define DIRENT_NO_NAMLEN

# define BSTRING_INCLUDE

# define DEFEAT_FP_COMP_OPTIMIZATION
# define POW_HANDLES_INF_CORRECTLY

# define NO_INLINE_KEYWORD

# define NO_USLEEP
# define USE_FCNTL_O_NONBLOCK

# define USE_TIMEZONE_AND_ALTZONE_VAR

#  ifdef MZ_X_THREADS
#    ifndef MZ_FAKE_THREADS
#      define MZ_FAKE_THREADS
#    endif
#  endif

#  ifndef MZ_FAKE_THREADS
#    ifdef IRIX_SPROCS
#      define MZ_REAL_THREADS
#      define MZ_USE_IRIX_SPROCS

#ifdef __cplusplus
extern "C" {
#endif
void *scheme_sproc_init_threads(void);
void  scheme_sproc_create_thread(void (*f)(void *), void *data, unsigned long *stackend, void **thp);
void  scheme_sproc_exit_thread();
void  scheme_sproc_break_thread(void *);
struct Scheme_Process *scheme_sproc_get_current_process();
void  scheme_sproc_set_current_process(struct Scheme_Process *);
void *scheme_sproc_make_mutex();
void  scheme_sproc_free_mutex(void *);
void scheme_sproc_lock_mutex(void *);
void scheme_sproc_unlock_mutex(void *);
void *scheme_sproc_make_semaphore(int init);
void  scheme_sproc_free_semaphore(void *);
int   scheme_sproc_semaphore_up(void *);
int   scheme_sproc_semaphore_down_breakable(void *);
int   scheme_sproc_semaphore_try_down(void *);
#ifdef __cplusplus
}
#endif

#define SCHEME_INIT_THREADS() scheme_sproc_init_threads()
#define SCHEME_CREATE_THREAD(f, data, slimit, thp) scheme_sproc_create_thread(f, data, slimit, thp)
#define SCHEME_EXIT_THREAD() scheme_sproc_exit_thread()
#define SCHEME_BREAK_THREAD(th) scheme_sproc_break_thread(th)
#define SCHEME_GET_CURRENT_PROCESS() scheme_sproc_get_current_process()
#define SCHEME_SET_CURRENT_PROCESS(p) scheme_sproc_set_current_process(p)
#define SCHEME_MAKE_MUTEX() scheme_sproc_make_mutex()
#define SCHEME_FREE_MUTEX(m) scheme_sproc_free_mutex(m)
#define SCHEME_LOCK_MUTEX(m) scheme_sproc_lock_mutex(m)
#define SCHEME_UNLOCK_MUTEX(m) scheme_sproc_unlock_mutex(m)
#define SCHEME_MAKE_SEMA(init) scheme_sproc_make_semaphore(init)
#define SCHEME_FREE_SEMA(s) scheme_sproc_free_semaphore(s)
#define SCHEME_SEMA_UP(s) scheme_sproc_semaphore_up(s)
#define SCHEME_SEMA_DOWN_BREAKABLE(s) scheme_sproc_semaphore_down_breakable(s)
#define SCHEME_SEMA_TRY_DOWN(s) scheme_sproc_semaphore_try_down(s)
#    endif /* IRIX_SPROCS */
#  endif /* !MZ_FAKE_THREADS */

#  define FLAGS_ALREADY_SET

#endif

  /************** Ultrix with gcc ****************/

#if defined(ultrix) || defined(__ultrix)

# define SCHEME_PLATFORM_LIBRARY_SUBPATH "mips-ultrix"

# include "uconfig.h"
# undef UNIX_DYNAMIC_LOAD
# undef USE_FCHDIR

# define DIRENT_NO_NAMLEN

# define STACK_GROWS_DOWN

# define NO_USLEEP
# define USE_FCNTL_O_NONBLOCK

# define FLAGS_ALREADY_SET

#endif

  /************** ALPHA/OSF1 with gcc ****************/

#if defined(__alpha)

# define SCHEME_PLATFORM_LIBRARY_SUBPATH "alpha-osf1"

# include "uconfig.h"

# define STACK_GROWS_DOWN

# define SIXTY_FOUR_BIT_INTEGERS

# define ALPHA_CONTROL_FP

# define FLAGS_ALREADY_SET

#endif

  /************** HP/UX with cc ****************/

#if defined(_PA_RISC1_0) || defined(_PA_RISC1_1)

# define SCHEME_PLATFORM_LIBRARY_SUBPATH "parisc-hpux"

# include "uconfig.h"

# undef UNIX_DYNAMIC_LOAD
# define SHL_DYNAMIC_LOAD

# define STACK_GROWS_UP

# define USE_SYSCALL_GETRUSAGE

# define USE_DIVIDE_MAKE_INFINITY
# define USE_IEEE_FP_PREDS
# define USE_EXPLICT_FP_FORM_CHECK
# define ZERO_MINUS_ZERO_IS_POS_ZERO

# define NO_INLINE_KEYWORD

# define USE_ULIMIT

# define USE_TIMEZONE_VAR_W_DLS

# define FLAGS_ALREADY_SET

#endif

  /************** x86/SCO Unix with gcc ****************/
  /* Contributed by Atanas Ivanov <nasko@noac.bg>      */

#if defined(_M_XENIX) && defined(_M_SYSV)

# define SCHEME_PLATFORM_LIBRARY_SUBPATH "sco-i386"

# include "uconfig.h"
# undef HAS_STANDARD_IOB
#ifndef __ELF__
# undef UNIX_DYNAMIC_LOAD
#endif

# define DIRENT_NO_NAMLEN

# define HAS_SCO_IOB

# define STACK_GROWS_DOWN

# define USE_SCO_IEEE_FP_PREDS
# define USE_EXPLICT_FP_FORM_CHECK
# define USE_FCNTL_O_NONBLOCK

# define REGISTER_POOR_MACHINE

# define FLAGS_ALREADY_SET

#endif

  /******** Windows with MS Visual C++ or CYGWIN **********/
  /* See the "windows" directory for more MSVC details.   */
  /* MzScheme is probably no longer Borland-friendly,     */
  /* since it currently relies on one MSVC-style inline   */
  /* assembly file. Nevertheless, the old flags and       */
  /* instructions have been preserved.                    */

#if (defined(__BORLANDC__) || defined(_MSC_VER) || defined(__CYGWIN__)) \
    && (defined(__WIN32__) || defined(WIN32) || defined(_WIN32))

# define SCHEME_PLATFORM_LIBRARY_SUBPATH "win32\\i386"

# define SYSTEM_TYPE_NAME "windows"
# define DOS_FILE_SYSTEM
# if defined(_MSC_VER)
#  define NO_READDIR
#  define USE_FINDFIRST
#  define NO_READLINK
#  define MKDIR_NO_MODE_FLAG
# endif
# if defined(__BORLANDC__)
#  define DIRENT_NO_NAMLEN
#  define NO_READLINK
#  define MKDIR_NO_MODE_FLAG
# endif
# if defined(__CYGWIN__)
#  define USE_GET_CURRENT_DIRECTORY
#  define USE_WINDOWS_FIND_FIRST
#  define DIRENT_NO_NAMLEN
# endif

# define TIME_SYNTAX
# ifndef __CYGWIN__
#   define USE_FTIME
# endif
# define GETENV_FUNCTION
# define DIR_FUNCTION

# define USE_TIMEZONE_VAR_W_DLS

# define STACK_GROWS_DOWN
# define DO_STACK_CHECK
# define WINDOWS_FIND_STACK_BOUNDS

# define USE_MZ_SETJMP

# define WINDOWS_DYNAMIC_LOAD
# define LINK_EXTENSIONS_BY_TABLE

#if defined(_MSC_VER)
# define NAN_EQUALS_ANYTHING
# define POW_HANDLES_INF_CORRECTLY
#endif
#ifdef __CYGWIN__
# define USE_DIVIDE_MAKE_INFINITY
#endif

# define IO_INCLUDE
# define DONT_IGNORE_PIPE_SIGNAL

# define PROCESS_FUNCTION
#ifdef __CYGWIN__
# define UNIX_PROCESSES
# define FILES_HAVE_FDS
# define HAS_CYGWIN_IOB
# define SIGCHILD_DOESNT_INTERRUPT_SELECT
#else
# define NO_SLEEP
# define WINDOWS_PROCESSES
# define DETECT_WIN32_CONSOLE_STDIN
#endif

# define SIGSET_IS_SIGNAL
# define SIGSET_NEEDS_REINSTALL

#ifdef __CYGWIN__
# define USE_UNIX_SOCKETS_TCP
# define CANT_SET_SOCKET_BUFSIZE
# define NO_NEED_FOR_BEGINTHREAD
# define USE_CREATE_PIPE
#else
# define USE_WINSOCK_TCP
#endif

# ifdef WIN32_THREADS
#  define MZ_REAL_THREADS
#  define MZ_USE_WIN32_THREADS
#ifdef __cplusplus
extern "C" {
#endif
void *scheme_win32_init_threads(void);
void scheme_win32_create_thread(void (*f)(void *), void *data, unsigned long *stackend, void **thp);
void scheme_win32_exit_thread();
void scheme_win32_break_thread(void *th);
struct Scheme_Process *scheme_win32_get_current_process();
void scheme_win32_set_current_process(struct Scheme_Process *);
void *scheme_win32_make_mutex();
void scheme_win32_free_mutex(void *s);
void scheme_win32_lock_mutex(void *);
void scheme_win32_unlock_mutex(void *);
void *scheme_win32_make_semaphore(int init);
void scheme_win32_free_semaphore(void *s);
int scheme_win32_semaphore_up(void *);
int scheme_win32_semaphore_down_breakable(void *);
int scheme_win32_semaphore_try_down(void *);
#ifdef __cplusplus
}
#endif

#define SCHEME_INIT_THREADS() scheme_win32_init_threads()
#define SCHEME_CREATE_THREAD(f, data, slimit, thp) scheme_win32_create_thread(f, data, slimit, thp)
#define SCHEME_BREAK_THREAD(th) scheme_win32_break_thread(th)
#define SCHEME_EXIT_THREAD() scheme_win32_exit_thread()
#define SCHEME_GET_CURRENT_PROCESS() scheme_win32_get_current_process()
#define SCHEME_SET_CURRENT_PROCESS(p) scheme_win32_set_current_process(p)
#define SCHEME_MAKE_MUTEX() scheme_win32_make_mutex()
#define SCHEME_FREE_MUTEX(m) scheme_win32_free_mutex(m)
#define SCHEME_LOCK_MUTEX(m) scheme_win32_lock_mutex(m)
#define SCHEME_UNLOCK_MUTEX(m) scheme_win32_unlock_mutex(m)
#define SCHEME_MAKE_SEMA(init) scheme_win32_make_semaphore(init)
#define SCHEME_FREE_SEMA(s) scheme_win32_free_semaphore(s)
#define SCHEME_SEMA_UP(s) scheme_win32_semaphore_up(s)
#define SCHEME_SEMA_DOWN_BREAKABLE(s) scheme_win32_semaphore_down_breakable(s)
#define SCHEME_SEMA_TRY_DOWN(s) scheme_win32_semaphore_try_down(s)
# endif

/* MS Visual C++ likes underscore prefixes */
#if defined(_MSC_VER)
# define MSC_IZE(x) _ ## x
# define DIRECT_INCLUDE
#endif

#if defined(__BORLANDC__)
# define DIR_INCLUDE
#endif

# define REGISTER_POOR_MACHINE

# define WINLATIN_CHAR_SET

# define FLAGS_ALREADY_SET

#endif

  /************ Macintosh with CodeWarrior *************/

#if defined(__MWERKS__) && !defined(__BEOS__)

# if defined(__POWERPC__)
#  define SCHEME_PLATFORM_LIBRARY_SUBPATH "ppc-mac"
# else
#  define SCHEME_PLATFORM_LIBRARY_SUBPATH "68k-mac"
# endif

# define SYSTEM_TYPE_NAME "macos"
# define MAC_FILE_SYSTEM

#define NO_READDIR
#define NO_READLINK
#define USE_MAC_FILE_TOOLBOX

# define MACINTOSH_EVENTS
# define MACINTOSH_GIVE_TIME
# define MACINTOSH_SIOUX

# if !defined(__POWERPC__)
#  define MACINTOSH_SET_STACK
#  define COMPUTE_NEG_INEXACT_TO_EXACT_AS_POS
#  define NAN_LT_COMPARISON_WRONG
#  define SQRT_NAN_IS_WRONG
#  define ATAN2_DOESNT_WORK_WITH_INFINITIES
# else
#  define CODEFRAGMENT_DYNAMIC_LOAD
# endif

# ifndef MZSCHEME_IS_CODEFRAGMENT
#  define LINK_EXTENSIONS_BY_TABLE
# endif

# define STACK_GROWS_DOWN

# define DO_STACK_CHECK
# define MACOS_FIND_STACK_BOUNDS
# define STACK_SAFETY_MARGIN 10000

# define TIME_SYNTAX
# define USE_MACTIME
# define CLOCK_IS_USER_TIME
# define DIR_FUNCTION
# define TIME_TYPE_IS_UNSIGNED

# define NO_SYS_INCLUDE_SUBDIR
# define NO_USLEEP
# define UNISTD_INCLUDE
# define DONT_IGNORE_PIPE_SIGNAL

# define POW_HANDLES_INF_CORRECTLY

# define USE_MAC_TCP

# define SIGSET_IS_SIGNAL

# define MACROMAN_CHAR_SET

# define FLAGS_ALREADY_SET

#endif

  /************** DOS with Borland C++ ****************/
  /*          (Never successfully supported)          */

#if defined(__BORLANDC__) && defined(__MSDOS__)

# define SCHEME_PLATFORM_LIBRARY_SUBPATH "dos\\i386"

# define USE_SENORA_GC
# define DOS_FAR_POINTERS
# define SMALL_HASH_TABLES

# define SYSTEM_TYPE_NAME "dos"
# define DOS_FILE_SYSTEM
# define USE_GETDISK
# define DIRENT_NO_NAMLEN
# define NO_READLINK
# define MKDIR_NO_MODE_FLAG

# define TIME_SYNTAX
# define USE_FTIME
# define GETENV_FUNCTION
# define DIR_FUNCTION

# define STACK_GROWS_DOWN

# define DO_STACK_CHECK
# define USE_STACKAVAIL
# define STACK_SAFETY_MARGIN 15000

# define IGNORE_BY_CONTROL_387

# define DIR_INCLUDE
# define IO_INCLUDE
# define NO_SLEEP
# define DONT_IGNORE_PIPE_SIGNAL

# define REGISTER_POOR_MACHINE

# define FLAGS_ALREADY_SET

#endif


  /***************************************************/

#endif /* end not OSKit */

/************** (END KNOWN ARCHITECTURE/SYSTEMS) ****************/


/************** (BEGIN PTHREAD SETUP) ***************/

#ifdef MZ_USE_PTHREADS

# define MZ_REAL_THREADS

#ifdef __cplusplus
extern "C" {
#endif
void *scheme_pthread_init_threads(void);
void scheme_pthread_create_thread(void (*f)(void *), void *data, unsigned long *stackend, void **thp);
void scheme_pthread_exit_thread();
void scheme_pthread_break_thread(void *th);
struct Scheme_Process *scheme_pthread_get_current_process();
void scheme_pthread_set_current_process(struct Scheme_Process *);
void *scheme_pthread_make_mutex();
void scheme_pthread_free_mutex(void *);
void scheme_pthread_lock_mutex(void *);
void scheme_pthread_unlock_mutex(void *);
void *scheme_pthread_make_semaphore(int init);
void scheme_pthread_free_semaphore(void *);
int scheme_pthread_semaphore_up(void *);
int scheme_pthread_semaphore_down_breakable(void *);
int scheme_pthread_semaphore_try_down(void *);
#ifdef __cplusplus
}
#endif

#define SCHEME_INIT_THREADS() scheme_pthread_init_threads()
#define SCHEME_CREATE_THREAD(f, data, slimit, thp) scheme_pthread_create_thread(f, data, slimit, thp)
#define SCHEME_EXIT_THREAD() scheme_pthread_exit_thread()
#define SCHEME_BREAK_THREAD(th) scheme_pthread_break_thread(th)
#define SCHEME_GET_CURRENT_PROCESS() scheme_pthread_get_current_process()
#define SCHEME_SET_CURRENT_PROCESS(p) scheme_pthread_set_current_process(p)
#define SCHEME_MAKE_MUTEX() scheme_pthread_make_mutex()
#define SCHEME_FREE_MUTEX(m) scheme_pthread_free_mutex(m)
#define SCHEME_LOCK_MUTEX(m) scheme_pthread_lock_mutex(m)
#define SCHEME_UNLOCK_MUTEX(m) scheme_pthread_unlock_mutex(m)
#define SCHEME_MAKE_SEMA(init) scheme_pthread_make_semaphore(init)
#define SCHEME_FREE_SEMA(s) scheme_pthread_free_semaphore(s)
#define SCHEME_SEMA_UP(s) scheme_pthread_semaphore_up(s)
#define SCHEME_SEMA_DOWN_BREAKABLE(s) scheme_pthread_semaphore_down_breakable(s)
#define SCHEME_SEMA_TRY_DOWN(s) scheme_pthread_semaphore_try_down(s)

#endif

/************** (END PTHREAD SETUP) ***************/

/***** (BEGIN CONFIGURATION FLAG DESCRPTIONS AND DEFAULTS) ******/

#ifndef FLAGS_ALREADY_SET

  /*********************/
 /* Operating System  */
/*********************/

#define SYSTEM_TYPE_NAME "unix"

  /* SYSTEM_TYPE_NAME must be a string; this will be converted into
     a symbol for the result of (system-type) */

  /* SCHEME_PLATFORM_LIBRARY_SUBPATH must be a string; if it is 
     undefined, it is automatically generated into a file named
     "schsys.h" into the same directory as .o files and #included
     by string.c. This string is returned by (system-library-subpath) */

  /*********************/
 /* Language Features */
/*********************/

#define TIME_SYNTAX
#define PROCESS_FUNCTION
#define DIR_FUNCTION
#define GETENV_FUNCTION

 /* TIME_SYNTAX adds the (time ...) syntax; this may need to be
     turned off for compilation on some systems.
    CLOCKS_PER_SEC relates the values returned by clock() to
     real seconds. (The difference between two clock() calls is
     devided by this number.) Usually, this is defined in <time.h>;
     it defaults to 1000000 */

 /* USE_FTIME uses ftime instead of gettimeofday; only for TIME_SYNTAX */
 
 /* USE_DIFFTIME uses time and difftime; only for TIME_SYNTAX */
 
 /* USE_MACTIME uses the Mac toolbox to implement time functions. */

 /* CLOCK_IS_USER_TIME uses the system time for user milliseconds. */

 /* USER_TIME_IS_CLOCK uses the user time for system milliseconds. */

 /* TIME_TYPE_IS_UNSIGNED converts time_t values as unsigned. */

 /* PROCESS_FUNCTION adds (process ...) and (system ...) functions */

 /* DIR_FUNCTION adds (current-directory ...) function */

 /* GETENV_FUNCTION adds (getenv ...) function */

 /* USE_TIMEZONE_VAR gets timezone offset from a timezone global.
    USE_TIMEZONE_VAR_W_DLS is similar, but adds 1 hour when daylight 
     savings is in effect.
    USE_TIMEZONE_AND_ALTZONE_VAR is similar, but uses altzone when
     daylight savings is in effect.
    USE_TM_GMTOFF_FIELD gets timezone offset from the tm_gmtoff field
     of the tm struct. */
 

  /*******************/
 /*   Filesystem    */
/*******************/

#define UNIX_FILE_SYSTEM
#define EXPAND_FILENAME_TILDE

 /* UNIX_FILE_SYSTEM indicates that filenames are as in Unix, with
    forward slash separators, ".." as the parent directory, "/" 
    as the root directory, and case-sensitivity */

 /* DOS_FILE_SYSTEM indicates that filenames are as in DOS, with 
    slash or backward slash separators, ".." as the parent directory, 
    "X:\", "X:/", "\", or "/" as a root directory (for some letter X),
    and case insensitivity */

 /* MAC_FILE_SYSTEM indicates that filenames are as on the Macintosh,
    with colon separators, "" as the parent directory, a volume name 
    (followed by a colon) as a root directory, and case insensitivity. */

 /* EXPAND_FILENAME_TILDE expands ~ in a filename with a user's home
     directory. */

 /* NO_STAT_PROC means that there is no stat() function. */

 /* NO_MKDIR means that there is no mkdir() function. */

 /* NO_READLINK means that there is no readlink() function. */

 /* USE_GETDISK uses getdisk() and setdisk() to implement the
     filesystem-root-list primitive under DOS. */

 /* NO_READDIR means that there is no opendir() and readdir() for
     implementing directory-list. */

 /* DIRENT_NO_NAMLEN specifies that dirent entries do not have a
     d_namlen field; this is used only when NO_READDIR is not
     specified. */
     
 /* MKDIR_NO_MODE_FLAG specifies that mkdir() takes only one argument,
     instead of a directory name and mode flags. */

 /* USE_GET_CURRENT_DIRECTORY uses Windows's GetCurrentDirectory()
    instead of getcwd(). */

 /* USE_WINDOWS_FIND_FIRST uses Window's FindFirstFile(), etc.
    instead for _findfirst(), etc. */

  /***********************/
 /*       Ports         */
/***********************/

/* These are flags about the implementation of char-ready? for FILE*s
   None of these flags are required, but char-ready? may return
   spurious #ts if they are set up incorrectly. */

#define HAS_STANDARD_IOB
#define FILES_HAVE_FDS
#define USE_UNIX_SOCKETS_TCP

 /* HAS_STANDARD_IOB, HAS_GNU_IOB, HAS_CYGWIN_IOB, HAS_LINUX_IOB,
    HAS_BSD_IOB, and HAS_SCO_IOB are mutually exclusive; they describe
    how to read the FILE* structure to determine if there are
    available cached characters. */

 /* FILES_HAVE_FDS means that a FILE* is always associated with a
    file desciptor, which can be select-ed to see if there are
    pending bytes. Don't use this unless one of the HAS_<X>_IOB
    flags is used. */

 /* USE_UNIX_SOCKETS_TCP means that the tcp- procedures can be implemented
    with the standard Unix socket functions. */

 /* USE_WINSOCK_TCP means that the tcp- procedures can be implemented
    with the Winsock toolbox. */

 /* USE_MAC_TCP means that the tcp- procedures can be implemented
    with the Mac TCP toolbox. */

 /* DETECT_WIN32_CONSOLE_STDIN notices character reads from console
    stdin so that char-ready? and blocking reads can be implemented
    correctly (so that Scheme threads are not blocked when no input
    is ready). If NO_STDIO_THREADS is defined, this flag is ignored. */

 /* NO_STDIO_THREADS turns off special Windows handling for stdin
    and process input ports. The special handling implements char-ready?
	and non-blocking reads (so that reading from one of these ports does
	not block other MzScheme threads). */

 /* USE_FCNTL_O_NONBLOCK uses O_NONBLOCK instead of FNDELAY for
    fcntl on Unix TCP sockets. (Posix systems need this flag). */

 /* USE_ULIMIT uses ulimit instead of getdtablesize (Unix). */

 /* USE_DYNAMIC_FDSET_SIZE allocates fd_set records based on the
    current fd limit instead of relying on the compile-time size
    of fd_set. */

 /* UNIX_LIMIT_FDSET_SIZE insures that the fd limit at start-up is
    no greater than FD_SETSIZE */

 /* CANT_SET_SOCKET_BUFSIZE turns off setting the buffer size for
    Unix TCP sockets. */

  /***********************/
 /* Processes & Signals */
/***********************/

/* These are flags about the implementation of system, process, etc. */

# define UNIX_PROCESSES
# define SIGSET_IS_SIGNAL
# define SIGSET_NEEDS_REINSTALL

 /* UNIX_PROCESSES implements the process functions for Unix; uses
    sigset() to install the signal handler. */

 /* WINDOWS_PROCESSES implements the process functions for Windows. */

 /* SIGSET_IS_SIGNAL uses signal() in place of sigset() for Unix. This 
    flag is often paired with SIGSET_NEEDS_REINSTALL for traditional
    Unix systems. */

 /* SIGSET_NEEDS_REINSTALL reinstalls a signal handler when it
    is called to handle a signal. The expected semantics of sigset()
    (when this flags is not defined) is that a signal handler is NOT
    reset to SIG_DFL after a handler is called to handle a signal. */

 /* DONT_IGNORE_FPE_SIGNAL stops MzScheme from ignoring floating-point 
    exception signals. */

 /* DONT_IGNORE_PIPE_SIGNAL stops MzScheme from ignoring SIGPIPE
    signals. */

 /* USE_CREATE_PIPE uses CreatePipe() instead of _pipe() for Windows. */

 /* SIGCHILD_DOESNT_INTERRUPT_SELECT indicates that the SIGCHILD
    signal, sent when a child OS process dies, does not interrupt
    select(). This flag is needed for Cygwin B20. */

  /**********************/
 /* Inexact Arithmetic */
/**********************/

 /* USE_SINGLE_FLOATS turns on support for single-precision
    floating point numbers. Otherwise, floating point numbers
    are always represented in double-precision. */

 /* USE_SINGLE_FLOATS_AS_DEFAULT, when used with
    USE_SINGLE_FLOATS, causes exact->inexact coercions to
    use single-precision numbers as the result rather
    than double-precision numbers. */

 /* USE_INFINITY_FUNC uses infinity() to get the infinity
     floating-point constant instead of using HUGE_VAL. */

 /* USE_DIVIDE_MAKE_INFINITY creates +inf.0 by dvividing by zero
    instead of using HUGE_VAL. */

 /* USE_IEEE_FP_PREDS uses isinf() and isnan() to implement tests for
    infinity. */

 /* USE_SCO_IEEE_FP_PREDS uses fpclass() and isnan() to implement tests for
    infinity. */

 /* DEFEAT_FP_COMP_OPTIMIZATION avoids a compiler optimization that
    converts (a == a) to TRUE, even if `a' is floating-point. Used
    only when USE_[SCO_]IEEE_FP_PREDS is not defined. */

 /* IGNORE_BY_CONTROL_387 turns off floating-point error for
    Intel '387 with _control87. DONT_IGNORE_PIPE_SIGNAL can be on or
    off. */

 /* FREEBSD_CONTROL_387 controls the floating-point processor under i386
    FreeBSD */

 /* LINUX_CONTROL_387 controls the floating-point processor under i386
    Linux using __setfpucw(). libc 6.1 doesn't export __setfpucw() and
    it doesn't matter; for Linux 2.0 and up, the default FP behavior
    is the one we want. This flag might be needed for older versions
    of Linux. */

 /* APLHA_CONTROL_FP controls the floating-point processor for Alpha
    OSF1 */

 /* NAN_EQUALS_ANYTHING indicates that the compiler is broken and
    equality comparisons with +nan.0 always return #t. Currently
    used for MSVC++ */
    
 /* ZERO_MINUS_ZERO_IS_POS_ZERO indicates that something (compiler?
    machine? fp flags?) is broken so that 0.0 - 0.0 = 0.0 instead of
    -0.0. This flag doesn't fix MzScheme completely, since (- 0.0) is
    still 0.0, but at least it lets MzScheme read and print 0.0 and
    -0.0 accurately. Currently used for HP/UX. */

 /* NAN_LT_COMPARISON_WRONG indicates that +nan.0 is not handled correctly
    by < or <=. Probably the compiler implements < as !>. */

 /* USE_EXPLICT_FP_FORM_CHECK circumvents bugs in strtod() under Linux,
    SunOS/Solaris, and HP/UX by explicit pre-checking the form of the 
    number and looking for values that are obviously +inf.0 or -inf.0 */

 /* POW_HANDLES_INF_CORRECTLY inidicates that thw pow() library procedure
    handles +/-inf.0 correctly. Otherwise, code in inserted to specifically
    check for infinite arguments. */
    
 /* ATAN2_DOESNT_WORK_WITH_INFINITIES indicates that atan2(+/-inf, +/-inf)
    is not the same as atan2(1, 1). */ 
    
 /* SQRT_NAN_IS_WRONG indicates that (sqrt +nan.0) must be forced to +nan.0
    (i.e., the C library function is bad). */
    
 /* COMPUTE_NEG_INEXACT_TO_EXACT_AS_POS computes inexact->exact of some
    negative inexact number x by computing the result for -x and negating
    it. Use this if (inexact->exact -0.1) is wrong. */

  /***********************/
 /* Stack Maniuplations */
/***********************/

# define DO_STACK_CHECK
# define UNIX_FIND_STACK_BOUNDS
# define STACK_SAFETY_MARGIN 50000

 /* STACK_GROWS_UP means that deeper stack values have higher
     numbered addresses.
    STACK_GROWS_DOWN means that deeper stack values have lower
     numbered addresses. This is usually the case (Sparc and
     Intel platforms, for example, use this).
    Use only one or none of these. (It's faster if you know which
     one applies, but it can also be figured it out dynamically.) */

 /* DO_STACK_CHECK checks for stack overflow during execution.
     Requires either UNIX_FIND_STACK_BOUNDS, USE_STACKAVAIL,
     MACOS_FIND_STACK_BOUNDS, or ASSUME_FIXED_STACK_SIZE. */

 /* UNIX_FIND_STACK_BOUNDS figures out the maximum stack position
     on Unix systems, using getrlimit() and the GC_find_stack_base()
     defined in the conservative garbage collector.
    USE_STACKAVIL uses stackavail() function for checking stack
     overflow; works with Borland C++, maybe other compilers.
    WINDOWS_FIND_STACK_BOUNDS figures out the maximum stack position
     under Windows (uses GC_find_stack_base())
    MACOS_FIND_STACK_BOUNDS figures out the stack limit on the Mac.
    ASSUME_FIXED_STACK_SIZE assumes that the main stack size is
     always FIXED_STACK_SIZE.
    Use only one of these if DO_STACK_CHECK is used, or none otherwise. */

 /* FIXED_STACK_SIZE <X> sets the stack size to <X> when the
     ASSUME_FIXED_STACK_SIZE stack-checking mode is on. */

 /* STACK_SAFETY_MARGIN <X> sets the number of bytes that should be
     available on the stack for "safety" to <X>. Used only if
     DO_STACK_CHECK is used. STACK_SAFETY_MARGIN defaults to 50000. */

 /* UNIX_LIMIT_STACK <X> limits stack usage to <X> bytes. This may
     be necessary to avoid GC-setup traversal over too much memory
     (with GC flag HEURISTIC2?). */

 /* OSKIT_FIXED_STACK_BOUNDS uses base_stack_start to get the hottest
     stack location */

  /***********************/
 /*   Dynamic Loading   */
/***********************/

#define UNIX_DYNAMIC_LOAD

 /* UNIX_DYNAMIC_LOAD implements dynamic extensions under Unix
     using dlopen(); you may have to add the -ldl flag in the LIBS 
     Makefile variable. The library doesn't exist under Linux without 
     ELF, so it won't work. If you get linker errors about dlopen(), etc., 
     this flag and the -ldl linker flag are the things to adjust.
    SHL_DYNAMIC_LOAD implement HP/UX dynamic loading.
    WINDOWS_DYNAMIC_LOAD implements dynamic extensions under Windows
     (Thanks to Patrick Barta).
    CODEFRAGMENT_DYNAMIC_LOAD implements dynamic extensions with
     MacOS's Code Fragment Manager (thanks to William Ng).
    Use only one or none of these. */

 /* UNDERSCORE_DYNLOAD_SYMBOL_PREFIX with UNIX_DYNAMIC_LOAD menas that
    an extra underscore ("_") must be placed in front of the name passed 
    to dlopen(). */

 /* LINK_EXTENSIONS_BY_TABLE specifies that the MzScheme functions
    used by an extension must be manually linked via a table of
    function pointers. Windows dynamic linking uses this method. */

 /* MZSCHEME_IS_CODEFRAGMENT exploits improved CFM linking when
    MzScheme is itself a shared library instead of embedded in
    an application */

  /***********************/
 /*     Heap Images     */
/***********************/

 /* UNIX_IMAGE_DUMPS turns on image save and restore for Unix systems. 
     This will only work if the final application is statically linked. 
     (As an exception, the dynamic-linking library itself can be 
     dynamically linked. This works because loading an extension in 
     MzScheme automatically turns off image saving.) */

  /*****************************/
 /*   Macintosh Standalone    */
/*****************************/

 /* MACINTOSH_EVENTS checks for a user break on the Mac. This should always
     be defined for MacOS. */

 /* MACINTOSH_GIVE_TIME lets background processes run when checking for
     a user break. */

 /* MACINTOSH_SIOUX interfaces with Metrowerks's SIOUX library */

 /* MACINTOSH_SET_STACK sets the stack to be 1/4 of the heap. This should
     be used for 68k machines, where the stack is not user-configurable. */

  /***********************/
 /*    Miscellaneous    */
/***********************/

#define UNISTD_INCLUDE
#define NO_INLINE_KEYWORD

 /* REGISTER_POOR_MACHINE guides a hand optimization that seems to
    be work best one way for Sparc machines, and better the other
    way for x86 machines. */

 /* SIXTY_FOUR_BIT_INTEGERS indicates that 'long's are 64-bits wide. */

 /* MACROMAN_CHAR_SET indicates that latin1-integer->char should convert
    Latin-1 values to MacRoman characters. */

 /* WINLATIN_CHAR_SET indicates that latin1-integer->char should return
    #f for values in #x80 to #x9F, and char->latin-1-integer should
    return #f for characters in that range. */

 /* NO_INLINE_KEYWORD indicates that the C compiler doesn't recognize
    C's `inline' keyword. */

 /* NO_USER_BREAK_HANDLER turns off handling of INT signal in main.c */

 /* DIR_INCLUDE if there's a <dir.h> file (mainly for Windows). */

 /* DIRECT_INCLUDE if there's a <direct.h> file (mainly for Windows). */

 /* IO_INCLUDE if there's a <io.h> file (mainly for Windows). */

 /* UNISTD_INCLUDE if there's a <unistd.h> file (mainly for Unix). */

 /* SELECT_INCLUDE if there's a <sys/select.h> file (mainly for Unix) 
     to be used with FILES_HAVE_FDS. */

 /* BSTRING_INCLUDE if there's a <bstring.h> file (mainly for Unix) 
     to be used with FILES_HAVE_FDS. */

 /* NO_SYS_INCLUDE_SUBDIR if include files should all be <XXX.h>; no
     includes of the form <sys/XXX.h>. Mainly used for <sys/stat.h>
     for MacOS. */

 /* USE_FCHDIR uses fchdir() to improve thread context switches when
    a small number of threads are active. */

 /* USE_GETRUSAGE uses getrusage() to for timing info; otherwise clock()
    is used. */

 /* USE_SYSCALL_GETRUSAGE uses syscall() to implement getrusage() for
    timing info. Used with USE_GETRUSAGE. */

 /* NO_SLEEP means that there is no sleep() function. Used only in
    standalone MzScheme. */

 /* NO_USLEEP means that there is no usleep() function. Used only in 
    standalone MzScheme. Used only if NO_SLEEP is undefined. */

 /* NO_NEED_FOR_BEGINTHREAD indicates that the C library used for
    Windows is always thread-ready and there's no need use the
	_beginthreadex() function instead of CreateThread(). This is only
	used when stdin and process ports are tested in a separate thread
	(see NO_STDIO_TREADS). */

 /* WIN32S_HACK uses a special hack to implement threads under Win32s
    with some compilers. Obsolete. */

#endif  /* FLAGS_ALREADY_SET */

/****** (END CONFIGURATION FLAG DESCRPTIONS AND DEFAULTS) *******/

#endif  /* FLAGS_ALREADY_SET */
