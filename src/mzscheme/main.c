/*
  MzScheme
  Copyright (c) 1995-97 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#include "scheme.h"

#ifdef FILES_HAVE_FDS
# include <sys/types.h>
# include <sys/time.h>
# ifdef SELECT_INCLUDE
#  include <sys/select.h>
# endif
#endif
#ifndef NO_USER_BREAK_HANDLER
# include <signal.h>
#endif
#ifdef UNISTD_INCLUDE
# include <unistd.h>
#endif
#ifdef USE_LOCALE
# include <locale.h>
#endif
#ifdef MACINTOSH_EVENTS
# include <Events.h>
#endif
#ifdef MACINTOSH_SIOUX
# include <console.h>
# include <SIOUX.h>
#endif
#ifdef MACINTOSH_SET_STACK
# include <Memory.h>
#endif
#ifdef MACINTOSH_EVENTS
# include "simpledrop.h"
#endif

/* #define NO_GCING */

#if defined(_IBMR2)
static void dangerdanger(int ignored)
{
  char *s = "mzscheme: danger - paging space low\n";
  write(2, s, strlen(s));
  scheme_collect_garbage();
}
#endif

#ifndef NO_USER_BREAK_HANDLER

#ifdef MACINTOSH_EVENTS
static int break_flag;
static Scheme_Object *orig_evaluator;
#endif

#ifndef MACINTOSH_EVENTS
extern Scheme_Process *scheme_main_process;
 
static void user_break_hit(int ignore)
{
  Scheme_Process *p = scheme_main_process;

  scheme_break_thread(p);

# ifdef SIGSET_NEEDS_REINSTALL
  MZ_SIGSET(SIGINT, user_break_hit);
# endif
}
#endif

#ifdef MACINTOSH_EVENTS
static int check_break_flag()
{
  QHdrPtr start;
  EvQEl *q;
  EventRecord event;
  
  start = GetEvQHdr();
  q = (EvQEl *)start->qHead;
  while (q) {
    if (q->evtQWhat == keyDown) {
      if ((((q->evtQMessage & charCodeMask) == '.') 
	   && (q->evtQModifiers & cmdKey))
      	  || (((q->evtQMessage & charCodeMask) == 3) 
	      && (q->evtQModifiers & controlKey))) {
        Dequeue((QElemPtr)q, (QHdrPtr)start);
        return 1;
      }
    }
    q = (EvQEl *)q->qLink;
  }
#ifdef MACINTOSH_GIVE_TIME
  WaitNextEvent(0, &event, 0, NULL);
#endif
  return 0;
}

static Scheme_Object *adjust_break_flag_and_eval(int argc, Scheme_Object **argv)
{
  break_flag = 0;
  return _scheme_tail_apply(orig_evaluator, argc, argv);
}
#endif

#endif

#define GDESC "Identifiers and symbols are initially case-sensitive.\n"
#define KDESC "Built-in globals are constant.\n"
#define UDESC "Primitive exceptions are secure.\n"
#define SDESC "Set! works on undefined identifiers.\n"
#define EDESC "Call/cc is replaced with call/ec.\n"
#define ADESC "Fall-through cond or case is an error.\n"
#define NDESC "Keywords are not enforced.\n"
#define YDESC "Only #%% syntactic forms are present.\n"

#ifdef MZ_STACK_START_HACK
void *mzscheme_stack_start;
#endif

#ifdef MACINTOSH_EVENTS
void Drop_Runtime(char **argv, int argc)
{
  int i;
	
  for (i = 0; i < argc; i++)
    scheme_load(argv[i]);
}

void Drop_Quit()
{
  exit(0);
}
#endif

static int is_number_arg(const char *s)
{
  while (*s) {
    if (*s < '0' || *s > '9') {
      if (*s == '.') {
	s++;
	while (*s) {
	  if (*s < '0' || *s > '9')
	    return 0;
	  else
	    s++;
	}
	return 1;
      } else
	return 0;
    } else
      s++;
  }

  return 1;
}

static char *make_load_cd(char *file)
{
  char *s;
  
  s = (char *)malloc(strlen(file) + 13);
  strcpy(s, "(load/cd \"");
  strcat(s, file);
  strcat(s, "\")");
  return s;
}

static char *make_require_lib(char *file)
{
  char *s;
  
  s = (char *)malloc(strlen(file) + 21);
  strcpy(s, "(require-library \"");
  strcat(s, file);
  strcat(s, "\")");
  return s;
}

#ifdef SPECIALIZE_STANDALONE
extern Scheme_Object *scheme_initialize(Scheme_Env *env);
#endif

#ifdef NO_GCING
extern int GC_free_space_divisor;
#endif

int actual_main(int argc, char *argv[])
{
  Scheme_Env *global_env;
  char *prog, **evals_and_loads, *real_switch = NULL;
  int *is_load, num_enl;
  int i;
  int no_more_switches = 0;
  int script_mode = 0;
  int no_rep = 0;
  int no_init_file = 0;
  int no_lib_path = 0;
  int mute_banner = 0;
  Scheme_Object *sch_argv;
  char *filename;
  int exit_val = 0;

#ifdef NO_GCING
  GC_free_space_divisor = 1;
#endif

#ifdef MACINTOSH_SET_STACK
  long calcLimit;
  THz zone;
	
  zone = GetZone();
  calcLimit = ((long)LMGetCurStackBase()-(*(long *)zone)-sizeof(Zone))*3/4;
  if (calcLimit % 2)
    calcLimit++;
  SetApplLimit((Ptr)((*(long *)zone)+sizeof(Zone)+calcLimit));
#endif

#ifdef MACINTOSH_EVENTS
  MaxApplZone();
	
  InitGraf(&qd.thePort);		/* initialize Mac stuff */
  InitFonts();
  InitWindows();
  InitMenus();
  TEInit();
  InitDialogs(NULL);
  
  MoreMasters();
  MoreMasters();
	
#ifdef MACINTOSH_SIOUX
  SIOUXSettings.initializeTB = 0;
#endif

  Drop_GetArgs(&argc, &argv);
#endif

#ifdef MACINTOSH_SIOUX
  if (argc == 1) {
    char *pname = argv[0];
    argc = ccommand(&argv);
    argv[0] = pname;
  }
  
  /* When not using far globals under 68k, CW may not be able to link this.
     Just comment it out. */
  SIOUXSettings.autocloseonquit = 0;
  SIOUXSettings.asktosaveonclose = 0;
#endif

#ifdef USE_LOCALE
  setlocale( LC_ALL, "" );
#endif
  
  if (argc) {
    prog = argv[0];
    argv++;
    --argc;
  } else
    prog = "MzScheme";

  if (argc && (!strcmp(argv[0], "--restore")
	       || ((argv[0][0] == '-') && (argv[0][1] == 'R')))) {
    printf("Image loading (with --restore or -R<file>) is not supported.\n");
    exit(-1);
  }


  evals_and_loads = (char **)malloc(sizeof(char *) * argc);
  is_load = (int *)malloc(sizeof(int) * argc);
  num_enl = 0;

  while (!no_more_switches && argc && argv[0][0] == '-' && !is_number_arg(argv[0] + 1)) {
    real_switch = argv[0];

    if (!strcmp("--help", argv[0]))
      argv[0] = "-h";
    else if (!strcmp("--case-sens", argv[0]))
      argv[0] = "-g";
    else if (!strcmp("--const-globs", argv[0]))
      argv[0] = "-k";
    else if (!strcmp("--secure-exns", argv[0]))
      argv[0] = "-u";
    else if (!strcmp("--esc-cont", argv[0]))
      argv[0] = "-c";
    else if (!strcmp("--set-undef", argv[0]))
      argv[0] = "-s";
    else if (!strcmp("--no-auto-else", argv[0]))
      argv[0] = "-a";
    else if (!strcmp("--no-key", argv[0]))
      argv[0] = "-n";
    else if (!strcmp("--hash-percent-syntax", argv[0]))
      argv[0] = "-y";
    else if (!strcmp("--script", argv[0]))
      argv[0] = "-r";
    else if (!strcmp("--script-cd", argv[0]))
      argv[0] = "-i";
    else if (!strcmp("--no-lib-path", argv[0]))
      argv[0] = "-x";
    else if (!strcmp("--version", argv[0]))
      argv[0] = "-v";
    else if (!strcmp("--no-init-file", argv[0]))
      argv[0] = "-q";
    else if (!strcmp("--mute-banner", argv[0]))
      argv[0] = "-m";
    else if (!strcmp("--awk", argv[0]))
      argv[0] = "-w";
#if defined(_IBMR2)
    else if (!strcmp("--persistent", argv[0]))
      argv[0] = "-p";
#endif
    else if (!strcmp("--restore", argv[0])) {
      printf("--restore or -R<file> must be the first (and only) switch.\n");
      goto show_need_help;
    }
    
    if (!argv[0][1] || (argv[0][1] == '-' && argv[0][2])) {
      goto bad_switch;
    } else {
      char *str;
      for (str = argv[0] + 1; *str; str++) {
	switch (*str) {
	case 'h':
	  goto show_help;
	  break;
	case 'g':
	  scheme_case_sensitive = 1;
	  break;
	case 'c':
	  scheme_escape_continuations_only = 1;
	  break;
	case 'k':
	  scheme_constant_builtins = 1;
	  break;
	case 'u':
	  scheme_secure_primitive_exn = 1;
	  break;
	case 's':
	  scheme_allow_set_undefined = 1;
	  break;
	case 'a':
	  scheme_allow_cond_auto_else = 0;
	  break;
	case 'n':
	  scheme_no_keywords = 1;
	  break;
	case 'y':
	  scheme_hash_percent_syntax_only = 1;
	  break;
	case 'e':
	  if (argc < 2) {
	    printf("%s: Missing expression after %s switch.\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  evals_and_loads[num_enl] = argv[0];
	  is_load[num_enl++] = 0;
	  break;
	case 'x':
	  no_lib_path = 1;
	  break;
	case 'r':
	  script_mode = 1;
	  no_more_switches = 1;
	case 'f':
	  if (argc < 2) {
	    printf("%s: Missing file name after %s switch.\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  evals_and_loads[num_enl] = argv[0];
	  is_load[num_enl++] = 1;
	  break;
	case 'i':
	  script_mode = 1;
	  no_more_switches = 1;
	case 'd':
	  if (argc < 2) {
	    printf("%s: Missing file name after %s switch.\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  evals_and_loads[num_enl] = make_load_cd(argv[0]);
	  is_load[num_enl++] = 0;
	  break;
	case 'F':
	  while (argc > 1) {
	    argv++;
	    --argc;
	    evals_and_loads[num_enl] = argv[0];
	    is_load[num_enl++] = 1;
	  }
	  break;
	case 'D':
	  while (argc > 1) {
	    argv++;
	    --argc;
	    evals_and_loads[num_enl] = make_load_cd(argv[0]);
	    is_load[num_enl++] = 0;
	  }
	  break;
	case 'l':
	  if (argc < 2) {
	    printf("%s: Missing file after %s switch.\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  evals_and_loads[num_enl] = make_require_lib(argv[0]);
	  is_load[num_enl++] = 0;
	  break;
	case 'w':
	  evals_and_loads[num_enl] = make_require_lib("awk.ss");
	  is_load[num_enl++] = 0;
	  break;
	case 'q':
	  no_init_file = 1;
	  break;
	case 'v':
	  no_rep = 1;
	  break;
	case 'm':
	  mute_banner = 1;
	  break;
	case '-':
	  no_more_switches = 1;
	  break;
#if defined(_IBMR2)
	case 'p':
	  sigset(SIGDANGER, dangerdanger);
	  break;
#endif
	case 'R':
	  printf("--restore or -R<file> must be the first (and only) switch.\n");
	  goto show_need_help;
	  break;
	default:
	  goto bad_switch;
	}
      }
    }
    argv++;
    --argc;
  }
  
  if (!script_mode && !mute_banner) {
    printf(scheme_banner());
    if (scheme_case_sensitive)
      printf(GDESC);
    if (scheme_escape_continuations_only)
      printf(EDESC);
    if (scheme_constant_builtins)
      printf(KDESC);
    if (scheme_secure_primitive_exn)
      printf(UDESC);
    if (scheme_allow_set_undefined)
      printf(SDESC);
    if (!scheme_allow_cond_auto_else)
      printf(ADESC);
    if (scheme_no_keywords)
      printf(NDESC);
    if (scheme_hash_percent_syntax_only)
      printf(YDESC);
#ifdef MACINTOSH_EVENTS
    printf("Warning: reading stdin may block threads.\n");
#endif
#ifdef DOS_FILE_SYSTEM
#if !defined(DETECT_WIN32_CONSOLE_STDIN)
    printf("Warning: reading stdin may block threads.\n");
#endif
#endif
  }
  
  global_env = scheme_basic_env();

  sch_argv = scheme_make_vector(argc, scheme_null);
  for (i = 0; i < argc; i++)
    SCHEME_VEC_ELS(sch_argv)[i] = scheme_make_string(argv[i]);

  scheme_add_global("argv", sch_argv, global_env);
  scheme_add_global("program", scheme_make_string(prog),
		    global_env);

#ifndef NO_FILE_SYSTEM_UTILS
  /* Setup path for "collects" collection directory: */
  if (!no_lib_path) {
    scheme_eval_string("(#%current-library-collection-paths "
		        "(#%path-list-string->path-list "
		         "(#%or (#%getenv \"PLTCOLLECTS\") \"\")"
		         "(#%or"
		          "(#%ormap"
		           "(#%lambda (f) (#%let ([p (f)]) (#%and p (#%directory-exists? p) (#%list p))))"
		           "(#%list"
		            "(#%lambda () (#%let ((v (#%getenv \"PLTHOME\")))"
		                          "(#%and v (#%build-path v \"collects\"))))"
		            "(#%lambda () (#%find-executable-path program \"collects\"))"
#ifdef UNIX_FILE_SYSTEM
		            "(#%lambda () \"/usr/local/lib/plt/collects\")"
#endif
#ifdef DOS_FILE_SYSTEM
		            "(#%lambda () \"c:\\plt\\collects\")"
#endif
		          ")) #%null)))",
		       global_env);
  }
#endif

  if (!no_init_file) {
#ifdef EXPAND_FILENAME_TILDE
    filename = "~/.mzschemerc";
#else
    filename = "mzscheme.rc";
#endif
    filename = scheme_expand_filename(filename, -1, "startup", NULL);
    if (scheme_file_exists(filename))
      scheme_load(filename);
  }

  for (i = 0; i < num_enl; i++) {
    if (is_load[i]) {
      if (!scheme_load(evals_and_loads[i])) {
	exit_val = -1;
	break;
      }
    } else if (!scheme_setjmp(scheme_error_buf))
      scheme_eval_string_all(evals_and_loads[i], global_env, 0);
    else {
      exit_val = -1;
      break;
    }
  }

#ifdef SPECIALIZE_STANDALONE
  scheme_initialize(global_env);
#endif

#ifndef MACINTOSH_EVENTS
  MZ_SIGSET(SIGINT, user_break_hit);
#else
  scheme_check_for_break = check_break_flag;
#endif

  if (!no_rep && !script_mode) {
#ifndef NO_USER_BREAK_HANDLER
#ifdef MACINTOSH_EVENTS
    scheme_set_param(scheme_config, MZCONFIG_ENABLE_BREAK, scheme_true);
    
    orig_evaluator = scheme_get_param(scheme_config, MZCONFIG_EVAL_HANDLER);
    scheme_set_param(scheme_config, MZCONFIG_EVAL_HANDLER, scheme_make_prim(adjust_break_flag_and_eval));
#endif
#endif

    /* enter read-eval-print loop */
    scheme_rep();
    printf("\n");
    exit_val = 0;
  }

  return exit_val;

 show_help:
  printf("%s Startup file and expression switches:\n"
	 "  -e <expr> : Evaluates <expr> after MzScheme starts.\n"
	 "  -f <file> : Loads <file> after MzScheme starts.\n"
	 "  -d <file> : Load/cds <file> after MzScheme starts.\n"
	 "  -F : Loads all remaining arguments after MzScheme starts.\n"
	 "  -D : Load/cds all remaining arguments after MzScheme starts.\n"
	 "  -l <file> : Same as -e '(require-library \"<file>\")'.\n"
	 "  -r, --script : Script mode: use as last switch for scripts. Same as -fmv-.\n" 
	 "  -i, --script-cd : Like -r, but also sets the directory. Same as -dmv-.\n"
	 "  -w, --awk : Same as -l awk.ss.\n"
	 " Initialization switches:\n"
         "  -x, --no-lib-path : Does not try to set current-library-collection-paths.\n"
	 "  -q, --no-init-file : Does not try to load "
#ifdef EXPAND_FILENAME_TILDE
	 "\"~/.mzschemerc\""
#else
	 "\"mzscheme.rc\""
#endif
	 ".\n"
	 " Language setting switches:\n"
	 "  -g, --case-sens : " GDESC
	 "  -c, --esc-cont : " EDESC
	 "  -k, --const-globs: " KDESC
	 "  -s, --set-undef : " SDESC
	 "  -a, --no-auto-else : " ADESC
	 "  -n, --no-key : " NDESC
	 "  -y, --hash-percent-syntax : " YDESC
	 " Miscellaneous switches:\n"
	 "  -- : No argument following this switch is used as a switch.\n"
#if defined(_IBMR2)
	 "  -p, --persistent : Catches SIGDANGER (low page space) signal.\n"
#endif
	 "  -m, --mute-banner : Suppresses the startup banner text.\n"
	 "  -v, --version : Suppresses the read-eval-print loop.\n"
	 "  -h, --help : Shows this information and exits; ignores other switches.\n"
	 "  -R<file>, --restore <file> : restores an image; must be the only switch.\n"
	 "Multiple single-letter switches can be collapsed, with arguments placed\n"
	 " after the collapsed switches; the first collapsed switch cannot be --.\n"
	 " E.g.: `-vfme file expr' is the same as `-v -f file -m -e expr'.\n"
	 "Extra arguments following the last switch are put into the Scheme global\n"
	 " variable `argv' as a vector of strings. The name used to start MzScheme\n"
	 " is put into the global variable `program' as a string.\n"
	 "Extra arguments after a `--restore' file are returned as a vector of\n"
	 " strings to the continuation of the `write-image-to-file' call that created\n"
	 " the image. Images are not supported on all platforms.\n"
         "Expressions/files are evaluated/loaded in order as provided.\n"
	 "The current-library-collection-paths is automatically set before any\n"
	 "  expressions/files are evaluated/loaded, unless the -x or --no-lib-path\n"
	 "  switch is used.\n"
	 "The file "
#ifdef EXPAND_FILENAME_TILDE
	 "\"~/.mzschemerc\""
#else
	 "\"mzscheme.rc\""
#endif
	 " is loaded before any provided expressions/files\n"
	 " are evaluated/loaded, unless the -q or --no-init-file switch is used.\n"
#ifdef UNIX_FILE_SYSTEM
	 " (Under Windows and MacOS, \"mzscheme.rc\" is loaded from the start-up\n"
         " working directory.)\n"
#else
         " The file \"mzscheme.rc\" is read from the start-up working directory.\n"
#endif
#ifdef WINDOWS_FILE_SYSTEM
	 " (MacOS is like Windows. Under Unix, \"~/.mzschemerc\" is loaded.)\n"
#endif
#ifdef WINDOWS_FILE_SYSTEM
	 " (Windows is like MacOS. Under Unix, \"~/.mzschemerc\" is loaded.)\n"
#endif
#ifdef MACINTOSH_EVENTS
	 "\n"
	 "Macintosh Startup files are alphabetized and put after the -F switch\n"
	 " on the command line.\n"
	 "If a single startup file is provided and it begins with #!, it\n"
	 " is handled specially. Starting with the next whitespace, the rest\n"
	 " of the line is used as command line arguments. Unless #!! is used,\n"
	 " the startup file name is added to the end of this command line.\n"
#endif
	 "For general information about MzScheme, see:\n"
	 "  http://www.cs.rice.edu/CS/PLT/packages/mzscheme/\n"
#if 0
	 "Submit bug reports via the web interface (encouraged):\n"
	 "  http://www.cs.rice.edu/CS/PLT/Bugs/\n"
	 "or via e-mail (discouraged):\n"
	 "  plt-bugs@cs.rice.edu\n"
#endif
         , scheme_banner());
  return exit_val;
 bad_switch:
  printf("%s: Bad switch %s.\n", prog, real_switch);
 show_need_help:
  printf("Use the --help or -h flag for help.\n");
  return -1;
}

int main(int argc, char **argv)
{
#ifdef USE_SENORA_GC
  void *mzscheme_stack_start;
#endif
#if defined(MZ_STACK_START_HACK) || defined(USE_SENORA_GC)
  long start2;

  mzscheme_stack_start = (void *)&start2;
#endif

#ifdef USE_SENORA_GC
  GC_set_stack_base(mzscheme_stack_start);
#endif

#if defined(_IBMR2) && !defined(USE_SENORA_GC)
  {
    int dummy;
    if ((unsigned long)&dummy > (unsigned long)0x2ff23000)
      scheme_stackbottom = 0x2ff80000;
    else
      scheme_stackbottom = 0x2ff23000;
  }
#endif

  scheme_actual_main = actual_main;

  return scheme_image_main(argc, argv);
}
