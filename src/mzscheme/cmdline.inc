
/***************************************************************/
/* This command-line parser is used by both MzScheme and MrEd. */
/***************************************************************/

#define GDESC "Identifiers and symbols are initially case-sensitive.\n"
#define SDESC "Set! works on undefined identifiers.\n"
#define EDESC "Call/cc is replaced with call/ec.\n"
#define ADESC "Fall-through cond or case is an error.\n"
#define NDESC "Keywords are not enforced.\n"
#define YDESC "Only #%% syntactic forms are present.\n"

char *cmdline_exe_hack = "[Replace me for EXE hack                                       ]";

#ifndef DONT_PARSE_COMMAND_LINE
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

static char *make_require_lib(const char *file, const char *coll)
{
  char *s;
  
  s = (char *)malloc(strlen(file) + strlen(coll) + 31);
  strcpy(s, "(require-library \"");
  strcat(s, file);
  strcat(s, "\" \"");
  strcat(s, coll);
  strcat(s, "\")");
  return s;
}

static char *make_embedded_load(const char *start, const char *end)
{
  char *s;

  s = (char *)malloc(strlen(start) + strlen(end) + 512);
  
  sprintf(s,
	  "(let* ([sp (find-system-path 'exec-file)] [exe (find-executable-path sp #f)]"
                 "[s (with-input-from-file exe (lambda () "
	              "(file-position (current-input-port) %s) (read-string (- %s %s))))]"
                 "[p (open-input-string s)])"
	     "(let loop () (let ([e (parameterize ([read-accept-compiled #t]) (read p))])"
 			    "(unless (eof-object? e) (eval e) (loop)))))",
	  start, end, start);

  return s;
}
#endif

typedef struct {
#ifndef DONT_PARSE_COMMAND_LINE
  char **evals_and_loads;
  int *is_load, num_enl;
#endif  
#ifndef DONT_LOAD_INIT_FILE
  int no_init_file;
#endif
#ifndef DONT_RUN_REP
  int no_rep;
  int script_mode;
#endif
#ifdef VERSION_YIELD_FLAG
  int add_yield;
#endif
  Scheme_Env *global_env;
} FinishArgs;

static int finish_cmd_line_run(FinishArgs *fa, void (*repl)(void))
{
  volatile int exit_val = 0;

#ifndef DONT_LOAD_INIT_FILE
  if (!fa->no_init_file) {
    char *filename;
    filename = scheme_expand_filename(GET_INIT_FILENAME(fa->global_env), 
				      -1, "startup", NULL);
    if (scheme_file_exists(filename))
      scheme_load(filename);
  }
#endif /* DONT_LOAD_INIT_FILE */

#ifndef DONT_PARSE_COMMAND_LINE
  {
    volatile int i;
    for (i = 0; i < fa->num_enl; i++) {
      if (fa->is_load[i]) {
	if (!scheme_load(fa->evals_and_loads[i])) {
	  exit_val = 1;
	  break;
	}
      } else if (!scheme_setjmp(scheme_error_buf))
	scheme_eval_string_all(fa->evals_and_loads[i], fa->global_env, 0);
      else {
	exit_val = 1;
	break;
      }
    }
  }
#endif /* DONT_PARSE_COMMAND_LINE */

#ifdef STANDALONE_WITH_EMBEDDED_EXTENSION
  {
    Scheme_Object *f, *a[1];
    if (!scheme_setjmp(scheme_error_buf)) {
      f = scheme_initialize(fa->global_env);
      a[0] = scheme_true;
      f = _scheme_apply(f, 1, a);
      _scheme_apply(f, 0, NULL);
    } else {
      exit_val = 1;
    }
  }
#endif

#ifndef DONT_RUN_REP
  if (!fa->no_rep && !fa->script_mode) {
    /* enter read-eval-print loop */
    repl();
    exit_val = 0;
  }
#endif /* DONT_RUN_REP */

#ifdef VERSION_YIELD_FLAG
  if (fa->add_yield) {
    yield_indefinitely();
  }
#endif

  return exit_val;
}

#ifdef USE_OSKIT_CONSOLE
/* Hack to disable normal input mode: */
int osk_not_console = 0;
#endif

#if defined(_IBMR2)
static void dangerdanger(int);
#endif

static int run_from_cmd_line(int argc, char *argv[], 
			     Scheme_Env *(*mk_basic_env)(void),
			     int (*cont_run)(FinishArgs *f))
{
  Scheme_Env *global_env;
  char *prog;
  Scheme_Object *sch_argv;
  int i;
#ifndef DONT_PARSE_COMMAND_LINE
  char **evals_and_loads, *real_switch = NULL;
  int *is_load, num_enl;
  int no_more_switches = 0;
  int mute_banner = 0;
#endif
#if !defined(DONT_RUN_REP) || !defined(DONT_PARSE_COMMAND_LINE)
  int no_rep = 0;
  int script_mode = 0;
#endif
#if !defined(DONT_LOAD_INIT_FILE) || !defined(DONT_PARSE_COMMAND_LINE)
  int no_init_file = 0;
#endif
#ifdef VERSION_YIELD_FLAG
  int add_yield = 0;
#endif
  int no_lib_path = 0;
  FinishArgs *fa;

#ifndef USE_SENORA_GC
# ifdef GC_MIGHT_USE_REGISTERED_STATICS
  GC_use_registered_statics = 1;
# endif
#endif

  /* Avoid exiting problems under Windows: */
  scheme_setup_forced_exit();

  prog = argv[0];
  argv++;
  --argc;
  
#ifdef DOS_FILE_SYSTEM
  {
    /* Make sure the .exe extension is present for consistency */
    int l = strlen(prog);
    if (l <= 4 
	|| (prog[l - 4] != '.')
	|| (tolower(((unsigned char *)prog)[l - 3]) != 'e')
	|| (tolower(((unsigned char *)prog)[l - 2]) != 'x')
	|| (tolower(((unsigned char *)prog)[l - 1]) != 'e')) {
      char *s = (char *)scheme_malloc_atomic(l + 4 + 1);
      memcpy(s, prog, l);
      memcpy(s + l, ".exe", 5);
      prog = s;
    }
  }
#endif
  
  /* If cmdline_exe_hack is changed, then we extract built-in
     arguments. */
  if (cmdline_exe_hack[0] != '[') {
    int n, i, d;
    char *p, **argv2;

    p = cmdline_exe_hack + 1;
    n = 0;
    while (*p) {
      n++;
      p += *p + 1;
    }

    argv2 = (char **)malloc(sizeof(char *) * (argc + n));
    p = cmdline_exe_hack + 1;
    for (i = 0; i < n; i++) {
      d = *p;
      argv2[i] = p + 1;
      p += d + 1;
    }
    for (; i < n + argc; i++) {
      argv2[i] = argv[i - n];
    }
    argv = argv2;
    argc += n;
  }

#ifndef DONT_PARSE_COMMAND_LINE
  if (argc && (!strcmp(argv[0], "--restore")
	       || ((argv[0][0] == '-') && (argv[0][1] == 'R')))) {
    PRINTF("Image loading (with --restore or -R<file>) is not supported.\n");
    return 1;
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
    else if (!strcmp("--binary", argv[0]))
      argv[0] = "-b";
# ifdef CMDLINE_STDIO_FLAG
    else if (!strcmp("--stdio", argv[0]))
      argv[0] = "-z";
# endif
# ifdef VERSION_YIELD_FLAG
    else if (!strcmp("--yield", argv[0]))
      argv[0] = "-V";
# endif
#if defined(_IBMR2)
    else if (!strcmp("--persistent", argv[0]))
      argv[0] = "-p";
#endif
    else if (!strcmp("--restore", argv[0])) {
      PRINTF("--restore or -R<file> must be the first (and only) switch.\n");
      goto show_need_help;
    }
    
    if (!argv[0][1] || (argv[0][1] == '-' && argv[0][2])) {
      goto bad_switch;
    } else {
      char *str, *se;
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
	    PRINTF("%s: Missing expression after %s switch.\n", 
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
	    PRINTF("%s: Missing file name after %s switch.\n", 
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
	    PRINTF("%s: Missing file name after %s switch.\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  se = make_load_cd(argv[0]);
	  evals_and_loads[num_enl] = se;
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
	    se = make_load_cd(argv[0]);
	    evals_and_loads[num_enl] = se;
	    is_load[num_enl++] = 0;
	  }
	  break;
	case 'l':
	  if (argc < 2) {
	    PRINTF("%s: Missing file after %s switch.\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  se = make_require_lib(argv[0], "mzlib");
	  evals_and_loads[num_enl] = se;
	  is_load[num_enl++] = 0;
	  break;
	case 'L':
	  if (argc < 3) {
	    PRINTF("%s: Missing %s after %s switch.\n", 
		   prog, 
		   (argc < 2) ? "file and collection" : "collection",
		   real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  se = make_require_lib(argv[0], argv[1]);
	  evals_and_loads[num_enl] = se;
	  argv++;
	  --argc;
	  is_load[num_enl++] = 0;
	  break;
	case 'w':
	  se = make_require_lib("awk.ss", "mzlib");
	  evals_and_loads[num_enl] = se;
	  is_load[num_enl++] = 0;
	  break;
	case 'k':
	  if (argc < 3) {
	    PRINTF("%s: Missing %s after %s switch.\n", 
		   prog, 
		   (argc < 2) ? "starting and ending offsets" : "ending offset",
		   real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  se = make_embedded_load(argv[0], argv[1]);
	  evals_and_loads[num_enl] = se;
	  argv++;
	  --argc;
	  is_load[num_enl++] = 0;
	  break;
	case 'q':
	  no_init_file = 1;
	  break;
	case 'v':
	  no_rep = 1;
	  break;
#ifdef VERSION_YIELD_FLAG
	case 'V':
	  no_rep = 1;
	  add_yield = 1;
	  break;
#endif
	case 'm':
	  mute_banner = 1;
	  break;
	case '-':
	  no_more_switches = 1;
	  break;
	case 'b':
	  scheme_binary_mode_stdio = 1;
	  break;
#ifdef CMDLINE_STDIO_FLAG
	case 'z':
	  no_rep = 1;
	  evals_and_loads[num_enl] = "(read-eval-print-loop)";
	  is_load[num_enl++] = 0;
	  break;
#endif
#if defined(_IBMR2)
	case 'p':
	  signal(SIGDANGER, dangerdanger);
	  break;
#endif
#ifdef USE_OSKIT_CONSOLE
	case 'S':
	  osk_not_console = 1;
	  break;
#endif
	case 'R':
	  PRINTF("--restore or -R<file> must be the first (and only) switch.\n");
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
#ifndef MZSCHEME_CMD_LINE  
    if (no_rep)
#endif
      PRINTF(BANNER);
#ifdef MZSCHEME_CMD_LINE  
    if (scheme_case_sensitive)
      PRINTF(GDESC);
    if (scheme_escape_continuations_only)
      PRINTF(EDESC);
    if (scheme_allow_set_undefined)
      PRINTF(SDESC);
    if (!scheme_allow_cond_auto_else)
      PRINTF(ADESC);
    if (scheme_no_keywords)
      PRINTF(NDESC);
    if (scheme_hash_percent_syntax_only)
      PRINTF(YDESC);
# ifdef DOS_FILE_SYSTEM
#  if !defined(FILES_HAVE_FDS)
#   if !defined(DETECT_WIN32_CONSOLE_STDIN) || defined(NO_STDIN_THREADS)
    PRINTF("Warning: read-eval-print-loop or read on stdin or process port may block threads.\n");
#   endif
#  endif
# endif
#endif

#ifdef USE_FD_PORTS
   fflush(stdout);
#endif
  }
#endif /* DONT_PARSE_COMMAND_LINE */
  
  global_env = mk_basic_env();

  sch_argv = scheme_make_vector(argc, scheme_null);
  for (i = 0; i < argc; i++) {
    Scheme_Object *so;
    so = scheme_make_string(argv[i]);
    SCHEME_VEC_ELS(sch_argv)[i] = so;
  }

  scheme_add_global("argv", sch_argv, global_env);
  {	
    Scheme_Object *ps;
    ps = scheme_set_exec_cmd(prog);
    scheme_add_global("program", ps, global_env);
  }

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
# ifdef USE_BEOS_SNOOZE
		            "(#%lambda () \"/boot/apps/plt/collects\")"
# else
		            "(#%lambda () \"/usr/local/lib/plt/collects\")"
# endif
#endif
#ifdef DOS_FILE_SYSTEM
		            "(#%lambda () \"c:\\plt\\collects\")"
#endif
		          ")) #%null)))",
		       global_env);
  }
#endif /* NO_FILE_SYSTEM_UTILS */

#ifndef MZSCHEME_CMD_LINE  
  scheme_eval_string("(#%with-handlers ([void void])"
		     "  (#%send (current-ps-setup) set-afm-path (collection-path \"afm\")))",
		     global_env);
#endif

  fa = (FinishArgs *)scheme_malloc(sizeof(FinishArgs));
#ifndef DONT_PARSE_COMMAND_LINE
  fa->evals_and_loads = evals_and_loads;
  fa->is_load = is_load;
  fa->num_enl = num_enl;
#endif  
#ifndef DONT_LOAD_INIT_FILE
  fa->no_init_file = no_init_file;
#endif
#ifndef DONT_RUN_REP
  fa->no_rep = no_rep;
  fa->script_mode = script_mode;
#endif
#ifdef VERSION_YIELD_FLAG
  fa->add_yield = add_yield;
#endif
  fa->global_env = global_env;

  scheme_set_param(scheme_config, MZCONFIG_ENABLE_BREAK, scheme_true);

  return cont_run(fa);

#ifndef DONT_PARSE_COMMAND_LINE
 show_help:
  PRINTF("%s"
# ifndef MZSCHEME_CMD_LINE
#  ifdef wx_x
	 " X switches (must precede all other flags):\n"
	 "  -display <display>, -geometry <geometry>, -fn <font>, -font <font>,\n"
         "  -bg <color>, -background <color>, -fg <color>, -foreground <color>,\n"
	 "  -iconic, -name <name>, -rv, -reverse, +rv, -selectionTimeout <timeout>,\n"
	 "  -synchronous, -title <title>, -xnllanguage <language>, -xrm <file>\n"
#  endif
# endif
	 " Startup file and expression switches:\n"
	 "  -e <expr> : Evaluates <expr> after " PROGRAM " starts.\n"
	 "  -f <file> : Loads <file> after " PROGRAM " starts.\n"
	 "  -d <file> : Load/cds <file> after " PROGRAM " starts.\n"
	 "  -F : Loads all remaining arguments after " PROGRAM " starts.\n"
	 "  -D : Load/cds all remaining arguments after " PROGRAM " starts.\n"
	 "  -l <file> : Same as -e '(require-library \"<file>\")'.\n"
	 "  -L <file> <coll> : Same as -e '(require-library \"<file>\" \"<coll>\")'.\n"
	 "  -r, --script : Script mode: use as last switch for scripts. Same as -fmv-.\n" 
	 "  -i, --script-cd : Like -r, but also sets the directory. Same as -dmv-.\n"
# ifdef CMDLINE_STDIO_FLAG
	 "  -z, --stdio : Use stdio REPL. Same as -ve '(read-eval-print-loop)'.\n"
# endif
	 "  -w, --awk : Same as -l awk.ss.\n"
	 "  -k <n> <m> : Load executable-embedded code from file offset <n> to <m>.\n"
	 " Initialization switches:\n"
         "  -x, --no-lib-path : Does not try to set current-library-collection-paths.\n"
	 "  -q, --no-init-file : Does not try to load " INIT_FILENAME ".\n"
	 " Language setting switches:\n"
	 "  -g, --case-sens : " GDESC
	 "  -c, --esc-cont : " EDESC
	 "  -s, --set-undef : " SDESC
	 "  -a, --no-auto-else : " ADESC
	 "  -n, --no-key : " NDESC
	 "  -y, --hash-percent-syntax : " YDESC
	 " Miscellaneous switches:\n"
	 "  -- : No argument following this switch is used as a switch.\n"
# if defined(_IBMR2)
	 "  -p, --persistent : Catches SIGDANGER (low page space) signal.\n"
# endif
	 "  -m, --mute-banner : Suppresses "
# ifdef MZSCHEME_CMD_LINE  
	 "the startup banner"
# else
	 "-v/--version"
# endif
	 " text.\n"
	 "  -v, --version : Suppresses the read-eval-print loop"
# ifdef MZSCHEME_CMD_LINE  
	 ".\n"
# else
	 ", prints version.\n"
# endif
# ifdef VERSION_YIELD_FLAG
	 "  -V, --yield : Like -v, adds yield after evaling/loading all exprs/files.\n"
# endif
	 "  -b, --binary : Read stdin and write stdout/stderr in binary mode.\n"
	 "  -h, --help : Shows this information and exits, ignoring other switches.\n"
#ifdef UNIX_IMAGE_DUMPS
	 "  -R<file>, --restore <file> : restores an image.\n"
#endif
	 "Multiple single-letter switches can be collapsed, with arguments placed\n"
	 " after the collapsed switches; the first collapsed switch cannot be --.\n"
	 " E.g.: `-vfme file expr' is the same as `-v -f file -m -e expr'.\n"
	 "Extra arguments following the last switch are put into the Scheme global\n"
	 " variable `argv' as a vector of strings. The name used to start " PROGRAM "\n"
	 " is put into the global variable `program' as a string.\n"
#ifdef UNIX_IMAGE_DUMPS
	 "Extra arguments after a `--restore' file are returned as a vector of\n"
	 " strings to the continuation of the `write-image-to-file' call that created\n"
	 " the image. Images are not supported on all platforms.\n"
#endif
         "Expressions/files are evaluated/loaded in order as provided.\n"
	 "The current-library-collection-paths is automatically set before any\n"
	 "  expressions/files are evaluated/loaded, unless the -x or --no-lib-path\n"
	 "  switch is used.\n"
	 "The file " INIT_FILENAME
	 " is loaded before any provided expressions/files\n"
	 " are evaluated/loaded, unless the -q or --no-init-file switch is used.\n"
# ifdef MACINTOSH_EVENTS
	 "\n"
	 "Macintosh Startup files are alphabetized and put after the -F switch\n"
	 " on the command line.\n"
	 "If a single startup file is provided and it begins with #!, it\n"
	 " is handled specially. Starting with the next whitespace, the rest\n"
	 " of the line is used as command line arguments. Unless #!! is used,\n"
	 " the startup file name is added to the end of this command line.\n"
# endif
	 "For general information about " PROGRAM ", see:\n"
	 "  http://www.cs.rice.edu/CS/PLT/packages/" PROGRAM_LC "/\n"
         , BANNER);
  return 0;
 bad_switch:
  PRINTF("%s: Bad switch %s.\n", prog, real_switch);
 show_need_help:
  PRINTF("Use the --help or -h flag for help.\n");
  return 1;
#endif
}
