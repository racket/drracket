/*
  Extension that uses the curses library.

  Link the extension to the curses library like this:
     mzc --ld hello.so hello.o -lcurses

  For obvious reasons, this library doesn't interact well
  with MzScheme's read-eval-print loop. The example file
  curses-demo.ss demos this extension.
*/

#include "escheme.h"
#include <curses.h>

/**************************************************/

static Scheme_Object *sch_clear(int argc, Scheme_Object **argv)
{
  clear();
}

static Scheme_Object *sch_put(int argc, Scheme_Object **argv)
{
  /* Puts a char or string on the screen */
  if (SCHEME_CHARP(argv[0]))
    addch(SCHEME_CHAR_VAL(argv[0]));
  else if (SCHEME_STRINGP(argv[0]))
    addstr(SCHEME_STR_VAL(argv[0]));
  else
    scheme_wrong_type("put", "character or string", 0, argc, argv);

  return scheme_void;
}

static Scheme_Object *sch_get(int argc, Scheme_Object **argv)
{
  /* Gets keyboard input */
  int c = getch();
  return scheme_make_character(c);
}

static Scheme_Object *sch_move(int argc, Scheme_Object **argv)
{
  /* Move the output cursor */
  if (!SCHEME_INTP(argv[0]))
    scheme_wrong_type("move", "exact integer", 0, argc, argv);
  if (!SCHEME_INTP(argv[1]))
    scheme_wrong_type("move", "exact integer", 1, argc, argv);

  move(SCHEME_INT_VAL(argv[0]), SCHEME_INT_VAL(argv[0]));

  return scheme_void;
}

static Scheme_Object *sch_get_size(int argc, Scheme_Object **argv)
{
  /* Returns two values */
  int w, h;
  Scheme_Object *a[2];

  w = getmaxx(stdscr);
  h = getmaxy(stdscr);

  a[0] = scheme_make_integer(w);
  a[1] = scheme_make_integer(h);
  return scheme_values(1, a);
}

static Scheme_Object *sch_refresh(int argc, Scheme_Object **argv)
{
  refresh();
  return scheme_void;
}

/**************************************************/

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  scheme_add_global("clear",
		    scheme_make_prim_w_arity(sch_clear, 
					     "clear", 
					     0, 0),
		    env);
  scheme_add_global("put",
		    scheme_make_prim_w_arity(sch_put, 
					     "put", 
					     1, 1),
		    env);
  scheme_add_global("get",
		    scheme_make_prim_w_arity(sch_get, 
					     "get", 
					     0, 0),
		    env);
  scheme_add_global("move",
		    scheme_make_prim_w_arity(sch_move, 
					     "move", 
					     2, 2),
		    env);
  scheme_add_global("get-size",
		    scheme_make_prim_w_arity(sch_get_size, 
					     "get-size", 
					     0, 0),
		    env);

  scheme_add_global("refresh",
		    scheme_make_prim_w_arity(sch_refresh,
					     "refresh", 
					     0, 0),
		    env);

  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  /* The first time we're loaded, initialize the screen: */
  initscr();
  cbreak();
  noecho();
  atexit(endwin);

  /* Then do the usual stuff: */
  return scheme_reload(env);
}
