#ifndef SCHEME_CURRENT_PROCESS
#define SCHEME_CURRENT_PROCESS scheme_current_process
#endif
#ifndef SCHEME_STACK_BOUNDARY
#define SCHEME_STACK_BOUNDARY scheme_stack_boundary
#endif

#ifdef SPAWN_NEW_STACK
  unsigned long _stk_pos;

  _stk_pos = (unsigned long)&_stk_pos;
  if (STK_COMP(_stk_pos, (unsigned long)SCHEME_CURRENT_PROCESS->stack_end))
#else
#ifdef USE_STACKAVAIL
  if (stackavail() < STACK_SAFETY_MARGIN)
#endif
#if defined(UNIX_FIND_STACK_BOUNDS) || defined(WINDOWS_FIND_STACK_BOUNDS) || defined(ASSUME_FIXED_STACK_SIZE)
  unsigned long _stk_pos;

  _stk_pos = (unsigned long)&_stk_pos;

  if (STK_COMP(_stk_pos, SCHEME_STACK_BOUNDARY))
#endif
#ifdef MACOS_STACK_LIMIT
  if (StackSpace() < STACK_SAFETY_MARGIN)
#endif
#endif

#undef SCHEME_CURRENT_PROCESS
#undef SCHEME_STACK_BOUNDARY
