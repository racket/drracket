/*
  MzScheme
  Copyright (c) 1995 Matthew Flatt

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
*/

#include "schpriv.h"
#include "schmach.h"

#ifdef STACK_GROWS_UP
#define DEEPPOS(b) ((unsigned long)(b)->stack_from+(unsigned long)(b)->stack_size)
#else
#ifdef STACK_GROWS_DOWN
#define DEEPPOS(b) ((unsigned long)(b)->stack_from)
#else
#define DEEPPOS(b) ((unsigned long)(b)->stack_from+ \
                 (scheme_stack_grows_up ? (unsigned long)(b)->stack_size : 0))
#endif
#endif

#ifdef memcpy
#undef memcpy
#endif

#define memcpy(dd, ss, ll) \
{  stack_val *d, *s; long l; \
   l = ll / sizeof(stack_val); d = (stack_val *)dd; s = (stack_val *)ss; \
   while (l--) *(d++) = *(s++); }

static void copy_stack(Scheme_Jumpup_Buf *b, void *start)
{
  long size;
  void *here;

  here = &size;

  size = (long)here - (long)start;
  if (scheme_stack_grows_up) {
    b->stack_from = start;
  } else {
    size = -size;
    b->stack_from = here;
  }

  if (size < 0)
    size = 0;

  if (b->stack_max_size < size) {
    b->stack_copy = scheme_malloc(size);
    b->stack_max_size = size;    
  }
  b->stack_size = size;
  
  memcpy(b->stack_copy,
	 b->stack_from,
	 size);
}

static void uncopy_stack(int ok, Scheme_Jumpup_Buf *b)
{
  Scheme_Jumpup_Buf *c;

  if (!ok) {
    unsigned long z;
    long junk[200];

    z = (unsigned long)&junk[0];

    uncopy_stack(STK_COMP(z, DEEPPOS(b)), b);
  }

  FLUSH_REGISTER_WINDOWS;

  c = b;
  while (c) {
    memcpy(c->stack_from,
	   c->stack_copy,
	   c->stack_size);
    c = c->cont;
  }

#ifdef WIN32_SETJMP_HACK
  /* Mystical hack for Win32 with Borland C++ 4.5 */
  /* My best guess is that the j_excep field is used by the
     Win32 kernal to gurantee that longjmp isn't used nastily
     by jumping across the handling of different events. Or something.
     Of course, I'm using longjmp nastily, so I have to trick the kernal. */
  /* That's just a guess. In any case, it seems to work. */
  {
    jmp_buf hack;
    setjmp(hack);
    b->buf->j_excep = hack->j_excep;
  }
#endif

  scheme_longjmp(b->buf, 1);
}

int scheme_setjmpup_relative(Scheme_Jumpup_Buf *b, void *start, 
			     Scheme_Jumpup_Buf *c)
{
  int local;

  FLUSH_REGISTER_WINDOWS;

  if (STK_COMP((unsigned long)start, (unsigned long)&local))
    start = (void *)&local;

  if (!(local = scheme_setjmp(b->buf))) {
    if (c) {
      b->cont = c;
      if (scheme_stack_grows_up) {
	start = (void *)((char *)c->stack_from + c->stack_size);
      } else {
	start = c->stack_from;
      }
    } else
      b->cont = NULL;
    copy_stack(b, start);
    return 0;
  }

  return local;
}

void scheme_longjmpup(Scheme_Jumpup_Buf *b)
{
  long z;

  uncopy_stack(STK_COMP((unsigned long)&z, DEEPPOS(b)), b);
}

void scheme_init_jmpup_buf(Scheme_Jumpup_Buf *b)
{
  b->stack_size = b->stack_max_size = 0;
  b->stack_from = b->stack_copy = NULL;
}

void scheme_ensure_stack_start(Scheme_Process *p, void *d)
{
  if (!p->stack_start 
      || (STK_COMP((unsigned long)p->stack_start, (unsigned long)d)))
    p->stack_start = d;
}
