/*
  MzScheme
  Copyright (c) 2000 Matthew Flatt

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

#include "schpriv.h"
#include "schminc.h"

Scheme_Object *scheme_init_unitsig(void)
{
  Scheme_Env *env;
  Scheme_Object *unitsig_macros = NULL;

  env = scheme_get_env(scheme_config);

#define EVAL_ONE_STR(str) unitsig_macros = scheme_eval_string(str, env)
#define EVAL_ONE_SIZED_STR(str, len) unitsig_macros = scheme_eval_compiled_sized_string(str, len, env)
#define JUST_DEFINED(x) /**/

#if USE_COMPILED_MACROS
#include "cunitsig.inc"
#else
#include "unitsig.inc"
#endif

  return unitsig_macros;
}
