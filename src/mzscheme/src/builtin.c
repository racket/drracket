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

static void
add_primitive_symbol(Scheme_Object *sym, Scheme_Object *obj, 
		     Scheme_Env *env)
{
  scheme_do_add_global_symbol(env, sym, obj, 1, 1);
}

static void primitive_syntax_through_scheme(const char *name, 
					    Scheme_Env *env)
{
  Scheme_Object *hp, *sym;

  hp = scheme_hash_percent_name(name, -1);
  scheme_set_keyword(hp, env);

  sym = scheme_intern_symbol(name);
  scheme_add_global_symbol(sym, scheme_lookup_global(hp, env), env);
}

static void primitive_function_through_scheme(const char *name, 
					      Scheme_Env *env)
{
  Scheme_Object *sym, *hp;

  sym = scheme_intern_symbol(name);

  hp = scheme_hash_percent_name(name, -1);
  
  add_primitive_symbol(hp, scheme_lookup_global(sym, env), env);

  scheme_set_keyword(hp, env);
}

static void primitive_cond_through_scheme(const char *name, 
					  Scheme_Env *env)
{
  primitive_syntax_through_scheme(name, env);
  scheme_init_empty_cond(env);
}

void scheme_add_embedded_builtins(Scheme_Env *env)
{
#define EVAL_ONE_STR(str) scheme_eval_string(str, env)
#define EVAL_ONE_SIZED_STR(str, len) scheme_eval_compiled_sized_string(str, len, env)
#define JUST_DEFINED(name) primitive_syntax_through_scheme(#name, env)
#define JUST_DEFINED_FUNC(name) primitive_function_through_scheme(#name, env)
#define JUST_DEFINED_KEY(name) primitive_syntax_through_scheme(#name, env)
#define JUST_DEFINED_COND(name) primitive_cond_through_scheme(#name, env)
#define JUST_DEFINED_QQ(name) JUST_DEFINED_KEY(name)

#if USE_COMPILED_MACROS
# include "cmacro.inc"
#else
# include "macro.inc"
#endif
}
