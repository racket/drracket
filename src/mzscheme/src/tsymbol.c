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

#define HASH_TABLE_SIZE 10
static Scheme_Hash_Table *type_symbol_table = NULL;

/* globals */

/* locals */
static Scheme_Object *tsymbol_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_tsymbol(int argc, Scheme_Object *argv[]);

void
scheme_init_type_symbol_table ()
{
  if (scheme_starting_up) {
    REGISTER_SO(type_symbol_table);
    type_symbol_table = scheme_hash_table(HASH_TABLE_SIZE, SCHEME_hash_ptr, 
					  0, 1);
  }
}

void
scheme_init_type_symbol (Scheme_Env *env)
{
  if (scheme_starting_up) {
  }

  scheme_add_global_constant("type-symbol?", 
			     scheme_make_folding_prim(tsymbol_p, 
						      "type-symbol?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("string->type-symbol", 
			     scheme_make_folding_prim(string_to_tsymbol, 
						      "string->type-symbol", 
						      1, 1, 1), 
			     env);
}

Scheme_Object *
scheme_make_type_symbol(Scheme_Object *symbol)
{
  Scheme_Object *sym;

  sym = scheme_alloc_small_object();

  sym->type = scheme_type_symbol_type;

  SCHEME_PTR_VAL(sym) = symbol;
    
  return (sym);
}

Scheme_Object *
scheme_intern_type_symbol(Scheme_Object *symbol)
{
  Scheme_Object *sym;

  sym = (Scheme_Object *)scheme_lookup_in_table(type_symbol_table, 
						(char *)symbol);

  if (!sym) {
    sym = scheme_make_type_symbol(symbol);
    scheme_add_to_table (type_symbol_table, (char *)symbol, sym, 0);
  }

  return (sym);
}

static Scheme_Object *
tsymbol_p(int argc, Scheme_Object *argv[])
{
  return (SCHEME_TSYMBOLP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
string_to_tsymbol(int argc, Scheme_Object *argv[])
{
  Scheme_Object *s, *ts;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string->type-symbol", "string", 0, argc, argv);

  s = scheme_intern_exact_symbol(SCHEME_STR_VAL(argv[0]), SCHEME_STRTAG_VAL(argv[0]));

  ts = scheme_intern_type_symbol(s);

  return ts;
}
