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

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#include "schpriv.h"

/* globals */
Scheme_Object scheme_null[1];

/* locals */
static Scheme_Object *pair_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cons_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *car_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *set_car_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *set_cdr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *pair_to_immutable (int argc, Scheme_Object *argv[]);
static Scheme_Object *null_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_star_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *length_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *append_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *append_bang_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *reverse_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *reverse_bang_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_tail_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_ref_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *memv (int argc, Scheme_Object *argv[]);
static Scheme_Object *memq (int argc, Scheme_Object *argv[]);
static Scheme_Object *member (int argc, Scheme_Object *argv[]);
static Scheme_Object *assv (int argc, Scheme_Object *argv[]);
static Scheme_Object *assq (int argc, Scheme_Object *argv[]);
static Scheme_Object *assoc (int argc, Scheme_Object *argv[]);
static Scheme_Object *caar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caaar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cadar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdaar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cddar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdddr_prim (int argc, Scheme_Object *argv[]);

static Scheme_Object *cddddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cadddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdaddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cddadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdddar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caaddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cadadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caddar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdaadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdadar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cddaar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdaaar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cadaar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caadar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caaadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caaaar_prim (int argc, Scheme_Object *argv[]);

static Scheme_Object *box (int argc, Scheme_Object *argv[]);
static Scheme_Object *box_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *unbox (int argc, Scheme_Object *argv[]);
static Scheme_Object *set_box (int argc, Scheme_Object *argv[]);

static Scheme_Object *make_hash_table(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_hash_table_weak(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_hash_table_weak(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_put(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_get(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_remove(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_map(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_for_each(int argc, Scheme_Object *argv[]);

static Scheme_Object *make_weak_box(int argc, Scheme_Object *argv[]);
static Scheme_Object *weak_box_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *weak_boxp(int argc, Scheme_Object *argv[]);

#define BOX "box"
#define BOXP "box?"
#define UNBOX "unbox"
#define SETBOX "set-box!"

void
scheme_init_list (Scheme_Env *env)
{
  if (scheme_starting_up) {
    scheme_null->type = scheme_null_type;
  }

  scheme_add_global_constant ("null", scheme_null, env);

  scheme_add_global_constant ("pair?", 
			      scheme_make_folding_prim(pair_p_prim, 
						       "pair?", 
						       1, 1, 1), 
			      env);
  scheme_add_global_constant ("cons", 
			      scheme_make_prim_w_arity(cons_prim, 
						       "cons", 
						       2, 2), 
			      env);
  scheme_add_global_constant ("car", 
			      scheme_make_prim_w_arity(car_prim, 
						       "car", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("cdr", 
			      scheme_make_prim_w_arity(cdr_prim,  
						       "cdr", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("set-car!", 
			      scheme_make_prim_w_arity(set_car_prim, 
						       "set-car!", 
						       2, 2), 
			      env);
  scheme_add_global_constant ("set-cdr!", 
			      scheme_make_prim_w_arity(set_cdr_prim, 
						       "set-cdr!", 
						       2, 2), 
			      env);
  scheme_add_global_constant ("pair->immutable-pair", 
			      scheme_make_prim_w_arity(pair_to_immutable, 
						       "pair->immutable-pair", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("null?", 
			      scheme_make_folding_prim(null_p_prim, 
						       "null?", 
						       1, 1, 1), 
			      env);
  scheme_add_global_constant ("list?", 
			      scheme_make_prim_w_arity(list_p_prim, 
						       "list?", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("list", 
			      scheme_make_prim_w_arity(list_prim, 
						       "list", 
						       0, -1), 
			      env);
  scheme_add_global_constant ("list*", 
			      scheme_make_prim_w_arity(list_star_prim, 
						       "list*", 
						       1, -1), 
			      env);
  scheme_add_global_constant ("length", 
			      scheme_make_prim_w_arity(length_prim, 
						       "length", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("append", 
			      scheme_make_prim_w_arity(append_prim, 
						       "append", 
						       0, -1), 
			      env);
  scheme_add_global_constant ("append!", 
			      scheme_make_prim_w_arity(append_bang_prim, 
						       "append!", 
						       0, -1), 
			      env);
  scheme_add_global_constant ("reverse", 
			      scheme_make_prim_w_arity(reverse_prim, 
						       "reverse", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("reverse!", 
			      scheme_make_prim_w_arity(reverse_bang_prim, 
						       "reverse!", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("list-tail", 
			      scheme_make_prim_w_arity(list_tail_prim, 
						       "list-tail", 
						       2, 2), 
			      env);
  scheme_add_global_constant ("list-ref", 
			      scheme_make_prim_w_arity(list_ref_prim, 
						       "list-ref", 
						       2, 2), 
			      env);
  scheme_add_global_constant ("memq", 
			      scheme_make_prim_w_arity(memq, 
						       "memq", 
						       2, 2), 
			      env);
  scheme_add_global_constant ("memv", 
			      scheme_make_prim_w_arity(memv, 
						       "memv", 
						       2, 2), 
			      env);
  scheme_add_global_constant ("member", 
			      scheme_make_prim_w_arity(member, 
						       "member", 
						       2, 2), 
			      env);
  scheme_add_global_constant ("assq", 
			      scheme_make_prim_w_arity(assq, 
						       "assq", 
						       2, 2), 
			      env);
  scheme_add_global_constant ("assv", 
			      scheme_make_prim_w_arity(assv, 
						       "assv", 
						       2, 2), 
			      env);
  scheme_add_global_constant ("assoc", 
			      scheme_make_prim_w_arity(assoc, 
						       "assoc", 
						       2, 2), 
			      env);
  scheme_add_global_constant ("caar", 
			      scheme_make_prim_w_arity(caar_prim,  
						       "caar", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("cadr", 
			      scheme_make_prim_w_arity(cadr_prim, 
						       "cadr", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("cdar", 
			      scheme_make_prim_w_arity(cdar_prim, 
						       "cdar", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("cddr", 
			      scheme_make_prim_w_arity(cddr_prim, 
						       "cddr", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("caaar", 
			      scheme_make_prim_w_arity(caaar_prim, 
						       "caaar", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("caadr", 
			      scheme_make_prim_w_arity(caadr_prim, 
						       "caadr", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("cadar", 
			      scheme_make_prim_w_arity(cadar_prim, 
						       "cadar", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("cdaar", 
			      scheme_make_prim_w_arity(cdaar_prim, 
						       "cdaar", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("cdadr", 
			      scheme_make_prim_w_arity(cdadr_prim, 
						       "cdadr", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("cddar", 
			      scheme_make_prim_w_arity(cddar_prim, 
						       "cddar", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("caddr", 
			      scheme_make_prim_w_arity(caddr_prim, 
						       "caddr", 
						       1, 1), 
			      env);
  scheme_add_global_constant ("cdddr", 
			      scheme_make_prim_w_arity(cdddr_prim, 
						       "cdddr", 
						       1, 1), 
			      env);  
  scheme_add_global_constant ("cddddr", 
			      scheme_make_prim_w_arity(cddddr_prim, 
						       "cddddr", 
						       1, 1), 
			      env);  
  
  scheme_add_global_constant ("cadddr", 
			      scheme_make_prim_w_arity(cadddr_prim, 
						       "cadddr", 
						       1, 1), 
			      env); 
  scheme_add_global_constant ("cdaddr", 
			      scheme_make_prim_w_arity(cdaddr_prim, 
						       "cdaddr", 
						       1, 1), 
			      env); 
  scheme_add_global_constant ("cddadr", 
			      scheme_make_prim_w_arity(cddadr_prim, 
						       "cddadr", 
						       1, 1), 
			      env); 
  scheme_add_global_constant ("cdddar", 
			      scheme_make_prim_w_arity(cdddar_prim, 
						       "cdddar", 
						       1, 1), 
			      env); 
  scheme_add_global_constant ("caaddr", 
			      scheme_make_prim_w_arity(caaddr_prim, 
						       "caaddr", 
						       1, 1), 
			      env); 
  scheme_add_global_constant ("cadadr", 
			      scheme_make_prim_w_arity(cadadr_prim, 
						       "cadadr", 
						       1, 1), 
			      env); 
  scheme_add_global_constant ("caddar", 
			      scheme_make_prim_w_arity(caddar_prim, 
						       "caddar", 
						       1, 1), 
			      env); 
  scheme_add_global_constant ("cdaadr", 
			      scheme_make_prim_w_arity(cdaadr_prim, 
						       "cdaadr", 
						       1, 1), 
			      env); 
  scheme_add_global_constant ("cdadar", 
			      scheme_make_prim_w_arity(cdadar_prim, 
						       "cdadar", 
						       1, 1), 
			      env); 
  scheme_add_global_constant ("cddaar", 
			      scheme_make_prim_w_arity(cddaar_prim, 
						       "cddaar", 
						       1, 1), 
			      env); 
  scheme_add_global_constant ("cdaaar", 
			      scheme_make_prim_w_arity(cdaaar_prim, 
						       "cdaaar", 
						       1, 1), 
			      env); 
  scheme_add_global_constant ("cadaar", 
			      scheme_make_prim_w_arity(cadaar_prim, 
						       "cadaar", 
						       1, 1), 
			      env); 
  scheme_add_global_constant ("caadar", 
			      scheme_make_prim_w_arity(caadar_prim, 
						       "caadar", 
						       1, 1), 
			      env); 
  scheme_add_global_constant ("caaadr", 
			      scheme_make_prim_w_arity(caaadr_prim, 
						       "caaadr", 
						       1, 1), 
			      env); 
  scheme_add_global_constant ("caaaar", 
			      scheme_make_prim_w_arity(caaaar_prim, 
						       "caaaar", 
						       1, 1), 
			      env); 
  
  scheme_add_global_constant(BOX, 
			     scheme_make_prim_w_arity(box, 
						      BOX, 
						      1, 1), 
			     env);
  scheme_add_global_constant(BOXP, 
			     scheme_make_folding_prim(box_p, 
						      BOXP, 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant(UNBOX, 
			     scheme_make_prim_w_arity(unbox,  
						      UNBOX, 
						      1, 1), 
			     env);
  scheme_add_global_constant(SETBOX, 
			     scheme_make_prim_w_arity(set_box, 
						      SETBOX, 
						      2, 2), 
			     env);
  
  scheme_add_global_constant("make-hash-table", 
			     scheme_make_prim_w_arity(make_hash_table, 
						      "make-hash-table", 
						      0, 0), 
			     env);
  scheme_add_global_constant("make-hash-table-weak", 
			     scheme_make_prim_w_arity(make_hash_table_weak, 
						      "make-hash-table-weak", 
						      0, 0), 
			     env);
  scheme_add_global_constant("hash-table?", 
			     scheme_make_folding_prim(hash_table_p, 
						      "hash-table?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("hash-table-put!", 
			     scheme_make_prim_w_arity(hash_table_put, 
						      "hash-table-put!", 
						      3, 3), 
			     env);
  scheme_add_global_constant("hash-table-get", 
			     scheme_make_prim_w_arity(hash_table_get, 
						      "hash-table-get", 
						      2, 3), 
			     env);
  scheme_add_global_constant("hash-table-remove!", 
			     scheme_make_prim_w_arity(hash_table_remove, 
						      "hash-table-remove!", 
						      2, 2), 
			     env);
  scheme_add_global_constant("hash-table-map", 
			     scheme_make_prim_w_arity(hash_table_map, 
						      "hash-table-map", 
						      2, 2), 
			     env);
  scheme_add_global_constant("hash-table-for-each", 
			     scheme_make_prim_w_arity(hash_table_for_each, 
						      "hash-table-for-each", 
						      2, 2), 
			     env);

  scheme_add_global_constant("make-weak-box",
			     scheme_make_prim_w_arity(make_weak_box,
						      "make-weak-box",
						      1, 1),
			     env);
  scheme_add_global_constant("weak-box-value",
			     scheme_make_prim_w_arity(weak_box_value,
						      "weak-box-value",
						      1, 1),
			     env);
  scheme_add_global_constant("weak-box?",
			     scheme_make_folding_prim(weak_boxp,
						      "weak-box?",
						      1, 1, 1),
			     env);
}

Scheme_Object *
scheme_make_pair (Scheme_Object *car, Scheme_Object *cdr)
{
  Scheme_Object *cons;

#if 0
  if (!SCHEME_INTP(cdr))
    scheme_check_home(cdr);
#endif

  cons = scheme_alloc_object();
  cons->type = scheme_pair_type;
  SCHEME_CAR(cons) = car;
  SCHEME_CDR(cons) = cdr;
  return cons;
}

Scheme_Object *scheme_build_list(int size, Scheme_Object **argv)
{
  Scheme_Object *first = scheme_null, *last = NULL, *pair;
  int i;

  for (i = 0; i < size; i++) {
    pair = scheme_make_pair(argv[i], scheme_null);
    if (!last)
      first = pair;
    else
      SCHEME_CDR (last) = pair;
    last = pair;
  }

  return first;  
}

Scheme_Object *scheme_alloc_list(int size)
{
  Scheme_Object *first = scheme_null, *last = NULL, *pair;
  int i;

  for (i = 0; i < size; i++) {
    pair = scheme_make_pair(scheme_false, scheme_null);
    if (!last)
      first = pair;
    else
      SCHEME_CDR(last) = pair;
    last = pair;
  }

  return first;
}

int
scheme_list_length (Scheme_Object *list)
{
  int len;

  len = 0;
  while (!SCHEME_NULLP(list)) {
    len++;
    if (SCHEME_PAIRP(list))
      list = SCHEME_CDR(list);
    else
      list = scheme_null;
  }

  return len;
}

int
scheme_proper_list_length (Scheme_Object *list)
{
  int len;
  Scheme_Object *turtle;

  len = 0;
  turtle = list;
  while (SCHEME_PAIRP(list)) {
    len++;
    list = SCHEME_CDR(list);
    if (!SCHEME_PAIRP(list))
      break;
    len++;
    list = SCHEME_CDR(list);

    if (SAME_OBJ(turtle, list))
      break;

    turtle = SCHEME_CDR(turtle);
  }
  
  if (SCHEME_NULLP(list))
    return len;

  return -1;
}

Scheme_Object *
scheme_named_map_1 (char *name, Scheme_Object *(*fun)(Scheme_Object*, Scheme_Object*), 
		    Scheme_Object *lst, Scheme_Object *form)
{
  if (SCHEME_NULLP(lst))
      return (scheme_null);
  else if (SCHEME_PAIRP(lst)) {
    Scheme_Object *v;
    v = fun(SCHEME_CAR(lst), form);
    return scheme_make_pair(v,
			    scheme_named_map_1(name, fun, 
					       SCHEME_CDR(lst), form));
  } else {
    scheme_wrong_syntax(name, lst, form, "bad syntax (" IMPROPER_LIST_FORM ")");
    return scheme_void;
  }
}

Scheme_Object *
scheme_map_1 (Scheme_Object *(*fun)(Scheme_Object*), Scheme_Object *lst)
{
  return scheme_named_map_1("map", 
			    (Scheme_Object *(*)(Scheme_Object *, Scheme_Object *))fun, 
			    lst, NULL);
}

Scheme_Object *
scheme_car (Scheme_Object *pair)
{
  return (SCHEME_CAR (pair));
}

Scheme_Object *
scheme_cdr (Scheme_Object *pair)
{
  return (SCHEME_CDR (pair));
}

Scheme_Object *
scheme_cadr (Scheme_Object *pair)
{
  return (SCHEME_CAR (SCHEME_CDR (pair)));
}

Scheme_Object *
scheme_caddr (Scheme_Object *pair)
{
  return (SCHEME_CAR (SCHEME_CDR (SCHEME_CDR (pair))));
}

/* local functions */

static Scheme_Object *
pair_p_prim (int argc, Scheme_Object *argv[])
{
  return (SCHEME_PAIRP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
cons_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *cons;

  cons = scheme_make_pair (argv[0], argv[1]);
  return (cons);
}

static Scheme_Object *
car_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_PAIRP(argv[0]))
    scheme_wrong_type("car", "pair", 0, argc, argv);
  return (SCHEME_CAR (argv[0]));
}

static Scheme_Object *
cdr_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_PAIRP(argv[0]))
    scheme_wrong_type("cdr", "pair", 0, argc, argv);

  return (SCHEME_CDR (argv[0]));
}

static Scheme_Object *
set_car_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_MUTABLE_PAIRP(argv[0]))
    scheme_wrong_type("set-car!", "mutable-pair", 0, argc, argv);

  SCHEME_CAR (argv[0]) = argv[1];
  return scheme_void;
}

static Scheme_Object *
set_cdr_prim (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_MUTABLE_PAIRP(argv[0]))
    scheme_wrong_type("set-cdr!", "mutable-pair", 0, argc, argv);

  SCHEME_CDR (argv[0]) = argv[1];
  return scheme_void;
}

static Scheme_Object *
pair_to_immutable (int argc, Scheme_Object *argv[])
{
  Scheme_Object *p;

  p = argv[0];

  if (!SCHEME_PAIRP(p))
    scheme_wrong_type("pair->immutable-pair", "pair", 0, argc, argv);

  if (SCHEME_MUTABLE_PAIRP(p)) {
    Scheme_Object *p2;
    p2 = scheme_make_pair(SCHEME_CAR(p), SCHEME_CDR(p));
    SCHEME_SET_PAIR_IMMUTABLE(p2);
    return p2;
  } else 
    return argv[0];
}

static Scheme_Object *
null_p_prim (int argc, Scheme_Object *argv[])
{
  return (SCHEME_NULLP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
list_p_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *obj1, *obj2;

  obj1 = obj2 = argv[0];
  do {
    if (SCHEME_NULLP(obj1))
      return scheme_true;
    if (!SCHEME_PAIRP(obj1))
      return (scheme_false);
    
    obj1 = SCHEME_CDR (obj1);
    
    if (SCHEME_NULLP(obj1))
      return scheme_true;
    if (!SCHEME_PAIRP(obj1))
      return scheme_false;
    
    obj1 = SCHEME_CDR(obj1);
    
    obj2 = SCHEME_CDR(obj2);
  } while (NOT_SAME_OBJ(obj1, obj2));

  return scheme_false;
}

static Scheme_Object *
list_exec (int argc, Scheme_Object *argv[], int star)
{
  int i;
  Scheme_Object *first, *last, *pair;

  if (star)
    --argc;

  first = last = scheme_null;
  for (i = 0; i < argc ; i++) {
    pair = scheme_make_pair (argv[i], scheme_null);
    if (SCHEME_NULLP(last))
      first = last = pair;
    else {
      SCHEME_CDR (last) = pair;
      last = pair;
    }
  }

  if (star) {
    if (SCHEME_NULLP(last))
      first = argv[argc];
    else
      SCHEME_CDR (last) = argv[argc];
  }

  return first;
}

static Scheme_Object *
list_prim (int argc, Scheme_Object *argv[])
{
  return list_exec(argc, argv, 0);
}

static Scheme_Object *
list_star_prim (int argc, Scheme_Object *argv[])
{
  return list_exec(argc, argv, 1);
}

static Scheme_Object *
length_prim (int argc, Scheme_Object *argv[])
{
  int l;

  if (!SCHEME_LISTP(argv[0]))
    scheme_wrong_type("length", "proper list", 0, argc, argv);

  l = scheme_proper_list_length(argv[0]);

  if (l < 0)
    scheme_wrong_type("length", "proper list", 0, argc, argv);

  return scheme_make_integer(l);
}

Scheme_Object *
scheme_append (Scheme_Object *lst1, Scheme_Object *lst2)
{
  Scheme_Object *first, *last, *orig1, *v;

  orig1 = lst1;

  first = last = NULL;
  while (SCHEME_PAIRP(lst1)) {
    v = scheme_make_pair(SCHEME_CAR(lst1), scheme_null);
    if (!first)
      first = v;
    else
      SCHEME_CDR(last) = v;
    last = v;
    lst1 = SCHEME_CDR(lst1);

    SCHEME_USE_FUEL(1);
  }
  
  if (!SCHEME_NULLP(lst1))
    scheme_wrong_type("append", "proper list", -1, 0, &orig1);

  if (!last)
    return lst2;
  
  SCHEME_CDR(last) = lst2;

  return first;
}

static Scheme_Object *
scheme_append_bang (Scheme_Object *lst1, Scheme_Object *lst2)
{
  if (SCHEME_NULLP(lst1))
    return lst2;
  else {
    Scheme_Object *prev, *orig;

    orig = lst1;

    do {
      prev = lst1;
      if (!SCHEME_PAIRP(lst1))
	scheme_wrong_type("append!", "proper list", -1, 0, &lst1);
      lst1 = SCHEME_CDR(lst1);

      SCHEME_USE_FUEL(1);
    } while (!SCHEME_NULLP(lst1));

    if (!SCHEME_MUTABLE_PAIRP(prev))
      scheme_wrong_type("append!", "mutable proper list", -1, 0, &lst1);
    SCHEME_CDR(prev) = lst2;

    return orig;
  }
}

static Scheme_Object *
append_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *res;
  int i;

  if (!argc)
    return scheme_null;

  res = argv[argc - 1];
  for (i = argc - 1; i--;  ) {
    res = scheme_append(argv[i], res);
  }

  return res;
}

static Scheme_Object *
append_bang_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *res;
  int i;

  if (!argc)
    return scheme_null;

  res = argv[argc - 1];
  for (i = argc - 1; i--; ) {
    res = scheme_append_bang(argv[i], res);
  }

  return res;
}

static Scheme_Object *
reverse_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *lst, *last;

  last = scheme_null;
  lst = argv[0];
  while (!SCHEME_NULLP (lst)) {
    if (!SCHEME_PAIRP(lst))
      scheme_wrong_type("reverse", "proper list", 0, argc, argv);
    last = scheme_make_pair (SCHEME_CAR (lst), last);
    lst = SCHEME_CDR (lst);

    SCHEME_USE_FUEL(1);
  }
  return (last);
}

static Scheme_Object *
reverse_bang_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *lst, *prev, *next;

  prev = NULL;
  lst = argv[0];
  while (!SCHEME_NULLP(lst)) {
    if (!SCHEME_MUTABLE_PAIRP(lst))
      scheme_wrong_type("reverse!", "mutable proper list", 0, argc, argv);
    next = SCHEME_CDR(lst);
    if (prev)
      SCHEME_CDR(lst) = prev;
    else
      SCHEME_CDR(lst) = scheme_null;
    prev = lst;
    lst = next;

    SCHEME_USE_FUEL(1);
  }

  if (prev)
    return prev;
  else
    return scheme_null;
}

#define OCCASIONAL_CHECK ((int)0xFF)
#ifdef PALMOS_STUFF
# define LISTREF_BIGNUM_SLICE 1000
#else
# define LISTREF_BIGNUM_SLICE 1000000
#endif

static Scheme_Object *
do_list_ref(char *name, int takecar, int argc, Scheme_Object *argv[])
{
  int i, k;
  Scheme_Object *lst, *index, *bnindex;

#if 0
  if (!SCHEME_LISTP(argv[0]))
    scheme_wrong_type(name, "list", 0, argc, argv);
#endif

  if (SCHEME_BIGNUMP(argv[1])) {
    bnindex = argv[1];
    k = 0;
#ifdef ALLOW_INEXACT_INDICES
  } else if (SCHEME_DBLP(argv[1])) {
    double d;
    d = SCHEME_DBL_VAL(argv[1]);
    bnindex = scheme_double_to_integer(name, d);
    if (!bnindex)
      scheme_wrong_type(name, "non-negative integer", 1, argc, argv);
    if (SCHEME_INTP(bnindex)) {
      k = SCHEME_INT_VAL(bnindex);
      bnindex = NULL;
    } else
      k = 0;
#endif
  } else if (!SCHEME_INTP(argv[1])) {
    scheme_wrong_type(name, "non-negative exact integer", 1, argc, argv);
    return NULL;
  } else {
    bnindex = NULL;
    k = SCHEME_INT_VAL(argv[1]);
  }

  lst = argv[0];
  index = argv[1];

  if ((bnindex && !SCHEME_BIGPOS(bnindex))
      || (!bnindex && (k < 0))) {
    scheme_wrong_type(name, "non-negative exact integer", 1, argc, argv);
    return NULL;
  }

  do {
    if (bnindex) {
      if (SCHEME_INTP(bnindex)) {
	k = SCHEME_INT_VAL(bnindex);
	bnindex = 0;
      } else {
	k = LISTREF_BIGNUM_SLICE;
	bnindex = scheme_bin_minus(bnindex, scheme_make_integer(LISTREF_BIGNUM_SLICE));
      }
    }

    for (i = 0; i < k; i++) {
      if (!SCHEME_PAIRP(lst)) {
	char *lstr;
	int llen;
	
	lstr = scheme_make_provided_string(argv[0], 2, &llen);
	scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
			 index,
			 "%s: index %s too large for list%s: %t", name,
			 scheme_make_provided_string(index, 2, NULL),
			 SCHEME_NULLP(lst) ? "" : " (not a proper list)",
			 lstr, llen);
	return NULL;
      }
      lst = SCHEME_CDR(lst);
      if (!(i & OCCASIONAL_CHECK))
	SCHEME_USE_FUEL(OCCASIONAL_CHECK);
    }
  } while(bnindex);

  if (takecar) {
    if (!SCHEME_PAIRP(lst)) {
      char *lstr;
      int llen;
      
      lstr = scheme_make_provided_string(argv[0], 2, &llen);
      scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		       index,
		       "%s: index %s too large for list%s: %t", name,
		       scheme_make_provided_string(index, 2, NULL),
		       SCHEME_NULLP(lst) ? "" : " (not a proper list)",
		       lstr, llen);
      return NULL;
    }
    
    return SCHEME_CAR(lst);
  } else
    return lst;
}

static Scheme_Object *
list_tail_prim(int argc, Scheme_Object *argv[])
{
  return do_list_ref("list-tail", 0, argc, argv);
}

static Scheme_Object *
list_ref_prim(int argc, Scheme_Object *argv[])
{
  return do_list_ref("list-ref", 1, argc, argv);
}


#define GEN_MEM(name, scheme_name, comp) \
static Scheme_Object * \
name (int argc, Scheme_Object *argv[]) \
{ \
  Scheme_Object *list; \
  list = argv[1]; \
  while (SCHEME_PAIRP(list)) \
    { \
      if (comp (argv[0], SCHEME_CAR (list))) \
	{ \
          return (list); \
	} \
      list = SCHEME_CDR (list); \
    } \
  if (!SCHEME_NULLP(list)) { \
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH, argv[1], \
		     "%s: not a proper list: %V", #scheme_name, \
		     argv[1]); \
  } \
  return (scheme_false); \
}
    
GEN_MEM(memv, memv, scheme_eqv)
GEN_MEM(memq, memq, scheme_eq)
GEN_MEM(member, member, scheme_equal)

#define GEN_ASS(name, scheme_name, comp) \
static Scheme_Object * \
name (int argc, Scheme_Object *argv[]) \
{ \
  Scheme_Object *pair, *list; \
  list = argv[1]; \
  while (SCHEME_PAIRP (list)) \
    { \
      pair = SCHEME_CAR (list); \
      if (!SCHEME_PAIRP (pair)) {\
        char *npstr, *lstr; \
        int nplen, llen; \
        npstr = scheme_make_provided_string(pair, 2, &nplen); \
        lstr = scheme_make_provided_string(argv[1], 2, &llen); \
	scheme_raise_exn(MZEXN_APPLICATION_MISMATCH, argv[1], \
			 "%s: non-pair found in list: %t in %t", #scheme_name, \
			 npstr, nplen, \
			 lstr, llen); \
	return NULL; \
      } \
      if (comp (argv[0], SCHEME_CAR (pair))) \
	{ \
          return (pair); \
	} \
      list = SCHEME_CDR (list); \
    } \
  if (!SCHEME_NULLP(list)) {\
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH, argv[1], \
		     "%s: not a proper list: %V", #scheme_name, \
		     argv[1]); \
  } \
  return (scheme_false); \
}

GEN_ASS(assv, assv, scheme_eqv)
GEN_ASS(assq, assq, scheme_eq)
GEN_ASS(assoc, assoc, scheme_equal)

#define LISTFUNC2(name, C, D) \
static Scheme_Object * \
name ## _prim (int argc, Scheme_Object *argv[]) \
{ \
  if (!(SCHEME_PAIRP(argv[0]) \
	&& SCHEME_PAIRP(SCHEME_ ## D (argv[0])))) \
      scheme_wrong_type(#name, #name "able value", 0, argc, argv); \
  return SCHEME_ ## C (SCHEME_ ## D (argv[0])); \
}

LISTFUNC2(cddr, CDR, CDR)
LISTFUNC2(cadr, CAR, CDR)
LISTFUNC2(cdar, CDR, CAR)
LISTFUNC2(caar, CAR, CAR)

#define LISTFUNC3(name, B, C, D) \
static Scheme_Object * \
name ## _prim (int argc, Scheme_Object *argv[]) \
{ \
  if (!((SCHEME_PAIRP(argv[0])) \
	&& SCHEME_PAIRP(SCHEME_ ## D (argv[0])) \
	&& SCHEME_PAIRP(SCHEME_ ## C (SCHEME_ ## D (argv[0]))))) \
    scheme_wrong_type(#name, #name "able value", 0, argc, argv); \
  return SCHEME_ ## B (SCHEME_ ## C (SCHEME_ ## D (argv[0]))); \
}

LISTFUNC3(cdddr, CDR, CDR, CDR)

LISTFUNC3(caddr, CAR, CDR, CDR)
LISTFUNC3(cdadr, CDR, CAR, CDR)
LISTFUNC3(cddar, CDR, CDR, CAR)

LISTFUNC3(cdaar, CDR, CAR, CAR)
LISTFUNC3(cadar, CAR, CDR, CAR)
LISTFUNC3(caadr, CAR, CAR, CDR)

LISTFUNC3(caaar, CAR, CAR, CAR)


#define LISTFUNC4(name, A, B, C, D) \
static Scheme_Object * \
name ## _prim (int argc, Scheme_Object *argv[]) \
{ \
  if (!(SCHEME_PAIRP(argv[0]) \
	&& SCHEME_PAIRP(SCHEME_ ## D (argv[0])) \
	&& SCHEME_PAIRP(SCHEME_ ## C (SCHEME_ ## D (argv[0]))) \
	&&SCHEME_PAIRP(SCHEME_ ## B (SCHEME_ ## C (SCHEME_ ## D (argv[0]))))))\
    scheme_wrong_type(#name, #name "able value", 0, argc, argv); \
  return SCHEME_ ## A (SCHEME_ ## B (SCHEME_ ## C (SCHEME_ ## D (argv[0]))));\
}

LISTFUNC4(cddddr, CDR, CDR, CDR, CDR)

LISTFUNC4(cadddr, CAR, CDR, CDR, CDR)
LISTFUNC4(cdaddr, CDR, CAR, CDR, CDR)
LISTFUNC4(cddadr, CDR, CDR, CAR, CDR)
LISTFUNC4(cdddar, CDR, CDR, CDR, CAR)

LISTFUNC4(caaddr, CAR, CAR, CDR, CDR)
LISTFUNC4(cadadr, CAR, CDR, CAR, CDR)
LISTFUNC4(caddar, CAR, CDR, CDR, CAR)
LISTFUNC4(cdaadr, CDR, CAR, CAR, CDR)
LISTFUNC4(cdadar, CDR, CAR, CDR, CAR)
LISTFUNC4(cddaar, CDR, CDR, CAR, CAR)

LISTFUNC4(cdaaar, CDR, CAR, CAR, CAR)
LISTFUNC4(cadaar, CAR, CDR, CAR, CAR)
LISTFUNC4(caadar, CAR, CAR, CDR, CAR)
LISTFUNC4(caaadr, CAR, CAR, CAR, CDR)

LISTFUNC4(caaaar, CAR, CAR, CAR, CAR)

Scheme_Object *scheme_box(Scheme_Object *v)
{
  Scheme_Object *obj;

  obj = scheme_alloc_small_object();
  obj->type = scheme_box_type;
  SCHEME_BOX_VAL(obj) = v;

  return obj;
}

Scheme_Object *scheme_unbox(Scheme_Object *obj)
{  
  if (!SCHEME_BOXP(obj))
      scheme_wrong_type(UNBOX, "box", 0, 1, &obj);
  return (Scheme_Object *)SCHEME_BOX_VAL(obj);
}

void scheme_set_box(Scheme_Object *b, Scheme_Object *v)
{
  if (!SCHEME_BOXP(b))
      scheme_wrong_type(SETBOX, "box", 0, 1, &b);
  SCHEME_BOX_VAL(b) = v;
}

static Scheme_Object *box(int c, Scheme_Object *p[])
{
  return scheme_box(p[0]);
}

static Scheme_Object *box_p(int c, Scheme_Object *p[])
{
  return SCHEME_BOXP(p[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *unbox(int c, Scheme_Object *p[])
{
  return scheme_unbox(p[0]);
}

static Scheme_Object *set_box(int c, Scheme_Object *p[])
{
  scheme_set_box(p[0], p[1]);
  return scheme_void;
}

static Scheme_Object *do_make_hash_table(int weak)
{
  return (Scheme_Object *)scheme_hash_table(20, 
					    weak
					    ? SCHEME_hash_weak_ptr
					    : SCHEME_hash_ptr, 
					    0, 0);
}

static Scheme_Object *make_hash_table(int argc, Scheme_Object *argv[])
{
  return do_make_hash_table(0);
}

static Scheme_Object *make_hash_table_weak(int argc, Scheme_Object *argv[])
{
  return do_make_hash_table(1);
}

static Scheme_Object *hash_table_p(int argc, Scheme_Object *argv[])
{
  return SCHEME_HASHTP(argv[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *hash_table_put(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_HASHTP(argv[0]))
    scheme_wrong_type("hash-table-put!", "hash table", 0, argc, argv);

  scheme_add_to_table((Scheme_Hash_Table *)argv[0], (char *)argv[1], 
		      (void *)argv[2], 0);

  return scheme_void;
}

static Scheme_Object *hash_table_get(int argc, Scheme_Object *argv[])
{
  void *v;

  if (!SCHEME_HASHTP(argv[0]))
    scheme_wrong_type("hash-table-get", "hash table", 0, argc, argv);

  v = scheme_lookup_in_table((Scheme_Hash_Table *)argv[0], (char *)argv[1]);

  if (v)
    return (Scheme_Object *)v;
  else if (argc == 3)
    return _scheme_tail_apply(argv[2], 0, NULL);
  else {
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		     argv[1],
		     "hash-table-get: no value found for key: %V",
		     argv[1]);
    return scheme_void;
  }
}

static Scheme_Object *hash_table_remove(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_HASHTP(argv[0]))
    scheme_wrong_type("hash-table-remove!", "hash table", 0, argc, argv);

  scheme_change_in_table((Scheme_Hash_Table *)argv[0], (char *)argv[1], NULL);

  return scheme_void;
}

static Scheme_Object *do_map_hash_table(int argc,
					Scheme_Object *argv[],
					char *name,
					int keep)
{
  int i;
  Scheme_Hash_Table *hash;
  Scheme_Object *f;
  Scheme_Bucket *bucket;
  Scheme_Object *first, *last = NULL, *v, *p[2];

  if (!SCHEME_HASHTP(argv[0]))
    scheme_wrong_type(name, "hash table", 0, argc, argv);
  scheme_check_proc_arity(name, 2, 1, argc, argv);

  hash = (Scheme_Hash_Table *)argv[0];
  f = argv[1];

  if (keep)
    first = scheme_null;
  else
    first = scheme_void;

  for (i = hash->size; i--; ) {
    bucket = hash->buckets[i];
    if (bucket && bucket->val && bucket->key) {
      if (hash->weak)
	p[0] = (Scheme_Object *)HT_EXTRACT_WEAK(bucket->key);
      else
	p[0] = (Scheme_Object *)bucket->key;
      p[1] = (Scheme_Object *)bucket->val;
      if (keep) {
	v = _scheme_apply(f, 2, p);
	v = scheme_make_pair(v, scheme_null);
	if (last)
	  SCHEME_CDR(last) = v;
	else
	  first = v;
	last = v;
      } else
	_scheme_apply_multi(f, 2, p);
    }
  }
  
  return first;
}

static Scheme_Object *hash_table_map(int argc, Scheme_Object *argv[])
{
  return do_map_hash_table(argc, argv, "hash-table-map", 1);
}

static Scheme_Object *hash_table_for_each(int argc, Scheme_Object *argv[])
{
  return do_map_hash_table(argc, argv, "hash-table-for-each", 0);
}

Scheme_Object *scheme_make_weak_box(Scheme_Object *v)
{
#ifdef MZ_PRECISE_GC
  return (Scheme_Object *)GC_malloc_weak_box(v, NULL, 0);
#else
  Scheme_Small_Object *obj;

  obj = MALLOC_ONE_TAGGED_WEAK(Scheme_Small_Object);

  obj->type = scheme_weak_box_type;

  obj->u.ptr_val = v;
  scheme_weak_reference((void **)&obj->u.ptr_val);

  return (Scheme_Object *)obj;
#endif
}

static Scheme_Object *make_weak_box(int argc, Scheme_Object *argv[])
{
  return scheme_make_weak_box(argv[0]);
}

static Scheme_Object *weak_box_value(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o;

  if (!SCHEME_WEAKP(argv[0]))
    scheme_wrong_type("weak-box-value", "weak-box", 0, argc, argv);

  o = SCHEME_BOX_VAL(argv[0]);
  if (!o)
    return scheme_false;
  else
    return o;
}

static Scheme_Object *weak_boxp(int argc, Scheme_Object *argv[])
{
  return (SCHEME_WEAKP(argv[0]) ? scheme_true : scheme_false);
}
