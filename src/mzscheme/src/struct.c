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

typedef struct {
  Scheme_Type type;
  Scheme_Object *name;
  Scheme_Object *fields;
  Scheme_Object *parent_type_expr;
  int num_fields;
  int count;
  Scheme_Object **memo_names; /* Memoize name generation */
} Struct_Info;

typedef enum {
  SCHEME_CONSTR = 1, 
  SCHEME_PRED, 
  SCHEME_GETTER, 
  SCHEME_SETTER
} Scheme_ProcT;

typedef struct {
  Scheme_Struct_Type *struct_type;
  Scheme_Object *func_name;
  short field;
} Struct_Proc_Info;

typedef struct {
  int num_levels;
  int *levels;
  Scheme_Hash_Table **tables;
  Scheme_Object *elsep;
} Struct_Case;

/* globals */
Scheme_Object *scheme_arity_at_least, *scheme_date;

/* locals */
#ifdef USE_STRUCT_CASE  
static Scheme_Object *make_struct_case(int argc, Scheme_Object **argv);
#endif

static Scheme_Object *struct_syntax(Scheme_Object *form, Scheme_Comp_Env *env, Scheme_Compile_Info *rec);
static Scheme_Object *struct_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);

static Scheme_Object *struct_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_type_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_length(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_ref(int argc, Scheme_Object *argv[]);

static Scheme_Object *struct_to_vector(int argc, Scheme_Object *argv[]);

static Scheme_Object *struct_setter_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_getter_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_pred_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *struct_constr_p(int argc, Scheme_Object *argv[]);

static Scheme_Object *make_struct_proc(Scheme_Struct_Type *struct_type, Scheme_Object *func_name, Scheme_ProcT proc_type, int field_num);

static Scheme_Object *make_name(const char *pre, const char *tn, int tnl, const char *post1, const char *fn, int fnl, const char *post2);

static Scheme_Object *struct_execute(Scheme_Object *form);

static Scheme_Object *write_struct_info(Scheme_Object *obj);
static Scheme_Object *read_struct_info(Scheme_Object *obj);

#define cons scheme_make_pair
#define _intern scheme_intern_symbol

#define BUILTIN_STRUCT_FLAGS 0

static Scheme_Object **as_names;
static Scheme_Object **as_values;
static int as_count;
#ifdef TIME_SYNTAX
static Scheme_Object **ts_names;
static Scheme_Object **ts_values;
static int ts_count;
#endif
#ifndef NO_UNIT_SYSTEM
static Scheme_Object *signature;
static Scheme_Object **us_names;
static Scheme_Object **us_values;
static int us_count;
#endif

static Scheme_Object *struct_symbol;

void
scheme_init_struct (Scheme_Env *env)
{
  int i;

  if (scheme_starting_up) {
    static const char *arity_fields[1] = { "value" };
#ifdef TIME_SYNTAX
    static const char *date_fields[9] = { "second", "minute", "hour",
					    "day", "month", "year",
					    "week-day", "year-day", "dst?" };
#endif
#ifndef NO_UNIT_SYSTEM
    static const char *unit_fields[3] = { "unit", "imports", "exports" };
#endif

    REGISTER_SO(scheme_arity_at_least);
    REGISTER_SO(as_names);
    REGISTER_SO(as_values);
#ifdef TIME_SYNTAX
    REGISTER_SO(scheme_date);
    REGISTER_SO(ts_names);
    REGISTER_SO(ts_values);
#endif
#ifndef NO_UNIT_SYSTEM
    REGISTER_SO(signature);
    REGISTER_SO(us_names);
    REGISTER_SO(us_values);
#endif

    REGISTER_SO(struct_symbol);

    scheme_register_syntax("k", struct_execute);

    struct_symbol = scheme_intern_symbol("#%struct");

    scheme_install_type_writer(scheme_struct_info_type, write_struct_info);
    scheme_install_type_reader(scheme_struct_info_type, read_struct_info);

    scheme_arity_at_least = scheme_make_struct_type_from_string("arity-at-least", NULL, 1);

    as_names = scheme_make_struct_names_from_array("arity-at-least",
						   1, arity_fields,
						   BUILTIN_STRUCT_FLAGS, 
						   &as_count);

#ifdef TIME_SYNTAX
    scheme_date = scheme_make_struct_type_from_string("date", NULL, 9);

    ts_names 
      = scheme_make_struct_names_from_array("date",
					    9, date_fields,
					    BUILTIN_STRUCT_FLAGS, &ts_count);
#endif

#ifndef NO_UNIT_SYSTEM
    signature = scheme_make_struct_type_from_string("unit-with-signature", NULL, 3);
    us_names 
      = scheme_make_struct_names_from_array("unit-with-signature",
					    3, unit_fields,
					    BUILTIN_STRUCT_FLAGS, &us_count);
#endif

  as_values = scheme_make_struct_values(scheme_arity_at_least, as_names, as_count, 
					BUILTIN_STRUCT_FLAGS);
#ifdef TIME_SYNTAX
  ts_values = scheme_make_struct_values(scheme_date, ts_names, ts_count, 
					BUILTIN_STRUCT_FLAGS);
#endif
#ifndef NO_UNIT_SYSTEM
  us_values = scheme_make_struct_values(signature, us_names, us_count, 
					BUILTIN_STRUCT_FLAGS);
#endif
  }

  scheme_add_global_keyword("struct", 
			    scheme_make_compiled_syntax(struct_syntax, 
							struct_expand), 
			    env);

  scheme_add_global_constant("struct?",
			    scheme_make_folding_prim(struct_p,
						     "struct?",
						     1, 1, 1),
			    env);
  scheme_add_global_constant("struct-type?",
			    scheme_make_folding_prim(struct_type_p,
						     "struct-type?",
						     1, 1, 1),
			    env);
  scheme_add_global_constant("struct-length",
			    scheme_make_folding_prim(struct_length,
						     "struct-length",
						     1, 1, 1),
			    env);
  scheme_add_global_constant("struct-ref",
			    scheme_make_prim_w_arity(struct_ref,
						     "struct-ref",
						     2, 2),
			    env);

  scheme_add_global_constant("struct->vector",
			    scheme_make_prim_w_arity(struct_to_vector,
						     "struct->vector",
						     1, 1),
			    env);

  scheme_add_global_constant("struct-setter-procedure?",
			     scheme_make_prim_w_arity(struct_setter_p,
						      "struct-setter-procedure?",
						      1, 1),
			    env);
  scheme_add_global_constant("struct-getter-procedure?",
			     scheme_make_prim_w_arity(struct_getter_p,
						      "struct-getter-procedure?",
						      1, 1),
			    env);
  scheme_add_global_constant("struct-predicate-procedure?",
			     scheme_make_prim_w_arity(struct_pred_p,
						      "struct-predicate-procedure?",
						      1, 1),
			    env);
  scheme_add_global_constant("struct-constructor-procedure?",
			     scheme_make_prim_w_arity(struct_constr_p,
						      "struct-constructor-procedure?",
						      1, 1),
			    env);
  
#ifdef USE_STRUCT_CASE  
  scheme_add_global_constant("make-struct-case",
			     scheme_make_prim_w_arity(make_struct_case,
						      "make-struct-case",
						      2, 3),
			     env);
#endif

  /* Add arity structure */
  for (i = 0; i < as_count; i++)
    scheme_add_global_constant(SCHEME_SYM_VAL(as_names[i]), as_values[i],
			       env);

#ifdef TIME_SYNTAX
  for (i = 0; i < ts_count; i++)
    scheme_add_global_constant(SCHEME_SYM_VAL(ts_names[i]), ts_values[i], 
			       env);
#endif

#ifndef NO_UNIT_SYSTEM
  for (i = 0; i < us_count; i++)
    scheme_add_global_constant(SCHEME_SYM_VAL(us_names[i]), us_values[i], 
			       env);  
#endif
}

static void wrong_struct_type(Scheme_Object *name, 
			      Scheme_Object *expected,
			      Scheme_Object *received,
			      int which, int argc,
			      Scheme_Object **argv)
{
  if ((SCHEME_SYM_LEN(expected) == SCHEME_SYM_LEN(received))
      && !strncmp(SCHEME_SYM_VAL(expected), 
		  SCHEME_SYM_VAL(received),
		  SCHEME_SYM_LEN(expected)))
    scheme_raise_exn(MZEXN_APPLICATION_TYPE, argv[which], expected,
			"%s: expects args of type <%s>; "
			"given instance of a different <%s>",
			scheme_symbol_name(name), 
			scheme_symbol_name(expected), 
			scheme_symbol_name(received));
  else
    scheme_wrong_type(scheme_symbol_name(name), 
		      scheme_symbol_name(expected), 
		      which, argc, argv);
}

#define STRUCT_TYPEP(st, v) \
        ((st->name_pos <= v->stype->name_pos) \
	 && (st == v->stype->parent_types[st->name_pos]))

int scheme_is_struct_instance(Scheme_Object *type, Scheme_Object *v)
{
  Scheme_Struct_Type *stype =  (Scheme_Struct_Type *)type;
  Scheme_Structure *s = (Scheme_Structure *)v;

  return STRUCT_TYPEP(stype, s);
}

Scheme_Object *
scheme_make_struct_instance(Scheme_Object *_stype, int argc, Scheme_Object **args)
{
  Scheme_Structure *inst;
  Scheme_Struct_Type *stype;
  int i;

  stype = (Scheme_Struct_Type *)_stype;
  
  inst = (Scheme_Structure *)
    scheme_malloc_tagged(sizeof(Scheme_Structure) 
			 + ((stype->num_slots - 1) * sizeof(Scheme_Object *)));
  
  inst->type = scheme_structure_type;
  inst->stype = stype;

  for (i = 0; i < argc; i++)
    inst->slots[i] = args[i];
  
  return (Scheme_Object *)inst;
}

static Scheme_Object *struct_pred(Scheme_Struct_Type *stype, int argc, Scheme_Object **args)
{
  if (SAME_TYPE(SCHEME_TYPE(args[0]), scheme_structure_type)
      && STRUCT_TYPEP(stype, ((Scheme_Structure *)args[0])))
    return scheme_true;
  else
    return scheme_false;
}

static Scheme_Object *struct_getter(Struct_Proc_Info *i, int argc, Scheme_Object **args)
{
  Scheme_Structure *inst;

  if (NOT_SAME_TYPE(SCHEME_TYPE(args[0]), scheme_structure_type))
    scheme_wrong_type(scheme_symbol_name(i->func_name), 
		      scheme_symbol_name(i->struct_type->type_name), 
		      0, argc, args);
  
  inst = (Scheme_Structure *)args[0];
  if (!STRUCT_TYPEP(i->struct_type, inst))
    wrong_struct_type(i->func_name, 
		      i->struct_type->type_name, 
		      SCHEME_STRUCT_NAME_SYM(inst), 
		      0, argc, args);
  
  return inst->slots[i->field];
}

static Scheme_Object *struct_setter(Struct_Proc_Info *i, int argc, Scheme_Object **args)
{
  Scheme_Structure *inst;

  if (NOT_SAME_TYPE(SCHEME_TYPE(args[0]), scheme_structure_type))
    scheme_wrong_type(scheme_symbol_name(i->func_name), 
		      scheme_symbol_name(i->struct_type->type_name), 
		      0, argc, args);
	
  inst = (Scheme_Structure *)args[0];
  if (!STRUCT_TYPEP(i->struct_type, inst))
    wrong_struct_type(i->func_name, 
		      i->struct_type->type_name, 
		      SCHEME_STRUCT_NAME_SYM(inst),
		      0, argc, args);
	
  inst->slots[i->field] = args[1];
  
  return scheme_void;
}

int scheme_equal_structs (Scheme_Object *obj1, Scheme_Object *obj2)
{
  Scheme_Structure *s1, *s2;
  int i;

  if (!SAME_TYPE(SCHEME_TYPE(obj1), scheme_structure_type)
      || !SAME_TYPE(SCHEME_TYPE(obj2), scheme_structure_type))
    return 0;

  s1 = (Scheme_Structure *)obj1;
  s2 = (Scheme_Structure *)obj2;

  if (SCHEME_STRUCT_TYPE(s1) != SCHEME_STRUCT_TYPE(s2))
    return 0;

  for (i = SCHEME_STRUCT_NUM_SLOTS(s1); i--; )
    if (!scheme_equal(s1->slots[i], s2->slots[i]))
      return 0;

  return 1;
}

static Scheme_Object *
struct_p(int argc, Scheme_Object *argv[])
{
  return SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_structure_type)
    ? scheme_true : scheme_false;
}

static Scheme_Object *
struct_type_p(int argc, Scheme_Object *argv[])
{
  return SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_struct_type_type)
    ? scheme_true : scheme_false;
}

static Scheme_Object *
struct_length(int argc, Scheme_Object *argv[])
{
  Scheme_Structure *s;

  if (NOT_SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_structure_type))
    scheme_wrong_type("struct-length", "struct", 0, argc, argv);

  s = (Scheme_Structure *)argv[0];

  return scheme_make_integer(SCHEME_STRUCT_NUM_SLOTS(s));
}

static Scheme_Object *
struct_ref(int argc, Scheme_Object *argv[])
{
  Scheme_Structure *s;
  long i, m;
  char *name = "struct-ref";

  if (NOT_SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_structure_type))
    scheme_wrong_type(name, "struct", 0, argc, argv);
  
  s = (Scheme_Structure *)argv[0];
  m = SCHEME_STRUCT_NUM_SLOTS(s);

  i = scheme_extract_index(name, 1, argc, argv, m);

  if (i >= m)
    scheme_raise_exn(MZEXN_APPLICATION_RANGE_BOUNDS_STRUCT,
		     argv[1],
		     scheme_make_integer(0),
		     scheme_make_integer(m - 1),
		     "%s: index %s out of range [%d, %d]",
		     name, 
		     scheme_make_provided_string(argv[1], 0, NULL), 
		     0, m - 1);

  return s->slots[i];
}

static Scheme_Object *struct_to_vector(int argc, Scheme_Object *argv[])
{
  Scheme_Structure *s;
  Scheme_Object *v, **array;
  int i, m;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_structure_type))
    scheme_wrong_type("struct->vector", "struct", 0, argc, argv);

  s = (Scheme_Structure *)argv[0];

  m = SCHEME_STRUCT_NUM_SLOTS(s);

  v = scheme_make_vector(m + 1, scheme_false);
  array = SCHEME_VEC_ELS(v);
  array[0] = SCHEME_STRUCT_NAME_SYM(s);
  array++;
  for (i = m; i--; )
    array[i] = s->slots[i];

  return v;
}

#define STRUCT_PROCP(o, t) \
    (SCHEME_STRUCT_PROCP(o) && (((Scheme_Closed_Primitive_Proc *)o)->flags & t))

static Scheme_Object *
struct_setter_p(int argc, Scheme_Object *argv[])
{
  return (STRUCT_PROCP(argv[0], SCHEME_PRIM_IS_STRUCT_SETTER)
	  ? scheme_true : scheme_false);
}

static Scheme_Object *
struct_getter_p(int argc, Scheme_Object *argv[])
{
  return (STRUCT_PROCP(argv[0], SCHEME_PRIM_IS_STRUCT_GETTER)
	  ? scheme_true : scheme_false);
}

static Scheme_Object *
struct_pred_p(int argc, Scheme_Object *argv[])
{
  return (STRUCT_PROCP(argv[0], SCHEME_PRIM_IS_STRUCT_PRED)
	  ? scheme_true : scheme_false);
}

static Scheme_Object *
struct_constr_p(int argc, Scheme_Object *argv[])
{
  return (STRUCT_PROCP(argv[0], SCHEME_PRIM_IS_STRUCT_CONSTR)
	  ? scheme_true : scheme_false);
}

#define NUM_BASE_VALUES 3
#define NUM_VALUES_PER_FIELD 2

#define TYPE_NAME(base, blen) make_name("struct:", base, blen, "", NULL, 0, "")
#define CSTR_NAME(base, blen) make_name("make-", base, blen, "", NULL, 0, "")
#define PRED_NAME(base, blen) make_name("", base, blen, "?", NULL, 0, "")
#define GET_NAME(base, blen, field, flen) make_name("", base, blen, "-", field, flen, "")
#define SET_NAME(base, blen, field, flen) make_name("set-", base, blen, "-", field, flen, "!")

Scheme_Object **scheme_make_struct_values(Scheme_Object *type,
					  Scheme_Object **names,
					  int count,
					  int flags)
{
  Scheme_Struct_Type *struct_type;
  Scheme_Object **values;
  int slot_num, pos;

  struct_type = (Scheme_Struct_Type *)type;

  values = (Scheme_Object **)scheme_malloc_stubborn(count * sizeof(Scheme_Object *));
 
#ifdef MEMORY_COUNTING_ON
  if (scheme_starting_up) {
    /* We know that these values will be kept (exns, arity-at-least, etc.). */
    scheme_misc_count += count * sizeof(Scheme_Object *);
  }
#endif

  pos = 0;
  if (!(flags & SCHEME_STRUCT_NO_TYPE))
    values[pos++] = (Scheme_Object *)struct_type;
  if (!(flags & SCHEME_STRUCT_NO_CONSTR))
    values[pos++] = make_struct_proc(struct_type,
				     names[pos],
				     SCHEME_CONSTR, 
				     struct_type->num_slots);
  if (!(flags & SCHEME_STRUCT_NO_PRED))
    values[pos++] = make_struct_proc(struct_type,
				     names[pos],
				     SCHEME_PRED,
				     0);

  slot_num = (struct_type->name_pos
	      ? struct_type->parent_types[struct_type->name_pos - 1]->num_slots 
	      : 0);
  while (pos < count) {
    if (!(flags & SCHEME_STRUCT_NO_GET))
      values[pos++] = make_struct_proc(struct_type,
				       names[pos],
				       SCHEME_GETTER,
				       slot_num);
    
    if (!(flags & SCHEME_STRUCT_NO_SET))
      values[pos++] = make_struct_proc(struct_type,
				       names[pos],
				       SCHEME_SETTER,
				       slot_num);
    slot_num++;
  }
  
  scheme_end_stubborn_change((void *)values);
 
  return values;
}

static Scheme_Object **_make_struct_names(const char *base, int blen,
					  int fcount,
					  Scheme_Object *field_symbols,
					  const char **field_strings,
					  int flags, int *count_out)
{
  Scheme_Object **names;
  const char *field_name;
  int count, fnlen;
  int slot_num, pos;

  count = 0;

  if (!(flags & SCHEME_STRUCT_NO_TYPE))
    count++;
  if (!(flags & SCHEME_STRUCT_NO_CONSTR))
    count++;
  if (!(flags & SCHEME_STRUCT_NO_PRED))
    count++;
  if (!(flags & SCHEME_STRUCT_NO_GET))
    count += fcount;
  if (!(flags & SCHEME_STRUCT_NO_SET))
    count += fcount;

  names = (Scheme_Object **)scheme_malloc_stubborn(count * sizeof(Scheme_Object *));

#ifdef MEMORY_COUNTING_ON
  if (scheme_starting_up) {
    /* We know that these names will be kept (exns, arity-at-least, etc.). */
    scheme_misc_count += count * sizeof(Scheme_Object *);
  }
#endif

  pos = 0;

  if (!(flags & SCHEME_STRUCT_NO_TYPE))
    names[pos++] = TYPE_NAME(base, blen);
  if (!(flags & SCHEME_STRUCT_NO_CONSTR))
    names[pos++] = CSTR_NAME(base, blen);
  if (!(flags & SCHEME_STRUCT_NO_PRED))
    names[pos++] = PRED_NAME(base, blen);

  if (fcount) {
    for (slot_num = 0; slot_num < fcount; slot_num++) {
      if (field_symbols) {
	Scheme_Object *fn = SCHEME_CAR(field_symbols);
	field_symbols = SCHEME_CDR(field_symbols);

	field_name = SCHEME_SYM_VAL(fn);
	fnlen = SCHEME_SYM_LEN(fn);
      } else {
	field_name = field_strings[slot_num];
	fnlen = strlen(field_name);
      }

      if (!(flags & SCHEME_STRUCT_NO_GET))
	names[pos++] = GET_NAME(base, blen, field_name, fnlen);
      if (!(flags & SCHEME_STRUCT_NO_SET))
	names[pos++] = SET_NAME(base, blen, field_name, fnlen);
    }
  }

  scheme_end_stubborn_change((void *)names);

  if (count_out)
    *count_out = count;

  return names;
}

Scheme_Object **scheme_make_struct_names(Scheme_Object *base, 
					 Scheme_Object *field_symbols,
					 int flags, int *count_out)
{
  return _make_struct_names(SCHEME_SYM_VAL(base),
			    SCHEME_SYM_LEN(base),
			    field_symbols ? scheme_list_length(field_symbols) : 0,
			    field_symbols, NULL,
			    flags, count_out);
}

Scheme_Object **scheme_make_struct_names_from_array(const char *base, 
						    int fcount,
						    const char **fields,
						    int flags, int *count_out)
{
  return _make_struct_names(base,
			    strlen(base),
			    fcount,
			    NULL, fields,
			    flags, count_out);
}

static Scheme_Object *_make_struct_type(const char *base, int blen,
					Scheme_Object *parent,
					int num_fields)
{
  Scheme_Struct_Type *struct_type, *parent_type;
  int j, depth;
  
  parent_type = (Scheme_Struct_Type *)parent;

  depth = parent_type ? (1 + parent_type->name_pos) : 0;

  struct_type =(Scheme_Struct_Type *)scheme_malloc_tagged(sizeof(Scheme_Struct_Type)
							  + (depth 
							     * sizeof(Scheme_Struct_Type *)));
  
  struct_type->type = scheme_struct_type_type;
  struct_type->name_pos = depth;
  struct_type->parent_types[depth] = struct_type;
  for (j = depth; j--; )
    struct_type->parent_types[j] = parent_type->parent_types[j];

  struct_type->type_name = TYPE_NAME(base, blen);
  struct_type->num_slots = num_fields + (parent_type ? parent_type->num_slots : 0);

  return (Scheme_Object *)struct_type;
}

Scheme_Object *scheme_make_struct_type(Scheme_Object *base,
				       Scheme_Object *parent,
				       int num_fields)
{
  return _make_struct_type(SCHEME_SYM_VAL(base),
			   SCHEME_SYM_LEN(base),
			   parent, num_fields);
}

Scheme_Object *scheme_make_struct_type_from_string(const char *base,
						   Scheme_Object *parent,
						   int num_fields)
{
  return _make_struct_type(base, strlen(base), parent, num_fields);
}

static Scheme_Object *
struct_execute (Scheme_Object *form)
{
  Struct_Info *info;
  Scheme_Object **values, **names, *parent;
  Scheme_Object *type;

  info = (Struct_Info *)form;

  parent = (SCHEME_NULLP(info->parent_type_expr)
	    ? NULL 
	    : _scheme_eval_compiled_expr(info->parent_type_expr));

  if (parent && !SAME_TYPE(SCHEME_TYPE(parent),
			   scheme_struct_type_type))
    scheme_raise_exn(MZEXN_STRUCT_STRUCT_TYPE, parent,
		     "struct: supertype expression returned "
		     "a value that is not a struct type value");
  
  type = scheme_make_struct_type(info->name, 
				 parent, 
				 info->num_fields);

  if (!info->memo_names)
    info->memo_names = scheme_make_struct_names(info->name,
						info->fields,
						0, 
						&info->count);
  names = info->memo_names;

  values = scheme_make_struct_values(type, names, info->count, 0);

  return scheme_values(info->count, values);
}

static Scheme_Object *
struct_link(Scheme_Object *expr, Link_Info *info)
{
  Struct_Info *sinfo;

  sinfo = (Struct_Info *)expr;

  if (sinfo->parent_type_expr)
    sinfo->parent_type_expr = scheme_link_expr(sinfo->parent_type_expr, info);

  return scheme_make_syntax_link(struct_execute, expr);
}

static Scheme_Object *
do_struct_syntax (Scheme_Object *forms, Scheme_Comp_Env *env, 
		  Scheme_Compile_Info *in_rec, int depth)
{
  Struct_Info *info;
  Scheme_Object *base_symbol, *field_symbols, *l, *form, *parent_expr;
  int count;

  form = SCHEME_CDR(forms);
  if (!SCHEME_PAIRP(form))
    scheme_wrong_syntax("struct", form, forms, NULL);
  base_symbol = SCHEME_CAR (form);
  form = SCHEME_CDR(form);
  if (!SCHEME_PAIRP(form))
    scheme_wrong_syntax("struct", form, forms, NULL);
  field_symbols = SCHEME_CAR (form);
  form = SCHEME_CDR(form);
  if (!SCHEME_NULLP(form))
    scheme_wrong_syntax("struct", form, forms, NULL);

  if (SCHEME_PAIRP(base_symbol)) {
    parent_expr = SCHEME_CDR(base_symbol);
    base_symbol = SCHEME_CAR(base_symbol);
    if (!SCHEME_PAIRP(parent_expr) || !SCHEME_NULLP(SCHEME_CDR(parent_expr))) {
      scheme_wrong_syntax("struct", parent_expr, forms, "improper name-parent expression");
      return NULL;
    }
    if (in_rec)
      parent_expr = scheme_compile_expr(SCHEME_CAR(parent_expr), env, in_rec);
    else
      parent_expr = scheme_expand_expr(SCHEME_CAR(parent_expr), env, depth);
  } else {
    parent_expr = NULL;

    if (in_rec)
      in_rec->max_let_depth = 0;
  }

  if (!SCHEME_SYMBOLP(base_symbol))
    scheme_wrong_syntax("struct", base_symbol, form, "struct name must be an identifier");
  
  if (in_rec) {
    info = (Struct_Info *)scheme_malloc_tagged(sizeof(Struct_Info));
    info->type = scheme_struct_info_type;
    
    info->name = base_symbol;
    info->fields = field_symbols;
    info->parent_type_expr = parent_expr ? parent_expr : scheme_null;
  } else
    info = NULL;

  count = 0;
  l = field_symbols;
  while (!SCHEME_NULLP(l)) {
    count++;
    if (!SCHEME_PAIRP(l))
      scheme_wrong_syntax("struct", l, form, "badly formed field list");
    if (!SCHEME_SYMBOLP(SCHEME_CAR(l)))
      scheme_wrong_syntax("struct", SCHEME_CAR(l), 
			  form, "field name must be an identifier");
    l = SCHEME_CDR(l);
  }

  if (in_rec) {
    info->num_fields = count;

    if (in_rec->can_optimize_constants)
      info->memo_names = scheme_make_struct_names(info->name,
						  info->fields,
						  0, 
						  &info->count);
    else
      info->memo_names = NULL;

    return scheme_make_syntax_compile(struct_link, (Scheme_Object *)info);
  } else
    return cons(struct_symbol,
		cons(parent_expr 
		     ? cons(base_symbol,
			    cons(parent_expr, scheme_null))
		     : base_symbol,
		     cons(field_symbols, scheme_null)));
}

static Scheme_Object *
struct_syntax (Scheme_Object *form, Scheme_Comp_Env *env, 
		   Scheme_Compile_Info *rec)
{
  return do_struct_syntax(form, env, rec, 0);
}

static Scheme_Object *
struct_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth)
{
  return do_struct_syntax(form, env, NULL, depth);
}

static Scheme_Object *
make_struct_proc(Scheme_Struct_Type *struct_type, 
		 Scheme_Object *func_name, 
		 Scheme_ProcT proc_type, int field_num)
{
  Scheme_Object *p;
  short flags = SCHEME_PRIM_IS_STRUCT_PROC;

  if (proc_type == SCHEME_CONSTR) {
    p = scheme_make_folding_closed_prim((Scheme_Closed_Prim *)scheme_make_struct_instance,
					(void *)struct_type,
					scheme_symbol_name(func_name),
					struct_type->num_slots,
					struct_type->num_slots,
					0);
    flags |= SCHEME_PRIM_IS_STRUCT_CONSTR;
  } else if (proc_type == SCHEME_PRED) {
    p = scheme_make_folding_closed_prim((Scheme_Closed_Prim *)struct_pred,
					(void *)struct_type,
					scheme_symbol_name(func_name),
					1, 1, 1);
    flags |= SCHEME_PRIM_IS_STRUCT_PRED;
  } else {
    Struct_Proc_Info *i;

    i = (Struct_Proc_Info *)scheme_malloc(sizeof(Struct_Proc_Info));
    i->struct_type = struct_type;
    i->func_name = func_name;
    i->field = field_num;

    if (proc_type == SCHEME_GETTER) {
      p = scheme_make_folding_closed_prim((Scheme_Closed_Prim *)struct_getter,
					  (void *)i,
					  scheme_symbol_name(func_name),
					  1, 1, 1);
      flags |= SCHEME_PRIM_IS_STRUCT_GETTER;
    } else {
      p = scheme_make_folding_closed_prim((Scheme_Closed_Prim *)struct_setter,
					  (void *)i,
					  scheme_symbol_name(func_name),
					  2, 2, 0);
      flags |= SCHEME_PRIM_IS_STRUCT_SETTER;
    }
  }

  ((Scheme_Closed_Primitive_Proc *)p)->flags |= flags;

  return p;
}

static Scheme_Object *make_name(const char *pre, const char *tn, int ltn,
				const char *post1, const char *fn, int lfn,
				const char *post2)
{
  int total, lp, lp1, lp2;
  char *name, buffer[256];

  total = lp = strlen(pre);
  total += ltn;
  total += (lp1 = strlen(post1));
  total += lfn;
  total += (lp2 = strlen(post2));

  if (total < 256)
    name = buffer;
  else
    name = (char *)scheme_malloc_atomic(sizeof(char)*(total + 1));
  
  memcpy(name, pre, lp);
  total = lp;
  memcpy(name + total, tn, ltn);
  total += ltn;
  memcpy(name + total, post1, lp1);
  total += lp1;
  memcpy(name + total, fn, lfn);
  total += lfn;
  memcpy(name + total, post2, lp2);
  total += lp2;

  name[total] = 0;

  return scheme_intern_symbol(name);
}

#ifdef USE_STRUCT_CASE  
static Scheme_Object *do_struct_case(void *d, int argc, Scheme_Object **argv)
{
  Scheme_Object *s = argv[0];
  Struct_Case *c = (Struct_Case *)d;
  Scheme_Struct_Type *t;
  int level, i, *levels;

  if (!SCHEME_STRUCTP(s)) {
    scheme_wrong_type("#<struct-case>", "struct", 0, argc, argv);
    return NULL;
  }
  
  t = SCHEME_STRUCT_TYPE(s);
  level = t->name_pos;

  levels = c->levels;
  i = c->num_levels; 
  while (i--) {
    if (levels[i] <= level) {
      do {
	Scheme_Struct_Type *st = t->parent_types[levels[i]];
	Scheme_Object *o;

	o = (Scheme_Object *)scheme_lookup_in_table(c->tables[i], 
						    (const char *)st);
	if (o) {
	  Scheme_Object **v;
	  Scheme_Process *p = scheme_current_process;

	  v = scheme_tail_apply_buffer_wp(1, p);
	  v[0] = argv[0];

	  return _scheme_tail_apply_no_copy_wp(o, 1, v, p);
	}
      } while (i--);

      break;
    }
  }

  if (c->elsep) {
    Scheme_Object **v;
    Scheme_Process *p = scheme_current_process;
    
    v = scheme_tail_apply_buffer_wp(1, p);
    v[0] = argv[0];
    
    return _scheme_tail_apply_no_copy_wp(c->elsep, 1, v, p);
  }
  
  scheme_raise_else("#<struct-case>", argv[0]);

  return NULL;
}

#define PREDICATE_TYPE(p) ((Scheme_Struct_Type *)((Scheme_Closed_Primitive_Proc *)p)->data)

static Scheme_Object *make_struct_case(int argc, Scheme_Object **argv)
{
  Scheme_Object *types, *procs, *l, *conflict_base = NULL, *conflict_sub = NULL;
  Struct_Case *c;
  Scheme_Hash_Table **tables;
  int i, count, *levels, num_levels;
  
  types = argv[0];
  procs = argv[1];

  num_levels = 0;
  levels = NULL;
  tables = NULL;

  for (count = 0, l = types; SCHEME_PAIRP(l); l = SCHEME_CDR(l), count++) {
    Scheme_Object *o = SCHEME_CAR(l);
    if (!STRUCT_PROCP(o, SCHEME_PRIM_IS_STRUCT_PRED))
      break;
  }

  if (!SCHEME_NULLP(l)) {
    scheme_wrong_type("make-struct-case", "list of struct predicate procedures",
		      0, argc, argv);
    return NULL;
  }


  for (i = 0, l = procs; SCHEME_PAIRP(l); l = SCHEME_CDR(l), i++) {
    Scheme_Object *p = SCHEME_CAR(l);
    if (!scheme_check_proc_arity(NULL, 1, -1, 0, &p))
      break;
  }

  if (!SCHEME_NULLP(l)) {
    scheme_wrong_type("make-struct-case", "list of procedures (arity 1)",
		      1, argc, argv);
    return NULL;
  }


  if (argc == 3)
    scheme_check_proc_arity("#<struct-case>", 1, 2, argc, argv);

  if (i != count) {
    scheme_raise_exn(MZEXN_APPLICATION_LIST_SIZES,
		     procs,
		     "make-struct-case: given %d struct-type%s and "
		     "%d procedure%s",
		     count,
		     (count == 1) ? "s" : "",
		     i,
		     (i == 1) ? "s" : "");
    return NULL;
  }


  for (count = 0; 
       SCHEME_PAIRP(types) && SCHEME_PAIRP(procs); 
       types = SCHEME_CDR(types), procs = SCHEME_CDR(procs), count++) {
    Scheme_Object *f = SCHEME_CAR(types), *p = SCHEME_CAR(procs);
    Scheme_Struct_Type *t;
    int level;

    t = PREDICATE_TYPE(f);

    level = t->name_pos;
    for (i = 0; i < num_levels; i++)
      if (levels[i] == level) {
	if (!conflict_base) {
	  if (!scheme_lookup_in_table(tables[i], (const char *)t))
	    scheme_add_to_table(tables[i], (const char *)t, p, 0);
	  else {
	    conflict_base = conflict_sub = (Scheme_Object *)f;
	  }
	}

	break;
      }
    if (i == num_levels) {
      int *naya, j, k, unset = 1;
      Scheme_Hash_Table **tnaya, *newtable;

      naya = (int *)scheme_malloc_atomic(sizeof(int) * (num_levels + 1));
      tnaya = (Scheme_Hash_Table **)scheme_malloc((num_levels + 1) * 
						  sizeof(Scheme_Hash_Table *));

      newtable = scheme_hash_table(7, SCHEME_hash_ptr, 0, 0);
      scheme_add_to_table(newtable, (const char *)t, p, 0);

      for (j = k = 0; j < num_levels; j++, k++) {
	if (unset) {
	  if (level < levels[j]) {
	    tnaya[k] = newtable;
	    naya[k++] = level;
	    unset = 0;
	  } else if (!conflict_base) {
	    Scheme_Struct_Type *st;
	    st = t->parent_types[levels[j]];
	    if (scheme_lookup_in_table(tables[j], (const char *)st)) {
	      conflict_base = (Scheme_Object *)st;
	      conflict_sub = (Scheme_Object *)f;
	    }
	  }
	}
	tnaya[k] = tables[j];
	naya[k] = levels[j];
      }
      if (unset) {
	naya[k] = level;
	tnaya[k] = newtable;
      }

      levels = naya;
      tables = tnaya;
      num_levels++;
    }
  }

  if (conflict_base) {
    if (SAME_OBJ(conflict_base, conflict_sub)) {
      scheme_raise_exn(MZEXN_APPLICATION_STRUCT_TYPE_CONFLICT,
		       conflict_sub,
		       conflict_base,
		       "make-struct-case: predicate for %s specified twice",
		       scheme_symbol_name(PREDICATE_TYPE(conflict_sub)->type_name));
    } else {
      /* Need to map conflict_base type back to predicate: */
      for (l = argv[0]; 1; l = SCHEME_CDR(l)) {
	Scheme_Object *p = SCHEME_CAR(l);
	if (SAME_OBJ((Scheme_Object *)PREDICATE_TYPE(p), conflict_base)) {
	  conflict_base = p;
	  break;
	}
      }

      scheme_raise_exn(MZEXN_APPLICATION_STRUCT_TYPE_CONFLICT,
		       conflict_sub,
		       conflict_base,
		       "make-struct-case: the predicate for %s entails the predicate for %s "
		       " that was specified later",
		       scheme_symbol_name(PREDICATE_TYPE(conflict_base)->type_name),
		       scheme_symbol_name(PREDICATE_TYPE(conflict_sub)->type_name));
    }

    return NULL;
  }

  c = MALLOC_ONE(Struct_Case);

  c->num_levels = num_levels;
  c->levels = levels;
  c->tables = tables;
  c->elsep = ((argc == 3) ? argv[2] : NULL);

  return scheme_make_closed_prim_w_arity(do_struct_case,
					 c, "struct-case",
					 1, 1);
}
#endif

/************************************************************************/

#define cons scheme_make_pair

static Scheme_Object *write_struct_info(Scheme_Object *obj)
{
  Struct_Info *info;

  info = (Struct_Info *)obj;

  return cons(scheme_make_integer(info->count),
	      cons(scheme_make_integer(info->num_fields),
		   cons(info->name, 
			cons(info->parent_type_expr,
			     info->fields))));
}

#define X_SCHEME_ASSERT(x, y)

static Scheme_Object *read_struct_info(Scheme_Object *obj)
{
  Scheme_Object *v, *first = scheme_null, *last = NULL;
  Struct_Info *info;

#define BAD_CS "bad compiled structure info"

  info = (Struct_Info *)scheme_malloc_stubborn_tagged(sizeof(Struct_Info));
  info->type = scheme_struct_info_type;

  X_SCHEME_ASSERT(SCHEME_PAIRP(obj), BAD_CS);
  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  X_SCHEME_ASSERT(SCHEME_INTP(v), BAD_CS);
  info->count = SCHEME_INT_VAL(v);

  X_SCHEME_ASSERT(SCHEME_PAIRP(obj), BAD_CS);
  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  X_SCHEME_ASSERT(SCHEME_INTP(v), BAD_CS);
  info->num_fields = SCHEME_INT_VAL(v);

  X_SCHEME_ASSERT(SCHEME_PAIRP(obj), BAD_CS);
  info->name = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  
  X_SCHEME_ASSERT(SCHEME_PAIRP(obj), BAD_CS);
  info->parent_type_expr = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  
  X_SCHEME_ASSERT(SCHEME_LISTP(obj), BAD_CS);

  /* must copy obj */
  while (SCHEME_PAIRP(obj)) {
    Scheme_Object *pair = scheme_make_pair(SCHEME_CAR(obj), scheme_null);

    if (last)
      SCHEME_CDR(last) = pair;
    else
      first = pair;
    last = pair;

    obj = SCHEME_CDR(obj);
  }

  info->fields = first;

  info->memo_names = NULL;
  
  scheme_end_stubborn_change((void *)info);

  return (Scheme_Object *)info;
}

#ifdef MEMORY_COUNTING_ON
void scheme_count_struct_info(Scheme_Object *o, long *s, long *e, 
			      Scheme_Hash_Table *ht)
{
  Struct_Info *info = (Struct_Info *)o;

  *s = sizeof(Struct_Info);
  *e = scheme_count_memory(info->name, ht)
     + scheme_count_memory(info->parent_type_expr, ht)
     + scheme_count_memory(info->fields, ht);
}
#endif
