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
#include "schrunst.h"
#include "schmach.h"
#include "schminc.h"

#ifndef NO_UNIT_SYSTEM

#define KIND "unit"

#define MAKE_UNIT "unit"
#define MAKE_COMPOUND_UNIT "compound-unit"
#define INVOKE_UNIT "invoke-unit"

static Scheme_Object *CloseUnit(Scheme_Object *data);
static Scheme_Object *CloseCompoundUnit(Scheme_Object *data);
static Scheme_Object *InvokeUnit(Scheme_Object *data);

static Scheme_Object *do_unit(Scheme_Object **boxes, Scheme_Object **anchors, 
			      Scheme_Unit *m, void *dr);
static Scheme_Object *do_compound_unit(Scheme_Object **boxes, Scheme_Object **anchors, 
				       Scheme_Unit *m, void *dr);

static Scheme_Object *import_symbol, *export_symbol, *with_symbol;
static Scheme_Object *define_values_symbol;

#define cons scheme_make_pair

#define scheme_bind scheme_add_good_binding
#define scheme_bind_anchored scheme_add_binding_with_anchor

static Scheme_Object *unit_symbol;
static Scheme_Object *compound_unit_symbol;
static Scheme_Object *invoke_unit_symbol;

static Scheme_Object *unitsig_macros;

typedef Scheme_Object *(*Init_Func)(Scheme_Object **boxes, Scheme_Object **anchors,
				    struct Scheme_Unit *m,
				    void *debug_request);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

/**********************************************************************/
/* Parsing tools                                                      */
/**********************************************************************/

typedef struct UnitId {
  MZTAG_IF_REQUIRED
  Scheme_Object *tag;
  Scheme_Object *int_id;
  Scheme_Object *ext_id;
  struct UnitId *next;
} UnitId;

typedef struct UnitIds {
  UnitId *first, *last;
} UnitIds;

enum {
  mm_body_def,
  mm_body_seq
};

typedef struct BodyVar {
  MZTAG_IF_REQUIRED
  Scheme_Object *id;
  short pos;
  short exported;
} BodyVar;

typedef struct BodyExpr {
  MZTAG_IF_REQUIRED
  short btype;
  union {
    struct {
      short count;
      Scheme_Object *expr;
      BodyVar *vars;
    } def;
    struct {
      Scheme_Object *expr;
    } seq;
  } u;
  struct BodyExpr *next;
} BodyExpr;

typedef struct BodyExprs {
  BodyExpr *first, *last;
} BodyExprs;

static void init_ids(UnitIds *ids)
{
  ids->first = ids->last = NULL;
}

static void extend_ids(UnitIds *ids, Scheme_Object *tag, 
		       Scheme_Object *int_id, Scheme_Object *ext_id)
{
  UnitId *naya;

  naya = MALLOC_ONE_RT(UnitId);
#ifdef MZTAG_REQUIRED
  naya->type = scheme_rt_unit_id;
#endif

  if (!ids->last)
    ids->first = naya;
  else
    ids->last->next = naya;
  ids->last = naya;

  naya->tag = tag;
  naya->int_id = int_id;
  naya->ext_id = ext_id;
  naya->next = NULL;
}

static int count_ids(UnitId *id)
{
  int c;

  for (c = 0; id; id = id->next) {
    c++;
  }

  return c;
}

/**********************************************************************/
/* Basic syntax checks and parsing                                    */
/**********************************************************************/

static void check_ext_ids_unique(DupCheckRecord *r, UnitIds *m, 
				 char *where, Scheme_Object *form)
{
  UnitId *id;
  
  r->scheck_size = 0;
  r = scheme_begin_dup_symbol_check(r);
  for (id = m->first; id; id = id->next) {
    scheme_dup_symbol_check(r, where, id->ext_id, "external", form, 0);
  }
}

static void check_int_ids_unique(UnitIds *m, 
				 char *where, Scheme_Object *form,
				 DupCheckRecord *r,
				 int continuing,
				 int inverted,
				 char *kind)
{
  UnitId *id;

  if (!kind)
    kind = "internal";
 
  if (!r || !continuing)
    r = scheme_begin_dup_symbol_check(r);
  for (id = m->first; id; id = id->next) {
    scheme_dup_symbol_check(r, where, id->int_id, kind, form, inverted);
  }
}

static void check_tags_unique(UnitIds *m, 
			      char *where, Scheme_Object *form,
			      DupCheckRecord *r,
			      int null_id_required,
			      int inverted,
			      int cont_check)
{
  UnitId *id;
 
  if (!r || !cont_check)
    r = scheme_begin_dup_symbol_check(r);

  for (id = m->first; id; id = id->next) {
    if (null_id_required) {
      if (null_id_required < 0) {
	if (!id->int_id)
	  continue;
      } else {
	if (id->int_id)
	  continue;
      }
    }
    if (id->tag)
      scheme_dup_symbol_check(r, where, id->tag, "tag", form, inverted);
  }
}

static void check_id_list(char *where,
			  Scheme_Object *l, Scheme_Object *form, 
			  Scheme_Comp_Env *env,
			  UnitIds *m, Scheme_Object *tag,
			  int rename_ok)
{
  while (SCHEME_PAIRP(l)) {
    Scheme_Object *id = SCHEME_CAR(l);

    if (rename_ok && !SCHEME_SYMBOLP(id)) {
      if (!SCHEME_PAIRP(id)
	  || !SCHEME_PAIRP(SCHEME_CDR(id))
	  || !SCHEME_NULLP(SCHEME_CDDR(id))
	  || !SCHEME_SYMBOLP(SCHEME_CADR(id)))
	scheme_wrong_syntax(where, 
			    id, form,
			    "bad syntax (external id must be an identifier)");
      scheme_check_identifier(where, SCHEME_CAR(id), NULL, env, form);
      scheme_check_identifier(where, SCHEME_CADR(id), NULL, env, form);
      extend_ids(m, tag, SCHEME_CAR(id), SCHEME_CADR(id));
    } else {
      scheme_check_identifier(where, id, NULL, env, form);
      extend_ids(m, tag, id, id);
    }

    l = SCHEME_CDR(l);
  }

  if (!SCHEME_NULLP(l))
    scheme_wrong_syntax(where, l, form, NULL);
}

static void check_tagged(char *where,
			 Scheme_Object *l, Scheme_Object *form, 
			 Scheme_Comp_Env *env,
			 int check_id, 
			 UnitIds *m,
			 int rename_ok,
			 int name_only_ok,
			 int must_have_ids,
			 Scheme_Object *self_check)
{
  while (SCHEME_PAIRP(l)) {
    Scheme_Object *tag_set = SCHEME_CAR(l);
    Scheme_Object *tag;

    if (SCHEME_SYMBOLP(tag_set) && name_only_ok) {
      extend_ids(m, 
		 (name_only_ok < 0) ? NULL : tag_set, 
		 (name_only_ok < 0) ? tag_set : NULL, 
		 NULL);
    } else {
      Scheme_Object *rest;

      if (!SCHEME_PAIRP(tag_set))
	scheme_wrong_syntax(where, tag_set, form,
			    "bad syntax (not a tagged set)");
      if (!SCHEME_SYMBOLP(SCHEME_CAR(tag_set)))
	scheme_wrong_syntax(where, tag_set, form,
			    "bad syntax (tag is not an identifier)");
      
      tag = SCHEME_CAR(tag_set);
      rest = SCHEME_CDR(tag_set);
      
      if (SCHEME_NULLP(rest) && must_have_ids)
	scheme_wrong_syntax(MAKE_COMPOUND_UNIT, 
			    tag_set, form,
			    "bad syntax (no identifiers provided for %s)",
			    must_have_ids < 0 ? "exporting" : "linking");
      
#ifdef NO_SELF_IMPORT
      if (SAME_OBJ(tag, self_check)) {
	scheme_wrong_syntax(MAKE_COMPOUND_UNIT, 
			    tag, form,
			    "bad syntax (self-import)");
      }
#endif

      if (check_id)
	check_id_list(where, rest, form, env, m, tag, rename_ok);
      else {
	if (!SCHEME_PAIRP(rest)
	    || !SCHEME_NULLP(SCHEME_CDR(rest))
	    || !SCHEME_PAIRP(SCHEME_CAR(rest)))
	  scheme_wrong_syntax(where, 
			      tag_set, form, 
			      "bad syntax (tag must be followed by a single "
			      KIND " application)");
	else {
	  Scheme_Object *app = SCHEME_CAR(rest), *expr, *links;
	  UnitIds ids;
	  
	  init_ids(&ids);
	  
	  expr = SCHEME_CAR(app);
	  links = SCHEME_CDR(app);
	  
	  check_tagged(where, links, form, env, 1, &ids, 0, -1, 0, tag);

	  extend_ids(m, tag, expr, (Scheme_Object *)ids.first);
	}
      }
    }

    l = SCHEME_CDR(l);
  }

  if (!SCHEME_NULLP(l))
    scheme_wrong_syntax(where, l, form, NULL);
}

static Scheme_Object *check_params(char *where, Scheme_Object *clause, 
				   Scheme_Object *form,
				   Scheme_Comp_Env *env,
				   DupCheckRecord *r,
				   int keyword_check, int cont_check)
{
  Scheme_Object *l;
  
  if (keyword_check) {
    if (!SCHEME_PAIRP(clause)
	|| !SAME_OBJ(SCHEME_CAR(clause), import_symbol))
      scheme_wrong_syntax(where, 
			  clause, form,
			  "expected `import' keyword");
    clause = SCHEME_CDR(clause);
  }
   
  l = clause;
  
  if (!r || !cont_check)
    r = scheme_begin_dup_symbol_check(r);
  
  while (SCHEME_PAIRP(l)) {
    Scheme_Object *s = SCHEME_CAR(l);
    if (!SCHEME_SYMBOLP(s))
      scheme_wrong_syntax(where, s, form, 
			  "bad syntax (import not an identifier)");
    else if (env)
      scheme_check_identifier(where, s, NULL, env, form);
    
    scheme_dup_symbol_check(r, where, s, "import", form, 0);
    
    l = SCHEME_CDR(l);
  }
  
  if (!SCHEME_NULLP(l)) {
    scheme_wrong_syntax(where, NULL, form, 
			"bad syntax (" IMPROPER_LIST_FORM " for imports)");
  }

  return clause;
}

static Scheme_Object *unit_varlist(BodyExpr *e, 
				   Scheme_Object *base, 
				   int exported)
{
  Scheme_Object *first = scheme_null, *last = NULL;

  if (!SCHEME_NULLP(base)) {
    first = base;
    last = base;
    while (!SCHEME_NULLP(SCHEME_CDR(last))) {
      last = SCHEME_CDR(last);
    }
  }

  for (; e; e = e->next) {
    switch (e->btype) {
    case mm_body_def:
      {
	BodyVar *vs;
	int i, c;

	c = e->u.def.count;
	vs = e->u.def.vars;
	for (i = 0; i < c; i++) {
	  if (vs[i].exported == exported) {
	    Scheme_Object *name;
	    
	    name = vs[i].id;
	    name = cons(name, scheme_null);
	    if (last)
	      SCHEME_CDR(last) = name;
	    else
	      first = name;
	    last = name;
	  }
	}
      }
      break;
    }
  }

  return first;
}

static int check_unit(Scheme_Object *form, Scheme_Comp_Env *env,
		      UnitIds *exports,
		      int *num_params,
		      BodyExprs *body,
		      Scheme_Compile_Info *rec, int drec,
		      int depth)
{
  Scheme_Comp_Env *indirect_env;
  DupCheckRecord dcrec;
  Scheme_Object *l = SCHEME_CDR(form), *export, *params, *bodystack;
  Scheme_Object *indirect_first = scheme_null, *indirect_last = NULL;
  UnitId *id;
  BodyExpr *e;
  int c, count, import_count, expr_count = 0;
  int dpos, ipos;
  Scheme_Compile_Info *recs;
  Scheme_Comp_Env *expand_env;
  
  expand_env = env;

  dcrec.scheck_size = 0;

  init_ids(exports);

  body->first = NULL;
  body->last = NULL;

  c = scheme_proper_list_length(l);
  if (c < 0)
    scheme_wrong_syntax(MAKE_UNIT, NULL, form, "bad syntax (" IMPROPER_LIST_FORM ")");
  else if (c < 2)
    scheme_wrong_syntax(MAKE_UNIT, 
			NULL, form, 
			"bad syntax (requires at least 3 parts; had %d parts)",
			c);
  
  params = SCHEME_CAR(l);
  l = SCHEME_CDR(l);
  export = SCHEME_CAR(l);
  l = SCHEME_CDR(l);

  params = check_params(MAKE_UNIT, params, form, env, &dcrec, 1, 0);

  import_count = scheme_list_length(params);
  *num_params = import_count;

  expand_env = scheme_add_compilation_frame(params, expand_env, 0);

  if (!SCHEME_PAIRP(export)
      || NOT_SAME_OBJ(SCHEME_CAR(export), export_symbol))
    scheme_wrong_syntax(MAKE_UNIT, 
			export, form,
			"expected `export' keyword");
  check_id_list(MAKE_UNIT, SCHEME_CDR(export), form, env, exports, NULL, 1);

  check_ext_ids_unique(&dcrec, exports, MAKE_UNIT, form);

  /* Check for aliases: */
  check_int_ids_unique(exports, MAKE_UNIT, form, &dcrec, 0, 0, "exported internal");
  check_params(MAKE_UNIT, params, form, NULL, &dcrec, 0, 1);

  {
    /* Make list of internal names for exported variables, then extend the
       expansion environment. */
    Scheme_Object *el = scheme_null;
    for (id = exports->first; id; id = id->next) {
      el = cons(id->int_id, el);
    }
    expand_env = scheme_add_compilation_frame(el, expand_env, 0);
  }

  bodystack = scheme_null; /* Used to flatten begins */
  while (SCHEME_PAIRP(l)) {
    Scheme_Object *expr = SCHEME_CAR(l);
    
    e = MALLOC_ONE_RT(BodyExpr);
#ifdef MZTAG_REQUIRED
    e->type = scheme_rt_body_expr;
#endif
    expr_count++;
    
    if (!body->last)
      body->first = e;
    else
      body->last->next = e;
    body->last = e;
    e->next = NULL;

    if (SCHEME_PAIRP(expr)) {
      Scheme_Object *gval;

      /* Check for macro expansion, which could mask the real define-values */
      expr = scheme_check_immediate_macro(expr, expand_env, rec, drec, depth, &gval);

      if (SAME_OBJ(gval, scheme_begin_syntax)) {
	Scheme_Object *cl;

	/* Flatten it out. */
	bodystack = cons(SCHEME_CDR(l), bodystack);
	l = expr;

	/* Check for good begin syntax: */
	for (cl = l; SCHEME_PAIRP(cl); cl = SCHEME_CDR(cl)) {}
	if (!SCHEME_NULLP(cl)) {
	  scheme_wrong_syntax(MAKE_UNIT, 
			      expr, form,
			      "bad syntax (bad immediate `begin' form)");
	  return 0;
	}

	/* A poor hack: set expr to NULL now, filter it out in a second pass. */
	e->btype = mm_body_seq;
	e->u.seq.expr = NULL;
      } else if (SAME_OBJ(gval, scheme_define_values_syntax)) {
	Scheme_Object *ll = SCHEME_CDR(expr);
	int ok;
	
	if (!SCHEME_PAIRP(ll)
	    || !SCHEME_PAIRP(SCHEME_CDR(ll))
	    || !SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(ll))))
	  ok = 0;
	else {
	  Scheme_Object *names = SCHEME_CAR(ll), *ns;
	  Scheme_Object *value = SCHEME_CADR(ll);
	  int count = 0, i;

	  ns = names;
	  while (SCHEME_PAIRP(ns)) {
	    count++;
	    ns = SCHEME_CDR(ns);
	  }
	  
	  if (SCHEME_NULLP(ns)) {
	    BodyVar *vs;
	    
	    e->btype = mm_body_def;
	    e->u.def.expr = value;
	    e->u.def.count = count;

	    vs = MALLOC_N_RT(BodyVar, count);
	    e->u.def.vars = vs;

	    for (i = 0; i < count; i++, names = SCHEME_CDR(names)) {
#ifdef MZTAG_REQUIRED
	      vs[i].type = scheme_rt_body_var;
#endif
	      vs[i].id = SCHEME_CAR(names);
	      scheme_check_identifier(MAKE_UNIT, vs[i].id, NULL, env, form);
	      vs[i].exported = 0;
	    }
	    ok = 1;
	  } else
	    ok = 0;
	}
	
	if (!ok)
	  scheme_wrong_syntax(MAKE_UNIT, 
			      expr, form,
			      "bad syntax (bad `define' form)");
      } else {
	e->btype = mm_body_seq;
	e->u.seq.expr = expr;
      }
    } else {
      e->btype = mm_body_seq;
      e->u.seq.expr = expr;
    }

    l = SCHEME_CDR(l);

    if (SCHEME_NULLP(l) && SCHEME_PAIRP(bodystack)) {
      l = SCHEME_CAR(bodystack);
      bodystack = SCHEME_CDR(bodystack);
    }
  }

  /* Get rid of NULL exprs (introduced by begin expansion: */
  {
    BodyExpr *prev = NULL, *next;
    e = body->first;
    while (e) {
      next = e->next;
      if ((e->btype == mm_body_seq) && !e->u.seq.expr) {
	if (!prev)
	  body->first = e->next;
	else
	  prev->next = e->next;
	if (!next)
	  body->last = prev;
	--expr_count;
      } else
	prev = e;
      e = next;
    }
  }

  /* Make sure all exported ids are defined and set `exported' flag,
     set position info, and build list of exported names: */
  l = params;
  while (SCHEME_PAIRP(l)) {
    Scheme_Object *v;
    v = cons(SCHEME_CAR(l), scheme_null);
    if (indirect_last)
      SCHEME_CDR(indirect_last) = v;
    else
      indirect_first = v;
    indirect_last = v;
    l = SCHEME_CDR(l);
  }
  ipos = import_count;
  for (id = exports->first; id; id = id->next) {
    for (e = body->first; e; e = e->next) {
      Scheme_Object *name;

      if (e->btype == mm_body_def) {
	int i, c = e->u.def.count;
	BodyVar *vs = e->u.def.vars;
	
	for (i = 0 ; i < c; i++) {
	  name = vs[i].id;
	  if (SAME_OBJ(id->int_id, name)) {
	    Scheme_Object *v;
	    v = cons(name, scheme_null);
	    if (indirect_last)
	      SCHEME_CDR(indirect_last) = v;
	    else
	      indirect_first = v;
	    indirect_last = v;
	    
	    vs[i].exported = 1;
	    vs[i].pos = ipos++;
	    break;
	  }
	}

	if (i < c)
	  break;
      }
    }
    
    if (!e)
      scheme_wrong_syntax(MAKE_UNIT, 
			  NULL, form,
			  "cannot export undefined id `%S'",
			  id->int_id);
  }

  /* Check that imported ids are not redefined: */
  check_params(MAKE_UNIT, params, form, NULL, &dcrec, 0, 0);
  for (e = body->first; e; e = e->next) {
    if (e->btype == mm_body_def) {
      int i, c = e->u.def.count;
      BodyVar *vs = e->u.def.vars;

      for (i = 0; i < c; i++) {
	scheme_dup_symbol_check(&dcrec, MAKE_UNIT, vs[i].id, "internal", form, 0);
      }
    }
  }
  
  /* Extend environment: */
  env = scheme_add_compilation_frame(indirect_first, env, 
				     SCHEME_AUTO_UNBOX
#ifndef MZ_PRECISE_GC 
				     | SCHEME_ANCHORED_FRAME
#endif
				     | SCHEME_PRIM_GLOBALS_ONLY);
  indirect_env = env;

  for (c = 0; c < import_count; c++) {
    scheme_unsettable_variable(env, c);
  }

  l = unit_varlist(body->first, scheme_null, 0);
  count = scheme_list_length(l);
  env = scheme_add_compilation_frame(l, env, 
				     SCHEME_AUTO_UNBOX
#ifndef MZ_PRECISE_GC 
				     | SCHEME_ANCHORED_FRAME
#endif
				     );

  if (rec) {
    recs = MALLOC_N_RT(Scheme_Compile_Info, expr_count);
    scheme_init_compile_recs(rec, drec, recs, expr_count);
    expr_count = 0;
  } else
    recs = NULL;

  /* Expand/compile expressions and set defin positions */
  dpos = 0;
  for (e = body->first; e; e = e->next) {
    Scheme_Object *expr;

    if (e->btype == mm_body_def) {
      int i, c = e->u.def.count;
      BodyVar *vs = e->u.def.vars;

      for (i = 0; i < c; i++) {
	if (!vs[i].exported)
	  vs[i].pos = dpos++;
      }

      expr = e->u.def.expr;
    } else
      expr = e->u.seq.expr;
      
    if (!rec)
      expr = scheme_expand_expr(expr, env, depth);
    else {
      if ((e->btype == mm_body_def)
	  && e->u.def.count == 1) {
	Scheme_Object *s = e->u.def.vars[0].id;

	/* Look for export renaming */
	if (e->u.def.vars[0].exported) {
	  UnitId *uid;
	  for (uid = exports->first; uid; uid = uid->next) {
	    if (SAME_OBJ(uid->int_id, s)) {
	      s = uid->ext_id;
	      break;
	    }
	  }
	}
	
	recs[expr_count].value_name = s;
      }
      expr = scheme_compile_expr(expr, env, recs, expr_count);
      expr_count++;
    }

    if (e->btype == mm_body_def)
      e->u.def.expr = expr;
    else
      e->u.seq.expr = expr;
  }

  if (rec) {
    scheme_merge_compile_recs(rec, drec, recs, expr_count);
    rec[drec].max_let_depth += (indirect_env->num_bindings + env->num_bindings);
  }

  return count;
}

static int check_compound_unit(Scheme_Object *form, Scheme_Comp_Env *env,
				 UnitIds *withs, 
				 UnitIds *exports,
				 int *num_params,
				 Scheme_Compile_Info *rec, int drec,
				 int depth)
{
  DupCheckRecord dcrec;
  Scheme_Object *l = SCHEME_CDR(form), *with, *export, *params;
  int c, num_withs = 0;
  UnitId *id;
  Scheme_Compile_Info *recs;

  dcrec.scheck_size = 0;

  init_ids(withs);
  init_ids(exports);

  c = scheme_proper_list_length(l);
  if (c < 0)
    scheme_wrong_syntax(MAKE_COMPOUND_UNIT, NULL, form,
			"bad syntax (" IMPROPER_LIST_FORM ")");
  else if (c != 3)
    scheme_wrong_syntax(MAKE_COMPOUND_UNIT, NULL, form,
			"bad syntax (requires 3 parts)");
  
  params = SCHEME_CAR(l);
  l = SCHEME_CDR(l);
  with = SCHEME_CAR(l);
  l = SCHEME_CDR(l);
  export = SCHEME_CAR(l);

  params = check_params(MAKE_COMPOUND_UNIT, params, form, env, &dcrec, 1, 0);
  c = scheme_list_length(params);
  *num_params = c;

  if (!SCHEME_PAIRP(with)
      || NOT_SAME_OBJ(SCHEME_CAR(with), with_symbol))
    scheme_wrong_syntax(MAKE_COMPOUND_UNIT, 
			with, form, 
			"expected `link' keyword");
  check_tagged(MAKE_COMPOUND_UNIT, SCHEME_CDR(with), form, env, 0, withs, 0, 0, 0, NULL);

  if (!SCHEME_PAIRP(export)
      || NOT_SAME_OBJ(SCHEME_CAR(export), export_symbol))
    scheme_wrong_syntax(MAKE_COMPOUND_UNIT, 
			export, form,
			"expected `export' keyword");
  check_tagged(MAKE_COMPOUND_UNIT, SCHEME_CDR(export), form, env, 1, exports, 1, 0, 0, NULL);

  check_tags_unique(withs, MAKE_COMPOUND_UNIT, form, &dcrec, 0, 0, 0);
  /* Check that export tags were defined by withs: */
  check_tags_unique(exports, MAKE_COMPOUND_UNIT, form, &dcrec, 0, 1, 1);

#if 0
  /* Check that standalone tags used just once (and not also un-standalone): */
  check_tags_unique(exports, MAKE_COMPOUND_UNIT, form, &dcrec, 1, 0, 0);
  check_tags_unique(exports, MAKE_COMPOUND_UNIT, form, &dcrec, -1, 0, 1);
#endif

  /* Check uses of tags in links: */
  check_tags_unique(withs, MAKE_COMPOUND_UNIT, form, &dcrec, 0, 0, 0);
  for (id = withs->first; id; id = id->next) {
    UnitIds tmp;
    tmp.first = (UnitId *)id->ext_id;

    check_tags_unique(&tmp, MAKE_COMPOUND_UNIT, form, &dcrec, 0, 1, 1);
  }

  /* Lookup use of imported in withs */
  for (id = withs->first; id; id = id->next) {
    UnitId *sub = (UnitId *)id->ext_id;

    num_withs++;

    for (; sub; sub = sub->next) {
      if (!sub->tag) {
	Scheme_Object *ll = params;
	int c;
	
	for (c = 0; SCHEME_PAIRP(ll); ll = SCHEME_CDR(ll), c++) {
	  if (SAME_OBJ(SCHEME_CAR(ll), sub->int_id)) {
	    sub->int_id = (Scheme_Object *)(long)c;
	    break;
	  }
	}

	if (SCHEME_NULLP(ll))
	  scheme_wrong_syntax(MAKE_COMPOUND_UNIT, 
			      sub->int_id, form,
			      "bad syntax (not an imported identifier)");
	
      }
#ifdef NO_SELF_IMPORT
      else if (SAME_OBJ(sub->tag, id->tag))
	scheme_wrong_syntax(MAKE_COMPOUND_UNIT, 
			    sub->tag, form,
			    "bad syntax (self-import)");
#endif
    }
  }

  check_ext_ids_unique(&dcrec, exports, MAKE_COMPOUND_UNIT, form);

  if (rec) {
    recs = MALLOC_N_RT(Scheme_Compile_Info, num_withs);
    scheme_init_compile_recs(rec, drec, recs, num_withs);
  } else
    recs = NULL;

  /* Compile/expand expressions of `link' clause: */
  for (c = 0, id = withs->first; id; id = id->next, c++) {
    if (!rec) {
      Scheme_Object *ee;
      ee = scheme_expand_expr(id->int_id, env, depth);
      id->int_id = ee;
    } else {
      Scheme_Object *ce;
      ce = scheme_compile_expr(id->int_id, env, recs, c);
      id->int_id = ce;
    }
  }

  if (rec)
    scheme_merge_compile_recs(rec, drec, recs, num_withs);

  return c;
}

static int check_invoke_unit(char *where, Scheme_Object *form, Scheme_Comp_Env *env)
{
  Scheme_Object *l;
  int count = 0;

  if (scheme_proper_list_length(form) < 2)
    scheme_wrong_syntax(where, NULL, form,
			"bad syntax (missing parts or " IMPROPER_LIST_FORM ")");
  
  l = SCHEME_CDR(SCHEME_CDR(form));

  while (SCHEME_PAIRP(l)) {
    Scheme_Object *first = SCHEME_CAR(l);

    l = SCHEME_CDR(l);
    
    scheme_check_identifier(where, first, "", env, form);
    
    count++;
  }
  
  return count;
}

/**********************************************************************/
/* Compilers and Expanders                                            */
/**********************************************************************/

/******* unit ********/

typedef struct BodyData {
  Scheme_Type type; /* scheme_unit_body_data_type  */
  short max_let_depth;
  BodyExpr *body;
  int num_locals;
  short closure_size;
  short *closure_map;
  Scheme_Object *defname;
} BodyData;

static Scheme_Unit *InitCompiledUnitRec(UnitId *exports, int num_imports,
					int debuggable)
{
  int i;
  UnitId *id;
  Scheme_Unit *m;

  m = MALLOC_ONE_TAGGED(Scheme_Unit);

  m->type = scheme_compiled_unit_type;

  m->num_imports = num_imports;
  for (i = 0, id = exports; id; id = id->next) {
    i++;
  }
  m->num_exports = i;

  if (m->num_exports) {
    Scheme_Object **sa;
    sa = MALLOC_N(Scheme_Object *, m->num_exports);
    m->exports = sa;
  } else
    m->exports = NULL;

  m->export_debug_names = NULL;

  for (i = 0, id = exports; id; id = id->next, i++) {
    m->exports[i] = id->ext_id;
  }
  
  return m;
}

static Scheme_Object *link_unit(Scheme_Object *o, Link_Info *info)
{
  Scheme_Unit *m = (Scheme_Unit *)o;
  BodyData *label = (BodyData *)m->data;
  BodyExpr *e;
  int c, i, j;

  i = label->closure_size;
  if (i) {
    info = scheme_link_info_extend(info, 0, 0, i);

    while (i--) {
      int pos = label->closure_map[i], flags, li;
      li = scheme_link_info_lookup(info, pos, &flags);
      label->closure_map[i] = li;
      scheme_link_info_add_mapping(info, pos, i, flags);
    }
  }

  for (j = 0; j < 2; j++) {
    if (!j)
      c = m->num_imports + m->num_exports;
    else
      c = label->num_locals;

    info = scheme_link_info_extend(info, 2 * c, c, c);
    scheme_link_info_set_anchor_offset(info, c);
    for (i = 0; i < c; i++) {
      scheme_link_info_add_mapping(info, i, i, 
				   SCHEME_INFO_BOXED 
#ifndef MZ_PRECISE_GC 
				   | SCHEME_INFO_ANCHORED
#endif
				   );
    }
  }

  for (e = label->body; e; e = e->next) {
    switch (e->btype) {
    case mm_body_def:
      {
	Scheme_Object *le;
	le = scheme_link_expr(e->u.def.expr, info);
	e->u.def.expr = le;
      }
      break;
    case mm_body_seq:
      {
	Scheme_Object *le;
	le = scheme_link_expr(e->u.seq.expr, info);
	e->u.seq.expr = le;
      }
      break;
    }
  }

  return scheme_make_syntax_link(CloseUnit, (Scheme_Object *)m);
}

static Scheme_Object *make_unit_syntax(Scheme_Object *form,
				       Scheme_Comp_Env *env,
				       Scheme_Compile_Info *rec, int drec)
{
  UnitIds exports;
  BodyExprs body;
  BodyData *label;
  Scheme_Unit *m;
  Scheme_Comp_Env *cenv;
  int count, num_params;
  Scheme_Compile_Info lam;
  Scheme_Object *defname;
  short lcs, *lcm;

  defname = rec[drec].value_name;
  scheme_compile_rec_done_local(rec, drec);

  cenv = scheme_new_compilation_frame(0, SCHEME_LAMBDA_FRAME, env);

  scheme_init_lambda_rec(rec, drec, &lam, 0);

  count = check_unit(form, cenv, &exports, &num_params, &body, &lam, 0, 0);

  scheme_merge_lambda_rec(rec, drec, &lam, 0);

  scheme_default_compile_rec(rec, drec);

  label = MALLOC_ONE_TAGGED(BodyData);

  label->type = scheme_unit_body_data_type;
  label->body = body.first;
  label->num_locals = count;
  label->max_let_depth = lam.max_let_depth;
  label->defname = defname;

  scheme_env_make_closure_map(cenv, &lcs, &lcm);
  label->closure_size = lcs;
  label->closure_map = lcm;

  m = InitCompiledUnitRec(exports.first, num_params, 1);

  m->data = (Scheme_Object *)label;

  return scheme_make_syntax_compile(link_unit, (Scheme_Object *)m);
}

static Scheme_Object *make_unit_expand(Scheme_Object *form,
					 Scheme_Comp_Env *env,
					 int depth)
{
  int num_params;
  UnitIds exports;
  BodyExprs body;
  BodyExpr *e;
  Scheme_Object *first = scheme_null, *last = NULL;

  (void)check_unit(form,  env, &exports, &num_params, &body, NULL, 0, depth);

  /* Rebuild the expression: */

  for (e = body.first; e; e = e->next) {
    Scheme_Object *c = NULL;

    switch (e->btype) {
    case mm_body_def:
      {
	int i, count = e->u.def.count;
	BodyVar *vs = e->u.def.vars;
	Scheme_Object *ids = scheme_null, *last = NULL;
	
	for (i = 0; i < count; i++) {
	  Scheme_Object *pr;
	  pr = cons(vs[i].id, scheme_null);
	  if (last)
	    SCHEME_CDR(last) = pr;
	  else
	    ids = pr;
	  last = pr;
	}

	c = cons(define_values_symbol,
		 cons(ids,
		      cons(e->u.def.expr, scheme_null)));
	break;
      }
    case mm_body_seq:
      c = e->u.seq.expr;
      break;
    }

    if (c) {
      c = cons(c, scheme_null);
      if (last)
	SCHEME_CDR(last) = c;
      else
	first = c;
      last = c;
    }
  }

  return cons(unit_symbol,
	      cons(SCHEME_CADR(form),
		   cons(SCHEME_CADR(SCHEME_CDR(form)),
			first)));
}

/******* compound-unit ********/

typedef struct {
  MZTAG_IF_REQUIRED
  int submod_index;
  Scheme_Object *ext_id;
} ExportSource;

typedef struct {
  MZTAG_IF_REQUIRED
  short index; /* -1 => compound param */
  union {
    short pos;
    Scheme_Object *ext_id;
  } u;
} ParamMap;

typedef struct CompoundData {
  Scheme_Type type; /* scheme_unit_compound_data_type  */
  short num_exports;
  ExportSource *exports;
  Scheme_Object **subunit_exprs;
  Scheme_Object **tags;
  int num_subunits;
  short *param_counts;
  ParamMap **param_maps;
  Scheme_Object *defname;
} CompoundData;

static Scheme_Object *
make_compound_unit_record(int count, /* subunits */
			  /* tag; int_id is exp; ext_id is id list (tag checked, int_id): */
			  UnitId *links,
			  /* tag, int_id  */
			  UnitId *exports,
			  /* symbols */
			  Scheme_Object *imports,
			  int num_imports,
			  Scheme_Object *form,
			  Scheme_Object *defname,
			  int promise_ok)
{
  CompoundData *data;
  Scheme_Unit *m;
  Scheme_Object **tags;
  ParamMap **param_maps;
  UnitId *id;
  int i, j;
  ExportSource *export_srcs;

  data = MALLOC_ONE_TAGGED(CompoundData);

  data->type = scheme_unit_compound_data_type;
  data->num_subunits = count;
  {
    Scheme_Object **sa;
    sa = MALLOC_N(Scheme_Object *, count);
    data->subunit_exprs = sa;
  }
  param_maps = MALLOC_N(ParamMap *, count);
  data->param_maps = param_maps;
  {
    short *sa;
    sa = MALLOC_N_ATOMIC(short, count);
    data->param_counts = sa;
  }
  data->defname = defname;
  
  tags = MALLOC_N(Scheme_Object *, count);
  data->tags = tags;

  for (i = 0, id = links; id; id = id->next, i++) {
    data->subunit_exprs[i] = id->int_id;
    tags[i] = id->tag;

    j = count_ids((UnitId *)id->ext_id);
    data->param_counts[i] = j;
    {
      ParamMap *pms;
      pms = MALLOC_N_RT(ParamMap, j);
      param_maps[i] = pms;
#ifdef MZTAG_REQUIRED
      {
	int k;
	for (k = 0; k < j; k++) {
	  pms[k].type = scheme_rt_param_map;
	}
      }
#endif
    }
  }

  /* Fill in param map: */
  for (i = 0, id = links; id; id = id->next, i++) {
    UnitId *sub = (UnitId *)id->ext_id;
    for (j = 0; sub; sub = sub->next, j++) {
      if (sub->tag) {
	/* Map to another unit's export */
	int k;
	if (SCHEME_INTP(sub->tag))
	  k = SCHEME_INT_VAL(sub->tag);
	else {
	  for (k = 0; k < count; k++) {
	    if (SAME_OBJ(sub->tag, tags[k]))
	      break;
	  }
	}

	param_maps[i][j].index = k;
	param_maps[i][j].u.ext_id = sub->int_id;
      } else {
	/* Map to imports */
	long pos;
	param_maps[i][j].index = -1;
	if (imports) {
	  Scheme_Object *l, *n;
	  n = sub->int_id;
	  if (SCHEME_INTP(n)) 
	    pos = SCHEME_INT_VAL(n);
	  else {
	    pos = 0;
	    for (l = imports; !SAME_OBJ(n, SCHEME_CAR(l)); l = SCHEME_CDR(l)) {
	      pos++;
	    }
	  }
	} else
	  pos = (long)sub->int_id;
	param_maps[i][j].u.pos = (short)pos;
      }
    }
  }  

  /* Fill in export map: */
  i = count_ids(exports);
  data->num_exports = i;
  {
    ExportSource *esa;
    esa = MALLOC_N_RT(ExportSource, i);
    data->exports = esa;
  }
  export_srcs = data->exports;
  for (i = 0, id = exports; id; id = id->next, i++) {
    Scheme_Object *tag = id->tag;
    if (SCHEME_INTP(tag)) 
      j = SCHEME_INT_VAL(tag);
    else {
      for (j = data->num_subunits; j--; ) {
	if (SAME_OBJ(tag, tags[j]))
	  break;
      }
    }
#ifdef MZTAG_REQUIRED
    export_srcs[i].type = scheme_rt_export_source;
#endif
    export_srcs[i].submod_index = j;
    export_srcs[i].ext_id = id->int_id;
  }

  /* Check that no sub-unit variable is exported twice: */
  if (!promise_ok) {
    for (i = data->num_exports; i--; ) {
      Scheme_Object *eid = export_srcs[i].ext_id;
      int which = export_srcs[i].submod_index, j;
      for (j = i; j--; ) {
	if ((export_srcs[j].submod_index == which)
	    && SAME_OBJ(export_srcs[j].ext_id, eid)) {
	  scheme_wrong_syntax(MAKE_COMPOUND_UNIT, NULL, form,
			      "`%S' of sub-unit tagged `%S' is exported twice",
			      eid,
			      tags[which]);
	}
      }
    }
  }

  m = InitCompiledUnitRec(exports, num_imports, 1);

  m->data = (Scheme_Object *)data;

  return (Scheme_Object *)m;
}

static Scheme_Object *link_compound_unit(Scheme_Object *o, Link_Info *info)
{
  Scheme_Unit *m = (Scheme_Unit *)o;
  CompoundData *data = (CompoundData *)m->data;
  int i;

  for (i = 0; i < data->num_subunits; i++) {
    Scheme_Object *le;
    le = scheme_link_expr(data->subunit_exprs[i], info);
    data->subunit_exprs[i] = le;
  }
  
  return scheme_make_syntax_link(CloseCompoundUnit, (Scheme_Object *)m);
}

static Scheme_Object *make_compound_unit_syntax(Scheme_Object *form,
						Scheme_Comp_Env *env,
						Scheme_Compile_Info *rec,
						int drec)
{
  UnitIds withs, exports;
  int num_imports, count;
  Scheme_Object *m, *defname;

  defname = rec[drec].value_name;
  scheme_compile_rec_done_local(rec, drec);

  count = check_compound_unit(form, env, &withs, &exports, &num_imports,
			      rec, drec, 0);
  
  m = make_compound_unit_record(count, withs.first, exports.first,
				NULL, num_imports, form, defname, 0);

  return scheme_make_syntax_compile(link_compound_unit, m);
}

Scheme_Object *scheme_assemble_compound_unit(Scheme_Object *imports,
					     Scheme_Object *links,
					     Scheme_Object *exports)
{
  UnitId *last, *link_ids, *export_ids;
  Scheme_Object *m;
  int count, num_imports;

  /* printf("ready assemble compound: %ld\n", scheme_get_process_milliseconds()); */

  count = scheme_list_length(links);
  if (SCHEME_INTP(imports))
    num_imports = SCHEME_INT_VAL(imports);
  else
    num_imports = scheme_list_length(imports);

  last = NULL;
  link_ids = NULL;
  for (m = links; SCHEME_PAIRP(m); m = SCHEME_CDR(m)) {
    UnitId *id, *ilast, *first;
    Scheme_Object *l = SCHEME_CAR(m);

    id = MALLOC_ONE_RT(UnitId);
#ifdef MZTAG_REQUIRED
    id->type = scheme_rt_unit_id;
#endif

    id->tag = SCHEME_CAR(l);
    id->int_id = NULL;
    first = NULL;
    ilast = NULL;
    for (l = SCHEME_CDR(l); SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
      UnitId *uid;
      Scheme_Object *i = SCHEME_CAR(l);

      uid = MALLOC_ONE_RT(UnitId);

#ifdef MZTAG_REQUIRED
      uid->type = scheme_rt_unit_id;
#endif

      if (ilast)
	ilast->next = uid;
      else
	first = uid;
      ilast = uid;

      if (!SCHEME_PAIRP(i)) {
	uid->tag = NULL;
	uid->int_id = i;
      } else {
	uid->tag = SCHEME_CAR(i);
	uid->int_id = SCHEME_CDR(i);
      }
    }
    id->ext_id = (Scheme_Object *)first;

    if (last)
      last->next = id;
    else
      link_ids = id;
    last = id;
  }

  last = NULL;
  export_ids = NULL;
  for (m = exports; SCHEME_PAIRP(m); m = SCHEME_CDR(m)) {
    UnitId *id;
    Scheme_Object *l = SCHEME_CAR(m);

    id = MALLOC_ONE_RT(UnitId);

#ifdef MZTAG_REQUIRED
    id->type = scheme_rt_unit_id;
#endif

    id->tag = SCHEME_CAR(l);
    l = SCHEME_CDR(l);
    if (SCHEME_SYMBOLP(l)) {
      id->int_id = l;
      id->ext_id = l;
    } else {
      id->int_id = SCHEME_CAR(l);
      id->ext_id = SCHEME_CDR(l);
    }
    
    if (last)
      last->next = id;
    else
      export_ids = id;
    last = id;
  }

  /* printf("start assemble compound: %ld\n", scheme_get_process_milliseconds()); */

  m = make_compound_unit_record(count, link_ids, export_ids,
				imports, num_imports, NULL, NULL, 
				1);

  /* printf("end assemble compound: %ld\n", scheme_get_process_milliseconds()); */

  return m;
}

static Scheme_Object *make_compound_unit_expand(Scheme_Object *form,
						   Scheme_Comp_Env *env,
						   int depth)
{
  UnitIds withs, exports;
  UnitId *id;
  int count, num_params;
  Scheme_Object *first = scheme_null, *last = NULL, *l;

  count = check_compound_unit(form, env, &withs, &exports, &num_params,
				NULL, 0, depth);

  l = SCHEME_CDR(SCHEME_CAR(SCHEME_CDR(SCHEME_CDR(form))));
  for (id = withs.first; id; id = id->next) {
    Scheme_Object *c;
    c = cons(cons(id->tag,
		  cons(cons(id->int_id, 
			    SCHEME_CDR(SCHEME_CAR(SCHEME_CDR(SCHEME_CAR(l))))),
		       scheme_null)),
	     scheme_null);

    if (last)
      SCHEME_CDR(last) = c;
    else
      first = c;
    last = c;

    l = SCHEME_CDR(l);
  }

  return cons(compound_unit_symbol,
	      cons(SCHEME_CADR(form),
		   cons(cons(with_symbol, first),
			SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(form))))));
}

/******* invoke-unit ********/

typedef struct {
  Scheme_Type type; /* scheme_invoke_unit_data_type */
  short num_exports, num_local_exports;
  short *anchor_positions; /* one for each local_type export */
  Scheme_Object **exports;
  Scheme_Object **anchors;
  Scheme_Object *expr;
} InvokeUnitData;

static Scheme_Object *link_invoke_unit(Scheme_Object *o, Link_Info *info)
{
  InvokeUnitData *data;
  int i, num_local = 0, j;

  data = (InvokeUnitData *)o;

  for (i = 0; i < data->num_exports; i++) {
    if (SAME_TYPE(SCHEME_TYPE(data->exports[i]), scheme_local_type))
      num_local++;
  }

  data->num_local_exports = num_local;
  if (num_local) {
    short *sa;
    sa = MALLOC_N_ATOMIC(short, num_local);
    data->anchor_positions = sa;
  }

  for (i = 0, j = 0; i < data->num_exports; i++) {
    if (SAME_TYPE(SCHEME_TYPE(data->exports[i]), scheme_local_type)) {
      int flags;
      int p;
      flags = scheme_link_info_flags(info, SCHEME_LOCAL_POS(data->exports[i]));

      if (flags & SCHEME_INFO_ANCHORED)
	p = scheme_link_info_lookup_anchor(info, SCHEME_LOCAL_POS(data->exports[i]));
      else
	p = -1;

      data->anchor_positions[j++] = (short)p;
    }

    {
      Scheme_Object *le;
      le = scheme_link_expr(data->exports[i], info);
      data->exports[i] = le;
    }
  }

  {
    Scheme_Object *le;
    le = scheme_link_expr(data->expr, info);
    data->expr = le;
  }

  return scheme_make_syntax_link(InvokeUnit, (Scheme_Object *)data);
}

static Scheme_Object *do_invoke_unit_syntax(char *where,
					    Scheme_Object *form,
					    Scheme_Comp_Env *env,
					    Scheme_Compile_Info *rec,
					    int drec)
{
  Scheme_Object *l;
  int c, i;
  InvokeUnitData *data;

  c = check_invoke_unit(where, form, env);

  data = MALLOC_ONE_TAGGED(InvokeUnitData);
  data->type = scheme_invoke_unit_data_type;
  data->num_exports = c;

  scheme_compile_rec_done_local(rec, drec);

  {
    Scheme_Object *ce;
    ce = scheme_compile_expr(SCHEME_CADR(form), env, rec, drec);
    data->expr = ce;
  }

  data->anchors = NULL;

  data->num_local_exports = 0;
  data->anchor_positions = NULL;

  {
    Scheme_Object **sa;
    sa = MALLOC_N(Scheme_Object *, c);
    data->exports = sa;
  }
  l = SCHEME_CDR(SCHEME_CDR(form));

  for (i = 0; i < c; i++, l = SCHEME_CDR(l)) {
    Scheme_Object *s, *v;

    s = SCHEME_CAR(l);
    v = scheme_static_distance(s, env, 
			       SCHEME_MUST_INDRECT
			       + SCHEME_GLOB_ALWAYS_REFERENCE
			       + SCHEME_LINKING_REF
			       + (rec[drec].dont_mark_local_use 
				  ? SCHEME_DONT_MARK_USE 
				  : 0));

    if (ENV_PRIM_GLOBALS_ONLY(env)
	&& SAME_TYPE(SCHEME_TYPE(v), scheme_variable_type)) {
      v = scheme_get_primitive_global(v, scheme_min_env(env), 0, 0, 0);

      if (!v)
	scheme_wrong_syntax("invoke", s, form,
			    "cannot access a global from within a unit");
    }
    
    data->exports[i] = v;
  }

  return scheme_make_syntax_compile(link_invoke_unit, (Scheme_Object *)data);
}

static Scheme_Object *do_invoke_unit_expand(char *where,
					    Scheme_Object *formname,
					    Scheme_Object *form,
					    Scheme_Comp_Env *env,
					    int depth)
{
  (void)check_invoke_unit(where, form, env);
  
  return cons(formname,
	      cons(scheme_expand_expr(SCHEME_CADR(form), env, depth),
		   SCHEME_CDR(SCHEME_CDR(form))));
}

static Scheme_Object *invoke_unit_syntax(Scheme_Object *form,
					 Scheme_Comp_Env *env,
					 Scheme_Compile_Info *rec,
					 int drec)
{
  return do_invoke_unit_syntax(INVOKE_UNIT, form, env, rec, drec);
}

static Scheme_Object *invoke_unit_expand(Scheme_Object *form,
					 Scheme_Comp_Env *env,
					 int depth)
{
  return do_invoke_unit_expand(INVOKE_UNIT, invoke_unit_symbol, form, env, depth);
}

/**********************************************************************/
/* Syntax execution                                                   */
/**********************************************************************/

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Object *data;
  Scheme_Env *env;
  Scheme_Object **closure_saved;
  Scheme_Object *defname;
} UnitDataClosure;

static Scheme_Object *CloseUnit(Scheme_Object *data)
{
  Scheme_Unit *orig, *m;

  orig = (Scheme_Unit *)data;

  m = MALLOC_ONE_TAGGED(Scheme_Unit);

  m->type = scheme_unit_type;
  m->num_imports = orig->num_imports;
  m->num_exports = orig->num_exports;
  m->exports = orig->exports;
  
  if (orig->data->type == scheme_unit_body_data_type) {
    UnitDataClosure *cl;
    BodyData *data;
    int i;
    short *map;
    Scheme_Object **stack, **saved;
#ifndef RUNSTACK_IS_GLOBAL
    Scheme_Process *p = scheme_current_process;
#endif

    data = (BodyData *)orig->data;

    cl = MALLOC_ONE_RT(UnitDataClosure);
    
#ifdef MZTAG_REQUIRED
    cl->type = scheme_rt_unit_data_closure;
#endif

    cl->env = scheme_get_env(scheme_config);
    cl->data = (Scheme_Object *)data;
    cl->defname = data->defname;

    i = data->closure_size;
    saved = MALLOC_N(Scheme_Object *, i);
    cl->closure_saved = saved;
    map = data->closure_map;
    stack = MZ_RUNSTACK;
    while (i--) {
      saved[i] = stack[map[i]];
    }
    
    m->data = (Scheme_Object *)cl;

    m->init_func = do_unit;
  } else {
    m->data = orig->data;
    m->init_func = do_compound_unit;
  }

  return (Scheme_Object *)m;
}

static void wrong_import(char *where,
			 Scheme_Object *unit,
			 Scheme_Object *sunit,
			 Scheme_Object *tag,
			 Scheme_Object *stag,
			 Scheme_Object *id)
{
  scheme_raise_exn(MZEXN_UNIT,
		   "%s: import at tag `%S' failed from tag `%S', "
		   "name `%S'",
		   where,
		   tag,
		   stag,
		   id);
}

typedef struct {
  short source; /* -1 => create; -2 => input */
  short pos;
} BoxMap;

typedef struct {
  MZTAG_IF_REQUIRED
  int num_submods;
  Scheme_Unit **subunits;
  BoxMap **boxMapsList;
  Scheme_Object **tags; /* for debugging */
  Scheme_Object *defname;
} CompoundLinkedData;

/* HERE */

static int find_exported(int unit_index,
			 Scheme_Object *id,
			 
			 int num_mods,
			 Scheme_Unit **subunits,
			 Scheme_Object **tags,
			 BoxMap **boxesList,
			 
			 int insert, short *i_pos)
{
  int k, n;
  Scheme_Object **exports;
  Scheme_Unit *sm;

  /* Look for exported id */

  k = unit_index;
   
  sm = subunits[k];
  exports = sm->exports;

  for (n = sm->num_exports; n--; ) {
    if (SAME_OBJ(exports[n], id)) {
      /* Found exported id in tagged unit: */
      if (insert) {
	BoxMap *map = &boxesList[k][n + sm->num_imports];
	map->source = -2;
	map->pos = *i_pos;
	*i_pos = n;
      } else
	*i_pos = n + sm->num_imports;
      return 1;
    }
  }

  return 0;
}

static Scheme_Object *
do_close_compound_unit(Scheme_Object *data_in, Scheme_Object **subs_in)
{
  Scheme_Unit **subunits, *sm, *m;
  BoxMap **boxesList, *boxes;
  CompoundData *data;
  CompoundLinkedData *linked;
  int num_mods, i, j, k;

  /* printf("begin link compound: %ld\n", scheme_get_process_milliseconds()); */

  m = (Scheme_Unit *)CloseUnit(data_in);

  data = (CompoundData *)m->data;

  num_mods = data->num_subunits;

  subunits = MALLOC_N(Scheme_Unit *, num_mods);
  boxesList = MALLOC_N(BoxMap *, num_mods);

  /* Evaluate units and create space for sub-exported box lists */
  for (i = 0; i < num_mods; i++) {
    Scheme_Object *v;
    
    if (subs_in)
      v = subs_in[i];
    else
      v = _scheme_eval_compiled_expr(data->subunit_exprs[i]);

    if (!SCHEME_UNITP(v)) {
      scheme_raise_exn(MZEXN_UNIT,
		       MAKE_COMPOUND_UNIT ": not a " KIND " for `%S' "
		       "in compound unit; provided %V",
		       data->tags[i],
		       v);
      return NULL;
    }
    
    subunits[i] = sm = (Scheme_Unit *)v;

    if (sm->num_imports != data->param_counts[i]) {
      scheme_raise_exn(MZEXN_UNIT,
		       MAKE_COMPOUND_UNIT ": " KIND " tagged `%S' "
		       "expects %d imports, given %d",
		       data->tags[i],
		       sm->num_imports,
		       data->param_counts[i]);
    }

    boxes = MALLOC_N_ATOMIC(BoxMap, (sm->num_imports + sm->num_exports));
    boxesList[i] = boxes;
    for (j = sm->num_imports; j--;) {
      boxes[j].source = 0;
      boxes[j].pos = 0;
    }

    k = sm->num_imports;
    for (j = sm->num_exports; j--;) {
      boxes[j + k].source = -1;
      boxes[j + k].pos = 0;
    }
  }

  /* Resolve sub-exported with locally exported */
  for (i = m->num_exports; i--; ) {
    short pos = m->num_imports + i;
    if (!find_exported(data->exports[i].submod_index, data->exports[i].ext_id,
		       num_mods, subunits, data->tags, boxesList,
		       1, &pos)) {
      scheme_raise_exn(MZEXN_UNIT,
		       MAKE_COMPOUND_UNIT ": compound " KIND " exported variable "
		       "not found for tag `%S', name `%S'",
		       data->tags[data->exports[i].submod_index],
		       data->exports[i].ext_id);
      return NULL;
    }
  }

  /* Link imports: */
  for (i = 0; i < num_mods; i++) {
    ParamMap *map;

    map = data->param_maps[i];
    for (j = subunits[i]->num_imports; j--; ) {
      if (map[j].index < 0) {
	boxesList[i][j].source = -2;
	boxesList[i][j].pos = map[j].u.pos;
      } else {
	Scheme_Object *orig_id = map[j].u.ext_id;
	short pos, source = map[j].index;

	if (!find_exported(source, orig_id,
			   num_mods, subunits, data->tags, boxesList,
			   0, &pos)) {
	  wrong_import(MAKE_COMPOUND_UNIT,
		       (Scheme_Object *)subunits[i], 
		       (Scheme_Object *)subunits[source], 
		       data->tags[i], 
		       data->tags[source], 
		       orig_id);
	  return NULL;
	}
      
	boxesList[i][j].source = source;
	boxesList[i][j].pos = pos;
      }
    }
  }

  /* All mappings are set up */
  linked = MALLOC_ONE_RT(CompoundLinkedData);
#ifdef MZTAG_REQUIRED
  linked->type = scheme_rt_compound_linked_data;
#endif
  linked->num_submods = num_mods;
  linked->subunits = subunits;
  linked->boxMapsList = boxesList;
  linked->tags = data->tags;
  linked->defname = data->defname;

  m->data = (Scheme_Object *)linked;

  /* printf("done link compound: %ld\n", scheme_get_process_milliseconds()); */

  return (Scheme_Object *)m;
}

static Scheme_Object *CloseCompoundUnit(Scheme_Object *data_in)
{
  return do_close_compound_unit(data_in, NULL);
}

Scheme_Object *
scheme_make_compound_unit(Scheme_Object *data_in, Scheme_Object **subs_in)
{
  return do_close_compound_unit(data_in, subs_in);
}

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Object **boxes;
  Scheme_Object **anchors;
  Scheme_Unit *unit;
} Do_Invoke_Data;

/* Tail-called by InvokeUnit: */
static Scheme_Object *do_invoke_unit(void *data, int argc, Scheme_Object **argv)
{
  Do_Invoke_Data *diu = (Do_Invoke_Data *)data;
  Init_Func f = diu->unit->init_func;

  return f(diu->boxes, diu->anchors, diu->unit, NULL);
}

static Scheme_Object *InvokeUnit(Scheme_Object *data_in)
{
  Scheme_Unit *unit;
  InvokeUnitData *data;
  Scheme_Env *link_env;
  Scheme_Object *v, **boxes, **anchors;
  int c, i, j;
#ifndef RUNSTACK_IS_GLOBAL
  Scheme_Process *p = scheme_current_process;
#endif

  data = (InvokeUnitData *)data_in;

  link_env = scheme_get_env(scheme_config);

  v = _scheme_eval_compiled_expr(data->expr);

  if (!SCHEME_UNITP(v))
    scheme_raise_exn(MZEXN_UNIT,
		     INVOKE_UNIT ": not a " KIND "; provided %V",
		     v);
  
  unit = (Scheme_Unit *)v;

  if (unit->num_imports != data->num_exports) {
    scheme_raise_exn(MZEXN_UNIT,
		     "%s: " KIND " "
		     "expects %d imports, given %d",
		     INVOKE_UNIT,
		     unit->num_imports,
		     data->num_exports);
  }

  c = unit->num_exports + unit->num_imports;

  boxes = MALLOC_N(Scheme_Object *, c);
  anchors = MALLOC_N(Scheme_Object *, c);

  if (data->anchors) {
    for (i = data->num_exports; i--; ) {
      boxes[i] = data->exports[i];
      anchors[i] = data->anchors[i];
    }
  } else {
    int ap = 0;

    for (i = data->num_exports; i--; ) {
      Scheme_Object *e = data->exports[i];
      if (SAME_TYPE(SCHEME_TYPE(e), scheme_variable_type)) {
	if (!((Scheme_Bucket *)e)->val) {
	  Scheme_Object *name = (Scheme_Object *)((Scheme_Bucket *)e)->key;
	  scheme_raise_exn(MZEXN_UNIT,
			   INVOKE_UNIT ": cannot link to undefined identifier: %S",
			   name);
	  return NULL;
	}
	((Scheme_Bucket_With_Const_Flag *)e)->flags |= GLOB_IS_PERMANENT;
#ifdef MZ_PRECISE_GC
	boxes[i] = e;
	anchors[i] = NULL;
#else
	boxes[i] = (Scheme_Object *)&((Scheme_Bucket *)e)->val;
	anchors[i] = e;
#endif
      } else {
	boxes[i] = MZ_RUNSTACK[SCHEME_LOCAL_POS(e)];
	if (data->anchor_positions[ap] > 0)
	  anchors[i] = MZ_RUNSTACK[data->anchor_positions[ap]];
	ap++;
      }
    }
  }

  for (i = 0, j = data->num_exports; j < c; j++, i++) {
    Scheme_Object *eb;
    eb = scheme_make_envunbox(scheme_undefined);
    boxes[j] = eb;
    anchors[j] = NULL;
  }

  /* Invoke the unit: */
  /* We tail-call do_invoke_unit to ensure that the invoked unit
     starts with a minimal stack and reset the continuation marks
     counter: */
  {
    Do_Invoke_Data *diu;
    Scheme_Object *invoke_unit_f;

    diu = MALLOC_ONE_RT(Do_Invoke_Data);

#ifdef MZTAG_REQUIRED
    diu->type = scheme_rt_do_invoke_data;
#endif
    diu->boxes = boxes;
    diu->anchors = anchors;
    diu->unit = unit;

    invoke_unit_f = scheme_make_closed_prim(do_invoke_unit, diu);

    return _scheme_tail_apply(invoke_unit_f, 0, NULL);
  }
}

Scheme_Object *scheme_invoke_unit(Scheme_Object *unit, int num_ins, 
				  Scheme_Object **ins, Scheme_Object **anchors,
				  int tail, int multi)
{
  InvokeUnitData *data;
  Scheme_Object *v;

  data = MALLOC_ONE(InvokeUnitData);

  data->expr = unit;

  data->num_exports = num_ins;
  data->exports = ins;
  data->anchors = anchors;

  v = InvokeUnit((Scheme_Object *)data);

  if (tail)
    return v;
  else {
    v = _scheme_force_value(v);

    if (!multi && SAME_OBJ(v, SCHEME_MULTIPLE_VALUES))
      scheme_wrong_return_arity(NULL, 1, scheme_multiple_count, scheme_multiple_array, NULL);

    return v;
  }
}

/**********************************************************************/
/* Unit execution                                                     */
/**********************************************************************/

static void *do_unit_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *v;
  Scheme_Object **boxes;
  Scheme_Object **anchors;
  Scheme_Unit *m;
  
  boxes = (Scheme_Object **)p->ku.k.p1;
  anchors = (Scheme_Object **)p->ku.k.p2;
  m = (Scheme_Unit *)p->ku.k.p3;
  
  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  v = do_unit(boxes, anchors, m, NULL);

  return scheme_force_value(v);
}

static Scheme_Object *do_unit(Scheme_Object **boxes, Scheme_Object **anchors,
			      Scheme_Unit *m, void *debug_request)
{
  UnitDataClosure *cl;
  BodyExpr *e;
  BodyData *data;
  Scheme_Object **indirect, **direct, **stack;
  int i, c, total;
  Scheme_Object *v, *result_expr = NULL;
  Scheme_Process *p = scheme_current_process;

#ifdef DO_STACK_CHECK  
# include "mzstkchk.h"
  {
    p->ku.k.p1 = (void *)boxes;
    p->ku.k.p2 = (void *)anchors;
    p->ku.k.p3 = (void *)m;

    return scheme_handle_stack_overflow((Scheme_Object *(*)(void))do_unit_k);
  }
#endif
  SCHEME_USE_FUEL(1);


  cl = (UnitDataClosure *)m->data;
  data = (BodyData *)cl->data;

  c = m->num_imports + m->num_exports;
  i = data->closure_size;

  total = (2 * c) + (2 * data->num_locals) + i + data->max_let_depth;
  if (!scheme_check_runstack(total)) {
    p->ku.k.p1 = (void *)boxes;
    p->ku.k.p2 = (void *)anchors;
    p->ku.k.p3 = (void *)m;
    return (Scheme_Object *)scheme_enlarge_runstack(total, do_unit_k);
  }

  stack = PUSH_RUNSTACK(p, MZ_RUNSTACK, i);
  direct = cl->closure_saved;
  while (i--) {
    stack[i] = direct[i];
  }
  
  indirect = PUSH_RUNSTACK(p, MZ_RUNSTACK, (2 * c));
  direct = PUSH_RUNSTACK(p, MZ_RUNSTACK, (2 * data->num_locals));

  for (i = 0; i < c; i++) {
    indirect[i] = boxes[i];
    indirect[i + c] = anchors[i];
  }

  for (e = data->body; e; e = e->next) {
    if (e->btype == mm_body_def) {
      BodyVar *vs = e->u.def.vars;
      for (i = 0; i < e->u.def.count; i++) {
	if (!vs[i].exported) {
	  Scheme_Object *eb;
	  eb = scheme_make_envunbox(scheme_undefined);
	  direct[vs[i].pos] = eb;
	  direct[vs[i].pos + data->num_locals] = NULL; /* anchor */
	}
      }
    }
  }

  for (e = data->body; e; e = e->next) {
    if (result_expr)
      _scheme_eval_compiled_expr_multi(result_expr);

    switch(e->btype) {
    case mm_body_def:
      {
	int i, c;
	BodyVar *vs = e->u.def.vars;
	Scheme_Object **values;

	v = e->u.def.expr;
	v = _scheme_eval_compiled_expr_multi(v);
	if (SAME_OBJ(v, SCHEME_MULTIPLE_VALUES)) {
	  values = scheme_current_process->ku.multiple.array;
	  c = scheme_current_process->ku.multiple.count;
	} else {
	  values = &v;
	  c = 1;
	}

	if (e->u.def.count != c) {
	  int i = e->u.def.count;
	  const char *symname;
	  symname = (i ? scheme_symbol_name(vs[0].id) : "");
	  scheme_raise_exn(MZEXN_APPLICATION_ARITY, 
			   scheme_make_integer(c),
			   scheme_make_integer(i),
			   "define-values (in unit): returned %d value%s "
			   "to a definition context%s%s%s expecting %d value%s",
			   c, (c == 1) ? "" : "s",
			   i ? " (" : "",
			   symname,
			   i ? ((i == 1) ? ")" : ", ...)") : "",
			   i, (i == 1) ? "" : "s"); 

	}

	for (i = 0; i < c; i++) {
	  v = values[i];
	  if (vs[i].exported)
	    SCHEME_ENVBOX_VAL(boxes[vs[i].pos]) = v;
	  else
	    SCHEME_ENVBOX_VAL(direct[vs[i].pos]) = v;
	}
	result_expr = NULL;
      }
      break;
    case mm_body_seq:
      result_expr = e->u.seq.expr;
      break;
    }
  }

  if (result_expr) {
    /* We'd like to have something like tail_eval here. */
    /* return _scheme_eval_compiled_expr_multi(result_expr); */
    return _scheme_tail_eval(result_expr);
  } else
    return scheme_void;
}

#ifdef _MSC_VER
# pragma optimize("", off)
#endif

static Scheme_Object *do_compound_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object **boxes;
  Scheme_Object **anchors;
  Scheme_Unit *m;
  
  boxes = (Scheme_Object **)p->ku.k.p1;
  anchors = (Scheme_Object **)p->ku.k.p2;
  m = (Scheme_Unit *)p->ku.k.p3;
  
  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return do_compound_unit(boxes, anchors, m, NULL);
}

static Scheme_Object *do_compound_unit(Scheme_Object **boxes_in, Scheme_Object **anchors_in,
				       Scheme_Unit *m, void *debug_request)
{
  Scheme_Unit **subunits, *sm;
  BoxMap **boxMapsList, *boxMaps;
  Scheme_Object ***boxesList, **boxes, *v = scheme_void;
  Scheme_Object ***anchorsList, **anchors;
  CompoundLinkedData *data;
  int num_mods, i, j, k;

#ifdef DO_STACK_CHECK  
# include "mzstkchk.h"
  {
    Scheme_Process *p = scheme_current_process;
    p->ku.k.p1 = (void *)boxes_in;
    p->ku.k.p2 = (void *)anchors_in;
    p->ku.k.p3 = (void *)m;

    return scheme_handle_stack_overflow(do_compound_k);
  }
#endif
  SCHEME_USE_FUEL(1);

  data = (CompoundLinkedData *)m->data;

  num_mods = data->num_submods;
  subunits = data->subunits;

  boxesList = MALLOC_N(Scheme_Object **, num_mods);
  anchorsList = MALLOC_N(Scheme_Object **, num_mods);

  boxMapsList = data->boxMapsList;

  /* Create all boxes: */
  for (i = 0; i < num_mods; i++) {
    int c;

    sm = subunits[i];

    c = (sm->num_imports + sm->num_exports);
    boxes = MALLOC_N(Scheme_Object *, c);
    anchors = MALLOC_N(Scheme_Object *, c);
    boxesList[i] = boxes;
    anchorsList[i] = anchors;

    boxMaps = boxMapsList[i];

    k = sm->num_imports;
    for (j = sm->num_exports; j--; ) {
      if (boxMaps[j + k].source == -1) {
	Scheme_Object *box;
	Scheme_Object *anchor;

	box = scheme_make_envunbox(scheme_undefined);
	anchor = NULL;

	boxes[j + k] = box;
	anchors[j + k] = anchor;
      } else if (boxMaps[j + k].source == -2) {
	int p = boxMaps[j + k].pos;
	boxes[j + k] = boxes_in[p];
	anchors[j + k] = anchors_in[p];
      }
    }
  }

  /* Link imports: */
  for (i = 0; i < num_mods; i++) {
    sm = subunits[i];

    boxes = boxesList[i];
    anchors = anchorsList[i];
    boxMaps = boxMapsList[i];

    for (j = sm->num_imports; j--; ) {
      Scheme_Object *box, *anchor;
      
      if (boxMaps[j].source == -2) {
	int p = boxMaps[j].pos;
	box = boxes_in[p];
	anchor = anchors_in[p];
      } else {
	int s = boxMaps[j].source, p = boxMaps[j].pos;
	box = boxesList[s][p];
	anchor = anchorsList[s][p];
      }

      boxes[j] = box;
      anchors[j] = anchor;
    }
  }

  /* All boxes are set up; now initialize units and we're done */
  for (i = 0; i < num_mods; i++) {
    Init_Func f = subunits[i]->init_func;
    v = f(boxesList[i], anchorsList[i], subunits[i], NULL);
    if (i + 1 < num_mods) {
      /* could be a tail-call or a tail-eval */
      v = scheme_force_value(v);
    }
  }

  return v;
}

#ifdef _MSC_VER
# pragma optimize("", on)
#endif

static Scheme_Object *unit_p(int argc, Scheme_Object *argv[])
{
  return SCHEME_UNITP(argv[0]) ? scheme_true : scheme_false;
}

/**********************************************************************/
/* Marshalling                                                        */
/**********************************************************************/

static Scheme_Object *array_to_list(Scheme_Object **a, int c, Scheme_Object *base)
{
  Scheme_Object *l = base;

  while (c--) {
    l = cons(a[c], l);
  }
  
  return l;
}


static Scheme_Object **list_to_array(Scheme_Object *l, int c, Scheme_Object **l_back)
{
  Scheme_Object **a;
  int i;

  if (!c)
    return NULL;

  a = MALLOC_N(Scheme_Object *, c);
  for (i = 0; i < c; i++, l = SCHEME_CDR(l)) {
    a[i] = SCHEME_CAR(l);
  }

  if (l_back)
    *l_back = l;

  return a;
}

static Scheme_Object *write_compiled_unit(Scheme_Object *o)
{
  Scheme_Unit *m;
  
  m = (Scheme_Unit *)o;

  return cons(scheme_make_integer(m->num_imports),
	      cons(scheme_make_integer(m->num_exports),
		   cons(m->data,
			array_to_list(m->exports,
				      m->num_exports,
				      scheme_false))));
}

static Scheme_Object *read_compiled_unit(Scheme_Object *o)
{
  Scheme_Unit *m;
  int c;

  m = MALLOC_ONE_TAGGED(Scheme_Unit);
  m->type = scheme_compiled_unit_type;

  m->num_imports = SCHEME_INT_VAL(SCHEME_CAR(o));
  o = SCHEME_CDR(o);
  c = m->num_exports = SCHEME_INT_VAL(SCHEME_CAR(o));
  o = SCHEME_CDR(o);

  m->data = SCHEME_CAR(o);
  o = SCHEME_CDR(o);

  {
    Scheme_Object **sa;
    sa = list_to_array(o, c, &o);
    m->exports = sa;
  }

  m->export_debug_names = NULL;

  if (SAME_TYPE(SCHEME_TYPE(m->data), scheme_unit_body_data_type))
    m->init_func = do_unit;
  else
    m->init_func = do_compound_unit;

  return (Scheme_Object *)m;
}

static Scheme_Object *write_body_data(Scheme_Object *o)
{
  BodyData *data;
  BodyExpr *body;
  Scheme_Object *l;
  int counter = 0;

  data = (BodyData *)o;

  l = scheme_null;

  for (body = data->body; body; body = body->next) {
    switch (body->btype) {
    case mm_body_def:
      {
	int i, count = body->u.def.count;
	BodyVar *vs = body->u.def.vars;
	
	for (i = count; i--; ) {
	  l = cons(vs[i].id,
		   cons(scheme_make_integer(vs[i].pos),
			cons(vs[i].exported ? scheme_true : scheme_false,
			     l)));
	}

	l = cons(body->u.def.expr,
		 cons(scheme_make_integer(body->u.def.count),
		      l));
      }
      break;
    case mm_body_seq:
      l = cons(body->u.seq.expr, l);
      break;
    }
    
    l = cons(scheme_make_integer(body->btype), l);
    counter++;
  }

  return cons(scheme_make_integer(data->num_locals), 
	      cons(scheme_make_integer(data->max_let_depth),
		   cons(scheme_make_svector(data->closure_size,
					    data->closure_map),
			cons(scheme_make_integer(counter),
			     cons(data->defname
				  ? data->defname
				  : scheme_null,
				  l)))));
}

static Scheme_Object *read_body_data(Scheme_Object *o)
{
  BodyData *data;
  BodyExpr *body;
  Scheme_Object *v;
  int counter;

  data = MALLOC_ONE_TAGGED(BodyData);
  data->type = scheme_unit_body_data_type;
  
  data->num_locals = SCHEME_INT_VAL(SCHEME_CAR(o));
  o = SCHEME_CDR(o);

  data->max_let_depth = SCHEME_INT_VAL(SCHEME_CAR(o));
  o = SCHEME_CDR(o);

  v = SCHEME_CAR(o);
  o = SCHEME_CDR(o);

  data->closure_size = SCHEME_SVEC_LEN(v);
  data->closure_map = SCHEME_SVEC_VEC(v);

  counter = SCHEME_INT_VAL(SCHEME_CAR(o));
  o = SCHEME_CDR(o);

  data->defname = SCHEME_CAR(o);
  if (SCHEME_NULLP(data->defname))
    data->defname = NULL;
  o = SCHEME_CDR(o);

  data->body = NULL;
  while (counter--) {
    body = MALLOC_ONE_RT(BodyExpr);
#ifdef MZTAG_REQUIRED
    body->type = scheme_rt_body_expr;
#endif
    body->next = data->body;
    data->body = body;

    body->btype = SCHEME_INT_VAL(SCHEME_CAR(o));
    o = SCHEME_CDR(o);

    switch (body->btype) {
    case mm_body_def: 
      {
	int i, c;
	BodyVar *vs;
	
	body->u.def.expr = SCHEME_CAR(o);
	o = SCHEME_CDR(o);
	body->u.def.count = SCHEME_INT_VAL(SCHEME_CAR(o));
	o = SCHEME_CDR(o);

	c = body->u.def.count;
	vs = MALLOC_N_RT(BodyVar, c);
	body->u.def.vars = vs;

	for (i = 0; i < c; i++) {
#ifdef MZTAG_REQUIRED
	  vs[i].type = scheme_rt_body_var;
#endif
	  vs[i].id = SCHEME_CAR(o);
	  o = SCHEME_CDR(o);
	  vs[i].pos = SCHEME_INT_VAL(SCHEME_CAR(o));
	  o = SCHEME_CDR(o);
	  vs[i].exported = SCHEME_TRUEP(SCHEME_CAR(o));
	  o = SCHEME_CDR(o);
	}
      }
      break;
    case mm_body_seq:
      body->u.seq.expr = SCHEME_CAR(o);
      o = SCHEME_CDR(o);
      break;
    }
  }

  return (Scheme_Object *)data;
}

static Scheme_Object *write_compound_data(Scheme_Object *o)
{
  CompoundData *data;
  Scheme_Object *exs, *subs, *tags, *counts, *maps, *lmap, *prev;
  ParamMap *map;
  int i, j;

  data = (CompoundData *)o;
  
  exs = scheme_null;
  for (i = data->num_exports; i--; ) {
    exs = cons(scheme_make_integer(data->exports[i].submod_index),
	       cons(data->exports[i].ext_id, exs));
  }

  subs = array_to_list(data->subunit_exprs, data->num_subunits, scheme_null);

  tags = array_to_list(data->tags, data->num_subunits, scheme_null);

  counts = scheme_null;
  for (i = data->num_subunits; i--; ) {
    counts = cons(scheme_make_integer(data->param_counts[i]), counts);
  }

  maps = scheme_null;
  for (i = data->num_subunits; i--; ) {
    map = data->param_maps[i];

    /* Put names with the same source index in a vector; this gets the .zo 
       "symvector" optimization for complex, repetitive linking */
    lmap = NULL;
    prev = NULL;
    for (j = data->param_counts[i]; j--; ) {
      Scheme_Object *x, *t;

      if (map[j].index < 0)
	x = scheme_make_integer(map[j].u.pos);
      else
	x = map[j].u.ext_id;
      t = scheme_make_integer(map[j].index);

      if (t != prev) {
	if (lmap) maps = cons(scheme_list_to_vector(lmap), maps);
	lmap = scheme_null;
      }
      lmap = cons(t, cons(x, lmap));
      prev = t;
    }
    if (lmap)
      maps = cons(scheme_list_to_vector(lmap), maps);
  }

  return cons(scheme_make_integer(data->num_exports),
	      cons(scheme_make_integer(data->num_subunits),
		   cons(data->defname
			? data->defname
			: scheme_null,
			cons(exs,
			     cons(subs,
				  cons(tags,
				       cons(counts, maps)))))));
}

static Scheme_Object *read_compound_data(Scheme_Object *o)
{
  CompoundData *data;
  Scheme_Object *vec;
  Scheme_Object *exs, *subs, *counts, *maps, *tags;
  ParamMap *map;
  int c, n, i, j, p = 0, d = 0;

  data = MALLOC_ONE_TAGGED(CompoundData);
  data->type = scheme_unit_compound_data_type;

  c = data->num_exports = SCHEME_INT_VAL(SCHEME_CAR(o));
  o = SCHEME_CDR(o);
  n = data->num_subunits = SCHEME_INT_VAL(SCHEME_CAR(o));
  o = SCHEME_CDR(o);

  data->defname = SCHEME_CAR(o);
  if (SCHEME_NULLP(data->defname))
    data->defname = NULL;
  o = SCHEME_CDR(o);

  exs = SCHEME_CAR(o);
  o = SCHEME_CDR(o);
  subs = SCHEME_CAR(o);
  o = SCHEME_CDR(o);
  tags = SCHEME_CAR(o);
  o = SCHEME_CDR(o);
  counts = SCHEME_CAR(o);
  maps = SCHEME_CDR(o);

  {
    ExportSource *esa;
    esa = MALLOC_N_RT(ExportSource, data->num_exports);
    data->exports = esa;
  }
  for (i = 0; i < c; i++) {
#ifdef MZTAG_REQUIRED
    data->exports[i].type = scheme_rt_export_source;
#endif
    data->exports[i].submod_index = SCHEME_INT_VAL(SCHEME_CAR(exs));
    exs = SCHEME_CDR(exs);
    data->exports[i].ext_id = SCHEME_CAR(exs);
    exs = SCHEME_CDR(exs);
  }

  {
    Scheme_Object **sa;
    sa = list_to_array(tags, n, NULL);
    data->tags = sa;
    sa = list_to_array(subs, n, NULL);
    data->subunit_exprs = sa;
  }

  {
    short *sa;
    sa = MALLOC_N_ATOMIC(short, n);
    data->param_counts = sa;
  }
  for (i = 0; i < n; i++, counts = SCHEME_CDR(counts)) {
    data->param_counts[i] = SCHEME_INT_VAL(SCHEME_CAR(counts));
  }

  {
    ParamMap **pms;
    pms = MALLOC_N(ParamMap *, n);
    data->param_maps = pms;
  }
  vec = NULL;
  for (i = 0; i < n; i++) {
    c = data->param_counts[i];
    map = MALLOC_N_RT(ParamMap, c);
    data->param_maps[i] = map;

    for (j = 0; j < c; j++) {
      if (d >= p) {
	vec = SCHEME_CAR(maps);
	maps = SCHEME_CDR(maps);
	p = SCHEME_VEC_SIZE(vec);
	d = 0;
      }

#ifdef MZTAG_REQUIRED
      map[j].type = scheme_rt_param_map;
#endif
      map[j].index = SCHEME_INT_VAL(SCHEME_VEC_ELS(vec)[d++]);
      if (map[j].index < 0)
	map[j].u.pos = SCHEME_INT_VAL(SCHEME_VEC_ELS(vec)[d++]);
      else
	map[j].u.ext_id = SCHEME_VEC_ELS(vec)[d++];
    }
  }

  return (Scheme_Object *)data;
}

static Scheme_Object *write_invoke_data(Scheme_Object *o)
{
  InvokeUnitData *data;

  data = (InvokeUnitData *)o;

  return cons(scheme_make_integer(data->num_exports),
	      cons(scheme_make_svector(data->num_local_exports,
				       data->anchor_positions),
		   cons(data->expr, array_to_list(data->exports,
						  data->num_exports,
						  scheme_null))));
}


static Scheme_Object *read_invoke_data(Scheme_Object *o)
{
  InvokeUnitData *data;
  Scheme_Object *v;

  data = MALLOC_ONE_TAGGED(InvokeUnitData);
  data->type = scheme_invoke_unit_data_type;

  data->num_exports = SCHEME_INT_VAL(SCHEME_CAR(o));
  o = SCHEME_CDR(o);

  v = SCHEME_CAR(o);
  o = SCHEME_CDR(o);

  data->num_local_exports = SCHEME_SVEC_LEN(v);
  data->anchor_positions = SCHEME_SVEC_VEC(v);

  data->expr = SCHEME_CAR(o);
  o = SCHEME_CDR(o);

  {
    Scheme_Object **ex;
    ex = list_to_array(o, data->num_exports, NULL);
    data->exports = ex;
  }

  data->anchors = NULL;

  return (Scheme_Object *)data;
}

/**********************************************************************/
/* Unit/sig                                                           */
/**********************************************************************/

static Scheme_Object *do_unitsig(void *which, int argc, Scheme_Object **argv)
{
  if (!unitsig_macros)
    unitsig_macros = scheme_init_unitsig();

  return _scheme_apply(SCHEME_VEC_ELS(unitsig_macros)[SCHEME_INT_VAL(which)],
		       argc, argv);
}

static Scheme_Object *make_unitsig_macro(int pos)
{
  Scheme_Object *macro, *proc;

  proc = scheme_make_closed_prim_w_arity(do_unitsig, scheme_make_integer(pos),
					 "unitsig", 0, -1);

  macro = scheme_alloc_stubborn_small_object();
  macro->type = scheme_macro_type;
  SCHEME_PTR_VAL(macro) = proc;
  scheme_end_stubborn_change((void *)macro);

  return macro;
}

static int hash_sig(Scheme_Object *src_sig, Scheme_Hash_Table *table)
{
  int i, c;

  if (!SCHEME_VECTORP(src_sig))
    return 0;

  c = SCHEME_VEC_SIZE(src_sig);

  for (i = 0; i < c; i++) {
    Scheme_Object *s = SCHEME_VEC_ELS(src_sig)[i];

    if (SCHEME_SYMBOLP(s)) {
      Scheme_Bucket *b;
      b = scheme_bucket_from_table(table, (const char *)s);
      if (b->val)
	return 0;
      b->val = s;
    } else {
      Scheme_Hash_Table *t;
      Scheme_Bucket *b;
      Scheme_Object *name;

      if (!SCHEME_PAIRP(s) || !SCHEME_SYMBOLP(SCHEME_CAR(s)))
	return 0;

      name = SCHEME_CAR(s);

      b = scheme_bucket_from_table(table, (const char *)name);
      if (b->val)
	return 0;

      t = scheme_hash_table(10, SCHEME_hash_ptr, 0, 0);
      b->val = t;

      if (!hash_sig(SCHEME_CDR(s), t))
	return 0;
    }
  }

  return 1;
}

static char *sig_path_name(Scheme_Object *name, Scheme_Object *path)
{
  char *s;
  int l;
  s = (char *)scheme_symbol_name(name);
  l = strlen(s);

  while (!SCHEME_NULLP(path)) {
    char *n, *v;
    int nl;
    n = (char *)scheme_symbol_name(SCHEME_CAR(path));
    nl = strlen(n);
    v = scheme_malloc_atomic(nl + l + 2);
    memcpy(v + nl + 1, s, l);
    memcpy(v, n, nl);
    v[nl] = ':';
    v[l + nl + 1] = 0;
    l += nl + 1;
    s = v;
    path = SCHEME_CDR(path);
  }

  return s;
}

static int check_sig_match(Scheme_Hash_Table *table, Scheme_Object *sig, Scheme_Object *path,
			   int exact, Scheme_Object *who, Scheme_Object *src_context, Scheme_Object *dest_context)
{
  int i, c;

  if (!SCHEME_VECTORP(sig))
    return 0;

  c = SCHEME_VEC_SIZE(sig);

  for (i = 0; i < c; i++) {
    Scheme_Object *s = SCHEME_VEC_ELS(sig)[i];
    
    if (SCHEME_SYMBOLP(s)) {
      Scheme_Bucket *b;
      b = scheme_bucket_from_table(table, (const char *)s);
      if (!b->val) {
	/* Missing value */
	char *p;
	p = sig_path_name(s, path);
	scheme_raise_exn(MZEXN_UNIT,
			 "%s: %Q is missing a value name `%q', required by %Q",
			 scheme_symbol_name(who),
			 src_context,
			 p,
			 dest_context);
	return 0;
      } else if (SCHEME_FALSEP((Scheme_Object *)b->val))
	return 0;
      else if (!SCHEME_SYMBOLP((Scheme_Object *)b->val)) {
	/* Kind mismatch */
	char *p;
	p = sig_path_name(s, path);
	scheme_raise_exn(MZEXN_UNIT,
			 "%s: %Q contains `%q' as a sub-unit name, but %Q contains `%q' as a value name",
			 scheme_symbol_name(who),
			 src_context,
			 p,
			 dest_context,
			 p);
	return 0;
      }

      b->val = scheme_false;
    } else {
      Scheme_Hash_Table *t;
      Scheme_Bucket *b;
      Scheme_Object *name;

      if (!SCHEME_PAIRP(s) || !SCHEME_SYMBOLP(SCHEME_CAR(s)))
	return 0;

      name = SCHEME_CAR(s);

      b = scheme_bucket_from_table(table, (const char *)name);
      if (!b->val) {
	/* Missing sub-unit */
	char *p;
	p = sig_path_name(name, path);
	scheme_raise_exn(MZEXN_UNIT,
			 "%s: %Q is missing a sub-unit name `%q', required by %Q",
			 scheme_symbol_name(who),
			 src_context,
			 p,
			 dest_context);
      } else if (SCHEME_FALSEP((Scheme_Object *)b->val))
	return 0;
      else if (!SCHEME_HASHTP((Scheme_Object *)b->val)) {
	/* Kind mismatch */
	char *p;
	p = sig_path_name(name, path);
	scheme_raise_exn(MZEXN_UNIT,
			 "%s: %Q contains `%q' as a value name, but %Q contains `%q' as a sub-unit name",
			 scheme_symbol_name(who),
			 src_context,
			 p,
			 dest_context,
			 p);

      }

      t = (Scheme_Hash_Table *)b->val;
      b->val = scheme_false;

      if (!check_sig_match(t, SCHEME_CDR(s), scheme_make_pair(name, path),
			   exact, who, src_context, dest_context))
	return 0;
    }
  }

  if (exact) {
    Scheme_Bucket **v = table->buckets;
    int i;
    for (i = table->size; i--; ) {
      if (v[i]) {
	Scheme_Bucket *b = v[i];
	if (SCHEME_TRUEP((Scheme_Object *)b->val)) {
	  /* Extra mismatch */
	  Scheme_Object *name = (Scheme_Object *)b->key;
	  char *p;
	  p = sig_path_name(name, path);
	  scheme_raise_exn(MZEXN_UNIT,
			   "%s: %Q contains an extra %s name `%q' that is not required by %Q",
			   scheme_symbol_name(who),
			   src_context,
			   SCHEME_SYMBOLP((Scheme_Object *)b->val) ? "value" : "sub-unit",
			   p,
			   dest_context);
	}
      }
    }
  }

  return 1;
}

static Scheme_Object *verify_signature_match(int argc, Scheme_Object **argv)
{
  Scheme_Object *who, *dest_context, *dest_sig, *src_context, *src_sig;
  Scheme_Hash_Table *src_table;
  int exact;

  /* printf("start verify signature: %ld\n", scheme_get_process_milliseconds()); */

  who = argv[0];
  exact = SCHEME_TRUEP(argv[1]);
  dest_context = argv[2];
  dest_sig = argv[3];
  src_context = argv[4];
  src_sig = argv[5];

  if (!SCHEME_SYMBOLP(who))
    scheme_wrong_type("verify-signature-match", "symbol", 0, argc, argv);
  if (!SCHEME_STRINGP(dest_context))
    scheme_wrong_type("verify-signature-match", "string", 2, argc, argv);
  if (!SCHEME_STRINGP(src_context))
    scheme_wrong_type("verify-signature-match", "string", 4, argc, argv);
  
  src_table = scheme_hash_table(10, SCHEME_hash_ptr, 0, 0);
  if (!hash_sig(src_sig, src_table))
    scheme_wrong_type("verify-signature-match", "signature", 5, argc, argv);

  if (!check_sig_match(src_table, dest_sig, scheme_null,
		       exact, who, src_context, dest_context))
    scheme_wrong_type("verify-signature-match", "signature", 3, argc, argv);

  /* printf("end verify signature: %ld\n", scheme_get_process_milliseconds()); */

  return scheme_void;
}

/**********************************************************************/
/* Initialization                                                     */
/**********************************************************************/

void scheme_init_unit(Scheme_Env *env)
{
  Scheme_Object *v;

  if (scheme_starting_up) {
#ifdef MZ_PRECISE_GC
    register_traversers();
#endif

    REGISTER_SO(unitsig_macros);

    REGISTER_SO(import_symbol);
    REGISTER_SO(export_symbol);
    REGISTER_SO(with_symbol);
    REGISTER_SO(define_values_symbol);
    REGISTER_SO(unit_symbol);
    REGISTER_SO(compound_unit_symbol);
    REGISTER_SO(invoke_unit_symbol);

    export_symbol = scheme_intern_symbol("export");
    import_symbol = scheme_intern_symbol("import");
    with_symbol = scheme_intern_symbol("link");

    define_values_symbol = scheme_intern_symbol("#%define-values");

    unit_symbol = scheme_intern_symbol("#%unit");
    compound_unit_symbol = scheme_intern_symbol("#%compound-unit");
    invoke_unit_symbol = scheme_intern_symbol("#%invoke-unit");

    scheme_register_syntax("m", CloseUnit);
    scheme_register_syntax("cm", CloseCompoundUnit);
    scheme_register_syntax("im", InvokeUnit);

    scheme_install_type_writer(scheme_compiled_unit_type, write_compiled_unit);
    scheme_install_type_reader(scheme_compiled_unit_type, read_compiled_unit);
    scheme_install_type_writer(scheme_unit_body_data_type, write_body_data);
    scheme_install_type_reader(scheme_unit_body_data_type, read_body_data);
    scheme_install_type_writer(scheme_unit_compound_data_type, write_compound_data);
    scheme_install_type_reader(scheme_unit_compound_data_type, read_compound_data);
    scheme_install_type_writer(scheme_invoke_unit_data_type, write_invoke_data);
    scheme_install_type_reader(scheme_invoke_unit_data_type, read_invoke_data);  
  }

  scheme_add_global_keyword(MAKE_UNIT, 
			    scheme_make_compiled_syntax(make_unit_syntax, 
							make_unit_expand), 
			    env);
  scheme_add_global_keyword(MAKE_COMPOUND_UNIT, 
			    scheme_make_compiled_syntax(make_compound_unit_syntax, 
							make_compound_unit_expand), 
			    env);
  scheme_add_global_keyword(INVOKE_UNIT, 
			    scheme_make_compiled_syntax(invoke_unit_syntax, 
							invoke_unit_expand), 
			    env);

  scheme_add_global_constant("unit?", 
			    scheme_make_folding_prim(unit_p, 
						     "unit?",
						     1, 1, 1),
			    env);

  scheme_add_global_constant("verify-signature-match", 
			     scheme_make_prim_w_arity(verify_signature_match,
						      "verify-signature-match",
						      6, 6),
			     env);

  /* Signed unit macros: */
  v = make_unitsig_macro(0);
  scheme_add_global_keyword("define-signature", v, env);
  v = make_unitsig_macro(1);
  scheme_add_global_keyword("let-signature", v, env);
  v = make_unitsig_macro(2);
  scheme_add_global_keyword("unit-with-signature", v, env);
  scheme_add_global_keyword("unit/sig", v, env);
  v = make_unitsig_macro(3);
  scheme_add_global_keyword("compound-unit-with-signature", v, env);
  scheme_add_global_keyword("compound-unit/sig", v, env);
  v = make_unitsig_macro(4);
  scheme_add_global_keyword("invoke-unit-with-signature", v, env);
  scheme_add_global_keyword("invoke-unit/sig", v, env);
  v = make_unitsig_macro(5);
  scheme_add_global_keyword("unit->unit-with-signature", v, env);
  scheme_add_global_keyword("unit->unit/sig", v, env);
}

const char *scheme_get_unit_name(Scheme_Object *o, int *len)
{
  Scheme_Unit *u;

  if (!SCHEME_UNITP(o))
    return NULL;

  u = (Scheme_Unit *)o;
  
  if (u->init_func == do_unit) {
    Scheme_Object *n;

    n = ((UnitDataClosure *)u->data)->defname;
    if (n) {
      *len = SCHEME_SYM_LEN(n);
      return scheme_symbol_val(n);
    }    
  } else if (u->init_func == do_compound_unit) {
    Scheme_Object *n;

    n = ((CompoundLinkedData *)u->data)->defname;
    if (n) {
      *len = SCHEME_SYM_LEN(n);
      return scheme_symbol_val(n);
    }
  }
   
  return NULL;
}

/**********************************************************************/
/* Memory Counting                                                    */
/**********************************************************************/

#ifdef MEMORY_COUNTING_ON
void scheme_count_unit(Scheme_Type type, Scheme_Object *o, long *s, long *e, 
		       Scheme_Hash_Table *ht)
{
  switch (type) {
  case scheme_unit_type:
  case scheme_compiled_unit_type:
    {
      Scheme_Unit *u = (Scheme_Unit *)o;

      *s = (sizeof(Scheme_Unit)
	    + (u->num_exports * sizeof(Scheme_Object *)));
#if 0
      if ((type == scheme_compiled_unit_type)
	  || (u->init_func == do_unit)
	  || (u->init_func == do_compound_unit))
	*e += scheme_count_memory(u->data, ht);
#endif
    }
    break;
  case scheme_unit_body_data_type:
    {
      BodyData *b = (BodyData *)o;
      BodyExpr *ex;

      *s = sizeof(BodyData) + (b->closure_size * sizeof(short));

      for (ex = b->body; ex; ex = ex->next) {
	*s += sizeof(BodyExpr);

	switch (ex->btype) {
	case mm_body_def:
	  {
#if 0
	    *e += scheme_count_memory(ex->u.def.expr, ht);
#endif
	    *s += (ex->u.def.count * sizeof(BodyVar));
	  }
	  break;
	case mm_body_seq:
#if 0
	  *e += scheme_count_memory(ex->u.seq.expr, ht);
#endif
	  break;
	}
      }
    }
    break;
  case scheme_unit_compound_data_type:
    {
      CompoundData *d = (CompoundData *)o;
      int i;

      *s = (sizeof(CompoundData)
	    + (sizeof(ExportSource) * d->num_exports)
	    + (sizeof(Scheme_Object *) * 2 * d->num_subunits)
	    + ((sizeof(int) + sizeof(ParamMap *)) * d->num_subunits));
      for (i = d->num_subunits; i--; ) {
	*s += (sizeof(ParamMap) * d->param_counts[i]);
	*e += (ht ? scheme_count_memory(d->subunit_exprs[i], ht) : 0);
      }
    }
    break;
  case scheme_invoke_unit_data_type:
    {
      InvokeUnitData *i = (InvokeUnitData *)o;
      
      *s = (sizeof(InvokeUnitData)
	    + (i->num_exports * (sizeof(Scheme_Object *)
				 + sizeof(Scheme_Object *))));
#if 0
      *e += scheme_count_memory(i->expr, ht);
#endif
    }
    break;
  }
}
#endif

/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_UNIT_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_unit_type, mark_unit_val);
  GC_REG_TRAV(scheme_compiled_unit_type, mark_unit_val);
  GC_REG_TRAV(scheme_unit_body_data_type, mark_unit_body_val);
  GC_REG_TRAV(scheme_unit_compound_data_type, compound_unit_data_val);
  GC_REG_TRAV(scheme_invoke_unit_data_type, invoke_unit_data_val);

  GC_REG_TRAV(scheme_rt_unit_id, mark_unit_id);
  GC_REG_TRAV(scheme_rt_body_expr, mark_body_expr);
  GC_REG_TRAV(scheme_rt_body_var, mark_body_var);
  GC_REG_TRAV(scheme_rt_param_map, mark_param_map);
  GC_REG_TRAV(scheme_rt_export_source, mark_export_source);
  GC_REG_TRAV(scheme_rt_unit_data_closure, mark_unit_data_closure);
  GC_REG_TRAV(scheme_rt_compound_linked_data, mark_compound_linked_data);
  GC_REG_TRAV(scheme_rt_do_invoke_data, mark_do_invoke_data);
}

END_XFORM_SKIP;

#endif

#endif /* NO_UNIT_SYSTEM */

