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
*/

/* This implements parsing, compilation, and execution for `class'
   expressions. See also object.c. */

#include "schpriv.h"

#ifndef NO_OBJECT_SYSTEM

static Scheme_Object *seq_symbol;
static Scheme_Object *pub_symbol, *ovr_symbol, *pri_symbol;
static Scheme_Object *inh_symbol;
static Scheme_Object *ren_symbol;

static Scheme_Object *class_star_symbol;

#define cons scheme_make_pair

static Scheme_Object *DefineClass_Execute(Scheme_Object *form);
static Scheme_Object *DefineClass(Scheme_Object *form, Scheme_Comp_Env *env,
				  Scheme_Compile_Info *rec, int drec);
static Scheme_Object *DefineClass_expand(Scheme_Object *form, Scheme_Comp_Env *env, int depth);
static Scheme_Object *write_Class_Data(Scheme_Object *obj);
static Scheme_Object *read_Class_Data(Scheme_Object *obj);

void scheme_init_objclass(Scheme_Env *env)
{
  if (scheme_starting_up) {
    REGISTER_SO(class_star_symbol);

    class_star_symbol = scheme_intern_symbol("#%class*/names");

    REGISTER_SO(seq_symbol);
    REGISTER_SO(pub_symbol);
    REGISTER_SO(ovr_symbol);
    REGISTER_SO(pri_symbol);
    REGISTER_SO(inh_symbol);
    REGISTER_SO(ren_symbol);
    
    seq_symbol = scheme_intern_symbol("sequence");
    pub_symbol = scheme_intern_symbol("public");
    ovr_symbol = scheme_intern_symbol("override");
    pri_symbol = scheme_intern_symbol("private");
    inh_symbol = scheme_intern_symbol("inherit");
    ren_symbol = scheme_intern_symbol("rename");

    scheme_register_syntax("dc", DefineClass_Execute);

    scheme_install_type_writer(scheme_class_data_type, write_Class_Data);
    scheme_install_type_reader(scheme_class_data_type, read_Class_Data);
  }

  scheme_add_global_keyword("class*/names", 
			    scheme_make_compiled_syntax(DefineClass, 
							DefineClass_expand), 
			    env);

}


static short *CheckInherited(Scheme_Class *sclass, ClassVariable *item)
{
  short *ref_map;
  Scheme_Class *superclass = sclass->superclass;

  ref_map = MALLOC_N_ATOMIC(short, sclass->num_ref);

  for (; item; item = item->next) {
    if (isref(item) || isoverride(item)) {
      int p;
      Scheme_Object *name = (isoverride(item)
			     ? IVAR_EXT_NAME(item)
			     : item->u.source.name);
      if ((p = scheme_DoFindName(superclass->num_public, superclass->public_names, name)) < 0) {
	const char *cl, *sc;

	cl = scheme_iget_class_name((Scheme_Object *)sclass, " for class: ");
	sc = scheme_iget_class_name((Scheme_Object *)superclass, " in superclass: ");

	scheme_raise_exn(MZEXN_OBJECT,
			 "class*" ": %s ivar not found: %S%s%s",
			 isoverride(item) ? "overridden" : "inherited",
			 name,
			 sc,
			 cl);
      } else {
	if (!isoverride(item))
	  ref_map[item->index] = superclass->public_map[p];
      }
    } else if (ispublic(item)) {
      Scheme_Object *name = IVAR_EXT_NAME(item);
      if (scheme_DoFindName(superclass->num_public, superclass->public_names, name) >= 0) {
	const char *cl, *sc;

	cl = scheme_iget_class_name((Scheme_Object *)sclass, " for class: ");
	sc = scheme_iget_class_name((Scheme_Object *)superclass, " in superclass: ");

	scheme_raise_exn(MZEXN_OBJECT,
			 "class*" ": superclass already includes public ivar: %S%s%s",
			 name,
			 sc,
			 cl);
      }
    }
  }

  return ref_map;
}

static short *MapIvars(Scheme_Class *sclass, ClassVariable *item)
{
  short *ivar_map;

  ivar_map = MALLOC_N_ATOMIC(short, sclass->num_ivar);

  for (; item; item = item->next) {
    if (ispublic(item)) {
      int pos;
      pos = scheme_DoFindName(sclass->num_public, sclass->public_names, item->name);
      ivar_map[item->index] = sclass->public_map[pos];
    }
  }

  return ivar_map;
}

static ClassVariable *ReadItemList(char *what, Scheme_Object *vars, 
				   int vartype,
				   ClassVariable *start,
				   ClassVariable **very_first,
				   Scheme_Object *form)
{
  Scheme_Object *l, *var, *value;
  ClassVariable *classvar;
  int alias, seq, input, islast;

  alias = isreftype(vartype);
  seq = (vartype == varNOTHING);
  input = (vartype == varINPUT);

  for (; !SCHEME_NULLP(vars); vars = SCHEME_CDR(vars)) {
    if (input && !SCHEME_PAIRP(vars)) {
      islast = 1;
      l = vars;
    } else {
      l = SCHEME_CAR(vars);
      islast = 0;
    }
    
    if (seq) {
      var = seq_symbol;
      value = l;
    } else if (SCHEME_LISTP(l)) {
      var = SCHEME_CAR(l);
	
      l = SCHEME_CDR(l);
      if (SCHEME_NULLP(l))
	value = NULL;
      else
	value = SCHEME_CAR(l);
    } else {
      var = l;
      value = NULL;
    }

    if (alias && !value)
      value = var;
    if (islast && !value)
      value = scheme_null;

    classvar = MALLOC_ONE_RT(ClassVariable);
#ifdef MZTAG_REQUIRED
    classvar->type = scheme_rt_class_var;
#endif
    classvar->name = var;
    classvar->vartype = vartype;
    
    if (alias)
      classvar->u.source.name = value;
    else
      classvar->u.value = value;
    classvar->next = NULL;

    if (start)
      start->next = classvar;
    else
      *very_first = classvar;

    start = classvar;

    if (islast)
      break;
  }

  return start;
}


#define check_MustPair 0x1
#define check_MustId 0x2
#define check_BindingMustId 0x4
#define check_CanRename 0x8

static void GenCheckIvarList(Scheme_Object *clause, 
			     Scheme_Object *naya, 
			     int flags,
			     Scheme_Object *form)
{
  Scheme_Object *l, *p, *n;

  for (l = naya; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    p = SCHEME_CAR(l);
    if (SCHEME_SYMBOLP(p)) {
      if (flags & check_MustPair)
	scheme_wrong_syntax("class*", p, form,
			    "%S clause must contain pairs",
			    clause);
    } else if (SCHEME_PAIRP(p)) {
      Scheme_Object *pr = p;

      if (flags & check_MustId) {
	if (!(flags & check_CanRename)) {
	  scheme_wrong_syntax("class*", p, form,
			      "%S clause must contain only identifiers",
			      clause);
	}
	n = p;
	p = scheme_null;
      } else {
	n = SCHEME_CAR(p);
	p = SCHEME_CDR(p);
      }

      if (!SCHEME_SYMBOLP(n)) {
	if (flags & check_CanRename) {
	  if (!SCHEME_PAIRP(n) || !SCHEME_PAIRP(SCHEME_CDR(n))
	      || !SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(n))))
	    scheme_wrong_syntax("class*", n, form,
				"badly formed %S clause (bad internal-external"
				" identifier pair form)", clause);
	  if (!SCHEME_SYMBOLP(SCHEME_CAR(n)))
	    scheme_wrong_syntax("class*", SCHEME_CAR(n), form,
				"badly formed %S clause (internal"
				" name not an identifier)", clause);
	  n = SCHEME_CDR(n);
	  if (!SCHEME_SYMBOLP(SCHEME_CAR(n)))
	    scheme_wrong_syntax("class*", SCHEME_CAR(n), form,
				"badly formed %S clause (external"
				" name not an identifier)", clause);
	} else
	  scheme_wrong_syntax("class*", n, form,
			      "badly formed %S clause (name"
			      " not an identifier)", clause);
      }

      if (!SCHEME_NULLP(p)) {
	if (!SCHEME_PAIRP(p))
	scheme_wrong_syntax("class*", pr, form,
			    "badly formed %S clause (identifier"
			    " declaration is improper)", 
			    clause);
	if (!SCHEME_NULLP(SCHEME_CDR(p)))
	  scheme_wrong_syntax("class*", pr, form,
			      "badly formed %S clause (identifier"
			      " declaration not a pair)", 
			      clause);
	if (flags & check_BindingMustId) {
	  n = SCHEME_CAR(p);
	  if (!SCHEME_SYMBOLP(n))
	    scheme_wrong_syntax("class*", pr, form,
				"association "
				"in %S clause "
				"must be an identifier",
				clause);
	}
      } else if (flags & check_MustPair)
	scheme_wrong_syntax("class*", pr, form,
			    "%S clause must contain pairs",
			    clause);
    } else
      scheme_wrong_syntax("class*", p, form,
			  "badly formed %S clause (declaration"
			  " not an identifier or pair)", 
			  clause);
  }
  
  if (!SCHEME_NULLP(l))
    scheme_wrong_syntax("class*", l, form,
			"badly formed %S clause (" 
			IMPROPER_LIST_FORM
			" for identifiers)", 
			clause);
  
}

static void CheckIvarList(Scheme_Object *t, Scheme_Object *l, int flags,
			  Scheme_Object *form)
{
  GenCheckIvarList(t, l, flags, form);
}

static Scheme_Object *GetNames(ClassVariable *ivar, 
			       int pblic, int prvate, int ref,
			       Scheme_Comp_Env *env)
{
  Scheme_Object *first = scheme_null, *last = NULL;
  int offset = 0;

  for (; ivar; ivar = ivar->next) {
    if ((ispublic(ivar) && pblic)
	|| (isref(ivar) && ref)
	|| (isprivate(ivar) && prvate)) {
      if (env) {
	scheme_unsettable_variable(env, offset);
      } else {
	Scheme_Object *p;
	p = cons(IVAR_INT_NAME(ivar), scheme_null);
	
	if (last)
	  SCHEME_CDR(last) = p;
	else
	  first = p;
	
	last = p;
      }

      offset++;
    }
  }

  return first;
}

static Scheme_Object *expandall(Scheme_Object *vars, Scheme_Comp_Env *env, int depth,
				int expand_expr, Scheme_Comp_Env *first_from)
{
  Scheme_Object *first, *last;
  Scheme_Object *name, *var;

  name = SCHEME_CAR(vars);
  vars = SCHEME_CDR(vars);

  first = cons(name, scheme_null);
  last = first;

  if (first_from) {
    name = SCHEME_CAR(vars);
    vars = SCHEME_CDR(vars);
    
    last = cons(name, scheme_null);
    SCHEME_CDR(first) = last;
  }

  while (!SCHEME_NULLP(vars)) {
    var = SCHEME_CAR(vars);

    if (!SCHEME_PAIRP(var)) {
    } else {
      Scheme_Object *rest, *name;
    
      name = SCHEME_CAR(var);
      rest = SCHEME_CDR(var);

      if (expand_expr)
	rest = scheme_expand_expr(rest, env, depth);
      
      var = cons(name, rest);
    }
    
    var = cons(var, scheme_null);

    SCHEME_CDR(last) = var;
    last = var;

    vars = SCHEME_CDR(vars);
  }

  return first;
}

static void CompileItemList(Scheme_Object *form,
			    ClassVariable *cvars,
			    Scheme_Comp_Env *env, 
			    Class_Data *data,
			    Scheme_Compile_Info *rec,
			    int drec)
{
  int alias, count;
  ClassVariable *classvar;
  Scheme_Compile_Info *recs;

  for (count = 0, classvar = cvars; classvar; classvar = classvar->next) {
    alias = isref(classvar);
    if (!alias)
      count++;
  }

  recs = MALLOC_N_RT(Scheme_Compile_Info, count);
  scheme_init_compile_recs(rec, drec, recs, count);

  for (count = 0, classvar = cvars; classvar; classvar = classvar->next) {
    alias = isref(classvar);

    if (!alias && !classvar->u.value) {
      Scheme_Object *cv;
      cv = scheme_compiled_void(rec[drec].can_optimize_constants);
      classvar->u.value = cv;
    }

    scheme_check_identifier("class*", IVAR_INT_NAME(classvar), NULL, env, form);

    if (!alias) {
      recs[count].value_name = IVAR_EXT_NAME(classvar);
      {
	Scheme_Object *ce;
	ce = scheme_compile_expr(classvar->u.value, env, recs, count);
	classvar->u.value = ce;
      }
      count++;
    }
  }

  scheme_merge_compile_recs(rec, drec, recs, count);
}

static void EnsureNamesReady(ClassVariable *ivars, int count, Scheme_Object ***names)
{
  int i;
  ClassVariable *cvar;

  if (count && !*names) {
    Scheme_Object **ns;
    ns = MALLOC_N(Scheme_Object*, count);
    *names = ns;

    for (i = 0, cvar = ivars; cvar; cvar = cvar->next) {
      if (ispublic(cvar))
	(*names)[i++] = _IVAR_EXT_NAME(cvar);
    }

    qsort((char *)*names, count,
	  sizeof(Scheme_Object *), 
	  (Compare_Proc)scheme_CompareObjectPtrs); 
  }
}

void scheme_InitData(Class_Data *data)
{
  ClassVariable *item;
  int pub_index = 0, ref_index = 0, priv_index = 0;
  int input_index;
  Scheme_Object **names;
  
  input_index = data->num_arg_vars;
  for (item = data->ivars; item; item = item->next) {
    if (SCHEME_PAIRP(item->name))
      item->name = IVAR_EXT_NAME(item);

    if (ispublic(item)) {
      item->index = pub_index++;
    } else if (isref(item)) {
      item->index = ref_index++;
    } else if (isprivate(item)) {
      item->index = priv_index++;
    } else if (isinput(item)) {
      item->index = --input_index;
    }
  }

  data->num_ivar = pub_index;
  data->num_private = priv_index;
  data->num_ref = ref_index;

  data->num_cmethod = 0;
  data->cmethod_names = NULL;
  data->cmethods = NULL;

  names = NULL;
  EnsureNamesReady(data->ivars, data->num_ivar, &names);
  data->ivar_names = names;
}

void scheme_InstallHeritage(Scheme_Class *sclass, Scheme_Class *superclass)
{
  int i;

  sclass->pos = superclass->pos + 1;
  {
    Scheme_Class **ca;
    ca = MALLOC_N(Scheme_Class*, (sclass->pos + 1));
    sclass->heritage = ca;
  }
  for (i = 0; i < sclass->pos; i++) {
    sclass->heritage[i] = superclass->heritage[i];
  }
  sclass->heritage[sclass->pos] = sclass;
  sclass->superclass = sclass->heritage[sclass->pos - 1];
}

/*========================================================================*/
/*                       class parse and execute                          */
/*========================================================================*/

Scheme_Object *scheme_DefineClass_Execute(Scheme_Object *form, int already_evaled)
{
  Scheme_Class *sclass, *superclass;
  Class_Data *data;
  Scheme_Interface **interfaces;
  Scheme_Object *superobj, **temp_array, **public_names, *il;
  int i, j, num_cmethod, newpos, num_local_interfaces, num_interfaces, num_public;
  int num_contrib_cmethod;
  short **imaps;
  CMethod **tmp_cmethods;
  short *tmp_cmethod_ready_level, *tmp_cmethod_source_map;

  data = (Class_Data *)form;

  if (!already_evaled)
    superobj = _scheme_eval_compiled_expr(data->super_expr);
  else
    superobj = data->super_expr;

  if (!SCHEME_CLASSP(superobj)) {
    const char *symname;
    symname = data->defname ? scheme_symbol_name(data->defname) : "";
    scheme_raise_exn(MZEXN_OBJECT,
		     "class*" ": superclass expression returned "
		     "a non-class: %s%s%s",
		     scheme_make_provided_string(superobj, 1, NULL),
		     data->defname ? " for class: " : "",
		     symname);
    return NULL;
  }
	
  superclass = (Scheme_Class *)superobj;

  /* Build a list instead of allocating an array immediately.
     See note in Interface_Execute(). */
  num_local_interfaces = data->num_interfaces;
  il = scheme_null;
  for (i = 0; i < num_local_interfaces; i++) {
    Scheme_Object *in;
    in = _scheme_eval_compiled_expr(data->interface_exprs[i]);
    if (!SCHEME_INTERFACEP(in)) {
      const char *symname;
      symname = data->defname ? scheme_symbol_name(data->defname) : "";
      scheme_raise_exn(MZEXN_OBJECT,
		       "class*" ": interface expression returned "
		       "a non-interface: %s%s%s",
		       scheme_make_provided_string(in, 1, NULL),
		       data->defname ? " for class: " : "",
		       symname);
      return NULL;
    }
    il = cons(in, il);
  }

  sclass = MALLOC_ONE_TAGGED(Scheme_Class);
  sclass->type = scheme_class_type;
  if ((superclass->priminit == pi_CPP) 
      || (superclass->priminit == pi_NOT_OVER_CPP)
      || (superclass->priminit == pi_COMP_OVER_CPP))
    sclass->priminit = pi_NOT_OVER_CPP;
  else
    sclass->priminit = pi_NOT;
  sclass->defname = data->defname;

  if ((superclass->priminit == pi_CPP) 
      && !superclass->piu.initf) {
    const char *cl;
    cl = scheme_iget_class_name((Scheme_Object *)sclass, " to create class: ");
    scheme_raise_exn(MZEXN_OBJECT,
		     "class*" ": can't derive from the class: %s%s", 
		     scheme_symbol_name(superclass->defname),
		     cl);
  }
  
  scheme_InstallHeritage(sclass, superclass);

  sclass->super_init_name = data->super_init_name;

  num_interfaces = num_local_interfaces + superclass->num_interfaces;

  if (num_interfaces) {
    interfaces = MALLOC_N(Scheme_Interface*, num_interfaces);
    for (i = num_local_interfaces; i--; ) {
      Scheme_Interface *in = (Scheme_Interface *)SCHEME_CAR(il);
      interfaces[i] = in;
      if (!scheme_is_subclass((Scheme_Object *)superclass, (Scheme_Object *)in->supclass)) {
	const char *inn;
	inn = scheme_iget_interface_name((Scheme_Object *)in, " for interface: ");
	scheme_raise_exn(MZEXN_OBJECT,
			 "class*" ": superclass doesn't satisfy implementation requirement of interface: %s%s", 
			 scheme_symbol_name(superclass->defname),
			 inn);
      }
      il = SCHEME_CDR(il);
    }
    for (i = superclass->num_interfaces; i--; ) {
      interfaces[i + num_local_interfaces] = superclass->interfaces[i];
    }
  } else
    interfaces = NULL;
  sclass->num_interfaces = num_interfaces;
  sclass->interfaces = interfaces;

  /* Setup public/private */
  sclass->ivars = data->ivars;
  sclass->num_args = data->num_args;
  sclass->num_required_args = data->num_required_args;
  sclass->num_arg_vars = data->num_arg_vars;
  sclass->num_private = data->num_private;
  sclass->num_ref = data->num_ref;
  sclass->num_ivar = data->num_ivar;

  /* How big is the union? */
  {
    int pub_count;
    pub_count = (scheme_MergeArray(data->num_ivar, data->ivar_names, NULL, NULL,
				   superclass->num_public, superclass->public_names, NULL, NULL,
				   NULL, NULL, 1)
		 + scheme_MergeArray(data->num_cmethod, data->cmethod_names, NULL, NULL,
				     superclass->num_public, superclass->public_names, NULL, NULL,
				     NULL, NULL, 1)
		 - superclass->num_public);
    sclass->num_public = pub_count;
  }

  /* Make room for the union */
  num_public = sclass->num_public;
  temp_array = MALLOC_N(Scheme_Object*, num_public);
  public_names = MALLOC_N(Scheme_Object*, num_public);
  sclass->public_names = public_names;
  
  /* Union names: */
  i = scheme_MergeArray(data->num_ivar, data->ivar_names, data->ivar_names, NULL,
			superclass->num_public, superclass->public_names, superclass->public_names, NULL,
			temp_array, NULL, 1);
  scheme_MergeArray(i, temp_array, temp_array, NULL,
		    data->num_cmethod, data->cmethod_names, data->cmethod_names, NULL,
		    public_names, NULL, 1);

  /* Map names to source: */
  {
    short *sa;
    sa = MALLOC_N_ATOMIC(short, num_public);
    sclass->public_map = sa;
    sa = MALLOC_N_ATOMIC(short, num_public);
    sclass->vslot_map = sa;
  }
  {
    slotkind *ska;
    ska = MALLOC_N_ATOMIC(slotkind, num_public);
    sclass->vslot_kind = ska;
  }
  sclass->num_slots = ((sclass->priminit == pi_NOT_OVER_CPP) ? EXTRA_PRIM_SLOTS : 0);
  num_cmethod = 0;
  num_contrib_cmethod = 0;
  newpos = superclass->num_public;

  if (!data->num_cmethod) {
    tmp_cmethods = NULL;
    tmp_cmethod_ready_level = NULL;
    tmp_cmethod_source_map = NULL;
  } else {
    /* Alloc for largest case */
    i = data->num_cmethod + superclass->num_public;
    tmp_cmethods = MALLOC_N(CMethod*, i);
    tmp_cmethod_ready_level = MALLOC_N_ATOMIC(short, i);
    tmp_cmethod_source_map = MALLOC_N_ATOMIC(short, i);
  }

  for (i = num_public; i--; ) {
    Scheme_Object *s = public_names[i];
    int cmpos, ipos;

    cmpos = scheme_DoFindName(data->num_cmethod, data->cmethod_names, s);
    if (cmpos < 0)
      ipos = scheme_DoFindName(data->num_ivar, data->ivar_names, s);
    else
      ipos = -1;

    j = scheme_DoFindName(superclass->num_public, superclass->public_names, s);
    if (j >= 0) {
      /* Overriding or inheriting: use superclass-determined vslot position. */
      int vp = superclass->public_map[j];
      sclass->public_map[i] = vp;
      if (ipos >= 0) {
	/* Override with ivar */
	sclass->vslot_map[vp] = sclass->num_slots++;
	sclass->vslot_kind[vp] = slot_TYPE_IVAR;
      } else if (cmpos >= 0) {
	/* Override with cmethod */
	tmp_cmethods[num_cmethod] = data->cmethods[cmpos];
	tmp_cmethod_ready_level[num_cmethod] = sclass->pos;
	tmp_cmethod_source_map[num_cmethod] = num_contrib_cmethod++;
	sclass->vslot_map[vp] = num_cmethod++;
	sclass->vslot_kind[vp] = slot_TYPE_CMETHOD;
      } else {
	/* Inherit */
	slotkind k = superclass->vslot_kind[vp];
	sclass->vslot_kind[vp] = k;
	if (k == slot_TYPE_CMETHOD) {
	  /* Inherit cmethod */
	  int cindex = superclass->vslot_map[vp];
	  if (tmp_cmethods) {
	    /* Adding cmethods, so re-shuffle: */
	    tmp_cmethods[num_cmethod] = superclass->cmethods[cindex];
	    tmp_cmethod_ready_level[num_cmethod] = superclass->cmethod_ready_level[cindex];
	    tmp_cmethod_source_map[num_cmethod] = -1;
	    sclass->vslot_map[vp] = num_cmethod++;
	  } else {
	    /* No new cmethods; using superclass's array: */
	    sclass->vslot_map[vp] = superclass->vslot_map[vp];
	  }
	} else {
	  /* Inherit ivar */
	  sclass->vslot_map[vp] = sclass->num_slots++;
	}
      }
    } else {
      /* New vars: */
      int vp = newpos++;
      if (cmpos >= 0) {
	/* New cmethod */
	sclass->public_map[i] = vp;
	tmp_cmethods[num_cmethod] = data->cmethods[cmpos];
	tmp_cmethod_ready_level[num_cmethod] = sclass->pos;
	tmp_cmethod_source_map[num_cmethod] = num_contrib_cmethod++;
	sclass->vslot_map[vp] = num_cmethod++;
	sclass->vslot_kind[vp] = slot_TYPE_CMETHOD;
      } else {
	/* New ivar */
	sclass->public_map[i] = vp;
	sclass->vslot_map[vp] = sclass->num_slots++;
	sclass->vslot_kind[vp] = slot_TYPE_IVAR;
      }
    }
  }

  sclass->contributes_cmethods = num_contrib_cmethod;
  if (!data->num_cmethod) {
    /* Use superclass array: */
    sclass->cmethods = superclass->cmethods;
    sclass->cmethod_ready_level = superclass->cmethod_ready_level;
    sclass->cmethod_source_map = superclass->cmethod_source_map;
  } else if (num_cmethod) {
    {
      CMethod **cma;
      cma = MALLOC_N(CMethod*, num_cmethod);
      sclass->cmethods = cma;
    }
    {
      short *sa;
      sa = MALLOC_N_ATOMIC(short, num_cmethod);
      sclass->cmethod_ready_level = sa;
    }
    if (num_contrib_cmethod) {
      short *sa;
      sa = MALLOC_N_ATOMIC(short, num_cmethod);
      sclass->cmethod_source_map = sa;
    } else
      sclass->cmethod_source_map = NULL;
    for (i = num_cmethod; i--; ) {
      sclass->cmethods[i] = tmp_cmethods[i];
      sclass->cmethod_ready_level[i] = tmp_cmethod_ready_level[i];
      if (num_contrib_cmethod)
	sclass->cmethod_source_map[i] = tmp_cmethod_source_map[i];
    }
  } else {
    sclass->cmethods = NULL;
    sclass->cmethod_ready_level = NULL;
    sclass->cmethod_source_map = NULL;
  }
  tmp_cmethods = NULL;
  tmp_cmethod_ready_level = NULL;
  tmp_cmethod_source_map = NULL;

  /* Map ref index to instance slot: */
  {
    short *sa;
    sa = CheckInherited(sclass, sclass->ivars);
    sclass->ref_map = sa;
  }

  /* Map public index to instance slot: */
  {
    short *sa;
    sa = MapIvars(sclass, sclass->ivars);
    sclass->ivar_map = sa;
  }

  /* Map interface name positions to class positions */
  if (num_interfaces)
    imaps = MALLOC_N(short*, num_interfaces);
  else
    imaps = NULL;
  sclass->interface_maps = imaps;
  for (i = 0; i < num_interfaces; i++) {
    int j, k = 0;
    Scheme_Interface *in = (Scheme_Interface *)interfaces[i];
    {
      short *sa;
      sa = MALLOC_N_ATOMIC(short, in->num_names);
      imaps[i] = sa;
    }
    for (j = 0; j < in->num_names; j++) {
      while ((k < num_public) && SLESSTHAN(public_names[k], in->names[j])) {
	k++;
      }
      if ((k >= num_public) || !SEQUALS(public_names[k], in->names[j])) {
	const char *cl, *inn;
	char buffer[20], *bf;

	cl = scheme_iget_class_name((Scheme_Object *)sclass, " by class: ");
	inn = scheme_iget_interface_name((Scheme_Object *)in, ": ");

	if (num_interfaces > 1) {
	  if (i == 0)
	    bf = "1st ";
	  else if (i == 1)
	    bf = "2nd ";
	  else if (num_interfaces == 2)
	    bf = "3rd ";
	  else {
	    sprintf(buffer, "%dth ", i + 1);
	    bf = buffer;
	  }
	} else
	  bf = "";
	
	scheme_raise_exn(MZEXN_OBJECT,
			 "class*" ": ivar not implemented: %S%s"
			 " as required by the %sinterface%s",
			 in->names[j],
			 cl, bf, inn);
	return NULL;
      }
      imaps[i][in->name_map[j]] = sclass->public_map[k];
    }
  }

  sclass->max_let_depth = data->max_let_depth;

  i = sclass->closure_size = data->closure_size;
  if (i) {
#ifndef RUNSTACK_IS_GLOBAL
    Scheme_Process *p = scheme_current_process;
#endif
    Scheme_Object **saved, **stack;
    short *map = data->closure_map;

    saved = MALLOC_N(Scheme_Object *, i);
    sclass->closure_saved = saved;
    stack = MZ_RUNSTACK;
    while (i--) {
      saved[i] = stack[map[i]];
    }
  }

  scheme_install_class_interface(sclass);

  return (Scheme_Object *)sclass;
}

static Scheme_Object *DefineClass_Link(Scheme_Object *form, Link_Info *info)
{
  Class_Data *data;
  int i;
  ClassVariable *ivar;

  data = (Class_Data *)form;

  {
    Scheme_Object *le;
    le = scheme_link_expr(data->super_expr, info);
    data->super_expr = le;
  }
  
  for (i = data->num_interfaces; i--; ) {
    Scheme_Object *le;
    le = scheme_link_expr(data->interface_exprs[i], info);
    data->interface_exprs[i] = le;
  }

  i = data->closure_size;
  info = scheme_link_info_extend(info, 0, 0, i);
  while (i--) {
    int pos = data->closure_map[i], flags, li;
    li = scheme_link_info_lookup(info, pos, &flags);
    data->closure_map[i] = li;
    scheme_link_info_add_mapping(info, pos, i, flags);
  }

  i = data->num_private;
  info = scheme_link_info_extend(info, i, i, i);
  while (i--) {
    scheme_link_info_add_mapping(info, i, i, SCHEME_INFO_BOXED);
  }

  i = data->num_ref;
  info = scheme_link_info_extend(info, i, i, i);
  while (i--) {
    scheme_link_info_add_mapping(info, i, i, SCHEME_INFO_BOXED);
  }

  i = data->num_ivar;
  info = scheme_link_info_extend(info, i, i, i);
  while (i--) {
    scheme_link_info_add_mapping(info, i, i, SCHEME_INFO_BOXED);
  }

  i = data->num_arg_vars + 2;
  info = scheme_link_info_extend(info, i, i, i);
  while (i--) {
    scheme_link_info_add_mapping(info, i, i, SCHEME_INFO_BOXED);
  }

  for (ivar = data->ivars; ivar; ivar = ivar->next) {
    switch(ivar->vartype) {
    case varPUBLIC:
    case varOVERRIDE:
    case varPRIVATE:
    case varNOTHING:
    case varINPUT:
      {
	Scheme_Object *le;
	le = scheme_link_expr(ivar->u.value, info);
	ivar->u.value = le;
      }
      break;
    case varINHERIT:
    case varRENAME:
      break;
    }
  }

  return scheme_make_syntax_link(DefineClass_Execute, 
				 (Scheme_Object *)data);
}

static Scheme_Object *Do_DefineClass(Scheme_Object *form, Scheme_Comp_Env *env,
				     Scheme_Compile_Info *rec, int drec, int depth)
{
#define BAD_MSG ": bad syntax in class definition"

  DupCheckRecord *r, *er, dcrec, erec;
  Scheme_Object *l, *superclass, *vars, *tag;
  Scheme_Object *superinitname, **interfaces;
  Scheme_Object *pub, *priv, *ref, *objl;
  ClassVariable *ivars = NULL, *next = NULL;
  Scheme_Object *make_args, *thisname;
  Scheme_Comp_Env *pubenv, *prienv, *refenv, *objenv, *firstenv;
  Class_Data *data;
  ClassVariable *item;
  int num_args, num_required_args, num_arg_vars, num_interfaces;

  dcrec.scheck_size = 0;
  erec.scheck_size = 0;

  l = SCHEME_CDR(form);

  if (!SCHEME_LISTP(l))
    scheme_wrong_syntax("class*", NULL, form, NULL);
  if (SCHEME_NULLP(l))
    scheme_wrong_syntax("class*/names", NULL, form, 
			"missing this/super-init specification");
    
  vars = SCHEME_CAR(l);
  if (!SCHEME_PAIRP(vars))
    scheme_wrong_syntax("class*/names", NULL, form, 
			"bad this/super-init specification");
  thisname = SCHEME_CAR(vars);
  scheme_check_identifier("class*/names", thisname, NULL, env, form);

  vars = SCHEME_CDR(vars);
  if (!SCHEME_PAIRP(vars))
    scheme_wrong_syntax("class*/names", NULL, form, 
			"bad this/super-init specification");
  superinitname = SCHEME_CAR(vars);
  scheme_check_identifier("class*/names", superinitname, NULL, env, form);
      
  vars = SCHEME_CDR(vars);
  if (!SCHEME_NULLP(vars))
    scheme_wrong_syntax("class*/names", NULL, form, 
			"bad this/super-init specification (extra syntax after %s)",
			scheme_symbol_name(superinitname));

  l = SCHEME_CDR(l);

  if (!SCHEME_LISTP(l))
    scheme_wrong_syntax("class*", NULL, form, NULL);
  if (SCHEME_NULLP(l))
    scheme_wrong_syntax("class*", NULL, form, 
			"missing superclass specification");
  superclass = SCHEME_CAR(l);
  
  l = SCHEME_CDR(l);

  if (!SCHEME_LISTP(l))
    scheme_wrong_syntax("class*", NULL, form, NULL);
  if (SCHEME_NULLP(l))
    scheme_wrong_syntax("class*", NULL, form, 
			"missing interfaces specification");

  num_interfaces = 0;
  {
    Scheme_Object *il = SCHEME_CAR(l);

    while (SCHEME_PAIRP(il)) {
      num_interfaces++;
      il = SCHEME_CDR(il);
    }

    if (!SCHEME_NULLP(il)) {
      scheme_wrong_syntax("class*", il, form, 
			  "bad interfaces specification");
    }

    if (num_interfaces) {
      int i;
      interfaces = MALLOC_N(Scheme_Object*, num_interfaces);
      il = SCHEME_CAR(l);
      for (i = 0; i < num_interfaces; i++, il = SCHEME_CDR(il)) {
	interfaces[i] = SCHEME_CAR(il);
      }
    } else
      interfaces = NULL;
  }

  l = SCHEME_CDR(l);

  num_args = num_arg_vars = num_required_args = 0;
  if (SCHEME_PAIRP(l)) {
    int found_nonrequired = 0;
    Scheme_Object *args = SCHEME_CAR(l);

    l = SCHEME_CDR(l);

    make_args = args;

    while (!SCHEME_NULLP(args)) {
      if (SCHEME_SYMBOLP(args)) {
	args = scheme_null;
	num_args = -1;
      } else {
	Scheme_Object *v;

	if (!SCHEME_PAIRP(args))
	  scheme_wrong_syntax("class*", args, form, 
			      "bad syntax in argument list");

	v = SCHEME_CAR(args);
	if (SCHEME_SYMBOLP(v)) {
	  if (found_nonrequired)
	    scheme_wrong_syntax("class*", v, form, 
				"bad syntax in argument list");
	  num_required_args++;
	} else {
	  if (!SCHEME_PAIRP(v) || !SCHEME_SYMBOLP(SCHEME_CAR(v)))
	    scheme_wrong_syntax("class*", v, form, 
				"bad syntax in argument list");
	  found_nonrequired = 1;
	}
	args = SCHEME_CDR(args);
	num_args++;
      }
      num_arg_vars++;
    }

    next = ReadItemList("arguments", make_args, varINPUT, next, &ivars, form);
  } else {
    scheme_wrong_syntax("class*", NULL, form, 
			"missing argument list");
    return scheme_void;
  }

  while (1) {
    if (!SCHEME_LISTP(l))
      scheme_wrong_syntax("class*", l, form, NULL);

    if (SCHEME_NULLP(l))
      break;
    vars = SCHEME_CAR(l);

    if (!SCHEME_PAIRP(vars))
      scheme_wrong_syntax("class*", vars, form, NULL);

    tag = SCHEME_CAR(vars);
    vars = SCHEME_CDR(vars);

    if (SAME_OBJ(tag, seq_symbol)) {
      Scheme_Object *ll = vars;
      while (SCHEME_PAIRP(ll)) {
	ll = SCHEME_CDR(ll);
      }
      if (!SCHEME_NULLP(ll))
	scheme_wrong_syntax("class*", ll, form, 
			    "bad syntax in sequence clause"
			    " (" IMPROPER_LIST_FORM ")");
      
      next = ReadItemList("sequence", vars, varNOTHING, next, &ivars, form);
    } else if (SAME_OBJ(tag, pub_symbol)) {
      CheckIvarList(tag, vars, check_CanRename, form);
      next = ReadItemList("public", vars, varPUBLIC, next, &ivars, form);
    } else if (SAME_OBJ(tag, ovr_symbol)) {
      CheckIvarList(tag, vars, check_CanRename, form);
      next = ReadItemList("override", vars, varOVERRIDE, next, &ivars, form);
    } else if (SAME_OBJ(tag, pri_symbol)) {
      CheckIvarList(tag, vars, 0, form);
      next = ReadItemList("private", vars, varPRIVATE, next, &ivars, form);
    } else if (SAME_OBJ(tag, inh_symbol)) {
      CheckIvarList(tag, vars, check_CanRename | check_MustId, form);
      next = ReadItemList("inherit", vars, varINHERIT, next, &ivars, form);
    } else if (SAME_OBJ(tag, ren_symbol)) {
      CheckIvarList(tag, vars, check_MustPair | check_BindingMustId, form);
      next = ReadItemList("rename", vars, varRENAME, next, &ivars, form);
    } else {
      if (SCHEME_SYMBOLP(tag))
	scheme_wrong_syntax("class*", tag, form, 
			    "bad syntax (bad clause keyword)");
      else
	scheme_wrong_syntax("class*", tag, form, 
			    "bad syntax (clause keyword missing)");
    } 

    l = SCHEME_CDR(l);
  }

  /* Get lists of names (preserves order) */
  pub = GetNames(ivars, 1, 0, 0, NULL);
  priv = GetNames(ivars, 0, 1, 0, NULL);
  ref = GetNames(ivars, 0, 0, 1, NULL);

  prienv = scheme_add_compilation_frame(priv, env, SCHEME_LAMBDA_FRAME | SCHEME_AUTO_UNBOX);
  firstenv = prienv;
  refenv = scheme_add_compilation_frame(ref, prienv, SCHEME_AUTO_UNBOX);
  pubenv = scheme_add_compilation_frame(pub, refenv, SCHEME_AUTO_UNBOX);

  /* References are not settable: */
  GetNames(ivars, 0, 0, 1, refenv);

  /* Initialize distinct name checks with ivars: */
  r = scheme_begin_dup_symbol_check(&dcrec);
  er = scheme_begin_dup_symbol_check(&erec);
  item = ivars;
  while (item) {
    if (item->vartype != varNOTHING) {
      scheme_dup_symbol_check(r, "class*", IVAR_INT_NAME(item), "internal ivar or initialization variable", form, 0);
      if (ispublic(item))
	scheme_dup_symbol_check(er, "class*", IVAR_EXT_NAME(item), "external ivar", form, 0);
    }
    item = item->next;
  }
  
  /* Add `super-init' */
  objl = cons(superinitname, scheme_null);
  scheme_dup_symbol_check(r, "class*", superinitname, "internal ivar or `super-init' variable", form, 0);

  /* Add `this' */
  objl = cons(thisname, objl);
  scheme_dup_symbol_check(r, "class*", thisname, "internal ivar or `this' variable", form, 0);

  /* Add input args (they end up in reverse order in the environment) */
  item = ivars;
  while (item && isinput(item)) {
    objl = cons(item->name, objl);
    item = item->next;
  }

  /* scheme_end_dup_symbol_check(); */

  objenv = scheme_add_compilation_frame(objl, pubenv, SCHEME_AUTO_UNBOX);

  if (!rec) {
    /* Just expanding: */
    Scheme_Object *first, *last, *il, *expanded_args;

    superclass = scheme_expand_expr(superclass, env, depth);

    {
      int i;
      for (i = 0; i < num_interfaces; i++) {
	Scheme_Object *ei;
	ei = scheme_expand_expr(interfaces[i], env, depth);
	interfaces[i] = ei;
      }
      
      il = scheme_null;
      for (i = num_interfaces; i--; ) {
	il = cons(interfaces[i], il);
      }
    }

    l = make_args;
    first = scheme_null;
    last = NULL;
    while (!SCHEME_NULLP(l)) {
      Scheme_Object *pr;

      if (SCHEME_PAIRP(l)) {
	Scheme_Object *arg;

	arg = SCHEME_CAR(l);
	l = SCHEME_CDR(l);

	if (SCHEME_PAIRP(arg)) {
	  arg = scheme_make_pair(SCHEME_CAR(arg),
				 scheme_expand_expr(SCHEME_CDR(arg),
						    objenv, depth));
	}
	
	pr = scheme_make_pair(arg, scheme_null);
      } else {
	pr = l;
	l = scheme_null;
      }

      if (last)
	SCHEME_CDR(last) = pr;
      else
	first = pr;
      last = pr;
    }
    expanded_args = first;

    l = SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(form)))));
    first = scheme_null;
    last = NULL;
    while (!SCHEME_NULLP(l)) {
      vars = SCHEME_CAR(l);

      tag = SCHEME_CAR(vars);

      if (SAME_OBJ(tag, pub_symbol) 
	  || SAME_OBJ(tag, ovr_symbol) 
	  || SAME_OBJ(tag, pri_symbol))
	vars = expandall(vars, objenv, depth, 1, NULL);
      else if (SAME_OBJ(tag, seq_symbol))
	vars = cons(tag, scheme_expand_list(SCHEME_CDR(vars), objenv, depth));
      else if (SAME_OBJ(tag, inh_symbol) 
	       || SAME_OBJ(tag, ren_symbol)) {
	vars = vars;
      } else
	break;

      vars = cons(vars, scheme_null);

      if (!last)
	first = vars;
      else
	SCHEME_CDR(last) = vars;

      last = vars;
      l = SCHEME_CDR(l);
    }

    return cons(class_star_symbol, 
		cons(cons(thisname, 
			  cons(superinitname, scheme_null)),
		     cons(superclass,
			  cons(il,
			       cons(expanded_args, first)))));
  } else {
    /* Compiling: */
    Scheme_Compile_Info lam;
    Scheme_Compile_Info *recs;
    short dcs, *dcm;

    data = MALLOC_ONE_TAGGED(Class_Data);
    data->type = scheme_class_data_type;

    data->defname = rec[drec].value_name;
    scheme_compile_rec_done_local(rec, drec);

    recs = MALLOC_N_RT(Scheme_Compile_Info, num_interfaces + 1);
    scheme_init_compile_recs(rec, drec, recs, num_interfaces + 1);

    data->super_init_name = superinitname;
    {
      Scheme_Object *cs;
      cs = scheme_compile_expr(superclass, env, recs, 0);
      data->super_expr = cs;
    }
    
    {
      int i;
      for (i = 0; i < num_interfaces; i++) {
	Scheme_Object *ci;
	ci = scheme_compile_expr(interfaces[i], env, recs, i + 1);
	interfaces[i] = ci;
      }
    }

    data->num_interfaces = num_interfaces;
    data->interface_exprs = interfaces;

    scheme_merge_compile_recs(rec, drec, recs, num_interfaces + 1);

    scheme_init_lambda_rec(rec, drec, &lam, 0);
  
    CompileItemList(form, ivars, objenv, data, &lam, 0);

    scheme_merge_lambda_rec(rec, drec, &lam, 0);

    data->ivars = ivars;

    data->num_args = num_args;
    data->num_required_args = num_required_args;
    data->num_arg_vars = num_arg_vars;

    scheme_InitData(data);

    data->max_let_depth = (lam.max_let_depth
			   + data->num_arg_vars
			   + data->num_ivar
			   + data->num_private
			   + data->num_ref
			   + 2); /* this + super-init = 2 */

    scheme_env_make_closure_map(firstenv, &dcs, &dcm);
    data->closure_size = dcs;
    data->closure_map = dcm;

    return scheme_make_syntax_compile(DefineClass_Link, 
				      (Scheme_Object *)data);
  }
}

static Scheme_Object *DefineClass_Execute(Scheme_Object *form)
{
  return scheme_DefineClass_Execute(form, 0);
}

static Scheme_Object *DefineClass(Scheme_Object *form, Scheme_Comp_Env *env,
				  Scheme_Compile_Info *rec, int drec)
{
  return Do_DefineClass(form, env, rec, drec, 0);
}

static Scheme_Object *DefineClass_expand(Scheme_Object *form, Scheme_Comp_Env *env,
					 int depth)
{
  return Do_DefineClass(form, env, NULL, 0, depth);
}

/**********************************************************************/

static Scheme_Object *CV_Bundle(ClassVariable *cvar)
{
  Scheme_Object *l = scheme_null, *f;

  while (cvar) {
    if (isref(cvar))
      f = cvar->u.source.name;
    else
      f = cvar->u.value;
    l = cons(_IVAR_EXT_NAME(cvar),
	     cons(scheme_make_integer(cvar->vartype),
		  cons(f, l)));
    cvar = cvar->next;
  }

  return l;
}

#define BAD_CCD "bad compiled class definition"

static ClassVariable *CV_Unbundle(Scheme_Object *l)
{
  ClassVariable *cvar = NULL, *prev;
  Scheme_Object *v;

  while (!SCHEME_NULLP(l)) {
    prev = cvar;
    cvar = MALLOC_ONE_RT(ClassVariable);
#ifdef MZTAG_REQUIRED
    cvar->type = scheme_rt_class_var;
#endif
    cvar->next = prev;

    cvar->name = SCHEME_CAR(l);
    l = SCHEME_CDR(l);

    cvar->vartype = (short)SCHEME_INT_VAL(SCHEME_CAR(l));
    l = SCHEME_CDR(l);

    v = SCHEME_CAR(l);
    if (isref(cvar)) {
     cvar->u.source.name = v;
    } else
      cvar->u.value = v;
    l = SCHEME_CDR(l);
  }

  return cvar;
}

static Scheme_Object *write_Class_Data(Scheme_Object *obj)
{
  Class_Data *data;
  int i;
  Scheme_Object *l = scheme_null;

  data = (Class_Data *)obj;
  
  for (i = 0; i < data->num_interfaces; i++) {
    l = cons(data->interface_exprs[i], l);
  }

  return cons(CV_Bundle(data->ivars),
	      cons(data->super_expr,
		   cons(data->super_init_name,
			cons(scheme_make_integer(data->num_args),
			     cons(scheme_make_integer(data->num_required_args),
				  cons(scheme_make_integer(data->num_arg_vars),
				       cons(scheme_make_integer(data->max_let_depth),
					    cons(scheme_make_svector(data->closure_size,
								     data->closure_map),
						 cons(data->defname
						      ? data->defname
						      : scheme_null,
						      cons(scheme_make_integer(data->num_interfaces),
							   l))))))))));
}

static Scheme_Object *read_Class_Data(Scheme_Object *obj)
{
  Scheme_Object *v;
  Class_Data *data;
  int i;

  data = MALLOC_ONE_TAGGED(Class_Data);
  data->type = scheme_class_data_type;

  {
    ClassVariable *cvar;
    cvar = CV_Unbundle(SCHEME_CAR(obj));
    data->ivars = cvar;
  }
  obj = SCHEME_CDR(obj);

  data->super_expr = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  data->super_init_name = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  
  data->num_args = (short)SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  data->num_required_args = (short)SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  data->num_arg_vars = (short)SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  data->max_let_depth = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  v = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  data->closure_size = SCHEME_SVEC_LEN(v);
  data->closure_map = SCHEME_SVEC_VEC(v);
  
  data->defname = SCHEME_CAR(obj);
  if (SCHEME_NULLP(data->defname))
    data->defname = 0;
  obj = SCHEME_CDR(obj);

  data->num_interfaces = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);
  {
    Scheme_Object **sa;
    sa = MALLOC_N(Scheme_Object*, data->num_interfaces);
    data->interface_exprs = sa;
  }

  for (i = data->num_interfaces; i--; ) {
    data->interface_exprs[i] = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);
  }

  scheme_InitData(data);

  return (Scheme_Object *)data;
}

#endif
