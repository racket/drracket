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

/* This file implements the MzScheme object system. See also
   objclass.c, which implements parsing, compilation, and execution for
   `class' expressions. */

#include "schpriv.h"
#include "schrunst.h"

#ifndef NO_OBJECT_SYSTEM

/* For testing handling primitive classes: */
#define ADD_TEST_PRIM_OBJ 0

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#define INTERFACE "interface"

#define CLASS_STAR "class*"
#define CLASS_STAR_W_NAMES "class*/names"
#define CREATE_OBJECT "make-object"

#define IVAR "ivar"
#define UQ_IVAR "ivar/proc"

#define MAKE_GENERIC "make-generic"
#define UQ_MAKE_GENERIC "make-generic/proc"

#define IS_CLASS "class?"
#define IS_INTERFACE "interface?"
#define IS_OBJECT "object?"
#define IS_A "is-a?"
#define SUBCLASS "subclass?"
#define IMPLEMENTATION "implementation?"
#define INTERFACE_EXTENSION "interface-extension?"
#define IVAR_IN_INTERFACE "ivar-in-interface?"
#define INTERFACE_IVAR_NAMES "interface->ivar-names"
#define OBJECT_INTERFACE "object-interface"
#define CLASS_TO_INTERFACE "class->interface"
#define CLASS_INIT_ARITY "class-initialization-arity"
#define NULL_CLASS "object%"

#define SUPER_INIT "super-init"

static Scheme_Object *interface_symbol;

#define cons scheme_make_pair
  
typedef struct Interface_Data {
  Scheme_Type type;
  short num_names, num_supers;
  Scheme_Object **names;
  struct Scheme_Object **super_exprs;
  Scheme_Object *defname;
} Interface_Data;

typedef struct Scheme_Interface_Assembly {
  MZTAG_IF_REQUIRED
  Interface_Data data;
} Scheme_Interface_Assembly;

typedef struct Scheme_Class_Assembly {
  MZTAG_IF_REQUIRED
  Class_Data data;
  Scheme_Instance_Init_Proc *init;
  int mina, maxa;
} Scheme_Class_Assembly;

typedef struct {
  Scheme_Object **ivars;
  Scheme_Object **refs;
  Scheme_Object **cmethods; /* cached cethod closures */
} Init_Frame;

typedef struct Internal_Object {
  struct {
    Scheme_Type type;
    MZ_HASH_KEY_EX
    Scheme_Object *sclass;
  } o;
  Scheme_Object *slots[1];
} Internal_Object;

typedef struct Init_Object_Rec {
  MZTAG_IF_REQUIRED
  short init_level;
#ifdef MZ_PRECISE_GC
  short count;
#endif
  Init_Frame frames[1];
} Init_Object_Rec;

typedef struct {
  MZTAG_IF_REQUIRED
  Internal_Object *o;
  Init_Object_Rec *irec;
  int level;
} SuperInitData;

typedef struct {
  Scheme_Type type;
  short kind;
  Scheme_Object *clori;
  Scheme_Object *ivar_name;
  int vp;
} Generic_Data;

static Scheme_Class *null_class = NULL;

static Scheme_Object *MakeSuperInitPrim(Internal_Object *o, Init_Object_Rec *irec, int level);
static short *find_implementation(Scheme_Object *c, Scheme_Object *n, int *offset);

#define FindName(c, s) scheme_DoFindName(c->num_public, c->public_names, s)

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

/* Stack for object initailization (all boxed, currently):

   last private ivar
   first private ivar

   last ref ivar
   first ref ivar

   last public ivar
   first public ivar

   super-init
   this

   first init var
   last init var
   */

/**********************************************************************/

#endif /* NO_OBJECT_SYSTEM */

#if !defined(NO_OBJECT_SYSTEM) || !defined(NO_UNIT_SYSTEM)

DupCheckRecord *scheme_begin_dup_symbol_check(DupCheckRecord *r)
{
  long i;
  
  if (r && r->scheck_size > 50)
    r->scheck_size = 0;    
  else {
    for (i = r->scheck_size; i--; ) {
      r->scheck_hash[i] = NULL;
    }
  }

  if (!r) {
    r = MALLOC_ONE_RT(DupCheckRecord);
#ifdef MZTAG_REQUIRED
    r->type = scheme_rt_dup_check;
#endif
    r->scheck_size = 0;
  }

  if (!r->scheck_size) {
    r->scheck_hash = NULL;
    r->scheck_step = 17;
    r->scheck_count = 0;
  }

  return r;
}

void scheme_dup_symbol_check(DupCheckRecord *r, const char *where,
			     Scheme_Object *symbol, char *what, 
			     Scheme_Object *form, int inverted)
{
  long pos;

  if (r->scheck_count >= r->scheck_size / 2) {
    Scheme_Object **old = r->scheck_hash;
    long oldsize = r->scheck_size, i;

    r->scheck_size = r->scheck_size ? 2 * r->scheck_size : 50;
    i = r->scheck_size;
    {
      Scheme_Object **sa;
      sa = MALLOC_N(Scheme_Object *, i);
      r->scheck_hash = sa;
    }
    memset(r->scheck_hash, 0, i);

    r->scheck_count = 0;
    for (i = 0; i < oldsize; i++) {
      if (old[i])
	scheme_dup_symbol_check(r, where, old[i], what, form, 0);
    }
  }

  pos = (scheme_hash_key(symbol) >> 2) % r->scheck_size;
  if (pos < 0)
    pos = -pos;
  while(r->scheck_hash[pos]) {
    if (SAME_OBJ(r->scheck_hash[pos], symbol)) {
      if (inverted)
	return;
      scheme_wrong_syntax(where, symbol, form,
			  "duplicate %s name", what);
    }
    pos = (pos + r->scheck_step) % r->scheck_size;
  }

  if (inverted > 0)
    scheme_wrong_syntax(where, symbol, form,
			"undeclared %s name", what);

  r->scheck_hash[pos] = symbol;
  r->scheck_count++;
}

#endif

#ifndef NO_OBJECT_SYSTEM

void scheme_install_class_interface(Scheme_Class *sclass)
{
  Scheme_Interface *in;

  in = MALLOC_ONE_TAGGED(Scheme_Interface);
  in->type = scheme_interface_type;
  in->num_supers = 0;
  in->supers = NULL;
  in->super_offsets = NULL;

  in->defname = sclass->defname;

  in->num_names = 0;
  in->names = NULL;
  in->name_map = NULL;
  
  in->for_class = 1;

  in->supclass = sclass;
  sclass->equiv_intf = in;
}

static Scheme_Object *NullClass(void) 
{
  if (!null_class) {
    null_class = MALLOC_ONE_TAGGED(Scheme_Class);
    
    null_class->type = scheme_class_type;
    
    null_class->priminit = pi_NOT;
    
    null_class->ivars = NULL;
    
    null_class->pos = 0;
    null_class->super_init_name = NULL;
    {
      Scheme_Class **ca;
      ca = MALLOC_N(Scheme_Class*,1);
      null_class->heritage = ca;
    }
    null_class->heritage[0] = null_class;
    null_class->superclass = NULL;
    
    null_class->num_args = 0;
    null_class->num_required_args = 0;
    null_class->num_arg_vars = 0;
    null_class->num_public = 0;
    null_class->num_private = 0;
    null_class->num_ref = 0;
    null_class->public_names = NULL;
    null_class->cmethods = NULL;
    null_class->cmethod_ready_level = NULL;
    null_class->cmethod_source_map = NULL;
    null_class->contributes_cmethods = FALSE;

    {
      Scheme_Object *s;
      s = scheme_intern_symbol(NULL_CLASS);
      null_class->defname = s;
    }
    
    null_class->closure_saved = NULL;
    null_class->closure_size = 0;
    null_class->max_let_depth = 0;

    scheme_install_class_interface(null_class);
  }

  return (Scheme_Object *)null_class;
}

/**********************************************************************/

int scheme_CompareObjectPtrs(Scheme_Object **a, Scheme_Object **b)
{
  long ha, hb;
  
  /* Can't just subtract hb from ha because it might overflow: */
  ha = scheme_hash_key(*a);
  hb = scheme_hash_key(*b);
  if (ha < hb)
    return -1;
  else if (ha == hb)
    return 0;
  else
    return 1;
}

int scheme_MergeArray(int ac, Scheme_Object **ak, Scheme_Object **a, short *as,
		      int bc, Scheme_Object **bk, Scheme_Object **b, short *bs,
		      Scheme_Object **d, short *ds, int nodup)
{
  int ai = 0, bi = 0, di = 0;

  while ((ai < ac) || (bi < bc)) {
    if ((ai < ac) && (bi < bc)) {
      if (SEQUALS(ak[ai], bk[bi])) {
	if (nodup == 2) {
	  /* Error: interface adds names that are already in the superinterfaces. */
	  scheme_raise_exn(MZEXN_OBJECT,
			   "interface: superinterface already contains name: %S",
			   ak[ai]);
	  return 0;
	}
	if (d)
	  d[di] = a[ai];
	if (ds)
	  ds[di] = as[ai];
	ai++;
	if (nodup)
	  bi++;
      } else if (SLESSTHAN(ak[ai], bk[bi])) {
	if (d)
	  d[di] = a[ai];
	if (ds)
	  ds[di] = as[ai];
	ai++;
      } else {
	if (d)
	  d[di] = b[bi];
	if (ds)
	  ds[di] = bs[bi];
	bi++;
      }
    } else if (ai < ac) {
      if (d)
	d[di] = a[ai];
      if (ds)
	ds[di] = as[ai];
      ai++;
    } else {
      if (d)
	d[di] = b[bi];
      if (ds)
	ds[di] = bs[bi];
      bi++;
    }
    di++;
  }

  return di;
}

/**********************************************************************/

static Scheme_Object *Interface_Execute(Scheme_Object *form)
{
  Scheme_Object *il;
  Interface_Data *data;
  Scheme_Interface *in, **supers;
  int num_supers, num_names, i;

  data = (Interface_Data *)form;

  /* Build a list instead of allocating an array immediately.
     Why? Suppose that the continuation is grabbed during the
     evaluation of an interface expression... */
  num_supers = data->num_supers;
  il = scheme_null;
  for (i = 0; i < num_supers; i++) {
    Scheme_Object *sin;
    sin = _scheme_eval_compiled_expr(data->super_exprs[i]);
    if (SCHEME_INTERFACEP(sin)) {
      il = cons(sin, il);
    } else {
      const char *symname;
      symname = (data->defname ? scheme_symbol_name(data->defname) : "");
      scheme_raise_exn(MZEXN_OBJECT,
		       "interface: interface expression returned "
		       "a non-interface: %V%s%s",
		       sin,
		       data->defname ? " for interface: " : "",
		       symname);
      return NULL;
    }
  }

  in = MALLOC_ONE_TAGGED(Scheme_Interface);
  in->type = scheme_interface_type;

  in->defname = data->defname;

  supers = MALLOC_N(Scheme_Interface*, num_supers);
  for (i = num_supers; i--; ) {
    supers[i] = (Scheme_Interface *)SCHEME_CAR(il);
    il = SCHEME_CDR(il);
  }
  in->num_supers = num_supers;
  in->supers = supers;

  /* Check consitency of superinterface classes: */
  {
    Scheme_Interface *source = NULL;
    in->supclass = null_class;
    for (i = 0; i < num_supers; i++) {
      if (!scheme_is_subclass((Scheme_Object *)in->supclass, (Scheme_Object *)supers[i]->supclass)) {
	if (!scheme_is_subclass((Scheme_Object *)supers[i]->supclass, (Scheme_Object *)in->supclass)) {
	  scheme_raise_exn(MZEXN_OBJECT,
			   "interface: inconsistent implementation requirements "
			   "between superinterfaces: %s and %s",
			   scheme_make_provided_string((Scheme_Object *)source, 2, NULL),
			   scheme_make_provided_string((Scheme_Object *)supers[i], 2, NULL));
	  return NULL;
	}
	in->supclass = supers[i]->supclass;
	source = supers[i];
      }
    }
  }
  in->for_class = 0;

  if (num_supers) {
    Scheme_Object **nbanka, **nbankb;
    short *mbanka, *mbankb;
    int total, mode = 1, newcount, offset;

    total = 0;
    for (i = num_supers; i--; ) {
      total += supers[i]->num_names; 
    }
    
    nbanka = MALLOC_N(Scheme_Object*, total);
    nbankb = MALLOC_N(Scheme_Object*, total);
    mbanka = MALLOC_N_ATOMIC(short, total);
    mbankb = MALLOC_N_ATOMIC(short, total);

    num_names = supers[num_supers - 1]->num_names;
    for (i = num_names; i--; ) {
      nbanka[i] = supers[num_supers - 1]->names[i];
      mbanka[i] = supers[num_supers - 1]->name_map[i];
    }

    /* Merge superclasses, keeping duplicates.
       Merge name map in parallel.
       Offset name maps according to superinterface offsets. */
    for (i = num_supers - 1; i--; ) {
      int j, count;
      short *mbank;

      count = supers[i]->num_names;
      mbank = mode ? mbanka : mbankb;
      for (j = 0; j < num_names; j++) {
	mbank[j] += count;
      }

      scheme_MergeArray(num_names, mode ? nbanka : nbankb, mode ? nbanka : nbankb, NULL,
			count, supers[i]->names, supers[i]->names, NULL,
			mode ? nbankb : nbanka, NULL, 0);
      num_names = scheme_MergeArray(num_names, mode ? nbanka : nbankb, NULL, mode ? mbanka : mbankb, 
				    count, supers[i]->names, NULL, supers[i]->name_map,
				    NULL, mode ? mbankb : mbanka, 0);

      mode = !mode;
    }

    if (!mode) {
      nbanka = nbankb;
      mbanka = mbankb;
    }

    /* Final merge: */
    total = scheme_MergeArray(data->num_names, data->names, NULL, NULL,
			      num_names, nbanka, NULL, NULL,
			      NULL, NULL, 2); /* 2 => must be disjoint */
    newcount = total - num_names;

    /* Check for variables already in supclass */
    {
      int count, j, k;
      Scheme_Object **a;
      count = in->supclass->num_public;
      a = in->supclass->public_names;
      j = k = 0;
      while ((j < count) && (k < data->num_names)) {
	if (SEQUALS(a[j], data->names[k])) {
	  scheme_raise_exn(MZEXN_OBJECT,
			   "interface: superinterface already contains name: %S",
			   a[j]);
	} else if (SLESSTHAN(a[j], data->names[k]))
	  j++;
	else
	  k++;
      }
    }
    
    mbankb = MALLOC_N_ATOMIC(short, data->num_names);
    /* init to -1; we'll go back and set the numbers later */
    for (i = data->num_names; i--; ) {
      mbankb[i] = -1;
    }
    /* offset by number of names to be added */
    for (i = num_names; i--; ) {
      mbanka[i] += newcount;
    }

    in->num_names = total;
    {
      Scheme_Object **sa;
      sa = MALLOC_N(Scheme_Object*, total);
      in->names = sa;
    }
    {
      short *sa;
      sa = MALLOC_N_ATOMIC(short, total);
      in->name_map = sa;
    }

    scheme_MergeArray(data->num_names, data->names, data->names, NULL,
		      num_names, nbanka, nbanka, NULL,
		      in->names, NULL, 1);
    scheme_MergeArray(data->num_names, data->names, NULL, mbankb, 
		      num_names, nbanka, NULL, mbanka,
		      NULL, in->name_map, 1);

    /* renumber -1s to distinct new numbers (0 to newcount): */
    {
      int j;
      j = 0;
      for (i = total; i--; ) {
	if (in->name_map[i] < 0)
	  in->name_map[i] = j++;
      }
    }

    /* Count total supers: */
    total = 0;
    for (i = num_supers; i--; ) {
      total += supers[i]->num_supers + 1;
    }
    in->num_supers = total;
    {
      Scheme_Interface **ia;
      ia = MALLOC_N(Scheme_Interface*, total);
      in->supers = ia;
    }
    {
      short *sa;
      sa = MALLOC_N_ATOMIC(short, total);
      in->super_offsets = sa;
    }
    {
      int j;
      j = 0;
      offset = newcount;
      for (i = 0; i < num_supers; i++) {
	int k;
	in->supers[j] = supers[i];
	in->super_offsets[j] = offset;
	j++;
	for (k = 0; k < supers[i]->num_supers; k++) {
	  in->supers[j] = supers[i]->supers[k];
	  in->super_offsets[j] = supers[i]->super_offsets[k] + offset;
	  j++;
	}
	offset += supers[i]->num_names;
      }
    }
  } else {
    num_names = data->num_names;
    in->num_names = num_names;
    in->names = data->names;
    {
      short *sa;
      sa = MALLOC_N_ATOMIC(short, in->num_names);
      in->name_map = sa;
    }
    for (i = num_names; i--; ) {
      in->name_map[i] = i;
    }
    in->num_supers = 0;
    in->supers = NULL;
    in->super_offsets = NULL;
  }

  return (Scheme_Object *)in;
}

static Scheme_Object *Interface_Link(Scheme_Object *form, Link_Info *info)
{
  Interface_Data *data;
  int i;

  data = (Interface_Data *)form;

  for (i = data->num_supers; i--; ) {
    Scheme_Object *le;
    le = scheme_link_expr(data->super_exprs[i], info);
    data->super_exprs[i] = le;
  }

  return scheme_make_syntax_link(Interface_Execute, 
				 (Scheme_Object *)data);
}

static Scheme_Object *Do_Interface(Scheme_Object *form, Scheme_Comp_Env *env,
				   Scheme_Compile_Info *rec, int drec, int depth)
{
  int i, num_supers, num_names;
  Scheme_Object *l, *sl, **supers, *nl;
  DupCheckRecord *r, dcrec;

  l = SCHEME_CDR(form);
  
  if (!SCHEME_LISTP(l))
    scheme_wrong_syntax(INTERFACE, NULL, form, NULL);
  if (SCHEME_NULLP(l))
    scheme_wrong_syntax(INTERFACE, NULL, form, 
			"missing superinterface specification");
  
  sl = SCHEME_CAR(l);
  num_supers = 0;
  while (SCHEME_PAIRP(sl)) {
    sl = SCHEME_CDR(sl);
    num_supers++;
  }

  if (!SCHEME_NULLP(sl)) {
    scheme_wrong_syntax(INTERFACE, sl, form, 
			"bad superinterface specification");
    return NULL;
  }

  supers = MALLOC_N(Scheme_Object*,num_supers);
  sl = SCHEME_CAR(l);
  for (i = 0; i < num_supers; i++, sl = SCHEME_CDR(sl)) {
    supers[i] = SCHEME_CAR(sl);
  }

  l = SCHEME_CDR(l);
  nl = l;

  dcrec.scheck_size = 0;
  r = scheme_begin_dup_symbol_check(&dcrec);

  num_names = 0;
  while (SCHEME_PAIRP(l)) {
    Scheme_Object *s = SCHEME_CAR(l);
    num_names++;
    scheme_check_identifier(INTERFACE, s, NULL, env, form);
    scheme_dup_symbol_check(r, INTERFACE, s, "identifier", form, 0);
    l = SCHEME_CDR(l);
  }

  if (!SCHEME_NULLP(l)) {
    scheme_wrong_syntax(INTERFACE, l, form, 
			"bad name sequence");
    return NULL;
  }

  if (!rec) {
    /* Expanding */
    for (i = 0; i < num_supers; i++) {
      Scheme_Object *ee;
      ee = scheme_expand_expr(supers[i], env, depth);
      supers[i] = ee;
    }
    l = scheme_null;
    for (i = num_supers; i--; ) {
      l = cons(supers[i], l);
    }

    return cons(interface_symbol, cons(l, nl));
  } else {
    /* Compiling */
    Interface_Data *data;
    Scheme_Compile_Info *recs;

    data = MALLOC_ONE_TAGGED(Interface_Data);
    data->type = scheme_interface_data_type;

    data->defname = rec[drec].value_name;
    scheme_compile_rec_done_local(rec, drec);

    data->num_names = num_names;
    {
      Scheme_Object **sa;
      sa = MALLOC_N(Scheme_Object*, num_names);
      data->names = sa;
    }
    l = nl;
    for (i = 0; i < num_names; i++, nl = SCHEME_CDR(nl)) {
      data->names[i] = SCHEME_CAR(nl);
    }

    /* Sort names: */
    qsort((char *)data->names, num_names,
	  sizeof(Scheme_Object *), 
	  (Compare_Proc)scheme_CompareObjectPtrs); 

    recs = MALLOC_N_RT(Scheme_Compile_Info, num_supers);
    scheme_init_compile_recs(rec, drec, recs, num_supers);

    for (i = 0; i < num_supers; i++) {
      Scheme_Object *ce;
      ce = scheme_compile_expr(supers[i], env, recs, i);
      supers[i] = ce;
    }

    scheme_merge_compile_recs(rec, drec, recs, num_supers);

    data->num_supers = num_supers;
    data->super_exprs = supers;

    return scheme_make_syntax_compile(Interface_Link, 
				      (Scheme_Object *)data);
  }
}

static Scheme_Object *Interface(Scheme_Object *form, Scheme_Comp_Env *env,
				Scheme_Compile_Info *rec, int drec)
{
  return Do_Interface(form, env, rec, drec, 0);
}

static Scheme_Object *Interface_expand(Scheme_Object *form, Scheme_Comp_Env *env,
				       int depth)
{
  return Do_Interface(form, env, NULL, 0, depth);
}

/************************************************************/

static int CompareSymbolNames(Scheme_Object **a, Scheme_Object **b)
{
  return strcmp(SCHEME_SYM_VAL(*a), SCHEME_SYM_VAL(*b));
}

static Scheme_Object *write_Interface_Data(Scheme_Object *obj)
{
  Interface_Data *data;
  int i;
  Scheme_Object *l = scheme_null, **sortednames;

  data = (Interface_Data *)obj;
  
  for (i = 0; i < data->num_supers; i++) {
    l = cons(data->super_exprs[i], l);
  }

  /* The names were alreadyed sort by pointer value. We re-sort by
     string val so that the output is consistent across runs and
     platforms. After reading back in, the names are resorted. */
  sortednames = MALLOC_N(Scheme_Object *, data->num_names);
  memcpy(sortednames, data->names, data->num_names * sizeof(Scheme_Object *));
  qsort((char *)sortednames, data->num_names,
	sizeof(Scheme_Object *), 
	(Compare_Proc)CompareSymbolNames); 

  for (i = 0; i < data->num_names; i++) {
    l = cons(sortednames[i], l);
  }
  
  return cons(scheme_make_integer(data->num_names),
	      cons(scheme_make_integer(data->num_supers),
		   cons(data->defname
			? data->defname
			: scheme_null,
			l)));
}

static Scheme_Object *read_Interface_Data(Scheme_Object *obj)
{
  Interface_Data *data;
  int i;

  data = MALLOC_ONE_TAGGED(Interface_Data);
  data->type = scheme_interface_data_type;

  data->num_names = (short)SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  data->num_supers = (short)SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  data->defname = SCHEME_CAR(obj);
  if (SCHEME_NULLP(data->defname))
    data->defname = 0;
  obj = SCHEME_CDR(obj);

  {
    Scheme_Object **sa;
    sa = MALLOC_N(Scheme_Object*, data->num_names);
    data->names = sa;
    sa = MALLOC_N(Scheme_Object*, data->num_supers);
    data->super_exprs = sa;
  }

  for (i = data->num_names; i--; obj = SCHEME_CDR(obj)) {
    data->names[i] = SCHEME_CAR(obj);
  }

  for (i = data->num_supers; i--; obj = SCHEME_CDR(obj)) {
    data->super_exprs[i] = SCHEME_CAR(obj);
  }

  /* Sort names: */
  qsort((char *)data->names, data->num_names,
	sizeof(Scheme_Object *), 
	(Compare_Proc)scheme_CompareObjectPtrs); 

  return (Scheme_Object *)data;
}

/**********************************************************************/

Scheme_Object *scheme_make_class(const char *name, Scheme_Object *sup, 
				 Scheme_Method_Prim *initf, int num_methods)
{
  Scheme_Class *sclass, *superclass;
  int i;

  if (sup && !SCHEME_CLASSP(sup))
    scheme_signal_error("make-prim-class: bad superclass");

  sclass = MALLOC_ONE_TAGGED(Scheme_Class);

  sclass->type = scheme_class_type;

  if (!sup)
    sup = NullClass();

  superclass = (Scheme_Class *)sup;

  scheme_InstallHeritage(sclass, superclass);

  num_methods += superclass->num_public;

  sclass->num_public = num_methods;
  {
    Scheme_Object **sa;
    sa = MALLOC_N(Scheme_Object*, num_methods);
    sclass->public_names = sa;
  }
  {
    CMethod **cma;
    cma = MALLOC_N(CMethod*, num_methods);
    sclass->cmethods = cma;
  }
  {
    short *sa;
    sa = MALLOC_N_ATOMIC(short, num_methods);
    sclass->cmethod_ready_level = sa;
    sa = MALLOC_N_ATOMIC(short, num_methods);
    sclass->cmethod_source_map = sa;
    sa = MALLOC_N_ATOMIC(short, num_methods);
    sclass->public_map = sa;
  }
  sclass->contributes_cmethods = 0;

  /* Copy superclass methods: */
  for (i = superclass->num_public; i--; ) {
    sclass->public_names[i] = superclass->public_names[i];
    sclass->cmethods[i] = superclass->cmethods[i];
    sclass->cmethod_ready_level[i] = superclass->cmethod_ready_level[i];
    sclass->cmethod_source_map[i] = -1;
    sclass->public_map[i] = superclass->public_map[i];
  }
  sclass->num_private = superclass->num_public;

  sclass->super_init_name = NULL;

  sclass->ivars = NULL;

  sclass->priminit = pi_CPP;
  sclass->piu.initf = (Scheme_Closed_Prim *)initf;

  sclass->num_args = -1; /* Irrelevant; really defined by initf */
  sclass->num_required_args = -1;
  sclass->num_arg_vars = -1;

  sclass->num_ref = 0;
  sclass->num_ivar = 0;
  sclass->num_slots = EXTRA_PRIM_SLOTS;

  sclass->closure_size = 0;
  sclass->closure_saved = NULL;
  sclass->max_let_depth = 0;

  {
    Scheme_Object *s;
    s = scheme_intern_exact_symbol(name, strlen(name));
    sclass->defname = s;
  }

  scheme_install_class_interface(sclass);

  return (Scheme_Object *)sclass;
}

const char *scheme_iget_class_name(Scheme_Object *sclass, const char *mode)
{
  Scheme_Class *c = (Scheme_Class *)sclass;
  const char *cl, *dn;
  char *r;
  int size;

  if (c->defname) {
    dn = scheme_symbol_name(c->defname);
    cl = NULL;
  } else {
    dn = NULL;
    cl = NULL;
    while (c) {
      c = c->superclass;
      if (c && c->defname) {
	cl = scheme_symbol_name(c->defname);
	break;
      }
    }
  }

  if (!cl && !dn)
    return "";

  size = (mode ? strlen(mode) : 0) + (cl ? strlen(cl) : 0) + (dn ? strlen(dn) : 0) + 20;
  r = (char *)scheme_malloc_atomic(size);
  
  sprintf(r, "%s%s%s%s",
	  mode ? mode : "",
	  dn ? dn : "",
	  cl ? "derived from " : "",
	  cl ? cl : "");

  return r;
}

const char *scheme_iget_interface_name(Scheme_Object *i, const char *mode)
{
  Scheme_Object *n;

  n = ((Scheme_Interface *)i)->defname;
  if (n) {
    const char *name;
    char *s;
    int len;
    int mlen = strlen(mode);

    name = scheme_symbol_name(n);
    len = strlen(name);
    s = (char *)scheme_malloc_atomic(len + mlen + 1);
    memcpy(s, mode, mlen);
    memcpy(s + mlen, name, len);
    s[len + mlen] = 0;

    return s;
  } else
    return "";
}

const char *scheme_get_class_name(Scheme_Object *c, int *len)
{
  Scheme_Object *n;

  if (!SCHEME_CLASSP(c))
    return NULL;

  n = ((Scheme_Class *)c)->defname;
  if (n) {
    *len = SCHEME_SYM_LEN(n);
    return scheme_symbol_val(n);
  } else
    return NULL;
}

const char *scheme_get_interface_name(Scheme_Object *i, int *len)
{
  Scheme_Object *n;

  if (!SCHEME_INTERFACEP(i))
    return NULL;

  n = ((Scheme_Interface *)i)->defname;
  if (n) {
    *len = SCHEME_SYM_LEN(n);
    return scheme_symbol_val(n);
  } else
    return NULL;
}

void scheme_add_method_w_arity(Scheme_Object *c, const char *name,
			       Scheme_Method_Prim *f, 
			       int mina, int maxa)
{
  Scheme_Object *s;
  Scheme_Class *sclass;
  CMethod *cmethod;
  int len, len2, i, count;

  if (!SCHEME_CLASSP(c))
    return;
  
  sclass = (Scheme_Class *)c;

  if (sclass->num_private >= sclass->num_public)
    scheme_signal_error("add-prim-method: added too many methods");

  cmethod = MALLOC_ONE_RT(CMethod);
#ifdef MZTAG_REQUIRED
  cmethod->type = scheme_rt_class_method;
#endif
  cmethod->f = (Scheme_Closed_Prim *)f;
  cmethod->mina = mina;
  cmethod->maxa = maxa;

  len = SCHEME_SYM_LEN(sclass->defname);
  len2 =  strlen(name);
  {
    char *ca;
    ca = (char *)scheme_malloc_atomic(len + len2 + 3);
    cmethod->closed_name = ca;
  }
  memcpy(cmethod->closed_name, SCHEME_SYM_VAL(sclass->defname), len);
  cmethod->closed_name[len] = ':';
  cmethod->closed_name[len + 1] = ':';
  memcpy(cmethod->closed_name + len + 2, name, len2);
  cmethod->closed_name[len + len2 + 2] = 0;

  s = scheme_intern_symbol(name);

  count = sclass->num_private;

  for (i = 0; i <= count; i++) {
    if ((i == count) || SLESSTHAN(s, sclass->public_names[i])) {
      /* Insert here: */
      int j;
      for (j = sclass->num_private; j > i; --j) {
	sclass->public_names[j] = sclass->public_names[j - 1];
	sclass->public_map[j] = sclass->public_map[j - 1];
      }
      sclass->public_names[i] = s;
      sclass->public_map[i] = count;
      sclass->cmethods[count] = cmethod;
      sclass->cmethod_ready_level[count] = sclass->pos;
      sclass->cmethod_source_map[count] = sclass->contributes_cmethods;
      sclass->num_private++;
      break;
    } else if (SEQUALS(s, sclass->public_names[i])) {
      /* Override */
      int cindex = sclass->public_map[i];
      sclass->cmethods[cindex] = cmethod;
      sclass->cmethod_ready_level[cindex] = sclass->pos;
      sclass->cmethod_source_map[cindex] = sclass->contributes_cmethods;
      break;
    }
  }

  sclass->contributes_cmethods++;
}

void scheme_add_method(Scheme_Object *c, const char *name,
		       Scheme_Method_Prim *f)
{
  scheme_add_method_w_arity(c, name, f, 0, -1);
}

static void InstallInterface(Scheme_Class *sclass, Scheme_Interface *in)
{
  int j, k = 0, num_public, c;
  short *imap, **save;
  Scheme_Interface **isave;

  c = sclass->num_interfaces;;
  sclass->num_interfaces = c + 1;
  save = sclass->interface_maps;
  {
    short **sa;
    sa = MALLOC_N(short*, (c+1));
    sclass->interface_maps = sa;
  }
  for (j = 0; j < c; j++) {
    sclass->interface_maps[j] = save[j];
  }
  isave = sclass->interfaces;
  {
    Scheme_Interface **ia;
    ia = MALLOC_N(Scheme_Interface*, (c+1));
    sclass->interfaces = ia;
  }
  for (j = 0; j < c; j++) {
    sclass->interfaces[j] = isave[j];
  }
  sclass->interfaces[c] = in;
  imap = MALLOC_N_ATOMIC(short, in->num_names);
  sclass->interface_maps[c] = imap;

  num_public = sclass->num_public;

  for (j = 0; j < in->num_names; j++) {
    while ((k < num_public) && SLESSTHAN(sclass->public_names[k], in->names[j])) {
      k++;
    }
    imap[in->name_map[j]] = sclass->public_map[k];
  }
}

void scheme_made_class(Scheme_Object *c)
{
  Scheme_Class *sclass;
  int i, num_methods;

  sclass = (Scheme_Class *)c;

  num_methods = sclass->num_private;
  sclass->num_private = 0;
  sclass->num_public = num_methods;

  {
    short *sa;
    sa = MALLOC_N_ATOMIC(short, num_methods);
    sclass->vslot_map = sa;
  }
  {
    slotkind *ska;
    ska = MALLOC_N_ATOMIC(slotkind, num_methods);
    sclass->vslot_kind = ska;
  }

  for (i = 0; i < num_methods; i++) {
    sclass->vslot_map[i] = i;
    sclass->vslot_kind[i] = slot_TYPE_CMETHOD;
  }

  if (sclass->superclass && sclass->superclass->num_interfaces) {
    for (i = 0; i < sclass->superclass->num_interfaces; i++) {
      InstallInterface(sclass, sclass->superclass->interfaces[i]);
    }
  }
}

Scheme_Object* scheme_class_to_interface(Scheme_Object *c, char *name)
{
  Scheme_Class *sclass = (Scheme_Class *)c;
  Scheme_Object *s;

  s = scheme_intern_symbol(name);

  sclass->equiv_intf->defname = s;
  
  return (Scheme_Object *)sclass->equiv_intf;
}

/**********************************************************************/

Scheme_Class_Assembly *
scheme_make_class_assembly(const char *name, int num_interfaces,
			   int n_public, Scheme_Object **publics,
			   int n_override, Scheme_Object **overrides,
			   int n_inh, Scheme_Object **inheriteds,
			   int n_ren, Scheme_Object **renames,
			   int mina, int maxa,
			   Scheme_Instance_Init_Proc *initproc)
{
  Scheme_Class_Assembly *a;
  int i;
  ClassVariable *v, *last = NULL;

  a = MALLOC_ONE_RT(Scheme_Class_Assembly);
#ifdef MZTAG_REQUIRED
  a->type = scheme_rt_class_assembly;
#endif
  
  a->mina = mina;
  a->maxa = maxa;
  a->init = initproc;

  for (i = 0; i < n_public; i++) {
    v = MALLOC_ONE_RT(ClassVariable);
#ifdef MZTAG_REQUIRED
    v->type = scheme_rt_class_var;
#endif

    v->vartype = varPUBLIC;
    v->name = publics[i];
    v->u.value = scheme_undefined;
    v->next = last;

    last = v;
  }

  for (i = 0; i < n_override; i++) {
    v = MALLOC_ONE_RT(ClassVariable);
#ifdef MZTAG_REQUIRED
    v->type = scheme_rt_class_var;
#endif

    v->vartype = varOVERRIDE;
    v->name = overrides[i];
    v->u.value = scheme_undefined;
    v->next = last;

    last = v;
  }

  for (i = 0; i < n_inh; i++) {
    v = MALLOC_ONE_RT(ClassVariable);
#ifdef MZTAG_REQUIRED
    v->type = scheme_rt_class_var;
#endif

    v->vartype = varINHERIT;
    
    v->name = inheriteds[i];
    v->u.source.name = inheriteds[i];
    v->next = last;

    last = v;
  }

  for (i = 0; i < n_ren; i++) {
    v = MALLOC_ONE_RT(ClassVariable);
#ifdef MZTAG_REQUIRED
    v->type = scheme_rt_class_var;
#endif

    v->vartype = varRENAME;
    
    v->name = renames[i];
    v->u.source.name = renames[i];
    v->next = last;

    last = v;
  }

  a->data.type = 0;

  a->data.ivars = last;

  a->data.num_args = a->data.num_required_args = a->data.num_arg_vars = 0;
  
  a->data.super_init_name = NULL;

  a->data.num_interfaces = num_interfaces;

  if (name) {
    Scheme_Object *s;
    s = scheme_intern_exact_symbol(name, strlen(name));
    a->data.defname = s;
  } else
    a->data.defname = NULL;

  scheme_InitData(&a->data);

  return a;
}

Scheme_Object *scheme_create_class(Scheme_Class_Assembly *a,
				   void *data,
				   Scheme_Object *super,
				   Scheme_Object **interfaces)
{
  Scheme_Object *o;
  Scheme_Class *cl;

  if (super)
    a->data.super_expr = super;
  else {
    super = NullClass();
    a->data.super_expr = super;
  }

  a->data.interface_exprs = interfaces;

  o = scheme_DefineClass_Execute((Scheme_Object *)&a->data, TRUE);

  cl = (Scheme_Class *)o;
  if (cl->priminit == pi_NOT)
    cl->priminit = pi_COMP;
  else
    cl->priminit = pi_COMP_OVER_CPP;
  cl->piu.insti.f = a->init;
  cl->piu.insti.data = data;

  cl->num_required_args = a->mina;
  cl->num_args = a->maxa;

  return o;
}

Scheme_Interface_Assembly *
scheme_make_interface_assembly(const char *name, int n_supers, int n_names, Scheme_Object **names)
{
  Scheme_Interface_Assembly *a;
  int i;

  a = MALLOC_ONE_RT(Scheme_Interface_Assembly);

#ifdef MZTAG_REQUIRED
  a->type = scheme_rt_class_assembly;
#endif

  a->data.num_names = n_names;
  {
    Scheme_Object **na;
    na = MALLOC_N(Scheme_Object *, n_names);
    a->data.names = na;
  }
  for (i = n_names; i--; ) {
    a->data.names[i] = names[i];
  }

  a->data.num_supers = n_supers;
  if (name) {
    Scheme_Object *s;
    s = scheme_intern_exact_symbol(name, strlen(name));
    a->data.defname = s;
  } else
    a->data.defname = NULL;

  /* Sort names: */
  qsort((char *)a->data.names, n_names,
	sizeof(Scheme_Object *), 
	(Compare_Proc)scheme_CompareObjectPtrs); 

  return a;
}

Scheme_Object *scheme_create_interface(Scheme_Interface_Assembly *a,
				       Scheme_Object **supers)
{
  a->data.super_exprs = supers;

  return Interface_Execute((Scheme_Object *)&a->data);
}


/**********************************************************************/

static Scheme_Object *CloseMethod(CMethod *cmethod, Internal_Object *obj)
{
  return scheme_make_closed_prim_w_arity(cmethod->f,
					 (void *)obj,
					 cmethod->closed_name,
					 cmethod->mina,
					 cmethod->maxa);
}

int scheme_DoFindName(int num_public, Scheme_Object **pn, Scheme_Object *symbol)
{
  int p, w, o;
  Scheme_Object *n;

  if (!num_public)
    return -1;

  o = 0;
  p = num_public >> 1;
  w = num_public;

  while (1) {
    n = pn[p];
    if (SAME_OBJ(n, symbol))
      return p;
    if (w == 1)
      return -1;
    if (scheme_hash_key(n) < scheme_hash_key(symbol)) {
      w = o + w - p;
      o = p;
    } else
      w = p - o;
    p = o + (w >> 1);
  }

  return p;
}

static Scheme_Object *GetIvar(Internal_Object *obj, Init_Object_Rec *irec, 
			      Scheme_Class *oclass, Scheme_Class *mclass, short vp,
			      int boxed, int force)
{
  if (mclass->vslot_kind[vp] == slot_TYPE_IVAR) {
    Scheme_Object *v = obj->slots[oclass->vslot_map[vp]];
    if (boxed)
      return v;
    else
      return SCHEME_ENVBOX_VAL(v);
  } else {
    if (!force)
      return NULL;
    else {
      /* A cmethod - close it or cache a box */
      if (irec) {
	int level = mclass->cmethod_ready_level[mclass->vslot_map[vp]];
	Scheme_Class *sclass = oclass->heritage[level];
	int cindex = sclass->vslot_map[vp];
	int lpos = sclass->cmethod_source_map[cindex];

	if (!irec->frames[level].cmethods) {
	  Scheme_Object **ca;
	  ca = MALLOC_N(Scheme_Object*, sclass->contributes_cmethods);
	  irec->frames[level].cmethods = ca;
	}

	if (!irec->frames[level].cmethods[lpos]) {
	  Scheme_Object *box;

	  if (irec->init_level > level) {
	    /* Not ready yet; return #<undefined> for now. */
	    box = scheme_make_envunbox(scheme_undefined);
	  } else
	    box = scheme_make_envunbox(CloseMethod(sclass->cmethods[cindex], obj));
	  
	  irec->frames[level].cmethods[lpos] = box;
	}

	if (boxed)
	  return irec->frames[level].cmethods[lpos];
	else
	  return SCHEME_ENVBOX_VAL(irec->frames[level].cmethods[lpos]);
      } else {
	Scheme_Object *m;
	m = CloseMethod(mclass->cmethods[mclass->vslot_map[vp]], obj);
	if (boxed)
	  return scheme_make_envunbox(m);
	else
	  return m;
      }
    }
  }
}

static void BuildObjectFrame(Internal_Object *obj,
			     Init_Object_Rec *irec,
			     Scheme_Class *oclass,
			     Scheme_Class *sclass,
			     int framepos)
{
  /* Called when object is created (before initialization) for each class,
     least-specific class first. */
  ClassVariable *cvar;
  Scheme_Object *box;
  Scheme_Object **ivars, **refs;

  if (sclass->num_ivar)
    ivars = MALLOC_N(Scheme_Object*, sclass->num_ivar);
  else
    ivars = NULL;
  irec->frames[framepos].ivars = ivars;
  
  if (sclass->num_ref)
    refs = MALLOC_N(Scheme_Object*, sclass->num_ref);
  else
    refs = NULL;
  irec->frames[framepos].refs = refs;

  /* Find all rename boxes, then make public boxes */

  for (cvar = sclass->ivars; cvar; cvar = cvar->next) {
    if (isprivref(cvar)) {
      Scheme_Class *superclass = sclass->superclass;
      short vp = sclass->ref_map[cvar->index];
      Scheme_Object *iv;
      /* If the superclass's ivar is a cmethod, the box will contain
	 #<undefined>. */
      iv = GetIvar(obj, irec, oclass, superclass, vp, 1, 1);
      refs[cvar->index] = iv;
    }
  }

  for (cvar = sclass->ivars; cvar; cvar = cvar->next) {
    if (ispublic(cvar)) {
      box = scheme_make_envunbox(scheme_undefined);
      obj->slots[oclass->vslot_map[sclass->ivar_map[cvar->index]]] = box;
      ivars[cvar->index] = box;
    }
  }
}

static Init_Object_Rec *CreateObjectFrames(Internal_Object *obj, Scheme_Class *sclass)
{
  /* Called when object is created (before initialization) */
  Init_Object_Rec *irec;
  int i;

  irec = (Init_Object_Rec *)scheme_malloc_rt(sizeof(Init_Object_Rec)
					     + (sclass->pos /* for +1 total */
						* sizeof(Init_Frame)));
#ifdef MZTAG_REQUIRED
  irec->type = scheme_rt_init_obj_rec;
  irec->count = sclass->pos + 1;
#endif

  irec->init_level = sclass->pos + 1;

  for (i = 0; i <= sclass->pos; i++) {
    BuildObjectFrame(obj, irec, sclass, sclass->heritage[i], i);
  }

  return irec;
}


static void
PushFrameVariables(Internal_Object *o, Init_Object_Rec *irec, Scheme_Class *sclass, 
		   int level, Scheme_Process *p,
		   Scheme_Object ***_priv_stack, Scheme_Object ***_obj_stack)
{
  /* Called at start of class-specific initialization */
  Scheme_Object **priv_stack, **pub_stack, **ref_stack, **obj_stack;
  Scheme_Class *oclass;
  ClassVariable *cvar;
  int i, count;

  priv_stack = PUSH_RUNSTACK(p, MZ_RUNSTACK, sclass->num_private);
  ref_stack = PUSH_RUNSTACK(p, MZ_RUNSTACK, sclass->num_ref);
  pub_stack = PUSH_RUNSTACK(p, MZ_RUNSTACK, sclass->num_ivar);
  count = sclass->num_arg_vars + 2;
  obj_stack = PUSH_RUNSTACK(p, MZ_RUNSTACK, count);

  /* Privates: create boxes: */
  for (i = sclass->num_private; i--; ) {
    Scheme_Object *bx;
    bx = scheme_make_envunbox(scheme_undefined);
    priv_stack[i] = bx;
  }

  /* this & super-init */
  {
    Scheme_Object *bx;
    bx = scheme_make_envunbox((Scheme_Object *)o);
    obj_stack[sclass->num_arg_vars] = bx;
    bx = scheme_make_envunbox(MakeSuperInitPrim(o, irec, sclass->pos - 1));
    obj_stack[sclass->num_arg_vars + 1] = bx;
  }

  cvar = sclass->ivars;
  oclass = (Scheme_Class *)o->o.sclass;

  /* Push boxes for public ivars & refs onto stack */
  for (i = 0; cvar; cvar = cvar->next) {
    if (ispublic(cvar)) {
      Scheme_Object *iv;
      iv = GetIvar(o, irec, oclass, oclass, sclass->ivar_map[cvar->index], 1, 1);
      pub_stack[cvar->index] = iv;
    } else if (isprivref(cvar)) {
      ref_stack[cvar->index] = irec->frames[level].refs[cvar->index];
    } else if (isref(cvar)) {
      Scheme_Object *box;
      /* If the superclass's ivar is a cmethod, the box will contain
	 #<undefined>. */
      box = GetIvar(o, irec, oclass, oclass, sclass->ref_map[cvar->index], 1, 1);
      irec->frames[level].refs[cvar->index] = box;
      ref_stack[cvar->index] = box;
    }
  }

  *_obj_stack = obj_stack;
  *_priv_stack = priv_stack;
}

/**************************************************************************/

static void SetIVarValues(Init_Object_Rec *irec, int level, /* Frame */
			  Scheme_Object **priv_stack,
			  Scheme_Object **obj_stack,
			  ClassVariable *items,  /* Items to be created */
			  int pthresh) /* threshold for needed init vals */
{
  Scheme_Object *box, *v;
  int i;

  i = 0;
  while (items) {
    if (!isref(items)) {
      if (ispublic(items))
	box = irec->frames[level].ivars[items->index];
      else
	box = NULL;

      if (isinput(items))
	v = items->u.value;
      else
	v = _scheme_eval_compiled_expr(items->u.value);
	  
      if (isprivate(items)) {
	box = priv_stack[items->index];
	SCHEME_ENVBOX_VAL(box) = v;
      } else if (isinput(items)) {
	if (items->index < pthresh) {
	  v = _scheme_eval_compiled_expr(items->u.value);
	  SCHEME_ENVBOX_VAL(obj_stack[items->index]) = v;
	}
      } else if (box)
	SCHEME_ENVBOX_VAL(box) = v;
    }

    items = items->next;
  }
}


/* forward decl */
static void InitObjectFrame(Internal_Object *o, Init_Object_Rec *irec, int level,
			    int argc, Scheme_Object *argv[]);

static void *init_obj_frame_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Internal_Object *o = (Internal_Object *)p->ku.k.p1;
  Scheme_Object **argv = (Scheme_Object **)p->ku.k.p2;
  Init_Object_Rec *irec = (Init_Object_Rec *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  InitObjectFrame(o, irec, p->ku.k.i1, p->ku.k.i2, argv);

  return NULL;
}

static void InitObjectFrame(Internal_Object *o, Init_Object_Rec *irec, int level,
			    int argc, Scheme_Object *argv[])
{
  int skip_cmethods;
  int i, j, pthresh;
  Scheme_Object **priv_stack, **obj_stack, **orig_stack;
  Scheme_Class *sclass;
  Scheme_Process *p = scheme_current_process;

  if (level == -1) {
    level = irec->init_level;
    skip_cmethods = 1;
  } else {
    if (irec->init_level <= level)
      return;
    
    irec->init_level = level;

    skip_cmethods = 0;
  }

  orig_stack = MZ_RUNSTACK;

  sclass = ((Scheme_Class *)o->o.sclass)->heritage[level];

  if (!skip_cmethods && sclass->contributes_cmethods && irec->frames[level].cmethods) {
    /* Fill in any cached boxed */
    int i;
    for (i = sclass->num_public; i--; ) {
      if (sclass->vslot_kind[i] == slot_TYPE_CMETHOD) {
	int cindex = sclass->vslot_map[i];
	if (sclass->cmethod_ready_level[cindex] == level) {
	  int lpos = sclass->cmethod_source_map[cindex];
	  if (irec->frames[level].cmethods[lpos]) {
	    Scheme_Object *m;
	    m = CloseMethod(sclass->cmethods[cindex], o);
	    SCHEME_ENVBOX_VAL(irec->frames[level].cmethods[lpos]) = m;
	  }
	}
      }
    }
  }

  if ((sclass->priminit == pi_NOT) || (sclass->priminit == pi_NOT_OVER_CPP)) {
    Scheme_Object **stack, **saved;
    short total;

    i = sclass->closure_size;
    total = i + sclass->max_let_depth;

    if (!scheme_check_runstack(total)) {
      void *p2;
      p2 = (void *)MALLOC_N(Scheme_Object*, argc);
      p->ku.k.p1 = o;
      p->ku.k.i1 = -1;
      p->ku.k.i2 = argc;
      p->ku.k.p2 = p2;
      p->ku.k.p3 = irec;
      for (i = argc; i--; ) {
	((Scheme_Object **)p2)[i] = argv[i];
      }

      (void)scheme_enlarge_runstack(total, init_obj_frame_k);
      return;
    }

    saved = sclass->closure_saved;
    stack = PUSH_RUNSTACK(p, MZ_RUNSTACK, i);
    while (i--) {
      stack[i] = saved[i];
    }

    PushFrameVariables(o, irec, sclass, level, scheme_current_process,
		       &priv_stack, &obj_stack);

    /* Set init arg values in environment: */
    for (i = sclass->num_arg_vars, j = 0; i--; j++) {
      if (i < argc) {
	if (!j && (sclass->num_args < 0)) {
	  /* Turn rest of args into a list */
	  int k;
	  Scheme_Object *l = scheme_null, *bx;
	  
	  for (k = argc; k-- > i; ) {
	    l = cons(argv[k], l);
	  }
	  
	  bx = scheme_make_envunbox(l);
	  obj_stack[j] = bx;
	} else {
	  Scheme_Object *bx;
	  bx = scheme_make_envunbox(argv[i]);
	  obj_stack[j] = bx;
	}
      } else {
	Scheme_Object *bx;
	bx = scheme_make_envunbox(scheme_undefined);
	obj_stack[j] = bx;
      }
    }

    if (argc > sclass->num_arg_vars)
      pthresh = 0;
    else
      pthresh = sclass->num_arg_vars - argc;
  } else {
    pthresh = 0;
    priv_stack = NULL;
    obj_stack = NULL;
  }

  SetIVarValues(irec, level, priv_stack, obj_stack, sclass->ivars, pthresh);

  MZ_RUNSTACK = orig_stack;

  if (sclass->priminit == pi_CPP) {
    if (sclass->superclass->pos)
      InitObjectFrame(o, irec, level - 1, 0, NULL);
    else
      irec->init_level = 0;
  } else if (((sclass->priminit == pi_NOT) || (sclass->priminit == pi_NOT_OVER_CPP)) && (irec->init_level >= level)) {
    if (sclass->superclass->pos > -1) {
      const char *cl;
      cl = scheme_iget_class_name((Scheme_Object *)sclass, " in class: ");

      scheme_raise_exn(MZEXN_OBJECT,
		       CREATE_OBJECT ": initialization did not invoke %s%s",
		       scheme_symbol_name(sclass->super_init_name),
		       cl);
    }
  }
}

static Scheme_Object *find_ivar(Internal_Object *obj, 
				Scheme_Object *name,
				int force)
{
  Scheme_Class *oclass = (Scheme_Class *)obj->o.sclass;
  short sp, vp;

  sp = (short)FindName(oclass, name);
  if (sp < 0)
    return NULL;

  vp = oclass->public_map[sp];

  return GetIvar(obj, NULL, oclass, oclass, vp, 0, force);
}

Scheme_Object *scheme_find_ivar(Scheme_Object *obj, 
				Scheme_Object *sym,
				int force)
{
  if (!SCHEME_OBJP(obj))
    scheme_signal_error("ivar: not an object");
  if (!SCHEME_SYMBOLP(sym))
    scheme_signal_error("ivar: not an identifier");

  return find_ivar(((Internal_Object *)obj), sym, force);
}

static Scheme_Class_Object *allocate_object(Scheme_Class *sclass, Init_Object_Rec **irec)
{
  Internal_Object *obj;

  if (!sclass)
    scheme_signal_error("bad primitive class");

  obj = (Internal_Object *)scheme_malloc_tagged(sizeof(Internal_Object) 
						+ (sizeof(Scheme_Object *) 
						   * (sclass->num_slots - 1)));

  obj->o.type = scheme_object_type;
  obj->o.sclass = (Scheme_Object *)sclass;

  {
    Init_Object_Rec *ii;
    ii = CreateObjectFrames(obj, sclass);
    *irec = ii;
  }
  
  return (Scheme_Class_Object *)obj;
}

Scheme_Object *scheme_make_uninited_object(Scheme_Object *c)
{
  Internal_Object *obj;
  Scheme_Class *sclass;
  Init_Object_Rec *irec;

  sclass = (Scheme_Class *)c;

  obj = (Internal_Object *)allocate_object(sclass, &irec);
  InitObjectFrame(obj, irec, irec->init_level - 1, 0, NULL);

  ((Scheme_Class_Object *)obj)->inited = 1;

  return (Scheme_Object *)&obj->o;
}

static void CallInitFrame(Internal_Object *o, Init_Object_Rec *irec, int level,
			  int argc, Scheme_Object *argv[])
{
  Scheme_Class *sclass;
  Scheme_Object *initfunc;

  sclass = ((Scheme_Class *)o->o.sclass)->heritage[level];

  if (!sclass->pos) {
    irec->init_level = 0;
    return;
  }

  if (sclass->priminit != pi_CPP) {
    if ((argc < sclass->num_required_args)
	|| ((sclass->num_args >= 0) && (argc > sclass->num_args))) {
      const char *cl;

      cl = scheme_iget_class_name((Scheme_Object *)sclass, "initialization for class: ");
      if (!*cl)
	cl = "class initialization";
      scheme_wrong_count(cl, 
			 sclass->num_required_args,
			 sclass->num_args,
			 argc, argv);
    }
  }
  
  InitObjectFrame(o, irec, level, argc, argv);
    
  /* Eval intialization function */
  if (sclass->priminit == pi_CPP) {
    if (sclass->piu.initf)
      initfunc = scheme_make_closed_prim(sclass->piu.initf, o);
    else
      initfunc = NULL;
    
    if (initfunc) {
      _scheme_apply_multi(initfunc, argc, argv);
      ((Scheme_Class_Object *)o)->inited = 1;
    }
  } else if ((sclass->priminit == pi_COMP)
	     || (sclass->priminit == pi_COMP_OVER_CPP)) {
    int i, c;
    Scheme_Object *superinit, **public_values, **env_values;
    ClassVariable *cvar;
    Scheme_Class *oclass;

    superinit = MakeSuperInitPrim(o, irec, level - 1);

    oclass = (Scheme_Class *)o->o.sclass;

    c = sclass->num_ivar + sclass->num_ref;
    public_values = MALLOC_N(Scheme_Object *, c);
    env_values = MALLOC_N(Scheme_Object *, c);
    for (cvar = sclass->ivars, i = c; i--; cvar = cvar->next) {
      if (ispublic(cvar)) {
	Scheme_Object *iv;
	iv = GetIvar(o, irec, oclass, oclass, sclass->ivar_map[cvar->index], 1, 1);
	env_values[i] = iv;
	public_values[i] = irec->frames[level].ivars[cvar->index];
      } else if (isprivref(cvar)) {
	public_values[i] = irec->frames[level].refs[cvar->index];
	env_values[i] = public_values[i];
      } else if (isref(cvar)) {
	Scheme_Object *iv;
	iv = GetIvar(o, irec, oclass, oclass, sclass->ref_map[cvar->index], 1, 1);
	public_values[i] = iv;
	env_values[i] = public_values[i];
      }
    }

    sclass->piu.insti.f(public_values,
			env_values,
			superinit,
			argc, argv,
			(Scheme_Object *)o,
			sclass->piu.insti.data);
  }

  if (!((sclass->priminit == pi_NOT) || (sclass->priminit == pi_NOT_OVER_CPP))) {
    if (irec->init_level && (irec->init_level >= level)) {
      const char *cl;
      cl = scheme_iget_class_name((Scheme_Object *)sclass, " for class: ");
      
      scheme_raise_exn(MZEXN_OBJECT,
		       CREATE_OBJECT ": superclass never initialized%s",
		       cl);
    }
  }
}

static Scheme_Class_Object *make_object(Scheme_Class *sclass,
					int argc,
					Scheme_Object *argv[])
{
  Internal_Object *obj;
  Init_Object_Rec *irec;
  
  obj = (Internal_Object *)allocate_object(sclass, &irec);
  
  CallInitFrame(obj, irec, irec->init_level - 1, argc, argv);

  return (Scheme_Class_Object *)obj;
}

Scheme_Object *scheme_make_object(Scheme_Object *sclass, 
				  int argc, Scheme_Object **argv)
{
  if (!SCHEME_CLASSP(sclass))
    scheme_signal_error("make-object: not a class");

  return (Scheme_Object *)make_object((Scheme_Class *)sclass, argc, argv);
}

static Scheme_Object *CreateObject(int argc, Scheme_Object *argv[])
{
# define QARGV_CNT 10
  Scheme_Object *classname;
  Scheme_Class *sclass;
  Internal_Object *obj;
  Scheme_Object **nargv, *qargv[QARGV_CNT];

  classname = argv[0];

  if (!SAME_TYPE(SCHEME_TYPE(classname), scheme_class_type))
    scheme_wrong_type(CREATE_OBJECT, "class", 0, argc, argv);

  sclass = (Scheme_Class *)classname;

  if ((sclass->priminit == pi_CPP)
      && !sclass->piu.initf)
    scheme_raise_exn(MZEXN_OBJECT,
		     CLASS_STAR ": can't create objects from the class %S",
		     sclass->defname);
  
  if (argc - 1 <= QARGV_CNT)
    nargv = qargv;
  else
    nargv = MALLOC_N(Scheme_Object *, argc - 1);

  memcpy(nargv, argv + 1, (argc - 1) * sizeof(Scheme_Object *));

  obj = (Internal_Object *)make_object(sclass, argc  - 1, nargv);
  
  return (Scheme_Object *)&obj->o;
}

/************************************************************************/

static Scheme_Object *DoSuperInitPrim(SuperInitData *data,
				      int argc, Scheme_Object **argv)
{
  if (data->irec->init_level <= data->level) {
    Scheme_Class **heritage = ((Scheme_Class *)data->o->o.sclass)->heritage;
    const char *cl;
    cl = scheme_iget_class_name((Scheme_Object *)heritage[data->level + 1], 
				" in class: ");
    scheme_raise_exn(MZEXN_OBJECT,
		     "multiple intializations of superclass%s",
		     cl);
    return NULL;
  }
  
  CallInitFrame(data->o, data->irec, data->level, argc, argv);

  return scheme_void;
}

static Scheme_Object *MakeSuperInitPrim(Internal_Object *o, Init_Object_Rec *irec, int level)
{
  SuperInitData *data;

  data = MALLOC_ONE_RT(SuperInitData);
#ifdef MZTAG_REQUIRED
  data->type = scheme_rt_super_init_data;
#endif

  data->o = o;
  data->irec = irec;
  data->level = level;
  
  return scheme_make_closed_prim((Scheme_Closed_Prim *)DoSuperInitPrim, 
				 (void *)data);
}

/*****************************************************************************/

Scheme_Object *scheme_get_generic_data(Scheme_Object *clori, 
				       Scheme_Object *name)
{
  short kind;
  int sp, vp;
  Generic_Data *data;

  if (SCHEME_CLASSP(clori)) {
    Scheme_Class *sclass = (Scheme_Class *)clori;
    
    sp = FindName(sclass, name);
    if (sp < 0)
      return NULL;

    vp = sclass->public_map[sp];
    kind = generic_KIND_CLASS;
  } else {
    Scheme_Interface *in = (Scheme_Interface *)clori;
    
    sp = scheme_DoFindName(in->num_names, in->names, name);

    if (sp >= 0) {
      vp = in->name_map[sp];
      kind = generic_KIND_INTERFACE;
    } else {
      sp = FindName(in->supclass, name);
      if (sp < 0)
	return NULL;
      
      vp = sp;
      kind = generic_KIND_CINTERFACE;
    }
  }
  
  data = MALLOC_ONE_TAGGED(Generic_Data);

  data->type = scheme_generic_data_type;
  data->kind = kind;
  data->clori = clori;
  data->ivar_name = name;
  data->vp = vp;

  return (Scheme_Object *)data;
}

Scheme_Object *scheme_apply_generic_data(Scheme_Object *gdata, 
					 Scheme_Object *sobj,
					 int force)
{
  Internal_Object *obj;
  Scheme_Class *sclass;
  Generic_Data *data;
  short vp;

  data = (Generic_Data *)gdata;

  obj = (Internal_Object *)sobj;

  sclass = (Scheme_Class *)obj->o.sclass;
  
  if (data->kind == generic_KIND_CLASS) {
    if (NOT_SAME_OBJ((Scheme_Object *)sclass, data->clori)) {
      if (!scheme_is_subclass((Scheme_Object *)sclass, data->clori)) {
	const char *cl;
	cl = scheme_iget_class_name(data->clori, ": ");
	scheme_raise_exn(MZEXN_OBJECT,
			 "generic for %s: object not an instance of the generic's class%s",
			 scheme_symbol_name(data->ivar_name),
			 cl);
	return NULL;
      }
    }
    vp = (short)data->vp;
  } else {
    short *map;
    int offset;

    map = find_implementation((Scheme_Object *)sclass, data->clori, &offset);
    if (!map) {
      const char *inn;
      inn = scheme_iget_interface_name(data->clori, ": ");
      scheme_raise_exn(MZEXN_OBJECT,
		       "generic for %s: object not an instance of the generic's interface%s",
		       scheme_symbol_name(data->ivar_name),
		       inn);
      return NULL;
    }

    if (data->kind == generic_KIND_CINTERFACE)
      vp = (short)data->vp;
    else
      vp = map[data->vp + offset];
  }

  return GetIvar(obj, NULL, sclass, sclass, vp, 0, force);
}

static Scheme_Object *DoGeneric(Generic_Data *data, 
				int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OBJP(argv[0])) {
    char *s, *n;
    s = (char *)scheme_symbol_name(data->ivar_name);
    n = scheme_malloc_atomic(strlen(s) + 20);
    strcpy(n, "generic for ");
    strcat(n, s);
    scheme_wrong_type(n, "object", 0, argc, argv);
  }

  return scheme_apply_generic_data((Scheme_Object *)data, argv[0], 1);
}

static Scheme_Object *MakeGeneric(int argc, Scheme_Object *argv[])
{
  Scheme_Object *data, *src;
  Scheme_Object *prim;
  const char *s;
  char *gname;
  int len;

  src = argv[0];

  if (!SCHEME_CLASSP(src) && !SCHEME_INTERFACEP(src))
    scheme_wrong_type(MAKE_GENERIC, "class or interface", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_type(MAKE_GENERIC, "symbol", 1, argc, argv);

  data = scheme_get_generic_data(src, argv[1]);

  if (!data) {
    if (SCHEME_CLASSP(src))
      s = scheme_iget_class_name(src, " in class: ");
    else
      s = scheme_iget_interface_name(src, " in interface: ");
  
    scheme_raise_exn(MZEXN_OBJECT,
		     MAKE_GENERIC ": can't find instance variable: %S%s",
		     argv[1],
		     s);

    return NULL;
  }

  if (SCHEME_CLASSP(src))
    s = scheme_get_class_name(src, &len);
  else
    s = scheme_get_interface_name(src, &len);
  
  gname = (char *)scheme_malloc_atomic(len + 30);
  strcpy(gname, "generic-procedure for ");
  strcat(gname, s);

  prim = scheme_make_closed_prim_w_arity((Scheme_Closed_Prim *)DoGeneric, (void *)data,
					 gname,
					 1, 1);
  
  ((Scheme_Closed_Primitive_Proc *)prim)->name = gname;
  ((Scheme_Closed_Primitive_Proc *)prim)->flags |= SCHEME_PRIM_IS_GENERIC;

  return prim;
}

/*****************************************************************************/

static Scheme_Object *IVar(int argc, Scheme_Object *argv[])
{
  Scheme_Object *name;
  Internal_Object *obj = NULL;
  Scheme_Object *v;

  if (!SCHEME_OBJP(argv[0]))
    scheme_wrong_type(UQ_IVAR, "object", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_type(UQ_IVAR, "symbol", 1, argc, argv);

  obj = (Internal_Object *)argv[0];
  name = argv[1];

  v = find_ivar(obj, name, 1);

  if (!v) {
    const char *cl;

    cl = scheme_iget_class_name(obj->o.sclass, " in class: ");
    
    scheme_raise_exn(MZEXN_OBJECT,
		     IVAR ": instance variable not found: %S%s",
		     name,
		     cl);
  }

  return v;
}

/*****************************************************************************/

static Scheme_Object *IsObject(int c, Scheme_Object *p[])
{
  return SCHEME_OBJP(p[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *IsClass(int c, Scheme_Object *p[])
{
  return SCHEME_CLASSP(p[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *IsInterface(int c, Scheme_Object *p[])
{
  return SCHEME_INTERFACEP(p[0]) ? scheme_true : scheme_false;
}

int scheme_is_subclass(Scheme_Object *c, Scheme_Object *base)
{
  Scheme_Class *dclass, *bclass;

  if (!SCHEME_CLASSP(c) || !SCHEME_CLASSP(base))
    return 0;

  dclass = (Scheme_Class *)c;
  bclass = (Scheme_Class *)base;

  if (dclass->pos < bclass->pos)
    return 0;

  return (SAME_OBJ(dclass->heritage[bclass->pos], bclass));
}

static short dummy_map[1];

static short *find_implementation(Scheme_Object *c, Scheme_Object *n, int *offset)
{
  Scheme_Class *sclass;
  Scheme_Interface *in, **ins;
  int i;

  sclass = (Scheme_Class *)c;
  in = (Scheme_Interface *)n;

  if (in->for_class) {
    if (scheme_is_subclass(c, (Scheme_Object *)in->supclass))
      return dummy_map;
    else
      return NULL;
  }

  ins = sclass->interfaces;
  for (i = sclass->num_interfaces; i--; ) {
    if (SAME_OBJ(ins[i], in)) {
      *offset = 0;
      return sclass->interface_maps[i];
    } else {
      Scheme_Interface **supers;
      int j;
      supers = ins[i]->supers;
      for (j = ins[i]->num_supers; j--; ) {
	if (SAME_OBJ(supers[j], in)) {
	  *offset = ins[i]->super_offsets[j];
	  return sclass->interface_maps[i];
	}
      }
    }
  }
  
  return NULL;
}

int scheme_is_implementation(Scheme_Object *c, Scheme_Object *n)
{
  int offset;

  if (!SCHEME_CLASSP(c) || !SCHEME_INTERFACEP(n))
    return 0;
  
  return find_implementation(c, n, &offset) ? 1 : 0;
}

int scheme_is_interface_extension(Scheme_Object *exn, Scheme_Object *basen)
{
  int i, offset;
  Scheme_Interface *ex, *base, **ins;

  if (!SCHEME_INTERFACEP(exn) || !SCHEME_INTERFACEP(basen))
    return 0;

  ex = (Scheme_Interface *)exn;
  base = (Scheme_Interface *)basen;

  ins = ex->supers;
  for (i = ex->num_supers; i--; ) {
    if (SAME_OBJ(base, ins[i]))
      return 1;
  }

  /* Maybe it's implicit inthe class... */
  return find_implementation((Scheme_Object *)ex->supclass, (Scheme_Object *)base, &offset) ? 1 : 0;
}

static Scheme_Object *IsA(int c, Scheme_Object *p[])
{
  if (!SCHEME_CLASSP(p[1]) && !SCHEME_INTERFACEP(p[1]))
    scheme_wrong_type(IS_A, "class or interface", 1, c, p);

  return scheme_is_a(p[0], p[1]) ? scheme_true : scheme_false;
}

int scheme_is_a(Scheme_Object *obj, Scheme_Object *base)
{
  Scheme_Object *cl;

  if (!SCHEME_OBJP(obj))
    return 0;

  cl = (Scheme_Object *)((Internal_Object *)obj)->o.sclass;

  if (SCHEME_INTERFACEP(base))
    return scheme_is_implementation(cl, base);
  else if (SCHEME_CLASSP(base))
    return scheme_is_subclass(cl, base);
  else
    return 0;
}

static Scheme_Object *IsSubclass(int c, Scheme_Object *p[])
{
  if (!SCHEME_CLASSP(p[1]))
    scheme_wrong_type(SUBCLASS, "class", 1, c, p);

  if (!SCHEME_CLASSP(p[0]))
    return scheme_false;

  return scheme_is_subclass(p[0], p[1]) ? scheme_true : scheme_false;
}

static Scheme_Object *IsImplementation(int c, Scheme_Object *p[])
{
  if (!SCHEME_INTERFACEP(p[1]))
    scheme_wrong_type(IMPLEMENTATION, "interface", 1, c, p);

  if (!SCHEME_CLASSP(p[0]))
    return scheme_false;

  return scheme_is_implementation(p[0], p[1]) ? scheme_true : scheme_false;
}

static Scheme_Object *IsExtension(int c, Scheme_Object *p[])
{
  if (!SCHEME_INTERFACEP(p[1]))
    scheme_wrong_type(INTERFACE_EXTENSION, "interface", 1, c, p);

  if (!SCHEME_INTERFACEP(p[0]))
    return scheme_false;

  return scheme_is_interface_extension(p[0], p[1]) ? scheme_true : scheme_false;
}

static Scheme_Object *IvarInInterface(int c, Scheme_Object *p[])
{
  Scheme_Interface *in;
  Scheme_Class *sclass;

  if (!SCHEME_SYMBOLP(p[0]))
    scheme_wrong_type(IVAR_IN_INTERFACE, "symbol", 0, c, p);
  if (!SCHEME_INTERFACEP(p[1]))
    scheme_wrong_type(IVAR_IN_INTERFACE, "interface", 1, c, p);

  in = (Scheme_Interface *)p[1];

  if (scheme_DoFindName(in->num_names, in->names, p[0]) >= 0)
    return scheme_true;

  sclass = in->supclass;
  if (FindName(sclass, p[0]) >= 0)
    return scheme_true;

  return scheme_false;
}

static Scheme_Object *ClassToInterface(int c, Scheme_Object *p[])
{
  if (!SCHEME_CLASSP(p[0]))
    scheme_wrong_type(CLASS_TO_INTERFACE, "class", 0, c, p);

  return (Scheme_Object *)((Scheme_Class *)p[0])->equiv_intf;
}

static Scheme_Object *ObjectInterface(int c, Scheme_Object *p[])
{
  if (!SCHEME_OBJP(p[0]))
    scheme_wrong_type(OBJECT_INTERFACE, "object", 0, c, p);

  return (Scheme_Object *)((Scheme_Class *)((Scheme_Class_Object *)p[0])->sclass)->equiv_intf;
}

static Scheme_Object *InterfaceIvarNames(int argc, Scheme_Object **argv)
{
  Scheme_Interface *interface = (Scheme_Interface *)argv[0];
  Scheme_Object *p = scheme_null, **a;
  int i;

  if (!SCHEME_INTERFACEP(argv[0]))
    scheme_wrong_type(INTERFACE_IVAR_NAMES, "interface", 0, argc, argv);

  i = interface->num_names;
  a = interface->names;
  while (i--) {
    p = scheme_make_pair(a[i], p);
  }

  i = interface->supclass->num_public;
  a = interface->supclass->public_names;
  while (i--) {
    p = scheme_make_pair(a[i], p);
  }

  return p;
}

static Scheme_Object *ClassInitArity(int n, Scheme_Object *p[])
{
  Scheme_Class *c;

  if (!SCHEME_CLASSP(p[0]))
    scheme_wrong_type(CLASS_INIT_ARITY, "class", 0, n, p);

  c = (Scheme_Class *)p[0];

  return scheme_make_arity(c->num_required_args, c->num_args);
}

#if 0
static Scheme_Object *ObjectClass(int c, Scheme_Object *p[])
{
  if (!SCHEME_OBJP(p[0]))
    scheme_wrong_type(OBJECT_CLASS, "object", 0, c, p);

  return ((Scheme_Class_Object *)p[0])->sclass;
}

static Scheme_Object *IvarInClass(int c, Scheme_Object *p[])
{
  Scheme_Class *sclass;

  if (!SCHEME_SYMBOLP(p[0]))
    scheme_wrong_type(IVAR_IN_CLASS, "symbol", 0, c, p);
  if (!SCHEME_CLASSP(p[1]))
    scheme_wrong_type(IVAR_IN_CLASS, "class", 1, c, p);

  sclass = (Scheme_Class *)p[1];
  if (FindName(sclass, p[0]) >= 0)
    return scheme_true;

  return scheme_false;
}
#endif

#if ADD_TEST_PRIM_OBJ
static Scheme_Object *ConstructPObject(Scheme_Object *obj, int argc, Scheme_Object **argv)
{
  if (argc != 1) 
    scheme_wrong_count("primclass%::initialization", 1, 1, argc, argv);

  ((Scheme_Class_Object *)obj)->primdata = argv[0];

  return obj;
}

static Scheme_Object *PObjectName(Scheme_Object *obj, int argc, Scheme_Object **argv)
{
  return (Scheme_Object *)((Scheme_Class_Object *)obj)->primdata;
}

static Scheme_Object *PObjectClassname(Scheme_Object *obj, int argc, Scheme_Object **argv)
{
  return scheme_make_string("primclass%");
}

static Scheme_Object *PSubObjectClassname(Scheme_Object *obj, int argc, Scheme_Object **argv)
{
  return scheme_make_string("primsubclass%");
}

static Scheme_Object *PSubObjectDetail(Scheme_Object *obj, int argc, Scheme_Object **argv)
{
  return (SCHEME_STRINGP((Scheme_Object *)((Scheme_Class_Object *)obj)->primdata)
	  ? scheme_true
	  : scheme_false);
}


#endif

void scheme_init_object(Scheme_Env *env)
{
  if (scheme_starting_up) {
#ifdef MZ_PRECISE_GC
    register_traversers();
#endif

    REGISTER_SO(null_class);

    REGISTER_SO(interface_symbol);

    interface_symbol = scheme_intern_symbol("#%interface");

    scheme_register_syntax("if", Interface_Execute);

    scheme_install_type_writer(scheme_interface_data_type, write_Interface_Data);
    scheme_install_type_reader(scheme_interface_data_type, read_Interface_Data);
  }

  scheme_add_global_keyword(INTERFACE, 
			    scheme_make_compiled_syntax(Interface, 
							Interface_expand), 
			    env);

  scheme_add_global_constant(CREATE_OBJECT, 
			     scheme_make_prim_w_arity(CreateObject, 
						      CREATE_OBJECT,
						      1, -1), 
			     env);
  
  scheme_add_global_constant(UQ_IVAR, 
			     scheme_make_prim_w_arity(IVar, 
						      UQ_IVAR,
						      2, 2),
			     env);

  scheme_add_global_constant(UQ_MAKE_GENERIC, 
			     scheme_make_prim_w_arity(MakeGeneric, 
						      UQ_MAKE_GENERIC,
						      2, 2), 
			     env);

  scheme_add_global_constant(IS_OBJECT, 
			     scheme_make_folding_prim(IsObject, 
						      IS_OBJECT,
						      1, 1, 1), 
			     env);
  scheme_add_global_constant(IS_CLASS, 
			     scheme_make_folding_prim(IsClass, 
						      IS_CLASS,
						      1, 1, 1), 
			     env);
  scheme_add_global_constant(IS_INTERFACE, 
			     scheme_make_folding_prim(IsInterface, 
						      IS_INTERFACE,
						      1, 1, 1), 
			     env);
  scheme_add_global_constant(IS_A, 
			     scheme_make_folding_prim(IsA, 
						      IS_A,
						      2, 2, 1), 
			     env);
  scheme_add_global_constant(SUBCLASS, 
			     scheme_make_folding_prim(IsSubclass, 
						      SUBCLASS,
						      2, 2, 1), 
			     env);
  scheme_add_global_constant(IMPLEMENTATION, 
			     scheme_make_folding_prim(IsImplementation, 
						      IMPLEMENTATION,
						      2, 2, 1), 
			     env);
  scheme_add_global_constant(INTERFACE_EXTENSION, 
			     scheme_make_folding_prim(IsExtension, 
						      INTERFACE_EXTENSION,
						      2, 2, 1), 
			     env);
  scheme_add_global_constant(IVAR_IN_INTERFACE,
			     scheme_make_folding_prim(IvarInInterface,
						      IVAR_IN_INTERFACE,
						      2, 2, 1), 
			     env);
  scheme_add_global_constant(CLASS_TO_INTERFACE, 
			     scheme_make_folding_prim(ClassToInterface, 
						      CLASS_TO_INTERFACE,
						      1, 1, 1), 
			     env);
  scheme_add_global_constant(OBJECT_INTERFACE, 
			     scheme_make_folding_prim(ObjectInterface, 
						      OBJECT_INTERFACE,
						      1, 1, 1), 
			     env);
  scheme_add_global_constant(INTERFACE_IVAR_NAMES,
			     scheme_make_prim_w_arity(InterfaceIvarNames, 
						      INTERFACE_IVAR_NAMES,
						      1, 1), 
			     env);
  scheme_add_global_constant(CLASS_INIT_ARITY, 
			     scheme_make_folding_prim(ClassInitArity, 
						      CLASS_INIT_ARITY,
						      1, 1, 1), 
			     env);

  scheme_add_global_constant(NULL_CLASS,
			     NullClass(),
			     env);

#if 0
  scheme_add_global_constant(OBJECT_CLASS, 
			     scheme_make_folding_prim(ObjectClass, 
						      OBJECT_CLASS,
						      1, 1, 1), 
			     env);
  scheme_add_global_constant(IVAR_IN_CLASS,
			     scheme_make_folding_prim(IvarInClass,
						      IVAR_IN_CLASS,
						      2, 2, 1), 
			     env);
#endif

#if ADD_TEST_PRIM_OBJ
  {
    Scheme_Object *pclass, *psubclass;

    pclass = scheme_make_class("primclass%", NULL, ConstructPObject, 2);
    scheme_add_method_w_arity(pclass, "name", PObjectName, 0, 0);
    scheme_add_method_w_arity(pclass, "class-name", PObjectClassname, 0, 0);
    scheme_made_class(pclass);
    scheme_add_global_constant("primclass%", pclass, env);

    psubclass = scheme_make_class("primsubclass%", pclass, ConstructPObject, 2);
    scheme_add_method_w_arity(psubclass, "detail", PSubObjectDetail, 0, 0);
    scheme_add_method_w_arity(psubclass, "class-name", PSubObjectClassname, 0, 0);
    scheme_made_class(psubclass);
    scheme_add_global_constant("primsubclass%", psubclass, env);
  }
#endif
}

#ifdef MEMORY_COUNTING_ON

void scheme_count_object(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht)
{
  int count;
  Internal_Object *obj = (Internal_Object *)o;

  *s = sizeof(Internal_Object);
  count = ((Scheme_Class *)obj->o.sclass)->num_slots;
  *s += count * sizeof(Scheme_Object *);

  *e = 0;
}

static long count_cvars(ClassVariable *cvar, Scheme_Hash_Table *ht)
{
  long e = 0;

  while (cvar) {
    e += (ht ? scheme_count_memory(cvar->name, ht) : 0);

    switch(cvar->vartype) {
    case varPUBLIC:
    case varOVERRIDE:
    case varPRIVATE:
    case varNOTHING:
    case varINPUT:
      e += (ht ? scheme_count_memory(cvar->u.value, ht) : 0);
      break;
    case varINHERIT:
    case varRENAME:
      e += (ht ? scheme_count_memory(cvar->u.source.name, ht) : 0);
    }

    cvar = cvar->next;
  }

  return e;
}

void scheme_count_class(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht)
{
  Scheme_Class *sclass = (Scheme_Class *)o;

  *s = sizeof(Scheme_Class)
    + sclass->num_public * (sizeof(short) + sizeof(short) + sizeof(slotkind))
    + sclass->num_public * sizeof(Scheme_Object*)
    + (sclass->pos + 1) * sizeof(Scheme_Class*)
    + sclass->closure_size * sizeof(int);

  *e += (ht ? scheme_count_memory((Scheme_Object *)sclass->superclass, ht) : 0);
  
  *e += count_cvars(sclass->ivars, ht);

  *e += scheme_count_closure(sclass->closure_saved, sclass->closure_size, ht);
}

void scheme_count_generic(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht)
{
  *s = sizeof(Generic_Data);
  *e = (ht ? scheme_count_memory(((Generic_Data *)o)->clori, ht) : 0);
  *e = (ht ? scheme_count_memory(((Generic_Data *)o)->ivar_name, ht) : 0);
}

void scheme_count_class_data(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht)
{
  Class_Data *data = (Class_Data *)o;

  *s = (sizeof(Class_Data)
	+ (data->num_ivar + data->num_private + data->num_ref) * sizeof(ClassVariable)
	+ data->closure_size * sizeof(short));

  *e += (ht ? scheme_count_memory(data->super_expr, ht) : 0);

  *e = 0;
}

#endif

/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_OBJECT_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_object_type, mark_object_val);
  GC_REG_TRAV(scheme_class_type, mark_class_val);
  GC_REG_TRAV(scheme_generic_data_type, mark_generic_data_val);
  GC_REG_TRAV(scheme_interface_type, mark_interface_val);
  GC_REG_TRAV(scheme_class_data_type, mark_class_data_val);
  GC_REG_TRAV(scheme_interface_data_type, mark_interface_data_val);

  GC_REG_TRAV(scheme_rt_dup_check, mark_dup_check);
  GC_REG_TRAV(scheme_rt_class_var, mark_class_var);
  GC_REG_TRAV(scheme_rt_class_method, mark_class_method);
  GC_REG_TRAV(scheme_rt_class_assembly, mark_class_assembly);
  GC_REG_TRAV(scheme_rt_init_obj_rec, mark_init_object_rec);
  GC_REG_TRAV(scheme_rt_super_init_data, mark_super_init_data);
}

END_XFORM_SKIP;

#endif

#endif
/* NO_OBJECT_SYSTEM */
