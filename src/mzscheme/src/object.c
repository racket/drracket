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

#ifndef NO_OBJECT_SYSTEM

/* For testing handling primitive classes: */
#define ADD_TEST_PRIM_OBJ 0

#include <memory.h>

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

/* Extra leading slot space needed for Scheme images of C++ objects: */
#define EXTRA_PRIM_SLOTS 2

#define INTERFACE "interface"

#define CLASS_STAR "class*"
#define CLASS_STAR_W_NAMES "class*/names"
#define CREATE_OBJECT "make-object"

#define IVAR "ivar"
#define UQ_IVAR "uq-ivar"

#define MAKE_GENERIC "make-generic"
#define UQ_MAKE_GENERIC "uq-make-generic"

#define IS_CLASS "class?"
#define IS_INTERFACE "interface?"
#define IS_OBJECT "object?"
#define IS_A "is-a?"
#define SUBCLASS "subclass?"
#define IMPLEMENTATION "implementation?"
#define INTERFACE_EXTENSION "interface-extension?"
#define IVAR_IN_CLASS "ivar-in-class?"
#define OBJECT_CLASS "object-class"

#define SUPER_INIT "super-init"

static Scheme_Object *seq_symbol;
static Scheme_Object *pub_symbol, *pri_symbol;
static Scheme_Object *inh_symbol;
static Scheme_Object *ren_symbol;

static Scheme_Object *class_star_symbol;
static Scheme_Object *interface_symbol;

#define cons scheme_make_pair
  
enum {
  varPUBLIC,
  varPRIVATE,
  varINHERIT,
  varRENAME,
  varNOTHING,
  varINPUT
};

enum {
  slot_TYPE_IVAR,
  slot_TYPE_CMETHOD
};

enum {
  generic_KIND_CLASS,
  generic_KIND_INTERFACE
};

#define ispublic(c) (((c)->vartype == varPUBLIC))
#define isreftype(vt) ((vt == varRENAME) || (vt == varINHERIT))
#define isref(c) isreftype((c)->vartype)
#define isprivref(c) ((c)->vartype == varRENAME)
#define isprivate(c) ((c)->vartype == varPRIVATE)
#define isinput(c) ((c)->vartype == varINPUT)

#define IVAR_INT_NAME(c) (SCHEME_SYMBOLP((c)->name) ? (c)->name : SCHEME_CAR((c)->name))
#define IVAR_EXT_NAME(c) (SCHEME_SYMBOLP((c)->name) ? (c)->name : SCHEME_CADR((c)->name))
/* Get external, but know to be external only: */
#define _IVAR_EXT_NAME(c) ((c)->name)

typedef struct ClassVariable {
  Scheme_Object *name;
  short vartype;
  short index;
  union {
    Scheme_Object *value;
    struct {
      Scheme_Closed_Prim *f;
      short mina, maxa;
    } prim;
    struct {
      Scheme_Object *name;
    } source;
  } u;
  struct ClassVariable *next;
} ClassVariable;

enum {
  pi_NOT = 0,
  pi_NOT_OVER_CPP,
  pi_CPP,
  pi_COMP,
  pi_COMP_OVER_CPP
};

typedef struct {
  Scheme_Closed_Prim *f;
  short mina, maxa;
  char *closed_name;
} CMethod;

typedef char slotkind;

typedef struct Scheme_Interface {
  Scheme_Type type;
  short num_names, num_supers;
  Scheme_Object **names;
  short *name_map; /* position in names => interface slot position */
  struct Scheme_Interface **supers; /* all superinterfaces (flattened hierarchy) */
  short *super_offsets; /* superinterface => super's slot position offset */
  Scheme_Object *defname;
} Scheme_Interface;

typedef struct Interface_Data {
  Scheme_Type type;
  short num_names, num_supers;
  Scheme_Object **names;
  struct Scheme_Object **super_exprs;
  Scheme_Object *defname;
} Interface_Data;

typedef struct Scheme_Interface_Assembly {
  Interface_Data data;
} Scheme_Interface_Assembly;

typedef struct Scheme_Class {
  Scheme_Type type;

  ClassVariable *ivars; /* Order determines order of evaluation */

  union {
    Scheme_Closed_Prim *initf;
    struct {
      Scheme_Instance_Init_Proc *f;
      void *data;
    } insti;
  } piu;
  short priminit;

  short pos;
  struct Scheme_Class **heritage;
  struct Scheme_Class *superclass; /* Redundant, but useful. */
  Scheme_Object *super_init_name;

  short num_args, num_required_args, num_arg_vars;
  short num_ivar, num_private, num_ref;
  short num_public, num_slots; /* num_vslots == num_public */
  Scheme_Object **public_names;
  short *public_map; /* position in public_names => virtual slot position */
  short *vslot_map; /* virtual slot position => slot position or cmethod position */
  slotkind *vslot_kind; /* virtual slot position => TYPE_CMETHOD | TYPE_IVAR */

  short *ivar_map; /* ivar index => class virtual slot */
  short *ref_map;  /* ref index => [super]class virtual slot position */

  CMethod **cmethods;
  short *cmethod_ready_level; /* class level where the cmethod originates */
  short *cmethod_source_map; /* cmethod position => local cmethod position */
  short contributes_cmethods;

  short closure_size;
  Scheme_Object **closure_saved;

  Scheme_Object *defname;

  int max_let_depth;

  short num_interfaces;
  Scheme_Interface **interfaces;
  short **interface_maps; /* interface slot position => virtual slot position */
} Scheme_Class;

typedef struct {
  Scheme_Type type;

  ClassVariable *ivars;

  short num_args, num_required_args, num_arg_vars;
  short num_ivar, num_private, num_ref;
  short num_cmethod;
  Scheme_Object **ivar_names;
  Scheme_Object **cmethod_names;
  CMethod **cmethods;

  short *closure_map;
  short closure_size;

  int max_let_depth;

  Scheme_Object *super_init_name;
  Scheme_Object *super_expr;

  int num_interfaces;
  Scheme_Object **interface_exprs;

  Scheme_Object *defname;
} Class_Data;

typedef struct Scheme_Class_Assembly {
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
    Scheme_Object *sclass;
  } o;
  Scheme_Object *slots[1];
} Internal_Object;

typedef struct Init_Object_Rec {
  short init_level;
  Init_Frame frames[1];
} Init_Object_Rec;

typedef struct {
  Internal_Object *o;
  Init_Object_Rec *irec;
  int level;
} SuperInitData;

typedef struct {
  Scheme_Type type;
  short kind;
  Scheme_Object *clori;
  int vp;
} Generic_Data;

static Scheme_Class *null_class = NULL;

static Scheme_Object *MakeSuperInitPrim(Internal_Object *o, Init_Object_Rec *irec, int level);
static int DoFindName(int count, Scheme_Object **pub, Scheme_Object *symbol);
static short *find_implementation(Scheme_Object *c, Scheme_Object *n, int *offset);

#define FindName(c, s) DoFindName(c->num_public, c->public_names, s)

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

static const char *get_class_name(Scheme_Object *c, const char *mode);
static const char *get_interface_name(Scheme_Object *i, const char *mode);

DupCheckRecord *scheme_begin_dup_symbol_check(DupCheckRecord *r)
{
  long i;
  
  if (r && r->scheck_size > 50)
    r->scheck_size = 0;    
  else {
    for (i = r->scheck_size; i--; )
      r->scheck_hash[i] = NULL;
  }

  if (!r) {
    r = (DupCheckRecord *)scheme_malloc(sizeof(DupCheckRecord));
    r->scheck_size = 0;
  }

  if (!r->scheck_size) {
    r->scheck_hash = NULL;
    r->scheck_step = 17;
    r->scheck_count = 0;
  }

  return r;
}

void scheme_dup_symbol_check(DupCheckRecord *r, char *where,
			     Scheme_Object *symbol, char *what, 
			     Scheme_Object *form, int inverted)
{
  long pos;

  if (r->scheck_count >= r->scheck_size / 2) {
    Scheme_Object **old = r->scheck_hash;
    long oldsize = r->scheck_size, i;

    r->scheck_size = r->scheck_size ? 2 * r->scheck_size : 50;
    r->scheck_hash = 
      (Scheme_Object **)scheme_malloc(i = r->scheck_size 
				      * sizeof(Scheme_Object *));
    memset(r->scheck_hash, 0, i);

    r->scheck_count = 0;
    for (i = 0; i < oldsize; i++)
      if (old[i])
	scheme_dup_symbol_check(r, where, old[i], what, form, 0);
  }

  pos = (((long)symbol) >> 2) % r->scheck_size;
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

static Scheme_Object *NullClass(void) 
{
  if (!null_class) {
    null_class = MALLOC_ONE_TAGGED(Scheme_Class);
    
    null_class->type = scheme_class_type;
    
    null_class->priminit = pi_NOT;
    
    null_class->ivars = NULL;
    
    null_class->pos = 0;
    null_class->super_init_name = NULL;
    null_class->heritage = MALLOC_ONE(Scheme_Class*);
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

    null_class->defname = NULL;
    
    null_class->closure_saved = NULL;
    null_class->closure_size = 0;
    null_class->max_let_depth = 0;
  }

  return (Scheme_Object *)null_class;
}

/**********************************************************************/

static int CompareObjectPtrs(Scheme_Object **a, Scheme_Object **b)
{
  return ((long)*a) - ((long)*b);
}

#define SEQUALS(a, b) (((unsigned long)a) == ((unsigned long)b))
#define SLESSTHAN(a, b) (((unsigned long)a) < ((unsigned long)b))

static int MergeArray(int ac, Scheme_Object **ak, Scheme_Object **a, short *as,
		      int bc, Scheme_Object **bk, Scheme_Object **b, short *bs,
		      Scheme_Object **d, short *ds, int nodup)
{
  int ai = 0, bi = 0, di = 0;

  while ((ai < ac) || (bi < bc)) {
    if ((ai < ac) && (bi < bc)) {
      if (SEQUALS(ak[ai], bk[bi])) {
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

    classvar = (ClassVariable *)scheme_malloc(sizeof(ClassVariable));
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

static void CompileItemList(Scheme_Object *form,
			    ClassVariable *cvars,
			    Scheme_Comp_Env *env, 
			    Class_Data *data,
			    Scheme_Compile_Info *rec)
{
  int alias, count;
  ClassVariable *classvar;
  Scheme_Compile_Info *recs;

  for (count = 0, classvar = cvars; classvar; classvar = classvar->next) {
    alias = isref(classvar);
    if (!alias)
      count++;
  }

  recs = MALLOC_N(Scheme_Compile_Info, count);
  scheme_init_compile_recs(rec, recs, count);

  for (count = 0, classvar = cvars; classvar; classvar = classvar->next) {
    alias = isref(classvar);

    if (!alias && !classvar->u.value)
      classvar->u.value = scheme_compiled_void(rec->can_optimize_constants);

    scheme_check_identifier(CLASS_STAR, IVAR_INT_NAME(classvar), NULL, env, form);

    if (!alias) {
      recs[count].value_name = IVAR_EXT_NAME(classvar);
      classvar->u.value = scheme_compile_expr(classvar->u.value, env, recs + count);
      count++;
    }
  }

  scheme_merge_compile_recs(rec, recs, count);
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
	scheme_wrong_syntax(CLASS_STAR, p, form,
			    "%s clause must contain pairs",
			    SCHEME_SYM_VAL(clause));
    } else if (SCHEME_PAIRP(p)) {
      Scheme_Object *pr = p;

      if (flags & check_MustId) {
	if (!(flags & check_CanRename)) {
	  scheme_wrong_syntax(CLASS_STAR, p, form,
			      "%s clause must contain only identifiers",
			      SCHEME_SYM_VAL(clause));
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
	    scheme_wrong_syntax(CLASS_STAR, n, form,
				"badly formed %s clause (bad internal-external"
				" identifier pair form)", SCHEME_SYM_VAL(clause));
	  if (!SCHEME_SYMBOLP(SCHEME_CAR(n)))
	    scheme_wrong_syntax(CLASS_STAR, SCHEME_CAR(n), form,
				"badly formed %s clause (internal"
				" name not an identifier)", SCHEME_SYM_VAL(clause));	  
	  n = SCHEME_CDR(n);
	  if (!SCHEME_SYMBOLP(SCHEME_CAR(n)))
	    scheme_wrong_syntax(CLASS_STAR, SCHEME_CAR(n), form,
				"badly formed %s clause (external"
				" name not an identifier)", SCHEME_SYM_VAL(clause));	  
	} else
	  scheme_wrong_syntax(CLASS_STAR, n, form,
			      "badly formed %s clause (name"
			      " not an identifier)", SCHEME_SYM_VAL(clause));
      }

      if (!SCHEME_NULLP(p)) {
	if (!SCHEME_PAIRP(p))
	scheme_wrong_syntax(CLASS_STAR, pr, form,
			    "badly formed %s clause (identifier"
			    " declaration is improper)", 
			    SCHEME_SYM_VAL(clause));
	if (!SCHEME_NULLP(SCHEME_CDR(p)))
	  scheme_wrong_syntax(CLASS_STAR, pr, form,
			      "badly formed %s clause (identifier"
			      " declaration not a pair)", 
			      SCHEME_SYM_VAL(clause));
	if (flags & check_BindingMustId) {
	  n = SCHEME_CAR(p);
	  if (!SCHEME_SYMBOLP(n))
	    scheme_wrong_syntax(CLASS_STAR, pr, form,
				"association "
				"in %s clause "
				"must be an identifier",
				SCHEME_SYM_VAL(clause));
	}
      } else if (flags & check_MustPair)
	scheme_wrong_syntax(CLASS_STAR, pr, form,
			    "%s clause must contain pairs",
			    SCHEME_SYM_VAL(clause));
    } else
      scheme_wrong_syntax(CLASS_STAR, p, form,
			  "badly formed %s clause (declaration"
			  " not an identifier or pair)", 
			  SCHEME_SYM_VAL(clause));
  }
  
  if (!SCHEME_NULLP(l))
    scheme_wrong_syntax(CLASS_STAR, l, form,
			"badly formed %s clause (" 
			IMPROPER_LIST_FORM
			" for identifiers)", 
			SCHEME_SYM_VAL(clause));
  
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
	Scheme_Object *p = cons(IVAR_INT_NAME(ivar), scheme_null);
	
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

  first = last = cons(name, scheme_null);

  if (first_from) {
    name = SCHEME_CAR(vars);
    vars = SCHEME_CDR(vars);
    
    SCHEME_CDR(first) = last = cons(name, scheme_null);
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

static short *CheckInherited(Scheme_Class *sclass, ClassVariable *item)
{
  short *ref_map;
  Scheme_Class *superclass = sclass->superclass;

  ref_map = MALLOC_N_ATOMIC(short, sclass->num_ref);

  for (; item; item = item->next) {
    if (isref(item)) {
      int p;
      if ((p = FindName(superclass, item->u.source.name)) < 0) {
	const char *cl, *sc;

	cl = get_class_name((Scheme_Object *)sclass, " for class: ");
	sc = get_class_name((Scheme_Object *)superclass, " in superclass: ");

	scheme_raise_exn(MZEXN_OBJECT_INHERIT,
			 item->u.source.name,
			 CLASS_STAR ": inherited ivar not found: %s%s%s",
			 scheme_symbol_name(item->u.source.name),
			 sc,
			 cl);
      } else {
	ref_map[item->index] = superclass->public_map[p];
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
    if (ispublic(item))
      ivar_map[item->index] = sclass->public_map[FindName(sclass, item->name)];
  }

  return ivar_map;
}

static void InsureNamesReady(ClassVariable *ivars, int count, Scheme_Object ***names)
{
  int i;
  ClassVariable *cvar;

  if (count && !*names) {
    *names = MALLOC_N(Scheme_Object*, count);

    for (i = 0, cvar = ivars; cvar; cvar = cvar->next)
      if (ispublic(cvar))
	(*names)[i++] = _IVAR_EXT_NAME(cvar);

    qsort((char *)*names, count,
	  sizeof(Scheme_Object *), 
	  (int (*)(const void *, const void *))CompareObjectPtrs); 
  }
}

static void InstallHeritage(Scheme_Class *sclass, Scheme_Class *superclass)
{
  int i;

  sclass->pos = superclass->pos + 1;
  sclass->heritage = MALLOC_N_ATOMIC(Scheme_Class*, (sclass->pos + 1));
  for (i = 0; i < sclass->pos; i++)
    sclass->heritage[i] = superclass->heritage[i];
  sclass->heritage[sclass->pos] = sclass;
  sclass->superclass = sclass->heritage[sclass->pos - 1];
}

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
    Scheme_Object *in;
    in = _scheme_eval_compiled_expr(data->super_exprs[i]);
    if (!SCHEME_INTERFACEP(in)) {
      scheme_raise_exn(MZEXN_OBJECT_INTERFACE_TYPE,
		       in,
		       "interface: interface expression returned "
		       "a non-interface: %s%s%s",
		       scheme_make_provided_string(in, 1, NULL),
		       data->defname ? " for interface: " : "",
		       data->defname ? scheme_symbol_name(data->defname) : "");
      return NULL;
    }
    il = cons(in, il);
  }

  in = MALLOC_ONE_TAGGED(Scheme_Interface);
  in->type = scheme_interface_type;

  in->defname = data->defname;

  supers = MALLOC_N(Scheme_Interface*, num_supers);
  for (i = data->num_supers; i--; ) {
    supers[i] = (Scheme_Interface *)SCHEME_CAR(il);
    il = SCHEME_CDR(il);
  }
  in->num_supers = num_supers;
  in->supers = supers;

  if (num_supers) {
    Scheme_Object **nbanka, **nbankb;
    short *mbanka, *mbankb;
    int total, mode = 1, newcount, offset;

    total = 0;
    for (i = num_supers; i--; )
      total += supers[i]->num_names; 
    
    nbanka = MALLOC_N(Scheme_Object*, total);
    nbankb = MALLOC_N(Scheme_Object*, total);
    mbanka = MALLOC_N(short, total);
    mbankb = MALLOC_N(short, total);

    num_names = supers[num_supers - 1]->num_names;
    for (i = num_names; i--; ) {
      nbanka[i] = supers[num_supers - 1]->names[i];
      mbanka[i] = supers[num_supers - 1]->name_map[i];
    }

    /* Merge superclasses, keeping duplicates.
       Merge name map in parallel.
       Offset name maps according to superinterface offsets. */
    for (i = data->num_supers - 1; i--; ) {
      int j, count;
      short *mbank;

      count = supers[i]->num_names;
      mbank = mode ? mbanka : mbankb;
      for (j = 0; j < num_names; j++)
	mbank[j] += count;

      MergeArray(num_names, mode ? nbanka : nbankb, mode ? nbanka : nbankb, NULL,
		 count, supers[i]->names, supers[i]->names, NULL,
		 mode ? nbankb : nbanka, NULL, 0);
      num_names = MergeArray(num_names, mode ? nbanka : nbankb, NULL, mode ? mbanka : mbankb, 
			     count, supers[i]->names, NULL, supers[i]->name_map,
			     NULL, mode ? mbankb : mbanka, 0);

      mode = !mode;
    }

    /* Final merge: */
    if (!mode) {
      nbanka = nbankb;
      mbanka = mbankb;
    }

    total = MergeArray(data->num_names, data->names, NULL, NULL,
		       num_names, nbanka, NULL, NULL,
		       NULL, NULL, 1);
    newcount = total - num_names;
    
    mbankb = MALLOC_N(short, data->num_names);
    /* init to -1; we'll go back and set the numbers later */
    for (i = data->num_names; i--; )
      mbankb[i] = -1;
    /* offset by number of names to be added */
    for (i = num_names; i--; )
      mbanka[i] += newcount;

    in->num_names = total;
    in->names = MALLOC_N(Scheme_Object*, total);
    in->name_map = MALLOC_N(short, total);

    MergeArray(data->num_names, data->names, data->names, NULL,
	       num_names, nbanka, nbanka, NULL,
	       in->names, NULL, 1);
    MergeArray(data->num_names, data->names, NULL, mbankb, 
	       num_names, nbanka, NULL, mbanka,
	       NULL, in->name_map, 1);

    /* renumber -1s to distinct new numbers (0 to newcount): */
    {
      int j;
      j = 0;
      for (i = total; i--; )
	if (in->name_map[i] < 0)
	  in->name_map[i] = j++;
    }

    /* Count total supers: */
    total = 0;
    for (i = num_supers; i--; )
      total += supers[i]->num_supers + 1;
    in->num_supers = total;
    in->supers = MALLOC_N(Scheme_Interface*, total);
    in->super_offsets = MALLOC_N(short, total);
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
    in->name_map = MALLOC_N(short, in->num_names);
    for (i = num_names; i--; )
      in->name_map[i] = i;
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

  for (i = data->num_supers; i--; )
    data->super_exprs[i] = scheme_link_expr(data->super_exprs[i], info);

  return scheme_make_syntax_link(Interface_Execute, 
				 (Scheme_Object *)data);
}

static Scheme_Object *Do_Interface(Scheme_Object *form, Scheme_Comp_Env *env,
				   Scheme_Compile_Info *rec, int depth)
{
  int i, num_supers, num_names;
  Scheme_Object *l, *sl, **supers, *nl;
  DupCheckRecord *r, drec;

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
  for (i = 0; i < num_supers; i++, sl = SCHEME_CDR(sl))
    supers[i] = SCHEME_CAR(sl);

  l = SCHEME_CDR(l);
  nl = l;

  drec.scheck_size = 0;
  r = scheme_begin_dup_symbol_check(&drec);

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
    for (i = 0; i < num_supers; i++)
      supers[i] = scheme_expand_expr(supers[i], env, depth);
    l = scheme_null;
    for (i = num_supers; i--; )
      l = cons(supers[i], l);

    return cons(interface_symbol, cons(l, nl));
  } else {
    /* Compiling */
    Interface_Data *data;
    Scheme_Compile_Info *recs;

    data = MALLOC_ONE_TAGGED(Interface_Data);
    data->type = scheme_interface_data_type;

    data->defname = rec->value_name;
    scheme_compile_rec_done_local(rec);

    data->num_names = num_names;
    data->names = MALLOC_N(Scheme_Object*, num_names);
    l = nl;
    for (i = 0; i < num_names; i++, nl = SCHEME_CDR(nl))
      data->names[i] = SCHEME_CAR(nl);

    /* Sort names: */
    qsort((char *)data->names, num_names,
	  sizeof(Scheme_Object *), 
	  (int (*)(const void *, const void *))CompareObjectPtrs); 

    recs = MALLOC_N(Scheme_Compile_Info, num_supers);
    scheme_init_compile_recs(rec, recs, num_supers);

    for (i = 0; i < num_supers; i++)
      supers[i] = scheme_compile_expr(supers[i], env, recs + i);

    scheme_merge_compile_recs(rec, recs, num_supers);

    data->num_supers = num_supers;
    data->super_exprs = supers;

    return scheme_make_syntax_compile(Interface_Link, 
				      (Scheme_Object *)data);
  }
}

static Scheme_Object *Interface(Scheme_Object *form, Scheme_Comp_Env *env,
				Scheme_Compile_Info *rec)
{
  return Do_Interface(form, env, rec, 0);
}

static Scheme_Object *Interface_expand(Scheme_Object *form, Scheme_Comp_Env *env,
				       int depth)
{
  return Do_Interface(form, env, NULL, depth);
}


static Scheme_Object *_DefineClass_Execute(Scheme_Object *form, int already_evaled)
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

  if (SCHEME_NULLP(superobj))
    superobj = NullClass();
  
  if (!SCHEME_CLASSP(superobj)) {
    scheme_raise_exn(MZEXN_OBJECT_CLASS_TYPE,
		     superobj,
		     CLASS_STAR ": superclass expression returned "
		     "a non-class: %s%s%s",
		     scheme_make_provided_string(superobj, 1, NULL),
		     data->defname ? " for class: " : "",
		     data->defname ? scheme_symbol_name(data->defname) : "");
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
      scheme_raise_exn(MZEXN_OBJECT_INTERFACE_TYPE,
		       in,
		       CLASS_STAR ": interface expression returned "
		       "a non-interface: %s%s%s",
		       scheme_make_provided_string(in, 1, NULL),
		       data->defname ? " for class: " : "",
		       data->defname ? scheme_symbol_name(data->defname) : "");
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
    const char *cl = get_class_name((Scheme_Object *)sclass, " to create class: ");
    scheme_raise_exn(MZEXN_OBJECT_PRIVATE_CLASS,
		     superclass,
		     "can't derive from the class: %s%s", 
		     scheme_symbol_name(superclass->defname),
		     cl);
  }
  
  InstallHeritage(sclass, superclass);

  sclass->super_init_name = data->super_init_name;

  num_interfaces = num_local_interfaces + superclass->num_interfaces;

  if (num_interfaces) {
    interfaces = MALLOC_N(Scheme_Interface*, num_interfaces);
    for (i = num_local_interfaces; i--; ) {
      interfaces[i] = (Scheme_Interface *)SCHEME_CAR(il);
      il = SCHEME_CDR(il);
    }
    for (i = superclass->num_interfaces; i--; )
      interfaces[i + num_local_interfaces] = superclass->interfaces[i];
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
  sclass->num_public = (MergeArray(data->num_ivar, data->ivar_names, NULL, NULL,
				   superclass->num_public, superclass->public_names, NULL, NULL,
				   NULL, NULL, 1)
			+ MergeArray(data->num_cmethod, data->cmethod_names, NULL, NULL,
				     superclass->num_public, superclass->public_names, NULL, NULL,
				     NULL, NULL, 1)
			- superclass->num_public);

  /* Make room for the union */
  num_public = sclass->num_public;
  temp_array = MALLOC_N(Scheme_Object*, num_public);
  public_names = MALLOC_N(Scheme_Object*, num_public);
  sclass->public_names = public_names;
  
  /* Union names: */
  i = MergeArray(data->num_ivar, data->ivar_names, data->ivar_names, NULL,
		 superclass->num_public, superclass->public_names, superclass->public_names, NULL,
		 temp_array, NULL, 1);
  MergeArray(i, temp_array, temp_array, NULL,
	     data->num_cmethod, data->cmethod_names, data->cmethod_names, NULL,
	     public_names, NULL, 1);

  /* Map names to source: */
  sclass->public_map = MALLOC_N_ATOMIC(short, num_public);
  sclass->vslot_map = MALLOC_N_ATOMIC(short, num_public);
  sclass->vslot_kind = MALLOC_N_ATOMIC(slotkind, num_public);
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

    cmpos = DoFindName(data->num_cmethod, data->cmethod_names, s);
    if (cmpos < 0)
      ipos = DoFindName(data->num_ivar, data->ivar_names, s);
    else
      ipos = -1;

    j = DoFindName(superclass->num_public, superclass->public_names, s);
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
    sclass->cmethods = MALLOC_N(CMethod*, num_cmethod);
    sclass->cmethod_ready_level = MALLOC_N_ATOMIC(short, num_cmethod);
    if (num_contrib_cmethod)
      sclass->cmethod_source_map = MALLOC_N_ATOMIC(short, num_cmethod);
    else
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
  sclass->ref_map = CheckInherited(sclass, sclass->ivars);

  /* Map public index to instance slot: */
  sclass->ivar_map = MapIvars(sclass, sclass->ivars);

  /* Map interface name positions to class positions */
  if (num_interfaces)
    imaps = MALLOC_N(short*, num_interfaces);
  else
    imaps = NULL;
  sclass->interface_maps = imaps;
  for (i = 0; i < num_interfaces; i++) {
    int j, k = 0;
    Scheme_Interface *in = (Scheme_Interface *)interfaces[i];
    imaps[i] = MALLOC_N(short, in->num_names);
    for (j = 0; j < in->num_names; j++) {
      while ((k < num_public) && SLESSTHAN(public_names[k], in->names[j]))
	k++;
      if ((k >= num_public) || !SEQUALS(public_names[k], in->names[j])) {
	const char *cl, *inn;
	char buffer[20], *bf;

	cl = get_class_name((Scheme_Object *)sclass, " by class: ");
	inn = get_interface_name((Scheme_Object *)in, ": ");

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
	
	scheme_raise_exn(MZEXN_OBJECT_IMPLEMENT,
			 in->names[j],
			 CLASS_STAR ": ivar not implemented: %s%s"
			 " as required by the %sinterface%s",
			 scheme_symbol_name(in->names[j]),
			 cl, bf, inn);
	return NULL;
      }
      imaps[i][in->name_map[j]] = sclass->public_map[k];
    }
  }

  sclass->max_let_depth = data->max_let_depth;

  i = sclass->closure_size = data->closure_size;
  if (i) {
    Scheme_Object **saved, **stack;
    short *map = data->closure_map;

    saved = sclass->closure_saved = MALLOC_N(Scheme_Object *, i);
    stack = scheme_current_process->runstack;
    while (i--)
      saved[i] = stack[map[i]];
  }

  return (Scheme_Object *)sclass;
}

static Scheme_Object *DefineClass_Execute(Scheme_Object *form)
{
  return _DefineClass_Execute(form, FALSE);
}

static Scheme_Object *DefineClass_Link(Scheme_Object *form, Link_Info *info)
{
  Class_Data *data;
  int i;
  ClassVariable *ivar;

  data = (Class_Data *)form;

  data->super_expr = scheme_link_expr(data->super_expr, info);
  
  for (i = data->num_interfaces; i--; )
    data->interface_exprs[i] = scheme_link_expr(data->interface_exprs[i], info);

  i = data->closure_size;
  info = scheme_link_info_extend(info, 0, 0, i);
  while (i--) {
    int pos = data->closure_map[i], flags;
    data->closure_map[i] = scheme_link_info_lookup(info, pos, &flags);
    scheme_link_info_add_mapping(info, pos, i, flags);
  }

  i = data->num_private;
  info = scheme_link_info_extend(info, i, i, i);
  while (i--)
    scheme_link_info_add_mapping(info, i, i, SCHEME_INFO_BOXED);

  i = data->num_ref;
  info = scheme_link_info_extend(info, i, i, i);
  while (i--)
    scheme_link_info_add_mapping(info, i, i, SCHEME_INFO_BOXED);

  i = data->num_ivar;
  info = scheme_link_info_extend(info, i, i, i);
  while (i--)
    scheme_link_info_add_mapping(info, i, i, SCHEME_INFO_BOXED);

  i = data->num_arg_vars + 2;
  info = scheme_link_info_extend(info, i, i, i);
  while (i--)
    scheme_link_info_add_mapping(info, i, i, SCHEME_INFO_BOXED);

  for (ivar = data->ivars; ivar; ivar = ivar->next) {
    switch(ivar->vartype) {
    case varPUBLIC:
    case varPRIVATE:
    case varNOTHING:
    case varINPUT:
      ivar->u.value = scheme_link_expr(ivar->u.value, info);
      break;
    case varINHERIT:
    case varRENAME:
      break;
    }
  }

  return scheme_make_syntax_link(DefineClass_Execute, 
				 (Scheme_Object *)data);
}

static void InitData(Class_Data *data)
{
  ClassVariable *item;
  int pub_index = 0, ref_index = 0, priv_index = 0;
  int input_index;
  
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

  data->ivar_names = NULL;
  
  InsureNamesReady(data->ivars, data->num_ivar, &data->ivar_names);
}

static Scheme_Object *Do_DefineClass(Scheme_Object *form, Scheme_Comp_Env *env,
				     Scheme_Compile_Info *rec, int depth)
{
#define BAD_MSG ": bad syntax in class definition"

  DupCheckRecord *r, *er, drec, erec;
  Scheme_Object *l, *superclass, *vars, *tag;
  Scheme_Object *superinitname, **interfaces;
  Scheme_Object *pub, *priv, *ref, *objl;
  ClassVariable *ivars = NULL, *next = NULL;
  Scheme_Object *make_args, *thisname;
  Scheme_Comp_Env *pubenv, *prienv, *refenv, *objenv, *firstenv;
  Class_Data *data;
  ClassVariable *item;
  int num_args, num_required_args, num_arg_vars, num_interfaces;

  drec.scheck_size = 0;
  erec.scheck_size = 0;

  l = SCHEME_CDR(form);

  if (!SCHEME_LISTP(l))
    scheme_wrong_syntax(CLASS_STAR, NULL, form, NULL);
  if (SCHEME_NULLP(l))
    scheme_wrong_syntax(CLASS_STAR_W_NAMES, NULL, form, 
			"missing this/super-init specification");
    
  vars = SCHEME_CAR(l);
  if (!SCHEME_PAIRP(vars))
    scheme_wrong_syntax(CLASS_STAR_W_NAMES, NULL, form, 
			"bad this/super-init specification");
  thisname = SCHEME_CAR(vars);
  scheme_check_identifier(CLASS_STAR_W_NAMES, thisname, NULL, env, form);

  vars = SCHEME_CDR(vars);
  if (!SCHEME_PAIRP(vars))
    scheme_wrong_syntax(CLASS_STAR_W_NAMES, NULL, form, 
			"bad this/super-init specification");
  superinitname = SCHEME_CAR(vars);
  scheme_check_identifier(CLASS_STAR_W_NAMES, superinitname, NULL, env, form);
      
  vars = SCHEME_CDR(vars);
  if (!SCHEME_NULLP(vars))
    scheme_wrong_syntax(CLASS_STAR_W_NAMES, NULL, form, 
			"bad this/super-init specification (extra syntax after %s)",
			scheme_symbol_name(superinitname));

  l = SCHEME_CDR(l);

  if (!SCHEME_LISTP(l))
    scheme_wrong_syntax(CLASS_STAR, NULL, form, NULL);
  if (SCHEME_NULLP(l))
    scheme_wrong_syntax(CLASS_STAR, NULL, form, 
			"missing superclass specification");
  superclass = SCHEME_CAR(l);
  
  l = SCHEME_CDR(l);

  if (!SCHEME_LISTP(l))
    scheme_wrong_syntax(CLASS_STAR, NULL, form, NULL);
  if (SCHEME_NULLP(l))
    scheme_wrong_syntax(CLASS_STAR, NULL, form, 
			"missing interfaces specification");

  num_interfaces = 0;
  {
    Scheme_Object *il = SCHEME_CAR(l);

    while (SCHEME_PAIRP(il)) {
      num_interfaces++;
      il = SCHEME_CDR(il);
    }

    if (!SCHEME_NULLP(il)) {
      scheme_wrong_syntax(CLASS_STAR, il, form, 
			  "bad interfaces specification");
    }

    if (num_interfaces) {
      int i;
      interfaces = MALLOC_N(Scheme_Object*, num_interfaces);
      il = SCHEME_CAR(l);
      for (i = 0; i < num_interfaces; i++, il = SCHEME_CDR(il))
	interfaces[i] = SCHEME_CAR(il);
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
	  scheme_wrong_syntax(CLASS_STAR, args, form, 
			      "bad syntax in argument list");

	v = SCHEME_CAR(args);
	if (SCHEME_SYMBOLP(v)) {
	  if (found_nonrequired)
	    scheme_wrong_syntax(CLASS_STAR, v, form, 
				"bad syntax in argument list");
	  num_required_args++;
	} else {
	  if (!SCHEME_PAIRP(v) || !SCHEME_SYMBOLP(SCHEME_CAR(v)))
	    scheme_wrong_syntax(CLASS_STAR, v, form, 
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
    scheme_wrong_syntax(CLASS_STAR, NULL, form, 
			"missing argument list");
    return scheme_void;
  }

  while (1) {
    if (!SCHEME_LISTP(l))
      scheme_wrong_syntax(CLASS_STAR, l, form, NULL);

    if (SCHEME_NULLP(l))
      break;
    vars = SCHEME_CAR(l);

    if (!SCHEME_PAIRP(vars))
      scheme_wrong_syntax(CLASS_STAR, vars, form, NULL);

    tag = SCHEME_CAR(vars);
    vars = SCHEME_CDR(vars);

    if (SAME_OBJ(tag, seq_symbol)) {
      Scheme_Object *l = vars;
      while (SCHEME_PAIRP(l))
	l = SCHEME_CDR(l);
      if (!SCHEME_NULLP(l))
	scheme_wrong_syntax(CLASS_STAR, l, form, 
			    "bad syntax in sequence clause"
			    " (" IMPROPER_LIST_FORM ")");
      
      next = ReadItemList("sequence", vars, varNOTHING, next, &ivars, form);
    } else if (SAME_OBJ(tag, pub_symbol)) {
      CheckIvarList(tag, vars, check_CanRename, form);
      next = ReadItemList("public", vars, varPUBLIC, next, &ivars, form);
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
	scheme_wrong_syntax(CLASS_STAR, tag, form, 
			    "bad syntax (bad clause keyword)");
      else
	scheme_wrong_syntax(CLASS_STAR, tag, form, 
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
  r = scheme_begin_dup_symbol_check(&drec);
  er = scheme_begin_dup_symbol_check(&erec);
  item = ivars;
  while (item) {
    if (item->vartype != varNOTHING) {
      scheme_dup_symbol_check(r, CLASS_STAR, IVAR_INT_NAME(item), "internal ivar", form, 0);
      if (ispublic(item))
	scheme_dup_symbol_check(er, CLASS_STAR, IVAR_EXT_NAME(item), "external ivar", form, 0);
    }
    item = item->next;
  }
  
  /* Add `super-init' */
  objl = cons(superinitname, scheme_null);
  scheme_dup_symbol_check(r, CLASS_STAR, superinitname, "ivar", form, 0);

  /* Add `this' */
  objl = cons(thisname, objl);
  scheme_dup_symbol_check(r, CLASS_STAR, thisname, "ivar", form, 0);

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
    Scheme_Object *first, *last, *il;

    superclass = scheme_expand_expr(superclass, env, depth);

    {
      int i;
      for (i = 0; i < num_interfaces; i++)
	interfaces[i] = scheme_expand_expr(interfaces[i], env, depth);
      
      il = scheme_null;
      for (i = num_interfaces; i--; )
	il = cons(interfaces[i], il);
    }

    l = SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(form)))));
    first = scheme_null;
    last = NULL;
    while (!SCHEME_NULLP(l)) {
      vars = SCHEME_CAR(l);

      tag = SCHEME_CAR(vars);

      if (SAME_OBJ(tag, pub_symbol) 
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
			       cons(make_args, first)))));
  } else {
    /* Compiling: */
    Scheme_Compile_Info lam;
    Scheme_Compile_Info *recs;

    data = MALLOC_ONE_TAGGED(Class_Data);
    data->type = scheme_class_data_type;

    data->defname = rec->value_name;
    scheme_compile_rec_done_local(rec);

    recs = MALLOC_N(Scheme_Compile_Info, num_interfaces + 1);
    scheme_init_compile_recs(rec, recs, num_interfaces + 1);

    data->super_init_name = superinitname;
    data->super_expr = scheme_compile_expr(superclass, env, recs);
    
    {
      int i;
      for (i = 0; i < num_interfaces; i++)
	interfaces[i] = scheme_compile_expr(interfaces[i], env, recs + i + 1);
    }

    data->num_interfaces = num_interfaces;
    data->interface_exprs = interfaces;

    scheme_merge_compile_recs(rec, recs, num_interfaces + 1);

    scheme_init_lambda_rec(rec, &lam);
  
    CompileItemList(form, ivars, objenv, data, &lam);

    scheme_merge_lambda_rec(rec, &lam);

    data->ivars = ivars;

    data->num_args = num_args;
    data->num_required_args = num_required_args;
    data->num_arg_vars = num_arg_vars;

    InitData(data);

    data->max_let_depth = (lam.max_let_depth
			   + data->num_arg_vars
			   + data->num_ivar
			   + data->num_private
			   + data->num_ref
			   + 2); /* this + super-init = 2 */

    scheme_env_make_closure_map(firstenv, &data->closure_size, &data->closure_map);

    return scheme_make_syntax_compile(DefineClass_Link, 
				      (Scheme_Object *)data);
  }
}

static Scheme_Object *DefineClass(Scheme_Object *form, Scheme_Comp_Env *env,
				  Scheme_Compile_Info *rec)
{
  return Do_DefineClass(form, env, rec, 0);
}

static Scheme_Object *DefineClass_expand(Scheme_Object *form, Scheme_Comp_Env *env,
					 int depth)
{
  return Do_DefineClass(form, env, NULL, depth);
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
    cvar = MALLOC_ONE(ClassVariable);
    cvar->next = prev;

    cvar->name = SCHEME_CAR(l);
    l = SCHEME_CDR(l);

    cvar->vartype = SCHEME_INT_VAL(SCHEME_CAR(l));
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
  
  for (i = 0; i < data->num_interfaces; i++)
    l = cons(data->interface_exprs[i], l);

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

  data->ivars = CV_Unbundle(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  data->super_expr = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);

  data->super_init_name = SCHEME_CAR(obj);
  obj = SCHEME_CDR(obj);
  
  data->num_args = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  data->num_required_args = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  data->num_arg_vars = SCHEME_INT_VAL(SCHEME_CAR(obj));
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
  data->interface_exprs = MALLOC_N(Scheme_Object*, data->num_interfaces);

  for (i = data->num_interfaces; i--; ) {
    data->interface_exprs[i] = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);
  }

  InitData(data);

  return (Scheme_Object *)data;
}

static Scheme_Object *write_Interface_Data(Scheme_Object *obj)
{
  Interface_Data *data;
  int i;
  Scheme_Object *l = scheme_null;

  data = (Interface_Data *)obj;
  
  for (i = 0; i < data->num_supers; i++)
    l = cons(data->super_exprs[i], l);

  for (i = 0; i < data->num_names; i++)
    l = cons(data->names[i], l);
  
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

  data->num_names = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  data->num_supers = SCHEME_INT_VAL(SCHEME_CAR(obj));
  obj = SCHEME_CDR(obj);

  data->defname = SCHEME_CAR(obj);
  if (SCHEME_NULLP(data->defname))
    data->defname = 0;
  obj = SCHEME_CDR(obj);

  data->names = MALLOC_N(Scheme_Object*, data->num_names);
  data->super_exprs = MALLOC_N(Scheme_Object*, data->num_supers);

  for (i = data->num_names; i--; obj = SCHEME_CDR(obj))
    data->names[i] = SCHEME_CAR(obj);

  for (i = data->num_supers; i--; obj = SCHEME_CDR(obj))
    data->super_exprs[i] = SCHEME_CAR(obj);

  /* Sort names: */
  qsort((char *)data->names, data->num_names,
	sizeof(Scheme_Object *), 
	(int (*)(const void *, const void *))CompareObjectPtrs); 

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

  if (!sup)
    sup = NullClass();

  superclass = (Scheme_Class *)sup;

  InstallHeritage(sclass, superclass);

  num_methods += superclass->num_public;

  sclass->num_public = num_methods;
  sclass->public_names = MALLOC_N(Scheme_Object*, num_methods);
  sclass->cmethods = MALLOC_N(CMethod*, num_methods);
  sclass->cmethod_ready_level = MALLOC_N_ATOMIC(short, num_methods);
  sclass->cmethod_source_map = MALLOC_N_ATOMIC(short, num_methods);
  sclass->public_map = MALLOC_N_ATOMIC(short, num_methods);
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

  sclass->type = scheme_class_type;

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

  sclass->defname = scheme_intern_symbol(name);

  return (Scheme_Object *)sclass;
}

static const char *get_class_name(Scheme_Object *sclass, const char *mode)
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

static const char *get_interface_name(Scheme_Object *i, const char *mode)
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
    return SCHEME_SYM_VAL(n);
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
    return SCHEME_SYM_VAL(n);
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

  cmethod = MALLOC_ONE(CMethod);
  cmethod->f = (Scheme_Closed_Prim *)f;
  cmethod->mina = mina;
  cmethod->maxa = maxa;

  len = SCHEME_SYM_LEN(sclass->defname);
  len2 =  strlen(name);
  cmethod->closed_name = (char *)scheme_malloc_atomic(len + len2 + 3);
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

void scheme_made_class(Scheme_Object *c)
{
  Scheme_Class *sclass;
  int i, num_methods;

  sclass = (Scheme_Class *)c;

  num_methods = sclass->num_private;
  sclass->num_private = 0;
  sclass->num_public = num_methods;

  sclass->vslot_map = MALLOC_N_ATOMIC(short, num_methods);
  sclass->vslot_kind = MALLOC_N_ATOMIC(slotkind, num_methods);

  for (i = 0; i < num_methods; i++) {
    sclass->vslot_map[i] = i;
    sclass->vslot_kind[i] = slot_TYPE_CMETHOD;
  }
}

/**********************************************************************/

Scheme_Class_Assembly *
scheme_make_class_assembly(const char *name, int num_interfaces,
			   int n_public, Scheme_Object **publics,
			   int n_inh, Scheme_Object **inheriteds,
			   int n_ren, Scheme_Object **renames,
			   int mina, int maxa,
			   Scheme_Instance_Init_Proc *initproc)
{
  Scheme_Class_Assembly *a;
  int i;
  ClassVariable *v, *last = NULL;

  a = MALLOC_ONE(Scheme_Class_Assembly);
  
  a->mina = mina;
  a->maxa = maxa;
  a->init = initproc;

  for (i = 0; i < n_public; i++) {
    v = MALLOC_ONE(ClassVariable);

    v->vartype = varPUBLIC;
    v->name = publics[i];
    v->u.value = scheme_undefined;
    v->next = last;

    last = v;
  }

  for (i = 0; i < n_inh; i++) {
    v = MALLOC_ONE(ClassVariable);

    v->vartype = varINHERIT;
    
    v->name = inheriteds[i];
    v->u.source.name = inheriteds[i];
    v->next = last;

    last = v;
  }

  for (i = 0; i < n_ren; i++) {
    v = MALLOC_ONE(ClassVariable);

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

  a->data.defname = (name ? scheme_intern_symbol(name) : NULL);

  InitData(&a->data);

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
  else
    a->data.super_expr = NullClass();

  a->data.interface_exprs = interfaces;

  o = _DefineClass_Execute((Scheme_Object *)&a->data, TRUE);

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
  Scheme_Interface_Assembly *a = MALLOC_ONE(Scheme_Interface_Assembly);
  int i;

  a->data.num_names = n_names;
  a->data.names = MALLOC_N(Scheme_Object *, n_names);
  for (i = n_names; i--; )
    a->data.names[i] = names[i];

  a->data.num_supers = n_supers;
  a->data.defname = (name ? scheme_intern_symbol(name) : NULL);

  /* Sort names: */
  qsort((char *)a->data.names, n_names,
	sizeof(Scheme_Object *), 
	(int (*)(const void *, const void *))CompareObjectPtrs); 

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

static int DoFindName(int num_public, Scheme_Object **pn, Scheme_Object *symbol)
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
    if (((long)n) < ((long)symbol)) {
      w = o + w - p;
      o = p;
    } else
      w = p - o;
    p = o + (w >> 1);
  }

  return p;
}

static Scheme_Object *GetIvar(Internal_Object *obj, Init_Object_Rec *irec, Scheme_Object **slots, 
			      Scheme_Class *oclass, Scheme_Class *mclass, short vp,
			      int boxed, int force)
{
  if (mclass->vslot_kind[vp] == slot_TYPE_IVAR) {
    Scheme_Object *v = slots[oclass->vslot_map[vp]];
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
	Init_Frame *frame = irec->frames + level;
	int cindex = sclass->vslot_map[vp];
	int lpos = sclass->cmethod_source_map[cindex];

	if (!frame->cmethods)
	  frame->cmethods = MALLOC_N(Scheme_Object*, sclass->contributes_cmethods);

	if (!frame->cmethods[lpos]) {
	  Scheme_Object *box;

	  if (irec->init_level > level) {
	    /* Not ready yet; return #<undefined> for now. */
	    box = scheme_make_envunbox(scheme_undefined);
	  } else
	    box = scheme_make_envunbox(CloseMethod(sclass->cmethods[cindex], obj));
	  
	  frame->cmethods[lpos] = box;
	}

	if (boxed)
	  return frame->cmethods[lpos];
	else
	  return SCHEME_ENVBOX_VAL(frame->cmethods[lpos]);
      } else {
	Scheme_Object *m = CloseMethod(mclass->cmethods[mclass->vslot_map[vp]], obj);
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
			     Init_Frame *frame,
			     Scheme_Object **slots)
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
  frame->ivars = ivars;
  
  if (sclass->num_ref)
    refs = MALLOC_N(Scheme_Object*, sclass->num_ref);
  else
    refs = NULL;
  frame->refs = refs;

  for (cvar = sclass->ivars; cvar; cvar = cvar->next) {
    if (ispublic(cvar)) {
      box = scheme_make_envunbox(scheme_undefined);
      slots[oclass->vslot_map[sclass->ivar_map[cvar->index]]] = box;
      ivars[cvar->index] = box;
    } else if (isprivref(cvar)) {
      Scheme_Class *superclass = sclass->superclass;
      int vp = sclass->ref_map[cvar->index];
      /* If the superclass's ivar is a cmethod, the box will contain
	 #<undefined>. */
      refs[cvar->index] = GetIvar(obj, irec, slots, oclass, superclass, vp, 1, 1);
    }
  }
}

static Init_Object_Rec *CreateObjectFrames(Internal_Object *obj, Scheme_Class *sclass)
{
  /* Called when object is created (before initialization) */
  Init_Frame *frames;
  Scheme_Object **slots;
  Init_Object_Rec *irec;
  int i;

  slots = obj->slots;
  irec = (Init_Object_Rec *)scheme_malloc(sizeof(Init_Object_Rec)
					  + (sclass->pos /* for +1 total */
					     * sizeof(Init_Frame)));
  frames = irec->frames;

  irec->init_level = sclass->pos + 1;

  for (i = 0; i <= sclass->pos; i++)
    BuildObjectFrame(obj, irec, sclass, sclass->heritage[i], frames + i, slots);

  return irec;
}


static void
PushFrameVariables(Internal_Object *o, Init_Object_Rec *irec, Scheme_Class *sclass, 
		   Init_Frame *frame, Scheme_Process *p,
		   Scheme_Object ***_priv_stack, Scheme_Object ***_obj_stack)
{
  /* Called at start of class-specific initialization */
  Scheme_Object **priv_stack, **pub_stack, **ref_stack, **obj_stack;
  Scheme_Object **slots;
  Scheme_Class *oclass;
  ClassVariable *cvar;
  int i, count;

  priv_stack = PUSH_RUNSTACK(p, p->runstack, sclass->num_private);
  ref_stack = PUSH_RUNSTACK(p, p->runstack, sclass->num_ref);
  pub_stack = PUSH_RUNSTACK(p, p->runstack, sclass->num_ivar);
  count = sclass->num_arg_vars + 2;
  obj_stack = PUSH_RUNSTACK(p, p->runstack, count);

  /* Privates: create boxes: */
  for (i = sclass->num_private; i--; )
    priv_stack[i] = scheme_make_envunbox(scheme_undefined);

  /* this & super-init */
  obj_stack[sclass->num_arg_vars] = scheme_make_envunbox((Scheme_Object *)o);
  obj_stack[sclass->num_arg_vars + 1] = scheme_make_envunbox(MakeSuperInitPrim(o, irec, sclass->pos - 1));
  
  slots = o->slots;
  cvar = sclass->ivars;
  oclass = (Scheme_Class *)o->o.sclass;

  /* Push boxes for public ivars & refs onto stack */
  for (i = 0; cvar; cvar = cvar->next) {
    if (ispublic(cvar)) {
      pub_stack[cvar->index] = GetIvar(o, irec, slots, oclass, oclass, sclass->ivar_map[cvar->index], 1, 1);
    } else if (isprivref(cvar)) {
      ref_stack[cvar->index] = frame->refs[cvar->index];
    } else if (isref(cvar)) {
      Scheme_Object *box;
      /* If the superclass's ivar is a cmethod, the box will contain
	 #<undefined>. */
      box = GetIvar(o, irec, slots, oclass, oclass, sclass->ref_map[cvar->index], 1, 1);
      frame->refs[cvar->index] = box;
      ref_stack[cvar->index] = box;
    }
  }

  *_obj_stack = obj_stack;
  *_priv_stack = priv_stack;
}

/**************************************************************************/

static void SetIVarValues(Init_Frame *frame, /* Frame */
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
	box = frame->ivars[items->index];
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
  Init_Frame *frame;
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

  orig_stack = p->runstack;

  frame = irec->frames + level;
  sclass = ((Scheme_Class *)o->o.sclass)->heritage[level];

  if (!skip_cmethods && sclass->contributes_cmethods && frame->cmethods) {
    /* Fill in any cached boxed */
    int i;
    for (i = sclass->num_public; i--; ) {
      if (sclass->vslot_kind[i] == slot_TYPE_CMETHOD) {
	int cindex = sclass->vslot_map[i];
	if (sclass->cmethod_ready_level[cindex] == level) {
	  int lpos = sclass->cmethod_source_map[cindex];
	  if (frame->cmethods[lpos])
	    SCHEME_ENVBOX_VAL(frame->cmethods[lpos])
	      = CloseMethod(sclass->cmethods[cindex], o);
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
      p->ku.k.p1 = o;
      p->ku.k.i1 = -1;
      p->ku.k.i2 = argc;
      p->ku.k.p2 = (void *)MALLOC_N(Scheme_Object*, argc);
      p->ku.k.p3 = irec;
      for (i = argc; i--; )
	((Scheme_Object **)p->ku.k.p2)[i] = argv[i];

      (void)scheme_enlarge_runstack(total, init_obj_frame_k);
      return;
    }

    saved = sclass->closure_saved;
    stack = PUSH_RUNSTACK(p, p->runstack, i);
    while (i--)
      stack[i] = saved[i];

    PushFrameVariables(o, irec, sclass, frame, scheme_current_process,
		       &priv_stack, &obj_stack);

    /* Set init arg values in environment: */
    for (i = sclass->num_arg_vars, j = 0; i--; j++) {
      if (i < argc) {
	if (!j && (sclass->num_args < 0)) {
	  /* Turn rest of args into a list */
	  int k;
	  Scheme_Object *l = scheme_null;
	  
	  for (k = argc; k-- > i; )
	    l = cons(argv[k], l);
	  
	  obj_stack[j] = scheme_make_envunbox(l);
	} else {
#if 0
	  /* Box everything for now: */
	  if (i < frame->sclass->num_required_args)
	    obj_stack[j] = argv[i];
	  else
#endif
	    obj_stack[j] = scheme_make_envunbox(argv[i]);
	}
      } else
	obj_stack[j] = scheme_make_envunbox(scheme_undefined);
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

  SetIVarValues(frame, priv_stack, obj_stack, sclass->ivars, pthresh);

  p->runstack = orig_stack;

  if (sclass->priminit == pi_CPP) {
    if (sclass->superclass->pos)
      InitObjectFrame(o, irec, level - 1, 0, NULL);
    else
      irec->init_level = 0;
  } else if (((sclass->priminit == pi_NOT) || (sclass->priminit == pi_NOT_OVER_CPP)) && (irec->init_level >= level)) {
    if (sclass->superclass->pos) {
      const char *cl = get_class_name((Scheme_Object *)sclass, " in class: ");

      scheme_raise_exn(MZEXN_OBJECT_INIT_NEVER,
		       o, 
		       sclass->superclass,
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
  int sp, vp;

  sp = FindName(oclass, name);
  if (sp < 0)
    return NULL;

  vp = oclass->public_map[sp];

  return GetIvar(obj, NULL, obj->slots, oclass, oclass, vp, 0, force);
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

  *irec = CreateObjectFrames(obj, sclass);
  
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

      cl = get_class_name((Scheme_Object *)sclass, "initialization for class: ");
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
    Init_Frame *frame;

    superinit = MakeSuperInitPrim(o, irec, level - 1);

    oclass = (Scheme_Class *)o->o.sclass;
    frame = irec->frames + level;

    c = sclass->num_ivar + sclass->num_ref;
    public_values = (Scheme_Object **)scheme_malloc(sizeof(Scheme_Object *) * c);
    env_values = (Scheme_Object **)scheme_malloc(sizeof(Scheme_Object *) * c);
    for (cvar = sclass->ivars, i = c; i--; cvar = cvar->next) {
      if (ispublic(cvar)) {
	env_values[i] = GetIvar(o, irec, o->slots, oclass, oclass, sclass->ivar_map[cvar->index], 1, 1);
	public_values[i] = frame->ivars[cvar->index];
      } else if (isprivref(cvar)) {
	public_values[i] = frame->refs[cvar->index];
	env_values[i] = public_values[i];
      } else if (isref(cvar)) {
	public_values[i] = GetIvar(o, irec, o->slots, oclass, oclass, sclass->ref_map[cvar->index], 1, 1);
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
      if (sclass->superclass->pos) {
	const char *cl = get_class_name((Scheme_Object *)sclass, " for class: ");
	
	scheme_raise_exn(MZEXN_OBJECT_INIT_NEVER,
			 o,
			 sclass->superclass,
			 CREATE_OBJECT ": superclass never initialized%s",
			 cl);
      }
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
  Scheme_Object *classname;
  Scheme_Class *sclass;
  Internal_Object *obj;

  classname = argv[0];

  if (!SAME_TYPE(SCHEME_TYPE(classname), scheme_class_type))
    scheme_wrong_type(CREATE_OBJECT, "class", 0, argc, argv);

  sclass = (Scheme_Class *)classname;

  if ((sclass->priminit == pi_CPP)
      && !sclass->piu.initf)
    scheme_raise_exn(MZEXN_OBJECT_PRIVATE_CLASS,
		     sclass,
		     "can't create objects from the class %s",
		     SCHEME_SYM_VAL(sclass->defname));
  
  obj = (Internal_Object *)make_object(sclass, argc  - 1, argv + 1);
  
  return (Scheme_Object *)&obj->o;
}

/************************************************************************/

static Scheme_Object *DoSuperInitPrim(SuperInitData *data,
				      int argc, Scheme_Object **argv)
{
  if (data->irec->init_level <= data->level) {
    Scheme_Class **heritage = ((Scheme_Class *)data->o->o.sclass)->heritage;
    const char *cl = get_class_name((Scheme_Object *)heritage[data->level + 1], 
				    " in class: ");
    scheme_raise_exn(MZEXN_OBJECT_INIT_MULTIPLE,
		     data->o,
		     heritage[data->level],
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

  data = MALLOC_ONE(SuperInitData);

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
    
    sp = DoFindName(in->num_names, in->names, name);

    if (sp < 0)
      return NULL;

    vp = in->name_map[sp];
    kind = generic_KIND_INTERFACE;
  }
  
  data = MALLOC_ONE_TAGGED(Generic_Data);

  data->type = scheme_generic_data_type;
  data->kind = kind;
  data->clori = clori;
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
  int vp;

  data = (Generic_Data *)gdata;

  obj = (Internal_Object *)sobj;

  sclass = (Scheme_Class *)obj->o.sclass;
  
  if (data->kind == generic_KIND_CLASS) {
    if (NOT_SAME_OBJ((Scheme_Object *)sclass, data->clori)) {
      if (!scheme_is_subclass((Scheme_Object *)sclass, data->clori)) {
	const char *cl = get_class_name(data->clori, ": ");
	scheme_raise_exn(MZEXN_OBJECT_GENERIC,
			 obj,
			 "generic" ": object not an instance of the generic's class%s",
			 cl);
	return NULL;
      }
    }
    vp = data->vp;
  } else {
    short *map;
    int offset;

    map = find_implementation((Scheme_Object *)sclass, data->clori, &offset);
    if (!map) {
      const char *inn = get_interface_name(data->clori, ": ");
      scheme_raise_exn(MZEXN_OBJECT_GENERIC,
		       obj,
		       "generic" ": object not an instance of the generic's interface%s",
		       inn);
      return NULL;
    }

    vp = map[data->vp + offset];
  }

  return GetIvar(obj, NULL, obj->slots, sclass, sclass, vp, 0, force);
}

static Scheme_Object *DoGeneric(Generic_Data *data, 
				int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OBJP(argv[0]))
    scheme_wrong_type("generic", "object", 0, argc, argv);

  return scheme_apply_generic_data((Scheme_Object *)data, argv[0], 1);
}

static Scheme_Object *MakeGeneric(int argc, Scheme_Object *argv[])
{
  Scheme_Object *data;
  Scheme_Object *prim;

  if (!SCHEME_CLASSP(argv[0]) && !SCHEME_INTERFACEP(argv[0]))
    scheme_wrong_type(MAKE_GENERIC, "class or interface", 0, argc, argv);
  if (!SCHEME_SYMBOLP(argv[1]))
    scheme_wrong_type(MAKE_GENERIC, "symbol", 1, argc, argv);

  data = scheme_get_generic_data(argv[0], argv[1]);

  if (!data) {
    const char *s;

    if (SCHEME_CLASSP(argv[0]))
      s = get_class_name(argv[0], " in class: ");
    else
      s = get_class_name(argv[0], " in interface: ");

    scheme_raise_exn(MZEXN_OBJECT_CLASS_IVAR,
		     argv[0],
		     argv[1],
		     MAKE_GENERIC ": can't find instance variable: %s%s",
		     scheme_symbol_name(argv[1]),
		     s);

    return NULL;
  }

  prim = scheme_make_closed_prim((Scheme_Closed_Prim *)DoGeneric, (void *)data);

  ((Scheme_Closed_Primitive_Proc *)prim)->name = "generic";
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

    cl = get_class_name(obj->o.sclass, " in class: ");
    
    scheme_raise_exn(MZEXN_OBJECT_IVAR,
		     obj,
		     name,
		     IVAR ": instance variable not found: %s%s",
		     scheme_symbol_name(name),
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

static short *find_implementation(Scheme_Object *c, Scheme_Object *n, int *offset)
{
  Scheme_Class *sclass;
  Scheme_Interface *in, **ins;
  int i;

  sclass = (Scheme_Class *)c;
  in = (Scheme_Interface *)n;

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
  int i;
  Scheme_Interface *ex, *base, **ins;

  if (!SCHEME_INTERFACEP(exn) || !SCHEME_INTERFACEP(basen))
    return 0;

  ex = (Scheme_Interface *)exn;
  base = (Scheme_Interface *)basen;

  ins = ex->supers;
  for (i = ex->num_supers; i--; )
    if (SAME_OBJ(base, ins[i]))
      return 1;

  return 0;
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

static Scheme_Object *ObjectClass(int c, Scheme_Object *p[])
{
  if (!SCHEME_OBJP(p[0]))
    scheme_wrong_type(OBJECT_CLASS, "object", 0, c, p);

  return ((Scheme_Class_Object *)p[0])->sclass;
}

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
    REGISTER_SO(null_class);

    REGISTER_SO(seq_symbol);
    REGISTER_SO(pub_symbol);
    REGISTER_SO(pri_symbol);
    REGISTER_SO(inh_symbol);
    REGISTER_SO(ren_symbol);
    
    REGISTER_SO(class_star_symbol);
    REGISTER_SO(interface_symbol);

    seq_symbol = scheme_intern_symbol("sequence");
    pub_symbol = scheme_intern_symbol("public");
    pri_symbol = scheme_intern_symbol("private");
    inh_symbol = scheme_intern_symbol("inherit");
    ren_symbol = scheme_intern_symbol("rename");
    
    class_star_symbol = scheme_intern_symbol("class*/names");
    interface_symbol = scheme_intern_symbol("interface");

    scheme_register_syntax("dc", DefineClass_Execute);
    scheme_register_syntax("if", Interface_Execute);

    scheme_install_type_writer(scheme_class_data_type, write_Class_Data);
    scheme_install_type_reader(scheme_class_data_type, read_Class_Data);
    scheme_install_type_writer(scheme_interface_data_type, write_Interface_Data);
    scheme_install_type_reader(scheme_interface_data_type, read_Interface_Data);
  }

  scheme_add_global_keyword(CLASS_STAR_W_NAMES, 
			    scheme_make_compiled_syntax(DefineClass, 
							DefineClass_expand), 
			    env);

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
  scheme_add_global_constant(IVAR_IN_CLASS,
			     scheme_make_folding_prim(IvarInClass,
						      IVAR_IN_CLASS,
						      2, 2, 1), 
			     env);
  scheme_add_global_constant(OBJECT_CLASS, 
			     scheme_make_folding_prim(ObjectClass, 
						      OBJECT_CLASS,
						      1, 1, 1), 
			     env);

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
    e += scheme_count_memory(cvar->name, ht);

    switch(cvar->vartype) {
    case varPUBLIC:
    case varPRIVATE:
    case varNOTHING:
    case varINPUT:
      e += scheme_count_memory(cvar->u.value, ht);
      break;
    case varINHERIT:
    case varRENAME:
      e += scheme_count_memory(cvar->u.source.name, ht);
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

  *e += scheme_count_memory((Scheme_Object *)sclass->superclass, ht);
  
  *e += count_cvars(sclass->ivars, ht);

  *e += scheme_count_closure(sclass->closure_saved, sclass->closure_size, ht);
}

void scheme_count_generic(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht)
{
  *s = sizeof(Generic_Data);
  *e = scheme_count_memory(((Generic_Data *)o)->clori, ht);
}

void scheme_count_class_data(Scheme_Object *o, long *s, long *e, Scheme_Hash_Table *ht)
{
  Class_Data *data = (Class_Data *)o;

  *s = (sizeof(Class_Data)
	+ (data->num_ivar + data->num_private + data->num_ref) * sizeof(ClassVariable)
	+ data->closure_size * sizeof(short));

  *e += scheme_count_memory(data->super_expr, ht);

  *e = 0;
}

#endif

#endif
/* NO_OBJECT_SYSTEM */
