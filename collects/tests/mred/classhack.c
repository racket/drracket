
#include "escheme.h"


/**************** Copied from plt/src/mzscheme/src/object.c **************/
typedef long ClassVariable;

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
  struct Scheme_Interface *equiv_intf; /* interface implied by this class */

  short num_args, num_required_args, num_arg_vars;
  short num_ivar, num_private, num_ref;
  short num_public, num_slots; /* num_vslots == num_public */
  Scheme_Object **public_names;
  /* ... */
} Scheme_Class;

typedef struct Scheme_Interface {
  Scheme_Type type;
  short num_names, num_supers;
  short for_class; /* 1 => impl iff subclass, 0 => normal interface */
  Scheme_Object **names;
  short *name_map; /* position in names => interface slot position */
  struct Scheme_Interface **supers; /* all superinterfaces (flattened hierarchy) */
  struct Scheme_Class *supclass;
  short *super_offsets; /* superinterface => super's slot position offset */
  Scheme_Object *defname;
} Scheme_Interface;

/*************************************************************************/

Scheme_Object *array_to_list(int c, Scheme_Object **names)
{
  Scheme_Object *p = scheme_null;

  while (c--)
    p = scheme_make_pair(names[c], p);

  return p;
}

Scheme_Object *arrays_to_list(int c1, Scheme_Object **ns1,
			      int c2, Scheme_Object **ns2)
     /* Merge arrays. Exploit the fact that they're both
	sorted. */
{
  Scheme_Object **ns;
  int c, i1, i2;

  ns = (Scheme_Object **)scheme_malloc(sizeof(Scheme_Object*) * (c1 + c2));
  c = i1 = i2 = 0;
  while ((i1 < c1) || (i2 < c2)) {
    if (i1 >= c1) {
      ns[c++] = ns2[i2++];
    } else if (i2 >= c2) {
      ns[c++] = ns1[i1++];
    } else {
      Scheme_Object *n1 = ns1[i1];
      Scheme_Object *n2 = ns2[i2];

      if (n1 == n2) {
	ns[c++] = n1;
	i1++;
	i2++;
      } else if ((unsigned long)n1 < (unsigned long)n2) {
	ns[c++] = ns1[i1++];
      } else {
	ns[c++] = ns2[i2++];
      }
    }
  }

  return array_to_list(c, ns);
}

Scheme_Object *class_to_names(int argc, Scheme_Object **argv)
{
  Scheme_Class *class = (Scheme_Class *)argv[0];

  if (!SCHEME_CLASSP(argv[0]))
    scheme_wrong_type("class->names", "class", 0, argc, argv);

  return array_to_list(class->num_public, class->public_names);
}

Scheme_Object *interface_to_names(int argc, Scheme_Object **argv)
{
  Scheme_Interface *interface = (Scheme_Interface *)argv[0];

  if (!SCHEME_INTERFACEP(argv[0]))
    scheme_wrong_type("interface->names", "interface", 0, argc, argv);

  return arrays_to_list(interface->num_names, interface->names,
			interface->supclass->num_public, interface->supclass->public_names);
}

Scheme_Object *interface_to_super_interfaces(int argc, Scheme_Object **argv)
{
  Scheme_Interface *interface = (Scheme_Interface *)argv[0];

  if (!SCHEME_INTERFACEP(argv[0]))
    scheme_wrong_type("interface->super-interfaces", "interface", 0, argc, argv);

  return array_to_list(interface->num_supers, (Scheme_Object**)interface->supers);
}


Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  return scheme_reload(env);
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  scheme_add_global("class->names", 
		    scheme_make_prim_w_arity(class_to_names,
					     "class->names",
					     1, 1),
		    env);
  scheme_add_global("interface->names", 
		    scheme_make_prim_w_arity(interface_to_names,
					     "interface->names",
					     1, 1),
		    env);
  scheme_add_global("interface->super-interfaces", 
		    scheme_make_prim_w_arity(interface_to_super_interfaces,
					     "interface->super-interfaces",
					     1, 1),
		    env);

  return scheme_void;
}
