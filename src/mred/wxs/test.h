extern int objscheme_istype_wxMenu(Scheme_Object *, const char *, int);
void objscheme_setup_x(Scheme_Env *env);
int objscheme_istype_x(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_x(class x *realobj);
class x *objscheme_unbundle_x(Scheme_Object *obj, const char *where, int nullOK);
