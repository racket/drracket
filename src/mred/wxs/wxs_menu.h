#ifndef WXS_SETUP_ONLY
extern int objscheme_istype_wxMenu(Scheme_Object *, const char *, int);
extern class wxMenu *objscheme_unbundle_wxMenu(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxMenu(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMenu(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMenu(class wxMenu *realobj);
class wxMenu *objscheme_unbundle_wxMenu(Scheme_Object *obj, const char *where, int nullOK);
extern class wxMenu *objscheme_unbundle_wxMenu(Scheme_Object *, const char *, int);
extern class wxMenu *objscheme_unbundle_wxMenu(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxMenuBar(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMenuBar(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMenuBar(class wxMenuBar *realobj);
class wxMenuBar *objscheme_unbundle_wxMenuBar(Scheme_Object *obj, const char *where, int nullOK);
#endif
