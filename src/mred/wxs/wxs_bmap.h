#ifndef WXS_SETUP_ONLY
extern class wxColourMap *objscheme_unbundle_wxColourMap(Scheme_Object *, const char *, int);
extern class wxColourMap *objscheme_unbundle_wxColourMap(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxBitmap(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxBitmap(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxBitmap(class wxBitmap *realobj);
class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *obj, const char *where, int nullOK);
#endif
