#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxMouseEvent(class wxMouseEvent *);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern class wxPanel *objscheme_unbundle_wxPanel(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxText(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxText(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxText(class wxText *realobj);
class wxText *objscheme_unbundle_wxText(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern class wxPanel *objscheme_unbundle_wxPanel(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxMultiText(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMultiText(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMultiText(class wxMultiText *realobj);
class wxMultiText *objscheme_unbundle_wxMultiText(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxMouseEvent(class wxMouseEvent *);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern class wxFont *objscheme_unbundle_wxFont(Scheme_Object *, const char *, int);
extern class wxMenu *objscheme_unbundle_wxMenu(Scheme_Object *, const char *, int);
extern int objscheme_istype_wxPanel(Scheme_Object *, const char *, int);
extern class wxPanel *objscheme_unbundle_wxPanel(Scheme_Object *, const char *, int);
extern class wxFrame *objscheme_unbundle_wxFrame(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxTextWindow(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxTextWindow(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxTextWindow(class wxTextWindow *realobj);
class wxTextWindow *objscheme_unbundle_wxTextWindow(Scheme_Object *obj, const char *where, int nullOK);
#endif
