#ifndef WXS_SETUP_ONLY
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxColour(class wxColour *);
extern Scheme_Object *objscheme_bundle_wxColour(class wxColour *);
extern Scheme_Object *objscheme_bundle_wxColour(class wxColour *);
extern class wxCommandEvent *objscheme_unbundle_wxCommandEvent(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxItem(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxItem(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxItem(class wxItem *realobj);
class wxItem *objscheme_unbundle_wxItem(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxMouseEvent(class wxMouseEvent *);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern int objscheme_istype_wxBitmap(Scheme_Object *, const char *, int);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern int objscheme_istype_wxPanel(Scheme_Object *, const char *, int);
extern int objscheme_istype_wxBitmap(Scheme_Object *, const char *, int);
extern class wxPanel *objscheme_unbundle_wxPanel(Scheme_Object *, const char *, int);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern class wxPanel *objscheme_unbundle_wxPanel(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxMessage(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxMessage(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxMessage(class wxMessage *realobj);
class wxMessage *objscheme_unbundle_wxMessage(Scheme_Object *obj, const char *where, int nullOK);
#endif
