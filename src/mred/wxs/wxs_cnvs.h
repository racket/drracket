#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxMouseEvent(class wxMouseEvent *);
extern Scheme_Object *objscheme_bundle_wxWindow(class wxWindow *);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern Scheme_Object *objscheme_bundle_wxCommandEvent(class wxCommandEvent *);
extern Scheme_Object *objscheme_bundle_wxKeyEvent(class wxKeyEvent *);
extern Scheme_Object *objscheme_bundle_wxMouseEvent(class wxMouseEvent *);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern class wxPen *objscheme_unbundle_wxPen(Scheme_Object *, const char *, int);
extern class wxFont *objscheme_unbundle_wxFont(Scheme_Object *, const char *, int);
extern class wxBrush *objscheme_unbundle_wxBrush(Scheme_Object *, const char *, int);
extern class wxBrush *objscheme_unbundle_wxBrush(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *, const char *, int);
extern class wxWindow *objscheme_unbundle_wxWindow(Scheme_Object *, const char *, int);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern class wxCommandEvent *objscheme_unbundle_wxCommandEvent(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxCanvasDC(class wxCanvasDC *);
extern class wxMenu *objscheme_unbundle_wxMenu(Scheme_Object *, const char *, int);
extern class wxKeyEvent *objscheme_unbundle_wxKeyEvent(Scheme_Object *, const char *, int);
extern class wxMouseEvent *objscheme_unbundle_wxMouseEvent(Scheme_Object *, const char *, int);
extern int objscheme_istype_wxPanel(Scheme_Object *, const char *, int);
extern class wxPanel *objscheme_unbundle_wxPanel(Scheme_Object *, const char *, int);
extern class wxFrame *objscheme_unbundle_wxFrame(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxCanvas(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxCanvas(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxCanvas(class wxCanvas *realobj);
class wxCanvas *objscheme_unbundle_wxCanvas(Scheme_Object *obj, const char *where, int nullOK);
#endif
