#ifndef WXS_SETUP_ONLY
#endif
void objscheme_setup_wxTimer(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxTimer(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxTimer(class wxTimer *realobj);
class wxTimer *objscheme_unbundle_wxTimer(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxClipboardClient(class wxClipboardClient *);
extern class wxClipboardClient *objscheme_unbundle_wxClipboardClient(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxClipboard(void *env);
#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxClipboard(class wxClipboard *);
int objscheme_istype_wxClipboard(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxClipboard(class wxClipboard *realobj);
class wxClipboard *objscheme_unbundle_wxClipboard(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxClipboardClient(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxClipboardClient(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxClipboardClient(class wxClipboardClient *realobj);
class wxClipboardClient *objscheme_unbundle_wxClipboardClient(Scheme_Object *obj, const char *where, int nullOK);
extern class wxPrintSetupData *objscheme_unbundle_wxPrintSetupData(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxPrintSetupData(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxPrintSetupData(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxPrintSetupData(class wxPrintSetupData *realobj);
class wxPrintSetupData *objscheme_unbundle_wxPrintSetupData(Scheme_Object *obj, const char *where, int nullOK);
#endif
