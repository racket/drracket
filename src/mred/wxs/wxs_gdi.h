#ifndef WXS_SETUP_ONLY
#endif
void objscheme_setup_wxFont(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxFont(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxFont(class wxFont *realobj);
class wxFont *objscheme_unbundle_wxFont(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxFont(class wxFont *);
#endif
void objscheme_setup_wxFontList(void *env);
#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxFontList(class wxFontList *);
int objscheme_istype_wxFontList(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxFontList(class wxFontList *realobj);
class wxFontList *objscheme_unbundle_wxFontList(Scheme_Object *obj, const char *where, int nullOK);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxColour(class wxColour *);
#endif
void objscheme_setup_wxColour(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxColour(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxColour(class wxColour *realobj);
class wxColour *objscheme_unbundle_wxColour(Scheme_Object *obj, const char *where, int nullOK);
extern Scheme_Object *objscheme_bundle_wxColour(class wxColour *);
#endif
void objscheme_setup_wxColourDatabase(void *env);
#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxColourDatabase(class wxColourDatabase *);
int objscheme_istype_wxColourDatabase(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxColourDatabase(class wxColourDatabase *realobj);
class wxColourDatabase *objscheme_unbundle_wxColourDatabase(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxPoint(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxPoint(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxPoint(class wxPoint *realobj);
class wxPoint *objscheme_unbundle_wxPoint(Scheme_Object *obj, const char *where, int nullOK);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxBitmap(class wxBitmap *);
extern int objscheme_istype_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxColour(class wxColour *);
extern int objscheme_istype_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxBrush(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxBrush(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxBrush(class wxBrush *realobj);
class wxBrush *objscheme_unbundle_wxBrush(Scheme_Object *obj, const char *where, int nullOK);
extern int objscheme_istype_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxBrush(class wxBrush *);
#endif
void objscheme_setup_wxBrushList(void *env);
#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxBrushList(class wxBrushList *);
int objscheme_istype_wxBrushList(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxBrushList(class wxBrushList *realobj);
class wxBrushList *objscheme_unbundle_wxBrushList(Scheme_Object *obj, const char *where, int nullOK);
extern class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxBitmap(class wxBitmap *);
extern int objscheme_istype_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxColour(class wxColour *);
extern int objscheme_istype_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxPen(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxPen(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxPen(class wxPen *realobj);
class wxPen *objscheme_unbundle_wxPen(Scheme_Object *obj, const char *where, int nullOK);
extern int objscheme_istype_wxColour(Scheme_Object *, const char *, int);
extern class wxColour *objscheme_unbundle_wxColour(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxPen(class wxPen *);
#endif
void objscheme_setup_wxPenList(void *env);
#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxPenList(class wxPenList *);
int objscheme_istype_wxPenList(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxPenList(class wxPenList *realobj);
class wxPenList *objscheme_unbundle_wxPenList(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxCursor(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxCursor(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxCursor(class wxCursor *realobj);
class wxCursor *objscheme_unbundle_wxCursor(Scheme_Object *obj, const char *where, int nullOK);
extern class wxRegion *objscheme_unbundle_wxRegion(Scheme_Object *, const char *, int);
extern class wxRegion *objscheme_unbundle_wxRegion(Scheme_Object *, const char *, int);
extern class wxRegion *objscheme_unbundle_wxRegion(Scheme_Object *, const char *, int);
extern Scheme_Object *objscheme_bundle_wxDC(class wxDC *);
extern class wxDC *objscheme_unbundle_wxDC(Scheme_Object *, const char *, int);
#endif
void objscheme_setup_wxRegion(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxRegion(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxRegion(class wxRegion *realobj);
class wxRegion *objscheme_unbundle_wxRegion(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxFontNameDirectory(void *env);
#ifndef WXS_SETUP_ONLY
extern Scheme_Object *objscheme_bundle_wxFontNameDirectory(class wxFontNameDirectory *);
int objscheme_istype_wxFontNameDirectory(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxFontNameDirectory(class wxFontNameDirectory *realobj);
class wxFontNameDirectory *objscheme_unbundle_wxFontNameDirectory(Scheme_Object *obj, const char *where, int nullOK);
#endif
