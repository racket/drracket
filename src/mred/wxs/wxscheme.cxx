
#if defined(_MSC_VER)
# include "wx.h"
#endif

#define Uses_XLib // Xt
#include "common.h" // wxWindows
#include "wx_media.h"
#include "wx_win.h"
#include "wxscheme.h"
#ifdef wx_msw
#include "wx_pdf.h"
#include "wx_main.h"
#endif
#include "wx_dcps.h"
#include "wxsmred.h"

#include "wxs_obj.h"
#define WXS_SETUP_ONLY 1
#include "wxs_win.h"
#include "wxs_fram.h"
#include "wxs_item.h"
#include "wxs_butn.h"
#include "wxs_ckbx.h"
#include "wxs_chce.h"
#include "wxs_evnt.h"
#include "wxs_panl.h"
#include "wxs_madm.h"
#include "wxs_mio.h"
#include "wxs_styl.h"
#include "wxs_menu.h"
#include "wxs_bmap.h"
#include "wxs_misc.h"
#include "wxs_rado.h"
#include "wxs_slid.h"
#include "wxs_gage.h"
#include "wxs_lbox.h"

#include "wxs_glob.h"

#undef WXS_SETUP_ONLY
#include "wxs_gdi.h"
#include "wxs_dc.h"
#include "wxs_cnvs.h"
#include "wxs_misc.h"
#include "wxs_medi.h"
#include "wxs_mede.h"
#include "wxs_mpb.h"
#include "wxs_snip.h"

#include <stdlib.h>

class GCBitmap {
public:
#ifdef MZ_PRECISE_GC
  Scheme_Object *canvasptr;
#else
  wxCanvas **canvasptr; /* weak reference */
#endif
  float x, y, w, h;
  float onx, ony, offx, offy;
  wxBitmap *on, *off;
  GCBitmap *next;
};

#ifdef MZ_PRECISE_GC
# define GET_CANVAS(gcbm) ((wxCanvas *)gcPTR_TO_OBJ(SCHEME_BOX_VAL(gcbm->canvasptr)))
#else
# define GET_CANVAS(gcbm) (*gcbm->canvasptr)
#endif


static GCBitmap *gc_bitmaps = NULL;
extern "C" void (*GC_collect_start_callback)(void);
extern "C" void (*GC_collect_end_callback)(void);
static void (*orig_collect_start_callback)(void);
static void (*orig_collect_end_callback)(void);
static void collect_start_callback(void);
static void collect_end_callback(void);

static void wxScheme_Install(Scheme_Env *env, void *global_env);

static Scheme_Object *setup_file_symbol, *init_file_symbol;

static Scheme_Object *get_file, *put_file, *get_ps_setup_from_user, *message_box, *execute;

static Scheme_Object *make_media_edit, *make_media_pasteboard, *make_media_snip, *none_symbol;

#define INSTALL_COUNT 520

static Scheme_Object *mred_unit_opener, *mred_sig;

#define CONS scheme_make_pair

typedef struct {
  int count;
  char *names[INSTALL_COUNT];
  Scheme_Object *vals[INSTALL_COUNT];
} InstallRec;

static void wxScheme_Invoke(Scheme_Env *env)
{
  Scheme_Object *save = scheme_get_param(scheme_config, MZCONFIG_ENV);
  scheme_set_param(scheme_config, MZCONFIG_ENV, (Scheme_Object *)env);

  _scheme_apply(mred_unit_opener, 0, NULL);

  scheme_set_param(scheme_config, MZCONFIG_ENV, save);

  if (!mred_sig) {
    wxREGGLOB(mred_sig);
    mred_sig = scheme_lookup_global(scheme_intern_symbol("mred^"), env);
  } else
    scheme_add_global("mred^", mred_sig, env);

  if (!get_file) {
    wxREGGLOB(get_file);
    wxREGGLOB(put_file);
    wxREGGLOB(get_ps_setup_from_user);
    wxREGGLOB(message_box);
    wxREGGLOB(execute);
    get_file = scheme_lookup_global(scheme_intern_symbol("get-file"), env);
    put_file = scheme_lookup_global(scheme_intern_symbol("put-file"), env);
    get_ps_setup_from_user = scheme_lookup_global(scheme_intern_symbol("get-ps-setup-from-user"), env);
    message_box = scheme_lookup_global(scheme_intern_symbol("message-box"), env);
    execute = scheme_lookup_global(scheme_intern_symbol("process"), env);
  }
}

static Scheme_Object *wxsUnit_Init(Scheme_Object **boxes, Scheme_Object ** /* anchors */,
				   Scheme_Unit *u, void * /* debug_request */)
{
  InstallRec *rec = (InstallRec *)u->data;
  int i;

  for (i = rec->count; i--; ) {
    SCHEME_ENVBOX_VAL(boxes[i]) = rec->vals[i];
  }

  return scheme_void;
}

extern "C" Scheme_Object *scheme_eval_compiled_sized_string(const char *str, int len, Scheme_Env *env);

void wxsScheme_setup(Scheme_Env *env)
{
  InstallRec *rec;
  Scheme_Unit *u;
  int i;
  Scheme_Object *link, *a[1], **exs;

  wxREGGLOB(gc_bitmaps);

  rec = (InstallRec *)scheme_malloc(sizeof(InstallRec));

  objscheme_init(env);

  wxREGGLOB(setup_file_symbol);
  wxREGGLOB(init_file_symbol);
  setup_file_symbol = scheme_intern_symbol("setup-file");
  init_file_symbol = scheme_intern_symbol("init-file");

  rec->count = 0;

  wxScheme_Install(env, rec);

  /* Note: the order of exports doesn't matter for matching the (sorted) sig */
  u = (Scheme_Unit *)scheme_malloc_tagged(sizeof(Scheme_Unit));
  u->type = scheme_unit_type;
  u->num_imports = 0;
  u->num_exports = rec->count;
  exs = (Scheme_Object **)scheme_malloc(sizeof(Scheme_Object *) * rec->count);
  u->exports = exs;
  u->export_debug_names = NULL;
  u->data = (Scheme_Object *)rec;
  for (i = rec->count; i--; ) {
    Scheme_Object *s;
    s = scheme_intern_symbol(rec->names[i]);
    u->exports[i] = s;
  }
  u->init_func = wxsUnit_Init;

#define EVAL_ONE_STR(x) link = scheme_eval_string(x, env)
#define EVAL_ONE_SIZED_STR(x, s) link = scheme_eval_compiled_sized_string(x, s, env)
#define JUST_DEFINED_FUNC(x) /**/
#define JUST_DEFINED_KEY(x) /**/
#define MZCOMPILED_STRING_FAR /**/
#if 1
# include "cwrap.inc"
#else
# include "wrap.inc"
#endif

  a[0] = (Scheme_Object *)u;
  wxREGGLOB(mred_unit_opener);
  mred_unit_opener = _scheme_apply(link, 1, a);

  wxScheme_Invoke(env);

  orig_collect_start_callback = GC_collect_start_callback;
  GC_collect_start_callback = collect_start_callback;
  orig_collect_end_callback = GC_collect_end_callback;
  GC_collect_end_callback = collect_end_callback;
}

void scheme_install_xc_global(char *name, Scheme_Object *val, void *env)
{
  InstallRec *rec = (InstallRec *)env;

  if (rec->count >= INSTALL_COUNT) {
    fprintf(stderr, "wx install overflow~n");
    exit(-1);
  }

  rec->names[rec->count] = name;
  rec->vals[rec->count] = val;
  rec->count++;
}

Scheme_Object * scheme_lookup_xc_global(char *name, void *env)
{
  InstallRec *rec = (InstallRec *)env;
  int i;

  for (i = 0; i < rec->count; i++) {
    if (!strcmp(rec->names[i], name))
      return rec->vals[i];
  }

  return NULL;
}


#ifdef wx_x
extern Display *MrEdGetXDisplay(void);
#endif

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static void draw_gc_bm(int on)
{
  GCBitmap *gcbm = gc_bitmaps;

#ifdef MZ_PRECISE_GC
  /* Too hard to make GCBlit et al. unconverted.
     We just save and restore the variable stack instead. */
  void **save_var_stack;
  save_var_stack = GC_variable_stack;
#endif

  while (gcbm) {
    wxCanvas *cnvs = GET_CANVAS(gcbm);
#ifdef MZ_PRECISE_GC
    if (!gcOBJ_TO_PTR(cnvs))
      cnvs = NULL;
#endif
    if (cnvs) {
      wxCanvasDC *dc;
      dc = (wxCanvasDC *)cnvs->GetDC();
      dc->GCBlit(gcbm->x, gcbm->y,
		 gcbm->w, gcbm->h,
		 on ? gcbm->on : gcbm->off,
		 0, 0);
    }
    gcbm = gcbm->next;
  }

#ifdef MZ_PRECISE_GC
  GC_variable_stack = save_var_stack;
#endif

#ifdef wx_x
  XFlush(MrEdGetXDisplay());
#endif
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

void wxsKeymapError(char *s)
{
  scheme_signal_error("%s", s);
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static void collect_start_callback(void)
{
  draw_gc_bm(1);
  orig_collect_start_callback();
}

static void collect_end_callback(void)
{
  orig_collect_end_callback();
  draw_gc_bm(0);
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

static Scheme_Object *wxSchemeUnregisterCollectingBitmap(int, Scheme_Object **a)
{
  GCBitmap *gcbm, *prev = NULL;
  wxCanvas *c;

  if (a)
    c = objscheme_unbundle_wxCanvas(a[0], "unregister-collecting-blit", 0);
  else
    c = NULL;
  
  gcbm = gc_bitmaps;
  while (gcbm) {
    if (!gcbm->canvasptr || (GET_CANVAS(gcbm) == c)) {
      if (prev)
	prev->next = gcbm->next;
      else
	gc_bitmaps = gcbm->next;
      gcbm->on = gcbm->off = NULL;
      gcbm->canvasptr = NULL;
    } else
      prev = gcbm;
    gcbm = gcbm->next;
  }

  return scheme_void;
}

static Scheme_Object *wxSchemeRegisterCollectingBitmap(int n, Scheme_Object **a)
{
  GCBitmap *gcbm;
  wxCanvas *cvs;

  gcbm = new GCBitmap;

  cvs = objscheme_unbundle_wxCanvas(a[0], "register-collecting-blit", 0);

#ifdef MZ_PRECISE_GC
  {
    void *cp;
    cp = GC_malloc_weak_box(gcOBJ_TO_PTR(cvs), NULL, 0);
    gcbm->canvasptr = (Scheme_Object *)cp;
  }
#else
  gcbm->canvasptr = (wxCanvas **)scheme_malloc_atomic(sizeof(wxCanvas*));
  *gcbm->canvasptr = cvs;
#endif

  gcbm->x = objscheme_unbundle_float(a[1], "register-collecting-blit");
  gcbm->y = objscheme_unbundle_float(a[2], "register-collecting-blit");
  gcbm->w = objscheme_unbundle_nonnegative_float(a[3], "register-collecting-blit");
  gcbm->h = objscheme_unbundle_nonnegative_float(a[4], "register-collecting-blit");
  gcbm->on = objscheme_unbundle_wxBitmap(a[5], "register-collecting-blit", 0);
  gcbm->off = objscheme_unbundle_wxBitmap(a[6], "register-collecting-blit", 0);
  gcbm->onx = gcbm->ony = gcbm->offx = gcbm-> offy = 0;
  if (n > 7) {
    gcbm->onx = objscheme_unbundle_float(a[7], "register-collecting-blit");
    if (n > 8) {
      gcbm->ony = objscheme_unbundle_float(a[8], "register-collecting-blit");
      if (n > 9) {
	gcbm->offx = objscheme_unbundle_float(a[9], "register-collecting-blit");
	if (n > 10) {
	  gcbm->offy = objscheme_unbundle_float(a[10], "register-collecting-blit");
	}
      }
    }
  }

  gcbm->next = gc_bitmaps;
  gc_bitmaps = gcbm;

#ifndef MZ_PRECISE_GC
  GC_general_register_disappearing_link((void **)gcbm->canvasptr, 
					*gcbm->canvasptr);
#endif

  wxSchemeUnregisterCollectingBitmap(0, NULL);

  return scheme_void;
}

#ifdef wx_msw
static BOOL do_choose_color(void *data, HWND parent)
{
  CHOOSECOLOR *c = (CHOOSECOLOR *)data;
  c->hwndOwner = parent;

  return ChooseColor(c);
}
#endif

static Scheme_Object *wxSchemeGetColourFromUser(int argc, Scheme_Object **argv)
{
  char *s;

  if (!argc || SCHEME_FALSEP(argv[0]))
    s = "Choose a color";
  else
    s = objscheme_unbundle_string(argv[0], "get-color-from-user");

#ifndef wx_x
# ifdef wx_msw
  wxWindow *parent = ((argc > 1)
		      ? objscheme_unbundle_wxWindow(argv[1], "get-color-from-user", 1)
		      : NULL);
# endif
  wxColour *c = ((argc > 2)
		 ? objscheme_unbundle_wxColour(argv[2], "get-color-from-user", 1)
		 : NULL);
#endif

#ifdef wx_x
  return scheme_false;
#endif
#ifdef wx_mac
  int l;
  Point pt = {0, 0};
  Str255 buf;
  RGBColor in, out;

  l = strlen(s);
  if (l > 255) l = 255;

  memcpy(buf + 1, s, l);
  buf[0] = l;

  if (c) {
    in.red = c->Red() << 8;
    in.green = c->Green() << 8;
    in.blue = c->Blue() << 8;
  } else
    in.red = in.green = in.blue = 0;

  if (!GetColor(pt, buf, &in, &out))
    return scheme_false;

  c = new wxColour(out.red >> 8, out.green >> 8, out.blue >> 8);

  return objscheme_bundle_wxColour(c);
#endif
#ifdef wx_msw
  CHOOSECOLOR *cc;
  static unsigned long userCustomColors[16];

  cc = new CHOOSECOLOR;
  cc->lStructSize = sizeof(CHOOSECOLOR);
  cc->hwndOwner = NULL; /* (parent ? parent->GetHWND() : (HWND)NULL */
  if (c)
    cc->rgbResult = RGB(c->Red(), c->Green(), c->Blue());
  cc->Flags = (c ? CC_RGBINIT : 0);
  cc->lpCustColors = userCustomColors;

  if (!wxPrimitiveDialog(do_choose_color, cc, 0))
    return scheme_false;

  c = new wxColour(GetRValue(cc->rgbResult), GetGValue(cc->rgbResult), GetBValue(cc->rgbResult));

  return objscheme_bundle_wxColour(c);
#endif
}

#ifdef wx_msw
static BOOL do_choose_font(void *data, HWND parent)
{
  CHOOSEFONT *c = (CHOOSEFONT *)data;
  c->hwndOwner = parent;

  return ChooseFont(c);
}
#endif

static Scheme_Object *wxSchemeGetFontFromUser(int argc, Scheme_Object **argv)
{
  char *prompt;

  if (!argc || SCHEME_FALSEP(argv[0]))
    prompt = "Choose a font";
  else
    prompt = objscheme_unbundle_string(argv[0], "get-font-from-user");

#ifdef wx_msw
  wxWindow *parent = ((argc > 1)
		      ? objscheme_unbundle_wxWindow(argv[1], "get-font-from-user", 1)
		      : NULL);
  wxFont *f = ((argc > 2)
	       ? objscheme_unbundle_wxFont(argv[2], "get-font-from-user", 1)
	       : NULL);
#endif

#ifdef wx_x
  return scheme_false;
#endif
#ifdef wx_mac
  return scheme_false;
#endif
#ifdef wx_msw
  CHOOSEFONT *c;
  LOGFONT *lf;
  int len;
  char *s;

  lf = new LOGFONT;
  c = new CHOOSEFONT;

  s = (f ? f->GetFaceString() : NULL);
  if (s) {
    len = strlen(s);
    if (len > 31)
      len = 31;
  } else
    len = 0;
  
  memcpy(lf->lfFaceName, s, len);
  lf->lfFaceName[len] = 0;
  
  lf->lfHeight = 0;
  lf->lfWidth = 0;
  lf->lfEscapement = 0;
  lf->lfOrientation = 0;
  if (f) {
    switch (f->GetWeight()) {
    case wxBOLD:
      lf->lfWeight = FW_BOLD;
      break;
    case wxLIGHT:
      lf->lfWeight = FW_LIGHT;
    default:
      lf->lfWeight = FW_NORMAL;
    } 
  } else
    lf->lfWeight = FW_NORMAL;
  if (f) {
    switch (f->GetStyle()) {
    case wxITALIC:
    case wxSLANT:
      lf->lfItalic = TRUE;
      break;
    default:
      lf->lfItalic = FALSE;
    } 
  } else
    lf->lfItalic = FALSE;
  lf->lfUnderline = f && f->GetUnderlined();
  lf->lfStrikeOut = FALSE;
  lf->lfCharSet = OEM_CHARSET;
  lf->lfOutPrecision = OUT_DEFAULT_PRECIS;
  lf->lfClipPrecision = CLIP_DEFAULT_PRECIS;
  lf->lfQuality = DEFAULT_QUALITY;
  lf->lfPitchAndFamily = DEFAULT_PITCH;
  if (f) {
    switch (f->GetFamily()) {
    case wxDECORATIVE:
      lf->lfPitchAndFamily |= FF_DECORATIVE;
      break;
    case wxMODERN:
      lf->lfPitchAndFamily = FIXED_PITCH | FF_MODERN;
      break;
    case wxROMAN:
      lf->lfPitchAndFamily |= FF_ROMAN;
      break;
    case wxSCRIPT:
      lf->lfPitchAndFamily |= FF_SCRIPT;
      break;
    case wxSWISS:
      lf->lfPitchAndFamily |= FF_SWISS;
      break;
    default:
    case wxDEFAULT:
      lf->lfPitchAndFamily |= FF_DONTCARE;
      break;
    } 
  } else
    lf->lfPitchAndFamily |= FF_DONTCARE;

  c->lStructSize = sizeof(CHOOSEFONT);
  c->hwndOwner = NULL; /* (parent ? parent->GetHWND() : (HWND)NULL) */
  c->lpLogFont = lf;
  c->iPointSize = 10 * (f ? f->GetPointSize() : 10);
  c->Flags = CF_INITTOLOGFONTSTRUCT | CF_SCREENFONTS;

  if (!wxPrimitiveDialog(do_choose_font, c, 0))
    return scheme_false;
  
  if (!lf->lfFaceName[0])
    s = NULL;
  else
    s = lf->lfFaceName;
  
  int fontFamily = wxSWISS;
  int fontStyle = wxNORMAL;
  int fontWeight = wxNORMAL;
  int fontPoints = 10;
  Bool fontUnderline = FALSE;

  int lfFamily = lf->lfPitchAndFamily;
  if (lfFamily & FIXED_PITCH)
    lfFamily -= FIXED_PITCH;
  if (lfFamily & VARIABLE_PITCH)
    lfFamily -= VARIABLE_PITCH;
  
  switch (lfFamily)
  {
    case FF_ROMAN:
      fontFamily = wxROMAN;
      break;
    case FF_SWISS:
      fontFamily = wxSWISS;
      break;
    case FF_SCRIPT:
      fontFamily = wxSCRIPT;
      break;
    case FF_MODERN:
      fontFamily = wxMODERN;
      break;
    case FF_DECORATIVE:
      fontFamily = wxDECORATIVE;
      break;
    default:
      fontFamily = wxSWISS;
      break;
  }
  switch (lf->lfWeight)
  {
    case FW_LIGHT:
      fontWeight = wxLIGHT;
      break;
    case FW_NORMAL:
      fontWeight = wxNORMAL;
      break;
    case FW_BOLD:
      fontWeight = wxBOLD;
      break;
    default:
      fontWeight = wxNORMAL;
      break;
  }
  if (lf->lfItalic)
    fontStyle = wxITALIC;
  else
    fontStyle = wxNORMAL;

  if (lf->lfUnderline)
    fontUnderline = TRUE;

  if (s)
    f = new wxFont(c->iPointSize / 10, s, fontFamily, fontStyle, 
		   fontWeight, fontUnderline);
  else
    f = new wxFont(c->iPointSize / 10, fontFamily, fontStyle, 
		   fontWeight, fontUnderline);

  return objscheme_bundle_wxFont(f);
#endif
}

#ifdef wx_x
static int indirect_strcmp(const void *a, const void *b)
{
  return strcmp(*(char **)a, *(char **)b);
}
#endif


#ifdef wx_msw
typedef struct {
  int count, size;
  char **names;
} gfData;

static int CALLBACK get_font(ENUMLOGFONT FAR*  lpelf, 
			     NEWTEXTMETRIC FAR* lpntm, 
			     DWORD type, 
			     LPARAM _data)
{
  gfData *data = (gfData *)_data;
  int l;
  char *s;
  
  if (data->count == data->size) {
    char **naya;

    data->size += (2 * data->size) + 10;
    naya = new char*[data->size];
    memcpy(naya, data->names, data->count * sizeof(char *));
    data->names = naya;
  }
  
  l = strlen(lpelf->elfLogFont.lfFaceName);
  s = new char[l + 1];
  memcpy(s, lpelf->elfLogFont.lfFaceName, l);

  data->names[data->count++] = s;

  return 1;
}
#endif

typedef int (*Indirect_Cmp_Proc)(const void *, const void *);

static Scheme_Object *wxSchemeGetFontList(int, Scheme_Object **)
{
  Scheme_Object *first = scheme_null, *last = NULL;

#ifdef wx_x
#ifdef wx_xt
#define __DISPLAY wxAPP_DISPLAY
#else
#define __DISPLAY wxGetDisplay()
#endif
  int count, i = 0;
  char **xnames, **names;
  int last_pos = -1, last_len = 0;

  xnames = XListFonts(__DISPLAY, "*", 50000, &count);

  names = new char* [count];
  for (i = 0; i < count; i++) {
    names[i] = xnames[i];
  }

  qsort(names, count, sizeof(char *), 
	(Indirect_Cmp_Proc)indirect_strcmp);

  i = 0;
#endif
#ifdef wx_mac
  MenuHandle fmenu;
  Str255 buffer;
  int count, i = 0;
  
  fmenu = NewMenu(42, "\p");
  if (!fmenu)
    return scheme_null;
  AppendResMenu(fmenu, 'FONT');
  
  count = CountMItems(fmenu);
#endif
#ifdef wx_msw
  gfData data;
  HDC dc;
  int i = 0;

  data.count = data.size = 0;
  data.names = NULL;

  dc = GetDC(NULL);

  EnumFontFamilies(dc, NULL, (FONTENUMPROC)get_font, (LPARAM)&data);
#endif

  while (1) {
    char *s;
    int l;
    Scheme_Object *pr;

#ifdef wx_x
    while ((i < count)
	   && ((last_pos >= 0) 
	       && !strncmp(names[i], names[last_pos], last_len))) {
      i++;
    }
    if (i >= count)
      break;

    last_pos = i;
    if (names[i][0] != '-') {
      l = strlen(names[i]);
    } else {
      int c = 0;
      for (l = 0; names[i][l]; l++) {
	if (names[i][l] == '-') {
	  c++;
	  if (c == 3) {
	    /* Special case: null weight, slant, non-normal */
	    if (names[i][l + 1] == '-') {
	      l++;
	      if (names[i][l + 1] == '-') {
		l++;
		if (names[i][l + 1] == '-')
		  l++;
	      }
	    }
	    break;
	  }
	}
      }
    }
    last_len = l;
    
    s = names[i++];
#endif
#ifdef wx_mac
    if (i == count) {
      s = "systemfont";
      l = strlen(s);
      i++;
    } else if (i == count + 1) {
      s = "applicationfont";
      l = strlen(s);
      i++;
    } else if (i > count)
      break;
    else {
      GetMenuItemText(fmenu, ++i, buffer);
      l = buffer[0];
      s = PtoCstr(buffer);
    }
#endif
#ifdef wx_msw
    if (i >= data.count)
      break;
    s = data.names[i++];
    l = strlen(s);
#endif
    
    pr = scheme_make_pair(scheme_make_sized_string(s, l, 1), scheme_null);
    if (last)
      SCHEME_CDR(last) = pr;
    else
      first = pr;
    last = pr;
  }

#ifdef wx_x
   XFreeFontNames(xnames);
#endif
#ifdef wx_msw
   ReleaseDC(NULL, dc);
#endif

  return first;
}

static Scheme_Object *wxSchemeGetPanelBackground(int, Scheme_Object **)
{
  wxColour *c;

#ifdef wx_x
  c = new wxColour("GRAY");
#endif
#ifdef wx_mac
  c = new wxColour(0xE8, 0xE8, 0xE8);
#endif
#ifdef wx_msw
  DWORD v = GetSysColor(COLOR_BTNFACE);

  c = new wxColour(GetRValue(v), GetGValue(v), GetBValue(v));
#endif

  return objscheme_bundle_wxColour(c);
}

#ifdef wx_mac
#include <Sound.h>
typedef struct AsyncSoundRec {
  SndChannelPtr snd;
  short file;
  struct AsyncSoundRec *next;
} AsyncSoundRec;

static AsyncSoundRec *playing = NULL, *finished = NULL;

static pascal void SoundFinished(SndChannelPtr snd)
{
  AsyncSoundRec **p = &playing;

  while (*p) {
    if ((*p)->snd == snd) {
      AsyncSoundRec *r = (*p); 
      (*p) = r->next;
      r->next = finished;
      finished = r;
      return;
    }
  }
}

void wxCheckFinishedSounds(void)
{
  while (finished) {
    AsyncSoundRec *p = finished;
    finished = finished->next;
    
    FSClose(p->file);
    SndDisposeChannel(p->snd, FALSE);
  }
}

static FilePlayCompletionUPP SoundFinishedProc = NewFilePlayCompletionProc(SoundFinished);
#endif

#ifndef wx_x

# ifdef wx_mac
static int IsFinished(void *r)
{
  AsyncSoundRec *f = finished;
  
  while (f) {
    if ((void *)f == r)
      return 1;
    f = f->next;
  }
  
  return 0;
}

extern "C" {
  int scheme_mac_path_to_spec(const char *filename, FSSpec *spec, long *type);
};
# endif

static Scheme_Object *wxPlaySound(int argc, Scheme_Object **argv)
{
  Bool async, ok;
  char *f;
  
  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("play-sound", "string", 0, argc, argv);
  
  async = SCHEME_TRUEP(argv[1]);
  
  f = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
			     SCHEME_STRTAG_VAL(argv[0]),
			     "play-sound",
			     NULL);

#ifdef wx_msw  
  ok = PlaySound(f, NULL, async ? SND_ASYNC : SND_SYNC);
#endif
#ifdef wx_mac
  Bool local_async = TRUE;
  SndChannelPtr snd;
  short file;
  long buffsize = 1024;
  OSErr snd_err;
  AsyncSoundRec *r;
  FSSpec spec;
  
  if (!scheme_mac_path_to_spec(f, &spec, FALSE))
    return scheme_false;
  
  if (FSpOpenDF(&spec, fsRdPerm, &file))
    return scheme_false;

  snd = NULL;
  if (local_async) {
    if (SndNewChannel(&snd, sampledSynth, 0, NULL)) {
      DisposePtr((Ptr)snd);
      snd = NULL;
    }
  }

  if (local_async) {
    r = new AsyncSoundRec;

    r->snd = snd;
    r->file = file;

    r->next = playing;
    playing = r;
  } else
    r = NULL;

  do {
    buffsize *= 2;
    snd_err = SndStartFilePlay(snd, file, 0, buffsize, NULL, NULL, 
  			 local_async ? SoundFinishedProc : NULL, 
  			 local_async);
  } while (snd_err == buffersTooSmall);

  ok = !snd_err;

  if (ok && local_async && !async) {
    wxDispatchEventsUntil(IsFinished, r);
  }

  if (!local_async)
    FSClose(file);

  if ((!local_async || !ok) && snd) {
    SoundFinished(snd);
    wxCheckFinishedSounds();
  }
#endif  

  return (ok ? scheme_true : scheme_false);
}
#endif

wxMediaSnip *wxsMakeMediaSnip(wxMediaBuffer *useme,
			      Bool border,
			      int lm, int tm, int rm, int bm,
			      int li, int ti, int ri, int bi,
			      float w, float W, float h, float H)
{
  if (make_media_snip) {
    Scheme_Object *a[20], *r;

    a[0] = (useme ? objscheme_bundle_wxMediaBuffer(useme) : scheme_false);
    a[1] = (border ? scheme_true : scheme_false);
    a[2] = scheme_make_integer(lm);
    a[3] = scheme_make_integer(tm);
    a[4] = scheme_make_integer(rm);
    a[5] = scheme_make_integer(bm);
    a[6] = scheme_make_integer(li);
    a[7] = scheme_make_integer(ti);
    a[8] = scheme_make_integer(ri);
    a[9] = scheme_make_integer(bi);
    a[10] = ((w > 0) ? scheme_make_double(w) : none_symbol);
    a[11] = ((W > 0) ? scheme_make_double(W) : none_symbol);
    a[12] = ((h > 0) ? scheme_make_double(h) : none_symbol);
    a[13] = ((H > 0) ? scheme_make_double(H) : none_symbol);

    r = scheme_apply(make_media_snip, 14, a);

    return objscheme_unbundle_wxMediaSnip(r, NULL, 0);
  } else {
    return new wxMediaSnip(useme, border,
			   lm, tm, rm, bm,
			   li, ti, ri, bi,
			   w, W, h, H);
  }
}

wxMediaEdit *wxsMakeMediaEdit()
{
  if (make_media_edit) {
    return objscheme_unbundle_wxMediaEdit(scheme_apply(make_media_edit, 0, NULL), 
					  NULL, 0);
  } else
    return new wxMediaEdit();
}

wxMediaPasteboard *wxsMakeMediaPasteboard()
{
  if (make_media_pasteboard) {
    return objscheme_unbundle_wxMediaPasteboard(scheme_apply(make_media_pasteboard, 0, NULL), 
						NULL, 0);
  } else
    return new wxMediaPasteboard();
}

static Scheme_Object *SetMediaSnipMaker(int, Scheme_Object *a[])
{
  wxREGGLOB(make_media_snip);
  wxREGGLOB(none_symbol);
  make_media_snip = a[0];
  none_symbol = scheme_intern_symbol("none");
  return scheme_void;
}

static Scheme_Object *SetMediaEditMaker(int, Scheme_Object *a[])
{
  wxREGGLOB(make_media_edit);
  make_media_edit = a[0];
  return scheme_void;
}

static Scheme_Object *SetMediaPasteboardMaker(int, Scheme_Object *a[])
{
  wxREGGLOB(make_media_pasteboard);
  make_media_pasteboard = a[0];
  return scheme_void;
}

#ifdef wx_mac
extern short wxMacDisableMods;
#define SCK_ARG p
#else
#define SCK_ARG /**/
#endif

Scheme_Object *wxs_app_file_proc;
Scheme_Object *wxs_app_quit_proc;

static Scheme_Object *SpecialCtlKey(int, Scheme_Object *SCK_ARG[])
{
#ifdef wx_mac
  if (SCHEME_FALSEP(p[0]))
    wxMacDisableMods = 0;
  else
    wxMacDisableMods = 4096;
#endif      

  return scheme_void;
}


static Scheme_Object *DefaultAppFileProc(int n, Scheme_Object *p[])
{
  if (!SCHEME_STRINGP(p[0]))
    scheme_wrong_type("default-application-file-handler", "string",
		      0, n, p);

  return scheme_void;
}

static Scheme_Object *ApplicationFileProc(int n, Scheme_Object *p[])
{
  if (!n)
    return wxs_app_file_proc;
  else {
    scheme_check_proc_arity("application-file-handler", 1,
			    0, n, p);
    wxs_app_file_proc = p[0];
    return scheme_void;
  }
}

static Scheme_Object *DefaultAppQuitProc(int, Scheme_Object **)
{
  return scheme_void;
}

static Scheme_Object *ApplicationQuitProc(int n, Scheme_Object *p[])
{
  if (!n)
    return wxs_app_quit_proc;
  else {
    scheme_check_proc_arity("application-quit-handler", 0, 0, n, p);
    wxs_app_quit_proc = p[0];
    return scheme_void;
  }
}

static Scheme_Object *Eventspace_p(int, Scheme_Object **argv)
{
  return ((SCHEME_TYPE(argv[0]) == mred_eventspace_type)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *wxSchemeCurrentEventspace(int argc, Scheme_Object **argv)
{
  return scheme_param_config("current-eventspace", 
			     scheme_make_integer(mred_eventspace_param),
			     argc, argv,
			     -1, Eventspace_p, "eventspace", 0);
}

static Scheme_Object *wxSchemeEventDispatchHandler(int argc, Scheme_Object **argv)
{
  return scheme_param_config("event-dispatch-handler", 
			     scheme_make_integer(mred_event_dispatch_param),
			     argc, argv,
			     1, NULL, NULL, 0);
}

extern void wxDispatchEventsUntil(int (*f)(void *), void *data);

static Scheme_Object *wxSchemeMakeEventspace(int, Scheme_Object **)
{
  return (Scheme_Object *)MrEdMakeEventspace((Scheme_Config *)NULL);
}

static Scheme_Object *PS_Setup_p(int, Scheme_Object **argv)
{
  return (objscheme_istype_wxPrintSetupData(argv[0], NULL, 0)
	  ? scheme_true
	  : scheme_false);
}

Scheme_Object *wxsBundlePSSetup(wxPrintSetupData *d)
{
  return objscheme_bundle_wxPrintSetupData(d);
}

wxPrintSetupData *wxsUnbundlePSSetup(Scheme_Object *o)
{
  return objscheme_unbundle_wxPrintSetupData(o, NULL, 0);
}

static Scheme_Object *wxSchemeCurrentPSSetup(int argc, Scheme_Object **argv)
{
  return scheme_param_config("current-ps-setup", 
			     scheme_make_integer(mred_ps_setup_param),
			     argc, argv,
			     -1, PS_Setup_p, "ps-setup% instance", 0);
}

static Scheme_Object *queue_callback(int argc, Scheme_Object **argv)
{
  MrEd_add_q_callback("queue-callback", argc, argv);
  return scheme_void;
}

static int check_sema(void *s)
{
  if (!*(void **)s)
    return 1;
  else {
    if (!scheme_wait_sema(*(Scheme_Object **)s, 1))
      return 0;
    *(void **)s = NULL;
    return 1;
  }
}

Bool wxSchemeYield(void *sema)
{
  if (sema) {
    void **s;

    if (!SCHEME_SEMAP((Scheme_Object *)sema))
      scheme_wrong_type("yield", "semaphore", -1, 0, (Scheme_Object **)&sema);

    s = new void*;
    *s = sema;

    wxDispatchEventsUntil(check_sema, s);

    return 1;
  } else
    return wxYield();
}

static Scheme_Object *wxSchemeCheckForBreak(int, Scheme_Object **)
{
  return (MrEdCheckForBreak()
	  ? scheme_true
	  : scheme_false);
}

Scheme_Object *wxsLocationToWindow(int, Scheme_Object **a)
{
  wxWindow *w;
  w = wxLocationToWindow(SCHEME_INT_VAL(a[0]), SCHEME_INT_VAL(a[1]));
  return objscheme_bundle_wxWindow(w);
}

static Scheme_Object *wxSchemeGetFrameList(int, Scheme_Object **)
{
  return MrEdGetFrameList();
}

static Scheme_Object *wLabelShortcutsVisible(int argc, Scheme_Object **argv)
{
  int menu_too;

  if (argc)
    menu_too = SCHEME_TRUEP(argv[0]);
  else
    menu_too = 0;

#ifdef wx_x
  return menu_too ? scheme_false : scheme_true;
#endif
#ifdef wx_msw
  return scheme_true;
#endif
#ifdef wx_mac
  return scheme_false;
#endif
}

static Scheme_Object *Shutdown_p(int argc, Scheme_Object **argv)
{
  Scheme_Type type = SCHEME_TYPE(argv[0]);

  if (type == mred_eventspace_type) {
    return wxsIsContextShutdown((void *)argv[0]) ? scheme_true : scheme_false;
  }

  scheme_wrong_type("eventspace-shutdown?", "eventspace", 0, argc, argv);
  return NULL;
}

extern "C" {
  void scheme_start_atomic(void);
  void scheme_end_atomic(void);
}

static Scheme_Object *wxInAtomicRegion(int, Scheme_Object **argv)
{
  if (SCHEME_SEMAP(argv[0])) {
    scheme_wait_sema(argv[0], 0);
    /* MzScheme promises that no break or kill will happen
       between receiving the semaphore post and returning to us. */
    scheme_start_atomic();
  } else
    scheme_end_atomic();

  return scheme_void;
}

#ifdef wx_mac
extern "C" {
 extern char *scheme_build_mac_filename(FSSpec *spec, int given_dir);
};
extern char *wxmac_startup_directory;
#endif

enum {
  id_init_file,
  id_setup_file
};

static Scheme_Object *wxSchemeFindDirectory(int argc, Scheme_Object **argv)
{
  int which;

  if (argv[0] == init_file_symbol)
    which = id_init_file;
  else if (argv[0] == setup_file_symbol)
    which = id_setup_file;
  else {
    scheme_wrong_type("find-graphical-system-path", "graphical path symbol",
		      0, argc, argv);
    return NULL;
  }

#ifdef wx_x
  {
    Scheme_Object *home;
    int ends_in_slash;
    
    home = scheme_make_string(scheme_expand_filename("~/", 2, NULL, NULL));
    
    ends_in_slash = (SCHEME_STR_VAL(home))[SCHEME_STRTAG_VAL(home) - 1] == '/';
    
    if (which == id_init_file)
      return scheme_append_string(home,
				  scheme_make_string("/.mredrc" + ends_in_slash));
    
    if (which == id_setup_file)
      return scheme_append_string(home,
				  scheme_make_string("/.mred.resources" + ends_in_slash));
  }
#endif

#ifdef wx_msw
  char *d, *p;
  Scheme_Object *home;

  d = getenv("HOMEDRIVE");
  p = getenv("HOMEPATH");

  if (d && p) {
    char *s;
    s = new char[strlen(d) + strlen(p) + 1];
    strcpy(s, d);
    strcat(s, p);
    
    if (scheme_directory_exists(s))
      home = scheme_make_string_without_copying(s);
    else
      home = NULL;
  } else 
    home = NULL;

  if (!home) {
    int i;
    char *s;

    p = wxTheApp->argv[0];
    s = copystring(p);

    i = strlen(s) - 1;
    
    while (i && (s[i] != '\\')) {
      --i;
    }
    s[i] = 0;
    home = scheme_make_string_without_copying(s);
  }

  int ends_in_slash;
  ends_in_slash = (SCHEME_STR_VAL(home))[SCHEME_STRTAG_VAL(home) - 1];
  ends_in_slash = ((ends_in_slash == '/') || (ends_in_slash == '\\'));

  if (which == id_init_file)
    return scheme_append_string(home,
				scheme_make_string("\\mredrc.ss" + ends_in_slash));

  if (which == id_setup_file)
    return scheme_append_string(home,
				scheme_make_string("\\mred.ini" + ends_in_slash));  
#endif

#ifdef wx_mac
  OSType t;
  FSSpec spec;
  Scheme_Object *home;

  switch (which) {
  case id_init_file:
  case id_setup_file:
    t = 'pref';
    break;
  default:
    t = 'temp';
    break;
  }

  if (!FindFolder(kOnSystemDisk, t, kCreateFolder, &spec.vRefNum, &spec.parID))
    home = scheme_make_string(scheme_build_mac_filename(&spec, 1));
  else if (wxmac_startup_directory) {
    home = scheme_make_string(wxmac_startup_directory);
  } else {
    home = scheme_make_string(scheme_os_getcwd(NULL, 0, NULL, 1));
  }
  
  int ends_in_colon;
  ends_in_colon = (SCHEME_STR_VAL(home))[SCHEME_STRTAG_VAL(home) - 1] == ':';

  if (which == id_init_file)
    return scheme_append_string(home,
				scheme_make_string(":mredrc.ss" + ends_in_colon));

  if (which == id_setup_file)
    return scheme_append_string(home,
				scheme_make_string(":mred.fnt" + ends_in_colon));  
#endif

  return scheme_void;
}

char *wxsFileDialog(char *message, char *default_path, 
		    char *default_filename, char *default_extension, 
		    int is_put, wxWindow *parent)
{
  Scheme_Object *a[6], *r;
  
  a[0] = !message ? scheme_false : scheme_make_string(message);
  a[1] = !parent ? scheme_false : objscheme_bundle_wxWindow(parent);
  a[2] = !default_path ? scheme_false : scheme_make_string(default_path);
  a[3] = !default_filename ? scheme_false : scheme_make_string(default_filename);
  a[4] = !default_extension ? scheme_false : scheme_make_string(default_extension);
  a[5] = scheme_null;

  r = scheme_apply(is_put ? put_file : get_file, 6, a);

  if (SCHEME_FALSEP(r))
    return NULL;
  else
    return SCHEME_STR_VAL(r);
}

extern wxPrintSetupData *wxGetThePrintSetupData();

Bool wxsPrinterDialog(wxWindow *parent)
{
  Scheme_Object *a[4], *r;
  
  a[0] = scheme_false;
  a[1] = !parent ? scheme_false : objscheme_bundle_wxWindow(parent);
  a[2] = scheme_false;
  a[3] = scheme_null;

  r = scheme_apply(get_ps_setup_from_user, 4, a);

  if (SCHEME_FALSEP(r)) {
    return 0;
  } else {
    wxPrintSetupData *p, *p2;
    p = objscheme_unbundle_wxPrintSetupData(r, NULL, 0);
    p2 = wxGetThePrintSetupData();
    p2->copy(p);
    return 1;
  }
}

int wxsMessageBox(char *message, char *caption, long style, wxWindow *parent)
{
  Scheme_Object *a[4], *r;
  
  a[0] = scheme_make_string(caption);
  a[1] = scheme_make_string(message);
  a[2] = !parent ? scheme_false : objscheme_bundle_wxWindow(parent);
  a[3] = ((style & wxYES_NO)
	  ? scheme_intern_symbol("yes-no")
	  : ((style & wxCANCEL)
	     ? scheme_intern_symbol("ok-cancel")
	     : scheme_intern_symbol("ok")));

  a[3] = scheme_make_pair(a[3], scheme_null);    

  r = scheme_apply(message_box, 4, a);

  if (SAME_OBJ(r, scheme_intern_symbol("ok"))) {
    return wxOK;
  }
  if (SAME_OBJ(r, scheme_intern_symbol("cancel"))) {
    return wxCANCEL;
  }
  if (SAME_OBJ(r, scheme_intern_symbol("yes"))) {
    return wxYES;
  }
  return wxNO;
}

int wxsGetImageType(char *fn)
{
  FILE *f;
  int type;
  char *expect = NULL;

  f = fopen(fn, "r");
  if (f) {
    switch (fgetc(f)) {
    case 'B':
      expect = "M";
      type = wxBITMAP_TYPE_BMP;
      break;
    case '#':
      expect = "define";
      type = wxBITMAP_TYPE_XBM;
      break;
    case '/':
      expect = "* XPM */";
      type = wxBITMAP_TYPE_XPM;
      break;
    case 'G':
      expect = "IF8";
      type = wxBITMAP_TYPE_GIF;
      break;
    default:
      type = 0;
      break;
    }

    if (expect) {
      while (*expect) {
	if (*expect != fgetc(f)) {
	  type = 0;
	  break;
	}
	expect++;
      }
    }

    fclose(f);
  } else
    type = 0;

  return type ? type : wxBITMAP_TYPE_XBM;
}

void wxsExecute(char **argv)
{
  int i, c;
  Scheme_Object *a[1], *s, *p;

  for (i = 0; argv[i]; i++) {
  }

  c = i;

  s = scheme_make_string("");
  for (i = 0; i < c; i++) {
    s = scheme_append_string(s, scheme_make_string(argv[i]));
    s = scheme_append_string(s, scheme_make_string(" "));
  }

  a[0] = s;
  p = scheme_apply(execute, 1, a);

  /* Close all the ports */
  scheme_close_input_port(SCHEME_CAR(p));
  scheme_close_output_port(SCHEME_CADR(p));
  p = SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(p)));
  scheme_close_input_port(SCHEME_CAR(p));
}

static void wxScheme_Install(Scheme_Env *WXUNUSED(env), void *global_env)
{
  static int installed = 0;

  scheme_defining_primitives = 1;
  
  if (!installed) {
    installed = 1;
    scheme_add_namespace_option(scheme_intern_symbol("mred"), wxScheme_Invoke);
    
    wxREGGLOB(wxs_app_quit_proc);
    wxREGGLOB(wxs_app_file_proc);

    wxs_app_file_proc = scheme_make_prim_w_arity(DefaultAppFileProc,
						 "default-application-file-handler",
						 1, 1);
    wxs_app_quit_proc = scheme_make_prim_w_arity(DefaultAppQuitProc,
						 "default-application-quit-handler",
						 0, 0);
  }

  scheme_install_xc_global("special-control-key", 
			   scheme_make_prim_w_arity(SpecialCtlKey, 
						    "special-control-key", 
						    1, 1), 
			   global_env);
  
  scheme_install_xc_global("application-file-handler",
			   scheme_make_prim_w_arity(ApplicationFileProc,
						    "application-file-handler",
						    0, 1),
			   global_env);
  scheme_install_xc_global("application-quit-handler",
			   scheme_make_prim_w_arity(ApplicationQuitProc,
						    "application-quit-handler",
						    0, 1),
			   global_env);
  
  scheme_install_xc_global("get-color-from-user",
			   scheme_make_prim_w_arity(wxSchemeGetColourFromUser,
						    "get-color-from-user",
						    0, 3),
			   global_env);
  
  scheme_install_xc_global("get-font-from-user",
			   scheme_make_prim_w_arity(wxSchemeGetFontFromUser,
						    "get-font-from-user",
						    0, 3),
			   global_env);
  
  scheme_install_xc_global("get-face-list",
			   scheme_make_prim_w_arity(wxSchemeGetFontList,
						    "get-face-list",
						    0, 0),
			   global_env);
  
  scheme_install_xc_global("get-panel-background",
			   scheme_make_prim_w_arity(wxSchemeGetPanelBackground,
						    "get-panel-background",
						    0, 0),
			   global_env);
  
#ifdef wx_x
  scheme_install_xc_global("play-sound", scheme_false, global_env);
#else
  scheme_install_xc_global("play-sound", 
			     scheme_make_prim_w_arity(wxPlaySound, 
						      "play-sound", 
						      2, 2), 
			     global_env);
#endif

  scheme_install_xc_global("make-eventspace",
			     scheme_make_prim_w_arity(wxSchemeMakeEventspace,
						      "make-eventspace",
						      0, 0),
			     global_env);
  scheme_install_xc_global("current-eventspace",
			   scheme_register_parameter(wxSchemeCurrentEventspace,
						     "current-eventspace",
						     mred_eventspace_param),
			   global_env);
  scheme_install_xc_global("event-dispatch-handler",
			   scheme_register_parameter(wxSchemeEventDispatchHandler,
						     "event-dispatch-handler",
						     mred_event_dispatch_param),
			   global_env);
  scheme_install_xc_global("eventspace?",
			   scheme_make_prim_w_arity(Eventspace_p,
						    "eventspace?",
						    1, 1),
			   global_env);

  scheme_install_xc_global("current-ps-setup",
			   scheme_register_parameter(wxSchemeCurrentPSSetup,
						     "current-ps-setup",
						     mred_ps_setup_param),
			   global_env);

  scheme_install_xc_global("queue-callback",
			   scheme_make_prim_w_arity(queue_callback,
						    "queue-callback",
						    1, 2),
			   global_env);
  MrEd_mid_queue_key = scheme_make_pair(scheme_false, scheme_false);
  scheme_install_xc_global("middle-queue-key", MrEd_mid_queue_key, global_env);


  scheme_install_xc_global("check-for-break",
			   scheme_make_prim_w_arity(wxSchemeCheckForBreak,
						    "check-for-break",
						    0, 0),
			   global_env);


  scheme_install_xc_global("find-graphical-system-path",
			   scheme_make_prim_w_arity(wxSchemeFindDirectory,
						    "find-graphical-system-path",
						    1, 1),
			   global_env);

  scheme_install_xc_global("get-top-level-windows",
			   scheme_make_prim_w_arity(wxSchemeGetFrameList,
						    "get-top-level-windows",
						    0, 0),
			   global_env);

  scheme_install_xc_global("register-collecting-blit",
			   scheme_make_prim_w_arity(wxSchemeRegisterCollectingBitmap,
						    "register-collecting-blit",
						    7, 11),
			   global_env);
  scheme_install_xc_global("unregister-collecting-blit",
			   scheme_make_prim_w_arity(wxSchemeUnregisterCollectingBitmap,
						    "unregister-collecting-blit",
						    1, 1),
			   global_env);

  scheme_install_xc_global("shortcut-visible-in-label?",
			   scheme_make_prim_w_arity(wLabelShortcutsVisible,
						    "shortcut-visible-in-label?",
						    0, 1),
			   global_env);


  scheme_install_xc_global("eventspace-shutdown?",
			   scheme_make_prim_w_arity(Shutdown_p,
						    "eventspace-shutdown?",
						    1, 1),
			   global_env);

  scheme_install_xc_global("in-atomic-region",
			   scheme_make_prim_w_arity(wxInAtomicRegion,
						    "in-atomic-region",
						    1, 1),
			   global_env);

  scheme_install_xc_global("set-editor-snip-maker",
			   scheme_make_prim_w_arity(SetMediaSnipMaker,
						    "set-editor-snip-maker",
						    1, 1),
			   global_env);
  
  scheme_install_xc_global("set-text-editor-maker",
			   scheme_make_prim_w_arity(SetMediaEditMaker,
						    "set-text-editor-maker",
						    1, 1),
			   global_env);
  
  scheme_install_xc_global("set-pasteboard-editor-maker",
			   scheme_make_prim_w_arity(SetMediaPasteboardMaker,
						    "set-pasteboard-editor-maker",
						    1, 1),
			   global_env);
  
  scheme_install_xc_global("location->window",
			   scheme_make_prim_w_arity(wxsLocationToWindow,
						    "location->window",
						    2, 2),
			   global_env);

  /* Order is important! Base class must be initialized before derived. */
  objscheme_setup_wxObject(global_env);
  objscheme_setup_wxWindow(global_env);
  objscheme_setup_wxFrame(global_env);
  objscheme_setup_wxColour(global_env);
  objscheme_setup_wxColourDatabase(global_env);
  objscheme_setup_wxPoint(global_env);
  objscheme_setup_wxBrush(global_env);
  objscheme_setup_wxBrushList(global_env);
  objscheme_setup_wxPen(global_env);
  objscheme_setup_wxPenList(global_env);
  objscheme_setup_wxBitmap(global_env);
  objscheme_setup_wxCursor(global_env);
  objscheme_setup_wxRegion(global_env);
  objscheme_setup_wxFont(global_env);
  objscheme_setup_wxFontList(global_env);
  objscheme_setup_wxFontNameDirectory(global_env);
  objscheme_setup_wxItem(global_env);
  objscheme_setup_wxMessage(global_env);
  objscheme_setup_wxButton(global_env);
  objscheme_setup_wxRadioBox(global_env);
  objscheme_setup_wxCheckBox(global_env);
  objscheme_setup_wxListBox(global_env);
  objscheme_setup_wxChoice(global_env);
  objscheme_setup_wxSlider(global_env);
  objscheme_setup_wxsGauge(global_env);
  objscheme_setup_wxMenu(global_env);
  objscheme_setup_wxMenuBar(global_env);
  objscheme_setup_wxsMenuItem(global_env);
  objscheme_setup_wxEvent(global_env);
  objscheme_setup_wxCommandEvent(global_env);
  objscheme_setup_wxPopupEvent(global_env);
  objscheme_setup_wxScrollEvent(global_env);
  objscheme_setup_wxKeyEvent(global_env);
  objscheme_setup_wxMouseEvent(global_env);
  objscheme_setup_wxDC(global_env);
  objscheme_setup_wxMemoryDC(global_env);
  objscheme_setup_wxPostScriptDC(global_env);
  objscheme_setup_basePrinterDC(global_env);
  objscheme_setup_wxCanvas(global_env);
  objscheme_setup_wxPanel(global_env);
  objscheme_setup_wxDialogBox(global_env);
  objscheme_setup_wxMediaGlobal(global_env);
  objscheme_setup_wxMediaCanvas(global_env);
  objscheme_setup_wxMediaBuffer(global_env);
  objscheme_setup_wxMediaEdit(global_env);
  objscheme_setup_wxMediaPasteboard(global_env);
  objscheme_setup_wxSnipClass(global_env);
  objscheme_setup_wxSnipClassList(global_env);
  objscheme_setup_wxSnip(global_env);
  objscheme_setup_wxTextSnip(global_env);
  objscheme_setup_wxTabSnip(global_env);
  objscheme_setup_wxImageSnip(global_env);
  objscheme_setup_wxMediaSnip(global_env);
  objscheme_setup_wxSnipAdmin(global_env);
  objscheme_setup_wxMediaAdmin(global_env);
  // objscheme_setup_wxCanvasMediaAdmin(global_env);
  objscheme_setup_wxMediaSnipMediaAdmin(global_env);
  objscheme_setup_wxBufferData(global_env);
  objscheme_setup_wxBufferDataClass(global_env);
  objscheme_setup_wxBufferDataClassList(global_env);
  objscheme_setup_wxKeymap(global_env);
  objscheme_setup_wxMediaStreamInBase(global_env);
  objscheme_setup_wxMediaStreamOutBase(global_env);
  objscheme_setup_wxMediaStreamInStringBase(global_env);
  objscheme_setup_wxMediaStreamOutStringBase(global_env);
  objscheme_setup_wxMediaStreamIn(global_env);
  objscheme_setup_wxMediaStreamOut(global_env);
  objscheme_setup_wxMediaWordbreakMap(global_env);
  objscheme_setup_wxAddColour(global_env);
  objscheme_setup_wxMultColour(global_env);
  objscheme_setup_wxStyleDelta(global_env);
  objscheme_setup_wxStyle(global_env);
  objscheme_setup_wxStyleList(global_env);
#if 0
  objscheme_setup_baseMetaFile(global_env);
  objscheme_setup_baseMetaFileDC(global_env);
#endif
  objscheme_setup_wxTimer(global_env);
  objscheme_setup_wxClipboard(global_env);
  objscheme_setup_wxClipboardClient(global_env);
  objscheme_setup_wxPrintSetupData(global_env);

  objscheme_setup_wxsGlobal(global_env);
  objscheme_setup_wxsMenuItemGlobal(global_env);

  scheme_defining_primitives = 0;
}

