
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
#include "wxsmred.h"

#define WXS_SETUP_ONLY 1
#include "wxs_obj.h"
#include "wxs_win.h"
#include "wxs_fram.h"
#include "wxs_item.h"
#include "wxs_butn.h"
#include "wxs_text.h"
#include "wxs_ckbx.h"
#include "wxs_chce.h"
#include "wxs_evnt.h"
#include "wxs_panl.h"
#include "wxs_list.h"
#include "wxs_medi.h"
#include "wxs_mede.h"
#include "wxs_madm.h"
#include "wxs_snip.h"
#include "wxs_mpb.h"
#include "wxs_mio.h"
#include "wxs_styl.h"
#include "wxs_menu.h"
#include "wxs_bmap.h"
#include "wxs_ipc.h"
#include "wxs_help.h"
#include "wxs_misc.h"
#include "wxs_rado.h"
#include "wxs_slid.h"
#include "wxs_gage.h"
#include "wxs_lbox.h"
#if USE_TOOLBAR 
#include "wxs_tbar.h"
#endif
#if USE_CONSTRAINTS && 0
#include "wxs_cstr.h"
#endif
// #include "wxs_form.h"

#include "wxs_glob.h"

#undef WXS_SETUP_ONLY
#include "wxs_gdi.h"
#include "wxs_dc.h"
#include "wxs_cnvs.h"

#include <stdlib.h>

char *WXSCHEME_wrong = "wrong number of parameters to method %s";
char *WXSCHEME_too_many = "too many parameters to method %s";
char *WXSCHEME_too_few = "too few parameters to method %s";
char *WXSCHEME_expected = "expected a %s in %s";

typedef struct GCBitmap {
  wxCanvas **canvasptr; /* weak reference */
  float x, y, w, h;
  float onx, ony, offx, offy;
  wxMemoryDC *on, *off;
  struct GCBitmap *next;
} GCBitmap;

static GCBitmap *gc_bitmaps = NULL;
extern "C" void (*GC_collect_start_callback)(void);
extern "C" void (*GC_collect_end_callback)(void);
static void (*orig_collect_start_callback)(void);
static void (*orig_collect_end_callback)(void);
static void collect_start_callback(void);
static void collect_end_callback(void);

static void wxScheme_Install(Scheme_Env *env, void *global_env);

static Scheme_Object *pref_dir_symbol, *pref_file_symbol, 
  *setup_file_symbol, *init_file_symbol, *init_dir_symbol, 
  *temp_dir_symbol, *autosaves_file_symbol;

#define INSTALL_COUNT 520

static Scheme_Unit *wxs_unit;
static Scheme_Object *wxs_siglist, *wxs_signed_unit, *wxs_signature;

#define CONS scheme_make_pair

static void wxScheme_Invoke(Scheme_Env * env)
{
  if (!wxs_signature) {
    scheme_eval(CONS(scheme_intern_symbol("#%define-signature"),
		     CONS(scheme_intern_symbol("wx^"),
			  CONS(wxs_siglist,
			       scheme_null))),
		env);
    wxs_signature = scheme_lookup_global(scheme_intern_symbol("wx^"), env);
  }
  scheme_add_global_constant("wx^", wxs_signature, env);

  if (!wxs_signed_unit) {
    wxs_signed_unit = scheme_eval(CONS(scheme_intern_symbol("#%unit->unit/sig"),
				       CONS((Scheme_Object *)wxs_unit,
					    CONS(scheme_null,
						 CONS(scheme_intern_symbol("wx^"),
						      scheme_null)))),
				  env);
  }
  
  scheme_add_global_constant("wx@", wxs_signed_unit, env);

  scheme_invoke_unit((Scheme_Object *)wxs_unit, 0, NULL, NULL, 1, "wx", 0, 0);
}

typedef struct {
  int count;
  struct {
    char *name;
    Scheme_Object *val;
  } v[INSTALL_COUNT];
} InstallRec;

static Scheme_Object *wxsUnit_Init(Scheme_Object **boxes, Scheme_Object ** /* anchors */,
				   Scheme_Unit *u, void * /* debug_request */)
{
  InstallRec *rec = (InstallRec *)u->data;
  int i;

  for (i = rec->count; i--; )
    SCHEME_ENVBOX_VAL(boxes[i]) = rec->v[i].val;

  return scheme_void;
}

void wxsScheme_setup(Scheme_Env *env)
{
  InstallRec *rec = (InstallRec *)scheme_malloc(sizeof(InstallRec));
  Scheme_Unit *u;
  int i;

  objscheme_init(env);

  setup_file_symbol = scheme_intern_symbol("setup-file");
  pref_dir_symbol = scheme_intern_symbol("pref-dir");
  pref_file_symbol = scheme_intern_symbol("pref-file");
  init_file_symbol = scheme_intern_symbol("init-file");
  init_dir_symbol = scheme_intern_symbol("init-dir");
  autosaves_file_symbol = scheme_intern_symbol("autosaves-file");
  temp_dir_symbol = scheme_intern_symbol("temp-dir");

  rec->count = 0;

  wxScheme_Install(env, rec);

  qsort(rec->v, rec->count, sizeof(char*) + sizeof(Scheme_Object *), 
	(int (*)(const void *, const void *))strcmp);
  u = (Scheme_Unit *)scheme_malloc(sizeof(Scheme_Unit));
  u->type = scheme_unit_type;
  u->num_imports = 0;
  u->num_exports = rec->count;
  u->exports = (Scheme_Object **)scheme_malloc(sizeof(Scheme_Object *) * rec->count);
  u->export_debug_names = NULL;
  u->data = (Scheme_Object *)rec;
  wxs_siglist = scheme_null;
  for (i = rec->count; i--; ) {
    Scheme_Object *s = scheme_intern_symbol(rec->v[i].name + 3); /* skip `wx:' */
    u->exports[i] = s;
    wxs_siglist = scheme_make_pair(s, wxs_siglist);
  }
  u->init_func = wxsUnit_Init;

  wxs_unit = u;

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

  rec->v[rec->count].name = name;
  rec->v[rec->count].val = val;
  rec->count++;
}

Scheme_Object * scheme_lookup_xc_global(char *name, void *env)
{
  InstallRec *rec = (InstallRec *)env;
  int i;

  for (i = 0; i < rec->count; i++)
    if (!strcmp(rec->v[i].name, name))
      return rec->v[i].val;

  return NULL;
}


#ifdef wx_x
extern Display *MrEdGetXDisplay(void);
#endif

static void draw_gc_bm(int on)
{
  GCBitmap *gcbm = gc_bitmaps;
  while (gcbm) {
    if (*gcbm->canvasptr)
      (*gcbm->canvasptr)->GetDC()->Blit(gcbm->x, gcbm->y,
					gcbm->w, gcbm->h,
					on ? gcbm->on : gcbm->off,
					0, 0);
    gcbm = gcbm->next;
  }
#ifdef wx_x
  XFlush(MrEdGetXDisplay());
#endif
}

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

static Scheme_Object *wxSchemeUnregisterCollectingBitmap(int, Scheme_Object **a)
{
  GCBitmap *gcbm, *prev = NULL;
  wxCanvas *c;

  if (a)
    c = objscheme_unbundle_wxCanvas(a[0], "wx:unregister-collecting-blit", 0);
  else
    c = NULL;
  
  gcbm = gc_bitmaps;
  while (gcbm) {
    if (!gcbm->canvasptr || (*gcbm->canvasptr == c)) {
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

  gcbm = new GCBitmap;

  gcbm->canvasptr = (wxCanvas **)scheme_malloc_atomic(sizeof(wxCanvas*));

  *gcbm->canvasptr = objscheme_unbundle_wxCanvas(a[0], "wx:register-collecting-blit", 0);
  gcbm->x = objscheme_unbundle_float(a[1], "wx:register-collecting-blit");
  gcbm->y = objscheme_unbundle_float(a[2], "wx:register-collecting-blit");
  gcbm->w = objscheme_unbundle_float(a[3], "wx:register-collecting-blit");
  gcbm->h = objscheme_unbundle_float(a[4], "wx:register-collecting-blit");
  gcbm->on = objscheme_unbundle_wxMemoryDC(a[5], "wx:register-collecting-blit", 0);
  gcbm->off = objscheme_unbundle_wxMemoryDC(a[6], "wx:register-collecting-blit", 0);
  gcbm->onx = gcbm->ony = gcbm->offx = gcbm-> offy = 0;
  if (n > 7) {
    gcbm->onx = objscheme_unbundle_float(a[7], "wx:register-collecting-blit");
    if (n > 8) {
      gcbm->ony = objscheme_unbundle_float(a[8], "wx:register-collecting-blit");
      if (n > 9) {
	gcbm->offx = objscheme_unbundle_float(a[9], "wx:register-collecting-blit");
	if (n > 10) {
	  gcbm->offy = objscheme_unbundle_float(a[10], "wx:register-collecting-blit");
	}
      }
    }
  }

  gcbm->next = gc_bitmaps;
  gc_bitmaps = gcbm;

  GC_general_register_disappearing_link((void **)gcbm->canvasptr, 
					*gcbm->canvasptr);

  wxSchemeUnregisterCollectingBitmap(0, NULL);

  return scheme_void;
}

static Scheme_Object *wxSchemeCanGetUserColour(int, Scheme_Object **)
{
#ifdef wx_x
  return scheme_false;
#else
  return scheme_true;
#endif
}

#ifdef wx_msw
static BOOL do_choose_color(void *data, HWND parent)
{
  CHOOSECOLOR *c = (CHOOSECOLOR *)data;
  c->hwndOwner = parent;

  return ChooseColor(c);
}
#endif

static Scheme_Object *wxSchemeGetColourFromUser(int, Scheme_Object **argv)
{
  char *s;

  if (SCHEME_NULLP(argv[0]))
    s = "Choose a color";
  else
    s = objscheme_unbundle_string(argv[0], "wx:get-colour-from-user");

#ifndef wx_x
  wxColour *c = objscheme_unbundle_wxColour(argv[1], "wx:get-colour-from-user", 1);
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
  cc->hwndOwner = NULL;
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

static Scheme_Object *wxSchemeCanGetUserFont(int, Scheme_Object **)
{
#ifdef wx_msw
  return scheme_true;
#else
  return scheme_false;
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

static Scheme_Object *wxSchemeGetFontFromUser(int, Scheme_Object **argv)
{
  char *prompt;

  if (SCHEME_NULLP(argv[0]))
    prompt = "Choose a font";
  else
    prompt = objscheme_unbundle_string(argv[0], "wx:get-font-from-user");

#ifdef wx_msw
  wxFont *f = objscheme_unbundle_wxFont(argv[1], "wx:get-font-from-user", 1);
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
  c->hwndOwner = NULL;
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
  char **xnames = XListFonts(__DISPLAY, "*", 50000, &count), **names;
  int last_pos = -1, last_len = 0;

  names = new char* [count];
  for (i = 0; i < count; i++)
    names[i] = xnames[i];

  qsort(names, count, sizeof(char *), 
	(int (*)(const void *, const void *))indirect_strcmp);

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

#ifdef wx_x
    while ((i < count)
	   && ((last_pos >= 0) 
	       && !strncmp(names[i], names[last_pos], last_len)))
      i++;
    if (i >= count)
      break;

    last_pos = i;
    if (names[i][0] != '-') {
      l = strlen(names[i]);
    } else {
      int c = 0;
      for (l = 0; names[i][l]; l++)
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
    last_len = l;
    
    s = names[i++];
#endif
#ifdef wx_mac
    if (i >= count)
      break;
    GetMenuItemText(fmenu, ++i, buffer);
    l = buffer[0];
    s = PtoCstr(buffer);
#endif
#ifdef wx_msw
    if (i >= data.count)
      break;
    s = data.names[i++];
    l = strlen(s);
#endif
    
    Scheme_Object *pr = scheme_make_pair(scheme_make_sized_string(s, l, 1), scheme_null);
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
    scheme_wrong_type("wx:play-sound", "string", 0, argc, argv);
  
  async = SCHEME_TRUEP(argv[1]);
  
  f = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
			     SCHEME_STRTAG_VAL(argv[0]),
			     "wx:play-sound",
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

#ifdef wx_mac
extern short wxMacDisableMods;
#define SCK_ARG p
#else
#define SCK_ARG /**/
#endif

Scheme_Object *wxs_app_file_proc;

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
    scheme_check_proc_arity("wx:application-file-handler", 1,
			    0, n, p);
    wxs_app_file_proc = p[0];
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
  return scheme_param_config("wx:current-eventspace", mred_eventspace_param,
			     argc, argv,
			     -1, Eventspace_p, "eventspace", 0);
}

static Scheme_Object *wxSchemeEventDispatchHandler(int argc, Scheme_Object **argv)
{
  return scheme_param_config("wx:event-dispatch-handler", 
			     mred_event_dispatch_param,
			     argc, argv,
			     1, NULL, NULL, 0);
}

static Scheme_Object *wxSchemeEventspaceConfig(int argc, Scheme_Object **argv)
{
  if (SCHEME_TYPE(argv[0]) != mred_eventspace_type)
    scheme_wrong_type("wx:eventspace-parameterization", "eventspace",
		      0, argc, argv);

  return MrEdEventspaceConfig(argv[0]);
}

extern void wxDispatchEventsUntil(int (*f)(void *), void *data);

static Scheme_Object *wxSchemeMakeEventspace(int argc, Scheme_Object **argv)
{
  if (argc && (SCHEME_TYPE(argv[0]) != scheme_config_type))
    scheme_wrong_type("wx:make-eventspace", "parameterization",
		      0, argc, argv);

  return (Scheme_Object *)MrEdMakeEventspace(argc ? (Scheme_Config *)argv[0] : (Scheme_Config *)NULL);
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
      scheme_wrong_type("wx:yield", "semaphore", -1, 0, (Scheme_Object **)&sema);

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

static Scheme_Object *wxSchemeGetFrameList(int, Scheme_Object **)
{
  return MrEdGetFrameList();
}

#ifdef wx_mac
extern "C" {
 extern char *scheme_build_mac_filename(FSSpec *spec, int given_dir);
};
extern char *wxmac_startup_directory;
#endif

static Scheme_Object *wxSchemeFindDirectory(int argc, Scheme_Object **argv)
{
  enum {
    id_tmp_dir,
    id_pref_dir,
    id_init_dir,
    id_pref_file,
    id_init_file,
    id_setup_file,
    id_autosaves_file
  };

  int which;

  if (argv[0] == temp_dir_symbol)
    which = id_tmp_dir;
  else if (argv[0] == pref_dir_symbol)
    which = id_pref_dir;
  else if (argv[0] == pref_file_symbol)
    which = id_pref_file;
  else if (argv[0] == init_file_symbol)
    which = id_init_file;
  else if (argv[0] == init_dir_symbol)
    which = id_init_dir;
  else if (argv[0] == setup_file_symbol)
    which = id_setup_file;
  else if (argv[0] == autosaves_file_symbol)
    which = id_autosaves_file;
  else {
    scheme_wrong_type("wx:find-path", "find-path-symbol",
		      0, argc, argv);
    return NULL;
  }

#ifdef wx_x
  if (which == id_tmp_dir) {
    char *p;
    
    if ((p = getenv("TMPDIR"))) {
      p = scheme_expand_filename(p, -1, NULL, NULL);
      if (p && scheme_directory_exists(p))
	return scheme_make_string(p);
    }

    if (scheme_directory_exists("/usr/tmp"))
      return scheme_make_string("/usr/tmp");

    return scheme_make_string("/tmp");
  }

  Scheme_Object *home;

  home = scheme_make_string(scheme_expand_filename("~/", 2, NULL, NULL));

  if ((which == id_pref_dir) || (which == id_init_dir))
    return home;

  int ends_in_slash;
  ends_in_slash = (SCHEME_STR_VAL(home))[SCHEME_STRTAG_VAL(home) - 1] == '/';

  if (which == id_pref_file)
    return scheme_append_string(home,
				scheme_make_string("/.mred.prefs" + ends_in_slash));

  if (which == id_init_file)
    return scheme_append_string(home,
				scheme_make_string("/.mredrc" + ends_in_slash));

  if (which == id_setup_file)
    return scheme_append_string(home,
				scheme_make_string("/.mred.resources" + ends_in_slash));

  if (which == id_autosaves_file)
    return scheme_append_string(home,
				scheme_make_string("/.mred.saves" + ends_in_slash));
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
    
    while (i && (s[i] != '\\'))
      --i;
    s[i] = 0;
    home = scheme_make_string_without_copying(s);
  }

  if (which == id_tmp_dir)
    return home;

  if (which == id_pref_dir)
    return home;

  if (which == id_init_dir)
    return home;

  int ends_in_slash;
  ends_in_slash = (SCHEME_STR_VAL(home))[SCHEME_STRTAG_VAL(home) - 1];
  ends_in_slash = ((ends_in_slash == '/') || (ends_in_slash == '\\'));

  if (which == id_pref_file)
    return scheme_append_string(home,
				scheme_make_string("\\mred.pre" + ends_in_slash));

  if (which == id_init_file)
    return scheme_append_string(home,
				scheme_make_string("\\mredrc.ss" + ends_in_slash));

  if (which == id_setup_file)
    return scheme_append_string(home,
				scheme_make_string("\\mred.ini" + ends_in_slash));  

  if (which == id_autosaves_file)
    return scheme_append_string(home,
				scheme_make_string("\\mred.asv" + ends_in_slash));  
#endif

#ifdef wx_mac
  OSType t;
  FSSpec spec;
  Scheme_Object *home;

  switch (which) {
  case id_pref_dir:
  case id_init_dir:
  case id_pref_file:
  case id_init_file:
  case id_setup_file:
    t = 'pref';
    break;
  case id_tmp_dir:
  default:
    t = 'temp';
    break;
  }

  if (!FindFolder(kOnSystemDisk, t, kCreateFolder, &spec.vRefNum, &spec.parID))
    home = scheme_make_string(scheme_build_mac_filename(&spec, 1));
  else if (wxmac_startup_directory) {
    home = scheme_make_string(wxmac_startup_directory);
  } else {
    home = scheme_make_string(scheme_getcwd(NULL, 0, NULL, 1));
  }
  
  if ((which == id_pref_dir) || (which == id_tmp_dir) || (which == id_init_dir))
    return home;

  int ends_in_colon;
  ends_in_colon = (SCHEME_STR_VAL(home))[SCHEME_STRTAG_VAL(home) - 1] == ':';

  if (which == id_pref_file)
    return scheme_append_string(home,
				scheme_make_string(":MrEd Preferences" + ends_in_colon));

  if (which == id_init_file)
    return scheme_append_string(home,
				scheme_make_string(":mredrc.ss" + ends_in_colon));

  if (which == id_setup_file)
    return scheme_append_string(home,
				scheme_make_string(":mred.fnt" + ends_in_colon));  

  if (which == id_pref_file)
    return scheme_append_string(home,
				scheme_make_string(":MrEd Autosaves" + ends_in_colon));
#endif

  return scheme_void;
}

static void wxScheme_Install(Scheme_Env *env, void *global_env)
{
  static int installed = 0;

  scheme_defining_primitives = 1;
  
  if (!installed) {
    installed = 1;
    scheme_add_namespace_option(scheme_intern_symbol("wx"), wxScheme_Invoke);
    
    wxs_app_file_proc = scheme_make_prim_w_arity(DefaultAppFileProc,
						 "default-application-file-handler",
						 1, 1);
  }

#ifdef wx_x
  scheme_install_xc_global("wx:platform", 
			   scheme_intern_symbol("unix"), global_env);
#ifdef wx_motif
  scheme_install_xc_global("wx:window-system", 
			   scheme_intern_symbol("motif"), global_env);
#else
#ifdef wx_xt
  scheme_install_xc_global("wx:window-system", 
			   scheme_intern_symbol("xt"), global_env);
#else
  scheme_install_xc_global("wx:window-system", 
			   scheme_intern_symbol("xview"), global_env);
#endif
#endif  
#endif
#ifdef wx_msw
  scheme_install_xc_global("wx:platform", 
			   scheme_intern_symbol("windows"), global_env);
  scheme_install_xc_global("wx:window-system", 
			   scheme_intern_symbol("windows"), global_env);
#endif  
#ifdef wx_mac
  scheme_install_xc_global("wx:platform", 
			   scheme_intern_symbol("macintosh"), global_env);
  scheme_install_xc_global("wx:window-system", 
			   scheme_intern_symbol("macintosh"), global_env);
#endif

  scheme_install_xc_global("wx:special-control-key", 
			   scheme_make_prim_w_arity(SpecialCtlKey, 
						    "wx:special-control-key", 
						    1, 1), 
			   global_env);
  
  scheme_install_xc_global("wx:application-file-handler",
			   scheme_make_prim_w_arity(ApplicationFileProc,
						    "wx:application-file-handler",
						    1, 1),
			   global_env);
  
  scheme_install_xc_global("wx:can-get-user-colour?",
			   scheme_make_prim_w_arity(wxSchemeCanGetUserColour,
						    "wx:can-get-user-colour?",
						    0, 0),
			   global_env);
  scheme_install_xc_global("wx:can-get-user-font?",
			   scheme_make_prim_w_arity(wxSchemeCanGetUserFont,
						    "wx:can-get-user-font?",
						    0, 0),
			   global_env);
  
  scheme_install_xc_global("wx:get-colour-from-user",
			   scheme_make_prim_w_arity(wxSchemeGetColourFromUser,
						    "wx:get-colour-from-user",
						    2, 2),
			   global_env);
  
  scheme_install_xc_global("wx:get-font-from-user",
			   scheme_make_prim_w_arity(wxSchemeGetFontFromUser,
						    "wx:get-font-from-user",
						    2, 2),
			   global_env);
  
  scheme_install_xc_global("wx:get-font-list",
			   scheme_make_prim_w_arity(wxSchemeGetFontList,
						    "wx:get-font-list",
						    0, 0),
			   global_env);
  
#ifdef wx_x
  Scheme_Object *ps;

  ps = scheme_eval_string("(#%lambda (f async?)"
			  "  (#%unless (#%string? f)"
			  "    (#%raise-type-error 'wx:play-sound \"string\" f))"
			  "  (#%let ([b (#%box \"cat ~s > /dev/audio\")])"
			  "    (#%wx:get-resource \"mred\" \"playcmd\" b)"
			  "    ((#%if async? (#%lambda (x) (#%process x) #t) #%system)"
			  "     (#%format (#%unbox b) (#%expand-path f)))))",
			  env);
  scheme_install_xc_global("wx:play-sound", ps, global_env);
#else
  scheme_install_xc_global("wx:play-sound", 
			     scheme_make_prim_w_arity(wxPlaySound, 
						      "wx:play-sound", 
						      2, 2), 
			     global_env);
#endif

  scheme_install_xc_global("wx:make-eventspace",
			     scheme_make_prim_w_arity(wxSchemeMakeEventspace,
						      "wx:make-eventspace",
						      0, 1),
			     global_env);
  scheme_install_xc_global("wx:current-eventspace",
			   scheme_register_parameter(wxSchemeCurrentEventspace,
						     "wx:current-eventspace",
						     mred_eventspace_param),
			   global_env);
  scheme_install_xc_global("wx:event-dispatch-handler",
			   scheme_register_parameter(wxSchemeEventDispatchHandler,
						     "wx:event-dispatch-handler",
						     mred_event_dispatch_param),
			   global_env);
  scheme_install_xc_global("wx:eventspace?",
			   scheme_make_prim_w_arity(Eventspace_p,
						    "wx:eventspace?",
						    1, 1),
			   global_env);
  scheme_install_xc_global("wx:eventspace-parameterization",
			   scheme_make_prim_w_arity(wxSchemeEventspaceConfig,
						    "wx:eventspace-parameterization",
						    1, 1),
			   global_env);

  scheme_install_xc_global("wx:check-for-break",
			   scheme_make_prim_w_arity(wxSchemeCheckForBreak,
						    "wx:check-for-break",
						    0, 0),
			   global_env);


  scheme_install_xc_global("wx:find-path",
			   scheme_make_prim_w_arity(wxSchemeFindDirectory,
						    "wx:find-path",
						    1, 1),
			   global_env);

  scheme_install_xc_global("wx:get-frame-list",
			   scheme_make_prim_w_arity(wxSchemeGetFrameList,
						    "wx:get-frame-list",
						    0, 0),
			   global_env);

  scheme_install_xc_global("wx:register-collecting-blit",
			   scheme_make_prim_w_arity(wxSchemeRegisterCollectingBitmap,
						    "wx:register-collecting-blit",
						    7, 11),
			   global_env);
  scheme_install_xc_global("wx:unregister-collecting-blit",
			   scheme_make_prim_w_arity(wxSchemeUnregisterCollectingBitmap,
						    "wx:unregister-collecting-blit",
						    1, 1),
			   global_env);

  /* Order is important! Base class must be initialized before derived. */
  objscheme_setup_wxObject(global_env);
#if 0
  objscheme_setup_wxNode(global_env);
  objscheme_setup_wxList(global_env);
  objscheme_setup_wxHashTable(global_env);
  objscheme_setup_wxPathList(global_env);
  objscheme_setup_wxStringList(global_env);
#endif
  objscheme_setup_wxWindow(global_env);
  objscheme_setup_wxFrame(global_env);
  objscheme_setup_wxColour(global_env);
  objscheme_setup_wxColourMap(global_env);
  objscheme_setup_wxColourDatabase(global_env);
  objscheme_setup_wxPoint(global_env);
  objscheme_setup_wxIntPoint(global_env);
  objscheme_setup_wxBrush(global_env);
  objscheme_setup_wxBrushList(global_env);
  objscheme_setup_wxPen(global_env);
  objscheme_setup_wxPenList(global_env);
  objscheme_setup_wxBitmap(global_env);
  objscheme_setup_wxIcon(global_env);
  objscheme_setup_wxCursor(global_env);
  objscheme_setup_wxFont(global_env);
  objscheme_setup_wxFontList(global_env);
  objscheme_setup_wxFontNameDirectory(global_env);
  objscheme_setup_wxItem(global_env);
  objscheme_setup_wxMessage(global_env);
  // objscheme_setup_wxGroupBox(global_env);
  objscheme_setup_wxButton(global_env);
  objscheme_setup_wxRadioBox(global_env);
  objscheme_setup_wxCheckBox(global_env);
  objscheme_setup_wxListBox(global_env);
  objscheme_setup_wxChoice(global_env);
  objscheme_setup_wxSlider(global_env);
  objscheme_setup_wxsGauge(global_env);
  objscheme_setup_wxText(global_env);
  objscheme_setup_wxMultiText(global_env);
  objscheme_setup_wxTextWindow(global_env);
  objscheme_setup_wxMenu(global_env);
  objscheme_setup_wxMenuBar(global_env);
//  objscheme_setup_wxMenuItem(global_env);
  objscheme_setup_wxEvent(global_env);
  objscheme_setup_wxCommandEvent(global_env);
  objscheme_setup_wxKeyEvent(global_env);
  objscheme_setup_wxMouseEvent(global_env);
  objscheme_setup_wxDC(global_env);
  objscheme_setup_wxCanvasDC(global_env);
#if 0
#ifndef wx_mac
  objscheme_setup_wxPanelDC(global_env);
#endif
#endif
  objscheme_setup_wxMemoryDC(global_env);
  objscheme_setup_wxPostScriptDC(global_env);
  objscheme_setup_basePrinterDC(global_env);
  objscheme_setup_wxCanvas(global_env);
  objscheme_setup_wxPanel(global_env);
  objscheme_setup_wxDialogBox(global_env);
#if USE_ENHANCED_DIALOG && 0
  objscheme_setup_wxEnhDialogBox(global_env);
#endif
//  objscheme_setup_wxFormItem(global_env);
//  objscheme_setup_wxForm(global_env);
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
  objscheme_setup_wxCanvasMediaAdmin(global_env);
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
  objscheme_setup_wxConnection(global_env);
  objscheme_setup_wxClient(global_env);
  objscheme_setup_wxServer(global_env);
#endif
#if USE_HELP && 0
  objscheme_setup_wxHelpInstance(global_env);
#endif
  objscheme_setup_baseMetaFile(global_env);
  objscheme_setup_baseMetaFileDC(global_env);
  objscheme_setup_wxTimer(global_env);
  objscheme_setup_wxClipboard(global_env);
  objscheme_setup_wxClipboardClient(global_env);
#if 0
  objscheme_setup_wxTypeTree(global_env);
#endif
#if USE_TOOLBAR 
  objscheme_setup_wxToolBar(global_env);
  objscheme_setup_wxToolBarTool(global_env);
#if USE_BUTTONBAR && defined(wx_msw)
  objscheme_setup_wxButtonBar(global_env);
#endif
#endif
#if USE_CONSTRAINTS && 0
  objscheme_setup_wxLayoutConstraints(global_env);
  objscheme_setup_wxIndividualLayoutConstraint(global_env);
#endif

  objscheme_setup_wxsGlobal(global_env);
#if 0
  objscheme_setup_wxsIPCGlobal(global_env);
#endif
#if 0
  objscheme_setup_wxsTypesGlobal(global_env);
#endif
//  objscheme_setup_wxFormGlobal(global_env);

  scheme_defining_primitives = 0;
}

