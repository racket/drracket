
@INCLUDE prefix.xci

#include "wx_utils.h"
#include "wx_dialg.h"
#include "wx_cmdlg.h"
#include "wx_timer.h"
#include "wx_dcps.h"
#include "wx_main.h"

#if USE_METAFILE
#include "wx_mf.h"
#endif

@INCLUDE wxs.xci

@HEADER

static void wxsFillPrivateColor(wxDC *dc, wxColour *c)
{
#ifdef wx_x
 ((wxWindowDC *)dc)->FillPrivateColor(c);
#endif
}

@BEGINSYMBOLS fileSelMode > ONE > PRED BUNDLE
@SYM "get" : wxOPEN
@SYM "put" : wxSAVE
@SYM "overwrite-prompt" : wxOVERWRITE_PROMPT
@SYM "hide-readonly" : wxHIDE_READONLY
@ENDSYMBOLS

#define USE_PRINTER 1

extern Bool wxSchemeYield(void *sema);

extern void wxFlushDisplay(void);

#ifdef wx_x
#define FILE_SEL_DEF_PATTERN "*"
#else
#define FILE_SEL_DEF_PATTERN "*.*"
#endif

static char *wxStripMenuCodes_Scheme(char *in)
{
  static char *buffer = NULL;
  static long buflen = 0;
  long len;

  len = strlen(in);
  if (buflen <= len) {
    if (buffer)
      delete[] buffer;
    buflen = 2 * len + 1;
    buffer = new char[buflen];
  }

  wxStripMenuCodes(in, buffer);
  return buffer;
}

#ifdef wx_xt
extern void wxBell(void);
#endif

@GLOBAL wxsGlobal

extern int objscheme_istype_wxFrame(Scheme_Object *obj, const char *stop, int nullOK);
extern class wxFrame *objscheme_unbundle_wxFrame(Scheme_Object *obj, const char *where, int nullOK);
extern int objscheme_istype_wxDialogBox(Scheme_Object *obj, const char *stop, int nullOK);
extern class wxDialogBox *objscheme_unbundle_wxDialogBox(Scheme_Object *obj, const char *where, int nullOK);

@MACRO ubFrameDialog[who] = (((n <= {s}) || XC_SCHEME_NULLP({x})) ? (wxWindow *)NULL : (objscheme_istype_wxFrame({x}, NULL, 1) ? (wxWindow *)objscheme_unbundle_wxFrame({x}, NULL, 0) : (objscheme_istype_wxDialogBox({x}, NULL, 1) ? (wxWindow *)objscheme_unbundle_wxDialogBox({x}, NULL, 0) : (scheme_wrong_type(<who>, "frame% or dialog%", -1, 0, &{x}), (wxWindow *)NULL))))
@MACRO cFrameDialog = (objscheme_istype_wxFrame({x}, NULL, 1) || objscheme_istype_wxDialogBox({x}, NULL, 1))

@ "file-selector" : nstring wxFileSelector(nstring,nstring=NULL,nstring=NULL,nstring=NULL,nstring=FILE_SEL_DEF_PATTERN,SYM[fileSelMode]=wxOPEN,wxWindow^//ubFrameDialog["file-selector"]/cFrameDialog=NULL,int=-1,int=-1);

@ "is-color-display?" : bool wxColourDisplay();
@ "get-display-depth" : int wxDisplayDepth();

#if !USE_METAFILE
#define wxMakeMetaFilePlaceable(a,b,c,d,e,f) TRUE
#endif
@ "make-meta-file-placeable" : bool wxMakeMetaFilePlaceable(string,float,float,float,float,float);


@ "begin-busy-cursor" : void wxBeginBusyCursor()
@ "is-busy?" : bool wxIsBusy();
@ "end-busy-cursor" : void wxEndBusyCursor();
@ "bell" : void wxBell();
@ "display-size" : void wxDisplaySize(int*,int*);

@ "label->plain-label" : string wxStripMenuCodes_Scheme(string);

@ "get-resource" : bool wxGetResource(string,string,string*,npathname=NULL); <> string
@ "get-resource" : bool wxGetResource(string,string,long*,npathname=NULL); <> number
@ "write-resource" : bool wxWriteResource(string,string,string,npathname=NULL); <> string
@ "write-resource" : bool wxWriteResource(string,string,ExactLong,npathname=NULL); <> number

@MACRO BundleVoidStar = (void *){x}
@MACRO spSema = semaphore

@ "yield" : void wxSchemeYield(void[]=NULL//BundleVoidStar///spSema/push);
@ "flush-display" : void wxFlushDisplay();

@ "fill-private-color" : void wxsFillPrivateColor(wxDC!, wxColour!);

@END
