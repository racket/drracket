
@INCLUDE prefix.xci

#include "wx_dccan.h"
#include "wx_dcmem.h"
#include "wx_dcps.h"
#ifndef wx_mac
#include "wx_dcpan.h"
#endif
#ifdef wx_msw
#include "wx_mf.h"
#endif
#include "wx_types.h"
#ifdef wx_mac
#include "wx_dcpr.h"
#endif
#include "wx_rgn.h"

@INCLUDE wxs.xci

@HEADER

@BEGINSYMBOLS textMode > ONE > PRED
@SYM "transparent" : wxTRANSPARENT
@SYM "solid" : wxSOLID
@ENDSYMBOLS

@BEGINSYMBOLS bitmapDrawStyle > ONE > PRED BUNDLE
@SYM "solid" : wxSOLID
@SYM "opaque" : wxSTIPPLE
@SYM "xor" : wxXOR
@ENDSYMBOLS

@INCLUDE wxs_drws.xci

@MACRO CastToSO = (Scheme_Object*){x}
@MACRO spAnything = _

static wxColour* dcGetTextBackground(wxDC *dc)
{
  wxColour *c, *bg;
  SETUP_VAR_STACK(2);
  VAR_STACK_PUSH(0, dc);
  VAR_STACK_PUSH(1, c);

  c = WITH_VAR_STACK(new wxColour());
#ifdef MZ_PRECISE_GC
  WITH_VAR_STACK(c->gcInit_wxColour());
#endif
  bg = WITH_VAR_STACK(dc->GetTextBackground());
  WITH_VAR_STACK(c->CopyFrom(bg));
  return c;
}

static wxColour* dcGetTextForeground(wxDC *dc)
{
  wxColour *c, *fg;
  SETUP_VAR_STACK(2);
  VAR_STACK_PUSH(0, dc);
  VAR_STACK_PUSH(1, c);

  c = WITH_VAR_STACK(new wxColour());
#ifdef MZ_PRECISE_GC
  WITH_VAR_STACK(c->gcInit_wxColour());
#endif
  fg = WITH_VAR_STACK(dc->GetTextForeground());
  WITH_VAR_STACK(c->CopyFrom(fg));
  return c;
}

static Bool DrawBitmap(wxDC *dc, wxBitmap *bm, float x, float y, int mode, wxColour *c)
{
  REMEMBER_VAR_STACK();
  if (bm->Ok()) {
    return WITH_REMEMBERED_STACK(dc->Blit(x, y, bm->GetWidth(), bm->GetHeight(), bm, 0, 0, mode, c));
  } else
    return FALSE;
}

static Bool DrawBitmapRegion(wxDC *dc, wxBitmap *bm, float x, float y, float dx, float dy, float dw, float dh, int mode, wxColour *c)
{
  REMEMBER_VAR_STACK();
  if (bm->Ok()) {
    return WITH_REMEMBERED_STACK(dc->Blit(x, y, dw, dh, bm, dx, dy, mode, c));
  } else
    return FALSE;
}

static void* MyTextExtent(wxDC *dc, char *s, wxFont *f, Bool big, int offset)
{
  float w, h, d, asc;
  Scheme_Object *a[4];
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH_ARRAY(0, a, 4);

  if (!dc->Ok()) {
    a[0] = a[1] = a[2] = a[3] = WITH_VAR_STACK(scheme_make_double(0.0));
  } else {
    WITH_VAR_STACK(dc->GetTextExtent(s, &w, &h, &d, &asc, f, big, offset));
    
    a[0] = WITH_VAR_STACK(scheme_make_double(w));
    a[1] = WITH_VAR_STACK(scheme_make_double(h));
    a[2] = WITH_VAR_STACK(scheme_make_double(d));
    a[3] = WITH_VAR_STACK(scheme_make_double(asc));
  }

  return WITH_VAR_STACK(scheme_values(4, a));
}

static void* MyGetSize(wxDC *dc)
{
  float w, h;
  Scheme_Object *a[2];
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH_ARRAY(0, a, 2);

  dc->GetSize(&w, &h);

  a[0] = WITH_VAR_STACK(scheme_make_double(w));
  a[1] = WITH_VAR_STACK(scheme_make_double(h));

  return WITH_VAR_STACK(scheme_values(2, a));
}

@MACRO CheckStringIndex[n.s.i] = if (x<i> > SCHEME_STRLEN_VAL(p[<s>])) WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("dc<%>",<n>), "string index too large: ", p[<i>]));

@CLASSBASE wxDC "dc":"object"
@INTERFACE "dc"

@CLASSID wxTYPE_DC

@SETMARK Q = U
@INCLUDE wxs_draw.xci

// Also in wxWindow:
@ m "get-text-extent" : void[]/CastToSO//spAnything MyTextExtent(string,wxFont^=NULL,bool=FALSE,nnint=0); : : /CheckStringIndex["get-text-extent".0.3]
@ Q "get-char-height" : float GetCharHeight();
@ Q "get-char-width" : float GetCharWidth();

@MACRO rZERO = return 0;
@MACRO rFALSE = return FALSE;

#ifndef wx_mac
#define HIDETHISSTATEMENT(x) x
#else
#define HIDETHISSTATEMENT(x) 
#endif

@MACRO IFNOTMAC = HIDETHISSTATEMENT(
@MACRO ENDIF = )

#ifndef wx_mac
#define CHECKTHISONE(x) x
#else
#define CHECKTHISONE(x) 1
#endif

@ m "draw-bitmap-section" : bool DrawBitmapRegion(wxBitmap!,float,float,float,float,nnfloat,nnfloat,SYM[bitmapDrawStyle]=wxSOLID,wxColour!=NULL); : : /CheckOkFalse|CheckFalse[0] : : rFALSE <> with size
@ m "draw-bitmap" : bool DrawBitmap(wxBitmap!,float,float,SYM[bitmapDrawStyle]=wxSOLID,wxColour!=NULL);

@ Q "try-color" : void TryColour(wxColour!,wxColour!);

@ Q "set-text-mode" : void SetBackgroundMode(SYM[textMode]); :  : /CheckOk
@ Q "set-scale" : void SetUserScale(nnfloat,nnfloat); : : /CheckOk
@ Q "set-origin" : void SetDeviceOrigin(float,float); : : /CheckOk

@ q "get-background" : wxColour! GetBackground();
@ q "get-text-mode" : SYM[textMode] GetBackgroundMode();
@ q "get-brush" : wxBrush! GetBrush();
@ q "get-font" : wxFont! GetFont();
@ q "get-pen" : wxPen! GetPen();
@ m "get-text-background" : wxColour! dcGetTextBackground();
@ m "get-text-foreground" : wxColour! dcGetTextForeground();

@ m "get-size" : void[]/CastToSO//spAnything MyGetSize();

@ q "ok?" : bool Ok();

@ Q "start-doc" : bool StartDoc(string); : : : rFALSE
@ Q "start-page" : void StartPage();
@ Q "end-doc" : void EndDoc();
@ Q "end-page" : void EndPage();

@END

@CLASSBASE wxMemoryDC "bitmap-dc":"dc"

@CLASSID wxTYPE_DC_MEMORY

@CREATOR ()

@ "get-pixel" : bool GetPixel(float,float,wxColour^)
@ "set-pixel" : void SetPixel(float,float,wxColour^)

@ "set-bitmap" : void SelectObject(wxBitmap^);  : : /CHECKOKFORDC[0.METHODNAME("memory-dc","set-bitmap")]
@ "get-bitmap" : wxBitmap^ GetObject();

@END

@CLASSBASE wxPostScriptDC "post-script-dc":"dc"

@CLASSID wxTYPE_DC_POSTSCRIPT

@INCLUDE wxs_dorf.xci

@CREATOR (bool=TRUE)

@END

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

#ifdef wx_x

class basePrinterDC : public wxObject
{
public:
  basePrinterDC();
};

basePrinterDC::basePrinterDC()
{
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "%s", 
		   METHODNAME("printer-dc%","initialization")": not supported for X Windows");
}

#else

class basePrinterDC : public wxPrinterDC
{
public:
  basePrinterDC();
};

basePrinterDC::basePrinterDC() 
: wxPrinterDC( )
{
}

#endif

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

@CLASSBASE basePrinterDC "printer-dc":"dc"

@CLASSID wxTYPE_DC_PRINTER

@CREATOR ();

@END




#if 0

#ifdef wx_msw

class baseMetaFileDC : public wxMetaFileDC {
public:
  baseMetaFileDC(char *s = NULL);

  baseMetaFile* baseClose() { return (baseMetaFile *)Close(); }
};

baseMetaFileDC::baseMetaFileDC(char *s)
    : wxMetaFileDC(s)
{
}

#else

class baseMetaFileDC : public wxObject 
{
public:
  baseMetaFileDC(char * = NULL) {
    scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		     "%s", 
		     METHODNAME("meta-file-dc%","initialization")": only supported for Windows");
  }

  baseMetaFile* baseClose() { return NULL; }
};

#endif

@CLASSBASE baseMetaFileDC "meta-file-dc":"dc"

@CLASSID wxTYPE_DC_METAFILE

@CREATOR (string=NULL);

@ "close" : baseMetaFile! baseClose()

@END

#endif
