
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

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxDC "wx:dc":"wx:object"

@CLASSID wxTYPE_DC

@SETMARK Q = U
// "U" for undefined (pure virtual); don't think we'll need it
// @SETMARK q = v
@INCLUDE wxs_draw.xci

// Also in wxWindow:
@ Q "get-text-extent" : void GetTextExtent(string,float*,float*,float?=NULL,float?=NULL,wxFont^=NULL,bool=FALSE); : : /CheckOk
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

@ "set-optimization" : void SetOptimization(bool); : : /IFNOTMAC/ENDIF/

#ifndef wx_mac
#define CHECKTHISONE(x) x
#else
#define CHECKTHISONE(x) 1
#endif

@MACRO CheckIcon[p] = if (!CHECKTHISONE(x<p>->Ok())) return scheme_void;

@ Q "draw-icon" : void DrawIcon(wxIcon!,float,float); : : /CheckOk|CheckIcon[0]

@ Q "blit" : bool Blit(float,float,float,float,wxCanvasDC!,float,float,int=wxCOPY); : : /CheckLogicalFunc[7.wxCOLOR."wx:dc::blit"]|CheckOkFalse|CheckFalse[4] : : rFALSE

@ Q "try-colour" : void TryColour(wxColour!,wxColour!);

@ Q "set-map-mode" : void SetMapMode(int); : : /CheckOk
@ Q "set-background-mode" : void SetBackgroundMode(int); :  : /CheckOk
@ q "set-colour-map" : void SetColourMap(wxColourMap!); : : /CheckOk
@ Q "set-user-scale" : void SetUserScale(nnfloat,nnfloat); : : /CheckOk
@ Q "set-device-origin" : void SetDeviceOrigin(float,float); : : /CheckOk

@ q "get-background" : wxBrush! GetBackground();
@ q "get-brush" : wxBrush! GetBrush();
@ q "get-font" : wxFont! GetFont();
@ q "get-logical-function" : int GetLogicalFunction();
@ q "get-map-mode" : int GetMapMode();
@ q "get-pen" : wxPen! GetPen();
@ q "get-text-background" : wxColour% GetTextBackground();
@ q "get-text-foreground" : wxColour% GetTextForeground();

@ q "get-size" : void GetSize(float*,float*);

// @ Q "device-to-logical-x" : float DeviceToLogicalX(int); : : : rZERO
// @ Q "device-to-logical-y" : float DeviceToLogicalY(int); : : : rZERO
// @ Q "logical-to-device-x" : float LogicalToDeviceX(int); : : : rZERO
// @ Q "logical-to-device-y" : float LogicalToDeviceY(int); : : : rZERO

@ q "max-x" : float MaxX();
@ q "max-y" : float MaxY();
@ q "min-x" : float MinX();
@ q "min-y" : float MinY();

@ q "ok?" : bool Ok();

@ Q "start-doc" : bool StartDoc(string); : : : rFALSE
@ Q "start-page" : void StartPage();
@ Q "end-doc" : void EndDoc();
@ Q "end-page" : void EndPage();

@CONSTANT "wx:const-oddeven-rule" : int wxODDEVEN_RULE
@CONSTANT "wx:const-winding-rule" : int wxWINDING_RULE

@CONSTANT "wx:const-and" : int wxAND
@CONSTANT "wx:const-and-invert" : int wxAND_INVERT
@CONSTANT "wx:const-and-reverse" : int wxAND_REVERSE
@CONSTANT "wx:const-clear" : int wxCLEAR
@CONSTANT "wx:const-copy" : int wxCOPY
@CONSTANT "wx:const-equiv" : int wxEQUIV
@CONSTANT "wx:const-invert" : int wxINVERT
@CONSTANT "wx:const-nand" : int wxNAND
@CONSTANT "wx:const-nor" : int wxNOR
@CONSTANT "wx:const-no-op" : int wxNO_OP
@CONSTANT "wx:const-or" : int wxOR
@CONSTANT "wx:const-or-invert" : int wxOR_INVERT
@CONSTANT "wx:const-or-reverse" : int wxOR_REVERSE
@CONSTANT "wx:const-set" : int wxSET
@CONSTANT "wx:const-src-invert" : int wxSRC_INVERT
@CONSTANT "wx:const-xor" : int wxXOR
@CONSTANT "wx:const-colour" : int wxCOLOR

@CONSTANT "wx:const-mm-twips" : int MM_TWIPS
@CONSTANT "wx:const-mm-points" : int MM_POINTS
@CONSTANT "wx:const-mm-metric" : int MM_METRIC
@CONSTANT "wx:const-mm-lometric" : int MM_LOMETRIC
@CONSTANT "wx:const-mm-text" : int MM_TEXT

@END

@CLASSBASE wxCanvasDC "wx:canvas-dc":"wx:dc"

@CLASSID wxTYPE_DC_CANVAS

@CREATOR ();

// @SETMARK Q = d
// @SETMARK q = d
// @INCLUDE wxs_drwf.xci

@ "get-pixel" : bool GetPixel(float,float,wxColour^)

@ "begin-set-pixel" : void BeginSetPixel()
@ "end-set-pixel" : void EndSetPixel()
@ "set-pixel" : void SetPixel(float,float,wxColour^)

@END


@CLASSBASE wxMemoryDC "wx:memory-dc":"wx:canvas-dc"

@CLASSID wxTYPE_DC_MEMORY

@CREATOR (); <> no argument
@CREATOR (wxCanvasDC!); <> wx:canvas-dc%

@ "select-object" : void SelectObject(wxBitmap^);

@END

@CLASSBASE wxPostScriptDC "wx:post-script-dc":"wx:dc"

@CLASSID wxTYPE_DC_POSTSCRIPT

@INCLUDE wxs_dorf.xci

@CREATOR (npathname,bool=TRUE,wxWindow^=NULL); : : /DLGORFRAME[2."wx:post-script-dc::initialization"]

// @SETMARK Q = d
// @SETMARK q = d
// @INCLUDE wxs_drwf.xci

@END

#ifdef wx_x

class basePrinterDC : public wxObject
{
public:
  basePrinterDC(char *, char *, char *, Bool = TRUE)
  {
    scheme_signal_error("wx:printer-dc%::initialization: not supported for X Windows");
  }
};

#else

class basePrinterDC : public wxPrinterDC
{
public:
  basePrinterDC(char *a, char *b, char *c, Bool d = TRUE)
    : wxPrinterDC(
#ifndef wx_mac
		  a, b, c, d
#endif
		  )
  {
  }
};

#endif

@CLASSBASE basePrinterDC "wx:printer-dc":"wx:dc"

@CLASSID wxTYPE_DC_PRINTER

// @SETMARK Q = d
// @SETMARK q = d
// @INCLUDE wxs_drwf.xci

@CREATOR (nstring,nstring,nstring,bool=TRUE);

@END


#ifdef wx_msw

class baseMetaFileDC : public wxMetaFileDC {
public:
  baseMetaFileDC(char *s = NULL);
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
    scheme_signal_error("wx:meta-file-dc%::initialization: only supported for Windows");
  }
};

#endif

@CLASSBASE baseMetaFileDC "wx:meta-file-dc":"wx:dc"

@CLASSID wxTYPE_DC_METAFILE

@CREATOR (string=NULL);

// @SETMARK Q = d
// @SETMARK q = d
// @INCLUDE wxs_drwf.xci

@END

