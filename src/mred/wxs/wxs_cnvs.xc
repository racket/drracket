
@INCLUDE prefix.xci

#include "wx_canvs.h"
#include "wx_panel.h"
#include "wx_frame.h"

@INCLUDE wxs.xci

@HEADER

#ifdef wx_xt
# include "wx_types.h"
# define CHECK_FOR_PANEL(x) !wxSubType((x)->__type, wxTYPE_CANVAS)
#else
# define CHECK_FOR_PANEL(x) 0
#endif

static void FillZero(int *a, int *b) {
  *a = *b = 0;
}

@BEGINSYMBOLS canvasStyle > > PRED BUNDLE
@SYM "border" : wxBORDER
@SYM "vscroll" : wxVSCROLL
@SYM "hscroll" : wxHSCROLL
@ENDSYMBOLS

@INCLUDE wxs_ornt.xci

/* Handle cases in Xt that are a problem because a wxPanel isn't really a wxCanvas */
@MACRO PANELREDIRECT[x] = if (CHECK_FOR_PANEL((wxObject *)((Scheme_Class_Object *)obj)->primdata)) { <x>; }

@CLASSBASE wxCanvas "canvas":"window"

// @CREATOR (wxFrame!,int=-1,int=-1,int=-1,int=-1,SYM[canvasStyle]=0,string="canvas") : : /NOZERO[3]|NOZERO[4]/ <> frame
@CREATOR (wxPanel!,int=-1,int=-1,int=-1,int=-1,SYM[canvasStyle]=0,string="canvas") : : /NOZERO[3]|NOZERO[4]/ <> panel

@ "allow-double-click" : void AllowDoubleClick(bool);

@SETMARK c = d
@INCLUDE wxs_cnvs.xci

@ "popup-menu" : void PopupMenu(wxMenu!, rint[0|10000], rint[0|10000]);

@ "get-dc" : wxDC! GetDC();

// @ "get-scroll-units" : void GetScrollUnitsPerPage(int*,int*); : : / PANELREDIRECT[ FillZero(x0,x1); return scheme_void]
@ "get-virtual-size" : void GetVirtualSize(int*,int*); : : / PANELREDIRECT[FillZero(x0,x1); return scheme_void]
@ "set-scrollbars" : void SetScrollbars(rint[0|10000],rint[0|10000],rint[0|10000],rint[0|10000],rint[1|10000],rint[1|10000],rint[0|10000]=0,rint[0|10000]=0,bool=TRUE);  : : / PANELREDIRECT[return scheme_void]
@ "view-start" : void ViewStart(int*,int*); : : / PANELREDIRECT[FillZero(x0,x1); return scheme_void]
@ "warp-pointer" : void WarpPointer(rint[0|10000],rint[0|10000]);  : : / PANELREDIRECT[return scheme_void]

@ "scroll" : void ScrollPercent(float,float);
@ "get-scroll-pos" : int GetScrollPos(SYM[orientation]);
@ "get-scroll-range" : int GetScrollRange(SYM[orientation]);
@ "get-scroll-page" : int GetScrollPage(SYM[orientation]);

@ "set-scroll-pos" : void SetScrollPos(SYM[orientation], rint[0|10000]);
@ "set-scroll-range" : void SetScrollRange(SYM[orientation], rint[0|10000]);
@ "set-scroll-page" : void SetScrollPage(SYM[orientation], rint[1|10000]);

@ v "on-scroll" : void OnScroll(wxScrollEvent!); : JMPDECL/SETJMP/RESETJMP : / PANELREDIRECT[return scheme_void]

@SETMARK w = d
@INCLUDE wxs_win.xci

// #define DrawsForCanvas
// @INCLUDE wxs_draw.xci

@END
