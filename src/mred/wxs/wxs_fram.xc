
@INCLUDE prefix.xci

#include "wx_frame.h"
#include "wx_gdi.h"

@INCLUDE wxs.xci

@HEADER

#ifdef wx_xt
#define HAS_GET_MENU_BAR 1
#define GET_THE_MENU_BAR(f) (f)->GetMenuBar()
#else
#define HAS_GET_MENU_BAR 0
#define GET_THE_MENU_BAR(f) (f)->wx_menu_bar
#endif

#ifdef wx_motif
#define wxALLOW_AUTO_RESIZE wxPUSH_PIN
#else
#define wxALLOW_AUTO_RESIZE 0
#endif

#define NO_GET_MENU_BAR !HAS_GET_MENU_BAR

@MACRO CHECKHASMENU[log] = if (<log>GET_THE_MENU_BAR(((wxFrame *)((Scheme_Class_Object *)obj)->primdata))) return scheme_void;

@INCLUDE wxs_espc.xci

@BEGINSYMBOLS frameStyle > > PRED BUNDLE
@SYM "no-caption" : wxNO_CAPTION
@SYM "mdi-parent" : wxMDI_PARENT
@SYM "mdi-child" : wxMDI_CHILD
@SYM "no-system-menu" : wxNO_SYSTEM_MENU
@SYM "no-resize-border" : wxNO_RESIZE_BORDER
@ENDSYMBOLS

@BEGINSYMBOLS iconKind > ONE > PRED BUNDLE
@SYM "both" : 0
@SYM "small" : 1
@SYM "large" : 2
@ENDSYMBOLS

#ifdef wx_msw
# define XTMAC_UNUSED(x) x
#else
# define XTMAC_UNUSED(x) /**/
#endif

static void frameMenu(wxFrame *XTMAC_UNUSED(f))
{
#ifdef wx_msw
  f->SystemMenu();
#endif
}

@CLASSBASE wxFrame "frame":"window"

@CLASSID wxTYPE_FRAME

@CREATOR (wxFrame^, string, int = -1, int = -1, int = -1, int = -1, SYM[frameStyle]=0, string = "frame") : : /CHECKEVENTSPACE[METHODNAME("frame%","initialization")]|NOZERO[4]|NOZERO[5]/

@MACRO CHECKICONOK[p] = if (x<p> && !x<p>->Ok()) scheme_arg_mismatch(METHODNAME("frame%","set-icon"), "bad bitmap: ", p[<p>]);
@MACRO CHECKICONBW[p] = if (x<p> && (x<p>->GetDepth() != 1)) scheme_arg_mismatch(METHODNAME("frame%","set-icon"), "mask bitmap is not monochrome: ", p[<p>]);

@ "set-title" : void SetTitle(string);
@ "iconize" : void Iconize(bool);
@ "set-icon" : void SetIcon(wxBitmap!,wxBitmap^ = NULL,SYM[iconKind] = 0); : : /CHECKICONOK[0]|CHECKICONOK[1]|CHECKICONBW[1]
@ "set-menu-bar" : void SetMenuBar(wxMenuBar!) : : /CHECKHASMENU[ ]
@IVAR r "menu-bar" : wxMenuBar^ wx_menu_bar ## NO_GET_MENU_BAR
@ "get-menu-bar" : wxMenuBar^ GetMenuBar() ## HAS_GET_MENU_BAR
@ "set-status-text" : void SetStatusText(string)
@ "iconized?" : bool Iconized();
@ "status-line-exists?" : bool StatusLineExists();
@ "maximize" : void Maximize(bool)
@ "create-status-line" : void CreateStatusLine(int = 1, string = "status_line")

@ m "system-menu" : void frameMenu();

@SETMARK f = d
@INCLUDE wxs_fram.xci

@ v "on-menu-command" : void OnMenuCommand(ExactLong)
@ v "on-menu-click" : void OnMenuClick()

@SETMARK w = d
@INCLUDE wxs_win.xci

@END
