
@INCLUDE prefix.xci

#include "wx_frame.h"
#include "wx_gdi.h"

@INCLUDE wxs.xci

@HEADER

#ifdef wx_xt
#define NOT_XT 0
#define HAS_GET_MENU_BAR 1
#define GET_THE_MENU_BAR(f) (f)->GetMenuBar()
#else
#define HAS_GET_MENU_BAR 0
#define NOT_XT 1
#define GET_THE_MENU_BAR(f) (f)->wx_menu_bar
#endif

#ifdef wx_motif
#define wxALLOW_AUTO_RESIZE wxPUSH_PIN
#else
#define wxALLOW_AUTO_RESIZE 0
#endif

#define NO_GET_MENU_BAR !HAS_GET_MENU_BAR

@MACRO CHECKHASMENU[log] = if (<log>GET_THE_MENU_BAR(((wxFrame *)((Scheme_Class_Object *)obj)->primdata))) return scheme_void;

@CLASSBASE wxFrame "wx:frame":"wx:window"

@CLASSID wxTYPE_FRAME

@CREATOR (wxFrame^, string, int = -1, int = -1, int = -1, int = -1, long = wxDEFAULT_FRAME, string = "frame") : : /NOZERO[4]|NOZERO[5]/

@MACRO CHECKICONOK[p] = if (x<p> && !x<p>->Ok()) return scheme_void;

@ "get-title" : string GetTitle();
@ "set-title" : void SetTitle(string);
@ "iconize" : void Iconize(bool);
@ "set-icon" : void SetIcon(wxIcon!); : : /CHECKICONOK[0]
@ "set-menu-bar" : void SetMenuBar(wxMenuBar!) : : /CHECKHASMENU[ ]
@IVAR r "menu-bar" : wxMenuBar^ wx_menu_bar ## NO_GET_MENU_BAR
@ "get-menu-bar" : wxMenuBar^ GetMenuBar() ## HAS_GET_MENU_BAR
@ "set-tool-bar" : void SetToolBar(wxToolBar^) ## USE_TOOLBAR 
@ "get-tool-bar" : wxToolBar^ GetToolBar() ## USE_TOOLBAR
@ "set-status-text" : void SetStatusText(string);
@ "iconized?" : bool Iconized();
@ "status-line-exists?" : bool StatusLineExists();
@ "maximize" : void Maximize(bool)
@ "load-accelerators" : void LoadAccelerators(string);
@ "create-status-line" : void CreateStatusLine(int = 1, string = "status_line")

@SETMARK f = d
@INCLUDE wxs_fram.xci

@ v "command" : void Command(int);
@ v "on-menu-command" : void OnMenuCommand(int)
@ v "on-menu-select" : void OnMenuSelect(int) : : CHECKHASMENU[!]

@SETMARK w = d
@INCLUDE wxs_win.xci

@MACRO TRUE = return TRUE;

@CONSTANT "wx:const-iconize" : long wxICONIZE
@CONSTANT "wx:const-minimize" : long wxMINIMIZE
@CONSTANT "wx:const-maximize" : long wxMAXIMIZE
@CONSTANT "wx:const-sdi" : long wxSDI
@CONSTANT "wx:const-mdi-parent" : long wxMDI_PARENT
@CONSTANT "wx:const-mdi-child" : long wxMDI_CHILD
@CONSTANT "wx:const-thick-frame" : long wxTHICK_FRAME
@CONSTANT "wx:const-system-menu" : long wxSYSTEM_MENU
@CONSTANT "wx:const-minimize-box" : long wxMINIMIZE_BOX
@CONSTANT "wx:const-maximize-box" : long wxMAXIMIZE_BOX
@CONSTANT "wx:const-resize-border" : long wxRESIZE_BORDER
@CONSTANT "wx:const-default-frame" : long wxDEFAULT_FRAME
@CONSTANT "wx:const-allow-auto-resize" : long wxALLOW_AUTO_RESIZE

@CONSTANT "wx:const-both" : int wxBOTH
@CONSTANT "wx:const-horizontal" : int wxHORIZONTAL
@CONSTANT "wx:const-vertical" : int wxVERTICAL

@END
