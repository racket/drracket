
@INCLUDE prefix.xci

#include "wx_panel.h"
#include "wx_enhdg.h"
#include "wx_dialg.h"
#include "wx_types.h"

@INCLUDE wxs.xci

@HEADER

#if !defined(wx_mac)
#define INTERACT_METHODS 1
#else
#define INTERACT_METHODS 0
#endif

/* The derivation wx:panel -> wx:canvas is a lie for Xt */
@CLASSBASE wxPanel "wx:panel":"wx:canvas"

@CLASSID wxTYPE_PANEL

@CREATOR (wxFrame!,int=-1,int=-1,int=-1,int=-1,long=0,string="panel") : : /NOZERO[3]|NOZERO[4] <> frame
@CREATOR (wxPanel!,int=-1,int=-1,int=-1,int=-1,long=0,string="panel") : : /NOZERO[3]|NOZERO[4] <> panel parent

@ "fit" : void Fit();
@ "get-default-item" : wxButton! GetDefaultItem();
@ "get-item-cursor" : void GetCursor(int*,int*);
@ "set-item-cursor" : void SetItemCursor(int,int);

@SETMARK p = v
@INCLUDE wxs_panl.xci

@INCLUDE wxs_cnvs.xci

@ "set-label-position" : void SetLabelPosition(int);
@ "get-label-position" : int GetLabelPosition();

@ "get-horizontal-spacing" : int GetHorizontalSpacing();
@ "get-vertical-spacing" : int GetVerticalSpacing();
@ "set-horizontal-spacing" : void SetHorizontalSpacing(int);
@ "set-vertical-spacing" : void SetVerticalSpacing(int);

@ "new-line" : void NewLine(); <> no argument
@ "new-line" : void NewLine(int); <> tab amount

@ "get-panel-dc" : wxDC! GetDC();

@INCLUDE wxs_ifnt.xci
@INCLUDE wxs_icol.xci

@ "tab" : void Tab(); <> no argument
@ "tab" : void Tab(int); <> tab amount

// @ "advance-cursor" : void AdvanceCursor(wxWindow!); ## !defined(wx_xt)
// @ "get-child" : wxObject! GetChild(int); ## !defined(wx_xt)

// @CONSTANT "wx:const-multiple-mask" : long wxMULTIPLE_MASK ## !defined(wx_xt)
@CONSTANT "wx:const-single" : long wxSINGLE       
@CONSTANT "wx:const-multiple" : long wxMULTIPLE     
@CONSTANT "wx:const-extended" : long wxEXTENDED     
// @CONSTANT "wx:const-sb-mask" : long wxSB_MASK ## !defined(wx_xt)
@CONSTANT "wx:const-needed-sb" : long wxNEEDED_SB    
@CONSTANT "wx:const-always-sb" : long wxALWAYS_SB    
@CONSTANT "wx:const-process-enter" : long wxPROCESS_ENTER
@CONSTANT "wx:const-password" : long wxPASSWORD
@CONSTANT "wx:const-vscroll" : long wxVSCROLL      
@CONSTANT "wx:const-hscroll" : long wxHSCROLL      
@CONSTANT "wx:const-caption" : long wxCAPTION      
// @CONSTANT "wx:const-editable" : long wxEDITABLE ## !defined(wx_xt)
@CONSTANT "wx:const-readonly" : long wxREADONLY      

@END

@CLASSBASE wxDialogBox "wx:dialog-box" : "wx:panel"

@CLASSID wxTYPE_DIALOG_BOX

@INCLUDE wxs_dorf.xci

@CREATOR (wxWindow^,nstring,bool=FALSE,int=300,int=300,int=500,int=500,long=wxDEFAULT_DIALOG_STYLE,string="dialogBox"); : : /DLGORFRAME[0."wx:dialog-box%::initialization"]|NOZERO[5]|NOZERO[6]

@CONSTANT "wx:const-default-dialog-style" : long wxDEFAULT_DIALOG_STYLE

// @ "centre" : void Centre(int=wxBOTH);

@SETMARK f = d
@INCLUDE wxs_fram.xci

@SETMARK p = d
@INCLUDE wxs_panl.xci

@END

#if 0
#if USE_ENHANCED_DIALOG

@CLASSBASE wxEnhDialogBox "wx:enh-dialog-box" : "wx:dialog-box"

@SET CALLBACK_CLASS = wxEnhDialogBox
@INCLUDE cb_start.xci

@MACRO rTRUE = return TRUE;

@CREATOR (wxFrame^,string,bool=FALSE,wxFunction=NULL/bCallback/ubCallback/cCallback,int=-1,int=0,int=0,int=10,int=10,long=wxDEFAULT_DIALOG_STYLE,string="Shell"); : : ubCallbackSetup///ubCallbackCreatorFinish

@IVAR "user-panel" : wxPanel! userPanel

@ "set-status" : void SetStatus(string=NULL);
@ "add-cmd" : wxButton! AddCmd(string,wxFunction/bCallback/ubCallback/cCallback,int=0); : : ubCallbackSetup///ubCallbackFinish
@ "add-cmd" : wxButton! AddCmd(wxBitmap!,wxFunction/bCallback/ubCallback/cCallback,int=0); : : ubCallbackSetup///ubCallbackFinish
	
@ "get-cmd" : wxButton^ GetCmd(int);
@ "set-pin" : void SetPin(bool);

@ v "on-close" : bool OnClose(); : : : rTRUE

@SETMARK p = d
@INCLUDE wxs_panl.xci

@END

@INCLUDE cb_end.xci

#endif
#endif
