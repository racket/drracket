
@INCLUDE prefix.xci 

#include "wx_win.h"
#include "wx_gdi.h"

@INCLUDE wxs.xci

@HEADER

#if 0
#ifndef wx_mac
#define HAS_EDIT_MODE 1
#else
#define HAS_EDIT_MODE 0
#endif
#else
#define HAS_EDIT_MODE 0
#endif

#ifdef wx_mac
#define Move(x, y) SetSize(-1, -1, x, y)
#endif

#if defined(wx_xt)
#define wxPOS_USE_MINUS_ONE 0
#endif

static int wxSchemeWindowGetWidth(wxWindow *w)
{
  int x, y;

  w->GetSize(&x, &y);
  
  return x;
}

static int wxSchemeWindowGetHeight(wxWindow *w)
{
  int x, y;

  w->GetSize(&x, &y);
  
  return y;
}

static int wxSchemeWindowGetX(wxWindow *w)
{
  int x, y;

  w->GetPosition(&x, &y);
  
  return x;
}

static int wxSchemeWindowGetY(wxWindow *w)
{
  int x, y;

  w->GetPosition(&x, &y);
  
  return y;
}

@CLASSBASE wxWindow "wx:window":"wx:object"

// @CREATOR ();

@MACRO CHECKCURSOROK[p] = if (x<p> && !x<p>->Ok()) x<p> = wxSTANDARD_CURSOR;

@ "centre" : void Centre(int = wxBOTH);

// @ "make-modal" : void MakeModal(bool);
@ "set-focus" : void SetFocus();
@ "set-size" : void SetSize(int,int,int,int,int=wxSIZE_AUTO); : : /NOZERO[2]|NOZERO[3]
@ "move" : void Move(int,int); 
// @ "set-client-size" : void SetClientSize(int,int); : : /NOZERO[0]|NOZERO[1]
// @ "set-colour-map" : void SetColourMap(wxColourMap!);
@ "set-cursor" : wxCursor^ SetCursor(wxCursor!); : : /CHECKCURSOROK[0]
@ "show" : void Show(bool);
@ "is-shown?" : bool IsShown();
@ "fit" : void Fit();
@ "get-size" : void GetSize(int*,int*);
@ "get-client-size" : void GetClientSize(int*,int*);
@ "get-position" : void GetPosition(int*,int*);
@ "enable" : void Enable(bool);

@ "get-name" : nstring GetName();

@ "get-char-height" : float GetCharHeight();
@ "get-char-width" : float GetCharWidth();
@ "client-to-screen" : void ClientToScreen(int*, int*);
// Destruction not allowed in wxScheme:
// @ "destroy-children" : void DestroyChildren();
// @ "drag-accept-files" : void DragAcceptFiles(bool);
// @ "add-child" : void AddChild(wxWindow!);
// @ "capture-mouse" : void CaptureMouse();
@ "screen-to-client" : void ScreenToClient(int*,int*);
@ "refresh" : void Refresh();
// @ "release-mouse" : void ReleaseMouse();
@ "get-parent" : wxWindow^ GetParent();
@ "get-text-extent" : void GetTextExtent(string,float*,float*,float?=NULL,float?=NULL,wxFont^=NULL,bool=FALSE);
// @ "get-children" : wxList! GetChildren();
@ "get-grand-parent" : wxWindow^ GetGrandParent();
@ "get-label" : string GetLabel();
@ "center" : void Center(int);

#if 0
@ "set-constraints" : void SetConstraints(wxLayoutConstraints^); ## USE_CONSTRAINTS
@ "get-constraints" : wxLayoutConstraints^ GetConstraints();  ## USE_CONSTRAINTS
@ "layout" : void Layout();  ## USE_CONSTRAINTS

@ "get-user-edit-mode" : bool GetUserEditMode(); ## HAS_EDIT_MODE
@ "set-user-edit-mode" : void SetUserEditMode(bool); ## HAS_EDIT_MODE
#endif

@ m "get-height" : int wxSchemeWindowGetHeight();
@ m "get-width" : int wxSchemeWindowGetWidth();
@ m "get-x" : int wxSchemeWindowGetX();
@ m "get-y" : int wxSchemeWindowGetY();

@CONSTANT "wx:const-size-auto" : int wxSIZE_AUTO
@CONSTANT "wx:const-size-auto-width" : int wxSIZE_AUTO_WIDTH
@CONSTANT "wx:const-size-auto-height" : int wxSIZE_AUTO_HEIGHT
@CONSTANT "wx:const-size-use-exsiting" : int wxSIZE_USE_EXISTING
@CONSTANT "wx:const-pos-use-minus-one" : int wxPOS_USE_MINUS_ONE


@SETMARK w = V
@INCLUDE wxs_win.xci

@END
