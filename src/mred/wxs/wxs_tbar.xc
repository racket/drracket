
#if 0

#include "wx_tbar.h"
#include "wx_bbar.h"

@INCLUDE wxs.xci

@HEADER

#if USE_TOOLBAR 

@CLASSBASE wxToolBarTool "wx:tool-bar-tool" : "wx:canvas"

@END

@CLASSBASE wxToolBar "wx:tool-bar" : "wx:canvas"

@CREATOR (wxFrame!,int=0,int=0,int=-1,int=-1,long=0,int=wxVERTICAL,int=2);

@CLASSID wxTYPE_TOOLBAR

@ "add-tool" : wxToolBarTool! AddTool(int,wxBitmap!,wxBitmap^=NULL,bool=FALSE,float=-1,float=-1,wxObject^=NULL);

@ "enable-tool" : void EnableTool(int,bool);

@ "find-tool-for-position" : wxToolBarTool^ FindToolForPosition(float,float);

@ "get-max-size" : void GetMaxSize(float*,float*);

@ "get-tool-client-data" : wxObject^ GetToolClientData(int);

@ "get-tool-enabled?" : bool GetToolEnabled(int);
@ "get-tool-state?" : bool GetToolState(int);

@ "set-margins" : void SetMargins(float,float);

@ "toggle-tool" : void ToggleTool(int,bool);

@END

#if USE_BUTTONBAR && defined(wx_msw)

@CLASSBASE wxButtonBar "wx:button-bar" : "wx:tool-bar"

@CLASSID wxTYPE_BUTTONBAR

@CREATOR (wxFrame!,int=0,int=0,int=-1,int=-1,long=0,int=wxVERTICAL,int=2);

@ "get-default-button-height" : float GetDefaultButtonHeight();
@ "get-default-button-width" : float GetDefaultButtonWidth();
@ "set-default-size" : void SetDefaultSize(float,float);

@END

#endif
#endif

#endif
