
@INCLUDE prefix.xci

#include "wx_buttn.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxButton "wx:button":"wx:item"

@CLASSID wxTYPE_BUTTON

@SET CALLBACK_CLASS = wxButton
@SET CALLBACK_CLASS_USER = "wx:button%::initialization"
@INCLUDE cb_start.xci

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback,string,int=-1,int=-1,int=-1,int=-1,long=0,string="button"); : : ubCallbackSetup/NOZERO[5]|NOZERO[6]//ubCallbackCreatorFinish <> string label

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback,wxBitmap!,int=-1,int=-1,int=-1,int=-1,long=0,string="button"); : : ubCallbackSetup/CHECKOK[2."wx:button%::initialization"]|NOZERO[5]|NOZERO[6]//ubCallbackCreatorFinish <> bitmap label

@INCLUDE wxs_item.xci

@ "set-default" : void SetDefault();
@ "set-label" : void SetLabel(wxBitmap!) : : /CHECKOK[0."wx:button%::set-label"] <> bitmap label
@ "set-label" : void SetLabel(string); <> string label

@END

@INCLUDE cb_end.xci
