
@INCLUDE prefix.xci

#include "wx_check.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxCheckBox "wx:check-box":"wx:item"

@SET CALLBACK_CLASS = wxCheckBox
@SET CALLBACK_CLASS_USER = "wx:check-box%::initialization"
@INCLUDE cb_start.xci

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback,string,int=-1,int=-1,int=-1,int=-1,long=0,string="checkBox"); : : ubCallbackSetup/NOZERO[5]|NOZERO[6]//ubCallbackCreatorFinish <> string label
@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback,wxBitmap!,int=-1,int=-1,int=-1,int=-1,long=0,string="checkBox"); : : ubCallbackSetup/CHECKOK[2."wx:check-box%::initialization"]|NOZERO[5]|NOZERO[6]//ubCallbackCreatorFinish <> bitmap label

@INCLUDE wxs_item.xci

@ "get-value" : bool GetValue();
@ "set-value" : void SetValue(bool);
@ "set-label" : void SetLabel(wxBitmap!); : : /CHECKOK[0."wx:check-box%::set-label"] <> bitmap label
@ "set-label" : void SetLabel(string); <> string label

@END

@INCLUDE cb_end.xci
