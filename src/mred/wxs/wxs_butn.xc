
@INCLUDE prefix.xci

#include "wx_buttn.h"

@INCLUDE wxs.xci

@HEADER

@BEGINSYMBOLS buttonStyle > > PRED BUNDLE
@SYM "border" : 1
@ENDSYMBOLS

@CLASSBASE wxButton "button":"item"

@CLASSID wxTYPE_BUTTON

@SET CALLBACK_CLASS = wxButton
@SET CALLBACK_CLASS_USER = METHODNAME("button%","initialization")
@INCLUDE cb_start.xci

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback/nopush,string,int=-1,int=-1,int=-1,int=-1,SYM[buttonStyle]=0,string="button"); : : ubCallbackSetup/NOZERO[5]|NOZERO[6]//ubCallbackCreatorFinish <> string label

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback/nopush,wxBitmap!,int=-1,int=-1,int=-1,int=-1,SYM[buttonStyle]=0,string="button"); : : ubCallbackSetup/CHECKOK[2.METHODNAME("button%","initialization")]|NOZERO[5]|NOZERO[6]//ubCallbackCreatorFinish <> bitmap label

@INCLUDE wxs_item.xci

@ "set-label" : void SetLabel(wxBitmap!) : : /CHECKOK[0.METHODNAME("button%","set-label")] <> bitmap label
@ "set-label" : void SetLabel(string); <> string label

@END

@INCLUDE cb_end.xci
