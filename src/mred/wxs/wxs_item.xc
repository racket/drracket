
@INCLUDE prefix.xci

#include "wx_item.h"
#include "wx_messg.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxItem "item":"window"

@CLASSID wxTYPE_ITEM

@ "command" : void Command(wxCommandEvent!);

// @INCLUDE wxs_icol.xci

@ "get-label" : nstring GetLabel();
@ "set-label" : void SetLabel(string);

// @ "set-background-colour" : void SetBackgroundColour(wxColour!);
// @ "set-label-colour" : void SetLabelColour(wxColour!);
// @ "set-button-colour" : void SetButtonColour(wxColour!);

@END

@BEGINSYMBOLS messageStyle > > BUNDLE
@ENDSYMBOLS

@CLASSBASE wxMessage "message" : "item"

@CREATOR (wxPanel!,string,int=-1,int=-1,SYM[messageStyle]=0,string="message"); <> string label
@CREATOR (wxPanel!,wxBitmap!,int=-1,int=-1,SYM[messageStyle]=0,string="message"); : : /CHECKOK[1.METHODNAME("message%","initialization")] <> bitmap label

@INCLUDE wxs_item.xci

@ "set-label" : void SetLabel(wxBitmap!) : : /CHECKOK[0.METHODNAME("message%","set-label")] <> bitmap label
@ "set-label" : void SetLabel(string); <> string label

@END
