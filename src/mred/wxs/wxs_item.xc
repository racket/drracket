
@INCLUDE prefix.xci

#include "wx_item.h"
#include "wx_messg.h"
#include "wx_group.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxItem "wx:item":"wx:window"

@CLASSID wxTYPE_ITEM

// @ "centre" : void Centre(int=wxHORIZONTAL);
@ "command" : void Command(wxCommandEvent%);

@INCLUDE wxs_icol.xci

@ "get-label" : nstring GetLabel();
@ "set-label" : void SetLabel(string);

@ "get-char-height" : float GetCharHeight();
@ "get-char-width" : float GetCharWidth();

@ "set-background-colour" : void SetBackgroundColour(wxColour!);
@ "set-label-colour" : void SetLabelColour(wxColour!);
@ "set-button-colour" : void SetButtonColour(wxColour!);

@END


@CLASSBASE wxMessage "wx:message" : "wx:item"

@CREATOR (wxPanel!,string,int=-1,int=-1,long=0,string="message"); <> string label
@CREATOR (wxPanel!,wxBitmap!,int=-1,int=-1,long=0,string="message"); : : /CHECKOK[1."wx:message::initialization"] <> bitmap label

@INCLUDE wxs_item.xci

@ "set-label" : void SetLabel(wxBitmap!) : : /CHECKOK[0."wx:button%::set-label"] <> bitmap label
@ "set-label" : void SetLabel(string); <> string label

@END

#if 0

@CLASSBASE wxGroupBox "wx:group-box":"wx:item"

@CLASSID wxTYPE_GROUP_BOX

@CREATOR (wxPanel!,string,int=-1,int=-1,int=-1,int=-1,long=0,string="groupBox");

@END

#endif
