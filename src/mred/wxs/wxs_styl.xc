
@INCLUDE prefix.xci

#include "wx_style.h"
#include "wx_mtype.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxMultColour "wx:mult-colour" : "wx:object"

@IVAR "r" : float r
@IVAR "g" : float g
@IVAR "b" : float b

@ "get" : void Get(float*,float*,float*);
@ "set" : void Set(float,float,float);

@END

@CLASSBASE wxAddColour "wx:add-colour" : "wx:object"

@IVAR "r" : short r
@IVAR "g" : short g
@IVAR "b" : short b

@ "get" : void Get(short*,short*,short*);
@ "set" : void Set(short,short,short);

@END


@CLASSBASE wxStyleDelta "wx:style-delta" : "wx:object"

@IVAR "family" : int family
@IVAR "face" : nstring face
@IVAR "size-mult" : float sizeMult
@IVAR "size-add" : int sizeAdd
@IVAR "weight-on" : int weightOn
@IVAR "weight-off" : int weightOff
@IVAR "style-on" : int styleOn
@IVAR "style-off" : int styleOff
@IVAR "underlined-on" : bool underlinedOn
@IVAR "underlined-off" : bool underlinedOff
@IVAR "transparent-text-backing-on" : bool transparentTextBackingOn
@IVAR "transparent-text-backing-off" : bool transparentTextBackingOff
@IVAR r "foreground-mult" : wxMultColour% foregroundMult
@IVAR r "background-mult" : wxMultColour% backgroundMult
@IVAR r "foreground-add" : wxAddColour% foregroundAdd
@IVAR r "background-add" : wxAddColour% backgroundAdd
@IVAR "alignment-on" : int alignmentOn
@IVAR "alignment-off" : int alignmentOff
  
@CREATOR (int=wxCHANGE_NOTHING,int=0);

@CLASSID wxTYPE_STYLE_DELTA

@ "set-delta" : wxStyleDelta! SetDelta(int,int=0);
@ "set-delta-face" : wxStyleDelta! SetDeltaFace(string);
@ "set-delta-background" : wxStyleDelta! SetDeltaBackground(string); <> color name
@ "set-delta-background" : wxStyleDelta! SetDeltaBackground(wxColour%); <> wx:colour%
@ "set-delta-foreground" : wxStyleDelta! SetDeltaForeground(string); <> color name
@ "set-delta-foreground" : wxStyleDelta! SetDeltaForeground(wxColour%); <> wx:colour%

@ "equal?" : bool Equal(wxStyleDelta%);
@ "collapse" : bool Collapse(wxStyleDelta%);
@ "copy" : void Copy(wxStyleDelta!);

@CONSTANT "wx:const-base" : int wxBASE

@CONSTANT "wx:const-change-nothing" : int wxCHANGE_NOTHING
@CONSTANT "wx:const-change-normal" : int wxCHANGE_NORMAL
@CONSTANT "wx:const-change-style" : int wxCHANGE_STYLE
@CONSTANT "wx:const-change-weight" : int wxCHANGE_WEIGHT
@CONSTANT "wx:const-change-underline" : int wxCHANGE_UNDERLINE
@CONSTANT "wx:const-change-size" : int wxCHANGE_SIZE
@CONSTANT "wx:const-change-bold" : int wxCHANGE_BOLD
@CONSTANT "wx:const-change-italic" : int wxCHANGE_ITALIC
@CONSTANT "wx:const-change-toggle-style" : int wxCHANGE_TOGGLE_STYLE
@CONSTANT "wx:const-change-toggle-weight" : int wxCHANGE_TOGGLE_WEIGHT
@CONSTANT "wx:const-change-toggle-underline" : int wxCHANGE_TOGGLE_UNDERLINE
@CONSTANT "wx:const-change-bigger" : int wxCHANGE_BIGGER
@CONSTANT "wx:const-change-smaller" : int wxCHANGE_SMALLER
@CONSTANT "wx:const-change-family" : int wxCHANGE_FAMILY
@CONSTANT "wx:const-change-alignment" : int wxCHANGE_ALIGNMENT
@CONSTANT "wx:const-change-normal-colour" : int wxCHANGE_NORMAL_COLOUR

@CONSTANT "wx:const-align-top" : int wxALIGN_TOP
@CONSTANT "wx:const-align-bottom" : int wxALIGN_BOTTOM
@CONSTANT "wx:const-align-center" : int wxALIGN_CENTER

@END


@CLASSBASE wxStyle "wx:style" : "wx:object"

@CLASSID wxTYPE_STYLE

@ "get-name" : string GetName();
@ "get-family" : int GetFamily();
@ "get-face" : nstring GetFace();
@ "get-size" : int GetSize();
@ "get-weight" : int GetWeight();
@ "get-style" : int GetStyle();
@ "get-underlined" : bool GetUnderlined();
@ "get-font" : wxFont! GetFont();
@ "get-foreground" : wxColour% GetForeground();
@ "get-colour" : wxColour% GetBackground();
@ "get-alignment" : int GetAlignment();
@ "get-transparent-text-backing" : bool GetTransparentTextBacking();

@ "get-text-height" : float GetTextHeight(wxDC!);
@ "get-text-descent" : float GetTextDescent(wxDC!);
@ "get-text-space" : float GetTextSpace(wxDC!);
@ "get-text-width" : float GetTextWidth(wxDC!);

@ "get-base-style" : wxStyle! GetBaseStyle();
@ "set-base-style" : void SetBaseStyle(wxStyle!);

@ "get-delta" : void GetDelta(wxStyleDelta%);
@ "set-delta" : void SetDelta(wxStyleDelta%);

@ "is-join?" : bool IsJoin();

@ "get-shift-style" :  wxStyle! GetShiftStyle();
@ "set-shift-style" : void SetShiftStyle(wxStyle!);

@ "switch-to" : void SwitchTo(wxDC!, wxStyle!); : : /CHECKOK[0."wx:style%::switch-to"]

@END


@CLASSBASE wxStyleList "wx:style-list" : "wx:object"

@CREATOR ();

@CLASSID wxTYPE_STYLE_LIST

@ "clear" : void Clear();
@ "copy" : void Copy(wxStyleList!);

@ "basic-style" : wxStyle! BasicStyle();

@ "number" : int Number();

@ "find-or-create-style" : wxStyle! FindOrCreateStyle(wxStyle^,wxStyleDelta!);
@ "find-or-create-join-style" : wxStyle! FindOrCreateJoinStyle(wxStyle^,wxStyle!);
@ "find-named-style" : wxStyle! FindNamedStyle(string);
@ "new-named-style" : wxStyle! NewNamedStyle(string,wxStyle^);
@ "replace-named-style" : wxStyle! ReplaceNamedStyle(string,wxStyle^);

@ "convert" : wxStyle! Convert(wxStyle!);

// @ "notify-on-change" : long NotifyOnChange(wxStyleNotifyFunc f, void *data);
// @ "forget-notification" : void ForgetNotification(long);

@ "index-to-style" : wxStyle^ IndexToStyle(int);
@ "style-to-index" : int StyleToIndex(wxStyle!);

@ "adjust-usage" : void AdjustUsage(bool);
@ "is-used? " : bool IsUsed();

@CONSTANT "wx:the-style-list" : wxStyleList! wxTheStyleList

@END

