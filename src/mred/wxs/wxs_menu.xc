
@INCLUDE prefix.xci

#include "wx_menu.h"

@HEADER

@INCLUDE wxs.xci

// @CLASSBASE wxMenuItem "wx:menu-item" : "wx:object"
// @END

// wxMenu is really derived from wxItem
//  but that makes no sense. Enforce different hierarchy here
@CLASSBASE wxMenu "wx:menu" : "wx:object"

@SET CALLBACK_CLASS = wxMenu
@SET CALLBACK_CLASS_USER = "wx:menu%::initialization"
@INCLUDE cb_start.xci

@MACRO CHECKNEG[pos.result] = if (x<pos> < 0) return <result>;
@MACRO CHECKNEGVOID[pos] = $$CHECKNEG[<pos>.scheme_void]
@MACRO CHECKNEGNULL[pos] = $$CHECKNEG[<pos>.scheme_null]
@MACRO CHECKNEGFALSE[pos] = $$CHECKNEG[<pos>.scheme_false]

@CREATOR (nstring=NULL,wxFunction=NULL/bCallback/ubCallback/cCallback//spCallback); : : ubCallbackSetup///ubCallbackCreatorFinish

@ "append" : void Append(int,string,wxMenu!,nstring=NULL); : : /CHECKNEGVOID[0] <> submenu
@ "append" : void Append(int,string,nstring=NULL,bool=FALSE); : : /CHECKNEGVOID[0] <> string item
@ "delete" : void Delete(int); : : /CHECKNEGVOID[0]
@ "delete-by-position" : void DeleteByPosition(int); : : /CHECKNEGVOID[0]
@ "append-separator" : void AppendSeparator();
@ "checked?" : bool Checked(int); : : /CHECKNEGFALSE[0]
@ "check" : void Check(int,bool); : : /CHECKNEGVOID[0]
@ "enable" : void Enable(int,bool); : : /CHECKNEGVOID[0]

@ "find-item" : int FindItem(string);
// @ "find-item-for-id" : wxMenuItem^ FindItemForId(int);
@ "get-help-string" : nstring GetHelpString(int); : : /CHECKNEGNULL[0]
@ "get-label" : nstring GetLabel(int); : : /CHECKNEGNULL[0]
@ "get-title" : nstring GetTitle();

@ "set-help-string" : void SetHelpString(int, nstring); : : /CHECKNEGVOID[0]
@ "set-label" : void SetLabel(int, string); : : /CHECKNEGVOID[0]
@ "set-title" : void SetTitle(string);

@END

@INCLUDE cb_end.xci

// wxMenuBar is really derived from wxItem
//  but that makes no sense. Enforce different hierarchy here
@CLASSBASE wxMenuBar "wx:menu-bar" : "wx:object"

@SET TYPE = string
@SET NOTEST = 1
@INCLUDE list.xci

@SET TYPE = wxMenu
@SET POINTERS = 1
@INCLUDE list.xci

@MACRO CHECKSAMELENGTH = if (scheme_proper_list_length(p[0]) != scheme_proper_list_length(p[1])) scheme_signal_error("wx:menu-bar%%::initialization: list size mismatch");

@MACRO spMenuList = (listof wxMenu-object)

@CREATOR (); <> no argument
@CREATOR (-int, wxMenu*[]/bList/ubList/cList//spMenuList, string[]/bList/ubList/cList); : : CHECKSAMELENGTH/glueListSet[wxMenu.0.1.0."wx:menu-bar%::initialization"] | glueListSet[string.1.2.0."wx:menu-bar%::initialization"]// <> wx:menu% list

@ "append" : void Append(wxMenu!,string);
@ "delete" : void Delete(wxMenu!,int=0);
@ "check" : void Check(int,bool); : : /CHECKNEGVOID[0]
@ "checked?" : bool Checked(int); : : /CHECKNEGFALSE[0]
@ "enable" : void Enable(int,bool); : : /CHECKNEGVOID[0]
@ "enable-top" : void EnableTop(int,bool); : : /CHECKNEGVOID[0]

@ "find-menu-item" : int FindMenuItem(string,string);
// @ "find-item-for-id" : wxMenuItem^ FindItemForId(int);

@ "get-help-string" : nstring GetHelpString(int); : : /CHECKNEGNULL[0]
@ "get-label" : nstring GetLabel(int); : : /CHECKNEGNULL[0]
@ "get-label-top" : nstring GetLabelTop(int); : : /CHECKNEGNULL[0]
@ "get-title" : nstring GetTitle();

@ "set-help-string" : void SetHelpString(int, nstring); : : /CHECKNEGVOID[0]
@ "set-label" : void SetLabel(int, string); : : /CHECKNEGVOID[0]
@ "set-label-top" : void SetLabelTop(int, string); : : /CHECKNEGVOID[0]
// @ "set-title" : void SetTitle(string);

@END
