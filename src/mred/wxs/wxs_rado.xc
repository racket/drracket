
@INCLUDE prefix.xci

#include "wx_rbox.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxRadioBox "wx:radio-box":"wx:item"

@SET CALLBACK_CLASS = wxRadioBox
@SET CALLBACK_CLASS_USER = "wx:radio-box%::initialization"
@INCLUDE cb_start.xci

#include "wxs_bmap.h"

@SET TYPE = string
@SET NOTEST = 1
@INCLUDE list.xci

@SET TYPE = wxBitmap
@SET POINTERS = 1
@SET DOOKTEST = 1
@DEFINE OKTESTWHERE "wx:radio-box%::initialization"
@INCLUDE list.xci

@MACRO cStringList = (SCHEME_LISTP({x}) && (SCHEME_NULLP({x}) || SCHEME_STRINGP(SCHEME_CAR({x}))))
@MACRO cBitmapList = (SCHEME_LISTP({x}) && (SCHEME_NULLP({x}) || objscheme_istype_wxBitmap((SCHEME_CAR({x})), NULL, 0)))

@MACRO spBitmapList = (listof wxBitmap-object)

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback,nstring,int=-1,int=-1,int=-1,int=-1,-int=0,string[]=NULL/bList/ubList/cStringList,int=0,long=wxVERTICAL,string="radioBox"); : : ubCallbackSetup/NOZERO[5]|NOZERO[6]|glueListSet[string.7.8.7."wx:radio-box%::initialization"]/glueCleanup[8]/ubCallbackCreatorFinish <> string list
@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback,nstring,int,int,int,int,-int,wxBitmap*[]/bList/ubList/cBitmapList//spBitmapList,int=0,long=wxVERTICAL,string="radioBox"); : : ubCallbackSetup/NOZERO[5]|NOZERO[6]|glueListSet[wxBitmap.7.8.7."wx:radio-box%::initialization"]/glueCleanup[8]/ubCallbackCreatorFinish <> bitmap list

@INCLUDE wxs_item.xci

#define RANGECLASS wxRadioBox
@INCLUDE range.xci

@ "find-string" : int FindString(string);
@ "get-selection" : int GetSelection();
@ "get-string-selection" : nstring GetStringSelection();
@ "number" : int Number();
@ "set-string-selection" : void SetStringSelection(string);
@ "set-selection" : void SetSelection(int); : : /RANGE[0]
@ "get-string" : string GetString(int);  : : /RANGERET[0.scheme_null]

@ "enable" : void Enable(int,bool); : : /RANGE[0] <> single-button
@ "enable" : void Enable(bool); <> all-buttons

@END

@INCLUDE cb_end.xci
