
@INCLUDE prefix.xci

#include "wx_lbox.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxListBox "wx:list-box":"wx:item"

@CLASSID wxTYPE_LIST_BOX

@SET CALLBACK_CLASS = wxListBox
@SET CALLBACK_CLASS_USER = "wx:list-box%::initialization"
@INCLUDE cb_start.xci

@SET TYPE = string
@SET NOTEST = 1
@INCLUDE list.xci

@INCLUDE qbool.xci

@MACRO CHECKKIND[n] = if (((x<n> & wxMULTIPLE) && (x<n> & wxEXTENDED)) || (x<n> & ~((Bool)(wxMULTIPLE | wxEXTENDED | wxALWAYS_SB)))) scheme_signal_error("wx:list-box%%::initialization: bad style specification: %d", x<n>);

@MACRO spNum = num

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback,nstring,Bool=wxSINGLE/bQBool/ubQBool/cQBool//spNum,int=-1,int=-1,int=-1,int=-1,-int=0,string[]=NULL/bList/ubList/cList,long=0,string="button"); : : ubCallbackSetup/NOZERO[6]|NOZERO[7]|CHECKKIND[3]|glueListSet[string.8.9.8."wx:list-box%::initialization"]/glueCleanup[9]/ubCallbackCreatorFinish

@INCLUDE wxs_item.xci

#define RANGECLASS wxListBox
@INCLUDE range.xci

@MACRO bAnythingFromString = (({x}) ? ((Scheme_Object *){x}) : scheme_null)
@MACRO ubAnythingToString = ((char *){x})
@MACRO cAnything = 1

@ "append" : void Append(string); <> without data
@ "append" : void Append(string, string//ubAnythingToString/cAnything); <> with data
@ "clear" : void Clear();
@ "delete" : void Delete(int); : : /RANGE[0]
@ "deselect" : void Deselect(int); : : /RANGE[0]
@ "set-selection" : void SetSelection(int,bool=TRUE); : : /RANGE[0]
@ "selected?" : bool Selected(int); : : /RANGE[0]
@ "get-string-selection" : string GetStringSelection();
@ "get-client-data" : string/bAnythingFromString GetClientData(int); : : /RANGERET[0.scheme_null]
@ "set-client-data" : void SetClientData(int, string//ubAnythingToString/cAnything); : : /RANGE[0]
@ "find-string" : int FindString(string);
@ "get-selection" : int GetSelection();
@ "number" : int Number();

@MACRO ubArrayBox = NULL
@MACRO cArrayBox = SCHEME_BOXP({x})
@MACRO boxArrayResult = MakeIntList(*x0, r)

Scheme_Object *MakeIntList(int *v, int c)
{
  Scheme_Object *cdr = scheme_null, *obj;

  while (c--) {
    obj = scheme_make_integer(v[c]);
    cdr = scheme_make_pair(obj, cdr);
  }
  
  return cdr;
}

@ "get-selections" : int GetSelections(int**/boxArrayResult/ubArrayBox/cArrayBox);

@ "set" : void Set(-int,string[]/bList/ubList/cList); : : /glueListSet[string.0.1.0."wx:list%::set"]//
@ "set-first-item" : void SetFirstItem(int); : : /RANGE[0] <> index
@ "set-first-item" : void SetFirstItem(string); <> string
@ "set-string-selection" : void SetStringSelection(string);
@ "get-string" : string GetString(int); : : /RANGERET[0.scheme_null]
@ "set-string" : void SetString(int,string); : : /RANGE[0]

@END

@INCLUDE cb_end.xci
