
@INCLUDE prefix.xci

#include "wx_lbox.h"

@INCLUDE wxs.xci

@HEADER

static Scheme_Object* GetSelectionList(wxListBox *l)
{
  int c, *v;
  Scheme_Object *cdr, *obj;;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH(0, l);
  VAR_STACK_PUSH(1, v);
  VAR_STACK_PUSH(2, cdr);


  c = WITH_VAR_STACK(l->GetSelections(&v));

  cdr = scheme_null;

  while (c--) {
    obj = WITH_VAR_STACK(scheme_make_integer(v[c]));
    cdr = WITH_VAR_STACK(scheme_make_pair(obj, cdr));
  }
  
  return cdr;
}

@BEGINSYMBOLS kind > ONE > PRED BUNDLE
@SYM "single" : wxSINGLE       
@SYM "multiple" : wxMULTIPLE     
@SYM "extended" : wxEXTENDED     
@ENDSYMBOLS

@BEGINSYMBOLS style > > PRED BUNDLE
@SYM "always-vscroll" : wxALWAYS_SB    
@SYM "hscroll" : wxHSCROLL
@ENDSYMBOLS

@CLASSBASE wxListBox "list-box":"item"

@CLASSID wxTYPE_LIST_BOX

@SET CALLBACK_CLASS = wxListBox
@SET CALLBACK_CLASS_USER = METHODNAME("list-box%","initialization")
@INCLUDE cb_start.xci

@SET TYPE = string
@SET NOTEST = 1
@INCLUDE list.xci

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback/nopush,nstring,SYM[kind]=wxSINGLE,int=-1,int=-1,int=-1,int=-1,-int=0,string[]=NULL/bList/ubList/cList///push,SYM[style]=0,string="button"); : : ubCallbackSetup/NOZERO[6]|NOZERO[7]|glueListSet[string.8.9.8.METHODNAME("list-box%","initialization")]/glueCleanup[9]/ubCallbackCreatorFinish

@INCLUDE wxs_item.xci

#define RANGECLASS wxListBox
@INCLUDE range.xci

@MACRO bAnythingFromString = (({x}) ? ((Scheme_Object *){x}) : XC_SCHEME_NULL)
@MACRO ubAnythingToString = ((char *){x})
@MACRO cAnything = 1

@ "append" : void Append(string); <> without data
@ "append" : void Append(string, string//ubAnythingToString/cAnything); <> with data
@ "clear" : void Clear();
@ "delete" : void Delete(int); : : /RANGE[0]
@ "select" : void SetSelection(int,bool=TRUE); : : /RANGE[0]
@ "set-selection" : void SetOneSelection(int); : : /RANGE[0]
@ "selected?" : bool Selected(int); : : /RANGERET[0.scheme_false]
@ "get-string-selection" : nstring GetStringSelection();
@ "get-data" : nstring/bAnythingFromString GetClientData(int); : : /RANGERET[0.XC_SCHEME_NULL]
@ "set-data" : void SetClientData(int, string//ubAnythingToString/cAnything); : : /RANGE[0]
@ "find-string" : int FindString(string);
@ "get-selection" : int GetSelection();
@ "number" : int Number();
@ "number-of-visible-items" : int NumberOfVisibleItems();
@ "get-first-item" : int GetFirstItem();

@MACRO bundleAny = ((Scheme_Object *){x})

@ m "get-selections" : Scheme_Object*/bundleAny GetSelectionList();

@ "set" : void Set(-int,string[]/bList/ubList/cList///push); : : /glueListSet[string.0.1.0.METHODNAME("list%","set")]//
@ "set-first-visible-item" : void SetFirstItem(int); : : /RANGE[0] <> index
@ "set-string-selection" : bool SetStringSelection(string);
@ "get-string" : nstring GetString(int); : : /RANGERET[0.XC_SCHEME_NULL]
@ "set-string" : void SetString(int,string); : : /RANGE[0]

@END

@INCLUDE cb_end.xci
