
#if 0

#include "wx_form.h"

@HEADER

@INCLUDE wxs.xci

@CLASSBASE wxFormItem "wx:form-item" : "wx:object"

@ "get-panel-item" : wxItem! GetPanelItem();

@END

@CLASSBASE wxForm "wx:form" : "wx:object"

@CREATOR (int=wxFORM_BUTTON_ALL,int=wxFORM_BUTTON_AT_TOP);

@ "add" : void Add(wxFormItem!,long=-1);
@ "associate-panel" : void AssociatePanel(wxPanel!);
@ "delete" : bool Delete(int);
@ "find-item" : wxNode^ FindItem(int);
@ v "on-cancel" : void OnCancel();
@ v "on-ok" : void OnOk();
@ v "on-revert" : void OnRevert();
@ v "on-update" : void OnUpdate();
@ "revert-values" : void RevertValues();
@ "update-values" : bool UpdateValues();
@ "set" : bool Set(int,wxFormItem!);

@CONSTANT "wx:const-form-default" : int wxFORM_DEFAULT
@CONSTANT "wx:const-form-single-list" : int wxFORM_SINGLE_LIST
@CONSTANT "wx:const-form-multi-list" : int wxFORM_MULTI_LIST
@CONSTANT "wx:const-form-choice" : int wxFORM_CHOICE
@CONSTANT "wx:const-form-checkbox" : int wxFORM_CHECKBOX
@CONSTANT "wx:const-form-text" : int wxFORM_TEXT
@CONSTANT "wx:const-form-multitext" : int wxFORM_MULTITEXT
@CONSTANT "wx:const-form-slider" : int wxFORM_SLIDER
@CONSTANT "wx:const-form-message" : int wxFORM_MESSAGE
@CONSTANT "wx:const-form-newline" : int wxFORM_NEWLINE
@CONSTANT "wx:const-form-button" : int wxFORM_BUTTON
@CONSTANT "wx:const-form-dumb-message" : int wxFORM_DUMB_MESSAGE
@CONSTANT "wx:const-form-radiobox" : int wxFORM_RADIOBOX

@CONSTANT "wx:const-form-button-ok" : int wxFORM_BUTTON_OK    
@CONSTANT "wx:const-form-button-cancal" : int wxFORM_BUTTON_CANCEL
@CONSTANT "wx:const-form-button-update" : int wxFORM_BUTTON_UPDATE
@CONSTANT "wx:const-form-button-revert" : int wxFORM_BUTTON_REVERT
@CONSTANT "wx:const-form-button-help" : int wxFORM_BUTTON_HELP  
@CONSTANT "wx:const-form-button-all" : int wxFORM_BUTTON_ALL

@CONSTANT "wx:const-form-button-at-top" : int wxFORM_BUTTON_AT_TOP
@CONSTANT "wx:const-form-button-at-bottom" : int wxFORM_BUTTON_AT_BOTTOM

@END

@GLOBAL wxFormGlobal

@SET CALLBACK_CLASS = wxButton
@INCLUDE cb_start.xci

@ "wx:make-form-button" : wxFormItem! wxMakeFormButton(string,wxFunction/bCallback/ubCallback/cCallback); : : ubCallbackSetup///ubCallbackFinish
@ "wx:make-form-message" : wxFormItem! wxMakeFormMessage(string);
@ "wx:make-form-newline" : wxFormItem! wxMakeFormNewLine();

// These aren't going to work right!
@ "wx:make-form-long" : wxFormItem! wxMakeFormLong(string,long*,int=wxFORM_DEFAULT,wxList^=NULL, string=NULL,int=0,int=-1,int=-1);
// @ "wx:make-form-short" : wxFormItem! wxMakeFormShort(string,short*,int=wxFORM_DEFAULT,wxList^=NULL, string=NULL,int=0,int=-1,int=-1);
// @ "wx:make-form-float" : wxFormItem! wxMakeFormFloat(string,float*,int=wxFORM_DEFAULT,wxList^=NULL, string=NULL,int=0,int=-1,int=-1);
// @ "wx:make-form-bool" : wxFormItem! wxMakeFormFloat(string,bool*,int=wxFORM_DEFAULT,wxList^=NULL, string=NULL,int=0,int=-1,int=-1);
// @ "wx:make-form-string" : wxFormItem! wxMakeFormFloat(string,string*,int=wxFORM_DEFAULT,wxList^=NULL, string=NULL,int=0,int=-1,int=-1);

@END

@INCLUDE cb_end.xci

#endif
