
@INCLUDE prefix.xci

#include "wx_media.h"

@INCLUDE wxs.xci

@HEADER

@INCLUDE wxs_eds.xci
@INCLUDE wxs_bmt.xci

@CLASSBASE wxMediaPasteboard "pasteboard" : "editor"

@CREATOR ()

@CLASSID wxTYPE_MEDIA_PASTEBOARD

@SETMARK X = 
@SETMARK Y = d
@SETMARK Z = d
@INCLUDE wxs_mbuf.xci

@ "insert" : void Insert(wxSnip!,float,float); <> snip% with location
@ "insert" : void Insert(wxSnip!,wxSnip^); <> snip% with before-snip%
@ "insert" : void Insert(wxSnip!,wxSnip^,float,float); <> snip% with before-snip% and location

@ "delete" : void Delete(); <> no argument
@ "delete" : void Delete(wxSnip!); <> snip%

@ v "do-copy" : void DoCopy(ExactLong,bool);
@ v "do-paste" : void DoPaste(ExactLong);

@ "erase" : void Erase();

@ "remove" : void Remove(wxSnip!);

@ "move-to" : void MoveTo(wxSnip!,float,float);
@ "move" : void Move(wxSnip!,float,float); <> snip%
@ "move" : void Move(float,float); <> without snip%

@ "resize" : bool Resize(wxSnip!, nnfloat, nnfloat);

@ "raise" : void Raise(wxSnip!);
@ "lower" : void Lower(wxSnip!);
@ "set-before" : void SetBefore(wxSnip!,wxSnip^);
@ "set-after" : void SetAfter(wxSnip!,wxSnip^);
  
@ "change-style" : void ChangeStyle(wxStyleDelta^,wxSnip^); <> style-delta% and snip%
@ "change-style" : void ChangeStyle(wxStyle^,wxSnip^=NULL); <> style%

@ "set-selected" : void SetSelected(wxSnip!);
@ "add-selected" : void AddSelected(wxSnip!); <> snip%
@ "add-selected" : void AddSelected(float,float,nnfloat,nnfloat); <> rectangle
@ "no-selected" :  void NoSelected();
@ "remove-selected" :  void RemoveSelected(wxSnip!);

@ "get-center" : void GetCenter(float*, float*);

@ "find-snip" : wxSnip^ FindSnip(float,float);
@ "is-selected?" : bool IsSelected(wxSnip^);
@ "find-next-selected-snip" : wxSnip^ FindNextSelectedSnip(wxSnip^);

@ v "can-insert?" : bool CanInsert(wxSnip!,wxSnip^,float, float);
@ v "on-insert" : void OnInsert(wxSnip!,wxSnip^,float, float);
@ v "after-insert" : void AfterInsert(wxSnip!,wxSnip^,float,float);
@ v "can-delete?" : bool CanDelete(wxSnip!);
@ v "on-delete" : void OnDelete(wxSnip!);
@ v "after-delete" :  void AfterDelete(wxSnip!);
@ v "can-move-to?" :  bool CanMoveTo(wxSnip!,float,float,bool);
@ v "on-move-to" :  void OnMoveTo(wxSnip!,float,float,bool);
@ v "after-move-to" :  void AfterMoveTo(wxSnip!,float,float,bool);
@ v "can-resize?" :  bool CanResize(wxSnip!,nnfloat,nnfloat);
@ v "on-resize" :  void OnResize(wxSnip!,nnfloat,nnfloat);
@ v "after-resize" :  void AfterResize(wxSnip!,nnfloat,nnfloat,bool);

@ v "can-select?" : bool CanSelect(wxSnip!, bool);
@ v "on-select" : void OnSelect(wxSnip!, bool);
@ v "after-select" :  void AfterSelect(wxSnip!, bool);

@ v "on-double-click" : void OnDoubleClick(wxSnip!, wxMouseEvent!);

@ v "interactive-adjust-mouse" : void InteractiveAdjustMouse(float*,float*);
@ v "interactive-adjust-move" : void InteractiveAdjustMove(wxSnip!,float*,float*);
@ v "interactive-adjust-resize" : void InteractiveAdjustResize(wxSnip!,nnfloat*,nnfloat*);

@ v "can-interactive-move?" : bool CanInteractiveMove(wxMouseEvent!);
@ v "on-interactive-move" : void OnInteractiveMove(wxMouseEvent!);
@ v "after-interactive-move" : void AfterInteractiveMove(wxMouseEvent!);
@ v "can-interactive-resize?" : bool CanInteractiveResize(wxSnip!);
@ v "on-interactive-resize" : void OnInteractiveResize(wxSnip!);
@ v "after-interactive-resize" : void AfterInteractiveResize(wxSnip!);

@ "get-dragable" : bool GetDragable();
@ "set-dragable" : void SetDragable(bool);
@ "get-selection-visible" : bool GetSelectionVisible();
@ "set-selection-visible" : void SetSelectionVisible(bool);

@ "get-scroll-step" : float GetScrollStep();
@ "set-scroll-step" : void SetScrollStep(nnfloat);

@END
