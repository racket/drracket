
@INCLUDE prefix.xci

#include "wx_media.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxMediaPasteboard "wx:media-pasteboard" : "wx:media-buffer"

@CREATOR ()

@CLASSID wxTYPE_MEDIA_PASTEBOARD

@SETMARK X = 
@SETMARK Y = d
@SETMARK Z = d
@INCLUDE wxs_mbuf.xci

@ "insert" : void Insert(wxSnip!,float,float); <> wx:snip% with location
@ "insert" : void Insert(wxSnip!,wxSnip^); <> wx:snip% with before-wx:snip%
@ "insert" : void Insert(wxSnip!,wxSnip^,float,float); <> wx:snip% with before-wx:snip% and location

@ "delete" : void Delete(); <> no argument
@ "delete" : void Delete(wxSnip!); <> wx:snip%

@ v "do-copy" : void DoCopy(long,bool);
@ v "do-paste" : void DoPaste(long);

@ "erase" : void Erase();

@ "remove" : void Remove(wxSnip!);

@ "move-to" : void MoveTo(wxSnip!,float,float);
@ "move" : void Move(wxSnip!,float,float); <> wx:snip%
@ "move" : void Move(float,float); <> without wx:snip%

@ "resize" : bool Resize(wxSnip!, float, float);

@ "raise" : void Raise(wxSnip!);
@ "lower" : void Lower(wxSnip!);
@ "set-before" : void SetBefore(wxSnip!,wxSnip^);
@ "set-after" : void SetAfter(wxSnip!,wxSnip^);
  
@ "change-style" : void ChangeStyle(wxStyleDelta^,wxSnip^); <> wx:style-delta% and wx:snip%
@ "change-style" : void ChangeStyle(wxStyle^,wxSnip^=NULL); <> wx:style%

@ "set-selected" : void SetSelected(wxSnip!);
@ "add-selected" : void AddSelected(wxSnip!); <> wx:snip%
@ "add-selected" : void AddSelected(float,float,float,float); <> rectangle
@ "no-selected" :  void NoSelected();
@ "remove-selected" :  void RemoveSelected(wxSnip!);

@ "get-center" : void GetCenter(float*, float*);

@ "add-pasteboard-functions" : void AddPasteboardFunctions(wxKeymap!);

@ "find-snip" : wxSnip^ FindSnip(float,float);
@ "find-first-snip" : wxSnip^ FindFirstSnip();
@ "is-selected?" : bool IsSelected(wxSnip^);
@ "find-next-selected-snip" : wxSnip^ FindNextSelectedSnip(wxSnip^);

@ v "on-insert" : bool OnInsert(wxSnip!,wxSnip^,float, float);
@ v "after-insert" : void AfterInsert(wxSnip!,wxSnip^,float,float);
@ v "on-delete" : bool OnDelete(wxSnip!);
@ v "after-delete" :  void AfterDelete(wxSnip!);
@ v "on-move-to" :  bool OnMoveTo(wxSnip!,float,float,bool);
@ v "after-move-to" :  void AfterMoveTo(wxSnip!,float,float,bool);
@ v "on-resize" :  bool OnResize(wxSnip!,float,float);
@ v "after-resize" :  void AfterResize(wxSnip!,float,float,bool);

@ v "on-select" : bool OnSelect(wxSnip!, bool);
@ v "after-select" :  void AfterSelect(wxSnip!, bool);

@ v "on-double-click" : void OnDoubleClick(wxSnip!, wxMouseEvent%);

@ v "interactive-adjust-mouse" : void InteractiveAdjustMouse(float*,float*);
@ v "interactive-adjust-move" : void InteractiveAdjustMove(wxSnip!,float*,float*);
@ v "interactive-adjust-resize" : void InteractiveAdjustResize(wxSnip!,float*,float*);

@ v "on-interactive-move" : bool OnInteractiveMove();
@ v "after-interactive-move" : void AfterInteractiveMove();
@ v "on-interactive-resize" : bool OnInteractiveResize(wxSnip!);
@ v "after-interactive-resize" : void AfterInteractiveResize(wxSnip!);

@ "get-dragable" : bool GetDragable();
@ "set-dragable" : void SetDragable(bool);
@ "get-selection-visible" : bool GetSelectionVisible();
@ "set-selection-visible" : void SetSelectionVisible(bool);

@ "get-scroll-step" : float GetScrollStep();
@ "set-scroll-step" : void SetScrollStep(float);

@END
