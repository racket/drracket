
@INCLUDE prefix.xci

#include "wx_media.h"

@INCLUDE wxs.xci

@HEADER

static void *cconvert(wxMediaBuffer *b, double x, double y, int todc)
{
  double dcx, dcy;
  wxMediaAdmin *admin;
  Scheme_Object *a[2];

  admin = b->GetAdmin();
  if (admin) {
    float dx, dy;
    admin->GetDC(&dx, &dy);
    if (!todc) {
      dcx = dx + x;
      dcy = dy + y;
    } else {
      dcx = x - dx;
      dcy = y - dy;
    }
  } else {
    dcx = x;
    dcy = y;
  }

  a[0] = objscheme_bundle_double(dcx);
  a[1] = objscheme_bundle_double(dcy);

  return scheme_values(2, a);
}

static void *wxbBufferToDC(wxMediaBuffer *b, double x, double y)
{
  return cconvert(b, x, y, 1);
}

static void *wxbDCToBuffer(wxMediaBuffer *b, double x, double y)
{
  return cconvert(b, x, y, 0);
}

@MACRO rNULL = return NULL;
@MACRO rFALSE = return FALSE;
@MACRO rZERO = return 0;

@CLASSBASE wxMediaBuffer "wx:media-buffer" : "wx:object"

@CLASSID wxTYPE_MEDIA_BUFFER

@IVAR "buffer-type" : int bufferType

@SETMARK Y = V
@SETMARK Z = v
@INCLUDE wxs_mbuf.xci

// X are Methods not intended to be overriden by the user,
// but acutally are implemented with virtual
@SETMARK X = D

@ X "load-file" : bool LoadFile(nstring=NULL,int=wxMEDIA_FF_GUESS,bool=TRUE);
@ X "save-file" : bool SaveFile(nstring=NULL,int=wxMEDIA_FF_SAME,bool=TRUE);
@ X "insert-file" : bool InsertFile(string,int=wxMEDIA_FF_GUESS,bool=TRUE);

@ X "copy-self" : wxMediaBuffer! CopySelf(); : : : : XrZERO

@ X "get-extent" : void GetExtent(float?,float?);
@ X "get-descent" : float GetDescent(); : : : : XrZERO
@ X "get-space" : float GetSpace(); : : : : XrZERO

@ X "get-max-width" : float GetMaxWidth(); : : : : XrZERO
@ X "get-min-width" : float GetMinWidth(); : : : : XrZERO
@ X "set-max-width" : void SetMaxWidth(float);
@ X "set-min-width" : void SetMinWidth(float);
@ X "get-max-height" : float GetMaxHeight(); : : : : XrZERO
@ X "get-min-height" : float GetMinHeight(); : : : : XrZERO
@ X "set-max-height" : void SetMaxHeight(float);
@ X "set-min-height" : void SetMinHeight(float);

@ X "read-from-file" : bool ReadFromFile(wxMediaStreamIn%); : : : : XrZERO
@ X "write-to-file" : bool WriteToFile(wxMediaStreamOut%); : : : : XrZERO

@ X "style-has-changed" : void StyleHasChanged(wxStyle^);

@ X "begin-edit-sequence" : void BeginEditSequence(bool=TRUE);
@ X "end-edit-sequence" : void EndEditSequence();
@ X "refresh-delayed?" : bool RefreshDelayed();

@ X "get-snip-location" : bool GetSnipLocation(wxSnip!,float?=NULL,float?=NULL,bool=FALSE); : : : : XrZERO

@ X "scroll-line-location" : float ScrollLineLocation(long); : : : : XrZERO
@ X "num-scroll-lines" : long NumScrollLines(); : : : : XrZERO
@ X "find-scroll-line" : long FindScrollLine(float); : : : : XrZERO

@ X "print-to-dc" : void PrintToDC(wxDC!); : : /CHECKOK[0."wx:media-buffer%::print-to-dc"]

@ X "get-admin" : wxMediaAdmin^ GetAdmin(); : : : rNULL
@ X "set-admin" : void SetAdmin(wxMediaAdmin^);


@ "global-to-local" : void GlobalToLocal(float*,float*);
@ "local-to-global" : void LocalToGlobal(float*,float*);

@ v "get-dc" : wxDC! GetDC();
@ v "get-view-size" : void GetViewSize(float?,float?);

@ "clear" : void Clear();
@ "select-all" : void SelectAll();

@ "undo" :  void Undo();
@ "redo": void Redo()
@ "clear-undos" : void ClearUndos();

@ "set-max-undo-history" : void SetMaxUndoHistory(int);
@ "get-max-undo-history" : int GetMaxUndoHistory();

@ "append-edit-items" : int AppendEditItems(wxMenu!,int=0);
@ "append-font-items" : int AppendFontItems(wxMenu!,int=0);
@ "do-edit" : void DoEdit(int,bool=TRUE,long=0);
@ "do-font" : void DoFont(int,bool=TRUE);

@ "set-keymap" : void SetKeymap(wxKeymap^=NULL);
@ "get-keymap" : wxKeymap! GetKeymap();

@ "add-buffer-functions" : void AddBufferFunctions(wxKeymap!);

@ "get-style-list" : wxStyleList! GetStyleList();
@ "set-style-list" : void SetStyleList(wxStyleList!);

@ "set-load-overwrites-styles" : void SetLoadOverwritesStyles(bool)
@ "get-load-overwrites-styles" : bool GetLoadOverwritesStyles();

@ "set-cursor" : void SetCursor(wxCursor^,bool=TRUE); : : /CHECKVOIDABLEOK[0]

@ "lock" : void Lock(bool);
@ "modified?" : bool Modified();

@ "get-filename" : nstring GetFilename(bool?=NULL);

@ "insert-box" : void InsertBox(int=wxEDIT_BUFFER);
@ "insert-image" : void InsertImage(nstring=NULL,long=-1,bool=FALSE,bool=TRUE);

@ "print" : void Print(nstring=NULL,bool=TRUE,bool=FALSE,int=0);

@ "begin-write-header-footer-to-file" : bool BeginWriteHeaderFooterToFile(wxMediaStreamOut%,string,long*);
@ "end-write-header-footer-to-file" : bool EndWriteHeaderFooterToFile(wxMediaStreamOut%,long);

@ "get-focus-snip" : wxSnip^ GetFocusSnip();

@ "get-inactive-caret-threshold" : int GetInactiveCaretThreshold();
@ "set-inactive-caret-threshold" : void SetInactiveCaretThreshold(int);

@MACRO bundleAny = ((Scheme_Object *){x})
 
@ m "buffer-location-to-dc-location" : void*/bundleAny wxbBufferToDC(double, double);
@ m "dc-location-to-buffer-location" : void*/bundleAny wxbDCToBuffer(double, double);

@CONSTANT "wx:const-edit-buffer" : int wxEDIT_BUFFER
@CONSTANT "wx:const-pasteboard-buffer" : int wxPASTEBOARD_BUFFER

@CONSTANT "wx:const-focus-immediate" : int wxFOCUS_IMMEDIATE
@CONSTANT "wx:const-focus-display" : int wxFOCUS_DISPLAY
@CONSTANT "wx:const-focus-global" : int wxFOCUS_GLOBAL

@END

@GLOBAL wxMediaGlobal

@ "wx:get-media-print-margin" : void wxGetMediaPrintMargin(long?=NULL,long?=NULL);
@ "wx:set-media-print-margin" : void wxSetMediaPrintMargin(long=-1,long=-1);

@ "wx:read-media-global-header" : bool wxReadMediaGlobalHeader(wxMediaStreamIn%);
@ "wx:read-media-global-footer" : bool wxReadMediaGlobalFooter(wxMediaStreamIn%);
@ "wx:write-media-global-header" : bool wxWriteMediaGlobalHeader(wxMediaStreamOut%);
@ "wx:write-media-global-footer" : bool wxWriteMediaGlobalFooter(wxMediaStreamOut%);

@ "wx:add-media-buffer-functions" : void wxAddMediaBufferFunctions(wxKeymap!);
@ "wx:add-media-editor-functions" : void wxAddMediaEditorFunctions(wxKeymap!);
@ "wx:add-media-pasteboard-functions" : void wxAddMediaPasteboardFunctions(wxKeymap!);

@ "wx:media-set-x-selection-mode" : void wxMediaSetXSelectionMode(bool);

@ "wx:get-the-snip-class-list" : wxSnipClassList! wxGetTheSnipClassList()
@ "wx:get-the-buffer-data-class-list" : wxBufferDataClassList! wxGetTheBufferDataClassList()

@END
