
@INCLUDE prefix.xci

#include "wx_media.h"

@INCLUDE wxs.xci

@HEADER

@SET TYPE = float
@SET NOTEST = 1
@INCLUDE list.xci

@MACRO rNULL = return NULL;
@MACRO rFALSE = return FALSE;
@MACRO rZERO = return 0;

@CLASSBASE wxMediaEdit "wx:media-edit" : "wx:media-buffer"

@CREATOR (float=1.0,float[]=NULL/bList/ubList/cList,-int=0); : : /glueListSet[float.1.1.2."wx:media-edit%::initialization"]//

@CLASSID wxTYPE_MEDIA_EDIT

@VAR Scheme_Object *scroll_closure;

@SETMARK X = 
@SETMARK Y = d
@SETMARK Z = d
@INCLUDE wxs_mbuf.xci

@ "get-position" : void GetPosition(long?,long?=NULL);
@ "get-start-position" : long GetStartPosition();
@ "get-end-position" : long GetEndPosition();
@ "set-position" : void SetPosition(long,long=-1,bool=FALSE,bool=TRUE,int=wxDEFAULT_SELECT);
@ "set-position-bias-scroll" : void SetPositionBiasScroll(int,long,long=-1,bool=FALSE,bool=TRUE,int=wxDEFAULT_SELECT);
@ "move-position" :  void MovePosition(long,bool=FALSE,int=wxMOVE_SIMPLE);
@ "scroll-to-position" : bool ScrollToPosition(long,bool=FALSE,long=-1,int=0);
@ "get-visible-position-range" : void GetVisiblePositionRange(long?,long?);
@ "get-visible-line-range" : void GetVisibleLineRange(long?,long?);

@ v "set-anchor" : void SetAnchor(bool);
@ "get-anchor" : bool GetAnchor();

@ "flash-on" : void FlashOn(long,long,bool=FALSE,bool=TRUE,long=500);
@ "flash-off" : void FlashOff();

@MACRO setStringLen[i.s] = x<i> = SCHEME_STRTAG_VAL(p[<s>]);
@MACRO checkStringLen[i.s] = if ((x<i> < 0) || (x<i> > SCHEME_STRTAG_VAL(p[<s>]))) scheme_signal_error("wx:media-edit%%::insert: bad string length");

@ "insert" : void Insert(-long,string,long,long=-1,bool=TRUE);  : : /setStringLen[0.0] <> string and position
@ "insert" : void Insert(-long,string);  : : /setStringLen[0.0] <> string without position
@ "insert" : void Insert(long,string,long,long=-1,bool=TRUE);  : : /checkStringLen[0.1] <> length and string without position
@ "insert" : void Insert(long,string);  : : /checkStringLen[0.1] <> length, string, and position
@ "insert" : void Insert(wxSnip!,long,long=-1,bool=TRUE); <> wx:snip% and position
@ "insert" : void Insert(uchar); <> character without position
@ "insert" : void Insert(uchar,long,long=-1); <> character and position

@ "delete" : void Delete(long, long=-1,bool=TRUE); <> position
@ "delete" : void Delete(); <> no position
@ "erase" :  void Erase();

@ "cut" :  void Cut(bool,long,long,long=-1); <> position
@ "copy" : void Copy(bool,long,long,long=-1); <> position
@ "paste" : void Paste(long,long,long=-1); <> position
@ "paste-next" : void PasteNext();
@ "kill" : void Kill(long,long,long); <> position

@ v "do-copy" : void DoCopy(long,long,long,bool);
@ v "do-paste" : void DoPaste(long,long);

@ "change-style" : void ChangeStyle(wxStyleDelta^,long,long=-1); <> wx:style-delta% and position
@ "change-style" : void ChangeStyle(wxStyle^,long=-1,long=-1); <> wx:style%
			
@ "split-snip" : void SplitSnip(long);

@ "find-position" : long FindPosition(float,float,bool?=NULL,bool?=NULL,float?=NULL);
@ "find-line" : long FindLine(float,bool?=NULL);
@ "find-position-in-line" : long FindPositionInLine(long,float,bool?=NULL,bool?=NULL,float?=NULL);

@ "get-between-threshold" : float GetBetweenThreshold();
@ "set-between-threshold" : void SetBetweenThreshold(float);

@ "position-line" : long PositionLine(long,bool=FALSE);
@ "position-location" :  void PositionLocation(long,float?=NULL,float?=NULL,bool=TRUE,bool=FALSE,bool=FALSE);
@ "line-location" : float LineLocation(long,bool=TRUE);

@ "line-start-position" : long LineStartPosition(long,bool=TRUE);
@ "line-end-position" : long LineEndPosition(long,bool=TRUE);
@ "line-length" : long LineLength(long);
@ "last-position" : long LastPosition();
@ "last-line" : long LastLine();

@ "position-paragraph" : long PositionParagraph(long,bool=FALSE);
@ "paragraph-start-position" : long ParagraphStartPosition(long,bool=TRUE);
@ "paragraph-end-position" : long ParagraphEndPosition(long,bool=TRUE);
@ "line-paragraph" : long LineParagraph(long);
@ "paragraph-start-line" : long ParagraphStartLine(long);
@ "pargraph-end-line" : long ParagraphEndLine(long);
@ "last-paragraph" : long LastParagraph();

@MACRO CHECKDIR[p] = if ((x<p> != 1) && (x<p> != -1)) scheme_signal_error("wx:media-edit%%::find-string: direction must be 1 or -1, given %d", x<p>);

@ "find-string" : long FindString(string,int=1,long=-1,long=-1,bool=TRUE,bool=TRUE);  : : /CHECKDIR[1]

@SET TYPE = long
@SET NOTEST = 1
@INCLUDE list.xci

@ "find-string-all" : long[]/bReturnList[long.1] FindStringAll(string,-long*,int=1,long=-1,long=-1,bool=TRUE,bool=TRUE);

@ "find-snip" : wxSnip^ FindSnip(long,int,long?=NULL)
@ "get-snip-position-and-location" : void GetSnipPositionAndLocation(wxSnip!,long?,float?,float?);
@ "get-snip-position" : long GetSnipPosition(wxSnip!);

@MACRO makeNoCopyString[len] = scheme_make_sized_string(r, <len>, 0)

@ "get-text" : string/makeNoCopyString[_x4] GetText(long=-1,long=-1,bool=FALSE,bool=FALSE,-long*=NULL);
@ "get-character" : uchar GetCharacter(long);

@ "insert-file" : bool InsertFile(string,int=wxMEDIA_FF_GUESS);

@ "read-from-file" : bool ReadFromFile(wxMediaStreamIn%,long,bool=FALSE); <> with position
@ "write-to-file" : bool WriteToFile(wxMediaStreamOut%,long,long=-1); <> with position

@ "get-file-format" : int GetFileFormat();
@ "set-file-format" : void SetFileFormat(int);

@ "get-overwrite-mode" : bool GetOverwriteMode();
@ "set-overwrite-mode" : void SetOverwriteMode(bool);

@MACRO checkNull = if (!x0) x0 = &_x0;

@ "get-tabs" : float[]/bReturnList[float.0] GetTabs(int?=NULL,float?=NULL,bool?=NULL); : : /checkNull/
@ "set-tabs" : void SetTabs(float[]/bList/ubList/cList,-int,float=wxTAB_WIDTH,bool=TRUE); : : /glueListSet[float.0.0.1."wx:media-edit%::set-tabs"]//

@ "add-editor-functions" : void AddEditorFunctions(wxKeymap!);

@ v "on-insert" : bool OnInsert(long,long);
@ v "after-insert" : void AfterInsert(long,long);
@ v "on-delete" : bool OnDelete(long,long);
@ v "after-delete" : void AfterDelete(long,long);
@ v "on-change-style" : bool OnChangeStyle(long,long);
@ v "after-change-style" : void AfterChangeStyle(long,long);
@ v "after-set-position" : void AfterSetPosition();
@ v "on-set-size-constraint" : bool OnSetSizeConstraint();
@ v "after-set-size-constraint" : void AfterSetSizeConstraint();

@ v "get-region-data" : wxBufferData^ GetRegionData(long,long);
@ v "set-region-data" : void SetRegionData(long, long, wxBufferData^);

@ "find-wordbreak" : void FindWordbreak(long?,long?,int);

@ "set-wordbreak-map" : void SetWordbreakMap(wxMediaWordbreakMap^);
@ "get-wordbreak-map" : wxMediaWordbreakMap^ GetWordbreakMap();

@ "hide-caret" : void HideCaret(bool);
@ "caret-hidden?" : bool CaretHidden();

@ v "on-new-text-snip" : wxTextSnip! OnNewTextSnip();
@ v "on-new-tab-snip" : wxTabSnip! OnNewTabSnip();

@ "set-autowrap-bitmap" : wxBitmap^ SetAutowrapBitmap(wxBitmap^);

static void WordbreakCallbackToScheme(wxMediaEdit *,long*,long*,int,Scheme_Object *);

@MACRO ubCallback = (wxWordbreakFunc)WordbreakCallbackToScheme
@MACRO ubData = p[0]
@MACRO spCallback = (wxMediaEdit-object (box num) (box num) num -> void)

@MACRO ubCallback2 = (wxClickbackFunc)ClickbackToScheme
@MACRO ubData2 = p[2]
@MACRO cCallback2 = SCHEME_PROCP({x})
@MACRO spCallback2 = (wxMediaEdit-object num num -> void)

@ "set-wordbreak-func" : void SetWordbreakFunc(wxWordbreakFunc//ubCallback/cCallback//spCallback,-unknown#void*//ubData);

@ "set-clickback" : void SetClickback(long,long,wxClickbackFunc//ubCallback2/cCallback2//spCallback2,-unknown#void*//ubData2,wxStyleDelta^=NULL,bool=FALSE);
@ "remove-clickback" : void RemoveClickback(long,long);

static void WordbreakCallbackToScheme(wxMediaEdit *media,
				      long *start, long *end,
				      int reason,
				      Scheme_Object *f)
{
  Scheme_Object *p[4], *s, *e;
  jmp_buf savebuf;

  COPY_JMPBUF(savebuf, scheme_error_buf);

  if (!scheme_setjmp(scheme_error_buf)) {
    p[0] = objscheme_bundle_wxMediaEdit(media);
    if (start)
      s = scheme_box(objscheme_bundle_integer(*start));
    else
      s = scheme_null;
    if (end)
      e = scheme_box(objscheme_bundle_integer(*end));
    else
      e = scheme_null;
    p[1] = s;
    p[2] = e;
    p[3] = scheme_make_integer(reason);

    scheme_apply(f, 4, p);
    if (start)
      *start = objscheme_unbundle_integer(scheme_unbox(s), "Scheme wordbreak callback");
    if (end)
      *end = objscheme_unbundle_integer(scheme_unbox(e), "Scheme wordbreak callback");
  }

  COPY_JMPBUF(scheme_error_buf, savebuf);
}

static void ClickbackToScheme(wxMediaEdit *media,
			      long start, long end,
			      Scheme_Object *f)
{
  Scheme_Object *p[3];
  jmp_buf savebuf;

  p[0] = objscheme_bundle_wxMediaEdit(media);
  p[1] = objscheme_bundle_integer(start);
  p[2] = objscheme_bundle_integer(end);

  COPY_JMPBUF(savebuf, scheme_error_buf);

  if (!scheme_setjmp(scheme_error_buf))
    scheme_apply(f, 3, p);

  COPY_JMPBUF(scheme_error_buf, savebuf);
}

@CONSTANT "wx:const-edit-undo" : int wxEDIT_UNDO
@CONSTANT "wx:const-edit-redo" : int wxEDIT_REDO
@CONSTANT "wx:const-edit-clear" : int wxEDIT_CLEAR
@CONSTANT "wx:const-edit-cut" : int wxEDIT_CUT
@CONSTANT "wx:const-edit-copy" : int wxEDIT_COPY
@CONSTANT "wx:const-edit-paste" : int wxEDIT_PASTE
@CONSTANT "wx:const-edit-kill" : int wxEDIT_KILL
@CONSTANT "wx:const-edit-insert-text-box" : int wxEDIT_INSERT_TEXT_BOX
@CONSTANT "wx:const-edit-insert-graphic-box" : int wxEDIT_INSERT_GRAPHIC_BOX
@CONSTANT "wx:const-edit-insert-image" : int wxEDIT_INSERT_IMAGE
@CONSTANT "wx:const-edit-select-all" : int wxEDIT_SELECT_ALL

@CONSTANT "wx:const-move-simple" : int wxMOVE_SIMPLE
@CONSTANT "wx:const-move-line" : int wxMOVE_LINE
@CONSTANT "wx:const-move-page" : int wxMOVE_PAGE
@CONSTANT "wx:const-move-word" : int wxMOVE_WORD

@CONSTANT "wx:const-media-ff-guess" : int wxMEDIA_FF_GUESS
@CONSTANT "wx:const-media-ff-std" : int wxMEDIA_FF_STD
@CONSTANT "wx:const-media-ff-text" : int wxMEDIA_FF_TEXT
@CONSTANT "wx:const-media-ff-text-force-cr" : int wxMEDIA_FF_TEXT_FORCE_CR
@CONSTANT "wx:const-media-ff-same" : int wxMEDIA_FF_SAME
@CONSTANT "wx:const-media-ff-copy" : int wxMEDIA_FF_COPY

@CONSTANT "wx:const-snip-draw-no-caret" : int wxSNIP_DRAW_NO_CARET
@CONSTANT "wx:const-snip-draw-show-caret" : int wxSNIP_DRAW_SHOW_CARET
@CONSTANT "wx:const-snip-draw-show-inactive-caret" : int wxSNIP_DRAW_SHOW_INACTIVE_CARET

@CONSTANT "wx:const-snip-before-or-null" : int wxSNIP_BEFORE_OR_NULL
@CONSTANT "wx:const-snip-before" : int wxSNIP_BEFORE
@CONSTANT "wx:const-snip-after" : int wxSNIP_AFTER
@CONSTANT "wx:const-snip-after-or-null" : int wxSNIP_AFTER_OR_NULL

@CONSTANT "wx:const-break-for-caret" : int wxBREAK_FOR_CARET
@CONSTANT "wx:const-break-for-line" : int wxBREAK_FOR_LINE
@CONSTANT "wx:const-break-for-selection" : int wxBREAK_FOR_SELECTION
@CONSTANT "wx:const-break-for-user-1" : int wxBREAK_FOR_USER_1
@CONSTANT "wx:const-break-for-user-2" : int wxBREAK_FOR_USER_2

@CONSTANT "wx:const-default-select" : int wxDEFAULT_SELECT
@CONSTANT "wx:const-x-select" : int wxX_SELECT
@CONSTANT "wx:const-local-select" : int wxLOCAL_SELECT

@END


