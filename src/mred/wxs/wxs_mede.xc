
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

@BEGINSYMBOLS selType > ONE > PRED BUNDLE
@SYM "default" : wxDEFAULT_SELECT
@SYM "x" : wxX_SELECT
@SYM "local" : wxLOCAL_SELECT
@ENDSYMBOLS

@BEGINSYMBOLS moveCode > ONE > PRED BUNDLE
@SYM "home" : WXK_HOME
@SYM "end" : WXK_END
@SYM "right" : WXK_RIGHT
@SYM "left" : WXK_LEFT
@SYM "up" : WXK_UP
@SYM "down" : WXK_DOWN
@ENDSYMBOLS

@BEGINSYMBOLS move > ONE > PRED BUNDLE
@SYM "simple" : wxMOVE_SIMPLE
@SYM "line" : wxMOVE_LINE
@SYM "page" : wxMOVE_PAGE
@SYM "word" : wxMOVE_WORD
@ENDSYMBOLS

@BEGINSYMBOLS findKind > ONE > PRED BUNDLE
@SYM "before-or-none" : wxSNIP_BEFORE_OR_NULL
@SYM "before" : wxSNIP_BEFORE
@SYM "after" : wxSNIP_AFTER
@SYM "after-or-none" : wxSNIP_AFTER_OR_NULL
@ENDSYMBOLS

@BEGINSYMBOLS breakType > ONE > PRED
@SYM "caret" : wxBREAK_FOR_CARET
@SYM "line" : wxBREAK_FOR_LINE
@SYM "selection" : wxBREAK_FOR_SELECTION
@SYM "user1" : wxBREAK_FOR_USER_1
@SYM "user2" : wxBREAK_FOR_USER_2
@ENDSYMBOLS

# define Sym_FORWARD 1
# define Sym_BACKWARD -1
@BEGINSYMBOLS direction > ONE > PRED
@SYM "forward" : Sym_FORWARD
@SYM "backward" : Sym_BACKWARD
@ENDSYMBOLS

# define Sym_RIGHT 1
# define Sym_CENTER 0
# define Sym_LEFT -1
@BEGINSYMBOLS horizontalAlignment > ONE > PRED BUNDLE
@SYM "left" : Sym_LEFT
@SYM "right" : Sym_RIGHT
@SYM "center" : Sym_CENTER
@ENDSYMBOLS

@INCLUDE wxs_eds.xci
@INCLUDE wxs_bmt.xci

# define Sym_END_ONLY 2
# define Sym_START_ONLY -2
@BEGINSYMBOLS Bias > ONE > PRED BUNDLE
@SYM "start-only" : Sym_START_ONLY
@SYM "start" : Sym_START
@SYM "none" : Sym_NONE
@SYM "end" : Sym_END
@SYM "end-only" : Sym_END_ONLY
@ENDSYMBOLS

@CLASSBASE wxMediaEdit "text" : "editor"

@CREATOR (nnfloat=1.0,float[]=NULL/bList/ubList/cList///push,-int=0); : : /glueListSet[float.1.1.2.METHODNAME("text%","initialization")]//

@CLASSID wxTYPE_MEDIA_EDIT

@VAR Scheme_Object *scroll_closure;

@SETMARK X = 
@SETMARK Y = d
@SETMARK Z = d
@INCLUDE wxs_mbuf.xci

@ "get-position" : void GetPosition(nnlong?,nnlong?=NULL);
@ "get-start-position" : long GetStartPosition();
@ "get-end-position" : long GetEndPosition();
@ "set-position" : void SetPosition(nnlong,nnls[same]=-1,bool=FALSE,bool=TRUE,SYM[selType]=wxDEFAULT_SELECT);
@ "set-position-bias-scroll" : void SetPositionBiasScroll(SYM[Bias],nnlong,nnls[same]=-1,bool=FALSE,bool=TRUE,SYM[selType]=wxDEFAULT_SELECT);
@ "move-position" :  void MovePosition(SYM[moveCode],bool=FALSE,SYM[move]=wxMOVE_SIMPLE);
@ "scroll-to-position" : bool ScrollToPosition(nnlong,bool=FALSE,nnls[same]=-1,SYM[bias]=0);
@ "get-visible-position-range" : void GetVisiblePositionRange(nnlong?,nnlong?);
@ "get-visible-line-range" : void GetVisibleLineRange(nnlong?,nnlong?);

@ v "set-anchor" : void SetAnchor(bool);
@ "get-anchor" : bool GetAnchor();

@ "flash-on" : void FlashOn(nnlong,nnlong,bool=FALSE,bool=TRUE,nnlong=500);
@ "flash-off" : void FlashOff();

@ "get-top-line-base" : float GetTopLineBase(); : : : : XrZERO

@MACRO setStringLen[i.s] = x<i> = SCHEME_STRTAG_VAL(p[<s>]);
@MACRO checkStringLen[i.s] = if ((x<i> < 0) || (x<i> > SCHEME_STRTAG_VAL(p[<s>]))) WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("text%","insert"), "bad string length: ", p[<i>]));

@ "insert" : void Insert(-long,string,nnlong,nnls[same]=-1,bool=TRUE);  : : /setStringLen[0.0] <> string and position
@ "insert" : void Insert(-long,string);  : : /setStringLen[0.0] <> string without position
@ "insert" : void Insert(nnlong,string,nnlong,nnls[same]=-1,bool=TRUE);  : : /checkStringLen[0.1] <> length and string without position
@ "insert" : void Insert(nnlong,string);  : : /checkStringLen[0.1] <> length, string, and position
@ "insert" : void Insert(wxSnip!,nnlong,nnls[same]=-1,bool=TRUE); <> snip% and position
@ "insert" : void Insert(uchar); <> character without position
@ "insert" : void Insert(uchar,nnlong,nnls[same]=-1); <> character and position

@ "delete" : void Delete(nnls[start],nnls[back]=-1,bool=TRUE); <> position
@ "delete" : void Delete(); <> no position
@ "erase" :  void Erase();

@ "cut" :  void Cut(bool,ExactLong,nnls[start],nnls[end]=-1); <> position
@ "copy" : void Copy(bool,ExactLong,nnls[start],nnls[end]=-1); <> position
@ "paste" : void Paste(ExactLong,nnls[end],nnls[same]=-1); <> position
@ "paste-next" : void PasteNext();
@ "kill" : void Kill(ExactLong,nnlong,nnlong); <> position

@ v "do-copy" : void DoCopy(nnlong,nnlong,ExactLong,bool);
@ v "do-paste" : void DoPaste(nnlong,ExactLong);

@ "change-style" : void ChangeStyle(wxStyleDelta^,nnls[start],nnls[end]=-1); <> style-delta% and position
@ "change-style" : void ChangeStyle(wxStyle^,nnls[start]=-1,nnls[end]=-1); <> style%
			
@ "split-snip" : void SplitSnip(nnlong);

@ "find-position" : long FindPosition(float,float,bool?=NULL,bool?=NULL,float?=NULL);
@ "find-line" : long FindLine(float,bool?=NULL);
@ "find-position-in-line" : long FindPositionInLine(nnlong,float,bool?=NULL,bool?=NULL,float?=NULL);

@ "get-between-threshold" : float GetBetweenThreshold();
@ "set-between-threshold" : void SetBetweenThreshold(nnfloat);

@ "position-line" : long PositionLine(nnlong,bool=FALSE);
@ "position-location" :  void PositionLocation(nnlong,float?=NULL,float?=NULL,bool=TRUE,bool=FALSE,bool=FALSE);
@ "line-location" : float LineLocation(nnlong,bool=TRUE);

@ "line-start-position" : long LineStartPosition(nnlong,bool=TRUE);
@ "line-end-position" : long LineEndPosition(nnlong,bool=TRUE);
@ "line-length" : long LineLength(nnlong);
@ "last-position" : long LastPosition();
@ "last-line" : long LastLine();

@ "position-paragraph" : long PositionParagraph(nnlong,bool=FALSE);
@ "paragraph-start-position" : long ParagraphStartPosition(nnlong,bool=TRUE);
@ "paragraph-end-position" : long ParagraphEndPosition(nnlong,bool=TRUE);
@ "line-paragraph" : long LineParagraph(nnlong);
@ "paragraph-start-line" : long ParagraphStartLine(nnlong);
@ "paragraph-end-line" : long ParagraphEndLine(nnlong);
@ "last-paragraph" : long LastParagraph();

@ "set-paragraph-margins" : void SetParagraghMargins(nnlong,nnfloat,nnfloat,nnfloat);
@ "set-paragraph-alignment" : void SetParagraghAlignment(nnlong,SYM[horizontalAlignment]);

@ "get-line-spacing" : nnfloat GetLineSpacing();
@ "set-line-spacing" : void SetLineSpacing(nnfloat);

@ "get-styles-sticky" : bool GetStickyStyles();
@ "set-styles-sticky" : void SetStickyStyles(bool);

@MACRO bNegAsFalse = (({x} < 0) ? scheme_false : scheme_make_integer({x}))

@ "find-string" : long/bNegAsFalse FindString(string,SYM[direction]=1,nnls[start]=-1,nnls[eof]=-1,bool=TRUE,bool=TRUE);

@SET TYPE = long
@SET NOTEST = 1
@INCLUDE list.xci

@ "find-string-all" : long[]/bReturnList[long.1] FindStringAll(string,-long*,SYM[direction]=1,nnls[start]=-1,nnls[eof]=-1,bool=TRUE,bool=TRUE);

@ "find-snip" : wxSnip^ FindSnip(nnlong,SYM[findKind],nnlong?=NULL)
@ "get-snip-position-and-location" : bool GetSnipPositionAndLocation(wxSnip!,nnlong?,float?=NULL,float?=NULL);
@ "get-snip-position" : long/bNegAsFalse GetSnipPosition(wxSnip!);

@MACRO makeNoCopyString[len] = WITH_VAR_STACK(scheme_make_sized_string(r, <len>, 0))

@ "get-text" : string/makeNoCopyString[_x4] GetText(nnlong=0,nnls[eof]=-1,bool=FALSE,bool=FALSE,-long*=NULL);
@ "get-character" : uchar GetCharacter(nnlong);

@ "read-from-file" : bool ReadFromFile(wxMediaStreamIn!,nnls[start],bool=FALSE); <> with position
@ "write-to-file" : bool WriteToFile(wxMediaStreamOut!,nnlong,nnls[eof]=-1); <> with position

@ "get-file-format" : SYM[fileType] GetFileFormat();
@ "set-file-format" : void SetFileFormat(SYM[fileType]);

@ "get-overwrite-mode" : bool GetOverwriteMode();
@ "set-overwrite-mode" : void SetOverwriteMode(bool);

@MACRO checkNull = if (!x0) x0 = &_x0;

@ "get-tabs" : float[]/bReturnList[float.0]///push GetTabs(nnint?=NULL,float?=NULL,bool?=NULL); : : /checkNull/
@ "set-tabs" : void SetTabs(float[]/bList/ubList/cList///push,-int,float=wxTAB_WIDTH,bool=TRUE); : : /glueListSet[float.0.0.1.METHODNAME("text%","set-tabs")]//

@ v "can-insert?" : bool CanInsert(nnlong,nnlong);
@ v "on-insert" : void OnInsert(nnlong,nnlong);
@ v "after-insert" : void AfterInsert(nnlong,nnlong);
@ v "can-delete?" : bool CanDelete(nnlong,nnlong);
@ v "on-delete" : void OnDelete(nnlong,nnlong);
@ v "after-delete" : void AfterDelete(nnlong,nnlong);
@ v "can-change-style?" : bool CanChangeStyle(nnlong,nnlong);
@ v "on-change-style" : void OnChangeStyle(nnlong,nnlong);
@ v "after-change-style" : void AfterChangeStyle(nnlong,nnlong);
@ v "after-set-position" : void AfterSetPosition();
@ v "can-set-size-constraint?" : bool CanSetSizeConstraint();
@ v "on-set-size-constraint" : void OnSetSizeConstraint();
@ v "after-set-size-constraint" : void AfterSetSizeConstraint();

@ v "get-region-data" : wxBufferData^ GetRegionData(nnlong,nnlong);
@ v "set-region-data" : void SetRegionData(nnlong,nnlong,wxBufferData^);

@ "find-wordbreak" : void FindWordbreak(nnlong?,nnlong?,SYM[breakType]);

@ "set-wordbreak-map" : void SetWordbreakMap(wxMediaWordbreakMap^);
@ "get-wordbreak-map" : wxMediaWordbreakMap^ GetWordbreakMap();

@ "hide-caret" : void HideCaret(bool);
@ "caret-hidden?" : bool CaretHidden();

@ v "on-new-string-snip" : wxTextSnip! OnNewTextSnip();
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

@ "set-wordbreak-func" : void SetWordbreakFunc(wxWordbreakFunc//ubCallback/cCallback//spCallback/nopush,-unknown#void*//ubData);

@ "set-clickback" : void SetClickback(nnlong,nnlong,wxClickbackFunc//ubCallback2/cCallback2//spCallback2/nopush,-unknown#void*//ubData2,wxStyleDelta^=NULL,bool=FALSE);
@ "remove-clickback" : void RemoveClickback(nnlong,nnlong);
@ "call-clickback" : void CallClickback(nnlong,nnlong);

static void WordbreakCallbackToScheme(wxMediaEdit *media,
				      long *start, long *end,
				      int reason,
				      Scheme_Object *f)
{
    Scheme_Object *p[4], *s, *e;
    SETUP_VAR_STACK(8);
    VAR_STACK_PUSH_ARRAY(0, p, 4);
    VAR_STACK_PUSH(3, s);
    VAR_STACK_PUSH(4, e);
    VAR_STACK_PUSH(5, start);
    VAR_STACK_PUSH(6, end);
    VAR_STACK_PUSH(7, f);

    p[0] = WITH_VAR_STACK(objscheme_bundle_wxMediaEdit(media));
    if (start)
      s = WITH_VAR_STACK(scheme_box(WITH_VAR_STACK(objscheme_bundle_integer(*start))));
    else
      s = XC_SCHEME_NULL;
    if (end)
      e = WITH_VAR_STACK(scheme_box(WITH_VAR_STACK(objscheme_bundle_integer(*end))));
    else
      e = XC_SCHEME_NULL;
    p[1] = s;
    p[2] = e;
    p[3] = WITH_VAR_STACK(bundle_symset_breakType(reason));

    WITH_VAR_STACK(scheme_apply_multi(f, 4, p));
    if (start)
      *start = WITH_VAR_STACK(objscheme_unbundle_integer(WITH_VAR_STACK(scheme_unbox(s)), "Scheme wordbreak callback"));
    if (end)
      *end = WITH_VAR_STACK(objscheme_unbundle_integer(WITH_VAR_STACK(scheme_unbox(e)), "Scheme wordbreak callback"));
}

static void ClickbackToScheme(wxMediaEdit *media,
			      long start, long end,
			      Scheme_Object *f)
{
  Scheme_Object *p[3];
  SETUP_VAR_STACK(4);
  VAR_STACK_PUSH_ARRAY(0, p, 3);
  VAR_STACK_PUSH(3, f);

  p[0] = WITH_VAR_STACK(objscheme_bundle_wxMediaEdit(media));
  p[1] = WITH_VAR_STACK(objscheme_bundle_integer(start));
  p[2] = WITH_VAR_STACK(objscheme_bundle_integer(end));

  WITH_VAR_STACK(scheme_apply_multi(f, 3, p));
}

@END
