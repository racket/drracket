/* Main header file for wxMedia */
/* Include this file for anything using wxMedia */

#ifndef __WX_MEDIA__
#define __WX_MEDIA__

#include "wx_panel.h"
#include "wx_canvs.h"
#include "wx_dcmem.h"
#include "wx_keym.h"
#include "wx_medio.h"
#include "wx_style.h"
#include "wx_mtype.h"

class wxMediaEdit;
class wxClickback;

#define wxFOCUS_IMMEDIATE 0
#define wxFOCUS_DISPLAY 1
#define wxFOCUS_GLOBAL 2

/* File formats */
enum {
  wxMEDIA_FF_GUESS,
  wxMEDIA_FF_STD,
  wxMEDIA_FF_TEXT,
  wxMEDIA_FF_TEXT_FORCE_CR,
  wxMEDIA_FF_SAME,
  wxMEDIA_FF_COPY
};

#include "wx_snip.h"
#include "wx_cgrec.h"
#include "wx_medad.h"

class wxSnipLocation;
class wxMediaFlashTimer;

class wxBitmap;

/* Edit commands */
enum {
  wxEDIT_UNDO = 1,
  wxEDIT_REDO,
  wxEDIT_CLEAR,
  wxEDIT_CUT,
  wxEDIT_COPY,
  wxEDIT_PASTE,
  wxEDIT_KILL,
  wxEDIT_INSERT_TEXT_BOX,
  wxEDIT_INSERT_GRAPHIC_BOX,
  wxEDIT_INSERT_IMAGE,
  wxEDIT_SELECT_ALL,
  _wx_EDIT_counter
};

/* Movement kinds */
enum {
  wxMOVE_SIMPLE = 1,
  wxMOVE_LINE,
  wxMOVE_PAGE,
  wxMOVE_WORD
};

/* For Finding Wordbreaks: */
enum {
  wxBREAK_FOR_CARET = 1,
  wxBREAK_FOR_LINE = 2,
  wxBREAK_FOR_SELECTION = 4,
  wxBREAK_FOR_USER_1 = 32,
  wxBREAK_FOR_USER_2 = 64
};

/* Selection: */
enum {
  wxDEFAULT_SELECT = 0,
  wxX_SELECT = 1,
  wxLOCAL_SELECT = 2
};

/* Drawing: */
enum {
  wxSNIP_DRAW_NO_CARET = 0,
  wxSNIP_DRAW_SHOW_INACTIVE_CARET,
  wxSNIP_DRAW_SHOW_CARET
};


/* For FindSnip: */
#define wxSNIP_BEFORE_OR_NULL (-2)
#define wxSNIP_BEFORE (-1)
#define wxSNIP_AFTER 1
#define wxSNIP_AFTER_OR_NULL 2

#define wxTAB_WIDTH 20

typedef void (*wxClickbackFunc)(wxMediaEdit *, long start, long end, void *);
typedef void (*wxWordbreakFunc)(wxMediaEdit *, long *start, long *end, 
				int reason, void *);

class wxMediaAdmin;


class wxMediaWordbreakMap : public wxObject
{
  int usage;
 public:
  char map[256];

  wxMediaWordbreakMap();

  void SetMap(int ch, int mask);
  int GetMap(int ch);

  void AdjustUsage(Bool newUser);
  Bool IsUsed(void);
};

extern wxMediaWordbreakMap wxTheMediaWordbreakMap;

class wxMediaEdit : public wxMediaBuffer
{
  friend class wxMediaLine;
  friend class wxMediaSnipMediaAdmin;
 public:
  wxMediaEdit(float lineSpacing = 1.0, 
	      float *tabstops = NULL, int numtabs = 0);
  ~wxMediaEdit();

  wxMediaBuffer *CopySelf(void);

  /* Usually called by wxMediaAdmin */
  virtual void OnEvent(wxMouseEvent &event);
  virtual void OnChar(wxKeyEvent &event);
  virtual wxCursor *AdjustCursor(wxMouseEvent &event);
  virtual void Refresh(float localx, float localy, float w, float h, 
		       int show_caret);
  virtual void OwnCaret(Bool ownit);
  virtual void SizeCacheInvalid(void);

  virtual void OnDefaultEvent(wxMouseEvent &event);
  virtual void OnDefaultChar(wxKeyEvent &event);

  /* Callbacks for the wxSnipAdmin: */
  virtual Bool ScrollTo(wxSnip *, float localx, float localy, 
			float w, float h, Bool refresh, int bias = 0);
  virtual void Resized(wxSnip *, Bool redraw_now);
  virtual Bool Recounted(wxSnip *, Bool redraw_now);
  virtual void NeedsUpdate(wxSnip *, float localx, float localy, 
			   float w, float h);
  virtual void SetCaretOwner(wxSnip *, int = wxFOCUS_IMMEDIATE);
  virtual Bool ReleaseSnip(wxSnip *);

  /* Methods for you to override: */
  virtual void OnChange(void);
  virtual Bool OnInsert(long start, long len);
  virtual void AfterInsert(long start, long len);
  virtual Bool OnDelete(long start, long len);
  virtual void AfterDelete(long start, long len);
  virtual Bool OnChangeStyle(long start, long len);
  virtual void AfterChangeStyle(long start, long len);
  virtual void AfterSetPosition(void);
  virtual Bool OnSetSizeConstraint(void);
  virtual void AfterSetSizeConstraint(void);

  /* Set the caret position: */
  void GetPosition(long *start, long *end = NULL);
  long GetStartPosition(void);
  long GetEndPosition(void);
  void SetPosition(long start, long end = -1, 
		   Bool eol = FALSE, Bool scroll = TRUE,
		   int seltype = wxDEFAULT_SELECT);
  void MovePosition(long code, Bool extend = FALSE,
		    int kind = wxMOVE_SIMPLE);
  void SetPositionBiasScroll(int bias, long start, long end = -1, 
			     Bool eol = FALSE, Bool scroll = TRUE,
			     int seltype = wxDEFAULT_SELECT);

  virtual void SetAnchor(Bool);
  Bool GetAnchor(void);

  Bool ScrollToPosition(long start, Bool ateol = FALSE, 
			long end = -1, int bias = 0);
  void GetVisiblePositionRange(long *start, long *end);
  void GetVisibleLineRange(long *start, long *end);

  /* Hilite a region without changing the selection position: */
  void FlashOn(long start, long end, Bool ateol = FALSE, 
	       Bool scroll = TRUE, long timeout = 500);
  void FlashOff();

  /* Change the text */
  void Insert(char *str, long start, long end = -1, Bool scrollOk = TRUE);
  void Insert(char *str);
  void Insert(long len, char *str, long start, long end = -1, Bool scrollOk=TRUE);
  void Insert(long len, char *str);
  void Insert(wxSnip *snip, long start, long end = -1, Bool scrollOk=TRUE);
  void Insert(wxSnip *snip);
  void Insert(unsigned char ascii);
  void Insert(unsigned char ascii, long start, long end = -1);
  void Delete(long start, long end = -1, Bool scrollOk = TRUE);
  void Delete();
  void Erase();

  void Cut(Bool extend, long time, long start, long end = -1);
  void Cut(Bool extend = FALSE, long time = 0);
  void Copy(Bool extend, long time, long start, long end = -1);
  void Copy(Bool extend = FALSE, long time = 0);
  void Paste(long time, long start, long end = -1);
  void Paste(long time = 0);
  void PasteNext(void);
  void Kill(long time = 0);
  void Kill(long time, long start, long end);
  void Clear(void);
  void SelectAll(void);

  virtual void DoCopy(long start, long end, long time, Bool extend);
  virtual void DoPaste(long start, long time);

  /* For making a lot of changes to be displayed at once: */
  void BeginEditSequence(Bool undoable = TRUE);
  void EndEditSequence(void);
  Bool RefreshDelayed(void);

  void ChangeStyle(wxStyleDelta *);
  void ChangeStyle(wxStyle *, long start = -1, long end = -1);
  void ChangeStyle(wxStyleDelta *, long start, long end = -1);
  /* Called automatically when a style is changed; no need to call this */
  void StyleHasChanged(wxStyle *style);

  void SetStyleList(wxStyleList *styles);

  /* Convert canvas co-ordinates to caret position */
  long FindPosition(float x, float y, Bool 
		    *ateol = NULL, Bool *onit = NULL,
		    float *how_close = NULL);
  long FindLine(float y, Bool *onit = NULL);
  long FindPositionInLine(long i, float x, 
			  Bool *ateol = NULL, Bool *onit =NULL,
			  float *how_close = NULL);

  /* Find the line or canvas co-ordinates for a caret position */
  long PositionLine(long start, Bool eol=FALSE);
  void PositionLocation(long start, 
			float *x = NULL, float *y = NULL, 
			Bool front = TRUE, Bool eol = FALSE,
			Bool wholeLine = FALSE);
  float LineLocation(long line, Bool top = TRUE);

  /* Get first/last caret position in a line: */
  long LineStartPosition(long i, Bool visibleOnly = TRUE);
  long LineEndPosition(long i, Bool visibleOnly = TRUE);
  long LineLength(long i);

  /* Paragraphs */
  long PositionParagraph(long start, Bool eol=FALSE);
  long ParagraphStartPosition(long i, Bool visibleOnly = TRUE);
  long ParagraphEndPosition(long i, Bool visibleOnly = TRUE);
  long LineParagraph(long line);
  long ParagraphStartLine(long i);
  long ParagraphEndLine(long i);

  long LastPosition(void);
  long LastLine(void);
  long LastParagraph(void);
  
  void GetExtent(float *w, float *h);
  float GetDescent(void);
  float GetSpace(void);

  float ScrollLineLocation(long line);
  long NumScrollLines();
  long FindScrollLine(float y);

  /* Searching */
  long FindString(char *str, int direction = 1, long start =-1, long end =-1,
		  Bool bos = TRUE, Bool caseSens = TRUE);
  long *FindStringAll(char *str, long *cnt, int direction = 1,
		      long start =-1, long end =-1, Bool bos = TRUE,
		      Bool caseSens = TRUE);
  long FindNewline(int direction = 1, long start =-1, long end =-1);

  /* Create clickable ranges: */
  void SetClickback(long start, long end, wxClickbackFunc, void *,
		    wxStyleDelta *hiliteDelta = NULL, Bool callOnDown = FALSE);
  void RemoveClickback(long start, long end);

  void SetClickback(wxClickback *); /* Used by undo record only */

  wxSnip *FindSnip(long p, int direction, long *sPos = NULL);
  Bool GetSnipPositionAndLocation(wxSnip *thesnip, long *pos, 
				  float *x = NULL, float *y = NULL);
  Bool GetSnipLocation(wxSnip *thesnip, float *x = NULL, float *y = NULL,
		       Bool bottomRight=FALSE);
  long GetSnipPosition(wxSnip *thesnip);

  char *GetFlattenedText(long *len = NULL);
  char *GetText(long start = -1, long end = -1, 
		Bool flattened = FALSE, Bool forceCR = FALSE,
		long *got = NULL);
  unsigned char GetCharacter(long start);

  Bool LoadFile(char *filename = NULL, int format = wxMEDIA_FF_GUESS, Bool showErrors = TRUE);
  Bool SaveFile(char *filename = NULL, int format = wxMEDIA_FF_SAME, Bool showErrors = TRUE);
  Bool InsertFile(char *filename, int format = wxMEDIA_FF_GUESS, Bool showErrors = TRUE);

  Bool ReadFromFile(wxMediaStreamIn &, long start, Bool overwritestyle = FALSE);
  Bool ReadFromFile(wxMediaStreamIn &, Bool overwritestyle = FALSE);
  Bool WriteToFile(wxMediaStreamOut &, long start, long end = -1);
  Bool WriteToFile(wxMediaStreamOut &);

  void SetFilename(char *, Bool temp = FALSE);
  int GetFileFormat(void);
  void SetFileFormat(int);

  void SplitSnip(long pos);

  float *GetTabs(int *length = NULL, 
		 float *tabInc = NULL, Bool *inUnits = NULL);
  void SetTabs(float *tabs, int count, 
	       float tabIncrement = wxTAB_WIDTH, Bool inUnits = TRUE);

  void AddEditorFunctions(wxKeymap *keymap);

  void SetWordbreakFunc(wxWordbreakFunc f, void *data);
  void FindWordbreak(long *start, long *end, int reason);

  wxMediaWordbreakMap *GetWordbreakMap(void);
  void SetWordbreakMap(wxMediaWordbreakMap *map);

  virtual void PrintToDC(wxDC *dc, int page = -1);
  virtual void *BeginPrint(wxDC *, Bool);
  virtual void EndPrint(wxDC *, void *);
  virtual Bool HasPrintPage(wxDC *dc, int page);

  void SetMaxWidth(float w);
  void SetMinWidth(float w);
  float GetMaxWidth();
  float GetMinWidth();
  void SetMinHeight(float h);
  void SetMaxHeight(float w);
  float GetMinHeight();
  float GetMaxHeight();

  virtual void InvalidateBitmapCache(float x=0.0, float y=0.0,
				     float w=-1.0, float h=-1.0);

  /* You might need to call this if you're doing text changes within
     BeginEditSequence() and EndEditSequence(). Hopefully, it will
     get called automatically. */
  void Recalculate(void);

  void SettingAdmin(wxMediaAdmin *);
  void InitNewAdmin(void);

  virtual wxBufferData *GetRegionData(long start, long end);
  virtual void SetRegionData(long start, long end, wxBufferData *);

  void PasteRegionData(wxBufferData *);

  virtual wxTextSnip *OnNewTextSnip();
  virtual wxTabSnip *OnNewTabSnip();

  wxBitmap *SetAutowrapBitmap(wxBitmap *bm);

  void HideCaret(Bool hide);
  Bool CaretHidden(void);

  float GetBetweenThreshold();
  void SetBetweenThreshold(float thresh);

  inline Bool GetOverwriteMode(void) { return overwriteMode; }
  inline void SetOverwriteMode(Bool m) { overwriteMode = !!m; }

#if ALLOW_X_STYLE_SELECTION
  virtual Bool OwnXSelection(Bool on, Bool update, Bool force);
#endif

#ifdef MEMORY_USE_METHOD
  virtual long MemoryUse(void);
#endif

 private:
#define TF_Flag(var) unsigned var : 1

  TF_Flag( readLocked );
  TF_Flag( flowLocked );
  TF_Flag( writeLocked );

  TF_Flag( hiliteOn );

  TF_Flag( changed ); /* Set if OnChange() needs to be called */

  TF_Flag( flash );
  TF_Flag( flashautoreset );
  TF_Flag( flashdirectoff );

  TF_Flag( posateol ); /* display the caret at the end of a line? */
  TF_Flag( flashposateol );
  TF_Flag( flashscroll ); /* Scroll back after unflashing? */

  TF_Flag( graphicsInvalid );
  TF_Flag( flowInvalid );
  TF_Flag( snipCacheInvalid );
  TF_Flag( graphicMaybeInvalid );
  TF_Flag( graphicMaybeInvalidForce );

  TF_Flag( typingStreak );
  TF_Flag( deletionStreak );
  TF_Flag( delayedStreak );
  TF_Flag( vcursorStreak );
  TF_Flag( killStreak );
  TF_Flag( anchorStreak );
  TF_Flag( insertForceStreak );
  TF_Flag( deleteForceStreak );

  TF_Flag( keepAnchorStreak );

  TF_Flag( dragging );
  TF_Flag( tracking );
  TF_Flag( extraLine ); /* Empty line at end of file with no representative */

  TF_Flag( delayedscrollateol );
  TF_Flag( delayedscrollbox );
  TF_Flag( caretOn );
  TF_Flag( drawCachedInBitmap );
  TF_Flag( refreshUnset );
  TF_Flag( refreshBoxUnset );
  TF_Flag( refreshAll );

  TF_Flag( tabSpaceInUnits );
  TF_Flag( overwriteMode );

#if ALLOW_X_STYLE_SELECTION
  TF_Flag( needXCopy );
#endif

#undef TF_Flag

  int lastDrawCaret;
  int lastDrawXSel;

  float lineSpacing;
  float maxWidth, minWidth, minHeight, maxHeight;
  float wrapBitmapWidth;

  wxBitmap *autoWrapBitmap;


  int delayRefresh;

  long len; /* Total length in "characters" == number of positions - 1 */

  long startpos, endpos;
  float startloc, endloc; /* for vcursorStreak */

  wxMediaFlashTimer *flashTimer;
  long flashstartpos, flashendpos;

  wxSnip *snips, *lastSnip; /* The contents of this edit session */
  long snipCount;

  wxStandardSnipAdmin *snipAdmin;
  
  class wxMediaLine *lineRoot, *firstLine, *lastLine; /* Line information */
  long numValidLines;

  float extraLineH;

  float totalHeight, totalWidth; /* Total height/width in canvas units */
  float finalDescent; /* Descent of last line */
  float initialSpace; /* Space from first line */

  wxStyle *caretStyle;

  long dragstart;

  wxClickback *trackClickback;

  long refreshStart, refreshEnd;
  float refreshL, refreshT, refreshR, refreshB;

  float lastDrawL, lastDrawT, lastDrawR, lastDrawB;

  float caretLocationX, caretLocationT, caretLocationB;

  long delayedscroll, delayedscrollend;
  int delayedscrollbias;
  wxSnip *delayedscrollsnip;
  float delayedscrollX, delayedscrollY, delayedscrollW, delayedscrollH;

  wxList *clickbacks;

  int fileFormat;

  float betweenThreshold;

  float *tabs;
  int tabcount;
  float tabSpace;
  
  long readInsert, readInsertStart;

  long prevPasteStart, prevPasteEnd;

  wxWordbreakFunc wordBreak;
  void *wordBreakData;

  wxMediaWordbreakMap *wordBreakMap;

  void _Insert(wxSnip *snip, long len, char *str, long start, long end = -1, 
	       Bool scrollOk = TRUE);
  void _Delete(long start, long end, Bool undo, Bool scrollOk);

  void _SetPosition(Bool setflash, int bias, long start, long end, 
		    Bool ateol, Bool scroll, int seltype);

  void _ChangeStyle(long start, long end, wxStyle *, wxStyleDelta *);

  void MakeOnlySnip(void);
  void SpliceSnip(wxSnip *snip, wxSnip *prev, wxSnip *next);
  void InsertSnip(wxSnip *before, wxSnip *snip);
  void AppendSnip(wxSnip *snip);
  void DeleteSnip(wxSnip *snip);
  wxSnip *SnipSetAdmin(wxSnip *snip, wxSnipAdmin *a);
  void SnipSplit(wxSnip *snip, long pos, wxSnip **a, wxSnip **b);

  void MakeSnipset(long start, long end);
  wxTextSnip *InsertTextSnip(long start, wxStyle *style = NULL);
  void CheckMergeSnips(long start);

  void AdjustClickbacks(long start, long end, long d, wxDeleteRecord *rec);
  wxClickback *FindClickback(long start);
  void SetClickbackHilited(wxClickback *, Bool);

  Bool ScrollToPosition(long start, Bool ateol, Bool refresh, long end,
			int bias);

  long FindFirstVisiblePosition(wxMediaLine *line, wxSnip *snip = NULL);
  void FindLastVisiblePosition(wxMediaLine *line, long *p,
			       wxSnip **snipP = NULL);

  long _FindPositionLine(long pos);
  void _CalcValidPositionLine(void);

  long _FindPositionInLine(Bool internal, long i, float x, 
			   Bool *ateol = NULL, Bool *onit =NULL,
			   float *how_close = NULL);
  long _FindPositionInSnip(wxDC *dc, float X, float Y,
			   wxSnip *snip, float x, float *how_close = NULL);

  void LinesInvalidAfter(long);
  void OneLineInvalid(long);
  void SnipChangedAt(long);

  long _FindStringAll(char *str, int direction,
		      long start, long end, long **positions, 
		      Bool, Bool, Bool);
  
  Bool InsertFile(FILE *f, char *filename, int& format, Bool clearStyles, Bool showErrors);

  void RecalcLines(wxDC *dc, Bool calcGraphic = TRUE);
  Bool CheckFlow(float maxw, wxDC *dc, float Y, long startp, wxSnip *start);
  Bool CheckRecalc(Bool need_graphic = TRUE, Bool need_write = TRUE, Bool no_display_ok = FALSE);
  void Redraw(wxDC *, float, float, float, float, float, float, int, int);
  void Redraw();

  void NeedRefresh(long start, long end = -1);
  void NeedRefresh(float, float, float, float);
  void RefreshByLineDemand(void);
  void RefreshBox(float x, float y, float w, float h);

  void NeedCaretRefresh(void);
  void CaretOn(void);
  Bool CaretOff(void);
  void CalcCaretLocation(void);
  
  void EndStreaks(int exception = 0);
 protected:
  Bool ReadInsert(wxSnip *snip);
  void InsertPasteSnip(wxSnip *snip, wxBufferData *);
  void InsertPasteString(char *str);
};

#include "wx_medpb.h"

void wxInitMedia(void);
void wxAddMediaEditorFunctions(wxKeymap *tab);
void wxAddMediaPasteboardFunctions(wxKeymap *tab);

extern const char *(*wxmeExpandFilename)(const char *);

#endif /* __WX_MEDIA__ */

