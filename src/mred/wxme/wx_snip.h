
#ifndef __WX_SNIP__
#define __WX_SNIP__

class wxSnipAdmin;

/* The default is that none of these flags are on: */
enum {
  wxSNIP_IS_TEXT = 0x1,
  wxSNIP_CAN_APPEND = 0x2,
  wxSNIP_INVISIBLE = 0x4,
  wxSNIP_NEWLINE = 0x8, /* Soft newline, typically inserted by wxMediaEdit */
  wxSNIP_HARD_NEWLINE = 0x10, /* => Snip must be follwed by newline */
  wxSNIP_HANDLES_EVENTS = 0x20,
  wxSNIP_WIDTH_DEPENDS_ON_X = 0x40,
  wxSNIP_HEIGHT_DEPENDS_ON_Y = 0x80,
  wxSNIP_WIDTH_DEPENDS_ON_Y = 0x100,
  wxSNIP_HEIGHT_DEPENDS_ON_X = 0x200,
  wxSNIP_ANCHORED = 0x400,
  wxSNIP_USES_BUFFER_PATH = 0x800,
  wxSNIP_CAN_SPLIT = 0x1000, /* safety feature */
  wxSNIP_OWNED = 0x2000,
  wxSNIP_CAN_DISOWN = 0x4000
};

#define WRITE_FUNC \
   Bool wxmbWriteSnipsToFile(class wxMediaStreamOut &, \
			     class wxStyleList *, \
			     class wxList *, class wxSnip *, \
			     class wxSnip *, wxList *, \
			     class wxMediaBuffer *)

class wxSnip;
extern WRITE_FUNC;

class wxSnipClass : public wxObject
{
  friend class wxStandardSnipClassList;
  friend class wxSnipClassList;
  friend class wxMediaBuffer;
  friend WRITE_FUNC;

  /* Special fields just for wxStandardSnipClassList & wxMediaEdit */
  Bool headerFlag;
  short mapPosition;
  int readingVersion;

 public:
  char *classname;
  int version;

  /* If this flag is TRUE, then files saved to disk will be written
     assuming that, when the file is read from disk, the class will
     always be present. */
  Bool required;
  
  wxSnipClass();

  virtual class wxSnip *Read(wxMediaStreamIn &) = 0;

  virtual Bool ReadHeader(wxMediaStreamIn &);
  virtual void ReadDone(void);

  virtual Bool WriteHeader(wxMediaStreamOut &);
  virtual void WriteDone(void);
};

class wxSnipClassList : public /* should be private */ wxList
{
 public:
  wxSnipClassList(void);
  ~wxSnipClassList();

  wxSnipClass *Find(char *name);
  short FindPosition(wxSnipClass *);
  int ReadingVersion(wxSnipClass *);
  void Add(wxSnipClass *snipclass); /* Checks for duplicates */
  int Number(void);
  wxSnipClass *Nth(int);
};

enum {
  wxRESET_NO_MSG = 0,
  wxRESET_DONE_READ,
  wxRESET_DONE_WRITE
};

class wxStandardSnipClassList : public wxSnipClassList
{
 private:
  wxList *unknowns;

 public:
  wxStandardSnipClassList(void);

  void ResetHeaderFlags(int doneMsg = wxRESET_NO_MSG);
  Bool Write(wxMediaStreamOut &f);
  Bool Read(wxMediaStreamIn &f);
  wxSnipClass *FindByMapPosition(short n);
};

#ifdef WXME_FOR_MRED
extern wxStandardSnipClassList *wxMakeTheSnipClassList();
extern wxStandardSnipClassList *wxGetTheSnipClassList();
#define wxTheSnipClassList (*wxGetTheSnipClassList())
#else
extern wxStandardSnipClassList wxTheSnipClassList;
#endif

/******************************************************************/

class wxSnip : public wxObject
{
 private:
  friend class wxMediaEdit;
  friend class wxMediaPasteboard;
  friend class wxMediaBuffer;
  friend class wxMediaLine;
  friend WRITE_FUNC;

  /* For use only by the owning wxMediaBuffer */
  wxSnip *prev, *next;
  class wxMediaLine *line;

  void Init(void);

 protected:
  wxSnipAdmin **admin_ptr;

 public:
  long count;
  long flags;
  wxSnipClass *snipclass;
  wxStyle *style;

  wxSnip();
  wxSnip(Bool cleanup);
  ~wxSnip();

  wxSnip *Next(void);
  wxSnip *Previous(void);

  wxSnipAdmin *GetAdmin(void);
  virtual void SetAdmin(wxSnipAdmin *);

  Bool IsOwned(void);
  Bool ReleaseFromOwner(void);

  void SetCount(long count);
  void SetFlags(long flags);

  virtual void OnEvent(wxDC *dc, float x, float y, 
		       float mediax, float mediay, 
		       wxMouseEvent &event);
  virtual void OnChar(wxDC *dc, float x, float y, 
		      float mediax, float mediay, 
		      wxKeyEvent &event);
  virtual wxCursor *AdjustCursor(wxDC *dc, float x, float y, 
				 float mediax, float mediay, 
				 wxMouseEvent &event);
  virtual void OwnCaret(Bool);  
  
  virtual void DoEdit(int op, Bool recursive = TRUE, long time = 0);
  virtual void DoFont(int op, Bool recursive = TRUE);

  virtual int Match(wxSnip *other); 

  virtual void SizeCacheInvalid(void);

  virtual void GetExtent(wxDC *dc, 
			 float x, float y,
			 float *w = NULL, float *h = NULL, 
			 float *descent = NULL, float *space = NULL,
			 float *lspace = NULL, float *rspace = NULL);
  virtual float PartialOffset(wxDC *, float x, float y, long);
  virtual void Draw(wxDC *dc, float x, float y, 
		    float,float,float,float, 
		    float dx, float xy, 
		    int show_caret);
  virtual void Split(long position, wxSnip **first, wxSnip **second);
  virtual wxSnip *MergeWith(wxSnip *pred);
  virtual void GetText(char *s, long offset, long num);
  virtual char *GetText(long offset, long num, Bool flattened = FALSE, long *got=NULL);
  virtual wxSnip *Copy();
  virtual void Write(wxMediaStreamOut &f);

  virtual Bool Resize(float w, float h);

  virtual long GetNumScrollSteps();
  virtual long FindScrollStep(float y);
  virtual float GetScrollStepOffset(long i);

  void SetStyle(wxStyle *s);

 protected:
  void Copy(wxSnip *); /* Copy basic values into given snip */
};

class wxTextSnip : public wxSnip
{
 protected:
  float w; /* < 0 => need to recalc size */

  void GetTextExtent(wxDC *dc, int count, float *wo);

 public:
  char *text;
  char *buffer;

  long allocated;

  wxTextSnip(long allocsize = 0); 
  ~wxTextSnip(); 

  virtual void SizeCacheInvalid(void);

  virtual void GetExtent(wxDC *dc,
			 float x, float y,
			 float *w = NULL, float *h = NULL, 
			 float *descent = NULL, float *space = NULL,
			 float *lspace = NULL, float *rspace = NULL);
  virtual float PartialOffset(wxDC *, float x, float y, long);
  virtual void Draw(wxDC *dc, float x, float y, 
		    float,float,float,float, 
		    float dx, float dy, int);
  virtual void Split(long position, wxSnip **first, wxSnip **second);
  virtual wxSnip *MergeWith(wxSnip *pred);

  virtual void Insert(char *str, long len, long pos = 0);
  virtual void Read(long len, wxMediaStreamIn &f);
  virtual void GetText(char *s, long offset, long num);
  virtual char *GetText(long offset, long num, Bool flattened = FALSE, long *got=NULL);
  virtual wxSnip *Copy(void);
  virtual void Write(wxMediaStreamOut &f);


#ifdef MEMORY_USE_METHOD
  long MemoryUse(void);
#endif

 protected:
  void Copy(wxTextSnip *); /* Copy text values into given snip */
};

class wxTabSnip : public wxTextSnip
{
 public:
  wxTabSnip();
  
  virtual void GetExtent(wxDC *dc, 
			 float x, float y,
			 float *w = NULL, float *h = NULL, 
			 float *descent = NULL, float *space = NULL,
			 float *lspace = NULL, float *rspace = NULL);
  virtual float PartialOffset(wxDC *, float x, float y, long);
  virtual void Draw(wxDC *dc, float x, float y, 
		    float,float,float,float, 
		    float dx, float dy, int);
  virtual wxSnip *Copy();
};

class wxImageSnip : public wxSnip
{
 private:
  Bool userbm;
  char *filename;
  long filetype; /* file != NULL => type of file, otherwise loaded 1 => XBM and 2 => XPM */
  wxBitmap *bm;
  wxMemoryDC *offscreen;
  int *sharing;
  Bool relativePath;

 protected:
  float w, h, vieww, viewh, viewdx, viewdy;
  Bool contentsChanged;

 public:
  wxImageSnip(char *name = NULL, long type = 0, Bool relative = FALSE, Bool inlineImg = TRUE); 
  ~wxImageSnip(); 

  virtual void SizeCacheInvalid(void);

  virtual void GetExtent(wxDC *dc,
			 float x, float y,
			 float *w = NULL, float *h = NULL, 
			 float *descent = NULL, float *space = NULL,
			 float *lspace = NULL, float *rspace = NULL);

  virtual void Draw(wxDC *dc, float x, float y, 
		    float,float,float,float, 
		    float dx, float dy, int);

  virtual wxSnip *Copy(void);

  virtual void Write(wxMediaStreamOut &f);

  void LoadFile(char *name, long type, Bool relative = FALSE, Bool inlineImg = TRUE);
  char *GetFilename(Bool *relative);
  long GetFiletype();

  void SetBitmap(wxBitmap *);

  void SetOffset(float dx, float dy);
  virtual Bool Resize(float w, float h);

  virtual long GetNumScrollSteps();
  virtual long FindScrollStep(float y);
  virtual float GetScrollStepOffset(long i);

  virtual void SetAdmin(wxSnipAdmin *);

 protected:
  void Copy(wxImageSnip *);
};

class wxSnipAdmin : public wxObject
{
 public:
  virtual wxMediaBuffer *GetMedia(void) = 0;

  virtual wxDC *GetDC() = 0;
  virtual void GetViewSize(float *h, float *w) = 0;
  virtual void GetView(float *x, float *y, float *h, float *w, wxSnip *snip = NULL) = 0;
  virtual Bool ScrollTo(wxSnip *, float localx, float localy, 
			float w, float h, Bool refresh, int bias = 0) = 0;
  virtual void SetCaretOwner(wxSnip *, int = wxFOCUS_IMMEDIATE) = 0;
  virtual void Resized(wxSnip *, Bool redraw_now) = 0;
  virtual Bool Recounted(wxSnip *, Bool redraw_now) = 0;
  virtual void NeedsUpdate(wxSnip *, float localx, float localy, 
			   float w, float h) = 0;
  virtual Bool ReleaseSnip(wxSnip *) = 0;

  virtual void UpdateCursor() = 0;
};

#define wxMSNIPBOX_XMARGIN 5
#define wxMSNIPBOX_YMARGIN 5
#define wxMSNIPBOX_XINSET 1
#define wxMSNIPBOX_YINSET 1

class wxMediaSnip : public wxSnip
{
  friend class wxMediaSnipMediaAdmin;

  wxMediaBuffer *me;
  class wxMediaSnipMediaAdmin *myAdmin;

  Bool withBorder;
  int leftMargin, topMargin, rightMargin, bottomMargin;
  int leftInset, topInset, rightInset, bottomInset;

  float minWidth, maxWidth, minHeight, maxHeight;

 public:
  wxMediaSnip(wxMediaBuffer *useme = NULL,
	      Bool withBorder = TRUE,
	      int leftMargin = wxMSNIPBOX_XMARGIN,
	      int topMargin = wxMSNIPBOX_YMARGIN,
	      int rightMargin = wxMSNIPBOX_XMARGIN,
	      int bottomMargin = wxMSNIPBOX_YMARGIN,
	      int leftInset = wxMSNIPBOX_XINSET,
	      int topInset = wxMSNIPBOX_YINSET,
	      int rightInset = wxMSNIPBOX_XINSET,
	      int bottomInset = wxMSNIPBOX_YINSET,
	      float w = -1, float W = -1, 
	      float h = -1, float H = -1);
  ~wxMediaSnip();

  virtual void SetAdmin(wxSnipAdmin *a);

  virtual void OnEvent(wxDC *, float, float, float,float, wxMouseEvent &event);
  virtual void OnChar(wxDC *, float, float, float, float, wxKeyEvent &event);
  virtual wxCursor *AdjustCursor(wxDC *, float, float, float,float, wxMouseEvent &event);
  virtual void OwnCaret(Bool);  

  virtual void DoEdit(int op, Bool recursive = TRUE, long time = 0);
  virtual void DoFont(int op, Bool recursive = TRUE);

  virtual Bool Match(wxSnip *other); 

  virtual void SizeCacheInvalid(void);

  virtual char *GetText(long offset, long num, Bool flattened = FALSE, long *got = NULL);

  virtual void GetExtent(wxDC *dc, 
			 float x, float y,
			 float *w = NULL, float *h = NULL, 
			 float *descent = NULL, float *space = NULL,
			 float *lspace = NULL, float *rspace = NULL);
  virtual void Draw(wxDC *dc, float x, float y, 
		    float, float, float, float, float dx, float dy, 
		    int show_caret);
  virtual wxSnip *Copy(void);

  virtual void Write(wxMediaStreamOut &f);

  virtual long GetNumScrollSteps();
  virtual long FindScrollStep(float y);
  virtual float GetScrollStepOffset(long i);

  void SetMaxWidth(float);
  void SetMaxHeight(float);
  float GetMaxWidth(void);
  float GetMaxHeight(void);
  void SetMinWidth(float);
  void SetMinHeight(float);
  float GetMinWidth(void);
  float GetMinHeight(void);

  void ShowBorder(Bool show);
  Bool BorderVisible();

  void SetMargin(int lm, int tm, int rm, int bm);
  void GetMargin(int *lm, int *tm, int *rm, int *bm);
  void SetInset(int lm, int tm, int rm, int bm);
  void GetInset(int *li, int *ti, int *ri, int *bi);

  virtual Bool Resize(float w, float h);

  wxMediaBuffer *GetThisMedia(void);
  void SetMedia(wxMediaBuffer *b);
};

/**********************************************************************/

class wxMediaStreamOut;
class wxBufferData;

class wxBufferDataClass : public wxObject
{
 private:
  friend class wxBufferDataClassList;
  friend class wxMediaBuffer;
  friend WRITE_FUNC;
  friend Bool wxmbWriteBufferData(wxMediaStreamOut&, wxBufferData *data);

  int mapPosition;
 public:
  char *classname;
  Bool required;

  wxBufferDataClass();

  virtual class wxBufferData *Read(wxMediaStreamIn &) = 0;
};

class wxBufferData : public wxObject
{
 public:
  wxBufferDataClass *dataclass;
  wxBufferData *next; /* Used to chain them in a list */

  wxBufferData();
  ~wxBufferData();

  virtual Bool Write(wxMediaStreamOut &) = 0;
};

class wxBufferDataClassList : public /* should be private */ wxList
{
 public:
  wxBufferDataClassList();
  ~wxBufferDataClassList();

  wxBufferDataClass *Find(char *name);
  short FindPosition(wxBufferDataClass *);
  void Add(wxBufferDataClass *dataclass); /* Checks for duplicates */
  int Number(void);
  wxBufferDataClass *Nth(int);

  Bool Write(wxMediaStreamOut &f);
  Bool Read(wxMediaStreamIn &f);
  wxBufferDataClass *FindByMapPosition(short n);
};

#ifdef WXME_FOR_MRED
extern wxBufferDataClassList *wxMakeTheBufferDataClassList();
extern wxBufferDataClassList *wxGetTheBufferDataClassList();
#define wxTheBufferDataClassList (*wxGetTheBufferDataClassList())
#else
extern wxBufferDataClassList wxTheBufferDataClassList;
#endif

/**********************************************************************/

class wxLocationBufferData : public wxBufferData
{
 public:
  float x, y;

  wxLocationBufferData();

  Bool Write(wxMediaStreamOut &);
};

#endif __WX_SNIP__
