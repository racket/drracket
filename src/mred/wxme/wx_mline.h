
#define CURSOR_WIDTH 2

class wxMediaParagraph;

const unsigned long
  WXLINE_RED = 0x1,
  WXLINE_BLACK = 0x2,
  WXLINE_MAX_W_HERE = 0x4,
  WXLINE_MAX_W_LEFT = 0x8,
  WXLINE_MAX_W_RIGHT = 0x10,
  WXLINE_CALC_HERE = 0x20,
  WXLINE_CALC_LEFT = 0x40,
  WXLINE_CALC_RIGHT = 0x80,
  WXLINE_FLOW_HERE = 0x100,
  WXLINE_FLOW_LEFT = 0x200,
  WXLINE_FLOW_RIGHT = 0x400,
  WXLINE_STARTS_PARA = 0x800;

class wxMediaLine
{
 public:
  wxMediaLine *next, *prev, *parent, *left, *right;
  long flags;
  wxMediaParagraph *paragraph;

  /* relative values: */
  long line;   // line number
  long pos;    // starting item position
  long scroll; // starting scroll step
  long parno;  // paragraph number
  float y;     // starting y location
  
  float maxWidth;

  wxSnip *snip, *lastSnip, *scrollSnip;

  long len, numscrolls;
  float lastH, lastW; /* height/width of last snip in line */
  float h, w; /* height/width of line */
  float bottombase, topbase; /* bottom baseline, top baseline (relative) */

  wxMediaLine();
  ~wxMediaLine();

  wxMediaLine *Insert(wxMediaLine **root, Bool before = TRUE);
  void Delete(wxMediaLine **root);

  wxMediaLine *FindLine(long line);
  wxMediaLine *FindPosition(long pos);
  wxMediaLine *FindScroll(long scroll);
  wxMediaLine *FindLocation(float y);
  wxMediaLine *FindParagraph(long parnp);

  long GetLine();
  long GetPosition();
  long GetScroll();
  float GetLocation();
  long GetParagraph();

  wxMediaParagraph *GetParagraphStyle(Bool *first = NULL);

  float ScrollOffset(long p);
  long FindExtraScroll(float y);

  void SetLength(long len);
  void CalcLineLength();
  void SetScrollLength(long numScrolls);
  void SetHeight(float h);

  void SetWidth(float w);
  void MarkRecalculate();
  void MarkCheckFlow();

  void SetStartsParagraph(Bool starts);
  inline int StartsParagraph(void) 
    { return (flags & WXLINE_STARTS_PARA) ? 1 : 0; };

  wxMediaLine *GetRoot();

  Bool UpdateFlow(wxMediaLine **root, wxMediaEdit *, float maxw, wxDC *dc);
  Bool UpdateGraphics(wxMediaEdit *media, wxDC *dc);

  long Number();
  wxMediaLine *First();
  wxMediaLine *Last();

  float GetLeftLocation(float maxWidth);
  float GetRightLocation(float maxWidth);

 private:
  void AdjustOffsets(wxMediaLine *newchild);
  void DeadjustOffsets(wxMediaLine *oldchild);

  void RotateLeft(wxMediaLine **root);
  void RotateRight(wxMediaLine **root);
  void AdjustNeedCalc(Bool recur = FALSE);
  void AdjustNeedFlow(Bool recur = FALSE);
  void AdjustMaxWidth(Bool recur = FALSE);
};

extern wxMediaLine *NIL;

const unsigned long
  WXPARA_LEFT = 0x0,
  WXPARA_CENTER = 0x1,
  WXPARA_RIGHT = 0x2;

class wxMediaParagraph
{
 public:
  float leftMarginFirst, leftMargin;
  float rightMargin;
  int alignment;

  wxMediaParagraph *Clone();
  float GetLineMaxWidth(float maxWidth, Bool first);
};
