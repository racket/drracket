/*
 * File:	wx_dcps.h
 * Purpose:	PostScript device context
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_dcps.h	1.2 5/9/94" */


#ifndef wx_dcpsh
#define wx_dcpsh

#ifdef __GNUG__
#pragma interface
#endif

#ifdef wx_xt
class wxBitmap;
class wxBrush;
class wxColour;
class wxColourMap;
class wxFont;
class wxIcon;
class wxList;
class wxPen;
class ofstream;
#else
#include "wx_dc.h"
#endif

#if USE_POSTSCRIPT

#ifdef IN_CPROTO
typedef       void    *wxPostScriptDC ;
#else

#ifndef DECLARE_DYNAMIC_CLASS
# define DECLARE_DYNAMIC_CLASS(x) /* empty */
#endif

#ifdef wx_xt
# define BLIT_DC_TYPE wxDC
# define DRAW_TEXT_CONST /* empty */
#else
# define BLIT_DC_TYPE wxCanvasDC
# define DRAW_TEXT_CONST const
#endif

class wxPostScriptDC: public wxDC
{
  DECLARE_DYNAMIC_CLASS(wxPostScriptDC)

 public:
#ifdef wx_xt
  char *title;
  Bool clipping;
#endif
  int page_number;
  ofstream *pstream;    // PostScript output stream
  char *filename;
  long boundingboxpos;
  unsigned char currentRed;
  unsigned char currentGreen;
  unsigned char currentBlue;
  /* MATTHEW: [8] */
  float clipx, clipy, clipw, cliph;

  float paper_x, paper_y, paper_w, paper_h, paper_x_scale, paper_y_scale;
  Bool landscape, resetFont, level2ok;

  int mode;
  char *preview_cmd, *print_cmd, *print_opts;

  // Create a printer DC
  wxPostScriptDC(void);
  wxPostScriptDC(char *output, Bool interactive = TRUE, wxWindow *parent = NULL);

  ~wxPostScriptDC(void);

  Bool Create(char *output, Bool interactive = TRUE, wxWindow *parent = NULL);

  Bool PrinterDialog(Bool interactive, wxWindow *parent = NULL);

  inline virtual void BeginDrawing(void) {} ;
  inline virtual void EndDrawing(void) {} ;

  void FloodFill(float x1, float y1, wxColour *col, int style=wxFLOOD_SURFACE) ;
  Bool GetPixel(float x1, float y1, wxColour *col) ;

  void DrawLine(float x1, float y1, float x2, float y2);
  void IntDrawLine(int x1, int y1, int x2, int y2);
  void CrossHair(float x, float y) ;
  void DrawArc(float x1,float y1,float x2,float y2,float xc,float yc);
  void DrawPoint(float x, float y);
  void DrawPoint(wxPoint& point) { DrawPoint(point.x, point.y); }
  void DrawLines(int n, wxPoint points[], float xoffset = 0, float yoffset = 0);
  void DrawLines(int n, wxIntPoint points[], int xoffset = 0, int yoffset = 0);
#ifdef wx_xt
  void IntDrawLines(int n, wxIntPoint points[], int xoffset = 0, int yoffset = 0) 
    { DrawLines(n, points, xoffset, yoffset); }
#endif
  void DrawLines(wxList *lines, float xoffset = 0, float yoffset = 0)
#ifdef wx_xt
    ;
#else
  { wxbDC::DrawLines(lines, xoffset, yoffset); }
#endif
  void DrawPolygon(int n, wxPoint points[], float xoffset = 0, float yoffset = 0, int fillStyle=wxODDEVEN_RULE);
  void DrawPolygon(wxList *lines, float xoffset = 0, float yoffset = 0, int fillStyle=wxODDEVEN_RULE)
#ifdef wx_xt
    ;
#else
  { wxbDC::DrawPolygon(lines, xoffset, yoffset, fillStyle); }
#endif

  void DrawRectangle(float x, float y, float width, float height);
  void DrawRoundedRectangle(float x, float y, float width, float height, float radius = 20);
  void DrawEllipse(float x, float y, float width, float height);
  // Splines
  // 3-point spline
  void DrawSpline(float x1, float y1, float x2, float y2, float x3, float y3);
  // Any number of control points - a list of pointers to wxPoints
  void DrawSpline(wxList *points);
  void DrawSpline(int n, wxPoint points[]);
  void DrawIcon(wxIcon *icon, float x, float y);
  /* MATTHEW: [2] 16-bit fonts */
  void DrawText(DRAW_TEXT_CONST char *text, float x, float y, Bool use16 = FALSE);

  void Clear(void);
  void SetFont(wxFont *font);
  void SetPen(wxPen *pen);
  void SetBrush(wxBrush *brush);
  void SetLogicalFunction(int function);
  void SetBackground(wxBrush *brush);
  void SetClippingRegion(float x, float y, float width, float height);
  /* MATTHEW: [8] */
  void GetClippingRegion(float *x, float *y, float *width, float *height);
  void DestroyClippingRegion(void);

  Bool StartDoc(char *message);
  void EndDoc(void);
  void StartPage(void);
  void EndPage(void);

  float GetCharHeight(void);
  float GetCharWidth(void);
  /* MATTHEW: [2] 16-bit fonts */
  void GetTextExtent(const char *string, float *x, float *y,
                     float *descent = NULL, float *externalLeading = NULL, 
		     wxFont *theFont = NULL, Bool use16 = FALSE);
  void SetMapMode(int mode);
  void SetUserScale(float x, float y);
  float DeviceToLogicalX(int x);
  float DeviceToLogicalY(int y);
  float DeviceToLogicalXRel(int x);
  float DeviceToLogicalYRel(int y);
  int LogicalToDeviceX(float x);
  int LogicalToDeviceY(float y);
  int LogicalToDeviceXRel(float x);
  int LogicalToDeviceYRel(float y);
  Bool Blit(float xdest, float ydest, float width, float height,
            BLIT_DC_TYPE *source, float xsrc, float ysrc, int rop = wxCOPY);
  inline Bool CanGetTextExtent(void) { return USE_AFM_FOR_POSTSCRIPT; }
  inline Bool CanDrawBitmap(void) { return TRUE; }

  void GetSize(float *width, float *height);
  void GetSizeMM(float *width, float *height);

  inline void SetColourMap(wxColourMap *WXUNUSED(cmap)) {}

  void SetBackgroundMode(int mode);
  void SetTextBackground(wxColour *col);
  void SetTextForeground(wxColour *col);
  void TryColour(wxColour *src, wxColour *dest);

  virtual Bool Ok() { return ok; }
};

void wxSetLevel2Ok(Bool ok);
Bool wxGetLevel2Ok(void);

#ifndef wx_xt

// Print Orientation (Should also add Left, Right)
enum {
  PS_PORTRAIT,
  PS_LANDSCAPE
};// ps_orientation = PS_PORTRAIT;

// Print Actions
enum {
  PS_PRINTER,
  PS_FILE,
  PS_PREVIEW
};// ps_action = PS_PREVIEW;

// PostScript printer settings
void wxSetPrinterCommand(char *cmd);
void wxSetPrintPreviewCommand(char *cmd);
void wxSetPrinterOptions(char *flags);
void wxSetPrinterOrientation(int orientation);
void wxSetPrinterScaling(float x, float y);
void wxSetPrinterTranslation(float x, float y);
void wxSetPrinterMode(int mode);
void wxSetPrinterFile(char *f);
void wxSetAFMPath(char *f);

// Get current values
char *wxGetPrinterCommand(void);
char *wxGetPrintPreviewCommand(void);
char *wxGetPrinterOptions(void);
Bool wxGetPrinterOrientation(void);
void wxGetPrinterScaling(float *x, float *y);
void wxGetPrinterTranslation(float *x, float *y);
int wxGetPrinterMode(void);
char *wxGetPrinterFile(void);
char *wxGetAFMPath(void);

/*
 * PostScript print setup information
 */

class wxPrintSetupData: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxPrintSetupData)

 public:
  char *printerCommand;
  char *previewCommand;
  char *printerFlags;
  char *printerFile;
  int printerOrient;
  float printerScaleX;
  float printerScaleY;
  float printerTranslateX;
  float printerTranslateY;
  // 1 = Preview, 2 = print to file, 3 = send to printer
  int printerMode;
  char *afmPath;
  // A name in the paper database (see wx_print.h: the printing framework)
  char *paperName;
  Bool printColour;
 public:
  wxPrintSetupData(void);
  ~wxPrintSetupData(void);

  void SetPrinterCommand(char *cmd);
  void SetPaperName(char *paper);
  void SetPrintPreviewCommand(char *cmd);
  void SetPrinterOptions(char *flags);
  void SetPrinterFile(char *f);
  void SetPrinterOrientation(int orient);
  void SetPrinterScaling(float x, float y);
  void SetPrinterTranslation(float x, float y);
  // 1 = Preview, 2 = print to file, 3 = send to printer
  void SetPrinterMode(int mode);
  void SetAFMPath(char *f);
  void SetColour(Bool col);

  // Get current values
  char *GetPrinterCommand(void);
  char *GetPrintPreviewCommand(void);
  char *GetPrinterOptions(void);
  char *GetPrinterFile(void);
  char *GetPaperName(void);
  int GetPrinterOrientation(void);
  void GetPrinterScaling(float *x, float *y);
  void GetPrinterTranslation(float *x, float *y);
  int GetPrinterMode(void);
  char *GetAFMPath(void);
  Bool GetColour(void);

  void operator=(wxPrintSetupData& data);
};

extern wxPrintSetupData *wxThePrintSetupData;
extern void wxInitializePrintSetupData(Bool init = TRUE);


/*
 * Again, this only really needed for non-Windows platforms
 * or if you want to test the PostScript printing under Windows.
 */

class wxPrintPaperType: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxPrintPaperType)

 public:
  int widthMM;
  int heightMM;
  int widthPixels;
  int heightPixels;
  char *pageName;

  wxPrintPaperType(char *name = NULL, int wmm = 0, int hmm = 0, int wp = 0, int hp = 0);
  ~wxPrintPaperType(void);
};

class wxPrintPaperDatabase: public wxList
{
  DECLARE_DYNAMIC_CLASS(wxPrintPaperDatabase)

 public:
  wxPrintPaperDatabase(void);
  ~wxPrintPaperDatabase(void);

  void CreateDatabase(void);
  void ClearDatabase(void);

  void AddPaperType(char *name, int wmm, int hmm, int wp, int hp);
  wxPrintPaperType *FindPaperType(char *name);
};

extern wxPrintPaperDatabase *wxThePrintPaperDatabase;

#endif

#endif // IN_CPROTO
#endif // USE_POSTSCRIPT
#endif // wx_dcpsh
