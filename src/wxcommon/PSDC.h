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
class wxList;
class wxPen;
class ofstream;
#else
#include "wx_dc.h"
#endif

class wxMemoryDC;

#if USE_POSTSCRIPT

#ifdef IN_CPROTO
typedef       void    *wxPostScriptDC ;
#else

#ifdef wx_xt
# define DRAW_TEXT_CONST /* empty */
#else
# define DRAW_TEXT_CONST const
#endif

class PSStream;

class wxPostScriptDC: public wxDC
{
 public:
#ifdef wx_xt
  char *title;
#endif
  int page_number;
  PSStream *pstream;    // PostScript output stream
  char *filename;
  long boundingboxpos;
  unsigned char currentRed;
  unsigned char currentGreen;
  unsigned char currentBlue;
  /* MATTHEW: [8] */
  float clipx, clipy, clipw, cliph;

  float paper_x, paper_y, paper_w, paper_h, paper_x_scale, paper_y_scale;
  Bool landscape, resetFont, level2ok;
  char *afm_path;

  int mode;
  char *preview_cmd, *print_cmd, *print_opts;

  // Create a printer DC
  wxPostScriptDC(void);
  wxPostScriptDC(Bool interactive);

  ~wxPostScriptDC(void);

  Bool Create(Bool interactive = TRUE);

  Bool PrinterDialog(Bool interactive);

  inline virtual void BeginDrawing(void) {} ;
  inline virtual void EndDrawing(void) {} ;

  void FloodFill(float x1, float y1, wxColour *col, int style=wxFLOOD_SURFACE) ;
  Bool GetPixel(float x1, float y1, wxColour *col) ;

  void DrawLine(float x1, float y1, float x2, float y2);
  void IntDrawLine(int x1, int y1, int x2, int y2);
  void CrossHair(float x, float y) ;
  void DrawArc(float x1,float y1,float w,float h,float start,float end);
  void DrawPoint(float x, float y);
  void DrawPoint(wxPoint* point) { DrawPoint(point->x, point->y); }
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

  void DrawSpline(float x1, float y1, float x2, float y2, float x3, float y3);

  void DrawRectangle(float x, float y, float width, float height);
  void DrawRoundedRectangle(float x, float y, float width, float height, float radius = 20);
  void DrawEllipse(float x, float y, float width, float height);
  void DrawText(DRAW_TEXT_CONST char *text, float x, float y, Bool use16 = FALSE, int dt = 0);

  void Clear(void);
  void SetFont(wxFont *font);
  void SetPen(wxPen *pen);
  void SetBrush(wxBrush *brush);
  void SetBackground(wxColour *c);
  void SetClippingRect(float x, float y, float width, float height);
  wxRegion *GetClippingRegion();
  void SetClippingRegion(wxRegion *r);
  void DestroyClippingRegion(void);

  Bool StartDoc(char *message);
  void EndDoc(void);
  void StartPage(void);
  void EndPage(void);

  float GetCharHeight(void);
  float GetCharWidth(void);
  void GetTextExtent(const char *string, float *x, float *y,
                     float *descent = NULL, float *externalLeading = NULL, 
		     wxFont *theFont = NULL, Bool use16 = FALSE, int dt = 0);
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
  float FLogicalToDeviceX(float x);
  float FLogicalToDeviceY(float y);
  float FLogicalToDeviceXRel(float x);
  float FLogicalToDeviceYRel(float y);
  Bool Blit(float xdest, float ydest, float width, float height,
            wxBitmap *source, float xsrc, float ysrc, int rop = wxSOLID, wxColour *c = NULL);
  Bool Blit(float xdest, float ydest, float width, float height,
            wxMemoryDC *source, float xsrc, float ysrc, int rop = wxSOLID, wxColour *c = NULL);
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

#endif

extern void wxInitializePrintSetupData(Bool init = TRUE);

class wxPrintSetupData : public wxObject {
public:
    wxPrintSetupData(void);
    ~wxPrintSetupData(void);

    void copy (wxPrintSetupData* data);

    void  SetPrinterCommand(char *cmd);
    void  SetPaperName(char *paper);
    void  SetPrintPreviewCommand(char *cmd);
    void  SetPrinterOptions(char *flags);
    void  SetPrinterFile(char *f);
    void  SetAFMPath(char *f);
    void  SetPrinterMode(int mode);
    void  SetPrinterOrientation(int orient)
	{ printer_orient = orient; }
    void  SetPrinterScaling(float x, float y)
	{ printer_scale_x = x; printer_scale_y = y; }
    void  SetPrinterTranslation(float x, float y)
	{ printer_translate_x = x; printer_translate_y = y; }
    void  SetColour(Bool col)
	{ print_colour = col; }
    void  SetLevel2(Bool l2)
	{ print_level_2 = l2; }
    void SetEditorMargin(long x, long y)
        { emargin_h = x; emargin_v = y; }

    inline char *GetPrinterCommand(void)
	{ return printer_command; }
    inline char *GetPrintPreviewCommand(void)
	{ return preview_command; }
    inline char *GetPrinterOptions(void)
	{ return printer_flags; }
    inline char *GetPrinterFile(void)
	{ return printer_file; }
    inline char *GetPaperName(void)
	{ return paper_name; }
    inline int GetPrinterOrientation(void)
	{  return printer_orient; }
    inline void GetPrinterScaling(float *x, float *y)
	{ *x=printer_scale_x; *y=printer_scale_y; }
    inline void GetPrinterTranslation(float *x, float *y)
	{ *x=printer_translate_x; *y=printer_translate_y; }
    inline int GetPrinterMode(void)
	{ return printer_mode; }
    inline char *GetAFMPath(void)
	{ return afm_path; }
    inline Bool GetColour(void)
	{ return print_colour; }
    inline Bool GetLevel2()
	{ return print_level_2; }
    void GetEditorMargin(long *x, long *y)
        { *x = emargin_h; *y = emargin_v; }

private:
    friend class wxPostScriptDC;

    char   *printer_command;
    char   *preview_command;
    char   *printer_flags;
    char   *printer_file;
    int    printer_orient;
    float  printer_scale_x;
    float  printer_scale_y;
    float  printer_translate_x;
    float  printer_translate_y;
    int    printer_mode;
    char   *afm_path;
    char   *paper_name;
    Bool   print_colour;
    Bool   print_level_2;
    long   emargin_h, emargin_v;
};

extern wxPrintSetupData *wxGetThePrintSetupData();
extern void wxSetThePrintSetupData(wxPrintSetupData *);

class wxPrintPaperType : public wxObject {
public:
    wxPrintPaperType(char *name=NULL, int wmm=0, int hmm=0, int wp=0, int hp=0);
    ~wxPrintPaperType(void);
public:
    int   widthMM;
    int   heightMM;
    int   widthPixels;
    int   heightPixels;
    char  *pageName;
};

class wxPrintPaperDatabase : public wxList {
public:
    wxPrintPaperDatabase(void);
    ~wxPrintPaperDatabase(void);

    void CreateDatabase(void);
    void ClearDatabase(void);

    void AddPaperType(char *name, int wmm, int hmm, int wp, int hp);
    wxPrintPaperType *FindPaperType(char *name);
};

extern wxPrintPaperDatabase *wxThePrintPaperDatabase;

#endif // IN_CPROTO
#endif // USE_POSTSCRIPT
#endif // wx_dcpsh
