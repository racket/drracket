/*
 * File:	wx_print.h
 * Purpose:	Printing-related classes
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 */

#ifndef wx_printh
#define wx_printh

#include "common.h"
#include "wx_canvs.h"
#include "wx_panel.h"
#include "wx_frame.h"
#include "wx_dialg.h"
#include "wx_choic.h"

#ifdef wx_mac
#include <Printing.h>
#endif

#ifdef IN_CPROTO
typedef       void    *wxPrintDialog ;
typedef       void    *wxPrinter ;
typedef       void    *wxPrintout ;
typedef       void    *wxPreviewCanvas ;
typedef       void    *wxPreviewControlBar ;
typedef       void    *wxPreviewFrame ;
typedef       void    *wxPrintPreview ;
#else

class wxDC;
class wxPrintout;
class wxPrinter;
class wxPrintDialog;
class wxPrintPreview;
class wxPreviewCanvas;
class wxPreviewControlBar;
class wxPreviewFrame;

/*
 * wxPrintData
 * Encapsulates information displayed and edited in the printer dialog box.
 */
 
class wxPrintData: public wxObject
{
 public:
  // macintosh
  THPrint macPrData;

  wxPrintData(void);
  ~wxPrintData(void);

  void SetAbortFlag(void); // set mac abort flag
  int GetFromPage(void);
  int GetToPage(void);
  int GetMinPage(void);
  int GetMaxPage(void);
  int GetNoCopies(void);
  Bool GetAllPages(void);
  Bool GetCollate(void);

  void SetFromPage(int);
  void SetToPage(int);
  void SetMinPage(int);
  void SetMaxPage(int);
  void SetNoCopies(int);
  void SetAllPages(Bool);
  void SetCollate(Bool);
  void SetPrintToFile(Bool);
  void SetSetupDialog(Bool);

  void EnablePrintToFile(Bool);
  void EnableSelection(Bool);
  void EnablePageNumbers(Bool);
  void EnableHelp(Bool);

  void operator=(const wxPrintData& data);
};

/*
 * wxPrinterDialog
 * The common dialog for printing.
 */
 
class wxPrintDialog: public wxDialogBox
{
 private:
  wxPrintData printData;
  wxDC *printerDC;
  Bool destroyDC;
  char *deviceName;
  char *driverName;
  char *portName;
  wxWindow *dialogParent;
 public:
  wxPrintDialog(wxWindow *parent, wxPrintData *data = NULL);
  ~wxPrintDialog(void);

#ifdef wx_mac
  virtual void Show(Bool flag);
#else
  virtual void Show(Bool flag);
#endif

  virtual wxPrintData& GetPrintData(void) { return printData; }
  virtual wxDC *GetPrintDC(void);
};

/*
 * Represents the printer: manages printing a wxPrintout object
 */
 
class wxPrinter: public wxObject
{
 private:
  wxPrintData printData;
  wxPrintout *currentPrintout;
#ifndef wx_mac
  FARPROC lpAbortProc;
#endif
 public:
  static wxWindow *abortWindow;
  static Bool abortIt;

  wxPrinter(wxPrintData *data = NULL);
  ~wxPrinter(void);

  virtual Bool Print(wxWindow *parent, wxPrintout *printout, Bool prompt = TRUE);
  virtual Bool PrintDialog(wxWindow *parent);
  virtual wxWindow *CreateAbortWindow(wxWindow *parent, wxPrintout *printout);
  virtual Bool Setup(wxWindow *parent);
  virtual void ReportError(wxWindow *parent, wxPrintout *printout, char *message);
  virtual wxPrintData &GetPrintData(void);
  virtual inline Bool Abort(void) { return abortIt; }
};

/*
 * wxPrintout
 * Represents an object via which a document may be printed.
 * The programmer derives from this, overrides (at least) OnPrintPage,
 * and passes it to a wxPrinter object for printing, or a wxPrintPreview
 * object for previewing.
 */
 
class wxPrintout: public wxObject
{
 private:
   char *printoutTitle;
   wxDC *printoutDC;

   int pageWidthPixels;
   int pageHeightPixels;

   int pageWidthMM;
   int pageHeightMM;

   int PPIScreenX;
   int PPIScreenY;
   int PPIPrinterX;
   int PPIPrinterY;

   Bool isPreview;
 public:
  wxPrintout(char *title = "Printout");
  ~wxPrintout(void);

  virtual Bool OnBeginDocument(int startPage, int endPage);
  virtual void OnEndDocument(void);
  virtual void OnBeginPrinting(void);
  virtual void OnEndPrinting(void);

  // Guaranteed to be before any other functions are called
  inline virtual void OnPreparePrinting(void) { }

  virtual Bool HasPage(int page);
  virtual Bool OnPrintPage(int page) = 0;
  virtual void GetPageInfo(int *minPage, int *maxPage, int *pageFrom, int *pageTo);

  inline virtual char *GetTitle(void) { return printoutTitle; }

  inline wxDC *GetDC(void) { return printoutDC; }
  inline void SetDC(wxDC *dc) { printoutDC = dc; }
  inline void SetPageSizePixels(int w, int  h) { pageWidthPixels = w; pageHeightPixels = h; }
  inline void GetPageSizePixels(int *w, int  *h) { *w = pageWidthPixels; *h = pageHeightPixels; }
  inline void SetPageSizeMM(int w, int  h) { pageWidthMM = w; pageHeightMM = h; }
  inline void GetPageSizeMM(int *w, int  *h) { *w = pageWidthMM; *h = pageHeightMM; }

  inline void SetPPIScreen(int x, int y) { PPIScreenX = x; PPIScreenY = y; }
  inline void GetPPIScreen(int *x, int *y) { *x = PPIScreenX; *y = PPIScreenY; }
  inline void SetPPIPrinter(int x, int y) { PPIPrinterX = x; PPIPrinterY = y; }
  inline void GetPPIPrinter(int *x, int *y) { *x = PPIPrinterX; *y = PPIPrinterY; }

  inline virtual Bool IsPreview(void) { return isPreview; }

  inline virtual void SetIsPreview(Bool p) { isPreview = p; }
};

/*
 * wxPreviewCanvas
 * Canvas upon which a preview is drawn.
 */
 
class wxPreviewCanvas: public wxCanvas
{
 private:
  wxPrintPreview *printPreview;
  static wxBrush *previewBackgroundBrush;
 public:
  wxPreviewCanvas(wxPrintPreview *preview, wxWindow *parent, int x = -1, int y = -1, int w = -1, int h = -1,
    long style = 0, char *name = "canvas");
  ~wxPreviewCanvas(void);

  void Paint(void);
  void OnEvent(wxMouseEvent& event);
  void OnChar(wxKeyEvent& event);
};

/*
 * wxPreviewFrame
 * Default frame for showing preview.
 */

class wxPreviewFrame: public wxFrame
{
 protected:
  wxCanvas *previewCanvas;
  wxPreviewControlBar *controlBar;
  wxPrintPreview *printPreview;
 public:
  wxPreviewFrame(wxPrintPreview *preview, wxFrame *parent, char *title = "Print Preview",
    int x = -1, int y = -1, int w = -1, int h = -1,
    long style = wxDEFAULT_FRAME|wxSDI, char *name = "frame");
  ~wxPreviewFrame(void);

  Bool OnClose(void);
  virtual void Initialize(void);
  virtual void CreateCanvas(void);
  virtual void CreateControlBar(void);
};

/*
 * wxPreviewControlBar
 * A panel with buttons for controlling a print preview.
 * The programmer may wish to use other means for controlling
 * the print preview.
 */

#define wxPREVIEW_PRINT        1
#define wxPREVIEW_PREVIOUS     2
#define wxPREVIEW_NEXT         4
#define wxPREVIEW_ZOOM         8

#define wxPREVIEW_DEFAULT      wxPREVIEW_PREVIOUS|wxPREVIEW_NEXT|wxPREVIEW_ZOOM

class wxPreviewControlBar: public wxPanel
{
 protected:
  wxPrintPreview *printPreview;
  wxButton *closeButton;
  wxButton *nextPageButton;
  wxButton *previousPageButton;
  wxButton *printButton;
  wxChoice *zoomControl;
  static wxFont *buttonFont;
  long buttonFlags;
 public:
  wxPreviewControlBar(wxPrintPreview *preview, long buttons,
    wxWindow *parent, int x = -1, int y = -1, int w = -1, int h = -1,
    long style = 0, char *name = "panel");
  ~wxPreviewControlBar(void);

  virtual void CreateButtons(void);
  virtual void SetZoomControl(int zoom);
  virtual int GetZoomControl(void);
  inline virtual wxPrintPreview *GetPrintPreview(void) { return printPreview; }
  void Paint(void);
};

/*
 * wxPrintPreview
 * Programmer creates an object of this class to preview a wxPrintout.
 */
 
class wxPrintPreview: public wxObject
{
 private:
  wxPrintData printData;
  wxCanvas *previewCanvas;
  wxFrame *previewFrame;
  wxBitmap *previewBitmap;
  wxPrintout *previewPrintout;
  wxPrintout *printPrintout;
  int currentPage;
  int currentZoom;
  float previewScale;
  int topMargin;
  int leftMargin;
  int pageWidth;
  int pageHeight;
  int minPage;
  int maxPage;
 public:
  wxPrintPreview(wxPrintout *printout, wxPrintout *printoutForPrinting = NULL, wxPrintData *data = NULL);
  ~wxPrintPreview(void);

  virtual Bool SetCurrentPage(int pageNum);
  virtual inline int GetCurrentPage(void) { return currentPage; };

  virtual void SetPrintout(wxPrintout *printout);
  virtual inline wxPrintout *GetPrintout(void) { return previewPrintout; };
  virtual inline wxPrintout *GetPrintoutForPrinting(void) { return printPrintout; };

  virtual void SetFrame(wxFrame *frame);
  virtual void SetCanvas(wxCanvas *canvas);

  inline virtual wxFrame *GetFrame(void) { return previewFrame; }
  inline virtual wxCanvas *GetCanvas(void) { return previewCanvas; }

  // The preview canvas should call this from Paint
  virtual Bool PaintPage(wxCanvas *canvas);

  // This draws a blank page onto the preview canvas
  virtual Bool DrawBlankPage(wxCanvas *canvas);

  // This is called by wxPrintPreview to render a page into
  // a wxMemoryDC.
  virtual Bool RenderPage(int pageNum);

  virtual wxPrintData &GetPrintData(void);
  virtual void SetZoom(int percent);
  virtual int GetZoom(void);

  // If we own a wxPrintout that can be used for printing, this
  // will invoke the actual printing procedure. Called
  // by the wxPreviewControlBar.
  virtual Bool Print(Bool interactive);

  virtual int GetMaxPage(void) { return maxPage; }
  virtual int GetMinPage(void) { return minPage; }

 protected:
 
  // Calculate scaling that needs to be done to get roughly
  // the right scaling for the screen pretending to be
  // the currently selected printer.
  virtual void DetermineScaling(void);
};

#endif // IN_CPROTO
#endif // wx_printh
