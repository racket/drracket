/*
 * File:        wx_print.cc
 * Purpose:     Printer implementation (mac)
 * Author:      Lj Birk (original msw by Julian Smart
 * Created:     1995
 * Updated:	October 1995
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */

#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"
#ifndef wx_mac
#pragma hdrstop
#endif

#ifdef testbmp
#include <tagobj.h>
#endif

#include "common.h"

#if USE_PRINTING_ARCHITECTURE
#if USE_COMMON_DIALOGS

#include "wx_utils.h"
#include "wx_print.h"
#include "wx_dc.h"
#include "wx_dcpr.h"
#include "wx_main.h"
#include "wx_frame.h"
#include "wx_buttn.h"
#include "wx_dcmem.h"
#include "wx_messg.h"

#include <stdlib.h>
#ifndef wx_mac
#include <commdlg.h>
#endif

#ifdef wx_mac
#include "wx_dcps.h"
#endif


void printIdle(void);

#ifdef	NO_UNIVERSAL

ProcPtr printIdleUPP = (ProcPtr)&printIdle;
extern ProcPtr printIdleUPP;

#else	// ifdef NO_UNIVERSAL

extern PrIdleUPP printIdleUPP;

// Have universal headers, compiling PPC
#define ppcPPC 1
#ifdef ppcPPC
RoutineDescriptor printIdleRD =
BUILD_ROUTINE_DESCRIPTOR(uppPrIdleProcInfo,printIdle);
PrIdleUPP printIdleUPP=(PrIdleUPP)&printIdleRD;

#else	// ifdef __powerpc__

// Have universal headers, compiling 68K
PrIdleUPP printIdleUPP=(PrIdleUPP)&printIdle;

#endif

#endif


wxPrintDialog::wxPrintDialog(wxWindow *p, wxPrintData *data):
 wxDialogBox((wxFrame *)p, "Printer Dialog")
{
  dialogParent = p;
  printerDC = NULL;
  destroyDC = TRUE;
  deviceName = NULL;
  driverName = NULL;
  portName = NULL;

  if (data)
    printData = *data;

}

wxPrintDialog::~wxPrintDialog(void)
{
  if (destroyDC && printerDC)
    delete printerDC;
  if (deviceName) delete[] deviceName;
  if (driverName) delete[] driverName;
  if (portName) delete[] portName;
  printData.macPrData = 0; // this is so ~wxPrintData wont delete THPrint
}

void wxPrintDialog::Show(Bool flag)
{

  Bool prtJob = FALSE;
  short strp, endp;

	/* MATTHEW: [6] */
#if 0
  endp = (**printData.macPrData).prJob.iLstPage;
  strp = (**printData.macPrData).prJob.iFstPage;
#endif

  PrOpen();

  PrValidate( printData.macPrData);

  if (PrError() != fnfErr) {
    prtJob = PrJobDialog(printData.macPrData);
    if (!prtJob)
    {
       (**printData.macPrData).prJob.iLstPage = 0;
       (**printData.macPrData).prJob.iFstPage = 0;
       PrClose();
       return;
    }
  }

  if (PrError())
    DisposeHandle((Handle)printData.macPrData);

  PrClose();

}

wxDC *wxPrintDialog::GetPrintDC(void)
{
  if (printerDC)
  {
    destroyDC = FALSE;
    return printerDC;
  }
  else
    return NULL;
}

/*
 * Print data
 */

wxPrintData::wxPrintData(void)
{
  THPrint pd = (THPrint)NewHandleClear(sizeof(TPrint));
  CheckMemOK(pd);
  macPrData = pd;
  
  /* MATTHEW: [6] */
  PrintDefault(macPrData);
}

wxPrintData::~wxPrintData(void)
{
  if (macPrData)
    DisposeHandle((Handle)macPrData);
}

void wxPrintData::SetAbortFlag()
{
	  (**macPrData).prJob.fFromUsr=TRUE;
}

int wxPrintData::GetFromPage(void)
{
  return (**macPrData).prJob.iFstPage;
}

int wxPrintData::GetToPage(void)
{
  return (**macPrData).prJob.iLstPage;
}

int wxPrintData::GetMinPage(void)
{
  return (**macPrData).prJob.iFstPage;
}

int wxPrintData::GetMaxPage(void)
{
  return (**macPrData).prJob.iLstPage;
}

int wxPrintData::GetNoCopies(void)
{
  return (**macPrData).prJob.iCopies;
}

Bool wxPrintData::GetAllPages(void)
{
  return ( GetMaxPage() == (**macPrData).prJob.iLstPage);
}

Bool wxPrintData::GetCollate(void)
{
  return FALSE;
}

/*
wxDC *wxPrintData::GetDC(void)
{
  if (printerDC)
  {
    destroyDC = FALSE;
    return printerDC;
  }
  else
    return NULL;
}
*/

void wxPrintData::SetFromPage(int p)
{
  (**macPrData).prJob.iFstPage = p;
}

void wxPrintData::SetToPage(int p)
{
  (**macPrData).prJob.iLstPage = p;
}

void wxPrintData::SetMinPage(int p)
{
  (**macPrData).prJob.iFstPage = p;
}

void wxPrintData::SetMaxPage(int p)
{
  (**macPrData).prJob.iLstPage = p;
}

void wxPrintData::SetNoCopies(int c)
{
  (**macPrData).prJob.iCopies = c;
}

void wxPrintData::SetAllPages(Bool flag)
{
}

void wxPrintData::SetCollate(Bool flag)
{
}

void wxPrintData::SetPrintToFile(Bool flag)
{
  //(**macPrData).
}

void wxPrintData::EnablePrintToFile(Bool flag)
{
}

void wxPrintData::EnableSelection(Bool flag)
{
}

void wxPrintData::EnablePageNumbers(Bool flag)
{
}

void wxPrintData::EnableHelp(Bool flag)
{
}

void wxPrintData::SetSetupDialog(Bool flag)
{
// page setup menu item
//  PrOpen();
//  PrStlDialog(printData.macPrData);
//  PrClose();
}

void wxPrintData::operator=(const wxPrintData& data)
{
   this->macPrData = data.macPrData;
}

/*
 * Printer
 */
 
wxPrinter::wxPrinter(wxPrintData *data)
{
  currentPrintout = NULL;
  abortWindow = NULL;
  abortIt = FALSE;
  if (data)
    printData = (*data);
  //lpAbortProc = MakeProcInstance((FARPROC) wxAbortProc, wxhInstance);
}

wxWindow *wxPrinter::abortWindow = NULL;
Bool wxPrinter::abortIt = FALSE;

wxPrinter::~wxPrinter(void)
{
  //FreeProcInstance(lpAbortProc);
}

Bool wxPrinter::Print(wxWindow *parent, wxPrintout *printout, Bool prompt)
{
  abortIt = FALSE;
  abortWindow = NULL;

  if (!printout)
    return FALSE;
    
  printout->SetIsPreview(FALSE);
  printout->OnPreparePrinting();

  // Get some parameters from the printout, if defined
  int fromPage, toPage;
  int minPage, maxPage;
  printout->GetPageInfo(&minPage, &maxPage, &fromPage, &toPage);

  if (maxPage == 0)
    return FALSE;

  PrOpen();
  if (PrError() != fnfErr) {
    PrintDefault(printData.macPrData);
  }

  printData.SetMinPage(minPage);
  printData.SetMaxPage(maxPage);
  if (fromPage != 0)
    printData.SetFromPage(fromPage);
  if (toPage != 0)
    printData.SetToPage(toPage);

  if (minPage != 0)
  {
    printData.EnablePageNumbers(TRUE);
    if (printData.GetFromPage() < printData.GetMinPage())
      printData.SetFromPage(printData.GetMinPage());
    else if (printData.GetFromPage() > printData.GetMaxPage())
      printData.SetFromPage(printData.GetMaxPage());
    if (printData.GetToPage() > printData.GetMaxPage())
      printData.SetToPage(printData.GetMaxPage());
    else if (printData.GetToPage() < printData.GetMinPage())
      printData.SetToPage(printData.GetMinPage());
  }
  else
    printData.EnablePageNumbers(FALSE);
  
  // Create a suitable device context  
  wxDC *dc = NULL;
  if (prompt)
  {
    // create a mac dc with a printer port
    // may need to create a dccan in order to draw
    // and set the dc as a print dc
    wxPrintDialog *dialog = new wxPrintDialog(parent, &printData);
    dialog->Show(TRUE);
    delete dialog;
    //printData = dialog.GetPrintData();
  }

  // sanity check  
  if (printData.GetFromPage() <= 0 || 
      printData.GetToPage() <= 0)
  {
    PrClose();
    return FALSE;
  }
  
    //dc = new wxPrinterDC(NULL, NULL, NULL, FALSE);
  dc = new wxPrinterDC(printData.macPrData); 

  // May have pressed cancel.
  if (!dc || !dc->Ok())
  {
    if (dc) delete dc; // PrSetError
    PrClose();
    return FALSE;
  }

  GDHandle gThisGDevice = GetMainDevice();
  int logPPIScreenX = (int)Fix2Long((**(**gThisGDevice).gdPMap).hRes);
  int logPPIScreenY = (int)Fix2Long((**(**gThisGDevice).gdPMap).vRes);

  printout->SetPPIScreen(logPPIScreenX, logPPIScreenY);

  int logPPIPrinterX = (**(printData.macPrData)).prInfo.iHRes;  //::GetDeviceCaps(dc->cdc, LOGPIXELSX);
  int logPPIPrinterY = (**(printData.macPrData)).prInfo.iVRes;  //::GetDeviceCaps(dc->cdc, LOGPIXELSY);

  printout->SetPPIPrinter(logPPIPrinterX, logPPIPrinterY);

  // Set printout parameters  
  printout->SetDC(dc);

///// TODO figure the equivalent
  float w, h;
  dc->GetSize(&w, &h);
  printout->SetPageSizePixels((int)w, (int)h);
  //dc->GetSizeMM(&w, &h);
  dc->GetSize(&w, &h);
  printout->SetPageSizeMM((int)w, (int)h);

  // Create an abort window
  //wxBeginBusyCursor();
  wxWindow *win = CreateAbortWindow(parent, printout);
  //wxYield();
  //::SetAbortProc(dc->cdc, lpAbortProc);
  
  if (!win)
  {
 //   wxEndBusyCursor();
    wxMessageBox("Sorry, could not create an abort dialog.", "Print Error", wxOK, (wxFrame *)parent);
    delete dc;
    PrClose();
	return FALSE;
  }
  abortWindow = win;
  abortWindow->Show(TRUE);
 // wxYield();

  //(**printData.macPrData).prJob.pIdleProc = printIdleUPP;

  printout->OnBeginPrinting();
  
  Bool keepGoing = TRUE;

  for (int copyCount = 1; copyCount <= printData.GetNoCopies(); copyCount ++)
  {
    if (!printout->OnBeginDocument(printData.GetFromPage(), printData.GetToPage()))
    {
      //wxEndBusyCursor();
      wxMessageBox("Could not start printing.", "Print Error");
      break;
    }
    if (abortIt) {
      printData.SetAbortFlag();
      wxDialogBox *dialog = new wxDialogBox(parent, "Print Aborted", 0, 0, 400, 400);
      break;
    }

    for (int pn = printData.GetFromPage(); 
             keepGoing && 
             (pn <= printData.GetToPage()) && printout->HasPage(pn);
         pn++)
    {
      if (abortIt)
      {
        printData.SetAbortFlag();
        wxDialogBox *dialog = new wxDialogBox(parent, "Print Aborted", 0, 0, 400, 400);
        keepGoing = FALSE;
        break;
      }
      else
      {
        dc->StartPage();
        printout->OnPrintPage(pn);
        dc->EndPage();
      }
    }
    printout->OnEndDocument();
  }

  printout->OnEndPrinting();

  if (abortWindow)
  {
    abortWindow->Show(FALSE);
    delete abortWindow;
    abortWindow = NULL;
  }
  
  //wxEndBusyCursor();

  delete dc;
  
  PrClose();

  return TRUE;
}

Bool wxPrinter::PrintDialog(wxWindow *parent)
{
  wxPrintDialog *dialog = new wxPrintDialog(parent, &printData);
  dialog->Show(TRUE);
  delete dialog;
  return 0;
}
// TODO make this a UPP and stuf in THPrintPtr
static void wxAbortWindowCancel(wxButton& but, wxCommandEvent& event)
{
  wxPrinter::abortIt = TRUE;
  wxPrinter::abortWindow->Show(FALSE);
  delete wxPrinter::abortWindow;
  wxPrinter::abortWindow = NULL;
}

wxWindow *wxPrinter::CreateAbortWindow(wxWindow *parent, wxPrintout *printout)
{
    //wxMessageBox("Printing, please wait.", "Printing", wxOK, (wxFrame *)parent);

  wxDialogBox *dialog = new wxDialogBox(parent, "Printing", FALSE, 0, 0, 400, 400);
  wxMessage *message = new wxMessage(dialog, "Press Cmd-. to stop printing");
  dialog->Fit();

  dialog->Centre(wxBOTH);
  
  return dialog;
}

Bool wxPrinter::Setup(wxWindow *parent)
{
  wxPrintDialog *dialog = new wxPrintDialog(parent, &printData);
  dialog->GetPrintData().SetSetupDialog(TRUE);
  dialog->Show(TRUE);
  delete dialog;
  return 0;
}

void wxPrinter::ReportError(wxWindow *parent, wxPrintout *printout, char *message)
{
  wxMessageBox(message, "Printing Error", wxOK, (wxFrame *)parent);
}

wxPrintData &wxPrinter::GetPrintData(void)
{
  return printData;
}

/*
 * Printout class
 */
 
wxPrintout::wxPrintout(char *title)
{
  printoutTitle = title ? copystring(title) : NULL;
  printoutDC = NULL;
  pageWidthMM = 0;
  pageHeightMM = 0;
  pageWidthPixels = 0;
  pageHeightPixels = 0;
  PPIScreenX = 0;
  PPIScreenY = 0;
  PPIPrinterX = 0;
  PPIPrinterY = 0;
  isPreview = FALSE;
}

wxPrintout::~wxPrintout(void)
{
  if (printoutTitle)
    delete[] printoutTitle;
}

Bool wxPrintout::OnBeginDocument(int startPage, int endPage)
{
  return GetDC()->StartDoc("Printing");
}

void wxPrintout::OnEndDocument(void)
{
  GetDC()->EndDoc();
}

void wxPrintout::OnBeginPrinting(void)
{
}

void wxPrintout::OnEndPrinting(void)
{
}

Bool wxPrintout::HasPage(int page)
{
  return (page == 1);
}

void wxPrintout::GetPageInfo(int *minPage, int *maxPage, int *fromPage, int *toPage)
{
  *minPage = 1;
  *maxPage = 32000;
  *fromPage = 1;
  *toPage = 1;
}

/****************************************************************************

    FUNCTION: wxAbortProc()

    PURPOSE:  Processes messages for the Abort Dialog box

****************************************************************************/

void printIdle(void)
{
	EventRecord theEvent;
	while (GetNextEvent(24,&theEvent)) {
		if ((theEvent.modifiers & cmdKey) &&
			(theEvent.message & 0xff) =='.') 
		    wxPrinter::abortIt = TRUE;
		}
}



/*
 * Preview canvas
 */
 
wxPreviewCanvas::wxPreviewCanvas(wxPrintPreview *preview, wxWindow *parent, int x, int y, int w, int h,
    long style, char *name):
 wxCanvas((wxFrame*)parent, x, y, w, h, style, name)
{
  printPreview = preview;
  if (!previewBackgroundBrush)
    previewBackgroundBrush = wxTheBrushList->FindOrCreateBrush("THISTLE", wxSOLID);
  SetBackground(previewBackgroundBrush);
  SetScrollbars(40, 40, 50, 50, 8, 8);
  //SetScrollbars(30, 30, 30, 30, 8, 8);
}

wxBrush *wxPreviewCanvas::previewBackgroundBrush = NULL;

wxPreviewCanvas::~wxPreviewCanvas(void)
{
}

void wxPreviewCanvas::Paint(void)
{
  wxCanvas::Paint();

 /*
  int w, h;
  GetSize(&w, &h);
  wxDC *dc = GetDC();
  dc->SetPen(wxBLACK_PEN);
  dc->SetBrush(wxTRANSPARENT_BRUSH);
  DrawLine(0, h-1, w, h-1);
*/


  if (printPreview)
    printPreview->PaintPage(this);
}

void wxPreviewCanvas::OnEvent(wxMouseEvent& event)
{
}

void wxPreviewCanvas::OnChar(wxKeyEvent& event)
{
}

/*
 * Preview control bar
 */
 
wxPreviewControlBar::wxPreviewControlBar(wxPrintPreview *preview, long buttons,
    wxWindow *parent, int x, int y, int w, int h,
    long style, char *name):
  wxPanel((wxFrame*)parent, x, y, w, h, style | wxBORDER, name)
{
  printPreview = preview;
  closeButton = NULL;
  nextPageButton = NULL;
  previousPageButton = NULL;
  printButton = NULL;
  zoomControl = NULL; // this was commented out for some reason
  buttonFlags = buttons;
}

wxFont *wxPreviewControlBar::buttonFont = NULL;

wxPreviewControlBar::~wxPreviewControlBar(void)
{
}

void wxPreviewControlBar::Paint(void)
{
  //wxPanel::Paint();
  wxPanel::Paint();

  int w, h;
  GetSize(&w, &h);
  //wxDC *dc = MacDC();
  //dc->SetPen(wxBLACK_PEN);
  //dc->SetBrush(wxTRANSPARENT_BRUSH);
  //dc->DrawLine(0, h-1, w, h-1);
}

static void wxPreviewCloseFunc(wxButton& but, wxCommandEvent& event)
{
  wxPreviewControlBar *controlBar = (wxPreviewControlBar *)but.GetParent();
  wxPreviewFrame *frame = (wxPreviewFrame *)controlBar->GetParent();
  if (frame->OnClose())
    delete frame;
}

static void wxPreviewPrintFunc(wxButton& but, wxCommandEvent& event)
{
  wxPreviewControlBar *controlBar = (wxPreviewControlBar *)but.GetParent();
  wxPrintPreview *preview = controlBar->GetPrintPreview();
  preview->Print(TRUE);
}

static void wxPreviewNextFunc(wxButton& but, wxCommandEvent& event)
{
  wxPreviewControlBar *controlBar = (wxPreviewControlBar *)but.GetParent();
  wxPrintPreview *preview = controlBar->GetPrintPreview();
  if (preview)
  {
    int currentPage = preview->GetCurrentPage();
    if ((preview->GetMaxPage() > 0) &&
        (currentPage < preview->GetMaxPage()) &&
        preview->GetPrintout()->HasPage(currentPage + 1))
    {
      preview->SetCurrentPage(currentPage + 1);
    }
  }
}

static void wxPreviewPreviousFunc(wxButton& but, wxCommandEvent& event)
{
  wxPreviewControlBar *controlBar = (wxPreviewControlBar *)but.GetParent();
  wxPrintPreview *preview = controlBar->GetPrintPreview();
  if (preview)
  {
    int currentPage = preview->GetCurrentPage();
    if ((preview->GetMinPage() > 0) &&
        (currentPage > preview->GetMinPage()) &&
        preview->GetPrintout()->HasPage(currentPage - 1))
    {
      preview->SetCurrentPage(currentPage - 1);
    }
  }
}

static void wxPreviewZoomFunc(wxChoice& but, wxCommandEvent& event)
{
  wxPreviewControlBar *controlBar = (wxPreviewControlBar *)but.GetParent();
  int zoom = controlBar->GetZoomControl();
  if (controlBar->GetPrintPreview())
    controlBar->GetPrintPreview()->SetZoom(zoom);
}

void wxPreviewControlBar::CreateButtons(void)
{
  SetLabelPosition(wxVERTICAL);
  if (!buttonFont)
    buttonFont = wxTheFontList->FindOrCreateFont(11, wxSWISS, wxNORMAL, wxBOLD);
  SetButtonFont(buttonFont);

  int buttonWidth = 65;
  int buttonHeight = 25;

  closeButton = new wxButton(this, (wxFunction)wxPreviewCloseFunc, "Close", -1, -1, buttonWidth, buttonHeight);
  closeButton->SetDefault();
  
  if (buttonFlags & wxPREVIEW_PRINT)
    printButton =  new wxButton(this, (wxFunction)wxPreviewPrintFunc, "Print...", -1, -1, buttonWidth, buttonHeight);
  if (buttonFlags & wxPREVIEW_PREVIOUS)
    previousPageButton = new wxButton(this, (wxFunction)wxPreviewPreviousFunc, "<<", -1, -1, buttonWidth, buttonHeight);
  if (buttonFlags & wxPREVIEW_NEXT)
    nextPageButton = new wxButton(this, (wxFunction)wxPreviewNextFunc, ">>", -1, -1, buttonWidth, buttonHeight);

  char *choices[] = { "10%", "20%", "25%", "30%", "35%", "40%", "45%", "50%", "55%", "60%",
    "65%", "70%", "75%", "80%", "85%", "90%", "95%", "100%", "110%", "120%", "150%", "200%" };
  int n = 22;
  if (buttonFlags & wxPREVIEW_ZOOM)
  {
    zoomControl = new wxChoice(this, (wxFunction)wxPreviewZoomFunc, "Scale", -1, -1, 100, 40, n, choices);
    SetZoomControl(printPreview->GetZoom());
  }

  SetSize(0, 0, 600, 40);

}

void wxPreviewControlBar::SetZoomControl(int zoom)
{
  char buf[20];
  sprintf(buf, "%d%%", zoom);
  if (zoomControl)
    zoomControl->SetStringSelection(buf);
}

int wxPreviewControlBar::GetZoomControl(void)
{
  char buf[20];
  if (zoomControl && zoomControl->GetStringSelection())
  {
    strcpy(buf, zoomControl->GetStringSelection());
    buf[strlen(buf) - 1] = 0;
    return (int)atoi(buf);
  }
  else return 0;
}

/*
 * Preview frame
 */

wxPreviewFrame::wxPreviewFrame(wxPrintPreview *preview, wxFrame *parent, char *title,
    int x, int y, int w, int h, long style, char *name):
 wxFrame(parent, title, x, y, w, h, style, name)
{
  printPreview = preview;
  controlBar = NULL;
  previewCanvas = NULL;
}

wxPreviewFrame::~wxPreviewFrame(void)
{
}

Bool wxPreviewFrame::OnClose(void)
{
  MakeModal(FALSE);
  
  // Need to delete the printout and the print preview
//  printPreview->EndPreview();
  wxPrintout *printout = printPreview->GetPrintout();
  if (printout)
  {
    delete printout;
    printPreview->SetPrintout(NULL);
    printPreview->SetCanvas(NULL);
    printPreview->SetFrame(NULL);
  }
  delete printPreview;
  return TRUE;
}

void wxPreviewFrame::Initialize(void)
{
  CreateStatusLine();
  
  CreateCanvas();
  CreateControlBar();

  printPreview->SetCanvas(previewCanvas);
  printPreview->SetFrame(this);

  // Set layout constraints here

  // Control bar constraints
  //wxLayoutConstraints *c1 = new wxLayoutConstraints;
  int w, h;
  controlBar->GetSize(&w, &h);

  //c1->left.SameAs       (this, wxLeft);
  //c1->top.SameAs        (this, wxTop);
  //c1->right.SameAs      (this, wxRight);
  //c1->height.Absolute   (h);

  //controlBar->SetConstraints(c1);

  // Canvas constraints
  //wxLayoutConstraints *c2 = new wxLayoutConstraints;

  //c2->left.SameAs       (this, wxLeft);
  //c2->top.Below         (controlBar);
  //c2->right.SameAs      (this, wxRight);
  //c2->bottom.SameAs     (this, wxBottom);

  //previewCanvas->SetConstraints(c2);

  MakeModal(TRUE);
}

void wxPreviewFrame::CreateCanvas(void)
{
  previewCanvas = new wxPreviewCanvas(printPreview, this, -1, 40, // TODO WHY
                                      Width(),  Height(),
                                      wxRETAINED | wxVSCROLL| wxHSCROLL);
}

void wxPreviewFrame::CreateControlBar(void)
{
  long buttons = wxPREVIEW_DEFAULT;
  if (printPreview->GetPrintoutForPrinting())
    buttons |= wxPREVIEW_PRINT;
    
  controlBar = new wxPreviewControlBar(printPreview, buttons, this, -1, -1, Width());
  controlBar->CreateButtons();
}
 
/*
 * Print preview
 */

wxPrintPreview::wxPrintPreview(wxPrintout *printout, wxPrintout *printoutForPrinting, wxPrintData *data)
{
  previewPrintout = printout;
  if (previewPrintout)
    previewPrintout->SetIsPreview(TRUE);
    
  printPrintout = printoutForPrinting;
  if (data)
    printData = (*data);

  previewCanvas = NULL;
  previewFrame = NULL;
  previewBitmap = NULL;
  currentPage = 1;
  currentZoom = 30;
  topMargin = 40;
  leftMargin = 40;
  pageWidth = 0;
  pageHeight = 0;

  printout->OnPreparePrinting();

  // Get some parameters from the printout, if defined
  int selFrom, selTo;
  printout->GetPageInfo(&minPage, &maxPage, &selFrom, &selTo);

  DetermineScaling();
}

wxPrintPreview::~wxPrintPreview(void)
{
  if (previewPrintout)
    delete previewPrintout;
  if (previewBitmap)
    delete previewBitmap;
  if (printPrintout)
    delete printPrintout;
}

Bool wxPrintPreview::SetCurrentPage(int pageNum)
{
  if (currentPage == pageNum)
    return TRUE;

  currentPage = pageNum;
  if (previewBitmap)
  {
    delete previewBitmap;
    previewBitmap = NULL;
  }

  if (previewCanvas)
  {
    RenderPage(pageNum);
    //previewCanvas->Refresh();
  }
  return TRUE;
}

void wxPrintPreview::SetPrintout(wxPrintout *printout)
{
  previewPrintout = printout;
}

void wxPrintPreview::SetFrame(wxFrame *frame)
{
  previewFrame = frame;
}
  
void wxPrintPreview::SetCanvas(wxCanvas *canvas)
{
  previewCanvas = canvas;
}

Bool wxPrintPreview::PaintPage(wxCanvas *canvas)
{
  DrawBlankPage(canvas);

  if (!previewBitmap)
    RenderPage(currentPage);
    
  if (!previewBitmap)
    return FALSE;

  if (!canvas)
    return FALSE;

  int canvasWidth, canvasHeight;
  canvas->GetSize(&canvasWidth, &canvasHeight);
  
  float zoomScale = (float)((float)currentZoom/(float)100);
  float actualWidth = (float)(zoomScale*pageWidth*previewScale);
  float actualHeight = (float)(zoomScale*pageHeight*previewScale);

  float x = (float)((canvasWidth - actualWidth)/2.0);
  if (x < leftMargin)
    x = leftMargin;
  float y = topMargin + 5;

  wxMemoryDC temp_dc;
  temp_dc.SelectObject(previewBitmap);
  canvas->GetDC()->Blit(x, y, previewBitmap->GetWidth(), previewBitmap->GetHeight(), &temp_dc, 0, 0);

  temp_dc.SelectObject(NULL);

  return TRUE;
}

Bool wxPrintPreview::RenderPage(int pageNum)
{
  int canvasWidth, canvasHeight;
  if (!previewCanvas)
  {
    wxMessageBox("wxPrintPreview::RenderPage: must use wxPrintPreview::SetCanvas to let me know about the canvas!",
      "Print Preview Failure", wxOK);
    return FALSE;
  }
  previewCanvas->GetSize(&canvasWidth, &canvasHeight);
  
  float zoomScale = (float)((float)currentZoom/(float)100);
  float actualWidth = (float)(zoomScale*pageWidth*previewScale);
  float actualHeight = (float)(zoomScale*pageHeight*previewScale);

  float x = (float)((canvasWidth - actualWidth)/2.0);
  if (x < leftMargin)
    x = leftMargin;
  float y = topMargin;

  if (!previewBitmap)
  {
    previewBitmap = new wxBitmap((int)actualWidth, (int)actualHeight);
    //previewBitmap = new wxBitmap((int)1024, (int)1024);
    if (!previewBitmap || !previewBitmap->Ok())
    {
      if (previewBitmap)
        delete previewBitmap;
      wxMessageBox("Sorry, not enough memory to create a preview.", "Print Preview Failure", wxOK);
      return FALSE;
    }
  }
  wxMemoryDC memoryDC; // tried (previewCanvas->wx_dc)
  memoryDC.SelectObject(previewBitmap);
  memoryDC.SetUserScale(zoomScale, zoomScale);

  memoryDC.Clear();

  previewPrintout->SetDC(&memoryDC);
  previewPrintout->SetPageSizePixels(pageWidth, pageHeight);

  previewPrintout->OnBeginPrinting();

  if (!previewPrintout->OnBeginDocument(printData.GetFromPage(), printData.GetToPage()))
  {
    wxMessageBox("Could not start document preview.", "Print Preview Failure", wxOK);
    memoryDC.SelectObject(NULL);
    delete previewBitmap;
    return FALSE;
  }
  previewPrintout->OnPrintPage(pageNum);
  previewPrintout->OnEndDocument();
  previewPrintout->OnEndPrinting();

  //memoryDC.EndDrawing();
  previewPrintout->SetDC(NULL);
  memoryDC.SelectObject(NULL);

  char buf[200];
  if (maxPage != 0)
    sprintf(buf, "Page %d of %d", pageNum, maxPage);
  else
    sprintf(buf, "Page %d", pageNum);

  if (previewFrame)
    previewFrame->SetStatusText(buf);

  return TRUE;
}

Bool wxPrintPreview::DrawBlankPage(wxCanvas *canvas)
{
  int canvasWidth, canvasHeight;
  canvas->GetSize(&canvasWidth, &canvasHeight);
  
  float zoomScale = (float)((float)currentZoom/(float)100);
  float actualWidth = zoomScale*pageWidth*previewScale;
  float actualHeight = zoomScale*pageHeight*previewScale;

  float x = (float)((canvasWidth - actualWidth)/2.0);
  if (x < leftMargin)
    x = leftMargin;
  float y = topMargin;

  wxDC *dc = canvas->GetDC();

  // Draw shadow, allowing for 1-pixel border AROUND the actual page
  int shadowOffset = 4;
  dc->SetPen(wxBLACK_PEN);
  dc->SetBrush(wxBLACK_BRUSH);
  dc->DrawRectangle(x-1 + shadowOffset, y-1 + shadowOffset, actualWidth+2, actualHeight+2);

  // Draw blank page allowing for 1-pixel border AROUND the actual page
  dc->SetPen(wxBLACK_PEN);
  dc->SetBrush(wxWHITE_BRUSH);
  dc->DrawRectangle(x-1, y-1, actualWidth+2, actualHeight+2);
  return TRUE;
}

wxPrintData &wxPrintPreview::GetPrintData(void)
{
  return printData;
}

void wxPrintPreview::SetZoom(int percent)
{
  if (currentZoom == percent)
    return;
    
  currentZoom = percent;
  if (previewBitmap)
  {
    delete previewBitmap;
    previewBitmap = NULL;
  }
  RenderPage(currentPage);
  
  if (previewCanvas)
  {
    previewCanvas->Clear();
    previewCanvas->Paint();
  }
}

int wxPrintPreview::GetZoom(void)
{
  return currentZoom;
}

Bool wxPrintPreview::Print(Bool interactive)
{
  if (!printPrintout)
    return FALSE;
  wxPrinter printer(&printData);
  return printer.Print(previewFrame, printPrintout, interactive);
}

void wxPrintPreview::DetermineScaling(void)
{

  GDHandle gThisGDevice = GetMainDevice();

  int screenWidth; // = (**(**gThisGDevice).gdPMap).hRes; // fix these
  int screenHeight; // = (**(**gThisGDevice).gdPMap).vRes;

  wxDisplaySize(&screenWidth, &screenHeight);

  int screenXRes =  screenWidth;
  int screenYRes =  screenHeight;
  int logPPIScreenX = (int)Fix2Long((**(**gThisGDevice).gdPMap).hRes);
  int logPPIScreenY = (int)Fix2Long((**(**gThisGDevice).gdPMap).vRes);

  previewPrintout->SetPPIScreen(logPPIScreenX, logPPIScreenY);

  int printerWidth = 552;
  int printerHeight = 730;
  int printerXRes = 552;
  int printerYRes = 730;

    // Get a device context for the currently selected printer
  /* create print record */
    THPrint context = (THPrint)
       NewHandleClear(sizeof(TPrint));
     CheckMemOK(context);
 
    PrOpen();
    if (PrError() != fnfErr) {
      PrintDefault(context);
    }

    PrClose();

    if (context) // otherwise use the defaults ???
    {
      printerWidth = (**context).prInfo.rPage.right - (**context).prInfo.rPage.left;
      printerHeight = (**context).prInfo.rPage.bottom - (**context).prInfo.rPage.top;
      printerXRes = printerWidth; // (**context).prStl.iPageH;
      printerYRes = printerHeight; // (**context).prStl.iPageV;

      int logPPIPrinterX = (**context).prInfoPT.iHRes;
      int logPPIPrinterY = (**context).prInfoPT.iVRes;
      previewPrintout->SetPPIPrinter(logPPIPrinterX, logPPIPrinterY);
    }
    
    DisposeHandle((Handle)context);

  pageWidth = printerXRes;
  pageHeight = printerYRes;

  // At 100%, the page should look about page-size on the screen.
  previewScale = (float)((float)screenWidth/(float)printerWidth);
  previewScale = previewScale * (float)((float)screenXRes/(float)printerYRes);
}

#endif
  // USE_COMMON_DIALOGS
#endif
  // End USE_PRINTING_ARCHITECTURE


