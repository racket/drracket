///////////////////////////////////////////////////////////////////////////////
// File:	wx_dcpr1.cc
// Purpose:	Print Canvas device context implementation (Macintosh version) (part 1)
// Author:	Lou Birk (copied from wx_dccan)
// Created:	1995
// Updated:	
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include <iostream.h>
#include <fstream.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <QuickDraw.h>
#include <Printing.h>
#include "wx_dcpr.h"
//#include "wx_canvs.h"
#include "wx_privt.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wx_area.h"

// Declarations local to this file
#define YSCALE(y) (yorigin - (y))
#define     wx_round(a)    (int)((a)+.5)

extern CGrafPtr wxMainColormap;

//-----------------------------------------------------------------------------
// Default constructor
//-----------------------------------------------------------------------------
wxPrinterDC::wxPrinterDC(void)
{
  // this should never happen since the cMacDC would not be set
  // to a printer port like below
}


//-----------------------------------------------------------------------------
wxPrinterDC::wxPrinterDC(THPrint pData)
{
	/* MATTHEW: [6] */
	__type = wxTYPE_DC_PRINTER;

	GrafPtr oldPort;
	::GetPort(&oldPort);
    prRecHandle = pData;
    PrOpen();
	if (PrError()) {
      PrClose();
      ok = FALSE;
      return;
    }

	prPort = PrOpenDoc(prRecHandle, 0, 0);

    if (PrError()) {
      PrCloseDoc(prPort);
      PrClose();
      ok = FALSE;
      return;
    }

    cMacDC = new wxMacDC((CGrafPtr)&(prPort->gPort));
	GrafPtr theMacGrafPort = (GrafPtr)cMacDC->macGrafPort();
	::SetPort((GrafPtr)theMacGrafPort);

	cMacDoingDrawing = FALSE;

  clipping = FALSE;
  selected_pixmap = NULL;

  current_reg = NULL ;
  user_reg = NULL ;
  onpaint_reg = NULL ;

  min_x = 0; min_y = 0;
  max_x = 0; max_y = 0;

  pixmapWidth = (**prRecHandle).prInfo.rPage.right;
  pixmapHeight = (**prRecHandle).prInfo.rPage.bottom;

  device = wxDEVICE_CANVAS;
  font = wxNORMAL_FONT;

  logical_origin_x = 0;
  logical_origin_y = 0;

  device_origin_x = 0;
  device_origin_y = 0;

  logical_scale_x = 1.0;
  logical_scale_y = 1.0;

  user_scale_x = 1.0;
  user_scale_y = 1.0;

  mapping_mode = MM_TEXT;

  title = NULL;


  ok = TRUE;
  current_pen_join = -1 ;
  current_pen_cap = -1 ;
  current_pen_nb_dash = -1 ;
  current_pen_dash = NULL ;
  current_logical_function = -1;
  current_stipple = NULL ;

  Colour = wxColourDisplay();

  current_pen = NULL;
  current_brush = NULL;
  current_background_brush = NULL;
  current_text_foreground = *wxBLACK;
//  current_text_background = NULL;
  SetBackground(wxWHITE_BRUSH);
  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);

  //::SetPort(oldPort);

  int clientWidth, clientHeight;
  //the_canvas->GetClientSize(&clientWidth, &clientHeight);
  clientWidth = pixmapWidth;
  clientHeight = pixmapHeight; // paper
  Rect paintRect = {0, 0, clientHeight, clientWidth};
  SetPaintRegion(&paintRect);

}

//-----------------------------------------------------------------------------
wxPrinterDC::~wxPrinterDC(void)
{
  if (current_pen) current_pen->Lock(-1);
  if (current_brush) current_brush->Lock(-1);
  if (current_background_brush) current_background_brush->Lock(-1);
  if (ok)
    PrCloseDoc(prPort);
}

//-----------------------------------------------------------------------------
void wxPrinterDC::BeginDrawing(void)
//-----------------------------------------------------------------------------
{
	if (cMacDoingDrawing) wxFatalError("Tried BeginDrawing while already DoingDrawing.");
	cMacDoingDrawing = TRUE;

	SetCurrentDC();
}

//-----------------------------------------------------------------------------
void wxPrinterDC::EndDrawing(void)
//-----------------------------------------------------------------------------
{
	if (!cMacDoingDrawing) wxFatalError("Tried EndDrawing while not DoingDrawing.");
	cMacDoingDrawing = FALSE;
}

//-----------------------------------------------------------------------------
void wxPrinterDC::SetCurrentDC(void) // mac platform only
//-----------------------------------------------------------------------------
{
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	if ((GrafPtr)theMacGrafPort != qd.thePort) ::SetPort((GrafPtr)theMacGrafPort);

	if (cMacDC->currentUser() != this)
	{ // must setup platform
		cMacDC->setCurrentUser(this);
		int theRootX = 0, theRootY = 0;
		::SetOrigin(-theRootX, -theRootY);
		wxMacSetClip();
		SetBackground(current_background_brush);
		wxMacSetCurrentTool(kNoTool);
	}
}

//-----------------------------------------------------------------------------
void wxPrinterDC::SetPrinterClipping(void)
//-----------------------------------------------------------------------------
{
	if (current_reg) ::DisposeRgn(current_reg);
	if (user_reg || onpaint_reg) {
		current_reg = ::NewRgn();
		CheckMemOK(current_reg);
	} else
		current_reg = NULL;

	if (onpaint_reg && user_reg)
		::SectRgn(onpaint_reg, user_reg, current_reg) ;
	else if (user_reg)
		::CopyRgn(user_reg, current_reg) ;
	else if (onpaint_reg)
		::CopyRgn(onpaint_reg, current_reg) ;

	wxObject* theCurrentUser = cMacDC->currentUser();
	//if (theCurrentUser == this)
	//{ // must update platfrom
//		GrafPtr theOldPort = qd.thePort;
//		CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
//		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort((GrafPtr)theMacGrafPort);
//		wxMacSetClip();

//		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort(theOldPort);
//	}
}

//-----------------------------------------------------------------------------
void wxPrinterDC::GetClippingBox(float *x,float *y,float *w,float *h)
//-----------------------------------------------------------------------------
{
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();

	if (current_reg)
	{
		Rect theClipRect = (**(theMacGrafPort->clipRgn)).rgnBBox;
		int theX = theClipRect.left;
		int theY = theClipRect.top;
		int theWidth = theClipRect.right - theClipRect.left;
		int theHeight = theClipRect.bottom - theClipRect.top;
		*x = XDEV2LOG(theX) ;
		*y = YDEV2LOG(theY) ;
		*w = XDEV2LOGREL(theWidth) ;
		*h = YDEV2LOGREL(theHeight) ;
	}
	else
		*x = *y = *w = *h = 0; // WCH wx_win: this doesn't seem right
}

//-----------------------------------------------------------------------------
void wxPrinterDC::SetPaintRegion(Rect* paintRect)
//-----------------------------------------------------------------------------
{
	if (onpaint_reg) ::DisposeRgn(onpaint_reg);
	onpaint_reg = ::NewRgn();
	CheckMemOK(onpaint_reg);
	float cx, cy, cw, ch;
	cx = paintRect->left;
	cy = paintRect->top;
	cw =  paintRect->right;
	ch =  paintRect->bottom;
	int left = XLOG2DEV(cx);
	int top = YLOG2DEV(cy);
	int right = XLOG2DEVREL(cx + cw);
	int bottom = YLOG2DEVREL(cy + ch);
	::SetRectRgn(onpaint_reg, left, top, right, bottom);
	SetPrinterClipping();
}

//-----------------------------------------------------------------------------
void wxPrinterDC::SetClippingRegion(float cx, float cy, float cw, float ch)
//-----------------------------------------------------------------------------
{
  if (user_reg) ::DisposeRgn(user_reg);
  user_reg = ::NewRgn();
  CheckMemOK(user_reg);
  int left = XLOG2DEV(cx);
  int top = YLOG2DEV(cy);
  int right = XLOG2DEVREL(cx + cw);
  int bottom = YLOG2DEVREL(cy + ch);
  ::SetRectRgn(user_reg, left, top, right, bottom);
  SetPrinterClipping();
}

//-----------------------------------------------------------------------------
void wxPrinterDC::GetClippingRegion(float *cx, float *cy, float *cw, float *ch)
//-----------------------------------------------------------------------------
{
  if (!user_reg) {
	*cx = *cy = 0;
	*cw = *ch = -1;
  } else {
	*cx = (**user_reg).rgnBBox.left;
	*cy = (**user_reg).rgnBBox.top;
	*cw = (**user_reg).rgnBBox.right - *cx;
	*ch = (**user_reg).rgnBBox.bottom - *cy;
  }
}

//-----------------------------------------------------------------------------
void wxPrinterDC::DestroyClippingRegion(void)
//-----------------------------------------------------------------------------
{
  if (user_reg) ::DisposeRgn(user_reg);
  user_reg = NULL;
  SetPrinterClipping();
}

//-----------------------------------------------------------------------------
void wxPrinterDC::SetFont(wxFont *the_font)
{
	font = the_font;

	wxObject* theCurrentUser = cMacDC->currentUser();
	if (theCurrentUser == this)
	{ // must update platfrom
		GrafPtr theOldPort = qd.thePort;
		CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort((GrafPtr)theMacGrafPort);

		//if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort(theOldPort);
	}
}

//-----------------------------------------------------------------------------
void wxPrinterDC::SetPen(wxPen *pen)
{
	if (current_pen) current_pen->Lock(-1);
	current_pen = pen;
	if (current_pen) current_pen->Lock(1);

	if (cMacDC->currentUser() == this && cMacCurrentTool == kPenTool)
	{
		wxMacSetCurrentTool(kNoTool); // platform pen no longer valid
	}

}

//-----------------------------------------------------------------------------
void wxPrinterDC::SetBrush(wxBrush *brush)
{
	if (current_brush) current_brush->Lock(-1);
	current_brush = brush;
	if (current_brush) current_brush->Lock(1);

	if (cMacDC->currentUser() == this && cMacCurrentTool == kBrushTool)
	{
		wxMacSetCurrentTool(kNoTool); // platform brush no longer valid
	}
}

//-----------------------------------------------------------------------------
void wxPrinterDC::SetBackground(wxBrush* brush)
{
	if (current_background_brush) current_background_brush->Lock(-1);
	current_background_brush = brush;
	if (current_background_brush) current_background_brush->Lock(1);

	if (cMacDC->currentUser() == this)
	{ // must update platform
		GrafPtr theOldPort = 0; //qd.thePort;
		CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort((GrafPtr)theMacGrafPort);

		RGBColor pixel = current_background_brush->GetColour().pixel;
		if (Colour)
			RGBBackColor(&pixel);
		else
		{
			unsigned char red = current_background_brush->GetColour().Red();
			unsigned char blue = current_background_brush->GetColour().Blue();
			unsigned char green = current_background_brush->GetColour().Green();
			Bool isBlackColour =
				(red == (unsigned char )0 &&
				 blue == (unsigned char)0 &&
				 green == (unsigned char)0);
			BackColor(isBlackColour ? blackColor : whiteColor);
		}
	
		int theBrushStyle = current_background_brush->GetStyle();
		if (theBrushStyle == wxSOLID)
#ifdef OLD_HEADERS
			BackPat(qd.white);
#else
			BackPat(&qd.white);
#endif
		else if (theBrushStyle == wxTRANSPARENT)
#ifdef OLD_HEADERS
			BackPat(qd.white);
#else
			BackPat(&qd.white);
#endif
		else if (IS_HATCH(theBrushStyle))
		{
			macGetHatchPattern(theBrushStyle, cMacPattern);
#ifdef OLD_HEADERS
			BackPat(cMacPattern);
#else
			BackPat(&cMacPattern);
#endif
		}
		else
		{
#ifdef OLD_HEADERS
			BackPat(qd.white);
#else
			BackPat(&qd.white);
#endif
		}

		//if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort(theOldPort);
	}
}

//-----------------------------------------------------------------------------
void wxPrinterDC::SetLogicalFunction(int function)
{
	current_logical_function = function;

	if (cMacDC->currentUser() == this)
	{ // must update platform
		GrafPtr theOldPort = 0; //qd.thePort;
		CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort((GrafPtr)theMacGrafPort);

		int theMacPenMode;
		switch (current_logical_function)
		{
		//	case wxCLEAR:  theMacPenMode = GXclear; break;
			case wxXOR:  theMacPenMode = patXor; break;
		//	case wxINVERT: theMacPenMode = GXinvert; break;
		//	case wxOR_REVERSE: theMacPenMode = GXorReverse; break;
		//	case wxAND_REVERSE: theMacPenMode = GXandReverse; break;
			case wxAND: theMacPenMode = notPatBic; break;
			case wxOR: theMacPenMode = patOr; break;
			case wxAND_INVERT: theMacPenMode = patBic; break;
		//	case wxNO_OP: theMacPenMode = GXnoop; break;
		//	case wxNOR: theMacPenMode = GXnor; break;
			case wxEQUIV: theMacPenMode = notPatXor; break;
			case wxSRC_INVERT: theMacPenMode = notPatCopy; break;
			case wxOR_INVERT: theMacPenMode = notPatOr; break;
		//	case wxNAND: theMacPenMode = GXnand; break;
		//	case wxSET: theMacPenMode = GXset; break;
			case wxCOPY:
			default:
				theMacPenMode = patCopy; break;
		}
	
		PenMode(theMacPenMode);

		//if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort(theOldPort);
	}
}

//-----------------------------------------------------------------------------
Bool wxPrinterDC::StartDoc(char *message) { return TRUE; }

//-----------------------------------------------------------------------------
void wxPrinterDC::EndDoc(void) { }

//-----------------------------------------------------------------------------
void wxPrinterDC::StartPage(void)
{
  if (prPort)
    PrOpenPage(prPort, 0); 
}

//-----------------------------------------------------------------------------
void wxPrinterDC::EndPage(void)
{
  if (prPort)
    PrClosePage(prPort);
}

//-----------------------------------------------------------------------------
void wxPrinterDC::SetMapMode(int mode)
{
  mapping_mode = mode;

  int pixel_width = 0;
  int pixel_height = 0;
  int mm_width = 0;
  int mm_height = 0;

  // First, calculate how to scale from mm to pixels.
  // Then we just need to find the scaling factor from ? to mm and multiply
  // by the first scaling factor.

#ifdef wx_mac
  pixel_width = 800; // 640; for printing
  pixel_height = 1200; //480;
  mm_width = 225;
  mm_height = 169;
#endif // wx_mac

  float mm2pixelsX = pixel_width/mm_width;
  float mm2pixelsY = pixel_height/mm_height;

  switch (mode)
  {
    case MM_TWIPS:
    {
      logical_scale_x = (float)(twips2mm * mm2pixelsX);
      logical_scale_y = (float)(twips2mm * mm2pixelsY);
      break;
    }
    case MM_POINTS:
    {
      logical_scale_x = (float)(pt2mm * mm2pixelsX);
      logical_scale_y = (float)(pt2mm * mm2pixelsY);
      break;
    }
    case MM_METRIC:
    {
      logical_scale_x = mm2pixelsX;
      logical_scale_y = mm2pixelsY;
      break;
    }
    case MM_LOMETRIC:
    {
      logical_scale_x = (float)(mm2pixelsX/10.0);
      logical_scale_y = (float)(mm2pixelsY/10.0);
      break;
    }
    default:
    case MM_TEXT:
    {
      logical_scale_x = 1.0;
      logical_scale_y = 1.0;
      break;
    }
  }

	if (cMacDC->currentUser() == this)
	{ // must update platform
		GrafPtr theOldPort = 0; //qd.thePort;
		CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort((GrafPtr)theMacGrafPort);

		if (cMacCurrentTool == kPenTool)
			wxMacSetCurrentTool(kPenTool); // Force recalculation of line width

		//if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort(theOldPort);
	}
}

//-----------------------------------------------------------------------------
void wxPrinterDC::SetUserScale(float x, float y)
//-----------------------------------------------------------------------------
{
	user_scale_x = x;
	user_scale_y = y;

	if (cMacDC->currentUser() == this)
	{ // must update platform
		GrafPtr theOldPort = 0; //qd.thePort;
		CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort((GrafPtr)theMacGrafPort);

		if (cMacCurrentTool == kPenTool)
			wxMacSetCurrentTool(kPenTool); // Force recalculation of line width

		//if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort(theOldPort);
	}
}

//-----------------------------------------------------------------------------
float wxPrinterDC::DeviceToLogicalX(int x) { return XDEV2LOG(x); }

//-----------------------------------------------------------------------------
float wxPrinterDC::DeviceToLogicalXRel(int x) { return XDEV2LOGREL(x); }

//-----------------------------------------------------------------------------
float wxPrinterDC::DeviceToLogicalY(int y) { return YDEV2LOG(y); }

//-----------------------------------------------------------------------------
float wxPrinterDC::DeviceToLogicalYRel(int y) { return YDEV2LOGREL(y); }

//-----------------------------------------------------------------------------
int wxPrinterDC::LogicalToDeviceX(float x) { return XLOG2DEV(x); }

//-----------------------------------------------------------------------------
int wxPrinterDC::LogicalToDeviceXRel(float x) { return XLOG2DEVREL(x); }

//-----------------------------------------------------------------------------
int wxPrinterDC::LogicalToDeviceY(float y) { return YLOG2DEV(y); }

//-----------------------------------------------------------------------------
int wxPrinterDC::LogicalToDeviceYRel(float y) { return YLOG2DEVREL(y); }

//-----------------------------------------------------------------------------
void wxPrinterDC::wxMacSetClip(void)
{
	SetCurrentDC();
	if (current_reg)
	{
		::SetClip(current_reg);
	}
	else
	{
		Rect largestClipRect = {-32767, -32767, 32767, 32767};
		::ClipRect(&largestClipRect);
	}
}

//-----------------------------------------------------------------------------
void wxPrinterDC::wxMacSetCurrentTool(wxMacToolType whichTool)
{
	SetCurrentDC();

	RGBColor pixel;

	switch (whichTool)
	{
		case kNoTool:
			break;
		case kBrushTool:
			int theBrushStyle = current_brush->GetStyle();
			if (theBrushStyle == wxSOLID)
#ifdef OLD_HEADERS
				PenPat(qd.black);
#else
				PenPat(&qd.black);
#endif
			else if (theBrushStyle == wxTRANSPARENT)
#ifdef OLD_HEADERS
				PenPat(qd.white); // WCH wx_mac: does this work??
#else
				PenPat(&qd.white); // WCH wx_mac: does this work??
#endif
			else if (IS_HATCH(theBrushStyle))
			{
				macGetHatchPattern(theBrushStyle, cMacPattern);
#ifdef OLD_HEADERS
				PenPat(cMacPattern);
#else
				PenPat(&cMacPattern);
#endif
			}
			else
			{
#ifdef OLD_HEADERS
				PenPat(qd.black);
#else
				PenPat(&qd.black);
#endif
			}
	
			pixel = current_brush->GetColour().pixel;
			if (Colour)
				RGBForeColor(&pixel);
			else
			{
				unsigned char red = current_brush->GetColour().Red();
				unsigned char blue = current_brush->GetColour().Blue();
				unsigned char green = current_brush->GetColour().Green();
				Bool isWhiteColour =
					(red == (unsigned char )255 &&
					 blue == (unsigned char)255 &&
					 green == (unsigned char)255);
				ForeColor(isWhiteColour ? whiteColor : blackColor);
			}
			break;
		case kPenTool:
			int thePenWidth = current_pen->GetWidth();
			int thePenHeight = current_pen->GetWidth();
			PenSize(thePenWidth, thePenHeight);

			int thePenStyle = current_pen->GetStyle();
			if (thePenStyle == wxSOLID)
#ifdef OLD_HEADERS
				PenPat(qd.black);
#else
				PenPat(&qd.black);
#endif
			else if (thePenStyle == wxTRANSPARENT)
#ifdef OLD_HEADERS
				PenPat(qd.white);
#else
				PenPat(&qd.white);
#endif
			else if (IS_HATCH(thePenStyle))
			{
				macGetHatchPattern(thePenStyle, cMacPattern);
#ifdef OLD_HEADERS
				PenPat(cMacPattern);
#else
				PenPat(&cMacPattern);
#endif
			}
			else
			{
#ifdef OLD_HEADERS
				PenPat(qd.black);
#else
				PenPat(&qd.black);
#endif
			}

			pixel = current_pen->GetColour().pixel;
			if (Colour)
				RGBForeColor(&pixel);
			else
			{
				unsigned char red = current_pen->GetColour().Red();
				unsigned char blue = current_pen->GetColour().Blue();
				unsigned char green = current_pen->GetColour().Green();
				Bool isWhiteColour =
					(red == (unsigned char )255 &&
					 blue == (unsigned char)255 &&
					 green == (unsigned char)255);
				ForeColor(isWhiteColour ? whiteColor : blackColor);
			}
			break;
		case kBlitTool:
			ForeColor(blackColor);
			BackColor(whiteColor);
			break;
		case kColorBlitTool:
			BackColor(whiteColor);
			pixel = current_pen->GetColour().pixel;
			if (Colour)
				RGBForeColor(&pixel);
			else {
				unsigned char red = current_pen->GetColour().Red();
				unsigned char blue = current_pen->GetColour().Blue();
				unsigned char green = current_pen->GetColour().Green();
				Bool isWhiteColour =
					(red == (unsigned char )255 &&
					 blue == (unsigned char)255 &&
					 green == (unsigned char)255);
				ForeColor(isWhiteColour ? whiteColor : blackColor);
			}
			break;
		case kQuillTool:
			break;
	}

	cMacCurrentTool = whichTool;
}
