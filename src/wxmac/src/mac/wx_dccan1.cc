///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan1.cc
// Purpose:	Canvas device context implementation (Macintosh version) (part 1)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include <iostream.h>
#include <fstream.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <QuickDraw.h>
#include "wx_dccan.h"
#include "wx_canvs.h"
#include "wx_privt.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wx_area.h"

//#include "bdiag.xbm"
//#include "fdiag.xbm"
//#include "cdiag.xbm"
//#include "horiz.xbm"
//#include "verti.xbm"
//#include "cross.xbm"
//#include "wx_area.h"

static PixMapHandle	bdiag,
		cdiag,
		fdiag,
		cross,
		horiz,
		verti;

// Declarations local to this file
#define YSCALE(y) (yorigin - (y))
#define     wx_round(a)    (int)((a)+.5)

extern CGrafPtr wxMainColormap;

//-----------------------------------------------------------------------------
// Default constructor
//-----------------------------------------------------------------------------
wxCanvasDC::wxCanvasDC(void)
{
  __type = wxTYPE_DC_CANVAS;

	selected_pixmap = NULL;
	canvas = NULL;
	cMacDoingDrawing = FALSE;

  pixmap = 0;
  pixmapWidth = 0;
  pixmapHeight = 0;
  clipping = FALSE;

  current_reg = NULL ;
  user_reg = NULL ;
  onpaint_reg = NULL ;

  device = wxDEVICE_CANVAS;
  font = wxNORMAL_FONT;

  min_x = 0; min_y = 0; max_x = 0; max_y = 0;

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

#ifdef wx_motif
  gc = NULL;
  gcBacking = NULL;
#endif

  ok = TRUE;
  current_pen_join = -1 ;
  current_pen_cap = -1 ;
  current_pen_nb_dash = -1 ;
  current_pen_dash = NULL ;
  current_stipple = NULL ;

  current_logical_function = -1;

  Colour = wxColourDisplay();

  current_pen = NULL;
  current_brush = NULL;
  current_background_brush = NULL;
  current_text_foreground = *wxBLACK;
//  current_text_background = NULL;
  SetBackground(wxWHITE_BRUSH);
}


//-----------------------------------------------------------------------------
wxCanvasDC::wxCanvasDC(wxCanvas* the_canvas): wxbCanvasDC(the_canvas)
{
__type = wxTYPE_DC_CANVAS;

	GrafPtr oldPort;
	::GetPort(&oldPort);
	canvas = the_canvas;
	WXGC_IGNORE(canvas);
	cMacDC = canvas->MacDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	::SetPort((GrafPtr)theMacGrafPort);

	cMacDoingDrawing = FALSE;
	pixmap = theMacGrafPort->portPixMap;	//CJC 

  clipping = FALSE;
  selected_pixmap = NULL;

  pixmapWidth = 0;
  pixmapHeight = 0;

  current_reg = NULL ;
  user_reg = NULL ;
  onpaint_reg = NULL ;

  min_x = 0; min_y = 0; max_x = 0; max_y = 0;

  device = wxDEVICE_CANVAS;
  font = wxNORMAL_FONT;
#ifndef LkB
  logical_origin_x = 0;
  logical_origin_y = 0;
#else
  // TO DO: temp fix for logical positioning
  int x,y;
  canvas->GetPosition(&x, &y);
  logical_origin_x = -x;
  logical_origin_y = -y;
#endif
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

  ::SetPort(oldPort);

// More initialization
	if (the_canvas)
	{
		int clientWidth, clientHeight;
		the_canvas->GetClientSize(&clientWidth, &clientHeight);
		Rect paintRect = {0, 0, clientHeight, clientWidth};
		SetPaintRegion(&paintRect);
	}
}

//-----------------------------------------------------------------------------
wxCanvasDC::~wxCanvasDC(void)
{
  if (current_pen) current_pen->Lock(-1);
  if (current_brush) current_brush->Lock(-1);
  if (current_background_brush) current_background_brush->Lock(-1);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::BeginDrawing(void)
//-----------------------------------------------------------------------------
{
	if (cMacDoingDrawing) {
	 // wxFatalError("Tried BeginDrawing while already DoingDrawing.");
	 return;
	}
	cMacDoingDrawing = TRUE;

	SetCurrentDC();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::EndDrawing(void)
//-----------------------------------------------------------------------------
{
	if (!cMacDoingDrawing) {
	  // wxFatalError("Tried EndDrawing while not DoingDrawing.");
	  return;
	}
	
	cMacDoingDrawing = FALSE;
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetCurrentDC(void) // mac platform only
//-----------------------------------------------------------------------------
{
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	if ((GrafPtr)theMacGrafPort != qd.thePort) 
		::SetPort((GrafPtr)theMacGrafPort);

	if (cMacDC->currentUser() != this)
	{ // must setup platform
		cMacDC->setCurrentUser(this);
		int theRootX = 0, theRootY = 0;
		if(canvas)
		{
#if 1
			canvas->ClientArea()->FrameContentAreaOffset(&theRootX, &theRootY);
#else
			canvas->GetPosition(&theRootX, &theRootY);
#endif
		}
		::SetOrigin(-theRootX, -theRootY);
		wxMacSetClip();
		cMacCurrentTool = kPenTool; /* to force setting bg, etc. */
		wxMacSetCurrentTool(kNoTool);
		SetLogicalFunction(current_logical_function);
	}
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetCanvasClipping(void)
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
	if (theCurrentUser == this)
	{ // must update platfrom
		GrafPtr theOldPort = qd.thePort;
		CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort((GrafPtr)theMacGrafPort);

		wxMacSetClip();

		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort(theOldPort);
	}
}

//-----------------------------------------------------------------------------
void wxCanvasDC::GetClippingBox(float *x,float *y,float *w,float *h)
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
void wxCanvasDC::SetPaintRegion(Rect* paintRect)
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
	SetCanvasClipping();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetClippingRegion(float cx, float cy, float cw, float ch)
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
  SetCanvasClipping();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::GetClippingRegion(float *cx, float *cy, float *cw, float *ch)
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
void wxCanvasDC::DestroyClippingRegion(void)
//-----------------------------------------------------------------------------
{
  if (user_reg) ::DisposeRgn(user_reg);
  user_reg = NULL;
  SetCanvasClipping();
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetFont(wxFont *the_font)
{
	font = the_font;

	wxObject* theCurrentUser = cMacDC->currentUser();
	if (theCurrentUser == this)
	{ // must update platfrom
		GrafPtr theOldPort = qd.thePort;
		CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort((GrafPtr)theMacGrafPort);

		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort(theOldPort);
	}

#ifdef wx_xview
  // Set the font according to the current scaling
  int scaled_size = (int)(user_scale_y*font->GetPointSize() + 0.5);
  Xv_Font xfont = wxFontPool->FindNearestFont(font->GetFamily(), font->GetStyle(),
                       font->GetWeight(), scaled_size, font->GetUnderlined(), 0, 0);
  font->x_font = xfont;
  Font theFont = (Font)xv_get(xfont, XV_XID);
#endif
#ifdef wx_motif
/*
        int res_x = (int)(DisplayWidth(dpy, screen)/(DisplayWidthMM(dpy, screen)/25.4));
        int res_y = (int)(DisplayHeight(dpy, screen)/(DisplayHeightMM(dpy, screen)/25.4));
*/
  int res_x = 100;
  int res_y = 100;

  int scaled_size = (int)(10 * ((int)(user_scale_y*font->GetPointSize() + 0.5)));

  XFontStruct *fontStruct = wxFontPool->FindNearestFont(font->GetFamily(), font->GetStyle(),
                                                   font->GetWeight(), scaled_size,
                                                   font->GetUnderlined(), res_x, res_y);
  font->xFont = fontStruct;
  Font theFont = fontStruct->fid;
#endif
#ifdef wx_motif_wch
  XSetFont(display, gc, theFont);
#endif
#ifdef wx_motif
  if (canvas && canvas->is_retained)
    XSetFont(display, gcBacking, theFont);
#endif
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetPen(wxPen *pen)
{
	if (current_pen) current_pen->Lock(-1);
	current_pen = pen;
	if (current_pen) current_pen->Lock(1);

	if (cMacDC->currentUser() == this && cMacCurrentTool == kPenTool)
	{
		wxMacSetCurrentTool(kNoTool); // platform pen no longer valid
	}

#ifdef wx_motif_wch
  if (!same_style)
  {
    int scaled_width = (int)XLOG2DEVREL(pen->GetWidth());
    if (scaled_width < 0)
      scaled_width = 0;

    int style;
    int join ;
    int cap ;
    static char dotted[] = {2, 5};
    static char short_dashed[] = {4, 4};
    static char long_dashed[] = {4, 8};
    static char dotted_dashed[] = {6, 6, 2, 6};

    // We express dash pattern in pen width unit, so we are
    // independent of zoom factor and so on...
    int req_nb_dash ;
    char *req_dash ;

    switch (pen->GetStyle())
    {
      case wxUSER_DASH:
        req_nb_dash = current_pen_nb_dash ;
        req_dash = current_pen_dash ;
        style = LineOnOffDash;
        break;
      case wxDOT:
        req_nb_dash = 2 ;
        req_dash = dotted ;
        style = LineOnOffDash;
        break;
      case wxSHORT_DASH:
        req_nb_dash = 2 ;
        req_dash = short_dashed ;
        style = LineOnOffDash;
        break;
      case wxLONG_DASH:
        req_nb_dash = 2 ;
        req_dash = long_dashed ;
        style = LineOnOffDash;
        break;
      case wxDOT_DASH:
        req_nb_dash = 4 ;
        req_dash = dotted_dashed ;
        style = LineOnOffDash;
        break;
      case wxSTIPPLE:
      case wxSOLID:
      case wxTRANSPARENT:
      default:
        style = LineSolid;
        req_dash = NULL ;
        req_nb_dash = 0 ;
    }

    if (req_dash&&req_nb_dash)
    {
      char *real_req_dash = new char[req_nb_dash] ;
      if (real_req_dash)
      {
        int factor = scaled_width==0? 1:scaled_width ;
        for (int i=0;i<req_nb_dash;i++)
          real_req_dash[i] = req_dash[i]*factor ;
#ifdef wx_motif_wch
        XSetDashes(display, gc, 0, real_req_dash, req_nb_dash);
#endif
#ifdef wx_motif
        if (canvas && canvas->is_retained)
          XSetDashes(display,gcBacking,0,real_req_dash,req_nb_dash);
#endif
        delete [] real_req_dash ;
      }
      else
      {
        // No Memory. We use non-scaled dash pattern...
#ifdef wx_motif_wch
        XSetDashes(display, gc, 0, req_dash, req_nb_dash);
#endif
#ifdef wx_motif
        if (canvas && canvas->is_retained)
          XSetDashes(display,gcBacking,0,req_dash,req_nb_dash);
#endif
      }
    }
 
    switch(pen->GetCap())
    {
    case wxCAP_PROJECTING: cap = CapProjecting ; break ;
    case wxCAP_BUTT:       cap = CapButt ;       break ;
    case wxCAP_ROUND:
    default:               cap = CapRound ;      break ;
    }

    switch(pen->GetJoin())
    {
    case wxJOIN_BEVEL: join = JoinBevel ; break ;
    case wxJOIN_MITER: join = JoinMiter ; break ;
    case wxJOIN_ROUND:
    default:           join = JoinRound ; break ;
    }

#ifdef wx_motif
    XSetLineAttributes(display, gc, scaled_width, style, cap, join);
#endif
#ifdef wx_motif
    if (canvas && canvas->is_retained)
      XSetLineAttributes(display, gcBacking, scaled_width, style, cap, join);
#endif
  }

#endif
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetBrush(wxBrush *brush)
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
void wxCanvasDC::SetBackground(wxBrush* brush)
{
	if (current_background_brush) current_background_brush->Lock(-1);
	current_background_brush = brush;
	if (current_background_brush) current_background_brush->Lock(1);

	if (cMacDC->currentUser() == this
		&& (cMacCurrentTool != kBlitTool)
		&& (cMacCurrentTool != kColorBlitTool))
	{ // must update platform
		if (!current_background_brush) {
			BackColor(whiteColor);
			return;
		}
	
		GrafPtr theOldPort = qd.thePort;
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
			BackPat(&qd.white);
		else if (theBrushStyle == wxTRANSPARENT)
			BackPat(&qd.white);
		else if (IS_HATCH(theBrushStyle))
		{
			macGetHatchPattern(theBrushStyle, cMacPattern);
			BackPat(&cMacPattern);
		}
		else
		{
			BackPat(&qd.white);
		}

		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort(theOldPort);
	}
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetLogicalFunction(int function)
{
	current_logical_function = function;

	if (cMacDC->currentUser() == this)
	{ // must update platform
		GrafPtr theOldPort = qd.thePort;
		CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort((GrafPtr)theMacGrafPort);

		int theMacPenMode;
		switch (current_logical_function)
		{
			case wxCLEAR:  theMacPenMode = patBic; break;
			case wxXOR:  theMacPenMode = patXor; break;
			case wxINVERT: theMacPenMode = notPatCopy /* GXinvert */; break;
			case wxOR_REVERSE: theMacPenMode = patXor /* GXorReverse */; break;
			case wxAND_REVERSE: theMacPenMode = patCopy /* GXandReverse */; break;
			case wxAND: theMacPenMode = adMin; break;
			case wxOR: theMacPenMode = adMax; break;
			case wxAND_INVERT: theMacPenMode = patBic; break;
			case wxNO_OP: theMacPenMode = patCopy /* GXnoop */; break;
			case wxNOR: theMacPenMode = notPatOr /* GXnor */; break;
			case wxEQUIV: theMacPenMode = notPatXor; break;
			case wxSRC_INVERT: theMacPenMode = notPatCopy; break;
			case wxOR_INVERT: theMacPenMode = notPatOr; break;
			case wxNAND: theMacPenMode = notPatCopy /* GXnand */; break;
			case wxSET: theMacPenMode = patCopy /* GXset */; break;
			case wxCOPY:
			default:
				theMacPenMode = patCopy; break;
		}
	
		PenMode(theMacPenMode);

		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort(theOldPort);
	}
}

//-----------------------------------------------------------------------------
Bool wxCanvasDC::StartDoc(char *message) { return TRUE; }

//-----------------------------------------------------------------------------
void wxCanvasDC::EndDoc(void) { }

//-----------------------------------------------------------------------------
void wxCanvasDC::StartPage(void) { }

//-----------------------------------------------------------------------------
void wxCanvasDC::EndPage(void){ }

//-----------------------------------------------------------------------------
void wxCanvasDC::SetMapMode(int mode)
{
  mapping_mode = mode;

  int pixel_width = 0;
  int pixel_height = 0;
  int mm_width = 0;
  int mm_height = 0;

  // First, calculate how to scale from mm to pixels.
  // Then we just need to find the scaling factor from ? to mm and multiply
  // by the first scaling factor.

  pixel_width = 640;
  pixel_height = 480;
  mm_width = 225;
  mm_height = 169;

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
		GrafPtr theOldPort = qd.thePort;
		CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort((GrafPtr)theMacGrafPort);

		if (cMacCurrentTool == kPenTool)
			wxMacSetCurrentTool(kPenTool); // Force recalculation of line width

		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort(theOldPort);
	}
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetUserScale(float x, float y)
//-----------------------------------------------------------------------------
{
	user_scale_x = x;
	user_scale_y = y;

	if (cMacDC->currentUser() == this)
	{ // must update platform
		GrafPtr theOldPort = qd.thePort;
		CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort((GrafPtr)theMacGrafPort);

		if (cMacCurrentTool == kPenTool)
			wxMacSetCurrentTool(kPenTool); // Force recalculation of line width

		if ((GrafPtr)theMacGrafPort != theOldPort) ::SetPort(theOldPort);
	}
}

//-----------------------------------------------------------------------------
float wxCanvasDC::DeviceToLogicalX(int x) { return XDEV2LOG(x); }

//-----------------------------------------------------------------------------
float wxCanvasDC::DeviceToLogicalXRel(int x) { return XDEV2LOGREL(x); }

//-----------------------------------------------------------------------------
float wxCanvasDC::DeviceToLogicalY(int y) { return YDEV2LOG(y); }

//-----------------------------------------------------------------------------
float wxCanvasDC::DeviceToLogicalYRel(int y) { return YDEV2LOGREL(y); }

//-----------------------------------------------------------------------------
int wxCanvasDC::LogicalToDeviceX(float x) { return XLOG2DEV(x); }

//-----------------------------------------------------------------------------
int wxCanvasDC::LogicalToDeviceXRel(float x) { return XLOG2DEVREL(x); }

//-----------------------------------------------------------------------------
int wxCanvasDC::LogicalToDeviceY(float y) { return YLOG2DEV(y); }

//-----------------------------------------------------------------------------
int wxCanvasDC::LogicalToDeviceYRel(float y) { return YLOG2DEVREL(y); }

//-----------------------------------------------------------------------------
void wxCanvasDC::wxMacSetClip(void)
{
	SetCurrentDC();
	if (canvas && !canvas->WantsFocus()) { // => canvas is hidden (HACK!)
		Rect zeroClipRect = {0, 0, 0, 0};
		::ClipRect(&zeroClipRect);
	} else {
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
}

//-----------------------------------------------------------------------------
void wxCanvasDC::wxMacSetCurrentTool(wxMacToolType whichTool)
{
	SetCurrentDC();

	// mflatt: shortcut
	if (whichTool == cMacCurrentTool)
		return;

	RGBColor pixel;

	if ((whichTool != kBlitTool)
		&& (whichTool != kColorBlitTool)) {
	  cMacCurrentTool = kNoTool;
	  SetBackground(current_background_brush);
	}

	switch (whichTool)
	{
		case kNoTool:
			break;
		case kBrushTool:
			int theBrushStyle = current_brush->GetStyle();
			if (theBrushStyle == wxSOLID)
				PenPat(&qd.black);
			else if (theBrushStyle == wxTRANSPARENT)
				PenPat(&qd.white); // WCH : does this work??
			else if (IS_HATCH(theBrushStyle))
			{
				macGetHatchPattern(theBrushStyle, cMacPattern);
				PenPat(&cMacPattern);
			}
			else
			{
				PenPat(&qd.black);
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
				PenPat(&qd.black);
			else if (thePenStyle == wxTRANSPARENT)
				PenPat(&qd.white);
			else if (IS_HATCH(thePenStyle))
			{
				macGetHatchPattern(thePenStyle, cMacPattern);
				PenPat(&cMacPattern);
			}
			else
			{
				PenPat(&qd.black);
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
