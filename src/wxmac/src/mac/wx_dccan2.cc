///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan2.cc
// Purpose:	Canvas device context implementation (Macintosh version) (part 2)
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
#include <QDOffscreen.h>
#include "wx_dccan.h"
#include "wx_utils.h"
#include "wx_canvs.h"
#include "wx_privt.h"

//#include "bdiag.xbm"
//#include "fdiag.xbm"
//#include "cdiag.xbm"
//#include "horiz.xbm"
//#include "verti.xbm"
//#include "cross.xbm"

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
void wxCanvasDC::Clear(void)
//-----------------------------------------------------------------------------
{
	SetCurrentDC();

	int w, h;
	if (canvas)
	{
		canvas->GetVirtualSize(&w, &h);
	}
	else
	{
		w = pixmapWidth;
		h = pixmapHeight;
	}

	Rect theClearRect = {0, 0, h, w};
	::EraseRect(&theClearRect);
}

void wxCanvasDC::GetSize(float *width, float *height)
{
	if (canvas) {
		int w, h;
		canvas->GetVirtualSize(&w, &h);
		*width = w;
		*height = h;
	} else {
		*width = pixmapWidth;
		*height = pixmapHeight;
	}
}

//-----------------------------------------------------------------------------
void wxCanvasDC::CrossHair(float x, float y)
//-----------------------------------------------------------------------------
{
	SetCurrentDC();

	if (cMacCurrentTool != kPenTool) wxMacSetCurrentTool(kPenTool);
	int xx = XLOG2DEV(x);
	int yy = YLOG2DEV(y);
	float ww, hh;
	GetSize(&ww, &hh) ;
	wxMacDrawLine(0, yy, ww, yy);
	wxMacDrawLine(xx, 0, xx, hh);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::FloodFill(float x, float y, wxColour *col, int style)
//=============================================================================
{
	SetCurrentDC();
}

//-----------------------------------------------------------------------------
Bool wxCanvasDC::GetPixel(float x, float y, wxColour *col)
//=============================================================================
{
	RGBColor rgb;

	SetCurrentDC();

	GetCPixel(XLOG2DEV(x), YLOG2DEV(y), &rgb);
	col->Set(rgb.red >> 8, rgb.green >> 8, rgb.blue >> 8);

	return TRUE;
}

//-----------------------------------------------------------------------------
void wxCanvasDC::SetPixel(float x, float y, wxColour *col)
//=============================================================================
{
	RGBColor rgb;

	SetCurrentDC();

	rgb.red = col->Red() << 8;
	rgb.green = col->Green() << 8;
	rgb.blue = col->Blue() << 8;
	SetCPixel(XLOG2DEV(x), YLOG2DEV(y), &rgb);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::IntDrawLine(int x1, int y1, int x2, int y2)
//-----------------------------------------------------------------------------
{
	SetCurrentDC();
	if (cMacCurrentTool != kPenTool) wxMacSetCurrentTool(kPenTool);
	wxMacDrawLine(XLOG2DEV(x1), YLOG2DEV(y1), XLOG2DEV(x2), YLOG2DEV(y2));
	CalcBoundingBox(x1, y1); // WCH: not in original. Why not?
	CalcBoundingBox(x2, y2); // WCH: not in original. Why not?
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawLine(float x1, float y1, float x2, float y2)
//-----------------------------------------------------------------------------
{
	SetCurrentDC();
	if (cMacCurrentTool != kPenTool) wxMacSetCurrentTool(kPenTool);
	wxMacDrawLine(XLOG2DEV(x1), YLOG2DEV(y1), XLOG2DEV(x2), YLOG2DEV(y2));
	CalcBoundingBox(x1, y1);
	CalcBoundingBox(x2, y2);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawArc(float x1,float y1,float x2,float y2,float xc,float yc)
{
	SetCurrentDC();

#ifdef wx_motif_wch
  int xx1 = XLOG2DEV(x1) ;
  int yy1 = YLOG2DEV(y1) ;
  int xx2 = XLOG2DEV(x2) ;
  int yy2 = YLOG2DEV(y2) ;
  int xxc = XLOG2DEV(xc) ;
  int yyc = YLOG2DEV(yc) ;
 
  double dx = xx1 - xxc ;
  double dy = yy1 - yyc ;
  double radius = sqrt(dx*dx+dy*dy) ;
  int r = (int)radius ;
 
  double radius1,radius2 ;
 
  if (xx1==xx2 && yy1==yy2)
  {
    radius1 = 0.0 ;
    radius2 = 360.0 ;
  }
  else if (radius==0.0)
    radius1 = radius2 = 0.0 ;
  else
  {
    if (xx1-xxc == 0)
      if (yy1-yyc<0)
        radius1 = 90.0 ;
      else
        radius1 = -90.0 ;
    else
        radius1 = -atan2((double)(yy1-yyc),(double)(xx1-xxc))*360.0 / (2*M_PI) ;

    if (xx2-xxc==0)
      if (yy2-yyc<0)
        radius2 = 90.0 ;
      else
        radius2 = -90.0 ;
    else
        radius2 = -atan2((double)(yy2-yyc),(double)(xx2-xxc))*360.0 / (2*M_PI) ;
  }
  radius1 *= 64.0 ;
  radius2 *= 64.0 ;
  int alpha1 = (int)radius1 ;
  int alpha2 = (int)(radius2-radius1) ;
  while (alpha2<=0)
    alpha2 += 360*64 ;
  while (alpha2>360*64)
    alpha2 -= 360*64 ;

  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
  {
    if (cMacCurrentTool != kBrushTool) wxMacSetCurrentTool(kBrushTool);
#ifdef wx_motif_wch
    XFillArc(display, pixmap, gc,
             xxc-r,yyc-r,2*r,2*r,alpha1,alpha2) ; 
#endif
  }

  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
  {
    if (cMacCurrentTool != kPenTool) wxMacSetCurrentTool(kPenTool);
#ifdef wx_motif_wch
    XDrawArc(display, pixmap, gc,
             xxc-r,yyc-r,2*r,2*r,alpha1,alpha2) ; 
#endif
  }
  CalcBoundingBox(x1, y1);
  CalcBoundingBox(x2, y2);
#endif
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawPoint(float x, float y)
//-----------------------------------------------------------------------------
{
	SetCurrentDC();
	if (cMacCurrentTool != kPenTool) wxMacSetCurrentTool(kPenTool);
	wxMacDrawPoint(XLOG2DEV(x), YLOG2DEV(y));
	CalcBoundingBox(x, y);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawPolygon(int n, wxPoint points[],
								float xoffset, float yoffset, int fillStyle)
{
	SetCurrentDC();
	if (n <= 0) return;
	Point *xpoints1 = new Point[n+1];
	for (int i = 0; i < n; i++)
	{
		xpoints1[i].h = XLOG2DEV(points[i].x + xoffset);
		xpoints1[i].v = YLOG2DEV(points[i].y + yoffset);
		CalcBoundingBox(points[i].x + xoffset, points[i].y + yoffset);
	}

	// Close figure
	xpoints1[n].h = xpoints1[0].h;
	xpoints1[n].v = xpoints1[0].v;

	PolyHandle thePolygon = OpenPoly();
	MoveTo(xpoints1[0].h, xpoints1[0].v);
	for (int j = 1; j <= n; j++)
	{
		LineTo(xpoints1[j].h, xpoints1[j].v);
	}
	ClosePoly();

	if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
	{
		if (cMacCurrentTool != kBrushTool) wxMacSetCurrentTool(kBrushTool);
		PaintPoly(thePolygon);
#ifdef wx_motif_wch
		XSetFillRule(display,gc,fillStyle==wxODDEVEN_RULE?EvenOddRule:WindingRule);
		XFillPolygon(display, pixmap, gc, xpoints1, n, Convex, 0);
		XSetFillRule(display,gc,EvenOddRule); // default mode
#endif
	}

	if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	{
		if (cMacCurrentTool != kPenTool) wxMacSetCurrentTool(kPenTool);
		FramePoly(thePolygon);
	}

	delete[] xpoints1;
	KillPoly(thePolygon);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawLines(int n, wxIntPoint points[], int xoffset, int yoffset)
{
	if (n <= 0) return;
	if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	{
		SetCurrentDC();
		if (cMacCurrentTool != kPenTool) wxMacSetCurrentTool(kPenTool);
	
		Point *xpoints = new Point[n];
	
		for (int i = 0; i < n; i++)
		{
			xpoints[i].h = XLOG2DEV(points[i].x + xoffset);
			xpoints[i].v = YLOG2DEV(points[i].y + yoffset);
			CalcBoundingBox(points[i].x + xoffset, points[i].y + yoffset); // WCH: not in original??
		}
	
		PolyHandle thePolygon = OpenPoly();
		MoveTo(xpoints[0].h, xpoints[0].v);
		for (int j = 1; j < n; j++)
		{
			LineTo(xpoints[j].h, xpoints[j].v);
		}
		ClosePoly();
	
		FramePoly(thePolygon);
	
		delete[] xpoints;
		KillPoly(thePolygon);
	}
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawLines(int n, wxPoint points[], float xoffset, float yoffset)
{
	if (n <= 0) return;
	if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	{
		SetCurrentDC();
		if (cMacCurrentTool != kPenTool) wxMacSetCurrentTool(kPenTool);
	
		Point *xpoints = new Point[n];
	
		for (int i = 0; i < n; i++)
		{
			xpoints[i].h = XLOG2DEV(points[i].x + xoffset);
			xpoints[i].v = YLOG2DEV(points[i].y + yoffset); // WCH: original mistype "h" for "v"
			CalcBoundingBox(points[i].x + xoffset, points[i].y + yoffset); // WCH: not in original??
		}
	
		PolyHandle thePolygon = OpenPoly();
		MoveTo(xpoints[0].h, xpoints[0].v);
		for (int j = 1; j < n; j++)
		{
			LineTo(xpoints[j].h, xpoints[j].v);
		}
		ClosePoly();
	
		FramePoly(thePolygon);
	
		delete[] xpoints;
		KillPoly(thePolygon);
	}
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawRectangle(float x, float y, float width, float height)
//-----------------------------------------------------------------------------
{
	SetCurrentDC();
	int top = YLOG2DEV(y);
	int left = XLOG2DEV(x);
	int bottom = YLOG2DEV(y + height);
	int right = XLOG2DEV(x + width);
	Rect theRect = {top, left, bottom, right};
	if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
	{
		if (cMacCurrentTool != kBrushTool) wxMacSetCurrentTool(kBrushTool);
		PaintRect(&theRect);
	}

	if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	{
		if (cMacCurrentTool != kPenTool) wxMacSetCurrentTool(kPenTool);
		FrameRect(&theRect);
	}
	CalcBoundingBox(x, y);
	CalcBoundingBox(x + width, y + height);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawRoundedRectangle
					(float x, float y, float width, float height, float radius)
{
	SetCurrentDC();
	if (radius < 0.0) radius = 0.0; // Negative radius can crash your ENTIRE X server. Wow!
    
	int phys_radius = XLOG2DEVREL(radius);

	int phys_rwidth = phys_radius * 2;
	int phys_rheight = phys_rwidth;

	int top = YLOG2DEV(y);
	int left = XLOG2DEV(x);
	int bottom = YLOG2DEV(y + height);
	int right = XLOG2DEV(x + width);
	Rect theRect = {top, left, bottom, right};

	if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
	{
		if (cMacCurrentTool != kBrushTool) wxMacSetCurrentTool(kBrushTool);
		PaintRoundRect(&theRect, phys_rwidth, phys_rheight);
	}

	if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	{
		if (cMacCurrentTool != kPenTool) wxMacSetCurrentTool(kPenTool);
		FrameRoundRect(&theRect, phys_rwidth, phys_rheight);
	}

	CalcBoundingBox(x, y);
	CalcBoundingBox(x + width, y + height);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawEllipse(float x, float y, float width, float height)
{
	SetCurrentDC();
	int top = YLOG2DEV(y);
	int left = XLOG2DEV(x);
	int bottom = YLOG2DEV(y + height);
	int right = XLOG2DEV(x + width);
	Rect theRect = {top, left, bottom, right};
	if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
	{
		if (cMacCurrentTool != kBrushTool) wxMacSetCurrentTool(kBrushTool);
		PaintOval(&theRect);
	}

	if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
	{
		if (cMacCurrentTool != kPenTool) wxMacSetCurrentTool(kPenTool);
		FrameOval(&theRect);
	}
	CalcBoundingBox(x, y);
	CalcBoundingBox(x + width, y + height);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawIcon(wxIcon *icon, float x, float y)
{
	int h, w;
	w = icon->GetWidth();
	h = icon->GetHeight();
	if (!icon->selectedIntoDC && icon->Ok()) {
		wxMemoryDC *mdc;
		mdc = new wxMemoryDC();
		mdc->SelectObject(icon);
		if (mdc->Ok())
			Blit(x, y, w, h, mdc, 0, 0);
		mdc->SelectObject(NULL);
		delete mdc;
	}
}

/*-----------------------------------
	Ideally, Blit() should munged its args and call the Mac ToolBox 
	CopyBits() function. 
    We also have to be aware that the 'source' arg could actually be a
	wxMemoryDC and we may have to switch GWorlds. 
		I.e if (this->device != source->device)
*/
Bool wxCanvasDC::Blit(float xdest, float ydest, float width, float height,
                wxCanvasDC *source, float xsrc, float ysrc, int rop)
{
	if (device != source->device) {
		// Switch Gworld to this
		SetGWorld(cMacDC->macGrafPort(), 0);
		cMacDC->setCurrentUser(NULL); // macDC no longer valid
		SetCurrentDC();
	}
	else {
		SetCurrentDC();
	}
	if (rop == wxCOLOR) {
	  wxMacSetCurrentTool(kColorBlitTool);
	  rop = wxCOPY;
	} else
	  wxMacSetCurrentTool(kBlitTool);
	Bool theResult = FALSE;

	if (pixmap && source->pixmap)
	{
		int mode;
		switch (rop)
		{ // FIXME -  these modes have not be tested
		//	case wxCLEAR:  theMacPenMode = GXclear; break;
			case wxXOR:  
				mode = srcXor; 
				break;
		//	case wxINVERT: theMacPenMode = GXinvert; break;
		//	case wxOR_REVERSE: theMacPenMode = GXorReverse; break;
		//	case wxAND_REVERSE: theMacPenMode = GXandReverse; break;
			case wxAND:
				mode = srcBic;
				break;
			case wxOR: 
				mode = srcOr; 
				break;
			case wxAND_INVERT: 
				mode = notSrcBic; break;
		//	case wxNO_OP: theMacPenMode = GXnoop; break;
		//	case wxNOR: theMacPenMode = GXnor; break;
			case wxEQUIV: 
				mode = notSrcXor; break;
			case wxSRC_INVERT: 
				mode = notSrcCopy; break;
			case wxOR_INVERT: 
				mode = notSrcOr; break;
		//	case wxNAND: theMacPenMode = GXnand; break;
		//	case wxSET: theMacPenMode = GXset; break;
			case wxCOPY:
			default:
				mode = srcCopy;
				break;
		}
		int h = height;
		int w = width;
		int x = XLOG2DEV(xdest);
		int y = YLOG2DEV(ydest);
		int ixsrc = source->LogicalToDeviceX(xsrc);
		int iysrc = source->LogicalToDeviceY(ysrc);
		Rect srcr = {iysrc, ixsrc, iysrc + h, ixsrc + w};
		Rect destr = {y, x, y+h, x+w };
		// Lock PixMaps
		int rs;
		rs = ::LockPixels(pixmap);
		PixMapHandle srpixh = source->pixmap;
		rs = ::LockPixels(srpixh);
		CopyBits((BitMap *) (* srpixh),   (BitMap *)(* pixmap),
			&srcr, &destr, mode, NULL);
		::UnlockPixels(pixmap);
		::UnlockPixels(srpixh);
		CalcBoundingBox(xdest, ydest);
		CalcBoundingBox(xdest + width, ydest + height);
		theResult = TRUE;
	}

  	return theResult;
}

void wxCanvasDC::TryColour(wxColour *src, wxColour *dest)
{
  SetCurrentDC();

  if (Colour) {
	RGBColor pixel = src->pixel;
	
	Index2Color(Color2Index(&pixel), &pixel);
	
	dest->Set(pixel.red >> 8, pixel.green >> 8, pixel.blue >> 8); 
  } else {
	unsigned char red = src->Red();
	unsigned char blue = src->Blue();
	unsigned char green = src->Green();
	Bool isWhiteColour =
		(red == (unsigned char )255 &&
		 blue == (unsigned char)255 &&
		 green == (unsigned char)255);
	if (isWhiteColour)
		dest->Set(255, 255, 255);
	else
		dest->Set(0, 0, 0);
  }
}
