///////////////////////////////////////////////////////////////////////////////
// File:	wx_dcpr2.cc
// Purpose:	Print Canvas device context implementation (Macintosh version) (part 2)
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
#include <Quickdraw.h>
#include <Printing.h>
#include "wx_dcpr.h"
#include "wx_dccan.h"
#include "wx_utils.h"
#include "wx_canvs.h"
#include "wx_privt.h"

#include <QDOffScreen.h>


class wxCanvasDC;

// Declarations local to this file
#define YSCALE(y) (yorigin - (y))
#define     wx_round(a)    (int)((a)+.5)

extern CGrafPtr wxMainColormap;

//-----------------------------------------------------------------------------
void wxPrinterDC::Clear(void)
//-----------------------------------------------------------------------------
{
	SetCurrentDC();

	float w, h;
	GetSize(&w, &h);

	Rect theClearRect = {0, 0, h, w};
	::EraseRect(&theClearRect);
}

void wxPrinterDC::GetSize(float *width, float *height)
{
	*width = pixmapWidth;
	*height = pixmapHeight;
}

//-----------------------------------------------------------------------------
void wxPrinterDC::CrossHair(float x, float y)
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
void wxPrinterDC::FloodFill(float x, float y, wxColour *col, int style)
//=============================================================================
{
	SetCurrentDC();
}

//-----------------------------------------------------------------------------
Bool wxPrinterDC::GetPixel(float x, float y, wxColour *col)
//=============================================================================
{
	SetCurrentDC();

	return FALSE ;
}

//-----------------------------------------------------------------------------
void wxPrinterDC::IntDrawLine(int x1, int y1, int x2, int y2)
//-----------------------------------------------------------------------------
{
	SetCurrentDC();
	if (cMacCurrentTool != kPenTool) wxMacSetCurrentTool(kPenTool);
	wxMacDrawLine(XLOG2DEV(x1), YLOG2DEV(y1), XLOG2DEV(x2), YLOG2DEV(y2));
	CalcBoundingBox(x1, y1); // WCH: not in original. Why not?
	CalcBoundingBox(x2, y2); // WCH: not in original. Why not?
}

//-----------------------------------------------------------------------------
void wxPrinterDC::DrawLine(float x1, float y1, float x2, float y2)
//-----------------------------------------------------------------------------
{
	SetCurrentDC();
	if (cMacCurrentTool != kPenTool) wxMacSetCurrentTool(kPenTool);
	wxMacDrawLine(XLOG2DEV(x1), YLOG2DEV(y1), XLOG2DEV(x2), YLOG2DEV(y2));
	CalcBoundingBox(x1, y1);
	CalcBoundingBox(x2, y2);
}

//-----------------------------------------------------------------------------
void wxPrinterDC::DrawArc(float x1,float y1,float x2,float y2,float xc,float yc)
{
	SetCurrentDC();


  if (current_brush && current_brush->GetStyle() != wxTRANSPARENT)
  {
    if (cMacCurrentTool != kBrushTool) wxMacSetCurrentTool(kBrushTool);
  }

  if (current_pen && current_pen->GetStyle() != wxTRANSPARENT)
  {
    if (cMacCurrentTool != kPenTool) wxMacSetCurrentTool(kPenTool);
  }
  CalcBoundingBox(x1, y1);
  CalcBoundingBox(x2, y2);
}

//-----------------------------------------------------------------------------
void wxPrinterDC::DrawPoint(float x, float y)
//-----------------------------------------------------------------------------
{
	SetCurrentDC();
	if (cMacCurrentTool != kPenTool) wxMacSetCurrentTool(kPenTool);
	wxMacDrawPoint(XLOG2DEV(x), YLOG2DEV(y));
	CalcBoundingBox(x, y);
}

//-----------------------------------------------------------------------------
void wxPrinterDC::DrawPolygon(int n, wxPoint points[],
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
void wxPrinterDC::DrawLines(int n, wxIntPoint points[], int xoffset, int yoffset)
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
void wxPrinterDC::DrawLines(int n, wxPoint points[], float xoffset, float yoffset)
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
void wxPrinterDC::DrawRectangle(float x, float y, float width, float height)
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
void wxPrinterDC::DrawRoundedRectangle
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
void wxPrinterDC::DrawEllipse(float x, float y, float width, float height)
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
void wxPrinterDC::DrawIcon(wxIcon *icon, float x, float y)
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

//-----------------------------------------------------------------------------
Bool wxPrinterDC::Blit(float xdest, float ydest, float width, float height,
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

	if (source->pixmap)
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
          // LJB Moonface, Inc. Changes to make wxmac stuff
          // work with the HTML viewer "Majestic"
          // for the mac, take the offscreen pixmap and blit
          // it to the screen as defined by the destination
          // rectangle information
		int w = width;
		int h = height;
		int x = XLOG2DEV(xdest);
		int y = YLOG2DEV(ydest);
		Rect srcr = {0, 0, h, w};
		Rect destr = {y, x, y+h, x+w };
        BitMap *dstbm = (BitMap *) &((GrafPtr)(cMacDC->macGrafPort()))->portBits;
		// Lock PixMaps
		//PixMapHandle srpixh = source->pixmap;
		// mflatt: this should be pixmap, right?
		PixMapHandle srpixh = source->pixmap;
		int rs = ::LockPixels(srpixh);


		CopyBits((BitMap *) (* srpixh),   (dstbm),	&srcr, &destr, mode, NULL);

		::UnlockPixels(srpixh);
		CalcBoundingBox(xdest, ydest);
		CalcBoundingBox(xdest + width, ydest + height);
		theResult = TRUE;
	}

  	return theResult;
}