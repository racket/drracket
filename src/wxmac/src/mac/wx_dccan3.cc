///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan3.cc
// Purpose:	Canvas device context implementation (Macintosh version) (part 3)
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
#include "wx_utils.h"

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
void wxCanvasDC::DrawText(const char* text, float x, float y, Bool use16)
{
	SetCurrentDC();
	wxColour current_colour;

	// CHANGES FOR MAJESTIC VIEWER LjB Moonface, Inc.
	::TextFont(font->GetMacFontNum());
	::TextSize(font->GetPointSize());
	::TextFace(font->GetMacFontStyle());
    wxMacSetCurrentTool(kPenTool); // end LjB

	FontInfo fontInfo;
	Point start, end;
	::GetFontInfo(&fontInfo);
	start.h = XLOG2DEV(x);
	start.v = YLOG2DEV(y + fontInfo.ascent);
	MoveTo(start.h, start.v); // move pen to start drawing text
	int theStrlen = strlen(text);
	::DrawText(text, 0, theStrlen); // WCH: kludge, mac procedure same name as wxWindows method

	// mflatt: look at pen, use distance travelled instead of calculating 
    // the length of the string (again)
	float w, h;
	::GetPen(&end);
	w = (end.h - start.h) / (logical_scale_x * user_scale_x);
	h = (end.v - start.v) / (logical_scale_y * user_scale_y);

	CalcBoundingBox(x + w, y + h);
	CalcBoundingBox(x, y);
}

//-----------------------------------------------------------------------------
float wxCanvasDC::GetCharHeight(void)
//-----------------------------------------------------------------------------
{
	int theCharHeight;
	if (!font)
  		theCharHeight = font->GetCharHeight();
  	else
  		theCharHeight = 12;

  	return XDEV2LOGREL(theCharHeight);
}

//-----------------------------------------------------------------------------
float wxCanvasDC::GetCharWidth(void)
//-----------------------------------------------------------------------------
{
	int theCharWidth;
	if (!font)
  		theCharWidth = font->GetCharWidth();
  	else
  		theCharWidth = 12;

  	return XDEV2LOGREL(theCharWidth);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::GetTextExtent(const char* string, float* x, float* y, float* descent,
  						float* externalLeading, wxFont* the_font, Bool use16)
{
	float x2, y2, descent2, externalLeading2;
	if (the_font)
	{
		the_font->GetTextExtent((char *)string, &x2, &y2, &descent2, &externalLeading2, use16);
	}
	else if (font)
	{
		font->GetTextExtent((char *)string, &x2, &y2, &descent2, &externalLeading2, use16);
	}
	else
	{
        *x = -1;
        *y = -1;
		if (descent) *descent = 0.0;
		if (externalLeading) *externalLeading = 0.0;
		return;
	}

	*x = XDEV2LOGREL(x2);
	*y = YDEV2LOGREL(y2);
	if (descent) *descent = descent2;
	if (externalLeading) *externalLeading = externalLeading2;
}