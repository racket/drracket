///////////////////////////////////////////////////////////////////////////////
// File:	wx_dcpr3.cc
// Purpose:	Print Canvas device context implementation (Macintosh version) (part 3)
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
//#include "wx_dccan.h"
#include "wx_utils.h"


// Declarations local to this file
#define YSCALE(y) (yorigin - (y))
#define     wx_round(a)    (int)((a)+.5)

extern CGrafPtr wxMainColormap;

//-----------------------------------------------------------------------------
void wxPrinterDC::DrawText(const char* text, float x, float y, Bool use16)
{
	SetCurrentDC();
	wxColour current_colour;

// CHANGES FOR MAJESTIC VIEWER LjB Moonface, Inc.
	::TextFont(font->GetMacFontNum());
	::TextSize(font->GetPointSize());
	::TextFace(font->GetMacFontStyle());

    //if (cMacCurrentTool != kPenTool)
      wxMacSetCurrentTool(kPenTool);

	FontInfo fontInfo;
	::GetFontInfo(&fontInfo);
	MoveTo(XLOG2DEV(x), YLOG2DEV(y + fontInfo.ascent)); // move pen to start drawing text
	int theStrlen = strlen(text);
	::DrawText(text, 0, theStrlen); // WCH: kludge, mac procedure same name as wxWindows method


	float w, h;
	GetTextExtent(text, &w, &h);
	CalcBoundingBox(x + w, y + h);
	CalcBoundingBox(x, y);
}

//-----------------------------------------------------------------------------
float wxPrinterDC::GetCharHeight(void)
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
float wxPrinterDC::GetCharWidth(void)
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
void wxPrinterDC::GetTextExtent(const char* string, float* x, float* y, float* descent,
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