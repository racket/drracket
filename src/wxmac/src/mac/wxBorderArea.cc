///////////////////////////////////////////////////////////////////////////////
// File:	wxBorderArea.cc
// Purpose:	Label area (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wxBorderArea.h"
#include "wxRectBorder.h"

//=============================================================================
// Construction methods
//=============================================================================

//-----------------------------------------------------------------------------
wxBorderArea::wxBorderArea(wxWindow* parentWindow, int margin, Direction direction,
							int whitespace) :
		wxArea(parentWindow)
{
	cBorder = new wxRectBorder(this, margin, direction, whitespace);
}

//-----------------------------------------------------------------------------
wxBorderArea::~wxBorderArea(void)	// destructor
{
//	delete cBorder;
}