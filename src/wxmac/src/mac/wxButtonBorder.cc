///////////////////////////////////////////////////////////////////////////////
// File:	wxButtonBorder.cc
// Purpose:	Macintosh ButtonBorder implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wxButtonBorder.h"
#include "wx_area.h"

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxButtonBorder::wxButtonBorder // Constructor (given parentArea)
	(
		wxArea*		parentArea,
		int			margin,
		Direction	direction,
 		char*		windowName,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		WXTYPE		objectType
	) :
		wxBorder (parentArea, windowName, x, y, width, height, style, objectType)
{
	parentArea->SetMargin(margin, direction);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxButtonBorder::~wxButtonBorder(void)
{
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxButtonBorder::Paint(void)
{
	if (cHidden) return;

	int margin = ParentArea()->Margin().Offset(Direction::wxTop);
	if (margin)
	{	
		int clientWidth = ClientArea()->Width();
		int clientHeight = ClientArea()->Height();
		Rect clientRect = {0, 0, clientHeight, clientWidth};
		SetCurrentDC();
		PenState oldPenState;
		::GetPenState(&oldPenState);
		::PenNormal();
		::PenSize(margin -1 , margin - 1);
		::FrameRoundRect(&clientRect, 16, 16);
		::SetPenState(&oldPenState);
	}
}