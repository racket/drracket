///////////////////////////////////////////////////////////////////////////////
// File:	wxRectBorder.cc
// Purpose:	Macintosh RectBorder implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wxRectBorder.h"
#include "wx_area.h"

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxRectBorder::wxRectBorder // Constructor (given parentArea)
	(
		wxArea*		parentArea,
		int			margin,
		Direction	direction,
		int         whitespace,
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
	cWhitespace = whitespace;
	parentArea->SetMargin(margin, direction);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxRectBorder::~wxRectBorder(void)
{
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

void wxRectBorder::DoShow(Bool on)
{
	if (!CanShow(on)) return;
	
	if (!on) {
		SetCurrentDC();

		int clientWidth, clientHeight;
		GetClientSize(&clientWidth, &clientHeight);
		Rect clientRect = {0, 0, clientHeight, clientWidth};
		
		::EraseRect(&clientRect);
		::InvalRect(&clientRect);
	}
	
	wxWindow::DoShow(on);
}

//-----------------------------------------------------------------------------
void wxRectBorder::Paint(void)
{
	if (cHidden) return;

	SetCurrentDC();

	int clientWidth, clientHeight;
	GetClientSize(&clientWidth, &clientHeight);
	Rect clientRect = {0, 0, clientHeight, clientWidth};
	int margin;

	margin = ParentArea()->Margin().Offset(Direction::wxTop) - cWhitespace;
	if (margin > 0) {
		::PenSize(margin, margin);
		::MoveTo(clientRect.left, clientRect.top);
		::LineTo(clientRect.right - margin, clientRect.top);
	}

	margin = ParentArea()->Margin().Offset(Direction::wxBottom) - cWhitespace;
	if (margin > 0) {
		::PenSize(margin, margin);
		::MoveTo(clientRect.left, clientRect.bottom - margin);
		::LineTo(clientRect.right - margin, clientRect.bottom - margin);
	}

	margin = ParentArea()->Margin().Offset(Direction::wxLeft) - cWhitespace;
	if (margin > 0) {
		::PenSize(margin, margin);
		::MoveTo(clientRect.left, clientRect.top);
		::LineTo(clientRect.left, clientRect.bottom - margin);
	}

	margin = ParentArea()->Margin().Offset(Direction::wxRight) - cWhitespace;
	if (margin > 0) {
		::PenSize(margin, margin);
		::MoveTo(clientRect.right - margin, clientRect.top);
		::LineTo(clientRect.right - margin, clientRect.bottom - margin);
	}
}
