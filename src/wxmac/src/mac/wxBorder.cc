///////////////////////////////////////////////////////////////////////////////
// File:	wxBorder.cc
// Purpose:	Macintosh Border implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wxBorder.h"
#include "wxMacDC.h"
#include "wx_area.h"
#include <Windows.h>
#include "wxBorderArea.h"

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxBorder::wxBorder // Constructor (given parentArea)
	(
		wxArea*		parentArea,
		char*		windowName,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		WXTYPE		objectType
	) :
		wxWindow (windowName, parentArea, x, y, width, height, style)
{
	__type = wxTYPE_BORDER;
	if (width < 0) cWindowWidth = parentArea->Width();
	if (height < 0) cWindowHeight = parentArea->Height();
	SetJustify(Direction::wxAll);
	SetGravitate(Direction::wxTop | Direction::wxLeft);
	cActive = TRUE; // Kludge, so that "ShowAsActive" method will be called
	
	SetEraser(wxCONTROL_BACKGROUND_BRUSH);

	if (GetParent()->IsHidden())
		DoShow(FALSE);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxBorder::~wxBorder(void)
{
#if 0
	// CJC - Lets Hack - we still have leaks only now they are 'unknown'
	wxArea* pa = cParentArea;
	cParentArea = NULL;
	delete pa;
#endif
}

//-----------------------------------------------------------------------------
void wxBorder::ChangeColour(void)
{
}

//-----------------------------------------------------------------------------
void wxBorder::DoShow(Bool show)
{
	wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxBorder::ShowAsActive(Bool flag) // mac platform only
{
	if (cHidden) return;

	if (flag)
	{
		// The following is a kludge, to paint border before subsequent update event
		Paint();
		int clientWidth, clientHeight;
		GetClientSize(&clientWidth, &clientHeight);
		int margin = ParentArea()->Margin().Offset(Direction::wxTop);
		Rect clientRect = {0, 0, clientHeight, clientWidth};
		RgnHandle outerRgn = ::NewRgn(); CheckMemOK(outerRgn);
		::RectRgn(outerRgn, &clientRect);
		RgnHandle innerRgn = ::NewRgn(); CheckMemOK(innerRgn);
		::CopyRgn(outerRgn, innerRgn); InsetRgn(innerRgn, margin, margin);
		::DiffRgn(outerRgn, innerRgn, outerRgn);
		::ValidRgn(outerRgn);
		::DisposeRgn(outerRgn);
		::DisposeRgn(innerRgn);
	}
}