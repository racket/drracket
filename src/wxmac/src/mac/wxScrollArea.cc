///////////////////////////////////////////////////////////////////////////////
// File:	wxScrollArea.cc
// Purpose:	Scroll area (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wxScrollArea.h"
#include "wx_sbar.h"
#include "wx_utils.h"

//=============================================================================
// Construction methods
//=============================================================================

//-----------------------------------------------------------------------------
wxScrollArea::wxScrollArea
	(
		wxWindow*	parentWindow,
		wxWindow*	parentScrollWindow,
		long		style
	) :
		wxArea (parentWindow),
		cStyle (style),
		cVScrollBar (NULL),
		cHScrollBar (NULL)
{
	if (!parentScrollWindow) wxFatalError("No parentScrollWindow for wxScrollArea");

	Bool bothScrolls = ((cStyle & wxVSCROLL) && (cStyle & wxHSCROLL));

	// mflatt:
    //  While a scrollbar should overap a frame edge when the scrollbar is positioned
    //    along a frame edge, the full scroll bar outline must be drawn when a scrollbar
    //    is not on a frame edge. This is why I went back to the old scrollbar positioning.
    //  If someone can ge the new positioning but still have all of the scrollbar outline
    //    drawn, I will be happy.

	if (cStyle & wxVSCROLL)
	{
		cVScrollBar = new wxScrollBar(this, NULL, "",
							0, 0, kVScrollBarWidth, 0, wxVSCROLL);
		parentScrollWindow->AddChildScrollWindow(cVScrollBar);

#if 0
 		// Vertical scrollbar should overlap top/right edges of frame
 		cVScrollBar->GravitateJustify
 					(Direction::wxRight | Direction::wxTop,
 					 Direction::wxVertical,
 					 0, -1,
 					 Width() + 1,
 					 Height() - (bothScrolls ? kHScrollBarHeight - 1 : 0) + 1);
 		cVScrollBar->SetJustify(Direction::wxVertical);
 		cVScrollBar->SetGravitate(Direction::wxRight);
		SetMargin(kVScrollBarWidth - 1, Direction::wxRight);
 		// Inset parent's client area
 		parentScrollWindow->ClientArea()->SetMargin(kVScrollBarWidth - 1, Direction::wxRight);
#else
		cVScrollBar->GravitateJustify
					(Direction::wxRight | Direction::wxTop,
					 Direction::wxVertical,
					 0, 0,
					 Width(),
					 Height() - (bothScrolls ? kHScrollBarHeight - 1 : 0));
		cVScrollBar->SetJustify(Direction::wxVertical);
		cVScrollBar->SetGravitate(Direction::wxRight);
		SetMargin(kVScrollBarWidth, Direction::wxRight);
#endif
	}

	if (cStyle & wxHSCROLL)
	{
		cHScrollBar = new wxScrollBar(this, NULL, "",
							0, 0, 0, kHScrollBarHeight, wxHSCROLL);
		parentScrollWindow->AddChildScrollWindow(cHScrollBar);
#if 0 //GRW
 		// Horizontal scrollbar should overlap left/bottom edges of frame
 		cHScrollBar->GravitateJustify
 					(Direction::wxBottom | Direction::wxLeft,
 					 Direction::wxHorizontal,
 					 -1, 0,
 					 Width() - (bothScrolls ? kVScrollBarWidth - 1 : 0) + 1,
 					 Height() + 1);
 		cHScrollBar->SetJustify(Direction::wxHorizontal);
 		cHScrollBar->SetGravitate(Direction::wxBottom);
		SetMargin(kHScrollBarHeight - 1, Direction::wxBottom);
 		// Inset parent's client area
 		parentScrollWindow->ClientArea()->SetMargin(kHScrollBarHeight - 1, Direction::wxBottom);
#else
		cHScrollBar->GravitateJustify
					(Direction::wxBottom | Direction::wxLeft,
					 Direction::wxHorizontal,
					 0, 0,
					 Width() - (bothScrolls ? kVScrollBarWidth - 1 : 0),
					 Height());
		cHScrollBar->SetJustify(Direction::wxHorizontal);
		cHScrollBar->SetGravitate(Direction::wxBottom);
		SetMargin(kHScrollBarHeight, Direction::wxBottom);
#endif
	}
}

//-----------------------------------------------------------------------------
wxScrollArea::~wxScrollArea(void)	// destructor
{
}
