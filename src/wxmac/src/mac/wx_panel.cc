///////////////////////////////////////////////////////////////////////////////
// File:	wx_panel.cc
// Purpose:	wxPanel class implementation (Mac version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wx_panel.h"
#include "wx_frame.h"
#include "wx_area.h"
#include "wxBorderArea.h"
#include "wxRectBorder.h"
#include "wxMacDC.h"

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxPanel::wxPanel // Constructor (given parentArea)
	(
		wxArea*		parentArea,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbPanel (windowName, parentArea, x, y, width, height, style)
{
	CreateWxPanel();
}

//-----------------------------------------------------------------------------
wxPanel::wxPanel // Constructor (given parentFrame)
	(
		wxFrame*	parentFrame,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbPanel (windowName, parentFrame, x, y, width, height, style)
{
	CreateWxPanel();
}

//-----------------------------------------------------------------------------
wxPanel::wxPanel // Constructor (given parentPanel)
	(
		wxPanel*	parentPanel,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbPanel (windowName, parentPanel, x, y, width, height, style)
{
	CreateWxPanel();
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxPanel::~wxPanel(void)
{
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxPanel::CreateWxPanel(void) // common constructor initialization
{
	InitDefaults();

	SetEraser(wxCONTROL_BACKGROUND_BRUSH);

	if (cStyle & wxBORDER) 
	  cPanelBorder = new wxBorderArea(this, 1, Direction::wxAll, 1);
	
	if (GetParent()->IsHidden())
		DoShow(FALSE);
}

//-----------------------------------------------------------------------------
void wxPanel::InitDefaults(void)
{
	// For absolute-positioning auto layout
	cursor_x = PANEL_LEFT_MARGIN;
	cursor_y = PANEL_TOP_MARGIN;
	max_width = 0;
	max_height = 0;
	max_line_height = 0;
	currentRow = 0;
	currentCol = 0;
	last_created = NULL;
	cPanelBorder = NULL;
}

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Item placement methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxPanel::NewLine(void)
{
	AdvanceCursor(NULL); // Kludge (new DESIGN)
	cursor_x = initial_hspacing;
    cursor_y = max_height + vSpacing;
    if (cursor_y > max_height) max_height = cursor_y;
    max_line_height = 0;
}

//-----------------------------------------------------------------------------
void wxPanel::NewLine(int pixels)
{
	AdvanceCursor(NULL); // Kludge (new DESIGN)
	cursor_x = initial_hspacing;
    cursor_y = max_height + pixels;
    if (cursor_y > max_height) max_height = cursor_y;
    max_line_height = 0;
}

//-----------------------------------------------------------------------------
void wxPanel::Tab(void)
{
	AdvanceCursor(NULL); // Kludge (new DESIGN)
	cursor_x += hSpacing;
	if (cursor_x > max_width) max_width = cursor_x;
}

//-----------------------------------------------------------------------------
void wxPanel::Tab(int pixels)
{
	AdvanceCursor(NULL); // Kludge (new DESIGN)
	cursor_x += pixels;
	if (cursor_x > max_width) max_width = cursor_x;
}

//-----------------------------------------------------------------------------
int wxPanel::GetHorizontalSpacing(void) { return hSpacing; }

//-----------------------------------------------------------------------------
void wxPanel::SetHorizontalSpacing(int sp)
{
	hSpacing = sp;
	current_hspacing = sp;
}

//-----------------------------------------------------------------------------
int wxPanel::GetVerticalSpacing(void) { return vSpacing; }

//-----------------------------------------------------------------------------
void wxPanel::SetVerticalSpacing(int sp)
{
	vSpacing = sp;
	current_vspacing = sp;
}

//-----------------------------------------------------------------------------
void wxPanel::GetCursor(int* x, int* y)
{
	AdvanceCursor(NULL);
	*x = cursor_x;
	*y = cursor_y;
}

//-----------------------------------------------------------------------------
void wxPanel::SetItemCursor(int x, int y)
{
	last_created = NULL;
	cursor_x = x;
	cursor_y = y;
}

//-----------------------------------------------------------------------------
// Update next cursor position
//-----------------------------------------------------------------------------
void wxPanel::AdvanceCursor(wxWindow* item)
{
//''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
// Major design change: I calculate cursor_x, cursor_y based on last_created,
//                      and I store the current item, for next time.
//
// In this way, I can change the size of the current item without harm.
// When I start a new item, the last_created should have correct size by then.
//
//''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

	if (last_created) // Kludge (new DESIGN)
	{
		int width, height;
		int x, y;
		last_created->GetSize(&width, &height);
		last_created->GetPosition(&x, &y);
	
		if ((x + width) > max_width) max_width = x + width;
		if ((y + height) > max_height) max_height = y + height;
		if (height > max_line_height) max_line_height = height;
	
		cursor_x = x + width + hSpacing;
		cursor_y = y;
	}

	last_created = item;
}

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Sizing methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxPanel::Centre(int direction)
{
	int width, height, panel_width, panel_height, new_x, new_y;

	wxPanel* father = (wxPanel*)GetParent();
	if (!father) return;

	father->GetClientSize(&panel_width, &panel_height);
	GetSize(&width, &height);

	new_x = cWindowX;
	new_y = cWindowY;

	if (direction & wxHORIZONTAL)
		new_x = (int)((panel_width - width)/2);

	if (direction & wxVERTICAL)
		new_y = (int)((panel_height - height)/2);

	SetSize(new_x, new_y, cWindowWidth, cWindowHeight, wxPOS_USE_MINUS_ONE);
}

//-----------------------------------------------------------------------------
void wxPanel::Fit(void)
{ // Fit panel around its items
	int maxX = 0;
	int maxY = 0;
	wxChildNode* childWindowNode = ClientArea()->Windows()->First();
	while (childWindowNode)
	{
		wxWindow* childWindow = (wxWindow*)childWindowNode->Data();
		int x, y, w, h;
		childWindow->GetPosition(&x, &y);
		childWindow->GetSize(&w, &h);
		if ((x + w) > maxX) maxX = x + w;
		if ((y + h) > maxY) maxY = y + h;
		childWindowNode = childWindowNode->Next();
	}

	SetClientSize(maxX + initial_hspacing, maxY + initial_vspacing);
}

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Tree methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //-----------------------------------------------------------------------------
  // tom: new two are needed to be able to destroy children and be able to
  // insert a new child afterwards
 
void wxPanel::DestroyChildren(void) 
{
	wxWindow::DestroyChildren();
	last_created = NULL;
}
 
void wxPanel::OnDeleteChild(wxWindow* win) 
{
	if (last_created==win)
		last_created = NULL;
}


//-----------------------------------------------------------------------------
void wxPanel::AddChild(wxObject* child) // WCH: why isn't the type wxWindow*?
{
	if (((wxWindow*)child)->ParentArea() == cClientArea) // WCH: kludge
	{
		if (!has_child)
		{
			initial_hspacing = cursor_x; // WCH: not quite correct
			initial_vspacing = cursor_y; // WCH: not quite correct
			has_child = TRUE;
		}
		AdvanceCursor((wxWindow*)child); // WCH: want AddChild(wxWindow* child)
	}

	children->Append(child);
}

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxPanel::SetBackgroundColour(wxColour*col)
{
	backColour = col;
	ChangeColour();
}

//-----------------------------------------------------------------------------
void wxPanel::SetLabelColour(wxColour*col)
{
	labelColour = col;
	ChangeColour();
}

//-----------------------------------------------------------------------------
void wxPanel::SetButtonColour(wxColour*col)
{
	buttonColour = col;
	ChangeColour();
}

//-----------------------------------------------------------------------------
void wxPanel::ChangeColour(void)
{
}

//-----------------------------------------------------------------------------
void wxPanel::DoShow(Bool show)
{
	if (!CanShow(show)) return;

    if (cPanelBorder)
	  ((wxBorderArea *)cPanelBorder)->cBorder->DoShow(show);

	wxNode* areaNode = cAreas.First();
	while (areaNode)
	{
		wxArea* area = (wxArea*)areaNode->Data();
		wxChildNode* childWindowNode = area->Windows()->First();
		while (childWindowNode)
		{
			wxWindow* childWindow = (wxWindow*)childWindowNode->Data();
			childWindow->DoShow(show);
			childWindowNode = childWindowNode->Next();
		}
		areaNode = areaNode->Next();
	}
	wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxPanel::Paint(void)
{
	if (cHidden) return;
	wxCanvas::Paint();
}

//-----------------------------------------------------------------------------
void wxPanel::OnChar(wxKeyEvent& event)
 {
 	switch (event.keyCode)
	{
 		case 0x03:	// enter
 		case 0x0D:	// return
 			OnDefaultAction(NULL);
 			return;

 		case 0x09:	// tab
 		{
 			// Step through panel items that want focus
 			wxFrame* rootFrame = GetRootFrame();
 			wxWindow* currentWindow = rootFrame->GetFocusWindow();
 			wxChildNode* childWindowNode;
 			wxWindow* childWindow;
 			wxWindow* wrapWindow = NULL;

 			// Tab steps forward, Shift-Tab steps backwards
 			Bool backwards = event.shiftDown;
#if 0
 			if (backwards)
 				childWindowNode = cClientArea->Windows()->Last();
 			else
#endif
 				childWindowNode = cClientArea->Windows()->First();

 			// Find current focus window
 			while (childWindowNode)
 			{
 				childWindow = (wxWindow*)childWindowNode->Data();
 				// In case we end up wrapping
 				if (!wrapWindow && childWindow->WantsFocus() && childWindow->CanAcceptEvent())
 					wrapWindow = childWindow;
 				// We actually want to be positioned one after/before the current window
#if 0
 				if (backwards)
 					childWindowNode = childWindowNode->Previous();
 				else
#endif
 					childWindowNode = childWindowNode->Next();
 				if (childWindow  == currentWindow)
 					break;
 			}
 			// Look for next window in chain that wants focus
 			while (childWindowNode)
 			{
 				childWindow = (wxWindow*)childWindowNode->Data();
				if (childWindow->WantsFocus() && childWindow->CanAcceptEvent())
 					break;
#if 0 					
 				if (backwards)
 					childWindowNode = childWindowNode->Previous();
 				else
#endif 
 					childWindowNode = childWindowNode->Next();
 			}
 			// Set new focus window
 			rootFrame->SetFocusWindow((childWindowNode)?childWindow:wrapWindow);
			return;
 		}

 		default:
 			break;
 	}
 }


Bool wxPanel::WantsFocus()
{
	return TRUE;
}

void wxPanel::ChangeToGray(Bool gray)
{
	ChildrenInternalGray(gray);
	wxWindow::ChangeToGray(gray);
}
