///////////////////////////////////////////////////////////////////////////////
// File:	wxArea.cc
// Purpose:	window area (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wx_area.h"
#include "wx_win.h"
#include "wx_frame.h"
#include "wx_utils.h"
#include "wx_screen.h"

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Constructors
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxArea::wxArea
(
	wxWindow* parentWindow
) :
	cWindows (wxChildList())
{
	__type = wxTYPE_AREA;	//cjc
	//__type = wxTYPE_PANEL; // CJC, WCH kludge
	if (!parentWindow) wxFatalError("No parentWindow for wxArea");

	cParentWindow = parentWindow; /*OLD*/
	parentWindow->Areas()->Insert(this);

	WXGC_IGNORE(cParentWindow);
}

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Destructor
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxArea::~wxArea(void)
{
	wxChildNode *node, *next;
	for (node = cWindows.First(); node; node = next) {
		next = node->Next();
		wxWindow *win = (wxWindow *)node->Data();
		if (win)
		  delete win;
	}
	if (cParentWindow) cParentWindow->OnDeleteChildArea(this);
}

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Geometry methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxMargin wxArea::Margin(void) { return cMargin; }

//-----------------------------------------------------------------------------
// Margin from this area to outer area
//-----------------------------------------------------------------------------
wxMargin wxArea::Margin(wxArea* outerArea)
{
	wxMargin result;
	wxArea* area = this;
	wxWindow* window = ParentWindow();
	while (area && area != outerArea)
	{
		area = area->Previous();
		if (area)
		{
			result += area->Margin();
		}
		else
		{
			if (window != wxScreen::gScreenWindow)
			{
				result += window->Margin(window->ParentArea());
				area = window->ParentArea();
				window = area->ParentWindow();
			}
		}
	}

	return result;
}

//-----------------------------------------------------------------------------
// Margin from this area to outer window
//-----------------------------------------------------------------------------
wxMargin wxArea::Margin(wxWindow* outerWindow)
{
	wxMargin result;
	wxArea* area = this;
	wxWindow* window = ParentWindow();
	while (area)
	{
		area = area->Previous();
		if (area)
		{
			result += area->Margin();
		}
		else
		{
			if (window != outerWindow)
			{
				if (window != wxScreen::gScreenWindow)
				{
					result += window->Margin(window->ParentArea());
					area = window->ParentArea();
					window = area->ParentWindow();
				}
			}
		}
	}

	return result;
}

//-----------------------------------------------------------------------------
int wxArea::Width(void)
{
	wxMargin margin = Margin(ParentWindow());
	return ParentWindow()->Width() - margin.Offset(Direction::wxHorizontal);
}

//-----------------------------------------------------------------------------
int wxArea::Height(void)
{
	wxMargin margin = Margin(ParentWindow());
	return ParentWindow()->Height() - margin.Offset(Direction::wxVertical);
}

//-----------------------------------------------------------------------------
// Convert from this area c.s. to screen c.s.
//-----------------------------------------------------------------------------
void wxArea::AreaToScreen(int* h, int* v)
{
	wxMargin screenMargin = Margin(wxScreen::gScreenWindow);
	*h += screenMargin.Offset(Direction::wxLeft);
	*v += screenMargin.Offset(Direction::wxTop);
}

//-----------------------------------------------------------------------------
// Convert from screen c.s. to this area c.s.
//-----------------------------------------------------------------------------
void wxArea::ScreenToArea(int* h, int* v)
{
	wxMargin screenMargin = Margin(wxScreen::gScreenWindow);
	*h -= screenMargin.Offset(Direction::wxLeft);
	*v -= screenMargin.Offset(Direction::wxTop);
}

//-----------------------------------------------------------------------------
Bool wxArea::WindowPointInArea(int windowH, int windowV)
{
	wxMargin margin = Margin(ParentWindow());
	int areaH = windowH - margin.Offset(Direction::wxLeft); // area c.s.
	int areaV = windowV - margin.Offset(Direction::wxTop); // area c.s.
	return (0 <= areaH && areaH <= Width() && 0 <= areaV && areaV <= Height());
}

//-----------------------------------------------------------------------------
void wxArea::FrameContentAreaOffset(int* x, int* y)
{
	wxFrame* frame = ParentWindow()->GetRootFrame();
	wxArea* frameContentArea = frame->ContentArea();
	wxMargin frameContentAreaMargin = Margin(frameContentArea);
	*x = frameContentAreaMargin.Offset(Direction::wxLeft);
	*y = frameContentAreaMargin.Offset(Direction::wxTop);
}

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Sizing methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxArea::SetSize(int width, int height)
{
	wxMargin margin = Margin(ParentWindow());
	int	newWindowWidth = width + margin.Offset(Direction::wxHorizontal);
	int newWindowHeight = height + margin.Offset(Direction::wxVertical);
	ParentWindow()->SetWidthHeight(newWindowWidth, newWindowHeight);
}

//-----------------------------------------------------------------------------
void wxArea::SetMargin(int margin, Direction direction)
{
	wxMargin newMargin(margin, direction);
	SetMargin(newMargin, direction);
}

//-----------------------------------------------------------------------------
void wxArea::SetMargin(wxMargin margin, Direction direction)
{
	int oldLeft = cMargin.Offset(Direction::wxLeft);
	int oldTop = cMargin.Offset(Direction::wxTop);
	int oldRight = cMargin.Offset(Direction::wxRight);
	int oldBottom = cMargin.Offset(Direction::wxBottom);

	cMargin.SetMargin(margin, direction);

	int dL = cMargin.Offset(Direction::wxLeft) - oldLeft;
	int dT = cMargin.Offset(Direction::wxTop) - oldTop;
	int dR = cMargin.Offset(Direction::wxRight) - oldRight;
	int dB = cMargin.Offset(Direction::wxBottom) - oldBottom;

	int dW = -(dR + dL);
	int dH = -(dB + dT);
	int dX = dL;
	int dY = dT;
	wxArea* area = Next(); // Resize younger sibling areas
	while (area)
	{
		area->OnSiblingDSize(dW, dH, dX, dY);
		area = area->Next();
	}
}

//-----------------------------------------------------------------------------
void wxArea::SetMargin(wxMargin margin, Direction direction,
						int parentWindowWidth, int parentWindowHeight,
						int parentWindowX, int parentWindowY)
{
// SetSize of parentWindow
	int oldWindowX, oldWindowY;
	cParentWindow->GetPosition(&oldWindowX, &oldWindowY);
	int oldWindowWidth = cParentWindow->Width();
	int oldWindowHeight = cParentWindow->Height();

	cParentWindow->DoSetSize(parentWindowX, parentWindowY, parentWindowWidth, parentWindowHeight);

	int newWindowX, newWindowY;
	cParentWindow->GetPosition(&newWindowX, &newWindowY);
	int newWindowWidth = cParentWindow->Width();
	int newWindowHeight = cParentWindow->Height();
	int dW = newWindowWidth - oldWindowWidth;
	int dH = newWindowHeight - oldWindowHeight;
	int dX = newWindowX - oldWindowX;
	int dY = newWindowY - oldWindowY;

// Notify older siblings and this area of parentWindow resizing
	wxArea* area = First();
	while (area && area != this)
	{
		area->OnSiblingDSize(dW, dH, dX, dY);
		area = area->Next();
	}
	if (area != this) wxFatalError("Error in wxArea::SetMargin");
	OnSiblingDSize(dW, dH, dX, dY);

// SetMargin of this area
	int oldLeft = cMargin.Offset(Direction::wxLeft);
	int oldTop = cMargin.Offset(Direction::wxTop);
	int oldRight = cMargin.Offset(Direction::wxRight);
	int oldBottom = cMargin.Offset(Direction::wxBottom);

	cMargin.SetMargin(margin, direction);

// Notify younger siblings of parentWindow resizing and margin changes for this area
	int dL = cMargin.Offset(Direction::wxLeft) - oldLeft;
	int dT = cMargin.Offset(Direction::wxTop) - oldTop;
	int dR = cMargin.Offset(Direction::wxRight) - oldRight;
	int dB = cMargin.Offset(Direction::wxBottom) - oldBottom;

	dW += -(dR + dL); // accumulate changes for parentWindow and this area
	dH += -(dB + dT); // accumulate changes for parentWindow and this area
	dX += dL; // accumulate changes for parentWindow and this area
	dY += dT; // accumulate changes for parentWindow and this area
	area = Next(); // Resize younger sibling areas
	while (area)
	{
		area->OnSiblingDSize(dW, dH, dX, dY);
		area = area->Next();
	}
}

//-----------------------------------------------------------------------------
void wxArea::OnSiblingDSize(int dW, int dH, int dX, int dY)
{
	// Must change W, H, X, Y of this area: but these are virtual variables.
	// Hence, change is effectively already done.

	OnAreaDSize(dW, dH, dX, dY);
}

//-----------------------------------------------------------------------------
void wxArea::OnAreaDSize(int dW, int dH, int dX, int dY)
{
	if (dW || dH || dX || dY)
	{
		if (this == cParentWindow->ClientArea())
		{ // Notify parent window of client resize.
			// The parent window manages the contents of the client area.
			// Hence, must notify parent window that its client area has resized.
			cParentWindow->OnClientAreaDSize(dW, dH, dX, dY);
		}
		else
		{ // Notify child windows of area resize.
			wxChildNode* childWindowNode = Windows()->First();
			while (childWindowNode)
			{
				wxWindow* childWindow = (wxWindow*)childWindowNode->Data();
				childWindow->OnAreaDSize(dW, dH, dX, dY);
				childWindowNode = childWindowNode->Next();
			}
		}
	}
}

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Tree (windows and areas) methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxWindow* wxArea::ParentWindow(void) { return cParentWindow; }

//-----------------------------------------------------------------------------
wxChildList* wxArea::Windows(void) { return &cWindows; } // kludge

//-----------------------------------------------------------------------------
wxArea* wxArea::First(void)
{
	wxArea* result = NULL;
	wxNode* nodeArea = ParentWindow()->Areas()->First();
	if (nodeArea) result = (wxArea*) nodeArea->Data();

	return result;
}

//-----------------------------------------------------------------------------
wxArea* wxArea::Previous(void)
{
	wxArea* result = NULL;
	wxNode* nodeArea = ParentWindow()->Areas()->Member(this);
	nodeArea = nodeArea->Previous();
	if (nodeArea) result = (wxArea*) nodeArea->Data();

	return result;
}

//-----------------------------------------------------------------------------
wxArea* wxArea::Next(void)
{
	wxArea* result = NULL;
	wxNode* nodeArea = ParentWindow()->Areas()->Member(this);
	nodeArea = nodeArea->Next();
	if (nodeArea) result = (wxArea*) nodeArea->Data();

	return result;
}

//-----------------------------------------------------------------------------
void wxArea::OnDeleteChildWindow(wxWindow* childWindow)
{
	cWindows.DeleteObject(childWindow);
}
