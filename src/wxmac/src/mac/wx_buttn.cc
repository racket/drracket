///////////////////////////////////////////////////////////////////////////////
// File:	wx_buttn.cc
// Purpose:	Panel item button implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wx_buttn.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_panel.h"
#include "wx_area.h"
#include "wxBorderArea.h"
#include "wb_gdi.h"
#include <Windows.h>
#include "wxButtonBorder.h"

#if 1
#define MIN_BUTTON_WIDTH 58
#define BUTTON_H_SPACE 12
#define BUTTON_V_SPACE 4
#else
/* Original parameters */
#define MIN_BUTTON_WIDTH 60
#define BUTTON_H_SPACE 20
#define BUTTON_V_SPACE 10
#endif

#define IB_MARGIN_X 2
#define IB_MARGIN_Y 2

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxButton::wxButton // Constructor (given parentPanel, label)
	(
		wxPanel*	parentPanel,
		wxFunction	function,
		char*		label,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbButton (parentPanel, x, y, width, height, style, windowName)
		
{
	Create(parentPanel, function, label, x, y, width, height, style, windowName, objectType);
}

void wxButton::Create // Real constructor (given parentPanel, label)
	(
		wxPanel*	parentPanel,
		wxFunction	function,
		char*		label,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) 	
{
    buttonBitmap = NULL;
	cColorTable = NULL;
		
	Callback(function);

	font = buttonFont; // WCH: mac platform only
	
	label = wxItemStripLabel(label);

	if (width <= 0 || height <= 0)
	{
		float fWidth, fHeight;
		GetTextExtent(label, &fWidth, &fHeight, NULL, NULL, buttonFont);
		if (width <= 0)
		{
			width = fWidth + BUTTON_H_SPACE;
			if (width < MIN_BUTTON_WIDTH) width = MIN_BUTTON_WIDTH;
			cWindowWidth = width;
		}

		if (height <= 0)
		{
			height = fHeight + BUTTON_V_SPACE;
			cWindowHeight = height;
		}
	}

	cBorderArea = new wxArea(this);
	new wxButtonBorder(cBorderArea);

	SetCurrentMacDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	Rect boundsRect = {0, 0, ClientArea()->Height(), ClientArea()->Width()};
	wxMacString theMacTitle = label;
	const Bool drawNow = TRUE; // WCH: use FALSE, then show after ChangeColour??
	const short offValue = 0;
	const short minValue = 0;
	const short maxValue = 1;
	short refCon = 0;
	cMacControl = ::NewControl((WindowPtr)theMacGrafPort, &boundsRect, theMacTitle(),
			drawNow, offValue, minValue, maxValue, pushButProc + useWFont, refCon);
	CheckMemOK(cMacControl);
	
	if (GetParent()->IsHidden())
		DoShow(FALSE);
}

//-----------------------------------------------------------------------------
wxButton::wxButton // Constructor (given parentPanel, bitmap)
	(
		wxPanel*	parentPanel,
		wxFunction	function,
		wxBitmap*	bitmap,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbButton (parentPanel, x, y, width, height, style, windowName),
		cColorTable(NULL)
{
	if (bitmap->Ok() && (bitmap->selectedIntoDC >= 0)) {
	  buttonBitmap = bitmap;
	  buttonBitmap->selectedIntoDC++;
	} else {
	  Create(parentPanel, function, "<bad-image>", x, y, width, height, style, windowName, objectType);
	  return;
	}

	Callback(function);
	
	SetEraser(wxWHITE_BRUSH);

	cBorderArea = new wxArea(this);
	new wxButtonBorder(cBorderArea);

	cMacControl = NULL;
	
	SetCurrentMacDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	Rect bounds = {0, 0, buttonBitmap->GetHeight(), buttonBitmap->GetWidth()};
	bounds.bottom += 2 * IB_MARGIN_Y;
	bounds.right += 2 * IB_MARGIN_X;
	cWindowHeight = bounds.bottom;
	cWindowWidth = bounds.right;

	::InvalRect(&bounds);
	
	if (GetParent()->IsHidden())
		DoShow(FALSE);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxButton::~wxButton(void)
{
	if (buttonBitmap == NULL) {
		if (cMacControl) ::DisposeControl(cMacControl);
	} else
		--buttonBitmap->selectedIntoDC;
}

//-----------------------------------------------------------------------------
void wxButton::ChangeColour(void)
{
	if (buttonBitmap)
		return;
	if (cColorTable == NULL)
	{
		cColorTable = (CCTabHandle)NewHandle(40);
		CheckMemOK(cColorTable);
		(**cColorTable).ccSeed = 0;
		(**cColorTable).ccRider = 0;
		(**cColorTable).ctSize = 2;
		(**cColorTable).ctTable[0].value = cFrameColor;
		(**cColorTable).ctTable[1].value = cBodyColor;
		(**cColorTable).ctTable[2].value = cTextColor;
		(**cColorTable).ctTable[3].value = cThumbColor;
	}

	RGBColor whiteColor = {65535, 65535, 65535};
	RGBColor blackColor = {0, 0, 0};

	wxColour* backgroundColour = GetBackgroundColour();
	RGBColor backgroundRGB;
	backgroundRGB = backgroundColour ? backgroundColour->pixel : blackColor;
	(**cColorTable).ctTable[0].rgb = backgroundRGB;

	wxColour* buttonColour = GetButtonColour();
	RGBColor buttonRGB;
	buttonRGB = buttonColour ? buttonColour->pixel : whiteColor;
	(**cColorTable).ctTable[1].rgb = buttonRGB;


	wxColour* labelColour = GetLabelColour();
	RGBColor labelRGB;
	labelRGB = labelColour ? labelColour->pixel : blackColor;
	(**cColorTable).ctTable[2].rgb = labelRGB;

     if (cMacControl)
	  ::SetCtlColor(cMacControl, cColorTable);
}

//-----------------------------------------------------------------------------
char* wxButton::GetLabel(void)
{
	Str255	pTitle;
	char	cTitle[256];
	if (buttonBitmap)
		return NULL;
	if (cMacControl)
	  ::GetCTitle(cMacControl, pTitle);
	wxMacPtoCString(pTitle, wxBuffer);
    return wxBuffer;
}

//-----------------------------------------------------------------------------
void wxButton::SetLabel(char* label)
{
  if (buttonBitmap)
		return;
  if (label)
  {
	SetCurrentDC();
  	wxMacString1 theMacString1 = label;
  	if (cMacControl)
  	  ::SetCTitle(cMacControl, theMacString1());
  }
}

//-----------------------------------------------------------------------------
void wxButton::SetLabel(wxBitmap* bitmap)
{
  if (!buttonBitmap || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;
  --buttonBitmap->selectedIntoDC;
  buttonBitmap = bitmap;
  buttonBitmap->selectedIntoDC++;
  Refresh();
}

//-----------------------------------------------------------------------------
void wxButton::SetDefault(Bool flag) // WCH : modification of original (see below too)
{ // WCH: a panel method should be swapping default buttons
  // WCH: we would then have: void wxPanel::SetDefault(wxItem* item), NULL item allowed
	wxPanel* panel = (wxPanel*) GetParent();
	if (!panel) wxFatalError("No panel for wxButton::SetDefault.");
	wxButton* currentDefault = panel->defaultItem; // WCH: let any wxItem be a default item ?

	if (flag) // this becoming default item
	{
		if (currentDefault != this)
		{
			if (currentDefault) currentDefault->OnSetDefault(FALSE);
			panel->defaultItem = this;
			OnSetDefault(TRUE);
		}
	}
	else // this no longer default item
	{
		if (currentDefault == this)
		{
			currentDefault->OnSetDefault(FALSE);
			panel->defaultItem = NULL;
		}
	}
}

//-----------------------------------------------------------------------------
void wxButton::OnSetDefault(Bool flag) // WCH : addition to original
{ // WCH: the panel should invoke the default button to distinguish itself
	if (buttonBitmap)
		return;
	if (flag)
	{
		wxMargin margin(4);
		cBorderArea->SetMargin(margin, Direction::wxAll,
						cWindowWidth + 8, cWindowHeight + 8,
						cWindowX - 4, cWindowY - 4);
	}
	else
	{
		wxMargin margin(0);
		cBorderArea->SetMargin(margin, Direction::wxAll,
						cWindowWidth - 8, cWindowHeight - 8,
						cWindowX + 4, cWindowY + 4);
	}
}

//-----------------------------------------------------------------------------
void wxButton::Enable(Bool enable)
{
	wxWindow::Enable(enable);
}

//-----------------------------------------------------------------------------
void wxButton::Paint(void)
{
	if (cHidden) return;
	SetCurrentDC();
	Rect r = { 0, 0, cWindowHeight, cWindowWidth};
	::EraseRect(&r);
	if (buttonBitmap) {
		FrameRoundRect(&r, 2 * IB_MARGIN_X, 2 * IB_MARGIN_Y);
		buttonBitmap->DrawMac(IB_MARGIN_X, IB_MARGIN_Y);
	}
	else if (cMacControl) {
		Bool isVisible = (**cMacControl).contrlVis == 255;
		if (!isVisible) return;
		::Draw1Control(cMacControl);
	}
	wxWindow::Paint();
}

//-----------------------------------------------------------------------------
void wxButton::DoShow(Bool show)
{
	if (!CanShow(show)) return;

	if (!buttonBitmap && cMacControl) {
	  SetCurrentDC();

	  if (show)
		::ShowControl(cMacControl);
	  else
		::HideControl(cMacControl);
	}
	
	wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxButton::ShowAsActive(Bool flag) // mac platform only
{

}

//-----------------------------------------------------------------------------
void wxButton::Highlight(Bool flag) // mac platform only
{
	if (buttonBitmap) {
		SetCurrentDC();
		Rect bounds = {1, 1, cWindowHeight - 1, cWindowWidth - 1};
		::InvertRect(&bounds /* , 2 * IB_MARGIN_X, 2 * IB_MARGIN_Y */);
	} else if (cMacControl) {
		if (cEnable)
		{
			SetCurrentDC();
			::HiliteControl(cMacControl, flag ? inButton : 0);
		}
	}
}

//-----------------------------------------------------------------------------
void wxButton::OnEvent(wxMouseEvent& event) // mac platform only
{
	if (event.LeftDown())
	{
		SetCurrentDC();
	
		float fStartH, fStartV;
		event.Position(&fStartH, &fStartV); // client c.s.
		int startH = fStartH;
		int startV = fStartV;
	
		Point startPt = {startH, startV}; // client c.s.
		int trackResult;
		if (::StillDown()) {
			if (buttonBitmap == NULL && cMacControl)
				trackResult = ::TrackControl(cMacControl, startPt, NULL);
			else
				trackResult = Track(startPt);
		} else {
			Highlight(TRUE); // highlight button
			long delayTicks = 4; // one tick is 1/60th of a second
			long finalTicks;
			Delay(delayTicks, &finalTicks);
			Highlight(FALSE); // unhighlight button
		
			trackResult = 1;
		}
		if (trackResult)
		{
			wxCommandEvent *commandEvent = new wxCommandEvent(wxEVENT_TYPE_BUTTON_COMMAND);
			commandEvent->eventObject = this;
	  		ProcessCommand(*commandEvent);
		}
	}
}

//-----------------------------------------------------------------------------
void wxButton::Command(wxCommandEvent& event) // mac platform only (also xview platform)
{
	if (cEnable)
	{
		Highlight(TRUE); // highlight button
		long delayTicks = 10; // one tick is 1/60th of a second
		long finalTicks;
		Delay(delayTicks, &finalTicks);
		Highlight(FALSE); // unhighlight button
	  	ProcessCommand(event);
  	}
}

void wxButton::ChangeToGray(Bool gray)
{
  SetCurrentDC();
  if (cMacControl)
    ::HiliteControl(cMacControl, gray ? kInactiveControl : kActiveControl);
    
  wxWindow::ChangeToGray(gray);
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxButton::OnClientAreaDSize(int dW, int dH, int dX, int dY) // mac platform only
{
	SetCurrentDC();
	if (buttonBitmap || !cMacControl)
		return;

	Bool isVisible = (**cMacControl).contrlVis == 255;
	Bool hideToPreventFlicker = (isVisible && (dX || dY) && (dW || dH));
	if (hideToPreventFlicker) ::HideControl(cMacControl);

	if (dW || dH)
	{
		int clientWidth, clientHeight;
		GetClientSize(&clientWidth, &clientHeight);
		::SizeControl(cMacControl, clientWidth, clientHeight);
	}

	if (dX || dY)
	{
		cMacDC->setCurrentUser(NULL); // macDC no longer valid
		SetCurrentDC(); // put new origin at (0, 0)
		::MoveControl(cMacControl, 0, 0);
	}

	if (hideToPreventFlicker) ::ShowControl(cMacControl);

	if (!cHidden && (dW || dH || dX || dY))
	{
		int clientWidth, clientHeight;
		GetClientSize(&clientWidth, &clientHeight);
		Rect clientRect = {0, 0, clientHeight, clientWidth};
		::InvalRect(&clientRect);
	}
}
