///////////////////////////////////////////////////////////////////////////////
// File:	wx_rbut.cc
// Purpose:	Panel item radioButton implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wx_rbut.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_panel.h"
#include "wx_area.h"
#include <QuickDraw.h>

#define IR_CIRCLE_SIZE 12
#define IR_X_SPACE 3
#define IR_Y_SPACE 2
#define IR_MIN_HEIGHT (IR_CIRCLE_SIZE + 2 * IR_Y_SPACE)
#define IR_ON_INSET 3

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxRadioButton::wxRadioButton // Constructor (given parentPanel, label)
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
		wxItem (parentPanel, x, y, width, height, style, windowName)
{
  Create(parentPanel, function, label, x, y, width, height, style, windowName, objectType);
}

void wxRadioButton::Create // Real constructor (given parentPanel, label)
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
	Callback(function);

	font = buttonFont; // WCH: mac platform only

	float fLabelWidth = 100.0;
	float fLabelHeight = 20.0;
	if (label)
	{
		GetTextExtent(label, &fLabelWidth, &fLabelHeight, NULL, NULL, buttonFont);
		fLabelWidth += 20; // add 20 for width of radio button icon
		if (fLabelHeight < 12) fLabelHeight = 12; // height of radio button icon is 12
	}

	if (width < 0) cWindowWidth = fLabelWidth;
	if (height < 0) cWindowHeight = fLabelHeight;

#if 0
	SetCurrentMacDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	int clientWidth = ClientArea()->Width();
	int clientHeight = ClientArea()->Height();
	Rect boundsRect = {0, 0, clientHeight, clientWidth};
	wxMacString theMacLabel = label;
	const Bool drawNow = TRUE; // WCH: use FALSE, then show after ChangeColour??
	const short offValue = 0;
	const short minValue = 0;
	const short maxValue = 1;
	short refCon = 0;
	cMacControl = ::NewControl((WindowPtr)theMacGrafPort, &boundsRect, theMacLabel(),
			drawNow, offValue, minValue, maxValue, radioButProc + useWFont, refCon);
	CheckMemOK(cMacControl);
#else
	labelString = label;
	cMacControl = NULL;
#endif
}

//-----------------------------------------------------------------------------
wxRadioButton::wxRadioButton // Constructor (given parentPanel, bitmap)
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
		wxItem (parentPanel, x, y, width, height, style, windowName)
{
	if (bitmap->Ok() && (bitmap->selectedIntoDC >= 0)) {
		buttonBitmap = bitmap;
		buttonBitmap->selectedIntoDC++;
	} else {
	    Create(parentPanel, function, "<bad-image>", x, y, width, height, style, windowName, objectType);
	}

	Callback(function);
	cMacControl = NULL;
	
	SetCurrentMacDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	Rect bounds = {0, 0, buttonBitmap->GetHeight(), buttonBitmap->GetWidth()};
	cWindowHeight = bounds.bottom;
	cWindowWidth = bounds.right + IR_CIRCLE_SIZE + IR_X_SPACE;
	if (cWindowHeight < IR_MIN_HEIGHT)
	  cWindowHeight = IR_MIN_HEIGHT;

	::InvalRect(&bounds);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxRadioButton::~wxRadioButton(void)
{
	if (cMacControl) ::DisposeControl(cMacControl);
	if (buttonBitmap)
	  --buttonBitmap->selectedIntoDC;
}


//-----------------------------------------------------------------------------
void wxRadioButton::ChangeColour(void)
{
}

//-----------------------------------------------------------------------------
char* wxRadioButton::GetLabel()
{
	if (cMacControl) {
		Str255	pLabel;

		::GetCTitle(cMacControl, pLabel);
		wxMacPtoCString(pLabel, wxBuffer);
	    return wxBuffer;
	 } else
	 	return "";
}

//-----------------------------------------------------------------------------
void wxRadioButton::SetLabel(char* label)
{
  if (label && !buttonBitmap)
  {
	SetCurrentDC();
  	wxMacString1 theMacString1 = label;
  	::SetCTitle(cMacControl, theMacString1());
  }
}

//-----------------------------------------------------------------------------
void wxRadioButton::SetLabel(wxBitmap* bitmap)
{
	if (buttonBitmap && bitmap->Ok() && (bitmap->selectedIntoDC >= 0)) {
		--buttonBitmap->selectedIntoDC;
		buttonBitmap = bitmap;
		buttonBitmap->selectedIntoDC++;
		Refresh();
	}
}

//-----------------------------------------------------------------------------
void wxRadioButton::SetValue(Bool val)
{
	SetCurrentDC();
	if (cMacControl)
	  ::SetCtlValue(cMacControl, val ? 1 : 0);
	else {
	  bitmapState = !!val;
	  if (!cHidden)
	    Paint();
	}
}

//-----------------------------------------------------------------------------
Bool wxRadioButton::GetValue(void)
{
	if (cMacControl) {
		short value = ::GetCtlValue(cMacControl);
		return (value != 0) ? TRUE : FALSE;
	} else
		return bitmapState;
}

//-----------------------------------------------------------------------------
void wxRadioButton::ChangeToGray(Bool gray)
{
	SetCurrentDC();
	if (cMacControl)
		::HiliteControl(cMacControl, gray ?  kInactiveControl : kActiveControl);
    wxWindow::ChangeToGray(gray);
}

//-----------------------------------------------------------------------------
void wxRadioButton::Paint(void)
{
	if (cHidden) return;
	SetCurrentDC();
	Rect r = { 0, 0, cWindowHeight, cWindowWidth};
	::EraseRect(&r);
	if (cMacControl) {
		::Draw1Control(cMacControl);
	} else {
	  if (buttonBitmap) {
	  	int btop = (cWindowHeight - buttonBitmap->GetHeight()) / 2;
	  	buttonBitmap->DrawMac(IR_CIRCLE_SIZE + IR_X_SPACE, btop);
	  } else if (labelString) {
	    float fWidth = 50.0;
		float fHeight = 12.0;
		float fDescent = 0.0;
		float fLeading = 0.0;
		GetTextExtent(labelString, &fWidth, &fHeight, &fDescent, &fLeading, labelFont);
		int stop = (cWindowHeight + fHeight) / 2;
		::MoveTo(IR_CIRCLE_SIZE + IR_X_SPACE, stop - fDescent - fLeading);
	  	::DrawText(labelString, 0, strlen(labelString));
	  }
	  int top = (cWindowHeight - IR_CIRCLE_SIZE) / 2;
	  Rect r = { top, 0, top + IR_CIRCLE_SIZE, IR_CIRCLE_SIZE };
	  PenSize(1, 1);
	  ForeColor(blackColor);
	  FrameOval(&r);
	  InsetRect(&r, 1, 1);
	  ForeColor(whiteColor);
	  PaintOval(&r);
	  ForeColor(blackColor);
	  if (bitmapState) {
	    InsetRect(&r, IR_ON_INSET - 1, IR_ON_INSET - 1);
		PaintOval(&r);	    
	  }
	  cMacDC->setCurrentUser(NULL);
	}
}

void wxRadioButton::Highlight(Bool on)
{
	int top = (cWindowHeight - IR_CIRCLE_SIZE) / 2;
	Rect r = { top + 1, 1, top + IR_CIRCLE_SIZE - 1, IR_CIRCLE_SIZE - 1};
	if (!on)
		ForeColor(whiteColor);
	PenSize(1, 1);
	FrameOval(&r);
	if (!on) ForeColor(blackColor);
}

//-----------------------------------------------------------------------------
void wxRadioButton::DoShow(Bool show)
{
	if (!CanShow(show)) return;

	SetCurrentDC();
	if (cMacControl) {
		if (show)
			::ShowControl(cMacControl);
		else
			::HideControl(cMacControl);
	}

	wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxRadioButton::ShowAsActive(Bool flag) // mac platform only
{
#if 0
	if (cEnable)
	{
		SetCurrentDC();
		::HiliteControl(cMacControl, flag ? inCheckBox : 0);
	}
#endif
}

//-----------------------------------------------------------------------------
void wxRadioButton::OnEvent(wxMouseEvent& event) // mac platform only
{
	if (cEnable)
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
				if (cMacControl)
				  trackResult = ::TrackControl(cMacControl, startPt, NULL);
				else
				  trackResult = Track(startPt);
			} else
				trackResult = 1;
			if (trackResult)
			{
				wxCommandEvent *commandEvent = new wxCommandEvent(wxEVENT_TYPE_RADIOBOX_COMMAND); // WCH: must change constant
				commandEvent->commandInt = 0;
				commandEvent->eventObject = this;
				ProcessCommand(*commandEvent);
			}
		}
	}
}

//-----------------------------------------------------------------------------
void wxRadioButton::Command(wxCommandEvent& event) // mac platform only (also xview platform)
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

//-----------------------------------------------------------------------------
void wxRadioButton::ProcessCommand(wxCommandEvent& event)
{
  if (wxNotifyEvent(event, TRUE))
    return;

  wxFunction fun = callback;
  if (fun)
  {
    (void)(*(fun))(*this, event);
  }

  wxNotifyEvent(event, FALSE);
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxRadioButton::OnClientAreaDSize(int dW, int dH, int dX, int dY) // mac platform only
{
	if (!cMacControl) return;

	SetCurrentDC();

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
