///////////////////////////////////////////////////////////////////////////////
// File:	wx_check.cc
// Purpose:	Panel item checkbox implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wx_check.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_area.h"
#include "wx_panel.h"
#include <QuickDraw.h>

#define IC_BOX_SIZE 12
#define IC_X_SPACE 3
#define IC_Y_SPACE 2
#define IC_MIN_HEIGHT (IC_BOX_SIZE + 2 * IC_Y_SPACE)
	
//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxCheckBox::wxCheckBox // Constructor (given parentPanel, label)
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
		wxbCheckBox (parentPanel, x, y, width, height, style, windowName)
{
  Create(parentPanel, function, label, x, y, width, height, style, windowName, objectType);
}

void wxCheckBox::Create // Constructor (given parentPanel, label)
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
	
    label = wxItemStripLabel(label);

	if (width <= 0 || height <= 0)
	{
		float fLabelWidth = 20.0;
		float fLabelHeight = 12;
		if (label)
		{
			GetTextExtent(label, &fLabelWidth, &fLabelHeight, NULL, NULL, buttonFont);
			fLabelWidth += 20; // add 20 for width of checkbox icon
			if (fLabelHeight < 12) fLabelHeight = 12; // height of checkbox icon is 12
		}
		if (width <= 0) cWindowWidth = fLabelWidth;
		if (height <= 0) cWindowHeight = fLabelHeight;
	}

#if 0
	SetCurrentMacDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	Rect boundsRect = {0, 0, cWindowHeight, cWindowWidth};
	wxMacString theMacLabel = label;
	const Bool drawNow = TRUE; // WCH: use FALSE, then show after ChangeColour??
	const short offValue = 0;
	const short minValue = 0;
	const short maxValue = 1;
	short refCon = 0;
	cMacControl = ::NewControl((WindowPtr)theMacGrafPort, &boundsRect, theMacLabel(),
			drawNow, offValue, minValue, maxValue, checkBoxProc + useWFont, refCon);
	CheckMemOK(cMacControl);
#else
	labelString = label;
	cMacControl = NULL;
#endif
	
	if (GetParent()->IsHidden())
		DoShow(FALSE);
}

//-----------------------------------------------------------------------------
wxCheckBox::wxCheckBox // Constructor (given parentPanel, label)
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
		wxbCheckBox (parentPanel, x, y, width, height, style, windowName)
{
	if (bitmap->Ok() && (bitmap->selectedIntoDC >= 0)) {
		buttonBitmap = bitmap;
		buttonBitmap->selectedIntoDC++;
	} else {
		Create(parentPanel, function, "<bad-bitmap>", x, y, width, height, style, windowName, objectType);
		return;
	}
	
	Callback(function);
	
	SetCurrentMacDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	Rect bounds = {0, 0, buttonBitmap->GetHeight(), buttonBitmap->GetWidth()};
	cWindowHeight = bounds.bottom;
	cWindowWidth = bounds.right + IC_BOX_SIZE + IC_X_SPACE;
	if (cWindowHeight < IC_MIN_HEIGHT)
	  cWindowHeight = IC_MIN_HEIGHT;
	
	::InvalRect(&bounds);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxCheckBox::~wxCheckBox(void)
{
	if (cMacControl) ::DisposeControl(cMacControl);
	if (buttonBitmap) --buttonBitmap->selectedIntoDC;
}

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Item methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
char* wxCheckBox::GetLabel(void)
{
	Str255	pLabel;

	if (cMacControl) {
	  ::GetCTitle(cMacControl, pLabel);
	  wxMacPtoCString(pLabel, wxBuffer);
      return wxBuffer;
    } else
    	return "";
}

//-----------------------------------------------------------------------------
void wxCheckBox::SetLabel(char* label)
{
  if (buttonBitmap)
    return;

  if (label && cMacControl)
  {
	SetCurrentDC();
  	wxMacString1 theMacString1 = label;
  	::SetCTitle(cMacControl, theMacString1());
  }
}

//-----------------------------------------------------------------------------
void wxCheckBox::SetLabel(wxBitmap* bitmap)
{
  if (!buttonBitmap || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;
  --buttonBitmap->selectedIntoDC;
  buttonBitmap = bitmap;
  buttonBitmap->selectedIntoDC++;
  Refresh();
}

//-----------------------------------------------------------------------------
void wxCheckBox::SetValue(Bool value)
{
	SetCurrentDC();
	if (cMacControl) 
	  ::SetCtlValue(cMacControl, value ? 1 : 0);
	else {
	  bitmapState = !!value;
	  if (!cHidden)
	    Paint();
	}
}

//-----------------------------------------------------------------------------
Bool wxCheckBox::GetValue(void)
{
    if (cMacControl) {
	  short value = ::GetCtlValue(cMacControl);
	  return (value != 0) ? TRUE : FALSE;
	} else
		return bitmapState;
}


	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Sizing methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxCheckBox::OnClientAreaDSize(int dW, int dH, int dX, int dY) // mac platform only
{
	if (!cMacControl)
	  return;
	  
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

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxCheckBox::ChangeColour(void)
{
}

//-----------------------------------------------------------------------------
void wxCheckBox::ChangeToGray(Bool gray)
{
	SetCurrentDC();
	if (cMacControl)
	  ::HiliteControl(cMacControl, gray ? kInactiveControl : kActiveControl);

    wxWindow::ChangeToGray(gray);
}

//-----------------------------------------------------------------------------
void wxCheckBox::Paint(void)
{
	if (cHidden) return;
	SetCurrentDC();
	Rect r = { 0, 0, cWindowHeight, cWindowWidth};
	::EraseRect(&r);
	if (cMacControl)
	  ::Draw1Control(cMacControl);
	else {
	  if (buttonBitmap) {
		int btop = (cWindowHeight - buttonBitmap->GetHeight()) / 2;
		buttonBitmap->DrawMac(IC_BOX_SIZE + IC_X_SPACE, btop);
	  } else if (labelString) {
	    float fWidth = 50.0;
		float fHeight = 12.0;
		float fDescent = 0.0;
		float fLeading = 0.0;
		GetTextExtent(labelString, &fWidth, &fHeight, &fDescent, &fLeading, labelFont);
		int stop = (cWindowHeight + fHeight) / 2;
		::MoveTo(IC_BOX_SIZE + IC_X_SPACE, stop - fDescent - fLeading);
	  	::DrawText(labelString, 0, strlen(labelString));
	  }
	  int top = (cWindowHeight - IC_BOX_SIZE) / 2;
	  Rect r = { top, 0, top + IC_BOX_SIZE, IC_BOX_SIZE };
      ForeColor(blackColor);
	  PenSize(1, 1);
	  FrameRect(&r);
	  ForeColor(whiteColor);
  	  InsetRect(&r, 1, 1);
  	  PaintRect(&r);
      ForeColor(blackColor);
	  if (bitmapState) {
	    MoveTo(0, top);
	    Line(IC_BOX_SIZE - 1, IC_BOX_SIZE - 1);
	    MoveTo(0, top + IC_BOX_SIZE - 1);
	    Line(IC_BOX_SIZE - 1, -(IC_BOX_SIZE - 1));
	  } else {
	  }
	  cMacDC->setCurrentUser(NULL);
	}
}

void wxCheckBox::Highlight(Bool on)
{
	int top = (cWindowHeight - IC_BOX_SIZE) / 2;
	Rect r = { top + 1, 1, top + IC_BOX_SIZE - 1, IC_BOX_SIZE - 1};
	PenSize(1, 1);
	if (on)
	  FrameRect(&r);
	else {
	  ForeColor(whiteColor);
	  FrameRect(&r);
	  ForeColor(blackColor);
	  if (bitmapState) {
	    MoveTo(0, top);
	    Line(IC_BOX_SIZE - 1, IC_BOX_SIZE - 1);
	    MoveTo(0, top + IC_BOX_SIZE - 1);
	    Line(IC_BOX_SIZE - 1, -(IC_BOX_SIZE - 1));
	  }
	}
}

//-----------------------------------------------------------------------------
void wxCheckBox::DoShow(Bool show)
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
void wxCheckBox::ShowAsActive(Bool flag) // mac platform only
{
}

//-----------------------------------------------------------------------------
void wxCheckBox::OnEvent(wxMouseEvent& event) // mac platform only
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
			wxCommandEvent *commandEvent = new wxCommandEvent(wxEVENT_TYPE_CHECKBOX_COMMAND);
			commandEvent->commandInt = !GetValue();
			commandEvent->eventObject = this;
			SetValue(commandEvent->Checked()); // toggle checkbox
	  		ProcessCommand(*commandEvent);
		}
	}
}

//-----------------------------------------------------------------------------
void wxCheckBox::Command(wxCommandEvent& event) // mac platform only (also xview platform)
{
	if (cEnable)
	{
		Highlight(TRUE); // highlight button
		long delayTicks = 10; // one tick is 1/60th of a second
		long finalTicks;
		Delay(delayTicks, &finalTicks);
		Highlight(FALSE); // unhighlight button
		SetValue(event.Checked()); // set checkbox
	  	ProcessCommand(event);
  	}
}
