///////////////////////////////////////////////////////////////////////////////
// File:	wx_sbar.cc
// Purpose:	Macintosh Scrollbar implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wx_sbar.h"
#include "wxScroll.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_area.h"
#include "wx_frame.h"
#include <Windows.h>

pascal void	TrackActionProc(ControlHandle theControl,short partCode);

//	Functions which are called from external scope, but in turn invoke
//	DocWindow methods. These really could be moved t

static ControlActionUPP
TrackActionProcUPP = NewControlActionProc(TrackActionProc);


//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxScrollBar::wxScrollBar // Constructor (given parentArea)
	(
		wxArea*		parentArea,
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
		wxWindow (windowName, parentArea, x, y, width, height, style)
{
	CreateWxScrollBar(function, label);
}

//-----------------------------------------------------------------------------
wxScrollBar::wxScrollBar // Constructor (given parentWindow)
	(
		wxWindow*	parentWindow,
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
		wxWindow (windowName, parentWindow, x, y, width, height, style)
{
	CreateWxScrollBar(function, label);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxScrollBar::~wxScrollBar(void)
{
	::DisposeControl(cMacControl);
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxScrollBar::CreateWxScrollBar // common constructor initialization
(
	wxFunction function,
	char* label
)
{
	InitDefaults(function);
	
	if (label)
	  label = wxItemStripLabel(label);

//////////////////////////////////////////
// do platform stuff
//////////////////////////////////////////
	SetCurrentMacDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	int clientWidth = ClientArea()->Width();
	int clientHeight = ClientArea()->Height();
	Rect boundsRect = {0, 0, clientHeight, clientWidth};
	wxMacString theMacLabel = label;
	const Bool drawNow = TRUE;
	const short offValue = 0;
	const short minValue = 0;
	const short maxValue = 0;
	long refCon = (long)this;
	cMacControl = ::NewControl((WindowPtr)theMacGrafPort, &boundsRect, theMacLabel(),
			drawNow, offValue, minValue, maxValue, scrollBarProc, refCon);
	CheckMemOK(cMacControl);
	
	if (GetParent()->IsHidden())
		DoShow(FALSE);
}

//-----------------------------------------------------------------------------
void wxScrollBar::InitDefaults(wxFunction function)
{
	cActive = TRUE;

	Callback(function);

	cStyle = (cStyle & wxHSCROLL ? wxHSCROLL : wxVSCROLL); // kludge
	wxScrollData* scrollData = new wxScrollData;
	cScroll = new wxScroll(this, scrollData);
}

//-----------------------------------------------------------------------------
void wxScrollBar::ChangeColour(void)
{
}

//-----------------------------------------------------------------------------
char* wxScrollBar::GetLabel()
{
	Str255	pLabel;

	::GetCTitle(cMacControl, pLabel);
	wxMacPtoCString(pLabel, wxBuffer);
    return wxBuffer;
}

//-----------------------------------------------------------------------------
void wxScrollBar::SetLabel(char* label)
{
  if (label)
  {
	SetCurrentDC();
  	wxMacString1 theMacString1 = label;
  	::SetCTitle(cMacControl, theMacString1());
  }
}

//-----------------------------------------------------------------------------
void wxScrollBar::SetValue(int val)
{
	SetCurrentDC();
	::SetCtlValue(cMacControl, val);
}

//-----------------------------------------------------------------------------
int wxScrollBar::GetValue(void)
{
	return ::GetCtlValue(cMacControl);
}

//-----------------------------------------------------------------------------
void wxScrollBar::SetMaxValue(int maxValue)
{
	SetCurrentDC();
	::SetCtlMax(cMacControl, maxValue);
}

//-----------------------------------------------------------------------------
int wxScrollBar::GetMaxValue(void)
{
	return ::GetCtlMax(cMacControl);
}

//-----------------------------------------------------------------------------
void wxScrollBar::Enable(Bool enable)
{
	if (cEnable != enable)
	{
		SetCurrentDC();
		const int kActiveControl = 0;
		const int kInactiveControl = 255;
		::HiliteControl(cMacControl, enable ? kActiveControl : kInactiveControl);
		cEnable = enable;
	}
}

//-----------------------------------------------------------------------------
void wxScrollBar::Paint(void)
{
	if (cHidden) return;

	SetCurrentDC();
 	// GRW
 	Bool isVisible = (**cMacControl).contrlVis == 255;
 	if (isVisible)
 	{
	 	::Draw1Control(cMacControl);
 	}
 	else
 	{
 		// Draw outline of hidden scrollbar (since we're clipping DrawGrowIcon)
 		Rect controlRect = (**cMacControl).contrlRect;
 		PenState oldPenState;
 		::GetPenState(&oldPenState);
 		::PenNormal();
 		::FrameRect(&controlRect);
 		::SetPenState(&oldPenState);
 	}
 	// GRW
}

//-----------------------------------------------------------------------------
void wxScrollBar::DoShow(Bool show)
{
	if (!CanShow(show)) return;

	SetCurrentDC();
	if (show)
		::ShowControl(cMacControl);
	else
		::HideControl(cMacControl);
	wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxScrollBar::ShowAsActive(Bool flag) // mac platform only
{
	if (cHidden) return;

	if (cEnable)
	{
		SetCurrentDC();
		const int kActiveControl = 0;
		const int kInactiveControl = 255;
 		//GRW
 		if (flag)
 			::ShowControl(cMacControl);
 		else
 		{
 			// Would look better than calling HideControl, if it worked:
 			//		(**cMacControl).contrlVis   0;
 			//		Rect controlRect   (**cMacControl).contrlRect;
 			//		::InvalRect(&controlRect);
 			::HideControl(cMacControl);
 		}
	   // OLD: ::HiliteControl(cMacControl, flag ? kActiveControl : kInactiveControl);
	
		// The following is a kludge, to prevent erasure during subsequent update event
		if (flag)
		{
			Bool isVisible = (**cMacControl).contrlVis == 255;
			if (isVisible)
			{
				::Draw1Control(cMacControl);
				Rect controlRect = (**cMacControl).contrlRect;
				::ValidRect(&controlRect);
			}
		}
	}
}

//-----------------------------------------------------------------------------
	pascal void TrackActionProc(ControlHandle theControl, short thePart);
	pascal void TrackActionProc(ControlHandle theControl, short thePart)
	{
		wxScrollBar* scrollBar = (wxScrollBar*) (**theControl).contrlRfCon;
		if (scrollBar) scrollBar->TrackAction(thePart);
	}

//-----------------------------------------------------------------------------
void wxScrollBar::OnEvent(wxMouseEvent& event) // mac platform only
{
	if (event.LeftDown())
	{
		SetCurrentDC();
	
		float fStartH, fStartV;
		event.Position(&fStartH, &fStartV); // client c.s.
		int startH = fStartH;
		int startV = fStartV;

		Point startPt = {startV, startH}; // client c.s.
		int thePart = ::TestControl(cMacControl, startPt);
		if (thePart)
		{
			if (thePart == inThumb)
			{
				if (::TrackControl(cMacControl, startPt, NULL))
				{
					Bool horizontal = cStyle & wxHSCROLL;
					wxWhatScrollData positionScrollData =
						(horizontal ? wxWhatScrollData::wxPositionH : wxWhatScrollData::wxPositionV);
					int newPosition = GetValue();
					cScroll->SetScrollData(newPosition, positionScrollData, NULL);
				}
			}
			else
			{
				::TrackControl(cMacControl, startPt, TrackActionProcUPP);
			}
		}
	}
}

//-----------------------------------------------------------------------------
void wxScrollBar::TrackAction(short part) // mac platform only
{
	if (part && cScroll)
	{
		Bool horizontal = cStyle & wxHSCROLL;

		wxScrollData* scrollData = cScroll->GetScrollData();
		int scrollsPerPage = scrollData->GetValue
			(horizontal ? wxWhatScrollData::wxPageW : wxWhatScrollData::wxPageH);
		int maxv = GetMaxValue();
	
		int delta = 0;
		switch (part)
		{
			case inUpButton: delta = -1; break;
			case inDownButton: delta = 1; break;
			case inPageUp: delta = -scrollsPerPage; break;
			case inPageDown: delta = scrollsPerPage; break;
		}

		int newPosition = GetValue() + delta;
		if (newPosition < 0) newPosition = 0;
		if (newPosition > maxv) newPosition = maxv;

		wxWhatScrollData positionScrollData =
			(horizontal ? wxWhatScrollData::wxPositionH : wxWhatScrollData::wxPositionV);
		cScroll->SetScrollData(newPosition, positionScrollData, NULL);

		SetCurrentDC(); // must reset cMacDC (kludge)
	}
}

//-----------------------------------------------------------------------------
void wxScrollBar::SetScrollData // adjust scrollBar to match scroll data setting
(
	wxScrollData*		scrollData,
	wxWhatScrollData	whatScrollData,
	wxWindow*			iniatorWindow
)
{
	if (this == iniatorWindow) return;

	Bool horizontal = cStyle & wxHSCROLL;

	wxWhatScrollData sizeScrollData =
		(horizontal ? wxWhatScrollData::wxSizeW : wxWhatScrollData::wxSizeH);
	if ((long)whatScrollData & (long)sizeScrollData)
	{
		int newSize = scrollData->GetValue(sizeScrollData);
		SetMaxValue(newSize);
	}

	wxWhatScrollData postionScrollData =
		(horizontal ? wxWhatScrollData::wxPositionH : wxWhatScrollData::wxPositionV);
	if ((long)whatScrollData & (long)postionScrollData)
	{
		int newPosition = scrollData->GetValue(postionScrollData);
		SetValue(newPosition);
	}
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxScrollBar::OnClientAreaDSize(int dW, int dH, int dX, int dY) // mac platform only
{
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

	if (cHidden && (dW || dH || dX || dY))
	{
		int clientWidth, clientHeight;
		GetClientSize(&clientWidth, &clientHeight);
		Rect clientRect = {0, 0, clientHeight, clientWidth};
		::InvalRect(&clientRect);
	}
}
