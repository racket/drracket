/*
 * File:	wx_slidr.cc
 * Purpose:	Panel item slider implementation (Macintosh version)
 * Author:	Cecil Coupe
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
 */

static const char sccsid[] = "%W% %G%";



#include <iostream.h>
#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "wx_mgstr.h"
#include "wx_utils.h"
#include "wx_slidr.h"


#define MEANING_CHARACTER	'0'

#define  USE_ACTIONPROC 0	// set to 1 to try the thumbtracking stuff
							// unfinished (crashes system) as of 7/29/95

// Slider
/* 
	For wxMac a wxSlider contains
	1. A scroll control (horizontal)
	2. A wxLabelArea for the Label/Title
	3. a Rect for displaying the current value

*/

#define KDEFAULTW  60	// number pixels wide for a default scroll control
#define KSCROLLH   16	// height of a mac scrollbar control
#define VSP			2	// space between scrollbar and value
#define HSP			2	
// Because I never get this right and t,l,b,r makes sense to me - CJC
//
#define SetBounds(rect, top, left, bottom, right) ::SetRect(rect, left, top, right, bottom)

wxSlider::wxSlider(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x, int y,
           long style, char *name, WXTYPE objectType
	):
  wxbSlider(panel, label, value, min_value, max_value, width, x, y, style, name)
{
  Create(panel, func, label, value, min_value, max_value, width, x, y, style, name);
}

Bool wxSlider::Create(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x, int y,
           long style, char *name)
{
	windowStyle = style;
	window_parent = panel;
	labelPosition = panel->label_position;
	buttonFont = panel->buttonFont;
	labelFont = panel->labelFont;
	backColour = panel->backColour;
	labelColour = panel->labelColour;
	buttonColour = panel->buttonColour;
	
	label = wxItemStripLabel(label);

    Callback(func);
	SetCurrentDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
	
  	s_min = min_value;
  	s_max = max_value;
	// page_size calculated 
	page_size = (s_max - s_min) / 15; // 15 == size of thumb ?
	if (page_size < 2)
		page_size = 2;

	valueFont = wxNORMAL_FONT;
	float fWidth;
	float fHeight;
	float fDescent;
	float fLeading;
	int	lblh=0;
	int lblw=0;
	if (label) {
		GetTextExtent(label, &fWidth, &fHeight, &fDescent, &fLeading, labelFont);
		lblh = fHeight;
		lblw = fWidth;
	}
	GetTextExtent("258", &fWidth, &fHeight, &fDescent, &fLeading, valueFont);
	int vwid = (int)fWidth;
	int vhgt = (int)fHeight;

	Rect boundsRect = {0, 0, KSCROLLH, KDEFAULTW};
	
	int adjust = 0;
	if (style & wxVERTICAL) {
		if (width < 0)
			cWindowHeight = KDEFAULTW + ((labelPosition == wxVERTICAL) ? lblh : 0);
		else
			cWindowHeight = width;
		cWindowWidth = vwid + KSCROLLH + HSP + ((labelPosition == wxVERTICAL) ? 0 : lblw + HSP);
		
		boundsRect.right = KSCROLLH;
		boundsRect.bottom = cWindowHeight - ((labelPosition == wxVERTICAL) ? lblh : 0);
		
		valueRect.left = cWindowWidth - vwid + 1;
		valueRect.top = (cWindowHeight - vhgt) / 2;
		adjust = -1;
	} else {
		if (width < 0)
			cWindowWidth = KDEFAULTW + ((labelPosition == wxHORIZONTAL) ? lblw : 0);
		else
			cWindowWidth = width;
		cWindowHeight = vhgt + KSCROLLH + VSP + ((labelPosition == wxVERTICAL) ? lblh + VSP : 0);
		
		boundsRect.right = cWindowWidth - ((labelPosition == wxVERTICAL) ? 0 : lblw);
		
		valueRect.top = cWindowHeight - vhgt;
		valueRect.left = (cWindowWidth - vwid) / 2;
	}

	valueRect.bottom = valueRect.top + vhgt;
	valueRect.right = valueRect.left + vwid + adjust;
	
	valuebase = fDescent;
	
	cMacControl = ::NewControl((WindowPtr)theMacGrafPort, &boundsRect, NULL,
			TRUE, value, min_value, max_value, scrollBarProc, 0);
	CheckMemOK(cMacControl);

	::SetCRefCon(cMacControl, (long)this);
	if (label)
	{
		cTitle = new wxLabelArea(this, label, labelFont,
				labelPosition == wxVERTICAL ? Direction::wxTop : Direction::wxLeft);
	}
	else
		cTitle = NULL;

	if (GetParent()->IsHidden())
		DoShow(FALSE);
	
	return TRUE;
}

// ------------ Destructor ----------------------------------------
wxSlider::~wxSlider(void)
{
	 delete cTitle;	// Special care needed to delete Areas
	::DisposeControl(cMacControl);
}


//------------ Event Handling --------------------------------------
void wxSlider::Paint(void)
{
	if (cHidden) return;

	SetCurrentDC();
	::Draw1Control(cMacControl);

	SetFont(valueFont);
	SetTextInfo();
	
	::MoveTo(valueRect.left, valueRect.bottom - valuebase);
	::EraseRect(&valueRect);
	char t[8];
	sprintf(t,"%d",::GetControlValue(cMacControl));
	::DrawText(t,0,strlen(t));
	wxWindow::Paint();
}

//-----------------------------------------------------------------------------
void wxSlider::DoShow(Bool show)
{
	if (!CanShow(show)) return;

	SetCurrentDC();

	if (show)
		::ShowControl(cMacControl);
	else
		::HideControl(cMacControl);
		
	cTitle->DoShow(show);
		
	wxWindow::DoShow(show);
}

void wxSlider::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
	SetCurrentDC();
	if (dW || dH)
	{	
		int clientWidth = ClientArea()->Width();
		int clientHeight= ClientArea()->Height();
		Rect viewRect = (**cMacControl).contrlRect;

		int vwid = valueRect.right - valueRect.left;
		int vhgt = valueRect.bottom - valueRect.top;
			
		if (windowStyle & wxVERTICAL) {
			int w = viewRect.right - viewRect.left;
			// the wid can't change
			::SizeControl(cMacControl, w, clientHeight);
			valueRect.top = (clientHeight - vhgt) / 2;
			valueRect.bottom = valueRect.top + vhgt;
			valueRect.right = clientWidth;
			valueRect.left = valueRect.right - vwid + 1;
		} else {
			int h = viewRect.bottom - viewRect.top;
			// the hgt can't change
			::SizeControl(cMacControl, clientWidth, h);
			valueRect.left = (clientWidth - vwid) / 2;
			valueRect.right = valueRect.left + vwid;
			valueRect.bottom = clientHeight;
			valueRect.top = valueRect.bottom - vhgt;
		}
	}

	if (dX || dY)
	{	// Changing the position
		cMacDC->setCurrentUser(NULL); // macDC no longer valid
		SetCurrentDC(); // put newViewRect at (0, 0)
	}
}

// ---- everything above this line is needed for visual respresentation of the wxListBox
#if USE_ACTIONPROC
static pascal void SCTrackActionProc(ControlHandle theControl, short thePart);
static ControlActionUPP SCTrackActionProcUPP = NewControlActionProc(SCTrackActionProc);
#endif

void wxSlider::OnEvent(wxMouseEvent& event) // WCH: mac only ?
{
	if (event.leftDown) {
		float fStartH, fStartV;
		event.Position(&fStartH, &fStartV); // client c.s.
		int startH = fStartH;
		int startV = fStartV;
		Point pt = {startV, startH};
		SetCurrentDC();
		int part;
		int oldval = ::GetCtlValue(cMacControl);
#if USE_ACTIONPROC
		part = ::TestControl(cMacControl, pt);
		if (part && part == inThumb) {
			::TrackControl(cMacControl, pt, SCTrackActionProcUPP);
		}
		else 
#endif
		{
			part = ::TrackControl(cMacControl, pt, NULL);
			switch (part) {
			case 0:
				break;
			case inUpButton:
				::SetCtlValue(cMacControl, max(s_min, oldval-1));
				break;
			case inDownButton:
				::SetCtlValue(cMacControl, min(s_max, oldval+1));
				break;
			case inPageUp:
				::SetCtlValue(cMacControl, max(s_min, oldval-page_size));
				break;
			case inPageDown:
				::SetCtlValue(cMacControl, min(s_max, oldval+page_size));
				break;
			case inThumb:
				break;
			} // end switch
			// Draw the new value or should we Invalidate the Rect or don't bother ?
			::MoveTo(valueRect.left+HSP, valueRect.bottom - valuebase);
			::EraseRect(&valueRect);
			char t[8];
			sprintf(t,"%d",::GetControlValue(cMacControl));
			::DrawText(t,0,strlen(t));
			// Creat a wxEvent
			strcpy(wxBuffer,t);
			wxCommandEvent *commandEvent = new wxCommandEvent(wxEVENT_TYPE_SLIDER_COMMAND);
			commandEvent->commandString = wxBuffer;
			commandEvent->commandInt = ::GetControlValue(cMacControl);
			commandEvent->eventObject = this;
	  		ProcessCommand(*commandEvent);
		}
	}
}

#if USE_ACTIONPROC
// Update the Value rect as the thumb is dragged around
static pascal void SCTrackActionProc(ControlHandle theControl, short thePart)
{
	wxSlider*	slider;
	slider = (wxSlider*)::GetCRefCon(theControl);
#if 0
	::MoveTo(slider->valueRect.left+HSP, slider->valueRect.bottom - slider->valuebase);
	::EraseRect(&slider->valueRect);
	char t[8];
	sprintf(t,"%d",::GetControlValue(theControl));
	::DrawText(t,0,strlen(t));
#endif	
}
#endif

// --------------------- Client API ---------------------
int wxSlider::GetValue(void)
{
	return ::GetControlValue(cMacControl);
}

void wxSlider::SetValue(int value)
{
	::SetControlValue(cMacControl, value);
}


#if 0 // the following 3 methods are in wxWindows 16.x but should not
	  // be implemented in wxMac because they interfere with the Area
	  // calcs. This is a FIXME
void wxSlider::SetSize(int x, int y, int width, int height)
{
}
void wxSlider::GetSize (int *x, int *y)
{
}

void wxSlider::GetPosition(int *x, int *y)
{
}
#endif

void wxSlider::SetBackgroundColour(wxColour*col)
{
} 

void wxSlider::SetLabelColour(wxColour*col)
{
}

void wxSlider::SetButtonColour(wxColour*col) 
{
}

char* wxSlider::GetLabel(void)
{
	return (cTitle ? cTitle->GetLabel() : NULL);
}

void wxSlider::SetLabel(char *label)
{
	if (cTitle) cTitle->SetLabel(label);
	
}

void wxSlider::ChangeToGray(Bool gray)
{
  if (cTitle)
	((wxLabelArea *)cTitle)->GetMessage()->InternalGray(gray);
  SetCurrentDC();
  if (cMacControl)
  	::HiliteControl(cMacControl, gray ? kInactiveControl : kActiveControl);
  wxWindow::ChangeToGray(gray);
}
