///////////////////////////////////////////////////////////////////////////////
// File:	wx_messg.cc
// Purpose:	Panel item message implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wx_item.h"
#include "wx_gdi.h"
#include "wx_messg.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_area.h"
#include "wxBorderArea.h"
#include "wxLabelArea.h"
#include <QuickDraw.h>
#include <TextEdit.h>

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxMessage::wxMessage // Constructor (given parentArea)
	(
		wxArea*		parentArea,
		char*		label,
		int 		x,
		int			y,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbMessage(parentArea, x, y, 0, 0, style, windowName)
{
	CreateWxMessage(label);
}

//-----------------------------------------------------------------------------
wxMessage::wxMessage // Constructor (given parentArea, font)
	(
		wxArea*		parentArea,
		char*		label,
		wxFont*		theFont,
		int 		x,
		int			y,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbMessage(parentArea, x, y, 0, 0, style, windowName)
{
	CreateWxMessage(label, theFont);
}

//-----------------------------------------------------------------------------
wxMessage::wxMessage // Constructor (given parentPanel)
	(
		wxPanel*	parentPanel,
		char*		label,
		int 		x,
		int			y,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbMessage(parentPanel, x, y, 0, 0, style, windowName)
{
	CreateWxMessage(label);
}

//-----------------------------------------------------------------------------
wxMessage::wxMessage // Constructor (given parentPanel, font)
	(
		wxPanel*	parentPanel,
		char*		label,
		wxFont*		theFont,
		int 		x,
		int			y,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbMessage(parentPanel, x, y, 0, 0, style, windowName)
{
	CreateWxMessage(label, theFont);
}

wxMessage::wxMessage // Constructor (given parentPanel and bitmap)
	(
		wxPanel*	parentPanel,
		wxBitmap*	bitmap,
		int 		x,
		int			y,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbMessage(parentPanel, x, y, 0, 0, style, windowName)
{
	SetEraser(wxCONTROL_BACKGROUND_BRUSH);
	if (bitmap->Ok() && (bitmap->selectedIntoDC >= 0)) {
		sBitmap = bitmap;
		sBitmap->selectedIntoDC++;
		cMessage = NULL;
		if (cStyle & wxBORDER) new wxBorderArea(this);
		SetClientSize(sBitmap->GetWidth(), sBitmap->GetHeight());
	} else
		CreateWxMessage("<bad-image>");
}


//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxMessage::~wxMessage(void)
{
	if (cMessage) delete [] cMessage;
	if (sBitmap) --sBitmap->selectedIntoDC;
	if (cParentArea->__type == wxTYPE_LABEL_AREA) {
		// CJC hack? clean up label area so it does point to us, since its 
		// about to go away.
		wxLabelArea *pa = (wxLabelArea *)cParentArea;
		pa->cLabelText = NULL;
	}
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxMessage::CreateWxMessage(char* label, wxFont* theFont) // common constructor initialization
{
	if (cStyle & wxBORDER) new wxBorderArea(this);
	sBitmap = NULL;
	cMessage = wxItemStripLabel(label);
	
	SetEraser(wxCONTROL_BACKGROUND_BRUSH);

	float clientWidth = 20;
	float clientHeight = 14;
	if (theFont) 
		font = theFont;
	if (!font) 
		font = wxNORMAL_FONT; // WCH: kludge
	if (font) {
		font->GetTextExtent(cMessage, &clientWidth, &clientHeight, NULL, NULL);
		if (font->GetStyle() != wxNORMAL || font->GetWeight() != wxNORMAL)
			clientWidth += 5; //cjc - try hello.cc italic labels are truncated
			//clientWidth += 0;	//tom and others
	}
	SetClientSize(clientWidth + 3, clientHeight); // mflatt: +3 is needed (even for plain)
	
	if (GetParent()->IsHidden())
		DoShow(FALSE);
}


//-----------------------------------------------------------------------------
void wxMessage::ChangeColour(void)
{
}

//-----------------------------------------------------------------------------
char* wxMessage::GetLabel(void)
{
	if (cMessage) {
	  strcpy(wxBuffer, cMessage);
	  return wxBuffer;
	} else
	  return "";
}

//-----------------------------------------------------------------------------
void wxMessage::SetLabel(wxBitmap *bitmap)
{
	if (!sBitmap || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
	  return;

    --sBitmap->selectedIntoDC;
	sBitmap = bitmap;
	sBitmap->selectedIntoDC++;
	
	// erase the old
	SetCurrentDC();
	int clientWidth, clientHeight;
	GetClientSize(&clientWidth, &clientHeight);
	Rect clientRect = {0, 0, clientHeight, clientWidth};
	::InvalRect(&clientRect);

//FIXME CJC	SetClientSize(sBitmap->GetWidth(), sBitmap->GetHeight());
	sBitmap->DrawMac();
}

//-----------------------------------------------------------------------------
void wxMessage::SetLabel(char* label)
{
	if (sBitmap) return;
    if (cMessage) delete [] cMessage;
	cMessage = macCopyString0(label);
	if (!cHidden) {
#if 0
		SetCurrentDC();
		int clientWidth, clientHeight;
		GetClientSize(&clientWidth, &clientHeight);
		Rect clientRect = {0, 0, clientHeight, clientWidth};
		::InvalRect(&clientRect);
#else
		Paint();
#endif
	}
}

//-----------------------------------------------------------------------------
void wxMessage::Paint(void)
{
	if (cHidden) return;

	SetCurrentDC();
	int clientWidth, clientHeight;
	GetClientSize(&clientWidth, &clientHeight);
	Rect clientRect = {0, 0, clientHeight, clientWidth};
	::EraseRect(&clientRect);

	if (sBitmap) {
		sBitmap->DrawMac();
	} else {
		::TextBox(cMessage, strlen(cMessage), &clientRect, teJustLeft);
	}
}

//-----------------------------------------------------------------------------
void wxMessage::DoShow(Bool show) 
{
	wxWindow::DoShow(show);
}

void wxMessage::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);
}

