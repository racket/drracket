///////////////////////////////////////////////////////////////////////////////
// File:	wxLabelArea.cc
// Purpose:	Label area (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wxLabelArea.h"
#include "wx_messg.h"
#include "wx_gdi.h"

//=============================================================================
// Construction methods
//=============================================================================

//-----------------------------------------------------------------------------
wxLabelArea::wxLabelArea
	(
		wxWindow*	parentWindow,
		char*		label,
		wxFont*		theFont,
		Direction	direction,
		int			xoffset,
		int			yoffset
	) :
		wxArea(parentWindow)
{
	__type = wxTYPE_LABEL_AREA;
	cLabelText = new wxMessage(this, label, theFont, xoffset, yoffset);
	cDirection = direction;

	int labelWidth = 0;
	int labelHeight = 0;
	wxFont* labelFont = cLabelText->font;
	if (labelFont)
	{
		char* labelText = cLabelText->GetLabel();
		float fLabelWidth, fLabelHeight, fDescent, fExternalLeading;
		labelFont->GetTextExtent(labelText, &fLabelWidth, &fLabelHeight,
								&fDescent, &fExternalLeading);
		labelWidth = fLabelWidth;
		labelHeight = fLabelHeight;
	}

	if (cDirection & Direction::wxTop)
	{
		SetMargin(labelHeight, Direction::wxTop);
	}
	else
	{
		SetMargin(labelWidth+5, Direction::wxLeft);
	}
}

//-----------------------------------------------------------------------------
wxLabelArea::~wxLabelArea(void)	// destructor
{
	delete cLabelText;
}

//=============================================================================
// Getter and setter methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxLabelArea::SetLabel(char* label) { cLabelText->SetLabel(label); }

//-----------------------------------------------------------------------------
char* wxLabelArea::GetLabel(void) { return cLabelText->GetLabel(); }

void wxLabelArea::DoShow(Bool on)
{
  cLabelText->DoShow(on);
}

