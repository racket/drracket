///////////////////////////////////////////////////////////////////////////////
// File:	wx_rbox.cc
// Purpose:	Panel item radioBox implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wx_rbox.h"
#include "wx_rbut.h"
#include "wx_messg.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_gdi.h"
#include "wx_area.h"
#include "wxBorderArea.h"

#define MEANING_CHARACTER	'0'

//-----------------------------------------------------------------------------
	void wxRadioButtonProc(wxRadioButton& radioButton, wxCommandEvent& event);
	void wxRadioButtonProc(wxRadioButton& radioButton, wxCommandEvent& event)
	{
		wxPanel* radioPanel = (wxPanel*)radioButton.GetParent();
		wxWindow *rb = radioPanel;
		while (wxSubType(rb->__type, wxTYPE_PANEL))
			rb = rb->GetParent();
		wxRadioBox* radioBox = (wxRadioBox *)rb;
		long radioButtonIndex = radioBox->cRadioButtons.MemberIndex(&radioButton);
		radioBox->SetSelection(radioButtonIndex);

		wxCommandEvent *commandEvent = new wxCommandEvent(wxEVENT_TYPE_RADIOBOX_COMMAND);
		commandEvent->commandInt = radioButtonIndex;
		commandEvent->eventObject = radioBox;
		radioBox->ProcessCommand(*commandEvent);
	}

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxRadioBox::wxRadioBox // Constructor (given parentPanel, label choices)
	(
		wxPanel*	parentPanel,
		wxFunction	function,
		char*		Title,
		int 		x,
		int			y,
		int			width,
		int			height,
		int			N,
		char**		Choices,
		int			majorDim,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbRadioBox (parentPanel, x, y, width, height, N, style, windowName),
		cRadioButtons (wxList(wxList::kNoDestroyData))
{
	Callback(function);

	cRadioPanel = new wxPanel(this->ClientArea(), 0, 0, 0, 0, 0);
	cRadioPanel->SetButtonFont(wxNORMAL_FONT);
	cRadioPanel->SetLabelFont(wxNORMAL_FONT);

	wxPanel *buttonHolder = cRadioPanel;
	

	if (Title)
	{
		Title = wxItemStripLabel(Title);
		cRadioTitle = new wxMessage(cRadioPanel, Title, labelFont);
		if (labelPosition != wxVERTICAL) {
			buttonHolder = new wxPanel(cRadioPanel->ClientArea(), -1, -1, 0, 0, 0);
		} else
			buttonHolder->NewLine();
	}
	else cRadioTitle = NULL;

	for (int i = 0; i < N; i++)
	{
	    char *choice = wxItemStripLabel(Choices[i]);
		if (i && ((style & wxVERTICAL) == wxVERTICAL))
			buttonHolder->NewLine();
		wxRadioButton* radioButton = new wxRadioButton(buttonHolder,
				(wxFunction)&wxRadioButtonProc, choice);
		cRadioButtons.Append(radioButton);
	}
	SetSelection(0);

	buttonHolder->Fit();
	if (buttonHolder != cRadioPanel) {
		cRadioPanel->Fit();
		cRadioTitle->Centre(wxVERTICAL);
	}

	if (style & wxBORDER) new wxBorderArea(this);

	if (width < 0 || height < 0)
	{
		Fit(); // WCH: need wxHorizontal and wxVertical for Fit(direction)
	}
	
	if (GetParent()->IsHidden())
		DoShow(FALSE);
}

//-----------------------------------------------------------------------------
wxRadioBox::wxRadioBox // Constructor (given parentPanel, bitmap choices)
	(
		wxPanel*	parentPanel,
		wxFunction	function,
		char*		Title,
		int 		x,
		int			y,
		int			width,
		int			height,
		int			N,
		wxBitmap**	Choices,
		int			majorDim,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbRadioBox (parentPanel, x, y, width, height, N, style, windowName),
		cRadioButtons (wxList(wxList::kNoDestroyData))
{
	Callback(function);

	cRadioPanel = new wxPanel(this->ClientArea(), 0, 0, 0, 0, 0);
	cRadioPanel->SetButtonFont(wxNORMAL_FONT);
	cRadioPanel->SetLabelFont(wxNORMAL_FONT);
	
	wxPanel *buttonHolder = cRadioPanel;
	
	if (Title)
	{
		cRadioTitle = new wxMessage(cRadioPanel, Title, labelFont);
		if (labelPosition != wxVERTICAL) {
			buttonHolder = new wxPanel(cRadioPanel->ClientArea(), -1, -1, 0, 0, 0);
		} else
			buttonHolder->NewLine();
	}
	else cRadioTitle = NULL;

	for (int i = 0; i < N; i++)
	{
		if (i && ((style & wxVERTICAL) == wxVERTICAL))
			buttonHolder->NewLine();
		wxRadioButton* radioButton = new wxRadioButton(buttonHolder,
				(wxFunction)&wxRadioButtonProc, Choices[i]);
		cRadioButtons.Append(radioButton);
	}
	SetSelection(0);

	buttonHolder->Fit();
	if (buttonHolder != cRadioPanel) {
		cRadioPanel->Fit();
		cRadioTitle->Centre(wxVERTICAL);
	}

	if (style & wxBORDER) new wxBorderArea(this);

	if (width < 0 || height < 0)
	{
		Fit(); // WCH: need wxHorizontal and wxVertical for Fit(direction)
	}
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxRadioBox::~wxRadioBox(void)
{
	//CJC - This shouldn't be neccessary - compiler bug?
	cRadioButtons.Clear();
}


//-----------------------------------------------------------------------------
void wxRadioBox::ChangeColour(void)
{
}

//-----------------------------------------------------------------------------
char* wxRadioBox::GetLabel(void)
{
	return (cRadioTitle ? cRadioTitle->GetLabel() : NULL);
}

//-----------------------------------------------------------------------------
void wxRadioBox::SetLabel(char* label)
{
	if (cRadioTitle) cRadioTitle->SetLabel(label);
}

//-----------------------------------------------------------------------------
char* wxRadioBox::GetLabel(int item)
{
	char* result = NULL;
	int numberItems = cRadioButtons.Number();
	if (0 <= item && item < numberItems)
	{
		wxNode* node = cRadioButtons.Nth(item);
		wxRadioButton* radioButton = (wxRadioButton*)node->Data();
		result = radioButton->GetLabel();
	}
	return result;
}

//-----------------------------------------------------------------------------
void wxRadioBox::SetLabel(int item, char* label)
{
	int numberItems = cRadioButtons.Number();
	if (0 <= item && item < numberItems)
	{
		wxNode* node = cRadioButtons.Nth(item);
		wxRadioButton* radioButton = (wxRadioButton*)node->Data();
		radioButton->SetLabel(label);
	}
}

//-----------------------------------------------------------------------------
void wxRadioBox::SetLabel(int item, wxBitmap* bitmap)
{
	int numberItems = cRadioButtons.Number();
	if (0 <= item && item < numberItems)
	{
		wxNode* node = cRadioButtons.Nth(item);
		wxRadioButton* radioButton = (wxRadioButton*)node->Data();
		radioButton->SetLabel(bitmap);
	}
}

//-----------------------------------------------------------------------------
int wxRadioBox::FindString(char* s)
{
	int result = -1;
	int numberItems = cRadioButtons.Number();
	for (int i = 0; i < numberItems && result == -1; i++)
	{
		char* radioButtonLabel = GetLabel(i);
		if (strcmp(s, radioButtonLabel) == 0) result = i;
//		delete [] radioButtonLabel;
	}
	return result;
}

//-----------------------------------------------------------------------------
void wxRadioBox::SetSelection(int N) // WCH: should use "item" for "N"
{
	int numberItems = cRadioButtons.Number();
	if (0 <= N && N < numberItems)
	{
		if (selected != N)
		{
			if (0 <= selected && selected < numberItems)
			{
				wxNode* selectedNode = cRadioButtons.Nth(selected);
				wxRadioButton* selectedRadioButton =
								(wxRadioButton*)selectedNode->Data();
				selectedRadioButton->SetValue(FALSE);
			}
		
			wxNode* node = cRadioButtons.Nth(N);
			wxRadioButton* radioButton = (wxRadioButton*)node->Data();
			radioButton->SetValue(TRUE);
		
			selected = N;
		}
	}
}

//-----------------------------------------------------------------------------
// Get selection
//-----------------------------------------------------------------------------
int wxRadioBox::GetSelection(void)
{
	return selected;
}

//-----------------------------------------------------------------------------
// Find string for position
//-----------------------------------------------------------------------------
char* wxRadioBox::GetString(int N) // WCH: duplicates GetLabel; so delete this
{
	return GetLabel(N);
}

//-----------------------------------------------------------------------------
void wxRadioBox::Enable(Bool enable)
{
	wxWindow::Enable(enable);
}

//-----------------------------------------------------------------------------
void wxRadioBox::Enable(int item, Bool enable)
{
	int numberItems = cRadioButtons.Number();
	if (0 <= item && item < numberItems)
	{
		wxNode* node = cRadioButtons.Nth(item);
		wxRadioButton* radioButton = (wxRadioButton*)node->Data();
		radioButton->Enable(enable);
	}
}

//-----------------------------------------------------------------------------
void wxRadioBox::DoShow(Bool show)
{
	if (!CanShow(show)) return;

	cRadioPanel->DoShow(show);	
	wxWindow::DoShow(show);
#if 0
	int numberItems = cRadioButtons.Number();
	for (int i = 0; i < numberItems; i++)
	{
		Show(i, show);
	}
#endif
}

//-----------------------------------------------------------------------------
void wxRadioBox::Show(int item, Bool show)
{
	int numberItems = cRadioButtons.Number();
	if (0 <= item && item < numberItems)
	{
		wxNode* node = cRadioButtons.Nth(item);
		wxRadioButton* radioButton = (wxRadioButton*)node->Data();
		radioButton->Show(show);
	}
}

//-----------------------------------------------------------------------------
void wxRadioBox::Command(wxCommandEvent& event) // mac platform only
{
	int selection = event.commandInt;
	int numberItems = cRadioButtons.Number();
	if (0 <= selection && selection < numberItems)
	{
		wxNode* node = cRadioButtons.Nth(selection);
		wxRadioButton* radioButton = (wxRadioButton*)node->Data();
		if (radioButton->IsEnable())
		{
			radioButton->Highlight(TRUE); // highlight button
			long delayTicks = 10; // one tick is 1/60th of a second
			long finalTicks;
			Delay(delayTicks, &finalTicks);
			radioButton->Highlight(FALSE); // unhighlight button
			SetSelection(selection); // set radioButton
		  	ProcessCommand(event);
	  	}
  	}
}

void wxRadioBox::ChangeToGray(Bool gray)
{
  ChildrenInternalGray(gray);
  wxWindow::ChangeToGray(gray);
}
