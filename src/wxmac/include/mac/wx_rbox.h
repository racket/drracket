///////////////////////////////////////////////////////////////////////////////
// File:	wx_rbox.h
// Purpose:	Declares radio box item (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_rboxh
#define wx_rboxh

#include "wb_rbox.h"

#ifdef IN_CPROTO
typedef       void* wxRadioBox;
#else

class wxBitmap;
class wxPanel;
class wxMessage;
class wxList;

class wxRadioBox: public wxbRadioBox
{
 public:
	wxPanel*	cRadioPanel;
	wxMessage*	cRadioTitle;
	wxList		cRadioButtons; // list of wxRadioButton items

//=============================================================================
// Public constructors
//=============================================================================
public:

	wxRadioBox // Constructor (given parentPanel, label choices)
	(
		wxPanel*	parentPanel,
		wxFunction	function,
		char*		Title,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		int			N = 0,
		char**		Choices = NULL,
		int			majorDim = 0,
		long		style = 0,
		char*		windowName = "radioBox",
		WXTYPE		objectType = wxTYPE_RADIO_BOX
	);

	wxRadioBox // Constructor (given parentPanel, bitmap choices)
	(
		wxPanel*	parentPanel,
		wxFunction	function,
		char*		Title,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		int			N = 0,
		wxBitmap**	Choices = NULL,
		int			majorDim = 0,
		long		style = 0,
		char*		windowName = "radioBox",
		WXTYPE		objectType = wxTYPE_RADIO_BOX
	);

//=============================================================================
// Public destructor
//=============================================================================
public:

  ~wxRadioBox(void);

//=============================================================================
// Public methods
//=============================================================================
public:

  int FindString(char* s);
  void SetSelection(int N);
  int GetSelection(void);
  char* GetString(int N);
  void Enable(Bool enable);
  void Enable(int item, Bool enable);
  void DoShow(Bool show);
  void Show(int item, Bool show);
  char* GetLabel(void);
  void SetLabel(char* label);
  void SetLabel(int item, char* label);
  void SetLabel(int item, wxBitmap* bitmap);
  char* GetLabel(int item);

  void ChangeColour(void);
  void ChangeToGray(Bool gray);

  void Command(wxCommandEvent& event); // mac platform only
};

#endif // IN_CPROTO
#endif // wx_rboxh
