///////////////////////////////////////////////////////////////////////////////
// File:	wx_buttn.h
// Purpose:	wxButton (Macintosh implementation)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_buttnh
#define wx_buttnh

#include "wb_buttn.h"
class wxPanel;

#ifdef IN_CPROTO
typedef       void* wxButton;
#else

// Pushbutton

class wxBitmap;
class wxArea;

class wxButton: public wxbButton
{
 private:
  // For window area
  	wxArea* cBorderArea; // mac platform only
	CCTabHandle cColorTable; // mac platform only

 public:
	wxBitmap* buttonBitmap;
	ControlHandle cMacControl; // mac control for push button

//=============================================================================
// Public constructors
//=============================================================================
public:

	wxButton // Constructor (given parentPanel, label)
	(
		wxPanel*	parentPanel,
		wxFunction	function,
		char*		label,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		char*		windowName = "button",
		WXTYPE		objectType = wxTYPE_BUTTON
	);

	wxButton // Constructor (given parentPanel, bitmap)
	(
		wxPanel*	parentPanel,
		wxFunction	function,
		wxBitmap*	bitmap,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		char*		windowName = "button",
		WXTYPE		objectType = wxTYPE_BUTTON
	);

    void Create // Constructor (given parentPanel, label)
	(
		wxPanel*	parentPanel,
		wxFunction	function,
		char*		label,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		char*		windowName = "button",
		WXTYPE		objectType = wxTYPE_BUTTON
	);

//=============================================================================
// Public destructor
//=============================================================================
public:

	~wxButton(void);

//=============================================================================
// Public methods
//=============================================================================
public:

//=============================================================================
// Window coordinate system transformation methods
//=============================================================================
	virtual void OnClientAreaDSize(int dW, int dH, int dX, int dY); // mac platform only
	void SetDefault(Bool flag = TRUE); // WCH wx_mac: original had void as argument
	virtual void OnSetDefault(Bool flag = TRUE); // WCH wx_mac: addition to original
	void SetLabel(char*);
	void SetLabel(wxBitmap* bitmap);
	char* GetLabel(void);
	void Command(wxCommandEvent& event);

	void ChangeColour(void);

	virtual void Enable(Bool enable);
	virtual void Paint(void);
	virtual void DoShow(Bool show);

	virtual void ShowAsActive(Bool flag); // mac platform only
	virtual void OnEvent(wxMouseEvent& event); // mac platform only
 	virtual void Highlight(Bool flag); // mac platform only, supercedes ShowAsActive

	virtual void ChangeToGray(Bool gray);
};

#endif // IN_CPROTO
#endif // wx_buttnh
