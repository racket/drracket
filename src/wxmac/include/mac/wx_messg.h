///////////////////////////////////////////////////////////////////////////////
// File:	wx_messg.h
// Purpose:	Declares message panel item (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_messgh
#define wx_messgh

#include "wb_messg.h"

#ifdef IN_CPROTO
typedef       void* wxMessage ;
#else

class wxMessage: public wxbMessage
{
 public:
 	char* 	cMessage; // mac platform only
	wxBitmap*	sBitmap;
//=============================================================================
// Public constructors
//=============================================================================
public:

	wxMessage // Constructor (given parentArea)
	(
		wxArea*		parentArea,
		char*		label,
		int 		x = -1,
		int			y = -1,
		long		style = 0,
		char*		windowName = "message",
		WXTYPE		objectType = wxTYPE_MESSAGE
	);

	wxMessage // Constructor (given parentArea, font)
	(
		wxArea*		parentArea,
		char*		label,
		wxFont*		theFont,
		int 		x = -1,
		int			y = -1,
		long		style = 0,
		char*		windowName = "message",
		WXTYPE		objectType = wxTYPE_MESSAGE
	);

	wxMessage // Constructor (given parentPanel)
	(
		wxPanel*	parentPanel,
		char*		label,
		int 		x = -1,
		int			y = -1,
		long		style = 0,
		char*		windowName = "message",
		WXTYPE		objectType = wxTYPE_MESSAGE
	);

	wxMessage // Constructor (given parentPanel, font)
	(
		wxPanel*	parentPanel,
		char*		label,
		wxFont*		theFont,
		int 		x = -1,
		int			y = -1,
		long		style = 0,
		char*		windowName = "message",
		WXTYPE		objectType = wxTYPE_MESSAGE
	);

	wxMessage // Constructor (given parentPanel)
	(
		wxPanel*	parentPanel,
		wxBitmap*	bitmap,
		int 		x = -1,
		int			y = -1,
		long		style = 0,
		char*		windowName = "message",
		WXTYPE		objectType = wxTYPE_MESSAGE
	);

//=============================================================================
// Public destructor
//=============================================================================
public:

	~wxMessage(void);

//=============================================================================
// Private methods
//=============================================================================
private:
	void CreateWxMessage(char* label, wxFont* theFont = NULL); // common constructor initialization

//=============================================================================
// Public methods
//=============================================================================
public:

//=============================================================================
// Window coordinate system transformation methods
//=============================================================================
	virtual char* GetLabel(void); // mac platform only
	virtual void SetLabel(char* label); // mac platform only
	virtual void SetLabel(wxBitmap *bitmap);
	virtual void Paint(void); // mac platform only
	virtual void DoShow(Bool show); // mac platform only

    virtual void ChangeToGray(Bool gray);

	void ChangeColour(void) ;
};

#endif // IN_CPROTO
#endif // wx_messg
