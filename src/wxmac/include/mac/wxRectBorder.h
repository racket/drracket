///////////////////////////////////////////////////////////////////////////////
// File:	wxRectBorder.h
// Purpose:	Declares wxRectBorder item (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxRectBorderh
#define wxRectBorderh

#include "wxBorder.h"

#ifdef IN_CPROTO
typedef       void* wxRectBorder ;
#else

// RectBorder item
class wxRectBorder: public wxBorder
{
private:
	int cWhitespace; // white border interior width

//=============================================================================
// Public constructors
//=============================================================================
public:

	wxRectBorder // Constructor (given parentArea)
	(
		wxArea*		parentArea,
		int			margin = 1,
		Direction	direction = Direction::wxAll,
		int			whitespace = 0,
		char*		windowName = "RectBorder",
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		WXTYPE		objectType = wxTYPE_BORDER 
	);

//=============================================================================
// Public destructor
//=============================================================================
public:

	~wxRectBorder(void);
    
	void SetBrush(wxBrush *b) { cBrush = b; }

//=============================================================================
// Public methods
//=============================================================================
public:
	virtual void Paint(void);

	virtual void DoShow(Bool on);
};

#endif // IN_CPROTO
#endif // wxRectBorderh