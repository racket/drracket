///////////////////////////////////////////////////////////////////////////////
// File:	wx_item.h
// Purpose:	Declares panel items (controls/widgets) for Mac
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_itemh
#define wx_itemh

#include "wb_item.h"

#ifdef IN_CPROTO
typedef       void* wxItem ;
#else

class wxItem: public wxbItem
{
public:
	wxItem(void);
	// Constructor (given parentArea)
	wxItem (wxArea*	parentArea, int x = -1, int y = -1, int width = -1, int height = -1,
		long style = 0, char* windowName = "item");
	// Constructor (given parentWindow)
	wxItem (wxWindow*	parentWindow, int x = -1, int y = -1, int width = -1, int height = -1,
		long		style = 0, char* windowName = "item");
	// Constructor (given objectType; i.e., menu or menuBar)
	wxItem (char* windowName);

	~wxItem(void);

#if 0 // FIXME - these are from 16.1 below comes from msw, they should be virtual?
	void GetSize(int *width, int *height);
	void GetPosition(int *x, int *y);
	void SetSize(int x, int y, int width, int height);
	void SetClientSize(int width, int height);
	void SetFocus(void);
	char *GetLabel(void);
#endif
	void SetLabel(char *label) {};	// This should be overridden in all subclasses

	virtual void SetBackgroundColour(wxColour* col);
	virtual void SetLabelColour(wxColour* col);
	virtual void SetButtonColour(wxColour* col);
	virtual void ChangeColour(void);

	virtual void OnChar(wxKeyEvent& event); // mac platform only
};

char *wxItemStripLabel(char *label);

#endif // IN_CPROTO
#endif // wx_itemh
