///////////////////////////////////////////////////////////////////////////////
// File:	wx_panel.h
// Purpose:	wxPanel subwindow, for panel items (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_panelh
#define wx_panelh

#include "wb_panel.h"

#define PANEL_HSPACING  10
#define PANEL_VSPACING  8
#define PANEL_LEFT_MARGIN 4
#define PANEL_TOP_MARGIN  4

#ifdef IN_CPROTO
typedef       void* wxPanel;
#else

class wxItem;
class wxFrame;

class wxPanel: public wxbPanel
{
//=============================================================================
// Protected variables
//=============================================================================
protected:

	int			cursor_x;
	int			cursor_y;
	int			max_width;
	int			max_height;
	int			max_line_height;
	int 		currentRow;
	int 		currentCol;
	wxWindow*	last_created;

    wxArea*     cPanelBorder;

friend class wxWindow;	
//=============================================================================
// Public methods
//=============================================================================
public:

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Constructors
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	wxPanel // Constructor (given parentArea)
	(
		wxArea*		parentArea,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		char*		windowName = "panel",
		WXTYPE		objectType = wxTYPE_PANEL
	);

	wxPanel // Constructor (given parentFrame)
	(
		wxFrame*	parentFrame,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		char*		windowName = "panel",
		WXTYPE		objectType = wxTYPE_PANEL
	);

	wxPanel // Constructor (given parentPanel)
	(
		wxPanel*	parentPanel,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		char*		windowName = "panel",
		WXTYPE		objectType = wxTYPE_PANEL
	);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Destructor
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	~wxPanel(void);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Sizing methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void Centre(int direction);
	void Fit(void);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Item placement methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void NewLine(void);
	void NewLine(int pixels);
	void Tab(void);
	void Tab(int pixels); // Tab specified number of pixels
	void GetCursor(int* x, int* y);
	void SetItemCursor(int x, int y);
	void SetHorizontalSpacing(int sp);
	int GetHorizontalSpacing(void);
	void SetVerticalSpacing(int sp);
	int GetVerticalSpacing(void);
	void AdvanceCursor(wxWindow* item); // Update next cursor position
	Bool WantsFocus();

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void SetBackgroundColour(wxColour*col);
	void SetLabelColour(wxColour*col);
	void SetButtonColour(wxColour*col);
	void ChangeColour(void);
	void DoShow(Bool show);
	void Paint(void);
	virtual void OnChar(wxKeyEvent& event); // mac platform only

 	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 	// Tree methods
 	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 	void DestroyChildren() ; // tom
 	void OnDeleteChild(wxWindow* win); //tom
 
    virtual void ChangeToGray(Bool gray);

//=============================================================================
// Protected methods
//=============================================================================
protected:

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Tree methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void AddChild(wxObject* child);
	//virtual void OnDeleteChildWindow(wxWindow* childWindow); 

//=============================================================================
// Private methods
//=============================================================================
private:

	void CreateWxPanel(void); // common constructor initialization
	void InitDefaults(void); // used by constructors
};

#endif // IN_CPROTO
#endif // wx_panelh
