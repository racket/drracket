///////////////////////////////////////////////////////////////////////////////
// File:	wx_canvs.h
// Purpose:	wxCanvas subwindow declarations (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_canvsh
#define wx_canvsh

#include "wb_canvs.h"

#ifdef IN_CPROTO
typedef       void* wxCanvas ;
#else

class wxFrame;

// Canvas subwindow for drawing on
class wxCanvas: public wxbCanvas
{
 public:
	int units_per_page_x;
	int units_per_page_y;
	int units_x ;
	int units_y ;
	int hExtent;   // Actual extent of virtual scrolled canvas
	int vExtent;
	Bool hScrollingEnabled;
	Bool vScrollingEnabled;
    Bool scrollAutomanaged;

//=============================================================================
// Public constructors
//=============================================================================
 public:

	wxCanvas // Constructor (given parentFrame)
	(
		wxFrame*	parentFrame,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = wxRETAINED,
		char*		windowName = "canvas",
		WXTYPE		objectType = wxTYPE_CANVAS
	);

 	wxCanvas // Constructor (given parentArea)
 	(
 		wxArea*		parentArea,
 		int 		x = -1,
 		int			y = -1,
 		int			width = -1,
 		int			height = -1,
 		long		style = wxRETAINED,
 		char*		windowName = "canvas",
 		WXTYPE		objectType = wxTYPE_CANVAS
 	);

 	wxCanvas // Constructor (given parentWindow)
 	(
 		wxWindow*	parentWindow,
 		int 		x = -1,
 		int			y = -1,
 		int			width = -1,
 		int			height = -1,
 		long		style = wxRETAINED,
 		char*		windowName = "canvas",
 		WXTYPE		objectType = wxTYPE_CANVAS
 	);

//=============================================================================
// Public destructor
//=============================================================================
public:

	virtual ~wxCanvas(void);

//=============================================================================
// Private methods
//=============================================================================
private:

	void InitDefaults(void);

//=============================================================================
// Public methods
//=============================================================================
 public:

	virtual void BeginDrawing(void);
	virtual void EndDrawing(void);
	inline wxCanvasDC *GetDC(void) { return wx_dc; }

	void OnClientAreaDSize(int dW, int dH, int dX, int dY);

	// Number of pixels per user unit (0 or -1 for no scrollbar)
	// Length of virtual canvas in user units
	// Length of page in user units
	void SetScrollbars(int horizontal, int vertical,
                             int x_length, int y_length,
                             int x_page, int y_page,
                             int x_pos = 0, int y_pos = 0,
							 Bool automgmt = TRUE 
						);

	virtual void SetScrollData // mac platform only
	(
		wxScrollData*		scrollData,
		wxWhatScrollData	whatScrollData,
		wxWindow*			iniatorWindow
	);

	// Scroll the canvas
	void Scroll(int xPos, int yPos);
	void GetScrollUnitsPerPage(int* x_page, int* y_page);
	virtual int GetScrollsPerPage(int orientation); // mac platform only

	void ViewStart(int* x, int* y);

	// Actual size in pixels when scrolling is taken into account
	void GetVirtualSize(int* x, int* y);

	void SetColourMap(wxColourMap* cmap);
	virtual void OnScroll(wxCommandEvent& event) { };

	// Enable/disable Windows 3.1 scrolling in either direction.
	// If TRUE, wxWindows scrolls the canvas and only a bit of
	// the canvas is invalidated; no Clear() is necessary.
	// If FALSE, the whole canvas is invalidated and a Clear() is
	// necessary. Disable for when the scroll increment is used
	// to actually scroll a non-constant distance
	void EnableScrolling(Bool x_scrolling, Bool y_scrolling);

	virtual void WarpPointer(int x_pos, int y_pos) ;

	void DoShow(Bool show);

	virtual void ClientToLogical(int* x, int* y); // mac platform only; testing
	// some new methods for wxMedia
	void SetScrollPage(int dir, int val);
	void SetScrollRange(int dir, int val);
	int  GetScrollPos(int dir);
    int  GetScrollPage(int dir);
    int  GetScrollRange(int dir);
	void SetScrollPos(int dir, int val);

	virtual void ChangeToGray(Bool gray);

	virtual void Paint(void);
	virtual void OnPaint(void);

	virtual void AddWhiteRgn(RgnHandle rgn);

	Bool WantsFocus();
};

#endif // IN_CPROTO
#endif // wx_canvsh
