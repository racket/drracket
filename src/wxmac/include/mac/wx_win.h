///////////////////////////////////////////////////////////////////////////////
// File:	wx_win.h
// Purpose:	wxWindow class declaration (Macintosh version).
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_winh
#define wx_winh


#include "wb_win.h"
//#include "wx_screen.h"
#include "wxDirection.h"
#include "wxScrollData.h"
#	include <Menus.h>
#	include <QuickDraw.h>
#	include <Fonts.h>
#	include <Events.h>
#	include <Dialogs.h>
#	include <TextEdit.h>
#	include	<Scrap.h>
#	include	<ToolUtils.h>
/*
 * Base class for frame, panel, canvas, panel items, dialog box.
 * and screen (for the mac)
 */

#ifdef IN_CPROTO
typedef       void* wxWindow ;
#else

class wxMacDC;
class wxFrame;
class wxBrush;
class wxArea;
class wxMargin;
class wxPanel;
class wxScroll;
class wxScreen;


class wxWindow: public wxbWindow
{
//=============================================================================
// Public variables
//=============================================================================
public:

	static wxWindow* gMouseWindow; // mac platform only (window that has captured mouse)

//=============================================================================
// Protected variables
//=============================================================================
protected:

	Bool 		cColour; 		// use colour for this window?
	Bool		cActive;		// active window is "highlighted" (mac platform only)
	Bool		cEnable;		// enabled window accepts mouse/keyboard events
	Bool		cHidden;		// Hidden?
	Bool		cUserHidden;	// Hidden because user asked (not just inherited)?
	long		cStyle;			// mac platform only
	Direction	cGravitate;		// mac platform only
	Direction	cJustify;		// mac platform only
	wxWindow* 	window_parent; 	// Each window always knows its parent
	wxChildList* children; 		// Window's children
	wxScroll* 	cScroll; 		// for scroll data and scroll synchronization
	wxMacDC*	cMacDC;			// mac platform only
	wxBrush* 	cBrush; 		// foreground brush
	wxBrush* 	cEraser; 		// background brush
	Pattern 	cMacPattern; 	// mac platform only (temp work pattern)

  // For window rectangle
 	int 		cWindowX;
 	int 		cWindowY;
 	int 		cWindowHeight;
 	int 		cWindowWidth;
 	
 	int			internal_gray;

  // For window area
  	wxArea* 	cParentArea; 	// mac platform only
  	wxList 		cAreas; 		// mac platform only
  	wxArea* 	cClientArea; 	// mac platform only

//=============================================================================
// Public methods
//=============================================================================
public:

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Constructors
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	wxWindow(void);

	wxWindow // Constructor (for screen window)
	(
		char*		windowName,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style
	);

	wxWindow // Constructor (given parentScreen; i.e., this is frame)
	(
		char*		windowName,
		wxScreen*	parentScreen,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style
	);

	wxWindow // Constructor (given parentArea)
	(
		char*		windowName,
		wxArea*		parentArea,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style
	);

	wxWindow // Constructor (given parentWindow)
	(
		char*		windowName,
		wxWindow*	parentWindow,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style
	);

	wxWindow // Constructor (given objectType; i.e., menu or menuBar)
	(
		char*		windowName
	);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Destructor
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	~wxWindow(void);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Geometry methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	int Width(void); // mac platform only
	int Height(void); // mac platform only
	wxMargin Margin(wxArea* outerArea); // mac platform only
	wxMargin Margin(wxWindow* outerWindow); // mac platform only
	virtual void GetPosition(int* windowX, int* windowY);
	virtual void GetSize(int* width, int* height);
	virtual void GetClientSize(int* width, int* height); // Size client can use
	virtual void ClientToScreen(int* x, int* y);
	virtual void ScreenToClient(int* x, int* y);
	virtual void ClientToLogical(int* x, int* y); // mac platform only; testing

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Sizing methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void SetWidthHeight(int width, int height); // mac platform only
	virtual void SetSize(int x, int y, int width, int height, int flags = wxSIZE_AUTO); // mac platform only
	virtual void DoSetSize(int x, int y, int width, int height); // mac platform only
	virtual void SetClientSize(int newClientWidth, int newClientHeight);
	void GravitateJustify(Direction gravitate, Direction justify,
				int left, int top, int right, int bottom); // mac platform only
	virtual void Fit(void);  // mac platform only

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Device context methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	wxMacDC* MacDC(void); // mac platform only

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Tree (windows and areas) methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	wxArea* ParentArea(void); //mac platform only
	wxList* Areas(void); //mac platform only
	virtual wxArea* ClientArea(void); // mac platform only
	virtual wxWindow* GetParent(void);
	virtual wxChildList* GetChildren(void);
	virtual wxWindow* GetGrandParent(void);
	virtual wxFrame* GetRootFrame(void); // mac platform only

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Scroll methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	virtual void AddChildScrollWindow(wxWindow* childScrollWindow); // mac platform only
	wxScroll* GetScroll(void); // mac platform only
	virtual void SetScrollData // Must override if window scrolls
	(
		wxScrollData*		scrollData,
		wxWhatScrollData	whatScrollData,
		wxWindow*			iniatorWindow
	);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Mouse methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void CaptureMouse(void);
	void ReleaseMouse(void);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Keyboard methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	virtual void SetFocus(void);
	virtual void OnSetFocus(void);
	virtual void OnKillFocus(void);
	virtual Bool WantsFocus(void); // mac platform only

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Quill methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  	virtual void SetFont(wxFont* theFont); // mac platform only
	float GetCharHeight(void);
	float GetCharWidth(void);
	void GetTextExtent(const char* string, float* x, float* y, float* descent = NULL,
  						float* externalLeading = NULL, wxFont *thefont = NULL,
  						Bool use16 = FALSE);
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	Bool PopupMenu(wxMenu* menu, float x, float y);
	wxCursor* SetCursor(wxCursor* cursor);
	void SetColourMap(wxColourMap* cmap);
	virtual Bool IsMacWindow(void); // mac platform only
	virtual void DoPeriodicAction(void); // mac platform only
//+++++ Begin Macintosh Platform only +++++
	virtual void Enable(Bool Flag);
	Bool IsEnable(void);
	Bool CanAcceptEvent(void);
	Direction GetGravitate(void);
	void SetGravitate(Direction direction);
	Direction GetJustify(void);
	void SetJustify(Direction direction);
	virtual void DragAcceptFiles(Bool accept);
	void SetEraser(wxBrush* eraser);
	inline wxBrush *GetEraser() { return cEraser; }
 	virtual void DestroyFocus(); // tom (fettig@dfki.uni-sb.de)
//+++++ End Macintosh Platform only +++++

	virtual void AddWhiteRgn(RgnHandle rgn);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Sizing methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	virtual void OnWindowDSize(int dW, int dH, int dX = 0, int dY = 0);  // mac platform only
	virtual void OnAreaDSize(int dW, int dH, int dX = 0, int dY = 0);  // mac platform only
	virtual void OnClientAreaDSize(int dW, int dH, int dX, int dY);  // mac platform only

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Device context methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void SetCurrentMacDCNoMargin(void); // mac platform only
	virtual void SetCurrentMacDC(void); // mac platform only
	virtual void SetCurrentDC(void); // mac platform only
	virtual void MacSetBackground(void); // mac platform only
	virtual void SetForeground(void); // mac platform only
	virtual void SetTextInfo(void); // mac platform only
	virtual void GetClipRect(wxArea* area, Rect* clipRect); // mac platform only

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Tree (windows and areas) methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	virtual void AddChild(wxObject* child); // Adds reference to the child object
	virtual void OnDeleteChildWindow(wxWindow* childWindow); // mac platform only
	virtual void OnDeleteChildArea(wxArea* childArea); // mac platform only
	virtual void DestroyChildren(void); // Removes and destroys all children

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Activate methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	virtual void Activate(Bool flag);	 // mac platform only
	virtual void ShowAsActive(Bool flag); // mac platform only
	virtual void OnActivate(Bool flag);  // mac platform only

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Mouse methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	virtual Bool SeekMouseEventArea(wxMouseEvent& mouseEvent); // mac platform only
	virtual Bool AdjustCursor(int mouseX, int mouseY); // mac platform only - GRW

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	virtual void Paint(void);
	void Refresh(void);
	
	virtual Bool PreOnEvent(wxWindow *win, wxMouseEvent *event);
	virtual Bool PreOnChar(wxWindow *win, wxKeyEvent *event);

	virtual void Show(Bool);

	virtual void DoShow(Bool);
	Bool CanShow(Bool);
	
	inline Bool IsHidden(void) { return cHidden; }
	inline Bool IsUserHidden(void) { return cUserHidden; }
	
	virtual Bool IsShown(void) { return !IsUserHidden(); }
	
	virtual void ChangeToGray(Bool gray);
	
	void InternalGray(Bool gray);
	Bool IsGray(void);
	void ChildrenInternalGray(Bool gray);
	
	virtual void Highlight(Bool flag);
	int Track(Point start);

	void ForEach(void (*foreach)(wxWindow *w, void *data), void *data);

//=============================================================================
// Private methods
//=============================================================================
private:

	void InitDefaults(void); // used by constructors
	void InitWindowPostion(int x, int y); // used by constructors

//=============================================================================
// Friend classes
//=============================================================================
private:

	friend class wxArea;
};

#endif // IN_CPROTO
#endif
