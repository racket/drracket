///////////////////////////////////////////////////////////////////////////////
// File:	wx_frame.h
// Purpose:	wxFrame declaration (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_frameh
#define wx_frameh

#include "wb_frame.h"

#ifdef IN_CPROTO
typedef       void* wxFrame;
#else

class wxMenuBar;
class wxCommandEvent;
class wxPanel;
class wxText;
class wxDialogBox;

class wxFrame: public wxbFrame
{
//=============================================================================
// Protected variables
//=============================================================================
protected:

	char		cWindowTitle[256];
	Bool		cMaximized;
	wxPanel*	cStatusPanel;
	wxText*		cStatusText;
  	wxArea* 	cPlatformArea; // mac platform only
  	wxArea* 	cContentArea; // mac platform only
  	wxArea* 	cControlArea; // mac platform only
	wxWindow*	cFocusWindow; // mac platform only
	MenuHandle	cAppleMenuHandle;
	Bool		cIsModal;

//=============================================================================
// Public methods
//=============================================================================
public:
    int         cBusyCursor;

	wxDialogBox* cDialogPanel; //cjc, mflatt

	void NowFront(Bool on);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Constructors
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	wxFrame // Constructor (for frame window)
	(
		wxFrame*	parentFrame,		// this is ignored
		char*		windowTitle,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = wxSDI | wxDEFAULT_FRAME,
		char*		windowName = "frame",
		WXTYPE		objectType = wxTYPE_FRAME
	);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Destructor
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	~wxFrame(void);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Geometry methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	wxArea* PlatformArea(void); // mac platform only
	wxArea* ContentArea(void); // mac platform only
	wxArea* ControlArea(void); // mac platform only

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Sizing methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	virtual void DoSetSize(int x, int y, int width, int height);
	void Maximize(Bool maximize);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Status line methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void CreateStatusLine(int number = 1, char* name = "status_line");
	void SetStatusText(char* text, int number = 0);
 	void SetStatusEraser(wxBrush* b); // tom

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Menubar methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void SetMenuBar(wxMenuBar* menu_bar);
	void Command(int id); // Call this to simulate a menu command

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Icon methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void SetIcon(wxIcon* icon);
	void Iconize(Bool iconize);
	Bool Iconized(void);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Platform methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	Bool IsVisible(void);
	void MacUpdateWindow(void);
   	void MacDrawGrowIcon(void);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	char* GetTitle(void);
	void SetTitle(char* title);
	void Show(Bool show);
	Bool IsFrontWindow(void);
	virtual Bool IsModal(void);
	void MakeModal(Bool on);
	wxWindow* GetFocusWindow(void);
	void SetFocusWindow(wxWindow* window);
	void LoadAccelerators(char* table);
	void OnActivate(Bool active);
	void Enable(Bool on);
	virtual void ChangeToGray(Bool on);
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	virtual void Paint(void);
	virtual void OnChar(wxKeyEvent& event); // mac platform only
	virtual void OnCommandEvent(wxCommandEvent& event) {}; // mac platform only

    WindowPtr macWindow(void);

//=============================================================================
// Protected methods
//=============================================================================
protected:

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Sizing methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void wxMacRecalcNewSize(void);

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Menubar methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void ProcessCommand(int id);
	virtual void ShowAsActive(Bool flag); // mac platform only

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Platform methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	virtual Bool IsMacWindow(void);
	void wxMacStartDrawing(GrafPtr& oldPort, int& savePortH, int& savePortV);
	void wxMacStopDrawing(GrafPtr oldPort, int savePortH, int savePortV);
	Rect wxMacGetContRect(void);
	Rect wxMacGetStrucRect(void);
	inline wxDialogBox* wxMacGetDialog(void) {return cDialogPanel;} 

//=============================================================================
// Private methods
//=============================================================================
private:

	void InitDefaults(void); // used by constructors

//=============================================================================
// Friend classes
//=============================================================================
private:

	friend class wxApp;
	friend class wxDialogBox;
};

#endif // IN_CPROTO
#endif // wx_frameh
