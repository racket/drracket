///////////////////////////////////////////////////////////////////////////////
// File:	wx_dlog.h
// Purpose:	wxDialogBox (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_dlogh
#define wx_dlogh
#include "wx_frame.h"
#include "wb_dialg.h"

#ifdef IN_CPROTO
typedef void* wxDialogBox;
#else

class wxPanel;
class wxButton;

class wxDialogBox: public wxPanel
{
 protected:
	int cButtonPressed;

//=============================================================================
// Public constructors
//=============================================================================
public:
	wxFrame *cFrame;		//mflatt, cjc

	wxDialogBox // Constructor (for dialog window)
	(
		wxWindow*	parentFrame,		// this is ignored, used to be wxFrame*
		char*		windowTitle,
		Bool		modal = FALSE,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = wxDEFAULT_DIALOG_STYLE,
		char*		windowName = "Dialog",
		WXTYPE		objectType = wxTYPE_DIALOG_BOX
	);

//=============================================================================
// Public destructor
//=============================================================================
public:

	~wxDialogBox();

//=============================================================================
// Public methods
//=============================================================================
public:

	virtual Bool IsShown(void);

	void Show(Bool show);
	Bool IsModal(void);
	void ShowModal(void);
	int GetButtonPressed(void);
	void SetButtonPressed(int buttonPressed);
	Bool OnClose(void);

	virtual void Fit(void);
	virtual void Centre(int d) { cFrame->Centre(d); }

	void SetIcon(wxIcon* icon) { cFrame->SetIcon(icon); }
	void Iconize(Bool iconize) { cFrame->Iconize(iconize); }
	Bool Iconized(void)	   { return cFrame->Iconized(); }
	
    char* GetTitle(void) { return cFrame->GetTitle(); }
	void SetTitle(char* title) { cFrame->SetTitle(title); }
	wxWindow* GetFocusWindow(void) { return cFrame->GetFocusWindow(); }
	void SetFocusWindow(wxWindow* window) { cFrame->SetFocusWindow(window); }
	void LoadAccelerators(char* table) { cFrame->LoadAccelerators(table); }
	virtual void SetSize(int x, int y, int width, int height, int flags = wxSIZE_AUTO);
	virtual void OnSize(int w, int h);
};

void wxDialogOkButton(wxButton& but, wxEvent& event);
void wxDialogCancelButton(wxButton& but, wxEvent& event);
void wxDialogYesButton(wxButton& but, wxEvent& event);
void wxDialogNoButton(wxButton& but, wxEvent& event);

void wxSplitMessage(char *message, wxList *messageList, wxPanel *panel);
void wxCentreMessage(wxList *messageList);

int wxMessageBox(char *message, char *caption = "Message", long style = wxOK|wxCENTRE,
  wxWindow* parent = NULL, int x = -1, int y = -1);

#endif // IN_CPROTO
#endif // wx_dialg.h
