///////////////////////////////////////////////////////////////////////////////
// File:	wx_txt.h
// Purpose:	wxText - simple text subwindow class (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_txth
#define wx_txth

#include "wb_txt.h"

#ifdef IN_CPROTO
typedef       void* wxText;
#else

class wxFrame;
class wxTextWindow;
class wxLabelArea;

// Single-line text item
class wxText: public wxbText
{
protected:
	TEHandle cMacTE; // mac platform only
	Bool cTextEditable; // mac platform only
	Bool cTextModified; // mac platform only
	wxLabelArea* cLabelArea; // mac platform only

//=============================================================================
// Public constructors
//=============================================================================
public:
	wxText(void);
	wxText // Constructor (given parentTextWindow)
	(
		wxTextWindow*	parentTextWindow,
		wxFunction		function,
		char*			label,
		char*			value = "",
		int 			x = -1,
		int				y = -1,
		int				width = -1,
		int				height = -1,
		long			style = 0,
		char*			windowName = "text",
		WXTYPE			objectType = wxTYPE_TEXT
	);

	wxText // Constructor (given parentPanel)
	(
		wxPanel*		parentPanel,
		wxFunction		function,
		char*			label,
		char*			value = "",
		int 			x = -1,
		int				y = -1,
		int				width = -1,
		int				height = -1,
		long			style = 0,
		char*			windowName = "text",
		WXTYPE			objectType = wxTYPE_TEXT
	);

//=============================================================================
// Public destructor
//=============================================================================
public:

	~wxText(void);

//=============================================================================
// Private methods
//=============================================================================
private:

	void CreateWxText(char* label, char* value); // common constructor initialization

//=============================================================================
// Public methods
//=============================================================================
public:

//=============================================================================
// Window coordinate system transformation methods
//=============================================================================
	void WriteText(char* text);
	virtual void OnClientAreaDSize(int dW, int dH, int dX, int dY);
	void Clear(void);
	void DiscardEdits(void);
	Bool Modified(void);
	void SetModified(Bool modified); // mac platform only
	char* GetContents(void);
	void SetInsertionPoint(long pos);
	void SetInsertionPointEnd(void);
	long GetInsertionPoint(void);
	long GetLastPosition(void);
	long XYToPosition(long x, long y);
	void PositionToXY(long* x, long* y);
	void ShowPosition(long pos);
	int GetLineLength(long lineNo);
	int GetLineText(long lineNo, char* buf);
	int GetNumberOfLines(void);
	void Replace(long from, long to, char* value);
	void Remove(long from, long to);
	void Highlight(long from, long to);
	void SetEditable(Bool editable);
	void Paint(void);
	void DoShow(Bool show);
	void DoPeriodicAction(void); // mac platform only
	void OnChar(wxKeyEvent& event); // mac platform only
	void OnEvent(wxMouseEvent& event); // WCH : mac only ?

	void ChangeToGray(Bool gray);

	void SetTextInfo(void); // mac platform only
  	virtual void SetFont(wxFont* theFont);

	void SetLabel(char*);
	char* GetLabel(void) ;
	char* GetValue(void);
	void SetValue(char* value);
	void ChangeColour(void);
	virtual void Copy(void);
	virtual void Cut(void);
	virtual void Paste(void);

	virtual Bool WantsFocus(void);

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Activate methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	void OnSetFocus(); // mac platform only
	void OnKillFocus();

//=============================================================================
// Protected methods
//=============================================================================
protected:

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Scroll methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	virtual void macAdjustScrollBars(void); // mac platform only
	virtual void SetScrollData // adjust text within window to match scroll bar setting
	(
		wxScrollData*		newScrollData,
		wxWhatScrollData	whatScrollData,
		wxWindow*			iniatorWindow
	);

};

#endif // IN_CPROTO
#endif // wx_txth
