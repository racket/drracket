///////////////////////////////////////////////////////////////////////////////
// File:	wx_text.h
// Purpose:	wxTextWindow - simple text subwindow class (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_texth
#define wx_texth

#include "wb_text.h"

#ifdef IN_CPROTO
typedef       void* wxTextWindow ;
#else

class wxFrame;
class wxMultiText;

class wxTextWindow: public wxbTextWindow
{
 public:
	wxMultiText* cTextBox; // mac platform only

//=============================================================================
// Public constructors
//=============================================================================
public:

	wxTextWindow // Constructor (given parentArea)
	(
		wxArea*	parentArea,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		char*		windowName = "textWindow",
		WXTYPE		objectType = wxTYPE_TEXT_WINDOW
	);

	wxTextWindow // Constructor (given parentFrame)
	(
		wxFrame*	parentFrame,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		char*		windowName = "textWindow",
		WXTYPE		objectType = wxTYPE_TEXT_WINDOW
	);

	wxTextWindow // Constructor (given parentFrame)
	(
		wxPanel*	parentFrame,
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		char*		windowName = "textWindow",
		WXTYPE		objectType = wxTYPE_TEXT_WINDOW
	);
//=============================================================================
// Public destructor
//=============================================================================
public:

	~wxTextWindow(void);

//=============================================================================
// Private methods
//=============================================================================
private:

	void CreateWxTextWindow(void); // common constructor initialization

//=============================================================================
// Public methods
//=============================================================================
public:

//=============================================================================
// Window coordinate system transformation methods
//=============================================================================
	Bool LoadFile(char* file);
	Bool SaveFile(char* file);
	void WriteText(char* text);
	void Clear(void);
	void DiscardEdits(void);
	Bool Modified(void);
	char* GetContents(void);
	void SetInsertionPoint(long pos);
	void SetInsertionPointEnd(void);
	long GetInsertionPoint(void);
	long GetLastPosition(void);
	long XYToPosition(long x, long y);
	void PositionToXY(long pos, long* x, long* y);	
	int GetNumberOfLines(void);
	void ShowPosition(long pos);
	int GetLineLength(long lineNo);
	int GetLineText(long lineNo, char* buf);
	void Replace(long from, long to, char* value);
	void Remove(long from, long to);
  	virtual void SetSelection(long from, long to);
 	virtual void Copy(void); // Copy selection to clipboard
  	virtual void Paste(void); // Paste clipboard into text window
 	virtual void Cut(void); // Copy selection to clipboard, then remove selection.
	void SetEditable(Bool editable);
  	virtual void SetFont(wxFont *theFont);
	// Functions not in wxbTextWindow
	virtual void Paint(void);
	virtual void DoShow(Bool show);
	void Enable(Bool Flag);
	virtual void SetFocus(void);
	void Highlight(long from, long to);
};

#endif // IN_CPROTO
#endif // wx_texth
