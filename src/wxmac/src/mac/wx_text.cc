///////////////////////////////////////////////////////////////////////////////
// File:	wx_text.cc
// Purpose:	wxTextWindow implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

//#include <iostream.h>
//#include <fstream.h>

#ifdef GUSI
#  include <sys/types.h>	// Get these from GUSI
#  include <sys/stat.h>
#elif !defined(MPW) 		// ie #if Symantec, plain CW
#  include <unix.h>
#else
#  include "macstat.h"		// Python or MPW
#endif
#include <stdio.h>
#include "wx_item.h"
#include "wx_text.h"
#include "wx_mtxt.h"
#include "wx_frame.h"
#include "wx_area.h"
#include "wxBorderArea.h"
#include "wxDirection.h"
#include "wx_mac_utils.h"

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxTextWindow::wxTextWindow // Constructor (given parentArea)
	(
		wxArea*	parentArea,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbTextWindow (objectType, windowName, parentArea, x, y, width, height, style)
{
	CreateWxTextWindow();
}

//-----------------------------------------------------------------------------
wxTextWindow::wxTextWindow // Constructor (given parentFrame)
	(
		wxFrame*	parentFrame,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbTextWindow (objectType, windowName, parentFrame, x, y, width, height, style)
{
	CreateWxTextWindow();
}

//-----------------------------------------------------------------------------
wxTextWindow::wxTextWindow // Constructor (given parentFrame)
	(
		wxPanel*	parentFrame,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbTextWindow (objectType, windowName, parentFrame, x, y, width, height, style)
{
	CreateWxTextWindow();
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxTextWindow::~wxTextWindow(void)
{
  // necessary change: We could be focus_window! So lets change the focus_window 
  // for our frame! - tom
  DestroyFocus();
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxTextWindow::CreateWxTextWindow(void) // common constructor initialization
{
// CJC 	if (cStyle & wxBORDER) new wxBorderArea(this);
	new wxBorderArea(this);

	int clientWidth = ClientArea()->Width();
	int clientHeight = ClientArea()->Height();
	if (cStyle & wxNATIVE_IMPL) {
		cStyle |= (wxVSCROLL | wxHSCROLL);
	}
	else {
		cStyle |= wxVSCROLL;
	}
	cTextBox = new wxMultiText(this, NULL, NULL, "",
							0, 0, clientWidth, clientHeight,
							cStyle, "multiTextBox");
	cTextBox->SetJustify(Direction::wxAll);
	
	if (GetParent()->IsHidden())
		DoShow(FALSE);
}

//-----------------------------------------------------------------------------
Bool wxTextWindow::LoadFile(char* file)
{
	struct stat statb;
	Bool result = FALSE;

	if (!file) return FALSE;

	if (file_name) delete[] file_name;

	file_name = macCopyString(file);

	if ((stat(file, &statb) == -1) || ((statb.st_mode & S_IFMT) != S_IFREG))
	{
		return FALSE;
	}
	FILE* fp = fopen(file, "rb"); // WCH: why require "rb" instead of "r" to get newline chars
	if (!fp) return FALSE;

	long len = statb.st_size;
	char* text = new char[len + 1];
	if (text)
	{
		if (fread(text, sizeof(char), len, fp) == len)
		{
			text[len] = 0;
			long lastPosition = GetLastPosition();
			Replace(0, lastPosition, text);
			cTextBox->SetModified(FALSE);
			result = TRUE;
		}
	
		delete [] text;
	}

	fclose(fp);
	return result;
}

//-----------------------------------------------------------------------------
// If file is null, try saved file name first; returns TRUE if succeeds.
//-----------------------------------------------------------------------------
Bool wxTextWindow::SaveFile(char* file)
{
	Bool result = FALSE;

	if (!file) return FALSE;

	if (FILE* fp = fopen(file, "w"))
	{
		char *buf = GetContents();
		char *t = buf;
		char lastc;
		while (*t) {
			lastc = *t;
			if (lastc == '\015') // Replace CR that ToolBox TxEdit uses for line endings
				lastc = '\n';
			fputc(lastc, fp);
			t++;
		}
		if (lastc != '\n')
			fputc('\n', fp);
		delete [] buf;
		fclose(fp);
	}

	return result;
}

//-----------------------------------------------------------------------------
void wxTextWindow::SetEditable(Bool editable)
{
	cTextBox->SetEditable(editable);
}

//-----------------------------------------------------------------------------
void wxTextWindow::Paint(void)
{
	if (cHidden) return;
	SetCurrentDC();
	wxWindow::Paint();
}

//-----------------------------------------------------------------------------
void wxTextWindow::DoShow(Bool show) {
	wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxTextWindow::Enable(Bool Flag) { cTextBox->Enable(Flag); }

//-----------------------------------------------------------------------------
void wxTextWindow::SetFont(wxFont *theFont) // mac platform only
{
	font = theFont;
	cTextBox->SetFont(theFont);
}

//-----------------------------------------------------------------------------
void wxTextWindow::SetFocus(void) { cTextBox->SetFocus(); }

//-----------------------------------------------------------------------------
void wxTextWindow::WriteText(char* text) { cTextBox->WriteText(text); }

//-----------------------------------------------------------------------------
void wxTextWindow::Clear(void) { cTextBox->Clear(); }

//-----------------------------------------------------------------------------
// Not clear whether Clear is required as well as DiscardEdits
//-----------------------------------------------------------------------------
void wxTextWindow::DiscardEdits(void) { cTextBox->DiscardEdits(); }

//-----------------------------------------------------------------------------
char* wxTextWindow::GetContents(void) { return cTextBox->GetContents(); }

//-----------------------------------------------------------------------------
void wxTextWindow::SetInsertionPoint(long pos) { cTextBox->SetInsertionPoint(pos); }

//-----------------------------------------------------------------------------
void wxTextWindow::SetInsertionPointEnd(void) { cTextBox->SetInsertionPointEnd(); }

//-----------------------------------------------------------------------------
long wxTextWindow::GetInsertionPoint(void) { return cTextBox->GetInsertionPoint(); }

//-----------------------------------------------------------------------------
long wxTextWindow::GetLastPosition(void) { return cTextBox->GetLastPosition(); }

//-----------------------------------------------------------------------------
long wxTextWindow::XYToPosition(long x, long y) // x is char pos, y is line number?
{
	return cTextBox->XYToPosition(x, y);
}

//-----------------------------------------------------------------------------
// CJC - pos is probably the character index. Return the line no. (y) and place in
// that line (x). NOTE: The save and restore of the current point may not be
// necessary ??
void wxTextWindow::PositionToXY(long pos, long* x, long* y) // counting from 0
{
	long saveipt = cTextBox->GetInsertionPoint();
	cTextBox->SetInsertionPoint(pos);
	cTextBox->PositionToXY(x, y);
	cTextBox->SetInsertionPoint(saveipt);
}

//-----------------------------------------------------------------------------
void wxTextWindow::ShowPosition(long pos) { cTextBox->ShowPosition(pos); }

//-----------------------------------------------------------------------------
int wxTextWindow::GetLineLength(long lineNo) // lineNo starts at zero
{
	return cTextBox->GetLineLength(lineNo);
}

//-----------------------------------------------------------------------------
int wxTextWindow::GetNumberOfLines(void) { return cTextBox->GetNumberOfLines(); }

//-----------------------------------------------------------------------------
int wxTextWindow::GetLineText(long lineNo, char* buf) // WCH: THIS ALLOCATES buf!!!
{
	return cTextBox->GetLineText(lineNo, buf);
}

//-----------------------------------------------------------------------------
void wxTextWindow::Replace(long from, long to, char* value)
{
	cTextBox->Replace(from, to, value);
}

//-----------------------------------------------------------------------------
void wxTextWindow::Remove(long from, long to) { cTextBox->Remove(from, to); }

//-----------------------------------------------------------------------------
void wxTextWindow::Highlight(long from, long to)
{
	cTextBox->Highlight(from, to);
}

//-----------------------------------------------------------------------------
Bool wxTextWindow::Modified(void) { return cTextBox->Modified(); }

// Cut, copy, paste, etc
 void wxTextWindow::Copy()
 {
   cTextBox->Copy();
 }

 void wxTextWindow::Paste()
 {
  cTextBox->Paste();
 }

 void wxTextWindow::Cut()
 {
  cTextBox->Cut();
 }

 void wxTextWindow::SetSelection(long start, long end)
 {
	cTextBox->SetInsertionPoint(start);
 }

