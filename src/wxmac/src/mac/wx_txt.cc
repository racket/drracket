///////////////////////////////////////////////////////////////////////////////
// File:	wx_txt.cc
// Purpose:	Panel item wxText implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wx_item.h"
#include "wx_txt.h"
#include "wxScroll.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_gdi.h"
#include "wx_stdev.h"
#include "wx_area.h"
#include "wxLabelArea.h"
#include "wxBorderArea.h"
#include "wxMacDC.h"
#include "wxScrollArea.h"
#include "wx_panel.h"
#include "wx_frame.h"
#include "wx_text.h"
#include "wx_timer.h"
#include <QuickDraw.h>
#include <Memory.h>

#define TEXT_MARGIN 2
#define DEFAULT_TEXT_WIDTH 60

extern char *wxBuffer;
//=============================================================================
// Public constructors
//=============================================================================

static wxText *idleTE;

class wxTE_Idler : public wxTimer
{
  void Notify() {
    if (idleTE)
      idleTE->DoPeriodicAction();
  }
};

static wxTE_Idler *TE_idler;

//-----------------------------------------------------------------------------
wxText::wxText // Constructor (given parentPanel)
	(
		wxPanel*		parentPanel,
		wxFunction		function,
		char*			label,
		char*			value,
		int 			x,
		int				y,
		int				width,
		int				height,
		long			style,
		char*			windowName,
		WXTYPE			objectType
	) :
		wxbText (parentPanel, x, y, width, height, style, windowName),
		cTextEditable (TRUE),
		cTextModified (FALSE)
{
	Callback(function);

	CreateWxText(label, value);
}

//-----------------------------------------------------------------------------
wxText::wxText // Constructor (given parentTextWindow)
	(
		wxTextWindow*	parentTextWindow,
		wxFunction		function,
		char*			label,
		char*			value,
		int 			x,
		int				y,
		int				width,
		int				height,
		long			style,
		char*			windowName,
		WXTYPE			objectType
	) :
		wxbText (parentTextWindow, x, y, width, height, style, windowName),
		cTextEditable (TRUE),
		cTextModified (FALSE)

{
	Callback(function);

	CreateWxText(label, value);
}
//==============
// A dummy to satisfy Linkers
//
wxText::wxText()
{
}
//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxText::~wxText(void)
{
 	DestroyFocus(); // tom: just to be sure
	if (cMacTE) 
	  ::TEDispose(cMacTE);
		
	if (TE_idler && (idleTE == this)) {
	  idleTE == NULL;
	  TE_idler->Stop();
	}
}

//=============================================================================
// Private methods
//=============================================================================

void wxText::CreateWxText(char* label, char* value) // common constructor initialization
{
	SetEraser(NULL);

	if (label)
  	  label = wxItemStripLabel(label);

//////////////////////////////////////////
// do platform stuff:
//////////////////////////////////////////
	if (!buttonFont)
	  buttonFont = wxNORMAL_FONT;
	if (!labelFont)
	  labelFont = wxNORMAL_FONT;

	SetCurrentMacDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();

	float tWidth, tHeight;
	float lWidth, lHeight;
	GetTextExtent("X", &tWidth, &tHeight, NULL, NULL, buttonFont);
	GetTextExtent(label ? label : "X", &lWidth, &lHeight, NULL, NULL, labelFont);

	int clientHeight = ClientArea()->Height();
	if (clientHeight <= 0) {
		int lblh = (label && (labelPosition == wxVERTICAL) ? (int)lHeight : 0);
		clientHeight = cWindowHeight = (int)tHeight + 2 * TEXT_MARGIN + 2; // +2 for border
		cWindowHeight = clientHeight + lblh;
	}
	int clientWidth = ClientArea()->Width();
	if (clientWidth <= 0) {
		int lblw = (label && (labelPosition != wxVERTICAL) ? (int)lWidth : 0);
		clientWidth = DEFAULT_TEXT_WIDTH + (2 * TEXT_MARGIN) + 2; // +2 for border
		cWindowWidth = clientWidth + lblw;
	}
	Rect destRect = {TEXT_MARGIN, TEXT_MARGIN, 
					 clientHeight - TEXT_MARGIN, clientWidth - TEXT_MARGIN};
	cMacTE = ::TENew(&destRect, &destRect);
	CheckMemOK(cMacTE);

	SetFont(buttonFont);
    HLock((Handle)cMacTE);
 	(*cMacTE)->crOnly = -1;
 	HUnlock((Handle)cMacTE);
 	//  tom (fettig@dfki.uni-sb.de)	 
 	//  True single line Mode!
 	::TEAutoView(TRUE,cMacTE);

//////////////////////////////////////////
// do more wxWindow stuff:
//////////////////////////////////////////
	if (cStyle & wxVSCROLL || cStyle & wxHSCROLL)
	{
		wxScrollData* scrollData = new wxScrollData;
#if 0 // mflatt: Hello didn't run with "1"
		new wxScrollArea(this, this, (cStyle & wxVSCROLL) | (cStyle & wxHSCROLL));
#else
		cScroll = new wxScroll(this, scrollData);
		if (cStyle & wxVSCROLL) new wxScrollArea(this, this, wxVSCROLL);
		if (cStyle & wxHSCROLL) new wxScrollArea(this, this, wxHSCROLL);
#endif
	}

	// mflatt: text should always have a border
#if 0
	if (cStyle & wxBORDER) 
#endif
		new wxBorderArea(this);

	if (label)
	{
		cLabelArea = new wxLabelArea(this, label, labelFont,
				labelPosition == wxVERTICAL ? Direction::wxTop : Direction::wxLeft,
				0,
				labelPosition == wxVERTICAL ? 0 : 1 + TEXT_MARGIN);
	}
	else cLabelArea = NULL;

	
	if (value) 
	{
		SetValue(value);
		cTextModified = FALSE; // kludge
	}
	
		
	if (GetParent()->IsHidden())
		DoShow(FALSE);
}


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxText::WriteText(char* text)
{
	SetCurrentDC();
	long endPosition = (**cMacTE).teLength;
	::TESetSelect(endPosition, endPosition, cMacTE);
	::TEInsert(text, strlen(text), cMacTE);
	macAdjustScrollBars();
	cTextModified = TRUE;
}

//-----------------------------------------------------------------------------
void wxText::Clear(void)
{
	SetCurrentDC();
	long endPosition = (**cMacTE).teLength;
	::TESetSelect(0, endPosition, cMacTE);
	::TEDelete(cMacTE);
	macAdjustScrollBars();
	cTextModified = TRUE;
}

//-----------------------------------------------------------------------------
Bool wxText::Modified(void) { return cTextModified; }

//-----------------------------------------------------------------------------
void wxText::SetModified(Bool modified) // mac platform only
{
	cTextModified = modified;
}

//-----------------------------------------------------------------------------
// Not clear whether Clear is required as well as DiscardEdits
//-----------------------------------------------------------------------------
void wxText::DiscardEdits(void)
{
	SetCurrentDC();
	long endPosition = (**cMacTE).teLength;
	::TESetSelect(0, endPosition, cMacTE);
	::TEDelete(cMacTE);
	macAdjustScrollBars();
	cTextModified = TRUE;
}

//-----------------------------------------------------------------------------
void wxText::SetEditable(Bool editable) { cTextEditable = editable; }

//-----------------------------------------------------------------------------
char* wxText::GetContents(void)
{
	CharsHandle theMacText = ::TEGetText(cMacTE);
	int theMacTextLength = (**cMacTE).teLength;
	char* result = new char[theMacTextLength + 1];
	BlockMove(*theMacText, result, theMacTextLength);
	result[theMacTextLength] = 0; // terminate "C" string
	return result;
}

//-----------------------------------------------------------------------------
void wxText::SetInsertionPoint(long pos)
{
	SetCurrentDC();
	::TESetSelect(pos, pos, cMacTE);
}

//-----------------------------------------------------------------------------
void wxText::SetInsertionPointEnd(void)
{
	SetCurrentDC();
	long endPosition = (**cMacTE).teLength;
	::TESetSelect(endPosition, endPosition, cMacTE);
}

//-----------------------------------------------------------------------------
long wxText::GetInsertionPoint(void) { return (**cMacTE).selStart; }

//-----------------------------------------------------------------------------
long wxText::GetLastPosition(void) { return (**cMacTE).teLength; }

//-----------------------------------------------------------------------------
long wxText::XYToPosition(long x, long y) // x is char pos, y is line number?
{
	long numberOfLines = GetNumberOfLines();

	if (y >= numberOfLines || y < 0) return 0;

	long startPos = (**cMacTE).lineStarts[y];

	return startPos + x;
}

//-----------------------------------------------------------------------------
void wxText::PositionToXY(long* x, long* y) // counting from 0
{
	long thePosition = GetInsertionPoint();
	long numberOfLines = GetNumberOfLines();

	long theY = -1;
	long lineAt = numberOfLines - 1; // counting from 0
	while (lineAt >= 0 && theY == -1) // count down to 0
	{
		if (thePosition < (**cMacTE).lineStarts[lineAt])
			lineAt--; // count down to 0
		else theY = lineAt;
	}

	if (theY != -1)
	{
		*x = thePosition - (**cMacTE).lineStarts[theY]; // counting from 0
		*y = theY; // counting from 0
	}
	else
	{
		*x = 0;
		*y = 0;
	}
}

//-----------------------------------------------------------------------------
void wxText::ShowPosition(long pos)
{
#ifdef wx_motif
  Widget textWidget = (Widget)handle;
  XmTextShowPosition(textWidget, (XmTextPosition)pos);
#endif
#ifdef wx_xview
  Textsw textsw = (Textsw)handle;
  
  xv_set(textsw, TEXTSW_FIRST, (Textsw_index)pos, NULL);
#endif
}

//-----------------------------------------------------------------------------
int wxText::GetLineLength(long lineNo) // lineNo starts at zero
{
	long numberOfLines = GetNumberOfLines();

	if (lineNo >= numberOfLines || lineNo < 0) return 0;

	long startPos = (**cMacTE).lineStarts[lineNo];

	long nextLineNo = lineNo + 1;
	long nextStartPos;
	if (nextLineNo < numberOfLines)
		 nextStartPos = (**cMacTE).lineStarts[nextLineNo];
	else nextStartPos = (**cMacTE).teLength;

	return nextStartPos - startPos;
}

//-----------------------------------------------------------------------------
int wxText::GetNumberOfLines(void) { return (**cMacTE).nLines; }

//-----------------------------------------------------------------------------
int wxText::GetLineText(long lineNo, char* buf) // WCH: THIS ALLOCATES buf!!!
{
	int length; // lenght of text returned in buf
	long numberOfLines = GetNumberOfLines();

	if (lineNo >= numberOfLines || lineNo < 0)
	{
		length = 0;
	}
	else
	{
		long startPos = (**cMacTE).lineStarts[lineNo];
	
		long nextLineNo = lineNo + 1;
		long nextStartPos;
		if (nextLineNo < numberOfLines)
			 nextStartPos = (**cMacTE).lineStarts[nextLineNo];
		else nextStartPos = (**cMacTE).teLength;
		length = nextStartPos - startPos;
	}

	buf = new char[length + 1]; // WCH: THIS ALLOCATES buf!!!
	CharsHandle theMacText = ::TEGetText(cMacTE);
	BlockMove(*theMacText, buf, length); // BlockMove allows args to overlap
	buf[length] = 0; // terminate "C" string

	return length;
}

//-----------------------------------------------------------------------------
void wxText::Replace(long from, long to, char* value)
{
	SetCurrentDC();
	::TESetSelect(from, to, cMacTE);
	::TEDelete(cMacTE); // doesn't copy selected text to scrap
	::TEInsert(value, strlen(value), cMacTE);
	macAdjustScrollBars();
	cTextModified = TRUE;
}

//-----------------------------------------------------------------------------
void wxText::Remove(long from, long to)
{
	SetCurrentDC();
	::TESetSelect(from, to, cMacTE);
	::TECut(cMacTE); // does copy selected text to scrap
	macAdjustScrollBars();
	cTextModified = TRUE;
}

//-----------------------------------------------------------------------------
void wxText::Highlight(long from, long to)
{
	SetCurrentDC();
	::TESetSelect(from, to, cMacTE);
}

//-----------------------------------------------------------------------------
void wxText::Paint(void)
{
	if (cHidden) return;

	SetCurrentDC();
	Rect viewRect = (**cMacTE).viewRect;
	Rect whiteRect = viewRect;
	InsetRect(&whiteRect, -TEXT_MARGIN, -TEXT_MARGIN);
	::EraseRect(&whiteRect);
	::TEUpdate(&viewRect, cMacTE);

	wxWindow::Paint();
}

//-----------------------------------------------------------------------------
void wxText::DoShow(Bool show) 
{
  wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxText::DoPeriodicAction(void) // mac platform only
{
	SetCurrentDC();
	::TEIdle(cMacTE);
}

//-----------------------------------------------------------------------------
void wxText::OnChar(wxKeyEvent& event) // mac platform only
{
	long st,se;
  
	HLock((Handle)cMacTE);
	st = (*cMacTE)->selStart;
	se = (*cMacTE)->selEnd;	
	if (cEnable)
	{
 		// GRW:
 		// wxText is really meant to be a single line input, which means that it can be used
 		// in dialogs and still have enter/return trigger the dialog's default action.  Since
 		// wxMultiText and wxTextWindow count on wxText supporting multiple lines, however,
 		// an expedient hack was to preserve this behavior when wxMULTIPLE is specified.
 		// Note that I haven't done all of the grunge to make wxText properly behave as a
 		// single line input (lines still wrap).

 		// lets implement the single line mode - tom (fettig@dfki.uni-sb.de)
		char code = event.keyCode;

		switch (event.keyCode)
		{
			case 0x03:	// enter
			case 0x0D:	// return
			case 0x09:	// tab
				if (cStyle & wxPROCESS_ENTER) {
					wxCommandEvent *command = new wxCommandEvent(wxEVENT_TYPE_TEXT_ENTER_COMMAND);
					command->eventObject = this;
					ProcessCommand(*command);
		 		} else if (!(cStyle & wxMULTIPLE)) {
				  wxItem::OnChar(event);
				  return;
				}
				break;
#if 0 // tom replaces:
			case WXK_UP:
				code = 30;
				break;					
			case WXK_DOWN:
				code = 31;
				break;					
			case WXK_LEFT:
				code = 28;
				break;					
			case WXK_RIGHT:
				code = 29;
				break;					
			default:
				break;
		}

		SetCurrentDC();
		::TEKey(code, cMacTE);
		macAdjustScrollBars();
		cTextModified = TRUE;
	}
#else
 			case WXK_UP:			
  			case WXK_DOWN:
 				break;	
  			case WXK_LEFT:
 	 	 	 if (st>0) {
 		  	    st--;
 		  	    TESetSelect(st,st,cMacTE);
 		  	  } 
 		  	  if ( (**cMacTE).selRect.right == 1) {
 	  			if (st!=0) {
 	   				TEScroll(10,0,cMacTE);
 	  			}
 			  }
 	 	 	break;
 	 	 	case WXK_RIGHT:
 	 	 	if (se<(*cMacTE)->teLength) {
 		  	    se++;
 		  	    TESetSelect(se,se,cMacTE);
 		  	    TESelView(cMacTE);
 		  	  }
 	 	 	break;
 	 	 	case WXK_HOME:
 	 	 	  st = 0;
 	 	 	  TESetSelect(st,st,cMacTE);
 	 	 	break;
 	 	 	case WXK_END:
 	 	 	  se = (*cMacTE)->teLength;
 	 	 	  TESetSelect(se,se,cMacTE);
 	 	 	break;
 	 	 	case WXK_DELETE:
 	 	      if (se==st) {
 	 	        if (se < (*cMacTE)->teLength) {
 	 	          se++;
	 	          TESetSelect(st,se,cMacTE);
 	 	        }
 	 	      }
 	 	    TEDelete(cMacTE);
 	 	 	break;
 	 	 	case WXK_PAGEUP:
 	 	 	break;
 	 	 	case WXK_PAGEDOWN:
 	 	 	break;		
  			default:
 			  SetCurrentDC(); // tom: I am not sure about this call...
 			  char theChar = event.keyCode;
 			  ::TEKey(theChar, cMacTE);
 			  //macAdjustScrollBars();
 			  cTextModified = TRUE;
 			break;
  		}
  		macAdjustScrollBars();
  	}
   if ( ((**cMacTE).viewRect.right-1) <= (**cMacTE).selRect.right) {
     TEScroll(-10,0,cMacTE);
   }
   HUnlock((Handle)cMacTE);
#endif
}

//-----------------------------------------------------------------------------
void wxText::OnEvent(wxMouseEvent& event) // WCH: mac only ?
{
	if (cEnable)
	{
		if (event.LeftDown())
		{
			// SetFocus(); // mflatt: no longer needed
	
			float fStartH, fStartV;
			event.Position(&fStartH, &fStartV); // client c.s.
			int startH = fStartH;
			int startV = fStartV;
	
			SetCurrentDC();
			Point startPt = {startV, startH}; // client c.s.
			Bool extendSelection = event.shiftDown;
			::TEClick(startPt, extendSelection, cMacTE);
		}
	}
}

//-----------------------------------------------------------------------------
void wxText::SetTextInfo(void) // mac platform only
{
	int oldTxFont = (**cMacTE).txFont;
	int oldTxSize = (**cMacTE).txSize;
	Style oldTxFace = (**cMacTE).txFace;
	int newTxFont;
	int newTxSize;
	Style newTxFace;

	if (!font)
	{
		newTxFont = 1;
		newTxSize = 12;
		newTxFace = 0;
	}
	else
	{
		newTxFont = font->GetMacFontNum();
		newTxSize = font->GetPointSize();
		newTxFace = font->GetMacFontStyle();
	}

	(**cMacTE).txFont = newTxFont;
	(**cMacTE).txSize = newTxSize;
	(**cMacTE).txFace = newTxFace;

	if (newTxFont != oldTxFont || newTxSize != oldTxSize || newTxFace != oldTxFace)
	{
		::TECalText(cMacTE);
		Rect theViewRect = (**cMacTE).viewRect;
		::InvalRect(&theViewRect);
		::EraseRect(&theViewRect);
	}
}

//-----------------------------------------------------------------------------
void wxText::SetFont(wxFont* theFont) // mac platform only
{
	SetCurrentMacDC();
	
	font = theFont;

	int oldTxFont = (**cMacTE).txFont;
	int oldTxSize = (**cMacTE).txSize;
	Style oldTxFace = (**cMacTE).txFace;
	int newTxFont;
	int newTxSize;
	Style newTxFace;
	int newLineHeight; 											// WCH: 94/12/26
	int newFontAscent; 											// WCH: 94/12/26

	if (!font)
	{
		newTxFont = 1;
		newTxSize = 12;
		newTxFace = 0;
		newLineHeight = 16; 									// WCH: 94/12/26
		newFontAscent = 12; 									// WCH: 94/12/26
	}
	else
	{
		newTxFont = font->GetMacFontNum();
		newTxSize = font->GetPointSize();
		newTxFace = font->GetMacFontStyle();

		float theWidth; 										// WCH: 94/12/26
		float theLineHeight; 									// WCH: 94/12/26
		float theDescent; 										// WCH: 94/12/26
		float theExternalLeading; 								// WCH: 94/12/26
		font->GetTextExtent("W", &theWidth, &theLineHeight,
							&theDescent, &theExternalLeading); 	// WCH: 94/12/26
		newLineHeight = theLineHeight; 							// WCH: 94/12/26
		newFontAscent = theLineHeight - theDescent - theExternalLeading; // WCH: 94/12/26
	}

	(**cMacTE).txFont = newTxFont;
	(**cMacTE).txSize = newTxSize;
	(**cMacTE).txFace = newTxFace;

	(**cMacTE).lineHeight = newLineHeight; 						// WCH: 94/12/26
	(**cMacTE).fontAscent = newFontAscent; 						// WCH: 94/12/26

	if (newTxFont != oldTxFont || newTxSize != oldTxSize || newTxFace != oldTxFace)
	{
		::TECalText(cMacTE);
		Rect theViewRect = (**cMacTE).viewRect;
		::InvalRect(&theViewRect);
		::EraseRect(&theViewRect);
	}
}

//-----------------------------------------------------------------------------
void wxText::SetLabel(char* label)
{
	if (cLabelArea) cLabelArea->SetLabel(label);
}

//-----------------------------------------------------------------------------
char* wxText::GetLabel(void)
{
	return (cLabelArea ? cLabelArea->GetLabel() : NULL);
}

//-----------------------------------------------------------------------------
char* wxText::GetValue(void)
{
	CharsHandle theMacText = ::TEGetText(cMacTE);
	int theMacTextLength = (**cMacTE).teLength;
#if 1 // CJC
	char *result = wxBuffer;
#else
	char* result = new char[theMacTextLength + 1];
#endif
	BlockMove(*theMacText, result, theMacTextLength);
	result[theMacTextLength] = 0; // terminate "C" string
	return result;
}

//-----------------------------------------------------------------------------
void wxText::SetValue(char* value)
{
	SetCurrentDC();
	long endPosition = (**cMacTE).teLength;
	::TESetSelect(0, endPosition, cMacTE);
	::TEDelete(cMacTE); // doesn't copy selected text to scrap
	::TEInsert(value, strlen(value), cMacTE);
	macAdjustScrollBars();
	cTextModified = TRUE;
}

//-----------------------------------------------------------------------------
//=============================================================================
void wxText::ChangeColour(void)
{
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Activate methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxText::OnSetFocus()
{
	if (cHidden) return;

	SetCurrentDC();
	// The following is a kludge, to prevent flicker during subsequent update event
	Rect viewRect = (**cMacTE).viewRect;
	::TEUpdate(&viewRect, cMacTE);
	::TEActivate(cMacTE); // This is the "ShowAsActive" part
	::ValidRect(&viewRect);
	
	if (!idleTE) {
	  if (!TE_idler)
	    TE_idler = new wxTE_Idler;
	  TE_idler->Start();
	}
	idleTE = this;
}

//-----------------------------------------------------------------------------
void wxText::OnKillFocus()
{
	SetCurrentDC();
	::TEDeactivate(cMacTE);
	
	if (TE_idler && (idleTE == this)) {
	  idleTE == NULL;
	  TE_idler->Stop();
	}
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Scroll methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxText::macAdjustScrollBars(void) // mac platform only
{
	if (cScroll)
	{
		wxWhatScrollData whatScrollData; // track what scrolldata changes
		wxScrollData* oldScrollData = cScroll->GetScrollData();
		wxScrollData scrollData;
		if (oldScrollData) scrollData = *oldScrollData;

		int maxTop = GetNumberOfLines();
		if (maxTop != scrollData.GetValue(wxWhatScrollData::wxSizeH))
		{
			scrollData.SetValue(maxTop, wxWhatScrollData::wxSizeH);
			whatScrollData |= wxWhatScrollData::wxSizeH;
		}

		int units_per_scroll_x = 1;
		if (units_per_scroll_x != scrollData.GetValue(wxWhatScrollData::wxUnitW))
		{
			scrollData.SetValue(units_per_scroll_x, wxWhatScrollData::wxUnitW);
			whatScrollData |= wxWhatScrollData::wxUnitW;
		}

		int scrolls_per_page_x = 1;
		if (scrolls_per_page_x != scrollData.GetValue(wxWhatScrollData::wxPageW))
		{
			scrollData.SetValue(scrolls_per_page_x, wxWhatScrollData::wxPageW);
			whatScrollData |= wxWhatScrollData::wxPageW;
		}

		int charHeight = GetCharHeight();
		int units_per_scroll_y = (charHeight > 0 ? charHeight : 1);
		if (units_per_scroll_y != scrollData.GetValue(wxWhatScrollData::wxUnitH))
		{
			scrollData.SetValue(units_per_scroll_y, wxWhatScrollData::wxUnitH);
			whatScrollData |= wxWhatScrollData::wxUnitH;
		}

		int clientHeight = ClientArea()->Height();
		int scrolls_per_page_y = (charHeight > 0 ? clientHeight / charHeight : 1);
		if (scrolls_per_page_y != scrollData.GetValue(wxWhatScrollData::wxPageH))
		{
			scrollData.SetValue(scrolls_per_page_y, wxWhatScrollData::wxPageH);
			whatScrollData |= wxWhatScrollData::wxPageH;
		}

		if ((long)whatScrollData != 0)
			cScroll->SetScrollData(&scrollData, whatScrollData, NULL);
	}
}

//-----------------------------------------------------------------------------
void wxText::SetScrollData // adjust text within window to match scroll bar setting
(
	wxScrollData*		newScrollData,
	wxWhatScrollData	whatScrollData,
	wxWindow*			iniatorWindow
)
{
	if (iniatorWindow == this) return;

	Bool isNewPositionH = (long)whatScrollData & wxWhatScrollData::wxPositionH;
	Bool isNewPositionV = (long)whatScrollData & wxWhatScrollData::wxPositionV;

	if (isNewPositionH || isNewPositionV)
	{
		SetCurrentDC();
		Rect viewRect = (**cMacTE).viewRect;
		Rect destRect = (**cMacTE).destRect;
		int oldH = viewRect.left - destRect.left;
		int oldV = viewRect.top - destRect.top;

		int dH = 0;
		if (isNewPositionH)
		{
			int newH = newScrollData->GetValue(wxWhatScrollData::wxPositionH) *
						newScrollData->GetValue(wxWhatScrollData::wxUnitW);
			dH = newH - oldH;
		}

		int dV = 0;
		if (isNewPositionV)
		{
			int newV = newScrollData->GetValue(wxWhatScrollData::wxPositionV) *
						newScrollData->GetValue(wxWhatScrollData::wxUnitH);
			dV = newV - oldV;
		}

		::TEScroll(-dH, -dV, cMacTE);
	}
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Sizing methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxText::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
	SetCurrentDC();

	int clientWidth = ClientArea()->Width();
	int clientHeight= ClientArea()->Height();

	Rect newDestRect = {TEXT_MARGIN, TEXT_MARGIN, 
						clientHeight - TEXT_MARGIN, clientWidth - TEXT_MARGIN};
	(**cMacTE).viewRect = newDestRect;
	(**cMacTE).destRect = newDestRect;
	 // must let platform recalculate any line changes:
	if (dW || dH)
	{
		::TECalText(cMacTE);
	}

	if (dX || dY)
	{
		cMacDC->setCurrentUser(NULL); // macDC no longer valid
		SetCurrentDC(); // put newViewRect at (0, 0)
	}

	::InvalRect(&newDestRect); // force redraw of text window
	::EraseRect(&newDestRect); 
}

// Cut, copy, paste
 void wxText::Copy()
 {
   ::TECopy(cMacTE);
 }

 void wxText::Paste()
 {
   ::TEPaste(cMacTE);
 }

 void wxText::Cut()
 {
   ::TECut(cMacTE);
 }

Bool wxText::WantsFocus()
{
  return TRUE;
}

void wxText::ChangeToGray(Bool gray)
{	
	ChildrenInternalGray(gray);
	wxWindow::ChangeToGray(gray);
}
