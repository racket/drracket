/*
 * File:	wx_lbox.cc
 * Purpose:	Panel item listBox implementation (Macintosh version)
 * Author:	Cecil Coupe (Jul 8, 1995)
 * Created:	1994
 * Updated:	
 *		11/1/95 - not deleting client data on delete or clear
 * Copyright:	(c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
 */

static const char sccsid[] = "%W% %G%";


#include "common.h"
#include "wx_lbox.h"
#include "wx_utils.h"
#include "wx_rbox.h"
#include "wx_rbut.h"
#include "wx_messg.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_gdi.h"
#include "wx_area.h"
#include "wxBorderArea.h"
#include "wxRectBorder.h"
#include <Lists.h>
#include "wxTimeScale.h"

#define MEANING_CHARACTER	'0'
/* 
   A wxWindows (Macintosh version) wxListBox has these (sub)components:
	1.  a Title
	2.  a Mac List Manager "control"
	3.  Optional Scrollbars (vertical only)
	4.  BorderBox drawn round the list control but not the title or scrollbar

  These are grouped/managed/defined by:
	Creating the Mac list box control in the ClientArea
	Creating a wxBorderArea (The Toolbox list mgr does not draw borders around
		 the control)
	Creating the wxLabelArea for the title (aka label);

  wxItem protocol needed for showing the wxListBox on a panel:
	1. A constructor of course thats makes a stab at sizing the Mac list box
		and groups the items as described above.
	2  A wxListBox::Paint method that calls LUpdate on the Mac list box
	   and calls OnPaint() - to draw the wxLabelArea and wxBorderArea.
	3. wxListBox::OnClientAreaDSize is needed to actually resize or reposition
		the Mac List Box (after the label and bounding box are created).

   The Mac list manager requires certain Mac events:
	1. Update - call LUpdate which handles the scroll bar and the list control
	2. MouseDown (but not all mouse events, trust me)
	3. Activate ???

   mflatt: Anytime the list might redraw itself, call SetCurrentDC() first; this
        gets the GrafPtr into the proper coordinate system.

   List Manager hints:
	1. Note that SetRect(...) and Rect x = {...} args are in different order
	   This causes me all kinds of errors and the List Mgr often doesn't
	   display anything if the BoundsRect is wrong!

	To do:
	1. Compute a cell height based on the font used (currently hardcoded at 12)
	   Also, the font used is different from wxText and RadioButtons.
	2. Load the list in the constructor if N is non zero
	3. This could be leaking memory ?
*/

// FIXME: Catch SetFont and fixup cell size and indent

#define DefItemWidth 50
#define KSBWidth	15	// number of pixels of a vertical scroll bars' width

// Constructor

wxListBox::wxListBox(
		wxPanel *parentPanel, 
		wxFunction func,
		char *Title, 
		Bool Multiple,
		int x, 
		int y, 
		int width, 
		int height,
		int N, 
		char **Choices, 
		long style, 
		char *windowName,
		WXTYPE		objectType
	) :
		wxbListBox (parentPanel, x, y, width, height, N, style, windowName),
 		cDataList (new wxList(wxKEY_INTEGER))

{
  Create(parentPanel, func, Title, Multiple, x, y, width, height, N, Choices,
         style, windowName);
}

Bool wxListBox::Create(wxPanel *panel, wxFunction func,
                       char *Title, Bool Multiple,
                       int x, int y, int width, int height,
                       int N, char **Choices, long style, char *name)
{
	SetEraser(NULL);

	labelFont = panel->labelFont ;
	backColour = panel->backColour ;
	labelColour = panel->labelColour ;
	buttonFont = panel->buttonFont ;
	buttonColour = panel->buttonColour ;

	selected = -1;
	selections = 0;
	multiple = Multiple & wxMULTIPLE_MASK;
	no_items = 0;
	cKeycnt =0;

	if (!buttonFont)
	  buttonFont = wxNORMAL_FONT;

	font = buttonFont;
	
	if (Title)
	  Title = wxItemStripLabel(Title);
	
	Callback(func);
	SetCurrentMacDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();

	float lblWidth, lblHeight;
	GetTextExtent(Title, &lblWidth, &lblHeight, NULL, NULL, labelFont);
	float tWidth, tHeight, tDescent;
	GetTextExtent("X", &tWidth, &tHeight, &tDescent, NULL, font);
	
	if (width < 0) {
	  cWindowWidth = (labelPosition == wxVERTICAL ? 0 : lblWidth) + DefItemWidth + KSBWidth;
	}
	if (height < 0) {
	  cWindowHeight = (labelPosition == wxVERTICAL ? lblHeight : 0) + 3 * tHeight;
	}
	
	int boxHeight = cWindowHeight;
	int boxWidth = cWindowWidth;
	// mflatt: wxNEEDED_SB = 0; if it's on, this code doesn't notice:
	// int vscrollwanted = (style & (wxNEEDED_SB | wxALWAYS_SB)) || 
	//	(Multiple & (wxNEEDED_SB | wxALWAYS_SB));
	// Since the choice is either NEEDED or ALWAYS, presumably we always want it on:
	int vscrollwanted = TRUE;
	Rect dataRect = {0, 0, 0 ,1 }; 			
	// start with no rows or columns, thats what the '1' {means in B, R}
	int	cellwid = boxWidth - (vscrollwanted ? KSBWidth : 0);
	Point cellsize = {(int)tHeight, cellwid};
	Rect viewRect = {0, 0, boxHeight, cellwid };
	cHaveVScroll = vscrollwanted;			// needed by OnClientAreaDSize or Paint

	cListHandle = ::LNew(&viewRect, &dataRect, cellsize, 0, (WindowPtr)theMacGrafPort,
		TRUE, FALSE, FALSE, vscrollwanted ? TRUE : FALSE);
	CheckMemOK(cListHandle);
	
	(**cListHandle).indent.v = tHeight - tDescent;

	// by default the Mac allows fancy selections
	if (multiple & wxMULTIPLE) {
#if 0
		(**cListHandle).selFlags = lExtendDrag | lNoDisjoint | lNoExtend | lNoRect 
			| lUseSense;
#endif
	}
	else {
		(**cListHandle).selFlags |= lOnlyOne;		// ell not one
	}
	cThinBorderArea = new wxBorderArea(this);		// Fits around the Mac listbox 'control'.
	if (Title)
	{
		cListTitle = new wxLabelArea(this, Title, labelFont,
				labelPosition == wxVERTICAL ? Direction::wxTop : Direction::wxLeft);
	}
	else
		cListTitle = NULL;

	cBorderArea = new wxBorderArea(this, 0, Direction::wxAll, 1); // mflatt: for showing keyboard focus
	wxMargin margin(3);
	cBorderArea->SetMargin(margin, Direction::wxAll,
						cWindowWidth + 6, cWindowHeight + 6,
						cWindowX - 3, cWindowY - 3);
	((wxBorderArea *)cBorderArea)->cBorder->SetBrush(wxCONTROL_BACKGROUND_BRUSH);
						
	if (N)
		this->Set(N, Choices);
		
	if (GetParent()->IsHidden())
		DoShow(FALSE);
		
	return TRUE;
}

wxListBox::~wxListBox(void)
{
  if (selections)
    delete[] selections;
  // if (cListTitle)		// deleting areas takes special care
  //	delete cListTitle;
  if (cDataList)
	delete cDataList;
  ::LDispose(cListHandle);
}

//------------ Event Handling --------------------------------------
void wxListBox::Paint(void)
{
	if (cHidden) return;

	SetCurrentDC();
	WindowPtr theMacWindow = (WindowPtr)cMacDC->macGrafPort();
	::LUpdate(theMacWindow->visRgn, cListHandle);
	
	/* White out any empty space in the list: */
	Point last, dlast;
	Rect lastR, allR;
	last.v = (**cListHandle).visible.bottom - 1;
	last.h = (**cListHandle).visible.right - 1;
	dlast.v = (**cListHandle).dataBounds.bottom - 1;
	dlast.h = (**cListHandle).dataBounds.right - 1;
	if (dlast.v < last.v)
		last.v = dlast.v;
	if (dlast.h < last.h)
		last.h = dlast.h;
	LRect(&lastR, last, cListHandle);
	allR = (**cListHandle).rView;
	if (allR.bottom > lastR.bottom) {
		allR.top = lastR.bottom;
		::EraseRect(&allR);
	}
	if (allR.right > lastR.right) {
		allR.top = (**cListHandle).rView.top;
		allR.left = lastR.right;
		::EraseRect(&allR);
	}
	
	wxWindow::Paint();
}

void wxListBox::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
	SetCurrentDC();

	int clientWidth = ClientArea()->Width();
	int clientHeight= ClientArea()->Height();
	if (cHaveVScroll) clientWidth -= KSBWidth;
	if (dW || dH)
	{	// Changing the size
		::LSize(clientWidth, clientHeight, cListHandle);
		Point size = { (**cListHandle).cellSize.v, clientWidth };
		::LCellSize(size, cListHandle);
		
	}

	if (dX || dY)
	{	// Changing the position
		cMacDC->setCurrentUser(NULL); // macDC no longer valid
		SetCurrentDC(); // put newViewRect at (0, 0)
	}
	
	if (!cHidden && (dW || dH || dX || dY))
	{
		Rect clientRect = {0, 0, clientHeight, clientWidth};
		::InvalRect(&clientRect);
	}
	
	wxWindow::OnClientAreaDSize(dW, dH, dX, dY);
}

// ---- everything above this line is needed for visual respresentation of the wxListBox

/* Manual Scrolling Implementation - mflatt
  
  I have no idea why, but lists sometimes get confused and LClick doesn't
  dispatch to the scrollbar. This manual implementation for vertical scrollbars
  always works, though.

*/

static ListHandle trackList;

static pascal void TrackActionProc(ControlHandle theControl, short part)
{
   int delta, scrollsPerPage;

    scrollsPerPage = (((**trackList).rView.bottom - (**trackList).rView.top)
                      / (**trackList).cellSize.v);

	switch (part) {
	  case inUpButton: delta = -1; break;
	  case inDownButton: delta = 1; break;
	  case inPageUp: delta = -scrollsPerPage; break;
	  case inPageDown: delta = scrollsPerPage; break;
	}
	
	::LScroll(0, delta, trackList);
}

static ControlActionUPP
TrackActionProcUPP = NewControlActionProc(TrackActionProc);

static void ManualScroll(ListHandle list, ControlHandle scroll, Point startPt, int part)
{
	if (part == inThumb) {
	  int oldPos = ::GetCtlValue(scroll);
	  if (::TrackControl(scroll, startPt, NULL)) {
         int newPos = ::GetCtlValue(scroll);
         ::LScroll(0, newPos - oldPos, list);
	  }
	} else {
	  trackList = list;
	  ::TrackControl(scroll, startPt, TrackActionProcUPP);
	}
}
		
void wxListBox::OnEvent(wxMouseEvent& event) // WCH : mac only ?
{
	SetCurrentDC();
	if (event.leftDown || event.rightDown) {
		float fStartH, fStartV;
		event.Position(&fStartH, &fStartV); // client c.s.
		int startH = fStartH;
		int startV = fStartV;
		
		Point startPt = {startV, startH}; 	// client c.s.
		int modifiers = 0;
		if (event.shiftDown)
			modifiers += shiftKey;
		if (event.altDown)
			modifiers += optionKey;
		if (event.rightDown) // mflatt: right button is Cmd-click
			modifiers += cmdKey;
		if (event.controlDown)
			modifiers += controlKey;

		if ((**cListHandle).vScroll) {
		  int thePart = ::TestControl((**cListHandle).vScroll, startPt);
		  if (thePart) {
		    ManualScroll(cListHandle, (**cListHandle).vScroll, startPt, thePart);
		    return;
		  }
		}

		::LClick(startPt, modifiers, cListHandle);
		if (PtInRect(startPt, &(**cListHandle).rView) == FALSE)
			return;							// ie in the scroll bars
		Cell cell = LLastClick(cListHandle);

		if (event.ButtonDown()) {
		  if ((cell.h == cLastClickCell.h)
			  && (cell.v == cLastClickCell.v)
			  && (event.timeStamp - cLastClickTime < SCALE_TIMESTAMP(GetDblTime()))) {
			// Double-click
			wxPanel *panel = (wxPanel *)GetParent();
			panel->OnDefaultAction(this);
			return;
		  }
		  cLastClickTime = event.timeStamp;
		  cLastClickCell.h = cell.h;
		  cLastClickCell.v = cell.v;
		}
		
		{
			int which = ((cell.v < no_items) ? cell.v : -1);
			wxCommandEvent *commandEvent = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_COMMAND);
			commandEvent->commandString = GetString(which);
			commandEvent->commandInt = which;
			commandEvent->eventObject = this;
	  		ProcessCommand(*commandEvent);
		}
	}
}

// mflatt:
void wxListBox::OnChar(wxKeyEvent& event)
{
	int move = 0;

	SetCurrentDC();

	switch (event.KeyCode()) {
		case WXK_UP:
		case WXK_LEFT:
			move = -1;
			break;
		case WXK_DOWN:
		case WXK_RIGHT:
			move = 1;
			break;
		case WXK_RETURN:
		case /* WXK_ENTER */ 3:
			wxPanel *panel = (wxPanel *)GetParent();
			panel->OnDefaultAction(this);
			break;
	}

	if (move) {
		Cell now, next, save;
		now.h = now.v = 0;
		if (LGetSelect(TRUE, &now, cListHandle)) {
			if (!((**cListHandle).selFlags & lOnlyOne) && (move > 0)) {
				// moving forward for multiple selections: find last selected
				Cell next;

				next.h = now.h;
				next.v = now.v + 1;
				while (LGetSelect(TRUE, &next, cListHandle)) {
					now.v = next.v;
					next.v++;
				}
			}
		} else if (move < 0)
			now.v = 1;
		else if (move > 0)
			now.v = no_items - 2;

		next.h = now.h;
		next.v = now.v + move;
		if (next.v < 0 || next.v >= no_items) {
			save.h = now.h;
			save.v = now.v;
			next.v = now.v;
		} else
			save.h = save.v = -1;

		if ((!event.shiftDown && !event.metaDown) || ((**cListHandle).selFlags & lOnlyOne)) {
			int i;
			Cell next;
			next.h = now.h;
			for (i = 0; i < no_items; i++)
				if (i != save.v) {
					next.v = i;
					LSetSelect(FALSE, next, cListHandle);
				}
		}

		LSetSelect(TRUE, next, cListHandle);
	  	
		// Make sure newly selected is visible: 
		Rect rect;
		LRect(&rect, next, cListHandle);
		if (rect.top < 0) {
			int amt = rect.top / (**cListHandle).cellSize.v;
			if (rect.top % (**cListHandle).cellSize.v)
				--amt;
			LScroll(0, amt, cListHandle);
		} else if (rect.bottom > (**cListHandle).rView.bottom) {
			int diff = rect.bottom - (**cListHandle).rView.bottom;
			int amt = diff / (**cListHandle).cellSize.v;
			if (diff % (**cListHandle).cellSize.v)
				amt++;
			LScroll(0, amt, cListHandle);
		}

		{
		    wxCommandEvent *commandEvent = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_COMMAND);
			commandEvent->commandString = GetString(next.v);
			commandEvent->commandInt = next.v;
			commandEvent->eventObject = this;
	  		ProcessCommand(*commandEvent);
	  	}
	}
}

Bool wxListBox::WantsFocus(void)
{
	return TRUE;
}

// ---------------- Routines to Handle the List Contents ------------------------------------

void wxListBox::Delete(int N)
{ 
  // (JDH) need to add code here to take care of clientDataList
  wxNode *node = cDataList->Find((long)N);  // get item from list
  if (node) cDataList->DeleteNode(node);    // if existed then delete from list
  node = cDataList->First();                // we now have to adjust all keys that 
  while (node)                                  // are >=N+1
   { if (node->key.integer >= (long)(N+1))      // very ugly C++ wise but no other way 
       node->key.integer--;                     // to look at or change key value
     node = node->Next();
   }
  SetCurrentDC();
  LDelRow(1, N, cListHandle);
  no_items --;
  cKeycnt--;
}

// Append an item to the list box
void wxListBox::Append(char *Item, char *Client_data)
{
  SetCurrentDC();
  LDoDraw(FALSE, cListHandle);
  Cell cell = {no_items, 0};		// Point = {v, h} so Cell = {row, col}
  LAddRow(1,no_items, cListHandle);
  LSetCell(Item, strlen(Item) ,cell, cListHandle);
  // LDraw(cell, cListHandle); // mflatt: can't get this to work; co-ordinate problems?
  LDoDraw(TRUE, cListHandle);
  cDataList->Append(no_items, (wxObject *)Client_data);
  no_items ++;
  if (Client_data) cKeycnt++;
  Paint();
}

void wxListBox::Append(char *Item)
{
  Append(Item, NULL);
 }

void wxListBox::Set(int n, char *choices[])
{
  if (no_items > 0) {
	this->Clear();
  }
  LDoDraw(FALSE, cListHandle);
  Cell cell = {0, 0};		// Point = {v, h} so Cell = {row, col}
  for (cell.v = 0; cell.v < n; cell.v++) {
	  cDataList->Append(cell.v, (wxObject *)NULL);
	  LAddRow(1,cell.v, cListHandle);
	  LSetCell(choices[cell.v], strlen(choices[cell.v]) ,cell, cListHandle);
  }
  no_items = cell.v;
  LDoDraw(TRUE, cListHandle);
  Paint();
}

int wxListBox::FindString(char *s)
{
	Cell cell = {0, 0};
	if (::LSearch(s, strlen(s), NULL, &cell, cListHandle) )
		return cell.v;
	else
		return -1;
}

void wxListBox::Clear(void)
{
  if (no_items<=0)
    return ;
  cDataList->Clear ();
  SetCurrentDC();
  ::LDelRow(0, 0, cListHandle);
  no_items = 0;
  cKeycnt = 0;
}

// Find string for position
char *wxListBox::GetString(int N)
{
	if (N >= no_items)
		return NULL;

	Cell cell = {N, 0};
	short	len = 255;				// FIXME - really should be size of wxBuffer
	LGetCell(wxBuffer, &len,  cell, cListHandle);
	if (len <= 0 )
		return NULL;
	
	wxBuffer[len] = '\0';
	return wxBuffer;
}

void wxListBox::SetString(int N, char *s)
{
	Cell cell = {N, 0};
	int	len = strlen(s);
    SetCurrentDC();
	LSetCell(s, len,  cell, cListHandle);
}

char *wxListBox::GetClientData(int N)
{
	wxNode *cdt = cDataList->Find(N);
	if (cdt)
		return((char *)cdt->Data());
	else
		return NULL;
}

void wxListBox::SetClientData(int N, char *s)
{
	wxNode *cdt = cDataList->Find(N);
	if (cdt)
		cdt->SetData((wxObject *)s);
}

// Undocumented !
void wxListBox::InsertItems(int nItems, char **Items, int pos) 
{
  LDoDraw(FALSE, cListHandle);
  Cell cell = {pos, 0};		// Point = {v, h} so Cell = {row, col}
  int n;
  for (n = 0;  n < nItems; cell.v++, n++) {
	  cDataList->Append(cell.v, (wxObject *)NULL);
	  LAddRow(1,cell.v, cListHandle);
	  LSetCell(Items[n], strlen(Items[n]) ,cell, cListHandle);
  }
  no_items = no_items + nItems;
  LDoDraw(TRUE, cListHandle);
  Paint();
}

// ------ Manage Selections ----------------------

void wxListBox::SetFirstItem(int N)	// Make item N the first visible item in list
{	// Kind of Kludgy
	SetCurrentDC();
	// Get Current Selection - save it
	int oldsel = GetSelection();
	// Set the selection to N
	SetSelection(N, TRUE);
	// AutoScroll
	::LAutoScroll(cListHandle);
	// Turn off the selection
	SetSelection(N, FALSE);
	// Restore the saved selection (if there was one);
	if (oldsel != -1)
		SetSelection(oldsel, TRUE);
}

void wxListBox::SetFirstItem(char *s) 
{
  int N = FindString(s) ;

  if (N>=0)
    SetFirstItem(N) ;
}

void wxListBox::SetSelection(int N, Bool select)
{
  if (N < 0 || (N >= no_items))
    return;

   if (select && ((**cListHandle).selFlags & lOnlyOne)) {
     int s = GetSelection();
     if (s == N)
       return;
     if (s >= 0) {
	   Cell cell = {s, 0};
	   SetCurrentDC();
	   LSetSelect(FALSE, cell, cListHandle);
	 }
   }
   
	Cell cell = {N, 0};
	SetCurrentDC();
	LSetSelect(select, cell, cListHandle);
}

Bool wxListBox::Selected(int N)
{
	Cell cell = {N, 0};
	return LGetSelect(FALSE, &cell, cListHandle);
}

void wxListBox::Deselect(int N)
{
	Cell cell = {N, 0};
    SetCurrentDC();
	LSetSelect(FALSE, cell, cListHandle);
}

// Return number of selections and an array of selected integers
// Use selections field to store data, which will be cleaned up
// by destructor if necessary.
int wxListBox::GetSelections(int **list_selections)
{
	Cell cell = {0, 0};
	int n = 0;
	while (LGetSelect(TRUE, &cell, cListHandle)) {
		cell.v++;
		n++;
	}
	if (n <= 0)
		return 0;
	cell.h = 0; cell.v = 0;
	if (selections)
	    delete[] selections;
	selections = new int[n];
	n = 0;
	while (LGetSelect(TRUE, &cell, cListHandle)) {
		selections[n++] = cell.v++;
	}
	*list_selections = selections;
	return n;
}

// Get single selection, for single choice list items
int wxListBox::GetSelection(void)
{
	int r;
	Cell cell = {0, 0};

	r = LGetSelect(TRUE, &cell, cListHandle);
	if (r == FALSE)
		return -1;
	else
		return cell.v;
}

void wxListBox::SetBackgroundColour(wxColour*col)
{
} 

void wxListBox::SetLabelColour(wxColour*col)
{
}

void wxListBox::SetButtonColour(wxColour*col) 
{
}

char* wxListBox::GetLabel(void)
{
	return (cListTitle ? cListTitle->GetLabel() : NULL);
}

void wxListBox::SetLabel(char *label)
{
	if (cListTitle) cListTitle->SetLabel(label);
}

//-----------------------------------------------------------------------------
void wxListBox::OnSetFocus()
{
	((wxBorderArea *)cBorderArea)->cBorder->SetBrush(wxBLACK_BRUSH);
	if (!cHidden)
		((wxBorderArea *)cBorderArea)->cBorder->Paint();

	wxWindow::OnSetFocus();
}

//-----------------------------------------------------------------------------
void wxListBox::OnKillFocus()
{
	((wxBorderArea *)cBorderArea)->cBorder->SetBrush(wxCONTROL_BACKGROUND_BRUSH);
	if (!cHidden)
		((wxBorderArea *)cBorderArea)->cBorder->Paint();

	wxWindow::OnKillFocus();
}

//-----------------------------------------------------------------------------
void wxListBox::ShowAsActive(Bool flag) // mac platform only
{
	if (cHidden || IsGray()) return;
	
	SetCurrentDC();
	::LActivate(flag, cListHandle);
}

void wxListBox::DoShow(Bool on)
{
	if (!CanShow(on))
		return;
		
	((wxBorderArea *)cBorderArea)->cBorder->DoShow(on);
	((wxBorderArea *)cThinBorderArea)->cBorder->DoShow(on);
	
	wxWindow::DoShow(on);
}

void wxListBox::ChangeToGray(Bool gray)
{
	if (cHidden) return;
	
	SetCurrentDC();
	::LActivate(!gray, cListHandle);
	((wxBorderArea *)cThinBorderArea)->cBorder->InternalGray(gray);
	if (cListTitle)
		((wxLabelArea *)cListTitle)->GetMessage()->InternalGray(gray);
	wxWindow::ChangeToGray(gray);
}
