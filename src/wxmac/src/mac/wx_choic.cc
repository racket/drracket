/*
 * File:        wx_choic.cc
 * Purpose:     Choice item implementation (Mac version)
 * Author:      Julian Smart/Cecil Coupe
 * Created:     1993
 * Updated:	April 1995
 *   July 22, 1995 - First Mac version - Cecil Coupe
 *
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */


#ifdef __GNUG__
#pragma implementation
#pragma implementation "wx_choic.h"
#endif

#include <stdlib.h>
#include "common.h"
#include "wx_utils.h"
#include "wx_privt.h"
#include "wx_choic.h"
#include "wx_menu.h"
#include "wx_mac_utils.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include <Controls.h>

/* 
   The Mac ctl manager routine for popup menus has a problem or two
   1. It appears that you can only popup menus that are resource based.
   This makes it hard for the running program to append items.
   
   2. Titles (Labels in wx) are only horizontal.
   
   Based on IM-V, there are many things to do if we don't have the
   control mgrs help.
   1. We have to draw the shadowed box
   2. Invert the Title (label) where the menu is showing
   3. We have to draw the currently selected value in the shadowed box
   Barrowed code from Apple DTS Snippets (PopMenus.p)

   To manage all this, a Mac wxChoice has these components and processing:
   1. The Title.
   2. The value area gets the shadowed box and displays the current value
   3. a Mac Menu handle
   4. When we get a paint event,
   		we draw the shadowed box and the value (will have to be cached?)
   		constrained to the height and width of the box.
   5. When we get a mousedown:
   		Invert the Title,
   		InsertMenu(ourmenuhandle, -1)
   		call PopUpMenuSelect()
   		DeleteMenu(ourmenuID)
   		invert the Title
   		draw the [newly] selected current value
   	6. enable and disable - ?
   	
   	Todo
   	1. Free the pieces/parts in the destructor
   	2. Finish the event handling (wxEvents - CommandString needed?)
   	3. Add enable and show
   	4. Finish fixed sizing

*/
// Because I never get this right and t,l,b,r makes sense to me - CJC
//
#define SetBounds(rect, top, left, bottom, right) ::SetRect(rect, left, top, right, bottom)
#define VSLOP 3
#define HSLOP 4
#define MSPACEX 4
#define MSPACEY 1
#define TRIANGLE_WIDTH 10
#define TRIANGLE_HEIGHT 5
#define TRANGLE_RIGHT_SPACE 5
#define Checkem	TRUE;		// adds code to do checkmarks to selected item


wxChoice::wxChoice (wxPanel * panel, wxFunction func, char *Title,
	  int x, int y, int width, int height, int N, char **Choices,
	  long style, char *name
	):
	wxbChoice (panel, func, Title, x, y, width, height, N, Choices, style, name)
{
  Create (panel, func, Title, x, y, width, height, N, Choices, style, name);
}


Bool wxChoice::
Create (wxPanel * panel, wxFunction func, char *Title,
	int x, int y, int width, int height, int N, char **Choices,
	long style, char *name)
{
	buttonFont = panel->buttonFont;
	labelFont = panel->labelFont;
	backColour = panel->backColour;
	labelColour = panel->labelColour;
	buttonColour = panel->buttonColour;
	window_parent = panel;
	labelPosition = panel->label_position;
	windowStyle = style;
	valueFont = wxNORMAL_FONT;
	Callback (func);
	
	SetCurrentMacDC();
	CGrafPtr theMacGrafPort = cMacDC->macGrafPort();
/*	if (!buttonFont)
		buttonFont = wxNORMAL_FONT;
*/
	if (!labelFont)
		labelFont = wxNORMAL_FONT;

	if (Title)
      Title = wxItemStripLabel(Title);

	float fWidth = 50.0;
	float fHeight = 12.0;
	float fDescent = 0.0;
	float fLeading = 0.0;
	labelbase = 9;
	if (Title)
	{
		GetTextExtent(Title, &fWidth, &fHeight, &fDescent, &fLeading, labelFont);
		if (fHeight < 12) fHeight = 12; 
		int n = strlen(Title);
		sTitle = (StringPtr)new char[n+1];
		sTitle[0] = n;
		memcpy(&sTitle[1], Title, n);
		labelbase = fDescent + fLeading;
	}
	else  {
		sTitle = NULL;
		fWidth = fHeight = 0;
	}
	int lblw = fWidth + (Title ? HSLOP : 0);
	int lblh = fHeight+2;
	
	int maxdfltw;
	int maxdflth;
	if (N) {
		maxdfltw = 30;
	} else {
		// No strings - pick something reasonable - say 60 pixels
		maxdfltw = 60;
	}

	GetTextExtent("Test", &fWidth, &fHeight, &fDescent, &fLeading, valueFont);
	maxdflth = fHeight;
	valuebase = fDescent + fLeading;

	// Build the menu and find the width of the longest string
	PopUpID = wxMenu::gMenuIdCounter++;
#ifdef PPCC
	Str255 ns = "\p";
	hDynMenu = NewMenu(PopUpID, ns);
#else
	hDynMenu = NewMenu(PopUpID, "\p");
#endif
	CheckMemOK(hDynMenu);
	int n,w,h;
	char temp[255];
	for (n = 0; n < N; n++) {
		// attempt to size control by width of largest string
		GetTextExtent(Choices[n], &fWidth, &fHeight, &fDescent, &fLeading, valueFont);
		w = fWidth;
		h = fHeight;
		maxdfltw = max(w, maxdfltw);
		maxdflth = max(h, maxdflth);
		strcpy(temp, Choices[n]);
		C2PStr(temp);
		::AppendMenu(hDynMenu, "\ptemp");
		::SetMenuItemText(hDynMenu, n + 1, (ConstStr255Param)temp);
	}
	no_strings = N;
	maxdflth += MSPACEY*2;			// extra pixels on top & bottom
	char checkm[] = {18, 0};
	GetTextExtent(checkm, &fWidth, &fHeight, &fDescent, &fLeading, valueFont);
	maxdfltw += fWidth + TRIANGLE_WIDTH + TRANGLE_RIGHT_SPACE + MSPACEX;
	// compute the Rects that contain everything.
	// note valuebase and labelbase are changed from font descents
	// to number of pixels to substract from the rect bottom.
	if (labelPosition == wxVERTICAL) {
		w = max(lblw, maxdfltw);
		if (Title)
			SetBounds(&TitleRect,1, 1, lblh+1, w);
		else
			SetBounds(&TitleRect,1, 1, 1, 1);
		SetBounds(&ValueRect, TitleRect.bottom + VSLOP, 1,
				  TitleRect.bottom + maxdflth + 1, maxdfltw);
		valuebase += MSPACEY;
		SetBounds(&CtlRect, 0, 0, ValueRect.bottom + 2, w+1);
	} else {
		h = max(lblh, maxdflth);
		SetBounds(&ValueRect, 1, lblw+1, h-1, maxdfltw + lblw);
		valuebase += MSPACEY;
		SetBounds(&CtlRect, 0, 0, h+1, lblw+maxdfltw+2);
		int bot = (h - valuebase) + labelbase;
		SetBounds(&TitleRect, bot-lblh, 1, bot, lblw);
	}
	if (width < 0 && height < 0) {
		// use the sizes we just calced
		cWindowWidth = CtlRect.right;
		cWindowHeight = CtlRect.bottom;
	} else {
		OnClientAreaDSize((width == -1 ? 0 : width),
			 (height == -1 ? 0 : height), (x == -1 ? 0 : x), (y == -1 ? 0 : y));
	}
	SetSelection(0);
	//DrawChoice(TRUE);

	
	if (GetParent()->IsHidden())
		DoShow(FALSE);

  	return TRUE;
}

int wxChoice::Number(void)
{
	return no_strings;
}

wxChoice::~wxChoice (void)
{
	::DeleteMenu(PopUpID);		// Make Menu Mgr forget
	::DisposeMenu(hDynMenu);
	delete[] sTitle;
}
// --------- Calculate the ValueRect based on the menu's strings ----
void wxChoice::ReCalcRect(void) {
	int maxdfltw = 30;
	int maxdflth = 12;
	float	fWidth, fHeight, fDescent, fLeading;
	int n,w,h;
	unsigned char temp[256];
	for (n = 0; n < no_strings; n++) {
		// attempt to size control by width of largest string
		::GetItem(hDynMenu, n+1, temp);
		temp[temp[0]+1] = '\0';
		GetTextExtent((char *)&temp[1], &fWidth, &fHeight, &fDescent, &fLeading, valueFont);
		w = fWidth;
		h = fHeight;
		maxdfltw = max(w, maxdfltw);
		maxdflth = max(h, maxdflth);
	}
	maxdflth += VSLOP*2;			// extra pixels top and bottom
	maxdfltw += HSLOP*2;			// extra pixels on each side
	// compute the Rects that contain everything.
	// note valuebase and labelbase are changed from font descents
	// to number of pixels to substract from the rect bottom.
	int lblw = TitleRect.right - TitleRect.left;
	int lblh = TitleRect.bottom - TitleRect.top;
	if (labelPosition == wxVERTICAL) {
		w = max(lblw, maxdfltw);
		SetBounds(&TitleRect,1, 1, lblh+1, w);
		SetBounds(&ValueRect,TitleRect.bottom+VSLOP, 1,
			TitleRect.bottom+VSLOP+maxdflth, maxdfltw);
		SetBounds(&CtlRect, 0, 0, ValueRect.bottom+VSLOP, w+1);
	} else {
		h = max(lblh, maxdflth);
		SetBounds(&ValueRect, 1, lblw+1, h-1, maxdfltw + lblw);
		SetBounds(&CtlRect, 0, 0, h+1, lblw+maxdfltw+2);
		int bot = (h - valuebase) + labelbase;
		SetBounds(&TitleRect, bot-lblh, 1, bot, lblw);
	}
}

// ---------Draw the Choice Control -----------
void wxChoice::DrawChoice(Bool active)
{
	SetCurrentDC();
	Rect t = TitleRect;
	::MoveTo(t.left, t.bottom - labelbase);
	SetFont(labelFont);
	SetTextInfo();
	int w = 0;
	int i;
	
	if (sTitle) {
		for (i = 1; i <= sTitle[0] && w < TitleRect.right; i++)
			w += ::CharWidth(sTitle[i]);
		for (; w >= TitleRect.right; i--)	// backup
			w -= ::CharWidth(sTitle[i]);
		
		::DrawText(sTitle, 1, i-1);
		//::DrawString(sTitle);
	}
	
	Rect r = ValueRect;
	::InsetRect(&r, -1, -1);
	::FrameRect(&r);
	::MoveTo(r.right, r.top+2);
	::LineTo(r.right, r.bottom);
	::LineTo(r.left+2, r.bottom);

	// mflatt:
	RGBColor save;
	GetForeColor(&save);
	ForeColor(whiteColor);
	::InsetRect(&r, 1, 1);
	::PaintRect(&r);
	::InsetRect(&r, -1, -1);
	::RGBForeColor(&save);
	
	PolyHandle poly;
	poly = OpenPoly();
	if (poly) {
		MoveTo(r.right - TRIANGLE_WIDTH - TRANGLE_RIGHT_SPACE, 
			   r.top + (r.bottom - r.top - TRIANGLE_HEIGHT) / 2);
	    Line(TRIANGLE_WIDTH, 0);
	    Line(-(TRIANGLE_WIDTH / 2), TRIANGLE_HEIGHT);
	    Line(-(TRIANGLE_WIDTH / 2), -TRIANGLE_HEIGHT);
		ClosePoly();
		PaintPoly(poly);
	}
	
	if (!no_strings)
		return;
		
	r.left += ::CharWidth('¥') + ::CharWidth(' ') + MSPACEX;
	r.right -= TRIANGLE_WIDTH - TRANGLE_RIGHT_SPACE;
	
	Str255	s;
	if (selection < 0)
		selection = 0;
	::GetItem(hDynMenu, selection+1, s);
	SetFont(valueFont);
	SetTextInfo();
	::MoveTo(r.left, r.bottom - valuebase);
	w = 0;
	int elw = ::CharWidth('É');
	int tgtw = r.right - r.left - elw;
	for (i = 1; i < s[0] && w < tgtw; i++)
		w+= ::CharWidth(s[i]);
	for (; w >= tgtw; i--)
		w -= ::CharWidth(s[i]);
	if (i != s[0])
		s[i] = 'É';
	::DrawText(s, 1, i);
	//DrawString(s);
}


// --------- Event Handling -------------------
void wxChoice::Paint(void)
{
	if (cHidden) return;
	DrawChoice(TRUE);
	wxWindow::Paint();
}

// Resize and/or Move the Control
void wxChoice::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
	SetCurrentDC();
	int clientWidth, clientHeight;

	if (dW || dH)
	{
		GetClientSize(&clientWidth, &clientHeight);
		if (clientWidth != CtlRect.right) {
			int needw = CtlRect.right - clientWidth;
			if (labelPosition == wxVERTICAL) {
				// Shrink width of both Title and Value
				if (sTitle)
					TitleRect.right -= needw;
				ValueRect.right -= needw;
			} else {
				// Shrink width of Value, Title strings if we have to
#if 0
				if (sTitle) {
					TitleRect.right -= needw/2;
					ValueRect.left -= needw/2;
				}
#endif
				ValueRect.right -= needw;
			}
			CtlRect.right = clientWidth;
		} 
		if (clientHeight != CtlRect.bottom) {
			int needh = CtlRect.bottom - clientHeight;
			if (labelPosition == wxVERTICAL) {
				// Shrink heights equally
				if (sTitle)
					TitleRect.bottom -= needh/2;
				ValueRect.top -= needh/2;
				ValueRect.bottom -= needh;
				
			} else {
				// Shrink heights eqally
				if (sTitle)
					TitleRect.bottom -= needh;
				ValueRect.bottom -= needh;
			}
			CtlRect.bottom = clientHeight;
		}
	}

	if (dX || dY)
	{
		cMacDC->setCurrentUser(NULL); // macDC no longer valid
		SetCurrentDC(); // put new origin at (0, 0)
	}

#if 0
	if (dW || dH || dX || dY)
	{
		GetClientSize(&clientWidth, &clientHeight);
		Rect clientRect = {0, 0, clientHeight, clientWidth};
		::InvalidRect(&clientRect);
	}
#endif
}

void wxChoice::ChangeToGray(Bool gray)
{
  wxWindow::ChangeToGray(gray);
}

//-----------------------------------------------------------------------------
void wxChoice::DoShow(Bool show)
{
	wxWindow::DoShow(show);
}

//-----------------------------------------------------------------------------
void wxChoice::ShowAsActive(Bool flag) // mac platform only
{
}


void wxChoice::OnEvent(wxMouseEvent& event) // mac platform only
{
	if (event.LeftDown() && (no_strings > 0))
	{
		SetCurrentDC();
	
		int	newsel;
		Point pos = {ValueRect.top, ValueRect.left};
		LocalToGlobal(&pos);
		// if (sTitle) ::InvertRect(&TitleRect);
		::InsertMenu(hDynMenu, -1);
		::CalcMenuSize(hDynMenu);
		newsel = ::PopUpMenuSelect(hDynMenu, pos.v, pos.h, selection+1);
		// if (sTitle) ::InvertRect(&TitleRect);
		::DeleteMenu(PopUpID);
		RGBColor save;
		::GetForeColor(&save);
		::ForeColor(whiteColor);
		::PaintRect(&ValueRect);
		::RGBForeColor(&save);
		if (newsel) {
			newsel = LoWord(newsel) -1;
			if (newsel != selection) {
#ifdef Checkem
				// selected a different item
				::CheckItem(hDynMenu, selection+1, FALSE);
				::CheckItem(hDynMenu, newsel+1, TRUE);
#endif
				selection = newsel;
				wxCommandEvent *commandEvent = new wxCommandEvent(wxEVENT_TYPE_CHOICE_COMMAND);
				commandEvent->eventObject = this;
				commandEvent->commandInt = selection;
				commandEvent->commandString = GetString(selection);
		  		ProcessCommand(*commandEvent);
			}
		}
		DrawChoice(TRUE);
	}
}


void wxChoice::Command(wxCommandEvent& event) // mac platform only (also xview platform)
{
	ProcessCommand(event);
}

// ------------ Methods available to user ------------

void wxChoice::Append (char *Item)
{
  Str255 s;
  int n = strlen(Item);
  memcpy(&s[1], Item, n);
  s[0] = n;
  ::InsertMenuItem(hDynMenu, "\ptemp", no_strings);
  ::SetMenuItemText(hDynMenu, no_strings + 1, s);
  no_strings++;
  //ReCalcRect();
  Paint();
}

void wxChoice::Clear (void)
{
	int n;
	for (n = 0; n < no_strings; n++)
		::DelMenuItem(hDynMenu, 1);
	no_strings = 0;
	selection = 0;
  Paint();
}


int wxChoice::GetSelection (void)
{
  return selection;
}

void wxChoice::SetSelection (int n)
{
	if ((n < 0) || (n >= no_strings))
	  return;
	
#ifdef Checkem
	::CheckItem(hDynMenu, selection+1, FALSE);
	::CheckItem(hDynMenu, n+1, TRUE);
#endif
	selection = n;
	DrawChoice(TRUE);
}

int wxChoice::FindString (char *s)
{
	int n = strlen(s);
	int i;
	Str255	ps;
	for (i = 0; i < no_strings; i++) {
		::GetItem(hDynMenu, i+1, ps);
		if (!strcmp(PtoCstr(ps), s))
			return i;
	}
	return -1;
}

char *wxChoice::GetString (int n)
{
	Str255	s;
	if (n < 0 || n >= no_strings)
		return NULL; // dummy
	::GetItem(hDynMenu, n+1, s);
	memcpy(wxBuffer, &s[1], s[0]);
	wxBuffer[s[0]] = '\0';
	return copystring(wxBuffer);
}


void wxChoice::SetBackgroundColour(wxColour*col)
{
} 

void wxChoice::SetLabelColour(wxColour*col)
{
}

void wxChoice::SetButtonColour(wxColour*col) 
{
}

char* wxChoice::GetLabel(void)
{
	int n;
	if (sTitle && (n = sTitle[0])) {
		memcpy(wxBuffer, &sTitle[1], n);
		wxBuffer[n] = '\0';
		return wxBuffer;
	}
	else
		return NULL;
}

void wxChoice::SetLabel(char *label)
{
	if (sTitle) {
		delete[] (char *)sTitle;
	}
	int n = strlen(label);
	sTitle = (StringPtr)new char[n+1];
	sTitle[0] = n;
	memcpy(&sTitle[1], label, n);
	Paint();
}

