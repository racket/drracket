///////////////////////////////////////////////////////////////////////////////
// File:	wx_frame.cc
// Purpose:	wxFrame implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wx_frame.h"
#include "wx_menu.h"
#include "wx_mnuit.h"
#include "wx_gdi.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_panel.h"
#include "wx_txt.h"
#include "wx_area.h"
#include "wx_screen.h"
#include "wx_mac_utils.h"
#include <QuickDraw.h>
#include <TextEdit.h>
#include <Menus.h>

extern wxScreen *theScreen;
static wxMenuBar *empty_menu_bar, *close_menu_bar;

//=============================================================================
// Public constructors
//=============================================================================


//-----------------------------------------------------------------------------
wxFrame::wxFrame // Constructor (for frame window)
	(
		wxFrame*	parentFrame,		// this is ignored
		char*		windowTitle,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbFrame (windowName, wxScreen::gScreenWindow,
				  x, y, width, height, style)
{
	InitDefaults();

	SetEraser(wxCONTROL_BACKGROUND_BRUSH);

	int X = cWindowX;
	int Y = cWindowY;

	Rect theBoundsRect;
	::SetRect(&theBoundsRect, X, Y, X + cWindowWidth, Y + cWindowHeight);
	Str255 theWindowTitle = "\p";
	if (windowTitle) wxMacCtoPString(windowTitle, theWindowTitle);
	const Bool WindowIsVisible = TRUE;

	cUserHidden = TRUE;

	int theProcID;
 	// GRW adds
 	if (!(cStyle & wxMINIMIZE_BOX))		//XXX need wxCLOSE_BOX
 		theProcID = movableDBoxProc;
  	else if (cStyle & wxMAXIMIZE_BOX)
 	{
 		if (cStyle & wxRESIZE_BORDER)
 			theProcID = zoomDocProc;
 		else
 			theProcID = zoomNoGrow;
 	}
 	else if (cStyle & wxRESIZE_BORDER)
 		theProcID = documentProc;
 	else
 		theProcID = noGrowDocProc;
 	// end of GRW replacement

	const WindowPtr MoveToFront = WindowPtr(-1L);
	const Bool HasGoAwayBox = TRUE;
	long theRefCon = (long)this;
	CWindowPtr theMacWindow = (CWindowPtr)::NewCWindow(NULL, &theBoundsRect, theWindowTitle,
					!WindowIsVisible, theProcID, MoveToFront, HasGoAwayBox, theRefCon);
	CheckMemOK(theMacWindow);
	cMacDC = new wxMacDC(theMacWindow);

  // Calculate the platformArea size
	Rect theStrucRect = wxMacGetStrucRect();
	Rect theContRect = wxMacGetContRect();
	wxMargin platformMargin;
	platformMargin.SetMargin(theContRect.left - theStrucRect.left, Direction::wxLeft);
	platformMargin.SetMargin(theContRect.top - theStrucRect.top, Direction::wxTop);
	platformMargin.SetMargin(theStrucRect.right - theContRect.right, Direction::wxRight);
	platformMargin.SetMargin(theStrucRect.bottom - theContRect.bottom, Direction::wxBottom);
	PlatformArea()->SetMargin(platformMargin);

  // The client has the requested window position, not the window: must move
	SetCurrentDC();
	wxMargin contentAreaMargin = ContentArea()->Margin(wxScreen::gScreenWindow);
	int theMacX = contentAreaMargin.Offset(Direction::wxLeft);
	int theMacY = contentAreaMargin.Offset(Direction::wxTop);
	MoveWindow((WindowPtr)theMacWindow, theMacX, theMacY, FALSE);

  // The client has the requested window size, not the window: must resize
	int theMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
	int theMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
	SizeWindow((WindowPtr)theMacWindow, theMacWidth, theMacHeight, FALSE);
}

//=============================================================================
// Public destructor
//=============================================================================

wxFrame::~wxFrame(void)
{
#if WXGARBAGE_COLLECTION_ON
    if (IsVisible()) 
		Show(FALSE);
#endif

	// Kludge needed here:
	// Bad:  DisposeWindow deletes the mac controls, but not their wxItems.
	// Good: DestroyChildren deletes the wxItems and their associated mac controls.
	if (cDialogPanel)
		cDialogPanel = NULL;
	DestroyChildren();

	CWindowPtr theMacWindow = cMacDC->macGrafPort();
	::DisposeWindow((WindowPtr)theMacWindow);
	delete cMacDC;
	if (wx_menu_bar)
		delete wx_menu_bar;
	if (cControlArea)
		delete cControlArea;
	if (cContentArea)
		delete cContentArea;
	if (cPlatformArea)
		delete cPlatformArea;
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxFrame::InitDefaults(void)
{
	cWindowTitle[0] = 0; // kludge

	cMaximized = FALSE;
	cIsModal = FALSE;
	cBusyCursor = 0;
	cFocusWindow = NULL;
	cStatusPanel = NULL;
	cStatusText = NULL;
    cDialogPanel = NULL;
  	cControlArea = new wxArea(this);
  	cContentArea = new wxArea(this);
  	cPlatformArea = new wxArea(this);
}

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Geometry methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxArea* wxFrame::PlatformArea(void) { return cPlatformArea; } // mac platform only

//-----------------------------------------------------------------------------
wxArea* wxFrame::ContentArea(void) { return cContentArea; } // mac platform only

//-----------------------------------------------------------------------------
wxArea* wxFrame::ControlArea(void) { return cControlArea; } // mac platform only

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Sizing methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
// Note: Can't set width < width of borders + etc.
// Note: Can't set height < height of borders + statusLine + etc.
// Note: Mac platform can't set width = width of borders + etc.
// Note: Mac platform can't set height = height of borders + statusLine + etc.
//-----------------------------------------------------------------------------
void wxFrame::DoSetSize(int x, int y, int width, int height)
{
 	// tom: to avoid unnecessary calculations!
 	if (x==-1) 
 		x= cWindowX;
 	if (y==-1) 
 		y = cWindowY;
 	if (width==-1) 
 		width = cWindowWidth;
 	if (height==-1) 
 		height = cWindowHeight;
 	
	Bool xIsChanged = (x != cWindowX);
	Bool yIsChanged = (y != cWindowY);
	Bool widthIsChanged = (width != cWindowWidth);
	Bool heightIsChanged = (height != cWindowHeight);

 	int oldWindowX = cWindowX;
 	int oldWindowY = cWindowY;
 	int oldWindowWidth = cWindowWidth;
 	int oldWindowHeight = cWindowHeight;
 
	if (width > cWindowWidth || height > cWindowHeight)
 	{
 		// Invalidate grow box:
 		int oldMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
 		int oldMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
 		Rect oldGrowRect = {oldMacHeight - 15, oldMacWidth - 15, oldMacHeight, oldMacWidth};
 		SetCurrentMacDC();
 		::InvalRect(&oldGrowRect);
 		::EraseRect(&oldGrowRect); /* MATTHEW: [5] */
 	}

	int dw, dh;

	dw = cWindowWidth - width;
	dh = cWindowHeight - height;

    if (xIsChanged) cWindowX = x;
    if (yIsChanged) cWindowY = y;
	if (widthIsChanged) cWindowWidth = width;
	if (heightIsChanged) cWindowHeight = height;

	SetCurrentDC();
	WindowPtr theMacWindow = (WindowPtr)cMacDC->macGrafPort();

	if (xIsChanged || yIsChanged)
	{
		wxMargin contentAreaMargin = ContentArea()->Margin(wxScreen::gScreenWindow);
		int theMacX = contentAreaMargin.Offset(Direction::wxLeft);
		int theMacY = contentAreaMargin.Offset(Direction::wxTop);
		::MoveWindow(theMacWindow, theMacX, theMacY, FALSE);
	}

	if (widthIsChanged || heightIsChanged)
	{
		int theMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
		int theMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
		::SizeWindow(theMacWindow, theMacWidth, theMacHeight, TRUE);
 		// Resizing puts windows into the unzoomed state
 		cMaximized = FALSE;

		if (dw > 0 || dh > 0) {
			// Invalidate new region:
	 		int w = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
	 		int h = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
	 		Rect r;
	 		SetCurrentMacDC();
	 		if (dw) {
	 			r.top = 0;
	 			r.bottom = h;
	 			r.left = w - dw;
	 			r.right = w;
		 		::InvalRect(&r);
	 		}
	 		if (dh) {
	 			r.top = h - dw;
	 			r.bottom = h;
	 			r.left = 0;
	 			r.right = w;
		 		::InvalRect(&r);
	 		}
	 	}
 		
 		if (cStatusPanel) {
 		  Rect r = {0, 0, 32000, 32000};
 		  cStatusPanel->SetCurrentDC();
 		  EraseRect(&r);
 		  
 		  cStatusPanel->SetSize(0, theMacHeight - cStatusPanel->Height(),
 		  	                    theMacWidth - 18, -1);
 		  int w, h;
 		  cStatusPanel->GetClientSize(&w, &h);
 		  cStatusText->SetSize(-1, -1, w, -1);

 		  cStatusPanel->SetCurrentDC();
 		  EraseRect(&r);
 		}
 		
 		// Call OnSize handler
 		OnSize(width, height);
#if 0
 		// Don't wait for update event to redraw grow box
 		if (cStyle & wxRESIZE_BORDER)
 			MacDrawGrowIcon();
        Paint();
       if (cStyle & wxRESIZE_BORDER) //tom 
  			MacDrawGrowIcon();
#endif
	}
}

//-----------------------------------------------------------------------------
void wxFrame::Maximize(Bool maximize)
{
	if (cMaximized != maximize)
	{
		int oldWindowX = cWindowX;
		int oldWindowY = cWindowY;
		int oldWindowWidth = cWindowWidth;
		int oldWindowHeight = cWindowHeight;

		SetCurrentDC();
		WindowPtr theMacWindow = (WindowPtr)cMacDC->macGrafPort();
		::EraseRect(&theMacWindow->portRect);
		::ZoomWindow(theMacWindow, maximize ? inZoomOut : inZoomIn, TRUE);
		::InvalRect(&theMacWindow->portRect);
		cMaximized = maximize;

		wxMacRecalcNewSize();
	
		int dW = cWindowWidth - oldWindowWidth;
		int dH = cWindowHeight - oldWindowHeight;
		int dX = cWindowX - oldWindowX;
		int dY = cWindowY - oldWindowY;
		OnWindowDSize(dW, dH, dX, dY);
	}
}

//-----------------------------------------------------------------------------
// Mac platform only; internal use only.
//-----------------------------------------------------------------------------
void wxFrame::wxMacRecalcNewSize(void)
{
	Rect theStrucRect = wxMacGetStrucRect();
	Rect theContRect = wxMacGetContRect();
	cWindowX = theStrucRect.left;
	cWindowY = theStrucRect.top - GetMBarHeight(); // WCH: kludge
	cWindowWidth = theStrucRect.right - theStrucRect.left;
	cWindowHeight = theStrucRect.bottom - theStrucRect.top;
}

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Status line methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxFrame::CreateStatusLine(int number, char* name)
{
	if (status_line_exists) return;

	nb_status = number;
	status_line_exists = TRUE;
	cStatusPanel = new wxPanel(ControlArea(), -1, -1, -1, -1, wxBORDER);
	cStatusText = new wxText(cStatusPanel, NULL, NULL);
	cStatusText->Enable(FALSE);
	cStatusText->SetFont(wxNORMAL_FONT);
	int statusLineHeight = cStatusText->GetCharHeight() * nb_status;
	int clientWidth, clientHeight;
	GetClientSize(&clientWidth, &clientHeight);
	cStatusText->SetWidthHeight(clientWidth - 18, statusLineHeight);
	cStatusPanel->Fit();
#if 1
 	// it is a hack to put the line down..
 	cStatusPanel->SetSize(0, clientHeight - cStatusPanel->Height(),
 		 cStatusPanel->Width() - 18, cStatusPanel->Height()); // tom!!
 	// tom: here the Statuspanel is placed over the controlArea!!
 	cControlArea->SetMargin(cStatusPanel->Height() + 1, Direction::wxBottom);
#else
  	cStatusPanel->SetSize(0, 0, cStatusPanel->Width(), cStatusPanel->Height());
  	cControlArea->SetMargin(cStatusPanel->Height() + 1, Direction::wxTop);
#endif
	// cStatusPanel->SetJustify(Direction::wxLeft);

	wxMargin clientAreaMargin = ClientArea()->Margin(wxScreen::gScreenWindow);
    clientAreaMargin.SetMargin(clientAreaMargin.Offset(Direction::wxBottom) 
    	                        + statusLineHeight, 
 							   	Direction::wxBottom);
	ClientArea()->SetMargin(clientAreaMargin);
	OnSize(cWindowWidth, cWindowHeight);
}

//-----------------------------------------------------------------------------
void wxFrame::SetStatusText(char* text, int number)
{
 	if (!status_line_exists) 
 		return;
 	cStatusText->Clear(); // tom: replace, don't append
  	cStatusText->WriteText(text);
}
  
// tom: perhaps this could be done otherwise:
//      get statusline from Frame
//		do statusline->SetEraser
void wxFrame::SetStatusEraser(wxBrush* b)
{
	if (!status_line_exists) 
	 		return;
	cStatusPanel->SetEraser(b);
	cStatusText->SetEraser(b);
 	
}

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Menubar methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxFrame::SetMenuBar(wxMenuBar* menu_bar)
{
	// mflatt: if this menu bar is already in use, give up
    if (menu_bar && menu_bar->menu_bar_frame)
		return;

	WindowPtr theMacWindow = (WindowPtr)cMacDC->macGrafPort();
	wxMenuBar* oldMenuBar = wx_menu_bar;
	if (oldMenuBar)
	{
		wx_menu_bar = NULL;
		oldMenuBar->menu_bar_frame = NULL;
		// must correct each menu->menu_bar and menuItem->menuBar
	}

  	menu_bar->handle = NULL; // menu_bar->handle is not used
	if (menu_bar) menu_bar->menu_bar_frame = this;
	wx_menu_bar = menu_bar;

	if (theMacWindow == ::FrontWindow())
	{
		NowFront(TRUE);
	}
}

//-----------------------------------------------------------------------------
void wxFrame::Command(int id)
{ // Call this to simulate a menu command
	wxMenuBar* menuBar = GetMenuBar();
	if (menuBar)
	{
		wxMenuItem* item = menuBar->FindItemForId(id);
		if (item)
		{
			wxMenu* theParentMenu = item->ParentMenu();
			if (theParentMenu)
			{
				HiliteMenu(theParentMenu->cMacMenuId); // hilite the menu
				ProcessCommand(id);
			}
		}
	}
}

//-----------------------------------------------------------------------------
void wxFrame::ProcessCommand(int id)
{
	wxCommandEvent *commandEvent = new wxCommandEvent(wxEVENT_TYPE_MENU_COMMAND);
	commandEvent->commandInt = id;
	commandEvent->eventObject = this;

	if (wxNotifyEvent(*commandEvent, TRUE))
	{
		HiliteMenu(0); // unhilite the hilited menu
		return;
	}

	wxMenuBar* menuBar = GetMenuBar() ;
	wxMenuItem* item = menuBar->FindItemForId(id) ;
	if (item)
	{
		if (item->IsCheckable()) item->Check(!item->IsChecked());
	}

	OnMenuCommand(id);
	HiliteMenu(0); // unhilite the hilited menu

	wxNotifyEvent(*commandEvent, FALSE);
}

//-----------------------------------------------------------------------------
void wxFrame::NowFront(Bool flag) // mac platform only
{
// Show the menuBar for this frame on becoming active
	if (flag)
	{
		wxWindow::gMouseWindow = NULL; // If the frame changes, force capture off
	
		if (wx_menu_bar)
			wx_menu_bar->Install();
		else {
			if (0 && cIsModal) {
				if (!empty_menu_bar)
					empty_menu_bar = new wxMenuBar;
				empty_menu_bar->Install();
			} else {
				if (!close_menu_bar) {
					close_menu_bar = new wxMenuBar;
					/* When a frame doesn't have a menubar, doMacInMenuBar
					 assumes that any menulelection is the close item. */
					wxMenu *file = new wxMenu();
					file->Append(1, "Close\tCmd+W");
					close_menu_bar->Append(file, "File");
				}
				close_menu_bar->Install();
			}
		}
	}
}

void wxFrame::ShowAsActive(Bool flag)
{
	// Invalidate grow box (appearance changes with window active/inactive)
 	SetCurrentDC();
 	int theMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
 	int theMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
 	Rect growRect = {theMacHeight - 15, theMacWidth - 15, theMacHeight, theMacWidth};
 	// Erase it now if we're becoming inactive
 	if (!flag)
 		::EraseRect(&growRect);
 	::InvalRect(&growRect);
}

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Icon methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

void wxFrame::SetIcon(wxIcon* wx_icon) { }		// not implemented

void wxFrame::Iconize(Bool iconize) { }			// not implemented

Bool wxFrame::Iconized(void) { return FALSE; } 	// not implemented

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Platform methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
WindowPtr wxFrame::macWindow(void)
{
	return cMacDC ? (WindowPtr)cMacDC->macGrafPort() : NULL;
}

//-----------------------------------------------------------------------------
Bool wxFrame::IsMacWindow(void) { return TRUE; }

//-----------------------------------------------------------------------------
Bool wxFrame::IsVisible(void)
{
	Bool result = FALSE;
	if (cMacDC)
	{
		WindowPtr theMacWindow = (WindowPtr)cMacDC->macGrafPort();
		result = ((WindowPeek)theMacWindow)->visible;
	}

	return result;
}

//-----------------------------------------------------------------------------
void wxFrame::wxMacStartDrawing(GrafPtr& oldPort, int& savePortH, int& savePortV)
{
	::GetPort(&oldPort);
	WindowPtr theMacWindow = (WindowPtr)cMacDC->macGrafPort();
	::SetPort(theMacWindow);
	savePortH = theMacWindow->portRect.left;
	savePortV = theMacWindow->portRect.top;
	::SetOrigin(0, 0);
}

//-----------------------------------------------------------------------------
void wxFrame::wxMacStopDrawing(GrafPtr oldPort, int savePortH, int savePortV)
{
	::SetOrigin(savePortH, savePortV);
	::SetPort(oldPort);
}

//-----------------------------------------------------------------------------
// Mac platform only; internal use only.
// Based on code in 2nd edition of "Macintosh Programming Secrets"
// by Scott Knaster and Keith Rollin.
//-----------------------------------------------------------------------------
Rect wxFrame::wxMacGetContRect(void)
{// express client area in screen window c.s.
	GrafPtr oldPort;
	int savePortH, savePortV;
	wxMacStartDrawing(oldPort, savePortH, savePortV);
	WindowPtr theMacWindow = (WindowPtr)cMacDC->macGrafPort();
	Rect theContRect = theMacWindow->portRect; // client c.s.
	Point topLeftPt = {0, 0};
	::LocalToGlobal (&topLeftPt);
	::OffsetRect(&theContRect, topLeftPt.h, topLeftPt.v); // screen window c.s.
	wxMacStopDrawing(oldPort, savePortH, savePortV);

	return theContRect; // screen window c.s.
}

//-----------------------------------------------------------------------------
// Mac platform only; internal use only.
// Based on code in 2nd edition of "Macintosh Programming Secrets"
// by Scott Knaster and Keith Rollin.
//-----------------------------------------------------------------------------
Rect wxFrame::wxMacGetStrucRect(void)
{// express window area in screen window c.s.
	Rect theStrucRect;

	GrafPtr oldPort;
	int savePortH, savePortV;
	wxMacStartDrawing(oldPort, savePortH, savePortV);

	WindowPtr theMacWindow = (WindowPtr)cMacDC->macGrafPort();

	if (((WindowPeek)theMacWindow)->visible)
	{
		theStrucRect = (**(WindowPeek(theMacWindow)->strucRgn)).rgnBBox; // screen window c.s.
	}
	else
	{
		const kOffScreenLocation = 0x4000;
	
		Rect theClientRect = theMacWindow->portRect; // client c.s.
		Point thePosition = {theClientRect.top, theClientRect.left}; // client c.s.
		::LocalToGlobal(&thePosition); // screen window c.s.
		::MoveWindow(theMacWindow, thePosition.h, kOffScreenLocation, FALSE);
		::ShowHide(theMacWindow, TRUE);
		theStrucRect = (**(WindowPeek(theMacWindow)->strucRgn)).rgnBBox;
		::ShowHide(theMacWindow, FALSE);
		::MoveWindow(theMacWindow, thePosition.h, thePosition.v, FALSE);
		::OffsetRect(&theStrucRect, 0, thePosition.v - kOffScreenLocation); // screen window c.s.
	}

	wxMacStopDrawing(oldPort, savePortH, savePortV);

	return theStrucRect; // screen window c.s.
}

//-----------------------------------------------------------------------------
void wxFrame::MacUpdateWindow(void)
{
	WindowPtr theMacWindow = macWindow();
	if (theMacWindow)
	{
		SetCurrentDC();
		::BeginUpdate(theMacWindow);
		if (!::EmptyRgn(theMacWindow->visRgn))
		{
 			// Erase update region
 			// ::EraseRect(&theMacWindow->portRect);

 			// Can't use UpdateControls since each control has it's own coordinate system
 			//		::UpdateControls(theMacWindow, theMacWindow->visRgn);

			Paint();

 			// Draw the grow box
 			if (cStyle & wxRESIZE_BORDER)
 				MacDrawGrowIcon();
		}
		::EndUpdate(theMacWindow);
	}
}

 //-----------------------------------------------------------------------------
 void wxFrame::MacDrawGrowIcon(void)
 {
 	SetCurrentMacDCNoMargin();
 	// Save the clipping region
 	RgnHandle saveClip = NewRgn();
 	CheckMemOK(saveClip);
	::GetClip(saveClip);
 	// Compute the bounding rect of the grow icon
 	int theMacWidth = cWindowWidth - PlatformArea()->Margin().Offset(Direction::wxHorizontal);
 	int theMacHeight = cWindowHeight - PlatformArea()->Margin().Offset(Direction::wxVertical);
 	Rect growRect = {theMacHeight - 15, theMacWidth - 15, theMacHeight, theMacWidth};
 	// Avoid drawing scrollbar outlines
 	::ClipRect(&growRect);
 	// Draw it
 	WindowPtr theMacWindow = macWindow();
 	RGBColor fore, back;
 	::GetForeColor(&fore);
 	::GetBackColor(&back);
 	::ForeColor(blackColor);
 	::BackColor(whiteColor);
 	::EraseRect(&growRect);
 	::DrawGrowIcon(theMacWindow);
 	// Restore the clipping region
 	::SetClip(saveClip);
 	::DisposeRgn(saveClip);
 	::RGBForeColor(&fore);
 	::RGBBackColor(&back);
 }

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
char* wxFrame::GetTitle(void) // WCH: return type should be "const char*"
{
	Str255		theTitle;

	WindowPtr theMacWindow = (WindowPtr)cMacDC->macGrafPort();
	::GetWTitle(theMacWindow, theTitle);
	wxMacPtoCString(theTitle, cWindowTitle);
	return cWindowTitle;
}

//-----------------------------------------------------------------------------
void wxFrame::SetTitle(char* title)
{
	wxMacString theMacString = title;
	WindowPtr theMacWindow = (WindowPtr)cMacDC->macGrafPort();
	::SetWTitle(theMacWindow, theMacString());
}

//-----------------------------------------------------------------------------
void wxFrame::Show(Bool show)
{
	if (show == IsVisible()) {
	  if (show)
	    ::SelectWindow((WindowPtr)cMacDC->macGrafPort());
	  return;
	}
	
	cUserHidden = !show;

	if (window_parent)
	  window_parent->GetChildren()->Show(this, show);
	if (cParentArea)
	  cParentArea->Windows()->Show(this, show);

#if 0
	if (!show && IsModal())
		MakeModal(FALSE);
#endif

	WindowPtr theMacWindow = (WindowPtr)cMacDC->macGrafPort();
	if (show) {
#if __WXGARBAGE_COLLECTION_ON
	   wxTopLevelWindows(ContextWindow())->Append(this);
#else
		wxTopLevelWindows(ContextWindow())->Show(this, show);
#endif
		::ShowWindow(theMacWindow);
 		::SelectWindow(theMacWindow); 
 		if (cMacDC->currentUser() == this)
			/* b/c may be optimized for hidden: */
			cMacDC->setCurrentUser(NULL);
	} else {
#if __WXGARBAGE_COLLECTION_ON
        wxTopLevelWindows(ContextWindow())->DeleteObject(this);
#else
		wxTopLevelWindows(ContextWindow())->Show(this, show);
#endif
		::HideWindow(theMacWindow);
    }
}

//-----------------------------------------------------------------------------
Bool wxFrame::IsFrontWindow(void)
{
	WindowPtr theMacWindow = macWindow();
	return (theMacWindow ? theMacWindow == ::FrontWindow() : FALSE);
}

//-----------------------------------------------------------------------------
Bool wxFrame::IsModal(void) { return cIsModal; } //cjc, mflatt

//-----------------------------------------------------------------------------
void wxFrame::MakeModal(Bool modal)
{
	cIsModal = modal;
	// wxbWindow::MakeModal(modal);
}

//-----------------------------------------------------------------------------
wxWindow* wxFrame::GetFocusWindow(void) { return cFocusWindow; }

//-----------------------------------------------------------------------------
void wxFrame::SetFocusWindow(wxWindow* window)
{
    if (window != cFocusWindow)
    {
	    if (cFocusWindow) cFocusWindow->OnKillFocus();
	    cFocusWindow = window;
	    if (window) window->OnSetFocus();
    }
}

//-----------------------------------------------------------------------------
void wxFrame::OnActivate(Bool active)
{
	if (!cFocusWindow && children) {
		wxChildNode *node = children->First();
		while (node) {
			wxWindow *win = (wxWindow *)node->Data();
			if (win->WantsFocus() && win->CanAcceptEvent()) {
				cFocusWindow = win;
				break;
			}
			node = node->Next();
		}
	}

	if (cFocusWindow && this != cFocusWindow)
    {
		if (active)
		  cFocusWindow->OnSetFocus();
		else
		  cFocusWindow->OnKillFocus();
	}

	wxWindow::OnActivate(active);
}

//-----------------------------------------------------------------------------
void wxFrame::LoadAccelerators(char* table) { } // Not Applicable for Mac platform

//-----------------------------------------------------------------------------
void wxFrame::Paint(void)
{
	SetCurrentDC();
	RgnHandle rgn, subrgn;
	if (rgn = NewRgn()) {
		if (subrgn = NewRgn()) {
			SetRectRgn(rgn, 0, 0, cWindowWidth, cWindowHeight);
			AddWhiteRgn(subrgn);
			DiffRgn(rgn, subrgn, rgn);
			EraseRgn(rgn);
			RGBColor save;
			GetForeColor(&save);
			ForeColor(whiteColor);
			PaintRgn(subrgn);
			RGBForeColor(&save);
			DisposeRgn(subrgn);
		}
		DisposeRgn(subrgn);
	}
	wxWindow::Paint();
	if (cStatusPanel) {
	  int statusLineHeight = cStatusPanel->Height();
	  wxMargin clientAreaMargin = ClientArea()->Margin(wxScreen::gScreenWindow);
	  clientAreaMargin.SetMargin(clientAreaMargin.Offset(Direction::wxBottom) 
    	                         - statusLineHeight, 
 							   	 Direction::wxBottom);
	  ClientArea()->SetMargin(clientAreaMargin);
	  cStatusPanel->Paint();
	  clientAreaMargin.SetMargin(clientAreaMargin.Offset(Direction::wxBottom) 
    	                         + statusLineHeight, 
 							   	 Direction::wxBottom);
 	  ClientArea()->SetMargin(clientAreaMargin);
    }
 	// SetCurrentDC();
 	// If the right type of Window: MacDrawGrowIcon();
}

//-----------------------------------------------------------------------------
void wxFrame::OnChar(wxKeyEvent& event) // mac platform only
{
	if (cFocusWindow)
	{
		if (cFocusWindow != this) // kludge to prevent infinite loop
			cFocusWindow->OnChar(event);
	}
}

//-----------------------------------------------------------------------------
void wxFrame::Enable(Bool enable)
{
	wxWindow::Enable(enable);
	// Enable/disbale menubar
	if (wx_menu_bar)
		wx_menu_bar->Install();	
}

void wxFrame::ChangeToGray(Bool gray)
{
  ChildrenInternalGray(gray);
  wxWindow::ChangeToGray(gray);
}
