/*
 * File:	wb_frame.cc
 * Purpose:	wxFrame implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_frame.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "%W% %G%"; */

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_frame.h"
#include "wx_gdi.h"
#include "wx_stdev.h"
#include "wx_main.h"
#include "wx_utils.h"
#include "wx_menu.h"
#include "wx_mnuit.h"
#ifdef wx_mac
#include "wx_screen.h"
#endif
// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"
 
#include "wx_stdev.h"

#include <iostream.h>

#ifndef wx_mac
wxbFrame::wxbFrame(void)
{
  __type = wxTYPE_FRAME;
  nb_status = 0;
  frameToolBar = NULL ;
}

wxbFrame::wxbFrame(wxFrame *Parent, char *title, int x, int y,
                 int width, int height, long style, char *name)
{
  __type = wxTYPE_FRAME;
  windowStyle = style;
  frameToolBar = NULL ;

  WXGC_IGNORE(context);
  context = wxGetContextForFrame();
}

Bool wxbFrame::Create(wxFrame *Parent, char *title, int x, int y,
                 int width, int height, long style, char *name)
{
  windowStyle = style;

  WXGC_IGNORE(context);
  context = wxGetContextForFrame();

  if (!Parent) {
    wxTopLevelWindows(ContextWindow())->Append(this);
    wxTopLevelWindows(ContextWindow())->Show(this, FALSE);
  }
  return TRUE;
}
#endif // wx_mac

#ifdef wx_mac
//-----------------------------------------------------------------------------
// Constructor (given parentScreen)
wxbFrame::wxbFrame (char* windowName, wxScreen* parentScreen,
		int x, int y, int width, int height, long style)
	:
		wxWindow ( windowName, parentScreen, x, y, width, height, style),
		frame_type (style & (wxSDI | wxMDI_PARENT | wxMDI_CHILD)),
		icon (NULL),
		modal_showing (FALSE),
		nb_status (0),
		status_line_exists (FALSE),
		wx_menu_bar (NULL)
{
	__type = wxTYPE_FRAME;

	WXGC_IGNORE(context);
	context = wxGetContextForFrame();

    wxTopLevelWindows(ContextWindow())->Append(this);
    wxTopLevelWindows(ContextWindow())->Show(this, FALSE);
}

#endif // wx_mac

wxbFrame::~wxbFrame(void)
{
  wxTopLevelWindows(ContextWindow())->DeleteObject(this);
}

// Default resizing behaviour - if only ONE subwindow,
// resize to client rectangle size
void wxbFrame::OnSize(int x, int y)
{
#if DEBUG > 1
  wxDebugMsg("wxbFrame::OnSize\n");
#endif
  if (frame_type == wxMDI_PARENT)
    return;

  // Search for a child which is a subwindow, not another frame.
  wxWindow *child = NULL;
  // Count the number of _subwindow_ children
  int noChildren = 0;
  for(wxChildNode *node = GetChildren()->First(); node; node = node->Next())
  {
    wxWindow *win = (wxWindow *)node->Data();
    WXTYPE winType = win->__type;

    if (wxSubType(winType, wxTYPE_PANEL) ||
        wxSubType(winType, wxTYPE_TEXT_WINDOW) ||
        wxSubType(winType, wxTYPE_CANVAS))
    {
      child = win;
      noChildren ++;
    }
  }
  if (!child || (noChildren > 1))
    return;

  int client_x, client_y;

#if DEBUG > 1
  wxDebugMsg("wxbFrame::OnSize: about to set the child's size.\n");
#endif

  GetClientSize(&client_x, &client_y);
  child->SetSize(0, 0, client_x, client_y, 0x70);
}

// Default activation behaviour - set the focus for the first child
// subwindow found.
void wxbFrame::OnActivate(Bool flag)
{
  wxWindow::OnActivate(flag); //GRW

  if (!flag)
    return;

  for(wxChildNode *node = GetChildren()->First(); node; node = node->Next())
  {
    // Find a child that's a subwindow, but not a dialog box.
    wxWindow *child = (wxWindow *)node->Data();
    if ((wxSubType(child->__type, wxTYPE_PANEL) &&
         !wxSubType(child->__type, wxTYPE_DIALOG_BOX)) ||
        wxSubType(child->__type, wxTYPE_TEXT_WINDOW) ||
        wxSubType(child->__type, wxTYPE_CANVAS))
    {
#if DEBUG > 1
      wxDebugMsg("wxbFrame::OnActivate: about to set the child's focus.\n");
#endif
      child->SetFocus();
      return;
    }
  }
}

// Default menu selection behaviour - display a help string
void wxbFrame::OnMenuSelect(int id)
{
  if (StatusLineExists())
  {
    if (id == -1)
      SetStatusText("");
    else
    {
      wxMenuBar *menuBar = GetMenuBar();
      if (menuBar)
      {
        char *helpString = GetMenuBar()->GetHelpString(id);
        if (helpString)
          SetStatusText(helpString);
      }
    }
  }
}

#ifdef wx_mac // The following belongs in this file! Moved from wb_item.cc
void wxbFrame::SetMenuBar(wxMenuBar *menu_bar)
{
}
#endif // wx_mac

wxMenuBar *wxbFrame::GetMenuBar(void)
{
  return wx_menu_bar;
}

Bool wxbFrame::StatusLineExists(void)
{
  return status_line_exists;
}

void wxbFrame::Centre(int direction)
{
  int display_width, display_height, width, height, x, y;
  wxDisplaySize(&display_width, &display_height);

  GetSize(&width, &height);
  GetPosition(&x, &y);

  if (direction & wxHORIZONTAL)
    x = (int)((display_width - width)/2);
  if (direction & wxVERTICAL)
    y = (int)((display_height - height)/2);

  SetSize(x, y, width, height, wxPOS_USE_MINUS_ONE);
}

// Call this to simulate a menu command
void wxbFrame::Command(int id)
{
  ProcessCommand(id);
}

void wxbFrame::ProcessCommand(int id)
{
  wxCommandEvent *_commandEvent = new wxCommandEvent(wxEVENT_TYPE_MENU_COMMAND);
  wxCommandEvent &commandEvent = *_commandEvent;
  
  commandEvent.commandInt = id;

  if (wxNotifyEvent(commandEvent, TRUE))
    return;

  wxMenuBar *bar = GetMenuBar() ;
  if (!bar)
    return;

  // Motif does the job by itself!!
#ifndef wx_motif
  wxMenuItem *item = bar->FindItemForId(id) ;
#ifdef wx_mac
  if (item && item->IsCheckable())
  {
    bar->Check(id,!bar->Checked(id)) ;
  }
#else // wx_mac
  if (item && item->checkable)
  {
//wxDebugMsg("Toggling id %d\n",id) ;
    bar->Check(id,!bar->Checked(id)) ;
  }
#endif // wx_mac

#endif
  OnMenuCommand(id);

  wxNotifyEvent(commandEvent, FALSE);
}

