/*
 * File:	wb_win.cc
 * Purpose:	wxWindow class implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_win.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "@(#)wb_win.cc	1.2 5/9/94"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_win.h"
#include "wx_gdi.h"
#include "wx_utils.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

// Constructor
wxbWindow::wxbWindow(void)
{
  __type = wxTYPE_WINDOW;
  windowStyle = 0;
  wx_client_data = NULL;
#ifndef wx_mac
  window_parent = NULL;
#endif
  font = NULL;
  handle = NULL;
  windowName = NULL;
  callback = 0;
  wx_cursor = wxSTANDARD_CURSOR;
#ifndef wx_mac
  children = new wxChildList;
#endif
  paintingEnabled = TRUE;
  doubleClickAllowed = 0 ;
  winCaptured = FALSE;
}

#ifdef wx_mac
void wxbWindow::InitDefaults(void)
{
 	__type = wxTYPE_WINDOW;
	windowStyle = 0;
	doubleClickAllowed = 0 ;
	paintingEnabled = TRUE;
	wx_client_data = NULL;
	handle = NULL;
	wx_cursor = wxSTANDARD_CURSOR;
	callback = NULL;
	font = NULL;
}

wxbWindow::wxbWindow // Constructor
	(
		char*		windowName
	) :
		wxObject (),
		windowName(copystring(windowName))
{
	InitDefaults();
}
#endif // wx_mac

// Destructor
wxbWindow::~wxbWindow(void)
{
  if (windowName) delete[] windowName;
}

char *wxbWindow::GetHandle(void)
{
  return handle;
}

// General callback setting
void wxbWindow::Callback(wxFunction Function)
{
  if (Function)
    callback = Function;
}

// Client data handling (any window, item etc.)
void wxbWindow::SetClientData(char *data)
{
  wx_client_data = data;
}

char *wxbWindow::GetClientData(void)
{
  return wx_client_data;
}

#ifndef wx_mac
wxWindow *wxbWindow::GetParent(void)
{
  return window_parent;
}

wxWindow *wxbWindow::GetGrandParent(void)
{
  if (window_parent)
    return window_parent->window_parent;
  return NULL;
}

void wxbWindow::AddChild(wxObject *child)
{
  children->Append(child);
}

void wxbWindow::RemoveChild(wxObject *child)
{
  if (children)
    children->DeleteObject(child);
}

void wxbWindow::DestroyChildren(void)
{
  if (children) {
    wxChildNode *node;
    while ((node = children->First()) != (wxNode *)NULL) {
      wxWindow *child;
      if ((child = (wxWindow *)node->Data()) != (wxWindow *)NULL) {
//      child->DestroyChildren();
        delete child;
      }
    } /* while */
  }
}
#endif // wx_mac

void wxbWindow::MakeModal(Bool modal)
{
  // Disable all other windows
  if (wxSubType(__type, wxTYPE_DIALOG_BOX) || wxSubType(__type, wxTYPE_FRAME))
  {
    wxChildNode *node = wxTopLevelWindows(ContextWindow())->First();
    while (node)
    {
      wxWindow *win = (wxWindow *)node->Data();
      if (win != this)
        win->Enable(!modal);

      node = node->Next();
    }
  }
}

void wxbWindow::SetName(char *name)
{
  if (windowName)
    delete[] windowName;
  if (name)
    windowName = copystring(name);
  else
    windowName = NULL;
}

wxWindow *wxbWindow::ContextWindow()
{
	if (wxSubType(__type, wxTYPE_FRAME))
		return (wxWindow *)this;
	if (wxSubType(__type, wxTYPE_DIALOG_BOX))
		return ((wxDialogBox *)this)->cFrame;
	return NULL;
}
