/*
 * File:	wb_text.cc
 * Purpose:	wxTextWindow implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_text.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "@(#)wb_text.cc	1.2 5/9/94"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_main.h"
#include "wx_text.h"
#include "wx_utils.h"
#include "wx_frame.h"
#include "wx_gdi.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#ifdef wx_mac
#include <iostream.h>
#include <fstream.h>
#include <stdio.h>
#endif

#ifndef wx_mac
wxbTextWindow::wxbTextWindow(void)
{
  __type = wxTYPE_TEXT_WINDOW;
  file_name = NULL;
  window_parent = NULL;
  font = wxSWISS_FONT;
}

wxbTextWindow::wxbTextWindow(wxWindow *parent, int x, int y, int width, int height,
                           long style, char *name)
{
  __type = wxTYPE_TEXT_WINDOW;
  windowStyle = style;
  file_name = NULL;
  window_parent = parent;
  font = wxSWISS_FONT;
}

wxbTextWindow::~wxbTextWindow(void)
{
}
#endif // wx_mac

wxbTextWindow& wxbTextWindow::operator<<(char *s)
{
  WriteText(s);
  return *this;
}

wxbTextWindow& wxbTextWindow::operator<<(float f)
{
  static char buf[100];
  sprintf(buf, "%.2f", f);
  WriteText(buf);
  return *this;
}

wxbTextWindow& wxbTextWindow::operator<<(double d)
{
  static char buf[100];
  sprintf(buf, "%.2f", d);
  WriteText(buf);
  return *this;
}

wxbTextWindow& wxbTextWindow::operator<<(int i)
{
  static char buf[100];
  sprintf(buf, "%i", i);
  WriteText(buf);
  return *this;
}

wxbTextWindow& wxbTextWindow::operator<<(long i)
{
  static char buf[100];
  sprintf(buf, "%ld", i);
  WriteText(buf);
  return *this;
}

wxbTextWindow& wxbTextWindow::operator<<(char c)
{
  char buf[2];

  buf[0] = c;
  buf[1] = 0;
  WriteText(buf);
  return *this;
}

#ifdef wx_mac
//=============================================================================
// Protected constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxbTextWindow::wxbTextWindow // Constructor (given parentArea)
	(
		WXTYPE		objectType,
		char*		windowName,
		wxArea*		parentArea,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style
	) :
		wxWindow (windowName, parentArea, x, y, width, height, style),
		file_name (NULL)
{
	font = wxSWISS_FONT;
}

//-----------------------------------------------------------------------------
wxbTextWindow::wxbTextWindow // Constructor (given parentWindow)
	(
		WXTYPE		objectType,
		char*		windowName,
		wxWindow*	parentWindow,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style
	) :
		wxWindow (windowName, parentWindow, x, y, width, height, style),
		file_name (NULL)
{
	font = wxSWISS_FONT;
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxbTextWindow::~wxbTextWindow(void)
{
	if (file_name) delete[] file_name;
}

#endif // wx_mac
