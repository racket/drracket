/*
 * File:	wb_txt.h
 * Purpose:	Text panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_txt.h	1.2 5/9/94" */

#ifndef wxb_txth
#define wxb_txth

#ifdef __GNUG__
#pragma interface
#endif

#ifdef wx_mac
#include "wx_item.h"
class wxPanel;
class wxTextWindow;
#else // wx_mac
#include "common.h"
#include "wx_panel.h"
#include "wx_txt.h"
#endif // wx_mac

#ifdef IN_CPROTO
typedef       void    *wxbText ;
#else

// Single-line text item
class wxbText: public wxItem
{
 public:
  wxbText(void);
  wxbText(wxPanel *panel, wxFunction func, char *label, char *value = "",
         int x = -1, int y = -1, int width = -1, int height = -1,
         long style = 0, char *name = "text");
#ifdef wx_mac
  // Constructor (given parentWindow)
  wxbText(wxWindow*	parentWindow, int x, int y, int width, int height,
		long style, char* windowName);
#endif // wx_mac

  ~wxbText(void);

  virtual char *GetValue(void) = 0;
  virtual void SetValue(char *value) = 0;

  // Clipboard operations
  virtual void Copy(void) = 0;
  virtual void Cut(void) = 0;
  virtual void Paste(void) = 0;

  void Command(wxCommandEvent& event);
  void ProcessCommand(wxCommandEvent& event);

};

#endif // IN_CPROTO
#endif // wxb_txth
