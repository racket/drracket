/*
 * File:	wb_mtxt.h
 * Purpose:	Multitext item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_mtxt.h	1.2 5/9/94" */

#ifndef wxb_mtxth
#define wxb_mtxth

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_panel.h"
#include "wx_txt.h"

#ifdef IN_CPROTO
typedef       void    *wxbMultiText ;
#else

// Multi-line text item
class wxbMultiText: public wxText
{
 public:
 
  wxbMultiText(void);
  wxbMultiText(wxPanel *panel, wxFunction func, char *label, char *value = "",
         int x = -1, int y = -1, int width = -1, int height = 50,
         long style = 0, char *name = "listBox");
#ifdef wx_mac
  // Constructor (given parentTextWindow)
  wxbMultiText(wxTextWindow* parentTextWindow, wxFunction function, char* label, char* value,
		int x, int y, int width, int height,
		long style, char* windowName);
#endif // wx_mac

  void Command(wxCommandEvent& event);
  void ProcessCommand(wxCommandEvent& event);

//  virtual char *GetValue(void) = 0;
//  virtual void GetValue(char *buffer, int maxLen) = 0;

};

#endif // IN_CPROTO
#endif // wxb_mtxth
