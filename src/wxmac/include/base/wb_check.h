/*
 * File:	wb_check.h
 * Purpose:	Check boxes
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_check.h	1.2 5/9/94" */

#ifndef wxb_checkh
#define wxb_checkh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

#ifdef IN_CPROTO
typedef void  *wxbCheckBox ;
#else

// Checkbox item (single checkbox)
class wxBitmap ;
class wxbCheckBox: public wxItem
{
 public:

  wxbCheckBox(void);
  wxbCheckBox(wxPanel *panel, wxFunction func, char *Title,
             int x = -1, int y = -1, int width = -1, int height = -1,
             long style = 0, char *name = "checkBox");
  wxbCheckBox(wxPanel *panel, wxFunction func, wxBitmap *bitmap,
             int x = -1, int y = -1, int width = -1, int height = -1,
             long style = 0, char *name = "checkBox");

#ifdef wx_mac
  // Constructor (given parentPanel)
  wxbCheckBox(wxPanel* parentPanel, int x, int y, int width, int height,
		long style, char* windowName);
#endif // wx_mac

  ~wxbCheckBox(void);

  void Command(wxCommandEvent& event);
  void ProcessCommand(wxCommandEvent& event);

  virtual void SetValue(Bool) = 0;
  virtual Bool GetValue(void) = 0;
  // Avoids compiler warning
  inline void SetLabel(char *label) { wxItem::SetLabel(label) ; }
  virtual void SetLabel(wxBitmap *bitmap) = 0;

};

#endif // IN_CPROTO
#endif // wxb_checkh
