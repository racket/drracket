/*
 * File:	wb_buttn.h
 * Purpose:	Buttons
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_buttn.h	1.2 5/9/94" */

#ifndef wxb_buttnh
#define wxb_buttnh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

#ifdef IN_CPROTO
typedef       void    *wxbButton ;
#else

// Pushbutton
class wxBitmap;
class wxbButton: public wxItem
{
 public:

  wxbButton(void);
  wxbButton(wxPanel *panel, wxFunction func, char *label, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, char *name = "button");
  wxbButton(wxPanel *panel, wxFunction func, wxBitmap *bitmap, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, char *name = "button");
#ifdef wx_mac
  wxbButton (wxPanel* parentPanel, int x, int y,
  		int width, int height, long	style, char* windowName);
#endif
  ~wxbButton(void);

  void Command(wxCommandEvent& event);
  void ProcessCommand(wxCommandEvent& event);

#ifdef wx_mac
  virtual void SetDefault(Bool flag = TRUE) = 0;
#else // wx_mac
  virtual void SetDefault(void) = 0;
#endif // wx_mac
  // Avoids compiler warning
  inline  void SetLabel(char *label) { wxItem::SetLabel(label) ; }
  virtual void SetLabel(wxBitmap *bitmap) = 0;

};

#endif // IN_CPROTO
#endif // wxb_buttnh
