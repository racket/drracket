/*
 * File:	wb_rbox.h
 * Purpose:	Radio box
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_rbox.h	1.2 5/9/94" */

#ifndef wxb_rboxh
#define wxb_rboxh

#ifdef __GNUG__
#pragma interface
#endif

#ifdef IN_CPROTO
typedef       void    *wxbRadioBox ;
#else

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

// Radio box item
class wxBitmap ;
class wxbRadioBox: public wxItem
{
 public:
  int no_items;
  int selected;

  wxbRadioBox(void);
  wxbRadioBox(wxPanel *panel, wxFunction func, char *Title,
             int x = -1, int y = -1, int width = -1, int height = -1,
             int N = 0, char **Choices = NULL,
             int majorDim = 0,
             long style = 0, char *name = "radioBox");
#ifdef wx_mac
  // Constructor (given parentPanel)
  wxbRadioBox(wxPanel* parentPanel, int x, int y, int width, int height, int N,
		long style, char* windowName);
#endif // wx_mac
#ifndef __BORLANDC__
  wxbRadioBox(wxPanel *panel, wxFunction func, char *Title,
             int x = -1, int y = -1, int width = -1, int height = -1,
             int N = 0, wxBitmap **Choices = NULL,
             int majorDim = 0,
             long style = 0, char *name = "radioBox");
#endif

  ~wxbRadioBox(void);

  virtual int FindString(char *s) = 0;
  virtual void SetSelection(int N) = 0;
  virtual int GetSelection(void) = 0;
  virtual char *GetStringSelection(void);
  virtual Bool SetStringSelection(char *s);
  virtual int Number(void);
  virtual char *GetString(int N) = 0;
  // Avoids compiler warning
  inline void Enable(Bool enable) { wxWindow::Enable(enable) ; }
  virtual void Enable(int item, Bool enable) = 0; // Enable/disable specific item
  
  // Avoids compiler warning
  inline void Show(Bool show) { wxItem::Show(show) ; }
  virtual void Show(int item, Bool show) = 0; // show/unshow specific item

  void Command(wxCommandEvent& event);
  void ProcessCommand(wxCommandEvent& event);

};

#endif // IN_CPROTO
#endif // wxb_rboxh
