/*
 * File:	wb_slidr.h
 * Purpose:	Slider
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_slidr.h	1.2 5/9/94" */

#ifndef wxb_slidrh
#define wxb_slidrh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

#ifdef IN_CPROTO
typedef       void    *wxbSlider ;
#else

// Slider
class wxbSlider: public wxItem
{
 public:
  wxbSlider(void);
  wxbSlider(wxPanel *panel, wxFunction func, char *label, int value,
           int min_value, int max_value, int width, int x = -1, int y = -1,
           long style = 0, char *name = "slider");
#ifdef wx_mac
  wxbSlider (wxPanel * panel, char *label, int value, int min_value, int max_value, 
		int width, int x, int y, long style, char *windowName);
#endif

  ~wxbSlider(void);

  virtual int GetValue(void) = 0;
  virtual void SetValue(int) = 0;

  void Command(wxCommandEvent& event);
  void ProcessCommand(wxCommandEvent& event);

};

#endif // IN_CPROTO
#endif // wxb_slidrh
