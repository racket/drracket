/*
 * File:	wb_messg.h
 * Purpose:	Declares panel items (controls/widgets)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_messg.h	1.2 5/9/94" */

#ifndef wxb_messgh
#define wxb_messgh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_panel.h"
#include "wx_messg.h"
#include "wx_item.h"

#ifdef IN_CPROTO
typedef       void    *wxbMessage ;
#else

// Message item
class wxbMessage: public wxItem
{
 public:

  wxbMessage(void);

  wxbMessage(wxPanel *panel, char *message, int x = -1, int y = -1, long style = 0,
             char *name = "message");
#ifdef wx_mac
  // Constructor (given parentArea)
  wxbMessage(wxArea* parentArea, int x, int y, int width, int height, long style,
		char*		windowName);
 // Constructor (given parentWindow)
  wxbMessage(wxWindow* parentWindow,int x, int y, int width, int height, long style,
		char* windowName);
#endif // wx_mac

#if USE_BITMAP_MESSAGE
  wxbMessage(wxPanel *panel, wxBitmap *image, int x = -1, int y = -1, long style = 0,
             char *name = "message");
#endif
  ~wxbMessage(void);
};

#endif // IN_CPROTO
#endif // wxb_messgh
