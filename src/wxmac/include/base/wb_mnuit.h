/*
 * File:	wb_mnuit.h
 * Purpose:	Declares menu item class
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_mnuit.h	1.2 5/9/94" */

#ifndef wxb_mnuith
#define wxb_mnuith

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_obj.h"

#ifdef IN_CPROTO
typedef       void    *wbMenuItem ;
#else

class wxMenu;
class wxMenuBar;
class wbMenuItem: public wxObject
{
 public:
  wxMenuBar *menuBar;
  int itemId;
  char *itemName;
  char *helpString;
  wxMenu *subMenu;
  wxMenu *topMenu;
  inline wbMenuItem(void)
  { itemId = 0; itemName = NULL; topMenu = NULL; subMenu = NULL;
    menuBar = NULL; helpString = NULL; }
  inline ~wbMenuItem(void) { if (itemName) delete[] itemName; if (helpString) delete[] helpString; }
};

#endif // IN_CPROTO
#endif // wxb_mnuith

