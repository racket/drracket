/*
 * File:	wx_group.h
 * Purpose:	wxGroupBox (X implementation)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_group.h	1.2 5/9/94" */

#ifndef wx_grouph
#define wx_grouph

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_win.h"
#include "wx_panel.h"
#include "wb_group.h"

#ifdef IN_CPROTO
typedef       void    *wxGroupBox ;
#else

// Group box
class wxGroupBox: public wxbGroupBox
{
 public:
  wxGroupBox(void);
  wxGroupBox(wxPanel *panel, char *label, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, char *name = "group");
  ~wxGroupBox(void);

  Bool Create(wxPanel *panel, char *label, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, char *name = "group");

//  void SetSize(int x, int y, int width, int height,int sizeFlags);
  void SetLabel(char *);
  char *GetLabel(void);
};

#endif // IN_CPROTO
#endif // wx_grouph
