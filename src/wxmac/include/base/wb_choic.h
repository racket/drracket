/*
 * File:	wb_choic.h
 * Purpose:	Choice items
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_choic.h	1.2 5/9/94" */

#ifndef wxb_choich
#define wxb_choich

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

#ifdef IN_CPROTO
typedef       void    *wxbChoice ;
#else

// Choice item
class wxbChoice: public wxItem
{
 public:
  int no_strings;

  wxbChoice(void);
  wxbChoice(wxPanel *panel, wxFunction func, char *Title,
           int x = -1, int y = -1, int width = -1, int height = -1,
           int N = 0, char **Choices = NULL,
           long style = 0, char *name = "choice");

  ~wxbChoice(void);

  virtual void Append(char *Item) = 0;
  virtual void Clear(void) = 0;
  virtual int GetSelection(void) = 0;
  virtual void SetSelection(int n) = 0;
  virtual int FindString(char *s) = 0;
  virtual char *GetStringSelection(void);
  virtual Bool SetStringSelection(char *s);
  virtual char *GetString(int n) = 0;
  void Command(wxCommandEvent& event);
  void ProcessCommand(wxCommandEvent& event);

};

#endif // IN_CPROTO
#endif // wxb_choich
