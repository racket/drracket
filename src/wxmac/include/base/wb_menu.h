/*
 * File:	wb_menu.h
 * Purpose:	Declares panel items (controls/widgets)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

#ifndef wxb_menuh
#define wxb_menuh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_item.h"
#include "wx_mnuit.h"

#ifdef IN_CPROTO
typedef       void    *wxbMenu ;
typedef       void    *wxbMenuBar;
#else

class wxMenuBar;
class wxMenu;

// Menu
class wxbMenu: public wxItem
{
 public:
  int no_items;
  char *title;
  wxMenu *top_level_menu;
  wxMenuBar *menu_bar;
  wxList menuItems;

  wxbMenu(char *Title = NULL, wxFunction func = NULL);
#ifdef wx_mac
  // Constructor (given Title)
  wxbMenu (char* Title, char* windowName);
#endif
  ~wxbMenu(void);
  
  virtual void AppendSeparator(void) = 0;
  virtual void Append(int id, char *Label, char *helpString = NULL,Bool checkable=FALSE) = 0;
  virtual void Append(int id, char *Label, wxMenu *SubMenu, char *helpString = NULL) = 0;
  // Avoids compiler warning
  inline void Enable(Bool enable) { wxWindow::Enable(enable) ; }
  virtual void Enable(int id, Bool Flag) = 0;
  virtual void Check(int id, Bool Flag) = 0;
  virtual Bool Checked(int id) = 0;
  virtual void SetHelpString(int id, char *helpString);
  virtual char *GetHelpString(int id);

  // Finds the item id matching the given string, -1 if not found.
  virtual int FindItem(char *itemString);

  // Find wxMenuItem for item ID, and return item's
  // menu too if itemMenu is non-NULL.
  wxMenuItem *FindItemForId(int itemId, wxMenu **itemMenu = NULL);

  void ProcessCommand(wxCommandEvent& event);

};

// Menu Bar (a la Windows)
class wxFrame;
class wxbMenuBar: public wxItem
{
 public:
  int n;
  wxMenu **menus;
  char **titles;
  wxFrame *menu_bar_frame;

  wxbMenuBar(void);
  wxbMenuBar(int n, wxMenu *menus[], char *Titles[]);
#ifdef wx_mac
  // Constructor (given objectType)
  wxbMenuBar(char* windowName);
  // Constructor (given Menus)
  wxbMenuBar(int N, wxMenu* Menus[], char* Titles[], char* windowName);
#endif
  ~wxbMenuBar(void);

  virtual void Append(wxMenu *menu, char *title);
  virtual void Delete(wxMenu *menu, int index = 0); // mflatt
  virtual Bool OnAppend(wxMenu *menu, char *title) = 0; // mflatt
  virtual Bool OnDelete(wxMenu *menu, int index) = 0; // mflatt

  // Avoids compiler warning
  inline void Enable(Bool enable) { wxWindow::Enable(enable) ; }

  // Must only be used AFTER menu has been attached to frame,
  // otherwise use individual menus to enable/disable items
  virtual void Enable(int Id, Bool Flag) = 0;
  virtual void EnableTop(int pos, Bool Flag) = 0;
  virtual void Check(int Id, Bool Flag) = 0;
  virtual Bool Checked(int id) = 0;
  virtual void SetHelpString(int Id, char *helpString);
  virtual char *GetHelpString(int Id);

  virtual int FindMenuItem(char *menuString, char *itemString);

  // Find wxMenuItem for item ID, and return item's
  // menu too if itemMenu is non-NULL.
  wxMenuItem *FindItemForId(int itemId, wxMenu **menuItem = NULL);
};

#endif // IN_CPROTO
#endif // wxb_menuh
