/*
 * File:	wb_panel.h
 * Purpose:	wxPanel subwindow, for panel items (widgets/controls)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_panel.h	1.2 5/9/94" */

#ifndef wxb_panelh
#define wxb_panelh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_win.h"
#include "wx_frame.h"
#include "wx_canvs.h"

#ifdef IN_CPROTO
typedef       void    *wxbPanel ;
#else

class wxItem;
class wxButton;
class wxPanel;
class wxColour;
class wxBrush;

#if (defined(wx_motif) && USE_PANEL_CANVAS_IN_X) || (defined(wx_msw) && USE_PANEL_CANVAS_IN_MSW) || (defined(wx_mac) && USE_PANEL_CANVAS_IN_MAC)
class wxbPanel: public wxCanvas
#else
class wxbPanel: public wxWindow
#endif
{
 public:
  Bool new_line;
  int label_position;
  wxButton *defaultItem;

  int hSpacing;
  int vSpacing;

  int current_hspacing ;
  int current_vspacing ;

  int initial_hspacing ;
  int initial_vspacing ;
  Bool has_child ;

  wxFont *labelFont ;
  wxFont *buttonFont;
  wxColour *backColour ;
  wxColour *labelColour;
  wxColour *buttonColour;

#ifndef wx_mac
  wxbPanel(void);
  wxbPanel(wxWindow *window,
          int x=-1, int y=-1, int width=-1, int height=-1, long style=0,
          char *name = "panel");
/*
  wxbPanel(wxPanel *panel,
          int x=-1, int y=-1, int width=-1, int height=-1, long style=0,
          char *name = "panel");
*/
#endif // wx_mac
#ifdef wx_mac
  // Constructor (given parentArea)
  wxbPanel(char* windowName, wxArea* parentArea, int x, int y, int width, int height,
		long style);
  // Constructor (given parentWindow)
  wxbPanel(char* windowName, wxWindow* parentWindow, int x, int y, int width, int height,
		long style);
#endif // wx_mac
  ~wxbPanel(void);

  // Set current label position, i.e. will label be on top or to the left
  void SetLabelPosition(int pos);  // wxHORIZONTAL or wxVERTICAL
  int GetLabelPosition(void);

  virtual void SetButtonFont(wxFont *font);
  virtual void SetLabelFont(wxFont *font);
  virtual void SetButtonColour(wxColour *col);
  virtual void SetLabelColour(wxColour *col);
  virtual void SetBackgroundColour(wxColour *col);

  inline virtual wxFont  *GetLabelFont(void)        { return labelFont ; }
  inline virtual wxFont  *GetButtonFont(void)       { return buttonFont ; }
  inline virtual wxColour*GetBackgroundColour(void) { return backColour ; }
  inline virtual wxColour*GetLabelColour(void)      { return labelColour ; }
  inline virtual wxColour*GetButtonColour(void)     { return buttonColour ; }

  // Start a new line
  virtual void NewLine(void) = 0;
  virtual void NewLine(int pixels) = 0;

  // Tab specified number of pixels
  virtual void Tab(void) = 0;
  virtual void Tab(int pixels) = 0;

  virtual void GetCursor(int *x, int *y) = 0;

  // Set/get horizontal spacing
  virtual void SetHorizontalSpacing(int sp) = 0;
  virtual int GetHorizontalSpacing(void) = 0;

  // Set/get vertical spacing
  virtual void SetVerticalSpacing(int sp) = 0;
  virtual int GetVerticalSpacing(void) = 0;

  // Update next cursor position
  virtual void AdvanceCursor(wxWindow *item) = 0;

#ifndef wx_mac
  // If x or y are not specified (i.e. < 0), supply
  // values based on left to right, top to bottom layout.
  // Internal use only.
  virtual void GetValidPosition(int *x, int *y) = 0;
#endif // wx_mac

  inline virtual wxButton *GetDefaultItem(void) { return defaultItem; }

  wxObject *GetChild(int number) ;

  // Override to define new behaviour for default action (e.g. double clicking
  // on a listbox)
  virtual void OnDefaultAction(wxItem *initiatingItem);

#if USE_WX_RESOURCES
  /*
   * Optional resource loading facility
   */

  Bool LoadFromResource(wxWindow *parent, char *resourceName);
#endif
#ifdef wx_mac
// Private methods
private:
	void InitDefaults(void);
	void InitMoreDefaults(void); // Poor name for this method

#endif // wx_mac
};

#endif // IN_CPROTO
#endif // wxb_panelh
