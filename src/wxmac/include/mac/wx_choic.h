/*
 * File:	wx_choic.h
 * Purpose:	Choice panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_choic.h	1.2 5/9/94" */

#ifndef wx_choich
#define wx_choich

#include "wb_choic.h"
#include "wxLabelArea.h"
#include <Controls.h>
#include <Menus.h>

#ifdef IN_CPROTO
typedef       void    *wxChoice ;
#else

// Choice item
class wxChoice: public wxbChoice
{
	Rect		TitleRect;
	Rect		ValueRect;
	Rect		CtlRect;
	MenuHandle 	hDynMenu;
	StringPtr	sTitle;
	int			selection;
	int			labelbase;			// number pixels from top to baseline
	int			valuebase;			// ""
	wxFont*		valueFont;
	short		PopUpID;			// Mac Menu Mgr ID - never reused I hope
 public:

  wxChoice (void);
  wxChoice(wxPanel *panel, wxFunction func, char *Title,
           int x = -1, int y = -1, int width = -1, int height = -1,
           int N = 0, char **Choices = NULL,
           long style = 0, char *name = "choice"
	);
  ~wxChoice(void);

  Bool Create(wxPanel *panel, wxFunction func, char *Title,
           int x = -1, int y = -1, int width = -1, int height = -1,
           int N = 0, char **Choices = NULL,
           long style = 0, char *name = "choice");
  void Append(char *Item);
  void Clear(void);
  int GetSelection(void);
  void SetSelection(int n);
  int FindString(char *s);
  char *GetString(int n);
#if 0	// CJC
  void SetSize(int x, int y, int width, int height, int sizeFlags);
  void GetSize(int *x, int *y);
  void GetPosition(int *x, int *y);
#endif
  char *GetLabel(void);
  void SetLabel(char *label);


  void SetBackgroundColour(wxColour*col) ;
  void SetLabelColour(wxColour*col) ;
  void SetButtonColour(wxColour*col) ;

  void SetColumns(int n = 1 ) { /* no effect */ } ;
  int GetColumns(void) { return 1 ; };
  
  int Number(void);
  
  // Mac only methods
	void OnClientAreaDSize(int dW, int dH, int dX, int dY);
	void Command(wxCommandEvent& event);

	virtual void ChangeToGray(Bool gray);
	virtual void Paint(void);
	virtual void DoShow(Bool show);

	virtual void ShowAsActive(Bool flag); // mac platform only
	virtual void OnEvent(wxMouseEvent& event); // mac platform only
	void DrawChoice(Bool flag);
	void ReCalcRect(void);
};

#endif // IN_CPROTO
#endif // wx_choich
