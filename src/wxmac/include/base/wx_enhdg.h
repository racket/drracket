/*
 * File:	wx_enhdg.h
 * Purpose:	wxEnhancedDialogBox
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_enhdg.h	1.2 5/9/94" */

#ifndef wx_enhdgh
#define wx_enhdgh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_setup.h"

#if USE_ENHANCED_DIALOG
#include "wx_item.h"
#include "wx_check.h"
#include "wx_messg.h"
#include "wx_dialg.h"

#ifdef IN_CPROTO
typedef	void	*wxEnhDialogBox;
#else

// Enhanced Dialog boxes
class wxEnhDialogBox: public wxDialogBox
{
 public:
  wxPanel *panel ;
#ifndef wx_xview
  wxPanel *pinPanel;
#endif
  long style ;
  Bool pinned ;
  wxCheckBox *pinCheck ;
  wxMessage *statusText ;
  wxPanel *userPanel;
  wxPanel *secondaryPanel;
  wxPanel *cmdPanel ;
  wxPanel *statusPanel;
  wxFunction unshow ;
  int userSpacing ;
  int maxWidth;
  int maxHeight ;

  // 0,0,10,10 to be sure that resize works.
  wxEnhDialogBox(wxFrame *frame, char *title, Bool modal = FALSE,
              wxFunction fun = NULL,int space = -1 ,
              int x = 0, int y = 0,
              int width = 10, int height = 10,
	      long style = wxENH_DEFAULT , char *name = "Shell" );
  ~wxEnhDialogBox();

  void SetStatus(char *label=NULL) ;
  wxButton* AddCmd(char *label,wxFunction fun=NULL,int tag = 0) ;
  wxButton* AddCmd(wxBitmap *bitmap,wxFunction fun=NULL,int tag = 0) ;
  wxButton *GetCmd(int number) ;
  void SetPin(Bool flag) ;
  void Show(Bool show,Bool flag = FALSE) ;
  void PreFit(void);	 // Get a secondary wxPanel
  void PrimaryFit(void); // Internal use only
  void Fit(void);	 // Final Fitting
} ;

#endif // IN_CPROTO
#endif // USE_ENHANCED_DIALOG
#endif // wx_enhdgh
