/*
 * File:	wb_dialg.h
 * Purpose:	wxDialogBox and common dialog declarations
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_dialg.h	1.2 5/9/94" */

#ifndef wxb_dialgh
#define wxb_dialgh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_panel.h"

#ifdef IN_CPROTO
typedef       void    *wxbDialogBox ;
#else

// Dialog boxes
#ifdef wx_mac
class wxbDialogBox: public wxFrame
#else
class wxbDialogBox: public wxPanel
#endif
{
 public:
  Bool modal;
  Bool is_show;

  wxbDialogBox(void);
  wxbDialogBox(wxWindow *parent, char *title, Bool modal = FALSE,
              int x = -1, int y = -1,
              int width = -1, int height = -1, long style = wxDEFAULT_DIALOG_STYLE, char *name = "panel");
  ~wxbDialogBox();

  Bool Create(wxWindow *window, char *title, Bool modal = FALSE,
              int x = -1, int y = -1,
              int width = -1, int height = -1, long style = wxDEFAULT_DIALOG_STYLE, char *name = "panel");

  virtual void Iconize(Bool iconize) = 0;
  virtual Bool Iconized(void) = 0;

  void Centre(int direction = wxBOTH);
};

// Handy dialog functions
char *wxGetTextFromUser(char *message, char *caption = "Input text",
                        char *default_value = "", wxWindow *parent = NULL,
                        int x = -1, int y = -1, Bool centre = TRUE);

#define wxCHOICE_HEIGHT 150
#define wxCHOICE_WIDTH 200

char *wxGetSingleChoice(char *message, char *caption,
                        int n, char *choices[], wxWindow *parent = NULL,
                        int x = -1, int y = -1, Bool centre = TRUE,
                        int width = wxCHOICE_WIDTH, int height = wxCHOICE_HEIGHT);

// Same as above but gets position in list of strings, instead of string,
// or -1 if no selection
int wxGetSingleChoiceIndex(char *message, char *caption,
                           int n, char *choices[], wxWindow *parent = NULL,
                           int x = -1, int y = -1, Bool centre = TRUE,
                           int width = wxCHOICE_WIDTH, int height = wxCHOICE_HEIGHT);

// Return client data instead
char *wxGetSingleChoiceData(char *message, char *caption,
                            int n, char *choices[], char *client_data[],
                            wxWindow *parent = NULL, int x = -1, int y = -1,
                            Bool centre = TRUE,
                            int width = wxCHOICE_WIDTH, int height = wxCHOICE_HEIGHT);
                           
int wxGetMultipleChoice(char *message, char *caption,
			  int n, char *choices[], 
			  int nsel, int * selection,
			  wxWindow *parent = NULL, int x = -1 , int y = -1, Bool centre = TRUE,
			  int width = wxCHOICE_WIDTH, int height = wxCHOICE_HEIGHT);

// type is an 'or' (|) of wxOK, wxCANCEL, wxYES_NO
// Returns wxYES/NO/OK/CANCEL
int wxbMessageBox(char *message, char *caption = "Message", long style = wxOK|wxCENTRE,
  wxWindow *parent = NULL, int x = -1, int y = -1);

#define wxOPEN 1
#define wxSAVE 2
#define wxOVERWRITE_PROMPT 4
#define wxHIDE_READONLY 8
#define wxDIR_ONLY	16
#define wxFILES_ONLY 32

// Generic file load dialog
char * wxLoadFileSelector(char *what = "Text", char *extension = "txt", char *default_name = NULL);

// Generic file save dialog
char * wxSaveFileSelector(char *what = "Text", char *extension = "txt", char *default_name = NULL);
// File selector
char *wxFileSelector(char *message = "Select a file", char *default_path = NULL,
                     char *default_filename = NULL, char *default_extension = NULL,
                     char *wildcard = "*.*", int flags = 0,
                     wxWindow *parent = NULL, int x = -1, int y = -1);
// Select a Directory
char *wxDirectorySelector(char *message = "Select a file", char *default_path = NULL,
                     char *default_filename = NULL, char *default_extension = NULL,
                     char *wildcard = "*.*", int flags = wxOPEN|wxDIR_ONLY,
                     wxWindow *parent = NULL, int x = -1, int y = -1);

#endif // IN_CPROTO
#endif // wxb_dialgh
