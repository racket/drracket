/*
 * File:	wb_main.h
 * Purpose:	wxApp declaration and a few other functions.
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_main.h	1.2 5/9/94" */

#ifndef wxb_mainh
#define wxb_mainh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_obj.h"
#include "wx_mgstr.h"

// Here's a macro you can use if your compiler
// really, really wants main() to be in your main program
// (e.g. hello.cc)

#if defined(AIX) || defined(AIX4)
#define IMPLEMENT_WXWIN_MAIN int main(int argc, char *argv[]) { return wxEntry(argc, argv); }
#else
#define IMPLEMENT_WXWIN_MAIN
#endif


#ifdef IN_CPROTO
typedef       void    *wxbApp ;
#else

class wxFrame;
class wxApp ;

// Represents the application. Derive OnInit and declare
// a new App object to start application
class wxbApp: public wxObject
{
 public:
  int wantDebugOutput ;
  char *wx_class;
  wxFrame *wx_frame;
  int argc;
  char **argv;
  Bool death_processed;
  void (*work_proc)(wxApp*app) ; // work procedure

  wxbApp(wxlanguage_t language = wxLANGUAGE_DEFAULT);
  ~wxbApp(void);
  virtual wxFrame *OnInit(void);
  virtual int OnExit(void);
  virtual int MainLoop(void) = 0;
  virtual Bool Initialized(void);
  virtual Bool Pending(void) = 0 ;
  virtual void Dispatch(void) = 0 ;
};

extern wxApp *wxTheApp;

void wxCleanUp(void);
void wxCommonCleanUp(void); // Call this from the platform's wxCleanUp()
void wxCommonInit(void);    // Call this from the platform's initialization

// Force an exit from main loop
void wxExit(void);

// Yield to other apps/messages
Bool wxYield(void);

#endif // IN_CPROTO
#endif
