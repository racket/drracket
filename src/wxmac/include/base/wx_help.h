/*
 * File:	wx_help.h
 * Purpose:	API for invoking wxHelp
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_help.h	1.2 5/9/94" */

#ifndef wxb_helph
#define wxb_helph

#ifdef __GNUG__
#pragma interface
#endif

#include <stdio.h>
#include "wx.h"
#if USE_HELP

#include <wx_ipc.h>

#ifdef IN_CPROTO
typedef       void    *wxHelpConnection ;
typedef       void    *wxHelpInstance;
#else

class wxHelpInstance;
class wxHelpConnection: public wxConnection
{
 public:
  wxHelpInstance *helpInstance;
  wxHelpConnection(wxHelpInstance *instance);
  Bool OnDisconnect(void);
};

// An application can have one or more instances of wxHelp,
// represented by an object of this class.
// Nothing happens on initial creation; the application
// must call a member function to display help.
// If the instance of wxHelp is already active, that instance
// will be used for subsequent help.

class wxHelpInstance: public wxClient
{
 public:
  char *helpFile;
  int  helpServer;
  char *helpHost;
  Bool helpRunning;
  Bool useNative; // Use native help system if possible
  wxHelpConnection *helpConnection;
  wxHelpInstance(Bool native = FALSE);
  ~wxHelpInstance(void);

  // Must call this to set the filename and server name
  Bool Initialize(char *file, int server = -1);
  // If file is NULL, reloads file given  in Initialize
  Bool LoadFile(char *file = NULL);
  Bool DisplayContents(void);
  Bool DisplaySection(int sectionNo);
  Bool DisplayBlock(long blockNo);
  Bool KeywordSearch(char *k);

  Bool Quit(void);
  virtual void OnQuit(void);

  // Private
  Bool Run(void);

  inline wxConnection *OnMakeConnection(void)
    { return new wxHelpConnection(this); 
    }
};

#endif // IN_CPROTO
#endif // USE_HELP
#endif // wxb_helph
