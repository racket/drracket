///////////////////////////////////////////////////////////////////////////////
// File:	wx_main.cc
// Purpose:	wxApp implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";
#include <Windows.h>
#include <iostream.h>
#include <stdlib.h>

#include "wx_main.h"
#include "wx_list.h"
#include "wx_utils.h"
#include "wx_frame.h"
#include "wxMacDC.h"

void wxCleanUp(void);

///////////////////////////////////////////////////////////////////////////////
// The procedure CreateApp initializes the whole application.
// You must declare and define it in the file that
// implements your derived class of the base class wxApp.
// It will look like this, where MyApp is the name of your derived class.
// You must use the name CreateApp for this procedure;
// you must use the name you chose for your derived class,
// which does not have to be MyApp.
//
//	void CreateApp(void);	// This procedure initializes the whole application
//	void CreateApp(void)
//	{
//		new MyApp;
//	}
//
///////////////////////////////////////////////////////////////////////////////
void CreateApp(void);

extern "C" {
  extern char *scheme_getcwd(char *buf, int buflen, int *actlen, int noexn);
  extern char *scheme_build_mac_filename(FSSpec *spec, int given_dir);
};


char *wxmac_startup_directory;

extern void wxInitResources(char *s);

//-----------------------------------------------------------------------------
int wxEntry(int argc, char* argv[])
{
	// CreateApp();	// This procedure initializes the whole application

	if (!wxTheApp)
	{
		cerr << "wxWindows error: You have to define an instance of wxApp!\n";
		exit(0);
	}

	wxTheApp->argc = argc;
	wxTheApp->argv = argv;

//	if (!wxTheApp->wx_class) wxTheApp->wx_class = macCopyString(argv[0]);

	wxmac_startup_directory = scheme_getcwd(NULL, 0, NULL, 1);
	
	FSSpec spec;
	if (!FindFolder(kOnSystemDisk, 'pref', kCreateFolder, &spec.vRefNum, &spec.parID)) {
	  char *home = scheme_build_mac_filename(&spec, 1);
	  int l = strlen(home);
	  char *s = new char[l + 15];
	  memcpy(s, home, l);
	  if (s[l - 1] != ':') {
	    s[l++] = ':';
	  }
	  strcpy(s + l, "mred.fnt");
      wxInitResources(s);
    }

	wxCommonInit();
	wxInitializePrintSetupData(1);

	wxTheApp->wx_frame = wxTheApp->OnInit();
	
	// Simulate a OnSize event for the frames - CJC
	for (wxChildNode * node = wxTopLevelWindows(NULL)->First (); node; node = node->Next ()) {
		wxWindow *win = (wxWindow *) node->Data ();
		if (win && node->IsShown()) {
			int wid, hgt;
			win->GetSize(&wid, &hgt);
			win->OnSize(wid, hgt);
		}
	}
		
	while (!wxTheApp->death_processed /* && wxTopLevelWindows.Number() > 0 */)
	{
		wxTheApp->MainLoop();
	}

	int retValue = wxTheApp->OnExit();
	wxCleanUp();
	return retValue;
}

// In theory, this entry pt won't be used unless the real main does not
// declare one.
int main(int argc, char* argv[])
{
	return wxEntry(argc, argv);
}

//-----------------------------------------------------------------------------
void wxCleanUp(void)
{// Cleans up any wxWindows internal structures left lying around
	wxCommonCleanUp();
	wxFlushResources();
}

//-----------------------------------------------------------------------------
Bool wxYield(void)
{ // Yield to incoming messages

    while (wxTheApp->Pending())
      wxTheApp->Dispatch();
      
	return TRUE;
}

//-----------------------------------------------------------------------------
void wxExit(void)
{
	int retValue = 0;
	if (wxTheApp) retValue = wxTheApp->OnExit();
	wxCleanUp();

	exit(retValue);
}