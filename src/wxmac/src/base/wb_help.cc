/*
 * File:	wb_help.cc
 * Purpose:	API for invoking wxHelp
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_help.cc,v 1.4 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "@(#)wb_help.cc	1.2 5/9/94"; */

#ifdef __GNUG__
#pragma implementation "wx_help.h"
#endif

#include "common.h"
#include "wx_help.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#if USE_HELP
#include <time.h>

#ifdef wx_msw
#include <wx_privt.h>
#endif

#ifdef wx_x
#include <netdb.h>

#ifdef SUN_CC
#include <sysent.h>
#endif // SUN_CC
#ifdef __hpux
#include <sys/unistd.h>
#endif // __hpux
#endif // wx_x

// Timeout in seconds
#define WX_HELP_TIMEOUT 15 /* was 30 */

// MAX path length
#define _MAXPATHLEN 500

// MAX length of Help descriptor
#define _MAX_HELP_LEN 500

#include "wx_help.h"

wxHelpInstance::wxHelpInstance(Bool native)
{
#ifdef wx_msw
  useNative = native;
#else
  useNative = FALSE;  // Vetoed under X -- no native help system
#endif

  __type = wxTYPE_HELP_INSTANCE;
  helpFile = NULL; helpServer = -1; helpHost = NULL;
  helpRunning = FALSE; helpConnection = NULL;
}

wxHelpInstance::~wxHelpInstance(void)
{
  if (helpFile) delete[] helpFile; 
  if (helpHost) delete[] helpHost;
}

Bool wxHelpInstance::Initialize(char *filename, int server)
{
#ifdef wx_x
  {
    char host_buf[255];
    if (wxGetHostName(host_buf, sizeof(host_buf)))
      helpHost = copystring(host_buf);
    else helpHost = NULL;
  }
#elif defined(wx_msw)
  if (!useNative)
    helpHost = copystring("dummy");
#else
#error "Not yet..."
#endif

  helpFile = copystring(filename);
  if (!useNative) {
    helpServer = server;
    wxIPCInitialize();
  }
  return TRUE;
}

Bool wxHelpInstance::LoadFile(char *file)
{
  if (!file)
    file = helpFile;
  else {
    if (helpFile) delete[] helpFile;
    helpFile = copystring(file);
  }

  if (!useNative) {
    if (!helpRunning) {
      if (!Run())
        return FALSE;
    }
    char buf[_MAX_HELP_LEN];
    sprintf(buf, "f %s", file);
    if (helpConnection)
      return helpConnection->Execute(buf);
    else return FALSE;
  } else {
#ifdef wx_msw
    char buf[_MAXPATHLEN];
    strcpy(buf, file);
    size_t len = strlen(file);
    if (!(file[len-1] == 'p' && file[len-2] == 'l' && file[len-3] == 'h' && file[len-4] == '.'))
      strcat(buf, ".hlp");
    if (wxTheApp->wx_frame)
    {
      wxWnd *wnd = (wxWnd *)(wxTheApp->wx_frame->handle);
      HWND hwnd = wnd->handle;
      WinHelp(hwnd, buf, HELP_CONTENTS, 0L);
     return TRUE;
    }
#endif
  }
  return FALSE;
}

Bool wxHelpInstance::DisplayContents(void)
{
  if (!useNative) {
    if (!helpRunning) {
      if (!Run())
        return FALSE;
    }
    if (helpConnection)
      return helpConnection->Execute("s -1");
    else return FALSE;
  } else {
#ifdef wx_msw
    if (!helpFile) return FALSE;
    
    char buf[_MAXPATHLEN];
    strcpy(buf, helpFile);
    size_t len = strlen(helpFile);
    if (!(helpFile[len-1] == 'p' && helpFile[len-2] == 'l' && helpFile[len-3] == 'h' && helpFile[len-4] == '.'))
      strcat(buf, ".hlp");
    if (wxTheApp->wx_frame)
    {
      wxWnd *wnd = (wxWnd *)(wxTheApp->wx_frame->handle);
      HWND hwnd = wnd->handle;
      WinHelp(hwnd, buf, HELP_CONTENTS, 0L);
     return TRUE;
    }
#endif
  }
  return FALSE;
}

Bool wxHelpInstance::DisplaySection(int section)
{
  if (!useNative) {
    if (!helpRunning) {
      if (!Run())
        return FALSE;
    }
    char buf[_MAX_HELP_LEN];
    sprintf(buf, "s %d", section);
    if (helpConnection)
      return helpConnection->Execute(buf);
    else return FALSE;
  }
  // No WinHelp equivalent for this
  return FALSE;
}

Bool wxHelpInstance::DisplayBlock(long block)
{
  if (!useNative) {
    if (!helpRunning) {
      if (!Run())
        return FALSE;
    }
    char buf[_MAX_HELP_LEN];
    sprintf(buf, "b %ld", block);
    if (helpConnection)
      return helpConnection->Execute(buf);
    else return FALSE;
  } else {
    // Use context number -- a very rough equivalent to block id!
#ifdef wx_msw
    if (!helpFile) return FALSE;
    
    char buf[_MAXPATHLEN];
    strcpy(buf, helpFile);
    size_t len = strlen(helpFile);
    if (!(helpFile[len-1] == 'p' && helpFile[len-2] == 'l' && helpFile[len-3] == 'h' && helpFile[len-4] == '.'))
      strcat(buf, ".hlp");
    if (wxTheApp->wx_frame) {
      wxWnd *wnd = (wxWnd *)(wxTheApp->wx_frame->handle);
      HWND hwnd = wnd->handle;
      WinHelp(hwnd, buf, HELP_CONTEXT, (DWORD)block);
     return TRUE;
    }
#endif
  }
  return FALSE;
}

Bool wxHelpInstance::KeywordSearch(char *k)
{
  if (!useNative) {
    if (!helpRunning) {
      if (!Run())
        return FALSE;
    }
    char buf[500];
    sprintf(buf, "k %s", k);
    if (helpConnection)
      return helpConnection->Execute(buf);
    else return FALSE;
  } else {
#ifdef wx_msw
    if (!helpFile) return FALSE;

    char buf[_MAXPATHLEN];
    strcpy(buf, helpFile);
    size_t len = strlen(helpFile);
    if (!(helpFile[len-1] == 'p' && helpFile[len-2] == 'l' && helpFile[len-3] == 'h' && helpFile[len-4] == '.'))
      strcat(buf, ".hlp");
    if (wxTheApp->wx_frame) {
      wxWnd *wnd = (wxWnd *)(wxTheApp->wx_frame->handle);
      HWND hwnd = wnd->handle;
      WinHelp(hwnd, buf, HELP_PARTIALKEY, (DWORD)k);
     return TRUE;
    }
#endif
  }
  return FALSE;
}

Bool wxHelpInstance::Quit(void)
{
  if (helpConnection)
    return helpConnection->Disconnect(); // Calls OnQuit via OnDisconnect
  else return TRUE;
}

void wxHelpInstance::OnQuit(void)
{
}

Bool wxHelpInstance::Run(void)
{
#ifdef wx_x
  if (!helpFile || !helpHost || helpRunning)
    return FALSE;
#endif
#ifdef wx_msw
  if (!helpFile || helpRunning)
    return FALSE;
#endif

  time_t current_time;
#ifdef wx_x
  // Invent a server name that's likely to be unique but different from
  // last time
  (void)time(&current_time);
  if (helpServer == -1)
    helpServer = (int)(4000 + (current_time % 4000));
#else
  // Only one instance of wxHelp at a time
  helpServer = 4000;
#endif

  char server[32];
  sprintf(server, "%d", helpServer);
#ifdef wx_msw
  // Only one instance of wxHelp under Windows.
  // See if there's already an instance of wxHelp
  if (helpConnection = (wxHelpConnection *)MakeConnection(helpHost, server, "WXHELP")) {
    helpRunning = TRUE;
    return TRUE;
  }
#endif

  // Start help process in server modus
//  char *argv[] = {"wxhelp", "-server", server, NULL}; // HP compiler complains
  char *argv[4];
  argv[0] = "wxhelp";
  argv[1] = "-server";
  argv[2] = server;
  argv[3] = NULL;

  if (wxExecute(argv) == FALSE)
    return FALSE; // Maybe we should print a message?

  time_t start_time;
  (void)time(&start_time);
  // Give it some time to respond
  do {
    wxSleep(1);
    helpConnection = (wxHelpConnection *)MakeConnection(helpHost, server, "WXHELP");
    (void)time(&current_time);
  } while (!helpConnection && ((current_time - start_time) < WX_HELP_TIMEOUT));

  if (helpConnection == NULL) {
    char buf[100];
    sprintf(buf, wxSTR_HELP_TIMEOUT, WX_HELP_TIMEOUT);
    (void)wxMessageBox(buf, wxSTR_ERROR);
    return FALSE;
  }
  helpRunning = TRUE;
  return TRUE;
}

wxHelpConnection::wxHelpConnection(wxHelpInstance *instance)
{
  helpInstance = instance;
}

Bool wxHelpConnection::OnDisconnect(void)
{
  helpInstance->OnQuit();
  helpInstance->helpRunning = FALSE;
  helpInstance->helpConnection = NULL;
  helpInstance->helpServer = -1;
  delete this;
  return TRUE;
}

#endif // USE_HELP
