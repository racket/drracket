/*
 * File:	wb_main.cc
 * Purpose:	wxApp implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_main.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "%W% %G%"; */

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"

#include "wx_panel.h"
#include "wx_frame.h"
#include "wx_main.h"
#include "wx_utils.h"
#include "wx_gdi.h"
#include "wx_dc.h"
#include "wx_dialg.h"
#include "wx_types.h"
#include "wx_dcps.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#include "wx_sysev.h"

#include <string.h>

extern char *wxBuffer;

wxApp *wxTheApp = NULL;

wxbApp::wxbApp(wxlanguage_t )
{
  __type = wxTYPE_APP;
  wx_class = NULL;
  wantDebugOutput = TRUE ;
}

wxbApp::~wxbApp(void)
{
}

Bool wxbApp::Initialized(void)
{
  return FALSE;
}

wxFrame *wxbApp::OnInit(void)
{
  return NULL;
}

int wxbApp::OnExit(void)
{
  return 0;
}

void wxCommonInit(void)
{
#if defined(wx_msw) || defined(wx_mac)
  wxBuffer = new char[1500];
#else
  wxBuffer = new char[BUFSIZ + 512];
#endif
//  wxTheColourList = new wxGDIList ;
  wxTheColourDatabase = new wxColourDatabase(wxKEY_STRING);
  wxTheColourDatabase->Initialize();
  wxTheFontNameDirectory.Initialize();
  wxInitializeStockObjects();
  wxInitStandardTypes();
  wxInitStandardEvents();
  wxThePrintPaperDatabase = new wxPrintPaperDatabase;
}

void wxCommonCleanUp(void)
{
  wxDeleteStockObjects() ;
  // Destroy all GDI lists, etc.
  delete wxTheBrushList;
  delete wxThePenList;
  delete wxTheFontList;
  delete wxTheBitmapList;
#ifdef wx_mac
  delete wxTheCursorList;
  delete wxTheIconList;
#endif
  delete wxThePrintPaperDatabase;

  delete wxTheColourDatabase;
//  delete wxTheColourList; // After ALL Gdi objects, remove remaining colors.
  wxDeleteEventLists() ;

  delete[] wxBuffer;
}

