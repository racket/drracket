/*
 * File:        edjr.cc
 * Purpose:     EdJr main file - demonstration of wxMedia classes
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 1995, Matthew Flatt
 */

#include "wx.h"
#include "wx_main.h"
#include "wx_media.h"
#include "edjr.h"

class EdJrApp: public wxApp
{
public:
  EdJrApp();
  wxFrame *OnInit(void);
};

#if WX_FORCE_APP_CREATION
EdJrApp *TheApp;
#else
EdJrApp TheApp;
#endif

EdJrApp::EdJrApp()
{
#ifdef wx_xt
  SetClassName("EdJr");
#else
  wx_class = "EdJr";
#endif
}

#ifdef WXME_FOR_MRED
static wxStandardSnipClassList *snipClassList;
static wxBufferDataClassList *bufferDataClassList;

wxStandardSnipClassList *wxGetTheSnipClassList()
{
  return snipClassList;
}

wxBufferDataClassList *wxGetTheBufferDataClassList()
{
  return bufferDataClassList;
}

#endif

wxFrame *EdJrApp::OnInit(void)
{
#ifdef WXME_FOR_MRED
  snipClassList = wxMakeTheSnipClassList();
  bufferDataClassList = wxMakeTheBufferDataClassList();
#endif

  wxInitMedia();

  return MakeEdJrFrame();
}

#if WX_FORCE_APP_CREATION
extern "C" { int wxEntry(int argc, char *argv[]); };

/* Solves C++ problem of intializing TheApp for some compilers */
int main(int argc, char *argv[])
{
  TheApp = new EdJrApp;
  return wxEntry(argc, argv);
}
#endif

#if WXGARBAGE_COLLECTION_ON

#ifdef __cplusplus
extern "C" 
{
#endif

void gc_mark_external_invalid(void *sobj)
{
  /* Do nothing. */
}

#ifdef __cplusplus
}
#endif

#endif

