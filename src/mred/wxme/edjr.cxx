/*
 * File:        edjr.cc
 * Purpose:     EdJr - for demonstration of wxMedia classes
 *                and backup editor in MrEd
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 1995, Matthew Flatt
 */

#include "wx_frame.h"
#include "wx_main.h"
#include "wx_menu.h"
#include "wx_utils.h"
#include "wx_dialg.h"
#ifndef OLD_WXWINDOWS
#include "wx_cmdlg.h"
#endif
#include "wx_media.h"
#include "edjr.h"

class EdJrWindow : public wxFrame
{
public:
  wxMediaCanvas *display;
  wxMediaEdit *media;

  int editOffset, fontOffset;

  EdJrWindow *parent;

  wxList *children;

  EdJrWindow(char *filename, EdJrWindow *parent);
  ~EdJrWindow();

  void OnSize(int x, int y);
  void OnMenuCommand(int op);

  Bool OnClose(void);
};

#ifdef wx_xt
# define MARGIN 0
#else
# define MARGIN 2
#endif

wxFrame *MakeEdJrFrame(void)
{
  return new EdJrWindow(NULL, NULL);
}

enum {
  EdJrFILE_OPEN = 1,
  EdJrFILE_SAVE,
  EdJrFILE_SAVE_AS,
  EdJrFILE_SAVE_AS_TEXT,
  EdJrFILE_SAVE_AS_FORMATTED,
  EdJrFILE_RELOAD,
  EdJrFILE_NEW_FRAME,
  EdJrFILE_QUIT,
  EdJrLAST_MENU_PLUS_ONE
  };

EdJrWindow::EdJrWindow(char *filename, EdJrWindow *mainParent)
: wxFrame(NULL, "EdJr", -1, -1, 600, 400, wxDEFAULT_FRAME + wxSDI, "EdJr")
{
  wxMenuBar *menubar;
  wxMenu *file, *edit;
  wxKeymap *km;

  display = new wxMediaCanvas(this);

  media = new wxMediaEdit();
  display->SetMedia(media);

  menubar = new wxMenuBar();

#ifdef wx_mac
#define OPEN_SUFFIX "\tCmd+O"
#define SAVE_SUFFIX "\tCmd+S"
#define NEW_SUFFIX "\tCmd+N"
#define CLOSE_SUFFIX "\tCmd+W"
#else
#define OPEN_SUFFIX
#define SAVE_SUFFIX
#define NEW_SUFFIX
#define CLOSE_SUFFIX
#endif

  file = new wxMenu();
  file->Append(EdJrFILE_NEW_FRAME, "New" NEW_SUFFIX);
  file->Append(EdJrFILE_OPEN, "Open..." OPEN_SUFFIX);
  file->Append(EdJrFILE_RELOAD, "Reload");  
  file->AppendSeparator();
  file->Append(EdJrFILE_SAVE, "Save" SAVE_SUFFIX);
  file->Append(EdJrFILE_SAVE_AS, "Save As...");
  file->Append(EdJrFILE_SAVE_AS_TEXT, "Save As Text...");
  file->Append(EdJrFILE_SAVE_AS_FORMATTED, "Save As Formatted...");
  file->AppendSeparator();
  file->Append(EdJrFILE_QUIT, 
	       !mainParent 
#ifndef wx_mac
	       ? "E&xit" 
#else
	       ? "Quit\tCmd+Q" 
#endif
	       : "Close" CLOSE_SUFFIX);
    
  editOffset = EdJrLAST_MENU_PLUS_ONE;

  edit = new wxMenu();
  fontOffset = media->AppendEditItems(edit, editOffset);
  edit->AppendSeparator();
  media->AppendFontItems(edit, fontOffset);
  
  menubar->Append(file, "&File");
  menubar->Append(edit, "&Edit");

  SetMenuBar(menubar);

  /* Map some basic keys: */
  km = media->GetKeymap();
  media->AddBufferFunctions(km);
  media->AddEditorFunctions(km);

  km->MapFunction("c:e", "end-of-line");
  km->MapFunction("END", "end-of-line");
  km->MapFunction("c:a", "beginning-of-line");
  km->MapFunction("HOME", "beginning-of-line");

  km->MapFunction("c:p", "previous-line");
  km->MapFunction("c:n", "next-line");
	
  km->MapFunction("c:h", "delete-previous-character");
  km->MapFunction("c:d", "delete-next-character");
  km->MapFunction("c:f", "forward-character");
  km->MapFunction("c:b", "backward-character");

  km->MapFunction("m:f", "forward-word");
  km->MapFunction("m:b", "backward-word");
  km->MapFunction("m:d", "kill-word");
  km->MapFunction("m:DEL", "backward-kill-word");

  km->MapFunction("ESC;f", "forward-word");
  km->MapFunction("ESC;b", "backward-word");
  km->MapFunction("ESC;d", "kill-word");
  km->MapFunction("ESC;DEL", "backward-kill-word");

  km->MapFunction("m:s:<", "beginning-of-file");
  km->MapFunction("m:s:>", "end-of-file");

  km->MapFunction("ESC;s:<", "beginning-of-file");
  km->MapFunction("ESC;s:>", "end-of-file");

  km->MapFunction("c:v", "next-page");
  km->MapFunction("m:v", "previous-page");
  km->MapFunction("ESC;v", "previous-page");
	
  km->MapFunction("c:k", "delete-to-end-of-line");
  km->MapFunction("c:y", "paste-clipboard");
  km->MapFunction("a:v", "paste-clipboard");
  km->MapFunction("c:s:_", "undo");
  km->MapFunction("c:s:+", "redo");
  km->MapFunction("a:z", "undo");
  km->MapFunction("c:x;u", "undo");
  km->MapFunction("c:w", "cut-clipboard");
  km->MapFunction("a:x", "cut-clipboard");
  km->MapFunction("m:w", "copy-clipboard");
  km->MapFunction("ESC;w", "copy-clipboard");
  km->MapFunction("a:c", "copy-clipboard");

  Show(TRUE);

  parent = mainParent;  
  if (parent) {
    parent->children->Append(this);
    children = NULL;
  } else
    children = new wxList;

  if (filename)
    media->LoadFile(filename);

#ifdef wx_mac
  OnSize(600, 400);
#endif
}

EdJrWindow::~EdJrWindow()
{
  wxNode *node, *next;

  if (parent)
    parent->children->DeleteObject(this);

  if (children)
    for (node = children->First(); node; node = next) {
      next = node->Next();
      delete (EdJrWindow *)node->Data();
    }

  delete display;
}

void EdJrWindow::OnMenuCommand(int op)
{
  if (!display)
    return;

  if (op >= fontOffset)
    media->DoFont(op - fontOffset);
  else if (op >= editOffset)
    media->DoEdit(op - editOffset);
  else {
    switch(op) {
    case EdJrFILE_OPEN:
      media->LoadFile("");
      break;
    case EdJrFILE_SAVE:
      media->SaveFile();
      break;
    case EdJrFILE_SAVE_AS:
      media->SaveFile("");
      break;
    case EdJrFILE_SAVE_AS_TEXT:
      media->SaveFile("", wxMEDIA_FF_TEXT);
      break;
    case EdJrFILE_SAVE_AS_FORMATTED:
      media->SaveFile("", wxMEDIA_FF_STD);
      break;
    case EdJrFILE_RELOAD:
      media->LoadFile();
      break;
    case EdJrFILE_NEW_FRAME:
      (void)(new EdJrWindow(NULL, parent ? parent : this));
      break;
    case EdJrFILE_QUIT:
      if (OnClose()) {
	if (!parent)
	  wxTheApp->ExitMainLoop();
	delete this;
      }
      return;
    }

    char *s = media->GetFilename();
    if (s)
      SetTitle(wxFileNameFromPath(s));
    else
      SetTitle("EdJr");
  }
}

void EdJrWindow::OnSize(int x, int y)
{
  int top_h;

  GetClientSize(&x, &y);

  top_h = 0;

  if (display)
    display->SetSize(MARGIN, MARGIN + top_h, 
		     x - 2 * MARGIN, y - (2 * MARGIN + top_h));
}

Bool EdJrWindow::OnClose(void)
{
  if (media->Modified()) {
    if (wxMessageBox("This file has been modified.\n\n"
		     "Are you sure you want to quit?",
		     "Warning", wxYES_NO) == wxNO)
      return FALSE;
  }
  if (children) {
    wxNode *node;
    for (node = children->First(); node; node = node->Next()) {
      EdJrWindow *child;
      child = (EdJrWindow *)node->Data();
      if (child->media->Modified()) {
	if (wxMessageBox("An open file (in another frame) "
			 "has been modified.\n\n"
			 "Are you sure you want to quit?",
			 "Warning", wxYES_NO) == wxNO)
	  return FALSE;
	break;
      }
    }
  }

  if (!parent)
    wxTheApp->ExitMainLoop();

  return TRUE;
}
