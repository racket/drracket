/*
 * File:	wb_types.cc
 * Purpose:	Explicit type implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_types.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#ifdef __GNUG__
#pragma implementation "wx_types.h"
#endif

#include "wx_utils.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"
#include "wx_types.h"

wxTypeTree wxAllTypes;

void wxInitStandardTypes(void)
{
  // Define explicit type hierarchy
  wxAllTypes.AddType(wxTYPE_WINDOW,      wxTYPE_ANY,                        "window");

  wxAllTypes.AddType(wxTYPE_CANVAS,      wxTYPE_WINDOW,                     "canvas");
#if (defined(wx_motif) && USE_PANEL_CANVAS_IN_X) || (defined(wx_msw) && USE_PANEL_CANVAS_IN_MSW)
	|| (defined(wx_mac) && USE_PANEL_CANVAS_IN_MAC)
  wxAllTypes.AddType(wxTYPE_PANEL,       wxTYPE_CANVAS,                     "panel");
#else
  wxAllTypes.AddType(wxTYPE_PANEL,       wxTYPE_WINDOW,                     "panel");
#endif
  wxAllTypes.AddType(wxTYPE_TEXT_WINDOW, wxTYPE_WINDOW,                     "text window");
  wxAllTypes.AddType(wxTYPE_FRAME,       wxTYPE_WINDOW,                     "frame");
  wxAllTypes.AddType(wxTYPE_ITEM,        wxTYPE_WINDOW,                     "item");

#if USE_MAC_DIALOG_PANEL == 0
  wxAllTypes.AddType(wxTYPE_DIALOG_BOX,  wxTYPE_FRAME,                      "dialog box");
#else // wx_mac
  wxAllTypes.AddType(wxTYPE_DIALOG_BOX,  wxTYPE_PANEL,                      "dialog box");
#endif
  wxAllTypes.AddType(wxTYPE_ENHANCED_DIALOG,  wxTYPE_DIALOG_BOX,            "enhanced dialog");

  wxAllTypes.AddType(wxTYPE_BUTTON,      wxTYPE_ITEM,                       "button");
  wxAllTypes.AddType(wxTYPE_MESSAGE,     wxTYPE_ITEM,                       "message");
  wxAllTypes.AddType(wxTYPE_TEXT,        wxTYPE_ITEM,                       "text");
  wxAllTypes.AddType(wxTYPE_MULTI_TEXT,  wxTYPE_TEXT,                       "multi text");
  wxAllTypes.AddType(wxTYPE_CHOICE,      wxTYPE_ITEM,                       "choice");
  wxAllTypes.AddType(wxTYPE_LIST_BOX,    wxTYPE_ITEM,                       "list box");
  wxAllTypes.AddType(wxTYPE_CHECK_BOX,   wxTYPE_ITEM,                       "check box");
  wxAllTypes.AddType(wxTYPE_SLIDER,      wxTYPE_ITEM,                       "slider");
  wxAllTypes.AddType(wxTYPE_MENU,        wxTYPE_ITEM,                       "menu");
  wxAllTypes.AddType(wxTYPE_MENU_BAR,    wxTYPE_ITEM,                       "menu bar");
  wxAllTypes.AddType(wxTYPE_RADIO_BOX,   wxTYPE_ITEM,                       "radio box");
  wxAllTypes.AddType(wxTYPE_GROUP_BOX,   wxTYPE_ITEM,                       "group box");
  wxAllTypes.AddType(wxTYPE_GAUGE,       wxTYPE_ITEM,                       "gauge");

  wxAllTypes.AddType(wxTYPE_EVENT,       wxTYPE_ANY,                        "event");
  wxAllTypes.AddType(wxTYPE_MOUSE_EVENT, wxTYPE_EVENT,                      "mouse event");
  wxAllTypes.AddType(wxTYPE_KEY_EVENT,   wxTYPE_EVENT,                      "key event");
  wxAllTypes.AddType(wxTYPE_COMMAND_EVENT,wxTYPE_EVENT,                     "command event");

  wxAllTypes.AddType(wxTYPE_DC,          wxTYPE_ANY,                        "device context");
  wxAllTypes.AddType(wxTYPE_DC_CANVAS,   wxTYPE_DC,                         "canvas device context");
  wxAllTypes.AddType(wxTYPE_DC_POSTSCRIPT,wxTYPE_DC,                        "PostScript device context");
  wxAllTypes.AddType(wxTYPE_DC_PRINTER,  wxTYPE_DC,                         "printer device context");
  wxAllTypes.AddType(wxTYPE_DC_METAFILE, wxTYPE_DC,                         "metafile device context");
  wxAllTypes.AddType(wxTYPE_DC_MEMORY,   wxTYPE_DC,                         "memory device context");

  wxAllTypes.AddType(wxTYPE_PEN,         wxTYPE_ANY,                        "pen");
  wxAllTypes.AddType(wxTYPE_BRUSH,       wxTYPE_ANY,                        "brush");
  wxAllTypes.AddType(wxTYPE_FONT,        wxTYPE_ANY,                        "font");
  wxAllTypes.AddType(wxTYPE_BITMAP,      wxTYPE_ANY,                        "bitmap");
  wxAllTypes.AddType(wxTYPE_ICON,        wxTYPE_BITMAP,                     "icon");
  wxAllTypes.AddType(wxTYPE_CURSOR,      wxTYPE_BITMAP,                     "cursor");
  wxAllTypes.AddType(wxTYPE_METAFILE,    wxTYPE_ANY,                        "metafile");
  wxAllTypes.AddType(wxTYPE_TIMER,       wxTYPE_ANY,                        "timer");
  wxAllTypes.AddType(wxTYPE_COLOUR,      wxTYPE_ANY,                        "colour");

  wxAllTypes.AddType(wxTYPE_LIST,        wxTYPE_ANY,                        "list");
  wxAllTypes.AddType(wxTYPE_STRING_LIST, wxTYPE_LIST,                       "string list");
  wxAllTypes.AddType(wxTYPE_NODE,        wxTYPE_ANY,                        "node");
  wxAllTypes.AddType(wxTYPE_HASH_TABLE,  wxTYPE_ANY,                        "hash table");
  wxAllTypes.AddType(wxTYPE_APP,         wxTYPE_ANY,                        "application");

  wxAllTypes.AddType(wxTYPE_DDE_SERVER,  wxTYPE_ANY,                        "DDE server");
  wxAllTypes.AddType(wxTYPE_DDE_CLIENT,  wxTYPE_ANY,                        "DDE client");
  wxAllTypes.AddType(wxTYPE_DDE_CONNECTION, wxTYPE_ANY,                     "DDE connection");
  wxAllTypes.AddType(wxTYPE_HELP_INSTANCE, wxTYPE_ANY,                      "wxHelp instance");
#ifdef wx_mac
  wxAllTypes.AddType(wxTYPE_BORDER,      wxTYPE_WINDOW,                     "border");
#endif
}

// Explicit type hierarchy required
wxTypeTree::wxTypeTree(void):wxHashTable(wxKEY_INTEGER)
{
}

wxTypeTree::~wxTypeTree(void)
{
  // Cleanup wxTypeDef allocated
  BeginFind();
  wxNode *node = Next();
  while (node)
  {
    wxTypeDef *typ = (wxTypeDef *)node->Data();
    delete typ;
    node = Next();
  }
}

void wxTypeTree::AddType(WXTYPE type, WXTYPE parent, char *name)
{
  wxTypeDef *typ = new wxTypeDef;
  typ->type = type;
  typ->parent = parent;
  typ->name = copystring(name);
  Put((long)type, (wxObject *)typ);
}

Bool wxSubType(WXTYPE type1, WXTYPE type2)
{
  if (type1 == type2)
    return TRUE;

  WXTYPE t = type1;
  while (TRUE)
  {
    wxTypeDef *typ = (wxTypeDef *)wxAllTypes.Get((long)t);
    if (!typ)
      return FALSE;

    if (type2 == typ->parent)
      return TRUE;

    t = typ->parent;
  }
}

char *wxGetTypeName(WXTYPE type)
{
  if (type == wxTYPE_ANY)
    return "any";

  wxTypeDef *typ = (wxTypeDef *)wxAllTypes.Get((long)type);
  if (!typ)
    return NULL;
  return typ->name;
}

