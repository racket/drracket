/*
 * File:	wb_panel.cc
 * Purpose:	wxPanel class implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_panel.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "@(#)wb_panel.cc	1.2 5/9/94"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wb_panel.h"
#include "wx_buttn.h"
#include "wx_stdev.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

class wxFrame;
class wxPanel;

// Constructors

#ifndef wx_mac
wxbPanel::wxbPanel(void)
{
  __type = wxTYPE_PANEL;
  defaultItem = NULL;
  new_line = FALSE;
  label_position = wxHORIZONTAL;
  window_parent = NULL;
}

wxbPanel::wxbPanel(wxWindow *parent, int x, int y, int width, int height, long style,
                  char *name)
{
  __type = wxTYPE_PANEL;
  windowStyle = style;
  defaultItem = NULL;
  new_line = FALSE;
  label_position = wxHORIZONTAL;

  window_parent = parent;
}

/*
wxbPanel::wxbPanel(wxPanel *frame, int x, int y, int width, int height, long style,
                   char *name)
{
  __type = wxTYPE_PANEL;
  windowStyle = style;
  defaultItem = NULL;
  new_line = FALSE;
  label_position = wxHORIZONTAL;

  if (!frame)
    return;

  window_parent = frame;
}
*/
#endif // wx_mac

wxbPanel::~wxbPanel(void)
{
}

wxObject* wxbPanel::GetChild(int number)
{
  // Return a pointer to the Nth object in the Panel
  if (!children)
    return(NULL) ;
  wxChildNode *node = GetChildren()->First();
  while (node && number--)
    node = node->Next() ;
  if (node)
  {
    wxObject *obj = (wxObject *)node->Data();
    return(obj) ;
  }
  else
    return NULL ;
}

void wxbPanel::SetLabelPosition(int pos)  // wxHORIZONTAL or wxVERTICAL
{
  label_position = pos;
}

int wxbPanel::GetLabelPosition(void)
{
  return label_position;
}

void wxbPanel::OnDefaultAction(wxItem *initiatingItem)
{
  wxButton *but = GetDefaultItem();
  if (but)
  {
    wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_BUTTON_COMMAND);
    event->eventObject = but;
    but->Command(*event);
  }
}

void wxbPanel::SetLabelFont(wxFont *fnt)
{
  labelFont = fnt ;
//***added by D.Chubraev 
  this->font=labelFont; 
#ifdef wx_motif
  int scaled_size = (int) (10 * ((int) (this->font->GetPointSize () + 0.5))); 
  int res_x = 100; 
  int res_y = 100; 
  XFontStruct *fontStruct = wxFontPool->FindNearestFont (this->font->GetFamily (
),  
                                this->font->GetStyle (), 
                                this->font->GetWeight (), scaled_size, 
                                this->font->GetUnderlined (), res_x, res_y); 
  this->font->xFont=fontStruct; 
#endif 
//*** 
}

void wxbPanel::SetButtonFont(wxFont *font)
{
  buttonFont = font ;
}

void wxbPanel::SetBackgroundColour(wxColour *col)
{
  backColour = col ;
}

void wxbPanel::SetLabelColour(wxColour *col)
{
  labelColour = col ;
}

void wxbPanel::SetButtonColour(wxColour *col)
{
  buttonColour = col ;
}

#if USE_WX_RESOURCES
#include "wx_res.h"
#include "wx_buttn.h"
#include "wx_check.h"
#include "wx_choic.h"
#include "wx_group.h"
#include "wx_messg.h"
#include "wx_txt.h"
#include "wx_mtxt.h"
#include "wx_rbox.h"
#include "wx_lbox.h"
#include "wx_gauge.h"
#include "wx_slidr.h"
#include "wx_dialg.h"

/*
 * Optional resource loading facility
 */

Bool wxbPanel::LoadFromResource(wxWindow *parent, char *resourceName)
{
  wxItemResource *resource = wxDefaultResourceTable.FindResource(resourceName);
  if (!resource || (resource->GetType() != wxTYPE_DIALOG_BOX))
    return FALSE;
  char *title = resource->GetTitle();
  long windowStyle = resource->GetStyle();
  Bool isModal = (Bool)resource->GetValue1();
  int x = resource->GetX();
  int y = resource->GetY();
  int width = resource->GetWidth();
  int height = resource->GetHeight();
  char *name = resource->GetName();

  wxFont *theButtonFont = (wxFont *)resource->GetValue2();
  wxFont *theLabelFont = (wxFont *)resource->GetValue3();

  if (wxSubType(__type, wxTYPE_DIALOG_BOX))
  {
    wxDialogBox *dialogBox = (wxDialogBox *)this;
    if (!dialogBox->Create(parent, title, isModal, x, y, width, height, windowStyle, name))
      return FALSE;
  }
  else
  {
    if (!((wxPanel *)this)->Create(parent, x, y, width, height, windowStyle, name))
      return FALSE;
  }

  if (theButtonFont)
    SetButtonFont(theButtonFont);
  if (theLabelFont)
    SetLabelFont(theLabelFont);

  // Now create children
  wxNode *node = resource->GetChildren().First();
  while (node)
  {
    wxItemResource *childResource = (wxItemResource *)node->Data();
    if (childResource->GetStyle() & wxVERTICAL_LABEL)
      SetLabelPosition(wxVERTICAL);
    else
      SetLabelPosition(wxHORIZONTAL);
      
    switch (childResource->GetType())
    {
      case wxTYPE_BUTTON:
      {
        wxButton *control = NULL;
        if (childResource->GetValue4())
        {
          // Bitmap button
          wxBitmap *bitmap = childResource->GetBitmap();
          if (!bitmap)
          {
            bitmap = wxResourceCreateBitmap(childResource->GetValue4());
            childResource->SetBitmap(bitmap);
          }
          if (bitmap)
           control = new wxButton((wxPanel *)this, (wxFunction)NULL, bitmap,
             childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
             childResource->GetStyle(), childResource->GetName());
        }
        else
          // Normal, text button
          control = new wxButton((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           childResource->GetStyle(), childResource->GetName());
        break;
      }
      case wxTYPE_MESSAGE:
      {
        wxMessage *control = NULL;
        if (childResource->GetValue4())
        {
          // Bitmap button
          wxBitmap *bitmap = childResource->GetBitmap();
          if (!bitmap)
          {
            bitmap = wxResourceCreateBitmap(childResource->GetValue4());
            childResource->SetBitmap(bitmap);
          }
          if (bitmap)
           control = new wxMessage((wxPanel *)this, bitmap,
             childResource->GetX(), childResource->GetY(), // childResource->GetWidth(), childResource->GetHeight(),
             childResource->GetStyle(), childResource->GetName());
        }
        else
        {
           control = new wxMessage((wxPanel *)this, childResource->GetTitle(),
             childResource->GetX(), childResource->GetY(), // childResource->GetWidth(), childResource->GetHeight(),
             childResource->GetStyle(), childResource->GetName());
        }
        break;
      }
      case wxTYPE_TEXT:
      {
        (void)new wxText((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(), childResource->GetValue4(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           childResource->GetStyle(), childResource->GetName());
        break;
      }
      case wxTYPE_MULTI_TEXT:
      {
        (void)new wxMultiText((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(), childResource->GetValue4(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           childResource->GetStyle(), childResource->GetName());
        break;
      }
      case wxTYPE_CHECK_BOX:
      {
        wxCheckBox *control = new wxCheckBox((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           childResource->GetStyle(), childResource->GetName());
        control->SetValue((Bool)childResource->GetValue1());
        break;
      }
#if USE_GAUGE
      case wxTYPE_GAUGE:
      {
        wxGauge *control = new wxGauge((wxPanel *)this, childResource->GetTitle(), (int)childResource->GetValue2(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           childResource->GetStyle(), childResource->GetName());
        control->SetValue((int)childResource->GetValue1());
        break;
      }
#endif
      case wxTYPE_SLIDER:
      {
        (void)new wxSlider((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(), (int)childResource->GetValue1(),
           (int)childResource->GetValue2(), (int)childResource->GetValue3(),
           childResource->GetWidth(), childResource->GetX(), childResource->GetY(),
           childResource->GetStyle(), childResource->GetName());
        break;
      }
      case wxTYPE_GROUP_BOX:
      {
        (void)new wxGroupBox((wxPanel *)this, childResource->GetTitle(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           childResource->GetStyle(), childResource->GetName());
        break;
      }
      case wxTYPE_LIST_BOX:
      {
        wxStringList *stringList = childResource->GetStringValues();
        char **strings = NULL;
        int noStrings = 0;
        if (stringList && (stringList->Number() > 0))
        {
          noStrings = stringList->Number();
          strings = new char *[noStrings];
          wxNode *node = stringList->First();
          int i = 0;
          while (node)
          {
            strings[i] = (char *)node->Data();
            i ++;
            node = node->Next();
          }
        }
        (void)new wxListBox((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(), (int)childResource->GetValue1(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           noStrings, strings, childResource->GetStyle(), childResource->GetName());

        if (strings)
          delete[] strings;

        break;
      }
      case wxTYPE_CHOICE:
      {
        wxStringList *stringList = childResource->GetStringValues();
        char **strings = NULL;
        int noStrings = 0;
        if (stringList && (stringList->Number() > 0))
        {
          noStrings = stringList->Number();
          strings = new char *[noStrings];
          wxNode *node = stringList->First();
          int i = 0;
          while (node)
          {
            strings[i] = (char *)node->Data();
            i ++;
            node = node->Next();
          }
        }
        (void)new wxChoice((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           noStrings, strings, childResource->GetStyle(), childResource->GetName());

        if (strings)
          delete[] strings;

        break;
      }
      case wxTYPE_RADIO_BOX:
      {
        wxStringList *stringList = childResource->GetStringValues();
        char **strings = NULL;
        int noStrings = 0;
        if (stringList && (stringList->Number() > 0))
        {
          noStrings = stringList->Number();
          strings = new char *[noStrings];
          wxNode *node = stringList->First();
          int i = 0;
          while (node)
          {
            strings[i] = (char *)node->Data();
            i ++;
            node = node->Next();
          }
        }
        (void)new wxRadioBox((wxPanel *)this, (wxFunction)NULL, childResource->GetTitle(),
           childResource->GetX(), childResource->GetY(), childResource->GetWidth(), childResource->GetHeight(),
           noStrings, strings, 0, childResource->GetStyle(), childResource->GetName());

        if (strings)
          delete[] strings;

        break;
      }
    }
    node = node->Next();
  }
  return TRUE;
}
#endif

#ifdef wx_mac
//=============================================================================
// Protected constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxbPanel::wxbPanel // Constructor (given parentArea)
	(
		char*		windowName,
		wxArea*		parentArea,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style
	) :
		wxCanvas (parentArea, x, y, width, height, style, windowName)
{
	__type = wxTYPE_PANEL;
	InitDefaults();
	InitMoreDefaults();
}

//-----------------------------------------------------------------------------
wxbPanel::wxbPanel // Constructor (given parentWindow)
	(
		char*		windowName,
		wxWindow*	parentWindow,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style
	) :
		wxCanvas (parentWindow, x, y, width, height, style, windowName)
{
	__type = wxTYPE_PANEL;
	InitDefaults();
	InitMoreDefaults();
}

//=============================================================================
// Private methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxbPanel::InitDefaults(void)
{
	defaultItem = NULL;

	has_child = FALSE;

	hSpacing = PANEL_HSPACING;
	vSpacing = PANEL_VSPACING;
	initial_hspacing = hSpacing;
	initial_vspacing = vSpacing;
	current_hspacing = hSpacing;
	current_vspacing = vSpacing;

	new_line = FALSE;
}

//-----------------------------------------------------------------------------
void wxbPanel::InitMoreDefaults(void) // Poor name for this method
{
    if (wxSubType(window_parent->__type, wxTYPE_PANEL) &&
    	cParentArea == window_parent->ClientArea())
	{
    	wxPanel* parentPanel = (wxPanel*) window_parent;
		backColour = parentPanel->backColour;
		buttonColour = parentPanel->buttonColour;
		buttonFont = parentPanel->buttonFont;
		labelColour = parentPanel->labelColour;
		labelFont = parentPanel->labelFont;
		label_position = parentPanel->label_position;
	}
	else
	{
		backColour = NULL;
		buttonColour = NULL;
		buttonFont = NULL;
		labelColour = NULL;
		labelFont = NULL;
		label_position = wxHORIZONTAL;
	}
}

#endif // wx_mac
