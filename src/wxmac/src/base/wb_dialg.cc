/*
 * File:	wb_dialg.cc
 * Purpose:	wxDialogBox and miscellaneous dialog functions
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_dialg.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "@(#)wb_dialg.cc	1.2 5/9/94"; */

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_dialg.h"
#include "wx_utils.h"
#include "wx_gdi.h"
#include "wx_frame.h"
#include "wx_panel.h"
#include "wx_item.h"
#include "wx_buttn.h"
#include "wx_messg.h"
#include "wx_choic.h"
#include "wx_check.h"
#include "wx_menu.h"
#include "wx_txt.h"
#include "wx_mtxt.h"
#include "wx_slidr.h"
#include "wx_lbox.h"
#include "wx_mgstr.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"
#include <iostream.h>
#include <stdio.h>

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300

// Generic file load/save dialog
// static inline char * // HP compiler complains
static char *
wxDefaultFileSelector(Bool load, const char *what, char *extension, char *default_name)
{
  char prompt[50];
  sprintf(prompt, load ? wxSTR_LOAD_FILE : wxSTR_SAVE_FILE, what);

  if (*extension == '.') extension++;
  char wild[60];
  sprintf(wild, "*.%s", extension);
#ifdef wx_mac
  return wxFileSelector (prompt, NULL, default_name, (char *)extension, wild, load ? wxOPEN : wxSAVE);
#else
  return wxFileSelector (prompt, NULL, default_name, (char *)extension, wild);
#endif
}


// Generic file load dialog
char *
wxLoadFileSelector(char *what, char *extension, char *default_name)
{
  return wxDefaultFileSelector(TRUE, what, extension, default_name);
}


// Generic file save dialog
char *
wxSaveFileSelector(char *what, char *extension, char *default_name)
{
  return wxDefaultFileSelector(FALSE, what, extension, default_name);
}


// Dialog box - like panel but doesn't need a frame, and is modal or
// non-modal
#ifndef wx_mac
wxbDialogBox::wxbDialogBox(void)
{
  __type = wxTYPE_DIALOG_BOX;
}
wxbDialogBox::wxbDialogBox(wxWindow *Parent, char *Title, Bool Modal, 
                         int x, int y, int width, int height, long style, char *name):
  wxPanel()
{
  __type = wxTYPE_DIALOG_BOX;
  windowStyle = style;
}
#else
#endif


Bool wxbDialogBox::Create(wxWindow *Parent, char *Title, Bool Modal, 
                         int x, int y, int width, int height, long style, char *name)
{
  windowStyle = style;
  if (!Parent) {
    wxTopLevelWindows(ContextWindow())->Append(this);
    wxTopLevelWindows(ContextWindow())->Show(this, FALSE);
  }
  return TRUE;
}

wxbDialogBox::~wxbDialogBox()
{
  wxTopLevelWindows(ContextWindow())->DeleteObject(this);
}

void wxbDialogBox::Centre(int direction)
{
#if 0
  int x_offset,y_offset ;
  int display_width, display_height;
  int  width, height, x, y;
  wxFrame *frame ;
  if (direction & wxCENTER_FRAME)
  {
    frame = (wxFrame*)GetParent() ;
    if (frame)
    {
      frame->GetPosition(&x_offset,&y_offset) ;
      frame->GetSize(&display_width,&display_height) ;
    }
  }
  else
    frame = NULL;

  if (frame==NULL)
  {
    wxDisplaySize(&display_width, &display_height);
    x_offset = 0 ;
    y_offset = 0 ;
  }


  GetSize(&width, &height);
  GetPosition(&x, &y);

  if (direction & wxHORIZONTAL)
    x = (int)((display_width - width)/2);
  if (direction & wxVERTICAL)
    y = (int)((display_height - height)/2);

  SetSize(x+x_offset, y+y_offset, width, height, wxPOS_USE_MINUS_ONE);
#endif
  ((wxDialogBox *)this)->cFrame->Centre(direction);
}


/*
 * Common dialogs code
 *
 */

/*
 * Message centring code
 *
 */
#ifndef wx_mac // code in wx_dialg.cc
void wxSplitMessage(char *message, wxList *messageList, wxPanel *panel)
{
  char *copyMessage = copystring(message);
  size_t i = 0;
  size_t len = strlen(copyMessage);
  char *currentMessage = copyMessage;

  while (i < len) {
    while ((i < len) && (copyMessage[i] != '\n')) i++;
    if (i < len) copyMessage[i] = 0;
    wxMessage *mess = new wxMessage(panel, currentMessage);
    messageList->Append(mess);
    panel->NewLine();

    currentMessage = copyMessage + i + 1;
  }
  delete[] copyMessage;
}

void wxCentreMessage(wxList *messageList)
{
  // Do the message centering
  for(wxNode *node = messageList->First(); node; node = node->Next()) {
    wxMessage *mess = (wxMessage *)node->Data();
    mess->Centre();
  }
}
#endif // wx_mac

/*
 * A general purpose dialog box with an OnClose that returns TRUE.
 *
 */

#ifdef __GNUG__
#pragma implementation
#pragma interface
#endif

class wxMessageBoxDialog: public wxDialogBox
{
 public:
  wxText *textItem;
  wxListBox *listBoxItem;
  static char *textAnswer;
  static char *listSelection;
  static char *listClientSelection;
  static int listPosition;
  static int buttonPressed;
  static  int  * listSelections;
  static int     nlistSelections;

  wxMessageBoxDialog(wxWindow *parent, char *caption, Bool modal, int x, int y,
    int w, int h, long type):
   wxDialogBox(parent, caption, modal, x, y, w, h, type)
 {
   textItem = NULL;
   listBoxItem = NULL;
   buttonPressed = wxCANCEL;
 }
 Bool OnClose(void)
 {
   return TRUE;
 }
};

char *wxMessageBoxDialog::textAnswer = NULL;
char *wxMessageBoxDialog::listSelection = NULL;
char *wxMessageBoxDialog::listClientSelection = NULL;
int wxMessageBoxDialog::listPosition = 0;
int wxMessageBoxDialog::buttonPressed = wxCANCEL;
int *  wxMessageBoxDialog::listSelections = NULL;
int    wxMessageBoxDialog::nlistSelections = -1 ;

void wxDialogOkButton(wxButton& but, wxEvent& event)
{
  wxPanel *panel = (wxPanel *)but.GetParent();
  // There is a possibility that buttons belong to a sub panel.
  // So, we must search the dialog.
  while (!wxSubType(panel->__type,wxTYPE_DIALOG_BOX))
    panel = (wxPanel*) panel->GetParent() ;

  wxMessageBoxDialog *dialog = (wxMessageBoxDialog *)panel;
  if (dialog->textItem)
  {
    if (dialog->textAnswer)
      delete[] dialog->textAnswer;
    dialog->textAnswer = copystring(dialog->textItem->GetValue());
  }

  if (dialog->listBoxItem)
    {
      if ( dialog->listBoxItem->multiple==wxSINGLE)
        {
          if (dialog->listSelection)
            delete[] dialog->listSelection;
          dialog->listSelection = 
            (dialog->listBoxItem->GetStringSelection() ?
             copystring(dialog->listBoxItem->GetStringSelection()) : NULL);
          dialog->listPosition = dialog->listBoxItem->GetSelection();
          dialog->listClientSelection = 
            dialog->listBoxItem->wxListBox::GetClientData(dialog->listPosition);
        }
      else 
        if (dialog->listBoxItem->multiple==wxMULTIPLE)
          {
            if (dialog->listSelections)
              delete[] dialog->listSelections;
            dialog->listSelections = 0;
            
            int * sels;
            dialog-> nlistSelections =
              dialog->listBoxItem->GetSelections (&sels);
            if ( dialog-> nlistSelections)
              {
                dialog->listSelections = new int [ dialog-> nlistSelections ];
                for (int i=0; i<  dialog-> nlistSelections; i++)
                  dialog->listSelections[i] = sels[i];
              }
          }
    }

  dialog->buttonPressed = wxOK;
  dialog->Show(FALSE);
  delete dialog;
}

void wxDialogCancelButton(wxButton& but, wxEvent& event)
{
  wxDialogBox *dialog = (wxDialogBox *)but.GetParent();
  // There is a possibility that buttons belong to a sub panel.
  // So, we must search the dialog.
  while (!wxSubType(dialog->__type,wxTYPE_DIALOG_BOX))
    dialog = (wxDialogBox*) ((wxPanel*)dialog)->GetParent() ;

  dialog->Show(FALSE);
  delete dialog;
}

void wxDialogYesButton(wxButton& but, wxEvent& event)
{
  wxPanel *panel = (wxPanel *)but.GetParent();
  // There is a possibility that buttons belong to a sub panel.
  // So, we must search the dialog.
  while (!wxSubType(panel->__type,wxTYPE_DIALOG_BOX))
    panel = (wxPanel*) panel->GetParent() ;

  wxMessageBoxDialog *dialog = (wxMessageBoxDialog *)panel;
  dialog->buttonPressed = wxYES;
  dialog->Show(FALSE);
  delete dialog;
}

void wxDialogNoButton(wxButton& but, wxEvent& event)
{
  wxPanel *panel = (wxPanel *)but.GetParent();
  // There is a possibility that buttons belong to a sub panel.
  // So, we must search the dialog.
  while (!wxSubType(panel->__type,wxTYPE_DIALOG_BOX))
    panel = (wxPanel*) panel->GetParent() ;

  wxMessageBoxDialog *dialog = (wxMessageBoxDialog *)panel;
  dialog->buttonPressed = wxNO;
  dialog->Show(FALSE);
  delete dialog;
}


/*
 * BUGBUG Julian Smart 12/93
 * define USE_PANEL_IN_PANEL
 * if you dare use panel in panel for dialogs; I can't
 * get it to work without absolute positioning, because
 * wxItem::SetSize DOES NOT WORK (try the hello.cc About box.
 *
 */
#ifndef wx_mac // for Mac, this function is in wx_dialog.cc/wxGetText.cc
// Return NULL if cancel pressed
char *wxGetTextFromUser(char *message, char *caption, char *default_value,
                        wxWindow *parent, int x, int y, Bool centre)
{
  wxBeginBusyCursor();

  wxMessageBoxDialog *dialog = new wxMessageBoxDialog(parent, caption, TRUE, x, y, 1000, 1000, wxDEFAULT_DIALOG_STYLE);

  wxList messageList;
  wxSplitMessage(message, &messageList, dialog);

  dialog->NewLine();

  dialog->textItem = new wxText(dialog, NULL, NULL, default_value, -1, -1, 320);
  dialog->NewLine();

#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  wxPanel *but_panel = new wxPanel(dialog) ;
#else
  // Until sub panels work in XView mode
  wxPanel *but_panel = dialog ;
#endif

  wxButton *ok = new wxButton(but_panel, (wxFunction)&wxDialogOkButton, wxSTR_BUTTON_OK);
  (void)new wxButton(but_panel, (wxFunction)&wxDialogCancelButton, wxSTR_BUTTON_CANCEL);

  ok->SetDefault();
  dialog->textItem->SetFocus();

#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Fit() ;
#endif
  dialog->Fit();
#ifndef wx_xview
  but_panel->Centre(wxHORIZONTAL) ;
#endif

  if (centre)
    wxCentreMessage(&messageList);

  if ((x < 0) && (y < 0))
    dialog->Centre(wxBOTH);
  else if (x < 0)
    dialog->Centre(wxHORIZONTAL);
  else if (y < 0)
    dialog->Centre(wxVERTICAL);

  wxEndBusyCursor();
  dialog->Show(TRUE);

  if (wxMessageBoxDialog::buttonPressed == wxOK)
    return wxMessageBoxDialog::textAnswer;

  return NULL;
}
#endif

char *wxGetSingleChoice(char *message, char *caption, int n, char *choices[],
                        wxWindow *parent, int x, int y, Bool centre, int width, int height)
{
  wxBeginBusyCursor();

  wxMessageBoxDialog *dialog = new wxMessageBoxDialog(parent, caption, TRUE, x, y, 600, 600, wxDEFAULT_DIALOG_STYLE);

  wxList messageList;
  wxSplitMessage(message, &messageList, dialog);

  dialog->NewLine();

  dialog->listBoxItem = new wxListBox(dialog, NULL, NULL, wxSINGLE,
                     -1, -1, width, height, n, choices);

  dialog->listBoxItem->SetSelection(0);

  dialog->NewLine();
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  wxPanel *but_panel = new wxPanel(dialog) ;
#else
  // Until sub panels work in XView mode
  wxPanel *but_panel = dialog ;
#endif

  wxButton *ok = new wxButton(but_panel, (wxFunction)&wxDialogOkButton, wxSTR_BUTTON_OK);
  (void)new wxButton(but_panel, (wxFunction)&wxDialogCancelButton, wxSTR_BUTTON_CANCEL);
  // must call SetDefault AFTER creating all buttons of the subPanel
  // (see comment in x/wx_item.cc)
  ok->SetDefault();

#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Fit() ;
#endif
  dialog->Fit();
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Centre(wxHORIZONTAL) ;
#endif

  if (centre)
    wxCentreMessage(&messageList);

  if ((x < 0) && (y < 0))
    dialog->Centre(wxBOTH);
  else if (x < 0)
    dialog->Centre(wxHORIZONTAL);
  else if (y < 0)
    dialog->Centre(wxVERTICAL);

  wxEndBusyCursor();
  dialog->Show(TRUE);

  if (wxMessageBoxDialog::buttonPressed == wxOK) {
    if (wxMessageBoxDialog::listPosition >= 0)
      return wxMessageBoxDialog::listSelection;
  }

  return NULL;
}

int wxGetSingleChoiceIndex(char *message, char *caption, int n, char *choices[],
                           wxWindow *parent, int x, int y, Bool centre,
                           int width, int height)
{
  wxBeginBusyCursor();

  wxMessageBoxDialog *dialog = new wxMessageBoxDialog(parent, caption, TRUE, x, y, 600, 600, wxDEFAULT_DIALOG_STYLE);

  wxList messageList;
  wxSplitMessage(message, &messageList, dialog);

  dialog->NewLine();

  dialog->listBoxItem = new wxListBox(dialog, NULL, NULL, wxSINGLE,
                     -1, -1, width, height, n, choices);
  dialog->listBoxItem->SetSelection(0);
  dialog->NewLine();

  // Create Buttons in a sub-panel, so they can be centered.
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  wxPanel *but_panel = new wxPanel(dialog) ;
#else
  // Until sub panels work in XView mode
  wxPanel *but_panel = dialog ;
#endif

  wxButton *ok = new wxButton(but_panel, (wxFunction)&wxDialogOkButton, wxSTR_BUTTON_OK);
  (void)new wxButton(but_panel, (wxFunction)&wxDialogCancelButton, wxSTR_BUTTON_CANCEL);
  // SetDefault must be done AFTER creating all buttons
  ok->SetDefault();

#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Fit() ;
#endif
  dialog->Fit();
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Centre(wxHORIZONTAL) ;
#endif

  if (centre)
    wxCentreMessage(&messageList);

  if ((x < 0) && (y < 0))
    dialog->Centre(wxBOTH);
  else if (x < 0)
    dialog->Centre(wxHORIZONTAL);
  else if (y < 0)
    dialog->Centre(wxVERTICAL);

  wxEndBusyCursor();
  dialog->Show(TRUE);

  if (wxMessageBoxDialog::buttonPressed == wxOK) {
    return wxMessageBoxDialog::listPosition;
  }

  return -1;
}

char *wxGetSingleChoiceData(char *message, char *caption, int n,
                            char *choices[], char *client_data[],
                            wxWindow *parent, int x, int y, Bool centre,
                            int width, int height)
{
  wxBeginBusyCursor();

  wxMessageBoxDialog *dialog = new wxMessageBoxDialog(parent, caption, TRUE, x, y, 600, 600, wxDEFAULT_DIALOG_STYLE);

  wxList messageList;
  wxSplitMessage(message, &messageList, dialog);

  dialog->NewLine();

  dialog->listBoxItem = new wxListBox(dialog, NULL, NULL, wxSINGLE, -1, -1,
                     width, height);
  dialog->listBoxItem->SetSelection(0);
  int i;
  for (i = 0; i < n; i++)
    dialog->listBoxItem->Append(choices[i], client_data[i]);

  dialog->NewLine();

  // Create Buttons in a sub-panel, so they can be centered.
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  wxPanel *but_panel = new wxPanel(dialog) ;
#else
  // Until sub panels work in XView mode
  wxPanel *but_panel = dialog ;
#endif

  wxButton *ok = new wxButton(but_panel, (wxFunction)&wxDialogOkButton, wxSTR_BUTTON_OK);
  (void)new wxButton(but_panel, (wxFunction)&wxDialogCancelButton, wxSTR_BUTTON_CANCEL);
  // SetDefault must be done AFTER creating all buttons
  ok->SetDefault();

#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Fit() ;
#endif
  dialog->Fit();
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Centre(wxHORIZONTAL) ;
#endif

  if (centre)
    wxCentreMessage(&messageList);

  if ((x < 0) && (y < 0))
    dialog->Centre(wxBOTH);
  else if (x < 0)
    dialog->Centre(wxHORIZONTAL);
  else if (y < 0)
    dialog->Centre(wxVERTICAL);

  wxEndBusyCursor();
  dialog->Show(TRUE);

  if (wxMessageBoxDialog::buttonPressed == wxOK) {
    if (wxMessageBoxDialog::listPosition >= 0)
      return wxMessageBoxDialog::listClientSelection;
  }

  return NULL;
}

/* Multiple choice dialog contributed by Robert Cowell
 *

The new data passed are in the "int nsel" and "int * selection"

The idea is to make a multiple selection from list of strings.
The returned value is the total number selected. initialily there
are nsel selected, with indices stored in
selection[0],...,selection[nsel-1] which appear highlighted to
begin with. On exit with value i
selection[0..i-1] contains the indices of the selected items.
(Some prior selectecions might be deselected.)
Thus selection must be as big as choices, in case all items are
selected.

*/

int wxGetMultipleChoice(char *message, char *caption,
			  int n, char *choices[], 
			  int nsel, int * selection,
			  wxWindow *parent , int x , int y, Bool centre,
			  int width, int height)
{
  wxMessageBoxDialog *dialog = new wxMessageBoxDialog(parent, caption, TRUE, x, y, 1000, 1000, wxDEFAULT_DIALOG_STYLE);

  wxList messageList;
  wxSplitMessage(message, &messageList, dialog);

  dialog -> NewLine();

  dialog->listBoxItem = new wxListBox(dialog, NULL, NULL, wxMULTIPLE,
		    -1, -1, width, height, n, choices);
 
  for (int i=0; i < nsel; i++)
      dialog->listBoxItem ->SetSelection(selection[i],TRUE);
 
  dialog->NewLine();

  // Create Buttons in a sub-panel, so they can be centered.
#if (!defined(wx_xview) && defined(USE_PANEL_IN_PANEL))
  wxPanel *but_panel = new wxPanel(dialog) ;
#else
  // Until sub panels work in XView mode
  wxPanel *but_panel = dialog ;
#endif

  wxButton *ok = new wxButton(but_panel, (wxFunction)&wxDialogOkButton, "OK");
  (void)new wxButton(but_panel, (wxFunction)&wxDialogCancelButton, "Cancel");
  // SetDefault must be done AFTER creating all buttons
  ok->SetDefault();

  wxMessageBoxDialog::buttonPressed = 0;

#if (!defined(wx_xview) && defined(USE_PANEL_IN_PANEL))
  but_panel->Fit() ;
#endif
  dialog->Fit();
#if (!defined(wx_xview) && defined(USE_PANEL_IN_PANEL))
  but_panel->Centre(wxHORIZONTAL) ;
#endif

  if (centre)
    wxCentreMessage(&messageList);

  if ((x < 0) && (y < 0))
    dialog->Centre(wxBOTH);
  else if (x < 0)
    dialog->Centre(wxHORIZONTAL);
  else if (y < 0)
    dialog->Centre(wxVERTICAL);
  
  dialog->Show(TRUE);
  if (wxMessageBoxDialog::buttonPressed == wxOK)
    {
      int i;
      for( i=0; i<wxMessageBoxDialog::nlistSelections; i++)
         selection[i] = wxMessageBoxDialog::listSelections[i] ;
      return wxMessageBoxDialog::nlistSelections;
    }
   else 
      return -1;
}

// Pop up a message box: generic version used by X.
int wxbMessageBox(char *message, char *caption, long type,
                 wxWindow *parent, int x, int y)
{
  wxBeginBusyCursor();

  wxMessageBoxDialog *dialog = new wxMessageBoxDialog(parent, caption, TRUE, x, y, 1000, 1000, wxDEFAULT_DIALOG_STYLE);

  Bool centre = ((type & wxCENTRE) == wxCENTRE);

  wxList messageList;
  wxSplitMessage(message, &messageList, dialog);

  dialog->NewLine();

  // Create Buttons in a sub-panel, so they can be centered.
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  wxPanel *but_panel = new wxPanel(dialog) ;
#else
  // Until sub panels work in XView mode
  wxPanel *but_panel = dialog ;
#endif

  wxButton *ok = NULL;
  wxButton *cancel = NULL;
  wxButton *yes = NULL;
  wxButton *no = NULL;

  if (type & wxYES_NO) {
    yes = new wxButton(but_panel, (wxFunction)&wxDialogYesButton, wxSTR_YES);
    no = new wxButton(but_panel, (wxFunction)&wxDialogNoButton, wxSTR_NO);
  }

  if (type & wxOK) {
    ok = new wxButton(but_panel, (wxFunction)&wxDialogOkButton, wxSTR_BUTTON_OK);
  }

  if (type & wxCANCEL) {
    cancel = new wxButton(but_panel, (wxFunction)&wxDialogCancelButton, wxSTR_BUTTON_CANCEL);
  }

  if (ok)
  {
    ok->SetDefault();
    ok->SetFocus();
  }
  else if (yes)
  {
    yes->SetDefault();
    yes->SetFocus();
  }

#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Fit() ;
#endif
  dialog->Fit();
#if (!defined(wx_xview) && USE_PANEL_IN_PANEL)
  but_panel->Centre(wxHORIZONTAL) ;
#endif

  // Do the message centering
  if (centre)
    wxCentreMessage(&messageList);

#if (defined(wx_xview) || !USE_PANEL_IN_PANEL)
  // Since subpanels don't work on XView, we must center ok button
  if (ok && !cancel && !yes && !no)
    ok->Centre();
#endif

  if ((x < 0) && (y < 0))
    dialog->Centre(wxBOTH);
  else if (x < 0)
    dialog->Centre(wxHORIZONTAL);
  else if (y < 0)
    dialog->Centre(wxVERTICAL);

  wxEndBusyCursor();
  dialog->Show(TRUE);

  return wxMessageBoxDialog::buttonPressed;
}
