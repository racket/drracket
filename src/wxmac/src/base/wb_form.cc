/*
 * File:      wb_form.cc
 * Purpose:     Form handling code
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_form.cc,v 1.4 1994/08/14 21:34:01 edz Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "@(#)wb_form.cc	1.2 5/9/94"; */

#ifdef __GNUG__
#pragma implementation "wx_form.h"
#pragma implementation
#pragma interface
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_utils.h"
#include "wx_dialg.h"
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
#include "wx_rbox.h"
#include "wx_hash.h"
#include "wx_mgstr.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

// Max. message length
#define _MAX_MESSAGE_LEN 300

#if USE_FORM

#include "wx_form.h"

#include <stdio.h>

// Sun CC compatibility (interference with xview/pkg.h, apparently...)
#if defined(SUN_CC) && defined(wx_xview)
#undef va_start
#undef va_end
#undef va_arg
#undef va_list
#endif

#include <stdarg.h>

// Helper function
wxFormItemConstraint *wxFormConstraintsContains (int type, wxList * constraints);

// Display items and callback functions
class wxFormButton:public wxButton
{
  public:
  wxForm * form;
  wxFormButton (wxPanel * panel, wxFunction func, char *label,
		int x = -1, int y = -1, int width = -1, int height = -1);
};

static void wxFormOk (wxFormButton & button, wxEvent & event);
static void wxFormRevert (wxFormButton & button, wxEvent & event);
static void wxFormCancel (wxFormButton & button, wxEvent & event);
static void wxFormUpdate (wxFormButton & button, wxEvent & event);
static void wxFormHelp (wxFormButton & button, wxEvent & event);

/*
 * Range
 *
 */

wxRealRange::wxRealRange (float the_lo, float the_hi)
{
  lo = the_lo;
  hi = the_hi;
}

/*
 * Form item constraint
 *
 */

wxFormItemConstraint::wxFormItemConstraint (int type)
{
  Type = type;
  localList = FALSE;
}

wxFormItemConstraint::~wxFormItemConstraint (void)
{
  switch (Type)
    {
    case wxFORM_CONSTRAINT_ONE_OF:
      {
	if (Constraint.OneOf)
	  {
	    wxNode *node = Constraint.OneOf->First ();
	    while (node)
	      {
		char *s = (char *) node->Data ();
		wxNode *next = node->Next ();
		delete[]s;
		delete node;
		node = next;
	      }
	    if (localList)
	      delete Constraint.OneOf;
	  }
	break;
      }
    case wxFORM_CONSTRAINT_RANGE:
      {
	if (Constraint.Range)
	  delete Constraint.Range;
	break;
      }
    default:
      break;
    }
}

/*
 * Form item
 *
 */

wxFormItem::wxFormItem (int type, int item_type)
{
  Id = 0;
  Type = type;
  ItemType = item_type;
  PanelItem = NULL;
  Constraints = NULL;
  style = 0;
  HelpString = NULL;
  Label = NULL;
  Height = -1;
  Width = -1;
}

wxFormItem::~wxFormItem (void)
{
  if (HelpString)
    delete[]HelpString;
  if (Label)
    delete[]Label;

/*** Patrick: Nooo... this pointer is to the user's data; don't want to
 * delete this!

  if (Type==wxFORM_STRING && *ValuePtr.StringValuePtr)
  {
    delete[] (*ValuePtr.StringValuePtr);
    *ValuePtr.StringValuePtr = NULL ;
  }
 */

  Form->FormItems.DeleteObject (this);

  if (Constraints)
    {
      for (wxNode * node = Constraints->First (); node; node = node->Next ())
	{
	  wxFormItemConstraint *constraint = (wxFormItemConstraint *) node->Data ();
	  delete constraint;
	}
      delete Constraints;
    }
}

void wxFormItem::MakePanelItem (wxPanel * panel)
{
  switch (Type)
    {
    case wxFORM_STRING:
      {
	switch (ItemType)
	  {
	  case wxFORM_SINGLE_LIST:
	    {
	      if (style & wxVERTICAL)
		panel->SetLabelPosition (wxVERTICAL);

	      PanelItem = new wxListBox (panel, (wxFunction) NULL, Label, wxSINGLE,
					 -1, -1, Width, Height);

	      if (style & wxVERTICAL)
		panel->SetLabelPosition (wxHORIZONTAL);

	      wxListBox *listbox = (wxListBox *) PanelItem;
	      wxFormItemConstraint *constraint =
	      wxFormConstraintsContains (wxFORM_CONSTRAINT_ONE_OF, Constraints);

	      if (constraint)
		for (wxNode * node = constraint->Constraint.OneOf->First (); node; node = node->Next ())
		  {
		    char *s = (char *) node->Data ();
		    listbox->Append (s);
		  }
	      break;
	    }
	  case wxFORM_CHOICE:
	    {
	      wxFormItemConstraint *constraint =
	      wxFormConstraintsContains (wxFORM_CONSTRAINT_ONE_OF, Constraints);
	      if (constraint)
		{
		  int n = constraint->Constraint.OneOf->Number ();
		  char **strings = new char *[n];

		  int i = 0;
		  for (wxNode * node = constraint->Constraint.OneOf->First (); node; node = node->Next ())
		    strings[i++] = (char *) node->Data ();

		  if (style & wxVERTICAL)
		    panel->SetLabelPosition (wxVERTICAL);

		  PanelItem = new wxChoice (panel, (wxFunction) NULL, Label,
					 -1, -1, Width, Height, n, strings);
		  delete[]strings;

		  if (style & wxVERTICAL)
		    panel->SetLabelPosition (wxHORIZONTAL);
		}
	      break;
	    }
	  case wxFORM_RADIOBOX:
	    {
	      wxFormItemConstraint *constraint =
	      wxFormConstraintsContains (wxFORM_CONSTRAINT_ONE_OF, Constraints);
	      int n = constraint->Constraint.OneOf->Number ();
	      char *choices[50];	// @@@@ Max 50 Radio Buttons in a group!

	      int i = 0;
	      if (constraint)
		{
		  for (wxNode * node = constraint->Constraint.OneOf->First (); node; node = node->Next ())
		    choices[i++] = (char *) node->Data ();

		  panel->SetLabelPosition (wxVERTICAL);

		  PanelItem = new wxRadioBox (panel, (wxFunction) NULL, Label,
			      -1, -1, Width, Height, n, choices, 1, wxFLAT);
		  panel->SetLabelPosition (wxHORIZONTAL);
		}
	      break;
	    }
	  case wxFORM_TEXT:
	    {
	      if (style & wxVERTICAL)
		panel->SetLabelPosition (wxVERTICAL);
	      PanelItem = new wxText (panel, (wxFunction) NULL, Label, "", -1, -1,
				      Width, Height);
	      if (style & wxVERTICAL)
		panel->SetLabelPosition (wxHORIZONTAL);
	      break;
	    }
	  case wxFORM_MULTITEXT:
	    {
	      if (style & wxVERTICAL)
		panel->SetLabelPosition (wxVERTICAL);
	      PanelItem = new wxMultiText (panel, (wxFunction) NULL, Label, "", -1, -1,
					   Width, Height);
	      if (style & wxVERTICAL)
		panel->SetLabelPosition (wxHORIZONTAL);
	      break;
	    }
	  default:
	    break;
	  }
	break;
      }
    case wxFORM_DUMB_MESSAGE:
      {
	PanelItem = new wxMessage (panel, Label);
	break;
      }
    case wxFORM_BUTTON:
      {
	PanelItem = new wxButton (panel, ButtonFunc, Label, -1, -1, Width, Height);
	break;
      }
    case wxFORM_NEWLINE:
      {
	panel->NewLine ();
	break;
      }
    case wxFORM_SHORT:
    case wxFORM_LONG:
      {
	switch (ItemType)
	  {
	  case wxFORM_SLIDER:
	    {
	      int lo = -100;
	      int hi = 100;
	      int width = 230;
	      wxFormItemConstraint *constraint =
	      wxFormConstraintsContains (wxFORM_CONSTRAINT_RANGE, Constraints);
	      if (constraint)
		{
		  wxRealRange *range = constraint->Constraint.Range;
		  lo = (int) range->lo;
		  hi = (int) range->hi;
		}
	      int default_value;
	      if (Type == wxFORM_SHORT)
		default_value = *(ValuePtr.ShortIntValuePtr);
	      else
		default_value = (int) *(ValuePtr.LongIntValuePtr);

	      if (style & wxVERTICAL)
		panel->SetLabelPosition (wxVERTICAL);
	      PanelItem = new wxSlider (panel, (wxFunction) NULL, Label,
					default_value, lo, hi, width);
	      if (style & wxVERTICAL)
		panel->SetLabelPosition (wxHORIZONTAL);
	      break;
	    }
	  case wxFORM_TEXT:
	  default:
	    {
	      if (style & wxVERTICAL)
		panel->SetLabelPosition (wxVERTICAL);
	      PanelItem = new wxText (panel, (wxFunction) NULL, Label, "", -1, -1,
				      Width, Height);
	      if (style & wxVERTICAL)
		panel->SetLabelPosition (wxHORIZONTAL);
	      break;
	    }
	  }
	break;
      }

    case wxFORM_BOOL:
      {
	switch (ItemType)
	  {
	  case wxFORM_CHECKBOX:
	  default:
	    {
	      if (style & wxVERTICAL)
		panel->SetLabelPosition (wxVERTICAL);
	      PanelItem = new wxCheckBox (panel, (wxFunction) NULL, Label, -1, -1,
					  Width, Height);
	      if (style & wxVERTICAL)
		panel->SetLabelPosition (wxHORIZONTAL);
	      break;
	    }
	  }
	break;
      }
    case wxFORM_FLOAT:
      {
	switch (ItemType)
	  {
	  case wxFORM_TEXT:
	  default:
	    {
	      if (style & wxVERTICAL)
		panel->SetLabelPosition (wxVERTICAL);
	      PanelItem = new wxText (panel, (wxFunction) NULL, Label, "", -1, -1,
				      Width, Height);
	      if (style & wxVERTICAL)
		panel->SetLabelPosition (wxHORIZONTAL);
	      break;
	    }
	  }
	break;
      }
    case wxFORM_LIST_OF_STRINGS:
      {
	break;
      }
    case wxFORM_LIST_OF_LONGS:
      {
	break;
      }

    default:
      break;
    }
}

Bool wxFormItem::CheckLongValue (long val)
{
  char msg_buf[_MAX_MESSAGE_LEN];
  if (Constraints)
    {
      wxNode *node = Constraints->First ();
      Bool ok = TRUE;
      while (node && ok)
	{
	  wxFormItemConstraint *constraint = (wxFormItemConstraint *) node->Data ();
	  switch (constraint->Type)
	    {
	    case wxFORM_CONSTRAINT_RANGE:
	      {
		wxRealRange *range = constraint->Constraint.Range;
		if ((range->lo > (float) val) || (range->hi < (float) val))
		  {
		    sprintf (msg_buf, wxSTR_VIOLATION_LONG,
			     Label, range->lo, range->hi);
		    ok = FALSE;
		  }
		break;
	      }
	    case wxFORM_CONSTRAINT_FUNCTION:
	      {
		ok = (*(constraint->Constraint.ConstraintFunc)) (Type, (char *) &val, Label, msg_buf);
		break;
	      }
	    default:
	      break;
	    }
	  if (!ok)
	    {
	      (void) wxMessageBox (msg_buf, wxSTR_CONSTRAINT_VIOLATION, wxOK, Form->wx_form_panel);
	    }
	  node = node->Next ();
	}
      return ok;
    }
  else
    return TRUE;
}

Bool wxFormItem::CheckBoolValue (Bool val)
{
  char msg_buf[_MAX_MESSAGE_LEN];
  if (Constraints)
    {
      wxNode *node = Constraints->First ();
      Bool ok = TRUE;
      while (node && ok)
	{
	  wxFormItemConstraint *constraint = (wxFormItemConstraint *) node->Data ();
	  switch (constraint->Type)
	    {
	    case wxFORM_CONSTRAINT_FUNCTION:
	      {
		ok = (*(constraint->Constraint.ConstraintFunc)) (Type, (char *) &val, Label, msg_buf);
		break;
	      }
	    default:
	      break;
	    }
	  if (!ok)
	    {
	      (void) wxMessageBox (msg_buf, wxSTR_CONSTRAINT_VIOLATION, wxOK, Form->wx_form_panel);
	    }
	  node = node->Next ();
	}
      return ok;
    }
  else
    return TRUE;
}

Bool wxFormItem::CheckStringValue (char *val)
{
  char msg_buf[_MAX_MESSAGE_LEN];
  Bool ok = TRUE;

  if (Constraints)
    {
      // Walk through and call the functions
      for (wxNode * node = Constraints->First (); node && ok; node = node->Next ())
	{
	  wxFormItemConstraint *constraint = (wxFormItemConstraint *) node->Data ();

	  switch (constraint->Type)
	    {
	    case wxFORM_CONSTRAINT_FUNCTION:
	      ok = (*(constraint->Constraint.ConstraintFunc)) (Type, val, Label, msg_buf);
	      break;
	    default:
	      break;
	    }			// switch()

	  if (!ok)
	    (void) wxMessageBox (msg_buf, wxSTR_CONSTRAINT_VIOLATION, wxOK, Form->wx_form_panel);
	}			// for()

    }
  return ok;
}

Bool wxFormItem::CheckFloatValue (float val)
{
  char msg_buf[_MAX_MESSAGE_LEN];
  if (Constraints)
    {
      wxNode *node = Constraints->First ();
      Bool ok = TRUE;
      while (node && ok)
	{
	  wxFormItemConstraint *constraint = (wxFormItemConstraint *) node->Data ();
	  switch (constraint->Type)
	    {
	    case wxFORM_CONSTRAINT_RANGE:
	      {
		wxRealRange *range = constraint->Constraint.Range;
		if ((range->lo > val) || (range->hi < val))
		  {
		    sprintf (msg_buf, wxSTR_VIOLATION_FLOAT,
			     Label, range->lo, range->hi);
		    ok = FALSE;
		  }
		break;
	      }
	    case wxFORM_CONSTRAINT_FUNCTION:
	      {
		ok = (*(constraint->Constraint.ConstraintFunc)) (Type, (char *) &val, Label, msg_buf);
		break;
	      }
	    default:
	      break;
	    }
	  if (!ok)
	    {
	      (void) wxMessageBox (msg_buf, wxSTR_CONSTRAINT_VIOLATION, wxOK, Form->wx_form_panel);
	    }
	  node = node->Next ();
	}
      return ok;
    }
  else
    return TRUE;
}

void wxFormItem::RevertValue (void)
{
  switch (Type)
    {
    case wxFORM_SHORT:
      {
	switch (ItemType)
	  {
	  case wxFORM_TEXT:
	    {
	      wxText *text_item = (wxText *) PanelItem;
	      char *svalue = IntToString (*(ValuePtr.ShortIntValuePtr));
	      if (svalue)
		{
		  text_item->SetValue (svalue);
		  // no more malloced
		  //delete[] svalue;
		}
	      break;
	    }
	  case wxFORM_SLIDER:
	    {
	      wxSlider *slider_item = (wxSlider *) PanelItem;
	      slider_item->SetValue (*(ValuePtr.ShortIntValuePtr));
	      break;
	    }
	  default:
	    break;
	  }
	break;
      }
    case wxFORM_LONG:
      {
	switch (ItemType)
	  {
	  case wxFORM_TEXT:
	    {
	      wxText *text_item = (wxText *) PanelItem;
	      char *svalue = LongToString (*(ValuePtr.LongIntValuePtr));
	      if (svalue)
		{
		  text_item->SetValue (svalue);
		  // no more malloced
		  //delete[] svalue;
		}
	      break;
	    }
	  case wxFORM_SLIDER:
	    {
	      wxSlider *slider_item = (wxSlider *) PanelItem;
	      slider_item->SetValue ((int) *(ValuePtr.LongIntValuePtr));
	      break;
	    }
	  default:
	    break;
	  }
	break;
      }
    case wxFORM_FLOAT:
      {
	switch (ItemType)
	  {
	  case wxFORM_TEXT:
	    {
	      wxText *text_item = (wxText *) PanelItem;
	      char *svalue = FloatToString (*(ValuePtr.FloatValuePtr));
	      if (svalue)
		{
		  text_item->SetValue (svalue);
		  // no more malloced
		  //delete[] svalue;
		}
	      break;
	    }
	  default:
	    break;
	  }
	break;
      }
    case wxFORM_BOOL:
      {
	switch (ItemType)
	  {
	  case wxFORM_CHECKBOX:
	    {
	      wxCheckBox *check_item = (wxCheckBox *) PanelItem;
	      check_item->SetValue (*(ValuePtr.BoolValuePtr));
	      break;
	    }
	  default:
	    break;
	  }
	break;
      }
    case wxFORM_STRING:
      {
	switch (ItemType)
	  {
	  case wxFORM_TEXT:
	  case wxFORM_MULTITEXT:
	    {
	      wxText *text_item = (wxText *) PanelItem;
	      char *svalue = *(ValuePtr.StringValuePtr);
	      if (svalue)
		{
		  text_item->SetValue (svalue);
		}
	      break;
	    }
	  case wxFORM_SINGLE_LIST:
	    {
	      wxListBox *list_item = (wxListBox *) PanelItem;
	      char *svalue = *(ValuePtr.StringValuePtr);
	      if (svalue)
		{
		  int pos = list_item->FindString (svalue);
		  if (pos > -1)
		    list_item->SetSelection (pos);
		  else
		    list_item->SetSelection (0);
		}
	      else
		list_item->SetSelection (0);
	      break;
	    }
	  case wxFORM_CHOICE:
	    {
	      wxChoice *choice_item = (wxChoice *) PanelItem;
	      char *svalue = *(ValuePtr.StringValuePtr);
	      if (svalue)
		{
		  int pos = choice_item->FindString (svalue);
		  if (pos > -1)
		    choice_item->SetSelection (pos);
		  else
		    choice_item->SetSelection (0);
		}
	      else
		choice_item->SetSelection (0);
	      break;
	    }
	  case wxFORM_RADIOBOX:
	    {
	      wxRadioBox *radiobox_item = (wxRadioBox *) PanelItem;
	      char *svalue = *(ValuePtr.StringValuePtr);
	      if (svalue)
		{
		  int pos = radiobox_item->FindString (svalue);
		  if (pos > -1)
		    radiobox_item->SetSelection (pos);
		  else
		    radiobox_item->SetSelection (0);
		}
	      else
		radiobox_item->SetSelection (0);
	      break;
	    }
	  default:
	    break;
	  }
	break;
      }
    default:
      break;
    }
}

Bool wxFormItem::UpdateValue (void)
{
  switch (Type)
    {
    case wxFORM_SHORT:
      {
	int short_value = 0;
	switch (ItemType)
	  {
	  case wxFORM_TEXT:
	  case wxFORM_MULTITEXT:
	    {
	      wxText *text_item = (wxText *) PanelItem;
	      char *svalue = text_item->GetValue ();
	      StringToInt (svalue, &short_value);
	      break;
	    }
	  case wxFORM_SLIDER:
	    {
	      wxSlider *slider_item = (wxSlider *) PanelItem;
	      short_value = slider_item->GetValue ();
	      break;
	    }
	  default:
	    break;
	  }
	if (CheckLongValue ((long) short_value))
	  *(ValuePtr.ShortIntValuePtr) = short_value;
	else
	  return FALSE;
	break;
      }
    case wxFORM_LONG:
      {
	long long_value;
	switch (ItemType)
	  {
	  case wxFORM_TEXT:
	    {
	      wxText *text_item = (wxText *) PanelItem;
	      char *svalue = text_item->GetValue ();
	      StringToLong (svalue, &long_value);
	      break;
	    }
	  case wxFORM_SLIDER:
	    {
	      wxSlider *slider_item = (wxSlider *) PanelItem;
	      long_value = slider_item->GetValue ();
	      break;
	    }
	  default:
	    break;
	  }
	if (CheckLongValue (long_value))
	  *(ValuePtr.LongIntValuePtr) = long_value;
	else
	  return FALSE;
	break;
      }
    case wxFORM_FLOAT:
      {
	float float_value;
	switch (ItemType)
	  {
	  case wxFORM_TEXT:
	    {
	      wxText *text_item = (wxText *) PanelItem;
	      char *svalue = text_item->GetValue ();
	      StringToFloat (svalue, &float_value);
	      break;
	    }
	  default:
	    break;
	  }
	if (CheckFloatValue (float_value))
	  *(ValuePtr.FloatValuePtr) = float_value;
	else
	  return FALSE;
	break;
      }
    case wxFORM_BOOL:
      {
	Bool bool_value = FALSE;
	switch (ItemType)
	  {
	  case wxFORM_CHECKBOX:
	    {
	      wxCheckBox *check_item = (wxCheckBox *) PanelItem;
	      bool_value = check_item->GetValue ();
	      break;
	    }
	  default:
	    break;
	  }
	if (CheckBoolValue (bool_value))
	  *(ValuePtr.BoolValuePtr) = bool_value;
	else
	  return FALSE;
	break;
      }
    case wxFORM_STRING:
      {
	char *string_value = NULL;
	switch (ItemType)
	  {
	  case wxFORM_TEXT:
	  case wxFORM_MULTITEXT:
	    {
	      wxText *text_item = (wxText *) PanelItem;
	      string_value = text_item->GetValue ();
	      break;
	    }
	  case wxFORM_SINGLE_LIST:
	    {
	      wxListBox *list_item = (wxListBox *) PanelItem;
	      string_value = list_item->GetStringSelection ();
	      break;
	    }
	  case wxFORM_CHOICE:
	    {
	      wxChoice *choice_item = (wxChoice *) PanelItem;
	      string_value = choice_item->GetStringSelection ();
	      break;
	    }
	  case wxFORM_RADIOBOX:
	    {
	      wxRadioBox *radiobox_item = (wxRadioBox *) PanelItem;
	      string_value = radiobox_item->GetStringSelection ();
	      break;
	    }
	  default:
	    break;
	  }

	// Check constraint values
	if (CheckStringValue (string_value))
	  {
	    if (*ValuePtr.StringValuePtr)
	      delete[](*ValuePtr.StringValuePtr);
	    *(ValuePtr.StringValuePtr) = copystring (string_value);
	  }
	else
	  {
	    return FALSE;
	  }
	break;
      }
    default:
      break;
    }
  return TRUE;
}

/*
 * Form
 *
 */

wxForm::wxForm (int button_use, int button_place)
{
  wx_form_panel = NULL;
  wx_editable = TRUE;
  buttonUse = button_use;
  buttonPlace = button_place;
}

wxForm::~wxForm (void)
{
  wxNode *node = FormItems.First ();
  while (node)
    {
      wxFormItem *item = (wxFormItem *) node->Data ();
      wxNode *next = node->Next ();
      delete item;
      node = next;
    }
}

void wxForm::Add (wxFormItem * item, long id)
{
  if (id < 0)
    id = NewId ();
  item->Id = id;
  item->Form = this;
  FormItems.Append (item);
}

wxNode *wxForm::FindItem (long id)
{
  for (wxNode * node = FormItems.First (); node; node = node->Next ())
    if (((wxFormItem *) node->Data ())->Id == id)
      return node;		// Found it

  return NULL;
}


Bool wxForm::Set (long id, wxFormItem * item)
{
  wxNode *found = FindItem (id);
  if (found)
    {
      wxNode *next = found->Next ();
      wxFormItem *old_item = (wxFormItem *) found->Data ();
      delete old_item;
      FormItems.Insert (next, item);
      return TRUE;
    }
  return FALSE;
}

Bool wxForm::Delete (long id)
{
  wxNode *found = FindItem (id);
  if (found)
    {
      wxFormItem *old_item = (wxFormItem *) found->Data ();
      delete old_item;
      return TRUE;
    }
  return FALSE;
}

void wxForm::AssociatePanel (wxPanel * panel)
{
  wx_form_panel = panel;

  if (buttonPlace == wxFORM_BUTTON_AT_TOP)
    {
      MakeAndPlaceButtons (wx_form_panel);
      panel->NewLine ();
    }

  panel->NewLine ();

  for (wxNode * node = FormItems.First (); node; node = node->Next ())
    {
      wxFormItem *item = (wxFormItem *) node->Data ();
      item->MakePanelItem (panel);
      item->RevertValue ();
    }				// for()

  if (buttonPlace == wxFORM_BUTTON_AT_BOTTOM)
    {
      panel->NewLine ();
      MakeAndPlaceButtons (wx_form_panel);
    }

//  panel->Fit();
}

void wxForm::MakeAndPlaceButtons (wxPanel * panel)
{
  wxFormButton *ok_button = NULL;
  if (buttonUse & wxFORM_BUTTON_OK)
    {
      ok_button = new wxFormButton (panel, (wxFunction) wxFormOk, wxSTR_BUTTON_OK);
      ok_button->form = this;
    }

  if (wx_editable && (buttonUse & wxFORM_BUTTON_UPDATE))
    {
      wxFormButton *update_button = new wxFormButton (panel, (wxFunction) wxFormUpdate, wxSTR_BUTTON_UPDATE);
      update_button->form = this;
    }

  if (buttonUse & wxFORM_BUTTON_REVERT)
    {
      wxFormButton *revert_button = new wxFormButton (panel, (wxFunction) wxFormRevert, wxSTR_BUTTON_REVERT);
      revert_button->form = this;
    }

  if (wx_editable && (buttonUse & wxFORM_BUTTON_CANCEL))
    {
      wxFormButton *cancel_button = new wxFormButton (panel, (wxFunction) wxFormCancel, wxSTR_BUTTON_CANCEL);
      cancel_button->form = this;
    }

  if (buttonUse & wxFORM_BUTTON_HELP)
    {
      wxFormButton *help_button = new wxFormButton (panel, (wxFunction) wxFormHelp, wxSTR_BUTTON_HELP);
      help_button->form = this;
    }

  if (ok_button)
    ok_button->SetDefault();
}

void wxForm::DisassociatePanel (void)
{
}

Bool wxForm::UpdateValues (void)
{
  wxNode *node = FormItems.First ();
  Bool success = TRUE;
  while (node && success)
    {
      wxFormItem *item = (wxFormItem *) node->Data ();
      if (!(item->UpdateValue ()))
	success = FALSE;
      node = node->Next ();
    }

  return success;
}

void wxForm::RevertValues (void)
{
  for(wxNode *node = FormItems.First (); node; node = node->Next ())
    {
      wxFormItem *item = (wxFormItem *) node->Data ();
      item->RevertValue ();
    }
}

void wxForm::OnOk (void)
{
  if (wx_form_panel)
    {
      wx_form_panel->Show (FALSE);
      delete wx_form_panel;
    }
  delete this;
}

void wxForm::OnCancel (void)
{
  if (wx_form_panel)
    {
      wx_form_panel->Show (FALSE);
      delete wx_form_panel;
    }
  delete this;
}

void wxForm::OnHelp (void)
{
}

void wxForm::OnRevert (void)
{
}

void wxForm::OnUpdate (void)
{
}

/*
 * Functions for creating items
 *
 */

wxFormItem *
wxMakeFormString (char *label, char **var,
		  int item_type, wxList * constraints,
		  char *help_string, int style, int width, int height)
{
  if (item_type == wxFORM_DEFAULT)
    {
      // Must decide what type of item to use.
      // If constraint list contains a OneOf, use a single-choice list box.
      // Else use a simple Text item
      if (wxFormConstraintsContains (wxFORM_CONSTRAINT_ONE_OF, constraints))
	item_type = wxFORM_SINGLE_LIST;
      else
	item_type = wxFORM_TEXT;
    }
  wxFormItem *item = new wxFormItem (wxFORM_STRING, item_type);
  item->Constraints = constraints;
  item->style = style;
  if (help_string)
    item->HelpString = copystring (help_string);
  item->ValuePtr.StringValuePtr = var;
  if (label)
    item->Label = copystring (label);
  item->Value.StringValue = NULL;
  item->Height = height;
  item->Width = width;
  return item;
}

wxFormItem *
wxMakeFormMessage (char *label)
{
  wxFormItem *item = new wxFormItem (wxFORM_DUMB_MESSAGE, wxFORM_DUMB_MESSAGE);
  if (label)
    item->Label = copystring (label);
  return item;
}

wxFormItem *
wxMakeFormButton (char *label, wxFunction func)
{
  wxFormItem *item = new wxFormItem (wxFORM_BUTTON, wxFORM_BUTTON);
  if (label)
    item->Label = copystring (label);
  item->ButtonFunc = func;
  return item;
}

wxFormItem *
wxMakeFormNewLine (void)
{
  return new wxFormItem (wxFORM_NEWLINE, wxFORM_NEWLINE);
}

wxFormItem *
wxMakeFormLong (char *label, long *var,
		int item_type, wxList * constraints,
		char *help_string, int style,
		int width, int height)
{
  if (item_type == wxFORM_DEFAULT)
    {
      if (wxFormConstraintsContains (wxFORM_CONSTRAINT_RANGE, constraints))
	item_type = wxFORM_SLIDER;
      else
	item_type = wxFORM_TEXT;
    }
  wxFormItem *item = new wxFormItem (wxFORM_LONG, item_type);
  item->Constraints = constraints;
  item->style = style;
  if (help_string)
    item->HelpString = copystring (help_string);
  item->ValuePtr.LongIntValuePtr = var;
  if (label)
    item->Label = copystring (label);
  item->Value.LongIntValue = 0;
  item->Height = height;
  item->Width = width;
  return item;
}

wxFormItem *
wxMakeFormShort (char *label, int *var,
		 int item_type, wxList * constraints,
		 char *help_string, int style,
		 int width, int height)
{
  if (item_type == wxFORM_DEFAULT)
    {
      if (wxFormConstraintsContains (wxFORM_CONSTRAINT_RANGE, constraints))
	item_type = wxFORM_SLIDER;
      else
	item_type = wxFORM_TEXT;
    }
  wxFormItem *item = new wxFormItem (wxFORM_SHORT, item_type);
  item->Constraints = constraints;
  item->style = style;
  if (help_string)
    item->HelpString = copystring (help_string);
  item->ValuePtr.ShortIntValuePtr = var;
  if (label)
    item->Label = copystring (label);
  item->Value.ShortIntValue = 0;
  item->Height = height;
  item->Width = width;
  return item;
}

wxFormItem *
wxMakeFormFloat (char *label, float *var,
		 int item_type, wxList * constraints,
		 char *help_string, int style,
		 int width, int height)
{
  if (item_type == wxFORM_DEFAULT)
    {
      item_type = wxFORM_TEXT;
    }
  wxFormItem *item = new wxFormItem (wxFORM_FLOAT, item_type);
  item->Constraints = constraints;
  item->style = style;
  if (help_string)
    item->HelpString = copystring (help_string);
  item->ValuePtr.FloatValuePtr = var;
  if (label)
    item->Label = copystring (label);
  item->Value.FloatValue = 0.0;
  item->Height = height;
  item->Width = width;
  return item;
}

wxFormItem *
wxMakeFormBool (char *label, Bool * var,
		int item_type, wxList * constraints,
		char *help_string, int style,
		int width, int height)
{
  if (item_type == wxFORM_DEFAULT)
    {
      item_type = wxFORM_CHECKBOX;
    }
  wxFormItem *item = new wxFormItem (wxFORM_BOOL, item_type);
  item->Constraints = constraints;
  item->style = style;
  if (help_string)
    item->HelpString = copystring (help_string);
  item->ValuePtr.BoolValuePtr = var;
  if (label)
    item->Label = copystring (label);
  item->Value.BoolValue = 0;
  item->Height = height;
  item->Width = width;
  return item;
}

/*
 * Functions for creating constraints
 *
 */

wxFormItemConstraint *
wxMakeConstraintStrings (wxList * list)
{
  wxFormItemConstraint *constraint =
  new wxFormItemConstraint (wxFORM_CONSTRAINT_ONE_OF);
  constraint->Constraint.OneOf = list;
  return constraint;
}

wxFormItemConstraint *
wxMakeConstraintStrings (char *first...)
{
// #ifndef __sgi
  wxList *list = new wxList;
  va_list ap;

  va_start (ap, first);
  list->Append ((wxObject *) copystring (first));

  for (;;)
    {
      char *s = va_arg (ap, char *);
      if ( s )
	list->Append ((wxObject *) copystring (s));
      else
	break;
    }

  va_end (ap);

  wxFormItemConstraint *constraint =
  new wxFormItemConstraint (wxFORM_CONSTRAINT_ONE_OF);
  constraint->Constraint.OneOf = list;
  constraint->localList = TRUE;	// This list MUST BE deleted.

  return constraint;
/*
#else
  fprintf (stderr, "Error: cannot use variable-argument functions on SGI!\n");
  return NULL;
#endif
*/
}

wxFormItemConstraint *
wxMakeConstraintRange (float lo, float hi)
{
  wxFormItemConstraint *constraint =
  new wxFormItemConstraint (wxFORM_CONSTRAINT_RANGE);
  constraint->Constraint.Range = new wxRealRange (lo, hi);
  return constraint;
}

wxFormItemConstraint *
wxMakeConstraintFunction (wxConstraintFunction func)
{
  wxFormItemConstraint *constraint =
  new wxFormItemConstraint (wxFORM_CONSTRAINT_FUNCTION);
  constraint->Constraint.ConstraintFunc = func;
  return constraint;
}

/*
 * Helper functions
 *
 */

wxFormItemConstraint *
wxFormConstraintsContains (int type, wxList * constraints)
{
  if (constraints)
   for(wxNode *node = constraints->First (); node; node = node->Next())
    {
      wxFormItemConstraint *constraint = (wxFormItemConstraint *) node->Data ();
      if (constraint && constraint->Type == type)
	return constraint;
    }

  return NULL;		// Nope
}

/*
 * Form display items and helper functions
 *
 */

wxFormButton::wxFormButton (wxPanel * panel, wxFunction func, char *label,
int x, int y, int width, int height):
wxButton (panel, func, label, x, y, width, height)
{
}

static void 
wxFormOk (wxFormButton & button, wxEvent &)
{
  Bool success = TRUE;

  if (button.form->wx_editable)
    success = button.form->UpdateValues ();

  if (success)
    button.form->OnOk ();
}

static void 
wxFormUpdate (wxFormButton & button, wxEvent &)
{
  Bool success = button.form->UpdateValues ();
  if (success)
    button.form->OnUpdate ();
}

static void 
wxFormRevert (wxFormButton & button, wxEvent &)
{
  button.form->RevertValues ();
  button.form->OnRevert ();
}

static void 
wxFormCancel (wxFormButton & button, wxEvent &)
{
  button.form->OnCancel ();
}

static void 
wxFormHelp (wxFormButton & button, wxEvent &)
{
  button.form->OnHelp ();
}

#endif // USE_FORM
