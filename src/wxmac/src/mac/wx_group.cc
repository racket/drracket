/*
 * File:        wx_group.cc
 * Purpose:     Group item implementation (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	April 1995
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation
#pragma implementation "wx_group.h"
#endif

#include <stdlib.h>
#include "common.h"
#include "wx_utils.h"
#include "wx_privt.h"
#include "wx_group.h"


/*
 * Group box
 */

wxGroupBox::wxGroupBox (void)
{
}

wxGroupBox::wxGroupBox (wxPanel * panel, char *label,
	  int x, int y, int width, int height, long style, char *name):
 wxbGroupBox(panel, label, x, y, width, height, style, name)
{
  Create (panel, label, x, y, width, height, style, name);
}

Bool wxGroupBox::Create (wxPanel * panel, char *label,
	int x, int y, int width, int height, long style, char *name)
{
  label = wxItemStripLabel(label);

  SetName(name);
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
  window_parent = panel;
  windowStyle = style;

  labelPosition = wxVERTICAL;

  return TRUE;
}


wxGroupBox::~wxGroupBox (void)
{
}

#if 0
void wxGroupBox::SetSize (int x, int y, int width, int height, int sizeFlags)
{
}
#endif

void wxGroupBox::SetLabel (char *label)
{
}

char *wxGroupBox::GetLabel (void)
{
  return NULL;	// dummy
}
