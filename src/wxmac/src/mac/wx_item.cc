///////////////////////////////////////////////////////////////////////////////
// File:	wx_item.cc
// Purpose:	Panel items implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

/**
Julian, I've (re)put here parts of my mods about wxFIXED_LENGHT.
I've done this because mod is only partially applied, which is not
a very good thing!! [you have applied the part after AttachWidget, and not
the part before creation]
Principle of wxFIXED_LENGTH: when creating a label for a wxItem, says
'this is a label' , it is created with  a string of same strlen containing
only '0' char. So:

	'this is a label     '  --> '0000000000000000000'
	'this is a long label'  --> '0000000000000000000'

Because strings are identical, they are aligned even if font is variable.
And after creation, changing attachement to the form has effect of not
resizing the label if it is changed back to its original value.
Heavy, but that's work!
**/

/* When implementing a new item, be sure to:
 *
 * - add the item to the parent panel
 * - set window_parent to the parent
 * - NULL any extra child window pointers not created for this item
 *   (e.g. label control that wasn't needed)
 * - delete any extra child windows in the destructor (e.g. label control)
 * - implement GetSize and SetSize
 * - to find panel position if coordinates are (-1, -1), use GetPosition
 * - call AdvanceCursor after creation, for panel layout mechanism.
 *
 */

/*
 Motif notes

 A panel is a form.
 Each item is created on a RowColumn or Form of its own, to allow a label to
 be positioned. wxListBox and wxMultiText have forms, all the others have RowColumns.
 This is to allow labels to be positioned to the top left (can't do it with a
 RowColumn as far as I know).
 AttachWidget positions widgets relative to one another (left->right, top->bottom)
 unless the x, y coordinates are given (more than -1).
 */

#include "wx_item.h"
#include "wx_gdi.h"


wxItem::wxItem(void)
	: wxbItem()
{
	SetEraser(wxCONTROL_BACKGROUND_BRUSH);
}

//-----------------------------------------------------------------------------
// Constructor (given parentArea)
wxItem::wxItem (wxArea* parentArea, int x, int y, int width, int height,
		long style, char* windowName)
	: wxbItem (windowName, parentArea, x, y, width, height, style)
{
	SetEraser(wxCONTROL_BACKGROUND_BRUSH);
}

//-----------------------------------------------------------------------------
// Constructor (given parentWindow)
wxItem::wxItem (wxWindow* parentWindow, int x, int y, int width, int height, 
		long style, char* windowName) 
	: wxbItem (windowName, parentWindow, x, y, width, height, style)
{
	SetEraser(wxCONTROL_BACKGROUND_BRUSH);
}

//-----------------------------------------------------------------------------
// Constructor (given objectType; i.e., menu or menuBar)
wxItem::wxItem (char* windowName) 
	: wxbItem (windowName)
{
	SetEraser(wxCONTROL_BACKGROUND_BRUSH);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxItem::~wxItem(void)
{
}

//-----------------------------------------------------------------------------
void wxItem::SetBackgroundColour(wxColour*col)
{
	backColour = col;
	ChangeColour();
}

//-----------------------------------------------------------------------------
void wxItem::SetLabelColour(wxColour*col)
{
	labelColour = col ;
	ChangeColour();
}

//-----------------------------------------------------------------------------
void wxItem::SetButtonColour(wxColour*col)
{
	buttonColour = col ;
	ChangeColour();
}

//-----------------------------------------------------------------------------
void wxItem::ChangeColour(void)
{
}

//-----------------------------------------------------------------------------
void wxItem::OnChar(wxKeyEvent& event)
{
   // Default is to pass chars up to our panel
   wxPanel *parent = (wxPanel *) GetParent();
   if (parent)
   {
     // parent is not always a wxPanel: can be a wxMenu...
     if (wxSubType(parent->__type,wxTYPE_PANEL))
     {
 		parent->OnChar(event);
     }
   }
}


char *wxItemStripLabel(char *label)
{
  if (!label)
    return NULL;
    
  int i;
  for (i = 0; label[i]; i++) {
    if (label[i] == '&') {
      /* Strip it: */
      char *naya = new char[strlen(label) + 1];
      int j = 0;
      for (i = 0; label[i]; i++) {
        if (label[i] == '&') {
          if (label[i + 1]) {
            naya[j++] = label[i + 1];
            i++;
          }
        } else
          naya[j++] = label[i];
      }
      
      return naya;
    }
  }
  
  return label;
}


