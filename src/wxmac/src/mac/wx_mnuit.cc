///////////////////////////////////////////////////////////////////////////////
// File:	wx_mnuit.cc (split from wx_item.cc)
// Purpose:	Menu items implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wx_mnuit.h"
#include "wx_menu.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Constructors
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxMenuItem::wxMenuItem
(
	void
) :
	parentMenu (NULL),
	checkable (FALSE),
	cIsChecked (FALSE),
	cIsEnabled (TRUE)
{
	WXGC_IGNORE(parentMenu);
}

wxMenuItem::wxMenuItem
(
	wxMenu* theParentMenu,
	Bool	isCheckable
) :
	parentMenu (theParentMenu),
	checkable (isCheckable),
	cIsChecked (FALSE),
	cIsEnabled (TRUE)
{
	if (!theParentMenu) wxFatalError("No parent menu for constructing menu item.");
	WXGC_IGNORE(parentMenu);
}

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Destructor
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxMenuItem::~wxMenuItem(void)
{
	if (parentMenu)
	{
		// Must detach this from parent menu
	}

	if (subMenu)
	{
		// Must detach submenu from this
		// Must delete submenu
	}
}

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// tree methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxMenu* wxMenuItem::ParentMenu(void) { return parentMenu; }

	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// Other methods
	//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxMenuItem::Check(Bool flag)
{
	if (checkable)
	{
		if (cIsChecked != flag)
		{
			cIsChecked = flag;
			short macMenuItem = GetMacItemNumber();
			if (macMenuItem > 0)
			{
				CheckItem(parentMenu->MacMenu(), macMenuItem, cIsChecked);
			}
		}
	}
}

//-----------------------------------------------------------------------------
Bool wxMenuItem::IsChecked(void) { return cIsChecked; }

//-----------------------------------------------------------------------------
Bool wxMenuItem::IsCheckable(void) { return checkable; }

//-----------------------------------------------------------------------------
void wxMenuItem::Enable(Bool flag)
{
	if (cIsEnabled != flag)
	{
		cIsEnabled = flag;
		short macMenuItem = GetMacItemNumber();
		if (macMenuItem > 0)
		{
			if (cIsEnabled)
				 EnableItem(parentMenu->MacMenu(), macMenuItem);
			else DisableItem(parentMenu->MacMenu(), macMenuItem);
		}
	}
}

//-----------------------------------------------------------------------------
char* wxMenuItem::GetHelpString(void) { return helpString; }

//-----------------------------------------------------------------------------
void wxMenuItem::SetHelpString(char* theHelpString)
{
    if (helpString) delete[] helpString;
    helpString = macCopyString(theHelpString);
}

//-----------------------------------------------------------------------------
char* wxMenuItem::GetLabel(void) {  return itemName; }

//-----------------------------------------------------------------------------
void wxMenuItem::SetLabel(char* label)
{
    if (itemName) delete[] itemName;
    itemName = macCopyString(label);

	short macMenuItem = GetMacItemNumber();
	if (macMenuItem > 0)
	{
		wxMacString1 theMacString1 = label;
		SetItem(parentMenu->MacMenu(), macMenuItem, theMacString1());
	}
}

//-----------------------------------------------------------------------------
short wxMenuItem::GetMacItemNumber(void) // mac platform only
{
	short result = 0;
	if (parentMenu)
	{
		long memberIndex = parentMenu->menuItems.MemberIndex(this);
		if (memberIndex >= 0) result = memberIndex + 1; // mac counts from one
	}

	return result; // zero result means not found
}