///////////////////////////////////////////////////////////////////////////////
// File:	wxMargin.cc
// Purpose:	wxMargin (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wxMargin.h"
#include "wxDirection.h"

//=============================================================================
// Construction methods
//=============================================================================

//-----------------------------------------------------------------------------
wxMargin::wxMargin(int margin)
{
	left = margin;
	top = margin;
	right = margin;
	bottom = margin;
}

//-----------------------------------------------------------------------------
wxMargin::wxMargin(int margin, Direction direction)
{
	left = (direction & Direction::wxLeft ? margin : 0);
	top = (direction & Direction::wxTop ? margin : 0);
	right = (direction & Direction::wxRight ? margin : 0);
	bottom = (direction & Direction::wxBottom ? margin : 0);
}

//-----------------------------------------------------------------------------
wxMargin::wxMargin(const wxMargin& margin)
{
	left = margin.left;
	top = margin.top;
	right = margin.right;
	bottom = margin.bottom;
}

//-----------------------------------------------------------------------------
wxMargin::~wxMargin(void)	// destructor
{
}

//=============================================================================
// Overloaded operator methods
//=============================================================================

wxMargin& wxMargin::operator +=(wxMargin margin)
{
	left += margin.left;
	top += margin.top;
	right += margin.right;
	bottom += margin.bottom;
	return *this;
}

//=============================================================================
// Getter and setter methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxMargin::SetMargin(wxMargin margin, Direction direction)
{
	if (direction & Direction::wxLeft) left = margin.left;
	if (direction & Direction::wxTop) top = margin.top;
	if (direction & Direction::wxRight) right = margin.right;
	if (direction & Direction::wxBottom) bottom = margin.bottom;
}

//-----------------------------------------------------------------------------
void wxMargin::SetMargin(int margin, Direction direction)
{
	if (direction & Direction::wxLeft) left = margin;
	if (direction & Direction::wxTop) top = margin;
	if (direction & Direction::wxRight) right = margin;
	if (direction & Direction::wxBottom) bottom = margin;
}

//-----------------------------------------------------------------------------
int wxMargin::Offset(Direction direction)
{
	int result = 0;
	if (direction & Direction::wxLeft) result += left;
	if (direction & Direction::wxTop) result += top;
	if (direction & Direction::wxRight) result += right;
	if (direction & Direction::wxBottom) result += bottom;

	return result;
}