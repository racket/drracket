///////////////////////////////////////////////////////////////////////////////
// File:	wxMacObj.cc
// Purpose:	wxObject base class implementation
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include "wxMacObj.h"

wxMacObject::wxMacObject(void)
{
  __type = wxTYPE_ANY;
}

wxMacObject::~wxMacObject(void)
{
}

