///////////////////////////////////////////////////////////////////////////////
// File:	wxMacDC.h
// Purpose:	MacDC (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxMacDCh
#define wxMacDCh

#include "wxMacObj.h"

class wxMacDC: public wxMacObject
{
  protected:
	CGrafPtr	cMacGrafPort;
	wxObject*	cCurrentUser;

  public:
	wxMacDC(CGrafPtr port);		// constructor
	~wxMacDC(void);				// destructor

	CGrafPtr macGrafPort(void);
	wxObject* currentUser(void);
	void setCurrentUser(wxObject* user);

	Bool isCurrentPort(void);
};

#endif // wxMacDCh