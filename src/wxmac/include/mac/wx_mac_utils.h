///////////////////////////////////////////////////////////////////////////////
// File:	wx_mac_utils.h
// Purpose:	Various utilities (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_mac_utilsh
#define wx_mac_utilsh

#include "wxMacObj.h"
#include <Strings.h>
#include <QuickDraw.h>

class wxMacString: public wxMacObject
{
  protected:
	Str255		pString;

  public:
	wxMacString(void);						// constructor
	wxMacString(const char* cString);				// constructor from cString
	~wxMacString(void);						// destructor

	wxMacString& operator=(char* cString);	// assignment of cString
	Str255& operator() (void);				// get reference to pascal string
};

class wxMacString1: public wxMacString
{
  public:
	wxMacString1(void);						// constructor
	wxMacString1(char* cString);				// constructor from cString
	wxMacString1& operator=(char* cString);	// assignment of cString
};

void wxMacCtoPString(char* theCString, Str255 thePString);
void wxMacPtoCString(Str255 thePString, char* theCString);
char* macCopyString(char* s);
char* macCopyString0(char* s);
char* macCopyString1(char* s);

void macGetHatchPattern(int hatchStyle, Pattern pattern);

#endif // wx_mac_utils