///////////////////////////////////////////////////////////////////////////////
// File:	wx_utils.cc
// Purpose:	Various utilities (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <fstream.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>
#include "wx_dialg.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#include <Memory.h>
#include <QuickDraw.h>
#include <ToolUtils.h>
#include <Dialogs.h>

//-----------------------------------------------------------------------------
wxMacString::wxMacString(void)
{
	pString[0] = 0;
}

//-----------------------------------------------------------------------------
wxMacString::wxMacString(const char* cString)
{
	if (cString)
	{
		long itsLength = strlen(cString);
		if (itsLength > 255) itsLength = 255;
		BlockMove(cString, pString+1, itsLength); // BlockMove allows args to overlap
		pString[0] = itsLength;
	}
	else pString[0] = 0;
}

//-----------------------------------------------------------------------------
wxMacString::~wxMacString(void)	// destructor
{
}

//-----------------------------------------------------------------------------
wxMacString& wxMacString::operator=(char* cString)
{ // assignment of cString
	if (cString)
	{
		long itsLength = strlen(cString);
		if (itsLength > 255) itsLength = 255;
		BlockMove(cString, pString+1, itsLength); // BlockMove allows args to overlap
		pString[0] = itsLength;
	}
	else pString[0] = 0;

	return *this;
}

//-----------------------------------------------------------------------------
Str255& wxMacString::operator() (void)
{
	return pString;
}

//-----------------------------------------------------------------------------
wxMacString1::wxMacString1(void)
{
	pString[0] = 1;
	pString[1] = ' ';
}

//-----------------------------------------------------------------------------
wxMacString1::wxMacString1(char* cString): wxMacString(cString)
{
	if (pString[0] == 0)
	{
		pString[0] = 1;
		pString[1] = ' ';
	}
}

//-----------------------------------------------------------------------------
wxMacString1& wxMacString1::operator=(char* cString)
{ // assignment of cString
	if (cString)
	{
		long itsLength = strlen(cString);
		if (itsLength > 255) itsLength = 255;
		BlockMove(cString, pString+1, itsLength); // BlockMove allows args to overlap
		pString[0] = itsLength;
	}
	else pString[0] = 0;

	if (pString[0] == 0)
	{
		pString[0] = 1;
		pString[1] = ' ';
	}

	return *this;
}

//-----------------------------------------------------------------------------
void wxMacCtoPString(char* theCString, Str255 thePString)
{
	long itsLength = strlen(theCString);
	if (itsLength > 255) itsLength = 255;
	BlockMove(theCString, thePString+1, itsLength); // BlockMove allows args to overlap
	thePString[0] = itsLength;
}

//-----------------------------------------------------------------------------
void wxMacPtoCString(Str255 thePString, char* theCString)
{
	long itsLength = thePString[0];
	BlockMove(thePString+1, theCString, itsLength); // BlockMove allows args to overlap
	theCString[itsLength] = 0;
}

//-----------------------------------------------------------------------------
void wxError(const char *msg, const char *title)
{	wxMessageBox((char *)msg, (char *)title,wxOK);		
}

//-----------------------------------------------------------------------------
void wxFatalError(const char* msg, const char* title)
{
#if defined(PPCC)
	wxMacString macMsg(msg);
	wxMacString macTitle(title);
	ParamText((const unsigned char*) macTitle(),
		(const unsigned char*)macMsg(),
		(const unsigned char*)"\p",
		(const unsigned char*)"\p");	
#else
	wxMacString macMsg(msg);
	wxMacString macTitle(title);
	ParamText(macTitle(), macMsg(), "\p", "\p");	// WCH: must redo this
#endif
	StopAlert(100, NULL); 		// WCH: must redo this
	abort();
}

//-----------------------------------------------------------------------------
void wxFlushResources(void)
{
	// Defined in wx_utils.cc
}

//-----------------------------------------------------------------------------
void wxFlushEvents(void)
{
	// Defined in wx_utils.cc
}

//-----------------------------------------------------------------------------
char* macCopyString(char* s)
{ // return a copy of the string or NULL
	if (s) return copystring(s); else return NULL;
}

//-----------------------------------------------------------------------------
char* macCopyString0(char* s)
{ // return at least an empty string
	if (s) return copystring(s); else return copystring("");
}

//-----------------------------------------------------------------------------
char* macCopyString1(char* s)
{ // return at least a blank string
	if (s)
	{
		if (strlen(s) > 0)
			 return copystring(s);
		else return copystring(" ");
	}
	else return copystring(" ");
}

//-----------------------------------------------------------------------------
void macGetHatchPattern(int hatchStyle, Pattern pattern)
{
	int thePatListID = sysPatListID;
	int theIndex;
	switch(hatchStyle)
	{
		case wxBDIAGONAL_HATCH:
			theIndex = 34; // WCH: this is not good
			break;
		case wxFDIAGONAL_HATCH:
			theIndex = 26;
			break;
		case wxCROSS_HATCH:
			theIndex = 5;
			break;
		case wxHORIZONTAL_HATCH:
			theIndex = 25;
			break;
		case wxVERTICAL_HATCH:
			theIndex = 6;
			break;
		case wxCROSSDIAG_HATCH:
			theIndex = 4; // WCH: this is not good
			break;
		default:
			theIndex = 1; // solid pattern
			break;
	}
#if !defined(OLD__HEADERS)
	GetIndPattern(&pattern, thePatListID, theIndex);	
#else
	GetIndPattern(pattern, thePatListID, theIndex);	
#endif
}