///////////////////////////////////////////////////////////////////////////////
// File:	wx_mtxt.cc
// Purpose:	Panel item multiText implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";
#include "wx_item.h"
#include "wx_mtxt.h"
#include "wx_utils.h"
#include <Memory.h>

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxMultiText::wxMultiText // Constructor (given parentPanel)
	(
		wxPanel*	parentPanel,
		wxFunction	function,
		char*		label,
		char*		value,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxbMultiText (parentPanel, function, label, value,
						x, y, width, (height != -1) ? height : 100,
						style|wxMULTIPLE, windowName)
{
  	// tom: set in multiple line mode
 	HLock((Handle)cMacTE);
 	(*cMacTE)->crOnly=1;
 	HUnlock((Handle)cMacTE);
}

//-----------------------------------------------------------------------------
wxMultiText::wxMultiText // Constructor (given parentTextWindow)
	(
		wxTextWindow*	parentTextWindow,
		wxFunction		function,
		char*			label,
		char*			value,
		int 			x,
		int				y,
		int				width,
		int				height,
		long			style,
		char*			windowName,
		WXTYPE			objectType
	) :
		wxbMultiText (parentTextWindow, function, label, value,
						x, y, width, (height != -1) ? height : 100,
						style|wxMULTIPLE, windowName)
{
 	// tom: set it in multiple line mode
 	HLock((Handle)cMacTE);
 	(*cMacTE)->crOnly=1;
 	HUnlock((Handle)cMacTE);
}

//-----------------------------------------------------------------------------
void wxMultiText::GetValue(char* buffer, int maxSize)
{
	if (maxSize <= 0) return; // WCH: maxSize should be tested (better: use unsigned int?)

	CharsHandle theMacText = ::TEGetText(cMacTE);
	int theMacTextLength = (**cMacTE).teLength;
	if (theMacTextLength > 0)
	{
		int moveSize = ( theMacTextLength > (maxSize - 1) ) ?
						maxSize - 1 : theMacTextLength;
		BlockMove(*theMacText, buffer, moveSize);
		buffer[moveSize] = 0; // terminate "C" string
	}
	else buffer[0] = 0;
}

char* wxMultiText::GetValue(void) 
{
	this->GetValue(wxBuffer, 1500);			// FIXME - HACK - should have constant for bufsize
	return wxBuffer;
}