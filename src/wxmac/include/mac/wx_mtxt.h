///////////////////////////////////////////////////////////////////////////////
// File:	wx_mtxt.h
// Purpose:	Declares multi-line text panel item (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_mtxth
#define wx_mtxth

#include "wb_mtxt.h"

#ifdef IN_CPROTO
typedef       void* wxMultiText ;
#else

class wxMultiText: public wxbMultiText
{

//=============================================================================
// Public constructors
//=============================================================================
public:

	wxMultiText // Constructor (given parentPanel)
	(
		wxPanel*	parentPanel,
		wxFunction	function,
		char*		label,
		char*		value = "",
		int 		x = -1,
		int			y = -1,
		int			width = -1,
		int			height = -1,
		long		style = 0,
		char*		windowName = "multiText",
		WXTYPE		objectType = wxTYPE_MULTI_TEXT
	);

	wxMultiText // Constructor (given parentTextWindow)
	(
		wxTextWindow*	parentTextWindow,
		wxFunction		function,
		char*			label,
		char*			value = "",
		int 			x = -1,
		int				y = -1,
		int				width = -1,
		int				height = -1,
		long			style = 0,
		char*			windowName = "multiText",
		WXTYPE			objectType = wxTYPE_MULTI_TEXT
	);

//=============================================================================
// Public methods
//=============================================================================
public:
	char *GetValue(void);		//  16.x ?
	void GetValue(char* buffer, int maxLen);
};

#endif // IN_CPROTO
#endif // wx_mtxth
