///////////////////////////////////////////////////////////////////////////////
// File:	wxMacObj.h
// Purpose:	Top level object for Macintosh platform
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxMacObjh
#define wxMacObjh

#include "wx_types.h"

#ifdef IN_CPROTO
typedef       void* wxMacObject ;
#else

class wxMacObject
{
  public:
  WXTYPE __type;
  wxMacObject(void);
  virtual ~wxMacObject(void);
};

#endif // IN_CPROTO
#endif // wxMacObjh
