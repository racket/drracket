/*
 * File:	wb_dcmem.h
 * Purpose:	Base memory device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_dcmem.h	1.2 5/9/94" */


#ifndef wxb_dcmemh
#define wxb_dcmemh

#ifdef __GNUG__
#pragma interface
#endif

#ifndef IN_CPROTO
#include <fstream.h>
#endif
#include "common.h"
#include "wx_dc.h"
#include "wx_dccan.h"

#ifdef IN_CPROTO
typedef       void    *wxbMemoryDC ;
#else

class wxbMemoryDC: public wxCanvasDC
{
 public:
  inline wxbMemoryDC(void) { }
  inline wxbMemoryDC(wxCanvasDC *old_dc) {} // Create compatible DC

  inline ~wxbMemoryDC(void) { }
  virtual void SelectObject(wxBitmap *bitmap) = 0;
};

#endif // IN_CPROTO
#endif // wx_dcmemh

