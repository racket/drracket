/*
 * File:	wb_dccan.h
 * Purpose:	Base canvas device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_dccan.h	1.2 5/9/94" */


#ifndef wxb_dccanh
#define wxb_dccanh

#ifdef __GNUG__
#pragma interface
#endif

#ifndef IN_CPROTO
#include <fstream.h>
#endif
#include "common.h"
#include "wx_dc.h"

#ifdef IN_CPROTO
typedef       void    *wxbCanvasDC ;
#else

class wxbCanvasDC: public wxDC
{
 public:
  inline wxbCanvasDC(void) { }
  inline wxbCanvasDC(wxCanvas *the_canvas) { }

  inline ~wxbCanvasDC(void) { }
};

#endif // IN_CPROTO
#endif // wxb_dccanh

