/*
 * File:	wx_dcmem.h
 * Purpose:	Memory device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
 */

/* sccsid[] = "%W% %G%" */


#ifndef wx_dcmemh
#define wx_dcmemh

#include "wb_dcmem.h"
#include <QDOffscreen.h>

#ifdef IN_CPROTO
typedef       void    *wxMemoryDC ;
#else

class wxMemoryDC: public wxbMemoryDC
{
 public:
  GWorldPtr	gworldH;
  wxMemoryDC(void);
  wxMemoryDC(wxCanvasDC *old_dc); // Create compatible DC

  ~wxMemoryDC(void);
  virtual void SelectObject(wxBitmap *bitmap);

  GWorldPtr MacCreateGWorld(int width, int height);
};

#endif // IN_CPROTO
#endif // wx_dcmemh

