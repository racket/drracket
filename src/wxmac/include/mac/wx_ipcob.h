/*
 * File:	wx_ipcob.h
 * Purpose:	Interprocess communication implementation, wxIPCObject (Mac)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Modified: Tomaso Paoletti
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_ipcobh
#define wx_ipcobh

#include "common.h"
#include "wx_setup.h"

#if USE_IPC

#include "wx_frame.h"
#include "wx_utils.h"
#include "wb_ipcob.h"

#ifdef IN_CPROTO
typedef       void    *wxIPCObject ;
#else

class wxIPCObject: public wxbIPCObject
{
 public:
  wxIPCObject();
  ~wxIPCObject();
  
  virtual void PeriodicTask(void) {};  // Mac specific
};

#endif // IN_CPROTO
#endif // USE_IPC
#endif // wx_ipcob.h
