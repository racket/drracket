/*
 * File:	wb_ipcob.h
 * Purpose:	IPC object definition
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_ipcob.h	1.2 5/9/94" */

#ifndef wxb_ipcobh
#define wxb_ipcobh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_setup.h"

#if USE_IPC

#include "wx_list.h"

#ifdef IN_CPROTO
typedef       void    *wxbIPCObject ;
#else

class wxbIPCObject: public wxObject
{
 public:
  int lastError;
  char *service_name; // Server only
  wxList connections;
  wxbIPCObject(void);
  ~wxbIPCObject(void);
};

#endif // IN_CPROTO
#endif // USE_IPC
#endif // wxb_ipcob.h
