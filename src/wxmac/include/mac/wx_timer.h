/*
 * File:	wx_timer.h
 * Purpose:	wxTimer - provides simple timer functionality (dummy version)
 * Author:	Julian Smart/Cecil Coupe
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_timer.h	1.2 5/9/94" */

#ifndef wx_timerh
#define wx_timerh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_timer.h"
#include "time.h"
#include <types.h>
#ifdef IN_CPROTO
typedef       void    *wxTimer ;
#else

class wxTimer: public wxbTimer
{
 public:
#if 0
 UInt32		fireTime;		// the future Tick when the timer fires
#else
 void *context;
 unsigned long expiration;
 int one_shot;
 int interval;
 wxTimer *prev, *next;
#endif
  wxTimer(void);
  ~wxTimer(void);
  Bool Start(int milliseconds = -1,Bool one_shot = FALSE ); // Start timer
  void Stop(void);                   // Stop timer
  void Dequeue();
};

#endif // IN_CPROTO
#endif // wx_timerh
