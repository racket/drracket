/*
 * File:      wx_timer.cc
 * Purpose:     wxTimer implementation (MacIntosh Version)
 * Author:      Julian Smart/Cecil Coupe
 * Created:     1993
 * Updated:	August 1995
 * RCS_ID:      $Id: wx_timer.cc,v 1.3 1994/08/14 21:28:43 edz Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

static const char sccsid[] = "@(#)wx_timer.cc	1.2 5/9/94";

/* MrEd re-implements */
#if 0

#ifdef __GNUG__
#pragma implementation
#endif

#include <time.h>
#include "wx_timer.h"
#include "wx_list.h"
#include "wx_main.h"
#include "wx_frame.h"
#include "wx_utils.h"

/*
   There are several ways to handle async timer events on the Mac. The trick is to
   remove them when an app quits, crashes, or when debugging. Also we have to make sure
   that the system is in a state that we can actually make other ToolBox calls. 
   
   As it stands today (8/25/95), we will only process timer events when the "system"
   can handle it, i.e., when taking events with WaitNextEvent. This implies that whatever
   mechanism we use for the async notification, it should build a MacOS event.
   
   Now, if accuracy and latency isn't too important, we could kludge this by having the wxMac 
   event loop processor, whenever it awakes from the wait, get the ansi time and search a list
   (ordered queue?) of wxTimers which have the a 'future fire' time data member. If the
   node(s) time is <= current time, call the callback, and then remove the node from the
   list. If its not a one shot timer node, calculate a new future time (now + timer increment)
   and (re)queue a (new)node. This can be done by modifying wx_app.cc [to possibly lower the 
   sleep time] and to call a function doMacTimerCheck(). Also we need a global or app scope
   variable for the queue head. Of course we'll have to create a class for these timer nodes
   and modify the wx_xxx.h and wb_xxx.h files accordingly.
   
*/

#ifdef wx_motif
void 
wxTimerCallback (wxTimer * timer)
{
  if (timer->timerId == 0)
    return;			// Avoid to process spurious timer events

  if (!timer->oneShot)
    timer->timerId = XtAppAddTimeOut (wxTheApp->appContext, timer->milli,
		  (XtTimerCallbackProc) wxTimerCallback, (XtPointer) timer);
  else
    timer->timerId = 0;
  timer->Notify ();
}
#endif

wxList gTimerList;

wxTimer::wxTimer (void)
{
}

wxTimer::~wxTimer (void)
{
  Stop ();
}

Bool wxTimer::Start (int milliseconds, Bool mode)
{
  oneShot = mode;
  if (milliseconds < 0)
    milliseconds = lastMilli;

  if (milliseconds <= 0)
    return FALSE;

  lastMilli = milli = milliseconds;
  fireTime = ::TickCount() + (milliseconds / (1000 / CLOCKS_PER_SEC));
  gTimerList.Append(this);
  return TRUE;
}

void wxTimer::Stop (void)
{
  milli = 0;
  gTimerList.DeleteObject(this);	// removes timer from list. Object still exists
}

#endif

