/*
 * File:	wb_timer.cc
 * Purpose:	wxTimer implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_timer.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "@(#)wb_timer.cc	1.2 5/9/94"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_list.h"
#include "wx_frame.h"
#include "wx_main.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"
#include "wx_timer.h"

#ifdef SVR4
#define SYSV
#endif

#include <time.h>
#ifndef wx_mac
#include <sys/types.h>
#endif

#if defined(SVR4) || defined(SYSV) || defined(__sgi) || defined(__alpha)
#include <sys/time.h>
#endif

#if defined(sun) || defined(__osf__)
// At least on Sun, ftime is undeclared.
// Need to be verified on other platforms.
extern "C" int ftime(struct timeb *tp);
// #include <sys/timeb.h>
#ifdef SVR4
// dito for gettimeofday on Solaris 2.x.
extern "C" int gettimeofday(struct timeval *tp, void *);
#endif
#endif

wxbTimer::wxbTimer(void)
{
 #if 0
  __type = wxTYPE_TIMER;
  milli = 0 ;
  lastMilli = -1 ;
 #endif
}

wxbTimer::~wxbTimer(void)
{
}

// Override me!
void wxbTimer::Notify(void)
{
}

int wxbTimer::Interval(void)
{
  return interval ;
}

/*
 * Timer functions
 *
 */

long wxStartTime = 0;
void wxStartTimer(void)
{
#if defined(__xlC__) || defined(_AIX) || defined(SVR4) || defined(SYSV) // || defined(AIXV3)
  struct timeval tp;
#ifdef SYSV
  gettimeofday(&tp, (struct timezone *)NULL);
#else
  gettimeofday(&tp);
#endif
  wxStartTime = 1000*tp.tv_sec + tp.tv_usec/1000;
#elif (defined(__SC__) || defined(__sgi) || defined(__bsdi__) || defined(__alpha)) || defined(wx_mac)
  time_t t0;
  struct tm *tp;
  time(&t0);
  tp = localtime(&t0);
  wxStartTime = 1000*(60*(60*tp->tm_hour+tp->tm_min)+tp->tm_sec);
#else
  struct timeb tp;
  ftime(&tp);
  wxStartTime = 1000*tp.time + tp.millitm;
#endif
}

// Returns elapsed time in milliseconds
long wxGetElapsedTime(Bool resetTimer)
{
#if defined(__xlC__) || defined(_AIX) || defined(SVR4) || defined(SYSV) // || defined(AIXV3)
  struct timeval tp;
#ifdef SYSV
  gettimeofday(&tp, (struct timezone *)NULL);
#else
  gettimeofday(&tp);
#endif
  long oldTime = wxStartTime;
  long newTime = 1000*tp.tv_sec + tp.tv_usec / 1000;
  if (resetTimer)
    wxStartTime = newTime;
#elif (defined(__SC__) || defined(__sgi) || defined(__bsdi__) || defined(__alpha)) || defined(wx_mac)
  time_t t0;
  struct tm *tp;
  time(&t0);
  tp = localtime(&t0);
  long oldTime = wxStartTime;
  long newTime = 1000*(60*(60*tp->tm_hour+tp->tm_min)+tp->tm_sec);
  if (resetTimer)
    wxStartTime = newTime;
#else
  struct timeb tp;
  ftime(&tp);
  long oldTime = wxStartTime;
  long newTime = 1000*tp.time + tp.millitm;
  if (resetTimer)
    wxStartTime = newTime;
#endif
  return newTime - oldTime;
}
