/*
 * File:	wb_obj.cc
 * Purpose:	wxObject base class implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_obj.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "@(#)wb_obj.cc	1.2 5/9/94"; */

#ifdef __GNUG__
#pragma implementation "wx_obj.h"
#endif

#include "wx_obj.h"
#include "wx_types.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

int wx_object_count;

wxObject::wxObject(void)
{
  __type = wxTYPE_ANY;
  wx_object_count++;
}

wxObject::wxObject(int cleanup)
: gc_cleanup(cleanup)
{
  __type = wxTYPE_ANY;
  wx_object_count++;
}

wxObject::wxObject(int cleanup, WXTYPE t)
: gc_cleanup(cleanup)
{
  __type = t;
  wx_object_count++;
}


wxObject::~wxObject(void)
{
  __type = -1;
  --wx_object_count;
}

