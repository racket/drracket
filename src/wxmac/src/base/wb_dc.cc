/*
 * File:        wb_dc.cc
 * Purpose:     Device context implementation
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_dc.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "@(#)wb_dc.cc      1.2 5/9/94"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#ifdef __GNUG__
#pragma implementation "wb_dc.h"
#pragma implementation "wb_dccan.h"
#pragma implementation "wb_dcmem.h"
#endif

#include "common.h"
#include "wx_frame.h"
#include "wx_dc.h"
#include "wx_dcps.h"
// wx_dcmem.h not strictly necessary but required
// for GNU GCC when using pragmas.
#include "wx_dcmem.h"
#include "wx_stdev.h"
#include "wx_utils.h"
#include "wx_canvs.h"
#include "wx_dialg.h"
#include "wx_main.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#include "math.h"

#ifndef wx_mac
#if USE_SPLINES
# if USE_XFIG_SPLINE_CODE
#  include "../base/xfspline.cc"
# else
#  include "../base/wxspline.cc"
# endif
#endif // USE_SPLINES
#endif

// Default constructor
wxbDC::wxbDC(void)
{
  __type = wxTYPE_DC;
  min_x = 0; min_y = 0; max_x = 0; max_y = 0;
  title = NULL;
  clipping = FALSE;
  autoSetting = TRUE ;
}

wxbDC::~wxbDC(void)
{
  if (title)
    delete[] title;
}

void wxbDC::DrawPolygon(wxList *list, float xoffset, float yoffset,int fillStyle)
{
  int n = list->Number();
  wxPoint *points = new wxPoint[n];

  int i = 0;
  for(wxNode *node = list->First(); node; node = node->Next()) {
    wxPoint *point = (wxPoint *)node->Data();
    points[i].x = point->x;
    points[i++].y = point->y;
  }
  DrawPolygon(n, points, xoffset, yoffset,fillStyle);
  delete[] points;
}

void wxbDC::DrawLines(wxList *list, float xoffset, float yoffset)
{
  int n = list->Number();
  wxPoint *points = new wxPoint[n];

  int i = 0;
  for(wxNode *node = list->First(); node; node = node->Next()) {
    wxPoint *point = (wxPoint *)node->Data();
    points[i].x = point->x;
    points[i++].y = point->y;
  }
  DrawLines(n, points, xoffset, yoffset);
  delete []points;
}

void wxbDC::SetTextForeground(wxColour *colour)
{
  if (colour)
    current_text_foreground = *colour;
}

void wxbDC::SetTextBackground(wxColour *colour)
{
  if (colour)
    current_text_background = *colour;
}

void wxbDC::SetBackgroundMode(int mode)
{
  current_bk_mode = mode;
}

void wxbDC::GetSize(float *width, float *height)
{
  if (!(min_x == 1000.0 && min_y == 1000.0 && max_x == -1000.0 && max_y == -1000.0))
  {
    *width = (float)(max_x - min_x);
    *height = (float)(max_y - min_y);
  } else {
    *width = 0.0;
    *height = 0.0;
  }
}

#if USE_SPLINES
// Make a 3-point spline
void wxbDC::DrawSpline(float x1, float y1, float x2, float y2, float x3, float y3)
{
  wxList *point_list = new wxList;

  wxPoint *point1 = new wxPoint;
  point1->x = x1; point1->y = y1;
  point_list->Append((wxObject*)point1);

  wxPoint *point2 = new wxPoint;
  point2->x = x2; point2->y = y2;
  point_list->Append((wxObject*)point2);

  wxPoint *point3 = new wxPoint;
  point3->x = x3; point3->y = y3;
  point_list->Append((wxObject*)point3);

  DrawSpline(point_list);

  for(wxNode *node = point_list->First(); node; node = node->Next()) {
    wxPoint *p = (wxPoint *)node->Data();
    delete p;
  }
  delete point_list;
/*
  wxSpline spline(point_list);

  wx_draw_open_spline(this, &spline);
  spline.DeletePoints();
 */
}
#endif

void wxbDC::SetLogicalOrigin(float x, float y)
{
  logical_origin_x = x;
  logical_origin_y = y;
}

void wxbDC::SetDeviceOrigin(float x, float y)
{
  device_origin_x = x;
  device_origin_y = y;
}

// For use by wxWindows only, unless custom units are required.
void wxbDC::SetLogicalScale(float x, float y)
{
  logical_scale_x = x;
  logical_scale_y = y;
}

void wxbDC::CalcBoundingBox(float x, float y)
{
  if (x < min_x) min_x = x;
  if (y < min_y) min_y = y;
  if (x > max_x) max_x = x;
  if (y > max_y) max_y = y;
}

void wxbDC::TryColour(wxColour *src, wxColour *dest)
{
  *dest = *src;
}

