/*
 * File:	wb_canvs.cc
 * Purpose:	wxbCanvas implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_canvs.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "@(#)wb_canvs.cc	1.2 5/9/94"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#ifdef __GNUG__
#pragma implementation
#endif

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_dc.h"
#include "wx_canvs.h"

#include "wx.h"

class wxFrame;

#ifndef wx_mac
wxbCanvas::wxbCanvas(void)
{
  __type = wxTYPE_CANVAS;
}
#endif // wx_mac

#ifndef wx_mac
wxbCanvas::wxbCanvas(wxWindow *window, int x, int y, int width, int height, long style,
                     char *name)
{
  __type = wxTYPE_CANVAS;
  windowStyle = style;
}
#endif // wx_mac
#ifdef wx_mac
// Constructor (given parentArea)
wxbCanvas::wxbCanvas (char* windowName, wxArea* parentArea, int x, int y,
		int width, int height, long style)
  : wxWindow ( windowName, parentArea, x, y, width, height, style),
		is_retained (FALSE), // Can only be retained after scrollbars have been set
		horiz_units (0),
		vert_units (0)
{
  __type = wxTYPE_CANVAS;
  windowStyle = style;
}

// Constructor (given parentWindow)
wxbCanvas::wxbCanvas(char* windowName, wxWindow* parentWindow, int x, int y,
		int width, int height, long style) 
  : wxWindow ( windowName, parentWindow, x, y, width, height, style),
		is_retained (FALSE), // Can only be retained after scrollbars have been set
		horiz_units (0),
		vert_units (0)
{
  __type = wxTYPE_CANVAS;
  windowStyle = style;
}
#endif // wx_mac

wxbCanvas::~wxbCanvas(void)
{
}

void wxbCanvas::AllowDoubleClick(int value)
{
  doubleClickAllowed = value ;
}

void wxbCanvas::SetClippingRegion(float cx, float cy, float cw, float ch)
{
  wx_dc->SetClippingRegion(cx, cy, cw, ch);
}

void wxbCanvas::GetClippingRegion(float *cx, float *cy, float *cw, float *ch)
{
  wx_dc->GetClippingRegion(cx, cy, cw, ch);
}

void wxbCanvas::DestroyClippingRegion(void)
{
  wx_dc->DestroyClippingRegion();
}

wxCanvasDC *wxbCanvas::GetDC(void)
{
  return wx_dc;
}

void wxbCanvas::Clear(void)
{
  wx_dc->Clear();
}

// Default input behaviour for a scrolling canvas should be to scroll
// according to the cursor keys pressed
void wxbCanvas::OnChar(wxKeyEvent& event)
{
  int x_page = 0;
  int y_page = 0;
  int start_x = 0;
  int start_y = 0;
  GetScrollUnitsPerPage(&x_page, &y_page);
  ViewStart(&start_x, &start_y);

  switch (event.keyCode)
  {
    case WXK_PRIOR:
    {
      if ((y_page > 0) && (start_y >= y_page))
        Scroll(start_x, start_y - y_page);
      break;
    }
    case WXK_NEXT:
    {
      if (y_page > 0)
        Scroll(start_x, start_y + y_page);
      break;
    }
    case WXK_UP:
    {
      if ((y_page > 0) && (start_y >= 1))
        Scroll(start_x, start_y - 1);
      break;
    }
    case WXK_DOWN:
    {
      if (y_page > 0)
        Scroll(start_x, start_y + 1);
      break;
    }
    case WXK_LEFT:
    {
      if ((x_page > 0) && (start_x >= 1))
        Scroll(start_x - 1, start_y);
      break;
    }
    case WXK_RIGHT:
    {
      if (x_page > 0)
        Scroll(start_x + 1, start_y);
      break;
    }
    case WXK_HOME:
    {
      Scroll(0, 0);
      break;
    }
  }
}

void wxbCanvas::FloodFill(float x, float y, wxColour *col, int style)
{
      wx_dc->FloodFill(x,y,col,style) ;
}

Bool wxbCanvas::GetPixel(float x, float y, wxColour *col)
{
  return wx_dc->GetPixel(x,y,col) ;
}

void wxbCanvas::DrawLine(float x1, float y1, float x2, float y2)
{
  wx_dc->DrawLine(x1, y1, x2, y2);
}

void wxbCanvas::IntDrawLine(int x1, int y1, int x2, int y2)
{
  wx_dc->IntDrawLine(x1, y1, x2, y2);
}

void wxbCanvas::CrossHair(float x, float y)
{
  wx_dc->CrossHair(x, y);
}

void wxbCanvas::DrawArc(float x1,float y1,float x2,float y2,float xc,float yc)
{
  wx_dc->DrawArc(x1, y1, x2, y2,xc,yc);
}

void wxbCanvas::DrawPoint(float x, float y)
{
  wx_dc->DrawPoint(x, y);
}

void wxbCanvas::DrawPolygon(int n, wxPoint points[], float xoffset, float yoffset,int fillStyle)
{
  wx_dc->DrawPolygon(n, points, xoffset, yoffset,fillStyle);
}

void wxbCanvas::DrawPolygon(wxList *list, float xoffset, float yoffset,int fillStyle)
{
  ((wxDC *)wx_dc)->DrawPolygon(list, xoffset, yoffset,fillStyle);
}

void wxbCanvas::DrawLines(int n, wxPoint points[], float xoffset, float yoffset)
{
  wx_dc->DrawLines(n, points, xoffset, yoffset);
}

void wxbCanvas::DrawLines(int n, wxIntPoint points[], int xoffset, int yoffset)
{
  wx_dc->DrawLines(n, points, xoffset, yoffset);
}

void wxbCanvas::DrawLines(wxList *list, float xoffset, float yoffset)
{
  ((wxDC *)wx_dc)->DrawLines(list, xoffset, yoffset);
}

void wxbCanvas::DrawRectangle(float x, float y, float width, float height)
{
  wx_dc->DrawRectangle(x, y, width, height);
}

void wxbCanvas::DrawRoundedRectangle(float x, float y, float width, float height, float radius)
{
  wx_dc->DrawRoundedRectangle(x, y, width, height, radius);
}

void wxbCanvas::DrawEllipse(float x, float y, float width, float height)
{
  wx_dc->DrawEllipse(x, y, width, height);
}


void wxbCanvas::SetFont(wxFont *the_font)
{
  wx_dc->SetFont(the_font);
}

void wxbCanvas::SetPen(wxPen *pen)
{
  wx_dc->SetPen(pen);
}

void wxbCanvas::SetTextForeground(wxColour *colour)
{
  wx_dc->SetTextForeground(colour);
}

void wxbCanvas::SetTextBackground(wxColour *colour)
{
  wx_dc->SetTextBackground(colour);
}

void wxbCanvas::SetBrush(wxBrush *brush)
{
  wx_dc->SetBrush(brush);
}

void wxbCanvas::DrawText(const char *text, float x, float y, Bool use16)
{
  wx_dc->DrawText(text, x, y, use16);
}

void wxbCanvas::SetBackground(wxBrush *brush)
{
  wx_dc->SetBackground(brush);
}

void wxbCanvas::SetLogicalFunction(int function)
{
  wx_dc->SetLogicalFunction(function);
}

#if USE_SPLINES
// Make a 3-point spline
void wxbCanvas::DrawSpline(float x1, float y1, float x2, float y2, float x3, float y3)
{
  wx_dc->DrawSpline(x1, y1, x2, y2, x3, y3);
}

void wxbCanvas::DrawSpline(wxList *list)
{
  wx_dc->DrawSpline(list);
}
#endif

float wxbCanvas::GetCharHeight(void)
{
  return wx_dc->GetCharHeight();
}

float wxbCanvas::GetCharWidth(void)
{
  return wx_dc->GetCharWidth();
}

#ifdef wx_mac
void wxbCanvas::GetTextExtent(const char* string, float* x, float* y, float* descent,
  						float* externalLeading, wxFont* the_font, Bool use16)
{
  wx_dc->GetTextExtent(string, x, y, descent, externalLeading, the_font, use16);
}
#else // wx_mac
void wxbCanvas::GetTextExtent(const char *string, float *x, float *y,
                              float *descent, float *externalLeading)
{
  wx_dc->GetTextExtent(string, x, y, descent, externalLeading);
}
#endif // wx_mac

