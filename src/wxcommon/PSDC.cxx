/*
 * File:      wb_ps.cc
 * Purpose:     Device context implementation (PostScript)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

/* This file is the same for all three version of wxWindows from
   PLT. */

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef wx_xt
# define  Uses_XLib
# define  Uses_wxList
# define  Uses_wxWindowDC
# define  Uses_wxMemoryDC
# define  Uses_wxPostScriptDC
# define  Uses_wxPrintSetup
# define  Uses_wxFontNameDirectory
# define  Uses_wxDialogBox
# define  Uses_wxButton
# define  Uses_wxRadioBox
# define  Uses_wxText
# define  Uses_wxChoice
# define  Uses_wxCheckBox
# include "wx.h"

# include <math.h>
# include <string.h>

#else

# ifdef __GNUG__
# pragma implementation "wx_dcps.h"
# pragma implementation
# pragma interface
#endif

# include "common.h"
# include "wx_frame.h"
# include "wx_dcps.h"
# include "wx_dcmem.h"
# include "wx_utils.h"
# include "wx_dialg.h"
# include "wx_cmdlg.h"
# include "wx_main.h"
# include "wx_lbox.h"
# include "wx_rbox.h"
# include "wx_buttn.h"
# include "wx_choic.h"
# include "wx_check.h"
# include "wx_messg.h"
# include "wx_txt.h"
# include "wx_mtxt.h"

#endif
#endif

#include "wx_rgn.h"

# define YSCALE(y) ((paper_h) - ((y) * user_scale_y + device_origin_y))
# define XSCALE(x) ((x) * user_scale_x + device_origin_x)
# define YOFFSET(y) ((paper_h) - ((y) + device_origin_y))
# define XOFFSET(x) ((x) + device_origin_x)
# define YSCALEREL(dy) ((dy) * user_scale_y)
# define XSCALEREL(dx) ((dx) * user_scale_x)
# define XSCALEBND(dx) (XSCALEREL(dx) + device_origin_x)
# define YSCALEBND(dy) (YSCALEREL(dy) + device_origin_y)

# define ASCALEREL(a) ((a) * ascale)

# define PIE pie

#define RESET_FONT 0x1
#define RESET_COLOR 0x2

static double pie = 0.0;
static int complained_afm = 0;

#ifndef WXUNUSED
# define WXUNUSED(x) x
#endif

#ifdef wx_xt
# define WXXTUNUSED(c) /* empty */
#else
# define WXXTUNUSED(x) x
#endif

#define DEFAULT_PAPER "Letter 8 1/2 x 11 in"

class wxCanvas;

#ifndef wx_xt
#include "wx_privt.h"
#endif

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>

static char *default_afm_path = NULL;

Bool XPrinterDialog(wxWindow *parent);

#ifdef wx_mac
wxPrintPaperDatabase *wxThePrintPaperDatabase;
#endif

#ifndef wx_xt
# define current_font font
#else
# define current_bk_mode current_text_bgmode
# define current_text_foreground current_text_fg
# define current_text_background current_text_bg
#endif

class PSStream : public wxObject {
  FILE *f;
  int int_width;

 public:

  PSStream(char *file) {
    f = fopen(file, "w");
    int_width = 0;
  }
  ~PSStream(void) {
    if (f) fclose(f);
  }

  int good(void) {
    return !!f;
  }

  void Out(char s) {
    fprintf(f, "%c", s);
  }
  void Out(const char *s) {
    fwrite(s, strlen(s), 1, f);
  }
  void Out(float n);
  void Out(double d) {
    Out((float)d);
  }
  void Out(long l);
  void Out(int i) {
    Out((long)i);
  }

  long tellp(void) {
    return ftell(f);
  }
  void seekp(long pos) {
    fseek(f, pos, 0);
  }

  void width(int w) {
    int_width = w;
  }
};

void PSStream::Out(float n)
{
  if (int_width > 0) {
    if ((float)(long)n == n) {
      Out((long)n);
      return;
    }
  }
  fprintf(f, "%f", n);
}

void PSStream::Out(long l)
{
  if (int_width > 0) {
    char buffer[50];
    sprintf(buffer, "%%+%d.%dld", int_width, int_width);
    fprintf(f, buffer, l);
    int_width = 0;
  } else
    fprintf(f, "%ld", l);
}

wxPostScriptDC::wxPostScriptDC (void)
{
  Create(TRUE);
}

wxPostScriptDC::wxPostScriptDC (Bool interactive)
{
  Create(interactive);
}

Bool wxPostScriptDC::Create(Bool interactive)
{
  wxPrintSetupData *wxThePrintSetupData;
  char *paperType;
  wxPrintPaperType *paper;

  if (!pie)
    pie = 2 * asin((double)1.0);

  __type = wxTYPE_DC_POSTSCRIPT;
#ifndef wx_xt
  wx_interactive = interactive;
#endif
  current_font = wxNORMAL_FONT;
  device = wxDEVICE_EPS;
  clipping = NULL;

#ifndef wx_xt
  logical_origin_x = 0;
  logical_origin_y = 0;

  device_origin_x = 0;
  device_origin_y = 0;

  logical_scale_x = 1.0;
  logical_scale_y = 1.0;

  user_scale_x = 1.0;
  user_scale_y = 1.0;

  min_x = 10000.0;
  min_y = 10000.0;
  max_x = -10000.0;
  max_y = -10000.0;

  current_pen = NULL;
  current_brush = NULL;
  current_background_color = new wxColour(wxWHITE);

  current_text_foreground = new wxColour(wxBLACK);

  mapping_mode = MM_TEXT;
#else
  current_pen = wxBLACK_PEN;
  current_pen->Lock(1);
  current_brush = wxWHITE_BRUSH;
  current_brush->Lock(1);
  current_background_color->CopyFrom(wxWHITE);
#endif

  title = NULL;

  filename = NULL;

  pstream = NULL;

  clipx = 0;
  clipy = 0;
  clipw = -1;
  cliph = -1;

  ok = PrinterDialog(interactive);
  if (!ok)
    return FALSE;

  currentRed = 0;
  currentGreen = 0;
  currentBlue = 0;

  Colour = TRUE;
  
  wxThePrintSetupData = wxGetThePrintSetupData();

  level2ok = wxThePrintSetupData->GetLevel2();
  afm_path = wxThePrintSetupData->GetAFMPath();

  paperType = wxThePrintSetupData->GetPaperName();
  if (!paperType)
    paperType = DEFAULT_PAPER;

  paper = wxThePrintPaperDatabase->FindPaperType(paperType);
  if (!paper)
    paper = wxThePrintPaperDatabase->FindPaperType(DEFAULT_PAPER);
  if (paper) {
    paper_w = (float)paper->widthPixels;
    paper_h = (float)paper->heightPixels;
  } else {
    paper_w = 1000;
    paper_h = 1000;
  }

  if (wxThePrintSetupData) {
    wxThePrintSetupData->GetPrinterTranslation(&paper_x, &paper_y);
    wxThePrintSetupData->GetPrinterScaling(&paper_x_scale, &paper_y_scale);
    if (wxThePrintSetupData->GetPrinterOrientation() == PS_LANDSCAPE)
      landscape = 1;
    else
      landscape = 0;
  } else {
    paper_x = paper_y = 0;
    paper_x_scale = paper_y_scale = 1;
    landscape = 0;
  }

  if (landscape) {
    float tmp;

    tmp = paper_w;
    paper_w = paper_h;
    paper_h = tmp;
  }

  paper_w /= paper_x_scale;
  if (!paper_w)
    paper_w = 1;
  paper_h /= paper_y_scale;
  if (!paper_h)
    paper_h = 1;

  return ok;
}

wxPostScriptDC::~wxPostScriptDC (void)
{
  if (current_brush) current_brush->Lock(-1);
  if (current_pen) current_pen->Lock(-1);

  if (pstream)
    DELETE_OBJ pstream;
}

Bool wxPostScriptDC::PrinterDialog(Bool interactive)
{
  wxPrintSetupData *wxThePrintSetupData;
  char *s;

  if (interactive) {
    ok = XPrinterDialog(NULL);
    if (!ok)
      return FALSE;
  } else
    ok = TRUE;
  
  wxThePrintSetupData = wxGetThePrintSetupData();

  mode = wxThePrintSetupData->GetPrinterMode();
  s = wxThePrintSetupData->GetPrintPreviewCommand();
  preview_cmd = copystring(s);
  s = wxThePrintSetupData->GetPrinterCommand();
  print_cmd = copystring(s);
  s = wxThePrintSetupData->GetPrinterOptions();
  print_opts = copystring(s);

  if ((mode == PS_PREVIEW) || (mode == PS_PRINTER)) {
    // For PS_PRINTER action this depends on a Unix-style print spooler
    // since the wx_printer_file can be destroyed during a session
    // @@@ TODO: a Windows-style answer for non-Unix
    char userId[256];
    char tmp[256];
    wxGetUserId (userId, sizeof (userId) / sizeof (char));
    strcpy (tmp, "/tmp/preview_");
    strcat (tmp, userId);
    strcat (tmp, ".ps");
    filename = copystring(tmp);
  } else if (mode == PS_FILE) {
    char *file;
    file = interactive ? (char *)NULL : wxThePrintSetupData->GetPrinterFile();
    if (!file)
      file = wxSaveFileSelector("PostScript", "ps");
    if (!file) {
      ok = FALSE;
      return FALSE;
    }
    filename = copystring(file);
    ok = TRUE;
  }

  return ok;
}

void wxPostScriptDC::SetClippingRect(float cx, float cy, float cw, float ch)
{
  wxRegion *r;

  if (!pstream)
    return;

  r = new wxRegion(this);
  r->SetRectangle(cx, cy, cw, ch);

  SetClippingRegion(r);
}

wxRegion *wxPostScriptDC::GetClippingRegion()
{
  if (clipping)
    return new wxRegion(this, clipping);
  else
    return NULL;
}

void wxPostScriptDC::SetClippingRegion(wxRegion *r)
{
  if (!pstream)
    return;
  if (r && (r->GetDC() != this))
    return;

  if (clipping) {
    clipping = NULL;
    pstream->Out("initclip\n");
  }

  if (r) {
    pstream->Out("newpath\n");
    if (r->ps) { /* => non-empty region */
      wxPSRgn *rl;
      char *s;
      rl = r->ps->Lift();
      s = rl->GetString();
      pstream->Out(s);
    }
    pstream->Out("clip\n");

    clipping = r;
  }
}

void wxPostScriptDC::Clear(void)
{
  unsigned char red, blue, green;

  if (!pstream)
    return;

  red = current_background_color->Red();
  blue = current_background_color->Blue();
  green = current_background_color->Green();

  {
    float redPS = (float) (((int) red) / 255.0);
    float bluePS = (float) (((int) blue) / 255.0);
    float greenPS = (float) (((int) green) / 255.0);
    
    /* Fill with current background */
    pstream->Out("gsave newpath\n");
    pstream->Out(redPS); pstream->Out(" "); pstream->Out(greenPS); pstream->Out(" "); pstream->Out(bluePS); pstream->Out(" setrgbcolor\n");
    pstream->Out(0); pstream->Out(" "); pstream->Out(0); pstream->Out(" moveto\n");
    pstream->Out(0); pstream->Out(" "); pstream->Out(paper_h); pstream->Out(" lineto\n");
    pstream->Out(paper_w); pstream->Out(" "); pstream->Out(paper_h); pstream->Out(" lineto\n");
    pstream->Out(paper_w); pstream->Out(" "); pstream->Out(0); pstream->Out(" lineto\n");
    pstream->Out("closepath\n");
    pstream->Out("fill grestore\n");
  }
}

void wxPostScriptDC::FloodFill(float WXUNUSED(x), float WXUNUSED(y), wxColour * WXUNUSED(col), int WXUNUSED(style))
{
}

Bool wxPostScriptDC::GetPixel(float WXUNUSED(x), float WXUNUSED(y), wxColour * WXUNUSED(col))
{
  return FALSE;
}

void wxPostScriptDC::IntDrawLine (int x1, int y1, int x2, int y2)
{
  DrawLine ((float) x1, (float) y1, (float) x2, (float) y2);
}

void wxPostScriptDC::CrossHair (float x, float y)
{
  DrawLine(0, y, paper_w, y);
  DrawLine(x, 0, x, paper_h);
}

void wxPostScriptDC::DrawLine (float x1, float y1, float x2, float y2)
{
  if (!pstream)
    return;
  if (current_pen)
    SetPen (current_pen);
  pstream->Out("newpath\n");
  pstream->Out(XSCALE(x1)); pstream->Out(" "); pstream->Out(YSCALE (y1)); pstream->Out(" moveto\n");
  pstream->Out(XSCALE(x2)); pstream->Out(" "); pstream->Out(YSCALE (y2)); pstream->Out(" lineto\n");
  pstream->Out("stroke\n");
  CalcBoundingBox(XSCALEBND(x1), YSCALEBND(y1));
  CalcBoundingBox(XSCALEBND(x2), YSCALEBND(y2));
}

void wxPostScriptDC::DrawArc (float x, float y, float w, float h, float start, float end)
{
  if (!pstream)
    return;

  if (start != end) {
    float a1, a2, radius, xscale;

    /* Before we scale: */
    CalcBoundingBox(XSCALEBND(x), YSCALEBND(y));
    CalcBoundingBox(XSCALEBND(x + w), YSCALEBND(y + h));

    x = XSCALE(x);
    y = XSCALE(y);
    w = XSCALEREL(w);
    h = YSCALEREL(h);

    radius = (h / 2);
    xscale = (w / h);

    a1 = start * (180 / pie);
    a2 = end * (180 / pie);

    pstream->Out("gsave\n");
    pstream->Out((x + w/2)); pstream->Out(" "); pstream->Out((paper_h - (y + h/2))); pstream->Out(" translate\n");
    pstream->Out(xscale); pstream->Out(" "); pstream->Out(1); pstream->Out(" scale\n");

    if (current_brush && current_brush->GetStyle () != wxTRANSPARENT) {
      SetBrush(current_brush);
      
      pstream->Out("newpath\n");
      pstream->Out(cos(start)*radius); pstream->Out(" "); 
      pstream->Out(sin(start)*radius); pstream->Out(" moveto\n");
      pstream->Out("0 0 "); pstream->Out(radius); pstream->Out(" "); pstream->Out(a1); 
      pstream->Out(" "); pstream->Out(a2); pstream->Out(" arc\n");

      pstream->Out("0 0 lineto\n");

      pstream->Out("closepath\n");

      pstream->Out("fill\n");
    }
    if (current_pen && current_pen->GetStyle () != wxTRANSPARENT) {
      SetPen(current_pen);

      pstream->Out("newpath\n");
      pstream->Out(cos(start)*radius); pstream->Out(" "); pstream->Out(sin(start)*radius); pstream->Out(" moveto\n");
      pstream->Out("0 0 "); pstream->Out(radius); pstream->Out(" ");
      pstream->Out(a1); pstream->Out(" "); pstream->Out(a2); pstream->Out(" arc\n");
      pstream->Out("stroke\n");
    }

    pstream->Out("grestore\n");
  }
}

void wxPostScriptDC::DrawPoint (float x, float y)
{
  if (!pstream)
    return;
  if (current_pen)
    SetPen (current_pen);
  pstream->Out("newpath\n");
  pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" moveto\n");
  pstream->Out(XSCALE(x+1)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" lineto\n");
  pstream->Out("stroke\n");
  CalcBoundingBox(XSCALEBND(x), YSCALEBND(y));
}

void wxPostScriptDC::DrawSpline(float x1, float y1, float x2, float y2, float x3, float y3)
{
  float x21, y21, x22, y22;
  float xm1, ym1, xm2, ym2;

  if (!pstream)
    return;

  pstream->Out("newpath\n");

  pstream->Out(XSCALE(x1)); pstream->Out(" "); pstream->Out(YSCALE(y1)); pstream->Out(" moveto ");

  x21 = (x1 + x2) / 2;
  y21 = (y1 + y2) / 2;

  pstream->Out(XSCALE(x21)); pstream->Out(" "); pstream->Out(YSCALE(y21)); pstream->Out(" lineto\n");

  x22 = (x2 + x3) / 2;
  y22 = (y2 + y3) / 2;
  
  xm1 = (x21 + x2) / 2;
  ym1 = (y21 + y2) / 2;

  xm2 = (x2 + x22) / 2;
  ym2 = (y2 + y22) / 2;

  pstream->Out(XSCALE(xm1)); pstream->Out(" "); pstream->Out(YSCALE(ym1)); pstream->Out(" "); 

  pstream->Out(XSCALE(xm2)); pstream->Out(" "); pstream->Out(YSCALE(ym2)); pstream->Out(" "); 

  pstream->Out(XSCALE(x22)); pstream->Out(" "); pstream->Out(YSCALE(y22)); pstream->Out(" curveto\n");

  pstream->Out(XSCALE(x3)); pstream->Out(" "); pstream->Out(YSCALE(y3)); pstream->Out(" lineto\n");

  pstream->Out("stroke\n");

  CalcBoundingBox(XSCALEBND(x1), YSCALEBND(y1));
  CalcBoundingBox(XSCALEBND(x2), YSCALEBND(y2));
  CalcBoundingBox(XSCALEBND(x3), YSCALEBND(y3));
}

void wxPostScriptDC::DrawPolygon (int n, wxPoint points[], float xoffset, float yoffset, int fillStyle)
{
  if (!pstream)
    return;
  if (n > 0)
    {
      if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
	{
	  int i;
	  float xx, yy;

	  SetBrush (current_brush);
	  pstream->Out("newpath\n");

	  xx = points[0].x + xoffset;
	  yy = (points[0].y + yoffset);
	  pstream->Out(XSCALE(xx)); pstream->Out(" "); pstream->Out(YSCALE(yy)); pstream->Out(" moveto\n");
	  CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));

	  for (i = 1; i < n; i++)
	    {
	      xx = points[i].x + xoffset;
	      yy = (points[i].y + yoffset);
	      pstream->Out(XSCALE(xx)); pstream->Out(" "); pstream->Out(YSCALE(yy)); pstream->Out(" lineto\n");
	      CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));
	    }
	  pstream->Out(((fillStyle == wxODDEVEN_RULE) ? "eofill\n" : "fill\n"));
	}

      if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
	{
	  int i;
	  float xx, yy;

	  SetPen (current_pen);
	  pstream->Out("newpath\n");

	  xx = points[0].x + xoffset;
	  yy = (points[0].y + yoffset);
	  pstream->Out(XSCALE(xx)); pstream->Out(" "); pstream->Out(YSCALE(yy)); pstream->Out(" moveto\n");
	  CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));

	  for (i = 1; i < n; i++)
	    {
	      xx = points[i].x + xoffset;
	      yy = (points[i].y + yoffset);
	      pstream->Out(XSCALE(xx)); pstream->Out(" "); pstream->Out(YSCALE(yy)); pstream->Out(" lineto\n");
	      CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));
	    }

	  // Close the polygon
	  xx = points[0].x + xoffset;
	  yy = (points[0].y + yoffset);
	  pstream->Out(XSCALE(xx)); pstream->Out(" "); pstream->Out(YSCALE(yy)); pstream->Out(" lineto\n");

	  // Output the line
	  pstream->Out("stroke\n");
	}
    }
}

void wxPostScriptDC::DrawLines (int n, wxIntPoint points[], int xoffset, int yoffset)
{
  if (!pstream)
    return;
  if (n > 0) {
    int i;
    float xx, yy;

      if (current_pen)
	SetPen (current_pen);

      pstream->Out("newpath\n");

      xx = (float) (points[0].x + xoffset);
      yy = (float) (points[0].y + yoffset);
      pstream->Out(XSCALE(xx)); pstream->Out(" "); pstream->Out(YSCALE(yy)); pstream->Out(" moveto\n");
      CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));

      for (i = 1; i < n; i++)
	{
	  xx = (float) (points[i].x + xoffset);
	  yy = (float) (points[i].y + yoffset);
	  pstream->Out(XSCALE(xx)); pstream->Out(" "); pstream->Out(YSCALE(yy)); pstream->Out(" lineto\n");
	  CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));
	}
      pstream->Out("stroke\n");
    }
}

void wxPostScriptDC::DrawLines (int n, wxPoint points[], float xoffset, float yoffset)
{
  if (!pstream)
    return;
  if (n > 0) {
    int i;
    float xx, yy;

      if (current_pen)
	SetPen (current_pen);

      pstream->Out("newpath\n");

      xx = points[0].x + xoffset;
      yy = (points[0].y + yoffset);
      pstream->Out(XSCALE(xx)); pstream->Out(" "); pstream->Out(YSCALE(yy)); pstream->Out(" moveto\n");
      CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));

      for (i = 1; i < n; i++)
	{
	  xx = points[i].x + xoffset;
	  yy = (points[i].y + yoffset);
	  pstream->Out(XSCALE(xx)); pstream->Out(" "); pstream->Out(YSCALE(yy)); pstream->Out(" lineto\n");
	  CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));
	}
      pstream->Out("stroke\n");
    }
}

#ifdef wx_xt

void wxPostScriptDC::DrawLines(wxList *list, float xoffset, float yoffset)
{
  int n, i;
  wxPoint *points;
  wxNode *node;

  n = list->Number();
#ifdef MZ_PRECISE_GC
  points = (wxPoint *)GC_malloc_atomic(sizeof(wxPoint) * n);
#else
  points = new wxPoint[n];
#endif

  i = 0;
  for (node = list->First(); node; node = node->Next()) {
    wxPoint *point;
    point = (wxPoint *)node->Data();
    points[i].x = point->x;
    points[i++].y = point->y;
  }
  DrawLines(n, points, xoffset, yoffset);
}

void wxPostScriptDC::DrawPolygon(wxList *list, float xoffset, float yoffset,int fillStyle)
{
  int n, i;
  wxPoint *points;
  wxNode *node;

  n = list->Number();
#ifdef MZ_PRECISE_GC
  points = (wxPoint *)GC_malloc_atomic(sizeof(wxPoint) * n);
#else
  points = new wxPoint[n];
#endif

  i = 0;
  for(node = list->First(); node; node = node->Next()) {
    wxPoint *point;
    point = (wxPoint *)node->Data();
    points[i].x = point->x;
    points[i++].y = point->y;
  }
  DrawPolygon(n, points, xoffset, yoffset, fillStyle);
}

#endif

void wxPostScriptDC::DrawRectangle (float x, float y, float width, float height)
{
  if (!pstream)
    return;
  if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
    {
      SetBrush (current_brush);

      pstream->Out("newpath\n");
      pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" moveto\n");
      pstream->Out(XSCALE(x + width)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" lineto\n");
      pstream->Out(XSCALE(x + width)); pstream->Out(" "); pstream->Out(YSCALE (y + height)); pstream->Out(" lineto\n");
      pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE (y + height)); pstream->Out(" lineto\n");
      pstream->Out("closepath\n");
      pstream->Out("fill\n");

      CalcBoundingBox(XSCALEBND(x), YSCALEBND(y));
      CalcBoundingBox(XSCALEBND(x + width), YSCALEBND(y + height));
    }
  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      SetPen (current_pen);

      pstream->Out("newpath\n");
      pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" moveto\n");
      pstream->Out(XSCALE(x + width)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" lineto\n");
      pstream->Out(XSCALE(x + width)); pstream->Out(" "); pstream->Out(YSCALE (y + height)); pstream->Out(" lineto\n");
      pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE (y + height)); pstream->Out(" lineto\n");
      pstream->Out("closepath\n");
      pstream->Out("stroke\n");

      CalcBoundingBox(XSCALEBND(x), YSCALEBND(y));
      CalcBoundingBox(XSCALEBND(x + width),  YSCALEBND(y + height));
    }
}

void wxPostScriptDC::DrawRoundedRectangle (float x, float y, float width, float height, float radius)
{
  float ascale;

  if (!pstream)
    return;

  if (radius < 0.0)
    {
      // Now, a negative radius is interpreted to mean
      // 'the proportion of the smallest X or Y dimension'
      float smallest = 0.0;
      if (width < height)
	smallest = width;
      else
	smallest = height;
      radius = (float) (-radius * smallest);
    }

  ascale = (user_scale_x < user_scale_y) ? user_scale_x : user_scale_y;

  if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
    {
      SetBrush (current_brush);
      // Draw rectangle anticlockwise
      pstream->Out("newpath\n");

      pstream->Out(XSCALE(x) + ASCALEREL(radius)); pstream->Out(" "); 
      pstream->Out(YSCALE(y)); pstream->Out(" moveto\n");

      pstream->Out(XSCALE(x) + ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y) - ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 90 180 arc\n");

      pstream->Out(XSCALE(x) + ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y + height) + ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 180 270 arc\n");

      pstream->Out(XSCALE(x + width) - ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y + height) + ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 270 0 arc\n");

      pstream->Out(XSCALE(x + width) - ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y) - ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 0 90 arc\n");

      pstream->Out("closepath\n");

      pstream->Out("fill\n");

      CalcBoundingBox(XSCALEBND(x), YSCALEBND(y));
      CalcBoundingBox(XSCALEBND(x + width), YSCALEBND(y + height));
    }
  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      SetPen (current_pen);
      // Draw rectangle anticlockwise
      pstream->Out("newpath\n");

      pstream->Out(XSCALE(x) + ASCALEREL(radius)); pstream->Out(" "); 
      pstream->Out(YSCALE(y)); pstream->Out(" moveto\n");

      pstream->Out(XSCALE(x) + ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y) - ASCALEREL(radius)); pstream->Out(" "); 
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 90 180 arc\n");

      pstream->Out(XSCALE(x) + ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y + height) + ASCALEREL(radius)); pstream->Out(" "); 
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 180 270 arc\n");

      pstream->Out(XSCALE(x + width) - ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y + height) + ASCALEREL(radius)); pstream->Out(" "); 
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 270 0 arc\n");

      pstream->Out(XSCALE(x + width) - ASCALEREL(radius)); pstream->Out(" ");
      pstream->Out(YSCALE(y) - ASCALEREL(radius)); pstream->Out(" "); 
      pstream->Out(ASCALEREL(radius)); pstream->Out(" 0 90 arc\n");

      pstream->Out("closepath\n");

      pstream->Out("stroke\n");

      CalcBoundingBox(XSCALEBND(x), YSCALEBND(y));
      CalcBoundingBox(XSCALEBND(x + width), YSCALEBND(y + height));
    }
}

void wxPostScriptDC::DrawEllipse (float x, float y, float width, float height)
{
  if (!pstream)
    return;
  if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
    {
      SetBrush (current_brush);

      pstream->Out("newpath\n");
      pstream->Out(XSCALE(x + width / 2)); pstream->Out(" "); pstream->Out(YSCALE(y + height / 2)); pstream->Out(" ");
      pstream->Out(XSCALEREL(width / 2)); pstream->Out(" "); pstream->Out(YSCALEREL(height / 2)); pstream->Out(" 0 360 ellipse\n");
      pstream->Out("fill\n");

      CalcBoundingBox(XSCALEBND(x - width), YSCALEBND(y - height));
      CalcBoundingBox(XSCALEBND(x + width), YSCALEBND(y + height));
    }
  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      SetPen (current_pen);

      pstream->Out("newpath\n");
      pstream->Out(XSCALE(x + width / 2)); pstream->Out(" "); pstream->Out(YSCALE(y + height / 2)); pstream->Out(" ");
      pstream->Out(XSCALEREL(width / 2)); pstream->Out(" "); pstream->Out(YSCALEREL(height / 2)); pstream->Out(" 0 360 ellipse\n");
      pstream->Out("stroke\n");

      CalcBoundingBox (XSCALEBND(x - width), YSCALEBND(y - height));
      CalcBoundingBox (XSCALEBND(x + width), YSCALEBND(y + height));
    }
}

void wxPostScriptDC::SetFont (wxFont * the_font)
{
  char *name;
  int Family, Style, Weight, size;

  if (!pstream)
    return;
  if ((current_font == the_font) && !(resetFont & RESET_FONT))
    return;

  resetFont -= (resetFont & RESET_FONT);

  current_font = the_font;
  Family = current_font->GetFontId();
  Style = current_font->GetStyle();
  Weight = current_font->GetWeight();

  name = wxTheFontNameDirectory->GetPostScriptName(Family, Weight, Style);
  if (!name)
    name = "Times-Roman";

  pstream->Out("/"); pstream->Out(name); pstream->Out(" findfont\n");
  size = current_font->GetPointSize();
  pstream->Out(YSCALEREL(size)); pstream->Out(" scalefont setfont\n");
}

static void set_pattern(wxPostScriptDC *dc, PSStream *pstream, wxBitmap *bm, int rop, wxColour *col)
{
  int width, height;

  width = bm->GetWidth();
  height = bm->GetHeight();

  pstream->Out("8 dict\n");
  pstream->Out("dup\n");
  pstream->Out("begin\n");
  pstream->Out(" /PatternType 1 def\n");
  pstream->Out(" /PaintType 1 def\n");
  pstream->Out(" /TilingType 1 def\n");
  pstream->Out(" /BBox [ 0 0 "); pstream->Out(width); pstream->Out(" "); pstream->Out(height); pstream->Out(" ] def\n");
  pstream->Out(" /XStep "); pstream->Out(width); pstream->Out(" def\n");
  pstream->Out(" /YStep "); pstream->Out(height); pstream->Out(" def\n");

  dc->Blit(0, 0, width, height, bm, 0, 0, -rop - 1, col);

  pstream->Out("end\n");
  pstream->Out(" matrix makepattern setpattern\n");
}

static char *dotted = "[2 5] 2";
static char *short_dashed = "[4 4] 2";
static char *long_dashed = "[4 8] 2";
static char *dotted_dashed = "[6 6 2 6] 4";

void wxPostScriptDC::SetPen (wxPen * pen)
{
  wxPen *oldPen = current_pen;
  char *psdash = NULL;
  unsigned char red, blue, green;
  float width;

  if (!pstream)
    return;

  if (current_pen) current_pen->Lock(-1);
  if (pen) pen->Lock(1);

  if ((current_pen = pen) == NULL)
    return;			/* NIL */

  // Line width
  width = pen->GetWidthF();
  pstream->Out(width);
  pstream->Out(" setlinewidth\n");

  if (level2ok) {
    wxBitmap *stipple;
    stipple = pen->GetStipple();
    if (stipple && stipple->Ok()) {
      int ps;
      wxColour *pc;
      ps = pen->GetStyle();
      pc = pen->GetColour();
      set_pattern(this, pstream, stipple, ps, pc);
      resetFont |= RESET_COLOR;
      return;
    }
  }

  switch (pen->GetStyle ())
    {
    case wxDOT:
      psdash = dotted;
      break;
    case wxSHORT_DASH:
      psdash = short_dashed;
      break;
    case wxLONG_DASH:
      psdash = long_dashed;
      break;
    case wxDOT_DASH:
      psdash = dotted_dashed;
      break;
    case wxSOLID:
    case wxTRANSPARENT:
    default:
      psdash = "[] 0";
      break;
    }
  if (oldPen != pen) {
    pstream->Out(psdash); pstream->Out(" setdash\n");
  }

  // Line colour
  {
    wxColour *pc;
    pc = pen->GetColour();
    red = pc->Red();
    blue = pc->Blue();
    green = pc->Green();
  }

  if (!Colour)
    {
      // Anything not white is black
      if (!(red == (unsigned char) 255 && blue == (unsigned char) 255
	    && green == (unsigned char) 255))
	{
	  red = (unsigned char) 0;
	  green = (unsigned char) 0;
	  blue = (unsigned char) 0;
	}
    }

  if (!(red == currentRed && green == currentGreen && blue == currentBlue)
      || (resetFont & RESET_COLOR)) {
    float redPS = (float) (((int) red) / 255.0);
    float bluePS = (float) (((int) blue) / 255.0);
    float greenPS = (float) (((int) green) / 255.0);

    pstream->Out(redPS); pstream->Out(" "); pstream->Out(greenPS); 
    pstream->Out(" "); pstream->Out(bluePS); pstream->Out(" setrgbcolor\n");
    
    currentRed = red;
    currentBlue = blue;
    currentGreen = green;
    resetFont -= (resetFont & RESET_COLOR);
  }
}

static char *ps_brush_hatch[] = { " 0 0 moveto 8 8",
				  " 0 0 moveto 8 8 lineto closepath stroke 8 0 moveto 0 8",
				  " 8 0 moveto 0 8",
				  " 0 4 moveto 8 4 lineto closepath stroke 4 0 moveto 4 8",
				  " 0 4 moveto 8 4",
				  " 4 0 moveto 4 8" };

void wxPostScriptDC::SetBrush(wxBrush * brush)
{
  unsigned char red, blue, green;
  int hatch_id;
  float redPS, bluePS, greenPS;

  if (!pstream)
    return;

  if (current_brush) current_brush->Lock(-1);
  if (brush) brush->Lock(1);

  if ((current_brush = brush) == NULL)
    return; 

  if (level2ok) {
    wxBitmap *stipple;
    stipple = brush->GetStipple();
    if (stipple && stipple->Ok()) {
      int bs;
      wxColour *bc;
      bs = brush->GetStyle();
      bc = brush->GetColour();
      set_pattern(this, pstream, stipple, bs, bc);
      resetFont |= RESET_COLOR;
      return;
    }
  }

  // Brush colour
  {
    wxColour *bc;
    bc = brush->GetColour(); 
    red = bc->Red();
    blue = bc->Blue();
    green = bc->Green();
  }

  if (!Colour) {
    // Anything not black is white
    if (!(red == (unsigned char) 0 && blue == (unsigned char) 0
	  && green == (unsigned char) 0)) {
      red = (unsigned char) 255;
      green = (unsigned char) 255;
      blue = (unsigned char) 255;
    }
  }

  hatch_id = -1;
  switch (brush->GetStyle()) {
  case wxBDIAGONAL_HATCH:
    hatch_id = 0;
    break;
  case wxCROSSDIAG_HATCH:
    hatch_id = 1;
    break;
  case wxFDIAGONAL_HATCH:
    hatch_id = 2;
    break;
  case wxCROSS_HATCH:
    hatch_id = 3;
    break;
  case wxHORIZONTAL_HATCH:
    hatch_id = 4;
    break;
  case wxVERTICAL_HATCH:
    hatch_id = 5;
    break;
  }

  redPS = (float) (((int) red) / 255.0);
  bluePS = (float) (((int) blue) / 255.0);
  greenPS = (float) (((int) green) / 255.0);

  if (hatch_id > -1) {
    pstream->Out("7 dict\n");
    pstream->Out("dup\n");
    pstream->Out("begin\n");
    pstream->Out(" /PatternType 1 def\n");
    pstream->Out(" /PaintType 1 def\n");
    pstream->Out(" /TilingType 1 def\n");
    pstream->Out(" /BBox [ 0 0 8 8 ] def\n");
    pstream->Out(" /XStep 8 def\n");
    pstream->Out(" /YStep 8 def\n");
    pstream->Out(" /PaintProc { begin gsave \n");

    pstream->Out(" 0 setlinewidth\n");
    pstream->Out(" [] 0 setdash\n");
    pstream->Out(" "); pstream->Out(redPS); pstream->Out(" "); pstream->Out(greenPS); 
    pstream->Out(" "); pstream->Out(bluePS); pstream->Out(" setrgbcolor\n");

    pstream->Out(" "); pstream->Out(ps_brush_hatch[hatch_id]); pstream->Out(" lineto closepath stroke \n");
    
    pstream->Out("grestore\n } def \n");
    
    pstream->Out("end\n"); pstream->Out(" matrix makepattern setpattern\n");

    resetFont |= RESET_COLOR;

    return;
  }

  if (!(red == currentRed && green == currentGreen && blue == currentBlue)
      || (resetFont & RESET_COLOR)) {
    pstream->Out(redPS); pstream->Out(" "); pstream->Out(greenPS); pstream->Out(" "); 
    pstream->Out(bluePS); pstream->Out(" setrgbcolor\n");
    currentRed = red;
    currentBlue = blue;
    currentGreen = green;
    resetFont -= (resetFont & RESET_COLOR);
  }
}

void wxPostScriptDC::DrawText(DRAW_TEXT_CONST char *text, float x, float y,
			      Bool use16, int dt)
{
  float tw, th;
  int i, len, size;

  if (!pstream)
    return;
  if (current_font)
    SetFont (current_font);

  GetTextExtent(text, &tw, &th, NULL, NULL, NULL, use16, dt);

  if (current_bk_mode == wxSOLID) {
    unsigned char red, blue, green;
    
    red = current_text_background->Red();
    blue = current_text_background->Blue();
    green = current_text_background->Green();
    
    {
      float redPS = (float) (((int) red) / 255.0);
      float bluePS = (float) (((int) blue) / 255.0);
      float greenPS = (float) (((int) green) / 255.0);
      
      pstream->Out("gsave newpath\n");
      pstream->Out(redPS); pstream->Out(" "); pstream->Out(greenPS); pstream->Out(" "); 
      pstream->Out(bluePS); pstream->Out(" setrgbcolor\n");
      pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" moveto\n");
      pstream->Out(XSCALE(x + tw)); pstream->Out(" "); pstream->Out(YSCALE (y)); pstream->Out(" lineto\n");
      pstream->Out(XSCALE(x + tw)); pstream->Out(" "); pstream->Out(YSCALE (y + th)); pstream->Out(" lineto\n");
      pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE (y + th)); pstream->Out(" lineto\n");
      pstream->Out("closepath\n");
      pstream->Out("fill grestore\n");
    }
  }

  if (current_text_foreground->Ok()) {
    unsigned char red, blue, green;

    red = current_text_foreground->Red();
    blue = current_text_foreground->Blue();
    green = current_text_foreground->Green();
    
    if (!Colour) {
      // Anything not white is black
      if (!(red == (unsigned char) 255 && blue == (unsigned char) 255
	    && green == (unsigned char) 255))
	{
	  red = (unsigned char) 0;
	  green = (unsigned char) 0;
	  blue = (unsigned char) 0;
	}
    }
    if (!(red == currentRed && green == currentGreen && blue == currentBlue)
	|| (resetFont & RESET_COLOR)) {
      float redPS = (float) (((int) red) / 255.0);
      float bluePS = (float) (((int) blue) / 255.0);
      float greenPS = (float) (((int) green) / 255.0);
      pstream->Out(redPS); pstream->Out(" "); pstream->Out(greenPS); pstream->Out(" "); 
      pstream->Out(bluePS); pstream->Out(" setrgbcolor\n");
      
      currentRed = red;
      currentBlue = blue;
      currentGreen = green;
      resetFont -= (resetFont & RESET_COLOR);
    }
  }
  
  size = 10;
  if (current_font)
    size = current_font->GetPointSize();

  pstream->Out(XSCALE(x)); pstream->Out(" "); pstream->Out(YSCALE (y + size)); pstream->Out(" moveto\n");

  pstream->Out("(");
  len = strlen(text + dt);
  for (i = 0; i < len; i++)
    {
      char ch = text[i + dt];
      if (ch == ')' || ch == '(' || ch == '\\')
	pstream->Out("\\");
      pstream->Out(ch);
    }

  pstream->Out(")"); pstream->Out(" show\n");

  CalcBoundingBox(XSCALEBND(x), YSCALEBND(y));
  CalcBoundingBox(XSCALEBND(x + tw), YSCALEBND(y + th));
}


void wxPostScriptDC::SetBackground (wxColour * c)
{
  current_background_color->CopyFrom(c);
}

void wxPostScriptDC::SetBackgroundMode(int mode)
{
  current_bk_mode = mode;
}

void wxPostScriptDC::SetTextBackground(wxColour *col)
{
  current_text_background->CopyFrom(col);  
}

void wxPostScriptDC::SetTextForeground(wxColour *col)
{
  current_text_foreground->CopyFrom(col);
}

void wxPostScriptDC::TryColour(wxColour *src, wxColour *dest)
{
  if (!Colour) {
    if ((src->Red() == 255)
	&& (src->Green() == 255)
	&& (src->Blue() == 255))
      dest->Set(255, 255, 255);
    else
      dest->Set(0, 0, 0);
  } else
    *dest = *src;
}

static const char *wxPostScriptHeaderEllipse = "\
/ellipsedict 8 dict def\n\
ellipsedict /mtrx matrix put\n\
/ellipse\n\
{ ellipsedict begin\n\
  /endangle exch def\n\
  /startangle exch def\n\
  /yrad exch def\n\
  /xrad exch def\n\
  /y exch def\n\
  /x exch def\n\
  /savematrix mtrx currentmatrix def\n\
  x y translate\n\
  xrad yrad scale\n\
  0 0 1 endangle startangle arcn\n\
  savematrix setmatrix\n\
  end\n\
  } def\n\
";

Bool wxPostScriptDC::StartDoc (char *message)
{
  char userID[256];

  if (device == wxDEVICE_EPS) {
    PSStream *pss;
    pss = new PSStream(filename);
    pstream = pss;

    if (!pstream || !pstream->good()) {
      ok = FALSE;
      pstream = NULL;
      return FALSE;
    }
    ok = TRUE;
  }

  pstream->Out("%!PS-Adobe-2.0 EPSF-2.0\n");	/* PostScript magic strings */
  if (title) {
    pstream->Out("%%Title: "); pstream->Out(title); pstream->Out("\n");
  }
  pstream->Out("%%Creator: "); pstream->Out("MrEd"); pstream->Out("\n");
  pstream->Out("%%CreationDate: "); pstream->Out(wxNow()); pstream->Out("\n");

  // User Id information
  if (wxGetEmailAddress(userID, sizeof(userID))) {
    char userName[245];
    pstream->Out("%%For: "); pstream->Out((char *)userID);
    if (wxGetUserName(userName, sizeof(userName))) {
      pstream->Out(" ("); pstream->Out((char *)userName); pstream->Out(")");
    }
    pstream->Out("\n");
  } else if ( wxGetUserName(userID, sizeof(userID))) {
    pstream->Out("%%For: "); pstream->Out((char *)userID); pstream->Out("\n");
  }

  boundingboxpos = pstream->tellp();

  pstream->Out("%%BoundingBox: -00000 -00000 -00000 -00000\n");
  pstream->Out("%%Pages: -00000\n");
  pstream->Out("%%EndComments\n\n");

  pstream->Out(wxPostScriptHeaderEllipse);

  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);

  page_number = 1;
  if (message) {
    title = copystring (message);
  }

  return TRUE;
}

#ifdef wx_x
extern void wxsExecute(char **);
#endif

void wxPostScriptDC::EndDoc (void)
{
  float llx;
  float lly;
  float urx;
  float ury;

  if (!pstream)
    return;
  if (clipping) {
    clipping = FALSE;
    pstream->Out("grestore\n");
  }

  // Compute the bounding box.  Note that it is in the default user
  // coordinate system, thus we have to convert the values.
  // If we're landscape, our sense of "x" and "y" is reversed.
  if (landscape) {
    llx = min_y * paper_y_scale + paper_y;
    lly = min_x * paper_x_scale + paper_x;
    urx = max_y * paper_y_scale + paper_y;
    ury = max_x * paper_x_scale + paper_x;
  } else {
    llx = min_x * paper_x_scale + paper_x;
    lly = paper_h * paper_y_scale - (max_y * paper_y_scale) + paper_y;
    urx = max_x * paper_x_scale + paper_x;
    ury = paper_h * paper_y_scale - (min_y * paper_y_scale) + paper_y;
  }

  // The Adobe specifications call for integers; we round as to make
  // the bounding larger.
  pstream->seekp(boundingboxpos);
  pstream->Out("%%BoundingBox: ");
  pstream->width(5);
  pstream->Out(floor(llx)); pstream->Out(" ");
  pstream->width(5);
  pstream->Out(floor(lly)); pstream->Out(" ");
  pstream->width(5);
  pstream->Out(ceil(urx) ); pstream->Out(" ");
  pstream->width(5);
  pstream->Out(ceil(ury)); pstream->Out("\n");
  pstream->Out("%%Pages: ");
  pstream->width(5);
  pstream->Out((page_number - 1)); pstream->Out("\n");

  DELETE_OBJ pstream;
  pstream = NULL;

#ifdef wx_x
  if (ok /* && wx_interactive */)
    {
      switch (mode) {
	case PS_PREVIEW:
	{
          char *argv[3];
	  argv[0] = preview_cmd;
          argv[1] = filename;
          argv[2] = NULL;
	  wxsExecute (argv);
	}
	break;

	case PS_PRINTER:
	{
          char *argv[4];
	  char *opts;
	  int i;
          argv[0] = print_cmd;
	  i = 1;
	  opts = print_opts;
	  if (opts && *opts)
	    argv[i++] = opts;
	  argv[i++] = filename;
	  argv[i] = NULL;
	  wxsExecute(argv);
	}
	break;

	case PS_FILE:
	  break;
	}
    }
#endif
}

void wxPostScriptDC::StartPage (void)
{
  if (!pstream)
    return;
  pstream->Out("%%Page: "); pstream->Out(page_number++); pstream->Out("\n");

  pstream->Out((paper_x + (landscape ? (paper_h * paper_y_scale) : 0)));
  pstream->Out(" "); pstream->Out(paper_y); pstream->Out(" translate\n");
  if (landscape) {
    pstream->Out(paper_y_scale); pstream->Out(" "); pstream->Out(paper_x_scale); pstream->Out(" scale\n");
    pstream->Out("90 rotate\n");
  } else {
    pstream->Out(paper_x_scale); pstream->Out(" "); pstream->Out(paper_y_scale); pstream->Out(" scale\n");
  }
  pstream->Out("2 setlinecap\n");

  resetFont = RESET_FONT | RESET_COLOR;

  if (clipping)
    SetClippingRegion(clipping);
}

void wxPostScriptDC::EndPage (void)
{
  if (!pstream)
    return;
  pstream->Out("showpage\n");
}


static void printhex(PSStream *pstream, int v)
{
  int h, l;
  char s[3];

  s[2] = 0;
  
  h = (v >> 4) & 0xF;
  l = v & 0xF;
  
  if (h <= 9)
    s[0] = '0' + h;
  else
    s[0] = 'a' + (h - 10);
  if (l <= 9)
    s[1] = '0' + l;
  else
    s[1] = 'a' + (l - 10);

  pstream->Out(s);
}


Bool wxPostScriptDC::
Blit (float xdest, float ydest, float fwidth, float fheight,
      wxMemoryDC *src, float xsrc, float ysrc, int rop, wxColour *dcolor)
{
  int mono;
  long j, i;
  wxColour *c;
  int pixel;
  int pr, pg, pb;

  wxCanvasDC *source = (wxCanvasDC *)src;
  long width, height, x, y;
  Bool asColour = level2ok;

  if (!pstream)
    return FALSE;

  width = (long)floor(fwidth);
  height = (long)floor(fheight);
  x = (long)floor(xsrc);
  y = (long)floor(ysrc);

  /* Since we want a definition, may need to start a dictionary: */
  if (rop >= 0) {
    pstream->Out("1 dict begin\n");
  }

  /* Allocate space. */
  pstream->Out("/DataString ");
  pstream->Out((width * (asColour ? 3 : 1) * ((rop < 0) ? height : 1)));
  pstream->Out(" string def\n");

  if (rop < 0) {
    pstream->Out(" /PaintProc { begin \n");
  }

  /* PostScript setup: */
  pstream->Out("gsave\n");
  if (rop >= 0) {
    pstream->Out(XSCALE(xdest)); pstream->Out(" "); pstream->Out(YSCALE(ydest) - fheight); pstream->Out(" translate\n");
  }
  pstream->Out(fwidth); pstream->Out(" "); pstream->Out(fheight); pstream->Out(" scale\n");
  pstream->Out(width); pstream->Out(" "); pstream->Out(height); pstream->Out(" 8 [ ");
  pstream->Out(width); pstream->Out(" 0 0 "); pstream->Out((-height)); pstream->Out(" 0 "); pstream->Out(height);
  pstream->Out(" ]\n");
  if (rop >= 0) {
    pstream->Out("{\n");
    pstream->Out("  currentfile DataString readhexstring pop\n");
    pstream->Out("} bind");
  } else {
    pstream->Out(" { DataString } ");
  }
  if (asColour) {
    pstream->Out(" false 3 colorimage\n");
  } else {
    pstream->Out(" image\n");
  }
  
  if (rop < 0) {
    pstream->Out("grestore\n } def \n");
    pstream->Out(" { currentfile DataString readhexstring pop pop } exec\n");
  }

  /* Output data as hex digits: */
  {
    wxBitmap *sbm;
    sbm = src->GetObject();
    mono = (sbm->GetDepth() == 1);
   }

  if (mono && dcolor) {
    pr = dcolor->Red();
    pg = dcolor->Green();
    pb = dcolor->Blue();
  } else
    pr = pg = pb = 0;

  c = new wxColour;
  for (j = 0; j < height; j++) {
    for (i = 0; i < width; i++) {
      int red, green, blue;

      source->GetPixel(i, j, c);

      red = c->Red();
      green = c->Green();
      blue = c->Blue();

      if (mono && !red && !green && !blue) {
	red = pr;
	green = pg;
	blue = pb;
      } else if (mono) {
	if ((rop != wxSOLID) && (rop != (-wxSOLID - 1))) {
	  red = current_background_color->Red();
	  green = current_background_color->Green();
	  blue = current_background_color->Blue();
	}
      }

      if (asColour) {
	printhex(pstream, red);
	printhex(pstream, green);
	printhex(pstream, blue);
      } else {
	float r, g, b;

	r = ((float)(red) / 255);
	g = ((float)(green) / 255);
	b = ((float)(blue) / 255);

	pixel = (int)(255 * sqrt(((r * r) + (g * g) + (b * b)) / 3));
	
	printhex(pstream, pixel);
      }
    }
    pstream->Out("\n");
  }

  if (rop >= 0) {
    pstream->Out("grestore\n");
    /* End dictionary: */
    pstream->Out("end\n");
  }

  if (rop >= 0) {
    CalcBoundingBox(XSCALEBND(xdest), YSCALEBND(ydest));
    /* Bitmap isn't scaled: */
    CalcBoundingBox(XSCALEBND(xdest) + fwidth, YSCALEBND(ydest) + fheight);
  }

  return TRUE;
}

static wxMemoryDC *temp_mdc;

Bool wxPostScriptDC::Blit (float xdest, float ydest, float fwidth, float fheight,
      wxBitmap *bm, float xsrc, float ysrc, int rop, wxColour *c)
{
  Bool v;

  if (!temp_mdc) {
    wxREGGLOB(temp_mdc);
    temp_mdc = new wxMemoryDC(1);
  }
  
  temp_mdc->SelectObject(bm);
  /* Might fail, so we double-check: */
  if (temp_mdc->GetObject()) {
    v = Blit(xdest, ydest, fwidth, fheight,
	     temp_mdc, xsrc, ysrc, rop, c);
    temp_mdc->SelectObject(NULL);
  } else
    v = FALSE;

  return v;
}

float wxPostScriptDC::GetCharHeight (void)
{
  if (current_font)
    return (float) current_font->GetPointSize ();
  else
    return 12.0;
}


float wxPostScriptDC::GetCharWidth (void)
{
  return 0;
}

// these static vars are for storing the state between calls
static int lastFamily= INT_MIN;
static int lastSize= INT_MIN;
static int lastStyle= INT_MIN;
static int lastWeight= INT_MIN;
static int lastDescender = INT_MIN;
static int capHeight = -1;
static int lastWidths[256]; // widths of the characters

void wxPostScriptDC::GetTextExtent (const char *string, float *x, float *y,
				    float *descent, float *topSpace, wxFont *theFont,
				    Bool WXUNUSED(use16), int dt)
{
  wxFont *fontToUse = theFont;
  int Family;
  int Size;
  int Style;
  int Weight;

  float widthSum = 0.0;
  float height;
  int dp;

  if (!fontToUse)
    fontToUse = current_font;
    
  if (!pstream) {
    *x = *y = 0;
    if (descent) *descent = 0.0;
    if (topSpace) *topSpace = 0.0;
    return;
  }
  
  // ************************************************************
  // method for calculating string widths in postscript:
  // read in the AFM (adobe font metrics) file for the
  // actual font, parse it and extract the character widths
  // and also the descender. this may be improved, but for now
  // it works well. the AFM file is only read in if the
  // font is changed. this may be chached in the future.
  // calls to GetTextExtent with the font unchanged are rather
  // efficient!
  //
  // for each font and style used there is an AFM file necessary.
  // currently i have only files for the roman font family.
  // i try to get files for the other ones!

  // get actual parameters
  Family = fontToUse->GetFontId();
  Size =   fontToUse->GetPointSize();
  Style =  fontToUse->GetStyle();
  Weight = fontToUse->GetWeight();
  
  // if we have a new font, read the font-metrics
  if (Family != lastFamily 
      || Size != lastSize 
      || Style != lastStyle
      || Weight != lastWeight) {
    char *name;
    char *afmName;
    FILE *afmFile;

    // store cached values
    lastFamily = Family;
    lastSize =   Size;
    lastStyle =  Style;
    lastWeight = Weight;

    // read in new font metrics **************************************

    // 1. construct filename ******************************************
    name = wxTheFontNameDirectory->GetPostScriptName(Family, Weight, Style);
    if (name && afm_path) {
      int len = strlen(afm_path);
      // get the directory of the AFM files
      afmName = new char[strlen(name) + len + 256];
      strcpy(afmName, afm_path);
#ifdef wx_mac
      if (len && (afm_path[len - 1] != ':'))
	strcat(afmName, ":");
#endif
#ifdef wx_x
      if (len && (afm_path[len - 1] != '/'))
	strcat(afmName, "/");
#endif
#ifdef wx_msw
      if (len && (afm_path[len - 1] != '/') && (afm_path[len - 1] != '\\'))
	strcat(afmName, "/");
#endif
      strcat(afmName, name);
      strcat(afmName,".afm");
    } else
      afmName = NULL;

    // 2. open and process the file **********************************

    // a short explanation of the AFM format:
    // we have for each character a line, which gives its size
    // e.g.:
    //
    //   C 63 ; WX 444 ; N question ; B 49 -14 395 676 ;
    //
    // that means, we have a character with ascii code 63, and width 
    // (444/1000 * fontSize) points.
    // the other data is ignored for now!
    //
    // when the font has changed, we read in the right AFM file and store the
    // character widths in an array, which is processed below (see point 3.).

    afmFile = (afmName ? fopen(afmName,"r") : (FILE *)NULL);

    if (afmFile==NULL) {
      int i;
      if (!complained_afm) {
	char bfr[256];
	sprintf(bfr, "Cannot open AFM file for %.150s; guessing font sizes.\n"
		"(Silently guessing fonts for future AFM failures.)"
		, name);
	wxError(bfr, "MrEd Warning");
	complained_afm = 1;
      }
      for (i = 0; i < 256; i++) {
	lastWidths[i] = 500; // an approximate value
      }
      lastDescender = -150; // dito.
    } else {
      // some variables for holding parts of a line
      char cString[256], semiString[256], WXString[256], descString[256];
      char line[256];
      int ascii, cWidth;
      int i;

      // init the widths array
      for (i = 0; i < 256; i++) {
	lastWidths[i] = INT_MIN;
      }

      // read in the file and parse it
      while (fgets(line,sizeof(line),afmFile)) {
        if (!strncmp(line, "Descender ", 10)) {
	  // descender
          if ((sscanf(line, "%s%d", descString, &lastDescender) != 2)
	      || strcmp(descString,"Descender")) {
	    wxDebugMsg("AFM-file '%s': line '%s' has error (bad descender)\n",
		       afmName, line);
          }
        } else if (!strncmp(line, "CapHeight ", 10)) {
	  // descender
          if ((sscanf(line, "%s%d", descString, &capHeight) != 2)
	      || strcmp(descString, "CapHeight")) {
	    wxDebugMsg("AFM-file '%s': line '%s' has error (bad cap height)\n",
		       afmName, line);
          }
        } else if (!strncmp(line, "C ", 2)){
	  // char-width
          if(sscanf(line, "%s%d%s%s%d", cString, &ascii, 
		    semiString, WXString, &cWidth) != 5) {
	    wxDebugMsg("AFM-file '%s': line '%s' has an error (bad character width)\n",
		       afmName, line);
          }
          if(strcmp(cString,"C") 
	     || strcmp(semiString,";") 
	     || strcmp(WXString,"WX")) {
	    wxDebugMsg("AFM-file '%s': line '%s' has a format error\n",afmName,line);
          }
          //printf("            char '%c'=%d has width '%d'\n",ascii,ascii,cWidth);
          if (ascii >= 0 && ascii < 256) {
            lastWidths[ascii] = cWidth; // store width
          } else {
	    /* This happens a lot; don't print an error */
            // wxDebugMsg("AFM-file '%s': ASCII value %d out of range\n",afmName,ascii);
          }
        }
      }
      fclose(afmFile);
    }
  }
  
  // 3. now the font metrics are read in, calc size *******************
  // this is done by adding the widths of the characters in the
  // string. they are given in 1/1000 of the size!

  height = (float)Size;
  for (dp = dt; string[dp]; dp++) {
    int ch;
    ch = ((unsigned char *)string)[dp];
    if (lastWidths[ch] == INT_MIN) {
      wxDebugMsg("GetTextExtent: undefined width for character '%c' (%d)\n",
                 ch, ch);
      widthSum += lastWidths[' ']; // assume space
    } else {
      widthSum += (lastWidths[ch] / 1000.0F) * Size;
    }
  }

  // add descender to height (it is usually a negative value)
  if (lastDescender != INT_MIN) {
    height += ((-lastDescender) / 1000.0F) * Size;
  }
  
  // return size values
  *x = widthSum;
  *y = height;

  // return other parameters
  if (descent){
    if (lastDescender != INT_MIN)
      *descent = ((-lastDescender) / 1000.0F) * Size;
    else
      *descent = 0.0;
  }

  if (topSpace) {
    if (capHeight > -1)
      *topSpace = ((1000 - capHeight) / 1000.0F) * Size;
    else
      *topSpace = 0.0;
  }
}

void wxPostScriptDC::SetMapMode (int WXXTUNUSED(mode))
{
#ifndef wx_xt
  mapping_mode = mode;
#endif
  return;
}

void wxPostScriptDC::SetUserScale (float x, float y)
{
  user_scale_x = x;
  user_scale_y = y;
}

float wxPostScriptDC::DeviceToLogicalX(int x)
{
  return (x - device_origin_x) / user_scale_x;
}

float wxPostScriptDC::DeviceToLogicalXRel(int x)
{
  return x / user_scale_x;
}

float wxPostScriptDC::DeviceToLogicalY(int y)
{
  float y2 = -(y - paper_h);
  return (y2 - device_origin_y) / user_scale_y;
}

float wxPostScriptDC::DeviceToLogicalYRel(int y)
{
  return y / user_scale_y;
}

int wxPostScriptDC::LogicalToDeviceX(float x)
{
  return (int)floor(XSCALE(x));
}

int wxPostScriptDC::LogicalToDeviceXRel(float x)
{
  return (int)floor(XSCALEREL(x));
}

int wxPostScriptDC::LogicalToDeviceY(float y)
{
  return (int)floor(YSCALE(y));
}

int wxPostScriptDC::LogicalToDeviceYRel(float y)
{
  return (int)floor(YSCALEREL(y));
}

float wxPostScriptDC::FLogicalToDeviceX(float x)
{
  return XSCALE(x);
}

float wxPostScriptDC::FLogicalToDeviceXRel(float x)
{
  return XSCALEREL(x);
}

float wxPostScriptDC::FLogicalToDeviceY(float y)
{
  return YSCALE(y);
}

float wxPostScriptDC::FLogicalToDeviceYRel(float y)
{
  return YSCALEREL(y);
}

void wxPostScriptDC::GetSize(float *width, float *height)
{
  if (width)
    *width = paper_w;
  if (height)
    *height = paper_h;
}

void wxPostScriptDC::GetSizeMM(float *WXUNUSED(width), float *WXUNUSED(height))
{
}

extern Bool wxsPrinterDialog(wxWindow *parent);

Bool XPrinterDialog(wxWindow *parent)
{
  return wxsPrinterDialog(parent);
}

//-----------------------------------------------------------------------------
// wxPrintSetup implementation
//-----------------------------------------------------------------------------

#define PS_DEFAULT_PAPER  "Letter 8 1/2 x 11 in"

#if defined(sun) && defined(wx_xview)
#	define PS_PREVIEW_COMMAND	"pageview"
#	define PS_PRINTER_COMMAND	"lpr"
#	define PS_PRINTER_OPTIONS	""
#	define PS_AFM_PATH		NULL
#elif defined(VMS)
#	define PS_PREVIEW_COMMAND	"view/format=ps/select=x_display"
#	define PS_PRINTER_COMMAND	"print"
#	define PS_PRINTER_OPTIONS	"/nonotify/queue=psqueue"
#	define PS_AFM_PATH		"sys$ps_font_metrics:"
#elif defined(__sgi)
#	define PS_PREVIEW_COMMAND	"dps"
#	define PS_PRINTER_COMMAND	"lpr"
#	define PS_PRINTER_OPTIONS	""
#	define PS_AFM_PATH		NULL
#elif defined(wx_x)
#	define PS_PREVIEW_COMMAND	"ghostview"
#	define PS_PRINTER_COMMAND	"lpr"
#	define PS_PRINTER_OPTIONS	""
#	define PS_AFM_PATH		NULL
#elif defined(wx_msw) || defined(wx_mac)
#	define PS_PREVIEW_COMMAND	"ghostview"
#	define PS_PRINTER_COMMAND	"print"
#	define PS_PRINTER_OPTIONS	""
#	define PS_AFM_PATH		NULL
#else
#	define PS_PREVIEW_COMMAND	NULL
#	define PS_PRINTER_COMMAND	NULL
#	define PS_PRINTER_OPTIONS	NULL
#	define PS_AFM_PATH		NULL
#endif

wxPrintSetupData::wxPrintSetupData(void)
{
    printer_command = PS_PRINTER_COMMAND;
    preview_command = PS_PREVIEW_COMMAND;
    printer_flags = PS_PRINTER_OPTIONS;
    printer_orient = PS_PORTRAIT;
    printer_scale_x = 1.0;
    printer_scale_y = 1.0;
    printer_translate_x = 0.0;
    printer_translate_y = 0.0;
#ifdef wx_x
    printer_mode = PS_PRINTER;
#else
    printer_mode = PS_FILE;
#endif
    afm_path = default_afm_path;
    paper_name = DEFAULT_PAPER;
    print_colour = TRUE;
    print_level_2 = TRUE;
    printer_file = NULL;
    emargin_v = emargin_h = 36;
}

wxPrintSetupData::~wxPrintSetupData(void)
{
}

void wxPrintSetupData::SetPrinterCommand(char *cmd)
{
    if (cmd == printer_command)
	return;
    if (cmd) {
	printer_command = copystring(cmd);
    } else
	printer_command = NULL;
}

void wxPrintSetupData::SetPrintPreviewCommand(char *cmd)
{
    if (cmd == preview_command)
	return;
    if (cmd) {
	preview_command = copystring(cmd);
    } else
	preview_command = NULL;
}

void wxPrintSetupData::SetPaperName(char *name)
{
  if (name == paper_name)
    return;
  if (name) {
    paper_name = copystring(name);
  } else
    paper_name = NULL;
}

void wxPrintSetupData::SetPrinterOptions(char *flags)
{
    if (printer_flags == flags)
      return;
    if (flags) {
      printer_flags = copystring(flags);
    } else
      printer_flags = NULL;
}

void wxPrintSetupData::SetPrinterFile(char *f)
{
    if (f == printer_file)
	return;
    if (f) {
	printer_file = copystring(f);
    } else
	printer_file = NULL;
}

void wxPrintSetupData::SetPrinterMode(int mode)
{
    printer_mode = PS_FILE;

    if (mode == PS_PREVIEW && preview_command
    ||  mode == PS_PRINTER && printer_command)
	printer_mode = mode;
}

void wxPrintSetupData::SetAFMPath(char *f)
{
    if (f && !default_afm_path) {
      wxREGGLOB(default_afm_path);
      default_afm_path = f;
    }
  
    if (f == afm_path)
	return;
    if (f) {
      afm_path = copystring(f);
    } else
	afm_path = NULL;
}

void wxPrintSetupData::copy(wxPrintSetupData* data)
{
  float x, y;
  char *s;
  int i;
  
  s = data->GetPrinterCommand();
  SetPrinterCommand(s);
  s = data->GetPrintPreviewCommand();
  SetPrintPreviewCommand(s);
  s = data->GetPrinterOptions();
  SetPrinterOptions(s);
  i = data->GetPrinterOrientation();
  SetPrinterOrientation(i);
  i = data->GetPrinterMode();
  SetPrinterMode(i);
  s = data->GetAFMPath();
  SetAFMPath(s);
  s = data->GetPaperName();
  SetPaperName(s);
  i = data->GetColour();
  SetColour(i);
  
  data->GetPrinterTranslation(&x, &y);
  SetPrinterTranslation(x, y);
  data->GetPrinterScaling(&x, &y);
  SetPrinterScaling(x, y);
}

//-----------------------------------------------------------------------------
// wxInitializePrintSetupData
//-----------------------------------------------------------------------------

void wxInitializePrintSetupData(Bool /* init */)
{
#ifdef wx_mac
  wxThePrintPaperDatabase = new wxPrintPaperDatabase;
  wxThePrintPaperDatabase->CreateDatabase();
#endif
  
  wxPrintSetupData *wxThePrintSetupData;
  
  wxThePrintSetupData = new wxPrintSetupData;
  
  wxThePrintSetupData->SetPrintPreviewCommand(PS_PREVIEW_COMMAND);
  wxThePrintSetupData->SetPrinterOrientation(PS_PORTRAIT);
#ifdef wx_x
  wxThePrintSetupData->SetPrinterMode(PS_PREVIEW);
#else
  wxThePrintSetupData->SetPrinterMode(PS_FILE);
#endif
  wxThePrintSetupData->SetPaperName(PS_DEFAULT_PAPER);
  wxThePrintSetupData->SetPrinterCommand(PS_PRINTER_COMMAND);
  wxThePrintSetupData->SetPrinterOptions(PS_PRINTER_OPTIONS);
  wxThePrintSetupData->SetAFMPath(PS_AFM_PATH);
  
  wxSetThePrintSetupData(wxThePrintSetupData);
}

//-----------------------------------------------------------------------------
// wxPrintPaperType implementation
//-----------------------------------------------------------------------------

wxPrintPaperType::wxPrintPaperType(char *name, int wmm, int hmm, int wp, int hp)
{
    widthMM = wmm;
    heightMM = hmm;
    widthPixels = wp;
    heightPixels = hp;
    pageName = copystring(name);
}

wxPrintPaperType::~wxPrintPaperType(void)
{
}


//-----------------------------------------------------------------------------
// wxPrintPaperDatabase implementation
//-----------------------------------------------------------------------------

wxPrintPaperDatabase::wxPrintPaperDatabase(void) : wxList(wxKEY_STRING)
{
    DeleteContents(TRUE);
}

wxPrintPaperDatabase::~wxPrintPaperDatabase(void)
{
}

void wxPrintPaperDatabase::CreateDatabase(void)
{
    // Need correct values for page size in pixels.
    // Each unit is one 'point' = 1/72 of an inch.
    // NOTE: WE NEED ALSO TO MAKE ADJUSTMENTS WHEN TRANSLATING
    // in wxPostScriptDC code, so we can start from top left.
    // So access this database and translate by appropriate number
    // of points for this paper size. OR IS IT OK ALREADY?
    // Can't remember where the PostScript origin is by default.
    // Heck, someone will know how to make it hunky-dory...
    // JACS 25/5/95
  
    AddPaperType("A4 210 x 297 mm", 210, 297,         595, 842);
    AddPaperType("A3 297 x 420 mm", 297, 420,         842, 1191);
    AddPaperType("Letter 8 1/2 x 11 in", 216, 279,    612, 791);
    AddPaperType("Legal 8 1/2 x 14 in", 216, 356,     612, 1009);
}

void wxPrintPaperDatabase::ClearDatabase(void)
{
    Clear();
}

void wxPrintPaperDatabase::AddPaperType(char *name, int wmm, int hmm,
					int wp, int hp)
{
  wxPrintPaperType *ppt;
  ppt = new wxPrintPaperType(name, wmm, hmm, wp, hp);
  Append(name, ppt);
}

wxPrintPaperType *wxPrintPaperDatabase::FindPaperType(char *name)
{
  wxNode *node;

  if ((node = Find(name)))
    return (wxPrintPaperType*)node->Data();
  else
    return NULL;
}
