/*
 * File:	xfspline.cc
 * Purpose:	
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

// Must be a comment as this file is #include\'d by wb_dc.cc
/* static const char sccsid[] = "%W% %G%"; */

/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1985 by Supoj Sutanthavibul
 */

#include "common.h"
#ifdef wx_mac
#include <QuickDraw.h>
#include "wx_setup.h"
#include <iostream.h>
#include <fstream.h>
#include <math.h>
#include "wx_obj.h"
#include "wx_dcps.h"
class wxList;
class wxPoint;
#endif //wx_mac

#if USE_SPLINES

#define     wx_round(a)    (int)((a)+.5)

class wxSpline: public wxObject
{
 public:
  int type;
  wxList *points;

  wxSpline(wxList *list);
  void DeletePoints(void);

  // Doesn't delete points
  ~wxSpline(void);
};

void wx_draw_open_spline(wxbDC *dc, wxSpline *spline);

#if USE_POSTSCRIPT
void wx_draw_open_spline_ps(wxPostScriptDC *dc, wxSpline *s);
const char *wxPostScriptHeaderSpline =	" \
/DrawSplineSection {\n\
	/y3 exch def\n\
	/x3 exch def\n\
	/y2 exch def\n\
	/x2 exch def\n\
	/y1 exch def\n\
	/x1 exch def\n\
	/xa x1 x2 x1 sub 0.666667 mul add def\n\
	/ya y1 y2 y1 sub 0.666667 mul add def\n\
	/xb x3 x2 x3 sub 0.666667 mul add def\n\
	/yb y3 y2 y3 sub 0.666667 mul add def\n\
	x1 y1 lineto\n\
	xa ya xb yb x3 y3 curveto\n\
	} def\n\
";
#endif

void wx_quadratic_spline(float a1, float b1, float a2, float b2,
                         float a3, float b3, float a4, float b4);
void wx_clear_stack(void);
int wx_spline_pop(float *x1, float *y1, float *x2, float *y2, float *x3,
        float *y3, float *x4, float *y4);
void wx_spline_push(float x1, float y1, float x2, float y2, float x3, float y3,
          float x4, float y4);
static Bool wx_spline_add_point(int x, int y);
static void wx_spline_draw_point_array(wxbDC *dc);
wxSpline *wx_make_spline(int x1, int y1, int x2, int y2, int x3, int y3);

void wxbDC::DrawSpline(int n, wxPoint points[])
{
  wxList list;
  for (int i =0; i < n; i++)
    list.Append((wxObject*)&points[i]);
  DrawSpline(&list);
}

void wxbDC::DrawSpline(wxList *list)
{
  wxSpline spline(list);

  wx_draw_open_spline(this, &spline);
}


wxList wx_spline_point_list;

void wx_draw_open_spline(wxbDC *dc, wxSpline *spline)
{
    wxPoint *p;
    float           cx1, cy1, cx2, cy2, cx3, cy3, cx4, cy4;
    float           x1, y1, x2, y2;

    wxNode *node = spline->points->First();
    p = (wxPoint *)node->Data();

    x1 = p->x;
    y1 = p->y;

    node = node->Next();
    p = (wxPoint *)node->Data();

    x2 = p->x;
    y2 = p->y;
    cx1 = (float)((x1 + x2) / 2);
    cy1 = (float)((y1 + y2) / 2);
    cx2 = (float)((cx1 + x2) / 2);
    cy2 = (float)((cy1 + y2) / 2);

    wx_spline_add_point((int) x1, (int) y1);

    node = node->Next();

    while (node)
    {
        p = (wxPoint *)node->Data();
	x1 = x2;
	y1 = y2;
	x2 = p->x;
	y2 = p->y;
        cx4 = (float)(x1 + x2) / 2;
        cy4 = (float)(y1 + y2) / 2;
        cx3 = (float)(x1 + cx4) / 2;
        cy3 = (float)(y1 + cy4) / 2;

        wx_quadratic_spline(cx1, cy1, cx2, cy2, cx3, cy3, cx4, cy4);

	cx1 = cx4;
	cy1 = cy4;
        cx2 = (float)(cx1 + x2) / 2;
        cy2 = (float)(cy1 + y2) / 2;

        node = node->Next();
    }

    wx_spline_add_point(wx_round(cx1), wx_round(cy1));
    wx_spline_add_point((int) x2, (int) y2);

    wx_spline_draw_point_array(dc);

}

void wx_draw_open_spline_ps(wxPostScriptDC *dc, wxSpline *s)
{
#if 0
	double		a, b, c, d, x1, y1, x2, y2, x3, y3;
        wxPoint *p, *q;
        if (dc->autoSetting)
          dc->SetPen(dc->current_pen);

        wxNode *node = s->points->First();
        p = (wxPoint *)node->Data();

	x1 = p->x; y1 = p->y;

        node = node->Next();
        p = (wxPoint *)node->Data();
	c = p->x; d = p->y;
        x3 = a = (float)(x1 + c) / 2;
        y3 = b = (float)(y1 + d) / 2;

        *(dc->pstream) << "newpath " << x1 << " " << dc->yorigin - y1 << " moveto " << x3 << " " << dc->yorigin - y3;
        *(dc->pstream) << " lineto\n";
        dc->CalcBoundingBox((float)x1, (float)(dc->yorigin - y1));
        dc->CalcBoundingBox((float)x3, (float)(dc->yorigin - y3));

        node = node->Next();

        while (node)
	{
          q = (wxPoint *)node->Data();

	  x1 = x3; y1 = y3;
	  x2 = c;  y2 = d;
	  c = q->x; d = q->y;
          x3 = (float)(x2 + c) / 2;
          y3 = (float)(y2 + d) / 2;
          *(dc->pstream) << x1 << " " << dc->yorigin - y1 << " " << x2 << " " << dc->yorigin - y2 << " ";
          *(dc->pstream) << x3 << " " << dc->yorigin - y3 << " DrawSplineSection\n";

          dc->CalcBoundingBox((float)x1, (float)(dc->yorigin - y1));
          dc->CalcBoundingBox((float)x3, (float)(dc->yorigin - y3));


         node = node->Next();
        }
	/*
	* At this point, (x2,y2) and (c,d) are the position of the 
	* next-to-last and last point respectively, in the point list
	*/
        *(dc->pstream) << c << " " << dc->yorigin - d << " lineto stroke\n";
#endif
}

/********************* CURVES FOR SPLINES *****************************

	The following spline drawing routine is from

	"An Algorithm for High-Speed Curve Generation"
	by George Merrill Chaikin,
	Computer Graphics and Image Processing, 3, Academic Press,
	1974, 346-349.

	and

	"On Chaikin's Algorithm" by R. F. Riesenfeld,
	Computer Graphics and Image Processing, 4, Academic Press,
	1975, 304-310.

***********************************************************************/

#define		half(z1, z2)	((z1+z2)/2.0)
#define		THRESHOLD	5

/* iterative version */

void wx_quadratic_spline(float a1, float b1, float a2, float b2, float a3, float b3, float a4,
                 float b4)
{
    register float  xmid, ymid;
    float           x1, y1, x2, y2, x3, y3, x4, y4;

    wx_clear_stack();
    wx_spline_push(a1, b1, a2, b2, a3, b3, a4, b4);

    while (wx_spline_pop(&x1, &y1, &x2, &y2, &x3, &y3, &x4, &y4)) {
        xmid = (float)half(x2, x3);
        ymid = (float)half(y2, y3);
	if (fabs(x1 - xmid) < THRESHOLD && fabs(y1 - ymid) < THRESHOLD &&
	    fabs(xmid - x4) < THRESHOLD && fabs(ymid - y4) < THRESHOLD) {
            wx_spline_add_point(wx_round(x1), wx_round(y1));
            wx_spline_add_point(wx_round(xmid), wx_round(ymid));
	} else {
            wx_spline_push(xmid, ymid, (float)half(xmid, x3), (float)half(ymid, y3),
                 (float)half(x3, x4), (float)half(y3, y4), x4, y4);
            wx_spline_push(x1, y1, (float)half(x1, x2), (float)half(y1, y2),
                 (float)half(x2, xmid), (float)half(y2, ymid), xmid, ymid);
	}
    }
}


/* utilities used by spline drawing routines */


typedef struct wx_spline_stack_struct {
    float           x1, y1, x2, y2, x3, y3, x4, y4;
}
                Stack;

#define         SPLINE_STACK_DEPTH             20
static Stack    wx_spline_stack[SPLINE_STACK_DEPTH];
static Stack   *wx_stack_top;
static int      wx_stack_count;

void wx_clear_stack(void)
{
    wx_stack_top = wx_spline_stack;
    wx_stack_count = 0;
}

void wx_spline_push(float x1, float y1, float x2, float y2, float x3, float y3, float x4, float y4)
{
    wx_stack_top->x1 = x1;
    wx_stack_top->y1 = y1;
    wx_stack_top->x2 = x2;
    wx_stack_top->y2 = y2;
    wx_stack_top->x3 = x3;
    wx_stack_top->y3 = y3;
    wx_stack_top->x4 = x4;
    wx_stack_top->y4 = y4;
    wx_stack_top++;
    wx_stack_count++;
}

int wx_spline_pop(float *x1, float *y1, float *x2, float *y2,
                  float *x3, float *y3, float *x4, float *y4)
{
    if (wx_stack_count == 0)
	return (0);
    wx_stack_top--;
    wx_stack_count--;
    *x1 = wx_stack_top->x1;
    *y1 = wx_stack_top->y1;
    *x2 = wx_stack_top->x2;
    *y2 = wx_stack_top->y2;
    *x3 = wx_stack_top->x3;
    *y3 = wx_stack_top->y3;
    *x4 = wx_stack_top->x4;
    *y4 = wx_stack_top->y4;
    return (1);
}

static Bool wx_spline_add_point(int x, int y)
{
  wxPoint *point = new wxPoint ;
  point->x = (float)x;
  point->y = (float)y;
  wx_spline_point_list.Append((wxObject*)point);
  return TRUE;
}

static void wx_spline_draw_point_array(wxbDC *dc)
{
  dc->DrawLines(&wx_spline_point_list, 0.0, 0.0);
  wxNode *node = wx_spline_point_list.First();
  while (node)
  {
    wxPoint *point = (wxPoint *)node->Data();
    delete point;
    delete node;
    node = wx_spline_point_list.First();
  }
}

wxSpline::wxSpline(wxList *list)
{
  points = list;
}

wxSpline::~wxSpline(void)
{
}

void wxSpline::DeletePoints(void)
{
  wxNode *node = points->First();
  while (node)
  {
    wxPoint *point = (wxPoint *)node->Data();
    delete point;
    delete node;
    node = points->First();
  }
  delete points;
}

#if USE_POSTSCRIPT

// Make a 3-point spline
void wxPostScriptDC::DrawSpline(float x1, float y1, float x2, float y2, float x3, float y3)
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

  wxSpline spline(point_list);

  wx_draw_open_spline_ps(this, &spline);
  spline.DeletePoints();
}

void wxPostScriptDC::DrawSpline(wxList *list)
{
  wxSpline spline(list);

  wx_draw_open_spline_ps(this, &spline);
}

void wxPostScriptDC::DrawSpline(int n, wxPoint points[])
{
  wxList list;
  for (int i =0; i < n; i++)
    list.Append((wxObject*)&points[i]);
  DrawSpline(&list);
}


#endif // USE_POSTSCRIPT

#endif // USE_SPLINES

