/*
 * File:        wx_medad.cc
 * Purpose:     wxMediaCanvas & wxDrawableMediaAdmin implementation
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 1995, Matthew Flatt
 */

#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "wx_canvs.h"
#include "wx_media.h"
#include "wx_types.h"
#include "wx_gcrct.h"
#include "wx_ptreq.h"
#include "wx_timer.h"
#include "wx_main.h"

#define XMARGIN 5
#define YMARGIN 5

#ifdef wx_motif
# define MEDIA_CANVAS_COMBAT_CLIP_BUG
#endif

class SimpleScroll
{
  Bool horizontal;

  int count, pageStep, value;

#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
  wxMediaCanvas *canvas;

  int h, w;
  int arrow_w, bar_w, thumb_w, thumb_x;

  int wherex, wherey;

  int thumb_drag_offset;
  
#endif
 public:
  SimpleScroll(wxMediaCanvas *, long style = 0,
	      int length = 0, int stepsPerPage = 4,
	      int position = 0);
  ~SimpleScroll();

#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
  void DoCallback(void);

  void Event(int x, int y, int w, int h, wxMouseEvent *);
  void Paint(int x, int y, int w, int h);

  void IntDrawLine(int x1, int y1, int x2, int y2);
  void DrawRectangle(float x, float y, float w, float h);

  void Click(int x);
  int trackState;
  Bool scrolling;
#endif

  void SetValue(int);
  int GetValue(void);
  void SetScroll(int len = -1, int page = -1, int position = -1);  
};

#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
# define SB_WIDTH 14
#endif

class wxUpdateCursorTimer : public wxTimer 
{
  wxCanvasMediaAdmin *admin;
 public:
  wxUpdateCursorTimer(wxCanvasMediaAdmin *a);
  void Notify(void);
  void Cancel();
};

wxUpdateCursorTimer::wxUpdateCursorTimer(wxCanvasMediaAdmin *a) {
  admin = a;
  Start(0, TRUE);
}

void wxUpdateCursorTimer::Notify(void) {
  Stop();
  if (admin) {
    admin->updateCursorTimer = NULL;
    admin->canvas->UpdateCursorNow();
  }
}

void wxUpdateCursorTimer::Cancel() {
  admin = NULL;
}

#define BLINK_DELAY 500

class wxBlinkTimer : public wxTimer 
{
  wxMediaCanvas *canvas;
 public:
  wxBlinkTimer(wxMediaCanvas *c);
  void Notify(void);
  void Kill();
};

wxBlinkTimer::wxBlinkTimer(wxMediaCanvas *c) {
  canvas = c;
}

void wxBlinkTimer::Notify(void) {
  wxYield();
  if (canvas)
    canvas->BlinkCaret();
}

void wxBlinkTimer::Kill() {
  canvas = NULL;
  Stop();
}

#define AUTO_DRAG_DELAY 100

class wxAutoDragTimer : public wxTimer 
{
  wxMediaCanvas *canvas;
  wxMouseEvent *event;
 public:
  wxAutoDragTimer(wxMediaCanvas *c, wxMouseEvent *e);
  void Notify(void);
  void Kill(void);
};

wxAutoDragTimer::wxAutoDragTimer(wxMediaCanvas *c, wxMouseEvent *e) {
  canvas = c;
  event = new wxMouseEvent(0);
  memcpy(event, e, sizeof(wxMouseEvent));
  Start(AUTO_DRAG_DELAY, TRUE);
}

void wxAutoDragTimer::Notify(void) {
  wxYield(); /* In case we get too much time */
  if (canvas) {
    event->timeStamp += AUTO_DRAG_DELAY;
    canvas->OnEvent(event);
  }
}

void wxAutoDragTimer::Kill(void) {
  canvas = NULL;
  Stop();
}

/************************************************************************/

#ifndef wxOVERRIDE_KEY_TRANSLATIONS
#define wxOVERRIDE_KEY_TRANSLATIONS 0
#endif

#define INIT_SB ((style & (wxMCANVAS_NO_H_SCROLL | wxMCANVAS_HIDE_H_SCROLL)) ? 0 : wxHSCROLL) \
                + ((style & (wxMCANVAS_NO_V_SCROLL | wxMCANVAS_HIDE_V_SCROLL)) ? 0 : wxVSCROLL)

#ifdef wx_x
# define wxmeBORDER wxNO_CAPTION
#else
# define wxmeBORDER wxBORDER
#endif

wxMediaCanvas::wxMediaCanvas(wxWindow *parent,
			     int x, int y,
			     int width, int height,
			     char *name,
			     long style,
			     int scrollsPP,
			     wxMediaBuffer *m)
: wxCanvas(parent, x, y, width, height,
	   wxRETAINED + wxmeBORDER + wxOVERRIDE_KEY_TRANSLATIONS + INIT_SB, name)
{
  static int type_added = FALSE;

#if USE_OLD_TYPE_SYSTEM
  if (!type_added) {
    wxAllTypes->AddType(wxTYPE_MEDIA_CANVAS, wxTYPE_CANVAS, "media-canvas");
    type_added = TRUE;
  }

  __type = wxTYPE_MEDIA_CANVAS;
#endif

  givenHScrollsPerPage = scrollsPP;

  allowXScroll = !(style  & wxMCANVAS_NO_H_SCROLL);
  allowYScroll = !(style  & wxMCANVAS_NO_V_SCROLL);
  fakeXScroll = !allowXScroll || (style & wxMCANVAS_HIDE_H_SCROLL);
  fakeYScroll = !allowYScroll || (style & wxMCANVAS_HIDE_V_SCROLL);

#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
  hscroll = new SimpleScroll(this, wxHORIZONTAL, 0, 1, 0);
  vscroll = new SimpleScroll(this, wxVERTICAL, 0, 1, 0);
  scrollWidth = scrollHeight = 0;
#else
  EnableScrolling(!fakeXScroll, !fakeYScroll);
  noloop = TRUE;
  wxCanvas::SetScrollbars(fakeXScroll ? -1 : 1, fakeYScroll ? -1 : 1,
#ifdef wx_msw
			  fakeXScroll ? -1 : 1, fakeYScroll ? -1 : 1,
#else
			  1, 1,
#endif
			  1, 1, 0, 0, FALSE);
  if (fakeXScroll) {
    SimpleScroll *ss;
    ss = new SimpleScroll(this, wxHORIZONTAL, 0, 1, 0) ;
    hscroll = ss;
  } else
    hscroll = (SimpleScroll *)NULL;
  if (fakeYScroll) {
    SimpleScroll *ss;
    ss = new SimpleScroll(this, wxVERTICAL, 0, 1, 0);
    vscroll = ss;
  } else
    vscroll = (SimpleScroll *)NULL;
  scrollWidth = fakeXScroll ? 0 : 1;
  scrollHeight = fakeYScroll ? 0 : 1;
#endif

  vscrollsPerPage = hscrollsPerPage = 1;
  hpixelsPerScroll = 0;

  noloop = FALSE;

  {
    wxCanvasMediaAdmin *cma;
    cma = new wxCanvasMediaAdmin(this);
    admin = cma;
  }
  admin->standard = 1;

  customCursor = NULL;
  customCursorOn = FALSE;

  focuson = FALSE;
  focusforcedon = FALSE;

  scrollToLast = FALSE;
  scrollBottomBased = FALSE;
  scrollOffset = 0;

  lastwidth = lastheight = -1;

  lazy_refresh = need_refresh = FALSE;

  autoDragger = NULL;

  if (m)
    SetMedia(m);

#ifndef wx_mac
  {
    wxDC *adc;
    adc = GetDC();
    adc->SetOptimization(TRUE);
  }
#endif
}

wxMediaCanvas::~wxMediaCanvas()
{
  if (autoDragger) {
    autoDragger->Kill();
    autoDragger = NULL;
  }

  if (blinkTimer) {
    ((wxBlinkTimer *)blinkTimer)->Kill();
    blinkTimer = NULL;
  }

  if (media) {
    if (admin->nextadmin || admin->prevadmin)
      SetMedia(NULL);
    else
      DELETE_OBJ media;
  }

  DELETE_OBJ admin;
}

void wxMediaCanvas::OnSize(int w, int h)
{
#ifndef wx_msw
  wxCanvas::OnSize(w, h);
#endif

  if (noloop)
    return;

  if (w == lastwidth
      && h == lastheight)
    return;

  if (media && media->printing)
    return;

  ResetVisual(FALSE);

#if defined(MEDIA_CANVAS_INTERNAL_SCROLLS) || defined(wx_mac)
#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
  DestroyClippingRegion();
#endif
  {
    wxDC *adc;
    adc = GetDC();
    adc->Clear();
  }
#endif

#ifdef NO_GET_CLIPPING_REGION
#ifndef MEDIA_CANVAS_INTERNAL_SCROLLS
  int cliph, clipw;
  GetClientSize(&clipw, &cliph);
  SetClippingRegion(XMARGIN, YMARGIN,
		    clipw - 2 * XMARGIN, cliph - 2 * YMARGIN);
#endif
#endif

  Refresh();
}

void wxMediaCanvas::OnFocus(Bool focus)
{
  if (focuson == focus)
    return;

  focuson = focus;
  if (media && !media->printing) {
    wxCanvasMediaAdmin *oldadmin;
    
    if (PTRNE((oldadmin = (wxCanvasMediaAdmin *)media->GetAdmin()), admin)) {
      media->SetAdmin(admin);
    }
    
    media->OwnCaret(focus);
    
    if (PTRNE(oldadmin, admin)) {
      media->SetAdmin(oldadmin);
    }
  }

  if (focuson) {
    if (!blinkTimer) {
      wxBlinkTimer *bt;
      bt = new wxBlinkTimer(this);
      blinkTimer = bt;
    }
    blinkTimer->Start(BLINK_DELAY, 1);
  }
}

void wxMediaCanvas::BlinkCaret()
{
  if (focuson) {
    if (media) {
      wxCanvasMediaAdmin *oldadmin;
    
      if (PTRNE((oldadmin = (wxCanvasMediaAdmin *)media->GetAdmin()), admin)) {
	media->SetAdmin(admin);
      }

      media->BlinkCaret();

      if (PTRNE(oldadmin, admin)) {
	media->SetAdmin(oldadmin);
      }
    }

    blinkTimer->Start(BLINK_DELAY, 1);
  }
}

void *wxMediaCanvas::CallAsPrimaryOwner(void *(*f)(void *), void *data)
{
  void *r;

  if (media) {
    wxCanvasMediaAdmin *oldadmin;
    
    if (PTRNE((oldadmin = (wxCanvasMediaAdmin *)media->GetAdmin()), admin)) {
      media->SetAdmin(admin);
    }
    
    r = f(data);
    
    if (PTRNE(oldadmin, admin)) {
      media->SetAdmin(oldadmin);
    }
  } else
    r = f(data);

  return r;
}

void wxMediaCanvas::OnSetFocus()
{
  OnFocus(TRUE);
}

void wxMediaCanvas::OnKillFocus()
{
  OnFocus(FALSE);
}

Bool wxMediaCanvas::IsFocusOn()
{
  return focuson;
}

void wxMediaCanvas::ForceDisplayFocus(Bool on)
{
  int old = focusforcedon;

  focusforcedon = on;

  admin->AdjustStdFlag();

  if ((focuson || focusforcedon) != ((focuson || old)))
    Repaint();
}

void wxMediaCanvas::OnEvent(wxMouseEvent *event)
{
  /* Turn of auto-dragger if there is one. */
  if (autoDragger) {
    autoDragger->Kill();
    autoDragger = NULL;
  }

  last_x = event->x;
  last_y = event->y;

#ifdef wx_msw
  if (!focuson && event->ButtonDown())
    SetFocus();
#endif
  
  if (media && !media->printing) {
#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
    if ((!event->Dragging() 
	 || (vscroll && vscroll->scrolling) 
	 || (hscroll && hscroll->scrolling))
	// Dragging into/outof window gives enter/leave events
	&& !(event->ButtonIsDown(-1) && (event->Leaving() || event->Entering()))) {
      int h, w;
      GetClientSize(&w, &h);
      if (!fakeYScroll) {
	if (vscroll->scrolling
	    || ((fakeXScroll || !hscroll->scrolling)
		&& (event->x > w - SB_WIDTH) 
		&& (event->y < h - (fakeXScroll ? 0 : SB_WIDTH)))) {
	  NoCustomCursor();
	  vscroll->Event(w - SB_WIDTH, 0, SB_WIDTH, h - SB_WIDTH, event);
	  return;
	}
      }
      if (!fakeXScroll) {
	if (hscroll->scrolling
	    || ((event->y > h - SB_WIDTH) 
		&& (event->x < w - (fakeYScroll ? SB_WIDTH : 0)))) {
	  NoCustomCursor();
	  hscroll->Event(0, h - SB_WIDTH, w - SB_WIDTH, SB_WIDTH, event);
	  return;
	}
      }
      if (!fakeXScroll && !fakeYScroll)
	if ((event->y > h - SB_WIDTH) || (event->x > w - SB_WIDTH)) {
	  NoCustomCursor();
	  return;
	}

      if (customCursor && !customCursorOn)
	SetCustomCursor(customCursor);
    }
#endif
    wxCanvasMediaAdmin *oldadmin;
    
    if (PTRNE((oldadmin = (wxCanvasMediaAdmin *)media->GetAdmin()), admin)) {
      media->SetAdmin(admin);
    }

    {
      wxCursor *c;
      c = media->AdjustCursor(event);
      SetCustomCursor(c);
    }
    media->OnEvent(event);
    
    if (PTRNE(oldadmin, admin)) {
      media->SetAdmin(oldadmin);
    }

    if (event->Dragging()) {
      int ch, cw;
      GetClientSize(&cw, &ch);
      if (event->x < 0 || event->y < 0 || event->x > cw || event->y > ch) {
	/* Dragging outside the canvas: auto-generate more events because the buffer
	   is probably scrolling. But make sure we're shown. */
	wxWindow *w = this;
	while (w && w->IsShown()) {
	  w = w->GetParent();
	}
	if (!w) {
	  autoDragger = new wxAutoDragTimer(this, event);
	}
      }
    }
  }
}

void wxMediaCanvas::UpdateCursorNow(void)
{
  wxMouseEvent *event;
  wxCanvasMediaAdmin *oldadmin;
    
  if (!media)
    return;

  event = new wxMouseEvent(wxEVENT_TYPE_MOTION);
  
  event->x = last_x;
  event->y = last_y;
  event->timeStamp = 0L;

  if (PTRNE((oldadmin = (wxCanvasMediaAdmin *)media->GetAdmin()), admin))
    media->SetAdmin(admin);
  
  {
    wxCursor *c;
    c = media->AdjustCursor(event);
    SetCustomCursor(c);
  }
    
  if (PTRNE(oldadmin, admin))
    media->SetAdmin(oldadmin);
}

void wxMediaCanvas::OnChar(wxKeyEvent *event)
{
  if (media && !media->printing) {
    wxCanvasMediaAdmin *oldadmin;
    
    if (PTRNE((oldadmin = (wxCanvasMediaAdmin *)media->GetAdmin()), admin)) {
      media->SetAdmin(admin);
    }

    media->OnChar(event);
    
    if (PTRNE(oldadmin, admin)) {
      media->SetAdmin(oldadmin);
    }
  }
}

void wxMediaCanvas::OnPaint(void)
{
  need_refresh = FALSE;

  if (media) {
    if (!media->printing) {
      float w, h, x, y;
#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
      PaintScrolls();
#endif
      GetView(&x, &y, &w, &h);
      Redraw(x, y, w, h);
    }
  } else {
    wxDC *adc;
    adc = GetDC();
    adc->Clear();
  }
  
  wxCanvas::OnPaint();
}

void wxMediaCanvas::Repaint(void)
{
  if (need_refresh)
    return;

  if (lazy_refresh) {
    need_refresh = TRUE;
    Refresh();
  } else
    OnPaint();
}

void wxMediaCanvas::PaintScrolls(void)
{
#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
  int h, w;

  DestroyClippingRegion();
  GetClientSize(&w, &h);
  if (!fakeXScroll)
    hscroll->Paint(0, h - (fakeYScroll ? 0 : SB_WIDTH), 
		   w - SB_WIDTH, SB_WIDTH);
  if (!fakeYScroll)
    vscroll->Paint(w - SB_WIDTH, 0, 
		   SB_WIDTH, h - (fakeXScroll ? 0 : SB_WIDTH));
  
  SetClippingRegion(XMARGIN, YMARGIN,
		    w - 2 * XMARGIN - (fakeYScroll ? 0 : SB_WIDTH),
		    h - 2 * YMARGIN - (fakeXScroll ? 0 : SB_WIDTH));
#endif
}

void wxMediaCanvas::SetLazyRefresh(Bool on)
{
  lazy_refresh = on;
  if (!on && need_refresh)
    OnPaint();
}

Bool wxMediaCanvas::GetLazyRefresh(void)
{
  return lazy_refresh;
}

void wxMediaCanvas::SetCustomCursor(wxCursor *cursor)
{
  if (!cursor) {
    NoCustomCursor();
  } else {
    customCursorOn = TRUE;
    customCursor = cursor;
    SetCursor(customCursor);
  }
}

void wxMediaCanvas::NoCustomCursor(void)
{
  static wxCursor *arrow = NULL;
  
  if (!arrow) {
    wxREGGLOB(arrow);
    arrow = new wxCursor(wxCURSOR_ARROW);
  }

  if (customCursorOn) {
    customCursorOn = FALSE;
    SetCursor(arrow);
  }
}

wxDC *wxMediaCanvas::GetDCAndOffset(float *fx, float *fy)
{
  int x, y;

  if (fx || fy) {
    GetScroll(&x, &y);
    if (fx)
      *fx = x * hpixelsPerScroll - XMARGIN;
    if (fy) {
      if (media && (y  || scrollBottomBased)) {
	int h, w;
	GetClientSize(&w, &h);
	h -= 2 * YMARGIN;
#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
	if (!fakeXScroll)
	  h -= SB_WIDTH;
#endif
	if (h < 0)
	  h = 0;
	{
	  float v;
	  v = media->ScrollLineLocation(y + scrollOffset) - YMARGIN;
	  *fy = v;
	}
	if (scrollBottomBased && (scrollHeight || scrollToLast))
	  *fy -= h;
      } else
	*fy = -YMARGIN;
    }
  }

  return GetDC();
}

void wxMediaCanvas::GetView(float *fx, float *fy, float *fw, float *fh, 
			    Bool WXUNUSED(full))
{
  int h, w;

  GetClientSize(&w, &h);
  GetDCAndOffset(fx, fy);
  if (1 /* !full */) {
    if (fx)
      *fx += XMARGIN;
    if (fy)
      *fy += YMARGIN;
  }
#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
  if (!fakeXScroll)
    h -= SB_WIDTH;
  if (!fakeYScroll)
    w -= SB_WIDTH;
  if (h < 0)
    h = 0;
  if (w < 0)
    w = 0;
#endif
  if (0 /* full */) {
    if (fh)
      *fh = h;
    if (fw)
      *fw = w;
  } else {
    if (fh) {
      if (h > 2 * YMARGIN)
	*fh = h - 2 * YMARGIN;
      else
	*fh = 0;
    }
    if (fw) {
      if (w > 2 * XMARGIN)
	*fw = w - 2 * XMARGIN;
      else
	*fw = 0;
    }
  }
}

void wxMediaCanvas::Redraw(float localx, float localy, float fw, float fh)
{
  float x, y, w, h, right, bottom;

  if (!media || media->printing)
    return;

#ifdef MEDIA_CANVAS_COMBAT_CLIP_BUG
  int cliph, clipw;
  GetClientSize(&clipw, &cliph);
#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
  if (!fakeXScroll)
    cliph -= SB_WIDTH;
  if (!fakeYScroll)
    clipw -= SB_WIDTH;
#endif
  {
    wxDC *adc;
    adc = GetDC();
    adc->SetClippingRect(XMARGIN, YMARGIN,
			 clipw - 2 * XMARGIN, cliph - 2 * YMARGIN);
  }
#endif
  
  GetView(&x, &y, &w, &h);

  right = x + w;
  bottom = y + h;

  if (localx > x)
    x = localx;
  if (localy > y)
    y = localy;

  if (right > localx + fw)
    right = localx + fw;
  if (bottom > localy + fh)
    bottom = localy + fh;
  w = right - x;
  h = bottom - y;

  if (w < 0)
    w = 0;
  if (h < 0)
    h = 0;

  if (w && h) {
    wxCanvasMediaAdmin *oldadmin;

    if (PTRNE((oldadmin = (wxCanvasMediaAdmin *)media->GetAdmin()), admin)) {
      media->SetAdmin(admin);
    }
    
    media->Refresh(x, y, w, h, 
		   (focuson || focusforcedon)
		   ? wxSNIP_DRAW_SHOW_CARET
		   : wxSNIP_DRAW_SHOW_INACTIVE_CARET);

    if (PTRNE(oldadmin, admin)) {
      media->SetAdmin(oldadmin);
    }
  }
}

Bool wxMediaCanvas::ScrollTo(float localx, float localy, float fw, float fh,
			     Bool refresh, int bias)
{
  int cy, cx, sy, sx;
  float iw, ih;
  float x, y;
  float find_dy;

  if (!media || media->printing || (!allowXScroll && !allowYScroll))
    return FALSE;

  GetView(&x, &y, &iw, &ih);

  if (!iw || !ih)
    return FALSE;

  if (scrollBottomBased)
    find_dy = ih;
  else
    find_dy = 0;

  GetScroll(&cx, &cy);

  if (allowYScroll) {
    if (// doesn't fit and bias is set:
	(bias == -1 && fh > ih) 
	// fits, need to shift down into view:
	|| (fh <= ih && localy < y) 
	// doesn't fit, no conflicting bias, can shift up to see more:
	|| (fh > ih && bias != 1 && localy < y)) 
      sy = media->FindScrollLine(find_dy + localy) - scrollOffset;
    else if (// doesn't fit, bias is set:
	     (bias == 1 && fh > ih) 
	     // fits, need to shift up into view:
	     || (fh <= ih && y + ih < localy + fh)) 
      sy = media->FindScrollLine(find_dy + localy + fh - ih) + 1 - scrollOffset;
    else if (// doesn't fit, no conflicting bias, maybe shift down to see more:
	     (fh > ih && bias != -1 && localy + fh > y + ih)) {
      // Shift to one more than the first scroll position that shows last line
      long my;
      my = media->FindScrollLine(find_dy + localy + fh - ih) + 1 - scrollOffset;
      // But only shift down the extra line if doing so doesn't skip the whole area
      if (media->ScrollLineLocation(my) < find_dy + localy + fh)
	sy = my;
      else if (my > 0)
	sy = my - 1;
      else
        sy = 0;
    } else
      sy = cy;
  } else
    sy = cy;

  if (allowXScroll) {
    if (hpixelsPerScroll) {
      if ((bias == -1 && fw > iw)
	  || (fw < iw && localx < x)
	  || (fw > iw && bias != 1 && localx < x))
	sx = (int)(localx / hpixelsPerScroll);
      else if ((bias == 1 && fw > iw)
	       || (fw < iw && x + iw < localx + fw)
	       || (fw > iw && bias != -1 && localx + fw > x + iw))
	sx = (int)((localx + fw - iw) / hpixelsPerScroll) + 1;
      else
	sx = cx;
    } else
      sx = 0;
  } else
    sx = cx;

  if (sy != cy || sx != cx) {
    if (hscroll)
      hscroll->SetValue(sx);
    if (vscroll)
      vscroll->SetValue(sy);
#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
    if (refresh)
      Repaint();
    else
      PaintScrolls();      
#else
    Scroll(sx, sy, refresh);
#endif
    return TRUE;
  } else
    return FALSE;
}

Bool wxMediaCanvas::ResetVisual(Bool reset_scroll)
{
  int x, y, sx, sy, lw, lh;
  float w, h;
  int hnumScrolls, vnumScrolls, hspp, vspp;
  long tw;
  float totalHeight, totalWidth;
  Bool retval = FALSE;

  if (givenHScrollsPerPage < 0) {
    givenHScrollsPerPage = -2;
    return FALSE;
  }

  while (1) {
    GetScroll(&sx, &sy);

    GetSize(&lw, &lh);
    lastwidth = lw;
    lastheight = lh;

    if (media && (allowXScroll || allowYScroll)) {
      if (reset_scroll)
	x = y = 0;
      else {
	x = sx;
	y = sy;
      }
      
      w = h = 0.0;
      GetView(NULL, NULL, &w, &h);
      totalWidth = totalHeight = 0.0;
      media->GetExtent(&totalWidth, &totalHeight);

      if (!h || (!scrollToLast && (h >= totalHeight))) {
	vnumScrolls = 0;
	scrollOffset = 0;
      } else {
	if (scrollBottomBased) {
	  vnumScrolls = media->NumScrollLines() - 1;
	  scrollOffset = 1;
	  if (!scrollToLast) {
	    long start;
	    start = media->FindScrollLine(h + 1) - 1;
	    scrollOffset += start;
	    vnumScrolls -= start;
	  }
	} else {
	  long top = (long)(totalHeight - (scrollToLast ? 0 : h));
	  if (top)
	    --top;
	  vnumScrolls = media->FindScrollLine(top) + 1;
	  if (vnumScrolls >= media->NumScrollLines())
	    vnumScrolls = media->NumScrollLines() - 1;
	  scrollOffset = 0;
	}
      }

      if (vnumScrolls > 0) {
	int numLines;
	numLines = media->NumScrollLines() - 1;
	vspp = (long)(((float)h * numLines) / totalHeight) - 1;
	if (vspp < 1)
	  vspp = 1;
      } else {
	vnumScrolls = 0;
	vspp = 1;
      }

      if (totalWidth >= w) {
	tw = (long)(totalWidth - w);

	hpixelsPerScroll = (long)(w / givenHScrollsPerPage);
	if (!hpixelsPerScroll)
	  hpixelsPerScroll = 2;

	if (tw % hpixelsPerScroll)
	  tw += (hpixelsPerScroll - (tw % hpixelsPerScroll));
	
	hnumScrolls = tw / hpixelsPerScroll;
	hspp = givenHScrollsPerPage;
      } else {
	hnumScrolls = 0;
	hspp = 1;
      }
    } else {
      x = y = 0;
      hnumScrolls = vnumScrolls = 0;
      vspp = hspp = 1;
      if (!media) {
	wxDC *adc;
	adc = GetDC();
	adc->Clear();
      }
    }    

    if (scrollWidth != hnumScrolls || scrollHeight != vnumScrolls
	|| vspp != vscrollsPerPage
	|| hspp != hscrollsPerPage
	|| x != sx || y != sy) {
      Bool goAgain;
      int savenoloop;
      int saveHSPP;
      
      if (hscroll)
	hscroll->SetScroll(hnumScrolls, hspp, x);
      if (vscroll)
	vscroll->SetScroll(vnumScrolls, vspp, y);
      
#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
      PaintScrolls();
#else
      savenoloop = noloop;
      saveHSPP = givenHScrollsPerPage;
      
      noloop = TRUE;
      givenHScrollsPerPage = -1;
      
      if (!fakeXScroll) {
	if (x > hnumScrolls)
	  x = hnumScrolls;
	if (hspp < hscrollsPerPage)
	  SetScrollPage(wxHORIZONTAL, hspp);
	if (x < sx)
	  SetScrollPos(wxHORIZONTAL, x);
	if (scrollWidth != hnumScrolls)
	  SetScrollRange(wxHORIZONTAL, hnumScrolls);
	if (x > sx)
	  SetScrollPos(wxHORIZONTAL, x);
	if (hspp > hscrollsPerPage)
	  SetScrollPage(wxHORIZONTAL, hspp);
      }

      if (!fakeYScroll) {
	if (y > vnumScrolls)
	  y = vnumScrolls;
	if (vspp < vscrollsPerPage)
	  SetScrollPage(wxVERTICAL, vspp);
	if (y < sy)
	  SetScrollPos(wxVERTICAL, y);
	if (scrollHeight != vnumScrolls)
	  SetScrollRange(wxVERTICAL, vnumScrolls);
	if (y > sy)
	  SetScrollPos(wxVERTICAL, y);
	if (vspp > vscrollsPerPage)
	  SetScrollPage(wxVERTICAL, vspp);
      }

      goAgain = (givenHScrollsPerPage < -1);
      givenHScrollsPerPage = saveHSPP;
      
      noloop = savenoloop;
#endif
      hscrollsPerPage = hspp;
      vscrollsPerPage = vspp;
      scrollWidth = hnumScrolls;
      scrollHeight = vnumScrolls;

#ifndef MEDIA_CANVAS_INTERNAL_SCROLLS
      if (!goAgain)
	return TRUE;
      else
	retval = TRUE;
#else
      return TRUE;
#endif
    } else
      return retval;
  }
}

#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
#define MCIS_UNUSED(x) 
#else
#define MCIS_UNUSED(x) x
#endif

void wxMediaCanvas::Scroll(int MCIS_UNUSED(x), int MCIS_UNUSED(y), 
			   Bool MCIS_UNUSED(refresh))
{
#ifndef MEDIA_CANVAS_INTERNAL_SCROLLS
  int savenoloop = noloop;
  noloop = TRUE;
  
  if (x > -1 && !fakeXScroll) {
    if (scrollWidth) {
      if (x > scrollWidth)
	x = scrollWidth;
      SetScrollPos(wxHORIZONTAL, x);
    }
  }

  if (y > -1 && !fakeYScroll) {
    if (scrollHeight) {
      if (y > scrollHeight)
	y = scrollHeight;
      SetScrollPos(wxVERTICAL, y);
    }
  }
  
  noloop = savenoloop;

  if (refresh)
    Repaint();
#endif
}

void wxMediaCanvas::Scroll(int, int)
{
  /* Nothing */
}

void wxMediaCanvas::SetScrollbars(int, int, int, int,
				  int, int, int, int,
				  Bool)
{
  /* Nothing */
}

void wxMediaCanvas::GetScroll(int *x, int *y)
{
  int v;
  
  /* Get fake scroll values if available */
  if (hscroll) {
    v = hscroll->GetValue();
    *x = v;
  }
  if (vscroll) {
    v = vscroll->GetValue();
    *y = v;
  }

#ifndef MEDIA_CANVAS_INTERNAL_SCROLLS
  if (!hscroll) {
    int v;
    v = GetScrollPos(wxHORIZONTAL);
    *x = v;
  }
  if (!vscroll) {
    int v;
    v = GetScrollPos(wxVERTICAL);
    *y = v;
  }
#endif
}

void wxMediaCanvas::OnScroll(wxScrollEvent *)
{
  if (noloop)
    return;
  Repaint();
}

wxMediaBuffer *wxMediaCanvas::GetMedia(void)
{
  return media;
}

void wxMediaCanvas::SetMedia(wxMediaBuffer *m, Bool update)
{
  if (media) {
    if (PTREQ((wxCanvasMediaAdmin *)media->GetAdmin(), admin)) {
      if (admin->nextadmin)
	media->SetAdmin(admin->nextadmin);
      else if (admin->prevadmin)
	media->SetAdmin(admin->prevadmin);
      else
	media->SetAdmin(NULL);
    }

    if (admin->nextadmin) {
      admin->nextadmin->prevadmin = admin->prevadmin;
      admin->nextadmin->AdjustStdFlag();
    }
    if (admin->prevadmin) {
      admin->prevadmin->nextadmin = admin->nextadmin;
      admin->prevadmin->AdjustStdFlag();
    }
    if (customCursor) {
      NoCustomCursor();
      customCursor = NULL;
    }
  }
  media = m;
  if (media) {
    wxMediaAdmin *oldadmin;
    if ((oldadmin = media->GetAdmin())) {
      if (!oldadmin->standard) {
	media = NULL;
	return;
      }
      admin->nextadmin = (wxCanvasMediaAdmin *)oldadmin;
      admin->prevadmin = admin->nextadmin->prevadmin;
      admin->nextadmin->prevadmin = admin;
      admin->nextadmin->AdjustStdFlag();
      if (admin->prevadmin) {
	admin->prevadmin->nextadmin = admin;
	admin->prevadmin->AdjustStdFlag();
      }
      /* Get the right cursor: */
      admin->UpdateCursor();
    } else {
      admin->nextadmin = admin->prevadmin = NULL;
      media->SetAdmin(admin);
      media->OwnCaret(focuson);
    }
  }

  admin->AdjustStdFlag();

  ResetVisual(TRUE);
  if (update)
    Repaint();
}

void wxMediaCanvas::AllowScrollToLast(Bool toLast)
{
  scrollToLast = toLast;
  ResetVisual(FALSE);
  Repaint();
}

void wxMediaCanvas::ScrollWithBottomBase(Bool bottom)
{
  scrollBottomBased = bottom;
  ResetVisual(FALSE);
  Repaint();
}

/************************************************************************/

wxCanvasMediaAdmin::wxCanvasMediaAdmin(wxMediaCanvas *c)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_CANVAS_MEDIA_ADMIN;
#endif

  canvas = c;
  resetFlag = FALSE;
  nextadmin = prevadmin = NULL;
  WXGC_IGNORE(this, canvas);
  // WXGC_IGNORE(nextadmin);
  // WXGC_IGNORE(prevadmin);

  updateCursorTimer = NULL;

  updateBlock = resizedBlock = FALSE;
}

wxCanvasMediaAdmin::~wxCanvasMediaAdmin()
{
  if (updateCursorTimer)
    updateCursorTimer->Cancel();
}

wxDC *wxCanvasMediaAdmin::GetDC(float *fx, float *fy)
{
  if (canvas->media && canvas->media->printing) {
    if (fx)
      *fx = 0;
    if (fy)
      *fy = 0;
    return canvas->media->printing;
  } else
    return canvas->GetDCAndOffset(fx, fy);
}

void wxCanvasMediaAdmin::GetView(float *fx, float *fy, float *fh, float *fw, 
				 Bool full)
{
  if (canvas->media && canvas->media->printing) {
    if (fx)
      *fx = 0;
    if (fy)
      *fy = 0;
    if (fh)
      *fh = 10000;
    if (fw)
      *fw = 10000;
  } else
    canvas->GetView(fx, fy, fh, fw, full);
}

void wxCanvasMediaAdmin::GetMaxView(float *fx, float *fy, float *fw, float *fh, 
				    Bool full)
{
  if ((!nextadmin && !prevadmin) || (canvas->media && canvas->media->printing)) {
    GetView(fx, fy, fw, fh, full);
  } else {
    wxCanvasMediaAdmin *a;
    float cx, x, cy, y, cw, w, ch, h, cr, r, cb, b;

    a = this;
    while (a->prevadmin) {
      a = a->prevadmin;
    }
    a->GetView(&cx, &cy, &cw, &ch);
    cr = cx + cw;
    cb = cy + ch;
    for (a = a->nextadmin; a; a = a->nextadmin) {
      a->GetView(&x, &y, &w, &h);
      r = x + w;
      b = y + h;

      if (x < cx)
	cx = x;
      if (y < cy)
	cy = y;
      if (r > cr)
	cr = r;
      if (b > cb)
	cb = b;
    }

    cw = cr - cx;
    ch = cb - cy;

    if (fx)
      *fx = cx;
    if (fy)
      *fy = cy;
    if (fw)
      *fw = cw;
    if (fh)
      *fh = ch;
  }
}

Bool wxCanvasMediaAdmin::ScrollTo(float localx, float localy,
				    float w, float h, Bool refresh, int bias)
{
  if (!canvas->IsFocusOn()) {
    wxCanvasMediaAdmin *a;
    
    for (a = nextadmin; a; a = a->nextadmin) {
      if (a->canvas->IsFocusOn())
	return a->ScrollTo(localx, localy, w, h, refresh, bias);
    }
    for (a = prevadmin; a; a = a->prevadmin) {
      if (a->canvas->IsFocusOn())
	return a->ScrollTo(localx, localy, w, h, refresh, bias);
    }
  }

  return canvas->ScrollTo(localx, localy, w, h, refresh, bias);
}

void wxCanvasMediaAdmin::GrabCaret(int dist)
{
  if (dist == wxFOCUS_GLOBAL)
    canvas->SetFocus();
}

void wxCanvasMediaAdmin::NeedsUpdate(float localx, float localy, 
				     float w, float h)
{
  if (updateBlock)
    return;

  updateBlock = TRUE;

  if (resetFlag) {
    canvas->Repaint();
    resetFlag = FALSE;
  } else
    canvas->Redraw(localx, localy, w, h);

  if (nextadmin)
    nextadmin->NeedsUpdate(localx, localy, w, h);
  if (prevadmin)
    prevadmin->NeedsUpdate(localx, localy, w, h);

  updateBlock = FALSE;
}

void wxCanvasMediaAdmin::Resized(Bool update)
{
  if (resizedBlock)
    return;

  resizedBlock = TRUE;

  if (canvas->ResetVisual(FALSE))
    resetFlag = TRUE;

  if (update) {
    canvas->Repaint();
    resetFlag = FALSE;
  }
  if (nextadmin)
    nextadmin->Resized(update);
  if (prevadmin)
    prevadmin->Resized(update);

  resizedBlock = FALSE;
}

void wxCanvasMediaAdmin::UpdateCursor()
{
  if (!updateCursorTimer) {
    updateCursorTimer = new wxUpdateCursorTimer(this);

    if (nextadmin)
      nextadmin->UpdateCursor();
    if (prevadmin)
      prevadmin->UpdateCursor();
  }
}

void wxCanvasMediaAdmin::AdjustStdFlag(void)
{ 
  /* 1 indicates that this is the sole, main admin. 
     This info is used for quick (Xor) caret refreshing
     by an editor buffer. */

  standard = (nextadmin 
	      || prevadmin 
	      || (canvas && canvas->focusforcedon)) 
    ? -1 : 1; 
}

void wxMediaAdmin::GetMaxView(float *fx, float *fy, float *fh, float *fw, 
			      Bool full)
{
  GetView(fx, fy, fh, fw, full);
}

Bool wxMediaAdmin::DelayRefresh()
{
  return FALSE;
}

/*************************************************************/

#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
static wxBrush *scrollBrush = NULL;
static wxBrush *scrollEraseBrush = NULL;
static wxPen *scrollErasePen = NULL;
#endif

#define MIN_THUMB_WIDTH 8

SimpleScroll::SimpleScroll(wxMediaCanvas *
#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
			   inside
#endif
			   ,
			   long style,
			   int length, int stepsPerPage,
			   int position)
{
#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS
  canvas = inside;

  if (!scrollBrush) {
    scrollBrush = wxTheBrushList->FindOrCreateBrush("BLACK", wxSOLID);
    scrollEraseBrush = wxTheBrushList->FindOrCreateBrush("WHITE", wxSOLID);
    scrollErasePen = wxThePenList->FindOrCreatePen("WHITE", 1, wxTRANSPARENT);
  }
#endif

  count = length;
  pageStep = stepsPerPage;
  value = position;

  horizontal = !!(style & wxHORIZONTAL);
  SetScroll(length, stepsPerPage, position);
}

SimpleScroll::~SimpleScroll()
{
}

void SimpleScroll::SetValue(int position)
{
  if (position < 0)
    position = 0;
  if (position >= count)
    position = count;

  value = position;
}

void SimpleScroll::SetScroll(int length, int stepsPerPage, int position)
{
  if (length > -1)
    count = length;
  if (stepsPerPage > 0)
    pageStep = stepsPerPage;
  if (position > -1)
    value = position;

  if (value < 0)
    value = 0;
  if (value > count)
    value = count;
}

int SimpleScroll::GetValue(void)
{
  return value;
}

#ifdef MEDIA_CANVAS_INTERNAL_SCROLLS

void SimpleScroll::IntDrawLine(int x1, int y1, int x2, int y2)
{
  if (horizontal)
    canvas->IntDrawLine(wherex + x1, wherey + y1, wherex + x2, wherey + y2);
  else
    canvas->IntDrawLine(wherex + y1, wherey + x1, wherex + y2, wherey + x2);
}

void SimpleScroll::DrawRectangle(float x, float y, float w, float h)
{
  if (horizontal)
    canvas->DrawRectangle(wherex + x, wherey + y, w, h);
  else
    canvas->DrawRectangle(wherex + y, wherey + x, h, w);
}

void SimpleScroll::Paint(int x, int y, int w, int h)
{
  int oldvalue;
  wxDC *dc;
  wxBrush *save;
  wxPen *savePen;
  int seg, half;

  if (!horizontal) {
    oldvalue = w;
    w = h;
    h = oldvalue;
  }

  h -= 2;

  wherex = x + (horizontal ? 0 : 1);
  wherey = y + (horizontal ? 1 : 0);

  if (w < 2 * h)
    arrow_w = w / 2;
  else
    arrow_w = h;

  bar_w = w - 2 * arrow_w;
  if (bar_w <= 0) {
    thumb_w = bar_w = 0;
    thumb_x = arrow_w;
  } else {
    if (!count) {
      thumb_w = bar_w;
      thumb_x = arrow_w;
    } else {
      thumb_w = (long)(((float)pageStep * bar_w) / (count + pageStep));
      if ((thumb_w < MIN_THUMB_WIDTH) && (bar_w > MIN_THUMB_WIDTH))
	thumb_w = MIN_THUMB_WIDTH;
      thumb_x = arrow_w + (long)(((float)value * (bar_w - thumb_w - 1)) / count);
    }
  }

  dc = canvas->GetDC();
  
  save = dc->GetBrush();
  savePen = dc->GetPen();
  dc->SetBrush(scrollBrush);

  IntDrawLine(arrow_w, 0, arrow_w, h);
  IntDrawLine(w - arrow_w, 0, w - arrow_w, h);
  IntDrawLine(w + 1, 0, w + 1, h);
  IntDrawLine(0, 0, w + 1, 0);
  IntDrawLine(0, h, w + 1, h);

  half = h / 2;
  seg = arrow_w / 4;

  IntDrawLine(3 * seg, 0, seg, half);
  IntDrawLine(3 * seg, h, seg + 1, half + 1);
  IntDrawLine(w - 3 * seg, 0, w - seg, half);
  IntDrawLine(w - 3 * seg, h, w - seg - 1, half + 1);

  if (bar_w) {
    if (thumb_w < bar_w) {
      if (thumb_x > arrow_w)
	DrawRectangle(arrow_w + 1, 1, thumb_x - arrow_w, h - 1);
      if (value < count)
	DrawRectangle(thumb_x + thumb_w, 1, 
		      w - arrow_w - (thumb_x + thumb_w), h - 1);
    } else
      --thumb_w;
  }

  if (thumb_w > 0) {
    dc->SetBrush(scrollEraseBrush);
    dc->SetPen(scrollErasePen);
    DrawRectangle(thumb_x + 1, 1, thumb_w, h - 1);
  }

  dc->SetBrush(save);
  dc->SetPen(savePen);
}

enum {
  ss_CLICK = 1,
  ss_PAGE_DOWN,
  ss_PAGE_UP,
  ss_THUMB
};

void SimpleScroll::Click(int x)
{
  int oldvalue = value;

  if (trackState == ss_CLICK) {
    if (x < arrow_w)
      SetValue(value - 1);
    else if (x > w - arrow_w)
      SetValue(value + 1);
  } else if (trackState == ss_PAGE_DOWN) {
    if (x > thumb_x + thumb_w)
      SetValue(value + pageStep);
  } else if (trackState == ss_PAGE_UP) {
    if (x < thumb_x)
      SetValue(value - pageStep);
  } else if (trackState == ss_THUMB) {
    x = (x - arrow_w - thumb_drag_offset);
    int w = bar_w - thumb_w;
    if (x < 0)
      x = 0;
    if (x > w)
      x = w;
    SetValue((int)(((float)(x * count) / w) + 0.5));
  }

  if (value != oldvalue)
    canvas->Repaint();
}

#include "wx_timer.h"

#define SS_TIMER_FIRST_DELAY 500
#define SS_TIMER_DELAY 80

class SSTimer : public wxTimer
{
  int x;
  Bool restarted;
  SimpleScroll *ss;
public:
  SSTimer(int _x, SimpleScroll *_ss) { x = _x; ss = _ss; restarted = FALSE;}
  void Notify(void) 
  {
    if (ss->trackState) {
      wxYield(); /* timer can take over: check for events */
      ss->Click(x);
      if (!restarted) {
	restarted = 1;
	Start(SS_TIMER_DELAY);
      }
    } else if (restarted)
      Stop();
  }
};

#define SS_USE_TIMER(trackState) \
 (trackState == ss_CLICK \
  || trackState == ss_PAGE_UP \
  || trackState == ss_PAGE_DOWN)

void SimpleScroll::Event(int x, int y, int w, int h, wxMouseEvent *event)
{
  x = (long)event->x - x;
  y = (long)event->y - y;

  if (!horizontal) {
    int oldvalue = x;
    x = y;
    y = oldvalue;
    oldvalue = w;
    w = h;
    h = oldvalue;
  }

  if (event->ButtonDown() && !scrolling) {
    if (x < arrow_w || x > w - arrow_w)
      trackState = ss_CLICK;
    else if (event->MiddleDown()) {
      thumb_drag_offset = (thumb_w / 2);
      trackState = ss_THUMB;
    } else {
      if (x < thumb_x)
	trackState = ss_PAGE_UP;
      else if (x > thumb_x + thumb_w)
	trackState = ss_PAGE_DOWN;
      else {
	thumb_drag_offset = x - thumb_x;
	trackState = ss_THUMB;
      }
    }

    if (SS_USE_TIMER(trackState))
      (new SSTimer(x, this))->Start(SS_TIMER_FIRST_DELAY, TRUE);
    scrolling = TRUE;
  } else if (event->Dragging()) {
    if (SS_USE_TIMER(trackState))
      return;
  } else if (event->ButtonUp()) {
    trackState = 0;
    scrolling = FALSE;
  } else
    return;

  /* In case mouse-up gets lost: */
  if (scrolling && !event->leftDown && !event->rightDown && !event->middleDown) {
    trackState = 0;
    scrolling = FALSE;
  }

  if (trackState)
    Click(x);
}

#endif
