/*
 * File:        wx_mpbd.cc
 * Purpose:     wxMediaPasteboard implementation
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 1995, Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 */

#include "wx_dialg.h"
#ifndef OLD_WXWINDOWS
# include "wx_cmdlg.h"
#endif
#include "wx_utils.h"
#include "wx_media.h"
#include "wx_gcrct.h"
#include "wx_ptreq.h"
#include <string.h>

extern "C" int scheme_directory_exists(char *dirname);

#define LINE_HEIGHT 16

#define DOT_WIDTH 5
#define HALF_DOT_WIDTH 2

static wxCursor *arrow = NULL;

int wxmeGetDoubleClickThreshold();
  
static wxSnipLocation *DoXSnipLoc(wxList *snipLocationList, wxSnip *s)
{
  /* FIXME: key should be address instead of long */
  wxNode *n;
  n = snipLocationList->FindPtr(s);
  if (n)
    return (wxSnipLocation *)n->Data();
  else
    return NULL;
}

#define XSnipLoc(snip) DoXSnipLoc(snipLocationList, snip)
#ifdef MZ_PRECISE_GC
# define SnipLoc(snip) XSnipLoc(snip)
# else
# define SnipLoc(snip) ((wxSnipLocation *)snipLocationList->FindPtr(snip)->Data())
#endif

inline Bool Inbox(float lx, float x)
{ 
  return ((lx - HALF_DOT_WIDTH <= x)
	  && (lx - HALF_DOT_WIDTH + DOT_WIDTH >= x));
}

class wxSnipLocation : public wxObject
{
 public:
  float x, y, w, h, r, b, hm, vm;
  float startx, starty;
  Bool selected, needResize;
  wxSnip *snip;

  void Resize(wxDC *dc);
};

static wxBrush *blackBrush = NULL, *whiteBrush = NULL, *rbBrush = NULL;
static wxPen *invisiPen = NULL, *rbPen = NULL;

static wxMediaPasteboard *skipBox = NULL;

#ifdef wx_mac
extern void wxMediaSetFileCreatorType(char *file, Bool is_binary);
#endif

/**********************************************************************/

wxMediaPasteboard::wxMediaPasteboard()
{
  wxList *sll;

  sizeCacheInvalid = TRUE;
  updateNonemtpy = FALSE;
  writeLocked = 0;

  snips = lastSnip = NULL;
  sll = new wxList(wxKEY_INTEGER);
  snipLocationList = sll;
  snipLocationList->DeleteContents(TRUE);

  sequence = 0;

#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_MEDIA_PASTEBOARD;
#endif
  bufferType = wxPASTEBOARD_BUFFER;

  totalWidth = totalHeight = realWidth = realHeight = 0;

  dragable = TRUE;
  selectionVisible = TRUE;

  sequenceStreak = FALSE;

  dragging = rubberband = FALSE;

  if (!blackBrush) {
    wxREGGLOB(skipBox);
    wxREGGLOB(blackBrush);
    wxREGGLOB(whiteBrush);
    wxREGGLOB(invisiPen);
    wxREGGLOB(rbBrush);
    wxREGGLOB(rbPen);
    blackBrush = wxTheBrushList->FindOrCreateBrush("BLACK", wxXOR);
    whiteBrush = wxTheBrushList->FindOrCreateBrush("WHITE", wxSOLID);
    invisiPen = wxThePenList->FindOrCreatePen("BLACK", 1, wxTRANSPARENT);
    rbBrush = wxTheBrushList->FindOrCreateBrush("BLACK", wxTRANSPARENT);
    rbPen = wxThePenList->FindOrCreatePen("BLACK", 1, wxXOR_DOT);
  }

  {
    wxStandardSnipAdmin *ssa;
    ssa = new wxStandardSnipAdmin(this);
    snipAdmin = ssa;
  }

  needResize = FALSE;

  keepSize = FALSE;

  scrollStep = LINE_HEIGHT;

  maxWidth = minWidth = minHeight = maxHeight = 0.0;
}

wxMediaPasteboard::~wxMediaPasteboard()
{
  wxSnip *snip, *next;
  for (snip = snips; snip; snip = next) {
    next = snip->next;
    DELETE_OBJ snip;
  }

  DELETE_OBJ snipLocationList;

  DELETE_OBJ snipAdmin;
}

void wxMediaPasteboard::RubberBand(float x, float y, float w, float h)
{
  wxPen *oldPen;
  wxBrush *oldBrush;
  wxDC *dc;
  float vx, vy, vw, vh, b, r, dx, dy;

  if (!admin)
    return;

  if (!w & !h)
    return;

  if (w < 0) {
    x += w;
    w = -w;
  }
  r = x + w;
  if (h < 0) {
    y += h;
    h = -h;
  }
  b = y + h;

  admin->GetView(&vx, &vy, &vw, &vh);

  if (x < vx)
    x = vx;
  if (y < vy)
    y = vy;
  if (r > vx + vw)
    r = vx + vw;
  if (b > vy + vh)
    b = vy + vh;

  if (x >= r || y >= b)
    return;

  dc = admin->GetDC(&dx, &dy);
  
  oldPen = dc->GetPen();
  oldBrush = dc->GetBrush();
  dc->SetPen(rbPen);
  dc->SetBrush(rbBrush);
  
  dc->DrawRectangle(x - dx, y - dy, 
		    r - x + GC_RECT_BRUSH_EXTEND, 
		    b - y + GC_RECT_BRUSH_EXTEND);
  
  dc->SetPen(oldPen);
  dc->SetBrush(oldBrush);;
}

wxCursor *wxMediaPasteboard::AdjustCursor(wxMouseEvent *event)
{
  float scrollx, scrolly;
  float x, y;
  wxSnip *snip;
  wxDC *dc;
  wxCursor *c;

  if (!admin)
    return NULL;

  dc = admin->GetDC(&scrollx, &scrolly);
  if (!dc)
    return NULL;

  x = event->x + scrollx;
  y = event->y + scrolly;

  if (!customCursorOverrides) {

    if (caretSnip && event->Dragging()) {
      float x, y;
      GetSnipLocation(caretSnip, &x, &y);
      c = caretSnip->AdjustCursor(dc, x - scrollx, y - scrolly, x, y, event);
      if (c)
	return c;
    }
    
    snip = FindSnip(x, y);
    
    if (snip && (snip == caretSnip)) {
      float x, y;
      GetSnipLocation(caretSnip, &x, &y);
      c = snip->AdjustCursor(dc, x - scrollx, y - scrolly, x, y, event);
      if (c)
	return c;
    }
  }
  
  if (customCursor)
    return customCursor;

  if (!arrow) {
    wxREGGLOB(arrow);
    arrow = new wxCursor(wxCURSOR_ARROW);
  }

  return arrow;
}

void wxMediaPasteboard::OnEvent(wxMouseEvent *event)
{
  float x, y, scrollx, scrolly;
  wxSnip *snip;
  wxSnipLocation *loc;
  wxDC *dc;

  if (!admin)
    return;

  if (event->ButtonDown() || caretSnip) {
    /* First, find clicked-on snip: */
    x = event->x;
    y = event->y;
    
    dc = admin->GetDC(&scrollx, &scrolly);
    y += scrolly;
    x += scrollx;
  } else {
    x = y = 0;
    dc = NULL;
  }

  if (event->ButtonDown())
    snip = FindSnip(x, y);
  else
    snip = caretSnip;

  if (caretSnip && PTREQ(snip, caretSnip)) {
    loc = SnipLoc(caretSnip);
    caretSnip->OnEvent(dc, loc->x - scrollx, loc->y - scrolly, loc->x, loc->y, event);
    return;
  }

  OnLocalEvent(event);
}

void wxMediaPasteboard::OnDefaultEvent(wxMouseEvent *event)
{
  float x, y, scrollx, scrolly;
  wxSnip *snip;
  wxSnipLocation *loc;
  wxDC *dc;
  Bool click;

  if (!admin)
    return;

  /* First, find clicked-on snip: */
  x = event->x;
  y = event->y;
  
  dc = admin->GetDC(&scrollx, &scrolly);
  y += scrolly;
  x += scrollx;

  InteractiveAdjustMouse(&x, &y);

  if (event->ButtonDown() 
      || (event->Moving() && !event->Dragging())
      || event->ButtonUp()) {
    Bool update = FALSE;

    keepSize = FALSE;
    if (dragging) {
      if (resizing) {
	BeginEditSequence();
	/* Move & resize back without Undo */
	if (sizedxm < 0 || sizedym < 0)
	  MoveTo(resizing, origX, origY);
	Resize(resizing, origW, origH);
	dragging = FALSE;
	/* Re-move and re-size with undo: */
	DoEventResize(lastX, lastY);
	AfterInteractiveResize(resizing);
	EndEditSequence();
	resizing = NULL;
      } else {
	FinishDragging(event);
      }
    }
    if (rubberband) {
      rubberband = FALSE;
      RubberBand(startX, startY, lastX - startX, lastY - startY);
      AddSelected(startX, startY, lastX - startX, lastY - startY);
      update = TRUE;
    }

    if (update) {
      UpdateAll();
    }
  }

  click = FALSE;
  if (event->ButtonDown())
    click = TRUE;
  if (event->Dragging() && !dragging && !rubberband)
    click = TRUE;

  if (click) {
    snip = FindSnip(x, y);

    if (dragable) {
      if (snip) {
	loc = SnipLoc(snip);
	origX = loc->x;
	origY = loc->y;
	origW = loc->w;
	origH = loc->h;
	if (!loc->selected) {
	  if (!event->shiftDown)
	    NoSelected();
	  SetCaretOwner(NULL);
	  AddSelected(snip);
	  InitDragging(event);
	} else {
	  long interval;

	  interval = event->timeStamp - lastTime;
	  if (interval < 0)
	    interval = -interval;
	  if (event->ButtonDown() && (interval < (map ? map->GetDoubleClickInterval() : wxmeGetDoubleClickThreshold())))
	    OnDoubleClick(snip, event);
	  else {
	    if (FindDot(loc, x, y, &sizedxm, &sizedym))
	      resizing = snip;
	    InitDragging(event);
	  }
	}
        if (event->ButtonDown())
	  lastTime = event->timeStamp;
      } else {
	if (!event->shiftDown)
	  NoSelected();
	SetCaretOwner(NULL);
	rubberband = TRUE;
      }
      startX = lastX = x;
      startY = lastY = y;
    } else
      SetCaretOwner(snip);

    return;
  }

  if (dragable) {
    if (event->Dragging()) {
      if (rubberband) {
	/* Erase old */
	RubberBand(startX, startY, lastX - startX, lastY - startY);
	/* Draw new: */
	RubberBand(startX, startY, x - startX, y - startY);
      } else {
	if (resizing)
	  DoEventResize(x, y);
	else {
	  DoEventMove(x, y);
	}
      }
      lastX = x;
      lastY = y;
    }
  }
}

void wxMediaPasteboard::OnDoubleClick(wxSnip *snip, wxMouseEvent *)
{
  if (snip->flags & wxSNIP_HANDLES_EVENTS) {
    NoSelected();
    SetCaretOwner(snip);
  }
}

void wxMediaPasteboard::OnChar(wxKeyEvent *event)
{
  float x, y, scrollx, scrolly;
  wxSnipLocation *loc;
  wxDC *dc;

  if (!admin)
    return;
  
  x = event->x;
  y = event->y;

  dc = admin->GetDC(&scrollx, &scrolly);
  y += scrolly;
  x += scrollx;

  if (caretSnip) {
    loc = SnipLoc(caretSnip);
    caretSnip->OnChar(dc, loc->x, loc->y, x - scrollx, y - scrolly, event);
    return;
  }

  OnLocalChar(event);
}

void wxMediaPasteboard::OnDefaultChar(wxKeyEvent *event)
{
  long code;

  if (!admin)
    return;

  code = event->KeyCode();

  switch(code) {
    case WXK_BACK:
    case WXK_DELETE:
      Delete();
      break;
    case WXK_RIGHT:
      Move(1, 0);
      break;
    case WXK_LEFT:
      Move(-1, 0);
      break;
    case WXK_UP:
      Move(0, -1);
      break;
    case WXK_DOWN:
      Move(0, 1);
      break;
    }
}

void wxMediaPasteboard::InitDragging(wxMouseEvent *e)
{
  wxSnip *s = NULL;

  if (resizing) {
    if (!CanInteractiveResize(resizing)) {
      resizing = NULL;
      return;
    }
    OnInteractiveResize(resizing);
  } else {
    if (!CanInteractiveMove(e))
      return;
    OnInteractiveMove(e);
  }

  dragging = TRUE;
  keepSize = TRUE;

  while ((s = FindNextSelectedSnip(s))) {
    wxSnipLocation *loc;
    loc = SnipLoc(s);
    loc->startx = loc->x;
    loc->starty = loc->y;
  }
}

void wxMediaPasteboard::FinishDragging(wxMouseEvent *e)
{
  wxSnip *s = NULL;

  BeginEditSequence();
  /* Move back without Undo and remember final */
  while ((s = FindNextSelectedSnip(s))) {
    float x, y;
    wxSnipLocation *loc;
    loc = SnipLoc(s);
    x = loc->startx;
    y = loc->starty;
    loc->startx = loc->x;
    loc->starty = loc->y;
    MoveTo(s, x, y);
  }
  dragging = FALSE;
  /* Move to final position with undo: */
  s = NULL;
  while ((s = FindNextSelectedSnip(s))) {
    wxSnipLocation *loc;
    loc = SnipLoc(s);
    MoveTo(s, loc->startx, loc->starty);
  }

  AfterInteractiveMove(e);

  EndEditSequence();
}

void wxMediaPasteboard::DoEventMove(float eventX, float eventY)
{
  wxSnip *s = NULL;
  float dx, dy;
  
  dx = eventX - startX;
  dy = eventY - startY;

  BeginEditSequence();

  while ((s = FindNextSelectedSnip(s))) {
    wxSnipLocation *loc;
    float x, y;

    loc = SnipLoc(s);
    x = loc->startx + dx;
    y = loc->starty + dy;
    InteractiveAdjustMove(s, &x, &y);
    MoveTo(s, x, y);
  }

  EndEditSequence();
}

void wxMediaPasteboard::DoEventResize(float eventX, float eventY)
{
  float Dx, Dy, w, h, x, y;
  
  Dx = eventX - startX;
  Dy = eventY - startY;

  w = origW + Dx * sizedxm;
  h = origH + Dy * sizedym;
  
  if (w < 0)
    w = 0;
  if (h < 0)
    h = 0;

  InteractiveAdjustResize(resizing, &w, &h);
  
  if (w < 0)
    w = 0;
  if (h < 0)
    h = 0;
  
  x = origX;
  if (sizedxm < 0) {
    x += (origW - w);
  }
  y = origY;
  if (sizedym < 0) {
    y += (origH - h);
  }
  
  BeginEditSequence();
  
  if (Resize(resizing, w, h)) {
    if ((sizedxm < 0 || sizedym < 0))
      MoveTo(resizing, x, y);
  }
  
  EndEditSequence();
}

void wxMediaPasteboard::InteractiveAdjustMouse(float *x, float *y)
{
  if (*x < 0)
    *x = 0;
  if (*y < 0)
    *y = 0;
}

void wxMediaPasteboard::InteractiveAdjustResize(wxSnip *, float *, float *)
{
  /* Do nothing */
}

void wxMediaPasteboard::InteractiveAdjustMove(wxSnip *, float *x, float *y)
{
  if (*x < 0)
    *x = 0;
  if (*y < 0)
    *y = 0;
}


/***************************************************************************/

void wxMediaPasteboard::SetSelected(wxSnip *snip)
{
  BeginEditSequence();
  NoSelected();
  AddSelected(snip);
  EndEditSequence();
}

void wxMediaPasteboard::DoSelect(wxSnip *snip, Bool on)
{
  wxSnipLocation *loc;

  if ((loc = XSnipLoc(snip))) {
    if (loc->selected != on) {
      writeLocked++;
      if (CanSelect(snip, on)) {
	OnSelect(snip, on);
	--writeLocked;
	loc->selected = on;
	AfterSelect(snip, on);
	UpdateLocation(loc);
      } else
	--writeLocked;
    }
  }
}

void wxMediaPasteboard::AddSelected(wxSnip *snip)
{
  DoSelect(snip, TRUE);
}

void wxMediaPasteboard::RemoveSelected(wxSnip *snip)
{
  DoSelect(snip, FALSE);
}

void wxMediaPasteboard::AddSelected(float x, float y, float w, float h)
{
  wxSnip *s;
  float r, b;

  if (w < 0) {
    x += w;
    w = -w;
  }
  if (h < 0) {
    y += h;
    h = -h;
  }

  r = x + w;
  b = y + h;

  BeginEditSequence();

  for (s = snips; s; s = s->next) {
    wxSnipLocation *loc;
    loc = SnipLoc(s);
    if (loc
	&& !loc->selected
	&& (loc->x <= r)
	&& (loc->y <= b)
	&& (loc->r >= x)
	&& (loc->b >= y)) {
      AddSelected(s);
    }
  }
  
  EndEditSequence();
}
				  
void wxMediaPasteboard::SelectAll(void)
{
  wxSnip *s;

  BeginEditSequence();

  for (s = snips; s; s = s->next) {
    AddSelected(s);
  }
  
  EndEditSequence();
}
				  
void wxMediaPasteboard::NoSelected()
{
  wxSnip *s;

  BeginEditSequence();

  for (s = snips; s; s = s->next) {
    RemoveSelected(s);
  }
  
  EndEditSequence();
}

void wxMediaPasteboard::Insert(wxSnip *snip, wxSnip *before, float x, float y)
{
  wxSnipLocation *loc;
  wxSnip *search;

  if (userLocked || writeLocked)
    return;

  if (snip->IsOwned())
    return;

  if (!snip->snipclass)
    wxmeError("Inserting a snip without a class."
	      " Data will be lost if you try to save the file.");

  writeLocked++;
  BeginEditSequence();
  if (!CanInsert(snip, before, x, y)) {
    EndEditSequence();
    --writeLocked;
    return;
  }
  OnInsert(snip, before, x, y);
  --writeLocked;

  if (snip->IsOwned()) {
    /* Disaster: Can/OnInsert made the snip owned. */
    snip = new wxImageSnip();
  }

  for (search = snips; search && (search != before); search = search->next) {
  }
  
  snip->next = search;
  if (snip->next) {
    snip->prev = search->prev;
    snip->next->prev = snip;
  } else {
    snip->prev = lastSnip;
    lastSnip = snip;
  }
  if (snip->prev)
    snip->prev->next = snip;
  else
    snips = snip;

  loc = new wxSnipLocation;
  loc->x = x;
  loc->y = y;
  loc->snip = snip;
  loc->needResize = TRUE;
  loc->selected = FALSE;
  snipLocationList->Append((void *)snip, loc);

  snip->style = styleList->Convert(snip->style);
  if (PTREQ(snip->style, styleList->BasicStyle())) {
    wxStyle *s;
    s = styleList->FindNamedStyle(STD_STYLE);
    if (s)
      snip->style = s;
  }

  snip->SizeCacheInvalid();

  SnipSetAdmin(snip, snipAdmin);

  if (!noundomode) {
    wxInsertSnipRecord *is;
    is = new wxInsertSnipRecord(snip, sequenceStreak);
    AddUndo(is);
  }
  if (sequence)
    sequenceStreak = TRUE;

  changed = TRUE;

  if (!modified)
    SetModified(TRUE);

  AfterInsert(snip, before, x, y);

  needResize = TRUE;
  UpdateLocation(loc);

  writeLocked++;
  EndEditSequence();
  --writeLocked;

  if (!sequence)
    UpdateNeeded();
}

void wxMediaPasteboard::Insert(wxSnip *snip, float x, float y)
{
  Insert(snip, snips, x, y);
}

void wxMediaPasteboard::Insert(wxSnip *snip)
{
  float x, y;

  GetCenter(&x, &y);
  Insert(snip, x, y);
}

void wxMediaPasteboard::Insert(wxSnip *snip, wxSnip *before)
{
  float x, y;

  GetCenter(&x, &y);
  Insert(snip, before, x, y);
}

void wxMediaPasteboard::Delete()
{
  wxNode *node;
  wxSnipLocation *loc;
  wxDeleteSnipRecord *del;

  if (userLocked || writeLocked)
    return;

  del = new wxDeleteSnipRecord(sequenceStreak);
  if (sequence)
    sequenceStreak = TRUE;

  BeginEditSequence();

  for (node = snipLocationList->First(); node; node = node->Next()) {
    loc = (wxSnipLocation *)node->Data();
    if (loc->selected) 
      _Delete(loc->snip, del);
  }

  if (!noundomode)
    AddUndo(del);

  EndEditSequence();
}

void wxMediaPasteboard::Erase()
{
  wxSnip *snip, *next;
  wxDeleteSnipRecord *del;

  if (userLocked || writeLocked)
    return;

  del = new wxDeleteSnipRecord(sequenceStreak);
  if (sequence)
    sequenceStreak = TRUE;

  BeginEditSequence();
  for (snip = snips; snip; snip = next) {
    next = snip->next;
    _Delete(snip, del);
  }

  if (!noundomode)
    AddUndo(del);

  EndEditSequence();
}

void wxMediaPasteboard::_Delete(wxSnip *del_snip,
				wxDeleteSnipRecord *del)
{
  wxSnip *snip;
  wxNode *node;
  wxSnipLocation *loc;
  Bool updateCursor = FALSE;

  for (snip = snips; snip; snip = snip->next) {
    if (PTREQ(snip, del_snip)) {
      writeLocked++;
      BeginEditSequence();
      if (!CanDelete(del_snip)) {
	EndEditSequence();
	--writeLocked;
	return;
      }
      OnDelete(del_snip);
      --writeLocked;

      if (del_snip == caretSnip) {
	caretSnip->OwnCaret(FALSE);
	caretSnip = NULL;
	updateCursor = TRUE;
      }

      UpdateSnip(del_snip);

      if (!snip->prev)
	snips = snip->next;
      else
	snip->prev->next = snip->next;
      if (!snip->next)
	lastSnip = snip->prev;
      else
	snip->next->prev = snip->prev;

      node = snipLocationList->FindPtr(snip);
      snipLocationList->DeleteNode(node);
      loc = (wxSnipLocation *)node->Data();
      if (del)
	del->InsertSnip(snip, snip->next, loc->x, loc->y);
      snip->next = snip->prev = NULL;

      snip->flags += wxSNIP_CAN_DISOWN;
      SnipSetAdmin(snip, NULL);
      snip->flags -= wxSNIP_CAN_DISOWN;

      if (!modified)
	SetModified(TRUE);

      AfterDelete(del_snip);
      changed = TRUE;

      writeLocked++;
      EndEditSequence();
      --writeLocked;

      if (!sequence)
	UpdateNeeded();
    }
  }  

  if (updateCursor)
    if (admin)
      admin->UpdateCursor();
}

void wxMediaPasteboard::Delete(wxSnip *del_snip)
{
  wxDeleteSnipRecord *del;

  if (userLocked || writeLocked)
    return;

  del = new wxDeleteSnipRecord(sequenceStreak);
  if (sequence)
    sequenceStreak = TRUE;

  _Delete(del_snip, del);

  if (!noundomode)
    AddUndo(del);
}

void wxMediaPasteboard::Remove(wxSnip *del_snip)
{
  if (userLocked || writeLocked)
    return;

  _Delete(del_snip, NULL);
}

void wxMediaPasteboard::MoveTo(wxSnip *snip, float x, float y)
{
  wxNode *node;
  wxSnipLocation *loc;
  wxMoveSnipRecord *rec;

  if (userLocked || writeLocked)
    return;

  if ((node = snipLocationList->FindPtr(snip))) {
    loc = (wxSnipLocation *)node->Data();

    if ((loc->x == x) && (loc->y == y))
      return;

    writeLocked++;
    BeginEditSequence();
    if (!CanMoveTo(snip, x, y, dragging)) {
      EndEditSequence();
      --writeLocked;
      return;
    }
    OnMoveTo(snip, x, y, dragging);
    --writeLocked;

    UpdateLocation(loc);

    if (!dragging) {
      rec = new wxMoveSnipRecord(loc->snip, loc->x, loc->y, 
				 FALSE, sequenceStreak);
      if (sequence)
	sequenceStreak = TRUE;
      if (!noundomode)
	AddUndo(rec);
    }

    loc->x = x;
    loc->y = y;
    loc->r = x + loc->w;
    loc->b = y + loc->h;
    loc->hm = x + loc->w/2;
    loc->vm = y + loc->h/2;
    UpdateLocation(loc);

    if (!dragging && !modified)
	SetModified(TRUE);

    AfterMoveTo(snip, x, y, dragging);

    needResize = TRUE;

    writeLocked++;
    EndEditSequence();
    --writeLocked;

    changed = TRUE;

    if (!sequence)
      UpdateNeeded();
  }
}

void wxMediaPasteboard::Move(wxSnip *snip, float dx, float dy)
{
  wxNode *node;
  wxSnipLocation *loc;

  if (userLocked || writeLocked)
    return;

  if ((node = snipLocationList->FindPtr(snip))) {
    loc = (wxSnipLocation *)node->Data();
    MoveTo(snip, loc->x + dx, loc->y + dy);
  }
}

void wxMediaPasteboard::Move(float dx, float dy)
{
  wxNode *node;
  wxSnipLocation *loc;

  if (userLocked || writeLocked)
    return;

  BeginEditSequence();

  for (node = snipLocationList->First(); node; node = node->Next()) {
    loc = (wxSnipLocation *)node->Data();
    if (loc->selected)
      Move(loc->snip, dx, dy);
  }

  EndEditSequence();
}

Bool wxMediaPasteboard::Resize(wxSnip *snip, float w, float h)
{
  wxNode *node;
  wxSnipLocation *loc;
  float oldw, oldh;
  Bool rv;
  
  if (!admin)
    return FALSE;

  if (!(node = snipLocationList->FindPtr(snip)))
    return FALSE;

  loc = (wxSnipLocation *)node->Data();
  oldw = loc->w;
  oldh = loc->h;

  writeLocked++;
  BeginEditSequence();
  if (!CanResize(snip, w, h)) {
    EndEditSequence();
    --writeLocked;
    return FALSE;
  }
  OnResize(snip, w, h);
  --writeLocked;

  if (!snip->Resize(w, h))
    rv = FALSE;
  else {
    if (!dragging) {
      if (!noundomode) {
	wxResizeSnipRecord *rs;
	rs = new wxResizeSnipRecord(snip, oldw, oldh, sequenceStreak);
	AddUndo(rs);
      }
      if (sequence)
	sequenceStreak = TRUE;
    }
    rv = TRUE;
  }

  if (rv && !dragging && !modified)
    SetModified(TRUE);

  AfterResize(snip, w, h, rv);

  writeLocked++;
  EndEditSequence();
  --writeLocked;

  changed = TRUE;

  if (!sequence)
    UpdateNeeded();

  return rv;
}

void wxMediaPasteboard::ChangeStyle(wxStyleDelta *delta)
{ 
  ChangeStyle(delta, NULL);
}

void wxMediaPasteboard::ChangeStyle(wxStyle *style, wxSnip *snip)
{
  _ChangeStyle(style, NULL, snip);
}

void wxMediaPasteboard::ChangeStyle(wxStyleDelta *delta, wxSnip *snip)
{
  _ChangeStyle(NULL, delta, snip);
}
 
void wxMediaPasteboard::_ChangeStyle(wxStyle *style, wxStyleDelta *delta, 
				     wxSnip *snip)
{
  wxNode *node;
  wxSnipLocation *loc;
  wxStyleChangeSnipRecord *rec;
  Bool didit = FALSE;

  if (userLocked || writeLocked)
    return;

  rec = new wxStyleChangeSnipRecord(sequenceStreak);
  if (sequence)
    sequenceStreak = TRUE;

  if (!style && !delta) {
    style = styleList->FindNamedStyle(STD_STYLE);
    if (!style)
      style = styleList->BasicStyle();
  }

  BeginEditSequence();

  if (snip) {
    rec->AddStyleChange(snip, snip->style);
    if (style)
      snip->style = style;
    else {
      snip->style = styleList->FindOrCreateStyle(snip->style, delta);
    }
    snip->SizeCacheInvalid();
    UpdateSnip(snip);
    didit = TRUE;
  } else {
    for (node = snipLocationList->First(); node; node = node->Next()) {
      loc = (wxSnipLocation *)node->Data();
      if (loc->selected) {
	rec->AddStyleChange(loc->snip, loc->snip->style);
	if (style)
	  loc->snip->style = style;
	else {
	  loc->snip->style = styleList->FindOrCreateStyle(loc->snip->style, 
							  delta);
	}
	loc->snip->SizeCacheInvalid();
	loc->needResize = TRUE;
	needResize = TRUE;
	UpdateLocation(loc);
	didit = TRUE;
      }
    }
  }
  
  if (didit) {
    if (!noundomode)
      AddUndo(rec);

    changed = TRUE;
    if (!modified)
      SetModified(TRUE);
  }

  EndEditSequence();
}

void wxMediaPasteboard::Raise(wxSnip *snip)
{
  wxSnip *prev;

  if (userLocked || writeLocked)
    return;

  if (!snipLocationList->FindPtr(snip))
    return;

  prev = snip->prev;
  if (prev) {
    prev->next = snip->next;
    if (!snip->next)
      lastSnip = prev;
    else
      snip->next->prev = prev;
    snip->prev = prev->prev;
    prev->prev = snip;
    if (!snip->prev)
      snips = snip;
    else
      snip->prev->next = snip;

    changed = TRUE;
    if (!modified)
      SetModified(TRUE);

    UpdateSnip(snip);
  }
}

void wxMediaPasteboard::Lower(wxSnip *snip)
{
  wxSnip *next;

  if (userLocked || writeLocked)
    return;

  if (!snipLocationList->FindPtr(snip))
    return;
  
  next = snip->next;
  if (next) {
    next->prev = snip->prev;
    if (!snip->prev)
      snips = next;
    else
      snip->prev->next = next;
    snip->next = next->next;
    next->next = snip;
    if (!snip->next)
      lastSnip = snip;
    else
      snip->next->prev = snip;

    changed = TRUE;
    if (!modified)
      SetModified(TRUE);

    UpdateSnip(snip);
  }
}

void wxMediaPasteboard::SetBefore(wxSnip *snip, wxSnip *before)
{
  if (userLocked || writeLocked)
    return;

  if (!before)
    before = snips;

  if (!snipLocationList->FindPtr(snip)
      || !snipLocationList->FindPtr(before))
    return;

  if (snip == before)
    return;
  
  /* Remove snip from current pos: */
  if (snip->prev)
    snip->prev->next = snip->next;
  else
    snips = snip->next;
  if (snip->next)
    snip->next->prev = snip->prev;
  else
    lastSnip = snip->prev;

  /* Insert before `before': */
  snip->prev = before->prev;
  snip->next = before;
  before->prev = snip;
  if (snip->prev)
    snip->prev->next = snip;
  else
    snips = snip;

  changed = TRUE;
  if (!modified)
    SetModified(TRUE);

  UpdateSnip(snip);
}

void wxMediaPasteboard::SetAfter(wxSnip *snip, wxSnip *after)
{
  if (userLocked || writeLocked)
    return;

  if (!after)
    after = lastSnip;

  if (!snipLocationList->FindPtr(snip)
      || !snipLocationList->FindPtr(after))
    return;

  if (snip == after)
    return;
  
  /* Remove snip from current pos: */
  if (snip->prev)
    snip->prev->next = snip->next;
  else
    snips = snip->next;
  if (snip->next)
    snip->next->prev = snip->prev;
  else
    lastSnip = snip->prev;

  /* Insert after `after': */
  snip->next = after->next;
  snip->prev = after;
  after->next = snip;
  if (snip->next)
    snip->next->prev = snip;
  else
    lastSnip = snip;

  changed = TRUE;
  if (!modified)
    SetModified(TRUE);

  UpdateSnip(snip);
}

wxSnip *wxMediaPasteboard::SnipSetAdmin(wxSnip *snip, wxSnipAdmin *a)
{
  wxSnipAdmin *orig_admin;
  orig_admin = snip->GetAdmin();

  /* Lock during SetAdmin! */
  snip->SetAdmin(a);

  if (snip->GetAdmin() != a) {
    /* Something went wrong. */
    if (!a && (snip->GetAdmin() == orig_admin)) {
      /* Force admin to NULL. */
      snip->wxSnip::SetAdmin(NULL);
    } else if (a) {
      /* Snip didn't accept membership into this buffer. Give up on it. */
      wxSnip *naya;
      naya = new wxSnip();
      naya->prev = snip->prev;
      naya->next = snip->next;
      if (naya->prev)
	naya->prev->next = naya;
      else
	snips = naya;
      if (naya->next)
	naya->next->prev = naya;
      else
	lastSnip = naya;

      snip->wxSnip::SetAdmin(NULL);

      naya->SetAdmin(a);
      snip = naya;
    }
  }

  return snip;
}

/***************************************************************************/

Bool wxMediaPasteboard::FindDot(wxSnipLocation *loc, float x, float y,
				float *dxm, float *dym)
{
  if (Inbox(loc->x, x)) {
    *dxm = -1;
    if (Inbox(loc->y, y))
      *dym = -1;
    else if (Inbox(loc->vm, y))
      *dym = 0;
    else if (Inbox(loc->b, y))
      *dym = 1;
    else
      return FALSE;
  } else if (Inbox(loc->hm, x)) {
    *dxm = 0;
    if (Inbox(loc->y, y))
      *dym = -1;
    else if (Inbox(loc->b, y))
      *dym = 1;
    else
      return FALSE;
  } else if (Inbox(loc->r, x)) {
    *dxm = 1;
    if (Inbox(loc->y, y))
      *dym = -1;
    else if (Inbox(loc->vm, y))
      *dym = 0;
    else if (Inbox(loc->b, y))
      *dym = 1;
    else
      return FALSE;
  } else
    return FALSE;

  return TRUE;
}

wxSnip *wxMediaPasteboard::FindSnip(float x, float y)
{
  wxSnip *snip;
  wxSnipLocation *loc;
  float dym, dxm;

  for (snip = snips; snip; snip = snip->next) {
    loc = SnipLoc(snip);
    if (loc->x <= x && loc->y <= y
	&& loc->r >= x && loc->b >= y)
      return snip;
    else if (loc->selected && FindDot(loc, x, y, &dxm, &dym))
      return snip;
  }

  return NULL;
}

wxSnip *wxMediaPasteboard::FindFirstSnip(void)
{
  return snips;
}

Bool wxMediaPasteboard::IsSelected(wxSnip *asnip)
{
  wxSnip *snip;
  wxSnipLocation *loc;

  for (snip = snips; snip; snip = snip->next) {
    if (PTREQ(asnip, snip)) {
      loc = SnipLoc(snip);
      return loc->selected;
    }
  }

  return FALSE;
}

wxSnip *wxMediaPasteboard::FindNextSelectedSnip(wxSnip *start)
{
  wxSnip *snip;
  wxSnipLocation *loc;

  if (!start)
    snip = snips;
  else {
    loc = XSnipLoc(start);
    if (!loc) return NULL; /* Not in this pasteboard */
    snip = start->next;
  }

  for (; snip; snip = snip->next) {
    loc = SnipLoc(snip);
    if (loc->selected)
      return snip;
  }

  return NULL;
}

/***************************************************************************/

void wxMediaPasteboard::Draw(wxDC *dc, float dx, float dy, 
			     float cx, float cy, float cw, float ch, 
			     int show_caret)
{
  wxSnip *snip;
  wxStyle *oldstyle = NULL;
  wxSnipLocation *loc;
  float cr, cb, x, y, r, b, hm, vm, dcx, dcy, dcr, dcb;

  if (!admin)
    return;

  writeLocked++;
  writeLocked = -writeLocked;

  dcx = cx + dx;
  dcy = cy + dy;

  cr = cx + cw;
  cb = cy + ch;

  dcr = dcx + cw;
  dcb = dcy + ch;

  if (skipBox != this) {
    wxPen *savePen;
    wxBrush *saveBrush;

    savePen = dc->GetPen();
    saveBrush = dc->GetBrush();

    dc->SetBrush(whiteBrush);
    dc->SetPen(invisiPen);
    dc->DrawRectangle(dcx, dcy,
		      cw + GC_RECT_BRUSH_EXTEND,
		      ch + GC_RECT_BRUSH_EXTEND);

    dc->SetBrush(saveBrush);
    dc->SetPen(savePen);
  }

  OnPaint(TRUE, dc, cx, cy, cr, cb, dx, dy, 
	  (show_caret && !caretSnip)
	  ? show_caret
	  : (int)wxSNIP_DRAW_NO_CARET);

  for (snip = lastSnip; snip; snip = snip->prev) {
    loc = SnipLoc(snip);

    if ((loc->x <= cr)
	&& (loc->y <= cb)
	&& (loc->r >= cx)
	&& (loc->b >= cy)) {
      snip->style->SwitchTo(dc, oldstyle);
      oldstyle = snip->style;

      x = loc->x + dx;
      y = loc->y + dy;
      
      snip->Draw(dc, x, y, dcx, dcy, dcr, dcb, dx, dy, 
		 PTREQ(snip, caretSnip) 
		 ? (show_caret ? show_caret : (int)wxSNIP_DRAW_NO_CARET)
		 : (int)wxSNIP_DRAW_NO_CARET);

      if ((show_caret == wxSNIP_DRAW_SHOW_CARET)
	  && ownCaret 
	  && selectionVisible
	  && loc->selected) {
	wxBrush *oldbrush;
	wxPen *oldpen;

	oldbrush = dc->GetBrush();
	oldpen = dc->GetPen();
	dc->SetBrush(blackBrush);
	dc->SetPen(invisiPen);

	r = loc->r + dx;
	b = loc->b + dy;
	hm = loc->hm + dx;
	vm = loc->vm + dy;

	dc->DrawRectangle(x - HALF_DOT_WIDTH, 
			  y - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);
	dc->DrawRectangle(hm - HALF_DOT_WIDTH, 
			  y - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);
	dc->DrawRectangle(r - HALF_DOT_WIDTH, 
			  y - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);
	dc->DrawRectangle(r - HALF_DOT_WIDTH, 
			  vm - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);
	dc->DrawRectangle(r - HALF_DOT_WIDTH, 
			  b - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND,
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);
	dc->DrawRectangle(hm - HALF_DOT_WIDTH, 
			  b - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);
	dc->DrawRectangle(x - HALF_DOT_WIDTH, 
			  b - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);
	dc->DrawRectangle(x - HALF_DOT_WIDTH, 
			  vm - HALF_DOT_WIDTH, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND, 
			  DOT_WIDTH + GC_RECT_BRUSH_EXTEND);

	dc->SetPen(oldpen);
	dc->SetBrush(oldbrush);
      }
    }
  }

  {
    wxStyle *bs;
    bs = styleList->BasicStyle();
    bs->SwitchTo(dc, oldstyle);
  }

  OnPaint(FALSE, dc, cx, cy, cr, cb, dx, dy, 
	  (show_caret && !caretSnip)
	  ? show_caret
	  : (int)wxSNIP_DRAW_NO_CARET);

  writeLocked = -writeLocked;
  --writeLocked;
}

void wxMediaPasteboard::Refresh(float localx, float localy, float w, float h, 
				int show_caret)
{
  float dx, dy, ddx, ddy;
  wxDC *dc;

  if (!admin || (writeLocked < 0))
    return;

  if ((h <= 0) || (w <= 0))
    return;

  ReadyOffscreen(w, h);

  dc = admin->GetDC(&dx, &dy);

  if (!offscreenInUse && bitmap && bitmap->Ok() && offscreen->Ok()) {
    /* Need to make sure that difference between coordinates is
       integral; otherwise, roundoff error could affect drawing */
    ddx = (localx - dx) - (long)(localx - dx);
    if (ddx < 0)
      ddx = 1 + ddx;
    localx -= ddx;
    w += ddx;
    ddy = (localy - dy) - (long)(localy - dy);
    if (ddy < 0)
      ddy = 1 + ddy;
    localy -= ddy;
    h += ddy;

#ifndef EACH_BUFFER_OWN_OFFSCREEN
    offscreenInUse = TRUE;
#endif

    Draw(offscreen, -localx, -localy, localx, localy, w, h, show_caret);
    {
      wxBitmap *bm;
      bm = offscreen->GetObject();
      dc->Blit(localx - dx, localy - dy, w, h, bm, 0, 0, wxCOPY);
    }

#ifndef EACH_BUFFER_OWN_OFFSCREEN
    offscreenInUse = FALSE;
    lastUsedOffscreen = this;
#endif
  } else {
    wxPen *pen;
    wxBrush *brush;
    wxFont *font;
    wxColour *fg, *bg;

    pen = dc->GetPen();
    brush = dc->GetBrush();
    font = dc->GetFont();
    {
      wxColour *clr;
      clr = dc->GetTextForeground();
      fg = new wxColour(clr);
      clr = dc->GetTextBackground();
      bg = new wxColour(clr);
    }

#ifndef NO_GET_CLIPPING_REGION
    wxRegion *rgn;
    rgn = dc->GetClippingRegion();
    dc->SetClippingRect(localx - dx, localy - dy, w, h);
#endif

    Draw(dc, -dx, -dy, localx, localy, w, h, show_caret);

#ifndef NO_GET_CLIPPING_REGION
    dc->SetClippingRegion(rgn);
#endif

    dc->SetBrush(brush);
    dc->SetPen(pen);
    dc->SetFont(font);
    dc->SetTextForeground(fg);
    dc->SetTextBackground(bg);
  }
}

void wxMediaPasteboard::CheckRecalc()
{
  float r, b;
  wxDC *dc;
  wxNode *node;
  wxSnipLocation *loc;
  
  if (!admin)
    return;

  dc = admin->GetDC();

  if (!dc)
    return;

  if (needResize) {
    /* Find right & bottom */
    r = b = 0;
    for (node = snipLocationList->First(); node; node = node->Next()) {
      loc = (wxSnipLocation *)node->Data();
      if (sizeCacheInvalid) {
	loc->snip->SizeCacheInvalid();
	loc->needResize = TRUE;
      }
      if (loc->needResize)
	loc->Resize(dc);
      if (loc->r + HALF_DOT_WIDTH > r)
	r = loc->r + HALF_DOT_WIDTH;
      if (loc->b + HALF_DOT_WIDTH > b)
	b = loc->b + HALF_DOT_WIDTH;
    }
  
    realWidth = r;
    realHeight = b;

    if (minWidth && (realWidth < minWidth))
      realWidth = minWidth;
    if (maxWidth && (realWidth > maxWidth))
      realWidth = maxWidth;

    if (minHeight && (realHeight < minHeight))
      realHeight = minHeight;
    if (maxHeight && (realHeight > maxHeight))
      realHeight = maxHeight;

    needResize = FALSE;
  }

  sizeCacheInvalid = FALSE;

  if (!keepSize) {
    if (realWidth != totalWidth || realHeight != totalHeight) {
      totalWidth = realWidth;
      totalHeight = realHeight;
      admin->Resized(FALSE);
    }
  }
}

void wxMediaPasteboard::Update(float x, float y, float w, float h)
{
  float r, b;

  r = x + w;
  b = y + h;

  if (x < 0)
    x = 0;
  if (y < 0)
    y = 0;
  if (r < 0)
    r = 0;
  if (b < 0)
    b = 0;

  if (!updateNonemtpy) {
    updateTop = y;
    updateLeft = x;
    if (h < 0)
      updateBottom = h;
    else
      updateBottom = b;
    if (w < 0)
      updateRight = w;
    else
      updateRight = r;
    updateNonemtpy = TRUE;
  } else {
    if (y < updateTop)
      updateTop = y;

    if (x < updateLeft)
      updateLeft = x;

    if ((h < 0) && (updateBottom > 0))
      updateBottom = -updateBottom;
    if (updateBottom < 0) {
      if (h < 0 && h < updateBottom)
	updateBottom = h;
      else if (h > 0 && (-b) < updateBottom)
	updateBottom = -b;
    } else if (b > updateBottom)
      updateBottom = b;

    if ((w < 0) && (updateRight > 0))
      updateRight = -updateRight;
    if (updateRight < 0) {
      if (w < 0 && w < updateRight)
	updateRight = w;
      else if (h > 0 && (-r) < updateRight)
	updateRight = -r;
    } else if (r > updateRight)
      updateRight = r;
  }

  if (sequence || !admin)
    return;

  CheckRecalc();

  if (updateBottom < 0) {
    updateBottom = -updateBottom;
    if (updateBottom < realHeight)
      updateBottom = realHeight;
  }
  if (updateRight < 0) {
    updateRight = -updateRight;
    if (updateRight < realWidth)
      updateRight = realWidth;
  }

  updateNonemtpy = FALSE;

  if (changed) {
    changed = FALSE;
    writeLocked++;
    OnChange();
    --writeLocked;
  }

  if (updateTop != updateBottom || updateLeft != updateRight) {
    /* Bizarre MSVC bug: if we inline w & h, h is wrong */
    float w = updateRight - updateLeft + 1;
    float h = updateBottom - updateTop + 1;

    admin->NeedsUpdate(updateLeft, updateTop, w, h);
  }
}

void wxMediaPasteboard::UpdateLocation(wxSnipLocation *loc)
{
  if (admin) {
    if (loc->needResize) {
      wxDC *dc;
      dc = admin->GetDC();
      if (dc)
	loc->Resize(dc);
      /* otherwise, still need resize... */
    }
    Update(loc->x - HALF_DOT_WIDTH, loc->y - HALF_DOT_WIDTH, 
	   loc->w + DOT_WIDTH, loc->h + DOT_WIDTH);
  }
}

void wxMediaPasteboard::UpdateSnip(wxSnip *snip)
{
  wxNode *node;
  wxSnipLocation *loc;

  if ((node = snipLocationList->FindPtr(snip))) {
    loc = (wxSnipLocation *)node->Data();
    UpdateLocation(loc);
  }
}

void wxMediaPasteboard::UpdateSelected()
{
  wxNode *node;
  wxSnipLocation *loc;

  BeginEditSequence();

  for (node = snipLocationList->First(); node; node = node->Next()) {
    loc = (wxSnipLocation *)node->Data();
    if (loc->selected)
      UpdateLocation(loc);
  }
  
  EndEditSequence();
}

void wxMediaPasteboard::UpdateAll()
{
  Update(0, 0, -1, -1);
}

void wxMediaPasteboard::UpdateNeeded()
{
  if (updateNonemtpy)
    Update(updateLeft, updateTop, 0, 0);
}

void wxMediaPasteboard::InvalidateBitmapCache(float x, float y, float w, float h)
{
  Update(x, y, w, h);
}

/***************************************************************************/

void wxMediaPasteboard::OwnCaret(Bool ownit)
{
  if (DoOwnCaret(ownit)) {
    UpdateSelected();
    OnFocus(ownit);
  }
}

void wxMediaPasteboard::BlinkCaret()
{
  if (caretSnip) {
    wxDC *dc;
    float dx, dy;
    if ((dc = admin->GetDC(&dx, &dy))) {
      float x, y;
      if (GetSnipLocation(caretSnip, &x, &y))
	caretSnip->BlinkCaret(dc, x - dx, y - dy);
    }
  }
}

void wxMediaPasteboard::SizeCacheInvalid(void)
{
  sizeCacheInvalid = TRUE;
  needResize = TRUE;
}


void wxMediaPasteboard::GetExtent(float *w, float *h)
{
  CheckRecalc();

  if (w)
    *w = totalWidth;
  if (h)
    *h = totalHeight;
}

Bool wxMediaPasteboard::ScrollTo(wxSnip *snip, 
				 float WXUNUSED(localx), float WXUNUSED(localy), 
				 float WXUNUSED(w), float WXUNUSED(h), 
				 Bool refresh, int WXUNUSED(bias))
{
  if (refresh)
    UpdateSnip(snip);
  return FALSE;
}

void wxMediaPasteboard::SetCaretOwner(wxSnip *snip, int dist)
{
  if (DoSetCaretOwner(snip, dist)) {
    UpdateAll();
    OnFocus(!snip);
  }
}


void wxMediaPasteboard::Resized(wxSnip *snip, Bool redraw_now)
{
  wxNode *node;
  wxSnipLocation *loc;

  if (!(node = snipLocationList->FindPtr(snip)))
    return;
  loc = (wxSnipLocation *)node->Data();  

  if (loc->needResize)
    return;

  changed = TRUE;

  if (!redraw_now)
    sequence++;
  BeginEditSequence();
  
  UpdateLocation(loc);

  loc->needResize = TRUE;
  needResize = TRUE;

  UpdateLocation(loc);

  EndEditSequence();
  if (!redraw_now)
    --sequence;
}

Bool wxMediaPasteboard::Recounted(wxSnip *snip, Bool redraw_now)
{
  Resized(snip, redraw_now);
  return TRUE;
}

void wxMediaPasteboard::NeedsUpdate(wxSnip *snip, float localx, float localy, 
				    float w, float h)
{
  float x, y;

  GetSnipLocation(snip, &x, &y);
  Update(x + localx, y + localy, w, h);
}

Bool wxMediaPasteboard::ReleaseSnip(wxSnip *WXUNUSED(snip))
{
  return FALSE;
}

/************************************************************************/

float wxMediaPasteboard::ScrollLineLocation(long line)
{
  return line * scrollStep;
}


long wxMediaPasteboard::NumScrollLines()
{
  return (long)((totalHeight + scrollStep - 1) / scrollStep);
}

long wxMediaPasteboard::FindScrollLine(float y)
{
  return (long)(y / scrollStep);
}

void wxMediaPasteboard::SetScrollStep(float s)
{
  if (scrollStep != s) {
    scrollStep = s;
    if (admin)
      admin->Resized(TRUE);
  }
}
 
float wxMediaPasteboard::GetScrollStep(void)
{
  return scrollStep;
}

/************************************************************************/

void wxMediaPasteboard::SetMinWidth(float w)
{
  if (w <= 0)
    minWidth = 0.0;
  else
    minWidth = w;

  needResize = TRUE;
  UpdateAll();
}

void wxMediaPasteboard::SetMaxWidth(float w)
{
  if (w <= 0)
    maxWidth = 0.0;
  else
    maxWidth = w;

  needResize = TRUE;
  UpdateAll();
}

float wxMediaPasteboard::GetMinWidth()
{
  return minWidth;
}

float wxMediaPasteboard::GetMaxWidth()
{
  return maxWidth;
}

void wxMediaPasteboard::SetMinHeight(float h)
{
  if (h <= 0)
    minHeight = 0.0;
  else
    minHeight = h;

  needResize = TRUE;
  UpdateAll();
}

void wxMediaPasteboard::SetMaxHeight(float h)
{
  if (h <= 0)
    maxHeight = 0.0;
  else
    maxHeight = h;

  needResize = TRUE;
  UpdateAll();
}

float wxMediaPasteboard::GetMinHeight()
{
  return minHeight;
}

float wxMediaPasteboard::GetMaxHeight()
{
  return maxHeight;
}

/************************************************************************/


wxMediaBuffer *wxMediaPasteboard::CopySelf(void)
{
  wxMediaPasteboard *pb;

  pb = new wxMediaPasteboard();

  CopySelfTo(pb);

  return pb;
}

void wxMediaPasteboard::CopySelfTo(wxMediaBuffer *b)
{
  wxMediaPasteboard *pb;

  if (b->bufferType != wxPASTEBOARD_BUFFER)
	return;
  pb = (wxMediaPasteboard *)b;

  wxMediaBuffer::CopySelfTo(pb);

  pb->SetDragable(GetDragable());
  pb->SetSelectionVisible(GetSelectionVisible());
  pb->SetScrollStep(GetScrollStep());
}

float wxMediaPasteboard::GetDescent(void)
{
  return 0;
}


float wxMediaPasteboard::GetSpace(void)
{
  return 0;
}

void wxMediaPasteboard::GetCenter(float *fx, float *fy)
{
  float x, y, w, h;

  if (!admin) {
    w = totalWidth;
    h = totalHeight;
    x = y = 0;
  } else
    admin->GetView(&x, &y, &w, &h, TRUE);

  if (w > 1000)
    // Don't belive it
    w = 500;
  if (h > 1000)
    // Don't belive it
    h = 500;

  if (fx)
    *fx = w / 2;
  if (fy)
    *fy = h / 2;

}

char *wxMediaPasteboard::GetFlattenedText(long *got)
{
  wxSnip *snip;
  char *t, *s, *old;
  long p, alloc, offset;

  alloc = 100;
  s = new char[alloc];

  snip = snips;

  p = 0;

  while (snip) {
    t = snip->GetText(0, snip->count, TRUE);

    offset = strlen(t);
    if (p + offset >= alloc) {
      alloc = 2 * (p + offset);
      old = s;
      s = new char[alloc];
      memcpy(s, old, p);
    }
    memcpy(s + p, t, offset);
    p += offset;

    snip = snip->next;
  }

  s[p] = 0;
  if (got)
    *got = p;

  return s;
}

/************************************************************************/

void wxMediaPasteboard::Clear()
{
  Delete();
}

void wxMediaPasteboard::Cut(Bool extend, long time)
{
  Copy(extend, time);
  Clear();
}

void wxMediaPasteboard::DoCopy(long time, Bool extend)
{
  wxSnip *snip, *asnip;
  wxSnipLocation *loc;
  wxStyleList *sl;

  wxmb_commonCopyRegionData = NULL;

  sl = (extend && wxmb_copyStyleList) ? wxmb_copyStyleList : styleList;
  
  for (snip = snips; snip; snip = snip->Next()) {
    loc = SnipLoc(snip);
    if (loc->selected) {
      asnip = snip->Copy();
      asnip->SetAdmin(NULL);
      asnip->style = sl->Convert(asnip->style);  
      wxmb_commonCopyBuffer->Append(asnip);
      wxmb_commonCopyBuffer2->Append(GetSnipData(loc->snip));
    }
  }

  InstallCopyBuffer(time, sl);
}

void wxMediaPasteboard::Copy(Bool extend, long time)
{
  BeginCopyBuffer();

  if (!extend)
    FreeOldCopies();

  DoCopy(time, extend);

  EndCopyBuffer();
}

void wxMediaPasteboard::DoPaste(long time)
{
  wxSnip *start, *snip;
  float cx, cy, left, right, top, bottom, dx, dy;
  wxSnipLocation *loc;
  wxDC *dc;

  if (userLocked || writeLocked)
    return;

  start = snips;
  GetCenter(&cx, &cy);

  DoBufferPaste(time);

  // Quiet the compiler:
  left = right = top = bottom = 0;

  if (admin && PTRNE(snips, start)) {
    dc = GetDC();

    if (dc) {
      /* Get top/left/bottom/right of pasted group: */
      for (snip = snips; PTRNE(snip, start); snip = snip->next) {
	loc = SnipLoc(snip);      
	if (loc->needResize)
	  loc->Resize(dc);
	if (PTREQ(snip, snips)) {
	  left = loc->x;
	  top = loc->y;
	  right = loc->r;
	  bottom = loc->b;
	} else {
	  if (loc->x < left)
	    left = loc->x;
	  if (loc->y < top)
	    top = loc->y;
	  if (loc->r > right)
	    right = loc->r;
	  if (loc->b > bottom)
	    bottom = loc->b;
	}
	AddSelected(snip);
      }

      dx = cx - (left + right) / 2;
      dy = cy - (top + bottom) / 2;
    
      /* Shift the pasted group to center: */
      Move(dx, dy);
    }
  } else {
    /* Just select them: */
    for (snip = snips; PTRNE(snip, start); snip = snip->next) {
      AddSelected(snip);
    }
  }
}

void wxMediaPasteboard::Paste(long time)
{
  if (userLocked || writeLocked)
    return;

  BeginEditSequence();

  NoSelected();

  DoPaste(time);

  EndEditSequence();
}

void wxMediaPasteboard::InsertPasteSnip(wxSnip *snip, wxBufferData *data)
{
  Insert(snip, snip);
  SetSnipData(snip, data);
}

void wxMediaPasteboard::InsertPasteString(char *str)
{
  wxTextSnip *snip;

  snip = new wxTextSnip();
  snip->style = styleList->FindNamedStyle(STD_STYLE);
  if (!snip->style) {
    snip->style = styleList->BasicStyle();
  }
  snip->Insert(str, strlen(str));
  
  InsertPasteSnip(snip, NULL);
}

void wxMediaPasteboard::Kill(long time)
{
  Cut(time);
}

/************************************************************************/

Bool wxMediaPasteboard::GetSnipLocation(wxSnip *thesnip, float *x, float *y, 
					Bool bottomRight)
{
  wxNode *node;
  wxSnipLocation *loc;

  if (!admin)
    return FALSE;

  if (bottomRight)
    CheckRecalc();

  node = snipLocationList->FindPtr(thesnip);
  if (!node)
    return FALSE;
  
  loc = (wxSnipLocation *)node->Data();
  if (x)
    *x = loc->x;
  if (y)
    *y = loc->y;
  if (bottomRight) {
    if (x)
      *x += loc->w;
    if (y)
      *y += loc->h;
  }

  return TRUE;
}

/************************************************************************/

wxBufferData *wxMediaPasteboard::GetSnipData(wxSnip *snip)
{
  wxNode *node;
  wxSnipLocation *loc;
  wxLocationBufferData *data;

  if (!(node = snipLocationList->FindPtr(snip)))
    return wxMediaBuffer::GetSnipData(snip);

  loc = (wxSnipLocation *)node->Data();  
  data = new wxLocationBufferData;
  data->x = loc->x;
  data->y = loc->y;

  data->next = wxMediaBuffer::GetSnipData(snip);

  return data;
}

void wxMediaPasteboard::SetSnipData(wxSnip *snip, wxBufferData *data)
{
  while (data) {
    if (data->dataclass && !strcmp(data->dataclass->classname, "wxloc")) {
      wxLocationBufferData *ldata;
      ldata = (wxLocationBufferData *)data;
      MoveTo(snip, ldata->x, ldata->y);
    }
    data = data->next;
  }
}

Bool wxMediaPasteboard::LoadFile(char *file, int WXUNUSED(format), Bool showErrors)
{
  FILE *f;
  Bool ok;

  if (userLocked || writeLocked)
    return FALSE;

  if (!file || !*file) {
    if ((file && !*file) || !filename || tempFilename) {
      char *path;
      
      if (filename) {
	path = PathOnly(filename);
	if (path)
	  path = copystring(path);
      } else
	path = NULL;
      
      file = GetFile(path);
    } else
      file = filename;
  }

  if (!file)
    return FALSE;

  if (!CanLoadFile(file, wxMEDIA_FF_STD))
    return FALSE;
  OnLoadFile(file, wxMEDIA_FF_STD);

  if (::scheme_directory_exists(file)) {
    if (showErrors)
      wxmeError("Can't load a directory.");
    AfterLoadFile(FALSE);
    return FALSE;
  }

  {
    const char *fn;
    fn = wxmeExpandFilename(file);
    f = fopen(fn, "rb");
  }
  
  if (!f) {
    if (showErrors)
      wxmeError("Couldn't open the file.");
    AfterLoadFile(FALSE);
    return FALSE;
  }

  wxBeginBusyCursor();

  BeginEditSequence();

  Erase();

  if (PTRNE(file, filename))
    SetFilename(file, FALSE);

  ok = InsertFile(f, loadoverwritesstyles, showErrors);

  EndEditSequence();

  ClearUndos();

  wxEndBusyCursor();

  SetModified(!ok);

  AfterLoadFile(ok);

  return ok;
}

Bool wxMediaPasteboard::InsertFile(char *file, int WXUNUSED(format), Bool showErrors)
{
  FILE *f;
  const char *fn;

  if (userLocked || writeLocked)
    return FALSE;

  fn = wxmeExpandFilename(file);
  f = fopen(fn, "rb");
  
  if (!f)
    return FALSE;

  return InsertFile(f, FALSE, showErrors);
}

Bool wxMediaPasteboard::InsertFile(FILE *f, Bool clearStyles, Bool showErrors)
{
  int n;
  char buffer[MRED_START_STR_LEN + 1];
  Bool fileerr;

  if (userLocked || writeLocked)
    return FALSE;

  n = fread((char *)buffer, 1, MRED_START_STR_LEN, f);
  buffer[MRED_START_STR_LEN] = 0;
  if ((n != MRED_START_STR_LEN) || strcmp(buffer, MRED_START_STR)){
    if (showErrors)
      wxmeError("This is not a MrEd file.");
    fileerr = TRUE;
  } else {
    wxMediaStreamInFileBase *b;
    wxMediaStreamIn *mf;
    
    b = new wxMediaStreamInFileBase(f);
    mf = new wxMediaStreamIn(b);
    
    fread((char *)mf->read_format, 1, MRED_FORMAT_STR_LEN, f);
    fread((char *)mf->read_version, 1, MRED_VERSION_STR_LEN, f);

    if (wxmeCheckFormatAndVersion(mf)) {
      if (wxReadMediaGlobalHeader(mf)) {
	if (mf->Ok())
	  fileerr = !ReadFromFile(mf, clearStyles);
	else
	  fileerr = TRUE;
      } else
	fileerr = TRUE;
      fileerr = !wxReadMediaGlobalFooter(mf) || fileerr;
    
      styleList->NewNamedStyle(STD_STYLE, NULL);
      
      fileerr = fileerr || !mf->Ok();
    } else
      fileerr = TRUE;
  }

  fileerr = fileerr || ferror(f);
  
  fclose(f);

  if (fileerr && showErrors)
    wxmeError("There was an error loading the file.");

  return !fileerr;
}

Bool wxMediaPasteboard::SaveFile(char *file, int format, Bool showErrors)
{
  FILE *f;
  Bool fileerr;
  Bool no_set_filename;
  wxMediaStreamOutFileBase *b;
  wxMediaStreamOut *mf;

  if (!file || !*file) {
    if ((file && !*file) || !filename || tempFilename) {
      char *path, *pfile;
      
      if (filename) {
	path = PathOnly(filename);
	if (path && *path)
	  path = copystring(path);
	else
	  path = NULL;
	pfile = copystring(FileNameFromPath(filename));
      } else
	path = pfile = NULL;
      
      file = PutFile(path, pfile);
    } else
      file = filename;
  }

  if (!file)
    return FALSE;

  if (format != wxMEDIA_FF_COPY)
    format = wxMEDIA_FF_STD;

  no_set_filename = (format == wxMEDIA_FF_COPY);

  if (!CanSaveFile(file, wxMEDIA_FF_STD))
    return FALSE;
  OnSaveFile(file, wxMEDIA_FF_STD);
  
  {
    const char *fn;
    fn = wxmeExpandFilename(file);
    f = fopen(fn, "wb");
  }
  
  if (!f) {
    if (showErrors)
      wxmeError("Couldn't write the file.");
    AfterSaveFile(FALSE);
    return FALSE;
  }

  wxBeginBusyCursor();

#ifdef wx_mac
  wxMediaSetFileCreatorType(file, TRUE);
#endif

  fwrite(MRED_START_STR, 1, MRED_START_STR_LEN, f);
  fwrite(MRED_FORMAT_STR, 1, MRED_FORMAT_STR_LEN, f);
  fwrite(MRED_VERSION_STR, 1, MRED_VERSION_STR_LEN, f);    

  b = new wxMediaStreamOutFileBase(f);
  mf = new wxMediaStreamOut(b);
  
  wxWriteMediaGlobalHeader(mf);
  if (mf->Ok())
    fileerr = !WriteToFile(mf);
  else
    fileerr = TRUE;
  wxWriteMediaGlobalFooter(mf);
  
  fileerr = fileerr || !mf->Ok();

  fclose(f);

  if (fileerr && showErrors)
    wxmeError("There was an error writing the file.");

  if (!no_set_filename)
    SetFilename(file, FALSE);

  wxEndBusyCursor();

  if (!no_set_filename)
    SetModified(fileerr);

  AfterSaveFile(!fileerr);

  return !fileerr;
}

Bool wxMediaPasteboard::WriteToFile(wxMediaStreamOut *f)
{
  if (!DoWriteHeadersFooters(f, TRUE))
    return FALSE;

  wxmbWriteSnipsToFile(f, styleList, NULL, snips, NULL, NULL, this);
  
  if (!DoWriteHeadersFooters(f, FALSE))
    return FALSE;

  return TRUE;
}


Bool wxMediaPasteboard::ReadFromFile(wxMediaStreamIn *f, Bool overwritestyle)
{
  if (userLocked || writeLocked)
    return FALSE;

  return ReadSnipsFromFile(f, overwritestyle);
}


Bool wxMediaPasteboard::ReadInsert(wxSnip *snip)
{
  Insert(snip, (wxSnip *)NULL);
  return TRUE;
}

void wxMediaPasteboard::SetFilename(char *name, Bool temp)
{
  wxSnip *snip;

  filename = copystring(name);
  tempFilename = temp;

  for (snip = snips; snip; snip = snip->next) {
    if (snip->flags & wxSNIP_USES_BUFFER_PATH)
      /* Just a notification */
      snip->SetAdmin(snipAdmin);
  }
}

/************************************************************************/

void wxMediaPasteboard::StyleHasChanged(wxStyle *style)
{
  if (!style) {
    changed = TRUE;
    UpdateAll();
    return;
  }
}

/************************************************************************/

void wxMediaPasteboard::BeginEditSequence(Bool undoable)
{
  if (noundomode || !undoable)
    noundomode++;

  if (!sequence && !writeLocked)
    OnEditSequence();

  sequence++;
}


void wxMediaPasteboard::EndEditSequence(void)
{
  if (!(--sequence) && !writeLocked) {
    sequenceStreak = FALSE;
    UpdateNeeded();
    AfterEditSequence();
  }

  if (noundomode)
    --noundomode;
}

Bool wxMediaPasteboard::RefreshDelayed(void)
{
  if (sequence)
    return 1;
  
  if (!admin)
    return 1;
  else
    return admin->DelayRefresh();
}

/************************************************************************/

void wxMediaPasteboard::AddPasteboardFunctions(wxKeymap *tab)
{
  wxAddMediaPasteboardFunctions(tab);
}

void wxAddMediaPasteboardFunctions(wxKeymap *)
{
}

Bool wxMediaPasteboard::GetDragable()
{
  return dragable;
}

void wxMediaPasteboard::SetDragable(Bool d)
{
  dragable = d;
}

Bool wxMediaPasteboard::GetSelectionVisible()
{
  return selectionVisible;
}

void wxMediaPasteboard::SetSelectionVisible(Bool v)
{
  selectionVisible = v;
}


/************************************************************************/

void wxMediaPasteboard::OnChange(void)
{
}

Bool wxMediaPasteboard::CanInsert(wxSnip *, wxSnip *, float, float)
{
  return TRUE;
}

void wxMediaPasteboard::OnInsert(wxSnip *, wxSnip *, float, float)
{
}

void wxMediaPasteboard::AfterInsert(wxSnip *, wxSnip *, float, float)
{
}

Bool wxMediaPasteboard::CanDelete(wxSnip *)
{
  return TRUE;
}

void wxMediaPasteboard::OnDelete(wxSnip *)
{
}

void wxMediaPasteboard::AfterDelete(wxSnip *)
{
}

Bool wxMediaPasteboard::CanMoveTo(wxSnip *, float, float, Bool WXUNUSED(dragging))
{
  return TRUE;
}

void wxMediaPasteboard::OnMoveTo(wxSnip *, float, float, Bool WXUNUSED(dragging))
{
}

void wxMediaPasteboard::AfterMoveTo(wxSnip *, float, float, Bool WXUNUSED(dragging))
{
}

Bool wxMediaPasteboard::CanResize(wxSnip *, float, float)
{
  return TRUE;
}

void wxMediaPasteboard::OnResize(wxSnip *, float, float)
{
}

void wxMediaPasteboard::AfterResize(wxSnip *, float, float, Bool WXUNUSED(did))
{
}

Bool wxMediaPasteboard::CanSelect(wxSnip *, Bool)
{
  return TRUE;
}

void wxMediaPasteboard::OnSelect(wxSnip *, Bool)
{
}

void wxMediaPasteboard::AfterSelect(wxSnip *, Bool)
{
}

#if ALLOW_X_STYLE_SELECTION
Bool wxMediaPasteboard::OwnXSelection(Bool on, Bool WXUNUSED(update), Bool force)
{
  return DoOwnXSelection(on, force);
}
#endif

Bool wxMediaPasteboard::CanInteractiveMove(wxMouseEvent *)
{
  return TRUE;
}

void wxMediaPasteboard::OnInteractiveMove(wxMouseEvent *)
{
}

void wxMediaPasteboard::AfterInteractiveMove(wxMouseEvent *)
{
}

Bool wxMediaPasteboard::CanInteractiveResize(wxSnip *)
{
  return TRUE;
}

void wxMediaPasteboard::OnInteractiveResize(wxSnip *)
{
}

void wxMediaPasteboard::AfterInteractiveResize(wxSnip *)
{
}

/************************************************************************/

extern void wxmeGetDefaultSize(float *w, float *h);

void *wxMediaPasteboard::BeginPrint(wxDC *, Bool)
{
  SizeCacheInvalid();  
  return NULL;
}

void wxMediaPasteboard::EndPrint(wxDC *, void *)
{
  SizeCacheInvalid();
}

Bool wxMediaPasteboard::HasPrintPage(wxDC *dc, int p)
{
  float H, W, h, w;
  long hm, vm, hcount, vcount;

  CheckRecalc();

  dc->GetSize(&W, &H);
  if (!W || !H)
    wxmeGetDefaultSize(&W, &H);
  wxGetMediaPrintMargin(&hm, &vm);
  W -= 2 * hm;
  H -= 2 * vm;

  w = h = 0.0;
  GetExtent(&w, &h);

  hcount = (long)(w / W);
  if (hcount * W < w)
    hcount++;

  vcount = (long)(h / H);
  if (vcount * H < h)
    vcount++;

  return (p <= (hcount * vcount));
}

void wxMediaPasteboard::PrintToDC(wxDC *dc, int page)
{
  float H, W, FH, FW, h, w;
  long hm, vm, hcount, vcount, hpos, vpos, startpage, endpage, p;

  CheckRecalc();

  dc->GetSize(&W, &H);
  if (!W || !H)
    wxmeGetDefaultSize(&W, &H);
  FH = H;
  FW = W;
  wxGetMediaPrintMargin(&hm, &vm);
  W -= 2 * hm;
  H -= 2 * vm;

  w = h = 0.0;
  GetExtent(&w, &h);

  hcount = (long)(w / W);
  if (hcount * W < w)
    hcount++;

  vcount = (long)(h / H);
  if (vcount * H < h)
    vcount++;

  if (page < 0) {
    startpage = 1;
    endpage = hcount * vcount;
  } else {
    startpage = endpage = page;
  }

  for (p = startpage; p <= endpage; p++) {
    float x, y;

    vpos = (p - 1) / hcount;
    hpos = (p - 1) % hcount;

    x = hpos * W;
    y = vpos * H;

    if (page < 0)
      dc->StartPage();
    
    /* Establish page size: */
    dc->DrawLine(0, 0, 0, 0);
    dc->DrawLine(FW, FH, FW, FH);

    skipBox = this;
    Draw(dc, -x + hm, -y + vm,
	 x, y, x + W, y + H,
	 FALSE);
    skipBox = NULL;

    if (page < 0)
      dc->EndPage();    
  }
}

/************************************************************************/

void wxSnipLocation::Resize(wxDC *dc)
{
  float ww, hh;

  ww = hh = 0.0;
  snip->GetExtent(dc, x, y, &ww, &hh);
  w = ww;
  h = hh;
  r = x + w;
  b = y + h;
  hm = x + w/2;
  vm = y + h/2;
  
  needResize = FALSE;
}
