/*
 * File:        wx_snip.cc
 * Purpose:     wxSnip implementations
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 1995-99, Matthew Flatt

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
#include "wx_cmdlg.h"
#endif
#include "wx_utils.h"
#include "wx_media.h"
#include "wx_ptreq.h"
#include <string.h>
#include "wx_gcrct.h"

#define SETWD_NOT_PRESENT 1

#ifdef wx_x
#define CHECK_SPLIT 1
#else
#define CHECK_SPLIT 0
#endif

#define CHECK_CS_FLAG 1

#ifdef MEMMOVE_NOT_PRESENT
static void memmove(char *dest, char *src, long size)
{
  if (dest < src) {
    while (size--)
      *(dest++) = *(src++);
  } else {
    dest += size;
    src += size;
    while (size--)
      *(--dest) = *(--src);
  }
}
#endif

# define WXGC_CLEANUP_ARG(a) a

/* MSW version needs this for just a little while longer... */
#ifndef WXGC_ATOMIC
#define WXGC_ATOMIC /* empty */
#endif

#define MAX_WASTE 3

#define IMAGE_PIXELS_PER_SCROLL 20
#define IMAGE_VOID_SIZE 20

#define ALWAYSZERO(x) if (x) *x = 0;

#define STRALLOC(n) new WXGC_ATOMIC char[n]
#define STRFREE(s) /* empty */

/***************************************************************/

wxSnipClass::wxSnipClass()
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_SNIP_CLASS;
#endif

  classname = "wxbad";
  version = 0;
  required = 0;
}

Bool wxSnipClass::ReadHeader(wxMediaStreamIn *)
{
  return TRUE;
}

Bool wxSnipClass::WriteHeader(wxMediaStreamOut *)
{
  return TRUE;
}

int wxSnipClass::ReadingVersion(wxMediaStreamIn *f)
{
  return f->ReadingVersion(this);
}

/***************************************************************/

wxSnip::wxSnip() : wxObject(WXGC_CLEANUP_ARG(WXGC_NO_CLEANUP))
{
  Init();
}

wxSnip::wxSnip(Bool cleanup) : wxObject(WXGC_CLEANUP_ARG(cleanup))
{
  Init();
}

wxInternalSnip::wxInternalSnip() : wxSnip()
{
}

wxInternalSnip::wxInternalSnip(Bool cleanup) : wxSnip(cleanup)
{
}

void wxSnip::Init(void)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_SNIP;
#endif

  count = 1;

  flags = 0;

  snipclass = NULL;


  next = prev = NULL;
  line = NULL;

  admin_ptr = new wxSnipAdmin*;
  *admin_ptr = NULL;

  style = wxTheStyleList->BasicStyle();
}

wxSnip::~wxSnip()
{
  next = prev = NULL;
  line = NULL;
}

wxSnip *wxSnip::Next(void)
{
  return next;
}

wxSnip *wxSnip::Previous(void)
{
  return prev;
}

wxSnipAdmin *wxSnip::GetAdmin(void)
{
  return *admin_ptr;
}

void wxSnip::SetAdmin(wxSnipAdmin *a)
{
  if (PTRNE(a, *admin_ptr) && (flags & wxSNIP_OWNED)
      && (a || !(flags & wxSNIP_CAN_DISOWN)))
    return;

  *admin_ptr = a;
  SizeCacheInvalid();
  if (!a) {
    prev = next = NULL;
    line = NULL;
  } else
    flags |= wxSNIP_OWNED;
}

void wxSnip::SetCount(long new_count)
{
  long old_count = count;

  if (new_count <= 0)
    new_count = 1;
  count = new_count;
  if (*admin_ptr) {
    if (!(*admin_ptr)->Recounted(this, TRUE))
      count = old_count;
  }
}

void wxInternalSnip::SetCount(long WXUNUSED(new_count))
{
  /* reject change */
}

void wxSnip::SetFlags(long new_flags)
{
  /* Make sure that wxSNIP_HARD_NEWLINE implies a wxSNIP_NEWLINE */
  if (new_flags & wxSNIP_NEWLINE)
    new_flags -= wxSNIP_NEWLINE;    
  if (new_flags & wxSNIP_HARD_NEWLINE)
    new_flags |= wxSNIP_NEWLINE;

  /* Make sure ownership and splitness flags don't change */
  if (new_flags & wxSNIP_OWNED)
    new_flags -= wxSNIP_OWNED;
  if (new_flags & wxSNIP_CAN_DISOWN)
    new_flags -= wxSNIP_CAN_DISOWN;
  if (new_flags & wxSNIP_CAN_SPLIT)
    new_flags -= wxSNIP_CAN_SPLIT; 

  if (flags & wxSNIP_OWNED)
    new_flags |= wxSNIP_OWNED;
  if (flags & wxSNIP_CAN_DISOWN)
    new_flags |= wxSNIP_CAN_DISOWN;
  if (flags & wxSNIP_CAN_SPLIT)
    new_flags |= wxSNIP_CAN_SPLIT; 

  flags = new_flags;
  if (*admin_ptr)
    (*admin_ptr)->Resized(this, TRUE);
}

void wxSnip::OnEvent(wxDC *, float, float, float, float, wxMouseEvent *)
{
}

wxCursor *wxSnip::AdjustCursor(wxDC *, float, float, float, float, wxMouseEvent *)
{
  return NULL;
}

void wxSnip::OnChar(wxDC *, float, float, float, float, wxKeyEvent *)
{
}

void wxSnip::DoEdit(int, Bool, long)
{
}

void wxSnip::DoFont(int, Bool)
{
}

Bool wxSnip::Match(wxSnip *other)
{
  if (PTRNE(other->snipclass, snipclass))
    return FALSE;
  if (other->count != count)
    return FALSE;
  return TRUE;
}
 
void wxSnip::OwnCaret(Bool)
{
}

void wxSnip::BlinkCaret(wxDC *, float, float)
{
}

void wxSnip::SizeCacheInvalid(void)
{
}

void wxSnip::GetExtent(wxDC *, 
		       float, float, 
		       float *w, float *h, 
		       float *descent, float *space,
		       float *lspace, float *rspace)
{
  if (w)
    *w = 0;
  if (h)
    *h = 0;
  if (descent)
    *descent = 0;
  if (space)
    *space = 0;
  if (lspace)
    *lspace = 0;
  if (rspace)
    *rspace = 0;
}

float wxSnip::PartialOffset(wxDC *dc, 
			    float x, float y, long offset)
{
  float w;

  if (!offset)
    return 0.0;

  w = 0.0;
  GetExtent(dc, x, y, &w);
  return w;
}

void wxSnip::Draw(wxDC *, float, float,
		  float, float, float, float, 
		  float, float, int)
{
}

void wxSnip::Split(long position, wxSnip **first, wxSnip **second)
{
  wxSnip *snip;

  snip = new wxSnip();
  snip->count = position;
  count -= position;

  *first = snip;
  *second = this;

#if CHECK_CS_FLAG
  if (!(flags & wxSNIP_CAN_SPLIT) && *admin_ptr)
    (*admin_ptr)->Resized(this, TRUE);
#endif
}

wxSnip *wxSnip::MergeWith(wxSnip *)
{
  return NULL;
}

void wxSnip::GetText(char *s, long offset, long num)
{
  char *str;

  if (num <= 0)
    return;
  str = GetText(offset, num, FALSE);
  if (!str)
    memset(s, '.', num);
  else
    memcpy(s, str, num);
}

char *wxSnip::GetText(long offset, long num, 
		      Bool WXUNUSED(flattened), long *got)
{
  char *s;

  if (num <= 0)
    return "";
  if (offset < 0)
    offset = 0;
  if (offset > count)
    return "";
  if (num > count - offset)
    num = count - offset;

  s = new WXGC_ATOMIC char[num + 1];
  memset(s, '.', num);
  s[num] = 0;

  if (got)
    *got = num;

  return s;
}

wxSnip *wxSnip::Copy()
{
  wxSnip *snip;

  snip = new wxSnip();
  Copy(snip);

  return snip;
}

void wxSnip::SetStyle(wxStyle *s)
{
  if (flags & wxSNIP_OWNED)
    return;

  style = s;
}

void wxSnip::Copy(wxSnip *snip)
{
  snip->count = count;
  snip->flags = flags;
  if (snip->flags & wxSNIP_OWNED)
    snip->flags -= wxSNIP_OWNED;
  if (snip->flags & wxSNIP_CAN_DISOWN)
    snip->flags -= wxSNIP_CAN_DISOWN;
  if (snip->flags & wxSNIP_CAN_SPLIT)
    snip->flags -= wxSNIP_CAN_SPLIT;
  snip->snipclass = snipclass;
  snip->style = style;
}

void wxSnip::Write(wxMediaStreamOut *)
{
}

Bool wxSnip::Resize(float, float)
{
  return FALSE;
}

long wxSnip::GetNumScrollSteps()
{
  return 1;
}

long wxSnip::FindScrollStep(float)
{
  return 0;
}

float wxSnip::GetScrollStepOffset(long)
{
  return 0;
}

Bool wxSnip::IsOwned(void)
{
  return !!(flags & wxSNIP_OWNED);
}

Bool wxSnip::ReleaseFromOwner(void)
{
  if (!IsOwned())
    return TRUE;

  if (!*admin_ptr)
    return FALSE;

  if ((*admin_ptr)->ReleaseSnip(this))
    return !(flags & wxSNIP_OWNED);
  else
    return FALSE;
}

/***************************************************************/

class TextSnipClass : public wxSnipClass
{
 public:
  TextSnipClass(void);

  virtual wxSnip *Read(wxMediaStreamIn *);
  wxSnip *Read(wxTextSnip *, wxMediaStreamIn *);
};

static TextSnipClass *TheTextSnipClass;

TextSnipClass::TextSnipClass(void)
{
  classname = "wxtext";
  version = 1;
  required = TRUE;
}

wxSnip *TextSnipClass::Read(wxMediaStreamIn *f)
{
  wxTextSnip *s;
  s = new wxTextSnip(0);
  return Read(s, f);
}

wxSnip *TextSnipClass::Read(wxTextSnip *snip, wxMediaStreamIn *f)
{
  long flags;
  long count, pos;

  f->Get(&flags);

  pos = f->Tell();
  f->Get(&count);
  f->JumpTo(pos);

  snip->Read(count, f);

  snip->flags = flags;

  return snip;
}

/***************************************************************/

#ifdef wx_mac
# define NON_BREAKING_SPACE 0xCA
#else
# define NON_BREAKING_SPACE 0xA0
#endif

wxTextSnip::wxTextSnip(long allocsize) 
{
  Init(allocsize);
}

wxTextSnip::wxTextSnip(char *initstring, long len) 
{
  Init(len + 2);
  Insert(initstring, len, 0);
}

void wxTextSnip::Init(long allocsize) 
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_TEXT_SNIP;
#endif

  flags |= wxSNIP_IS_TEXT | wxSNIP_CAN_APPEND;

  w = -1.0;

  if (allocsize > 5000)
    allocsize = 5000;

  allocated = (allocsize > 0) ? 2 * allocsize : 20;
  buffer = STRALLOC(allocated + 1);
  dtext = 0;

  snipclass = TheTextSnipClass;
  
  count = 0;
}

wxTextSnip::~wxTextSnip()
{
  STRFREE(buffer);
  buffer = NULL;
}

void wxTextSnip::SizeCacheInvalid(void)
{
  w = -1.0;
}

void wxTextSnip::GetTextExtent(wxDC *dc, int count, float *wo)
{
  char save;
  float _w, h;
  wxFont *font;
  int i;

  save = buffer[dtext + count];
  buffer[dtext + count] = 0;

  for (i = count; i--; ) {
    unsigned char c = ((unsigned char *)buffer)[dtext + i]; 
    if (!c || (c == NON_BREAKING_SPACE))
      break;
  }
  
  font = style->GetFont();
#ifdef BROKEN_GET_TEXT_EXTENT 
  dc->SetFont(font);
#endif

  if (i < 0) {
    dc->GetTextExtent(buffer, &_w, &h, NULL, NULL, font, FALSE, dtext);
  } else {
    /* text includes null chars */
    float ex_w;
    int start = 0, i;
    
#ifndef BROKEN_GET_TEXT_EXTENT 
    dc->SetFont(font);
#endif
    dc->GetTextExtent(" ", &ex_w, &h, NULL, NULL, font);
    
    _w = 0;
    for (i = 0; i <= count; i++) {
      if (!buffer[dtext + i] || (((unsigned char *)buffer)[dtext + i] == NON_BREAKING_SPACE) || (i == count)) {
	if (i > start) {
	  float piece_w, h;
	  char save = buffer[dtext + i];
	  buffer[dtext + i] = 0;
	  dc->GetTextExtent(buffer, &piece_w, &h, NULL, NULL, NULL, FALSE, dtext + start);
	  buffer[dtext + i] = save;
	  _w += piece_w;
	}
	if (i < count) {
	  start = i + 1;
	  _w += ex_w;
	}
      }
    }
  }
  
  buffer[dtext + count] = save;

  *wo = _w;
}

void wxTextSnip::GetExtent(wxDC *dc, 
			   float WXUNUSED(x), float WXUNUSED(y), 
			   float *wo, float *ho, float *dso, float *so,
			   float *ls, float *rs)
{

  if (w < 0) {
    if ((flags & wxSNIP_INVISIBLE) || !count 
	|| (count == 1 && buffer[dtext] == '\n')
	|| (count == 1 && buffer[dtext] == '\t')) {
      if (count == 1 && buffer[dtext] == '\t') {
	float tw;
	tw = style->GetTextWidth(dc);
	w = tw;
      } else
	w = 0;
    } else {
      float ww;
      GetTextExtent(dc, count, &ww);
      w = ww;
    }
  }

  if (wo)
    *wo = w;
  if (ho) {
    float th;
    th = style->GetTextHeight(dc);
    *ho = th;
  }
  if (dso) {
    float td;
    td = style->GetTextDescent(dc);
    *dso = td;
  }
  if (so) {
    float ts;
    ts = style->GetTextSpace(dc);
    *so = ts;
  }
  if (ls)
    *ls = 0.0;
  if (rs)
    *rs = 0.0;
}

float wxTextSnip::PartialOffset(wxDC *dc, float, float, long offset)
{
  float _w;
  
  if (offset > count)
    offset = count;

  GetTextExtent(dc, offset, &_w);

  return _w;
}

void wxTextSnip::Draw(wxDC *dc, float x, float y, 
		      float, float, float, float, 
		      float WXUNUSED(dx), float WXUNUSED(dy), 
		      int)
{
  char save;
  int i;

  if (flags & wxSNIP_INVISIBLE)
    return;

  save = buffer[dtext + count];
  buffer[dtext + count] = 0;

  for (i = count; i--; ) {
    unsigned char c = ((unsigned char *)buffer)[dtext + i]; 
    if (!c || (c == NON_BREAKING_SPACE))
      break;
  }
  
  if (i < 0)
    dc->DrawText(buffer, x, y, FALSE, dtext);
  else {
    /* text includes null chars */
    float px, h, ex_w;
    int start = 0, i;

    dc->GetTextExtent(" ", &ex_w, &h, NULL, NULL);
    
    px = x;
    for (i = 0; i <= count; i++) {
      if (!buffer[dtext + i] || (((unsigned char *)buffer)[dtext + i] == NON_BREAKING_SPACE) || (i == count)) {
	if (i > start) {
	  float piece_w, h;
	  char save = buffer[dtext + i];
	  buffer[dtext + i] = 0;
	  dc->GetTextExtent(buffer, &piece_w, &h, NULL, NULL, NULL, FALSE, dtext + start);
	  dc->DrawText(buffer, px, y, FALSE, dtext + start);
	  buffer[dtext + i] = save;
	  px += piece_w;
	}
	if (i < count) {
	  /* In case there's a background, draw a space: */
	  dc->DrawText(" ", px, y);

	  /* Draw box for nul: */
	  if (!buffer[dtext + i])
	    if (h > 2 && ex_w > 2)
	      dc->DrawRectangle(px + 1, y + 1, ex_w - 2, h - 2);

	  start = i + 1;
	  px += ex_w;
	}
      }
    }
  }

#ifdef wx_x
  if (style->GetUnderlined()) {
    float descent, h;
    
    descent = style->GetTextDescent(dc);
    h = style->GetTextHeight(dc);
    
    if (descent >= 2)
      y += h - (descent / 2);
    else
      y += h - descent;
    dc->DrawLine(x, y, x + w + GC_LINE_EXTEND, y);
  }
#endif

  buffer[dtext + count] = save;
}

void wxTextSnip::Split(long position, wxSnip **first, wxSnip **second)
{
  wxTextSnip *snip;

  if (position < 0 || position > count)
    return;

  snip = new wxTextSnip(position);

  w = -1.0;

  memcpy(snip->buffer + snip->dtext, buffer + dtext, position);
  dtext += position;

  snip->count = position;
  count -= position;

  if (count && ((allocated / count) > MAX_WASTE)) {
    char *naya;
    allocated = count;
    naya = STRALLOC(allocated + 1);
    memcpy(naya, buffer + dtext, count + 1);
    buffer = naya;
    dtext = 0;
  }

  *first = snip;
  *second = this;

#if CHECK_CS_FLAG
  if (!(flags & wxSNIP_CAN_SPLIT) && *admin_ptr)
    (*admin_ptr)->Resized(this, TRUE);
#endif
}

wxSnip *wxTextSnip::MergeWith(wxSnip *pred)
{
#if USE_OLD_TYPE_SYSTEM
  if (pred->__type != wxTYPE_TEXT_SNIP)
    return this;
#endif

  w = -1.0;

  Insert(((wxTextSnip *)pred)->buffer + ((wxTextSnip *)pred)->dtext, pred->count, 0);

#if CHECK_CS_FLAG
  if (!(flags & wxSNIP_CAN_SPLIT) && (*admin_ptr))
    (*admin_ptr)->Resized(this, TRUE);
#endif

  return this;
}

void wxTextSnip::Insert(char *str, long len, long pos)
{
  if (len <= 0)
    return;
  if (pos < 0)
    pos = 0;

  if (allocated < count + len) {
    char *naya;

    allocated = 2 * (count + len);
    naya = STRALLOC(allocated + 1);
    
    memcpy(naya, buffer + dtext, count);

    buffer = naya;
    dtext = 0;
  } else if (dtext && (dtext + count + len > allocated)) {
    memmove(buffer, buffer + dtext, count);
    dtext = 0;
  }
   
  if (pos < count)
    memmove(buffer + dtext + pos + len, buffer + dtext + pos, count - pos);
  memcpy(buffer + dtext + pos, str, len);
  
  count += len;

  w = -1.0;
  
#if CHECK_CS_FLAG
  if (!(flags & wxSNIP_CAN_SPLIT) && (*admin_ptr))
    if (!(*admin_ptr)->Recounted(this, TRUE))
      count -= len;
#endif
}

void wxTextSnip::GetText(char *s, long offset, long num)
{
  if (num <= 0)
    return;

  memcpy(s, buffer + dtext + offset, num);
}

char *wxTextSnip::GetText(long offset, long num, Bool flat, long *got)
{
  if (offset < 0) offset = 0;
  if ((num <= 0) || (offset >= count)) {
    if (got)
      *got = 0;
    return "";
  }
  if (num + offset > count)
    num = count - offset;

  if (flat && (flags & wxSNIP_HARD_NEWLINE)) {
    char *s;

#ifdef wx_msw
#define NWL_RC 2
#else
#define NWL_RC 1
#endif

    if (got)
      *got = NWL_RC;
    
    s = new WXGC_ATOMIC char[NWL_RC + 1];
#ifdef wx_x
    s[0] = '\n';
#else
#ifdef wx_msw
    s[0] = '\r';
    s[1] = '\n';
#else
    // Macintosh
    s[0] = '\r';
#endif
#endif
    s[NWL_RC] = 0;
    return s;
  } else {
    char *s;
    s = new WXGC_ATOMIC char[num + 1];
    memcpy(s, buffer + dtext + offset, num);
    s[num] = 0;
    if (got)
      *got = num;
    return s;
  }
}

wxSnip *wxTextSnip::Copy()
{
  wxTextSnip *snip;

  snip = new wxTextSnip(count);
  Copy(snip);
  return snip;
}

void wxTextSnip::Copy(wxTextSnip *snip) 
{
  wxSnip::Copy(snip);
  
  memcpy(snip->buffer + snip->dtext, buffer + dtext, count);
  snip->count = count;

  snip->w = -1.0;
}

void wxTextSnip::Write(wxMediaStreamOut *f)
{
  long writeFlags;

  writeFlags = flags;
  if (writeFlags & wxSNIP_OWNED)
    writeFlags -= wxSNIP_OWNED;
  if (writeFlags & wxSNIP_CAN_DISOWN)
    writeFlags -= wxSNIP_CAN_DISOWN;
  if (writeFlags & wxSNIP_CAN_SPLIT)
    writeFlags -= wxSNIP_CAN_SPLIT;

  f->Put(writeFlags);
  f->Put(count, buffer + dtext);
}

void wxTextSnip::Read(long len, wxMediaStreamIn *f)
{
  if (len <= 0)
    return;

  if (allocated < len) {
    allocated = 2 * len;
    STRFREE(buffer);
    buffer = STRALLOC(allocated + 1);
  }

  dtext = 0;
  f->Get((long *)&len, buffer);
  count = len;
  w = -1.0;
}

#ifdef MEMORY_USE_METHOD
long wxTextSnip::MemoryUse(void)
{
  return allocated + wxObject::MemoryUse();
}
#endif

/***************************************************************/

class TabSnipClass : public TextSnipClass
{
 public:
  TabSnipClass(void);

  virtual wxSnip *Read(wxMediaStreamIn *);
};

static TabSnipClass *TheTabSnipClass;

TabSnipClass::TabSnipClass(void)
{
  classname = "wxtab";
  version = 1;
  required = TRUE;
}

wxSnip *TabSnipClass::Read(wxMediaStreamIn *f)
{
  wxTabSnip *ts;
  ts = new wxTabSnip();
  return TextSnipClass::Read(ts, f);
}

/***************************************************************/

wxTabSnip::wxTabSnip() : wxTextSnip(1)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_TAB_SNIP;
#endif

  snipclass = TheTabSnipClass;

  flags |= wxSNIP_WIDTH_DEPENDS_ON_X;
  flags -= (flags & wxSNIP_CAN_APPEND);
}

void wxTabSnip::GetExtent(wxDC *dc, 
			  float x, float y, 
			  float *wi, float *h, 
			  float *descent, float *space,
			  float *lspace, float *rspace)
{
  float *tabs, oldw;
  float tabspace;
  int n, i;
  Bool changed;

  changed = (w < 0);
  oldw = w;

  wxTextSnip::GetExtent(dc, x, y, wi, h, descent, space, lspace, rspace);

  if (changed) {
    /* w is now width of a space */
    float mult;
    wxMediaBuffer *media = NULL;

    if ((*admin_ptr) && (media = (*admin_ptr)->GetMedia()) && (media->bufferType == wxEDIT_BUFFER)) {
      float space;
      Bool units;
      wxMediaEdit *edt;

      edt = (wxMediaEdit *)(*admin_ptr)->GetMedia();
      tabs = edt->GetTabs(&n, &space, &units);
      tabspace = space;
      mult = units ? 1 : w;
    } else {
      n = 0;
      tabs = NULL;
      tabspace = wxTAB_WIDTH;
      mult = 1;
    }
    
    for (i = 0; i < n; i++) {
      if (tabs[i] * mult > x) {
	w = tabs[i] * mult - x;
	break;
      }
    }

    if (i >= n) {
      float base;

      base = tabs ? (tabs[n - 1] * mult) : 0;
      x -= base;

      tabspace *= mult;
      w = base + ((long)tabspace - ((long)x % (long)tabspace));
    }
    
  } else
    w = oldw;

  if (wi)
    *wi = w;
}

float wxTabSnip::PartialOffset(wxDC *dc, float x, float y, long offset)
{
  float _w;

  if (!offset)
    return 0;
  else {
    _w = 0.0;
    GetExtent(dc, x, y, &_w);
    return _w;
  }
}

void wxTabSnip::Draw(wxDC *, float, float, 
		     float, float, float, float, 
		     float, float, int)
{
  /* Do nothing! */
}

wxSnip *wxTabSnip::Copy()
{
  wxTabSnip *snip;

  snip = new wxTabSnip();
  wxTextSnip::Copy(snip);
  return snip;
}

/***************************************************************/

#define IMG_MOVE_BUF_SIZE 500

class ImageSnipClass : public wxSnipClass
{
 public:
  ImageSnipClass(void);

  virtual wxSnip *Read(wxMediaStreamIn *);
};

static ImageSnipClass *TheImageSnipClass;

ImageSnipClass::ImageSnipClass(void)
{
  classname = "wximage";
  version = 2;
  required = FALSE;
}

wxSnip *ImageSnipClass::Read(wxMediaStreamIn *f)
{
  wxImageSnip *snip;
  char *filename, *delfile = NULL, *loadfile;
  long type;
  Bool relative, inlined = FALSE;
  float w, h, dx, dy;
  wxStandardSnipClassList *scl;
  Bool canInline;

  scl = wxGetTheSnipClassList();
  canInline = (f->ReadingVersion(this) > 1);

  filename = f->GetString(NULL);
  f->Get(&type);
  f->Get(&w);
  f->Get(&h);
  f->Get(&dx);
  f->Get(&dy);
  f->Get(&relative);

  loadfile = filename;

  if (filename && !*filename && canInline && type) {
    /* read inlined image */

    long len;
    f->GetFixed(&len);

    if (len) {
      char *fname;
      FILE *fi;
      char buffer[IMG_MOVE_BUF_SIZE + 1];
    
      fname = wxGetTempFileName("img", NULL);

      fi = fopen(fname, "wb");
      if (fi) {
	long c;
	
	while (len--) {
	  c = IMG_MOVE_BUF_SIZE + 1;
	  f->Get(&c, buffer);
	  
	  c = fwrite(buffer, 1, c, fi);
	}
	fclose(fi);

	loadfile = fname;
	type = ((type == 1) ? wxBITMAP_TYPE_XBM : wxBITMAP_TYPE_XPM);
	inlined = TRUE;
      }

      delfile = fname;      
    }
  }
  
  snip = new wxImageSnip(loadfile, type, relative, inlined);

  if (delfile) {
    wxRemoveFile(delfile);
  }

  snip->Resize(w, h);
  snip->SetOffset(dx, dy);

  return (wxSnip *)snip;
}

/***************************************************************/

wxImageSnip::wxImageSnip(char *name, long type, Bool relative, Bool inlineImg)
{
  Init();

  if (name && *name)
    LoadFile(name, type, relative, inlineImg);
}

wxImageSnip::wxImageSnip(wxBitmap *bm)
{
  Init();

  SetBitmap(bm);
}

void wxImageSnip::Init(void)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_IMAGE_SNIP;
#endif

  snipclass = TheImageSnipClass;

  contentsChanged = TRUE;
  filename = NULL;
  filetype = 0;
  relativePath = FALSE;
  bm = NULL;
  vieww = viewh = -1.0;
  viewdx = viewdy = 0.0;
}

wxImageSnip::~wxImageSnip()
{
}

void wxImageSnip::SizeCacheInvalid(void)
{
  contentsChanged = TRUE;  
}


void wxImageSnip::GetExtent(wxDC *,
			    float WXUNUSED(x), 
			    float WXUNUSED(y),
			    float *wi, float *hi, 
			    float *descent, float *space,
			    float *lspace, float *rspace)
{
  if (contentsChanged) {
    if (bm && bm->Ok()) {
      if (viewh < 0) {
	int bmh;
	bmh = bm->GetHeight();
	h = bmh;
      } else
	h = viewh;
      if (vieww < 0) {
	int bmw;
	bmw = bm->GetWidth();
	w = bmw;
      } else
	w = vieww;
    } else {
      h = w = 0;
    }
    if (!h)
      h = IMAGE_VOID_SIZE;
    if (!w)
      w = IMAGE_VOID_SIZE;
  }

  if (wi)
    *wi = w;
  if (hi)
    *hi = h;
  if (descent) {
    if (!bm || !bm->Ok())
      *descent = 1;
    else
      *descent = 0;
  }
  ALWAYSZERO(space);
  ALWAYSZERO(lspace);
  ALWAYSZERO(rspace);
}

void wxImageSnip::Draw(wxDC *dc, float x, float y, 
		       float WXUNUSED(l), float WXUNUSED(t), 
		       float WXUNUSED(r), float WXUNUSED(b),
		       float WXUNUSED(dx), float WXUNUSED(dy), 
		       int)
{
  if (!bm || !bm->Ok()) {
    dc->DrawRectangle(x + 1, y + 1, 
		      w - 2 + GC_RECT_FRAME_EXTEND, 
		      h - 2 + GC_RECT_FRAME_EXTEND);
    dc->DrawLine(x + 1, y + 1, 
		 x + w - 2 + GC_LINE_EXTEND, 
		 y + h - 2 + GC_LINE_EXTEND);
    dc->DrawLine(x + 1, y + h - 2, 
		 x + w - 2 + GC_LINE_EXTEND, 
		 y + 1 - GC_LINE_EXTEND);
    return;
  }


  dc->Blit(x, y, w, h, bm, 0, 0, wxCOPY);
  return;
}

wxSnip *wxImageSnip::Copy(void)
{
  wxImageSnip *snip;

  snip = new wxImageSnip();
  Copy(snip);

  return (wxSnip *)snip;
}

void wxImageSnip::Write(wxMediaStreamOut *f)
{
  int writeBm = 0, writePm = 0;

  f->Put((filename ? filename : (char *)""));
  if (filename)
    f->Put(filetype);
  else {
    if (!bm)
      f->Put(0);
    else if (bm->GetDepth() == 1) {
      f->Put(1);
      writeBm = 1;
    } else {
      f->Put(2);
      writePm = 1;
    }
  }
  f->Put(vieww);
  f->Put(viewh);
  f->Put(viewdx);
  f->Put(viewdy);
  f->Put(relativePath);

  /* inline the image */
  if (writeBm || writePm) {
    FILE *fi;
    char buffer[IMG_MOVE_BUF_SIZE];
    long lenpos, numlines = 0;
    char *fname;
    long end;

    lenpos = f->Tell();
    f->PutFixed(0);

    fname = wxGetTempFileName("img", NULL);

    bm->SaveFile(fname, writeBm ? wxBITMAP_TYPE_XBM : wxBITMAP_TYPE_XPM, NULL);
    
    fi = fopen(fname, "rb");
    if (fi) {
      while (1) {
	int c;
	c = fread(buffer, 1, IMG_MOVE_BUF_SIZE, fi);
	if (c) {
	  numlines++;
	  f->Put(c, buffer);
	} else 
	  break;
      }
      fclose(fi);
    }

    wxRemoveFile(fname);

    end = f->Tell();
    f->JumpTo(lenpos);
    f->PutFixed(numlines);
    f->JumpTo(end);
  }
}

void wxImageSnip::LoadFile(char *name, long type, Bool relative, Bool inlineImg)
{
  if (name && !*name)
    name = NULL;

  bm = NULL;

  if (relative && name) {
#ifdef wx_mac
    if (name[0] != ':') {
      int i;
      for (i = 0; name[i]; i++)
	if (name[i] == ':') {
	  relative = FALSE;
	  break;
	}
    }
#else
    if (name[0] == '/')
      relative = FALSE;
#ifdef wx_msw
    if (name[0] == '\\')
      relative = FALSE;
    if (name[0] && name[1] == ':')
      relative = FALSE;
#endif
#ifdef wx_x
    if (name[0] == '~')
      relative = FALSE;
#endif
#endif
  }

  relativePath = relative && name;

  if (relativePath)
    flags |= wxSNIP_USES_BUFFER_PATH;
  else if (flags & wxSNIP_USES_BUFFER_PATH)
    flags -= wxSNIP_USES_BUFFER_PATH;

  if (name) {
    char *loadname, *fn;

    loadname = name;

    if (!relativePath || (*admin_ptr)) {
      if (relativePath) {
	wxMediaBuffer *b;
	char *path;
	
	b = (*admin_ptr) ? (*admin_ptr)->GetMedia() : (wxMediaBuffer *)NULL;
	fn = b ? b->GetFilename() : (char *)NULL;
	if (fn) {
	  path = wxPathOnly(fn);
	  if (path) {
	    loadname = new WXGC_ATOMIC char[strlen(path) + strlen(name) + 2];
	    strcpy(loadname, path);
#ifdef wx_x
	    strcat(loadname, "/");
#else
#ifdef wx_mac
	    strcat(loadname, ":");
#else
	    strcat(loadname, "\\");
#endif	    
#endif
	    strcat(loadname, name);
	  }
	}
      }
      
      wxBeginBusyCursor();

      fn = (char *)wxmeExpandFilename(loadname);
      {
	wxBitmap *nbm;
	nbm = new wxBitmap(fn, type);
	bm = nbm;
      }

      wxEndBusyCursor();
      
      if (!bm->Ok()) {
	DELETE_OBJ bm;
	bm = NULL;
      }
    }

    if (!inlineImg) {
      filename = copystring(name);
      filetype = type;
    } else
      filename = NULL;
  } else {
    filename = NULL;
  }

  contentsChanged = TRUE;

  if (*admin_ptr)
    (*admin_ptr)->Resized(this, TRUE);
}

void wxImageSnip::Copy(wxImageSnip *newSnip)
{
  wxSnip::Copy(newSnip);
  
  if (filename) {
    newSnip->filename = copystring(filename);
  } else
    newSnip->filename = NULL;
  newSnip->filetype = filetype;
  newSnip->relativePath = relativePath;

  newSnip->vieww = vieww;
  newSnip->viewh = viewh;
  newSnip->viewdx = viewdx;
  newSnip->viewdy = viewdy;
  newSnip->bm = bm;
}

char *wxImageSnip::GetFilename(Bool *rel)
{
  if (rel)
    *rel = filename && relativePath;

  return filename;
}

long wxImageSnip::GetFiletype()
{
  return filename ? 0 : filetype;
}

void wxImageSnip::SetBitmap(wxBitmap *map)
{
#ifdef wx_x
  if (map->selectedTo)
    return;
#endif
#if defined(wx_mac) || defined(wx_msw)
  if (map->selectedInto)
    return;
#endif

  bm = NULL;

  if (!map->Ok())
    return;

  bm = map;

  contentsChanged = TRUE;
  
  if (*admin_ptr)
    (*admin_ptr)->Resized(this, TRUE);
}

void wxImageSnip::SetOffset(float x, float y)
{
  viewdx = x;
  viewdy = y;

  contentsChanged = TRUE;

  if (*admin_ptr)
    (*admin_ptr)->NeedsUpdate(this, 0, 0, w, h);
}

Bool wxImageSnip::Resize(float w, float h)
{
  vieww = w;
  viewh = h;

  contentsChanged = TRUE;

  if (*admin_ptr)
    (*admin_ptr)->Resized(this, TRUE);

  return TRUE;
}

long wxImageSnip::GetNumScrollSteps()
{
  long ss;

  ss = (long)(h / IMAGE_PIXELS_PER_SCROLL);

  return ss ? ss : 1;
}

long wxImageSnip::FindScrollStep(float y)
{
  return (long)(y / IMAGE_PIXELS_PER_SCROLL);
}

float wxImageSnip::GetScrollStepOffset(long i)
{
  return ((float )i) * IMAGE_PIXELS_PER_SCROLL;
}

void wxImageSnip::SetAdmin(wxSnipAdmin *a)
{
  if (PTRNE(*admin_ptr, a))
    wxSnip::SetAdmin(a);
  if (*admin_ptr && relativePath && filename)
    LoadFile(filename, filetype, TRUE);
}

/***************************************************************/

class MediaSnipClass : public wxSnipClass
{
 public:
  MediaSnipClass(void);

  virtual wxSnip *Read(wxMediaStreamIn *);
};

static MediaSnipClass *TheMediaSnipClass;

MediaSnipClass::MediaSnipClass(void)
{
  classname = "wxmedia";
  version = 3;
  required = TRUE;
}

wxSnip *MediaSnipClass::Read(wxMediaStreamIn *f)
{
  wxMediaBuffer *media;
  wxMediaSnip *snip;
  Bool border, tightFit = 0, alignTopLine = 0;
  int lm, tm, rm, bm, li, ti, ri, bi, type;
  float w, W, h, H;
  wxStandardSnipClassList *scl;

  f->Get(&type);
  f->Get(&border);
  f->Get(&lm);
  f->Get(&tm);
  f->Get(&rm);
  f->Get(&bm);
  f->Get(&li);
  f->Get(&ti);
  f->Get(&ri);
  f->Get(&bi);
  f->Get(&w);
  f->Get(&W);
  f->Get(&h);
  f->Get(&H);
  
  scl = wxGetTheSnipClassList();
  if (f->ReadingVersion(this) > 1)
    f->Get(&tightFit);
  if (f->ReadingVersion(this) > 2)
    f->Get(&alignTopLine);
  
  if (!type)
    media = NULL;
  else if (type == wxEDIT_BUFFER)
    media = wxsMakeMediaEdit();
  else
    media = wxsMakeMediaPasteboard();

  snip = wxsMakeMediaSnip(media, border, lm, tm, rm, bm, li, ti, ri, bi,
			  w, W, h, H);
  if (tightFit)
    snip->SetTightTextFit(1);
  if (alignTopLine)
    snip->SetAlignTopLine(1);
  
  if (media) {
    wxStyleList *sl;
    sl = media->GetStyleList();
    sl->Clear();
    media->ReadFromFile(f);
  } else
    snip->SetMedia(NULL);

  return snip;
}

/***************************************************************/

wxSnipClassList::wxSnipClassList(void)
: wxList((KeyType)wxKEY_STRING)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_SNIP_CLASS_LIST;
#endif
}

wxSnipClassList::~wxSnipClassList()
{
}

wxSnipClass *wxSnipClassList::Find(char *name)
{
  wxNode *node;

  node = wxList::Find(name);

  return node ? (wxSnipClass *)node->Data() : (wxSnipClass *)NULL;
}

short wxSnipClassList::FindPosition(wxSnipClass *sclass)
{
  wxNode *node;
  short i;
  
  for (i = 0, node = First(); node; node = node->Next(), i++) {
    if (PTREQ(sclass, (wxSnipClass *)node->Data()))
      return i;
  }

  return -1;
}

void wxSnipClassList::Add(wxSnipClass *snipclass)
{
  if (wxList::Find(snipclass->classname))
    return;

  Append(snipclass->classname, snipclass);
}

int wxSnipClassList::Number(void)
{
  return wxList::Number();
}

wxSnipClass *wxSnipClassList::Nth(int n)
{
  wxNode *node;
  node = wxList::Nth(n);

  if (node)
    return (wxSnipClass *)node->Data();
  else
    return NULL;
}

/***************************************************************/

wxStandardSnipClassList::wxStandardSnipClassList(void)
{
  wxList *ul;

  ul = new wxList((KeyType)wxKEY_INTEGER);
  unknowns = ul;

  Add(TheTextSnipClass);
  Add(TheTabSnipClass);
  Add(TheMediaSnipClass);
  Add(TheImageSnipClass);
}

void wxStandardSnipClassList::ResetHeaderFlags(wxMediaStream *s)
{
  s->sl = NULL;
  s->dl = NULL;
}

Bool wxStandardSnipClassList::Write(wxMediaStreamOut *f)
{
  wxNode *node;
  wxSnipClass *sclass;
  short i;

  f->Put(Number());

  for (i = 0, node = First(); node; node = node->Next(), i++) {
    wxSnipClassLink *sl;

    sclass = (wxSnipClass *)node->Data();
    f->Put(sclass->classname);
    f->Put(sclass->version);
    f->Put(sclass->required);

    sl = new wxSnipClassLink;
    sl->c= sclass;
    sl->mapPosition = i;
    sl->headerFlag = 0;
    sl->next = f->sl;
    f->sl = sl;
  }

  return TRUE;
}

Bool wxStandardSnipClassList::Read(wxMediaStreamIn *f)
{
  int count, i;
  long _n;
  wxSnipClass *sclass;
  char buffer[256];
  int version;
  Bool required;
  wxNode *node, *next;
  wxSnipClassLink *sl;

  f->Get(&count);

  buffer[255] = 0;

  for (node = unknowns->First(); node; node = next) {
    next = node->Next();
    DELETE_OBJ node;
  }

  for (i = 0; i < count; i++) {
    _n = 255;
    f->Get((long *)&_n, (char *)buffer);
    f->Get(&version);
    f->Get(&required);
    if (!f->Ok())
      return FALSE;

    sclass = Find(buffer);

    sl = new wxSnipClassLink;
    sl->c = sclass;
    sl->mapPosition = i;
    sl->next = f->sl;
    f->sl = sl;

    if (!sclass || (sclass->version < version)) {
      /* unknown class/version; remember name in case it's used */
      sl->name = copystring(buffer);
    } else {
      sl->readingVersion = version;
    }
  }

  return TRUE;
}

wxSnipClass *wxStandardSnipClassList::FindByMapPosition(wxMediaStream *f, short n)
{
  wxSnipClassLink *sl;
  
  if (n < 0)
    return NULL;

  for (sl = f->sl; sl; sl = sl->next) {
    if (sl->mapPosition == n) {
      if (sl->name) {
	/* Show error and then remove name so it isn't shown again. */
	char buffer2[256];
	sprintf(buffer2, "Unknown snip class or version: \"%.100s\".", sl->name);
	wxmeError(buffer2);

	sl->name = NULL;
      }
      return sl->c;
    }
  }

  return NULL;
}

wxStandardSnipClassList *wxMakeTheSnipClassList(void)
{
  return new wxStandardSnipClassList;
}

/***************************************************************/

wxBufferDataClass::wxBufferDataClass()
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_BUFFER_DATA_CLASS;
#endif

  classname = "wxbad";
  required = 0;
}

wxBufferData::wxBufferData()
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_BUFFER_DATA;
#endif

  next = NULL;
}

wxBufferData::~wxBufferData()
{
  if (next)
    DELETE_OBJ next;
}

class LocationBufferDataClass : public wxBufferDataClass
{
 public:
  LocationBufferDataClass();
  wxBufferData *Read(wxMediaStreamIn *);
};

LocationBufferDataClass::LocationBufferDataClass()
{
  classname = "wxloc";
  required = 1;
}

wxBufferData *LocationBufferDataClass::Read(wxMediaStreamIn *f)
{
  wxLocationBufferData *data;

  data = new wxLocationBufferData;
  f->Get(&data->x);
  f->Get(&data->y);

  return data;
}

static LocationBufferDataClass *TheLocationBufferDataClass;

wxLocationBufferData::wxLocationBufferData()
{
  x = y = 0;
  dataclass = TheLocationBufferDataClass;
}

Bool wxLocationBufferData::Write(wxMediaStreamOut *f)
{
  f->Put(x);
  f->Put(y);
  return TRUE;
}

/**************************************************************/

wxBufferDataClassList::wxBufferDataClassList(void)
: wxList((KeyType)wxKEY_STRING)
{
  wxList *ul;

#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_BUFFER_DATA_CLASS_LIST;
#endif
  
  ul = new wxList((KeyType)wxKEY_INTEGER);
  unknowns = ul;

  Add(TheLocationBufferDataClass);
}

wxBufferDataClassList::~wxBufferDataClassList()
{
}

wxBufferDataClass *wxBufferDataClassList::Find(char *name)
{
  wxNode *node;

  node = wxList::Find(name);

  return node ? (wxBufferDataClass *)node->Data() : (wxBufferDataClass *)NULL;
}

short wxBufferDataClassList::FindPosition(wxBufferDataClass *sclass)
{
  wxNode *node;
  short i;
  
  for (i = 0, node = First(); node; node = node->Next(), i++) {
    if (PTREQ(sclass, (wxBufferDataClass *)node->Data()))
      return i + 1;
  }

  return 0;
}

void wxBufferDataClassList::Add(wxBufferDataClass *dataclass)
{
  if (wxList::Find(dataclass->classname))
    return;

  Append(dataclass->classname, dataclass);
}

int wxBufferDataClassList::Number(void)
{
  return wxList::Number();
}

wxBufferDataClass *wxBufferDataClassList::Nth(int n)
{
  wxNode *o;

  o = wxList::Nth(n);

  if (!o)
    return NULL;
  else
    return (wxBufferDataClass *)o->Data();
}

Bool wxBufferDataClassList::Write(wxMediaStreamOut *f)
{
  wxNode *node;
  wxBufferDataClass *sclass;
  wxDataClassLink *dl;
  short i;

  f->Put(Number());

  for (i = 0, node = First(); node; node = node->Next(), i++) {
    sclass = (wxBufferDataClass *)node->Data();
    f->Put(sclass->classname);

    dl = new wxDataClassLink;
    dl->d = sclass;
    dl->mapPosition = i + 1;
    dl->next = f->dl;
    f->dl = dl;
  }

  return TRUE;
}

Bool wxBufferDataClassList::Read(wxMediaStreamIn *f)
{
  int _count, i;
  long _n;
  wxBufferDataClass *sclass;
  wxDataClassLink *dl;
  char buffer[256];
  
  f->Get(&_count);

  buffer[255] = 0;

  for (i = 0; i < _count; i++) {
    _n = 255;
    f->Get((long *)&_n, (char *)buffer);
    if (!f->Ok())
      return FALSE;

    sclass = Find(buffer);

    dl = new wxDataClassLink;
    dl->d = sclass;
    dl->mapPosition = i + 1;
    dl->next = f->dl;
    f->dl = dl;

    if (!sclass) {
      dl->name = copystring(buffer);
    }
  }

  return TRUE;
}

wxBufferDataClass *wxBufferDataClassList::FindByMapPosition(wxMediaStream *f, short n)
{
  wxDataClassLink *dl;
  
  if (n <= 0)
    return NULL;

  for (dl = f->dl; dl; dl = dl->next) {
    if (dl->mapPosition == n) {
      if (dl->name) {
	char buffer2[256];
	sprintf(buffer2, "Unknown snip data class or version: \"%.100s\".", dl->name);
	wxmeError(buffer2);
	dl->name = NULL;
      }
      return dl->d;
    }
  }

  return NULL;
}

wxBufferDataClassList *wxMakeTheBufferDataClassList()
{
  return new wxBufferDataClassList;
}

/**************************************************/

void wxInitSnips(void)
{
  wxREGGLOB(TheTextSnipClass);
  wxREGGLOB(TheTabSnipClass);
  wxREGGLOB(TheMediaSnipClass);
  wxREGGLOB(TheImageSnipClass);
  wxREGGLOB(TheLocationBufferDataClass);


  TheTextSnipClass = new TextSnipClass;
  TheTabSnipClass = new TabSnipClass;
  TheMediaSnipClass = new MediaSnipClass;
  TheImageSnipClass = new ImageSnipClass;
  TheLocationBufferDataClass = new LocationBufferDataClass;
}
