/*
 * File:        wx_mpriv.cc
 * Purpose:     wxMediaEdit private methods implementation
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

#define Uses_wxPrintSetup /* for wx_xt */
#define Uses_XLib /* for getting black pixel value */
#include "wx_dialg.h"
#ifndef OLD_WXWINDOWS
#include "wx_cmdlg.h"
#endif
#include "wx_utils.h"
#include "wx_dcps.h"
#include "wx_media.h"
#ifdef wx_xt
#include "wx_types.h"
#endif

#include <string.h>
#include <stdlib.h>
#include <fstream.h>
#include <ctype.h>

#include "wx_mpriv.h"

#include "wx_gcrct.h"

/* Debugging: */
#define CHECK_CONSISTENCY 0
#define LOOK_FOR_ZEROED 0
#define PLOT_SNIP_DOT 0

#define A_VERY_BIG_NUMBER 1e50

/****************************************************************/
/*                             PRIVATE                          */
/****************************************************************/

void wxMediaEdit::_SetPosition(Bool setflash, int bias, long start, long end, 
			       Bool ateol, Bool scroll, int seltype)
{
  long oldstart, oldend, sPos;
  Bool oldateol;
  wxSnip *snip;
  Bool needRefresh, needFullRefresh, changedPos;

  if (flowLocked)
    return;
  
  if (!setflash && (!flash || !flashautoreset || !flashdirectoff))
    EndStreaks(wxSTREAK_EXCEPT_DELAYED);
  
  if ((start < 0) 
      || ((end != -1) && (start > end)))
    return;

  if (end == -1)
    end = start;
  else if (end > len)
    end = len;

  if (start > len)
    start = len;

  if (ateol) {
    if (start != end)
      ateol = FALSE;
    else {
      snip = FindSnip(start, -1, &sPos);
      if (!(snip->flags & wxSNIP_NEWLINE) 
	  || (snip->flags & wxSNIP_INVISIBLE) 
	  || start != sPos + snip->count)
	ateol = FALSE;
    }
  }

  if (flash) {
    oldstart = flashstartpos;
    oldend = flashendpos;
    oldateol = flashposateol;
  } else {
    oldstart = startpos;
    oldend = endpos;
    oldateol = posateol;
  }
  
  if (!setflash && flash && flashautoreset) {
    flash = FALSE;
    if (flashTimer) {
      flashTimer->Stop();
      delete flashTimer;
      flashTimer = NULL;
    }
  }

  if (start == oldstart && end == oldend && ateol == oldateol)
    needRefresh = changedPos = FALSE;
  else {
    needRefresh = changedPos = TRUE;

    if (setflash) {
      flashstartpos = start;
      flashendpos = end;
      flashposateol = ateol;
    } else {
#if ALLOW_X_STYLE_SELECTION
      if (start == end || wxMediaXSelectionAllowed != this 
	  || (seltype == wxLOCAL_SELECT)) {
	if (!delayRefresh || needXCopy) {
	  needXCopy = FALSE;
	  CopyOutXSelection();
	}
      }
#endif

      CheckMergeSnips(startpos);
      CheckMergeSnips(endpos);
    
      caretStyle = NULL;
      startpos = start;
      endpos = end;
      posateol = ateol;
    }
  }

  needFullRefresh = FALSE;
#if ALLOW_X_STYLE_SELECTION
  if (!setflash && wxMediaXSelectionMode) {
    if ((seltype != wxLOCAL_SELECT) 
	&& start != end 
	&& this != wxMediaXSelectionOwner) {
      if (OwnXSelection(TRUE, FALSE, seltype == wxX_SELECT)) {
	needFullRefresh = TRUE;
	needRefresh = TRUE;
      }
    } else if ((start == end 
		|| wxMediaXSelectionAllowed != this 
		|| (seltype == wxLOCAL_SELECT))
	       && this == wxMediaXSelectionOwner) {
      if (OwnXSelection(FALSE, FALSE, FALSE)) {
	needFullRefresh = TRUE;
	needRefresh = TRUE;
      }
    }
  }
#endif

  if (setflash)
    flash = TRUE;

  if (scroll) {
    long scrollStart, scrollEnd;

    if (bias < -1) {
      scrollStart = scrollEnd = start;
      bias = 0;
    } else if (bias > 1) {
      scrollStart = scrollEnd = end;
      bias = 0;
    } else {
      scrollStart = start;
      scrollEnd = end;
    }
  
    if (ScrollToPosition(scrollStart, posateol, TRUE, scrollEnd, bias))
      needRefresh = FALSE;
  }

  if (needRefresh) {
    if (hiliteOn && admin && (admin->standard > 0) && !delayRefresh 
	&& oldstart == oldend && start == end && caretOn
	&& caretLocationX >= 0 && !flash) {
      /* Try to take a shortcut */
      if (CaretOff()) {
	/* Shortcut works */
	caretLocationX = -1;
	CaretOn();
	needRefresh = FALSE;
      }
    }

    if (needRefresh) {
      if (start >= oldend || end <= oldstart || needFullRefresh) {
	/* No overlap: */
	NeedRefresh(oldstart, oldend);
	NeedRefresh(start, end);
      } else {
	if (start < oldstart)
	  NeedRefresh(start, oldstart);
	if (oldstart < start)
	  NeedRefresh(oldstart, start);
	if (end < oldend)
	  NeedRefresh(end, oldend);
	if (oldend < end)
	  NeedRefresh(oldend, end);
      }
    }
  }

  if (changedPos && !setflash)
    AfterSetPosition();
}

void wxMediaFlashTimer::Notify(void)
{
  media->FlashOff();
}

/**********************************************************************/

void wxMediaEdit::_ChangeStyle(long start, long end, 
			       wxStyle *newStyle, wxStyleDelta *delta)
{
  wxSnip *gsnip, *startSnip, *endSnip;
  wxStyleChangeRecord *rec;
  wxStyle *style, *style2;
  long p;
  int something;

  if (writeLocked || userLocked)
    return;

  if (newStyle && (styleList->StyleToIndex(newStyle) < 0))
    return;

  if (start < 0)
    start = 0;
  if (start > len)
    start = len;
  if (end > len)
    end = len;
  if (start > end)
    return;

  if (!newStyle && !delta)
    newStyle = styleList->FindNamedStyle(STD_STYLE);

  if ((startpos == start) && (endpos == end) && (start == end) && len) {
    if (newStyle)
      caretStyle = newStyle;
    else {
      gsnip = FindSnip(start, -1);
      caretStyle = styleList->FindOrCreateStyle(gsnip->style, delta);
    }
    return;
  }

  writeLocked = TRUE;

  if (!OnChangeStyle(start, end - start))
    goto give_up;
  
  flowLocked = TRUE;

  MakeSnipset(start, end);
  
  if (!len) {
    startSnip = snips;
    endSnip = NULL;
  } else {
    startSnip = FindSnip(start, +1);
    endSnip = FindSnip(end, +2);
  }

  if (!noundomode)
    rec = new wxStyleChangeRecord(start, end, delayedStreak || !modified);
  else
    rec = NULL;

  something = FALSE;

  p = start;
  for (gsnip = startSnip; PTRNE(gsnip, endSnip); gsnip = gsnip->next) {
    style = gsnip->style;

    if (newStyle)
      style2 = newStyle;
    else
      style2 = styleList->FindOrCreateStyle(style, delta);

    if (PTRNE(style, style2)) {
      gsnip->style = style2;
      if (rec)
	rec->AddStyleChange(p, p + gsnip->count, style);

      gsnip->SizeCacheInvalid();
      gsnip->line->MarkRecalculate();
      if (maxWidth > 0)
	gsnip->line->MarkCheckFlow();

      something = TRUE;
    }

    p += gsnip->count;
  }

  if (something) {
    if (startSnip->line->prev
	&& !(startSnip->line->prev->lastSnip->flags & wxSNIP_HARD_NEWLINE))
      startSnip->line->prev->MarkCheckFlow();
    
    if (!modified)
      AddUndo(new wxUnmodifyRecord);
    if (rec)
      AddUndo(rec);
    if (delayRefresh)
      delayedStreak = TRUE;
    
    CheckMergeSnips(start);
    CheckMergeSnips(end);
    
    if (!modified)
      SetModified(TRUE);

    writeLocked = FALSE;
    flowLocked = FALSE;

    RefreshByLineDemand();
  } else {
    if (rec)
      delete rec;
    writeLocked = FALSE;
    flowLocked = FALSE;

    CheckMergeSnips(start);
    CheckMergeSnips(end);
  }

  AfterChangeStyle(start, end - start);

  return;

 give_up:
  writeLocked = FALSE;
  flowLocked = FALSE;

  return;
}

/****************************************************************/

void wxMediaEdit::SettingAdmin(wxMediaAdmin * /* newadmin */)
{
  caretLocationX = -1;
}

void wxMediaEdit::InitNewAdmin(void)
{
  Redraw();
}

void wxMediaEdit::EndStreaks(int exception)
{
  if (map && (exception != wxSTREAK_KEY_SEQUENCE))
    map->BreakSequence();
  if (flash && flashautoreset && !flashdirectoff)
    FlashOff();

  typingStreak = FALSE;
  deletionStreak = FALSE;
  vcursorStreak = FALSE;  
  if (anchorStreak && !keepAnchorStreak)
    SetAnchor(FALSE);

  if (exception != wxSTREAK_EXCEPT_DELAYED)
    delayedStreak = FALSE;

  killStreak = FALSE;

  prevPasteStart = -1;
}


/****************************************************************/

long wxMediaEdit::_FindPositionInLine(Bool internal, long i, float x, 
				      Bool *ateol, Bool *onit,
				      float *how_close)
{
  float w, X, topy;
  wxSnip *snip;
  wxDC *dc;
  long p, sPos;
  wxMediaLine *line;
  Bool atsnipend;

  if (onit)
    *onit = FALSE;
  if (how_close)
    *how_close = 100;

  if (!internal && !CheckRecalc(TRUE, FALSE))
    return 0;

  if (i < 0)
    return 0;
  if (i >= numValidLines)
    return len;

  line = lineRoot->FindLine(i);

  if (ateol)
    *ateol = FALSE;

  if (x <= 0)
    return FindFirstVisiblePosition(line);

  p = line->GetPosition();

  if (x >= line->w) {
    /* snip == the last one */
    snip = line->lastSnip;
    sPos = p + line->len - snip->count;
    p += line->len;
  } else {
    if (onit)
      *onit = TRUE;

    dc = admin->GetDC();
    if (!dc)
      return 0;
    
    X = 0;

    Bool wl = writeLocked, fl = flowLocked;
    writeLocked = TRUE;
    flowLocked = TRUE;

    /* linear seach for snip */
    snip = NULL;
    topy = line->GetLocation();
    while(1) {
      snip = snip ? snip->next : line->snip;

      snip->GetExtent(dc, X, topy, &w);

      if (x > w && snip->next) {
	x -= w;
	X += w;
	p += snip->count;
      } else {
	/* Found the right snip */
	sPos = p;
	p += _FindPositionInSnip(dc, X, topy, snip, x, how_close);
	break;
      }
    }

    writeLocked = wl;
    flowLocked = fl;
  }

  /* Back up over invisibles */
  atsnipend = (p - sPos == snip->count);
  if (atsnipend)
    FindLastVisiblePosition(line, &p, &snip);

  if (ateol && atsnipend && snip && PTREQ(snip, line->lastSnip))
    *ateol = TRUE;

  return p;
}

long wxMediaEdit::FindFirstVisiblePosition(wxMediaLine *line, wxSnip *snip)
{
  wxSnip *nextSnip;
  long p, startp;

  if (readLocked)
    return 0;

  if (!snip)
    snip = line->snip;

  p = startp = line->GetPosition();
  nextSnip = line->lastSnip->next;

  while (PTRNE(snip, nextSnip)) {
    if (snip->flags & wxSNIP_INVISIBLE)
      p += snip->count;
    else
      break;
    snip = snip->next;
  }

  if (PTREQ(snip, nextSnip)) {
    /* If everything is invisible, then presumably the CR is forced,
       so go to the beginning of the line anyway */
    p = startp;
  }

  return p;
}

void wxMediaEdit::FindLastVisiblePosition(wxMediaLine *line, long *p, 
					  wxSnip **snipP)
{
  if (readLocked)
    return;

  wxSnip *snip;
  
  snip = snipP ? *snipP : (wxSnip *)NULL;

  if (!snip)
    snip = line->lastSnip;

  do {
    if (snip->flags & wxSNIP_INVISIBLE) {
      *p -= snip->count;
      if (PTRNE(snip, line->snip))
	snip = snip->prev;
    }
  } while ((snip->flags & wxSNIP_INVISIBLE) && PTRNE(snip, line->snip));

  if (snipP)
    *snipP = snip;
}

/****************************************************************/

long wxMediaEdit::_FindStringAll(char *str, int direction, 
				 long start, long end,
				 long **positions, Bool justOne,
				 Bool bos, Bool caseSens)
{
  wxSnip *snip;
  char text[256], *oldStr, c;
  long *smap;
  long sPos, p, n, thistime, thisoffset, need, checked, offset, shorten, i;
  long slen, s, sbase, beyond, sgoal, totalCount, allocFound, foundCount;

  if (!direction)
    direction = 1;
  if (direction < -1)
    direction = -1;
  else if (direction > 1)
    direction = 1;

  if (start < 0)
    start = startpos;
  if (end < 0) {
    if (direction < 0)
      end = 0;
    else
      end = len;
  }
  if (start > len)
    start = len;
  if (end > len)
    end = len;

  if (direction < 0)
    totalCount = start - end;
  else
    totalCount = end - start;
  if (totalCount < 0)
    return -1;

  slen = strlen(str);
  if (!slen)
    return -1;

  if (!caseSens) {
    oldStr = str;
    str = new char[slen + 1];
    for (i = 0; i < slen; i++)
      str[i] = tolower(oldStr[i]);
    str[i] = 0;
  }
  
  snip = FindSnip(start, direction, &sPos);
  if (!snip)
    return -1;

  if (direction > 0) {
    offset = start - sPos;
    shorten = 0;
    sbase = 0;
    beyond = -1;
    sgoal = slen;
  } else {
    shorten = (sPos + snip->count) - start;
    offset = 0;
    sbase = slen - 1;
    beyond = slen;
    sgoal = -1;
  }

  smap = new long[slen];

  smap[sbase] = beyond;
  s = beyond;
  for (i = sbase + direction; i != sgoal; i += direction) {
    while ((s != beyond) && (str[s + direction] != str[i]))
      s = smap[s];
    if (str[s + direction] == str[i])
      s += direction;
    smap[i] = s;
  }

  s = beyond;

  if (!justOne) {
    allocFound = 10;
    *positions = new long[allocFound];
    foundCount = 0;
  } else
    allocFound = foundCount = 0;

  while (snip && totalCount) {
    need = snip->count - shorten - offset;
    if (need > totalCount) {
      if (direction < 0)
	offset += (need - totalCount);
      need = totalCount;
    }

    checked = 0;

    totalCount -= need;

    do {
      thistime = need;
      if (thistime > 255)
	thistime = 255;
      need -= thistime;

      thisoffset = offset + ((direction < 0) ? need : checked);

      Bool wl = writeLocked, fl = flowLocked;
      writeLocked = TRUE;
      flowLocked = TRUE;
      
      snip->GetText((char *)text, thisoffset, thistime);

      writeLocked = wl;
      flowLocked = fl;

      text[thistime] = 0;
      
      i = (direction > 0) ? 0 : thistime - 1;
      n = thistime;
      while(n--) {
	c = text[i];
	if (!caseSens)
	  if (c >= 'A' && c <= 'Z')
	    c += ('a' - 'A');
	while ((s != beyond) && (str[s + direction] != c))
	  s = smap[s];
	if (str[s + direction] == c) {
	  s += direction;
	  if (s + direction == sgoal) {
	    p = sPos + i + thisoffset;
	    if (bos) {
	      if (direction < 0)
		p += slen;
	      else
		p -= (slen - 1);
	    } else if (direction > 0)
	      p++;
	    if (justOne)
	      goto search_done;
	    else {
	      if (foundCount == allocFound) {
		long *old = *positions, oldCount = allocFound;

		allocFound *= 2;
		*positions = new long[allocFound];

		memcpy(*positions, old, oldCount * sizeof(long));
	      }
	      (*positions)[foundCount++] = p;
	      s = beyond;
	    }
	  }
	}
	i += direction;
      }

      checked += thistime;
    } while(need);

    if (direction > 0)
      sPos += snip->count;
    snip = (direction > 0) ? snip->next : snip->prev;
    if (snip) {
      if (direction < 0)
	sPos -= snip->count;
    }
    offset = shorten = 0;
  }

  p = -1;

 search_done:
  delete[] smap;

  return justOne ? p : foundCount;
}

/****************************************************************/

void wxMediaEdit::MakeOnlySnip(void)
{
  snips = new wxTextSnip();
  snips->style = styleList->FindNamedStyle(STD_STYLE);
#if CHECK_CONSISTENCY
  if (!snips->style)
    fprintf(stderr, "NULL style for STD_STYLE!\n");
#endif
  snips->count = 0;
  snips->SetAdmin(snipAdmin);
  snips->prev = NULL;
  snips->next = NULL;

  snips->line = lineRoot = firstLine = lastLine = new wxMediaLine;

  lineRoot->snip = lineRoot->lastSnip = snips;

  lastSnip = snips;
  snipCount = 1;

  numValidLines = 1;
}

void wxMediaEdit::SpliceSnip(wxSnip *snip, wxSnip *prev, wxSnip *next)
{
  if (prev)
    prev->next = snip;
  else
    snips = snip;
  snip->prev = prev;
  snip->next = next;
  if (next)
    next->prev = snip;
  else
    lastSnip = snip;
}

void wxMediaEdit::InsertSnip(wxSnip *before, wxSnip *snip)
{
  if (PTREQ(snips, lastSnip) && !snips->count) {
    AppendSnip(snip);
  } else {
    SpliceSnip(snip, before->prev, before);
    snipCount++;
  }
}

void wxMediaEdit::AppendSnip(wxSnip *snip)
{
  if (PTREQ(snips, lastSnip) && !snips->count) {
    /* Get rid of empty snip */
    delete snips;
    snips = lastSnip = snip;
  } else {
    SpliceSnip(snip, lastSnip, NULL);
    snipCount++;
  }
}

void wxMediaEdit::DeleteSnip(wxSnip *snip)
{
  if (snip->next)
    SpliceSnip(snip->next, snip->prev, snip->next->next);
  else if (snip->prev)
    SpliceSnip(snip->prev, snip->prev->prev, snip->next);
  else {
    lastSnip = snips = NULL;
  }
  --snipCount;
  snip->flags += wxSNIP_CAN_DISOWN;
  SnipSetAdmin(snip, NULL);
  snip->line = NULL;
  snip->prev = snip->next = NULL;
  snip->flags -= wxSNIP_CAN_DISOWN;
}

wxSnip *wxMediaEdit::SnipSetAdmin(wxSnip *snip, wxSnipAdmin *a)
{
  wxSnipAdmin *orig_admin = snip->GetAdmin();
  long orig_count = snip->count;
  wxMediaLine *line = snip->line;

  Bool wl = writeLocked, fl = flowLocked;
  readLocked = writeLocked = flowLocked = TRUE;
  snip->SetAdmin(a);
  readLocked = FALSE; writeLocked = wl; flowLocked = fl;

  if (snip->GetAdmin() != a) {
    /* Something went wrong. */
    if (!a && (snip->GetAdmin() == orig_admin)) {
      /* Force admin to NULL. */
      snip->wxSnip::SetAdmin(NULL);
    } else if (a) {
      /* Snip didn't accept membership into this buffer. Give up on it. */
      wxSnip *naya = new wxSnip();
      naya->count = orig_count;
      SpliceSnip(naya, snip->prev, snip->next);
      naya->line = line;

      if (line) {
	if (line->snip == snip)
	  line->snip = naya;
	if (line->lastSnip == snip)
	  line->lastSnip = naya;
      }

      snip->wxSnip::SetAdmin(NULL);

      naya->SetAdmin(a);
      snip = naya;
    }
  }

  /* Force count to be consistent: */
  if (a && (snip->count != orig_count))
    snip->count = orig_count;

  return snip;
}

void wxMediaEdit::SnipSplit(wxSnip *snip, long pos, wxSnip **a_ptr, wxSnip **b_ptr)
{
  int c = snip->count;
  wxSnip *a, *b;

  snip->flags |= wxSNIP_CAN_SPLIT;
  wxSnip *orig = snip;

  DeleteSnip(snip);
  orig->flags -= wxSNIP_OWNED;

  Bool wl = writeLocked, fl = flowLocked;
  readLocked = writeLocked = flowLocked = TRUE;
  *a_ptr = NULL;
  *b_ptr = NULL;
  snip->Split(pos, a_ptr, b_ptr);
  readLocked = FALSE; writeLocked = wl; flowLocked = fl;

  a = *a_ptr;
  b = *b_ptr;

  if (!a)
    a = new wxSnip();
  if (!b)
    b = new wxSnip();

  if (a->IsOwned()) {
    /* uh-oh: make up a dummy */
     a = new wxSnip();
  }
  if (b->IsOwned()) {
    /* uh-oh: make up a dummy */
     b = new wxSnip();
  }

  *a_ptr = a;
  *b_ptr = b;

  if (a->flags & wxSNIP_CAN_SPLIT)
    a->flags -= wxSNIP_CAN_SPLIT;
  if (b->flags & wxSNIP_CAN_SPLIT)
    b->flags -= wxSNIP_CAN_SPLIT;
  if (orig->flags & wxSNIP_CAN_SPLIT)
    orig->flags -= wxSNIP_CAN_SPLIT;

  /* Make *sure* that count is right */
  a->count = pos;
  b->count = c - pos;
}

/****************************************************************/

wxSnip *wxMediaEdit::FindSnip(long p, int direction, long *sPos)
{
  wxSnip *snip;
  wxMediaLine *line;
  long pos;

  if ((direction < -1) && !p)
    return NULL;

  line = lineRoot->FindPosition(p);
  pos = line->GetPosition();
  p -= pos;
  if (sPos)
    *sPos = pos;

  snip = line->snip;
  if (!p && snip->prev) {
    /* Back up one: */
    snip = snip->prev;
    p += snip->count;
    if (sPos)
      *sPos -= snip->count;
  }

  for (; snip; snip = snip->next) {
    p -= snip->count;
    if ((!direction && !p)
	|| ((direction < 0) && (p <= 0))
	|| ((direction > 0) && (p < 0)))
      return snip;

    if (!direction && (p < 0))
      return NULL;

    if (sPos)
      *sPos += snip->count;
  }

  if (direction < 2)
    return lastSnip;
  else
    return NULL;
}

void wxMediaEdit::MakeSnipset(long start, long end) 
{
  long sPos;
  wxSnip *prev, *next, *snip, *insSnip;
  wxStyle *style;
  wxMediaLine *line;
  Bool atStart, atEnd;

  if (start) {
    snip = FindSnip(start, +1, &sPos);
    if (start != sPos) {
      line = snip->line;
      prev = snip->prev; next = snip->next;
      style = snip->style;
      
      atStart = PTREQ(line->snip, snip);
      atEnd = PTREQ(line->lastSnip, snip);
      
      SnipSplit(snip, start - sPos, &insSnip, &snip);
      
      snip->style = insSnip->style = style;
      
      snip->line = insSnip->line = line;
      if (atStart)
	line->snip = insSnip;
      if (atEnd)
	line->lastSnip = snip;
      
      SpliceSnip(snip, prev, next);
      snipCount++;
      InsertSnip(snip, insSnip);
      
      SnipSetAdmin(snip, snipAdmin);
      SnipSetAdmin(insSnip, snipAdmin);
    }
  }

  if (end) {
    snip = FindSnip(end, -1, &sPos);
    if (end != sPos + snip->count) {
      line = snip->line;
      prev = snip->prev; next = snip->next;
      style = snip->style;
      
      atStart = PTREQ(line->snip, snip);
      atEnd = PTREQ(line->lastSnip, snip);
      
      SnipSplit(snip, end - sPos, &insSnip, &snip);
      
      snip->style = insSnip->style = style;
      
      snip->line = insSnip->line = line;
      if (atStart)
	line->snip = insSnip;
      if (atEnd)
	line->lastSnip = snip;
      
      SpliceSnip(snip, prev, next);
      snipCount++;
      InsertSnip(snip, insSnip);
      
      SnipSetAdmin(snip, snipAdmin);
      SnipSetAdmin(insSnip, snipAdmin);
    }
  }
}

wxTextSnip *wxMediaEdit::InsertTextSnip(long start, wxStyle *style)
{
  long sPos;
  wxSnip *gsnip, *insGsnip, *prev, *next;
  wxTextSnip *snip;
  wxStyle *gstyle;
  wxMediaLine *line;
  Bool atStart, atEnd;

  snip = OnNewTextSnip();
  if (snip->IsOwned() || snip->count) {
    /* Uh-oh. Resort to wxTextSnip() */
    snip = new wxTextSnip();
  }
  snip->style = (style ? style : styleList->FindNamedStyle(STD_STYLE));
  wxSnip *rsnip = SnipSetAdmin(snip, snipAdmin);
  if (rsnip != snip) {
    /* Uh-oh. Resort to wxTextSnip() */
    snip = new wxTextSnip();
    snip->style = (style ? style : styleList->FindNamedStyle(STD_STYLE));
    snip->SetAdmin(snipAdmin);
  }
  snip->count = 0;

  gsnip = FindSnip(start, +2, &sPos);
  if (!gsnip) {
    AppendSnip(snip);
    snip->line = lastLine;
    if (PTREQ(lastLine->snip, lastSnip))
      lastLine->snip = lastLine->lastSnip = snip;
    else
      lastLine->lastSnip = snip;
  } else if (start == sPos) {
    InsertSnip(gsnip, snip);
    snip->line = gsnip->line;
    if (PTREQ(snip->line->snip, gsnip))
      snip->line->snip = snip;
  } else {
    prev = gsnip->prev; next = gsnip->next;
    gstyle = gsnip->style;
    line = gsnip->line;

    atStart = PTREQ(line->snip, gsnip);
    atEnd = PTREQ(line->lastSnip, gsnip);

    SnipSplit(gsnip, start - sPos, &insGsnip, &gsnip);
    
    gsnip->style = insGsnip->style = gstyle;

    gsnip->line = insGsnip->line = snip->line = line;
    if (atStart)
      line->snip = insGsnip;
    if (atEnd)
      line->lastSnip = gsnip;

    SpliceSnip(gsnip, prev, next);
    snipCount++;
    InsertSnip(gsnip, snip);
    InsertSnip(snip, insGsnip);

    SnipSetAdmin(gsnip, snipAdmin);
    SnipSetAdmin(insGsnip,snipAdmin);
  }

  return snip;
}

void wxMediaEdit::CheckMergeSnips(long start)
{
  wxSnip *snip1, *snip2, *prev, *next;
  long sPos1, sPos2, c;

  wxMediaLine *line;
  Bool atStart, atEnd;

  snip1 = FindSnip(start, -1, &sPos1);
  snip2 = FindSnip(start, +1, &sPos2);

  if (PTRNE(snip1, snip2)) {
    if (snip1->snipclass 
#if USE_OLD_TYPE_SYSTEM
	&& (snip1->__type == snip2->__type)
#endif
	&& PTREQ(snip1->snipclass, snip2->snipclass)
	&& PTREQ(snip1->style, snip2->style)) {
      if (!(snip1->flags & wxSNIP_NEWLINE)
	  && (snip1->flags & wxSNIP_CAN_APPEND) 
	  && (snip2->flags & wxSNIP_CAN_APPEND)
	  && (snip1->count + snip2->count < MAX_COUNT_FOR_SNIP)) {
	if (!snip1->count) {
	  if (PTREQ(snip1->line->snip, snip1))
	    snip1->line->snip = snip2;
	  DeleteSnip(snip1);
	  snip1->flags -= wxSNIP_OWNED;
	  CheckMergeSnips(start);
	} else if (!snip2->count) {
	  if (PTREQ(snip1->line->lastSnip, snip2)) {
	    snip1->line->lastSnip = snip1;
	    snip1->line->MarkRecalculate(); // need lastW updated
	    graphicMaybeInvalid = TRUE;
	  }
	  DeleteSnip(snip2);
	  snip2->flags -= wxSNIP_OWNED;
	  CheckMergeSnips(start);
	} else {
	  wxSnip *naya;

	  c = snip1->count + snip2->count;
	  prev = snip1->prev;
	  next = snip2->next;
	  line = snip1->line;
	  atStart = PTREQ(line->snip, snip1);
	  atEnd = PTREQ(line->lastSnip, snip2);
	  snip2->flags |= wxSNIP_CAN_SPLIT;

	  Bool wl = writeLocked, fl = flowLocked;
	  readLocked = writeLocked = flowLocked = TRUE;
	  naya = snip2->MergeWith(snip1);
	  readLocked = FALSE; writeLocked = wl; flowLocked = fl;

	  if (naya) {
	    if (snip1->flags & wxSNIP_CAN_SPLIT)
	      snip1->flags -= wxSNIP_CAN_SPLIT;
	    if (snip2->flags & wxSNIP_CAN_SPLIT)
	      snip2->flags -= wxSNIP_CAN_SPLIT;

	    /* Claim snip1 & snip2 unowned for naya test: */
	    snip1->flags -= wxSNIP_OWNED;
	    snip2->flags -= wxSNIP_OWNED;
	    if (naya->IsOwned()) {
	      /* Uh-oh. Make dummy */
	      naya = new wxSnip();
	    }
	    if (naya->flags & wxSNIP_CAN_SPLIT)
	      naya->flags -= wxSNIP_CAN_SPLIT;
	    snip1->flags += wxSNIP_OWNED;
	    snip2->flags += wxSNIP_OWNED;

	    DeleteSnip(snip1);
	    snip1->flags -= wxSNIP_OWNED;
	    DeleteSnip(snip2);
	    snip2->flags -= wxSNIP_OWNED;

	    SpliceSnip(naya, prev, next);
	    snipCount++;
	    /* Make *sure* that count is right */
	    naya->count = c;

	    naya = SnipSetAdmin(naya, snipAdmin);

	    naya->line = line;
	    if (atStart)
	      line->snip = naya;
	    if (atEnd) {
	      line->lastSnip = naya;
	      line->MarkRecalculate(); // need lastW updated
	      graphicMaybeInvalid = TRUE;
	    }
	  } else if (snip2->flags & wxSNIP_CAN_SPLIT)
	    snip2->flags -= wxSNIP_CAN_SPLIT;
	}
      }
    }
  }
}

wxTextSnip *wxMediaEdit::OnNewTextSnip()
{
  return new wxTextSnip();
}

wxTabSnip *wxMediaEdit::OnNewTabSnip()
{
  return new wxTabSnip();
}

/****************************************************************/

Bool wxMediaEdit::GetSnipPositionAndLocation(wxSnip *thesnip, long *pos, 
					     float *x, float *y)
{
  wxSnip *snip;
  long p;

  if (!CheckRecalc(x || y, FALSE))
    return FALSE;

  if (!thesnip->line || PTRNE(thesnip->line->GetRoot(), lineRoot))
    return FALSE;

  if (pos || x || y) {
    p = thesnip->line->GetPosition();
    
    for (snip = thesnip->line->snip; PTRNE(snip, thesnip); snip = snip->next)
      p += snip->count;
    
    if (pos)
      *pos = p;
    
    if (x || y) 
      PositionLocation(p, x, y);
  }

  return TRUE;
}

Bool wxMediaEdit::GetSnipLocation(wxSnip *thesnip, float *x, float *y, Bool bottomRight)
{
  float lx, ly;

  if (bottomRight) {
    if (!x)
      x = &lx;
    if (!y)
      y = &ly;
  }

  if (GetSnipPositionAndLocation(thesnip, NULL, x, y)) {
    if (bottomRight) {
      wxDC *dc = admin->GetDC();
      float w, h;
      
      Bool wl = writeLocked, fl = flowLocked;
      writeLocked = TRUE;
      flowLocked = TRUE;
      
      thesnip->GetExtent(dc, *x, *y, &w, &h);

      writeLocked = wl;
      flowLocked = fl;

      *x += w;
      *y += h;
    }
     
    return TRUE;
  } else
    return FALSE;
}

long wxMediaEdit::GetSnipPosition(wxSnip *thesnip)
{
  long pos;

  if (GetSnipPositionAndLocation(thesnip, &pos))
    return pos;
  else
    return -1;
}

/****************************************************************/

void wxMediaEdit::AdjustClickbacks(long start, long end, 
				   long d, wxDeleteRecord *rec)
{
  wxNode *node, *next;
  wxClickback *click;
  Bool deleteit;

  for (node = clickbacks->First(); node; node = next) {
    next = node->Next();
    click = (wxClickback *)node->Data();
    deleteit = FALSE;
    if (click->start >= start && click->end <= end) {
      deleteit = TRUE;
    } else if (click->start >= end) {
      click->start += d;
      click->end += d;
    } else if (click->start <= start && click->end > end)
      click->end += d;
    else if (click->start > start && click->end > end) {
      click->start = start;
      click->end += d;
    }

    if (click->end == click->start)
      deleteit = TRUE;
    if (deleteit) {
      clickbacks->DeleteNode(node);
      if (rec)
	rec->AddClickback(click);
      else
	delete click;
    }
  }
}

wxClickback *wxMediaEdit::FindClickback(long start)
{
  wxNode *node;
  wxClickback *click;

  for (node = clickbacks->First(); node; node = node->Next()) {
    click = (wxClickback *)node->Data();
    if (click->start <= start && click->end > start)
      return click;
  }

  return NULL;
}

void wxMediaEdit::SetClickbackHilited(wxClickback *click, Bool on)
{
  if (on != click->hilited) {
    if (on) {
      interceptmode = TRUE;
      intercepted = new wxList();
      
      BeginEditSequence();
      FlashOn(click->start, click->end, FALSE, FALSE, -1);
      ChangeStyle(click->delta, click->start, click->end);
      EndEditSequence();

      click->unhilite = intercepted;
      interceptmode = FALSE;
    } else {
      PerformUndoList(click->unhilite);

      wxNode *node;
      for (node = click->unhilite->First(); node; node = node->Next())
	delete (wxChangeRecord *)node->Data();
  
      delete click->unhilite;
      FlashOff();
    }
    click->hilited = on;
  }
}

/****************************************************************/

Bool wxMediaEdit::CheckRecalc(Bool need_graphic, Bool need_write, Bool no_display_ok)
{
  if (readLocked)
    return FALSE;
  if (writeLocked && need_write)
    return FALSE;

  if (need_graphic) {
    if (!admin)
      return FALSE;

    if (graphicMaybeInvalid) {
      if (flowLocked)
	return FALSE;

      wxDC *dc;

      dc = admin->GetDC();
      if (!dc) {
	if (no_display_ok)
	  return TRUE;
	return FALSE;
      }
      
      RecalcLines(dc, need_graphic);
    }
  }

  return TRUE;
}

Bool wxMediaEdit::CheckFlow(float maxw, wxDC *dc, float Y, 
			    long startp, wxSnip *start)
  /* This method is called with writeLocked and flowLocked already TRUE */
{
  long p, c, origc, b;
  float totalWidth, w;
  wxSnip *snip;
  Bool checkingUnderflow; // no overflow => check move up from next line
  Bool checkingUnderflowAtNext;
  Bool noChangeIfEndOfSnip, noChangeIfStartOfSnip;
  Bool theFirstSnip, firstUnderflow, hadNewline;

  totalWidth = 0;
  p = startp;
  checkingUnderflow = FALSE; // start by insuring no overflow
  checkingUnderflowAtNext = FALSE;
  noChangeIfEndOfSnip = TRUE; // Because an immediate overflow can't be helped
  noChangeIfStartOfSnip = FALSE;
  theFirstSnip = TRUE;
  firstUnderflow = FALSE;

  for (snip = start; 
       snip && !(snip->flags & wxSNIP_HARD_NEWLINE); 
       snip = snip->next) {

    if (!checkingUnderflow) {
      if ((checkingUnderflow = checkingUnderflowAtNext))
	firstUnderflow = TRUE;
    }
    noChangeIfStartOfSnip = noChangeIfEndOfSnip;

    if (snip->flags & wxSNIP_NEWLINE) {
      noChangeIfEndOfSnip = !checkingUnderflow;
      snip->flags -= wxSNIP_NEWLINE;
      checkingUnderflowAtNext = TRUE;
      hadNewline = TRUE;
    } else {
      noChangeIfEndOfSnip = FALSE;
      checkingUnderflowAtNext = FALSE;
      hadNewline = FALSE;
    }

    snip->GetExtent(dc, totalWidth, Y, &w);
    totalWidth += w;
    if (totalWidth > maxw) {
      totalWidth -= w;
      /* Get best breaking position: (0.1 is hopefully a positive value smaller than any character) */
      origc = _FindPositionInSnip(dc, totalWidth, Y, snip, maxw - totalWidth - 0.1);

      /* get legal breaking position before optimal: */
      b = p + origc + 1;
      FindWordbreak(&b, NULL, wxBREAK_FOR_LINE);
      c = b - p;
      if (c > origc)
	c = origc;

      if (c <= 0) {
	if ((b <= startp) && checkingUnderflow && origc) {
	  /* The word was currently force-broken; shift some part to here */
	  p = p + origc;
	} else if ((checkingUnderflow && firstUnderflow
		    && (b <= startp || c>= 0))
		   || (!theFirstSnip 
		       && (!c || (!origc && (c < 0) && (b <= startp))))) {
	  /* Can't fit this snip in the line */
	  if (snip->prev)
	    snip->prev->flags |= wxSNIP_NEWLINE;
	  if (hadNewline && snip->next)
	    snip->flags |= wxSNIP_NEWLINE;
	  if (noChangeIfStartOfSnip && (!hadNewline || snip->next))
	    return FALSE;
	  refreshAll = TRUE;
	  return TRUE;
	} else if ((c < 0) && (b > startp)) {
	  /* Overflow, but previous wordbreak was before this snip */
	  p = b;
	} else {
	  /* Overflow: we have to break the word anyway */
	  if (!origc) {
	    if ((snip->count == 1) 
		&& snip->next
		&& (snip->next->flags & wxSNIP_HARD_NEWLINE)) {
	      /* don't insert a break before a real newline */
	      break;
	    } else
	      p++;
	  } else
	    p += origc;
	}
      } else {
	p = p + c;
      }
      MakeSnipset(p, p);
      snip = FindSnip(p, -1);
      if (snip->next)
	snip->flags |= wxSNIP_NEWLINE;
      refreshAll = TRUE;
      return TRUE;
    }
    
    p += snip->count;

    theFirstSnip = FALSE;
    firstUnderflow = FALSE;
  }

  if (!snip 
      && (lastSnip->flags & wxSNIP_NEWLINE)
      && !(lastSnip->flags & wxSNIP_HARD_NEWLINE)) {
    lastSnip->flags -= wxSNIP_NEWLINE;
    refreshAll = TRUE;
    return TRUE;
  }

  if (!checkingUnderflow || noChangeIfEndOfSnip)
    return FALSE;
  else {
    refreshAll = TRUE;
    return TRUE;
  }
}

#if CHECK_CONSISTENCY
static int CheckRBConsistent(wxMediaLine *line)
{
  Bool red;
  int l, r;

  if (line == NIL)
    return 0;

  red = (line->flags & 1);

  if (red) {
    if (line->left->flags & 1)
      fprintf(stderr, "Red left inconsistency\n");
    if (line->right->flags & 1)
      fprintf(stderr, "Red right inconsistency\n");
  }

  l = CheckRBConsistent(line->left);
  r = CheckRBConsistent(line->right);

  if (l != r)
    fprintf(stderr, "Black count inconsistency\n");
  
  return l + (red ? 0 : 1);
}
#endif

void wxMediaEdit::RecalcLines(wxDC *dc, Bool calcGraphics)
{
  wxMediaLine *line;
  wxSnip *snip;
  float X, Y, descent, space;
  Bool changed, resized;

  if (!calcGraphics)
    return;

  changed = FALSE;

#if CHECK_CONSISTENCY
  long p, p2;

  snip = snips;

  if (firstLine != lineRoot->First())
    fprintf(stderr, "First line inconsistency\n");
  if (lastLine != lineRoot->Last())
    fprintf(stderr, "Last line inconsistency\n");

  if (firstLine->prev)
    fprintf(stderr, "First line prev inconsistency\n");
  if (lastLine->next)
    fprintf(stderr, "First line next inconsistency\n");

  if (firstLine->snip != snips)
    fprintf(stderr, "First line first snip inconsistency\n");
  if (lastLine->lastSnip != lastSnip)
    fprintf(stderr, "Last line last snip inconsistency\n");

  CheckRBConsistent(lineRoot);

  Y = 0;
  p = 0;
  for (line = firstLine; line; line = line->next) {
    if (line->snip != snip)
      fprintf(stderr, "Line start inconsistency\n");
    
    if (line->GetPosition() != p)
      fprintf(stderr, "Line position inconsistency\n");
    if (line->GetLocation() != Y)
      fprintf(stderr, "Line location inconsistency\n");

    if (line->prev && (line->snip != line->prev->lastSnip->next))
      fprintf(stderr, "Line snip continuity inconsistency\n");

    p2 = p;
    for (snip = line->snip; snip && snip != line->lastSnip; snip = snip->next){
      if (snip->line != line)
	fprintf(stderr, "Snip's line inconsistency\n");
      if (!snip->style)
	fprintf(stderr, "Snip's style missing\n");
      if (*snip->admin_ptr != snipAdmin)
	fprintf(stderr, "Snip's admin inconsistency\n");
      if (!(snip->flags & wxSNIP_OWNED))
	fprintf(stderr, "Snip's is-owned inconsistency\n");
      p2 += snip->count;
    }
    if (snip->line != line)
	fprintf(stderr, "Snip's line inconsistency\n");

    if (!snip)
      fprintf(stderr, "Line end inconsistency\n");

    p2 += snip->count;
    if ((p2 - p) != line->len)
      fprintf(stderr, "Line count inconsistency\n");

    snip = line->lastSnip->next;

    if (!line->next && line != lastLine)
      fprintf(stderr, "Last line link inconsistency\n");

    wxMediaLine *other;
    if (line->left != NIL) {
      for (other=line->prev; other && other != line->left; other=other->prev);
      if (!other)
	fprintf(stderr, "Left link inconsistency\n");
    }
    if (line->right != NIL) {
      for (other=line->next; other && other!=line->right; other=other->next);
      if (!other)
	fprintf(stderr, "Right link inconsistency\n");
    }
    if (line->parent == NIL) {
      if (line != lineRoot)
	fprintf(stderr, "Root inconsistency\n");
    } else if (line != line->parent->right && line != line->parent->left)
      fprintf(stderr, "Parent inconsistency\n");

    Y += line->h;
    p += line->len;
  }
  if (p != len)
    fprintf(stderr, "Total count inconsistency\n");

#endif

  if (snipCacheInvalid)
    for (snip = snips; snip; snip = snip->next)
      snip->SizeCacheInvalid();

  float old_max_width = maxWidth;

  if (flowInvalid && (maxWidth <= 0))
    maxWidth = A_VERY_BIG_NUMBER;

  if (graphicsInvalid || flowInvalid || snipCacheInvalid) {
    /* Set all lines invalid. */
    for (line = firstLine; line; line = line->next) {
      line->MarkRecalculate();
      if (flowInvalid)
	line->MarkCheckFlow();
    }
  }

#if LOOK_FOR_ZEROED
  if (len)
    for (snip = snips, i = 0; snip; snip = snip->next)
      if (!snip->count)
	fprintf(stderr, "zero snip found at %d\n", i);
      else
	i += snip->count;
#endif

  if (maxWidth > 0) {
    Bool fl = flowLocked, wl = writeLocked;

    /* If any flow is update, snip sizing methods will be called. */
    flowLocked = TRUE;
    writeLocked = TRUE;

    float w;
    w = maxWidth - CURSOR_WIDTH;
    while (lineRoot->UpdateFlow(&lineRoot, this, w, dc))
      changed = TRUE;

    flowLocked = fl;
    writeLocked = wl;    
  }

  if (old_max_width != maxWidth)
    maxWidth = old_max_width;

  if (changed) {
    refreshAll = TRUE;
    firstLine = lineRoot->First();
    lastLine = lineRoot->Last();
    numValidLines = lineRoot->Number();
  }

  if (lineRoot->UpdateGraphics(this, dc))
    changed = TRUE;

  if (!changed && !graphicMaybeInvalidForce) {
    graphicMaybeInvalid = FALSE;
    return;
  }

  graphicMaybeInvalid = FALSE;
  graphicMaybeInvalidForce = FALSE;

  Y = lastLine->GetLocation() + lastLine->h;

  if (lastSnip->flags & wxSNIP_NEWLINE) {
    extraLine = TRUE;
    extraLineH = lastLine->lastH + lineSpacing;
    Y += extraLineH;
  } else {
    extraLine = FALSE;
    extraLineH = 0;
  }

  X = lineRoot->maxWidth + CURSOR_WIDTH;
  if (minWidth > 0 && X < minWidth)
    X = minWidth;

  if (minHeight > 0 && Y < minHeight)
    Y = minHeight;
  if (maxHeight > 0 && Y > maxHeight)
    Y = maxHeight;

  descent = lastLine->h - lastLine->bottombase;
  space = firstLine->topbase;
  
  if (totalHeight != Y || totalWidth != X 
      || finalDescent != descent || initialSpace != space) {
    totalHeight = Y;
    totalWidth = X;
    finalDescent = descent;
    initialSpace = space;
    resized = TRUE;
  } else
    resized = FALSE;

  graphicsInvalid = FALSE;  
  flowInvalid = FALSE;
  snipCacheInvalid = FALSE;

  drawCachedInBitmap = FALSE;

  if (resized && admin)
    admin->Resized(FALSE);
}

static wxMemoryDC *autowrapMemDC = NULL;
static wxBitmap *lastSetAWMDC = NULL;

wxBitmap *wxMediaEdit::SetAutowrapBitmap(wxBitmap *bm)
{
  wxBitmap *old;
  float oldWidth;

  if (flowLocked)
    return NULL;

  old = autoWrapBitmap;

  autoWrapBitmap = bm;
  oldWidth = wrapBitmapWidth;
  if (autoWrapBitmap)
    wrapBitmapWidth = autoWrapBitmap->GetWidth();
  else
    wrapBitmapWidth = 0;

  if (maxWidth > 0)
    SetMaxWidth(maxWidth + oldWidth);

  if (old && PTREQ(old, lastSetAWMDC)) {
    autowrapMemDC->SelectObject(NULL);
    lastSetAWMDC = NULL;
  }

  return old;
}

static skipBox = FALSE;
static wxPen *caretPen = NULL;

/* This does the actual drawing */
void wxMediaEdit::Redraw(wxDC *dc, float starty, float endy, 
			 float leftx, float rightx,
			 float dy, float dx, 
			 int show_caret, int show_xsel)
{
  wxMediaLine *line;
  wxSnip *snip, *first, *last;
  float x, topbase, bottombase, hxs, hxe, hsxs, hsxe, hsys, hsye, down, bottom;
  float tleftx, tstarty, trightx, tendy;
  float h, w, descent, space, ycounter, prevwasfirst = 0.0;
  long p, pcounter;
  long startpos, endpos;
  Bool posAtEol;
  Bool hilite, hiliteSome;
  int align;
  wxStyle *oldStyle;
  wxPen *savePen;
  wxBrush *saveBrush;
  static wxPen *outlinePen = NULL;
  static wxBrush *outlineBrush = NULL;
#if ALLOW_X_STYLE_SELECTION
  static wxBrush *outlineNonownerBrush = NULL;
#if 1
  static char xpattern[32] = {0x88, 0x88,
			      0x00, 0x00,
			      0x22, 0x22, 
			      0x00, 0x00,
			      0x88, 0x88,
			      0x00, 0x00,
			      0x22, 0x22,
			      0x00, 0x00,
			      0x88, 0x88,
			      0x00, 0x00,
			      0x22, 0x22, 
			      0x00, 0x00,
			      0x88, 0x88,
			      0x00, 0x00,
			      0x22, 0x22,
			      0x00, 0x00};
#else
  static char xpattern[32] = {0x80, 0x80,
			      0x00, 0x00,
			      0x08, 0x08, 
			      0x00, 0x00,
			      0x80, 0x80,
			      0x00, 0x00,
			      0x08, 0x08,
			      0x00, 0x00,
			      0x80, 0x80,
			      0x00, 0x00,
			      0x08, 0x08, 
			      0x00, 0x00,
			      0x80, 0x80,
			      0x00, 0x00,
			      0x08, 0x08,
			      0x00, 0x00};
#endif
#endif
  static wxBrush *clearBrush = NULL;

  if (flowLocked)
    return;

  Bool wl = writeLocked;

  flowLocked = TRUE;
  writeLocked = TRUE;

  if (flash) {
    startpos = wxMediaEdit::flashstartpos;
    endpos = wxMediaEdit::flashendpos;
    posAtEol = flashposateol;
  } else {
    startpos = wxMediaEdit::startpos;
    endpos = wxMediaEdit::endpos;
    posAtEol = posateol;
  }

  if (!outlinePen) {
    outlinePen = new wxPen("BLACK", 0, wxTRANSPARENT);
    if (!caretPen)
      caretPen = new wxPen("BLACK", 1, wxSOLID);
    outlineBrush = new wxBrush("BLACK", wxSOLID);
#if ALLOW_X_STYLE_SELECTION
    outlineNonownerBrush = new wxBrush();
    wxTheBrushList->RemoveBrush(outlineNonownerBrush);
    outlineNonownerBrush->SetColour("BLACK");
    outlineNonownerBrush->SetStipple(new wxBitmap(xpattern, 16, 16, 1));
    outlineNonownerBrush->SetStyle(wxSTIPPLE);
#endif
    clearBrush = new wxBrush("WHITE", wxSOLID);
  }
  
  dc->SetBackgroundMode(wxSOLID);

  line = lineRoot->FindLocation(starty);

  if (!skipBox) {
    wxPen *savePen = dc->GetPen();
    wxBrush *saveBrush = dc->GetBrush();

    dc->SetBrush(clearBrush);
    dc->SetPen(outlinePen);
    dc->DrawRectangle(leftx + dx, starty + dy, 
		      rightx - leftx + GC_RECT_BRUSH_EXTEND,
		      endy - starty + GC_RECT_BRUSH_EXTEND);

    dc->SetBrush(saveBrush);
    dc->SetPen(savePen);
  }

  OnPaint(TRUE, dc, leftx, starty, rightx, endy, dx, dy, 
	  (show_caret && !caretSnip) 
	  ? show_caret
	  : (int)wxSNIP_DRAW_NO_CARET);

  if (!line)
    goto paint_done;

  tleftx = leftx + dx;
  tstarty = starty + dy;
  trightx = rightx + dx;
  tendy = endy + dy;

  oldStyle = NULL;

  ycounter = line->GetLocation();
  pcounter = line->GetPosition();

  /* Quiet the compiler: */
  hsxs = hsxe = hsys = hsye = 0;

  for (; line; line = line->next) {
    if (ycounter >= endy)
      goto paint_done;

    first = line->snip;
    last = line->lastSnip->next;

    x = 0;
    bottombase = ycounter + line->bottombase;
    topbase = ycounter + line->topbase;
    p = pcounter;

    hiliteSome = FALSE;

    for (snip = first; PTRNE(snip, last); snip = snip->next) {
      snip->style->SwitchTo(dc, oldStyle);
      oldStyle = snip->style;

      snip->GetExtent(dc, x, ycounter, &w, &h, &descent, &space);
      
      align = snip->style->GetAlignment();
      
      if (align == wxALIGN_BOTTOM)
	down = bottombase - h + descent;
      else if (align == wxALIGN_TOP)
	down = topbase - space;
      else {
	down = (h - descent - space) / 2;
	down = (topbase + bottombase) / 2 - down - space;
      }

      if ((x <= rightx) && (x + w >= leftx)) {
	snip->Draw(dc, x + dx, down + dy, 
		   tleftx, tstarty, trightx, tendy, 
		   dx, dy, 
		   (PTREQ(snip, caretSnip) && show_caret)
		   ? show_caret
		   : (int)wxSNIP_DRAW_NO_CARET);

#if PLOT_SNIP_DOT
	dc->DrawLine(x + dx, down + dy, x + dx + 2, down + dy + 2);
#endif
      }

      /* The rules for hiliting are surprisingly complicated: */

      if (hiliteOn && !skipBox
	  && (show_xsel
	      || (!caretSnip
		  && ((show_caret == wxSNIP_DRAW_SHOW_CARET)
		      || ((show_caret >= inactiveCaretThreshold)
			  && (startpos != endpos)))))) {
	if (posAtEol)
	  hilite = (startpos == p + snip->count);
	else
	  hilite = (((startpos < p + snip->count) && (endpos >= p)
		     && (startpos == endpos || endpos > p))
		    || (p + snip->count == len && startpos == len));
	
	if (hilite && (snip->flags & wxSNIP_NEWLINE))
	  /* End of line: */
	  hilite = ((startpos != p + snip->count)
		    || (startpos == endpos && posAtEol)
		    || (startpos != endpos && startpos < p + snip->count));
	if (hilite && PTREQ(snip, first))
	  /* Beginning of line: */
	  hilite = ((endpos != p)
		    || (startpos == endpos && !posAtEol)
		    || (startpos != endpos && endpos > p));
      } else
	hilite = FALSE;

      if (hilite) {
	bottom = down + h;

	if (startpos <= p)
	  hxs = x;
	else
	  hxs = x + snip->PartialOffset(dc, x, ycounter, startpos - p);
	
	if (endpos >= p + snip->count) {
	  if (snip->flags & wxSNIP_NEWLINE) {
	    if (endpos == startpos)
	      hxe = hxs;
	    else {
	      hxe = rightx;
	      bottom = ycounter + line->h;
	    }
	  } else
	    hxe = x + w;
	} else
	  hxe = x + snip->PartialOffset(dc, x, ycounter, endpos - p);
	
	if (!hiliteSome) {
	  hsxs = hxs;
	  hsxe = hxe;
	  hsys = down;
	  hsye = bottom;
	  hiliteSome = TRUE;
	} else {
	  hsxe = hxe;
	  if (down < hsys)
	    hsys = down;
	  if (bottom > hsye)
	    hsye = bottom;
	}
      }

      x += w;
      
      p += snip->count;
    }

    if (wrapBitmapWidth 
	&& !(line->lastSnip->flags & wxSNIP_HARD_NEWLINE)
	&& last
	&& (rightx >= maxWidth) && autoWrapBitmap->Ok()) {
      int h;

      if (PTRNE(autoWrapBitmap, lastSetAWMDC)) {
	if (!autowrapMemDC)
	  autowrapMemDC = new wxMemoryDC;
	autowrapMemDC->SelectObject(autoWrapBitmap);
	lastSetAWMDC = autoWrapBitmap;
      }

      if (autowrapMemDC->Ok()) {
	h = (int)autoWrapBitmap->GetHeight();
	if (h > line->bottombase)
	  h = (int)line->bottombase;

#ifndef WXME_FOR_MRED
# define wxCOLOR wxCOPY
#endif

	dc->Blit(maxWidth + dx - 1, bottombase - h + dy, 
		 wrapBitmapWidth, h,
		 autowrapMemDC, 0, 0,
		 wxCOLOR);
      }
    }

    if (hiliteSome) {
      if (hsxe != hsxs) {
	if ((hsxs <= rightx) && (hsxe >= leftx)) {
	  savePen = dc->GetPen();
	  dc->SetLogicalFunction(wxXOR);
	  
	  if (hsxs < leftx)
	    hsxs = leftx;
	  if (hsxe > rightx)
	    hsxe = rightx;

	  if (!show_xsel && (show_caret < wxSNIP_DRAW_SHOW_CARET)) {
	    int lastHilite, firstHilite;
	    
	    firstHilite = (startpos >= pcounter);
	    lastHilite = (endpos <= pcounter + line->len);
	    
	    dc->SetPen(caretPen);
	    
	    if (firstHilite) {
	      dc->DrawLine(hsxs + dx, hsys + dy, hsxe + dx - 1, hsys + dy);
	      prevwasfirst = hsxs;
	    } else if (prevwasfirst) {
	      dc->DrawLine(0 + dx, hsys + dy, prevwasfirst + dx, hsys + dy);
	      prevwasfirst = 0.0;
	    }
	    dc->DrawLine(hsxs + dx, hsys + dy, hsxs + dx, hsye + dy - 1);
	    dc->DrawLine(hsxe + dx - 1, hsys + dy, hsxe + dx - 1, hsye + dy - 1);
	    if (lastHilite) {
	      dc->DrawLine(hsxs + dx, hsye + dy, hsxe + dx - 1, hsye + dy);
	      if (!firstHilite)
		dc->DrawLine(hsxe + dx, hsys + dy, rightx + dx, hsys + dy);
	    }
	  } else {
	    saveBrush = dc->GetBrush();
	    dc->SetPen(outlinePen);
	    dc->SetBrush(outlineBrush);
	    
	    dc->DrawRectangle(hsxs + dx, hsys + dy, 
			      hsxe - hsxs + GC_RECT_BRUSH_EXTEND, 
			      hsye - hsys + GC_RECT_BRUSH_EXTEND);
#if ALLOW_X_STYLE_SELECTION
	    if (show_xsel) {
	      dc->SetBrush(outlineNonownerBrush);
	      
	      int log = wxAND_INVERT;
#ifdef wx_x
	      
	      static Bool _black_pixel_set_ = FALSE;
	      static unsigned long _black_pixel_;
	      
	      if (!_black_pixel_set_) {
#ifdef wx_xt
		Display *d = wxAPP_DISPLAY;
#else
		Display *d = wxGetDisplay();
#endif
		int sc = DefaultScreen(d);
		_black_pixel_ = BlackPixel(d, sc);
		_black_pixel_set_ = TRUE;
	      }
	      
	      /* If black == 0, then reverse */
	      if (!_black_pixel_)
		log = wxOR;
#endif
	      dc->SetLogicalFunction(log);
	      
	      dc->DrawRectangle(hsxs + dx, hsys + dy, 
				hsxe - hsxs + GC_RECT_BRUSH_EXTEND, 
				hsye - hsys + GC_RECT_BRUSH_EXTEND);
	    }
#endif
	    
	    dc->SetBrush(saveBrush);
	  }
	  dc->SetLogicalFunction(wxCOPY);
	  dc->SetPen(savePen);
	}
      } else {
	if (show_caret == wxSNIP_DRAW_SHOW_CARET) {
	  if ((hsxs <= rightx) && (hsxs >= leftx)) {
	    savePen = dc->GetPen();
	    dc->SetPen(caretPen);
	    dc->SetLogicalFunction(wxXOR);
	    dc->DrawLine(hsxs + dx, hsys + dy, 
			 hsxs + dx, 
			 hsye + dy - 1 + GC_LINE_EXTEND);
	    dc->SetLogicalFunction(wxCOPY);
	    dc->SetPen(savePen);
	  } 
	  caretLocationX = hsxs;
	  caretLocationT = hsys;
	  caretLocationB = hsye + GC_LINE_EXTEND;
	  caretOn = TRUE;
	}
      }
    }

    pcounter += line->len;
    ycounter += line->h;
  }

  styleList->BasicStyle()->SwitchTo(dc, oldStyle);

  if ((show_caret == wxSNIP_DRAW_SHOW_CARET) && !caretSnip)
    if (!line && extraLine)
      if (!posAtEol && startpos == len && endpos == startpos
	  && hiliteOn) {
	float y;
	y = ycounter;

	savePen = dc->GetPen();
	dc->SetPen(caretPen);
	dc->SetLogicalFunction(wxXOR);
	dc->DrawLine(dx, y + dy, dx, 
		     y + extraLineH + dy - 1 + GC_LINE_EXTEND);
	dc->SetLogicalFunction(wxCOPY);
	dc->SetPen(savePen);

	caretLocationX = 0;
	caretLocationT = y;
	caretLocationB = y + extraLineH + GC_LINE_EXTEND;
	caretOn = TRUE;
      }
 
paint_done:

  OnPaint(FALSE, dc, leftx, starty, rightx, endy, dx, dy, 
	  (show_caret && !caretSnip)
	  ? show_caret
	  : (int)wxSNIP_DRAW_NO_CARET);

  writeLocked = wl;
  flowLocked = FALSE;
}

/* This one notifies the administrator that we need to be updated. */
void wxMediaEdit::Redraw()
{
  float w, h;
  float top, bottom, height, width, left, right;
  float x, y, origx, origy;
  float fy, fx;
  Bool oneline;
  wxDC *dc;

  if (flowLocked || !admin)
    return;

  if (admin->DelayRefresh()) {
    /* Do we know the refresh box already? */
    if (!delayedscroll && !delayedscrollbox && (refreshAll || refreshUnset)) {
      /* Yes ... */
      if (!refreshAll && refreshBoxUnset)
	return; /* Nothing to do */
      admin->GetMaxView(&x, &y, &w, &h);
      top = y;
      bottom = y + h;
      left = x;
      right = left + w;
      if (!refreshAll) {
	if (refreshL > left)
	  left = refreshL;
	if (refreshR < right)
	  right = refreshR;
	if (refreshT > top)
	  top = refreshT;
	if (refreshB < bottom)
	  bottom = refreshB;
      }
      refreshUnset = refreshBoxUnset = TRUE;
      refreshAll = FALSE;
      height = bottom - top;
      width = right - left;
      admin->NeedsUpdate(left, top, width, height);
    }
  }

  dc = admin->GetDC(&x, &y);

  if (!dc)
    return;

  origx = x;
  origy = y;

  RecalcLines(dc);

  if (delayedscroll != -1) {
    if (ScrollToPosition(delayedscroll, delayedscrollateol, FALSE,
			 delayedscrollend, delayedscrollbias))
      refreshAll = TRUE;
  } else if (delayedscrollbox) {
    delayedscrollbox = FALSE;
    if (ScrollTo(delayedscrollsnip, delayedscrollX, delayedscrollY,
		 delayedscrollW, delayedscrollH, FALSE, delayedscrollbias))
      refreshAll = TRUE;
  }

  admin->GetDC(&x, &y);
  
  if (x != origx || y != origy)
    refreshAll = TRUE;

  admin->GetMaxView(&x, &y, &w, &h);

  top = y;
  bottom = y + h;
  left = x;
  right = left + w;

  /* Figure out the minimal refresh area. The refresh area may be
     determined by character position ranges, box coordinates, or
     both. If neither is specified, we have to assume that everything
     needs to be refreshed. */
  if (!refreshAll && (!refreshUnset || !refreshBoxUnset)) {
    if (!refreshUnset) {
      top = y;
      if (refreshStart > -1 && refreshEnd > -1) {
	// oneline = PositionLine(refreshStart) == PositionLine(refreshEnd);
	oneline = FALSE; // b/c it doesn't seem to help
      } else
	oneline = FALSE;
      if (refreshStart > -1) {
	PositionLocation(refreshStart, 
			 oneline ? (float *)&fx : (float *)NULL, (float *)&fy, 
			 TRUE, TRUE, TRUE);
	if (fy > top)
	  top = (long)fy;
	if (oneline && fx > left)
	  left = fx;
      }
      bottom = y + h;
      if (refreshEnd > -1) {
	PositionLocation(refreshEnd, 
			 oneline ? (float *)&fx : (float *)NULL, (float *)&fy, 
			 FALSE, FALSE, TRUE);
	if (fy < bottom)
	  bottom = (long)fy;
	if (oneline && fx + CURSOR_WIDTH < right)
	  right = fx + CURSOR_WIDTH;
      }

      if (!refreshBoxUnset) {
	if (refreshT < top)
	  top = refreshT;
	if (refreshB > bottom)
	  bottom = refreshB;
      }
    } else {
      if (refreshL > left)
	left = refreshL;
      if (refreshR < right)
	right = refreshR;
      if (refreshT > top)
	top = refreshT;
      if (refreshB < bottom)
	bottom = refreshB;
    }
  } else if (!refreshAll)
    return; /* Nothing needs to be updated! */

  refreshUnset = refreshBoxUnset = TRUE;
  refreshAll = FALSE;

  height = bottom - top;
  width = right - left;

  admin->NeedsUpdate(left, top, width, height);
}

/* This one is called by the administrator: */
void wxMediaEdit::Refresh(float left, float top, float width, float height,
			  int show_caret)
{
  float x, y, bottom, right, ddx, ddy;
  Bool ps;
  wxDC *dc;
  int show_xsel = 0;

  if ((width <= 0) || (height <= 0))
    return;

  if (graphicMaybeInvalid) {
    /* This Refresh command was not requested by us and we're busy. 
       (Probably in the middle of a begin-/end-edit-sequnce.)
       Add the given region to our own invalid-region tracking and
       we'll get back to it when we're done with whatever. */
    RefreshBox(left, top, width, height);
    return;
  }

  dc = admin->GetDC(&x, &y);
  if (!dc)
    return;

  caretLocationX = -1;
  caretOn = FALSE;

  if (ReadyOffscreen(width, height))
    drawCachedInBitmap = FALSE;

  bottom = top + height;
  right = left + width;

#if USE_OLD_TYPE_SYSTEM
  ps = wxSubType(dc->__type, wxTYPE_DC_POSTSCRIPT)
    || wxSubType(dc->__type, wxTYPE_DC_PRINTER);
#else
  ps = FALSE; /* FIXME! */
#endif

#if ALLOW_X_STYLE_SELECTION
  if (((show_caret != wxSNIP_DRAW_SHOW_CARET) || caretSnip)
      && (wxMediaXSelectionOwner == this)
      && !flash 
      && (startpos != endpos))
    show_xsel = 1;
#endif

  if (!offscreenInUse && bitmap && bitmap->Ok() && offscreen->Ok() && !ps) {
    /* Need to make sure that difference between coordinates is
       integral; otherwise, roundoff error could affect drawing */
    ddx = (left - x) - (long)(left - x);
    if (ddx < 0)
      ddx = 1 + ddx;
    left -= ddx;
    width += ddx;
    ddy = (top - y) - (long)(top - y);
    if (ddy < 0)
      ddy = 1 + ddy;
    top -= ddy;
    height += ddy;
#ifndef EACH_BUFFER_OWN_OFFSCREEN
    offscreenInUse = TRUE;
#endif
    if (!drawCachedInBitmap 
#ifndef EACH_BUFFER_OWN_OFFSCREEN
	|| (lastUsedOffscreen != this)
#endif
	|| (top != lastDrawT) || (bottom != lastDrawB)
	|| (left != lastDrawL) || (right != lastDrawR)
	|| (lastDrawCaret != show_caret)
	|| (lastDrawXSel != show_xsel)) {
      offscreen->BeginDrawing();
      Redraw(offscreen, top, bottom, left, right, -top, -left, 
	     show_caret, show_xsel);
      offscreen->EndDrawing();
      lastDrawL = left;
      lastDrawT = top;
      lastDrawR = right;
      lastDrawB = bottom;
      lastDrawCaret = show_caret;
      lastDrawXSel = show_xsel;
      drawCachedInBitmap = TRUE;
    }
    dc->Blit(left - x, top - y, width, height, offscreen, 0, 0, wxCOPY);
#ifndef EACH_BUFFER_OWN_OFFSCREEN
    offscreenInUse = FALSE;
    lastUsedOffscreen = this;
#endif
  } else {
    if (ps)
      skipBox = TRUE;

    wxPen *pen;
    wxBrush *brush;
    wxFont *font;
#if defined(wx_xt) && !defined(WXME_FOR_MRED)
    wxColour *fg, *bg;
#else
    wxColour fg, bg;
#endif
    int log;

    log = dc->GetLogicalFunction();
    pen = dc->GetPen();
    brush = dc->GetBrush();
    font = dc->GetFont();
#if defined(wx_xt) && !defined(WXME_FOR_MRED)
    fg = &dc->GetTextForeground();
    bg = &dc->GetTextBackground();
#else
    fg = dc->GetTextForeground();
    bg = dc->GetTextBackground();
#endif

#ifndef NO_GET_CLIPPING_REGION
    float cx, cy, cw, ch;
    dc->GetClippingRegion(&cx, &cy, &cw, &ch);
    dc->SetClippingRegion(left - x, top - y, width, height);
#endif

    Redraw(dc, top, bottom, left, right, -y, -x, show_caret, show_xsel);

#ifndef NO_GET_CLIPPING_REGION
    if (cw < 0)
      dc->DestroyClippingRegion();
    else
      dc->SetClippingRegion(cx, cy, cw, ch);
#endif

    dc->SetLogicalFunction(log);
    dc->SetBrush(brush);
    dc->SetPen(pen);
    dc->SetFont(font);
#if defined(wx_xt) && !defined(WXME_FOR_MRED)
    dc->SetTextForeground(fg);
    dc->SetTextBackground(bg);
#else
    dc->SetTextForeground(&fg);
    dc->SetTextBackground(&bg);
#endif

    if (ps)
      skipBox = FALSE;
  }

  if (changed) {
    changed = FALSE;
    OnChange();
  }
}

/* This one is used internally to delay refreshes: */
void wxMediaEdit::NeedRefresh(long start, long end)
{
  if (refreshUnset) {
    refreshStart = start;
    refreshEnd = end;
    refreshUnset = FALSE;
  } else {
    if (start < refreshStart)
      refreshStart = start;
    if (end == -1)
      refreshEnd = -1;
    else if (refreshEnd != -1 && end > refreshEnd)
      refreshEnd = end;
  }

  drawCachedInBitmap = FALSE;

  if (!delayRefresh && !printing && (!admin || !admin->DelayRefresh()))
    Redraw();
  else if (admin && !admin->standard)
    admin->Resized(FALSE);
}

void wxMediaEdit::RefreshByLineDemand(void)
{
  if (!graphicMaybeInvalid)
    graphicMaybeInvalid = TRUE;

  if (!delayRefresh && !printing && (!admin || !admin->DelayRefresh()))
    Redraw();
  else if (admin && !admin->standard)
    admin->Resized(FALSE);
}

void wxMediaEdit::NeedCaretRefresh(void)
{
  if (!admin || (admin->standard <= 0) || delayRefresh 
      || startpos != endpos || flash || !hiliteOn)
    NeedRefresh(startpos, endpos);
  else if (ownCaret) {
    if (hiliteOn)
      CaretOn();
  } else
    CaretOff();
}

void wxMediaEdit::CalcCaretLocation(void)
{
  if (caretLocationX < 0) {
    PositionLocation(startpos, &caretLocationX, &caretLocationT,
		     TRUE, posateol, FALSE);
    PositionLocation(startpos, NULL, &caretLocationB,
		     FALSE, posateol, FALSE);
  }
}

Bool wxMediaEdit::CaretOff(void)
{
  wxDC *dc;
  float dx, dy, x, y, w, h, X, T, B;

  if (!CheckRecalc(TRUE, FALSE))
    return FALSE;

  if (refreshAll || !refreshUnset || !refreshBoxUnset || (delayedscroll!=-1)) {
    Redraw();
    return FALSE;
  }

  dc = admin->GetDC(&dx, &dy);

  if (!dc)
    return FALSE;

  admin->GetView(&x, &y, &w, &h);

  CalcCaretLocation();

  X = caretLocationX;
  T = caretLocationT;
  B = caretLocationB;

  if (B < y)
    return TRUE;
  if (T >= y + h)
    return TRUE;
  if (X < x || X >= x + w)
    return TRUE;

  if (T < y)
    T = y;
  if (B > y + h)
    B = y + h;

  if (!caretPen)
    caretPen = new wxPen("BLACK", 1, wxSOLID);

  dc->SetPen(caretPen);
  dc->SetLogicalFunction(wxXOR);
  dc->DrawLine(X - dx, T - dy, X - dx, B - dy - 1 + GC_LINE_EXTEND);
  dc->SetLogicalFunction(wxCOPY);

  drawCachedInBitmap = FALSE;

  caretOn = FALSE;

  return TRUE;
}

void wxMediaEdit::CaretOn(void)
{
  if (CaretOff())
    caretOn = TRUE;
}

/* 8.5" x 11" Paper, 0.5" Margin; usually not used */
static long page_width = 612, page_height = 792;
/* Printing margins: */
static long h_margin = 36, v_margin = 36;

void wxGetMediaPrintMargin(long *hm, long *vm)
{
  if (hm)
    *hm = h_margin;
  if (vm)
    *vm = v_margin;
}

void wxSetMediaPrintMargin(long hm, long vm)
{
  if (hm > -1)
    h_margin = hm;
  if (vm > -1)
    v_margin = vm;
}

typedef struct {
  float maxw;
  wxBitmap *bm;
} SaveSizeInfo;

void *wxMediaEdit::BeginPrint(wxDC *dc, Bool fit)
{
  if (flowLocked)
    return NULL;

  CheckRecalc();

  SizeCacheInvalid();

  if (fit) {
    float w, h;
    SaveSizeInfo *savedInfo = new SaveSizeInfo;
    
    savedInfo->maxw = GetMaxWidth();
    savedInfo->bm = SetAutowrapBitmap(NULL);

    dc->GetSize(&w, &h);
    w -= 2 * h_margin;
    SetMaxWidth(w);

    return savedInfo;
  } else
    return NULL;
}

void wxMediaEdit::EndPrint(wxDC *, void *data)
{
  if (flowLocked)
    return;

  SizeCacheInvalid();  
  if (data) {
    SaveSizeInfo *savedInfo = (SaveSizeInfo *)data;
    
    SetMaxWidth(savedInfo->maxw);
    SetAutowrapBitmap(savedInfo->bm);

    delete data;
  }
}

Bool wxMediaEdit::HasPrintPage(wxDC *dc, int page)
{
  if (flowLocked)
    return FALSE;

  float H, W, h;
  int i, this_page = 1;
  wxMediaLine *line;

  RecalcLines(dc, TRUE);

  dc->GetSize(&W, &H);

  if (!W || !H) {
    W = page_width;
    H = page_height;

    if (wxGetPrinterOrientation() != PS_PORTRAIT)  {
      float tmp;
      
      tmp = H;
      H = W;
      W = tmp;
    }
  }

  line = firstLine;
  for (i = 0; i < numValidLines; this_page++) {
    h = 0;
    while (!h || ((i < numValidLines) && (line->h < H - h))) {
      h += line->h;
      i++;
      line = line->next;
    }

    if (this_page >= page)
      return TRUE;
  }

  return FALSE;
}

void wxMediaEdit::PrintToDC(wxDC *dc, int page)
{
  if (flowLocked)
    return;

  float H, W, FH, FW, y, h;
  int i, this_page = 1;
  wxMediaLine *line;

  RecalcLines(dc, TRUE);

  dc->GetSize(&W, &H);

  if (!W || !H) {
    W = page_width;
    H = page_height;

    if (wxGetPrinterOrientation() != PS_PORTRAIT)  {
      float tmp;
      
      tmp = H;
      H = W;
      W = tmp;
    }
  }

  FH = H;
  FW = W;

  H -= (2 * v_margin);
  W -= (2 * h_margin);

  y = 0;
  line = firstLine;
  for (i = 0; i < numValidLines; this_page++) {
    h = 0;
    while (!h || ((i < numValidLines) && (line->h < H - h))) {
      h += line->h;
      i++;
      line = line->next;
    }

    if (page < 0 || (page == this_page)) {
      if (page < 0)
	dc->StartPage();
      
      /* Establish page size: */
      dc->DrawLine(0, 0, 0, 0);
      dc->DrawLine(FW, FH, FW, FH);
      
      skipBox = TRUE;
      Redraw(dc, y + (i ? 1 : 0), y + h, 0, W, -y + v_margin, h_margin, 
	     wxSNIP_DRAW_NO_CARET, 0);
      skipBox = FALSE;
      
      if (page < 0)
	dc->EndPage();

      if (page >= 0)
	break;
    }

    y += h;
  }
}

#if ALLOW_X_STYLE_SELECTION

Bool wxMediaEdit::OwnXSelection(Bool on, Bool update, Bool force)
{
  if (DoOwnXSelection(on, force)) {
    if (update)
      NeedCaretRefresh();
    return TRUE;
  } else
    return FALSE;
}

#endif

