/*
 * File:        wx_keym.cc
 * Purpose:     wxKeymap implementation
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

#include "wx_main.h"
#include "wx_keym.h"
#include "wx_mtype.h"
#include "wx_utils.h"
#include <string.h>
#include <ctype.h>
#include "wx_ptreq.h"

#define DOUBLE_CLICK_THRESHOLD 500 /* Half-second */

#if WXGARBAGE_COLLECTION_ON
#define FUNC_REC_INHERITANCE /* empty */
#else
#define FUNC_REC_INHERITANCE : public wxObject
#endif

class wxKeyFunc FUNC_REC_INHERITANCE
{
 public:
  char *name;
  wxKeyFunction f;
  void *data;

  wxKeyFunc(char *name, wxKeyFunction, void *);
#if !WXGARBAGE_COLLECTION_ON
  ~wxKeyFunc();
#endif
  Bool Call(wxObject *, wxKeyEvent &);
};

class wxMouseFunc FUNC_REC_INHERITANCE
{
 public:
  char *name;
  wxMouseFunction f;
  void *data;

  wxMouseFunc(char *name, wxMouseFunction, void *);
#if !WXGARBAGE_COLLECTION_ON
  ~wxMouseFunc();
#endif
  Bool Call(wxObject *, wxMouseEvent &);
};

class wxKeycode FUNC_REC_INHERITANCE
{
 public:
  long code;
  Bool shift;
  Bool ctrl;
  Bool alt;
  Bool meta;
  char *fname;

  Bool isprefix;
  wxKeycode *seqprefix;

  wxKeycode *next;
};

/***************************************************************/

static int doubleClickThreshold = -1;

wxKeymap::wxKeymap()
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_KEYMAP;
#endif

  keyfunctions = NULL;
  mousefunctions = NULL;
  keys = NULL;

  numImpliedShifts = allocedImplies = 0;
  impliesShift = NULL;

  prefix = NULL;
  err = NULL;
  WXGC_IGNORE(errdata);

  active_mouse_function = NULL;

  grabKeyFunction = NULL;
  grabMouseFunction = NULL;
  onBreak = NULL;

  WXGC_IGNORE(grabKeyData);
  WXGC_IGNORE(grabMouseData);
  WXGC_IGNORE(onBreakData);

  chainCount = 0;
  chainTo = NULL;

  usage = 0;

  lastButton = 0;

  if (doubleClickThreshold < 0) {
#if USE_RESOURCES
    if (!wxGetResource(wxTheApp->wx_class, "doubleClickTime", 
		       &doubleClickThreshold))
#endif
      doubleClickThreshold = DOUBLE_CLICK_THRESHOLD;
  }

  doubleInterval = doubleClickThreshold;
}

wxKeymap::~wxKeymap()
{
  int i;

  BreakSequence();

  for (i = chainCount; i--; )
    RemoveChainedKeymap(chainTo[i]);
  if (chainTo)
    delete[] chainTo;

  if (keys) {
#if !WXGARBAGE_COLLECTION_ON
    keys->DeleteContents(TRUE);
#endif
    delete keys;
  }

  if (keyfunctions) {
#if !WXGARBAGE_COLLECTION_ON
    keyfunctions->DeleteContents(TRUE);
#endif
    delete keyfunctions;
  }

  if (mousefunctions) {
#if !WXGARBAGE_COLLECTION_ON
    mousefunctions->DeleteContents(TRUE);
#endif
    delete mousefunctions;
  }

  if (impliesShift)
    delete[] impliesShift;
}

void wxKeymap::Reset(void)
{
  int i;

  prefix = NULL;

  for (i = 0; i < chainCount; i++)
    chainTo[i]->Reset();  
}

void wxKeymap::BreakSequence(void)
{
  int i;

  prefix = NULL;

  if (onBreak) {
    wxBreakSequenceFunction f;
    void *data;

    f = onBreak;
    data = onBreakData;

    onBreak = NULL;
    onBreakData = NULL;

    f(data);
  }

  for (i = 0; i < chainCount; i++)
    chainTo[i]->BreakSequence();
}

void wxKeymap::SetBreakSequenceCallback(wxBreakSequenceFunction f, 
					void *data)
{
  wxBreakSequenceFunction fold;
  void *dataold;
  
  fold = onBreak;
  dataold = onBreakData;

  onBreak = f;
  onBreakData = data;

  if (fold)
    fold(dataold);
}

wxKeycode *wxKeymap::FindKey(long code, Bool shift, Bool ctrl, 
			     Bool alt, Bool meta,
			     wxKeycode *prefix)
{
  wxKeycode *key;
  
  if (!keys)
    return NULL;

  shift = !!shift;
  ctrl = !!ctrl;
  alt = !!alt;
  meta = !!meta;

  key = (wxKeycode *)keys->Get(code);
  while (key) {
    if (key->code == code
	&& key->shift == shift
	&& key->ctrl == ctrl
	&& key->alt == alt
	&& key->meta == meta
	&& key->seqprefix == prefix) {
      return key;
    }
    key = key->next;
  }

  return key;
}

wxKeycode *wxKeymap::MapFunction(long code, Bool shift, Bool ctrl, 
				 Bool alt, Bool meta,
				 char *fname, wxKeycode *prev, int type)
{
  wxKeycode *key, *newkey;

  if ((key = FindKey(code, shift, ctrl, alt, meta, prev))) {
    if ((type == wxKEY_PREFIX) != key->isprefix) {
      if (err) {
	char buffer[256];
	
	if (isprint(code))
	  sprintf(buffer, "keymap: key %c ", (char)code);
	else
	  sprintf(buffer, "keymap: key code %ld ", code);

	if (shift)
	  strcat(buffer, "+ shift ");
	if (ctrl)
	  strcat(buffer, "+ control ");
	if (alt)
	  strcat(buffer, "+ alt ");
	if (meta)
	  strcat(buffer, "+ meta ");

	strcat(buffer, "is ");
	if (!key->isprefix)
	  strcat(buffer, "not ");
	strcat(buffer, "a prefix key");

	err(errdata, buffer);
      }

      return NULL;
    }  else {
      if (strcmp(key->fname, fname)) {
	delete[] key->fname;
	key->fname = copystring(fname);
      }
      return key;
    }
  }

  newkey = new wxKeycode;

  newkey->code = code;
  newkey->shift = shift;
  newkey->ctrl = ctrl;
  newkey->alt = alt;
  newkey->meta = meta;
  newkey->fname = copystring(fname);
  newkey->next = NULL;

  newkey->seqprefix = prev;

  newkey->isprefix = (type == wxKEY_PREFIX);

  if (!keys)
    keys = new wxHashTable(wxKEY_INTEGER, 25);

  key = (wxKeycode *)keys->Get(code);
  if (!key)
    keys->Put(code, (wxObject *)newkey);
  else {
    while (key->next)
      key = key->next;
    key->next = newkey;
  }

  return newkey;
}

static struct {
  char *str;
  long code;
} keylist[] = { { "leftbutton" , WXK_MOUSE_LEFT },
		{ "rightbutton" , WXK_MOUSE_RIGHT },
		{ "middlebutton" , WXK_MOUSE_MIDDLE },
		{ "leftbuttondouble" , WXK_MOUSE_LEFT_DOUBLE },
		{ "rightbuttondouble" , WXK_MOUSE_RIGHT_DOUBLE },
		{ "middlebuttondouble" , WXK_MOUSE_MIDDLE_DOUBLE },
		{ "leftbuttontriple" , WXK_MOUSE_LEFT_TRIPLE },
		{ "rightbuttontriple" , WXK_MOUSE_RIGHT_TRIPLE },
		{ "middlebuttontriple" , WXK_MOUSE_MIDDLE_TRIPLE },
		{ "esc", WXK_ESCAPE }, 
		{ "delete", WXK_DELETE },
		{ "del", WXK_DELETE },
		{ "insert", WXK_INSERT },
		{ "ins", WXK_INSERT },
		{ "add", WXK_ADD },
		{ "subtract", WXK_SUBTRACT },
		{ "multiply", WXK_MULTIPLY },
		{ "divide", WXK_DIVIDE },
		{ "backspace", WXK_BACK },
		{ "back", WXK_BACK },
		{ "return", WXK_RETURN },
		{ "tab", WXK_TAB },
		{ "space", WXK_SPACE },
		{ "right", WXK_RIGHT },
		{ "left", WXK_LEFT },
		{ "up", WXK_UP },
		{ "down", WXK_DOWN },
		{ "home", WXK_HOME },
		{ "end", WXK_END },
		{ "pageup", WXK_PRIOR },
		{ "pagedown", WXK_NEXT },
		{ "semicolon", ';' },
		{ "colon", ':' },
		{ "numpad1", WXK_NUMPAD1 },
		{ "numpad2", WXK_NUMPAD2 },
		{ "numpad3", WXK_NUMPAD3 },
		{ "numpad4", WXK_NUMPAD4 },
		{ "numpad5", WXK_NUMPAD5 },
		{ "numpad6", WXK_NUMPAD6 },
		{ "numpad7", WXK_NUMPAD7 },
		{ "numpad8", WXK_NUMPAD8 },
		{ "numpad9", WXK_NUMPAD9 },
		{ "f1", WXK_F1 },
		{ "f2", WXK_F2 },
		{ "f3", WXK_F3 },
		{ "f4", WXK_F4 },
		{ "f5", WXK_F5 },
		{ "f6", WXK_F6 },
		{ "f7", WXK_F7 },
		{ "f8", WXK_F8 },
		{ "f9", WXK_F9 },
		{ "f10", WXK_F10 },
		{ "f11", WXK_F11 },
		{ "f12", WXK_F12 },
		{ "f13", WXK_F13 },
		{ "f14", WXK_F14 },
		{ "f15", WXK_F15 },
		{ "f16", WXK_F16 },
		{ "f17", WXK_F17 },
		{ "f18", WXK_F18 },
		{ "f19", WXK_F19 },
		{ "f20", WXK_F20 },
		{ "f21", WXK_F21 },
		{ "f22", WXK_F22 },
		{ "f23", WXK_F23 },
		{ "f24", WXK_F24 },
		{ NULL, 0 }};

static long GetCode(char **keyseqp)
{
  char *keyseq = *keyseqp;
  long i, code;
#define MAX_BUF 256
  char buffer[MAX_BUF], first;

  first = buffer[0] = *(keyseq++);
  for (i = 1; *keyseq && (*keyseq != ';'); i++) {
    if (i >= MAX_BUF - 1)
      return 0;
    buffer[i] = tolower(*keyseq);
    keyseq++;
  }
  buffer[i] = 0;
  code = 0;
  if (buffer[1]) {
    buffer[0] = tolower(buffer[0]);
    for (i = 0; keylist[i].str; i++)
      if (!strcmp(buffer, keylist[i].str)) {
	code = keylist[i].code;
	break;
      }
  } else
    code = first;

  *keyseqp = keyseq;

  return code;
}

void wxKeymap::ImpliesShift(char *str)
{
  long code;
  char buffer[256];

  code = GetCode(&str);
  if (!code) {
    if (err) {
      sprintf(buffer, "implies-shift: bad key: \"%.100s\"", str);
      err(errdata, buffer);
    }
    return;
  }

  if (numImpliedShifts == allocedImplies) {
    long *old;

    old = impliesShift;
    allocedImplies = (!allocedImplies ? 40 : 2 * allocedImplies);
    impliesShift = new long[allocedImplies];
    if (old) {
      memcpy(impliesShift, old, allocedImplies * sizeof(long));
      delete[] old;
    }
  }

  impliesShift[numImpliedShifts++] = code;
}

void wxKeymap::MapFunction(char *keys, char *fname)
{
  char *keyseq = keys;
  wxKeycode *key = NULL;
  Bool shift, ctrl, alt, meta;
  int part = 1, i;
  long code;
  char *errstr;
  char buffer[256];

  while (*keyseq) {
    shift = ctrl = alt = meta = 0;
    code = 0;
    
    while (*keyseq && (*keyseq != ';')) {
      if (isspace(*keyseq)) {
      } else if (keyseq[1] == ':') {
	switch (tolower(*keyseq)) {
	case 's':
	  shift = TRUE;
	  break;
	case 'c':
	  ctrl = TRUE;
	  break;
	case 'm':
#ifdef wx_mac
	  return; // impossible
#endif
	  meta = TRUE;
	  break;
	case 'd':
#ifndef wx_mac
	  return; // impossible
#endif
	  meta = TRUE;
	  break;
	case 'a':
	  alt = TRUE;
	  break;
	default:
	  errstr = "bad modifier";
	  goto key_error;
	}
	keyseq += 2;
      } else {
	code = GetCode(&keyseq);
	if (!code) {
	  errstr = "bad keyname";
	  goto key_error;
	}
      }
    }

    if (code) {
      if ((code > 0) && (code < 256) && isalpha(code)) {
	if (shift)
	  code = toupper(code);
	else if (isupper(code))
	  shift = TRUE;
      } 

      if (!shift) {
	for (i = 0; i < numImpliedShifts; i++)
	  if (code == impliesShift[i]) {
	    shift = TRUE;
	    break;
	  }
      }

      key = MapFunction(code, shift, ctrl, alt, meta, fname, key, 
			*keyseq ? wxKEY_PREFIX : wxKEY_FINAL);
      part++;
      if (*keyseq)
	keyseq++;
    } else {
      errstr = "no non-modifier key";
      goto key_error;
    }

    if (!key)
      return;
  }

  return;

 key_error:
  if (err) {
    sprintf(buffer, "keymap: %s in keystring: \"%.100s\", part %d", 
	    errstr, keys, part);
    err(errdata, buffer);
  }
}

int wxKeymap::HandleEvent(long code, Bool shift, Bool ctrl, 
			  Bool alt, Bool meta,
			  char **fname)
{
  wxKeycode *key;

  key = FindKey(code, shift, ctrl, alt, meta, prefix);
  
  prefix = NULL;

  if (key) {
    if (key->isprefix) {
      prefix = key;
      *fname = NULL;
      return 1;
    }
    *fname = key->fname;
    return 1;
  }

  return 0;
}

void wxKeymap::SetGrabKeyFunction(wxGrabKeyFunction grab, void *grabData)
{
  grabKeyFunction = grab;
  grabKeyData = grabData;
}

void wxKeymap::RemoveGrabKeyFunction(void)
{
  grabKeyFunction = NULL;
  grabKeyData = NULL;
}

Bool wxKeymap::HandleKeyEvent(wxObject *media, wxKeyEvent &event)
{
  if (event.keyCode == WXK_SHIFT
      || event.keyCode == WXK_CONTROL
      || !event.keyCode)
    return TRUE;

  return ChainHandleKeyEvent(media, event, NULL, NULL, 0) ? TRUE : FALSE;
}

int wxKeymap::OtherHandleKeyEvent(wxObject *media, wxKeyEvent &event,
				  wxGrabKeyFunction grab, void *grabData,
				  int try_state)
{
  int i, result = 0;
  
  for (i = 0; i < chainCount; i++) {
    int r = chainTo[i]->ChainHandleKeyEvent(media, event, grab, grabData, try_state);
    if (r > 0) {
      Reset();
      return r;
    } else if (r)
      result = r;
  }
  
  return result;
}

int wxKeymap::ChainHandleKeyEvent(wxObject *media, wxKeyEvent &event,
				  wxGrabKeyFunction grab, void *grabData,
				  int try_state)
{
  char *fname;

  lastTime = event.timeStamp;
  lastButton = 0;

  if (grabKeyFunction) {
    grab = grabKeyFunction;
    grabData = grabKeyData;
  }

  if (!prefix && (try_state >= 0)) {
    int r = OtherHandleKeyEvent(media, event, grab, grabData, 1);
    
    if (r > 0)
      return r;

    if (try_state > 0)
      return r;
    else
      try_state = -1;
  } else if (prefix && (try_state < 0))
    return OtherHandleKeyEvent(media, event, grab, grabData, -1);

  if (HandleEvent(event.keyCode,
		  event.shiftDown,
		  event.controlDown,
		  event.altDown,
		  event.metaDown,
		  &fname)) {
    if (fname) {
      Reset();
      if (grab && grab(fname, this, media, event, grabData))
	return 1;
      return CallFunction(fname, media, event) ? 1 : 0;
    } else {
      if (prefix) {
	/* Just found prefix; try others */
	int r = OtherHandleKeyEvent(media, event, grab, grabData, try_state);
	if (r > 0)
	  return r;
	return -1;
      }
    }
  }

  int result = OtherHandleKeyEvent(media, event, grab, grabData, try_state);

  if (!result && grabKeyFunction)
    if (grabKeyFunction(NULL, this, media, event, grabKeyData))
      return 1;

  return result;
}

static inline long Abs(long x)
{
  if (x < 0)
    return -x;
  else
    return x;
}

void wxKeymap::SetGrabMouseFunction(wxGrabMouseFunction grab, void *grabData)
{
  grabMouseFunction = grab;
  grabMouseData = grabData;
}

void wxKeymap::RemoveGrabMouseFunction(void)
{
  grabMouseFunction = NULL;
  grabMouseData = NULL;
}

Bool wxKeymap::HandleMouseEvent(wxObject *media, wxMouseEvent &event)
{
  return ChainHandleMouseEvent(media, event, NULL, NULL, 0) ? TRUE : FALSE;
}

int wxKeymap::OtherHandleMouseEvent(wxObject *media, wxMouseEvent &event,
				    wxGrabMouseFunction grab, void *grabData,
				    int try_state)
{
  int i, result = 0;
  
  for (i = 0; i < chainCount; i++) {
    int r = chainTo[i]->ChainHandleMouseEvent(media, event, grab, grabData, try_state);
    if (r > 0) {
      Reset();
      return r;
    } else if (r)
      result = r;
  }
  
  return result;
}

int wxKeymap::ChainHandleMouseEvent(wxObject *media, wxMouseEvent &event,
				    wxGrabMouseFunction grab, void *grabData,
				    int try_state)
{
  long code, origCode, lastCode;
  char *fname;

  if (grabMouseFunction) {
    grab = grabMouseFunction;
    grabData = grabMouseData;
  }

  if (!prefix && (try_state >= 0)) {
    int r = OtherHandleMouseEvent(media, event, grab, grabData, 1);
    
    if (r > 0)
      return r;

    if (try_state > 0)
      return r;
    else
      try_state = -1;
  } else if (prefix && (try_state < 0))
    return OtherHandleMouseEvent(media, event, grab, grabData, -1);

  if (!event.ButtonDown()) {
    Bool v;

    if (!event.Dragging() && !event.ButtonUp()) {
      /* We must have missed the button-up */
      active_mouse_function = NULL;
    }

    if (!active_mouse_function)
      return 0;
    if (grab && grab(active_mouse_function, this, media, event, grabData))
      v = 1;
    else
      v = CallFunction(active_mouse_function, media, event);
    if (event.ButtonUp())
      active_mouse_function = NULL;
    return v;
  }

  if (event.RightDown())
    code = WXK_MOUSE_RIGHT;
  else if (event.LeftDown())
    code = WXK_MOUSE_LEFT;
  else if (event.MiddleDown())
    code = WXK_MOUSE_MIDDLE;
  else
    return 0;

  origCode = code;

  if (code == lastButton && event.x == lastX && event.y == lastY) {
    if (Abs(event.timeStamp - lastTime) < doubleInterval) {
      code += WXK_CLICK_ADDER * clickCount;
      clickCount++;
    } else
      clickCount = 1;
  } else {
    lastButton = code;
    clickCount = 1;
  }
  lastTime = event.timeStamp;
  lastX = event.x;
  lastY = event.y;

  do {
    if (HandleEvent(code,
		    event.shiftDown,
		    event.controlDown,
		    event.altDown,
		    event.metaDown,
		    &fname)) {
      if (fname) {
	Reset();
	if (grab && grab(fname, this, media, event, grabData))
	  return 1;
	return CallFunction(fname, media, event) 
	  ? 1 : 0;
      } else {
	int r = OtherHandleMouseEvent(media, event, grab, grabData, try_state);

	if (r > 0)
	  return r;
	return -1;
      }
    }
    lastCode = code;
    code = origCode;
  } while (lastCode != origCode);

  int result = OtherHandleMouseEvent(media, event, grab, grabData, try_state);

  if (!result && grabMouseFunction)
    if (grabMouseFunction(NULL, this, media, event, grabMouseData))
      return 1;
  
  return result;
}

void wxKeymap::AddKeyFunction(char *name, wxKeyFunction func, void *data)
{
  wxKeyFunc *f;

  if (!keyfunctions)
    keyfunctions = new wxHashTable(wxKEY_STRING, 50);

  f = new wxKeyFunc(name, func, data);
  if (keyfunctions->Get(f->name))
    keyfunctions->Delete(f->name);
  keyfunctions->Put(f->name, (wxObject *)f);
}
  
void wxKeymap::AddMouseFunction(char *name, wxMouseFunction func, 
				void *data)
{
  wxMouseFunc *f;

  if (!mousefunctions)
    mousefunctions = new wxHashTable(wxKEY_STRING, 50);

  f = new wxMouseFunc(name, func, data);
  if (mousefunctions->Get(f->name))
    mousefunctions->Delete(f->name);
  mousefunctions->Put(f->name, (wxObject *)f);
}
  
Bool wxKeymap::CallFunction(char *name, wxObject *media, wxKeyEvent &event,
			    Bool try_chained)
{
  wxKeyFunc *f;

  if (keyfunctions) {
    f = (wxKeyFunc *)keyfunctions->Get(name);
    if (f)
      return f->Call(media, event);
  }

  if (try_chained) {
    int i;

    for (i = 0; i < chainCount; i++)
      if (chainTo[i]->CallFunction(name, media, event, TRUE))
	return TRUE;
  } else if (err) {
    char buffer[256];
    
    sprintf(buffer, "keymap: no key function \"%s\"", name);
    err(errdata, buffer);
  }

  return 0;
}

Bool wxKeymap::CallFunction(char *name, wxObject *media, wxMouseEvent &event,
			    Bool try_chained)
{
  wxMouseFunc *f;

  active_mouse_function = name;

  if (mousefunctions) {
    f = (wxMouseFunc *)mousefunctions->Get(name);
    if (f)
      return f->Call(media, event);
  }

  if (try_chained) {
    int i;

    for (i = 0; i < chainCount; i++)
      if (chainTo[i]->CallFunction(name, media, event, TRUE))
	return TRUE;
  } else if (err) {
    char buffer[256];
    
    sprintf(buffer, "keymap: no mouse function \"%s\"", name);
    err(errdata, buffer);
  }

  return FALSE;
}

void wxKeymap::SetErrorCallback(wxKeyErrorFunction f, void *d)
{
  err = f;
  errdata = d;
}

void wxKeymap::AdjustUsage(Bool newUser)
{
  if (newUser)
    usage++;
  else
    --usage;
}

Bool wxKeymap::IsUsed(void)
{
  return (usage != 0);
}

long wxKeymap::GetDoubleClickInterval()
{
  return doubleInterval;
}

void wxKeymap::SetDoubleClickInterval(long d)
{
  doubleInterval = d;
}

Bool wxKeymap::CycleCheck(wxKeymap *km)
{
  int i;

  for (i = 0; i < chainCount; i++)
    if (PTREQ(km, chainTo[i]) || chainTo[i]->CycleCheck(km))
      return TRUE;
  
  return FALSE;
}

typedef wxKeymap *wxKeymapPtr;

void wxKeymap::ChainToKeymap(wxKeymap *km, Bool prefix)
{
  wxKeymap **old;
  
  if ((km == this) || CycleCheck(km) || km->CycleCheck(this))
    return;

  old = chainTo;
  chainTo = new wxKeymapPtr[chainCount + 1];

  memcpy(chainTo + (prefix ? 1 : 0), old, chainCount * sizeof(wxKeymap *));
  chainTo[prefix ? 0 : chainCount] = km;

  if (old)
    delete[] old;

  chainCount++;

  km->AdjustUsage(TRUE);
}

void wxKeymap::RemoveChainedKeymap(wxKeymap *km)
{
  int i;

  for (i = 0; i < chainCount; i++)
    if (PTREQ(km, chainTo[i]))
      break;
  
  if (i >= chainCount)
    return;

  memcpy(chainTo + i, chainTo + i + 1, 
	 sizeof(wxKeymap *) * (chainCount - i - 1));

  chainCount--;

  km->AdjustUsage(FALSE);
#if !WXGARBAGE_COLLECTION_ON
  if (!km->IsUsed())
    delete km;
#endif
}

/***************************************************************/

wxKeyFunc::wxKeyFunc(char *fname, wxKeyFunction func, void *d)
{
  name = copystring(fname);
  f = func;
  data = d;
}

#if !WXGARBAGE_COLLECTION_ON
wxKeyFunc::~wxKeyFunc()
{
  delete[] name;
}
#endif

Bool wxKeyFunc::Call(wxObject *media, wxKeyEvent &event)
{
  return f(media, event, data);
}

/***************************************************************/

wxMouseFunc::wxMouseFunc(char *fname, wxMouseFunction func, void *d)
{
  name = copystring(fname);
  f = func;
  data = d;
}

#if !WXGARBAGE_COLLECTION_ON
wxMouseFunc::~wxMouseFunc()
{
  delete[] name;
}
#endif

Bool wxMouseFunc::Call(wxObject *media, wxMouseEvent &event)
{
  return f(media, event, data);
}
