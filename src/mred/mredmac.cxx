/*
 * File:        mredmac.cc
 * Purpose:     MrEd MacOS event loop
 * Author:      Matthew Flatt
 * Created:     1996
 * Copyright:   (c) 1996, Matthew Flatt
 */

// #define USE_OS_QUEUE
#define SELF_SUSPEND_RESUME

#ifdef SELF_SUSPEND_RESUME
/* Note on handling Suspend/Resume events:
    Something in the handling of events messes up the sending of 
    suspend and resume events. So, we ignore these events if they happen 
    to occur, but notice suspension and resumption ourselves (by testing 
    for the current process).
*/
#endif

#include "wx_main.h"
#include "wx_media.h"
#include "scheme.h"

#include "mred.h"

#include <Events.h>
#include <Processes.h>

#define SLEEP_TIME 30

static long resume_ticks;

static int dispatched = 1;

void MrEdInitFirstContext(MrEdContext *)
{
}

void MrEdInitNewContext(MrEdContext *)
{
}

void MrEdDestroyContext(MrEdFinalizedContext *)
{
}

static wxFrame *_wxWindowPtrToFrame(WindowPtr w, wxChildList *l)
{
  wxChildNode *n;

  for (n = l->First(); n; n = n->Next()) {
    wxFrame *f = (wxFrame *)n->Data();
    if (f->macWindow() == w)
      return f;
  }

  return NULL;
}

static wxFrame *wxWindowPtrToFrame(WindowPtr w, MrEdContext *c)
{
  if (c)
    return _wxWindowPtrToFrame(w, c->topLevelWindowList);
  else {
    for (c = mred_contexts; c; c = c->next) {
      wxFrame *f;
      if ((f = _wxWindowPtrToFrame(w, c->topLevelWindowList)))
	return f;
    }
  }

  return NULL;
}

typedef struct MrQueueElem {
  EventRecord event;
  RgnHandle rgn;
  struct MrQueueElem *next, *prev;
} MrQueueElem;

MrQueueElem *first, *last;

WindowPtr needsActive, needsDeactive;

static void TransferQueue(int all)
{
  EventRecord e;
  short mask;

#ifdef USE_OS_QUEUE
   if (all)
     mask = everyEvent;
   else
    mask = everyEvent - mDownMask - mUpMask - keyDownMask - keyUpMask - autoKeyMask;
#else
  mask = everyEvent;
#endif
  
  while (WaitNextEvent(mask, &e, dispatched ? SLEEP_TIME : 0, NULL)) {
    MrQueueElem *q;
    int done;
    
    dispatched = 0;

    done = 0;
#ifdef USE_OS_QUEUE
    if ((e.what == mouseDown) 
    	  || (e.what == mouseUp)
    	  || (e.what == keyDown)
    	  || (e.what == keyUp)
    	  || (e.what == autoKey)) {
    	/* Throw it away and stop */
    	return;
    } else 
#endif
    if (e.what == updateEvt) {
      WindowPtr w = (WindowPtr)e.message;
      for (q = first; q; q = q->next) {
	if ((q->event.what == updateEvt)
	    && (w == ((WindowPtr)q->event.message))) {
	  UnionRgn(((WindowRecord *)w)->updateRgn, q->rgn, q->rgn);
	  BeginUpdate(w);
	  EndUpdate(w);
	  done = 1;
	  // break;
	}
      }
    }

    if (!done) {
      q = new MrQueueElem;
      memcpy(&q->event, &e, sizeof(EventRecord));
      q->next = NULL;
      q->prev = last;
      if (last)
	last->next = q;
      else
	first = q;
      last = q;
      
      q->rgn = NULL;
      
      if (e.what == updateEvt) {
	WindowPtr w = (WindowPtr)e.message;
	q->rgn = NewRgn();
	CopyRgn(((WindowRecord *)w)->updateRgn, q->rgn);
	BeginUpdate(w);
	EndUpdate(w);
      } else if ((e.what == osEvt)
      		 && ((e.message >> 24) & 0x0ff) == suspendResumeMessage) {
#ifdef SELF_SUSPEND_RESUME
	/* Forget it; we do fg/bg ourselves. See note at top. */
	last = q->prev;
	if (last)
	  last->next = NULL;
	else
	  first = NULL;
#else
	int we_are_front = e.message & resumeFlag;
	WindowPtr front = FrontWindow();

	if (we_are_front) {     
          TEFromScrap();
          resume_ticks = TickCount();
        } else {
          ZeroScrap();
          TEToScrap();
        }
        
        q->event.what = activateEvt;
        q->event.modifiers = we_are_front ? activeFlag : 0;
        q->event.message = (long)front;
#endif
      }
    }
  }
}

#ifdef USE_OS_QUEUE
static void DEQUEUE(QElemPtr q, QHdrPtr)
{
  while (1) {
    QHdrPtr osqstart = GetEvQHdr();
    EvQEl *osq = (EvQEl *)osqstart->qHead;
    
    while (osq) {
      if (osq == (EvQEl *)q)
        break;
      osq = (EvQEl *)osq->qLink;
    }
    
    if (osq) {
      TransferQueue(1);
    } else
      break;
  }
}
#endif

static void MrDequeue(MrQueueElem *q)
{
  if (q->prev)
    q->prev->next = q->next;
  else
    first = q->next;
  if (q->next)
    q->next->prev = q->prev;
  else
    last = q->prev;
}

static MrEdContext *KeyOk(int current_only)
{
  WindowPtr w;
  wxFrame *fr;
  MrEdContext *c;
  
  c = current_only ? MrEdGetContext() : NULL;
  
  w = FrontWindow();
  fr = wxWindowPtrToFrame(w, c);
  if (!fr || (c && (fr->context != (void *)c)) 
      || (!c && !((MrEdContext *)fr->context)->ready))
    return NULL;
  
  return (fr ? (MrEdContext *)fr->context : c);
}

static int WindowStillHere(WindowPtr win)
{
  WindowPtr f = FrontWindow();

  while (f) {
    if (f == win)
      return TRUE;
    f = (WindowPtr)((WindowRecord *)f)->nextWindow;
  }

  return FALSE;
}

#if defined(SELF_SUSPEND_RESUME) || defined(USE_OS_QUEUE)
static int WeAreFront()
{
  static int inited;
  static ProcessSerialNumber us;
  ProcessSerialNumber front;
  Boolean r;
  
  if (!inited) {
    GetCurrentProcess(&us);
    inited = 1;
  }
  GetFrontProcess(&front);
  SameProcess(&us, &front, &r);
  
  return r;
}
#endif

static MrEdContext *cont_event_context;
static short cont_event_context_modifiers;
static Point last_mouse;
static int last_was_front;

#ifdef RECORD_HISTORY
FILE *history;
#endif

int MrEdGetNextEvent(int check_only, int current_only,
		             EventRecord *event, MrEdContext **which)
{
  /* Search for an event. Handle clicks in non-frontmost windows
     immediately. */
#ifdef USE_OS_QUEUE
  QHdrPtr osqstart;
  EvQEl *osq, *next;
#else
  MrQueueElem *osq, *next;
#endif
  EventRecord *e, ebuf;
  MrQueueElem *q;
  MrEdContext *c, *keyOk, *fc, *foundc;
  WindowPtr window;
  wxFrame *fr;
  int found = 0;
  int saw_mdown = 0, saw_kdown = 0, we_are_front;
  
  if (!event)
    event = &ebuf;
  
  c = current_only ? MrEdGetContext() : NULL;
  
  keyOk = KeyOk(current_only);
  
#ifdef RECORD_HISTORY
  if (!history) history = fopen("history3", "w");
  fprintf(history, "%lx %lx %lx\n",
  	  c, keyOk, cont_event_context);
#endif
  
  TransferQueue(0);

#ifdef SELF_SUSPEND_RESUME 
  /* Do fg/bg ourselves. See note at top. */
  we_are_front = WeAreFront();
  if (we_are_front != last_was_front) {
     last_was_front = we_are_front;

     if (we_are_front) {     
       TEFromScrap();
       resume_ticks = TickCount();
     } else {
       ZeroScrap();
       TEToScrap();
     }
     
     WindowPtr front = FrontWindow();
  
     if (front) {
      q = new MrQueueElem;
      q->next = NULL;
      q->prev = last;
      if (last)
	last->next = q;
      else
	first = q;
      last = q;
      
      q->rgn = NULL;
    
      q->event.what = activateEvt;
      q->event.modifiers = we_are_front ? activeFlag : 0;
      q->event.message = (long)front;
    }
  }
#endif
  
  /* First service mouse & key events: */
#ifdef USE_OS_QUEUE
  osqstart = GetEvQHdr();
  osq = (EvQEl *)osqstart->qHead;
#else
  osq = first;
#endif
  while (osq 
#ifdef USE_OS_QUEUE
  	 && WeAreFront()
#endif
  	 ) {
#ifdef USE_OS_QUEUE
    next = (EvQEl *)osq->qLink;
    e = (EventRecord *)&osq->evtQWhat;
#else
    next = osq->next;
    e = &osq->event;
#endif
    switch (e->what) {
      case mouseDown:
      {
	WindowPtr window, front;
	int part;

	saw_mdown = 1;

        part = FindWindow(e->where, &window);
	front = FrontWindow();
	if (part == inMenuBar)
	  window = front;

	if (!window) {
#ifdef USE_OS_QUEUE
	  DEQUEUE((QElemPtr)osq, (QHdrPtr)osqstart);
	  osqstart = GetEvQHdr();
	  next = (EvQEl *)osqstart->qHead;
#else
	  MrDequeue(osq);
#endif
	  cont_event_context = NULL;
        } else if (window != front) {
	  fr = wxWindowPtrToFrame(window, NULL);
	  fc = fr ? (MrEdContext *)fr->context : NULL;
	  if (!fc->modal_window || (fr == fc->modal_window)) {
	    SelectWindow(window);
#ifdef USE_OS_QUEUE
	    DEQUEUE((QElemPtr)osq, (QHdrPtr)osqstart);
	    osqstart = GetEvQHdr();
	    next = (EvQEl *)osqstart->qHead;
#else
	    MrDequeue(osq);
#endif
	    cont_event_context = NULL;
	  }
	} else if (resume_ticks > e->when) {
	    /* Clicked us into foreground - toss the event */
#ifdef USE_OS_QUEUE
	    DEQUEUE((QElemPtr)osq, (QHdrPtr)osqstart);
	    osqstart = GetEvQHdr();
	    next = (EvQEl *)osqstart->qHead;
#else
	    MrDequeue(osq);
#endif
	} else {
	  foundc = keyOk;
	  if (foundc) {
	    last_mouse.h = -1;
	    found = 1;
	    if (!check_only && (part != inMenuBar)) {
	      cont_event_context = foundc;
	      cont_event_context_modifiers = e->modifiers;
	      cont_event_context_modifiers |= btnState;
	    } else
	      cont_event_context = NULL;
	  }
	}
      }
      break;
    case mouseUp:
      if (!cont_event_context) {
      	if (!saw_mdown) {
#ifdef USE_OS_QUEUE
	  DEQUEUE((QElemPtr)osq, (QHdrPtr)osqstart);
	  osqstart = GetEvQHdr();
	  next = (EvQEl *)osqstart->qHead;
#else
	  MrDequeue(osq);
#endif
        }
      } else if (keyOk == cont_event_context) {
	foundc = keyOk;
	if (foundc) {
	  found = 1;
	  if (!check_only)
	    cont_event_context = NULL;
	}
      }
      break;
    case keyDown:
    case autoKey:
      foundc = keyOk;
      if (foundc) {
#if GETS_KEY_UP
	if (!check_only)
	  cont_event_context = foundc;
#endif
	found = 1;
      }
      break;
    case keyUp:
      if (!cont_event_context) {
        if (!saw_kdown) {
#ifdef USE_OS_QUEUE
	  DEQUEUE((QElemPtr)osq, (QHdrPtr)osqstart);
	  osqstart = GetEvQHdr();
	  next = (EvQEl *)osqstart->qHead;
#else
	  MrDequeue(osq);
#endif
        }
      } else if (keyOk == cont_event_context) {
	foundc = keyOk;
	if (foundc)
	  found = 1;
	if (!check_only)
	  cont_event_context = NULL;
      }
      break;
    }

    if (found)
      break;

    osq = next;
  }

  if (found) {
    /* Remove intervening mouse/key events: */
#ifdef USE_OS_QUEUE
#if 0
    EvQEl *qq;
    for (qq = (EvQEl *)osqstart->qHead; qq && (qq != osq); qq = next) {
      next = (EvQEl *)qq->qLink;
      Dequeue((QElemPtr)qq, (QHdrPtr)osqstart);
    }
    e = (EventRecord *)&osq->evtQWhat;
#endif
#else
    MrQueueElem *qq;
    for (qq = first; qq && (qq != osq); qq = next) {
      next = qq->next;
      MrDequeue(qq);
    }
    e = &osq->event;
#endif

    if (which)
      *which = foundc;

#ifdef RECORD_HISTORY
    fprintf(history, "mouse or key\n");
    fflush(history);
#endif

    if (check_only)
      return TRUE;
    
    memcpy(event, e, sizeof(EventRecord));
#ifdef USE_OS_QUEUE
    DEQUEUE((QElemPtr)osq, (QHdrPtr)osqstart);
#else
    MrDequeue(osq);
#endif
    
    return TRUE;
  }
  
  if (keyOk) {
     if ((check_only && EventAvail(autoKeyMask, event))
         || (!check_only && GetNextEvent(autoKeyMask, event))) {
     	if (which)
     	  *which = keyOk;

#ifdef RECORD_HISTORY
	fprintf(history, "autokey\n");
	fflush(history);
#endif

     	return TRUE;
     }
  }
  
  // TransferQueue(0);
    
  /* Try activate events: */
  for (q = first; q; q = q->next) {
    switch (q->event.what) {
    case activateEvt:
      window = (WindowPtr)q->event.message;
      if (WindowStillHere(window)) {
        fr = wxWindowPtrToFrame(window, c);
        fc = fr ? (MrEdContext *)fr->context : NULL;
        if ((!c && !fr) || (!c && fc->ready) || (fc == c)) {
	  if (which)
	    *which = fc;

#ifdef RECORD_HISTORY
	  fprintf(history, "activate\n");
	  fflush(history);
#endif

	  if (check_only)
	    return TRUE;
	
	  MrDequeue(q);
	  memcpy(event, &q->event, sizeof(EventRecord));
	  return TRUE;
        }
      } else
	MrDequeue(q);
      break;
    }
  }
  
  /* Update events: */
  for (q = first; q; q = q->next) {
    switch (q->event.what) {
    case updateEvt:
      window = (WindowPtr)q->event.message;
      if (WindowStillHere(window)) {
	fr = wxWindowPtrToFrame(window, c);
	fc = fr ? (MrEdContext *)fr->context : NULL;
	if ((!c && !fr) || (!c && fc->ready) || (fc == c)) {
	  if (which)
	    *which = fc;

#ifdef RECORD_HISTORY
	  fprintf(history, "update\n");
	  fflush(history);
#endif

	  if (check_only)
	    return TRUE;
	
	  // MrDequeue(q);
	  memcpy(event, &q->event, sizeof(EventRecord));
	  return TRUE;
	}
      } else {
	DisposeRgn(q->rgn);
	MrDequeue(q);
      }
      break;
    }
  }

  /* Generate a motion event? */
  if ((cont_event_context == c) || (cont_event_context == keyOk)) {      
      GetMouse(&event->where);
      LocalToGlobal(&event->where);
      
      if ((event->where.v != last_mouse.v)
          || (event->where.h != last_mouse.h)) {
          
        if (which)
	  *which = (cont_event_context ? cont_event_context : keyOk);
        if (check_only) {
#ifdef RECORD_HISTORY
	  fprintf(history, "move or drag\n");
	  fflush(history);
#endif
          return TRUE;
        }

        last_mouse.v = event->where.v;
        last_mouse.h = event->where.h;
        
        event->what = nullEvent;
        event->when = TickCount();
        if (cont_event_context && StillDown()) {
	  /* Dragging... */
	  event->modifiers = cont_event_context_modifiers;
	  event->message = 1;
#ifdef RECORD_HISTORY
	  fprintf(history, "drag\n");
  	  fflush(history);
#endif
        } else {
          event->modifiers = 0;
	  event->message = (keyOk ? 1 : 0);
#ifdef RECORD_HISTORY
	  fprintf(history, "move\n");
  	  fflush(history);
#endif
        }
        return TRUE;
     }
  }
  
#ifdef RECORD_HISTORY
  fprintf(history, "no event\n");
  fflush(history);
#endif
  
  return FALSE;
}

extern void wxCheckFinishedSounds(void);

void MrEdDispatchEvent(EventRecord *e)
{
  dispatched = 1;

  if (e->what == updateEvt) {
    /* Find the update event for this window: */
    RgnHandle rgn;
    MrQueueElem *q;
    WindowPtr w;
    GrafPtr p;    

    w = (WindowPtr)e->message;

    for (q = first; q; q = q->next) {
      if ((q->event.what == updateEvt)
	  && (w == ((WindowPtr)q->event.message))) {
	rgn = q->rgn;
	MrDequeue(q);
	break;
      }
    }

    Point o = {0, 0};
    GetPort(&p);
    SetPort(w);
    GlobalToLocal(&o);
    OffsetRgn(rgn, o.h, o.v);
    InvalRgn(rgn);
    SetPort(p);

    DisposeRgn(rgn);
  }
    
  wxTheApp->doMacPreEvent();
  wxTheApp->doMacDispatch(e);
  wxTheApp->doMacPostEvent();
  
  wxCheckFinishedSounds();
}

int MrEdCheckForBreak(void)
{
#ifdef USE_OS_QUEUE
  QHdrPtr start;
  EvQEl *q;
#else
  MrQueueElem *q;
#endif
  EventRecord event;
  
  if (!KeyOk(TRUE))
    return 0;
  
#ifdef USE_OS_QUEUE
  start = GetEvQHdr();
  q = (EvQEl *)start->qHead;
  while (q) {
    if (q->evtQWhat == keyDown) {
      if ((((q->evtQMessage & charCodeMask) == '.') 
	   && (q->evtQModifiers & cmdKey))
      	  || (((q->evtQMessage & charCodeMask) == 3) 
	      && (q->evtQModifiers & controlKey))) {
        DEQUEUE((QElemPtr)q, (QHdrPtr)start);
        return TRUE;
      }
    }
    q = (EvQEl *)q->qLink;
  }
#else
  TransferQueue(0);

  for (q = first; q; q = q->next) {
    if (q->event.what == keyDown) {
      if ((((q->event.message & charCodeMask) == '.') 
	   && (q->event.message & cmdKey))
      	  || (((q->event.message & charCodeMask) == 3) 
	      && (q->event.modifiers & controlKey))) {
        MrDequeue(q);
        return TRUE;
      }
    }
  }
#endif
  
  return FALSE;
}

void MrEdMacSleep(float secs)
{
  secs = 0;
  
  EventRecord e;
  
#if 0
  /* This is right only if there is no TCP blocking */
  RgnHandle rgn;
  rgn = ::NewRgn();
  if (rgn) {
    Point pt;
    GetMouse(&pt);
    LocalToGlobal(&pt);
    ::SetRectRgn(rgn, pt.h - 1, pt.v - 1, pt.h + 1, pt.v + 1);
  }
#else
  RgnHandle rgn = NULL;
#endif
  
  WaitNextEvent(0, &e, secs ? secs * 60 : SLEEP_TIME, rgn);
}

