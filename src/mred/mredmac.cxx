/*
 * File:        mredmac.cc
 * Purpose:     MrEd MacOS event loop
 * Author:      Matthew Flatt
 * Created:     1996
 * Copyright:   (c) 1996, Matthew Flatt
 */

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

#define FG_SLEEP_TIME 0
#define BG_SLEEP_TIME 30
#define DELAY_TIME 5

static long resume_ticks;

static int dispatched = 1;

static int QueueTransferredEvent(EventRecord *e);

void MrEdInitFirstContext(MrEdContext *)
{
  scheme_handle_aewait_event = (void (*)(EventRecord*))QueueTransferredEvent;
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

static MrQueueElem *first, *last;

static int last_was_front;

static int QueueTransferredEvent(EventRecord *e)
{
  MrQueueElem *q;
  int done;
  
  dispatched = 0;
  
  done = 0;
  if (e->what == updateEvt) {
    WindowPtr w = (WindowPtr)e->message;
    for (q = first; q; q = q->next) {
      if ((q->event.what == updateEvt)
	  && (w == ((WindowPtr)q->event.message))) {
	UnionRgn(((WindowRecord *)w)->updateRgn, q->rgn, q->rgn);
	BeginUpdate(w);
	EndUpdate(w);
	done = 1;
      }
    }
  }
    
  if (e->what == kHighLevelEvent) {
    /* We have to dispatch the event immediately */
    AEProcessAppleEvent(e);
    done = 1;
  }

  if (!done) {
    q = new MrQueueElem;
    memcpy(&q->event, e, sizeof(EventRecord));
    q->next = NULL;
    q->prev = last;
    if (last)
      last->next = q;
    else
      first = q;
    last = q;
      
    q->rgn = NULL;
      
    if (e->what == updateEvt) {
      WindowPtr w = (WindowPtr)e->message;
      q->rgn = NewRgn();
      CopyRgn(((WindowRecord *)w)->updateRgn, q->rgn);
      BeginUpdate(w);
      EndUpdate(w);
    } else if ((e->what == osEvt)
	       && ((e->message >> 24) & 0x0ff) == suspendResumeMessage) {
#ifdef SELF_SUSPEND_RESUME
      /* Forget it; we do fg/bg ourselves. See note at top. */
      last = q->prev;
      if (last)
	last->next = NULL;
      else
	first = NULL;
#else
      int we_are_front = e->message & resumeFlag;
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

  return 1;
}

/* Called by wxWindows to queue leave events: */
void QueueMrEdEvent(EventRecord *e)
{
  QueueTransferredEvent(e);
}

static void GetSleepTime(int *sleep_time, int *delay_time)
{
#if FG_SLEEP_TIME
  if (last_was_front && Button())
    *sleep_time = 0;
  else
#endif
   *sleep_time = last_was_front ? FG_SLEEP_TIME : BG_SLEEP_TIME;
   
  *delay_time = last_was_front ? DELAY_TIME : 0;
}

static int TransferQueue(int all)
{
  EventRecord e;
  short mask;
  int sleep_time, delay_time;
  
  GetSleepTime(&sleep_time, &delay_time);
  
  /* Don't call WaitNextEvent too often. */
  static unsigned long lastTime;
  if (TickCount() <= lastTime + delay_time)
    return 0;

  mask = everyEvent;
  
  while (WaitNextEvent(mask, &e, dispatched ? sleep_time : 0, NULL)) {
    if (!QueueTransferredEvent(&e))
      break;
  }
  
  lastTime = TickCount();
  
  return 1;
}

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

#if defined(SELF_SUSPEND_RESUME)
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

static int GetMods(void)
{
  KeyMap km;
  int mods = 0;
	  
  GetKeys(km);
  if (km[1] & 32768)
    mods |= cmdKey;
  if (km[1] & 1)
    mods |= shiftKey;
  if (km[1] & 4)
    mods |= optionKey;
  if (km[1] & 8)
    mods |= controlKey;
  
  return mods;
}

static MrEdContext *cont_event_context;
static WindowPtr cont_event_context_window;
static Point last_mouse;
static WindowPtr last_front_window;

#ifdef RECORD_HISTORY
FILE *history;
#endif

#define leaveEvt 42

int MrEdGetNextEvent(int check_only, int current_only,
		     EventRecord *event, MrEdContext **which)
{
  /* Search for an event. Handle clicks in non-frontmost windows
     immediately. */
  MrQueueElem *osq, *next;
  EventRecord *e, ebuf;
  MrQueueElem *q;
  MrEdContext *c, *keyOk, *fc, *foundc;
  WindowPtr window;
  wxFrame *fr;
  int found = 0, kill_context = 0;
  int saw_mup = 0, saw_mdown = 0, saw_kdown = 0, we_are_front;
  
  if (!event)
    event = &ebuf;
  
  c = current_only ? MrEdGetContext() : NULL;
  
  keyOk = KeyOk(current_only);
  
#ifdef RECORD_HISTORY
  if (!history) history = fopen("history3", "w");
  fprintf(history, "%lx %lx %lx\n",
  	  c, keyOk, cont_event_context);
#endif

  if (cont_event_context)
    if (!StillDown())
      kill_context = 1;

  if (!TransferQueue(0))
    kill_context = 0;

  if (cont_event_context)
    if (!WindowStillHere(cont_event_context_window))
      cont_event_context = NULL;
  
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
      
      if (we_are_front)
        wxSetCursor(NULL); /* reset cursor */
    }
  }
#endif
  
  /* First, service leave events: */
  for (q = first; q; q = q->next) {
    switch (q->event.what) {
    case leaveEvt:
      {
        wxWindow *win = (wxWindow *)q->event.message;

        if ((win->__type != -1) && win->IsShown()) {
          fr = (wxFrame *)win->GetRootFrame();
	      fc = fr ? (MrEdContext *)fr->context : NULL;
	      if ((!c && !fr) || (!c && fc->ready) || (fc == c)) {
	        if (which)
	          *which = fc;

#ifdef RECORD_HISTORY
	        fprintf(history, "leave\n");
	        fflush(history);
#endif

	        if (check_only)
	          return TRUE;
	
	        MrDequeue(q);
	        memcpy(event, &q->event, sizeof(EventRecord));
	        return TRUE;
	      }
        } else {
          MrDequeue(q);
        }
      }
    }
  }
  
  /* Next, service mouse & key events: */
  osq = first;
  while (osq) {
    next = osq->next;
    e = &osq->event;
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
	  MrDequeue(osq);
	  found = 1;
	  foundc = keyOk;
	  cont_event_context = NULL;
        } else if (window != front) {
          /* Handle bring-window-to-front click immediately */
          if (!WindowStillHere(window)) {
            MrDequeue(osq);
          } else {
	    fr = wxWindowPtrToFrame(window, NULL);
	    fc = fr ? (MrEdContext *)fr->context : NULL;
	    if (fc && (!fc->modal_window || (fr == fc->modal_window))) {
	      SelectWindow(window);
	      MrDequeue(osq);
	      cont_event_context = NULL;
	    } else if (fc && fc->modal_window) {
	      SysBeep(0);
	      MrDequeue(osq);
	      cont_event_context = NULL;
	      SelectWindow(((wxFrame *)fc->modal_window)->macWindow());
	    }
	  }
	} else if (resume_ticks > e->when) {
	  /* Clicked MrEd into foreground - toss the event */
	  MrDequeue(osq);
	} else {
	  foundc = keyOk;
	  if (foundc) {
	    last_mouse.h = -1;
	    found = 1;
	    if (!check_only && (part != inMenuBar)) {
	      cont_event_context = foundc;
	      cont_event_context_window = window;
	      kill_context = 0;
	    } else
	      cont_event_context = NULL;
	  }
	}
      }
      break;
    case mouseUp:
      saw_mup = 1;
      if (!cont_event_context) {
      	if (!saw_mdown) {
	  MrDequeue(osq);
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
	found = 1;
      }
      break;
    case keyUp:
      if (!cont_event_context) {
        if (!saw_kdown) {
	  MrDequeue(osq);
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
  
  if (kill_context && !saw_mup)
    cont_event_context = NULL;
  
  if (found) {
    /* Remove intervening mouse/key events: */
    MrQueueElem *qq;
    for (qq = first; qq && (qq != osq); qq = next) {
      next = qq->next;
      switch (qq->event.what) {
        case mouseDown:
        case mouseUp:
        case keyDown:
        case keyUp:
        case autoKey:
          MrDequeue(qq);
          break;
      }
    }
    e = &osq->event;

    if (which)
      *which = foundc;

#ifdef RECORD_HISTORY
    fprintf(history, "mouse or key\n");
    fflush(history);
#endif

    if (check_only)
      return TRUE;
    
    memcpy(event, e, sizeof(EventRecord));
    MrDequeue(osq);
    
    return TRUE;
  }
  
  // TransferQueue(0);
    
  /* Try activate and high-level events: */
  for (q = first; q; q = q->next) {
    switch (q->event.what) {
    case diskEvt:
    case kHighLevelEvent:
       fc = NULL;
	   if ((!c && !fc) || (!c && fc->ready) || (fc == c)) {
	    if (which)
	      *which = fc;
        if (check_only)
          return TRUE;
        MrDequeue(q);
	    memcpy(event, &q->event, sizeof(EventRecord));
	    return TRUE;
	  }
	  break;
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
  if (keyOk) {
      GetMouse(&event->where);
      LocalToGlobal(&event->where);
      
      if (((event->where.v != last_mouse.v)
           || (event->where.h != last_mouse.h)
           || last_front_window != FrontWindow())
          && (!cont_event_context || (cont_event_context == keyOk))) {
          
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
        last_front_window = FrontWindow();
        
        event->what = nullEvent;
        event->when = TickCount();
        if (cont_event_context) {
	  /* Dragging... */
	  event->modifiers = GetMods() | btnState;
	  event->message = 1;
#ifdef RECORD_HISTORY
	  fprintf(history, "drag\n");
  	  fflush(history);
#endif
        } else {
          event->modifiers = (keyOk ? GetMods() : 0);
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

	if (!((WindowRecord *)w)->updateRgn)
	  ((WindowRecord *)w)->updateRgn = rgn;
	else {
      RgnHandle update = ((WindowRecord *)w)->updateRgn;
	  UnionRgn(update, rgn, update);
      DisposeRgn(rgn);
    }
  }
    
  wxTheApp->doMacPreEvent();
  wxTheApp->doMacDispatch(e);
  wxTheApp->doMacPostEvent();
  
  wxCheckFinishedSounds();
}

int MrEdCheckForBreak(void)
{
  MrQueueElem *q;
  EventRecord event;
  
  if (!KeyOk(TRUE))
    return 0;
  
  TransferQueue(0);

  for (q = first; q; q = q->next) {
    if (q->event.what == keyDown) {
      if ((((q->event.message & charCodeMask) == '.') 
	   && (q->event.modifiers & cmdKey))
      	  || (((q->event.message & charCodeMask) == 3) 
	      && (q->event.modifiers & controlKey))) {
        MrDequeue(q);
        return TRUE;
      }
    }
  }
  
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
  
  if (WaitNextEvent(0, &e, secs ? secs * 60 : BG_SLEEP_TIME, rgn))
    QueueTransferredEvent(&e);
}

/**********************************************************************/

wxWindow *wxLocationToWindow(int x, int y)
{
  return NULL;
}
