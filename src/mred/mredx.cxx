/*
 * File:        mredx.cc
 * Purpose:     MrEd X Windows event loop
 * Author:      Matthew Flatt
 * Created:     1996
 * Copyright:   (c) 1996, Matthew Flatt
 */

#define Uses_XtIntrinsic
#define Uses_XtIntrinsicP
#define Uses_XLib

#include "wx_main.h"
#include "wx_media.h"
#include "scheme.h"

#if defined(wx_motif)
#include <X11/Xlib.h>
#include <X11/keysymdef.h>
#endif

#include "mred.h"

static int short_circuit = 0, just_check = 0, checking_for_break = 0;
#ifdef wx_xt
static Widget just_this_one;
#else
static MrEdContext *just_this_one;
#endif

#ifdef wx_xt
static Widget orig_top_level;
static Widget save_top_level = 0;
#endif

static KeyCode breaking_code;
static int breaking_code_set = 0;

#ifdef wx_xt
static Widget *grab_stack, grabber;
static int grab_stack_pos = 0, grab_stack_size = 0;
#define WSTACK_INC 3

extern "C" {
  void wxAddGrab(Widget w)
    {
      if (!grab_stack_pos) {
	Widget *naya;
	grab_stack_size += WSTACK_INC;
	naya = (Widget *)scheme_malloc(grab_stack_size * sizeof(Widget));
	memcpy(naya + WSTACK_INC, grab_stack, (grab_stack_size - WSTACK_INC) * sizeof(Widget));
	grab_stack = naya;
	grab_stack_pos = WSTACK_INC;
      }

      grabber = grab_stack[--grab_stack_pos] = w;
    }

  void wxRemoveGrab(Widget w)
    {
      if (w != grabber)
	return;

      if (++grab_stack_pos < grab_stack_size)
	grabber = grab_stack[grab_stack_pos];
      else
	grabber = NULL;
    }
};
#endif

#ifdef wx_motif
Scheme_Hash_Table *registered_widgets;

void wxRegisterFrameWidget(Widget w)
{
  if (!registered_widgets) {
    /* Use SCHEME_hash_weak_ptr so elements can be deleted from the table */
    registered_widgets = scheme_hash_table(7, SCHEME_hash_weak_ptr, 0, 0);
  }

  Scheme_Bucket *b;
  
  b = scheme_bucket_from_table(registered_widgets, (const char *)w);
  b->val = (void *)MrEdGetContext();
}

void wxUnregisterFrameWidget(Widget w)
{
  if (!registered_widgets)
    return;

  if (scheme_lookup_in_table(registered_widgets, (const char *)w)) {
    Scheme_Bucket *b;
    
    b = scheme_bucket_from_table(registered_widgets, (const char *)w);
    /* Removes from hash table: */
    *(void **)b->key = NULL;
  }
}

static MrEdContext *WidgetContext(Widget w)
{
  if (!registered_widgets)
    return NULL;

  void *v = scheme_lookup_in_table(registered_widgets, (const char *)w);

  return (MrEdContext *)v;
}

typedef struct IdleCallback {
  MrEdContext *context;
  void (*f)(void *);
  void *data;
  struct IdleCallback *next;
} IdleCallback;

IdleCallback *idleList = NULL;

void wxRegsiterIdleCallback(void (*f)(void *), void *data, wxWindow *w)
{
  IdleCallback *i = new IdleCallback, *j = idleList;
  i->next = NULL;
  i->f = f;
  i->data = data;
  i->context = MrEdGetContext(w);

  if (j) {
    while (j->next)
      j = j->next;
    j->next = i;
  } else
    idleList = i;
}

#endif

#if wx_xt
Widget wxGetAppToplevel()
{
  if (save_top_level)
    return save_top_level;
  else
    return MrEdGetContext()->finalized->toplevel;
}

void wxPutAppToplevel(Widget w)
{
  save_top_level = w;
}
#endif

void MrEdInitFirstContext(MrEdContext *c)
{
#ifdef wx_xt
  orig_top_level = save_top_level;
  c->finalized->toplevel = save_top_level;
  save_top_level = 0;
#endif
}

void MrEdInitNewContext(MrEdContext *c)
{
#ifdef wx_xt
  wxInitNewToplevel();
  c->finalized->toplevel = save_top_level;
  save_top_level = 0;
#endif
}

void MrEdDestroyContext(MrEdFinalizedContext *c)
{
#ifdef wx_xt
  XtDestroyWidget(c->toplevel);
#endif
}

Window GetEventWindow(XEvent *e)
{
  Window window = 0;

#define WINCASEEX(type, record, field) case type: window = e->record.field; break
#define WINCASE(type, record) WINCASEEX(type, record, window)

  switch (e->type) {
    WINCASE(MappingNotify, xmapping);
    WINCASE(ClientMessage, xclient);
    WINCASE(SelectionClear, xselectionclear);
    WINCASEEX(SelectionNotify, xselection, requestor);
    WINCASEEX(SelectionRequest, xselectionrequest, owner);
    WINCASE(ButtonPress, xbutton);
    WINCASE(ButtonRelease, xbutton);
    WINCASE(MotionNotify, xmotion);
    WINCASE(ColormapNotify, xcolormap);
    WINCASE(EnterNotify, xcrossing);
    WINCASE(LeaveNotify, xcrossing);
    WINCASE(FocusIn, xfocus);
    WINCASE(FocusOut, xfocus);
    WINCASE(Expose, xexpose);
    WINCASEEX(GraphicsExpose, xgraphicsexpose, drawable);
    WINCASEEX(NoExpose, xnoexpose, drawable);
    WINCASE(VisibilityNotify, xvisibility);
    WINCASE(KeyPress, xkey);
    WINCASE(KeyRelease, xkey);
    WINCASE(KeymapNotify, xkeymap);
    WINCASE(PropertyNotify, xproperty);
    WINCASE(ResizeRequest, xresizerequest);
    WINCASE(CirculateNotify, xcirculate);
    WINCASE(ConfigureNotify, xconfigure);
    WINCASE(DestroyNotify, xdestroywindow);
    WINCASE(GravityNotify, xgravity);
    WINCASE(MapNotify, xmap);
    WINCASE(ReparentNotify, xreparent);
    WINCASE(UnmapNotify, xunmap);
    WINCASE(CirculateRequest, xcirculaterequest);
    WINCASE(ConfigureRequest, xconfigurerequest);
    WINCASE(MapRequest, xmaprequest);
  }

  return window;
}

#if 0
char *get_event_type(XEvent *e)
{

#define NAMECASE(type) case type: return #type;

  switch (e->type) {
    NAMECASE(MappingNotify);
    NAMECASE(ClientMessage);
    NAMECASE(SelectionClear);
    NAMECASE(SelectionNotify);
    NAMECASE(SelectionRequest);
    NAMECASE(ButtonPress);
    NAMECASE(ButtonRelease);
    NAMECASE(MotionNotify);
    NAMECASE(ColormapNotify);
    NAMECASE(EnterNotify);
    NAMECASE(LeaveNotify);
    NAMECASE(FocusIn);
    NAMECASE(FocusOut);
    NAMECASE(Expose);
    NAMECASE(GraphicsExpose);
    NAMECASE(NoExpose);
    NAMECASE(VisibilityNotify);
    NAMECASE(KeyPress);
    NAMECASE(KeyRelease);
    NAMECASE(KeymapNotify);
    NAMECASE(PropertyNotify);
    NAMECASE(ResizeRequest);
    NAMECASE(CirculateNotify);
    NAMECASE(ConfigureNotify);
    NAMECASE(DestroyNotify);
    NAMECASE(GravityNotify);
    NAMECASE(MapNotify);
    NAMECASE(ReparentNotify);
    NAMECASE(UnmapNotify);
    NAMECASE(CirculateRequest);
    NAMECASE(ConfigureRequest);
    NAMECASE(MapRequest);
  }
}
#endif

static Bool CheckPred(Display *display, XEvent *e, char *args)
{
  if (short_circuit)
    return FALSE;

#if 0
  printf("trying %s\n", get_event_type(e));
#endif

  Window window = GetEventWindow(e);
  Widget widget;

  if (window)
    widget = XtWindowToWidget(display, window);
  else
    widget = 0;

  if (widget) {
    Widget parent;
    for (parent = widget; XtParent(parent); parent = XtParent(parent));
    
#ifdef wx_motif
    MrEdContext *parent_context = WidgetContext(parent);
#endif

#if 0
    printf("parent: %lx context: %lx\n", parent, parent_context);
#endif

    if (just_this_one) {
      if (
#ifdef wx_xt
	  parent == just_this_one
#else
	  just_this_one == parent_context
#endif
	  ) {
	if (checking_for_break) {
	  if (e->type == KeyPress) {
	    if ((e->xkey.state & ControlMask) 
#if BREAKING_REQUIRES_SHIFT
		&& (e->xkey.state & ShiftMask)
#endif
		&& (e->xkey.keycode == breaking_code))
	      goto found;
	  }
	} else {
	  goto found;
	}
      } else {
#if 0
	printf("wrong eventspace (%lx != %lx)\n", just_this_one, parent_context);
#endif
      }
    } else {
      MrEdContext *c;
      
      for (c = mred_contexts; c; c = c->next)
	if (
#ifdef wx_xt
	    c->finalized->toplevel == parent
#else
	    c == parent_context
#endif
	    ) {
	  if (!c->ready) {
#if 0
	    printf("not ready\n");
#endif
	    return FALSE;
	  } else {
	    *(MrEdContext **)args = c;
	    goto found;
	  }
	}

      /* Toplevel without context; perhaps it's the main context: */
      if (checking_for_break)
	return FALSE;
      else
	goto found;
    }

  } else {
#if 0
    printf("warning: window->widget mapping failed!\n");
#endif
    if (checking_for_break)
      return FALSE;
    else
      goto found;
  }

  return FALSE;

 found:
  if (just_check) {
    short_circuit = TRUE;
    return FALSE;
  } else
    return TRUE;
}


int MrEdGetNextEvent(int check_only, int current_only, 
		     XEvent *event, MrEdContext **which)
{
  Display *d;

  if (which)
    *which = NULL;

  just_check = check_only;
#ifdef wx_xt
  just_this_one = (current_only ? wxGetAppToplevel() : (Widget)NULL);
#else
  just_this_one = (current_only ? MrEdGetContext() : NULL);
#endif

#ifdef wx_motif
  d = XtDisplay(wxTheApp->topLevel);
#else
  if (!orig_top_level)
    d = XtDisplay(save_top_level);
  else
    d = XtDisplay(orig_top_level);
#endif

  if (XCheckIfEvent(d, event, CheckPred, (char *)which)) {
    just_check = 0;
    return 1;
  } else if (short_circuit) {
    short_circuit = 0;
    return 1;
  }

#ifdef wx_motif
  if (idleList) {
    MrEdContext *c = MrEdGetContext();
    IdleCallback *i = idleList, *prev = NULL, *x;

    while (i) {
      if (!current_only || (idleList->context == c)) {
	x = i;
	i = i->next;
	if (prev)
	  prev->next = i;
	else
	  idleList = i;

	x->f(x->data);
      } else {
	prev = i;
	i = i->next;
      }
    }
  }
#endif

  return 0;
}

Scheme_Hash_Table *disabled_widgets;

void wxSetSensitive(Widget w, Bool enabled)
{
  if (!disabled_widgets) {
    if (enabled)
      return;

    /* Use SCHEME_hash_weak_ptr so elements can be deleted from the table */
    disabled_widgets = scheme_hash_table(7, SCHEME_hash_weak_ptr, 0, 0);
  }

  if (enabled) {
    if (scheme_lookup_in_table(disabled_widgets, (const char *)w)) {
      /* Removes from hash table: */
      scheme_change_in_table(disabled_widgets, (const char *)w, NULL);
    }
  } else {
    Scheme_Bucket *b;

    b = scheme_bucket_from_table(disabled_widgets, (const char *)w);
    b->val = (void *)0x1;
  }
}

Display *MrEdGetXDisplay(void)
{
#ifdef wx_motif
  return XtDisplay(wxTheApp->topLevel);
#else
  if (!orig_top_level)
    return XtDisplay(save_top_level);
  else
    return XtDisplay(orig_top_level);
#endif
}

void MrEdDispatchEvent(XEvent *event)
{
  if (disabled_widgets) {
    int type = event->type;
    Display *d;
    
    d = MrEdGetXDisplay();

    if ((type == KeyPress)
	|| (type == KeyRelease)
	|| (type == ButtonPress)
	|| (type == ButtonRelease)
	|| (type == MotionNotify)
	|| ((type == ClientMessage)
	    && !strcmp(XGetAtomName(d, event->xclient.message_type), "WM_PROTOCOLS")
	    && !strcmp(XGetAtomName(d, event->xclient.data.l[0]), "WM_DELETE_WINDOW"))) {
      Window window = GetEventWindow(event);
      Widget widget, ow;

      if (window)
	widget = XtWindowToWidget(d, window);
      else
	widget = 0;
      ow = widget;

      while (widget) {
#ifdef wx_xt
	if (widget == grabber)
	  break;
#endif
	if (scheme_lookup_in_table(disabled_widgets, (const char *)widget)) {
#if 0
	  printf("disabled for %s: %lx from %lx\n", get_event_type(event), widget, ow);
#endif
#ifdef wx_motif
	  if (type == ButtonPress
	      || type == ButtonRelease) {
	    /* Mouse down: make sure the event is dispatched, but redirect it to a harmless
	       window. Otherwise, if a popup menu is active, Motif gets confused. */
	    extern wxFrame *mred_real_main_frame;
	    if (mred_real_main_frame) {
	      event->xbutton.window = XtWindow(mred_real_main_frame->clientArea);
	      break;
	    }
	  }
#endif
	  return;
	}
	widget = XtParent(widget);
      }
    }
  }

  XtDispatchEvent(event);
}

int MrEdCheckForBreak(void)
{
  int br;
  XEvent e;
  Display *d;

#ifdef wx_motif
  d = XtDisplay(wxTheApp->topLevel);
#else
  if (!orig_top_level)
    d = XtDisplay(save_top_level);
  else
    d = XtDisplay(orig_top_level);
#endif

  if (!breaking_code_set) {
    breaking_code = XKeysymToKeycode(d, 'c');
    breaking_code_set = 1;
  }

  XFlush(d);

  checking_for_break = 1;
  br = MrEdGetNextEvent(0, 1, &e, NULL);
  checking_for_break = 0;

  return br;
}

#ifdef wx_xt
#include "wx_timer.h"

class wxXtTimer : public wxTimer
{
public:
  XtTimerCallbackProc callback;
  XtPointer data;
  int ok;

  wxXtTimer(XtTimerCallbackProc c, XtPointer d) : wxTimer()
    {
      callback = c;
      data = d;
      ok = 1;
    }

  void Stopped() { ok = 0; }

  void Notify(void) {
    wxYield();
    if (ok)
      callback(data, NULL);
  }
};

extern "C" {

  void wxRemoveTimeOut(long timer)
    {
      ((wxTimer *)timer)->Stop();
      ((wxXtTimer *)timer)->Stopped();
    }
  
  long wxAppAddTimeOut(XtAppContext, unsigned long interval, 
		       XtTimerCallbackProc callback, XtPointer data)
    {
      wxTimer *t = new wxXtTimer(callback, data);
      t->Start(interval, TRUE);
      return (long)t;
    }
}
#endif

