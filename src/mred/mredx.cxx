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

#include "mred.h"

#include <X11/Shell.h>

static int short_circuit = 0, just_check = 0, checking_for_break = 0;
static Widget just_this_one;

static Widget orig_top_level;
static Widget save_top_level = 0;

static KeyCode breaking_code;
static int breaking_code_set = 0;

static Widget *grab_stack, grabber;
static int grab_stack_pos = 0, grab_stack_size = 0;
#define WSTACK_INC 3

extern "C" {
  void wxAddGrab(Widget w)
    {
      if (!grab_stack_pos) {
	Widget *naya;
	if (!grab_stack)
	  wxREGGLOB(grab_stack);
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

Widget wxGetAppToplevel()
{
  if (save_top_level)
    return save_top_level;
  else {
    MrEdContext *c;
    c = MrEdGetContext();
    return c->finalized->toplevel;
  }
}

void wxPutAppToplevel(Widget w)
{
  save_top_level = w;
}

void MrEdInitFirstContext(MrEdContext *c)
{
  orig_top_level = save_top_level;
  c->finalized->toplevel = save_top_level;
  save_top_level = 0;
}

void MrEdInitNewContext(MrEdContext *c)
{
  wxInitNewToplevel();
  c->finalized->toplevel = save_top_level;
  save_top_level = 0;
}

void MrEdDestroyContext(MrEdFinalizedContext *c)
{
  XtDestroyWidget(c->toplevel);
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
    WINCASE(CreateNotify, xcreatewindow);
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

static Bool CheckPred(Display *display, XEvent *e, char *args)
{
  Window window;
  Widget widget;

  if (short_circuit)
    return FALSE;

#if 0
  printf("trying %s\n", get_event_type(e));
#endif

  window = GetEventWindow(e);

  if (window) {
    widget = XtWindowToWidget(display, window);
#if 1
    if (widget)
      if (e->type == DestroyNotify)
	printf("DestroyNotified window %lx is still widget-mapped; BadWindow error is imminent.\n", window);
#endif
  } else
    widget = 0;

  if (widget) {
    Widget parent;
    for (parent = widget; XtParent(parent); parent = XtParent(parent)) {
    }
    
#if 0
    printf("parent: %lx context: %lx\n", parent, parent_context);
#endif

    if (just_this_one) {
      if (parent == just_this_one) {
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
      
      for (c = mred_contexts; c; c = c->next) {
	if (c->finalized->toplevel == parent) {
	  if (!c->ready) {
#if 0
	    printf("not ready\n");
#endif
	    return FALSE;
	  } else {
	    if (args)
	      *(MrEdContext **)args = c;
	    goto found;
	  }
	}
      }

      /* Toplevel without context; handle in the main context: */
#if 0
      printf("Can't map top-level to eventspace for %lx\n", window);
#endif
      if (checking_for_break)
	return FALSE;
      else {
	if (args)
	  *(MrEdContext **)args = NULL;
	goto found;
      }
    }

  } else {
#if 0
    printf("warning: window->widget mapping failed: %lx; event: %d; parent: %lx\n", 
	   window, e->type, ((XCreateWindowEvent *)e)->parent);
#endif
    if (checking_for_break)
      return FALSE;
    else {
      /* Toplevel without context; handle in the main context: */
      if (args)
	*(MrEdContext **)args = NULL;
      goto found;
    }
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
  just_this_one = (current_only ? wxGetAppToplevel() : (Widget)NULL);

  if (!orig_top_level)
    d = XtDisplay(save_top_level);
  else
    d = XtDisplay(orig_top_level);

  if (XCheckIfEvent(d, event, CheckPred, (char *)which)) {
    just_check = 0;
    return 1;
  } else if (short_circuit) {
    short_circuit = 0;
    return 1;
  }

  return 0;
}

static Scheme_Hash_Table *disabled_widgets;

#ifdef MZ_PRECISE_GC
static void widget_hash_indices(void *_key, long *_h, long *_h2)
{
  long lkey;
  long h, h2;
  
  lkey = (long)_key;

  h = (lkey >> 2);
  h2 = (lkey >> 3);

  *_h = h;
  *_h2 = h2;
}
#endif

void wxSetSensitive(Widget w, Bool enabled)
{
  if (!disabled_widgets) {
    if (enabled)
      return;

    /* Use SCHEME_hash_weak_ptr so elements can be deleted from the table */
    wxREGGLOB(disabled_widgets);
    disabled_widgets = scheme_hash_table(7, SCHEME_hash_weak_ptr, 0, 0);
#ifdef MZ_PRECISE_GC
    disabled_widgets->make_hash_indices = widget_hash_indices;
#endif
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

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif
/* No GC here because it's used to draw GC bitmaps */

Display *MrEdGetXDisplay(void)
{
  if (!orig_top_level)
    return XtDisplay(save_top_level);
  else
    return XtDisplay(orig_top_level);
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

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
      Window window;
      Widget widget, ow, exempt = 0;
      MrEdContext *c;
      wxWindow *ew;

      window = GetEventWindow(event);

      if (window)
	widget = XtWindowToWidget(d, window);
      else
	widget = 0;
      ow = widget;

      c = MrEdGetContext();
      ew = c->modal_window;
      if (ew) {
	wxWindow_Xintern *ph;
	ph = ew->GetHandle();
	exempt = ph->frame;
      }

      while (widget) {
	if (widget == grabber)
	  break;

	/* Only start checking the enabled state with the first
	   top-level window. That way, PreOnChar and PreOnEvent are
           called appropriately. wxWindows/Xt ensures that key and mouse
           events are not dispatched to disabled items. */

	if (XtIsSubclass(widget, transientShellWidgetClass)
	    || XtIsSubclass(widget, topLevelShellWidgetClass)) {
	  
	  if (scheme_lookup_in_table(disabled_widgets, (const char *)widget)) {
#if 0
	    printf("disabled: %lx from %lx\n", widget, ow);
#endif
	    return;
	  }
	}

	if (widget == exempt)
	  break;

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

  if (!orig_top_level)
    d = XtDisplay(save_top_level);
  else
    d = XtDisplay(orig_top_level);

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

#include "wx_timer.h"

class wxXtTimer : public wxTimer
{
public:
  XtTimerCallbackProc callback;
  XtPointer data;
  int ok;

  wxXtTimer(XtTimerCallbackProc c, XtPointer d);

  void Stopped() { ok = 0; }

  void Notify(void);
};

wxXtTimer::wxXtTimer(XtTimerCallbackProc c, XtPointer d)
: wxTimer()
{
  callback = c;
  data = d;
  ok = 1;
}

void wxXtTimer::Notify(void) {
  wxYield();
  if (ok)
    callback(data, NULL);
}


extern "C" {

  void wxRemoveTimeOut(long timer)
    {
      wxXtTimer *t;
#ifdef MZ_PRECISE_GC
      t = *(wxXtTimer **)timer;
      GC_free_immobile_box((void *)timer);
#else
      t = (wxXtTimer *)timer;
#endif

      t->Stop();
      t->Stopped();
    }
  
  long wxAppAddTimeOut(XtAppContext, unsigned long interval, 
		       XtTimerCallbackProc callback, XtPointer data)
    {
      wxTimer *t;
      t = new wxXtTimer(callback, data);
      t->Start(interval, TRUE);
#ifdef MZ_PRECISE_GC
      return (long)GC_malloc_immobile_box(t);
#else
      return (long)t;
#endif
    }
}

/***********************************************************************/

typedef struct {
  Widget w;
  wxWindow *wx;
} FindRec;

void *IsWidgetFrame(wxObject *f, void *d)
{
  FindRec *fr = (FindRec *)d;
  wxWindow_Xintern *i;
  
  i = ((wxWindow *)f)->GetHandle();
  if (i->frame == fr->w) {
    fr->wx = (wxWindow *)f;
  }

  return d;
}

static wxWindow *FindMrEdWindow(Display *d, Window xw)
{
  Widget w;
  w = XtWindowToWidget(d, xw);
  if (w) {
    FindRec fr;
    fr.w = w;
    fr.wx = NULL;
    MrEdForEachFrame(IsWidgetFrame, &fr);
    return fr.wx;
  } else {
    wxWindow *m;
    Window root, parent, *children;
    unsigned int n, i;
    if (XQueryTree(d, xw, &root, &parent, &children, &n)) {
      if (children) {
	m = NULL;
	for (i = 0; i < n; i++) {
	  m = FindMrEdWindow(d, children[i]);
	  if (m)
	    break;
	}
	XFree(children);
	return m;
      }
    }
     
    return NULL;
  }
}

wxWindow *wxLocationToWindow(int x, int y)
{
  Display *d;
  Window root, parent, *children;
  unsigned int n, i;
  XWindowAttributes a;
  wxWindow *result = NULL;

  if (!orig_top_level)
    d = XtDisplay(save_top_level);
  else
    d = XtDisplay(orig_top_level);

  if (XQueryTree(d, DefaultRootWindow(d),
		 &root, &parent, &children, &n)) {
    for (i = n; i--; ) {
      XGetWindowAttributes(d, children[i], &a);

      if (a.map_state == IsViewable
	  && (a.x <= x) && (a.x + a.width >= x)
	  && (a.y <= y) && (a.y + a.height >= y)) {
	/* Found the X window, now see if it's a MrEd window: */
	result = FindMrEdWindow(d, children[i]);
	break;
      }
    }
    
    if (children)
      XFree(children);
  }
 
 return result;
}
