// event.cpp -- event-related functions

#include "stdafx.h"

#include <objbase.h>
#include <mshtml.h>
#include <initguid.h>
#include <winnls.h>
#include <exdisp.h>

#include "escheme.h"

#include "myspage.h"
#include "myssink.h"

#include "mysterx.h"

// number of elts should be same as in EVENT_TYPE enumeration
WCHAR *eventNames[11]; 

static BOOL html_event_available(MX_Browser_Object *browser) {
  VARIANT_BOOL val;

  val = 0;
  browser->pIEventQueue->get_EventAvailable(&val);

  return val;
}

static void html_event_sem_fun(MX_Browser_Object *browser,void *fds) {
  scheme_add_fd_eventmask(fds,QS_ALLEVENTS);
  scheme_add_fd_handle(browser->readSem,fds,TRUE); 
}

Scheme_Object *mx_block_until_event(int argc,Scheme_Object **argv) {
  if (MX_BROWSERP(argv[0]) == FALSE) {
    scheme_wrong_type("block-until-event","mx-browser",0,argc,argv) ;
  }

  scheme_block_until((int (*)(Scheme_Object *))html_event_available,
  		     (void (*)(Scheme_Object *,void *))html_event_sem_fun,
  		     argv[0],0.0F);

  return scheme_void;
}

void initEventNames(void) {
  eventNames[click] = L"click";
  eventNames[dblclick] = L"dblclick";
  eventNames[error] = L"error";
  eventNames[keydown] = L"keydown";
  eventNames[keypress] = L"keypress";
  eventNames[keyup] = L"keyup";
  eventNames[mousedown] = L"mousedown";
  eventNames[mousemove] = L"mousemove";
  eventNames[mouseout] = L"mouseout";
  eventNames[mouseover] = L"mouseover";
  eventNames[mouseup] = L"mouseup";
}

IEvent *getEventInterface(Scheme_Object *ev,char *fname) {
  if (MX_EVENTP(ev) == FALSE) {
    scheme_wrong_type(fname,"com-event",-1,0,&ev) ;
  }

  return MX_EVENT_VAL(ev);
}


Scheme_Object *mx_event_tag(int argc,Scheme_Object **argv) {
  BSTR tag;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"mx-event-tag");

  pEvent->get_srcTag(&tag);

  return BSTRToSchemeString(tag);
}

Scheme_Object *mx_event_id(int argc,Scheme_Object **argv) {
  BSTR id;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"mx-event-id");
  
  pEvent->get_srcId(&id);

  return BSTRToSchemeString(id);
}

Scheme_Object *mx_event_from_tag(int argc,Scheme_Object **argv) {
  BSTR tag;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"mx-event-from-tag");

  pEvent->get_fromTag(&tag);

  return BSTRToSchemeString(tag);
}

Scheme_Object *mx_event_from_id(int argc,Scheme_Object **argv) {
  BSTR id;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"mx-event-from-id");
  
  pEvent->get_fromId(&id);

  return BSTRToSchemeString(id);
}

Scheme_Object *mx_event_to_tag(int argc,Scheme_Object **argv) {
  BSTR tag;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"mx-event-to-tag");
  
  pEvent->get_toTag(&tag);

  return BSTRToSchemeString(tag);
}

Scheme_Object *mx_event_to_id(int argc,Scheme_Object **argv) {
  BSTR id;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"mx-event-to-id");
  
  pEvent->get_toId(&id);

  return BSTRToSchemeString(id);
}

Scheme_Object *mx_event_keycode(int argc,Scheme_Object **argv) {
  long code;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"mx-event-keycode");
  
  pEvent->get_keyCode(&code);

  return scheme_make_integer(code);
}

Scheme_Object *mx_event_shiftkey(int argc,Scheme_Object **argv) {
  VARIANT_BOOL vb;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"mx-event-shiftkey");
  
  pEvent->get_shiftPressed(&vb);

  return (vb == 0) ? scheme_false : scheme_true;
}

Scheme_Object *mx_event_altkey(int argc,Scheme_Object **argv) {
  VARIANT_BOOL vb;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"mx-event-altkey");
  
  pEvent->get_altPressed(&vb);

  return (vb == 0) ? scheme_false : scheme_true;
}

Scheme_Object *mx_event_ctrlkey(int argc,Scheme_Object **argv) {
  VARIANT_BOOL vb;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"mx-event-ctrlkey");
  
  pEvent->get_ctrlPressed(&vb);

  return (vb == 0) ? scheme_false : scheme_true;
}

Scheme_Object *mx_event_x(int argc,Scheme_Object **argv) {
  long x;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"mx-event-x");
  
  pEvent->get_x(&x);

  return scheme_make_integer(x);
}

Scheme_Object *mx_event_y(int argc,Scheme_Object **argv) {
  long y;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"mx-event-y");
  
  pEvent->get_y(&y);

  return scheme_make_integer(y);
}

Scheme_Object *mx_event_type_pred(int argc,Scheme_Object **argv,WCHAR *evType) {
  EVENT_TYPE actualType;
  IEvent *pEvent;

  pEvent = getEventInterface(argv[0],"event-<event-type>?");

  pEvent->get_eventType(&actualType);

  if (wcscmp(evType,eventNames[actualType]) == 0) {
      return scheme_true;
  }
  
  return scheme_false;
}
  
Scheme_Object *mx_event_pred(int argc,Scheme_Object **argv) {
  if (MX_EVENTP(argv[0])) {
    return scheme_true;
  }
  return scheme_false;
}

Scheme_Object *mx_event_keypress_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"keypress");
}

Scheme_Object *mx_event_keydown_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"keydown");
}

Scheme_Object *mx_event_keyup_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"keyup");
}

Scheme_Object *mx_event_mousedown_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"mousedown");
}

Scheme_Object *mx_event_mouseover_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"mouseover");
}

Scheme_Object *mx_event_mousemove_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"mousemove");
}

Scheme_Object *mx_event_mouseout_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"mouseout");
}

Scheme_Object *mx_event_mouseup_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"mouseup");
}

Scheme_Object *mx_event_click_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"click");
}

Scheme_Object *mx_event_dblclick_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"dblclick");
}

Scheme_Object *mx_event_error_pred(int argc,Scheme_Object **argv) {
  return mx_event_type_pred(argc,argv,L"error");
}


Scheme_Object *mx_get_event(int argc,Scheme_Object **argv) {
  HRESULT hr;
  IEvent *pEvent;
  IEventQueue *pEventQueue;
  MX_Event *event_object;
  
  if (MX_BROWSERP(argv[0]) == FALSE) {
    scheme_wrong_type("mx-get-event","mx-browser",0,argc,argv) ;
  }

  pEventQueue = MX_BROWSER_EVENTQUEUE(argv[0]);

  pEvent = NULL; // DCOM requires this for some reason

  hr = pEventQueue->GetEvent(&pEvent);  // blocking call

  if (hr != S_OK || pEvent == NULL) {
    codedComError("Error retrieving event",hr);
  }

  pEvent->AddRef();

  event_object = (MX_Event *)scheme_malloc(sizeof(MX_Event));

  event_object->type = mx_event_type;
  event_object->released = FALSE;
  event_object->pEvent = pEvent;

  mx_register_simple_com_object((Scheme_Object *)event_object,pEvent);

  return (Scheme_Object *)event_object;
}
