/*
 * File:	wb_stdev.cc
 * Purpose:	Standard event definitions
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_stdev.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "@(#)wb_stdev.cc	1.2 5/9/94"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#ifdef __GNUG__
#pragma implementation "wx_stdev.h"
#endif

#include "common.h"
#include "wx_item.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#include "wx_stdev.h"
#include "wx_utils.h"

/*
 * Command events
 *
 */

wxCommandEvent::wxCommandEvent(WXTYPE commandType)
{
  eventClass = wxTYPE_COMMAND_EVENT;
  eventType = commandType;
  clientData = NULL;
  extraLong = 0;
  commandInt = 0;
  commandString = NULL;
  labelString = NULL;
}

Bool wxCommandEvent::ReadEvent(istream& in)
{
  switch (eventType) {
    case wxEVENT_TYPE_BUTTON_COMMAND:
      return TRUE;
    case wxEVENT_TYPE_TEXT_COMMAND:
      // @@@@@ Hugh?
      if (!wxReadString(in, &commandString))
        return FALSE;
    default:
      return FALSE;
  }
  // NOTREADED
}

Bool wxCommandEvent::WriteEvent(ostream& out)
{
  switch (eventType) {

    case wxEVENT_TYPE_BUTTON_COMMAND:
      return TRUE;

    case wxEVENT_TYPE_TEXT_COMMAND:
      wxWriteString(out, commandString);
      return TRUE;

    default:
      return FALSE;
  }
  // NOTREACHED
}

static wxEvent *wxCommandEventConstructor(WXTYPE eventClass, WXTYPE eventType)
{
  return new wxCommandEvent(eventType);
}

/*
 * Mouse events
 *
 */

wxMouseEvent::wxMouseEvent(WXTYPE commandType)
{
  eventClass = wxTYPE_MOUSE_EVENT;
  eventType = commandType;
}

wxMouseEvent& wxMouseEvent::operator =(wxMouseEvent& src)
{
  eventClass = src.eventClass;
  eventType = src.eventType;
  x = src.x;
  y = src.y;
  leftDown = src.leftDown;
  middleDown = src.middleDown;
  rightDown = src.rightDown;

  controlDown = src.controlDown;
  shiftDown = src.shiftDown;
  altDown = src.altDown;
  metaDown = src.metaDown;
  
  objectType = src.objectType;
  eventObject = src.eventObject;
  timeStamp = src.timeStamp;
  eventHandle = src.eventHandle;
  
  return *this;
}

Bool wxMouseEvent::ReadEvent(istream& in)
{
  return FALSE;
}

Bool wxMouseEvent::WriteEvent(ostream& out)
{
  return FALSE;
}

static wxEvent *wxMouseEventConstructor(WXTYPE eventClass, WXTYPE eventType)
{
  return new wxMouseEvent(eventType);
}

Bool wxMouseEvent::ControlDown(void)
{
  return controlDown;
}

Bool wxMouseEvent::ShiftDown(void)
{
  return shiftDown;
}

// Is a button event (*doesn't* mean: is any button *down*?)
Bool wxMouseEvent::IsButton(void)
{
  return (Button(-1)) ;
}

// True if was a button dclick event (1 = left, 2 = middle, 3 = right)
// or any button dclick event (but = -1)
Bool wxMouseEvent::ButtonDClick(int but)
{
  switch (but) {
    case -1:
      return (LeftDClick() || MiddleDClick() || RightDClick());
    case 1:
      return LeftDClick();
    case 2:
      return MiddleDClick();
    case 3:
      return RightDClick();
    default:
      return FALSE;
  }
  // NOTREACHED
}

// True if was a button down event (1 = left, 2 = middle, 3 = right)
// or any button down event (but = -1)
Bool wxMouseEvent::ButtonDown(int but)
{
  switch (but) {
    case -1:
      return (LeftDown() || MiddleDown() || RightDown());
    case 1:
      return LeftDown();
    case 2:
      return MiddleDown();
    case 3:
      return RightDown();
    default:
      return FALSE;
  }
  // NOTREACHED
}

// True if was a button up event (1 = left, 2 = middle, 3 = right)
// or any button up event (but = -1)
Bool wxMouseEvent::ButtonUp(int but)
{
  switch (but) {
    case -1:
      return (LeftUp() || MiddleUp() || RightUp());
    case 1:
      return LeftUp();
    case 2:
      return MiddleUp();
    case 3:
      return RightUp();
    default:
      return FALSE;
  }
  // NOTREACHED
}

// True if the given button is currently changing state
Bool wxMouseEvent::Button(int but)
{
  switch (but) {
    case -1:
      return (ButtonUp(-1) || ButtonDown(-1) || ButtonDClick(-1)) ;
    case 1:
      return (LeftDown() || LeftUp() || LeftDClick());
    case 2:
      return (MiddleDown() || MiddleUp() || MiddleDClick());
    case 3:
      return (RightDown() || RightUp() || RightDClick());
    default:
      return FALSE;
  }
  // NOTREACHED
}

Bool wxMouseEvent::LeftDown(void)
{
  return (eventType == wxEVENT_TYPE_LEFT_DOWN);
}

Bool wxMouseEvent::MiddleDown(void)
{
  return (eventType == wxEVENT_TYPE_MIDDLE_DOWN);
}

Bool wxMouseEvent::RightDown(void)
{
  return (eventType == wxEVENT_TYPE_RIGHT_DOWN);
}

Bool wxMouseEvent::LeftDClick(void)
{
  return (eventType == wxEVENT_TYPE_LEFT_DCLICK);
}

Bool wxMouseEvent::MiddleDClick(void)
{
  return (eventType == wxEVENT_TYPE_MIDDLE_DCLICK);
}

Bool wxMouseEvent::RightDClick(void)
{
  return (eventType == wxEVENT_TYPE_RIGHT_DCLICK);
}

Bool wxMouseEvent::LeftUp(void)
{
  return (eventType == wxEVENT_TYPE_LEFT_UP);
}

Bool wxMouseEvent::MiddleUp(void)
{
  return (eventType == wxEVENT_TYPE_MIDDLE_UP);
}

Bool wxMouseEvent::RightUp(void)
{
  return (eventType == wxEVENT_TYPE_RIGHT_UP);
}

Bool wxMouseEvent::Dragging(void)
{
  return ((eventType == wxEVENT_TYPE_MOTION) && (LeftIsDown() || MiddleIsDown() || RightIsDown()));
}

Bool wxMouseEvent::ButtonIsDown(int but)
{
  switch (but) {
    case -1:
      return (LeftIsDown() || MiddleIsDown() || RightIsDown());
    case 1:
      return LeftIsDown();
    case 2:
      return MiddleIsDown();
    case 3:
      return RightIsDown();
    default:
      return FALSE;
  }
  // NOTREACHED
}

Bool wxMouseEvent::LeftIsDown(void)
{
  return leftDown;
}

Bool wxMouseEvent::MiddleIsDown(void)
{
  return middleDown;
}

Bool wxMouseEvent::RightIsDown(void)
{
  return rightDown;
}

Bool wxMouseEvent::Moving(void)
{
  return (eventType == wxEVENT_TYPE_MOTION);
}

Bool wxMouseEvent::Entering(void)
{
  return (eventType == wxEVENT_TYPE_ENTER_WINDOW) ;
}

Bool wxMouseEvent::Leaving(void)
{
  return (eventType == wxEVENT_TYPE_LEAVE_WINDOW) ;
}

void wxMouseEvent::Position(float *xpos, float *ypos)
{
  *xpos = x;
  *ypos = y;
}

/*
 * Keyboard events
 *
 */

wxKeyEvent::wxKeyEvent(WXTYPE type)
{
  eventClass = wxTYPE_KEY_EVENT;
  eventType = type;
  shiftDown = FALSE;
  controlDown = FALSE;
  altDown = FALSE;
  keyCode = 0;
}

Bool wxKeyEvent::ReadEvent(istream& in)
{
  return FALSE;
}

Bool wxKeyEvent::WriteEvent(ostream& out)
{
  return FALSE;
}

static wxEvent *wxKeyEventConstructor(WXTYPE eventClass, WXTYPE eventType)
{
  return new wxKeyEvent(eventType);
}

Bool wxKeyEvent::ControlDown(void)
{
  return controlDown;
}

Bool wxKeyEvent::ShiftDown(void)
{
  return shiftDown;
}

long wxKeyEvent::KeyCode(void)
{
  return keyCode;
}

void wxKeyEvent::Position(float *xpos, float *ypos)
{
  *xpos = x;
  *ypos = y;
}

/*
 * Standard primary event handler
 *
 */

// (find-window) can use the title or widget label:
// (bind ?ok (find-window (find-top-window) "OK"))
// (send-event ButtonCommand ?ok)

// Aaargh! another problem. What if a button kills a dialog containing
// it, but a post event handler is called? The object pointer will be invalid!
// If we used IDs in a hash table, then the handler could check if the ID
// was still valid. This would need minimum coding if we put it in the
// wxWindow constructor and destructor.

static Bool wxStandardEventHandler(wxEvent *event, Bool external)
{
  switch (event->eventClass) {
    case wxTYPE_COMMAND_EVENT:
    {
      ((wxItem *)event->eventObject)->ProcessCommand(*((wxCommandEvent *)event));
      break;
    }
    default:
      return FALSE;
  }
  return FALSE;
}

/*
 * Initialize all the standard event classes
 *
 */
 
void wxInitStandardEvents(void)
{
  wxRegisterEventClass(wxTYPE_COMMAND_EVENT, wxTYPE_EVENT,
                       (wxEventConstructor) wxCommandEventConstructor,
                       "command event");
  wxRegisterEventClass(wxTYPE_MOUSE_EVENT, wxTYPE_EVENT,
                       (wxEventConstructor) wxMouseEventConstructor,
                       "mouse event");
  wxRegisterEventClass(wxTYPE_KEY_EVENT, wxTYPE_EVENT,
                       (wxEventConstructor) wxKeyEventConstructor,
                       "keyboard event");
  wxRegisterEventName(wxEVENT_TYPE_BUTTON_COMMAND, wxTYPE_COMMAND_EVENT, "ButtonCommand");
  // AND THE REST!!
}
