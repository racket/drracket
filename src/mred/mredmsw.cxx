/*
 * File:        mredmsw.cc
 * Purpose:     MrEd Windows event loop
 * Author:      Matthew Flatt
 * Created:     1996
 * Copyright:   (c) 1996, Matthew Flatt
 */


#include "wx_main.h"
#include "wx_media.h"
#include "scheme.h"
#include "wx_dialg.h"

#include "mred.h"

#include "../mzscheme/src/schwinfd.h"

#include <winsock.h>

extern long last_msg_time;

int mred_do_prepost_gm = 0;
extern void wxDoPreGM(void);
extern void wxDoPostGM(void);

void MrEdInitFirstContext(MrEdContext *c)
{
}

void MrEdInitNewContext(MrEdContext *c)
{
}

void MrEdDestroyContext(MrEdFinalizedContext *)
{
}

void wxCreatedWindow(wxWindow *w)
{
}

void wxDestroyedWindow(void *context, wxWindow *w)
{
}

extern wxWindow *wxHWNDtoWindow(HWND);

static MrEdContext *GetContext(HWND hwnd)
{
  HWND next = hwnd, wnd;
  do {
    do {
      wnd = next;
      next = GetParent(next);
    } while (next);
    next = GetWindow(wnd, GW_OWNER);
  } while (next);

  wxWindow *w;
  w = wxHWNDtoWindow(wnd);
  
  if (!w)
    return NULL;

  if (wxSubType(w->__type, wxTYPE_FRAME))
    return (MrEdContext *)((wxFrame *)w)->context;
  else if (wxSubType(w->__type, wxTYPE_DIALOG_BOX))
    return (MrEdContext *)((wxDialogBox *)w)->context;
  else
    return NULL;
}

typedef struct {
  MrEdContext *c, *c_return;
  MSG *msg;
  int remove;
  HWND wnd;
} CheckInfo;

static BOOL CALLBACK CheckWindow(HWND wnd, LPARAM param)
{
  CheckInfo *info = (CheckInfo *)param;
  MrEdContext *c;

  c = GetContext(wnd);

  if ((!info->c && (!c || c->ready))
      || (info->c == c)) {
    if (PeekMessage(info->msg, wnd, NULL, NULL, 
                    info->remove ? PM_REMOVE : PM_NOREMOVE)) {
      info->wnd = wnd;
      info->c_return = c;
      return FALSE;
    }
  }

  return TRUE;
}

int FindReady(MrEdContext *c, MSG *msg, int remove, MrEdContext **c_return)
{
  MSG backup;
  CheckInfo info;

  if (!msg)
    msg = &backup;

  info.c = c;
  info.msg = msg;
  info.remove = remove;

  if (!EnumWindows((WNDENUMPROC)CheckWindow, (LPARAM)&info)) {
    if (c_return)
      *c_return = info.c_return;
    return TRUE;
  }

  return FALSE;
}

int MrEdGetNextEvent(int check_only, int current_only, 
		     MSG *event, MrEdContext **which)
{
  MrEdContext *c;

#if 0
  /* Test code: (doesn't work with modal dialogs!) */
  if (!current_only && PeekMessage(event, NULL, NULL, NULL, 
			   check_only ? PM_NOREMOVE : PM_REMOVE)) {
    if (which)
       *which = NULL;
     return TRUE;
  } else
    return FALSE;
#endif

  if (which)
    *which = NULL;

  if (current_only)
    c = MrEdGetContext();
  else
    c = NULL;

  return FindReady(c, event, !check_only, which);

#if 0
  if (current_only) {
    c = MrEdGetContext();
    if (FindReady(c, event, !check_only)) {
      if (which)
	*which = c;
      return TRUE;
    }

    return FALSE;
  }

#if 0
  if (AnyPopup()) {
    if (PeekMessage(event, NULL, NULL, NULL, 
	            check_only ? PM_NOREMOVE : PM_REMOVE)) {
      if (which)
	*which = c;
      return TRUE;
    }
  }
#endif

  for (c = mred_contexts; c; c = c->next) {
    if (c->ready && FindReady(c, event, !check_only)) {
      if (which)
	*which = c;
      return TRUE;
    }
  }

  return FALSE;
#endif
}

void MrEdDispatchEvent(MSG *msg)
{
  if (!wxTheApp->ProcessMessage(msg)) {
    TranslateMessage(msg);
    last_msg_time = msg->time;
    DispatchMessage(msg);
  }
}

static void SaveMsg(int& msgs_count, int& msgs_size, 
		    MSG*& msgs, MSG&msg)
{
  if (msgs_count == msgs_size) {
    MSG *save = msgs;

    msgs_size = (msgs_size ? 2 * msgs_size : 10);
    msgs = new MSG[msgs_size];

    memcpy(msgs, save, msgs_count * sizeof(MSG));

    delete[] save;
  }
  memcpy(msgs + (msgs_count++), &msg, sizeof(MSG));
}

#if 0
int MrEdCheckForBreak(void)
{ 
  MSG msg;
  MSG *msgs = NULL, *omsgs = NULL;
  int msgs_size = 0, msgs_count = 0;
  int omsgs_size = 0, omsgs_count = 0;
  int i, retval = FALSE;
  Bool ctlDown = FALSE;
#if BREAKING_REQUIRES_SHIFT
  Bool shiftDown = FALSE;
#endif
  MrEdContext *lc = MrEdGetContext();

  if (!::PeekMessage(&msg, NULL, WM_KEYDOWN, WM_KEYDOWN, PM_NOREMOVE))
    return FALSE;

  // To search all the messages, we have to Peek them all and then
  // Post them back. This will mess up the timing and locations of
  // these events.
  //
  // Is there a better way to do this???

  while (::PeekMessage(&msg, NULL, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE)) {
    if (GetContext(msg.hwnd) == lc) {
      if (msg.message == WM_KEYDOWN) {
	if (msg.wParam == VK_CONTROL)
	  ctlDown = TRUE;
#if BREAKING_REQUIRES_SHIFT
	else if (msg.wParam == VK_SHIFT)
	  shiftDown = TRUE;
#endif
	else if (ctlDown 
#if BREAKING_REQUIRES_SHIFT
		 && shiftDown
#endif
		 && (msg.wParam == 'C')) {
	  delete[] msgs;
	  retval = TRUE;
	  msgs_count = 0;
	}
      } else if (msg.message == WM_KEYUP) {
	if (msg.wParam == VK_CONTROL)
	  ctlDown = FALSE;
#if BREAKING_REQUIRES_SHIFT
	else if (msg.wParam == VK_SHIFT)
	  shiftDown = FALSE;
#endif
      }
      SaveMsg(msgs_count, msgs_size, msgs, msg);
    } else
      SaveMsg(omsgs_count, omsgs_size, omsgs, msg);
  }

  for (i = 0; i < msgs_count; i++)
    ::PostMessage(msgs[i].hwnd, msgs[i].message, 
		  msgs[i].wParam, msgs[i].lParam);
  for (i = 0; i < omsgs_count; i++)
    ::PostMessage(omsgs[i].hwnd, omsgs[i].message, 
		  omsgs[i].wParam, omsgs[i].lParam);

  delete[] msgs;

  return retval;
}
#endif

int MrEdCheckForBreak(void)
{
  HWND w = GetActiveWindow();
  if (MrEdGetContext() != GetContext(w))
    return 0;

  SHORT hit = 0x8000;
  SHORT hitnow = 0x0001;
  SHORT c = GetAsyncKeyState('C');
#if BREAKING_REQUIRES_SHIFT
  SHORT shift = GetAsyncKeyState(VK_SHIFT);
#else
  SHORT shift = hit;
#endif
  SHORT control = GetAsyncKeyState(VK_CONTROL);
  
  return ((c & hit) && (c & hitnow) && (control & hit) && (shift & hit));
}

static long signal_waitdone(void *)
{
  WaitMessage();
  return 0;
}

static long signal_fddone(void *fds)
{
  win_extended_fd_set *r = (win_extended_fd_set *)fds;
  win_extended_fd_set *w = ((win_extended_fd_set *)fds) + 1;
  win_extended_fd_set *e = ((win_extended_fd_set *)fds) + 2;
  
  select(0, &r->set, &w->set, &e->set, 0);

  return 0;
}

static HANDLE wait_msg_thread = NULL;

void MrEdMSWSleep(float secs, void *fds)
{
  if (mred_do_prepost_gm)
    wxDoPreGM();

  win_extended_fd_set *r, *w, *e;
  
  if (fds) {
    r = (win_extended_fd_set *)fds;
    w = ((win_extended_fd_set *)fds) + 1;
    e = ((win_extended_fd_set *)fds) + 2;
  } else
    r = w = e = NULL;
    
  /* Block: use different stratgey if there are handles or fds to watch: */
  if (fds && ((r->set.fd_count || w->set.fd_count || e->set.fd_count)
              || r->num_handles)) {
      
    int num_handles = r->num_handles, *rps, two_rps[2];
    HANDLE *handles, two_handles[2];
    SOCKET fake;

    if (num_handles) {
      /* handles has been set up with an extra couple of slots: */ 
      handles = r->handles;
      rps = r->repost_sema;
    } else {
      handles = two_handles;
      rps = two_rps;
    }

  
    HANDLE th1, th2;
    DWORD result;
    DWORD id;

    if (r->set.fd_count || w->set.fd_count || e->set.fd_count) {
      fake = socket(PF_INET, SOCK_STREAM, 0);
      FD_SET(fake, e);

      th2 = CreateThread(NULL, 5000, 
	              (LPTHREAD_START_ROUTINE)signal_fddone,
		      fds, 0, &id);
    
      rps[num_handles] = 0;
      handles[num_handles++] = th2;
    } else
      th2 = NULL;

    if (wait_msg_thread)
      if (WaitForSingleObject(wait_msg_thread, 0) == WAIT_OBJECT_0) {
	CloseHandle(wait_msg_thread);
	wait_msg_thread = NULL;
      }

    if (!wait_msg_thread)
      wait_msg_thread = CreateThread(NULL, 5000, 
				     (LPTHREAD_START_ROUTINE)signal_waitdone,
				     NULL, 0, &id);

    rps[num_handles] = 0;
    handles[num_handles++] = wait_msg_thread;

    result = WaitForMultipleObjects(num_handles, handles, FALSE, 
                                    secs ? (DWORD)(secs * 1000) : INFINITE);

    if ((result >= WAIT_OBJECT_0) && (result < WAIT_OBJECT_0 + num_handles)) {
      result -= WAIT_OBJECT_0;
      if (rps[result])
        ReleaseSemaphore(handles[result], 1, NULL);
    }

    if (th2) {
      closesocket(fake);
      CloseHandle(th2);
    }
  } else {
    UINT id;
    
    if (secs)
      id = SetTimer(NULL, 0, secs * 1000, NULL);
    else
      id = 0;
  
    if (wxTheApp->keep_going)
      WaitMessage();

    if (secs)
      KillTimer(NULL, id);
  }
  
  if (mred_do_prepost_gm)
    wxDoPostGM();
}
