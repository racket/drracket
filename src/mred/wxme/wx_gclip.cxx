/*
 * File:        wx_gclip.cc
 * Purpose:     a generalized clipboard class
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 1995, Matthew Flatt
 */

#include "wx_main.h"
#include "wx_gclip.h"
#include "wx_utils.h"

#if defined(wx_msw) || defined(wx_mac)
#define CLIP_USE_WX_UTILS
#endif

#ifdef CLIP_USE_WX_UTILS
#include "wx_clipb.h"
#endif

wxClipboard *wxTheClipboard;

#ifdef wx_xview
#include <xview/sel_pkg.h>
extern Xv_Server xview_server;

#define ATOM(atom) xv_get(xview_server, SERVER_ATOM, atom)
#define VALUE_TYPE Xv_opaque
#endif

#ifdef wx_motif
#define ATOM(atom) XInternAtom(XtDisplay(wxTheApp->topLevel), atom, FALSE)
#define VALUE_TYPE void*
#endif

#ifdef wx_x
Atom xa_text, xa_targets;
#endif

void wxInitClipboard(void)
{
  if (!wxTheClipboard)
    wxTheClipboard = new wxClipboard;
#ifdef wx_x
  xa_text = ATOM("TEXT");
  xa_targets = ATOM("TARGETS");
#endif
}

wxClipboard::wxClipboard()
{
  clipOwner = NULL;
  cbString = NULL;
#ifdef wx_xview
  sel_owner = 0;
#endif
}

wxClipboard::~wxClipboard()
{
  if (clipOwner)
    clipOwner->BeingReplaced();
  if (cbString)
    delete[] cbString;
}

#ifdef CLIP_USE_WX_UTILS

static int FormatStringToID(char *str)
{
  if (!strcmp(str, "TEXT"))
    return wxCF_TEXT;
  
  return wxRegisterClipboardFormat(str);
}

#endif

#ifdef wx_x

#ifdef wx_motif
static Boolean wxConvertClipboard(Widget w, Atom *selection, Atom *target,
				  Atom *type_return, XtPointer *value_return,
				  unsigned long *length_return,
				  int *format_return)
#endif
#ifdef wx_xview
static int wxConvertClipboard(Selection_owner sel, Atom *target,
			      Xv_opaque *value_return, long *length_return,
			      int *format_return)
#endif
{
  wxClipboard *cb;
  Atom xa;
  char **formats;
  int i, count, extra;

  cb = wxTheClipboard;

  if (*target == xa_targets) {
    if (cb->clipOwner) {
      count = cb->clipOwner->formats.Number();
      extra = (cb->clipOwner->formats.Member("TEXT")) ? 1 : 0;
      cb->receivedTargets = new Atom[count + extra];
      formats = cb->clipOwner->formats.ListToArray(FALSE);
      for (i = 0; i < count; i++)
	((Atom *)cb->receivedTargets)[i] = ATOM(formats[i]);
      if (extra)
	((Atom *)cb->receivedTargets)[count] = XA_STRING;
    } else {
      count = 2;
      cb->receivedTargets = new Atom[2];
      ((Atom *)cb->receivedTargets)[0] = XA_STRING;
      ((Atom *)cb->receivedTargets)[1] = xa_text;
      extra = 0;
    }

    *value_return = (VALUE_TYPE)cb->receivedTargets;
#ifdef wx_motif
    *type_return = XA_ATOM;
#else
    *target = XA_ATOM;
#endif
    *format_return = 8 * sizeof(Atom);
    *length_return = count + extra;

    cb->sentString = NULL;

    return TRUE;
  } 
  
  cb->receivedTargets = NULL;

  if (cb->clipOwner) {
    formats = cb->clipOwner->formats.ListToArray(FALSE);
    for (i = cb->clipOwner->formats.Number(); i--; ) {
      xa = ATOM(formats[i]);
      if (xa == *target)
	break;
      if (xa == xa_text && *target == XA_STRING)
	break;
    }
    if (i < 0)
      return FALSE;
  } else if (*target != xa_text && *target != XA_STRING)
    return FALSE;

#ifdef wx_motif
  *type_return = XA_STRING;
#else
  *target = XA_STRING;
#endif
  *format_return = 8;
  if (cb->clipOwner) {
    cb->sentString = cb->clipOwner->GetData(formats[i], (long *)length_return);
    *value_return = (VALUE_TYPE)cb->sentString;
  } else {
    *value_return = (VALUE_TYPE)cb->cbString;
    *length_return = strlen(cb->cbString);
  }

  return TRUE;
}

#ifdef wx_motif
static void wxSelectionDone(Widget w, Atom *selection, Atom *target)
#endif
#ifdef wx_xview
static void wxSelectionDone(Selection_owner sel, Xv_opaque data, Atom target)
#endif
{
  wxClipboard *cb;

  cb = wxTheClipboard;
  if (cb->sentString) {
    delete[] cb->sentString;
    cb->sentString = NULL;
  }
  if (cb->receivedTargets)
    delete[]  cb->receivedTargets;
}

#ifdef wx_motif
static void wxLoseClipboard(Widget w, Atom *selection)
#endif
#ifdef wx_xview
static void wxLoseClipboard(Selection_owner sel)
#endif
{
  wxClipboard *cb;

  cb = wxTheClipboard;
  
  if (cb->clipOwner) {
    cb->clipOwner->BeingReplaced();
    cb->clipOwner = NULL;
  }
  if (cb->cbString) {
    delete[] cb->cbString;
    cb->cbString = NULL;
  }
}

#endif

void wxClipboard::SetClipboardClient(wxClipboardClient *client, long time)
{
  Bool got_selection;

  if (clipOwner)
    clipOwner->BeingReplaced();
  clipOwner = client;
  if (cbString) {
    delete[] cbString;
    cbString = NULL;
  }

#ifdef wx_x
#ifdef wx_motif
  got_selection = XtOwnSelection(wxTheApp->topLevel, XA_PRIMARY, time,
				 wxConvertClipboard, wxLoseClipboard, 
				 wxSelectionDone);
#endif
#ifdef wx_xview
  if (!sel_owner) {
    Frame x_frame = (Frame)wxTheApp->wx_frame->handle;
    sel_owner = xv_create(x_frame, SELECTION_OWNER, NULL);
  }

  xv_set(sel_owner, SEL_OWN, TRUE,
	 SEL_CONVERT_PROC, wxConvertClipboard,
	 SEL_LOSE_PROC, wxLoseClipboard,
	 SEL_DONE_PROC, wxSelectionDone,
	 NULL);

  got_selection = TRUE;
#endif

#else
  if (wxOpenClipboard()) {
    char **formats, *data;
    int i;
    int ftype;
    long size;

    wxEmptyClipboard();

    formats = clipOwner->formats.ListToArray(FALSE);
    for (i = clipOwner->formats.Number(); i--; ) {
      ftype = FormatStringToID(formats[i]);
      data = clipOwner->GetData(formats[i], &size);
      if (!wxSetClipboardData(ftype, (wxObject *)data, size, 1)) {
	got_selection = FALSE;
	break;
      }
    }

    if (i < 0)
      got_selection = wxCloseClipboard();
  } else
    got_selection = FALSE;
  
  got_selection = FALSE; // Assume another process takes over

#endif

  if (!got_selection) {
    clipOwner->BeingReplaced();
    clipOwner = NULL;
  }
}

wxClipboardClient *wxClipboard::GetClipboardClient()
{
  return clipOwner;
}

void wxClipboard::SetClipboardString(char *str, long time)
{
  Bool got_selection;

  if (clipOwner) {
    clipOwner->BeingReplaced();
    clipOwner = NULL;
  }
  if (cbString)
    delete[] cbString;

  cbString = str;

#ifdef wx_x

#ifdef wx_motif
  got_selection = XtOwnSelection(wxTheApp->topLevel, XA_PRIMARY, time,
				 wxConvertClipboard, wxLoseClipboard, NULL);
#endif
#ifdef wx_xview
  if (!sel_owner) {
    Frame x_frame = (Frame)wxTheApp->wx_frame->handle;
    sel_owner = xv_create(x_frame, SELECTION_OWNER, NULL);
  }

  xv_set(sel_owner, SEL_OWN, TRUE,
	 SEL_CONVERT_PROC, wxConvertClipboard,
	 SEL_LOSE_PROC, wxLoseClipboard,
	 SEL_DONE_PROC, wxSelectionDone,
	 NULL);

  got_selection = TRUE;
#endif

#else

  if (wxOpenClipboard()) {    
    wxEmptyClipboard();
    if (!wxSetClipboardData(wxCF_TEXT, (wxObject *)str))
      got_selection = FALSE;
    else
      got_selection = wxCloseClipboard();
  } else
    got_selection = FALSE;

  got_selection = FALSE; // Assume another process takes over

#endif

  if (!got_selection) {
    delete[] cbString;
    cbString = NULL;
  }
}

#ifdef wx_motif
static void wxGetTargets(Widget w, XtPointer cbv, Atom *sel, Atom *type,
			 XtPointer value, unsigned long *len, int *format)
{
  wxClipboard *cb;

  cb = (wxClipboard *)cbv;
  if (*len <= 0) {
    cb->receivedTargets = (void *)1; /* To break the waiting loop */
    cb->receivedLength = 0;
  } else {
    cb = (wxClipboard *)cbv;
    cb->receivedTargets = new Atom[*len];
    memcpy(cb->receivedTargets, value, *len * sizeof(Atom));
    cb->receivedLength = *len;
  }
}

static void wxGetSelection(Widget w, XtPointer cbv, Atom *sel, Atom *type,
			   XtPointer value, unsigned long *len, int *format)
{
  wxClipboard *cb;

  cb = (wxClipboard *)cbv;
  cb->receivedString = new char[*len + 1];
  memcpy(cb->receivedString, value, *len);
  cb->receivedString[*len] = 0;
  cb->receivedLength = *len;
}
#endif

char *wxClipboard::GetClipboardString(long time)
{
  char *str;
  long length;

  str = GetClipboardData("TEXT", &length, time);
  if (!str) {
    str = new char[1];
    *str = 0;
  }

  return str;
}

char *wxClipboard::GetClipboardData(char *format, long *length, long time)
{
  if (clipOwner)  {
    if (clipOwner->formats.Member(format))
      return clipOwner->GetData(format, length);
    else
      return NULL;
  } else if (cbString) {
    if (!strcmp(format, "TEXT"))
      return copystring(cbString);
    else
      return NULL;
  } else {
#ifdef wx_x
    receivedString = NULL;
    receivedTargets = NULL;

#ifdef wx_motif
    XtGetSelectionValue(wxTheApp->topLevel, XA_PRIMARY,
			xa_targets, wxGetTargets, (XtPointer)this, time);

    while (!receivedTargets)
      wxYield();
#endif
#if wx_xview
    Selection_requestor req;
    Frame x_frame = (Frame)wxTheApp->wx_frame->handle;
    int get_format;

    req = (Selection_requestor)xv_create(x_frame, SELECTION_REQUESTOR, 
					 SEL_TYPE, xa_targets, NULL);
    receivedTargets = (void *)xv_get(req, SEL_DATA, &receivedLength, 
				     &get_format);
    if (!receivedTargets)
      receivedLength = 0;
#endif

    Atom xa;
    long i;

    xa = ATOM(format);

    for (i = 0; i < receivedLength; i++)
      if (((Atom *)receivedTargets)[i] == xa
	  || (((Atom *)receivedTargets)[i] == XA_STRING
	      && xa == xa_text))
	break;

#ifdef wx_motif
    if (receivedLength)
      delete[] receivedTargets;
#endif
#ifdef wx_xview
    if (receivedLength)
      free(receivedTargets);
#endif

    if (i >= receivedLength)
      return NULL;

#ifdef wx_motif
    XtGetSelectionValue(wxTheApp->topLevel, XA_PRIMARY,
			xa, wxGetSelection, (XtPointer)this, 0);
    
    while (!receivedString)
      wxYield();

    *length = receivedLength;
#endif
#ifdef wx_xview
    char *str;

    xv_set(req, SEL_TYPE, xa, NULL);
    str = (char *)xv_get(req, SEL_DATA, length, &get_format);
    if (!str)
      *length = 0;
    
    if (str) {
      receivedString = new char[*length + 1];
      memcpy(receivedString, str, *length);
      receivedString[*length] = 0;
      free(str);
    } else
      receivedString = NULL;
#endif

#else
    if (wxOpenClipboard()) {
      receivedString = (char *)wxGetClipboardData(FormatStringToID(format), 
						  length);
      wxCloseClipboard();
    } else
      receivedString = NULL;
#endif

    return receivedString;
  }
}
