/*
 * File:        wx_mbuf.cc
 * Purpose:     wxMediaBuffer implementation
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

#define  Uses_wxPrintSetup /* for wx_xt */
#include "wx_utils.h"
#include "wx_win.h"
#include "wx_menu.h"
#include "wx_dialg.h"
#include "wx_buttn.h"
#include "wx_rbox.h"
#include "wx_check.h"
#include "wx_dcps.h"
#ifndef OLD_WXWINDOWS
#include "wx_cmdlg.h"
#endif
#include "wx_print.h"

#include "wx_media.h"
#ifndef OLD_WXWINDOWS
#include "wx_clipb.h"
#else
#include "wx_gclip.h"
#endif
#include "wx_ptreq.h"
#include <ctype.h>
#include <string.h>

#define NUM_MAX_UNDOS 500

#if ALLOW_X_STYLE_SELECTION
Bool wxMediaXSelectionMode = TRUE;
wxMediaBuffer *wxMediaXSelectionOwner = NULL;
wxMediaBuffer *wxMediaXSelectionAllowed = NULL;
static Bool xSelectionCopied = FALSE;
static Bool xClipboardHack = FALSE;
#endif

static void InitCutNPaste(void);

static void MediaStyleNotify(wxStyle *which, wxMediaBuffer *media);

class wxMediaClipboardClient : public wxClipboardClient
{
 public:
  wxMediaClipboardClient();
  char *GetData(char *format, long *size);
  void BeingReplaced(void);
};
static wxMediaClipboardClient TheMediaClipboardClient;

#if ALLOW_X_STYLE_SELECTION
class wxMediaXClipboardClient : public wxClipboardClient
{
 public:
  wxMediaXClipboardClient();
  char *GetData(char *format, long *size);
  void BeingReplaced(void);
};
static wxMediaXClipboardClient TheMediaXClipboardClient;
#endif

typedef wxChangeRecord *wxChangeRecordPtr;

wxBitmap *wxMediaBuffer::bitmap;
wxMemoryDC *wxMediaBuffer::offscreen = NULL;
long wxMediaBuffer::bmHeight, wxMediaBuffer::bmWidth;
#if defined(wx_msw) && defined(USE_SEP_WINDOW_MGR)
#define OFFSCREEN_IN_USE_INIT TRUE  /* Until we fix Win32 bug */
#else
#define OFFSCREEN_IN_USE_INIT FALSE
#endif
Bool wxMediaBuffer::offscreenInUse = OFFSCREEN_IN_USE_INIT;
wxMediaBuffer *wxMediaBuffer::lastUsedOffscreen = NULL;


/******************************************************************/

#ifndef EACH_BUFFER_OWN_OFFSCREEN
static bcounter = 0;
#endif

wxMediaBuffer::wxMediaBuffer()
{  
  map = new wxKeymap();
  map->AdjustUsage(TRUE);
  // AddBufferFunctions(map);

  styleList = new wxStyleList;
  styleList->AdjustUsage(TRUE);
  styleList->NewNamedStyle(STD_STYLE, NULL);
  notifyId = styleList->NotifyOnChange((wxStyleNotifyFunc)MediaStyleNotify, 
				       this);

  filename = NULL;

  undomode = redomode = interceptmode = FALSE;
  maxUndos = NUM_MAX_UNDOS;
  changes = new wxChangeRecordPtr[maxUndos];
  changes_start = changes_end = 0;
  redochanges =  new wxChangeRecordPtr[maxUndos];
  redochanges_start = redochanges_end = 0;

  customCursor = NULL;

  loadoverwritesstyles = 1;

  noundomode = 0;

  ownCaret = FALSE;
  caretSnip = NULL;  

  InitCutNPaste();

  admin = NULL;
  WXGC_IGNORE(admin);

#ifdef EACH_BUFFER_OWN_OFFSCREEN
  offscreen = NULL;
#endif

  if (!offscreen) {
    bitmap = NULL;
    offscreen = new wxMemoryDC();
    bmHeight = bmWidth = 0;
#ifndef wx_mac
    offscreen->SetOptimization(TRUE);
#endif
  }

  inactiveCaretThreshold = wxSNIP_DRAW_SHOW_INACTIVE_CARET;

#ifndef EACH_BUFFER_OWN_OFFSCREEN
  bcounter++;
#endif
}

wxMediaBuffer::~wxMediaBuffer()
{
#if ALLOW_X_STYLE_SELECTION
  if (wxMediaXSelectionOwner == this)
    wxMediaXSelectionOwner = NULL;
#endif

  if (map)
    SetKeymap(NULL);

  styleList->AdjustUsage(FALSE);
  styleList->ForgetNotification(notifyId);
#if !WXGARBAGE_COLLECTION_ON
  if (!styleList->IsUsed())
    delete styleList;
#endif

#ifndef EACH_BUFFER_OWN_OFFSCREEN
  --bcounter;
  if (!bcounter) {
#endif
    offscreen->SelectObject(NULL);
    delete offscreen;
    if (bitmap)
      delete bitmap;
#ifndef EACH_BUFFER_OWN_OFFSCREEN
  }
#endif

  if (filename)
    delete[] filename;

  ClearUndos();

  delete[] changes;
  delete[] redochanges;
}

/******************************************************************/

void wxMediaBuffer::OnLocalEvent(wxMouseEvent &event)
{
  if (map) {
    if (map->HandleMouseEvent(this, event))
      return;
    else if (!event.Moving())
      map->BreakSequence();
  }

  OnDefaultEvent(event);
}

void wxMediaBuffer::OnLocalChar(wxKeyEvent &event)
{
  if (map) {
    if (map->HandleKeyEvent(this, event))
      return;
    else
      map->BreakSequence();
  }

  OnDefaultChar(event);
}

void wxMediaBuffer::OnFocus(Bool WXUNUSED(on))
{
}

/******************************************************************/

void wxMediaBuffer::SetAdmin(wxMediaAdmin *administrator)
{
  SettingAdmin(administrator);

  admin = administrator;
  if (!admin)
    ownCaret = FALSE;

  if (admin)
    InitNewAdmin();
}

void wxMediaBuffer::SettingAdmin(wxMediaAdmin *)
{
}

void wxMediaBuffer::InitNewAdmin()
{
}

wxMediaAdmin *wxMediaBuffer::GetAdmin(void)
{
  return admin;
}

/******************************************************************/

#define ShowsGhostCaret() 1

Bool wxMediaBuffer::DoOwnCaret(Bool ownit)
{
  Bool refresh;

  refresh = (!caretSnip && (((Bool)ownCaret != ownit) || ShowsGhostCaret()));

  ownCaret = ownit;
  if (caretSnip)
    caretSnip->OwnCaret(ownit);
  if (map && !ownit && refresh)
    map->BreakSequence();

#if ALLOW_X_STYLE_SELECTION
  if (ownit && !caretSnip)
    wxMediaXSelectionAllowed = this;
  else if (wxMediaXSelectionAllowed == this)
    wxMediaXSelectionAllowed = NULL;
#endif

  if (admin)
    admin->UpdateCursor();

  return refresh;
}

wxDC *wxMediaBuffer::GetDC()
{
  /* This can be called by snips to get a DC appropriate for
     sizing text, etc., outside of draws. It isn't the destination 
     for draws, though. */
  if (admin)
    return admin->GetDC(NULL, NULL);
  else
    return NULL;
}

void wxMediaBuffer::GetViewSize(float *w, float *h)
{
  if (admin)
    admin->GetView(NULL, NULL, w, h);
  else {
    if (w)
      *w = 0;
    if (h)
      *h = 0;
  }
}

Bool wxMediaBuffer::DoSetCaretOwner(wxSnip *snip, int dist)
{
  Bool hadCaret, visCaret;
  wxSnip *oldCaret;
  Bool refresh;

  if (PTREQ(snip, caretSnip)) {
    if (!admin || (dist == wxFOCUS_IMMEDIATE))
      return FALSE;
    admin->GrabCaret(dist);
  }

  refresh = FALSE;

  visCaret = ownCaret || ShowsGhostCaret();

  if (!snip || !(snip->flags & wxSNIP_HANDLES_EVENTS)) {
    if (caretSnip) {
      caretSnip->OwnCaret(FALSE);
      if (visCaret)
	refresh = TRUE;
    }
    caretSnip = NULL;
#if ALLOW_X_STYLE_SELECTION
    wxMediaXSelectionAllowed = this;
#endif
    if (admin)
      admin->UpdateCursor();
    return refresh;
  }

  if (!GetSnipLocation(snip, NULL, NULL))
    return refresh;

  if (!ownCaret)
    hadCaret = FALSE;
  else
    hadCaret = !caretSnip;

  oldCaret = caretSnip;
  caretSnip = snip;

  BeginEditSequence();
  if (oldCaret)
    oldCaret->OwnCaret(FALSE);
  else if (visCaret)
    refresh = TRUE;
  snip->OwnCaret(ownCaret);
  EndEditSequence();

  if (admin && (dist != wxFOCUS_IMMEDIATE))
    admin->GrabCaret(dist);

  if (admin)
    admin->UpdateCursor();

  return refresh;
}

void wxMediaBuffer::GlobalToLocal(float *x, float *y)
{
  float lx, ly;

  if (admin) {
    admin->GetDC(&lx, &ly);
    if (x)
      *x += lx;
    if (y)
      *y += ly;
  }
}

void wxMediaBuffer::LocalToGlobal(float *x, float *y)
{
  float lx, ly;

  if (admin) {
    admin->GetDC(&lx, &ly);
    if (x)
      *x -= lx;
    if (y)
      *y -= ly;
  }
}

void wxMediaBuffer::SetCursor(wxCursor *c, Bool override)
{  
  customCursor = c;
  customCursorOverrides = override;

  if (admin)
    admin->UpdateCursor();
}

/******************************************************************/

#ifndef ROUND
#include <math.h>
#define ROUND(x) (int)floor(x)
#endif

#define REDICULOUS_SIZE 2000

Bool wxMediaBuffer::ReadyOffscreen(float width, float height)
{
  if ((width > REDICULOUS_SIZE)
      || (height > REDICULOUS_SIZE))
    return FALSE;

  if (!offscreenInUse && (height > bmHeight || width > bmWidth)) {
    wxBitmap *oldbm = bitmap;

    bmWidth = ROUND(width);
    bmHeight = ROUND(height);

    bitmap = new wxBitmap(bmWidth, bmHeight);

    offscreen->SelectObject(NULL);
    if (oldbm)
      delete oldbm;
  
    if (bitmap->Ok())
      offscreen->SelectObject(bitmap);

    return TRUE;
  } 
  
  return FALSE;
}

/******************************************************************/

void wxMediaBuffer::SetKeymap(wxKeymap *keymap)
{
  if (map) {
    map->AdjustUsage(FALSE);
#if !WXGARBAGE_COLLECTION_ON
    if (!map->IsUsed())
      delete map;
#endif
  }
  map = keymap;
  if (keymap)
    keymap->AdjustUsage(TRUE);
}

wxKeymap *wxMediaBuffer::GetKeymap(void)
{
  return map;
}

wxStyleList *wxMediaBuffer::GetStyleList(void)
{
  return styleList;
}

void wxMediaBuffer::SetStyleList(wxStyleList *newList)
{
  styleList->AdjustUsage(FALSE);
  styleList->ForgetNotification(notifyId);
#if !WXGARBAGE_COLLECTION_ON
  if (!styleList->IsUsed())
    delete styleList;
#endif
  newList->AdjustUsage(TRUE);
  notifyId = newList->NotifyOnChange((wxStyleNotifyFunc)MediaStyleNotify, 
				     this);
  styleList = newList;

  if (!styleList->FindNamedStyle(STD_STYLE))
    styleList->NewNamedStyle(STD_STYLE, NULL);
}

static void MediaStyleNotify(wxStyle *which, wxMediaBuffer *media)
{
  if (media)
    media->StyleHasChanged(which);
}

/******************************************************************/

#define NUM_IMAGE_TYPES 4

static long image_type[NUM_IMAGE_TYPES]
  = {wxBITMAP_TYPE_BMP,
     wxBITMAP_TYPE_GIF,
     wxBITMAP_TYPE_XBM,
     wxBITMAP_TYPE_XPM};
static char *image_type_name[NUM_IMAGE_TYPES]
  = {"Windows Bitmap", "GIF", "X Bitmap", "XPM"};
static char *image_extension[NUM_IMAGE_TYPES]
  = {"bmp", "gif", "xbm", "xpm"};

static int image_hit_ok;

static void wxmbSetCancel(wxObject& obj, wxEvent& WXUNUSED(evt))
{
  image_hit_ok = 0;
  ((wxButton *)&obj)->GetParent()->Show(FALSE);
}

static void wxmbSetOK(wxObject& obj, wxEvent& evt)
{
  wxmbSetCancel(obj, evt);
  image_hit_ok = 1;
}

#ifdef wx_mac
#define wxmbUNDO_SUFFIX "\tCmd+Z"
#define wxmbREDO_SUFFIX 
#define wxmbCLEAR_SUFFIX
#define wxmbCOPY_SUFFIX "\tCmd+C"
#define wxmbCUT_SUFFIX "\tCmd+X"
#define wxmbPATSE_SUFFIX "\tCmd+V"
#else
#define wxmbUNDO_SUFFIX 
#define wxmbREDO_SUFFIX 
#define wxmbCLEAR_SUFFIX
#define wxmbCOPY_SUFFIX
#define wxmbCUT_SUFFIX
#define wxmbPATSE_SUFFIX
#endif

int wxMediaBuffer::AppendEditItems(wxMenu *edit, int idOffset)
{
  edit->Append(idOffset + wxEDIT_UNDO, "Undo" wxmbUNDO_SUFFIX );
  edit->Append(idOffset + wxEDIT_REDO, "Redo" wxmbREDO_SUFFIX );
  edit->AppendSeparator();
  edit->Append(idOffset + wxEDIT_CLEAR, "Clear" wxmbCLEAR_SUFFIX);
  edit->Append(idOffset + wxEDIT_COPY, "Copy" wxmbCOPY_SUFFIX);
  edit->Append(idOffset + wxEDIT_CUT, "Cut" wxmbCUT_SUFFIX);
  edit->Append(idOffset + wxEDIT_PASTE, "Paste" wxmbPATSE_SUFFIX);
  edit->AppendSeparator();
  edit->Append(idOffset + wxEDIT_INSERT_TEXT_BOX, "Insert Text Box");
  edit->Append(idOffset + wxEDIT_INSERT_GRAPHIC_BOX, "Insert Graphic Box");
  edit->Append(idOffset + wxEDIT_INSERT_IMAGE, "Insert Image...");

  return idOffset + _wx_EDIT_counter + 1;
}

void wxMediaBuffer::DoEdit(int op, Bool recursive, long time)
{  
  if (recursive && caretSnip) {
    caretSnip->DoEdit(op, TRUE, time);
    return;
  }

  switch(op) {
  case wxEDIT_UNDO:
    Undo();
    break;
  case wxEDIT_REDO:
    Redo();
    break;
  case wxEDIT_CLEAR:
    Clear();
    break;
  case wxEDIT_CUT:
    Cut(FALSE, time);
    break;
  case wxEDIT_COPY:
    Copy(FALSE, time);
    break;
  case wxEDIT_PASTE:
    Paste(time);
    break;
  case wxEDIT_KILL:
    Kill(time);
    break;
  case wxEDIT_INSERT_TEXT_BOX:
    InsertBox(wxEDIT_BUFFER);
    break;
  case wxEDIT_INSERT_GRAPHIC_BOX:
    InsertBox(wxPASTEBOARD_BUFFER);
    break;
  case wxEDIT_INSERT_IMAGE:
    InsertImage();
    break;
  case wxEDIT_SELECT_ALL:
    SelectAll();
    break;
  }
}

#define NUM_FAMILIES 7
#define NUM_DELTA_SIZES 10
#define NUM_SIZES 8
#define NUM_STYLES 3
#define NUM_WEIGHTS 3
#define NUM_UNDERLINES 3
#define NUM_ALIGNS 3
#define NUM_COLOURS 11

int m_families[NUM_FAMILIES] = {wxDEFAULT,wxDECORATIVE,wxROMAN,
			      wxSCRIPT,wxSWISS,wxMODERN, -1};
int m_pl_deltasizes[NUM_DELTA_SIZES] = {1,2,4,8,16,32,0,0,0,0};
int m_ti_deltasizes[NUM_DELTA_SIZES] = {1,1,1,1,01,01,2,3,4,5};
int m_sizes[NUM_SIZES] = {9, 10, 12, 14, 16, 24, 32, 48};
int m_styles[NUM_STYLES] = {wxNORMAL, wxSLANT, wxITALIC};
int m_weights[NUM_WEIGHTS] = {wxNORMAL, wxLIGHT, wxBOLD};
int m_alignments[NUM_ALIGNS] = {wxALIGN_TOP, wxALIGN_CENTER, wxALIGN_BOTTOM};
const char *m_colour_names[NUM_COLOURS] = {"Black", "White",
				      "Red", "Orange", "Yellow",
				      "Green", "Blue", "Purple",
				      "Cyan", "Magenta", "Grey"};
const char *m_colours[NUM_COLOURS] = {"BLACK", "WHITE",
				      "RED", "ORANGE", "YELLOW",
				      "GREEN", "BLUE", "PURPLE",
				      "CYAN", "MAGENTA", "GREY"};
				      

int wxMediaBuffer::AppendFontItems(wxMenu *font, int idOffset)
{
  wxMenu *family, *size, *style, *weight, *underline;
  wxMenu *colour, *background, *alignment;
  wxMenu *bigger, *smaller;
  int i;
  
  family = new wxMenu(NULL, (wxFunction)NULL);
  bigger = new wxMenu(NULL, (wxFunction)NULL);
  smaller = new wxMenu(NULL, (wxFunction)NULL);
  size = new wxMenu(NULL, (wxFunction)NULL);
  style = new wxMenu(NULL, (wxFunction)NULL);
  weight = new wxMenu(NULL, (wxFunction)NULL);
  underline = new wxMenu(NULL, (wxFunction)NULL);
  colour = new wxMenu(NULL, (wxFunction)NULL);
  background = new wxMenu(NULL, (wxFunction)NULL);
  alignment = new wxMenu(NULL, (wxFunction)NULL);
  
  font->Append(0, "Face", family);
  font->Append(1, "Size", size);
  font->Append(2, "Style", style);
  font->Append(3, "Weight", weight);
  font->Append(4, "Underline", underline);
  font->Append(5, "Alignment", alignment);
  font->Append(6, "Color", colour);
  font->Append(7, "Background", background);

  family->Append(idOffset++, "Standard");
  family->Append(idOffset++, "Decorative");
  family->Append(idOffset++, "Roman");
  family->Append(idOffset++, "Script");
  family->Append(idOffset++, "Swiss");
  family->Append(idOffset++, "Fixed");
  family->AppendSeparator();
  family->Append(idOffset++, "Other...");

  bigger->Append(idOffset++, "+1");
  bigger->Append(idOffset++, "+2");
  bigger->Append(idOffset++, "+4");
  bigger->Append(idOffset++, "+8");
  bigger->Append(idOffset++, "+16");
  bigger->Append(idOffset++, "+32");
  bigger->AppendSeparator();
  bigger->Append(idOffset++, "x2");
  bigger->Append(idOffset++, "x3");
  bigger->Append(idOffset++, "x4");
  bigger->Append(idOffset++, "x5");
  
  smaller->Append(idOffset++, "-1");
  smaller->Append(idOffset++, "-2");
  smaller->Append(idOffset++, "-4");
  smaller->Append(idOffset++, "-8");
  smaller->Append(idOffset++, "-16");
  smaller->Append(idOffset++, "-32");
  smaller->AppendSeparator();
  smaller->Append(idOffset++, "1/2");
  smaller->Append(idOffset++, "1/3");
  smaller->Append(idOffset++, "1/4");
  smaller->Append(idOffset++, "1/5");

  size->Append(0, "Bigger", bigger);
  size->Append(1, "Smaller", smaller);
  size->Append(idOffset++, "9");
  size->Append(idOffset++, "10");
  size->Append(idOffset++, "12");
  size->Append(idOffset++, "14");
  size->Append(idOffset++, "16");
  size->Append(idOffset++, "24");
  size->Append(idOffset++, "32");
  size->Append(idOffset++, "48");

  style->Append(idOffset++, "Normal");
  style->Append(idOffset++, "Slant");
  style->Append(idOffset++, "Italic");

  weight->Append(idOffset++, "Normal");
  weight->Append(idOffset++, "Light");
  weight->Append(idOffset++, "Bold");

  underline->Append(idOffset++, "No Underline");
  underline->Append(idOffset++, "Underlined");
  underline->Append(idOffset++, "Toggle");

  alignment->Append(idOffset++, "Top");
  alignment->Append(idOffset++, "Center");
  alignment->Append(idOffset++, "Bottom");

  for (i = 0; i < NUM_COLOURS; i++)
    colour->Append(idOffset++, (char *)m_colour_names[i]);

  background->Append(idOffset++, "Transparent");
  background->AppendSeparator();
  for (i = 0; i < NUM_COLOURS; i++)
    background->Append(idOffset++, (char *)m_colour_names[i]);

  return idOffset;
}

void wxMediaBuffer::DoFont(int op, Bool recursive)
{
  if (recursive && caretSnip) {
    caretSnip->DoFont(op, TRUE);
    return;
  }

  if (op < NUM_FAMILIES) {
    int family = m_families[op];
    if (family < 0) {
      char *s = wxGetTextFromUser("Face name:", "Select Face");
      if (!s)
	return;
      wxStyleDelta delta;

      delta.SetDeltaFace(s);

      ChangeStyle(&delta);
    } else {
      wxStyleDelta delta(wxCHANGE_FAMILY, family);
      ChangeStyle(&delta);
    }
    return;
  }
  op -= NUM_FAMILIES;
  if (op < NUM_DELTA_SIZES) {
    wxStyleDelta delta(wxCHANGE_BIGGER, m_pl_deltasizes[op]);
    delta.sizeMult = m_ti_deltasizes[op];
    ChangeStyle(&delta);
    return;
  }
  op -= NUM_DELTA_SIZES;
  if (op < NUM_DELTA_SIZES) {
    wxStyleDelta delta(wxCHANGE_SMALLER, m_pl_deltasizes[op]);
    delta.sizeMult = 1.0/m_ti_deltasizes[op];
    ChangeStyle(&delta);
    return;
  }
  op -= NUM_DELTA_SIZES;
  if (op < NUM_SIZES) {
    wxStyleDelta delta(wxCHANGE_SIZE, m_sizes[op]);
    ChangeStyle(&delta);
    return;
  }
  op -= NUM_SIZES;
  if (op < NUM_STYLES) {
    wxStyleDelta delta(wxCHANGE_STYLE, m_styles[op]);
    ChangeStyle(&delta);
    return;
  }
  op -= NUM_STYLES;
  if (op < NUM_WEIGHTS) {
    wxStyleDelta delta(wxCHANGE_WEIGHT, m_weights[op]);
    ChangeStyle(&delta);
    return;
  }
  op -= NUM_WEIGHTS;
  if (op < NUM_UNDERLINES) {
    wxStyleDelta delta;

    if (op == 2)
      delta.SetDelta(wxCHANGE_TOGGLE_UNDERLINE);
    else
      delta.SetDelta(wxCHANGE_UNDERLINE, op);

    ChangeStyle(&delta);
    return;
  }
  op -= NUM_UNDERLINES;
  if (op < NUM_ALIGNS) {
    wxStyleDelta delta(wxCHANGE_ALIGNMENT, m_alignments[op]);
    ChangeStyle(&delta);
    return;
  }
  op -= NUM_ALIGNS;
  if (op < NUM_COLOURS) {
    wxStyleDelta delta;
    delta.SetDeltaForeground((char *)m_colours[op]);
    ChangeStyle(&delta);
    return;
  }
  op -= NUM_COLOURS;
  if (!op) {
    wxStyleDelta delta;
    delta.transparentTextBackingOn = TRUE;
    ChangeStyle(&delta);
    return;
  }
  op--;
  if (op < NUM_COLOURS) {
    wxStyleDelta delta;
    delta.SetDeltaBackground((char *)m_colours[op]);
    ChangeStyle(&delta);
    return;
  }
  op -= NUM_COLOURS;
}

void wxMediaBuffer::InsertBox(int type)
{
  wxSnip *snip;

  snip = OnNewBox(type);
  if (!snip)
    return;

  BeginEditSequence();
  snip->style = styleList->FindNamedStyle(STD_STYLE);
  Insert(snip);
  SetCaretOwner(snip);
  EndEditSequence();
}

wxSnip *wxMediaBuffer::OnNewBox(int type)
{
  wxMediaSnip *snip;
  wxMediaBuffer *media;

  if (type == wxEDIT_BUFFER)
    media = new wxMediaEdit();
  else
    media = new wxMediaPasteboard();
  snip = new wxMediaSnip(media);

  media->SetKeymap(map);  
  media->SetStyleList(styleList);

  return snip;
}

#ifdef wx_msw
#define WILDCARD "*.*"
#else
#define WILDCARD "*"
#endif

void wxMediaBuffer::InsertImage(char *filename, long type, Bool relative, Bool inlineImg)
{
  wxDialogBox *dial;
  wxRadioBox *boxes;
  wxCheckBox *inlineBox;
  int i, len;
  char extension[10];
    
  if (!filename)
    filename = GetFile(NULL);

  if (!filename)
    return;

  for (i = 0; i < NUM_IMAGE_TYPES; i++)
    if (type == image_type[i])
      break;

  if (i >=  NUM_IMAGE_TYPES) {
    dial = new wxDialogBox((wxFrame *)NULL, "Image Type", TRUE);
    
    len = strlen(filename);
    for (i = len; --i; )
      if (filename[i] == '.')
	break;
    
    if (i + 10 > len) {
      strcpy(extension, filename + i + 1);
      for (i = 0; extension[i]; i++)
	extension[i] = tolower(extension[i]);
    } else
      extension[0] = 0;
    
    dial->SetLabelPosition(wxVERTICAL);
    boxes = new wxRadioBox(dial, NULL,
			   "Image format:", -1, -1, -1, -1,
			   NUM_IMAGE_TYPES, image_type_name,
			   0, wxVERTICAL);
    
    for (i = 0; i < NUM_IMAGE_TYPES; i++)
      if (!strcmp(image_extension[i], extension))
	boxes->SetSelection(i);
    
    dial->NewLine();

    inlineBox = new wxCheckBox(dial, NULL, "Inline Image");
    if (inlineImg)
      inlineBox->SetValue(1);
    
    dial->NewLine();
    
    (void)(new wxButton(dial, (wxFunction)wxmbSetCancel, "Cancel"));
    (void)(new wxButton(dial, (wxFunction)wxmbSetOK, "OK"));
    
    dial->NewLine();
    
    dial->Fit();
    dial->Center(wxBOTH);
    dial->Show(TRUE);
    
    inlineImg = inlineBox->GetValue();

    type = image_type[boxes->GetSelection()];

    delete dial;

    if (!image_hit_ok)
      return;
  }

  wxImageSnip *snip;
      
  snip = OnNewImageSnip(filename, type, relative, inlineImg);
  Insert(snip);
}

wxImageSnip *wxMediaBuffer::OnNewImageSnip(char *filename, long type, 
					   Bool relative, Bool inlineImg)
{
  return new wxImageSnip(filename, type, relative, inlineImg);
}

/**********************************************************************/

wxBufferData *wxMediaBuffer::GetSnipData(wxSnip *)
{
  return NULL;
}

void wxMediaBuffer::SetSnipData(wxSnip *, wxBufferData *)
{
}

/**********************************************************************/

void *wxMediaFileIOReady = NULL;

Bool wxReadMediaGlobalHeader(wxMediaStreamIn &f)
{
  if (wxMediaFileIOReady) {
    wxMessageBox("File I/O already in progress for some stream.", "Error");
    return FALSE;
  }

  wxMediaFileIOReady = (void *)&f;

  wxTheSnipClassList.ResetHeaderFlags();
  if (!wxTheSnipClassList.Read(f))
    return FALSE;

  wxmbSetupStyleReadsWrites();

  return wxTheBufferDataClassList.Read(f);
}

Bool wxReadMediaGlobalFooter(wxMediaStreamIn &f)
{
  if (wxMediaFileIOReady != (void *)&f) {
    wxMessageBox("File reading not in progress for this stream.", "Error");
    return FALSE;
  }

  wxmbDoneStyleReadsWrites();
  wxTheSnipClassList.ResetHeaderFlags(wxRESET_DONE_READ);

  wxMediaFileIOReady = NULL;

  return TRUE;
}

Bool wxWriteMediaGlobalHeader(wxMediaStreamOut &f)
{
  if (wxMediaFileIOReady) {
    wxMessageBox("File I/O already in progress for some stream.", "Error");
    return FALSE;
  }

  wxMediaFileIOReady = (void *)&f;

  wxTheSnipClassList.ResetHeaderFlags();
  if (!wxTheSnipClassList.Write(f))
    return FALSE;

  wxmbSetupStyleReadsWrites();

  return wxTheBufferDataClassList.Write(f);
}

Bool wxWriteMediaGlobalFooter(wxMediaStreamOut &f)
{
  if (wxMediaFileIOReady != (void *)&f) {
    wxMessageBox("File writing not in progress for this stream.", "Error");
    return FALSE;
  }

  wxmbDoneStyleReadsWrites();
  wxTheSnipClassList.ResetHeaderFlags(wxRESET_DONE_WRITE);

  wxMediaFileIOReady = NULL;

  return TRUE;
}

/**********************************************************************/

char wxme_current_read_format[MRED_FORMAT_STR_LEN + 1];
char wxme_current_read_version[MRED_VERSION_STR_LEN + 1];

int wxmeCheckFormatAndVersion(void)
{
  if (strcmp(wxme_current_read_format, MRED_FORMAT_STR)) {
    wxMessageBox("Unknown format number.", "Error");
    return 0;
  }
  if (strcmp(wxme_current_read_version, MRED_VERSION_STR)
      && strcmp(wxme_current_read_version, "01")
      && strcmp(wxme_current_read_version, "02")) {
    wxMessageBox("Unknown version number.", "Error");
    return 0;
  }

  return 1;
}

Bool wxMediaBuffer::ReadHeaderFromFile(wxMediaStreamIn &, char *headerName)
{
  char buffer[256];

  sprintf(buffer, "Unknown header data: \"%.100s\"."
	  " The file will be loaded anyway.", headerName);

  wxMessageBox(buffer, "Warning");

  return TRUE;
}

Bool wxMediaBuffer::ReadFooterFromFile(wxMediaStreamIn &, char *headerName)
{
  char buffer[256];

  sprintf(buffer, "Unknown header data: \"%.100s\"."
	  " The file will be loaded anyway.", headerName);

  wxMessageBox(buffer, "Warning");

  return TRUE;
}

Bool wxMediaBuffer::WriteHeadersToFile(wxMediaStreamOut &WXUNUSED(f))
{
  return TRUE;
}

Bool wxMediaBuffer::WriteFootersToFile(wxMediaStreamOut &WXUNUSED(f))
{
  return TRUE;
}

Bool wxMediaBuffer::BeginWriteHeaderFooterToFile(wxMediaStreamOut &f, 
						 char *headerName,
						 long *dataBuffer)
{
  *dataBuffer = f.Tell();
  f.PutFixed(0);
  f << headerName;

  return TRUE;
}

Bool wxMediaBuffer::EndWriteHeaderFooterToFile(wxMediaStreamOut &f, 
					       long dataBuffer)
{
  long end, pos;

  end = f.Tell();

  f.JumpTo(dataBuffer);
  f.PutFixed(0);
  pos = f.Tell();
  
  f.JumpTo(dataBuffer);
  f.PutFixed(end - pos);

  f.JumpTo(end);

  numExtraHeaders++;
  
  return TRUE;
}

Bool wxMediaBuffer::ReadHeadersFooters(wxMediaStreamIn &f, Bool headers)
{
  char headerName[256];
  long len, hlen, i, pos, numHeaders;

  f.GetFixed(numHeaders);
  if (numHeaders > 1000) {
    if (wxMessageBox("File contains suspiciously large value for special"
		     " headers/footers. Should I give up?", "Warning",
		     wxYES_NO | wxCENTRE) == wxYES)
      return FALSE;
  }

  for (i = 0; i < numHeaders; i++) {
    f.GetFixed(len);
    if (!f.Ok())
      return FALSE;
    if (len) {
      pos = f.Tell();
      f.SetBoundary(len);
      hlen = 256;
      f.Get((long *)&hlen, (char *)headerName);
      if (headers) {
	if (!ReadHeaderFromFile(f, headerName))
	  return FALSE;
      } else {
	if (!ReadFooterFromFile(f, headerName))
	  return FALSE;
      }
      if (!f.Ok())
	return FALSE;
      f.RemoveBoundary();
      if (len -= (f.Tell() - pos))
	f.Skip(len);
      if (!f.Ok())
	return FALSE;
    }
  }

  return TRUE;
}

Bool wxMediaBuffer::DoWriteHeadersFooters(wxMediaStreamOut &f, Bool headers)
{
  long allStart, allEnd;

  allStart = f.Tell();
  f.PutFixed(0);
  numExtraHeaders = 0;

  if (headers) {
    if (!WriteHeadersToFile(f))
      return FALSE;
  } else {
    if (!WriteFootersToFile(f))
      return FALSE;
  }

  if (numExtraHeaders) {
    allEnd = f.Tell();
    f.JumpTo(allStart);
    f.PutFixed(numExtraHeaders);
    f.JumpTo(allEnd);
  }

  return TRUE;
}

/**********************************************************************/

static wxBufferData *ReadBufferData(wxMediaStreamIn &f)
{
  wxBufferData *data, *newdata;
  wxBufferDataClass *dclass;
  int extraDataIndex;
  long datalen;

  data = NULL;
  do {
    f >> extraDataIndex;
    if (extraDataIndex) {
      dclass = wxTheBufferDataClassList.FindByMapPosition(extraDataIndex);
      
      if (!dclass || !dclass->required)
	f >> datalen;
      else
	datalen = -1;
      
      if (dclass) {
	long start = f.Tell();
	if (datalen >= 0)
	  f.SetBoundary(datalen);
	if (!(newdata = dclass->Read(f)))
	  return FALSE;
	newdata->next = data;
	data = newdata;
	if (datalen >= 0) {
	  long rcount = f.Tell() - start;
	  if (rcount < datalen) {
	    wxMessageBox("Warning: underread caused by file "
			 "corruption or unknown internal error.", 
			 "Warning");
	    f.Skip(datalen - rcount);
	  }
	  f.RemoveBoundary();
	}
      } else {
	/* Unknown extra data */
	f.Skip(datalen);
      }
      if (!f.Ok())
	return FALSE;
    }
  } while (extraDataIndex);

  return data;
}

Bool wxMediaBuffer::ReadSnipsFromFile(wxMediaStreamIn &f, Bool overwritestylename)
{
  long len, numHeaders, numSnips, i;
  int styleIndex;
  short n;
  wxStyleList *newList;
  wxSnipClass *sclass;
  wxBufferData *data;
  wxSnip *snip;

  if (!ReadHeadersFooters(f, TRUE))
    return FALSE;

  if (!(newList = wxmbReadStylesFromFile(styleList, f, overwritestylename)))
    return FALSE;

  if (PTRNE(newList, styleList))
    SetStyleList(newList);
  
  f.GetFixed(numHeaders);
  
  if (numHeaders > 100) {
    if (wxMessageBox("File contains suspiciously large value for class"
		     " headers. Should I give up?", "Warning",
		     wxYES_NO | wxCENTRE) == wxYES)
      return FALSE;
  }

  for (i = 0; i < numHeaders; i++) {
    f >> n;
    f.GetFixed(len);
    if (!f.Ok())
      return FALSE;
    if (len) {
      sclass = wxTheSnipClassList.FindByMapPosition(n);
      if (sclass) {
	long start = f.Tell();

	f.SetBoundary(len);
	if (!sclass->ReadHeader(f))
	  return FALSE;
	if (!f.Ok())
	  return FALSE;
	sclass->headerFlag = 1;

	long rcount = f.Tell() - start;
	if (rcount < len) {
	  wxMessageBox("Warning: underread caused by file "
		       "corruption or unknown internal error.", 
		       "Warning");
	  f.Skip(len - rcount);
	}
	
	f.RemoveBoundary();
      } else {
	f.Skip(len);
      }
      if (!f.Ok())
	return FALSE;
    }
  }

  f >> numSnips;

  for (i = 0; i < numSnips; i++) {
    f >> n;
    if (n >= 0)
      sclass = wxTheSnipClassList.FindByMapPosition(n);
    else
      sclass = NULL; /* -1 => unknown */
    if (!sclass || !sclass->required)
      f.GetFixed(len);
    else
      len = -1;
    if (!f.Ok())
      return FALSE;
    if (len) {
      if (sclass) {
	long start = f.Tell();

	if (len >= 0)
	  f.SetBoundary(len);
	f >> styleIndex;
	if (styleIndex < 0 || styleIndex >= styleList->numMappedStyles) {
	  wxMessageBox("Bad style index for snip.", "Error");
	  return FALSE;
	}
	if ((snip = sclass->Read(f))) {
	  if (snip->flags & wxSNIP_OWNED)
	    snip->flags -= wxSNIP_OWNED;
	  snip->style = styleList->MapIndexToStyle(styleIndex);
	  if (!ReadInsert(snip))
	    return FALSE;
	} else
	  return FALSE;

	data = ReadBufferData(f);
	if (!f.Ok())
	  return FALSE;

	if (data)
	  SetSnipData(snip, data);

	if (len >= 0) {
	  long rcount = f.Tell() - start;
	  if (rcount < len) {
	    wxMessageBox("Warning: underread caused by file "
			 "corruption or unknown internal error.", 
			 "Warning");
	    f.Skip(len - rcount);
	  }
	  f.RemoveBoundary();
	}
      } else {
	f.Skip(len);
      }
      if (!f.Ok())
	return FALSE;
    }
  }

  if (!ReadHeadersFooters(f, FALSE))
    return FALSE;

  return TRUE;
}

Bool wxmbWriteBufferData(wxMediaStreamOut &f, wxBufferData *data)
{
  long dataPos = 0, dataStart = 0, dataEnd;
  
  while (data) {
    f << data->dataclass->mapPosition;
    
    if (!data->dataclass->required) {
      dataStart = f.Tell();
      f.PutFixed(0);
      dataPos = f.Tell();
    }

    if (!data->Write(f))
      return FALSE;
    
    if (!data->dataclass->required) {
      dataEnd = f.Tell();
      f.JumpTo(dataStart);
      f.PutFixed(dataEnd - dataPos);
      f.JumpTo(dataEnd);
    }
    data = data->next;
  }

  f << 0;

  return TRUE;
}

Bool wxmbWriteSnipsToFile(wxMediaStreamOut &f, 
			  wxStyleList *styleList,
			  wxList *snipList, 
			  wxSnip *startSnip, wxSnip *endSnip,
			  wxList *extraData,
			  wxMediaBuffer *buffer)
{
  long allStart, allEnd, headerPos, headerStart, headerEnd;
  long snipCount, snipPos = 0, snipStart = 0, snipEnd;
  long numHeaders;
  int styleIndex;
  wxNode *node = NULL, *node2;
  wxSnip *snip;
  wxSnipClass *sclass;
  wxBufferData *data;

  if (!wxmbWriteStylesToFile(styleList, f))
    return FALSE;
  
  allStart = f.Tell();
  f.PutFixed(0);

  if (snipList) {
    node = snipList->First();
    if (!node)
      return FALSE;
    startSnip = (wxSnip *)node->Data();
    endSnip = NULL;
  } else
    node = NULL;

  numHeaders = 0;
  snipCount = 0;
  for (snip = startSnip; PTRNE(snip, endSnip); snipCount++) {
    sclass = snip->snipclass;
    if (!sclass) {
      wxMessageBox("There's a snip without a class."
		   " Data will be lost.", "Warning");
    } else if (!sclass->headerFlag) {
      f << (short)sclass->mapPosition;
      headerStart = f.Tell();
      f.PutFixed(0);
      headerPos = f.Tell();
      if (!sclass->WriteHeader(f))
	return FALSE;
      sclass->headerFlag = 1;
      headerEnd = f.Tell();
      f.JumpTo(headerStart);
      f.PutFixed(headerEnd - headerPos);
      f.JumpTo(headerEnd);
      numHeaders++;
      if (!f.Ok())
	return FALSE;
    }

    if (snipList) {
      node = node->Next();
      if (node)
	snip = (wxSnip *)node->Data();
      else
	snip = NULL;
    } else
      snip = snip->next;
  }

  allEnd = f.Tell();
  f.JumpTo(allStart);
  f.PutFixed(numHeaders);
  f.JumpTo(allEnd);

  f << snipCount;

  if (snipList)
    node = snipList->First();
  else
    node = NULL;
  if (extraData)
    node2 = extraData->First();
  else
    node2 = NULL;

  for (snip = startSnip; PTRNE(snip, endSnip); ) {
    sclass = snip->snipclass;

    if (sclass)
      f << (short)sclass->mapPosition;
    else
      f << (short)(-1);

    if (!snip->snipclass || !snip->snipclass->required) {
      snipStart = f.Tell();
      f.PutFixed(0);
      snipPos = f.Tell();
    }

    styleIndex = styleList->StyleToIndex(snip->style);
    if (styleIndex < 0) {
      wxMessageBox("Bad style discovered.", "Warning");
      styleIndex = 0;
    }
    f << styleIndex;

    snip->Write(f);
    if (node2)
      data = (wxBufferData *)node2->Data();
    else
      data = buffer->GetSnipData(snip);

    if (!wxmbWriteBufferData(f, data))
      return FALSE;

    if (!snip->snipclass || !snip->snipclass->required) {
      snipEnd = f.Tell();
      f.JumpTo(snipStart);
      f.PutFixed(snipEnd - snipPos);
      f.JumpTo(snipEnd);
    }

    if (!f.Ok())
      return FALSE;

    if (snipList) {
      node = node->Next();
      if (node)
	snip = (wxSnip *)node->Data();
      else
	snip = NULL;
    } else
      snip = snip->next;
    if (extraData)
      node2 = node2->Next();
  }

  return TRUE;
}

char *wxMediaBuffer::GetFilename(Bool *temp)
{
  if (temp)
    *temp = tempFilename;
  return filename;
}

#ifndef wx_xt
class wxMediaPrintout : public wxPrintout
{
private:
  wxMediaBuffer *b;
  void *data;
  Bool fitToPage;

public:
  wxMediaPrintout(wxMediaBuffer *buffer, Bool fit) : wxPrintout()
    {
      b = buffer;
      fitToPage = fit;
    }

  Bool HasPage(int page)
    {
      return b->HasPrintPage(GetDC(), page);
    }

  Bool OnPrintPage(int page)
    {
      b->PrintToDC(GetDC(), page);

      return TRUE;
    }

  Bool OnBeginDocument(int startPage, int endPage)
    {
      b->printing = GetDC();
      data = b->BeginPrint(b->printing, fitToPage);
      return wxPrintout::OnBeginDocument(startPage, endPage);
    }

  void OnEndDocument()
    {
      wxDC *pr = b->printing;
      b->printing = NULL;
      b->EndPrint(pr, data);
      wxPrintout::OnEndDocument();
    }
};
#endif

#ifdef wx_x
# define WXUNUSED_X(x) /* empty */
#else
# define WXUNUSED_X(x) x
#endif

void wxMediaBuffer::Print(char *filename, Bool interactive, Bool fitToPage, 
			  int WXUNUSED_X(output_mode))
{
  int ps;

#ifndef wx_x
  if (output_mode == 2) {
    char *choices[2] = {"PostScript File", "Normal"};
    int choice = wxGetSingleChoiceIndex("Print as:", "Print Form", 2, choices);
    if (choice == -1)
      return;
    ps = !choice;
  } else
    ps = (output_mode == 1);
#else
  ps = 1;
#endif

  if (ps) {
    wxDC *dc;
    void *data;
    
    dc = new wxPostScriptDC(filename, interactive);

    if (dc->Ok()) { 
      dc->StartDoc("Printing buffer");

      printing = dc;
      data = BeginPrint(dc, fitToPage);

      PrintToDC(dc);

      printing = NULL;
      EndPrint(dc, data);

      dc->EndDoc();
    }

    delete dc;
    
    return;
  } 

#ifndef wx_x
  wxPrinter *p = new wxPrinter();
  wxPrintout *o = new wxMediaPrintout(this, fitToPage);
  
  p->Print(NULL, o, interactive);

  delete o;
  delete p;
#endif
}

/****************************************************************/

void wxMediaBuffer::Undo(void)
{
  undomode = TRUE;

  PerformUndos(changes, changes_start, changes_end);

  undomode = FALSE;
}

void wxMediaBuffer::Redo(void)
{ 
  redomode = TRUE;

  PerformUndos(redochanges, redochanges_start, redochanges_end);

  redomode = FALSE;
}

static void wxmeClearUndos(wxChangeRecord **changes, int& start, int& end,
			   int maxUndos)
{
  int i;

  for (i = start; i != end; i = (i + 1) % maxUndos) {
    delete changes[i];
    changes[i] = NULL;
  }

  start = end = 0;  
}

void wxMediaBuffer::AddUndo(wxChangeRecord *rec)
{
  if (interceptmode)
    intercepted->Append((wxObject *)rec);
  else if (undomode)
    AppendUndo(rec, redochanges, redochanges_start, redochanges_end);
  else if (!noundomode) {
    if (!redomode)
      wxmeClearUndos(redochanges, redochanges_start,
		     redochanges_end, maxUndos);
    AppendUndo(rec, changes, changes_start, changes_end);    
  } else
    delete rec;
}

void wxMediaBuffer::AppendUndo(wxChangeRecord *rec, wxChangeRecord **changes, 
			       int& start, int& end)
{
  if (maxUndos) {
    changes[end] = rec;
    end = (end + 1) % maxUndos;
    if (end == start) {
      delete changes[start];
      changes[start] = NULL;
      start = (start + 1) % maxUndos;
    }
  } else
    delete rec;
}

void wxMediaBuffer::PerformUndos(wxChangeRecord **changes, 
				 int& start, int& end)
{
  wxChangeRecord *rec;
  Bool cont;
  
  BeginEditSequence();
  while (start != end) {
    end = (end - 1 + maxUndos) % maxUndos;
    rec = changes[end];
    changes[end] = NULL;
    cont = rec->Undo(this);
    delete rec;
    if (!cont)
      break;
  }
  EndEditSequence();
}

void wxMediaBuffer::PerformUndoList(wxList *changes)
{
  wxNode *node;
  wxChangeRecord *rec;
  Bool cont = FALSE;
  
  BeginEditSequence();
  do {
    node = changes->Last();
    if (node) {
      rec = (wxChangeRecord *)node->Data();
      cont = rec->Undo(this);
      delete rec;
      changes->DeleteNode(node);
    }
  } while (node && cont);
  EndEditSequence();
}

void wxMediaBuffer::ClearUndos()
{
  wxmeClearUndos(changes, changes_start, changes_end, maxUndos);
  wxmeClearUndos(redochanges, redochanges_start, redochanges_end, maxUndos);
}

void wxMediaBuffer::SetMaxUndoHistory(int v)
{
  if (undomode || redomode || (v == maxUndos))
    return;

  wxChangeRecord **naya;
  int i, j;

  naya = new wxChangeRecordPtr[v];
  for (j = 0, i = changes_start; 
       (i != changes_end) && (j < v); 
       j++, i = (i + 1) % maxUndos)
    naya[j] = changes[i];
  changes_start = 0;
  changes_end = v ? (j % v) : 0;

  naya = new wxChangeRecordPtr[v];
  for (j = 0, i = redochanges_start; 
       (i != redochanges_end) && (j < v); 
       j++, i = (i + 1) % maxUndos)
    naya[j] = redochanges[i];
  redochanges_start = 0;
  redochanges_end = v ? (j % v) : v;

  maxUndos = v;
}

int wxMediaBuffer::GetMaxUndoHistory()
{
  return maxUndos;
}

/****************************************************************/

typedef struct {
  wxList *buffer1, *buffer2;
  wxStyleList *style;
  wxBufferData *data;
} CopyRingElem;

static int copyDepth = 0;

static int copyRingSize = 30;
static int copyRingPos = 0, copyRingMax = 0;
static CopyRingElem *copyRing = NULL;

wxList *wxmb_commonCopyBuffer = NULL;
wxList *wxmb_commonCopyBuffer2 = NULL;
wxStyleList *wxmb_copyStyleList = NULL;
wxBufferData *wxmb_commonCopyRegionData = NULL;

static void InitCutNPaste()
{
  if (!copyRing) {
    copyRing = new CopyRingElem[copyRingSize];

    wxmb_commonCopyBuffer = new wxList();
    wxmb_commonCopyBuffer2 = new wxList();
  }
}

void wxMediaBuffer::CopyRingNext(void)
{
  if (!copyRingMax)
    return;

  copyRingPos = copyRingPos - 1;
  if (copyRingPos < 1)
    copyRingPos = copyRingMax - 1;
  
  wxList *temp;
  wxStyleList *stemp;
  wxBufferData *dtemp;

  temp = wxmb_commonCopyBuffer;
  wxmb_commonCopyBuffer = copyRing[copyRingPos].buffer1;
  copyRing[copyRingPos].buffer1 = temp;

  temp = wxmb_commonCopyBuffer2;
  wxmb_commonCopyBuffer2 = copyRing[copyRingPos].buffer2;
  copyRing[copyRingPos].buffer2 = temp;
  
  dtemp = wxmb_commonCopyRegionData;
  wxmb_commonCopyRegionData = copyRing[copyRingPos].data;
  copyRing[copyRingPos].data = dtemp;

  stemp = wxmb_copyStyleList;
  wxmb_copyStyleList = copyRing[copyRingPos].style;
  copyRing[copyRingPos].style = stemp;
}

void wxMediaBuffer::BeginCopyBuffer(void)
{
  copyDepth++;
}

void wxMediaBuffer::EndCopyBuffer(void)
{
  --copyDepth;
}

void wxMediaBuffer::FreeOldCopies(void)
{
  if (!wxmb_copyStyleList)
    return;

  if (copyDepth > 1) {
    /* Delete current ring occupant: */
    wxmb_commonCopyBuffer->DeleteContents(TRUE);
    delete wxmb_commonCopyBuffer;
    wxmb_commonCopyBuffer2->DeleteContents(TRUE);
    delete wxmb_commonCopyBuffer2;

    if (wxmb_commonCopyRegionData)
      delete wxmb_commonCopyRegionData;
    if (wxmb_copyStyleList) {
      wxmb_copyStyleList->AdjustUsage(FALSE);
#ifndef WXGARBAGE_COLLECTION_ON
      if (!wxmb_copyStyleList->IsUsed())
	delete wxmb_copyStyleList;
#endif
    }

    wxmb_commonCopyBuffer = new wxList();
    wxmb_commonCopyBuffer2 = new wxList();

    wxmb_commonCopyRegionData = NULL;

    wxmb_copyStyleList = NULL;

    return;
  }

  if (copyRingMax > copyRingPos) {
    /* Delete current ring occupant: */
    copyRing[copyRingPos].buffer1->DeleteContents(TRUE);
    delete copyRing[copyRingPos].buffer1;
    copyRing[copyRingPos].buffer2->DeleteContents(TRUE);
    delete copyRing[copyRingPos].buffer2;

    if (copyRing[copyRingPos].data)
      delete copyRing[copyRingPos].data;
    if (copyRing[copyRingPos].style) {
      copyRing[copyRingPos].style->AdjustUsage(FALSE);
#ifndef WXGARBAGE_COLLECTION_ON
      if (!copyRing[copyRingPos].style->IsUsed())
	delete copyRing[copyRingPos].style;
#endif
    }
  }

  copyRing[copyRingPos].buffer1 = wxmb_commonCopyBuffer;
  copyRing[copyRingPos].buffer2 = wxmb_commonCopyBuffer2;
  
  wxmb_commonCopyBuffer = new wxList();
  wxmb_commonCopyBuffer2 = new wxList();

  copyRing[copyRingPos].data = wxmb_commonCopyRegionData;
  wxmb_commonCopyRegionData = NULL;

  copyRing[copyRingPos].style = wxmb_copyStyleList;
  wxmb_copyStyleList = NULL;

  copyRingPos++;
  if (copyRingMax < copyRingPos)
    copyRingMax = copyRingPos;
  if (copyRingPos > copyRingSize)
    copyRingPos = 0;
}

void wxMediaBuffer::InstallCopyBuffer(long time, wxStyleList *sl)
{
  wxmb_copyStyleList = sl;
  wxmb_copyStyleList->AdjustUsage(TRUE);

#if ALLOW_X_STYLE_SELECTION
  if (!xClipboardHack)
#endif
    wxTheClipboard->SetClipboardClient(&TheMediaClipboardClient, time);
}

void wxMediaBuffer::DoBufferPaste(long time, Bool local)
{
  wxClipboardClient *owner;
  wxNode *node, *node2;
  wxSnip *snip;
  
  owner = wxTheClipboard->GetClipboardClient();
  if (local || PTREQ(owner, &TheMediaClipboardClient)) {
    copyDepth++;
    for (node = wxmb_commonCopyBuffer->First(),
	 node2 = wxmb_commonCopyBuffer2->First(); 
	 node; 
	 node = node->Next(),
	 node2 = node2->Next())
      InsertPasteSnip(snip = ((wxSnip *)node->Data())->Copy(),
		      (wxBufferData *)node2->Data());
    --copyDepth;
    if (wxmb_commonCopyRegionData && bufferType == wxEDIT_BUFFER)
      ((wxMediaEdit *)this)->PasteRegionData(wxmb_commonCopyRegionData);
  } else {
    char *str;
    long len;

    if ((str = wxTheClipboard->GetClipboardData("WXME", &len, time))) {

      strcpy(wxme_current_read_format, MRED_FORMAT_STR);
      strcpy(wxme_current_read_version, MRED_VERSION_STR);

      wxMediaStreamInStringBase b(str, len);
      wxMediaStreamIn mf(b);

      if (wxReadMediaGlobalHeader(mf))
	if (mf.Ok())
	  if (ReadFromFile(mf)) {
	    wxBufferData *data;
	    data = ReadBufferData(mf);
	    if (data && bufferType == wxEDIT_BUFFER)
	      ((wxMediaEdit *)this)->PasteRegionData(data);
	  }
      wxReadMediaGlobalFooter(mf);
      delete[] str;
    } else {
      str = wxTheClipboard->GetClipboardString(time);
      InsertPasteString(str);
      delete[] str;
    }
  }
}

wxMediaClipboardClient::wxMediaClipboardClient()
{
  formats.Add("TEXT");
  formats.Add("WXME");
}

char *wxMediaClipboardClient::GetData(char *format, long *size)
{
  wxNode *node;
  wxSnip *snip;
  long l, length = 0;
  char *str, *total = NULL, *old;

  if (!strcmp(format, "TEXT")) {
    for (node = wxmb_commonCopyBuffer->First(); node; node = node->Next()) {
      snip = (wxSnip *)node->Data();
      str = snip->GetText(0, snip->count, TRUE);
      l = strlen(str);
      if (total) {
	old = total;
	total = new char[length + l + 1];
	memcpy(total, old, length);
	memcpy(total + length, str, l);
	delete[] old;
      } else
	total = str;
      length += l;
    }
    
    if (!total)
      total = new char[1];
    
    total[length] = 0;
    
    *size = length;

    return total;
  } else if (!strcmp(format, "WXME")) {
    wxMediaStreamOutStringBase b;
    wxMediaStreamOut mf(b);

    wxWriteMediaGlobalHeader(mf);
    if (mf.Ok()) {
      mf.PutFixed(0);
      if (!wxmbWriteSnipsToFile(mf, wxmb_copyStyleList, wxmb_commonCopyBuffer, 
				NULL, NULL, wxmb_commonCopyBuffer2, NULL))
	return FALSE;
      mf.PutFixed(0);
      wxmbWriteBufferData(mf, wxmb_commonCopyRegionData);
    }
    wxWriteMediaGlobalFooter(mf);

    return b.GetString(size);
  } else {
    *size = 0;
    return "";
  }
}  

void wxMediaClipboardClient::BeingReplaced(void)
{
}

#if ALLOW_X_STYLE_SELECTION

wxMediaXClipboardClient::wxMediaXClipboardClient()
{
  formats.Add("TEXT");
  formats.Add("WXME");
}

char *wxMediaXClipboardClient::GetData(char *format, long *size)
{
  if (!xSelectionCopied && !wxMediaXSelectionOwner) {
    *size = 0;
    return "";
  }

  if (!xSelectionCopied || wxMediaXSelectionOwner) {
    xClipboardHack = TRUE;
    wxMediaXSelectionOwner->Copy(FALSE, 0L);
    xClipboardHack = FALSE;
  }

  /* If nothing is copied (e.g., DoCopy is overriden to not copy anything
     or copies directly to clipboard): */
  if (!wxmb_copyStyleList) {
    if (wxTheClipboard->GetClipboardClient() == this)
      return NULL;
    else
      return wxTheClipboard->GetClipboardData(format, size, 0);
  }

  return TheMediaClipboardClient.GetData(format, size);
}

void wxMediaXClipboardClient::BeingReplaced(void)
{
  if (wxMediaXSelectionOwner) {
    wxMediaBuffer *b = wxMediaXSelectionOwner;
    wxMediaXSelectionOwner= NULL;
    xSelectionCopied = FALSE;
    b->OwnXSelection(FALSE, TRUE, FALSE);
  } else
    xSelectionCopied = FALSE;
}

#endif

/****************************************************************/

#if ALLOW_X_STYLE_SELECTION

Bool wxMediaBuffer::DoOwnXSelection(Bool on, Bool force)
{
  if (on) {
    if (!force && wxMediaXSelectionAllowed != this)
      return FALSE;
    if (wxMediaXSelectionOwner) {
      wxMediaXSelectionOwner->OwnXSelection(FALSE, TRUE, FALSE);
      wxMediaXSelectionOwner = NULL; // should be redundant
    }
    xSelectionCopied = FALSE;
    wxTheClipboard->SetClipboardClient(&TheMediaXClipboardClient, 0L);
    wxMediaXSelectionOwner = this;
  } else if (this == wxMediaXSelectionOwner) {
    wxMediaXSelectionOwner = NULL;
    if (!xSelectionCopied
	&& PTREQ(wxTheClipboard->GetClipboardClient(), 
		 &TheMediaXClipboardClient)) {
      wxTheClipboard->SetClipboardString("", 0L);
    }
  }

  return TRUE;
}

void wxMediaBuffer::CopyOutXSelection(void)
{
  if (this == wxMediaXSelectionOwner) {
    xClipboardHack = TRUE;
    wxMediaXSelectionOwner->Copy(FALSE, 0L);
    xClipboardHack = FALSE;
    xSelectionCopied = TRUE;
  }
}

#endif

void wxMediaSetXSelectionMode(Bool on)
{
#if ALLOW_X_STYLE_SELECTION
  wxMediaXSelectionMode = on;
  if (!on && PTREQ(wxTheClipboard->GetClipboardClient(), 
		   &TheMediaXClipboardClient))
    wxTheClipboard->SetClipboardString("", 0L);
#endif
}

/****************************************************************/

void wxMediaBuffer::Lock(Bool lock)
{
  userLocked = lock;
}

Bool wxMediaBuffer::Modified(void)
{
  return modified;
}

void wxMediaBuffer::SetModified(Bool mod)
{
  modified = mod;
}

int wxMediaBuffer::GetInactiveCaretThreshold(void)
{
  return inactiveCaretThreshold;
}

void wxMediaBuffer::SetInactiveCaretThreshold(int v)
{
  inactiveCaretThreshold = v;
}


void wxMediaBuffer::OnPaint(Bool WXUNUSED(pre),
			    wxDC *WXUNUSED(dc), 
			    float WXUNUSED(l), float WXUNUSED(t), 
			    float WXUNUSED(r), float WXUNUSED(b), 
			    float WXUNUSED(dx), float WXUNUSED(dy),
			    int WXUNUSED(show_caret))
{
  /* Do nothing */
}

Bool wxMediaBuffer::OnSaveFile(char *WXUNUSED(filename), int WXUNUSED(format))
{
  return TRUE;
}

void wxMediaBuffer::AfterSaveFile(Bool WXUNUSED(success))
{
  /* do nothing */
}

Bool wxMediaBuffer::OnLoadFile(char *WXUNUSED(filename), int WXUNUSED(format))
{
  return TRUE;
}

void wxMediaBuffer::AfterLoadFile(Bool WXUNUSED(success))
{
  /* do nothing */
}

void wxMediaBuffer::OnEditSequence(void)
{
  /* Do nothing */
}

void wxMediaBuffer::AfterEditSequence(void)
{
  /* Do nothing */
}

char *wxMediaBuffer::GetFile(char *path)
{
  return wxFileSelector("Choose a file", path, NULL,
			NULL, WILDCARD, wxOPEN);
}
 
char *wxMediaBuffer::PutFile(char *path, char *suggested_name)
{
  return wxFileSelector("Save file as", path,
			suggested_name, NULL, WILDCARD, wxSAVE);
}

void wxMediaBuffer::SetLoadOverwritesStyles(Bool b)
{
  loadoverwritesstyles = b;
}

Bool wxMediaBuffer::GetLoadOverwritesStyles()
{
  return loadoverwritesstyles;
}


/****************************************************************/

#if USE_OLD_TYPE_SYSTEM
#define __CHECK_TYPE(b) b->__type != wxTYPE_MEDIA_EDIT && b->__type != wxTYPE_MEDIA_PASTEBOARD
#else
#define __CHECK_TYPE(b) TRUE
#endif

#define edf(name, action, kname) \
     static Bool ed_##name(wxMediaBuffer *b, wxKeyEvent kname, void *) \
     { if (__CHECK_TYPE(b)) \
        return FALSE; \
       b->action; \
       return TRUE; } \

edf(copy, Copy(FALSE, event.timeStamp), &event)
edf(copyappend, Copy(TRUE, event.timeStamp), &event)
edf(paste, Paste(event.timeStamp), &event)
edf(cut, Cut(FALSE, event.timeStamp), &event)
edf(kill, Kill(event.timeStamp), &event)
edf(cutappend, Cut(TRUE, event.timeStamp), &event)
edf(undo, Undo(), &)
edf(redo, Redo(), &)
edf(delete, Clear(), &)
edf(select_all, SelectAll(), &)

void wxMediaBuffer::AddBufferFunctions(wxKeymap *tab)
{
  wxAddMediaBufferFunctions(tab);
}

void wxAddMediaBufferFunctions(wxKeymap *tab)
{
#define setf(name, func) \
  tab->AddKeyFunction(name, (wxKeyFunction)ed_##func, NULL)

  setf("copy-clipboard", copy);
  setf("copy-append-clipboard", copyappend);
  setf("cut-clipboard", cut);
  setf("cut-append-clipboard", cutappend);
  setf("paste-clipboard", paste);
  setf("delete-selection", delete);
  setf("clear-selection", delete);
  setf("delete-to-end-of-line", kill);

  setf("undo", undo);
  setf("redo", redo);

  setf("select-all", select_all);
}

/****************************************************************/

#ifdef MEMORY_USE_METHOD
long wxMediaBuffer::MemoryUse(void)
{
  return ((filename ? strlen(filename) + 1 : 0)
	  + (maxUndos * 2 * sizeof(wxChangeRecord*))
	  + wxObject::MemoryUse());
}
#endif

/****************************************************************/

wxStandardSnipAdmin::wxStandardSnipAdmin(wxMediaBuffer *m)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_MEDIA_SNIP_ADMIN;
#endif

  media = m;
  WXGC_IGNORE(media);
}

wxMediaBuffer *wxStandardSnipAdmin::GetMedia(void)
{
  return media;
}

wxDC *wxStandardSnipAdmin::GetDC()
{
  return media->GetDC();
}

void wxStandardSnipAdmin::GetViewSize(float *w, float *h)
{
  GetView(NULL, NULL, w, h, NULL);
}

void wxStandardSnipAdmin::GetView(float *x, float *y, float *w, float *h, wxSnip *snip)
{
  wxMediaAdmin *admin = media->GetAdmin();
    
  if (snip) {
    if (admin) {
      float mx, my, mh, mw, mr, mb, sl, st, sr, sb;

      admin->GetView(&mx, &my, &mw, &mh, FALSE);

      mb = my + mh;
      mr = mx + mw;
      if (media->GetSnipLocation(snip, &sl, &st, FALSE)) {
	media->GetSnipLocation(snip, &sr, &sb, TRUE);
	
	float l, t, r, b;
	l = (mx > sl ? mx : sl);
	t = (my > st ? my : st);
	r = (mr > sr ? sr : mr);
	b = (mb > sb ? sb : mb);
	
	if (x)
	  *x = l - sl;
	if (y)
	  *y = t - st;
	if (w)
	  *w = (r - l);
	if (h)
	  *h = (b - t);
	
	return;
      }
    }
  } else {
    if (admin) {
      admin->GetView(x, y, w, h, TRUE);
      return;
    }
  }

  if (x) *x = 0;
  if (y) *y = 0;
  if (w) *w = 0;
  if (h) *h = 0;
}

Bool wxStandardSnipAdmin::ScrollTo(wxSnip *s, float localx, float localy, 
				   float w, float h, Bool refresh, int bias)
{
  return media->ScrollTo(s, localx, localy, w, h, refresh, bias);
}

void wxStandardSnipAdmin::SetCaretOwner(wxSnip *s, int dist)
{
  media->SetCaretOwner(s, dist);
}
 
void wxStandardSnipAdmin::Resized(wxSnip *s, Bool redraw_now)
{
  media->Resized(s, redraw_now);
}

Bool wxStandardSnipAdmin::Recounted(wxSnip *s, Bool redraw_now)
{
  return media->Recounted(s, redraw_now);
}

void wxStandardSnipAdmin::NeedsUpdate(wxSnip *s, float localx, float localy, 
				      float w, float h)
{
  media->NeedsUpdate(s, localx, localy, w, h);
}

Bool wxStandardSnipAdmin::ReleaseSnip(wxSnip *snip)
{
  return media->ReleaseSnip(snip);
}

void wxStandardSnipAdmin::UpdateCursor()
{
  if (media->admin)
    media->admin->UpdateCursor();
}

#ifdef wx_mac

#include <Files.h>

long wxMediaCreatorId = 'WXME';

void wxMediaSetFileCreatorType(char *file, Bool is_binary)
{
  FSSpec spec;
  FInfo info;
  Str255 filename;

  memcpy(filename, file, 255);
  filename[255] = 0;
  CtoPstr((char *)filename);

  FSMakeFSSpec(0, 0, filename, &spec);

  FSpGetFInfo(&spec, &info);
  info.fdCreator = wxMediaCreatorId;
  info.fdType = is_binary ? 'WXME' : 'TEXT';
  FSpSetFInfo(&spec, &info);
}

#endif
