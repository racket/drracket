/*
 * File:      wb_gdi.cc
 * Purpose:     GDI (Graphics Device Interface) objects and functions
 * Author:      Julian Smart
 * Created:     1993
 * Updated:     August 1994
 * RCS_ID:      $Id: wb_gdi.cc,v 1.4 1994/08/14 22:59:35 edz Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "%W% %G%"; */

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_list.h"
#include "wx_utils.h"
#include "wx_main.h"
#include "wx_gdi.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#ifdef wx_x
extern Colormap wxMainColormap;
#endif
#ifdef wx_xview
extern Xv_Server xview_server;
#endif
#if USE_IMAGE_LOADING_IN_MAC
#include "wx_image.h"
#endif


wxbFont::wxbFont (void)
{
  __type = wxTYPE_FONT;
}

/* Constructor for a font. Note that the real construction is done
 * in wxDC::SetFont, when information is available about scaling etc.
 */
wxbFont::wxbFont (int PointSize, int Family, int Style, int Weight, Bool Underline)
{
  __type = wxTYPE_FONT;
}

wxbFont::~wxbFont ()
{
}

char *wxbFont::GetFamilyString(void)
{
  char *fam = NULL;
  switch (GetFamily())
  {
    case wxDECORATIVE:
      fam = "wxDECORATIVE";
      break;
    case wxROMAN:
      fam = "wxROMAN";
      break;
    case wxSCRIPT:
      fam = "wxSCRIPT";
      break;
    case wxSWISS:
      fam = "wxSWISS";
      break;
    case wxMODERN:
      fam = "wxMODERN";
      break;
    case wxTELETYPE:
      fam = "wxTELETYPE";
      break;
    case wxSYSTEM:
      fam = "wxSYSTEM";
      break;
    default:
      fam = "wxDEFAULT";
      break;
  }
  return fam;
}

char *wxbFont::GetFaceString(void)
{
  return wxTheFontNameDirectory.GetFontName(fontid); 
}

char *wxbFont::GetStyleString(void)
{
  char *styl = NULL;
  switch (GetStyle())
  {
    case wxITALIC:
      styl = "wxITALIC";
      break;
    case wxSLANT:
      styl = "wxSLANT";
      break;
    default:
      styl = "wxNORMAL";
      break;
  }
  return styl;
}

char *wxbFont::GetWeightString(void)
{
  char *w = NULL;
  switch (GetWeight())
  {
    case wxBOLD:
      w = "wxBOLD";
      break;
    case wxLIGHT:
      w = "wxLIGHT";
      break;
    default:
      w = "wxNORMAL";
      break;
  }
  return w;
}

// Colour

wxColour::wxColour (void)
{
  __type = wxTYPE_COLOUR;
  isInit = FALSE;
#ifdef wx_x
  pixel = -1;
#endif
#ifdef wx_msw
  pixel = 0;
#endif
#ifdef wx_mac
  red = 0; green = 0; blue = 0;
  pixel.red = 0;
  pixel.green = 0;
  pixel.blue = 0;
#endif
  locked = 0;
//  wxTheColourList->Append (this);
}

wxColour::wxColour (unsigned char r, unsigned char g, unsigned char b)
{
  __type = wxTYPE_COLOUR;
  red = r;
  green = g;
  blue = b;
  isInit = TRUE;
#ifdef wx_x
  pixel = -1;
#endif
#ifdef wx_msw
  pixel = RGB (red, green, blue);
#endif
#ifdef wx_mac
  pixel.red = red << 8;
  pixel.green = green << 8;
  pixel.blue = blue << 8;
#endif
  locked = 0;
//  wxTheColourList->Append (this);
}

wxColour::wxColour (const char *col)
{
  __type = wxTYPE_COLOUR;
  wxColour *the_colour = wxTheColourDatabase->FindColour (col);
  if (the_colour)
    {
      red = the_colour->Red ();
      green = the_colour->Green ();
      blue = the_colour->Blue ();
      isInit = TRUE;
    }
  else
    {
      red = 0;
      green = 0;
      blue = 0;
      isInit = FALSE;
    }
#ifdef wx_x
  pixel = -1;
#endif
#ifdef wx_msw
  pixel = RGB (red, green, blue);
#endif
#ifdef wx_mac
  pixel.red = red << 8;
  pixel.green = green << 8;
  pixel.blue = blue << 8;
#endif
//  wxTheColourList->Append (this);
  locked = 0;
}

wxColour::~wxColour (void)
{
//  wxTheColourList->DeleteObject (this);
}

wxColour& wxColour::operator =(wxColour& src)
{
  red = src.red;
  green = src.green;
  blue = src.blue;
  pixel = src.pixel;
  isInit = src.isInit;
#ifdef wx_mac
  pixel.red = red << 8;
  pixel.green = green << 8;
  pixel.blue = blue << 8;
#endif
  return (*this);
}

wxColour& wxColour::operator = (const char *col)
{
  wxColour *the_colour = wxTheColourDatabase->FindColour (col);
  if (the_colour)
    {
      red = the_colour->Red ();
      green = the_colour->Green ();
      blue = the_colour->Blue ();
      isInit = TRUE;
    }
  else
    {
      red = 0;
      green = 0;
      blue = 0;
      isInit = FALSE;
    }
#ifdef wx_x
  pixel = -1;
#endif
#ifdef wx_msw
  pixel = RGB (red, green, blue);
#endif
#ifdef wx_mac
  pixel.red = red << 8;
  pixel.green = green << 8;
  pixel.blue = blue << 8;
#endif
  return (*this);
}

void wxColour::Set (unsigned char r, unsigned char g, unsigned char b)
{
  red = r;
  green = g;
  blue = b;
  isInit = TRUE;
#ifdef wx_x
  pixel = -1;
#endif
#ifdef wx_msw
  pixel = RGB (red, green, blue);
#endif
#ifdef wx_mac
  pixel.red = red << 8;
  pixel.green = green << 8;
  pixel.blue = blue << 8;
#endif
}

void wxColour::Get (unsigned char *r, unsigned char *g, unsigned char *b)
{
  *r = red;
  *g = green;
  *b = blue;
}

#ifdef wx_mac
wxColourDatabase::wxColourDatabase(KeyType type):wxList(type)
#else
wxColourDatabase::wxColourDatabase (int type):
wxList (type)
#endif
{
}

wxColourDatabase::~wxColourDatabase (void)
{
  // Cleanup Colour allocated in Initialize()
  wxNode *node = First ();
  while (node)
    {
      wxColour *col = (wxColour *) node->Data ();
      wxNode *next = node->Next ();
      delete col;
      node = next;
    }
}

// Colour database stuff
void wxColourDatabase::Initialize (void)
{
  // Don't initialize for X: colours are found
  // in FindColour below.
#if defined(wx_msw) || defined(wx_mac)
#define APPEND(name, c) tmpc = c; tmpc->Lock(1); Append(name, tmpc)
  wxColour *tmpc;
  APPEND ("AQUAMARINE", new wxColour (112, 219, 147));
  APPEND ("BLACK", new wxColour (0, 0, 0));
  APPEND ("BLUE", new wxColour (0, 0, 255));
  APPEND ("BLUE VIOLET", new wxColour (159, 95, 159));
  APPEND ("BROWN", new wxColour (165, 42, 42));
  APPEND ("CADET BLUE", new wxColour (95, 159, 159));
  APPEND ("CORAL", new wxColour (255, 127, 0));
  APPEND ("CORNFLOWER BLUE", new wxColour (66, 66, 111));
  APPEND ("CYAN", new wxColour (0, 255, 255));
  APPEND ("DARK GREY", new wxColour (47, 47, 47));	// ?

  APPEND ("DARK GREEN", new wxColour (47, 79, 47));
  APPEND ("DARK OLIVE GREEN", new wxColour (79, 79, 47));
  APPEND ("DARK ORCHID", new wxColour (153, 50, 204));
  APPEND ("DARK SLATE BLUE", new wxColour (107, 35, 142));
  APPEND ("DARK SLATE GREY", new wxColour (47, 79, 79));
  APPEND ("DARK TURQUOISE", new wxColour (112, 147, 219));
  APPEND ("DIM GREY", new wxColour (84, 84, 84));
  APPEND ("FIREBRICK", new wxColour (142, 35, 35));
  APPEND ("FOREST GREEN", new wxColour (35, 142, 35));
  APPEND ("GOLD", new wxColour (204, 127, 50));
  APPEND ("GOLDENROD", new wxColour (219, 219, 112));
#ifdef wx_msw
  APPEND ("GREY", new wxColour (128, 128, 128));
#else
  APPEND ("GREY", new wxColour (192, 192, 192));
#endif
  APPEND ("GREEN", new wxColour (0, 255, 0));
  APPEND ("GREEN YELLOW", new wxColour (147, 219, 112));
  APPEND ("INDIAN RED", new wxColour (79, 47, 47));
  APPEND ("KHAKI", new wxColour (159, 159, 95));
  APPEND ("LIGHT BLUE", new wxColour (191, 216, 216));
#ifdef wx_msw
  APPEND ("LIGHT GREY", new wxColour (192, 192, 192));
#else
  APPEND ("LIGHT GREY", new wxColour (168, 168, 168));
#endif
  APPEND ("LIGHT STEEL BLUE", new wxColour (143, 143, 188));
  APPEND ("LIME GREEN", new wxColour (50, 204, 50));
  APPEND ("LIGHT MAGENTA", new wxColour (255, 0, 255));
  APPEND ("MAGENTA", new wxColour (255, 0, 255));
  APPEND ("MAROON", new wxColour (142, 35, 107));
  APPEND ("MEDIUM AQUAMARINE", new wxColour (50, 204, 153));
  APPEND ("MEDIUM GREY", new wxColour (100, 100, 100));
  APPEND ("MEDIUM BLUE", new wxColour (50, 50, 204));
  APPEND ("MEDIUM FOREST GREEN", new wxColour (107, 142, 35));
  APPEND ("MEDIUM GOLDENROD", new wxColour (234, 234, 173));
  APPEND ("MEDIUM ORCHID", new wxColour (147, 112, 219));
  APPEND ("MEDIUM SEA GREEN", new wxColour (66, 111, 66));
  APPEND ("MEDIUM SLATE BLUE", new wxColour (127, 0, 255));
  APPEND ("MEDIUM SPRING GREEN", new wxColour (127, 255, 0));
  APPEND ("MEDIUM TURQUOISE", new wxColour (112, 219, 219));
  APPEND ("MEDIUM VIOLET RED", new wxColour (219, 112, 147));
  APPEND ("MIDNIGHT BLUE", new wxColour (47, 47, 79));
  APPEND ("NAVY", new wxColour (35, 35, 142));
  APPEND ("ORANGE", new wxColour (204, 50, 50));
  APPEND ("ORANGE RED", new wxColour (255, 0, 127));
  APPEND ("ORCHID", new wxColour (219, 112, 219));
  APPEND ("PALE GREEN", new wxColour (143, 188, 143));
  APPEND ("PINK", new wxColour (188, 143, 234));
  APPEND ("PLUM", new wxColour (234, 173, 234));
  APPEND ("PURPLE", new wxColour (176, 0, 255));
  APPEND ("RED", new wxColour (255, 0, 0));
  APPEND ("SALMON", new wxColour (111, 66, 66));
  APPEND ("SEA GREEN", new wxColour (35, 142, 107));
  APPEND ("SIENNA", new wxColour (142, 107, 35));
  APPEND ("SKY BLUE", new wxColour (50, 153, 204));
  APPEND ("SLATE BLUE", new wxColour (0, 127, 255));
  APPEND ("SPRING GREEN", new wxColour (0, 255, 127));
  APPEND ("STEEL BLUE", new wxColour (35, 107, 142));
  APPEND ("TAN", new wxColour (219, 147, 112));
  APPEND ("THISTLE", new wxColour (216, 191, 216));
  APPEND ("TURQUOISE", new wxColour (173, 234, 234));
  APPEND ("VIOLET", new wxColour (79, 47, 79));
  APPEND ("VIOLET RED", new wxColour (204, 50, 153));
  APPEND ("WHEAT", new wxColour (216, 216, 191));
  APPEND ("WHITE", new wxColour (255, 255, 255));
  APPEND ("YELLOW", new wxColour (255, 255, 0));
  APPEND ("YELLOW GREEN", new wxColour (153, 204, 50));
#endif
}

/*
 * Changed by Ian Brown, July 1994.
 *
 * When running under X, the Colour Database starts off empty. The X server
 * is queried for the colour first time after which it is entered into the
 * database. This allows our client to use the server colour database which
 * is hopefully gamma corrected for the display being used.
 */

wxColour *wxColourDatabase::FindColour(const char *colour)
{
  const char *p;
  
  // Insure upcased:
  for (p = colour; *p && !islower(*p); p++);
  if (*p) {
  	char *naya = new char[strlen(colour) + 1], *q;
  	for (p = colour, q = naya; *p; p++, q++)
  		*q = toupper(*p);
  	*q = 0;
  	colour = naya;
  }

  wxNode *node = Find(colour);
  if (node)
    return (wxColour *)node->Data();
#if defined(wx_msw) || defined(wx_mac)
  else return NULL;
#else
  else {
    XColor xcolour;

#ifdef wx_motif
    Display *display = XtDisplay(wxTheApp->topLevel) ;
#endif
#ifdef wx_xview
    Xv_Screen screen = xv_get(xview_server, SERVER_NTH_SCREEN, 0);
    Xv_opaque root_window = xv_get(screen, XV_ROOT);
    Display *display = (Display *)xv_get(root_window, XV_DISPLAY);
#endif

    if (!XParseColor(display, wxMainColormap,colour,&xcolour))
      return NULL;

    unsigned char r = (unsigned char)(xcolour.red >> 8);
    unsigned char g = (unsigned char)(xcolour.green >> 8);
    unsigned char b = (unsigned char)(xcolour.blue >> 8);

    wxColour *col = new wxColour(r, g, b);
    Append(colour, col);

    return col;
  }
#endif
}

/* Old FindColour
wxColour *wxColourDatabase::FindColour (const char *colour)
{
  wxNode *node = Find (colour);
  return node ? (wxColour *) node->Data () : NULL ;
}
*/

char *wxColourDatabase::FindName (wxColour& colour)
{
  unsigned char red = colour.Red ();
  unsigned char green = colour.Green ();
  unsigned char blue = colour.Blue ();

  for (wxNode * node = First (); node; node = node->Next ())
    {
      wxColour *col = (wxColour *) node->Data ();
      if (col->Red () == red && col->Green () == green && col->Blue () == blue)
	{
	  char *found = node->key.string;
	  if (found)
	    return found;
	}
    }
  return NULL;			// Not Found

}


void 
wxInitializeStockObjects (void)
{
  wxTheBrushList = new wxBrushList;
  wxThePenList = new wxPenList;
  wxTheFontList = new wxFontList;
  wxTheBitmapList = new wxGDIList;
#ifdef wx_mac
  wxTheCursorList = new wxGDIList;
  wxTheIconList =  new wxGDIList;
#endif
  // wxTheColourList =  new wxGDIList;

#ifdef wx_motif
#endif
#ifdef wx_x
  wxFontPool = new XFontPool;
#endif

#if 1
  wxNORMAL_FONT = new wxFont (12, wxSYSTEM, wxNORMAL, wxNORMAL);
#else
  wxNORMAL_FONT = new wxFont (12, wxMODERN, wxNORMAL, wxNORMAL);
#endif
  wxSMALL_FONT = new wxFont (10, wxSWISS, wxNORMAL, wxNORMAL);
  wxITALIC_FONT = new wxFont (12, wxROMAN, wxITALIC, wxNORMAL);
  wxSWISS_FONT = new wxFont (12, wxSWISS, wxNORMAL, wxNORMAL);

  wxRED_PEN = new wxPen ("RED", 0, wxSOLID);
  wxCYAN_PEN = new wxPen ("CYAN", 0, wxSOLID);
  wxGREEN_PEN = new wxPen ("GREEN", 0, wxSOLID);
  wxBLACK_PEN = new wxPen ("BLACK", 0, wxSOLID);
  wxWHITE_PEN = new wxPen ("WHITE", 0, wxSOLID);
  wxTRANSPARENT_PEN = new wxPen ("BLACK", 0, wxTRANSPARENT);
  wxBLACK_DASHED_PEN = new wxPen ("BLACK", 0, wxSHORT_DASH);
  wxGREY_PEN = new wxPen ("GREY", 0, wxSOLID);
  wxMEDIUM_GREY_PEN = new wxPen ("MEDIUM GREY", 0, wxSOLID);
  wxLIGHT_GREY_PEN = new wxPen ("LIGHT GREY", 0, wxSOLID);

  wxWHITE_PEN->Lock(1);
  wxBLACK_PEN->Lock(1);

  wxBLUE_BRUSH = new wxBrush ("BLUE", wxSOLID);
  wxGREEN_BRUSH = new wxBrush ("GREEN", wxSOLID);
  wxWHITE_BRUSH = new wxBrush ("WHITE", wxSOLID);
  wxBLACK_BRUSH = new wxBrush ("BLACK", wxSOLID);
  wxTRANSPARENT_BRUSH = new wxBrush ("BLACK", wxTRANSPARENT);
  wxCYAN_BRUSH = new wxBrush ("CYAN", wxSOLID);
  wxRED_BRUSH = new wxBrush ("RED", wxSOLID);
  wxGREY_BRUSH = new wxBrush ("GREY", wxSOLID);
  wxMEDIUM_GREY_BRUSH = new wxBrush ("MEDIUM GREY", wxSOLID);
  wxLIGHT_GREY_BRUSH = new wxBrush ("LIGHT GREY", wxSOLID);
  
  wxWHITE_BRUSH->Lock(1);
  wxBLACK_BRUSH->Lock(1);
  
  wxColour ctlGray(0xF0, 0xF0, 0xF0);
  wxCONTROL_BACKGROUND_BRUSH = new wxBrush(ctlGray, wxSOLID);
  wxCONTROL_BACKGROUND_BRUSH->Lock(1);

  wxBLACK = new wxColour ("BLACK");
  wxWHITE = new wxColour ("WHITE");
  wxRED = new wxColour ("RED");
  wxBLUE = new wxColour ("BLUE");
  wxGREEN = new wxColour ("GREEN");
  wxCYAN = new wxColour ("CYAN");
  wxLIGHT_GREY = new wxColour ("LIGHT GREY");

  wxSTANDARD_CURSOR = new wxCursor (wxCURSOR_ARROW);
  wxHOURGLASS_CURSOR = new wxCursor (wxCURSOR_WAIT);
  wxCROSS_CURSOR = new wxCursor (wxCURSOR_CROSS);
}

void 
wxDeleteStockObjects (void)
{
  if (wxBLACK)
    delete wxBLACK;
  if (wxWHITE)
    delete wxWHITE;
  if (wxRED)
    delete wxRED;
  if (wxBLUE)
    delete wxBLUE;
  if (wxGREEN)
    delete wxGREEN;
  if (wxCYAN)
    delete wxCYAN;
  if (wxLIGHT_GREY)
    delete wxLIGHT_GREY;

  if (wxSTANDARD_CURSOR)
    delete wxSTANDARD_CURSOR;
  if (wxHOURGLASS_CURSOR)
    delete wxHOURGLASS_CURSOR;
  if (wxCROSS_CURSOR)
    delete wxCROSS_CURSOR;
}

// Pens

wxbPen::wxbPen (void)
{
  __type = wxTYPE_PEN;
  locked = 0;
}

wxbPen::~wxbPen ()
{
	if (stipple)
		--stipple->selectedIntoDC;
}

wxbPen::wxbPen (wxColour& col, int Width, int Style)
{
  locked = 0;
  __type = wxTYPE_PEN;
}

wxbPen::wxbPen (const char *col, int Width, int Style)
{
  locked = 0;
  __type = wxTYPE_PEN;
}

int wxbPen::GetWidth (void)
{
  return width;
}

int wxbPen::GetStyle (void)
{
  return style;
}

int wxbPen::GetJoin (void)
{
  return join;
}

wxBitmap *wxbPen::GetStipple (void)
{
  return stipple;
}

int wxbPen::GetCap (void)
{
  return cap;
}

int wxbPen::GetDashes (wxDash ** ptr)
{
  *ptr = dash;
  return nb_dash;
}

wxColour& wxbPen::GetColour (void)
{
  return colour;
}

void wxbPen::SetColour (wxColour& col)
{
  colour = col;
}

void wxbPen::SetColour (const char *col)
{
  colour = col;
}

void wxbPen::SetColour (char red, char green, char blue)
{
 colour.Set(red, green, blue);
}

void wxbPen::SetWidth (int Width)
{
  width = Width;
}

void wxbPen::SetCap (int Cap)
{
  cap = Cap;
}

void wxbPen::SetJoin (int Join)
{
  join = Join;
}

void wxbPen::SetStyle (int Style)
{
  style = Style;
}

void wxbPen::SetDashes (int nbDash, wxDash * Dash)
{
  nb_dash = nbDash;
  dash = Dash;
}

void wxbPen::SetStipple (wxBitmap * Stipple)
{
  if (Stipple && (Stipple->selectedIntoDC < 0))
    return;
  if (Stipple)
    Stipple->selectedIntoDC++;
  if (stipple)
    --stipple->selectedIntoDC;

  stipple = Stipple;
}

// Brushes

wxbBrush::wxbBrush (void)
{
  __type = wxTYPE_BRUSH;
  locked = 0;
}

wxbBrush::~wxbBrush ()
{
	if (stipple)
		--stipple->selectedIntoDC;
}

wxbBrush::wxbBrush (wxColour& col, int Style)
{
  __type = wxTYPE_BRUSH;
  locked = 0;
}

wxbBrush::wxbBrush (char *col, int Style)
{
  __type = wxTYPE_BRUSH;
  locked = 0;
}

int wxbBrush::GetStyle (void)
{
  return style;
}

wxBitmap *wxbBrush::GetStipple (void)
{
  return stipple;
}

wxColour& wxbBrush::GetColour (void)
{
  return colour;
}

void wxbBrush::SetColour (wxColour& col)
{
  colour = col;
}

void wxbBrush::SetColour (const char *col)
{
  colour = col;
}

void wxbBrush::SetColour (char red, char green, char blue)
{
  colour.Set(red, green, blue);
}

void wxbBrush::SetStyle (int Style)
{
  style = Style;
}

void wxbBrush::SetStipple (wxBitmap * Stipple)
{
  if (Stipple && (Stipple->selectedIntoDC < 0))
    return;
  if (Stipple)
    Stipple->selectedIntoDC++;
  if (stipple)
    --stipple->selectedIntoDC;

  stipple = Stipple;
}

wxGDIList::wxGDIList (void)
{
}

wxGDIList::~wxGDIList (void)
{
#ifndef wx_x
  wxNode *node = First ();
  while (node)
    {
      wxObject *object = (wxObject *) node->Data ();
      wxNode *next = node->Next ();
      delete object;
      node = next;
    }
#endif
}

// Pen and Brush lists
wxPenList::~wxPenList (void)
{
  wxNode *node = First ();
  while (node)
    {
      wxPen *pen = (wxPen *) node->Data ();
      wxNode *next = node->Next ();
      delete pen;
      node = next;
    }
}

void wxPenList::AddPen (wxPen * pen)
{
  Append (pen);
  pen->Lock(1);
}

void wxPenList::RemovePen (wxPen * pen)
{
  DeleteObject (pen);
}

wxPen *wxPenList::FindOrCreatePen (wxColour * colour, int width, int style)
{
  wxPen *pen;

  if (!colour)
    return NULL;

  for (wxNode * node = First (); node; node = node->Next ())
    {
      wxPen *each_pen = (wxPen *) node->Data ();
      if (each_pen &&
	  each_pen->GetWidth () == width &&
	  each_pen->GetStyle () == style &&
	  each_pen->GetColour ().Red () == colour->Red () &&
	  each_pen->GetColour ().Green () == colour->Green () &&
	  each_pen->GetColour ().Blue () == colour->Blue ())
	return each_pen;
    }
  pen = new wxPen (*colour, width, style);
#if WXGARBAGE_COLLECTION_ON
  AddPen(pen);
#endif
  return pen;
}

wxPen *wxPenList::FindOrCreatePen (char *colour, int width, int style)
{
  wxColour *the_colour = wxTheColourDatabase->FindColour (colour);
  if (the_colour)
    return FindOrCreatePen (the_colour, width, style);
  else
    return NULL;
}

wxBrushList::~wxBrushList (void)
{
  wxNode *node = First ();
  while (node)
    {
      wxBrush *brush = (wxBrush *) node->Data ();
      wxNode *next = node->Next ();
      delete brush;
      node = next;
    }
}

void wxBrushList::AddBrush (wxBrush * brush)
{
  brush->Lock(1);
  Append (brush);
}

wxBrush *wxBrushList::FindOrCreateBrush (wxColour * colour, int style)
{
  wxBrush *brush;

  if (!colour)
    return NULL;

  for (wxNode * node = First (); node; node = node->Next ())
    {
      wxBrush *each_brush = (wxBrush *) node->Data ();
      if (each_brush &&
	  each_brush->GetStyle () == style &&
	  each_brush->GetColour ().Red () == colour->Red () &&
	  each_brush->GetColour ().Green () == colour->Green () &&
	  each_brush->GetColour ().Blue () == colour->Blue ())
	return each_brush;
    }
  brush = new wxBrush (*colour, style);
#if WXGARBAGE_COLLECTION_ON
  AddBrush(brush);
#endif
  return brush;
}

wxBrush *wxBrushList::FindOrCreateBrush (char *colour, int style)
{
  wxColour *the_colour = wxTheColourDatabase->FindColour (colour);
  if (the_colour)
    return FindOrCreateBrush (the_colour, style);
  else
    return NULL;
}


void wxBrushList::RemoveBrush (wxBrush * brush)
{
  DeleteObject (brush);
}

wxFontList::~wxFontList (void)
{
  wxNode *node = First ();
  while (node)
    {
      wxFont *font = (wxFont *) node->Data ();
      wxNode *next = node->Next ();
      delete font;
      node = next;
    }
}

void wxFontList::AddFont (wxFont * font)
{
  Append (font);
}

void wxFontList::RemoveFont (wxFont * font)
{
  DeleteObject (font);
}

wxFont *wxFontList::
FindOrCreateFont (int PointSize, int FamilyOrFontId, int Style, int Weight, Bool underline)
{
  wxFont *font;

  for (wxNode * node = First (); node; node = node->Next ())
    {
      wxFont *each_font = (wxFont *) node->Data ();
      if (each_font &&
	  each_font->GetPointSize () == PointSize &&
	  each_font->GetStyle () == Style &&
	  each_font->GetWeight () == Weight &&
	  each_font->GetFontId () == FamilyOrFontId &&
	  each_font->GetUnderlined () == underline)
	return each_font;
    }
  font = new wxFont (PointSize, FamilyOrFontId, Style, Weight, underline);
#if WXGARBAGE_COLLECTION_ON
  AddFont(font);
#endif
  return font;
}

wxFont *wxFontList::
FindOrCreateFont (int PointSize, const char *Face, int Family, int Style, int Weight, Bool underline)
{
  return FindOrCreateFont(PointSize,
			  wxTheFontNameDirectory.FindOrCreateFontId(Face, Family),
			  Style,
			  Weight,
			  underline);
}

#if (!USE_TYPEDEFS)
wxPoint::wxPoint (void) : wxObject(WXGC_NO_CLEANUP)
{
}

wxPoint::wxPoint (float the_x, float the_y)  : wxObject(WXGC_NO_CLEANUP)
{
  x = the_x;
  y = the_y;
}

wxPoint::~wxPoint (void)
{
}
#endif

#if (!USE_TYPEDEFS)
wxIntPoint::wxIntPoint (void)  : wxObject(WXGC_NO_CLEANUP)
{
}

wxIntPoint::wxIntPoint (int the_x, int the_y)  : wxObject(WXGC_NO_CLEANUP)
{
  x = the_x;
  y = the_y;
}

wxIntPoint::~wxIntPoint (void)
{
}
#endif

char *font_defaults[] = {
  /* MATTHEW: [4] Family map */
  "FamilyDefault", "Default",
  "FamilyRoman", "Roman",
  "FamilyDecorative", "Decorative",
  "FamilyModern", "Modern",
  "FamilyTeletype", "Teletype",
  "FamilySwiss", "Swiss",
  "FamilyScript", "Script",
  "FamilySystem", "System",

  "AfmMedium", "",
  "AfmBold", "Bo",
  "AfmLight", "",
  "AfmStraight", "",
  "AfmItalic", "${AfmSlant}",
  "AfmSlant", "O",
  "AfmRoman", "Ro",

  "AfmTimes", "Times",
  "AfmHelvetica", "Helv",
  "AfmCourier", "Cour",

  "Afm___", "${AfmTimes,$[weight],$[style]}",

  "AfmTimes__", "${AfmTimes}${Afm$[weight]}${Afm$[style]}",
  "AfmTimesMediumStraight", "${AfmTimes}${AfmRoman}",
  "AfmTimesLightStraight", "${AfmTimes}${AfmRoman}",
  "AfmTimes_Italic", "${AfmTimes}$[weight]${AfmItalic}",
  "AfmTimes_Slant", "${AfmTimes}$[weight]${AfmItalic}",

  "AfmSwiss__", "${AfmHelvetica}${Afm$[weight]}${Afm$[style]}",
  "AfmModern__", "${AfmCourier}${Afm$[weight]}${Afm$[style]}",

  "AfmTeletype__", "${AfmModern,$[weight],$[style]}",

  "PostScriptMediumStraight", "",
  "PostScriptMediumItalic", "-Oblique",
  "PostScriptMediumSlant", "-Oblique",
  "PostScriptLightStraight", "",
  "PostScriptLightItalic", "-Oblique",
  "PostScriptLightSlant", "-Oblique",
  "PostScriptBoldStraight", "-Bold",
  "PostScriptBoldItalic", "-BoldOblique",
  "PostScriptBoldSlant", "-BoldOblique",

#if WX_NORMALIZED_PS_FONTS
  "PostScript___", "${PostScriptTimes,$[weight],$[style]}",
#else
  "PostScriptRoman__", "${PostScriptTimes,$[weight],$[style]}",
  "PostScript___", "LucidaSans${PostScript$[weight]$[style]}",
#endif

  "PostScriptTimesMedium", "",
  "PostScriptTimesLight", "",
  "PostScriptTimesBold", "Bold",

  "PostScriptTimes__", "Times${PostScript$[weight]$[style]}",
  "PostScriptTimesMediumStraight", "Times-Roman",
  "PostScriptTimesLightStraight", "Times-Roman",
  "PostScriptTimes_Slant", "Times-${PostScriptTimes$[weight]}Italic",
  "PostScriptTimes_Italic", "Times-${PostScriptTimes$[weight]}Italic",

  "PostScriptSwiss__", "Helvetica${PostScript$[weight]$[style]}",
  "PostScriptModern__", "Courier${PostScript$[weight]$[style]}",

  "PostScriptTeletype__", "${PostScriptModern,$[weight],$[style]}",

#if !WX_NORMALIZED_PS_FONTS
  "PostScriptScript__", "Zapf-Chancery-MediumItalic",
#endif

#ifdef wx_x
  "ScreenMedium", "medium",
  "ScreenBold", "bold",
  "ScreenLight", "light",
  "ScreenStraight", "r",
  "ScreenItalic", "i",
  "ScreenSlant", "o",

  /* MATTHEW: [4] "Family" -> "Base" */
  "ScreenDefaultBase", "*-*",
  "ScreenRomanBase", "*-times",
  "ScreenDecorativeBase", "*-helvetica",
  "ScreenModernBase", "*-courier",
  "ScreenTeletypeBase", "*-lucidatypewriter", /* MATTHEW: [4] Not courier */
  "ScreenSwissBase", "*-lucida",
  "ScreenScriptBase", "*-zapfchancery",

  /* MATTHEW: [4] Use ${ScreenStdSuffix} */
  "ScreenStdSuffix", "-${Screen$[weight]}-${Screen$[style]}"
    "-normal-*-*-%d-*-*-*-*-*-*",

  "Screen___",
  "-${ScreenDefaultBase}${ScreenStdSuffix}",
  "ScreenRoman__",
  "-${ScreenRomanBase}${ScreenStdSuffix}",
  "ScreenDecorative__",
  "-${ScreenDecorativeBase}${ScreenStdSuffix}",
  "ScreenModern__",
  "-${ScreenModernBase}${ScreenStdSuffix}",
  "ScreenTeletype__",
  "-${ScreenTeletypeBase}${ScreenStdSuffix}",
  "ScreenSwiss__",
  "-${ScreenSwissBase}${ScreenStdSuffix}",
  "ScreenScript__",
  "-${ScreenScriptBase}${ScreenStdSuffix}",
#endif
#ifdef wx_msw
  "ScreenDefault__", "MS Sans Serif",
  "ScreenRoman__", "Times New Roman",
  "ScreenDecorative__", "",
  "ScreenModern__", "Courier New",
  "ScreenTeletype__", "${ScreenModern,$[weight],$[style]}",
  "ScreenSwiss__", "Arial",
  "ScreenScript__", "Script",
#endif
#ifdef wx_mac
  "ScreenDefault__", "applicationfont",
  "ScreenSystem__", "systemfont",
  "ScreenRoman__", "times",
  "ScreenDecorative__", "geneva",
  "ScreenModern__", "monaco", /* "courier" is also good */
  "ScreenTeletype__", "${ScreenModern,$[weight],$[style]}",
  "ScreenSwiss__", "helvetica",
  "ScreenScript__", "geneva",
#endif
  NULL
};


wxFontNameDirectory wxTheFontNameDirectory;

enum {
  wxWEIGHT_NORMAL,
  wxWEIGHT_BOLD,
  wxWEIGHT_LIGHT,
  wxNUM_WEIGHTS
  };

enum {
  wxSTYLE_NORMAL,
  wxSTYLE_ITALIC,
  wxSTYLE_SLANT,
  wxNUM_STYLES
  };

class wxSuffixMap {
 public:
  char *map[wxNUM_WEIGHTS][wxNUM_STYLES];
  void Initialize(const char *, const char *);
  void Cleanup(void);
};


class wxFontNameItem : public wxObject
{
 public:
  int id;
  int family;
  char *name;
  wxSuffixMap screen, printing, afm;
  Bool isroman;
  
  ~wxFontNameItem(void);
};

wxFontNameItem::~wxFontNameItem(void)
{
	screen.Cleanup();
	printing.Cleanup();
	afm.Cleanup();
	delete [] name;
}

static int WCoordinate(int w)
{
  switch (w) {
  case wxBOLD:
    return wxWEIGHT_BOLD;
  case wxLIGHT:
    return wxWEIGHT_LIGHT;
  case wxNORMAL:
  default:
    return wxWEIGHT_NORMAL;
  }
}

static int SCoordinate(int s)
{
  switch (s) {
  case wxITALIC:
    return wxSTYLE_ITALIC;
  case wxSLANT:
    return wxSTYLE_SLANT;
  case wxNORMAL:
  default:
    return wxSTYLE_NORMAL;
  }
}

wxFontNameDirectory::wxFontNameDirectory(void)
{
  table = new wxHashTable(wxKEY_INTEGER, 20);
  nextFontId = -1;
}

wxFontNameDirectory::~wxFontNameDirectory()
{ // The data in the hash table must be deleted as well as the table
  table->DeleteContents(1);
  delete table;
}

int wxFontNameDirectory::GetNewFontId(void)
{
  return (nextFontId--);
}

#if !USE_RESOURCES
#define wxGetResource(a, b, c) 0
#endif

static void SearchResource(const char *prefix, const char **names, int count, char **v)
{
  int k, i, j;
  char resource[1024], **defaults, *internal;

  k = 1 << count;

  *v = NULL;
  internal = NULL;

  for (i = 0; i < k; i++) {
    strcpy(resource, prefix);
    for (j = 0; j < count; j++) {
      if (!(i & (1 << j)))
		strcat(resource, names[j]);
      else
		strcat(resource, "_");
    }

    if (wxGetResource(wxTheApp->wx_class, (char *)resource, v) && **v)
      return;

    if (!internal) {
      defaults = font_defaults;
      while (*defaults) {
		if (!strcmp(*defaults, resource)) {
	  	  internal = defaults[1];
	  	  break;
		}
		defaults += 2;
      }
    }
  }

  if (internal)
    *v = copystring(internal);
}

void wxFontNameDirectory::Initialize()
{
  wxTheFontNameDirectory.Initialize(wxDEFAULT, wxDEFAULT, "Default");
  wxTheFontNameDirectory.Initialize(wxDECORATIVE, wxDECORATIVE, "Decorative");
  wxTheFontNameDirectory.Initialize(wxROMAN, wxROMAN, "Roman");
  wxTheFontNameDirectory.Initialize(wxMODERN, wxMODERN, "Modern");
  wxTheFontNameDirectory.Initialize(wxTELETYPE, wxTELETYPE, "Teletype");
  wxTheFontNameDirectory.Initialize(wxSWISS, wxSWISS, "Swiss");
  wxTheFontNameDirectory.Initialize(wxSCRIPT, wxSCRIPT, "Script");
#ifdef wx_mac
  wxTheFontNameDirectory.Initialize(wxSYSTEM, wxSYSTEM, "System");
#endif
}

typedef char *a_charptr;
// Note from CJC - Initialize leaked like crazy because the copystring() returned by
// searchresource() was not deleted on expansions.
//
void wxSuffixMap::Initialize(const char *resname, const char *devresname)
{
  const char *weight, *style;
  char *v;
  int i, j, k;
  const char *names[3];

  for (k = 0; k < wxNUM_WEIGHTS; k++) {
    switch (k) {
    case wxWEIGHT_NORMAL:
      weight = "Medium";
      break;
    case wxWEIGHT_LIGHT:
      weight = "Light";
      break;
    case wxWEIGHT_BOLD:
      default:
      weight = "Bold";
  	}
    for (j = 0; j < wxNUM_STYLES; j++) {
      switch (j) {
      case wxSTYLE_NORMAL:
		style = "Straight";
		break;
      case wxSTYLE_ITALIC:
		style = "Italic";
		break;
      case wxSTYLE_SLANT:
	      default:
		style = "Slant";
   	  }

      names[0] = resname;
      names[1] = weight;
      names[2] = style;
      
      SearchResource(devresname, names, 3, &v);

      /* Expand macros in the found string: */
  found:
      int len, closer = 0, startpos = 0;

      len = (v ? strlen(v) : 0);
      for (i = 0; i < len; i++)
		if (v[i] == '$' && ((v[i+1] == '[') || (v[i+1] == '{'))) {
	 		startpos = i;
	  		if (v[i+1] == '[')
	   		  closer = ']';
	  		else
	    	  closer = '}';
	  		i++;
		} else if (v[i] == closer) {
	  	  int newstrlen;
	  	  const char *r = NULL;
	  	  char *naya, *name;
	  	  char *tmp; //cjc
	  
	  	  name = v + startpos + 2;
	  	  v[i] = 0;
		  tmp = NULL; //cjc
	  	  if (closer == '}') {
	    	int i, count, len;
	    	char **names;

	    	for (i = 0, count = 1; name[i]; i++)
	      		if (name[i] == ',')
				  count++;
	    
	    	len = i;

	    	names = new a_charptr[count];
	    	names[0] = name;
	    	for (i = 0, count = 1; i < len; i++)
	      		if (name[i] == ',') {
					names[count++] = name + i + 1;
					name[i] = 0;
	      		}

	    	SearchResource("", (const char **)names, count, (char **)&r);
	    	delete[] names;
			tmp = (char *)r;	// cjc
	    	if (!r) {
	      		for (i = 0; i < len; i++)
					if (!name[i])
		  				name[i] = ',';
	      		r = "";
	      		printf("Bad resource name \"%s\" in font lookup\n", name);
	    	}
	  	  } else if (!strcmp(name, "weight")) {
	    	r = weight;
	      } else if (!strcmp(name, "style")) {
	    	r = style;
	      } else if (!strcmp(name, "family")) {
	    	r = resname;
	      } else {
	    	r = "";
	    	printf("Bad font macro name \"%s\"\n", name);
	      }
	    newstrlen = strlen(r);

	    naya = new char[len + newstrlen + 1];
	    memcpy(naya, v, startpos);
	    memcpy(naya + startpos, r, newstrlen);
	    memcpy(naya + startpos + newstrlen, v + i + 1, len - i + 1);
	    delete[] v;
	    delete[] tmp; //cjc
	    v = naya;

	    goto found;
	  }

#if defined(wx_msw) || defined(wx_mac)
      if (!v)
		v = copystring(resname);
#endif
      /* We have a final value: */
      map[k][j] = v;
    } // wxNUM_STYLES (j);
  } // wxNUM_WEIGHTS (k)
}

void wxSuffixMap::Cleanup(void)
{
	int i,j;
	char *v, **vec,*str;
	vec = map[0,0];
	for (i = 0; i < wxNUM_WEIGHTS; i++) {
		for (j = 0; j < wxNUM_STYLES; j++) {
			str = *vec++;
			if (*str) {
				delete   str;
			}
		}
	}
}

void wxFontNameDirectory::Initialize(int fontid, int family, const char *resname)
{
  wxFontNameItem *item = new wxFontNameItem;
  char *fam, resource[256];
  
  item->id = fontid;
  item->family = family;
  
  sprintf(resource, "Family%s", resname);
  fam = NULL;
  SearchResource((const char *)resource, NULL, 0, (char **)&fam);
  if (fam) {
    if (!strcmp(fam, "Default"))
      item->family = wxDEFAULT;
    else if (!strcmp(fam, "Roman"))
      item->family = wxROMAN;
    else if (!strcmp(fam, "Decorative"))
      item->family = wxDECORATIVE;
    else if (!strcmp(fam, "Modern"))
      item->family = wxMODERN;
    else if (!strcmp(fam, "Teletype"))
      item->family = wxTELETYPE;
    else if (!strcmp(fam, "Swiss"))
      item->family = wxSWISS;
    else if (!strcmp(fam, "Script"))
      item->family = wxSCRIPT;
#ifdef wx_mac
    else if (!strcmp(fam, "System"))
      item->family = wxSYSTEM;
#endif
	delete [] fam;
  }

  item->name = copystring(resname);
  item->screen.Initialize(resname, "Screen");
  item->printing.Initialize(resname, "PostScript");
  item->afm.Initialize(resname, "Afm");

  table->Put(fontid, item);
}

int wxFontNameDirectory::FindOrCreateFontId(const char *name, int family)
{
  int id;

  if (id = GetFontId(name))
    return id;

  id = GetNewFontId();
  Initialize(id, family, name);

  return id;
}

char *wxFontNameDirectory::GetScreenName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return NULL;

  return item->screen.map[WCoordinate(weight)][SCoordinate(style)];
}

char *wxFontNameDirectory::GetPostScriptName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return NULL;

  return item->printing.map[WCoordinate(weight)][SCoordinate(style)];
}

char *wxFontNameDirectory::GetAFMName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return NULL;

  return item->afm.map[WCoordinate(weight)][SCoordinate(style)];
}

char *wxFontNameDirectory::GetFontName(int fontid)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return NULL;

  return item->name;
}

int wxFontNameDirectory::GetFontId(const char *name)
{
  wxNode *node;

  table->BeginFind();

  while (node = table->Next()) {
    wxFontNameItem *item = (wxFontNameItem *)node->Data();
    if (!strcmp(name, item->name))
      return item->id;
  }

  return 0;
}

int wxFontNameDirectory::GetFamily(int fontid)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return wxDEFAULT;

  return item->family;
}
