/*
 * Global Data
 *
 * RCS_ID:      $Id: wb_data.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 */

/* static const char sccsid[] = "@(#)wb_data.cc	1.2 5/9/94"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_list.h"
#include "wx_gdi.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#define _MAXPATHLEN 500

// Useful buffer, initialized in wxCommonInit
char *wxBuffer = NULL;

#if 0
// Windows List
wxList wxTopLevelWindows;
#endif

// Message Strings for Internationalization
char **wx_msg_str = (char**)NULL;

// Some global printer parameters
char wx_printer_file[_MAXPATHLEN];
float wx_printer_scale_x = 1.0;
float wx_printer_scale_y = 1.0;
float wx_printer_translate_x = 0.0;
float wx_printer_translate_y = 0.0;
int wxPageNumber;

// GDI Object Lists
wxBrushList *wxTheBrushList = NULL;
wxPenList   *wxThePenList = NULL;
wxFontList   *wxTheFontList = NULL;
wxGDIList   *wxTheBitmapList = NULL;
//wxGDIList   *wxTheColourList = NULL;

wxColourDatabase *wxTheColourDatabase = NULL;

// Stock objects
wxFont *wxNORMAL_FONT;
wxFont *wxSMALL_FONT;
wxFont *wxITALIC_FONT;
wxFont *wxSWISS_FONT;
wxPen *wxRED_PEN;

wxPen *wxCYAN_PEN;
wxPen *wxGREEN_PEN;
wxPen *wxBLACK_PEN;
wxPen *wxWHITE_PEN;
wxPen *wxTRANSPARENT_PEN;
wxPen *wxBLACK_DASHED_PEN;
wxPen *wxGREY_PEN;
wxPen *wxMEDIUM_GREY_PEN;
wxPen *wxLIGHT_GREY_PEN;

wxBrush *wxBLUE_BRUSH;
wxBrush *wxGREEN_BRUSH;
wxBrush *wxWHITE_BRUSH;
wxBrush *wxBLACK_BRUSH;
wxBrush *wxTRANSPARENT_BRUSH;
wxBrush *wxCYAN_BRUSH;
wxBrush *wxRED_BRUSH;
wxBrush *wxGREY_BRUSH;
wxBrush *wxMEDIUM_GREY_BRUSH;
wxBrush *wxLIGHT_GREY_BRUSH;

wxBrush *wxCONTROL_BACKGROUND_BRUSH;

wxColour *wxBLACK;
wxColour *wxWHITE;
wxColour *wxRED;
wxColour *wxBLUE;
wxColour *wxGREEN;
wxColour *wxCYAN;
wxColour *wxLIGHT_GREY;

wxCursor *wxSTANDARD_CURSOR = NULL;
wxCursor *wxHOURGLASS_CURSOR = NULL;
wxCursor *wxCROSS_CURSOR = NULL;

/*
 * Edward, haven't added in your wx_version and wx_version_string constants:
 * do you think they're necessary over and above the #defines?
 */

