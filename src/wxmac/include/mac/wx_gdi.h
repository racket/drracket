///////////////////////////////////////////////////////////////////////////////
// File:	wx_gdi.h (Macintosh version)
// Purpose:	Declaration of various graphics objects - fonts, pens, icons etc.
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_gdih
#define wx_gdih

#include "wb_gdi.h"
#include <QDOffscreen.h>

#ifdef IN_CPROTO
typedef       void    *wxFont ;
typedef       void    *wxColourMap;
typedef       void    *wxPen;
typedef       void    *wxBrush;
typedef       void    *wxIcon;
typedef       void    *wxCursor;
typedef       void    *wxBitmap;
typedef       void    *XFontInfo;
typedef       void    *XFontPool;
#else

// Font
class wxFont: public wxbFont
{
 private:
	short macFontId;

	void Create(int PointSize, int FontId, int Family, int Style, int Weight, Bool underlined);

 public:
 	static CGrafPtr gMacFontGrafPort; // mac platform only

 public:
	wxFont(void);
	wxFont(int PointSize, int FontOrFamilyId, int Style, int Weight, Bool underlined = FALSE);
	wxFont(int PointSize, const char *Face, int Family, int Style, int Weight, 
	 Bool underlined = FALSE);
	~wxFont(void);

	float GetCharHeight(void);		// Mac Platform Only?
	float GetCharWidth(void);
	void  GetTextExtent(char* string, float* x, float* y,
						float* descent, float* externalLeading, Bool use16 = FALSE);

	int GetMacFontNum(void); // mac platform only
	Style GetMacFontStyle(void); // mac platform only
};

class wxColourMap: public wxObject
{
 public:
 	CGrafPtr cmap;

  wxColourMap(void);
  ~wxColourMap(void);
};

#define wxColorMap wxColourMap

// Pen
class wxPen: public wxbPen
{
 public:
  wxPen(void);
  wxPen(wxColour& col, int width, int style);
  wxPen(char *col, int width, int style);
  ~wxPen(void);

};

// Brush
class wxBrush: public wxbBrush
{
 public:
  wxBrush(void);
  wxBrush(wxColour& col, int style);
  wxBrush(char *col, int style);
  ~wxBrush(void);

};


// Bitmap
class wxItem;
class wxBitmap: public wxObject
{
 protected:
  int width;
  int height;
  int depth;
  Bool ok;
 public:
  GWorldPtr x_pixmap;
  Bool freePixmap;
  class wxMemoryDC *selectedInto; // mflatt
  Bool selectedIntoDC;

  wxBitmap(void) ;
  wxBitmap(char bits[], int width, int height, int depth = 1);
#if USE_XPM_IN_MAC
  // Initialize with XPM data
  wxBitmap(char **bits, wxItem *anItem = NULL);
#endif
  // Load a file or resource
  wxBitmap(char *name, long flags = wxBITMAP_DISCARD_COLOURMAP | wxBITMAP_TYPE_RESOURCE);

  // If depth is omitted, will create a bitmap compatible with the display
  wxBitmap(int width, int height, int depth = -1);
  ~wxBitmap(void);

  virtual Bool Create(int width, int height, int depth = -1);
  virtual Bool LoadFile(char *name, long flags = wxBITMAP_DISCARD_COLOURMAP | wxBITMAP_TYPE_RESOURCE);
  virtual Bool SaveFile(char *name, int type, wxColourMap *cmap = NULL);

  inline Bool Ok(void) { return ok; }
  inline int GetWidth(void) { return width; }
  inline int GetHeight(void) { return height; }
  inline int GetDepth(void) { return depth; }
  inline void SetWidth(int w) { width = w; }
  inline void SetHeight(int h) {height = h; }
  inline void SetDepth(int d) { depth = d; }
  inline void SetOk(Bool isOk) { ok = isOk; }
  // Heres a couple of methods used by wxExtend/wxPython exgdi.cc
  void SetColourMap(wxColourMap *cmap);
  wxColourMap* GetColourMap(void);


  // Some Mac extensions ---- should only be used inside wxWindows, Please.
  void DrawMac(void);
  void DrawMac(int x, int y);
};

// Icon
class wxIcon: public wxBitmap
{
 protected:
 public:
  wxIcon(void);
  wxIcon(char bits[], int width, int height);
  wxIcon(char *name, int type = wxBITMAP_TYPE_XBM);
  ~wxIcon(void);
};

// Cursor
class wxCursor: public wxObject
{
 public:
  CursHandle cMacCursor;

  wxCursor(void);
  wxCursor(char bits[], int width, int height, int depth = 1);
  wxCursor(char *name);
  wxCursor(int cursor_type);
  ~wxCursor(void);

  Bool Ok(void);
};

#endif // IN_CPROTO
#endif // wx_gdih

