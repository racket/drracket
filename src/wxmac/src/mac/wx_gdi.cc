///////////////////////////////////////////////////////////////////////////////
// File:	wx_gdi.cc (Macintosh version)
// Purpose:	GDI (Graphics Device Interface) objects and functions
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";
#include "common.h"
#include "wx_list.h"
#include "wx_utils.h"
#include "wx_gdi.h"
#include "wx_dcmem.h"
#include <Strings.h>
#include <Resources.h>
#include <QDOffscreen.h>
#if USE_XPM_IN_MAC
#define FOR_MAC
#include "xpm34.h"
#endif
#if USE_IMAGE_LOADING_IN_MAC
#include "wx_image.h"
#endif
CGrafPtr wxFont::gMacFontGrafPort = NULL; // mac platform only

wxGDIList   *wxTheIconList = NULL;
wxGDIList   *wxTheCursorList = NULL;
wxGDIList   *wxTheColourList = NULL;

void *XpmMalloc(size_t size)
{
  return new char[size];
}

void *XpmMallocA(size_t size)
{
  return new WXGC_ATOMIC char[size];
}

static void *DoXpmRealloc(void *(*alloc)(size_t), void *ptr, size_t size)
{
  void *naya;
  size_t osize;
  
  naya = alloc(size);
  
  osize = GC_size(ptr);
  if (size < osize)
    osize = size;
    
  memcpy(naya, ptr, osize);
  
  return naya;
}

void *XpmRealloc(void *ptr, size_t size)
{
  return DoXpmRealloc(XpmMalloc, ptr, size);
}

void *XpmReallocA(void *ptr, size_t size)
{
  return DoXpmRealloc(XpmMallocA, ptr, size);
}

void XpmFree(void *)
{
	/* Do nothing */
}

#ifdef wx_xview

/* These cursors courtesy of xfig
 */

static unsigned short    bull_cursor_array[16] = {
    0x0F00, 0x30C0, 0x4020, 0x4020, 0x8010, 0x8610, 0x8610, 0x8010,
    0x4020, 0x4020, 0x30C0, 0x0F00, 0x0000, 0x0000, 0x0000, 0x0000
};

static unsigned short    char_cursor_data[16] = {
    0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00,
    0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00,
};

static unsigned short    crosshair_cursor_data[16] = {
    0x1000, 0x1000, 0x1000, 0xFE00, 0x1000, 0x1000, 0x1000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

static unsigned short    magnifier_cursor_array[16] = {
    0x0F80, 0x3060, 0x4010, 0x4010, 0x8008, 0x8008, 0x8008, 0x8008,
    0x8008, 0x4010, 0x4010, 0x3078, 0x0F9C, 0x000E, 0x0007, 0x0003,
};

static unsigned short    pencil_cursor_array[16] = {
    0x0000, 0x0018, 0x0024, 0x0075, 0x009B, 0x0117, 0x022E, 0x045C,
    0x08B8, 0x1170, 0x22E0, 0x25C0, 0x7B80, 0x6700, 0x8600, 0x0800,
};

static unsigned short    vbar_cursor_array[16] = {
    0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000,
    0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000,
};

static unsigned short hand_cursor_array[] =
{
  0x0C00,0x1200,0x1200,0x1380,0x1240,0x7270,0x9248,0x924E,
  0x9249,0x9249,0x9009,0x8001,0x4002,0x4002,0x2004,0x2004
};
#endif

//-----------------------------------------------------------------------------
wxFont::wxFont(void)
{
	Create(10, wxDEFAULT, 
			wxDEFAULT, 
			wxNORMAL, wxNORMAL, FALSE);
}

//-----------------------------------------------------------------------------
// Constructor for a font. Note that the real construction is done
// in wxDC::SetFont, when information is available about scaling etc.
//-----------------------------------------------------------------------------
wxFont::wxFont(int PointSize, int FontOrFamilyId, int Style, int Weight, Bool Underlined)
{
	Create(PointSize, FontOrFamilyId, 
			wxTheFontNameDirectory.GetFamily(FontOrFamilyId), 
			Style, Weight, Underlined);
}

wxFont::wxFont(int PointSize, const char *Face, int Family, int Style, int Weight, 
	 	         Bool underlined)
{
	int id = wxTheFontNameDirectory.FindOrCreateFontId(Face, Family);
	int fam = wxTheFontNameDirectory.GetFamily(id);
	
	Create(PointSize, id, fam, Style, Weight, underlined);
}

void wxFont::Create(int PointSize, int Font, int Family, int Style, int Weight, 
	 			Bool Underlined)
{
	fontid = Font;
	family = Family;
	style = Style;
	weight = Weight;
	point_size = PointSize;
	underlined = Underlined;

	char *name = wxTheFontNameDirectory.GetScreenName(fontid, Weight, Style);
	Str255 buffer;

	if (!strcmp(name, "systemfont"))
		macFontId = GetSysFont();
	else if (!strcmp(name, "applicationfont"))
		macFontId = GetAppFont();
	else {
		strcpy((char *)buffer, name);
		C2PStr((char *)buffer);
		::GetFNum((ConstStr255Param)buffer, &macFontId);
	}

#if !WXGARBAGE_COLLECTION_ON
	wxTheFontList->Append(this);
#endif
}

//-----------------------------------------------------------------------------
wxFont::~wxFont()
{
#if !WXGARBAGE_COLLECTION_ON
  wxTheFontList->DeleteObject(this);
#endif
}

//-----------------------------------------------------------------------------
float wxFont::GetCharHeight(void)
{
	GrafPtr oldPort;
	::GetPort(&oldPort);
	::SetPort((GrafPtr)gMacFontGrafPort);
	::TextFont(GetMacFontNum());
	::TextSize(point_size);
	::TextFace(GetMacFontStyle());
	FontInfo fontInfo;
	::GetFontInfo(&fontInfo);
	::SetPort(oldPort);
	return fontInfo.ascent + fontInfo.descent + fontInfo.leading;
}

//-----------------------------------------------------------------------------
float wxFont::GetCharWidth(void)
{
	GrafPtr oldPort;
	::GetPort(&oldPort);
	::SetPort((GrafPtr)gMacFontGrafPort);
	::TextFont(GetMacFontNum());
	::TextSize(point_size);
	::TextFace(GetMacFontStyle());
	FontInfo fontInfo;
	::GetFontInfo(&fontInfo);
	::SetPort(oldPort);
	return fontInfo.widMax;
}

//-----------------------------------------------------------------------------
void wxFont::GetTextExtent(char* string, float* x, float* y,
							float* descent, float* externalLeading, Bool use16)
{
	GrafPtr oldPort;
	::GetPort(&oldPort);
	::SetPort((GrafPtr)gMacFontGrafPort);
	::TextFont(GetMacFontNum());
	::TextSize(point_size);
	::TextFace(GetMacFontStyle());
	FontInfo fontInfo;
	::GetFontInfo(&fontInfo);
	*x = TextWidth(string, 0, strlen(string)); // width
#if 0	// CJC, mflatt
	*x += 5; // WCH: kludge, to handle italic font and word wrapping at end of line
#endif
	*y = fontInfo.ascent + fontInfo.descent + fontInfo.leading; // height
	if (descent) *descent = fontInfo.descent;
	if (externalLeading) *externalLeading = fontInfo.leading;
	::SetPort(oldPort);
}

//-----------------------------------------------------------------------------
int wxFont::GetMacFontNum(void) // mac platform only
{
	return macFontId;
}

//-----------------------------------------------------------------------------
Style wxFont::GetMacFontStyle(void) // mac platform only
{
	Style result = 0;
	if (weight == wxBOLD)
		 result |= bold;
	if (style == wxITALIC || style == wxSLANT) 
		result |= italic;
	if (underlined) 
		result |= underline;
	return result;
}



/*
 * Colour map
 *
 */

//-----------------------------------------------------------------------------
wxColourMap::wxColourMap(void)
{
#ifdef wx_x
  cmap = 0;
#endif
}

//-----------------------------------------------------------------------------
wxColourMap::~wxColourMap(void)
{
}


// Pens

//-----------------------------------------------------------------------------
wxPen::wxPen(void)
{
  colour = NULL;
  stipple = NULL ;
  style = wxSOLID;
  join = wxJOIN_ROUND ;
  cap = wxCAP_ROUND ;
  nb_dash = 0 ;
  dash = NULL ;
  width = 1;

#if !WXGARBAGE_COLLECTION_ON
  wxThePenList->AddPen(this);
#endif
}

//-----------------------------------------------------------------------------
wxPen::~wxPen()
{
#if !WXGARBAGE_COLLECTION_ON
  wxThePenList->RemovePen(this);
#endif
}

//-----------------------------------------------------------------------------
wxPen::wxPen(wxColour& col, int Width, int Style):
  wxbPen(col, Width, Style)
{
  colour = col;
  stipple = NULL ;
  width = Width;
  if (width < 1)
  	width = 1;
  style = Style;
  join = wxJOIN_ROUND ;
  cap = wxCAP_ROUND ;
  nb_dash = 0 ;
  dash = NULL ;
#if !WXGARBAGE_COLLECTION_ON
  wxThePenList->AddPen(this);
#endif
}

//-----------------------------------------------------------------------------
wxPen::wxPen(char *col, int Width, int Style):
  wxbPen(col, Width, Style)
{
  SetColour(col);
  stipple = NULL ;
  width = Width;
  if (width < 1)
  	width = 1;
  style = Style;
  join = wxJOIN_ROUND ;
  cap = wxCAP_ROUND ;
  nb_dash = 0 ;
  dash = NULL ;

#if !WXGARBAGE_COLLECTION_ON
  wxThePenList->AddPen(this);
#endif
}

// Brushes

//-----------------------------------------------------------------------------
wxBrush::wxBrush(void)
{
  colour = NULL;
  style = wxSOLID;
  stipple = NULL ;
#if !WXGARBAGE_COLLECTION_ON
  wxTheBrushList->AddBrush(this);
#endif
}

//-----------------------------------------------------------------------------
wxBrush::~wxBrush()
{
#if !WXGARBAGE_COLLECTION_ON
  wxTheBrushList->RemoveBrush(this);
#endif
}

//-----------------------------------------------------------------------------
wxBrush::wxBrush(wxColour& col, int Style):
  wxbBrush(col, Style)
{
  SetColour(col);
  style = Style;
  stipple = NULL ;
#if !WXGARBAGE_COLLECTION_ON
  wxTheBrushList->AddBrush(this);
#endif
}

//-----------------------------------------------------------------------------
wxBrush::wxBrush(char *col, int Style):
  wxbBrush(col, Style)
{
  SetColour(col);
  style = Style;
  stipple = NULL ;
#if !WXGARBAGE_COLLECTION_ON
  wxTheBrushList->AddBrush(this);
#endif
}

// Icons - Remember a wxIcon is for minimized windows which the
// Mac can't do.

//-----------------------------------------------------------------------------
wxIcon::wxIcon(char bits[], int Width, int Height)
: wxBitmap(bits, Width, Height)
{
  __type = wxTYPE_ICON;
}

//-----------------------------------------------------------------------------
wxIcon::wxIcon(void) : wxBitmap()
{
  __type = wxTYPE_ICON;
}

//-----------------------------------------------------------------------------
wxIcon::wxIcon(char *icon_file, int file_format)
: wxBitmap(icon_file, file_format)
{
  __type = wxTYPE_ICON;
}

//-----------------------------------------------------------------------------
wxIcon::~wxIcon(void)
{
}

// Cursors

//-----------------------------------------------------------------------------
wxCursor::wxCursor(void)
{
  __type = wxTYPE_CURSOR;
  cMacCursor = NULL;
#if !WXGARBAGE_COLLECTION_ON
  wxTheCursorList->Append(this) ;
#endif
}

//-----------------------------------------------------------------------------
wxCursor::wxCursor(char bits[], int width, int height, int depth)
{
  __type = wxTYPE_CURSOR;
  cMacCursor = NULL;
#if !WXGARBAGE_COLLECTION_ON
  wxTheCursorList->Append(this) ;
#endif
}

//-----------------------------------------------------------------------------
wxCursor::wxCursor(char *cursor_file)
{
  __type = wxTYPE_CURSOR;
  cMacCursor = NULL;
#if !WXGARBAGE_COLLECTION_ON
  wxTheCursorList->Append(this) ;
#endif
}

//-----------------------------------------------------------------------------
// Cursors by stock number
//-----------------------------------------------------------------------------
wxCursor::wxCursor(int cursor_type)
{
  __type = wxTYPE_CURSOR;
  cMacCursor = NULL;

  switch (cursor_type)
  {
    case wxCURSOR_WAIT:
    case wxCURSOR_WATCH:
    {
	  cMacCursor = GetCursor(watchCursor);
      break;
    }
    case wxCURSOR_CROSS:
    {
	  cMacCursor = GetCursor(crossCursor);
      break;
    }
    case wxCURSOR_CHAR:
    {
      break;
    }
    case wxCURSOR_HAND:
    {
      break;
    }
    case wxCURSOR_BULLSEYE:
    {
      break;
    }
    case wxCURSOR_PENCIL:
    {
      break;
    }
    case wxCURSOR_MAGNIFIER:
    {
     break;
    }
    case wxCURSOR_IBEAM:
    {
	  cMacCursor = GetCursor(iBeamCursor);
      break;
    }
    case wxCURSOR_NO_ENTRY:
    {
      break;
    }

    case wxCURSOR_LEFT_BUTTON:
    {
      break;
    }
    case wxCURSOR_RIGHT_BUTTON:
    {
      break;
    }
    case wxCURSOR_MIDDLE_BUTTON:
    {
      break;
    }
    case wxCURSOR_QUESTION_ARROW:
    {
      break;
    }
    case wxCURSOR_SIZING:
    {
      break;
    }
    case wxCURSOR_SPRAYCAN:
    {
      break;
    }
    case wxCURSOR_PAINT_BRUSH:
    {
      break;
    }
    case wxCURSOR_SIZENWSE:
    case wxCURSOR_SIZENESW:
    {
      break;
    }
    case wxCURSOR_SIZEWE:
    {
      break;
    }
    case wxCURSOR_SIZENS:
    {
      break;
    }
    case wxCURSOR_POINT_LEFT:
    {
      break;
    }
    case wxCURSOR_POINT_RIGHT:
    {
      break;
    }
    default:
    case wxCURSOR_ARROW:
    {
      break;
    }
    case wxCURSOR_BLANK:
    {
      break ;
    }
  }
#if !WXGARBAGE_COLLECTION_ON
  wxTheCursorList->Append(this) ;
#endif
}

//-----------------------------------------------------------------------------
wxCursor::~wxCursor(void)
{
#if !WXGARBAGE_COLLECTION_ON
  wxTheCursorList->DeleteObject(this) ;
#endif
}

//-----------------------------------------------------------------------------
Bool wxCursor::Ok(void)
{
  return !!cMacCursor;
}

//-----------------------------------------------------------------------------
// Global cursor setting
//-----------------------------------------------------------------------------
void wxSetCursor(wxCursor *cursor)
{
  static wxCursor *curCursor;
  
  if (cursor != curCursor) {
	  if (cursor->cMacCursor)
		::SetCursor(*(cursor->cMacCursor));
	  else
	 	::SetCursor(&(qd.arrow));
	 curCursor = cursor;
  }
  wxFlushEvents();
}

// Misc. functions

//-----------------------------------------------------------------------------
// Return TRUE if we have a colour display
//-----------------------------------------------------------------------------
Bool wxColourDisplay(void)
{
	return wxDisplayDepth() > 1;
}

//-----------------------------------------------------------------------------
// Returns depth of screen
//-----------------------------------------------------------------------------
int wxDisplayDepth(void)
{
#if USE_XPM_IN_MAC
	return XDefaultDepth(NULL, NULL); // Args are not used for Mac
#else
    int d, b;
	PixMapHandle pmap;
	GDHandle dev;
	dev = GetGDevice();
	pmap = (**dev).gdPMap;
	b = (**pmap).pixelSize;
    return (b);
#endif
}

//-----------------------------------------------------------------------------
// Get size of display
//-----------------------------------------------------------------------------
void wxDisplaySize(int *width, int *height)
{
	*width = qd.screenBits.bounds.right - qd.screenBits.bounds.left;
	*height = qd.screenBits.bounds.bottom - qd.screenBits.bounds.top - LMGetMBarHeight();
}

//------------------ BitMaps ------------------------------------------
/*
	on the Mac, the wxBitMap needs to be a structure that will allow
	us to redraw. Externally a wxBitmap is a picture (file or resource).
	Internally, its an offscreen GWorld (and its pixmap).

	We will have to extend the API so that parts of wxWindow (like
	wx_buttn.cc) can draw a wxBitMap.

	Also wxDC::DrawIcon might want to use this code.

	Warning - might leak memory. 
    wxTheBitmapList (is it needed?)
*/
wxBitmap::wxBitmap(void)
{
  __type = wxTYPE_BITMAP;
  ok = FALSE;
  width = 0;
  height = 0;
  depth = 0;
  x_pixmap = NULL;
  selectedInto = NULL;
  WXGC_IGNORE(selectedInto);
  // wxTheBitmapList->Append(this); 
  //Create() should be used to add this to the list
}

//-----------------------------------------------------------------------------
wxBitmap::wxBitmap(char bits[], int the_width, int the_height, int no_bits)
{
  __type = wxTYPE_BITMAP;
  depth = no_bits;
  width = the_width;
  height = the_height;
  //Rect bounds = {0, 0, the_height, the_width};
  GDHandle savegd;
  CGrafPtr saveport;
  GetGWorld(&saveport, &savegd);
  Create(the_width, the_height, no_bits);
  if (ok) {
  	SetGWorld(x_pixmap, 0);
  	int i, j;
  	char byte;
  	int bit1;
	RGBColor	cpix;
	// look in contrib/wxwxpm/simx.c for a clue on finishing this 
  	switch (no_bits) {
	  case 0:
	  case 1:
		GetForeColor(&cpix);
		for (i = 0; i < the_height; i++) {
			for (j = 0; j < the_width; i++) {
				byte = bits[j,i];
				for (int k = 0; k < 8; k++) {
					if (byte & 1) {			
						::SetCPixel(j, i, &cpix);
					}
					byte = byte >> 1;
				}
			}
		}
		break;
	  case 8:
		for (i = 0; i < the_height; i++) {
			for (j = 0; j < the_width; i++) {
				// convert byte to RGB - how ? FIXME				
				::SetCPixel(j, i, &cpix);
			}
		}
		break;
	  case 16:
	  case 24:
	  case 32:
		break;
	} // end switch
    SetGWorld(saveport, savegd);
  }
  //ok = TRUE;
}

//-----------------------------------------------------------------------------
wxBitmap::wxBitmap(char *bitmap_file, long flags)
{
	__type = wxTYPE_BITMAP;
    selectedInto = NULL;
    WXGC_IGNORE(selectedInto);
	if (flags & wxBITMAP_TYPE_PICT_RESOURCE)	{ 
		// look for a 'PICT' resource with the given name
		Str255 resname;
		PicHandle	h;
		ResType	thetype;
		strcpy((char *)resname, bitmap_file);
		C2PStr((char *)resname);
		h = (PicHandle)::GetNamedResource('PICT', resname);
		if (h) {
			depth =  wxDisplayDepth();
			width = (*h)->picFrame.right;
			height = (*h)->picFrame.bottom;
			GDHandle savegd;
			CGrafPtr saveport;
			GetGWorld(&saveport, &savegd);
			Rect bounds = {0, 0, height, width};
			Create(width, height, depth);
			SetGWorld(x_pixmap, 0);
			DrawPicture( h, &bounds);
	  		::ReleaseResource((Handle)h);
			::SetGWorld(saveport, savegd);
			return;
		}
	}
	// we also get here if we asked for a resource but it wasn't found
	x_pixmap = NULL;
	if (LoadFile(bitmap_file, flags) == FALSE) {
		char t[200];
		sprintf(t, "Could not load Bitmap: %s", bitmap_file);
		// mflatt: This is not a fatal error
		// wxFatalError(t);
		// wxError(t);
	}
	
}

//-----------------------------------------------------------------------------
// Create a new bitmap of a given size and depth
//-----------------------------------------------------------------------------
wxBitmap::wxBitmap(int w, int h, int d)
{
	Create(w, h, d);
}

//-----------------------------------------------------------------------------
wxBitmap::~wxBitmap(void)
{
#if !WXGARBAGE_COLLECTION_ON
	wxTheBitmapList->DeleteObject(this);
#endif
	
	if (selectedInto)
		selectedInto->SelectObject(NULL);

	if (x_pixmap)
		// Louis Birk Suggests:
#ifdef LkB
		::DisposeCTable((*x_pixmap->portPixMap)->pmTable);
		(*x_pixmap->portPixMap)->pmTable = 0;
		::DisposePtr((Ptr) (*x_pixmap->portPixMap)->baseAddr);
		(*x_pixmap->portPixMap)->baseAddr = 0;
		// End of birk@moonface.com mods
#else
		DisposeGWorld(x_pixmap);
#endif
}

Bool wxBitmap::Create(int wid, int hgt, int deep)
{
  __type = wxTYPE_BITMAP;
  width = wid;
  height = hgt;
  depth = deep;
  selectedInto = NULL;
  WXGC_IGNORE(selectedInto);
  Rect bounds = {0, 0, height, width};
  // Looks like we need to build a offscreen GWorld to draw the Picture in
  GDHandle savegw;
  CGrafPtr saveport;
  GetGWorld(&saveport, &savegw);
  QDErr err;
  GWorldPtr	newGWorld;
  err = NewGWorld(&newGWorld, 0, &bounds, NULL, NULL, noNewDevice);
  if (err == noErr) {
	  SetGWorld(newGWorld, 0);
	  if (depth < 1)
	    depth = wxDisplayDepth();
	  ::EraseRect(&bounds);
	  ok = TRUE;
	  x_pixmap = newGWorld;
	  SetGWorld(saveport, savegw);
#if !WXGARBAGE_COLLECTION_ON
	  wxTheBitmapList->Append(this);  
#endif
  }
  else {				// matt flatt suggests 
	ok = FALSE;
	x_pixmap = NULL;
  }
  return ok;
}

#if USE_XPM_IN_MAC
// Load a bitmap with xpm data (compiled in)
wxBitmap::wxBitmap(char **data, wxItem *anItem)
{
  __type = wxTYPE_BITMAP;
  width = 0;
  height = 0;
  depth = 0;
  freePixmap = FALSE;
  XImage	*ximage;
  XpmAttributes xpmAttr;

  ok = FALSE;

  xpmAttr.valuemask = XpmReturnInfos;	/* nothing yet, but get infos back */
  int  ErrorStatus = XpmCreateImageFromData(NULL,	// don't have a Display dpy
					 data,
					 &ximage,
					 NULL,				// don't want a shapemask 
					 &xpmAttr);

  if (ErrorStatus == XpmSuccess) {
    // Set attributes
    width=xpmAttr.width;
    height = xpmAttr.height;
	depth = ximage->depth;
    XpmFreeAttributes(&xpmAttr);
    ok = TRUE;
    x_pixmap = ximage->bitmap;	// Actually a GWorldPtr!
	XImageFree(ximage);			// does not delete the GWorld
  } else {
		// XpmDebugError(ErrorStatus, NULL);
		ok = False;
  }
}
#endif

/*
	if USE_XPM_IN_MAC and USE_IMAGE_LOADING_IN_MAC are not defined in wx_setup.h
	then the only thing we can load is a PICT file.
	USE_XPM_IN_MAC requires that the flags arg has the wxBITMAP_TYPE_XPM bit set.
	USE_IMAGE_LOADING... does NOT look at the flags, instead it looks at file
	extensions. This deserves a proper cleanup.
*/
Bool wxBitmap::LoadFile(char *name, long flags)
{
	if (selectedIntoDC) return FALSE;
	
	if (x_pixmap) {
		DisposeGWorld(x_pixmap);
		x_pixmap = NULL;
	}
	wxColourMap *colourmap;
	ok = FALSE;

#if USE_XPM_IN_MAC
	if (flags & wxBITMAP_TYPE_XPM) {
		XImage	*ximage;
		XpmAttributes xpmAttr;
		
		xpmAttr.valuemask = XpmReturnInfos;	/* nothing yet, but get infos back */
    	int ErrorStatus = XpmReadFileToImage(NULL,	// don't have a Display dpy
				   name,
				   &ximage,							// we get this back
				   NULL,							// don't want a shapemask
				   &xpmAttr);						// where to put the attributes

		if (ErrorStatus == XpmSuccess) {
			// Set attributes
			width=xpmAttr.width;
			height = xpmAttr.height;
			depth = ximage->depth;
			XpmFreeAttributes(&xpmAttr);
			ok = TRUE;
			x_pixmap = ximage->bitmap;	// Actually a GWorldPtr!
			XImageFree(ximage);			// does not delete the GWorld
		}
		return ok;
	}
#endif
#if USE_IMAGE_LOADING_IN_MAC
	if (flags & wxBITMAP_TYPE_GIF) {
		ok = wxLoadGifIntoBitmap(name, this, &colourmap);
	} else if (flags & wxBITMAP_TYPE_PICT) {
		ok = wxLoadPICTIntoBitmap(name, this, &colourmap);
	} else if (flags & wxBITMAP_TYPE_XBM) {
		ok = wxLoadXBMIntoBitmap(name, this, &colourmap);
	} else if (flags & wxBITMAP_TYPE_BMP) {
		ok = wxLoadBMPIntoBitmap(name, this, &colourmap);
	} else if (flags & wxBITMAP_TYPE_ANY) {
    	ok = wxLoadIntoBitmap(name,this, &colourmap);
	} else {
		ok = FALSE;
	}
#else
	FILE *fp = fopen(name,"rb");
	if (fp) {
		// I don't know why we skip 512 bytes. I would have
		// thought fopen only processes the data fork. I suppose
		// it could be the "Mac Draw" header block (IM-V, pg 88)
		fseek(fp, 0, SEEK_END);
		int fsize = ftell(fp) - 512;
		fseek(fp, 512, SEEK_SET);	// 0 didn't work
		PicHandle ph = (PicHandle)NewHandle(fsize);
		CheckMemOK(ph);
		int rsize = fread((char *)*ph, 1, fsize, fp);
		width = (*ph)->picFrame.right;
		height = (*ph)->picFrame.bottom;
		depth = wxDisplayDepth();
		GDHandle savegd;
		CGrafPtr saveport;
		GetGWorld(&saveport, &savegd);
		QDErr err;
		GWorldPtr	newGWorld;
		Rect	bounds = {0, 0, height, width};
		err = NewGWorld(&x_pixmap, 0, &bounds, NULL, NULL, noNewDevice);
		if (!err) {
		  SetGWorld(x_pixmap, 0);
		  DrawPicture(ph, &bounds);
		  DisposeHandle((Handle)ph);
		  SetGWorld(saveport, savegd);
		  ok = TRUE;
		} else {
		  ok = FALSE;
        }
	} else
		ok = FALSE;
#endif
	return ok;
}

Bool wxBitmap::SaveFile(char *name, int type, wxColourMap *cmap)
{
	Bool ok = FALSE;

	if (type & wxBITMAP_TYPE_XBM) {
		ok = wxSaveXBMFromBitmap(name, this, NULL);
	} else if (type & wxBITMAP_TYPE_XPM) {
	  if (!Ok()) return FALSE;
	
	  XImage ximage;
	  
	  GDHandle savegw;
	  CGrafPtr saveport;
	  GetGWorld(&saveport, &savegw);
	
	  SetGWorld(x_pixmap, 0);
	
	  ximage.width = GetWidth(); 
	  ximage.height = GetHeight();
      ximage.depth = GetDepth(); 
      ximage.bitmap = NULL;
      
      int errorStatus = XpmWriteFileFromImage(NULL, name,
                                              &ximage, (XImage *)NULL, 
                                              (XpmAttributes *)NULL);

	  SetGWorld(saveport, savegw);

      ok = (errorStatus == XpmSuccess);
	}
	
	return ok;
}

void wxBitmap::SetColourMap(wxColourMap *cmap)
{
}

wxColourMap* wxBitmap::GetColourMap(void)
{
	return NULL;
}

//  --------------- Some Mac extensions ---- should only be used inside
//		wxwindows, like from wx_button, wxDC, wxMemoryDC, etc.
//		we ASSUME that SetMacDC() is set to the proper destination port.
void wxBitmap::DrawMac(void)
{
	DrawMac(0, 0);
}

void wxBitmap::DrawMac(int x, int y)
{
	if (x_pixmap) {
		Rect sbounds = {0, 0, height, width};
		Rect dbounds = {y, x, height+y, width+x};
		PixMapHandle  srcpixh = ::GetGWorldPixMap(x_pixmap);
		CGrafPtr here;
		::GetPort( (GrafPtr *)&here);
		PixMapHandle destpixh = here->portPixMap;
		::CopyBits( (BitMap *) (*srcpixh), (BitMap *) (*destpixh),
			&sbounds, &dbounds, srcCopy, NULL);
	}
}
