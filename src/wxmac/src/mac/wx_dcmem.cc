///////////////////////////////////////////////////////////////////////////////
// File:	wx_dcmem.cc
// Purpose:	Memory device context implementation (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

static const char sccsid[] = "%W% %G%";

#include <iostream.h>
#include <fstream.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <Quickdraw.h>
#include "wx_dcmem.h"
#include "wx_utils.h"
#include "wx_canvs.h"
#include "wx_privt.h"

/* 
	A wxMemoryDC is a pointer to a bitmap, which is an offscreen GWorld. 

	When the wxMemoryDC(void) constructor is used we don't know how large a pixmap
	(boundsRect) to create so we wait until SelectObject() is called. 
   mflatt:
    This is unlike X-Windows and Windows! These platforms allocate a bitmap
     anyway of some size. Since the manual says to select a bitmap first, I
     see no problem with this Mac implementation.
*/
extern CGrafPtr wxMainColormap;

wxMemoryDC::wxMemoryDC(void)
{
  __type = wxTYPE_DC_MEMORY;
  device = wxDEVICE_PIXMAP;
  current_pen_join = -1 ;
  current_pen_cap = -1 ;
  current_pen_nb_dash = -1 ;
  current_pen_dash = NULL ;
  current_stipple = NULL ;

  pixmap = 0;
  pixmapWidth = 0;
  pixmapHeight = 0;
  canvas = NULL;
  clipping = FALSE;
  
  ok = FALSE;
  title = NULL;

  current_logical_function = -1;
  font = wxNORMAL_FONT;
  min_x = 0; min_y = 0; max_x = 0; max_y = 0;
  logical_origin_x = 0;
  logical_origin_y = 0;

  device_origin_x = 0;
  device_origin_y = 0;

  logical_scale_x = 1.0;
  logical_scale_y = 1.0;

  user_scale_x = 1.0;
  user_scale_y = 1.0;

  mapping_mode = MM_TEXT;

  current_pen = NULL;
  current_brush = NULL;
  current_background_brush = wxWHITE_BRUSH;
  current_text_foreground = *wxBLACK;
//  current_text_background = NULL;

  // mflatt: NOT ok
  // ok = TRUE;
  selected_pixmap = NULL;
  gworldH = NULL;

  Colour = wxColourDisplay();
  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);
}

/*
 * Create a new dc from an old dc
 *
 */

wxMemoryDC::wxMemoryDC(wxCanvasDC *old_dc):wxbMemoryDC(old_dc)
{
  min_x = 0; min_y = 0; max_x = 0; max_y = 0;
  __type = wxTYPE_DC_MEMORY;
  device = wxDEVICE_PIXMAP;
  current_pen_join = -1 ;
  current_pen_cap = -1 ;
  current_pen_nb_dash = -1 ;
  current_pen_dash = NULL ;
  current_stipple = NULL ;

  pixmap = 0;
  pixmapWidth = 0;
  pixmapHeight = 0;
  canvas = NULL;

  ok = FALSE;
  title = NULL;

  current_logical_function = -1;
  font = wxNORMAL_FONT;
  logical_origin_x = 0;
  logical_origin_y = 0;

  device_origin_x = 0;
  device_origin_y = 0;

  logical_scale_x = 1.0;
  logical_scale_y = 1.0;

  user_scale_x = 1.0;
  user_scale_y = 1.0;

  mapping_mode = MM_TEXT;

  current_pen = NULL;
  current_brush = NULL;
  current_background_brush = wxWHITE_BRUSH;
  current_text_foreground = *wxBLACK;
//  current_text_background = NULL;

  // mflatt: NOT ok
  // ok = TRUE;
  selected_pixmap = NULL;
  gworldH = NULL;
  Colour = wxColourDisplay();
  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);
}

wxMemoryDC::~wxMemoryDC(void)
{
  if (selected_pixmap) {
	selected_pixmap->selectedInto = NULL;
	selected_pixmap->selectedIntoDC = 0;
	gworldH = NULL;
  } else {
    if (gworldH) {
	  ::DisposeGWorld(gworldH);
	  gworldH = NULL;
    }
 }
 
 if (cMacDC) {
	delete cMacDC;
	cMacDC = NULL;
  }
}

void wxMemoryDC::SelectObject(wxBitmap *bitmap)
{
  if (selected_pixmap == bitmap) {
		// set cMacDC ??
		return;
  }
  if (bitmap && bitmap->selectedIntoDC)
	// This bitmap is selected into a different memoryDC
    return;

  if (selected_pixmap) {
	selected_pixmap->selectedInto = NULL;
	selected_pixmap->selectedIntoDC = 0;
	gworldH = NULL;
  } else {
    if (gworldH) {
  	  ::DisposeGWorld(gworldH);
      gworldH = NULL;
    }
  }

  if (cMacDC) {
	delete cMacDC;
	cMacDC = NULL;
  }
  ok = FALSE;
  selected_pixmap = bitmap;
  if (bitmap == NULL) {	// deselect a bitmap
    pixmapWidth = 0;
    pixmapHeight = 0;
	pixmap = NULL;
	return;
  }
  bitmap->selectedInto = this;
  bitmap->selectedIntoDC = -1;
  pixmapWidth = bitmap->GetWidth();
  pixmapHeight = bitmap->GetHeight();
  if (bitmap->Ok()) {
    gworldH = bitmap->x_pixmap;
    // gworldH = MacCreateGWorld(pixmapWidth, pixmapHeight);
    if (gworldH) {
	  pixmap = ::GetGWorldPixMap(gworldH);
	
	  SetGWorld(gworldH, 0);
	  cMacDC = new wxMacDC(gworldH);
	  // bitmap->DrawMac(0, 0);
	  ok = TRUE;
    }
  }
}

GWorldPtr wxMemoryDC::MacCreateGWorld(int width, int height)
{
	QDErr err;
	GWorldPtr	newGWorld;
	Rect	bounds = {0, 0, height, width};

	err = NewGWorld(&newGWorld, 0, &bounds, NULL, NULL, noNewDevice);
	if (err == noErr)
		return newGWorld;
	else
		return NULL;
}

