///////////////////////////////////////////////////////////////////////////////
// File:	wx_dc.h
// Purpose:	wxDC device context declaration (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wx_dch
#define wx_dch

#include "wb_dc.h"
#include "wx_gdi.h"
#include "wxMacDC.h"

#ifdef IN_CPROTO
typedef       void    *wxDC ;
#else

class wxDC: public wxbDC
{
 public:
	wxMacDC* cMacDC; // mac platform only
	Pattern cMacPattern; // mac platform only (temp work pattern)

	enum wxMacToolType {kNoTool, kPenTool, kBrushTool, kQuillTool, kBlitTool, kColorBlitTool}; // mac platform only
	wxMacToolType cMacCurrentTool; // mac platform only

 public:
	wxDC(void);
	~wxDC(void);

// Mac platform only
	void wxMacDrawPoint(int x1, int y1);
	void wxMacDrawLine(int x1, int y1, int x2, int y2);
};

/*
 * X
 * Ok, so all this isn't neat, but it about works.
 */

#include <math.h>

// Logical to device
// Absolute
#define XLOG2DEV(x) (int)floor(((x) - logical_origin_x)*logical_scale_x*user_scale_x + device_origin_x + 0.5)
#define YLOG2DEV(y) (int)floor(((y) - logical_origin_y)*logical_scale_y*user_scale_y + device_origin_y + 0.5)

// Logical to device without the device translation
#define XLOG2DEV_2(x) (int)floor(((x) - logical_origin_x)*logical_scale_x*user_scale_x + 0.5)
#define YLOG2DEV_2(y) (int)floor(((y) - logical_origin_y)*logical_scale_y*user_scale_y + 0.5)

// Relative
#define XLOG2DEVREL(x) (int)floor((x)*logical_scale_x*user_scale_x + 0.5)
#define YLOG2DEVREL(y) (int)floor((y)*logical_scale_y*user_scale_y + 0.5)

// Device to logical
// Absolute
#define XDEV2LOG(x) (int)(((x) - device_origin_x)/(logical_scale_x*user_scale_x) + logical_origin_x)

#define YDEV2LOG(y) (int)(((y) - device_origin_y)/(logical_scale_y*user_scale_y) + logical_origin_y)

// Relative
#define XDEV2LOGREL(x) (int)((x)/(logical_scale_x*user_scale_x))
#define YDEV2LOGREL(y) (int)((y)/(logical_scale_y*user_scale_y))

#endif // IN_CPROTO
#endif // wx_dch
