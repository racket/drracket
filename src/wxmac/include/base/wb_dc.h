/*
 * File:	wb_dc.h
 * Purpose:	wxDC device context declaration
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_dc.h	1.2 5/9/94" */


#ifndef wxb_dch
#define wxb_dch

#ifdef __GNUG__
#pragma interface
#endif

#ifndef IN_CPROTO
#include <fstream.h>
#endif
#include "common.h"
#include "wx_frame.h"
#include "wx_gdi.h"

#define wxDEVICE_CANVAS  1
                            // X canvas
#define wxDEVICE_EPS     2
                            // Encapsulated PostScript on any platform
#define wxDEVICE_WINDOWS 3
                            // MS Windows device (canvas, printer)
#define wxDEVICE_PIXMAP  4
                            // X pixmap

#if defined(wx_x) || defined(wx_mac)
#define MM_TEXT        1
#define MM_ISOTROPIC   2
#define MM_ANISOTROPIC 3
#define MM_LOMETRIC    4
#define MM_HIMETRIC    5
#define MM_TWIPS       6
#endif

#define MM_POINTS      7
#define MM_METRIC      8

#ifdef IN_CPROTO
typedef       void    *wxbDC ;
#else

class wxCanvas;
class wxCanvasDC;
class wxDC;
class wxbDC: public wxObject
{
 public:
  int device;
  Bool ok;
  Bool clipping;
  Bool wx_interactive;

  // Coordinate system variables
  float logical_origin_x;
  float logical_origin_y;

  float device_origin_x;
  float device_origin_y;

  float logical_scale_x;
  float logical_scale_y;

  float user_scale_x;
  float user_scale_y;

  int mapping_mode;

  float min_x;          // bounding box
  float min_y;
  float max_x;
  float max_y;
  char *title;

  Bool Colour;

  int current_logical_function;
  int current_bk_mode;

  wxPen *current_pen;
  wxBrush *current_brush;
  wxBrush *current_background_brush;
  wxColour current_text_foreground;
  wxColour current_text_background;
  wxFont *font;
  Bool autoSetting ;

  wxbDC(void);

  // Create a printer DC
//  wxDC(char *driver, char *device, char *output, Bool interactive = TRUE);

  ~wxbDC(void);

  //
  // This function is intended to improves drawing, by avoiding to
  // repeatly call ::SetPen/::SetBrush. If set to FALSE, these functions
  // aren't called when calling ::DrawLine(),...
  // Please note that this is YOUR responsability to use it, and do it
  // only when you KNOWN that pen/brush isn't changed between 2 calls to
  // DrawLine,... !!!
  // Note also that in X, we don't test autoSetting on brushes, because they
  // modify Foreground, as pens. So, convention is:
  //   - call your SetBrush(), THEN your SetPen, THEN AutoSetTools(FALSE)
  //   - call DrawLine,...
  // [mainly coded for Windows]
  inline virtual void AutoSetTools(Bool auto_setting) { autoSetting = auto_setting ; }
 
  inline virtual void BeginDrawing(void) {} ;
  inline virtual void EndDrawing(void) {} ;

  virtual void FloodFill(float x1, float y1, wxColour *col, int style=wxFLOOD_SURFACE) = 0;
  virtual Bool GetPixel(float x1, float y1, wxColour *col) = 0;

  virtual void DrawLine(float x1, float y1, float x2, float y2) = 0;
  virtual void IntDrawLine(int x1, int y1, int x2, int y2) = 0;
  virtual void CrossHair(float x, float y) = 0;
  virtual void DrawArc(float x1,float y1,float x2,float y2,float xc,float yc)=0;
  virtual void DrawPoint(float x, float y) = 0;
  inline virtual void DrawPoint(wxPoint& point) { DrawPoint(point.x, point.y); }
  virtual void DrawLines(int n, wxPoint points[], float xoffset = 0, float yoffset = 0) = 0;
  virtual void DrawLines(int n, wxIntPoint points[], int xoffset = 0, int yoffset = 0) = 0;
  virtual void DrawLines(wxList *list, float xoffset = 0, float yoffset = 0);
  virtual void DrawPolygon(int n, wxPoint points[], float xoffset = 0, float yoffset = 0, int fillStyle=wxODDEVEN_RULE) = 0;
  virtual void DrawPolygon(wxList *list, float xoffset = 0, float yoffset = 0, int fillStyle=wxODDEVEN_RULE);
  virtual void DrawRectangle(float x, float y, float width, float height) = 0;
  virtual void DrawRoundedRectangle(float x, float y, float width, float height, float radius = 20) = 0;
  virtual void DrawEllipse(float x, float y, float width, float height) = 0;
#if USE_SPLINES
  // Splines
  // 3-point spline
  virtual void DrawSpline(float x1, float y1, float x2, float y2, float x3, float y3);
  // Any number of control points - a list of pointers to wxPoints
  virtual void DrawSpline(wxList *points);
  virtual void DrawSpline(int n, wxPoint points[]);
#endif
  virtual void DrawIcon(wxIcon *icon, float x, float y) = 0;
  virtual void DrawText(const char *text, float x, float y, Bool use16 = FALSE) = 0;
  virtual void Clear(void) = 0;

  virtual Bool StartDoc(char *message) = 0;
  virtual void EndDoc(void) = 0;
  virtual void StartPage(void) = 0;
  virtual void EndPage(void) = 0;

  virtual void SetFont(wxFont *font) = 0;
  virtual void SetPen(wxPen *pen) = 0;
  virtual void SetBrush(wxBrush *brush) = 0;
  virtual void SetLogicalFunction(int function) = 0;
  virtual void SetBackground(wxBrush *brush) = 0;
  virtual void SetTextForeground(wxColour *colour);
  virtual void SetTextBackground(wxColour *colour);
  virtual void SetBackgroundMode(int mode); // wxSOLID or wxTRANSPARENT
                                            // for drawing background colour
  virtual void SetClippingRegion(float x, float y, float width, float height)= 0;
  virtual void GetClippingRegion(float *x, float *y, float *width, float *height)= 0; // mflatt
  inline virtual void SetColourMap(wxColourMap *cmap) {};
  virtual void DestroyClippingRegion(void) = 0;

  virtual float GetCharHeight(void) = 0;
  virtual float GetCharWidth(void) = 0;
  virtual void GetTextExtent(const char* string, float* x, float* y, float* descent = NULL,
  						float* externalLeading = NULL, wxFont* the_font = NULL, Bool use16=FALSE) = 0;
  inline virtual Bool Ok(void) {return ok;};
  virtual void SetMapMode(int mode) = 0;
  inline virtual int  GetMapMode(void) {return mapping_mode;};

  // The following methods provide a cleaner interface
  inline virtual wxBrush *GetBackground(void)      { return current_background_brush ;}
  inline virtual wxBrush *GetBrush(void)           { return current_brush ;}
  inline virtual wxFont  *GetFont(void)            { return font ;}
  inline virtual int      GetLogicalFunction(void) { return current_logical_function ;}
  inline virtual wxPen   *GetPen(void)             { return current_pen ;}
  inline virtual wxColour&GetTextBackground(void)  { return current_text_background ;}
  inline virtual wxColour&GetTextForeground(void)  { return current_text_background ;}
 
  virtual void SetLogicalOrigin(float x, float y);
  virtual void SetDeviceOrigin(float x, float y);
  virtual void SetLogicalScale(float x, float y);
  virtual void SetUserScale(float x, float y) = 0;
  virtual float DeviceToLogicalX(int x) = 0;
  virtual float DeviceToLogicalY(int y) = 0;
  virtual float DeviceToLogicalXRel(int x) = 0;
  virtual float DeviceToLogicalYRel(int y) = 0;
  virtual int LogicalToDeviceX(float x) = 0;
  virtual int LogicalToDeviceY(float y) = 0;
  virtual int LogicalToDeviceXRel(float x) = 0;
  virtual int LogicalToDeviceYRel(float y) = 0;
  // Only works for PostScript *after* you've printed an image.
  // Gives width and height of image.
  virtual void GetSize(float *width, float *height);
  virtual void CalcBoundingBox(float x, float y);
  // Get the final bounding box of the PostScript or Metafile picture.
  virtual inline float MinX(void) { return min_x; }
  virtual inline float MaxX(void) { return max_x; }
  virtual inline float MinY(void) { return min_y; }
  virtual inline float MaxY(void) { return max_y; }
  virtual Bool Blit(float xdest, float ydest, float width, float height,
            wxCanvasDC *source, float xsrc, float ysrc, int rop = wxCOPY) = 0;
            
    virtual void TryColour(wxColour *src, wxColour *dest);
};

extern char wx_printer_file[];
extern float wx_printer_scale_x;
extern float wx_printer_scale_y;
extern float wx_printer_translate_x;
extern float wx_printer_translate_y;
extern int wxPageNumber;

// Conversion
#define METRIC_CONVERSION_CONSTANT  0.0393700787

// Scaling factors for various unit conversions
#define mm2inches (METRIC_CONVERSION_CONSTANT)
#define inches2mm (1/METRIC_CONVERSION_CONSTANT)

#define mm2twips (METRIC_CONVERSION_CONSTANT*1440)
#define twips2mm (1/(METRIC_CONVERSION_CONSTANT*1440))

#define mm2pt (METRIC_CONVERSION_CONSTANT*72)
#define pt2mm (1/(METRIC_CONVERSION_CONSTANT*72))

#define     wx_round(a)    (int)((a)+.5)


#endif // IN_CPROTO
#endif // wxb_dch
