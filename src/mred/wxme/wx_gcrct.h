
#if !WX_STANDARD_GRAPHICS

/* Corrections when not using standard graphics: */

#ifdef wx_x
#define GC_RECT_FRAME_EXTEND (-1)
#endif

#ifdef wx_msw
#define GC_LINE_EXTEND 1
#define GC_RECT_BRUSH_EXTEND 1
#endif

#endif




#ifndef GC_LINE_EXTEND
#define GC_LINE_EXTEND 0
#endif

#ifndef GC_RECT_BRUSH_EXTEND
#define GC_RECT_BRUSH_EXTEND 0
#endif

#ifndef GC_RECT_FRAME_EXTEND
#define GC_RECT_FRAME_EXTEND 0
#endif
