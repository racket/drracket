/*
 * File:	common.h
 * Purpose:	Declarations/definitions common to all wx source files
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)common.h	1.2 5/9/94" */

#ifndef wxb_commonh
#define wxb_commonh

#include <stddef.h>
#include <string.h>
#include "wx_setup.h"
#include "wx_ver.h"

//////////////////////////////////////////////////////////////////////////////////
// Currently Only MS-Windows/NT, XView and Motif are supported
//
#if defined(wx_hp) && !defined(wx_motif) && !defined(wx_xview)
# define wx_motif
#endif
#if defined(wx_xview) || defined(wx_motif)
# define wx_x
#elif defined(macintosh) || defined(applec) || defined(__MWERKS__)
	// the Mac test must come before the MS-Windows test because __WINDOWS__
	// is defined whenever the Apple <Windows.h> file is included
#	ifndef wx_mac
#		define wx_mac
#	endif
#	ifndef PYLIB
#		ifndef GUSI
#			define GUSI
#		endif
#	endif
#elif defined(__WINDOWS__) || defined(__WINDOWS_386__) || defined(__NT__) || defined(__MSDOS__) 
# ifndef wx_msw
#  define wx_msw
# endif
#endif

// Make sure the environment is set correctly
#if defined(wx_msw) && defined(wx_x)
# error "Target can't be both X and Windows"
#elif defined(wx_xview) && defined(wx_motif)
# error "Target can't be both XView and Motif!"
#elif !defined(wx_xview) && !defined(wx_motif) && !defined(wx_msw) && !defined(wx_mac)
# error "No Target! Use -D[wx_motif|wx_xview|wx_msw|wx_mac]"
#endif

#ifdef wx_motif
  typedef int Bool;
# define TRUE  1
# define FALSE 0
# define Bool_DEFINED
#endif
#if defined(wx_xview)
# define Bool int
# define True  1
# define False 0
# define TRUE  1
# define FALSE 0
# define Bool_DEFINED
#endif
#if defined(wx_msw)
# include <windows.h>
# ifndef Bool
   typedef int Bool;
#  define Bool_DEFINED
# endif
#endif
#ifdef wx_mac
typedef int Bool;
#define Bool_DEFINED
#endif

#ifndef TRUE
# define TRUE  1
# define FALSE 0
#endif

// wxWindows checks for WIN32, not __WIN32__
#if (defined(__WIN32__) && !defined(WIN32))
#define WIN32
#endif

typedef short int WXTYPE;

// Styles for wxListBox - Yes, all in Multiple , and nothing in Style
#define wxMULTIPLE_MASK     0x03
#define wxSINGLE            0x00
#define wxMULTIPLE          0x01
#define wxEXTENDED          0x02

#define wxSB_MASK           0x08
#define wxNEEDED_SB         0x00
#define wxALWAYS_SB         0x08

// Frame/dialog/subwindow style flags
#define wxVSCROLL           0x00000001
#define wxHSCROLL           0x00000002
#define wxCAPTION           0x00000004
// Hint to Windowing system not to try anything clever: ***OBSOLETE***
#define wxABSOLUTE_POSITIONING  8

// Frame/dialog style flags
#define wxSTAY_ON_TOP       0x00000008
#define wxICONIZE           0x00000010
#define wxMINIMIZE          wxICONIZE
#define wxMAXIMIZE          0x00000020
#define wxSDI               0x00000040
#define wxMDI_PARENT        0x00000080
#define wxMDI_CHILD         0x00000100
#define wxTHICK_FRAME       0x00000200
#define wxSYSTEM_MENU       0x00000400
#define wxMINIMIZE_BOX      0x00000800
#define wxMAXIMIZE_BOX      0x00001000
#define wxTINY_CAPTION_HORIZ 0x00002000
#define wxTINY_CAPTION_VERT 0x00004000
#define wxRESIZE_BOX       wxMAXIMIZE_BOX
#define wxRESIZE_BORDER	    0x00800000

#define wxDEFAULT_FRAME    (wxRESIZE_BORDER | wxMINIMIZE_BOX | wxMAXIMIZE_BOX | wxTHICK_FRAME | wxSYSTEM_MENU | wxCAPTION)

// Subwindow style flags
#define wxBORDER           0x00000040
#define wxRETAINED         0x00000080
#define wxEDITABLE         0x00000200
#define wxREADONLY         0x00000400

// wxText style flags
#define wxPROCESS_ENTER    0x00001000
#define wxPASSWORD         0x00002000

// Use native implementation, e.g. Text EDIT control for wxTextWindow
// under MSW
#define wxNATIVE_IMPL      0x01000000
// Extended (or simply alternative) implementation, e.g. large
// but not editable wxTextWindow under Windows
#define wxEXTENDED_IMPL    0x02000000
// Override CTL3D etc. control colour processing to
// allow own background colour
#define wxUSER_COLOURS     0x04000000
#define wxVERTICAL_LABEL   0x08000000

#ifndef wx_motif
# define wxFLAT            wxBORDER
# define wxBACKINGSTORE    0x00000000
# define wxMOTIF_RESIZE    0x00000000
#else
# define wxFLAT            0x00000100
# define wxBACKINGSTORE    0x00004000
# define wxMOTIF_RESIZE    0x01000000
#endif

// Effect of this flags: when creating wxItem with labels and/or value,
// say new wxText(...,"label",...,"init_value"), the item is created with
// strings containing only '0' ("00000" and "0000000000" in the exemple),
// then SetLabel/SetValue are called. This make alignement more easy:
//
// LabelPosition(wxHORIZONTAL)
// new wxText("label    ","initval1")
// NewLine()
// new wxText("longlabel","initval2")
//
// the 2 texts are EXACTLY aligned...
//
// Please note that:
//   - I choose '0' as constant character, because it has a mean width.
//   - This style is useful only if LabelPosition is wxHorizontal...
#define wxFIXED_LENGTH          0x00020000

// Enhanced Dialog styles
// Command area placment
#define wxBOTTOM_COMMANDS       0x00000000
#define wxRIGHT_COMMANDS        0x00040000
#define wxMASK_COMMANDS         0x00040000
// Status Area
#define wxSTATUS_FOOTER         0x00000000
#define wxNO_STATUS_FOOTER      0x00080000
#define wxMASK_STATUS           0x00080000
// Cancel Button/Pushpin Emulation
#define wxNO_CANCEL_BUTTON      0x00000000
#define wxCANCEL_BUTTON_FIRST   0x00100000
#define wxCANCEL_BUTTON_LAST    0x00200000
#define wxCANCEL_BUTTON_SECOND  0x00300000
#define wxMASK_CANCEL           0x00300000

#if MOTIF_MANAGE && defined(wx_motif)
#define wxDEFAULT_DIALOG_STYLE	(wxMOTIF_RESIZE|wxSYSTEM_MENU|wxCAPTION|wxTHICK_FRAME)
#else
#define wxDEFAULT_DIALOG_STYLE	(wxSYSTEM_MENU|wxCAPTION|wxTHICK_FRAME)
#endif

#ifdef wx_motif
#define       wxENH_DEFAULT   (wxCAPTION|wxMOTIF_RESIZE|wxBOTTOM_COMMANDS|wxSTATUS_FOOTER|wxNO_CANCEL_BUTTON)
#elif defined(wx_xview)
# define wxENH_DEFAULT   (wxBOTTOM_COMMANDS|wxSTATUS_FOOTER|wxNO_CANCEL_BUTTON)
#elif defined(wx_msw)
# define wxENH_DEFAULT   (wxRIGHT_COMMANDS|wxSTATUS_FOOTER|wxCANCEL_BUTTON_SECOND)
#elif defined(wx_mac)
# define wxENH_DEFAULT 0L
#else
#error "Only Motif, XView and MS-Windows/Windows-NT platforms are currently supported"
#endif
#define wxCOLOURED             0x00400000


// GDI descriptions

enum {
// Text font families
  wxDEFAULT    = 70,
  wxDECORATIVE,
  wxROMAN,
  wxSCRIPT,
  wxSWISS,
  wxMODERN,
  wxTELETYPE,  /* @@@@ */
  wxSYSTEM,

// Proportional or Fixed width fonts (not yet used)
  wxVARIABLE   = 80,
  wxFIXED,

  wxNORMAL     = 90,
  wxLIGHT,
  wxBOLD,
// Also wxNORMAL for normal (non-italic text)
  wxITALIC,
  wxSLANT,

// Pen styles
  wxSOLID      =   100,
  wxDOT,
  wxLONG_DASH,
  wxSHORT_DASH,
  wxDOT_DASH,
  wxUSER_DASH,

  wxTRANSPARENT,

// Brush & Pen Stippling. Note that a stippled pen cannot be dashed!!
// Note also that stippling a Pen IS meaningfull, because a Line is
// drawn with a Pen, and without any Brush -- and it can be stippled.
  wxSTIPPLE =          110,
  wxBDIAGONAL_HATCH,
  wxCROSSDIAG_HATCH,
  wxFDIAGONAL_HATCH,
  wxCROSS_HATCH,
  wxHORIZONTAL_HATCH,
  wxVERTICAL_HATCH,
#define IS_HATCH(s)	((s)>=wxBDIAGONAL_HATCH && (s)<=wxVERTICAL_HATCH)
  wxOPAQUE_STIPPLE,

  wxJOIN_BEVEL =     120,
  wxJOIN_MITER,
  wxJOIN_ROUND,

  wxCAP_ROUND =      130,
  wxCAP_PROJECTING,
  wxCAP_BUTT
};


// Logical ops
typedef enum {
  wxCLEAR,      // 0
  wxXOR,        // src XOR dst
  wxINVERT,     // NOT dst
  wxOR_REVERSE, // src OR (NOT dst)
  wxAND_REVERSE,// src AND (NOT dst)
  wxCOPY,       // src
  wxAND,        // src AND dst
  wxAND_INVERT, // (NOT src) AND dst
  wxNO_OP,      // dst
  wxNOR,        // (NOT src) AND (NOT dst)
  wxEQUIV,      // (NOT src) XOR dst
  wxSRC_INVERT, // (NOT src)
  wxOR_INVERT,  // (NOT src) OR dst
  wxNAND,       // (NOT src) OR (NOT dst)
  wxOR,         // src OR dst
  wxSET,        // 1
  wxSRC_OR,     // source _bitmap_ OR destination
  wxSRC_AND,     // source _bitmap_ AND destination
  wxCOLOR
} form_ops_t;

// Flood styles
#define  wxFLOOD_SURFACE   1
#define  wxFLOOD_BORDER    2

// Polygon filling mode
#define  wxODDEVEN_RULE    1
#define  wxWINDING_RULE    2

// Directions
#define wxHORIZONTAL     0x01
#define wxVERTICAL       0x02
#define wxBOTH           (wxVERTICAL|wxHORIZONTAL)
#define wxCENTER_FRAME   0x04  /* centering into frame rather than screen */

// ToolPanel in wxFrame
#define	wxTOOL_TOP	   1
#define	wxTOOL_BOTTOM	   2
#define	wxTOOL_LEFT	   3
#define	wxTOOL_RIGHT	   4

// Dialog specifiers/return values
// Unfortunately const's cause too many 'defined but not used'
// in GCC.
// messages. So we're returning to defines for now.
/*
const wxOK =                0x0001;
const wxYES_NO =            0x0002;
const wxCANCEL =            0x0004;
const wxYES =               0x0008;
const wxNO =                0x0010;

const wxICON_EXCLAMATION =  0x0020;
const wxICON_HAND =         0x0040;
const wxICON_QUESTION =     0x0080;
const wxICON_INFORMATION =  0x0100;
*/

#define wxOK                0x0001
#define wxYES_NO            0x0002
#define wxCANCEL            0x0004
#define wxYES               0x0008
#define wxNO                0x0010

#define wxICON_EXCLAMATION  0x0020
#define wxICON_HAND         0x0040
#define wxICON_QUESTION     0x0080
#define wxICON_INFORMATION  0x0100

#define wxICON_STOP         wxICON_HAND
#define wxICON_ASTERISK     wxICON_INFORMATION
#define wxICON_MASK         (0x0020|0x0040|0x0080|0x0100)

#define wxCENTRE            0x0200
#define wxCENTER wxCENTRE

// Use internally-calculated width if -1
#define wxSIZE_AUTO_WIDTH       1
// Use internally-calculated height if -1
#define wxSIZE_AUTO_HEIGHT      2
// Use internally-calculated width and height if each is -1
#define wxSIZE_AUTO             3
// Use internally-calculated x if -1

// -1 as x/y position really means -1; don't use default
#define wxPOS_USE_MINUS_ONE 4

// Ignore missing (-1) dimensions (use existing).
// For readability only: test for wxSIZE_AUTO_WIDTH/HEIGHT in code.
#define wxSIZE_USE_EXISTING     0

// Clipboard formats
#ifdef wx_msw
# define wxCF_TEXT               CF_TEXT
# define wxCF_BITMAP             CF_BITMAP
# define wxCF_METAFILE           CF_METAFILEPICT
# define wxCF_DIB                CF_DIB
# define wxCF_OEMTEXT            CF_OEMTEXT
#else
# define wxCF_TEXT               1
# define wxCF_BITMAP             2
# define wxCF_METAFILE           3
# define wxCF_DIB                4
# define wxCF_OEMTEXT            5
#endif

// Virtual keycodes
enum _Virtual_keycodes {
 WXK_BACK    =   8,
 WXK_TAB     =   9,
 WXK_RETURN  =	13,
 WXK_ESCAPE  =	27,
 WXK_SPACE   =	32,
 WXK_DELETE  = 127,

 WXK_START   = 300,
 WXK_LBUTTON,
 WXK_RBUTTON,
 WXK_CANCEL,
 WXK_MBUTTON,
 WXK_CLEAR,
 WXK_SHIFT,
 WXK_CONTROL,
 WXK_MENU,
 WXK_PAUSE,
 WXK_CAPITAL,
 WXK_PRIOR,
 WXK_NEXT,
 WXK_END,
 WXK_HOME,
 WXK_LEFT,
 WXK_UP,
 WXK_RIGHT,
 WXK_DOWN,
 WXK_SELECT,
 WXK_PRINT,
 WXK_EXECUTE,
 WXK_SNAPSHOT,
 WXK_INSERT,
 WXK_HELP,
 WXK_NUMPAD0,
 WXK_NUMPAD1,
 WXK_NUMPAD2,
 WXK_NUMPAD3,
 WXK_NUMPAD4,
 WXK_NUMPAD5,
 WXK_NUMPAD6,
 WXK_NUMPAD7,
 WXK_NUMPAD8,
 WXK_NUMPAD9,
 WXK_MULTIPLY,
 WXK_ADD,
 WXK_SEPARATOR,
 WXK_SUBTRACT,
 WXK_DECIMAL,
 WXK_DIVIDE,
 WXK_F1,
 WXK_F2,
 WXK_F3,
 WXK_F4,
 WXK_F5,
 WXK_F6,
 WXK_F7,
 WXK_F8,
 WXK_F9,
 WXK_F10,
 WXK_F11,
 WXK_F12,
 WXK_F13,
 WXK_F14,
 WXK_F15,
 WXK_F16,
 WXK_F17,
 WXK_F18,
 WXK_F19,
 WXK_F20,
 WXK_F21,
 WXK_F22,
 WXK_F23,
 WXK_F24,
 WXK_NUMLOCK,
 WXK_SCROLL,
 WXK_PAGEUP,
 WXK_PAGEDOWN 
};

// Colours - see wx_gdi.cc for database

// OS mnemonics -- Identify the running OS (useful for Windows)
// [Not all platforms are currently available or supported]
enum {
  wxCURSES,
  wxXVIEW_X,	// Sun's XView OpenLOOK toolkit
  wxMOTIF_X,	// OSF Motif 1.x.x
  wxCOSE_X,	// OSF Common Desktop Environment
  wxNEXTSTEP,	// NeXTStep
  wxMACINTOSH,	// Apple System 7
  wxGEOS,	// GEOS
  wxOS2_PM,	// OS/2 Workplace
  wxWINDOWS,	// Windows or WfW
  wxPENWINDOWS,	// Windows for Pen Computing
  wxWINDOWS_NT,	// Windows NT
  wxWIN32S,	// Windows 32S API
  wxWIN386	// Watcom 32-bit supervisor modus
};

const int kActiveControl = 0;
const int kInactiveControl = 255;

#define IMPLEMENT_DYNAMIC_CLASS(x, y) /* empty */

#endif // wxb_commonh
