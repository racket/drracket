
@INCLUDE prefix.xci

#include "wx_obj.h"
#include "wx_list.h"
#include "wx_gdi.h"
#ifdef wx_xt
#include "wx_dc.h"
#endif

@INCLUDE wxs.xci

#include "wxs_bmt.h"

@HEADER

#define USE_FONT_NAME_DIRECTORY 1

#if defined(wx_mac) || defined(wx_xt)
#define COLORMAP_CREATE 0
#else
#define COLORMAP_CREATE 1
#endif

@MACRO CHECKMUT[TYPE.what.who] = if (!((<TYPE> *)((Scheme_Class_Object *)obj)->primdata)->IsMutable()) scheme_signal_error("%s: this wx:%s%% object is locked (in use by a wx:dc%% or in a list of %s constants)", <who>, <what>, <what>);

@CLASSBASE wxFont "wx:font":"wx:object"

@CREATOR (); <> no argument
@CREATOR (nnint,int,int,int,bool=0) <> font id
@CREATOR (nnint,cstring,int,int,int,bool=0) <> font name ## USE_FONT_NAME_DIRECTORY

@ "get-family" : int GetFamily();
@ "get-font-id" : int GetFontId(); ## USE_FONT_NAME_DIRECTORY
@ "get-style" : int GetStyle();
@ "get-point-size" : int GetPointSize();
@ "get-weight" : int GetWeight();
@ "get-underlined" : bool GetUnderlined();

@CONSTANT "wx:const-default" : int wxDEFAULT
@CONSTANT "wx:const-decorative" : int wxDECORATIVE
@CONSTANT "wx:const-roman" : int wxROMAN
@CONSTANT "wx:const-script" : int wxSCRIPT
@CONSTANT "wx:const-swiss" : int wxSWISS
@CONSTANT "wx:const-modern" : int wxMODERN
@CONSTANT "wx:const-teletype" : int wxTELETYPE
@CONSTANT "wx:const-system" : int wxSYSTEM

// Not used:
// @CONSTANT "wx:const-variable" : int wxVARIABLE
// @CONSTANT "wx:const-fixed" : int wxFIXED

@CONSTANT "wx:const-normal" : int wxNORMAL
@CONSTANT "wx:const-light" : int wxLIGHT
@CONSTANT "wx:const-bold" : int wxBOLD

@CONSTANT "wx:const-italic" : int wxITALIC
@CONSTANT "wx:const-slant" : int wxSLANT

@END


@CLASSBASE wxFontList "wx:font-list":"wx:object"

@CREATOR ();

@ "find-or-create-font" : wxFont! FindOrCreateFont(nnint,int,int,int,bool=0) <> font id
@ "find-or-create-font" : wxFont! FindOrCreateFont(nnint,cstring,int,int,int,bool=0) <> font name ## USE_FONT_NAME_DIRECTORY
// @ "remove-font" : void RemoveFont(wxFont!);

@CONSTANT "wx:the-font-list" : wxFontList! wxTheFontList

@END


@CLASSBASE wxColour "wx:colour" : "wx:object"

@CREATOR (); <> no argument
@CREATOR (ubyte,ubyte,ubyte); <> rgb values
@CREATOR (string); <> color name

@ "=" : wxColour% operator=(wxColour%);  : : /CHECKMUT[wxColour."colour"."wx:colour%::="]
@ "get" : void Get(ubyte*,ubyte*,ubyte*);
@ "ok?" : bool Ok();
@ "set" : void Set(ubyte,ubyte,ubyte);   : : /CHECKMUT[wxColour."colour"."wx:colour%::set"]

@ "red" : ubyte Red();
@ "green" : ubyte Green();
@ "blue" : ubyte Blue();

@END


@CLASSBASE wxColourMap "wx:colour-map" : "wx:object"

// @CREATOR (); ## !defined(wx_mac)

// @ "create" : bool Create(int,custring,custring,custring); ## COLORMAP_CREATE

@END

#ifdef wx_mac
#define _KEY_TYPE KeyType
#else
#define _KEY_TYPE int
#endif
@MACRO bInt = objscheme_bundle_integer((int){x})
@MACRO ubIntKey = (_KEY_TYPE)objscheme_unbundle_integer({x}, "wxColourDatabase")
@MACRO tInt = objscheme_istype_number({x}, NULL)

// Since we don't allow creating this anymore, need a Mac fix:
#if defined(wx_mac)
#define CDB_FIX os_wxColourDatabase(_KEY_TYPE x) : wxColourDatabase(x) {}
#else
#define CDB_FIX 
#endif

@CLASSBASE wxColourDatabase "wx:colour-database" : "wx:object"

@VAR CDB_FIX

// @CREATOR (_KEY_TYPE/bInt/ubIntKey/tInt);

@ "find-colour" : wxColour^ FindColour(string);
@ "find-name" : string FindName(wxColour%);
// @ "initialize" : void Initialize(); ## !defined(wx_xt)
@ "append" : void Append(string, wxColour!);

@CONSTANT "wx:the-colour-database" : wxColourDatabase! wxTheColourDatabase

@END


@CLASSBASE wxPoint "wx:point" : "wx:object"

@CREATOR (); <> no argument
@CREATOR (float,float); <> xy values

@IVAR "x" : float x
@IVAR "y" : float y

@END

@CLASSBASE wxIntPoint "wx:int-point" : "wx:object"

@CREATOR (); <> no argument
@CREATOR (int,int) <> xy values

@IVAR "x" : int x
@IVAR "y" : int y

@END

@CLASSBASE wxBrush "wx:brush" : "wx:object"

@CREATOR (); <> no argument
@CREATOR (wxColour%,int); <> wx:colour%
@CREATOR (string,int); <> color name

@ "get-colour" : wxColour% GetColour();
@ "set-colour" : void SetColour(wxColour%); : : /CHECKMUT[wxBrush."brush"."wx:brush::set-colour"] <> wx:colour%
@ "set-colour" : void SetColour(string); : : /CHECKMUT[wxBrush."brush"."wx:brush::set-colour"] <> color name
@ "set-colour" : void SetColour(int,int,int); : : /CHECKMUT[wxBrush."brush"."wx:brush::set-colour"] <> rgb values

@ "get-stipple" : wxBitmap! GetStipple();
@ "set-stipple" : void SetStipple(wxBitmap^); : : /CHECKVOIDABLEOK[0]|CHECKMUT[wxBrush."brush"."wx:brush::set-stipple"]

@ "get-style" : int GetStyle();
@ "set-style" : void SetStyle(int); : : /CHECKMUT[wxBrush."brush"."wx:brush::set-style"]

@CONSTANT "wx:const-transparent" : int wxTRANSPARENT
@CONSTANT "wx:const-solid" : int wxSOLID
@CONSTANT "wx:const-bdiagonal-hatch" : int wxBDIAGONAL_HATCH
@CONSTANT "wx:const-crossdiag-hatch" : int wxCROSSDIAG_HATCH
@CONSTANT "wx:const-fdiagonal-hatch" : int wxFDIAGONAL_HATCH
@CONSTANT "wx:const-cross-hatch" : int wxCROSS_HATCH
@CONSTANT "wx:const-horizontal-hatch" : int wxHORIZONTAL_HATCH
@CONSTANT "wx:const-vertical-hatch" : int wxVERTICAL_HATCH
@CONSTANT "wx:const-stipple" : int wxSTIPPLE
@CONSTANT "wx:const-opaque-stipple" : int wxOPAQUE_STIPPLE

@END

@CLASSBASE wxBrushList "wx:brush-list" : "wx:object"

@CREATOR ();

@ "find-or-create-brush" : wxBrush! FindOrCreateBrush(wxColour!,int); <> wx:colour%
@ "find-or-create-brush" : wxBrush! FindOrCreateBrush(string,int); <> color name
// @ "remove-brush" : void RemoveBrush(wxBrush!);

@CONSTANT "wx:the-brush-list" : wxBrushList! wxTheBrushList

@END

@CLASSBASE wxPen "wx:pen" : "wx:object"

@CREATOR (); <> no argument
@CREATOR (wxColour%,nnint,int); <> wx:colour%
@CREATOR (string,nnint,int); <> color name

@ "get-width" : int GetWidth();
@ "set-width" : void SetWidth(int);
@ "get-cap" : int GetCap();
@ "set-cap" : void SetCap(int);
@ "get-join" : int GetJoin();
@ "set-join" : void SetJoin(int);

@ "get-colour" : wxColour% GetColour();
@ "set-colour" : void SetColour(wxColour%);  : : /CHECKMUT[wxPen."pen"."wx:pen::set-colour"] <> wx:colour%
@ "set-colour" : void SetColour(string);  : : /CHECKMUT[wxPen."pen"."wx:pen::set-colour"] <> color name
@ "set-colour" : void SetColour(int,int,int);  : : /CHECKMUT[wxPen."pen"."wx:pen::set-colour"] <> rgb values

@ "get-stipple" : wxBitmap! GetStipple();
@ "set-stipple" : void SetStipple(wxBitmap^); : : /CHECKVOIDABLEOK[0]|CHECKMUT[wxPen."pen"."wx:pen::set-stipple"]

@ "get-style" : int GetStyle();
@ "set-style" : void SetStyle(int); : : /CHECKMUT[wxPen."pen"."wx:pen::set-style"]

@CONSTANT "wx:const-join-bevel" : int wxJOIN_BEVEL
@CONSTANT "wx:const-join-miter" : int wxJOIN_MITER
@CONSTANT "wx:const-join-round" : int wxJOIN_ROUND

@CONSTANT "wx:const-cap-round" : int wxCAP_ROUND
@CONSTANT "wx:const-cap-projecting" : int wxCAP_PROJECTING
@CONSTANT "wx:const-cap-butt" : int wxCAP_BUTT

@CONSTANT "wx:const-dot" : int wxDOT
@CONSTANT "wx:const-long-dash" : int wxLONG_DASH
@CONSTANT "wx:const-short-dash" : int wxSHORT_DASH
@CONSTANT "wx:const-dot-dash" : int wxDOT_DASH

@END


@CLASSBASE wxPenList "wx:pen-list" : "wx:object"

@CREATOR ();

@ "find-or-create-pen" : wxPen! FindOrCreatePen(wxColour!,nnint,int); <> wx:colour%
@ "find-or-create-pen" : wxPen! FindOrCreatePen(string,nnint,int); <> color name
// @ "remove-pen" : void RemovePen(wxPen!);

@CONSTANT "wx:the-pen-list" : wxPenList! wxThePenList

@END

@CLASSBASE wxCursor "wx:cursor" : "wx:object"

// @CREATOR ();
@CREATOR (string,long=wxBITMAP_TYPE_DEFAULT,int=0,int=0); <> cursor name
@CREATOR (int); <> cursor id

@ "ok?" : bool Ok();

@CONSTANT "wx:const-cursor-arrow" : int wxCURSOR_ARROW
@CONSTANT "wx:const-cursor-bullseye" : int wxCURSOR_BULLSEYE
@CONSTANT "wx:const-cursor-char" : int wxCURSOR_CHAR
@CONSTANT "wx:const-cursor-cross" : int wxCURSOR_CROSS
@CONSTANT "wx:const-cursor-hand" : int wxCURSOR_HAND
@CONSTANT "wx:const-cursor-ibeam" : int wxCURSOR_IBEAM
@CONSTANT "wx:const-cursor-left-button" : int wxCURSOR_LEFT_BUTTON
@CONSTANT "wx:const-cursor-magnifier" : int wxCURSOR_MAGNIFIER
@CONSTANT "wx:const-cursor-middle-button" : int wxCURSOR_MIDDLE_BUTTON
@CONSTANT "wx:const-cursor-no-entry" : int wxCURSOR_NO_ENTRY
@CONSTANT "wx:const-cursor-painr-brush" : int wxCURSOR_PAINT_BRUSH
@CONSTANT "wx:const-cursor-pencil" : int wxCURSOR_PENCIL
@CONSTANT "wx:const-cursor-point-left" : int wxCURSOR_POINT_LEFT
@CONSTANT "wx:const-cursor-point-right" : int wxCURSOR_POINT_RIGHT
@CONSTANT "wx:const-cursor-question-arrow" : int wxCURSOR_QUESTION_ARROW
@CONSTANT "wx:const-cursor-right-button" : int wxCURSOR_RIGHT_BUTTON
@CONSTANT "wx:const-cursor-sizenesw" : int wxCURSOR_SIZENESW
@CONSTANT "wx:const-cursor-sizens" : int wxCURSOR_SIZENS
@CONSTANT "wx:const-cursor-sizenwse" : int wxCURSOR_SIZENWSE
@CONSTANT "wx:const-cursor-sizewe" : int wxCURSOR_SIZEWE
@CONSTANT "wx:const-cursor-sizing" : int wxCURSOR_SIZING
@CONSTANT "wx:const-cursor-spraycan" : int wxCURSOR_SPRAYCAN
@CONSTANT "wx:const-cursor-wait" : int wxCURSOR_WAIT
@CONSTANT "wx:const-cursor-watch" : int wxCURSOR_WATCH

@END


@CLASSBASE wxIcon "wx:icon" : "wx:bitmap"

// @CREATOR ();
@CREATOR (string, int=wxBITMAP_TYPE_DEFAULT);

// in wx:bitmap%: 
// @ "ok?" : bool Ok();

@END

#if USE_FONT_NAME_DIRECTORY

@CLASSBASE wxFontNameDirectory "wx:font-name-directory":"wx:object"

@ "get-screen-name" : nstring GetScreenName(int,int,int);
@ "get-post-script-name" : nstring GetPostScriptName(int,int,int);
@ "get-afm-name" : nstring GetAFMName(int,int,int);

@ "get-new-font-id" :   int GetNewFontId()
@ "initialize" : void Initialize(int,int,string);

@ "get-font-id" : int GetFontId(string);
@ "get-font-name" : nstring GetFontName(int);
@ "get-family" : int GetFamily(int);

@ "find-or-create-font-id" : int FindOrCreateFontId(cstring,int);

@CONSTANT "wx:the-font-name-directory" : wxFontNameDirectory% wxTheFontNameDirectory

@END

#endif
