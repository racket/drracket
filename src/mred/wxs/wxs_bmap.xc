
@INCLUDE prefix.xci

#include "wx_gdi.h"

@INCLUDE wxs.xci

#include "wxs_bmt.h"

#ifndef wx_mac
#define wxBITMAP_TYPE_PICT 0
#define wxBITMAP_TYPE_PICT_RESOURCE 0
#endif

@HEADER

@CLASSBASE wxBitmap "wx:bitmap" : "wx:object"

@SET TYPE = char
@SET NOTEST = 1
@INCLUDE list.xci

@MACRO ZEROERR[p.woh] = if ((x<p> < 1) || (x<p> > 100000)) scheme_signal_error("wx:bitmap%%::initialization: bad " <woh> ": %d", x<p>);

@MACRO NONZERODEPTH = if (x3 != 1) scheme_signal_error("wx:bitmap%%::initialization: depth %d is illegal (only depth 1 is supported)", x3);
@MACRO LISTENOUGH = if (scheme_proper_list_length(p[0]) < (((x1 * x2) >> 3) * x3)) scheme_signal_error("wx:bitmap%%::initialization: byte list too short");
	
@CREATOR (char[]/bList/ubList/cList,int,int,int=1); : : /ZEROERR[1."width"]|ZEROERR[2."height"]|NONZERODEPTH|LISTENOUGH|glueUncountedListSet[char.0.0."wx:bitmap%::initialization"]// <> character list
@CREATOR (int,int,int=-1); : : /ZEROERR[0."width"]|ZEROERR[1."height"] <> width/height
@CREATOR (pathname,long=wxBITMAP_TYPE_DEFAULT); <> pathname

@ "get-depth" : int GetDepth();
@ "get-height" : int GetHeight();
@ "get-width" : int GetWidth();
@ "ok?" : bool Ok();

@ "load-file" : void LoadFile(pathname,long=wxBITMAP_TYPE_DEFAULT);
@ "save-file" : void SaveFile(pathname,int,wxColourMap^=NULL);

@ "set-colour-map" : void SetColourMap(wxColourMap^);

@CONSTANT "wx:const-bitmap-type-default" : long wxBITMAP_TYPE_DEFAULT
@CONSTANT "wx:const-bitmap-discard-colourmap" : long wxBITMAP_DISCARD_COLOURMAP
@CONSTANT "wx:const-bitmap-type-bmp" : long wxBITMAP_TYPE_BMP
@CONSTANT "wx:const-bitmap-type-bmp-resource" : long wxBITMAP_TYPE_BMP_RESOURCE
@CONSTANT "wx:const-bitmap-type-gif" : long wxBITMAP_TYPE_GIF
@CONSTANT "wx:const-bitmap-type-xbm" : long wxBITMAP_TYPE_XBM
// @CONSTANT "wx:const-bitmap-type-xbm-data" : long wxBITMAP_TYPE_XBM_DATA
@CONSTANT "wx:const-bitmap-type-xpm" : long wxBITMAP_TYPE_XPM
// @CONSTANT "wx:const-bitmap-type-xpm-data" : long wxBITMAP_TYPE_XPM_DATA
// @CONSTANT "wx:const-bitmap-type-resource" : long wxBITMAP_TYPE_RESOURCE
@CONSTANT "wx:const-bitmap-type-pict" : long wxBITMAP_TYPE_PICT
@CONSTANT "wx:const-bitmap-type-pict-resource" : long wxBITMAP_TYPE_PICT_RESOURCE

@END
