
@INCLUDE prefix.xci

#include "wx_gdi.h"

@INCLUDE wxs.xci

@HEADER

@INCLUDE wxs_bmt.xci

@BEGINSYMBOLS saveBitmapType > ONE > PRED BUNDLE
@SYM "bmp" : wxBITMAP_TYPE_BMP
@SYM "xbm" : wxBITMAP_TYPE_XBM
@SYM "xpm" : wxBITMAP_TYPE_XPM
@SYM "pict" : wxBITMAP_TYPE_PICT
@ENDSYMBOLS

static Bool IsColor(wxBitmap *bm)
{
  return (bm->GetDepth() == 1);
}

@CLASSBASE wxBitmap "bitmap" : "object"

@MACRO STRINGENOUGH = if (SCHEME_STRTAG_VAL(p[0]) < (((x1 * x2) + 7) >> 3)) WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("bitmap%","initialization"), "string too short: ", p[0]));

@MACRO USEALLFUEL[ok] = if (<ok>) WITH_VAR_STACK(scheme_process_block(0.0));

@CREATOR (string////string,rint[1|10000],rint[1|10000]); : : /STRINGENOUGH// <> datastring
@CREATOR (rint[1|10000],rint[1|10000],bool=0); : : <> width/height
@CREATOR (pathname////string,SYM[bitmapType]=0); : : //USEALLFUEL[realobj->Ok()] <> pathname

@ "get-depth" : int GetDepth();
@ "get-height" : int GetHeight();
@ "get-width" : int GetWidth();
@ "ok?" : bool Ok();
@ m "is-color?" : bool IsColor();

@ "load-file" : bool LoadFile(pathname,SYM[bitmapType]=0);  : : //USEALLFUEL[r]
@ "save-file" : bool SaveFile(pathname,SYM[saveBitmapType]);  : : //USEALLFUEL[1]

@END
