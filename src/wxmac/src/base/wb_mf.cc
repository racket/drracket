/*
 * File:	wb_mf.cc
 * Purpose:	Metafiles and metafile DCs
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_mf.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "@(#)wb_mf.cc	1.2 5/9/94"; */

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#if USE_METAFILE
#include "wb_mf.h"
#include "wx_types.h"

/*
 * Metafiles - Windows 3.1 only
 * Currently, the only purpose for making a metafile is to put
 * it on the clipboard.
 */

wxbMetaFile::wxbMetaFile(void)
{
  __type = wxTYPE_METAFILE;
}

wxbMetaFile::~wxbMetaFile(void)
{
}

Bool wxbMetaFile::SetClipboard(int width, int height)
{
  return FALSE;
}

/*
 * Metafile device context
 *
 */

wxbMetaFileDC::wxbMetaFileDC(char *file)
{
  __type = wxTYPE_DC_METAFILE;
}

wxbMetaFileDC::~wxbMetaFileDC(void)
{
}

wxMetaFile *wxbMetaFileDC::Close(void)
{
  return NULL;
}

void wxbMetaFileDC::SetMapMode(int mode)
{
}

#endif // USE_METAFILE
