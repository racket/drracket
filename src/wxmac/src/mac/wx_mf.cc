/*
 * File:        wx_mf.cc
 * Purpose:     Metafiles and metafile DCs (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_mf.cc,v 1.3 1994/08/14 21:28:43 edz Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

static const char sccsid[] = "@(#)wx_mf.cc	1.2 5/9/94";

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"

#if USE_METAFILE

#include "wx_mf.h"

/*
 * Metafiles - Windows 3.1 only
 * Currently, the only purpose for making a metafile is to put
 * it on the clipboard.
 */

IMPLEMENT_DYNAMIC_CLASS(wxMetaFile, wxObject)

wxMetaFile::wxMetaFile (void)
{
}

wxMetaFile::~wxMetaFile (void)
{
}

Bool wxMetaFile::SetClipboard (int width, int height)
{
  return FALSE;
}

/*
 * Metafile device context: not implemented yet.
 *
 */

IMPLEMENT_ABSTRACT_CLASS(wxMetaFileDC, wxDC)

wxMetaFileDC::wxMetaFileDC (char *file)
{
  ok = FALSE;
}

wxMetaFileDC::~wxMetaFileDC (void)
{
}

wxMetaFile *wxMetaFileDC::Close (void)
{
  return NULL;
}

void wxMetaFileDC::SetMapMode (int mode)
{
}

#endif // USE_METAFILE
