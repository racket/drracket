/*
 * File:	wx_mf.h
 * Purpose:	Metafile device context declaration (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_mf.h	1.2 5/9/94" */


#ifndef wx_mfh
#define wx_mfh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_setup.h"

#if USE_METAFILE
#include "wb_mf.h"

/*
 * Metafile and metafile device context classes - work in Windows 3.1 only
 *
 */

#ifdef IN_CPROTO
typedef       void    *wxMetaFile ;
typedef       void    *wxMetaFileDC ;
#else

class wxMetaFile: public wxbMetaFile
{
  DECLARE_DYNAMIC_CLASS(wxMetaFile)

 public:
    
  wxMetaFile(void);
  ~wxMetaFile(void);

  // After this is called, the metafile cannot be used for anything
  // since it is now owned by the clipboard.
  Bool SetClipboard(int width = 0, int height = 0);
};

class wxMetaFileDC: public wxbMetaFileDC
{
  DECLARE_DYNAMIC_CLASS(wxMetaFileDC)

 public:
  wxMetaFileDC(char *file = NULL);
  ~wxMetaFileDC(void);

  // Should be called at end of drawing
  wxMetaFile *Close(void);
  void SetMapMode(int mode);
};

#endif // IN_CPROTO
#endif // USE_METAFILE
#endif // wx_mfh
