/*
 * File:	wb_mf.h
 * Purpose:	Metafiles
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_mf.h	1.2 5/9/94" */


#ifndef wxb_mfh
#define wxb_mfh
#include "wx_setup.h"

#ifdef __GNUG__
#pragma interface
#endif

#if USE_METAFILE
#include "wx_dc.h"

/*
 * Metafile and metafile device context classes - work in Windows 3.1 only
 *
 */

#ifdef IN_CPROTO
typedef       void    *wxbMetaFile ;
typedef       void    *wxbMetaFileDC ;
#else

class wxbMetaFile: public wxObject
{
 public:
    
  wxbMetaFile(void);
  ~wxbMetaFile(void);

  // After this is called, the metafile cannot be used for anything
  // since it is now owned by the clipboard.
  virtual Bool SetClipboard(int width = 0, int height = 0);
};

class wxMetaFile;
class wxbMetaFileDC: public wxDC
{
 public:
  wxbMetaFileDC(char *file = NULL);
  ~wxbMetaFileDC(void);

  // Should be called at end of drawing
  virtual wxMetaFile *Close(void);
  virtual void SetMapMode(int mode);
};

#endif // IN_CPROTO
#endif // USE_METAFILE
#endif // wxb_mfh
