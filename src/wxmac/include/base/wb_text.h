/*
 * File:	wb_text.h
 * Purpose:	wxTextWindow - simple text subwindow class
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_text.h	1.2 5/9/94" */

#ifndef wxb_texth
#define wxb_texth

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_frame.h"

#ifdef IN_CPROTO
typedef       void    *wxbTextWindow ;
#else

/*
 * I would make this also derived from ostream:
 *
 * class wxTextWindow: public wxWindow, public virtual ostream
 *
 * but weirdly, the operator << isn't declared virtual in (isn't even
 * a member of) ostream, so we can't use a wxTextWindow in place of an ostream.
 *
 */

class wxbTextWindow: public wxWindow
{
 public:
  char *file_name;

#ifndef wx_mac
  wxbTextWindow(void);
  wxbTextWindow(wxWindow *parent, int x=-1, int y=-1, int width=-1, int height=-1,
               long style=0, char *name = "textWindow");
#endif // wx_mac
#ifdef wx_mac
  // Constructor (given parentArea)
  wxbTextWindow(WXTYPE objectType, char* windowName, wxArea* parentArea, int x, int y,
		int width, int height, long style);
  // Constructor (given parentWindow)
  wxbTextWindow (WXTYPE objectType, char* windowName, wxWindow*	parentWindow, int x, int y,
		int width, int height, long style);
#endif // wx_mac

  ~wxbTextWindow(void);

  virtual Bool LoadFile(char *file) = 0;
  virtual Bool SaveFile(char *file) = 0;
  virtual void WriteText(char *text) = 0;
  virtual void Clear(void) = 0;
  virtual void DiscardEdits(void) = 0;
  virtual Bool Modified(void) = 0;
  virtual char *GetContents(void) = 0;
  virtual void SetInsertionPoint(long pos) = 0;
  virtual void SetInsertionPointEnd(void) = 0;
  virtual long GetInsertionPoint(void) = 0;
  virtual long GetLastPosition(void) = 0;
  virtual long XYToPosition(long x, long y) = 0;
  virtual void PositionToXY(long pos, long *x, long *y) = 0;
  virtual int GetNumberOfLines(void) = 0;
  virtual void ShowPosition(long pos) = 0;
  virtual int GetLineLength(long lineNo) = 0;
  virtual int GetLineText(long lineNo, char *buf) = 0;
  virtual void Replace(long from, long to, char *value) = 0;
  virtual void Remove(long from, long to) = 0;
  virtual void SetSelection(long from, long to) = 0;
  virtual void Copy(void) = 0; // Copy selection to clipboard
  virtual void Paste(void) =0; // Paste clipboard into text window
  virtual void Cut(void) = 0; // Copy selection to clipboard, then remove selection.
  virtual void SetEditable(Bool editable) = 0;
#ifndef wx_mac
  inline virtual void SetFont(wxFont *theFont) { font = theFont; }
#endif // wx_mac
  inline virtual wxFont *GetFont(void) { return font; }

  wxbTextWindow& operator<<(char *s);
  wxbTextWindow& operator<<(int i);
  wxbTextWindow& operator<<(long i);
  wxbTextWindow& operator<<(float f);
  wxbTextWindow& operator<<(double d);
  wxbTextWindow& operator<<(char c);

};

#define wxTEXT_MAX_LINES 1000

#endif // IN_CPROTO
#endif // wxb_texth
