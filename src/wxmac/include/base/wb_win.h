/*
 * File:	wb_win.h
 * Purpose:	
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_win.h	1.2 5/9/94" */

/*
 * Purpose:  wxWindow class declaration. Base class for all windows and
 *           panel items.
 */


#ifndef wxb_winh
#define wxb_winh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_obj.h"
#include "wx_stdev.h"
#include "wx_list.h"

#ifdef IN_CPROTO
typedef       void    *wxFunction ;
typedef       void    *wxbWindow ;
#else

class wxCursor;
class wxFont;
class wxIcon;
class wxColourMap;
class wxMenu;
class wxWindow;

// Callback function type definition
typedef void (*wxFunction) (wxObject&, wxEvent&);

/*
 * Base class for frame, panel, canvas, panel items, dialog box.
 *
 */

class wxWindow;
class wxMenu;
class wxbWindow: public wxObject
{
 protected:
  // Store the window's style
  long windowStyle;
 public:
  int doubleClickAllowed ;
  // Font - created on demand, not deleted with window
  wxFont *font;                               // Window's font
  wxCursor *wx_cursor;                        // Window's cursor

  char *wx_client_data;                       // Any user client data
#ifndef wx_mac
  wxList *children;                           // Window's children
#endif // wx_mac
  Bool paintingEnabled;
  Bool winCaptured;

#ifndef wx_mac
  wxWindow *window_parent;                     // Each window always knows its parent
#endif // wx_mac

  char *handle;                                // Pointer to real window
  char *windowName;                            // Window name

  wxFunction callback;                         // Callback associated with the window
  virtual void Callback(wxFunction);           // Adds callback

  // Constructors/Destructors
  wxbWindow(void);
#ifdef wx_mac
  wxbWindow(char* windowName);
#endif 

  virtual ~wxbWindow(void);

  inline virtual void Paint(void) {};             // Called when needs painting
  inline virtual void OnSize(int width, int height) {}; // Called on resize
  inline virtual void OnEvent(wxMouseEvent& event) {};  // Called on mouse event
  inline virtual void OnChar(wxKeyEvent& event) {};     // Called on character event
  inline virtual Bool OnClose(void) { return TRUE; };  // Delete window if returns TRUE
  inline virtual void OnActivate(Bool active) {};       // Called on window activation (MSW)
  inline virtual void OnSetFocus(void) {};              // Called on setting focus
  inline virtual void OnKillFocus(void) {};             // Called on killing focus
  inline virtual void OnDropFiles(int n, char *files[], int x, int y) {};
                                                 // Called when files dropped
  inline virtual void OnCommand(wxWindow& win, wxCommandEvent& event) {};
                                                 // Called if child control has no
                                                 // callback function

  virtual void GetSize(int *width, int *height) = 0;
  virtual void GetPosition(int *x, int *y) = 0;
  virtual void GetClientSize(int *width, int *height) = 0; // Size client can use
  virtual void SetSize(int x, int y, int width, int height, int flags = wxSIZE_AUTO) = 0;
  virtual void SetClientSize(int width, int size) = 0;
  virtual void ClientToScreen(int *x, int *y) = 0;
  virtual void ScreenToClient(int *x, int *y) = 0;
  virtual void Enable(Bool enable) = 0;
  virtual void SetFocus(void) = 0;
  virtual void CaptureMouse(void) = 0;
  virtual void ReleaseMouse(void) = 0;
  virtual void DragAcceptFiles(Bool accept) = 0;
  virtual void MakeModal(Bool modal);

  virtual char *GetHandle(void);
  char *GetClientData(void);
#ifdef wx_mac
  virtual wxWindow *GetParent(void) = 0;
  virtual wxWindow *GetGrandParent(void) = 0;
  virtual wxChildList *GetChildren(void) = 0;
#else // wx_mac
  virtual wxWindow *GetParent(void);
  virtual wxWindow *GetGrandParent(void);
  inline virtual wxList *GetChildren() { return children; }
#endif // wx_mac

  void SetClientData(char *);
  virtual void Show(Bool show) = 0;
  virtual wxCursor *SetCursor(wxCursor *cursor) = 0;
  virtual void SetColourMap(wxColourMap *cmap) = 0;

  virtual float GetCharWidth(void) = 0;
  virtual float GetCharHeight(void) = 0;
#ifdef wx_mac
  inline virtual void GetTextExtent(const char* string, float* x, float* y, float* descent = NULL,
  						float* externalLeading = NULL, wxFont* the_font = NULL, Bool use16=FALSE) {};
#else // wx_mac
  inline virtual void GetTextExtent(const char *string, float *x, float *y,
       float *descent = NULL, float *externalLeading = NULL, wxFont *theFont = NULL) {};
#endif
  inline virtual void SetTitle(char *title) {};      // Set window title
  inline virtual char *GetTitle(void) { return NULL; }; // Set window title
  // Most windows have the concept of a label; for frames, this is the
  // title; for items, this is the label or button text.
  inline virtual char *GetLabel(void) { return GetTitle(); }

  inline virtual char *GetName(void) { return windowName; }
  virtual void SetName(char *name);

  inline virtual void Fit(void) {};                  // Size window to fit contents
  inline virtual void Centre(int direction) {};      // Centre item on panel,
                                               // or frame on screen
  // Renamed from GetWindowStyle since it clashed with a
  // macro in windowsx.h.
  inline long GetWindowStyleFlag(void) { return windowStyle; }
  // A concession to our friends across the pond
  inline void Center(int direction = wxHORIZONTAL) { Centre(direction); }

  inline virtual void EnablePainting(Bool enable) { paintingEnabled = enable; }

  virtual Bool PopupMenu(wxMenu *menu, float x, float y) = 0;

  // INTERNAL FUNCTIONS
#ifdef wx_mac
  virtual void AddChild(wxObject *child) = 0;      // Adds reference to the child object
  virtual void DestroyChildren(void) = 0;  		   // Removes and destroys all children
#else // wx_mac
  virtual void AddChild(wxObject *child);      // Adds reference to the child object
  virtual void RemoveChild(wxObject *child);   // Removes reference to child
                                       // (but doesn't delete the child object)
  virtual void DestroyChildren(void);  // Removes and destroys all children
#endif // wx_mac

  wxWindow *ContextWindow(void);

#ifdef wx_mac
//=============================================================================
// Private methods
//=============================================================================
private:

	void InitDefaults(void);

#endif // wx_mac

};

#if 0
extern wxList wxTopLevelWindows;
#else
extern wxChildList *wxGetTopLevelWindowsList(wxObject*);
#define wxTopLevelWindows(w) (wxGetTopLevelWindowsList(w))
#endif

extern void *wxGetContextForFrame();

extern void wxDispatchEventsUntil(int (*f)(void *), void *data);
extern wxWindow *wxGetModalWindow(wxObject *o);
extern void wxPutModalWindow(wxObject *o, wxWindow *win);

#endif // IN_CPROTO
#endif // wxb_winh
