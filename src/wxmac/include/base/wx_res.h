/*
 * File:	wb_res.h
 * Purpose:	Resource processor
 * Author:	Julian Smart
 * Created:	1994
 * Updated:	
 * Copyright:	(c) 1994, Julian Smart
 */

/* sccsid[] = "%W% %G%" */

#ifndef wxb_resh
#define wxb_resh

#ifdef __GNUG__
#pragma interface
#endif

#include "wx_setup.h"

#if USE_WX_RESOURCES
#include <stdio.h>

// A few further types not in wx_types.h
#define wxRESOURCE_TYPE_SEPARATOR   1000
#define wxRESOURCE_TYPE_XBM_DATA    1001
#define wxRESOURCE_TYPE_XPM_DATA    1002

/*
 * Internal format for control/panel item
 */
 
class wxItemResource: public wxObject
{
 protected:
  wxList children;
  WXTYPE itemType;
  int x, y, width, height;
  char *title;
  char *name;
  long windowStyle;
  long value1, value2, value3;
  char *value4;
  wxStringList *stringValues; // Optional string values
  wxBitmap *bitmap;
 public:
 
  wxItemResource(void);
  ~wxItemResource(void);

  inline void SetType(WXTYPE typ) { itemType = typ; }
  inline void SetStyle(long styl) { windowStyle = styl; }
  inline void SetBitmap(wxBitmap *bm) { bitmap = bm; }
  inline wxBitmap *GetBitmap(void) { return bitmap; }
  inline void SetSize(int xx, int yy, int ww, int hh)
  {  x = xx; y = yy; width = ww; height = hh; }
  void SetTitle(char *t);
  void SetName(char *n);
  inline void SetValue1(long v) { value1 = v; }
  inline void SetValue2(long v) { value2 = v; }
  inline void SetValue3(long v) { value3 = v; }
  void SetValue4(char *v);
  inline void SetStringValues(wxStringList *svalues) { stringValues = svalues; }

  inline WXTYPE GetType(void) { return itemType; }
  inline int GetX(void) { return x; }
  inline int GetY(void) { return y; }
  inline int GetWidth(void) { return width; }
  inline int GetHeight(void) { return height; }

  inline char *GetTitle(void) { return title; }
  inline char *GetName(void) { return name; }
  inline long GetStyle(void) { return windowStyle; }

  inline long GetValue1(void) { return value1; }
  inline long GetValue2(void) { return value2; }
  inline long GetValue3(void) { return value3; }
  inline char *GetValue4(void) { return value4; }
  inline wxList& GetChildren(void) { return children; }
  inline wxStringList *GetStringValues(void) { return stringValues; }
};

/*
 * Resource table (normally only one of these)
 */
 
class wxResourceTable: public wxHashTable
{
  protected:
    
  public:
    wxResourceTable(void);
    ~wxResourceTable(void);
    
    wxItemResource *FindResource(char *name);
    void AddResource(wxItemResource *item);
    Bool DeleteResource(char *name);

    Bool ParseResourceFile(char *filename);
    Bool ParseResourceData(char *data);
    Bool SaveResource(char *filename);

    // Register XBM/XPM data
    Bool RegisterResourceBitmapData(char *name, char bits[], int width, int height);
    Bool RegisterResourceBitmapData(char *name, char **data);

    void ClearTable(void);
};

extern wxResourceTable wxDefaultResourceTable;

extern long wxParseWindowStyle(char *style);

class wxMenuBar;
class wxMenu;
class wxBitmap;
class wxIcon;
extern wxBitmap *wxResourceCreateBitmap(char *resource);
extern wxIcon *wxResourceCreateIcon(char *resource);
extern wxMenuBar *wxResourceCreateMenuBar(char *resource);
extern wxMenu *wxResourceCreateMenu(char *resource);
extern Bool wxResourceParseData(char *resource);
extern Bool wxResourceParseFile(char *filename);
extern void wxResourceClear(void);
// Register XBM/XPM data
extern Bool wxResourceRegisterBitmapData(char *name, char bits[], int width, int height);
extern Bool wxResourceRegisterBitmapData(char *name, char **data);
#define wxResourceRegisterIconData wxResourceRegisterBitmapData

/*
 * Resource identifer code: #define storage
 */

extern Bool wxResourceAddIdentifier(char *name, int value);
extern int wxResourceGetIdentifier(char *name);

/*
 * Proposed PROLOGIO resource file format
 */
/*
 * Dialog/panel
static char *name1 = "dialog(name = \"name\",\
  title = \"title\",\
  style = \"wxDEFAULT_DIALOG_STYLE\",\
  button_font = [12, 'wxSWISS', 'wxITALIC', 'wxBOLD', 0],
  label_font = [12, 'wxSWISS', 'wxITALIC', 'wxBOLD', 0],
  modal = 1,
  x = 0, y = 0, width = 100, height = 200,\
  control = [wxButton, \"OK\", \"style\", \"name\", 10, 10, 55, 30],\
  control = [wxButton, \"\", \"style\", \"name\", 10, 10, 55, 30, \"bitmapresource\"],\
  control = [wxText  , \"Text\", \"style\", \"name\", 10, 10, 55, 30, \"default value\"],\
  control = [wxMultiText, \"Lines\", \"style\", \"name\", 10, 10, 55, 30, \"default value\"],\
  control = [wxListBox, \"List\", \"wxHSCROLL\", \"name\", 10, 10, 55, 30, [\"value1\", \"value2\"], wxMULTIPLE],\
  control = [wxChoice, \"Choice\", \"style\", \"name\", 10, 10, 55, 30, [\"value1\", \"value2\"]],\
  control = [wxGroupBox, \"Group\", \"style\", \"name\", 10, 10, 55, 30],\
  control = [wxMessage, \"Hello\", \"style\", \"name\", 10, 10, 55, 30],\
  control = [wxMessage, \"\", \"style\", \"name\", 10, 10, 55, 30, \"bitmapresource\"],\
  control = [wxRadioBox, \"Radio\", \"style\", \"name\", 10, 10, 55, 30, [\"value1\", \"value2\"]],\
  control = [wxCheckBox, \"Checkbox\", \"style\", \"name\", 10, 10, 55, 30, 1]).\
*/
/*
 * Bitmap resource
 * Allows for different bitmap formats on different platforms.
 * A bitmap specification:
 * bitmap = [filename, bitmaptype, platform, colours, xresolution, yresolution]
 * Everything after 'filename' is optional.
static char *bitmap1 = "bitmap(name = \"bitmap1\",
  bitmap = [\"open.bmp\", wxBITMAP_TYPE_BMP, \"WINDOWS\"],
  bitmap = [\"open\", wxBITMAP_TYPE_RESOURCE, \"WINDOWS\"],
  bitmap = [\"open.xpm\", wxBITMAP_TYPE_XPM, \"X\", 16, 20, 20],
  bitmap = [\"open.xbm\", wxBITMAP_TYPE_XBM, \"X\", 2, 20, 20],
 */
/*
 * Menu/menubar
 * Each menu is a list of lists.
 * The inner list is a menu item specifier as follows:
 * [string, identifier, helpString, optionalSubMenu]
 * or
 * [] for a separator.
 
static char *menuBar1 = "menubar(name = 'menu1',\
  menu = [[\"&File\", 'FILE_ID', '',\
            ['&Open', 'OPEN_FILE_ID', \"Open a file\"],\
            ['&Close', 'CLOSE_FILE_ID', \"Close a file\"],\
            [],\
            ['&Exit', 'EXIT_ID', \"Exit program\"]\
          ],\
          [\"&Help\", '', '',\
           ['&About', 'ABOUT_ID', \"About this program\"]]]).";

 * Problem: how to define the identifiers, e.g. OPEN_FILE_ID, so that
 * they can be used in switch statements too????
 * Could have another resource format:
 * identifiers(\
 *   'FILE_ID' = 1,\
 *   'OPEN_FILE_ID' = 2,\
 *   'CLOSE_FILE_ID' = 3).
 *
 */
#endif
#endif
