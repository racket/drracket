
@INCLUDE prefix.xci

#include "wx_obj.h"
#include "wx_win.h"
#include "wx_timer.h"
#include "wx_types.h"
#include "wx_stdev.h"
#include "wx_dc.h"
#include "wx_clipb.h"

@INCLUDE wxs.xci

@HEADER

#if !defined(wx_mac)
#define NEWEST_TYPES 1
#else
#define NEWEST_TYPES 0
#endif

#ifdef wx_msw

#include "wx_mf.h"

class baseMetaFile : public wxMetaFile
{
};

#else

class baseMetaFile : public wxObject
{
public:
  Bool Ok() { return FALSE; }
  void Play(wxDC*) { }
  Bool SetClipboard(int, int) { return FALSE; }

};

#endif

@CLASSBASE baseMetaFile "wx:meta-file" : "wx:object"

// @CREATOR (string=NULL);

@ "ok?" : bool Ok();
@ "play" : void Play(wxDC!);
@ "set-clipboard" : bool SetClipboard(int=0,int=0);

@END

@MACRO rFALSE = return FALSE;

@CLASSBASE wxTimer "wx:timer" : "wx:object"

@CREATOR ();

@ "interval" : int Interval();
@ v "notify" : void Notify();
@ "start" : bool Start(int,bool=FALSE); : : : : rFALSE
@ "stop" : void Stop();

@END


void AddType(wxClipboardClient *c, char *s) 
{ 
  c->formats.Add(s); 
}

Scheme_Object *GetTypes(wxClipboardClient *c)
{
  wxNode *n = c->formats.First();
  Scheme_Object *first = scheme_null, *last = NULL;
  for (; n; n = n->Next()) {
    Scheme_Object *p;
    
    p = scheme_make_pair(scheme_make_string((char *)n->Data()), scheme_null);
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;
  }
  
  return first;
}

@MACRO makeSizedString[i] = (r ? scheme_make_sized_string(r, _x<i>, 1) : scheme_null)

@CLASSBASE wxClipboard "wx:clipboard" : "wx:object"

@ "set-clipboard-client" : void SetClipboardClient(wxClipboardClient!,long);
@ "set-clipboard-string" : void SetClipboardString(string,long);
@ "get-clipboard-client" : wxClipboardClient^ GetClipboardClient();
@ "get-clipboard-string" : nstring GetClipboardString(long);
@ "get-clipboard-data" : nstring/makeSizedString[1] GetClipboardData(string,-long*,long);

@CONSTANT "wx:the-clipboard" : wxClipboard^ wxTheClipboard

@END


@MACRO setStringSize[cn] = if (SCHEME_STRINGP(v)) (*x<cn>) = SCHEME_STRTAG_VAL(v);
@MACRO identity = {x}
@MACRO XrNULL = return NULL;

@MACRO sbString = str

@CLASSBASE wxClipboardClient "wx:clipboard-client" : "wx:object"

@CREATOR ()

@ V "being-replaced" : void BeingReplaced();
@ V "get-data" : nstring GetData(string,-long*); : //setStringSize[1] : : : XrNULL

@ m "add-type" : void AddType(string);
@ m "get-types" : Scheme_Object*/identity//sbString GetTypes();

@END


#if 0

@CLASSBASE wxTypeTree "wx:type-tree" : "wx:list"

@CREATOR ();

@ "add-type" : void AddType(int,int,string);

@CONSTANT "wx:const-type-any" : int wxTYPE_ANY
@CONSTANT "wx:const-type-object" : int wxTYPE_OBJECT
@CONSTANT "wx:const-type-window" : int wxTYPE_WINDOW
@CONSTANT "wx:const-type-dialog-box" : int wxTYPE_DIALOG_BOX
@CONSTANT "wx:const-type-item" : int wxTYPE_ITEM
@CONSTANT "wx:const-type-panel" : int wxTYPE_PANEL
@CONSTANT "wx:const-type-canvas" : int wxTYPE_CANVAS
@CONSTANT "wx:const-type-text-window" : int wxTYPE_TEXT_WINDOW
@CONSTANT "wx:const-type-frame" : int wxTYPE_FRAME
@CONSTANT "wx:const-type-button" : int wxTYPE_BUTTON
@CONSTANT "wx:const-type-text" : int wxTYPE_TEXT
@CONSTANT "wx:const-type-message" : int wxTYPE_MESSAGE
@CONSTANT "wx:const-type-choice" : int wxTYPE_CHOICE
@CONSTANT "wx:const-type-list-box" : int wxTYPE_LIST_BOX
@CONSTANT "wx:const-type-slider" : int wxTYPE_SLIDER
@CONSTANT "wx:const-type-check-box" : int wxTYPE_CHECK_BOX
@CONSTANT "wx:const-type-menu" : int wxTYPE_MENU
@CONSTANT "wx:const-type-menu-bar" : int wxTYPE_MENU_BAR
@CONSTANT "wx:const-type-multi-text" : int wxTYPE_MULTI_TEXT
@CONSTANT "wx:const-type-radio-box" : int wxTYPE_RADIO_BOX
@CONSTANT "wx:const-type-group-box" : int wxTYPE_GROUP_BOX
@CONSTANT "wx:const-type-guage" : int wxTYPE_GAUGE
@CONSTANT "wx:const-type-scroll-box" : int wxTYPE_SCROLL_BAR  ## NEWEST_TYPES
@CONSTANT "wx:const-type-virt-list-box" : int wxTYPE_VIRT_LIST_BOX ## NEWEST_TYPES
@CONSTANT "wx:const-type-event" : int wxTYPE_EVENT
@CONSTANT "wx:const-type-dc" : int wxTYPE_DC
@CONSTANT "wx:const-type-dc-canvas" : int wxTYPE_DC_CANVAS
@CONSTANT "wx:const-type-dc-postscript" : int wxTYPE_DC_POSTSCRIPT
@CONSTANT "wx:const-type-dc-printer" : int wxTYPE_DC_PRINTER
@CONSTANT "wx:const-type-dc-metafile" : int wxTYPE_DC_METAFILE
@CONSTANT "wx:const-type-dc-memory" : int wxTYPE_DC_MEMORY
@CONSTANT "wx:const-type-pen" : int wxTYPE_PEN
@CONSTANT "wx:const-type-brush" : int wxTYPE_BRUSH
@CONSTANT "wx:const-type-font" : int wxTYPE_FONT
@CONSTANT "wx:const-type-icon" : int wxTYPE_ICON
@CONSTANT "wx:const-type-bitmap" : int wxTYPE_BITMAP
@CONSTANT "wx:const-type-metafile" : int wxTYPE_METAFILE
@CONSTANT "wx:const-type-timer" : int wxTYPE_TIMER
@CONSTANT "wx:const-type-colour" : int wxTYPE_COLOUR
@CONSTANT "wx:const-type-colourmap" : int wxTYPE_COLOURMAP
@CONSTANT "wx:const-type-cusor" : int wxTYPE_CURSOR
@CONSTANT "wx:const-type-dde-cliebt" : int wxTYPE_DDE_CLIENT
@CONSTANT "wx:const-type-dde-server" : int wxTYPE_DDE_SERVER
@CONSTANT "wx:const-type-dde-connection" : int wxTYPE_DDE_CONNECTION
@CONSTANT "wx:const-type-help-instance" : int wxTYPE_HELP_INSTANCE
@CONSTANT "wx:const-type-list" : int wxTYPE_LIST
@CONSTANT "wx:const-type-string-list" : int wxTYPE_STRING_LIST
@CONSTANT "wx:const-type-hash-table" : int wxTYPE_HASH_TABLE
@CONSTANT "wx:const-type-node" : int wxTYPE_NODE
@CONSTANT "wx:const-type-app" : int wxTYPE_APP
@CONSTANT "wx:const-type-enhanced-dialog" : int wxTYPE_ENHANCED_DIALOG
@CONSTANT "wx:const-type-toolbar" : int wxTYPE_TOOLBAR  ## NEWEST_TYPES
@CONSTANT "wx:const-type-buttonbar" : int wxTYPE_BUTTONBAR  ## NEWEST_TYPES
@CONSTANT "wx:const-type-user" : int wxTYPE_USER


@END

@GLOBAL wxsTypesGlobal

@ "wx:get-type-name" : string wxGetTypeName(int);

@END


#endif
