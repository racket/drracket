
@INCLUDE prefix.xci

#include "wx_obj.h"
#include "wx_win.h"
#include "wx_timer.h"
#include "wx_types.h"
#include "wx_stdev.h"
#include "wx_dc.h"
#include "wx_dcps.h"
#include "wx_clipb.h"

@INCLUDE wxs.xci

@HEADER

#if !defined(wx_mac)
#define NEWEST_TYPES 1
#else
#define NEWEST_TYPES 0
#endif

@MACRO rFALSE = return FALSE;

@INCLUDE wxs_espc.xci

@CLASSBASE wxTimer "timer" : "object"

@CREATOR (); : : /CHECKEVENTSPACE[METHODNAME("timer%","initialization")]

@ "interval" : int Interval();
@ v "notify" : void Notify();
@ "start" : void Start(rint[0|1000000000],bool=FALSE); : : : : rFALSE
@ "stop" : void Stop();

@END


void AddType(wxClipboardClient *c, char *s) 
{ 
  c->formats->Add(s); 
}

Scheme_Object *GetTypes(wxClipboardClient *c)
{
  wxNode *n;
  Scheme_Object *first = scheme_null, *last = NULL;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH(0, n);
  VAR_STACK_PUSH(1, first);
  VAR_STACK_PUSH(2, last);

  n = WITH_VAR_STACK(c->formats->First());
  for (; n; n = WITH_VAR_STACK(n->Next())) {
    Scheme_Object *p, *s;
    
    s = WITH_VAR_STACK(scheme_make_string((char *)n->Data()));
    p = WITH_VAR_STACK(scheme_make_pair(s, scheme_null));
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;
  }
  
  return first;
}

@MACRO makeSizedString[i] = (r ? scheme_make_sized_string(r, _x<i>, 1) : XC_SCHEME_NULL)

@CLASSBASE wxClipboard "clipboard" : "object"
@INTERFACE "clipboard"

@ "set-clipboard-client" : void SetClipboardClient(wxClipboardClient!,ExactLong);
@ "set-clipboard-string" : void SetClipboardString(string,ExactLong);
@ "get-clipboard-client" : wxClipboardClient^ GetClipboardClient();
@ "get-clipboard-string" : nstring GetClipboardString(ExactLong);
@ "get-clipboard-data" : nstring/makeSizedString[1] GetClipboardData(string,-long*,ExactLong);

@CONSTANT "the-clipboard" : wxClipboard^ wxTheClipboard

@END


@MACRO setStringSize[cn] = if (SCHEME_STRINGP(v)) (*x<cn>) = SCHEME_STRTAG_VAL(v);
@MACRO identity = {x}
@MACRO XrNULL = return NULL;

@MACRO sbString = str

@CLASSBASE wxClipboardClient "clipboard-client" : "object"

@CREATOR ()

@ V "on-replaced" : void BeingReplaced();
@ V "get-data" : nstring GetData(string,-long*); : //setStringSize[1] : : : XrNULL

@ m "add-type" : void AddType(string);
@ m "get-types" : Scheme_Object*/identity//sbString GetTypes();

@END


@BEGINSYMBOLS psMode > ONE > PRED
@SYM "preview" : PS_PREVIEW
@SYM "file" : PS_FILE
@SYM "printer" : PS_PRINTER
@ENDSYMBOLS

@BEGINSYMBOLS psOrientation > ONE > PRED
@SYM "portrait" : PS_PORTRAIT
@SYM "landscape" : PS_LANDSCAPE
@ENDSYMBOLS

#ifdef wx_xt
void check_ps_mode(int, Scheme_Object *) {}
#else
void check_ps_mode(int v, Scheme_Object *p)
{
  if ((v == PS_PREVIEW) || (v == PS_PRINTER)) {
    scheme_arg_mismatch(METHODNAME("ps-setup%","set-mode"), 
	"only file mode is allowed for this platform, given: ",
	p);
  }
}
#endif

@MACRO checkPSMode[cn] = WITH_VAR_STACK(check_ps_mode(x<cn>, p[<cn>]));

@CLASSBASE wxPrintSetupData "ps-setup" : "object"

@CREATOR ()

@ "get-command" : string GetPrinterCommand();
@ "get-file" : string GetPrinterFile();
@ "get-preview-command" : string GetPrintPreviewCommand();
@ "get-mode" : SYM[psMode] GetPrinterMode();
@ "get-orientation" : SYM[psOrientation] GetPrinterOrientation();
@ "get-options" : string GetPrinterOptions();
@ "get-scaling" : void GetPrinterScaling(nnfloat*,nnfloat*);
@ "get-translation" : void GetPrinterTranslation(float*,float*);
@ "get-paper-name" : nstring GetPaperName();
@ "get-afm-path" : nstring GetAFMPath();
@ "get-level-2" : bool GetLevel2();
@ "get-editor-margin" : void GetEditorMargin(nnlong*,nnlong*);

@ "set-command" : void SetPrinterCommand(string);
@ "set-file" : void SetPrinterFile(npathname);
@ "set-preview-command" : void SetPrintPreviewCommand(string); 
@ "set-mode" : void SetPrinterMode(SYM[psMode]); : : /checkPSMode[0]
@ "set-orientation" : void SetPrinterOrientation(SYM[psOrientation]);
@ "set-options" : void SetPrinterOptions(string);
@ "set-scaling" : void SetPrinterScaling(nnfloat,nnfloat);
@ "set-translation" : void SetPrinterTranslation(float,float);
@ "set-paper-name" : void SetPaperName(nstring);
@ "set-afm-path" : void SetAFMPath(nstring);
@ "set-level-2" : void SetLevel2(bool);
@ "set-editor-margin" : void SetEditorMargin(nnlong,nnlong);

@ "copy-from" : void copy(wxPrintSetupData!);

@END



#if 0

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

@CLASSBASE baseMetaFile "meta-file" : "object"
@INTERFACE "meta-file"

// @CREATOR (string=NULL);

@ "ok?" : bool Ok();
@ "play" : void Play(wxDC!);
@ "set-clipboard" : bool SetClipboard(int=0,int=0);

@END

#endif
