
@INCLUDE prefix.xci

#include "wx_utils.h"
#include "wx_dialg.h"
#include "wx_cmdlg.h"
#include "wx_timer.h"
#include "wx_dcps.h"
#include "wx_main.h"
#ifdef wx_xt
#define Uses_wxPrintSetup
#include "wx_types.h"
#include "wx_print.h"
#endif

#ifdef wx_msw
#ifdef _MSC_VER
#include <direct.h>
#else
#include <dir.h>
#endif
#endif

#if USE_METAFILE
#include "wx_mf.h"
#endif

@INCLUDE wxs.xci

@HEADER

#define USE_PRINTER 1

#define wxSetPrintPaperName wxThePrintSetupData->SetPaperName
#define wxGetPrintPaperName wxThePrintSetupData->GetPaperName

extern Bool wxSchemeYield(void *sema);

extern void wxFlushDisplay(void);

#ifdef wx_xt
#define NO_XT 0
#else
#define NO_XT 1
#endif

#ifdef wx_x
#define FILE_SEL_DEF_PATTERN "*"
#else
#define FILE_SEL_DEF_PATTERN "*.*"
#endif

#define BUF_SIZE 1000
static char buffer[BUF_SIZE];

static char *wxGetHostName_Scheme(void)
{
  if (!wxGetHostName(buffer, BUF_SIZE))
    return NULL;
  
  return buffer;
}
static char *wxGet_Scheme(Bool (*f)(char *, int))
{
  if (!f(buffer, BUF_SIZE))
    return NULL;
  else
    return buffer;
}

static char *wxExpandPath_Scheme(char *path)
{
  return wxExpandPath(buffer, (const char *)path);
}

static char *wxGetEmailAddress_Scheme(void)
{
  return wxGet_Scheme(wxGetEmailAddress);
}

static char *wxGetUserId_Scheme(void)
{
  return wxGet_Scheme(wxGetUserId);
}

static char *wxGetUserName_Scheme(void)
{
  return wxGet_Scheme(wxGetUserName);
}

static char *wxGetHomeDir_Scheme(void)
{
#if defined(wx_msw) || defined(wx_mac)
  return NULL;
#else
  return wxGetHomeDir(buffer);
#endif
}

static char *wxStripMenuCodes_Scheme(char *in)
{
  static char *buffer = NULL;
  static long buflen = 0;
  long len;

  len = strlen(in);
  if (buflen <= len) {
    if (buffer)
      delete[] buffer;
    buflen = 2 * len + 1;
    buffer = new char[buflen];
  }

  wxStripMenuCodes(in, buffer);
  return buffer;
}

#if !defined(wx_x) || defined(wx_xt)
Bool wxSetDisplay(char *)
{
  return FALSE;
}

char *wxGetDisplayName(void)
{
  return NULL;
}
#endif

#ifdef wx_xt
extern void wxBell(void);
#endif

@GLOBAL wxsGlobal

// @ "wx:dir-exists?" : bool DirExists(string);
// @ "wx:file-exists?" : bool FileExists(string);
// @ "wx:file-name-from-path" : string FileNameFromPath(string);
// @ "wx:is-absolute-path?" : bool IsAbsolutePath(string);
// @ "wx:path-only" : string PathOnly(string);
@ "wx:concat-files" : bool wxConcatFiles(pathname,pathname,pathname);
// @ "wx:expand-path" : string wxExpandPath_Scheme(string);
// @ "wx:copy-file" : bool wxCopyFile(pathname,pathname);
// @ "wx:get-working-directory" : string wxGetWorkingDirectory();
// @ "wx:set-working-directory" : void wxSetWorkingDirectory(string);
@ "wx:get-temp-file-name" : string wxGetTempFileName(string);
// @ "wx:is-wild?" : bool wxIsWild(string);
// @ "wx:match-wild?" : bool wxMatchWild(string,string,bool);
// @ "wx:mkdir" : bool wxMkdir(string);
// Doesn't exist? @ "wx:rmdir" : bool wxRmdir(string);
// @ "wx:remove-file" : bool wxRemoveFile(string);
// @ "wx:rename-file" : bool wxRenameFile(string,string);
// @ "wx:get-user-home" : string wxGetUserHome(ncstring=NULL);

@ "wx:get-host-name" : nstring wxGetHostName_Scheme();
// @ "wx:get-home-dir" : string wxGetHomeDir_Scheme();
@ "wx:get-email-address" : nstring wxGetEmailAddress_Scheme();
@ "wx:get-user-id" : nstring wxGetUserId_Scheme();
@ "wx:get-user-name" : nstring wxGetUserName_Scheme();

@ "wx:string-match?" : bool StringMatch(string,string,bool=TRUE,bool=FALSE);

@ "wx:set-display" : bool wxSetDisplay(nstring);
@ "wx:get-display-name" : nstring wxGetDisplayName();

// @ "wx:exit" : void wxExit();

@ "wx:file-selector" : string wxFileSelector(string,nstring=NULL,nstring=NULL,nstring=NULL,string=FILE_SEL_DEF_PATTERN,int=0,wxWindow^=NULL,int=-1,int=-1);
@ "wx:message-box" : int wxMessageBox(string, string="Message",int=wxOK|wxCENTER,wxWindow^=NULL,int=-1,int=-1);
@ "wx:get-text-from-user" : string wxGetTextFromUser(string,string="Input text",string="",wxWindow^=NULL,int=-1,int=-1,bool=TRUE);

@SET TYPE = string
@SET NOTEST = 1
@SET NULLTERM = 1
@INCLUDE list.xci

static int *__CopyIntArray(Scheme_Object *vec, Scheme_Object *list_for_count)
{
  int c = scheme_proper_list_length(list_for_count);
  int *r = new int[c];
  int i;
  Scheme_Object **a;
 
  if (c < 0)
   scheme_wrong_type("wx:get-multiple-choice", "proper list", -1, 0, &list_for_count);

  if (!SCHEME_VECTORP(vec))
    scheme_wrong_type("wx:get-multiple-choice", "vector", -1, 0, &vec);

  if (SCHEME_VEC_SIZE(vec) != c)
    scheme_signal_error("wx:get-multiple-choice: result array is size %d "
			"but there are %d choices",
			SCHEME_VEC_SIZE(vec), c);

  a = SCHEME_VEC_ELS(vec);

  for (i = 0; i < c; i++)
    r[i] = objscheme_unbundle_integer(a[i], "wx:get-multiple-choice");

  return r;
}

static void __CopyBackIntArray(int count, Scheme_Object *vec, int *r)
{
  int i;
  Scheme_Object **a;
  
  a = SCHEME_VEC_ELS(vec);
  for (i = 0; i < count; i++)
    a[i] = objscheme_bundle_int(r[i]);

  delete[] r;
}

@MACRO ubMutArray = __CopyIntArray({x}, p[2])
@MACRO mutArrayFinish = __CopyBackIntArray(r, p[4], x5);

@MACRO checkSameLength[n.m.who] = if (scheme_proper_list_length(p[<n>]) != scheme_proper_list_length(p[<m>])) { scheme_signal_error("%s: choice and data lists are different lengths", <who>); }

@ "wx:get-multiple-choice" : int wxGetMultipleChoice(string,string,-int,string[]/bList/ubList/cList,int,int[]//ubMutArray,wxWindow^=NULL,int=-1,int=-1,bool=TRUE,int=150,int=200); : /// : /glueListSet[string.2.3.2."wx:get-multiple-choice"]/glueCleanup[3]|mutArrayFinish/

@ "wx:get-single-choice" : string wxGetSingleChoice(string,string,-int,string[]/bList/ubList/cList,wxWindow^=NULL,int=-1,int=-1,bool=TRUE,int=150,int=200); : /// : /glueListSet[string.2.3.2."wx:get-single-choice"]/glueCleanup[3]/
@ "wx:get-single-choice-index" : int wxGetSingleChoiceIndex(string,string,-int,string[]/bList/ubList/cList,wxWindow^=NULL,int=-1,int=-1,bool=TRUE,int=150,int=200); : /// : /glueListSet[string.2.3.2."wx:get-single-choice-index"]/glueCleanup[3]/
@ "wx:get-single-choice-data" : string wxGetSingleChoiceData(string,string,-int,string[]/bList/ubList/cList,string[]/bList/ubList/cList,wxWindow^=NULL,int=-1,int=-1,bool=TRUE,int=150,int=200); : /// : /checkSameLength[2.3."wx:get-single-choice-data"]|glueListSet[string.2.3.2."wx:get-single-choice-data"]|glueListSet[string.3.4.2."wx:get-single-choice-data"]/glueCleanup[3] | glueCleanup[4] /

@ "wx:colour-display?" : bool wxColourDisplay();
@ "wx:display-depth" : int wxDisplayDepth();

#if !USE_METAFILE
#define wxMakeMetaFilePlaceable(a,b,c,d,e,f) TRUE
#endif
@ "wx:make-meta-file-placeable" : bool wxMakeMetaFilePlaceable(string,float,float,float,float,float);

@ "wx:set-cursor" : void wxSetCursor(wxCursor!); : : /CHECKVOIDABLEOK[0]

@ "wx:get-printer-command" : string wxGetPrinterCommand(); ## USE_PRINTER
@ "wx:get-printer-file" : string wxGetPrinterFile(); ## USE_PRINTER
@ "wx:get-printer-preview-command" : string wxGetPrintPreviewCommand(); ## USE_PRINTER
@ "wx:get-printer-mode" : int wxGetPrinterMode(); ## USE_PRINTER
@ "wx:get-printer-orientation" : int wxGetPrinterOrientation(); ## USE_PRINTER
@ "wx:get-printer-options" : string wxGetPrinterOptions(); ## USE_PRINTER
@ "wx:get-printer-scaling" : void wxGetPrinterScaling(float*,float*); ## USE_PRINTER
@ "wx:get-printer-translation" : void wxGetPrinterTranslation(float*,float*); ## USE_PRINTER
@ "wx:get-print-paper-name" : nstring wxGetPrintPaperName(); ## USE_PRINTER
@ "wx:get-afm-path" : nstring wxGetAFMPath(); ## USE_PRINTER
@ "wx:get-post-script-level-2" : bool wxGetLevel2Ok(); ## USE_PRINTER

@ "wx:set-printer-command" : void wxSetPrinterCommand(string); ## USE_PRINTER
@ "wx:set-printer-file" : void wxSetPrinterFile(pathname); ## USE_PRINTER
@ "wx:set-printer-preview-command" : void wxSetPrintPreviewCommand(string);  ## USE_PRINTER
@ "wx:set-printer-mode" : void wxSetPrinterMode(int); ## USE_PRINTER
@ "wx:set-printer-orientation" : void wxSetPrinterOrientation(int); ## USE_PRINTER
@ "wx:set-printer-options" : void wxSetPrinterOptions(string); ## USE_PRINTER
@ "wx:set-printer-scaling" : void wxSetPrinterScaling(nnfloat,nnfloat); ## USE_PRINTER
@ "wx:set-printer-translation" : void wxSetPrinterTranslation(float,float); ## USE_PRINTER
@ "wx:set-print-paper-name" : void wxSetPrintPaperName(nstring); ## USE_PRINTER
@ "wx:set-afm-path" : void wxSetAFMPath(nstring); ## USE_PRINTER
@ "wx:set-post-script-level-2" : void wxSetLevel2Ok(bool); ## USE_PRINTER

// @ "wx:new-id" : long wxNewId(); ## NO_XT
// @ "wx:register-id" : void wxRegisterId(long); ## NO_XT

@ "wx:begin-busy-cursor" : void wxBeginBusyCursor(wxCursor!=wxHOURGLASS_CURSOR); : : /CHECKOK[0."wx:begin-busy-cursor"]
@ "wx:is-busy?" : bool wxIsBusy();
@ "wx:end-busy-cursor" : void wxEndBusyCursor();
@ "wx:bell" : void wxBell();
@ "wx:display-size" : void wxDisplaySize(int*,int*);

// @ "wx:error" : void wxError(string,string="wxWindows Internal Error");
// @ "wx:fatal-error" : void wxFatalError(string,string="wxWindows Fatal Error");

// @ "wx:execute" : void wxExecute(string,bool=FALSE); ## !defined(wx_mac)
// @ "wx:execute" : void wxExecute(string[]/bList/ubList/cList,bool=FALSE); : /// : /glueUncountedListSet[string.0.0."wx:execute"]/glueCleanup[0]/ ## !defined(wx_mac)
// @ "wx:shell" : void wxShell(string); ## !defined(wx_mac)

// @ "wx:find-first-file" : string wxFindFirstFile(cstring, int=0);
// @ "wx:find-next-file" : nstring wxFindNextFile();

// @ "wx:find-menu-item-id" : int wxFindMenuItemId(wxFrame!,string,string);
@ "wx:find-window-by-label" : wxWindow^ wxFindWindowByLabel(string,wxWindow^=NULL);
@ "wx:find-window-by-name" : wxWindow^ wxFindWindowByName(string,wxWindow^=NULL);
@ "wx:strip-menu-codes" : string wxStripMenuCodes_Scheme(string);

// @ "wx:get-elapsed-time" : long wxGetElapsedTime(bool=TRUE);  ## NO_XT
// @ "wx:sleep" : void wxSleep(int);
// @ "wx:start-timer" : void wxStartTimer(); ## NO_XT

// @ "wx:sub-type?" : bool wxSubType(int,int);

// @ "wx:to-lower" : char wxToLower(char);
// @ "wx:to-upper" : char wxToUpper(char);

@ "wx:get-free-memory" : long wxGetFreeMemory();
// @ "wx:get-os-version" : int wxGetOsVersion(int?,int?);

@ "wx:get-resource" : bool wxGetResource(string,string,string*,nstring=NULL); <> string
@ "wx:get-resource" : bool wxGetResource(string,string,long*,nstring=NULL); <> number
@ "wx:write-resource" : bool wxWriteResource(string,string,string,nstring=NULL); <> string
@ "wx:write-resource" : bool wxWriteResource(string,string,long,nstring=NULL); <> number

@MACRO BundleVoidStar = (void *){x}
@MACRO spSema = semaphore

@ "wx:yield" : bool wxSchemeYield(void[]=NULL//BundleVoidStar///spSema);
@ "wx:flush-display" : void wxFlushDisplay();

@CONSTANT "wx:const-ok" : int wxOK
@CONSTANT "wx:const-yes-no" : int wxYES_NO
@CONSTANT "wx:const-cancel" : int wxCANCEL
@CONSTANT "wx:const-yes" : int wxYES
@CONSTANT "wx:const-no" : int wxNO

@CONSTANT "wx:const-icon-exclamation" : int wxICON_EXCLAMATION
@CONSTANT "wx:const-icon-hand" : int wxICON_HAND
@CONSTANT "wx:const-icon-question" : int wxICON_QUESTION
@CONSTANT "wx:const-icon-information" : int wxICON_INFORMATION

@CONSTANT "wx:const-icon-stop" : int wxICON_STOP
@CONSTANT "wx:const-icon-asterisk" : int wxICON_ASTERISK
@CONSTANT "wx:const-icon-mask" : int wxICON_MASK

@CONSTANT "wx:const-center" : int wxCENTER
@CONSTANT "wx:const-centre" : int wxCENTRE

// @CONSTANT "wx:const-file" : int wxFILE ## NO_XT
// @CONSTANT "wx:const-dir" : int wxDIR

@CONSTANT "wx:const-ps-portrait" : int PS_PORTRAIT
@CONSTANT "wx:const-ps-landscape" : int PS_LANDSCAPE
 
@CONSTANT "wx:const-ps-preview" : int PS_PREVIEW
@CONSTANT "wx:const-ps-file" : int PS_FILE
@CONSTANT "wx:const-ps-printer" : int PS_PRINTER
 
@CONSTANT "wx:const-open" : int wxOPEN
@CONSTANT "wx:const-save" : int wxSAVE
@CONSTANT "wx:const-overwrite-prompt" : int wxOVERWRITE_PROMPT
@CONSTANT "wx:const-hide-readonly" : int wxHIDE_READONLY

@CONSTANT "wx:hourglass-cursor" : wxCursor! wxHOURGLASS_CURSOR

@END
