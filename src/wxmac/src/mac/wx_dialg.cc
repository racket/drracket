///////////////////////////////////////////////////////////////////////////////
// File:	wx_dialg.cc
// Purpose:	wxDialogBox (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

//-----------------------------------------------------------------------------
// Dialog box - like panel but doesn't need a frame, and is modal or non-modal
//-----------------------------------------------------------------------------

static const char sccsid[] = "%W% %G%";
#if 0
#ifdef GUSI
#include "TFileSpec.h"
#endif
#endif
#include "wx_dialg.h"
#include "wx_panel.h"
#include "wx_utils.h"
#include "wx_messg.h"
#include "wx_main.h"
#include "wx_buttn.h"
#include <StandardFile.h>
#include <TextUtils.h>
#include <Strings.h>
#ifdef PYLIB
extern "C" {
#include "nfullpath.h"
}
#endif

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300

extern wxApp* wxTheApp;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

static int IsUnshown(void *data)
{
	return !((wxDialogBox *)data)->IsShown();
}

//-----------------------------------------------------------------------------
void wxDialogBox::Show(Bool show)
{
	cFrame->Show(show);
	if (show) {
  	  if (cFrame->IsModal()) {
		wxWindow *oldm = wxGetModalWindow(ContextWindow());
		wxPutModalWindow(ContextWindow(), cFrame);
		
		wxDispatchEventsUntil(IsUnshown, (void *)this);

		wxPutModalWindow(ContextWindow(), oldm);
	  }
    }
}

Bool wxDialogBox::IsShown(void)
{
  return cFrame->IsShown();
}

void wxDialogBox::SetSize(int x, int y, int width, int height, int flags)
{
    if (!(flags & 0x70)) {
      int w, h, cw, ch;
      
      cFrame->GetSize(&w, &h);
      cFrame->GetClientSize(&cw, &ch);
      
	  cFrame->SetSize(x, y, width + (w - cw), height + (h - ch), flags);
	} else
	  wxWindow::SetSize(x, y, width, height, flags);
}


// Default resizing behaviour - if only ONE subwindow,
// resize to client rectangle size
void wxDialogBox::OnSize(int x, int y)
{
  // Search for a child which is a subwindow, not another frame.
  wxWindow *child = NULL;
  // Count the number of _subwindow_ children
  int noChildren = 0;
  for(wxChildNode *node = GetChildren()->First(); node; node = node->Next())
  {
    wxWindow *win = (wxWindow *)node->Data();
    WXTYPE winType = win->__type;

    if (wxSubType(winType, wxTYPE_PANEL) ||
        wxSubType(winType, wxTYPE_TEXT_WINDOW) ||
        wxSubType(winType, wxTYPE_CANVAS))
    {
      child = win;
      noChildren ++;
    }
  }
  if (!child || (noChildren > 1))
    return;

  int client_x, client_y;

  GetClientSize(&client_x, &client_y);
  child->SetSize(0, 0, client_x, client_y, 0x70);
}

//-----------------------------------------------------------------------------
Bool wxDialogBox::IsModal(void)
{
	return cFrame->IsModal();
}

//-----------------------------------------------------------------------------
void wxDialogBox::ShowModal(void)
{
	Show(TRUE);
	if (!cFrame->IsModal()) {
  	  while (IsShown())
		wxTheApp->MainLoop();
	}
}

void wxDialogBox::Fit(void)
{
	int x, y;

	wxPanel::Fit();
	wxPanel::GetSize(&x, &y);
	// cFrame->SetClientSize(x, y);
}

//-----------------------------------------------------------------------------
int wxDialogBox::GetButtonPressed(void)
{
	return cButtonPressed;
}

//-----------------------------------------------------------------------------
void wxDialogBox::SetButtonPressed(int buttonPressed)
{
	cButtonPressed = buttonPressed;
}

//-----------------------------------------------------------------------------
Bool wxDialogBox::OnClose(void)
{
	Bool result;
	if (IsModal())
	{
		Show(FALSE);
		result = FALSE; // don't want dialog frame deleted
	}
	else
	{
		result = TRUE;
	}

	if (result)
	  return cFrame->OnClose();

	return result;
}

//=============================================================================
//
// Message centring code
//
//=============================================================================

//-----------------------------------------------------------------------------
void wxSplitMessage(char* message, wxList* messageList, wxPanel* panel)
{
  char* copyMessage = copystring(message);
  int i = 0;
  int len = strlen(copyMessage);
  char* currentMessage = copyMessage;
  while (i < len)
  {
    while ((i < len) && (copyMessage[i] != '\n')) i ++;
    if (i < len) copyMessage[i] = 0;
    wxMessage* mess = new wxMessage(panel, currentMessage);
    messageList->Append(mess);
    panel->NewLine();

    currentMessage = copyMessage + i + 1;
  }
  delete[] copyMessage;
}

//-----------------------------------------------------------------------------
void wxCentreMessage(wxList* messageList)
{
  // Do the message centering
  wxNode* node = messageList->First();
  while (node)
  {
    wxMessage* mess = (wxMessage*)node->Data();
    mess->Centre();
    node = node->Next();
  }
}

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxDialogBox::wxDialogBox // Constructor (for dialog window)
	(
		wxWindow*	parentFrame,		// this is ignored, used to be wxFrame*
		char*		windowTitle,
		Bool		modal,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxPanel (new wxFrame(NULL, windowTitle, 
				  x, y,
				  width, height, style, windowName, objectType),
	              0, 0, width, height),
		cButtonPressed (0)
{
  cFrame = (wxFrame *)GetParent();
  cFrame->cDialogPanel = this;
  cFrame->MakeModal(modal);
  
  /* Set dialog panel to frame's client size: */
  int w, h;
  cFrame->GetClientSize(&w, &h);
  SetSize(-1, -1, w, h, 0x70);
  
  __type = wxTYPE_DIALOG_BOX;
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxDialogBox::~wxDialogBox()
{
	if (cFrame) {
		  wxTopLevelWindows(ContextWindow())->DeleteObject(cFrame);
		  cFrame = NULL;
	}
}

static wxDialogBox* wxGetDialogFromButton(wxButton& button);
static void wxOkButtonProc(wxButton& button, wxEvent& event);
static void wxCancelButtonProc(wxButton& button, wxEvent& event);
static void wxYesButtonProc(wxButton& button, wxEvent& event);
static void wxNoButtonProc(wxButton& button, wxEvent& event);

//-----------------------------------------------------------------------------
// Pop up a message box
//-----------------------------------------------------------------------------
int wxMessageBox(char* message, char* caption, long style,
                 wxWindow* parent, int x, int y)
{
	wxDialogBox* dialog = new wxDialogBox((wxFrame*)NULL, caption, TRUE, x, y);

//============================================
	wxPanel* dialogPanel = dialog;
	wxFont* theFont = new wxFont(12, wxSYSTEM, wxNORMAL, wxNORMAL);
	dialogPanel->SetLabelFont(theFont);
	dialogPanel->SetButtonFont(theFont);

//============================================
	wxPanel* messagePanel = new wxPanel(dialogPanel, -1, -1, -1, -1);
	Bool centre = ((style & wxCENTRE) == wxCENTRE);

	wxList messageList;
	wxSplitMessage(message, &messageList, messagePanel);
	messagePanel->Fit();
	if (centre)
	{
		wxCentreMessage(&messageList);
	}
	//dialogPanel->AdvanceCursor(messagePanel); // WCH: kludge

//============================================
	dialogPanel->NewLine();
	wxPanel* buttonPanel = new wxPanel(dialogPanel, -1, -1, -1, -1);

	wxButton* ok = NULL;
	wxButton* cancel = NULL;
	wxButton* yes = NULL;
	wxButton* no = NULL;

	if (style & wxYES_NO)
	{
		yes = new wxButton(buttonPanel, (wxFunction)&wxYesButtonProc, "Yes");
		no = new wxButton(buttonPanel, (wxFunction)&wxNoButtonProc, "No");
	}

	if (style & wxCANCEL)
	{
		cancel = new wxButton(buttonPanel, (wxFunction)&wxCancelButtonProc, "Cancel");
	}

	if (!(style & wxYES_NO) && (style & wxOK))
	{
		ok = new wxButton(buttonPanel, (wxFunction)&wxOkButtonProc, "OK");
	}

	if (ok)
		ok->SetDefault();
	else if (yes)
		yes->SetDefault();
	buttonPanel->Fit();
	buttonPanel->SetFocus();
	//dialogPanel->AdvanceCursor(messagePanel); // WCH: kludge

//============================================
	
	dialogPanel->Fit();
	dialog->Fit();

	messagePanel->Centre(wxHORIZONTAL);
	buttonPanel->Centre(wxHORIZONTAL);

	dialogPanel->Centre(wxBOTH);

//============================================
	
	dialog->Show(TRUE);
	int result = dialog->GetButtonPressed();
	delete dialog;

//============================================

	return result;
}

//=============================================================================
//
// Dialog button callback procedures
//
//=============================================================================

static wxDialogBox* wxGetDialogFromButton(wxButton& button)
{
	wxFrame *fr = button.GetRootFrame();
	wxDialogBox* dialog = fr->cDialogPanel;
	if (dialog == NULL) {
   		wxFatalError("No DialogBox found on RootFrame.");
   	}
   	return dialog;
}


//-----------------------------------------------------------------------------
static void wxOkButtonProc(wxButton& button, wxEvent& event)
{
//	wxDialogBox* dialog = (wxDialogBox*) button.GetRootFrame();
	wxDialogBox* dialog = wxGetDialogFromButton(button);

	dialog->SetButtonPressed(wxOK);
	dialog->Show(FALSE);
}

//-----------------------------------------------------------------------------
static void wxCancelButtonProc(wxButton& button, wxEvent& event)
{
//	wxDialogBox* dialog = (wxDialogBox*) button.GetRootFrame();
	wxDialogBox* dialog = wxGetDialogFromButton(button);

	dialog->SetButtonPressed(wxCANCEL);
	dialog->Show(FALSE);
}

//-----------------------------------------------------------------------------
static void wxYesButtonProc(wxButton& button, wxEvent& event)
{
//	wxDialogBox* dialog = (wxDialogBox*) button.GetRootFrame();
	wxDialogBox* dialog = wxGetDialogFromButton(button);

	dialog->SetButtonPressed(wxYES);
	dialog->Show(FALSE);
}

//-----------------------------------------------------------------------------
static void wxNoButtonProc(wxButton& button, wxEvent& event)
{
//	wxDialogBox* dialog = (wxDialogBox*) button.GetRootFrame();
	wxDialogBox* dialog = wxGetDialogFromButton(button);

	dialog->SetButtonPressed(wxNO);
	dialog->Show(FALSE);
}

extern "C" {
 extern char *scheme_build_mac_filename(FSSpec *f, int);
};

//= T.P. ==============================================================================
char *wxFileSelector(char *message, char *default_path,
                     char *default_filename, char *default_extension,
                     char *wildcard, int flags,
                     wxWindow *parent, int x, int y)
{	
	StandardFileReply	rep;
	SFTypeList typeList = { 'TEXT' };
	char * name;
	short numTypes = -1; // all types
	Str255	p_prompt,p_defname;
	OSErr err;

#if 0
	if (flags & wxDIR_ONLY)
		return wxDirectorySelector(message, default_path, default_filename,
			default_extension, wildcard, flags, parent, x, y);
#endif
			
	if ((flags == 0) || (flags & wxOPEN))
	{	// get file
		::StandardGetFile( NULL, numTypes, typeList, &rep);	
	} else
	{	// put file
		if (message)
		{	
			strcpy((char *)p_prompt,message);
		} else 
		{
			strcpy((char *)p_prompt, "Save File Name");
		}
		C2PStr((char *)p_prompt);
		if (default_filename)
		{	strcpy((char *)p_defname, default_filename);
		} else 
		{
			strcpy((char *)p_defname, "");
		}
		C2PStr((char *)p_defname);
		::StandardPutFile( p_prompt, p_defname, &rep);
	}
	
	if (!rep.sfGood)
	  return NULL;
	
#if 1
	return scheme_build_mac_filename(&rep.sfFile, 0);
#else	
#ifdef GUSI
	char *rtnval = NULL;
	if (rep.sfGood)
	{	
		
		TFileSpec tFile;
		char *name;
		
		if (rep.sfFile.name)
		{
		  tFile = TFileSpec(rep.sfFile.vRefNum,
		                    rep.sfFile.parID,
		                    rep.sfFile.name, TRUE);
		  name = tFile.FullPath(); // destroyed on exit, try copystring
		  strcpy(wxBuffer, name);
		  rtnval = wxBuffer;
		}
    }
	return rtnval;
#elif defined(PYLIB)
	//int nfullpath(FSSpec *, char *);	/* Generate full path from fsspec */
	char *rtnval = NULL;
	if (rep.sfGood)
	{	
				
		if (rep.sfFile.name)
		{
		  nfullpath(&rep.sfFile, wxBuffer);
		  rtnval = wxBuffer;
		}
    }
	return rtnval;
#else
	if (rep.sfGood)
	{	
		/***** HELL, trying to get the default directory.. any hint?
		char msg[200];
				err = ::SetVol(0,CurDirStore);
				sprintf(msg,"File name:%s Parent-folder ID:%ld VolRefNum:%ld CurDir:%ld -- Err:%d --",
					rep.sfFile.name,rep.sfFile.parID,rep.sfFile.vRefNum,CurDirStore,err);
				wxError(msg);
		******/
		::p2cstr(rep.sfFile.name); 
		strcpy(wxBuffer, (char *)rep.sfFile.name);
		return wxBuffer;	
	} 
	return 0;
#endif
#endif
}

//---------------- New to Wxwindows - Select a Directory ----------------------

// This code barrowed from StandardGetFolder written by Paul Forrester (paulf@apple.com)
// and modified by kkirksey1@aol.com 
// converted to wxMac by Cecil Coupe
#include <String.h>
#include <Script.h>
#include <Aliases.h>
#include <StandardFile.h>
#include <LowMem.h>

// defined later
#define GetSFCurDir  LMGetCurDirStore

#define GetSFVRefNum  LMGetSFSaveDisk

static pascal Boolean MyCustomGetDirectoryFileFilter(  CInfoPBPtr  myPB, 
                                                Ptr         myDataPtr);

static void SetButtonTitle ( Handle    ButtonHdl, 
                      Str255    name, 
                      Rect      *ButtonRect);

static pascal short MyCustomGetDirectoryDlogHook( short        item, 
                                           DialogPtr    theDialog,
                                           Ptr          myDataPtr);

static char *StrCpy (char *s1, char *s2);

static char *StrCat (char *s1, char *s2);                                      

typedef struct {
	StandardFileReply	*sfr;
	int					wxflags;
} HookExtra, *HookExtraPtr;

/*=============================================================================+
|                               Resource IDs                                   |
+=============================================================================*/
#define rGetFolderButton            10
#define rGetFolderMessage           11
#define kFolderBit                  0x0010  
#define rGetFolderDialog            2008


/*=============================================================================+
|                             Global Variables                                 |
+=============================================================================*/
static  char    gCurrentSelectedFolder [256];

/*******************************************************************************
* MyCustomGetDirectoryFileFilter                                               *
*                                                                              *
*     This is the file filter passed to CustomGetFile. It passes folders only, *
*	  files only, or both depending on the flags in the HookExtra struct.      *
*     The default (flags == 0) is to pass everything, ie filter nothing.       *
*******************************************************************************/
static pascal Boolean
MyCustomGetDirectoryFileFilter( CInfoPBPtr  myPB, 
                                Ptr         myDataPtr )
{
	HookExtraPtr hp = (HookExtraPtr)myDataPtr;
	int result = 0;						// Boolean actually
	if (hp->wxflags & (wxDIR_ONLY | wxFILES_ONLY)) {
		if ((hp->wxflags & wxDIR_ONLY) && (myPB->hFileInfo.ioFlAttrib & ioDirMask))
			result = 1;
		if ((hp->wxflags & wxFILES_ONLY) && !(myPB->hFileInfo.ioFlAttrib & ioDirMask))
			result = 1;			
	} else {
		// nothing special asked for, return all files and dirs
		result = 1;
	}
	return !result;	// FALSE ==> display in dialog list
}

static FileFilterYDUPP ffupp = NewFileFilterYDProc(MyCustomGetDirectoryFileFilter);

/*******************************************************************************
* MyCustomGetDirectoryDlogHook                                                 *
*                                                                              *
*     This function lets us process item hits in the GetFolderDialog.  We're   *
*     only interested if the user hit the selectFolder button. We pass all     *
*     other item hits back to ModalDialog.                                     *
*******************************************************************************/

static pascal short MyCustomGetDirectoryDlogHook(  short       item, 
                                            DialogPtr   theDialog, 
                                            Ptr         myDataPtr )
{

    WindowPeek      dlgPeek;
    Str255          selectedName;
    CInfoPBRec      pb;
    short           MyCustomGetDirectoryDlogHook;
    OSErr           err;
    short           itemType;           
    Rect            itemRect;                                   
    Handle          itemHandle;
    Boolean         isAlias,
                    isFolder;
    StandardFileReply *mySFRPtr;

    
    
    /*-------------------------------------------------------------------------+
    | Set the return value to defualt to the item that was passed in.          |
    +-------------------------------------------------------------------------*/
    MyCustomGetDirectoryDlogHook = item;

    /*-------------------------------------------------------------------------+
    | CustomGet calls dialog hook for both main and subsidiary dialog boxes.   |
    | Make sure that dialog record indicates that this is the main GetFolder   |
    | dialog.                                                                  |
    +-------------------------------------------------------------------------*/
    dlgPeek = (WindowPeek)(theDialog);
    if ( (OSType)(dlgPeek->refCon) == sfMainDialogRefCon )
    {
        /*---------------------------------------------------------------------+
        | Get a handle to the select folder button, in case we need to change  |
        | the label.                                                           |
        +---------------------------------------------------------------------*/
        GetDialogItem(theDialog, rGetFolderButton, &itemType, &itemHandle, &itemRect);

        /*---------------------------------------------------------------------+
        | If this is the first time the dialog hook has been called...         |
        +---------------------------------------------------------------------*/
        if ( item == sfHookFirstCall )
        {
            /*-----------------------------------------------------------------+
            | Set the prompt displayed above the file list...                  |
            +-----------------------------------------------------------------*/
            GetDialogItem ( theDialog, rGetFolderMessage, &itemType, &itemHandle, 
                        &itemRect );
            mySFRPtr = ((HookExtraPtr)(myDataPtr))->sfr;
            SetIText ( itemHandle, mySFRPtr->sfFile.name );

            /*-----------------------------------------------------------------+
            | And the name of the currently selected folder in the select      |
            | folder button.                                                   |
            +-----------------------------------------------------------------*/
            pb.hFileInfo.ioCompletion = NULL;
            pb.hFileInfo.ioNamePtr = (StringPtr)selectedName;
            pb.hFileInfo.ioVRefNum = GetSFVRefNum();
            pb.hFileInfo.ioDirID = GetSFCurDir();
            pb.hFileInfo.ioFDirIndex = -1;
            err = PBGetCatInfo( &pb, FALSE);

            /*-----------------------------------------------------------------+
            | Note that this error return is important! When the dialog hook   |
            | is called for the first time, Super Boomerang (and possibly      |
            | Norton directory assistance aren't finished doing their          |
            | rebounting, so the values returned by GetSFVRefNum and           |
            | GetSFCurDir may not be valid, and hence PBGetCatInfo will return |
            | an error.  That one took me a while to figure out.               |
            +-----------------------------------------------------------------*/
            if ( err != noErr )
            {
                return (MyCustomGetDirectoryDlogHook);
            }
            
            GetDItem(theDialog, rGetFolderButton, &itemType, &itemHandle, 
                     &itemRect);
            SetButtonTitle( itemHandle, selectedName, &itemRect);
        }
        
        else
        {
            /*-----------------------------------------------------------------+
            | Cast myDataPtr back to a SFReply pointer.                        |
            +-----------------------------------------------------------------*/
            mySFRPtr = ((HookExtraPtr)(myDataPtr))->sfr;


            /*-----------------------------------------------------------------+
            | If the selected folder is an alias, resolve it. isFolder will    |
            | be set to true if a folder or aliased folder is selected.        |
            +-----------------------------------------------------------------*/
            ResolveAliasFile (&(mySFRPtr->sfFile), TRUE, &isFolder, &isAlias);
            if ( (isAlias) && (isFolder) )
                StrCpy( (char *)selectedName, (char *)mySFRPtr->sfFile.name );
                
            /*-----------------------------------------------------------------+
            | If the selected item is a folder or volume, just copy the name   |
            | into selectedName...                                             |
            +-----------------------------------------------------------------*/
            else if (( mySFRPtr->sfIsFolder) || (mySFRPtr->sfIsVolume) )
                StrCpy( (char *)selectedName, (char *)mySFRPtr->sfFile.name );

            /*-----------------------------------------------------------------+
            | Otherwise, copy the name of the selected item's parent directory |
            | into selectedName.                                               |
            +-----------------------------------------------------------------*/
            else
            {
                pb.hFileInfo.ioCompletion = NULL;
                pb.hFileInfo.ioNamePtr = (StringPtr)selectedName;
                pb.hFileInfo.ioVRefNum = mySFRPtr->sfFile.vRefNum;
                pb.hFileInfo.ioDirID = mySFRPtr->sfFile.parID;
                pb.hFileInfo.ioFDirIndex = -1;
                err = PBGetCatInfo( &pb, FALSE);
                if ( err != noErr)
                    return (MyCustomGetDirectoryDlogHook);
            }
            
            /*-----------------------------------------------------------------+
            | If the selected folder has changed since the last call to this   |
            | dialog hook function, re-draw the button with the new selected   |
            | folder name.                                                     |
            +-----------------------------------------------------------------*/
            if ( !EqualString( selectedName, (StringPtr)gCurrentSelectedFolder, 
                                FALSE, FALSE ) ) 
                SetButtonTitle(itemHandle, selectedName, &itemRect);

            /*-----------------------------------------------------------------+
            | If the user clicked the select folder button, force a cancel and |
            | set the sfGood field of the Reply record to true.                |
            +-----------------------------------------------------------------*/
            if (item == rGetFolderButton)
            {
                MyCustomGetDirectoryDlogHook = sfItemCancelButton;
                mySFRPtr->sfGood = TRUE;
            }
                
        }
    }
    
    return  (MyCustomGetDirectoryDlogHook );
}

static DlgHookYDUPP dlghookupp = NewDlgHookYDProc(MyCustomGetDirectoryDlogHook);

/*******************************************************************************
* SetButtonTitle                                                               *
*                                                                              *
*     Whenever the selected folder is changed, SetButtonTitle is called to     *
*     redraw the get folder button.  Pass it a handle to the button, the new   *
*     string to be drawn in the button, and a pointer to the rect the button   *
*     is drawn within.                                                         *
*******************************************************************************/
static void SetButtonTitle(    Handle      ButtonHdl, 
                        Str255      name, 
                        Rect        *ButtonRect )
{
    short   resultCode;
    short   width;
    char    TmpStr[ 256 ];

    StrCpy( gCurrentSelectedFolder, (char*) name );
    
    /*-------------------------------------------------------------------------+
    | Find the width left over in the button after drawing the word 'Select'   |
    | the quotation marks. Truncate the new name to this length.               |
    +-------------------------------------------------------------------------*/
    width = (ButtonRect->right - ButtonRect->left) -
            (StringWidth((StringPtr)"\pSelect \"\"") +
             CharWidth('J'));
    
    resultCode = TruncString(width, name, smTruncEnd );
    if ( resultCode < 0 );
    
    /*-------------------------------------------------------------------------+
    | Redraw the button.                                                       |
    +-------------------------------------------------------------------------*/
    sprintf( TmpStr, "Select \"%#s\"", name );
    c2pstr( TmpStr );
    SetCTitle((ControlHandle)(ButtonHdl), (StringPtr)TmpStr );
    ValidRect(ButtonRect);
}

/*******************************************************************************
* StrCpy                                                                       *
*                                                                              *
*     Just like strcpy except that it takes pascal strings as arguments.       *                                                                               *
*******************************************************************************/
static char *StrCpy( char *s1, char *s2 )
{
    /*-------------------------------------------------------------------------+
    | Copy The Length Byte                                                     |
    +-------------------------------------------------------------------------*/
    *s1 = *s2;

    /*-------------------------------------------------------------------------+
    | Copy The Rest.                                                           |
    +-------------------------------------------------------------------------*/
    return( (char *)memcpy( s1+1, s2+1, *s1 ) );
}



/*******************************************************************************
* StrCat                                                                       *
*                                                                              *
*     Just like strcat except that it takes pascal strings as arguments.       *                                                                               *
*******************************************************************************/
static char *StrCat( char *s1, char *s2 )
{
    int OriginalLen;

    OriginalLen = *s1;
    *s1 += *s2;

    return( (char *)memcpy( s1+OriginalLen+1, s2+1, *s2 ) );
}

//===============================================================================

#if 0

char *wxDirectorySelector(char *message, char *default_path,
                     char *default_filename, char *default_extension,
                     char *wildcard, int flags,
                     wxWindow *parent, int x, int y)
{	
	StandardFileReply	rep;
	SFTypeList typeList = { 'TEXT' };
	char * name;
	short numTypes = -1; // all types
	Str255	p_prompt,p_defname;
	OSErr err;
	Point where = {x, y};

	if ((flags == 0) || (flags & wxOPEN))
	{	
    	CInfoPBRec	pb;
    	HookExtra	extra_data;
    	extra_data.sfr = &rep;
    	extra_data.wxflags = flags;
    	
		CustomGetFile(ffupp, numTypes, typeList,
			&rep, 
			rGetFolderDialog, 
			where,
			dlghookupp, 
			NULL, 
			(short*)NULL,
			NULL, 
			(Ptr)&extra_data
    	);
	    /*-------------------------------------------------------------------------+
	    | Ok, now the reply record contains the volume reference number and the    |
	    | name of the selected folder/file. We need to use PBGetCatInfo to get the |    
	    | directory ID of the selected folder.                                     |
	    +-------------------------------------------------------------------------*/
	    pb.hFileInfo.ioCompletion = NULL;
	    pb.hFileInfo.ioNamePtr = rep.sfFile.name;
	    pb.hFileInfo.ioVRefNum = rep.sfFile.vRefNum;
	    pb.hFileInfo.ioFDirIndex = 0;
	    pb.hFileInfo.ioDirID = rep.sfFile.parID;
	
	    err = PBGetCatInfo( &pb, FALSE);
	    
	    /*-------------------------------------------------------------------------+
	    | Insert your error handler here. I couldn't think of one so I left it     |
	    | empty. Works fine without it.                                            |
	    +-------------------------------------------------------------------------*/
	    if (  err != noErr);
	    
	    if (pb.hFileInfo.ioFlAttrib & ioDirMask) {
	    	/*-------------------------------------------------------------------------+
	    	| Copy the directory ID of the selected folder to the sfFile field of the  |
	    	| SFReply record.                                                          |
	   	 	+-------------------------------------------------------------------------*/
	    	//rep.sfFile.parID = pb.dirInfo.ioDrDirID;
	    	rep.sfFile.name[0] += 1;
	    	rep.sfFile.name[rep.sfFile.name[0]] = ':';
	    }

	}

#ifdef GUSI
	char *rtnval = NULL;
	if (rep.sfGood)
	{	
		
		TFileSpec tFile;
		char *name;
		
		if (rep.sfFile.name)
		{
		  tFile = TFileSpec(rep.sfFile.vRefNum,
		                    rep.sfFile.parID,
		                    rep.sfFile.name, TRUE);
		  name = tFile.FullPath(); // destroyed on exit, try copystring
		  strcpy(wxBuffer, name);
		  rtnval = wxBuffer;
		}
    }
	return rtnval;
#elif defined(PYLIB)
	//int nfullpath(FSSpec *, char *);	/* Generate full path from fsspec */
	char *rtnval = NULL;
	if (rep.sfGood)
	{	
				
		if (rep.sfFile.name)
		{
		  nfullpath(&rep.sfFile, wxBuffer);
		  rtnval = wxBuffer;
		}
    }
	return rtnval;
#else
	if (rep.sfGood)
	{	
		/***** HELL, trying to get the default directory.. any hint?
		char msg[200];
				err = ::SetVol(0,CurDirStore);
				sprintf(msg,"File name:%s Parent-folder ID:%ld VolRefNum:%ld CurDir:%ld -- Err:%d --",
					rep.sfFile.name,rep.sfFile.parID,rep.sfFile.vRefNum,CurDirStore,err);
				wxError(msg);
		******/
		::p2cstr(rep.sfFile.name); 
		strcpy(wxBuffer, (char *)rep.sfFile.name);
		return wxBuffer;	
	} 
	return 0;
#endif 
}

#endif

