/*
 * File:	wxMacPPC.c
 * Purpose:	IPC primitives (Macintosh version)
 * Author:	Tomaso Paoletti
 * Created:	1994
 * Updated:	
 * Copyright:	(c) 1993-1994, AIAI, University of Edinburgh
 */
  
#define GUSI
#include <Script.h>
#include <string.h>
#include <stdio.h>
#include "wxMacPPC.h"
#include <Errors.h>
#include <Strings.h>
#ifndef TRUE			/* Remember, this is C, not C++, don't include "common.h" */
#	define TRUE 1
#endif
#ifndef FALSE
#	define FALSE 0
#endif
// global variables used to relay information to the application
short gSessionState = sClosed;

// Here's a piece of header joy
#if defined(MrCpp) || defined(GUSI)
#define CtoPstr c2pstr
#endif

Boolean gPBInUse = FALSE;

PPCParamBlockPtr	PBptr  = NULL;
PPCWritePBPtr WritePBptr = NULL;
PPCReadPBPtr	ReadPBptr = NULL;
EventRecord noLockEvent;

/* 
	Need to set up UPP's for these Completion Routines: 
	pascal void MyInformCompProc( PPCParamBlockPtr );
	pascal void MyAcceptCompProc(PPCParamBlockPtr);
	pascal void MyRejectCompProc(PPCParamBlockPtr);
	
	From PPCToolBox.h :
	typedef pascal void (*PPCCompProcPtr)(PPCParamBlockPtr pb);

	typedef PPCCompProcPtr PPCCompUPP;

	define NewPPCCompProc(userRoutine)		\
		((PPCCompUPP) (userRoutine))
	define NewPPCFilterProc(userRoutine)		\
		((PPCFilterUPP) (userRoutine))
		
	define NewPPCCompProc(userRoutine)		\
	(PPCCompUPP) NewRoutineDescriptor((ProcPtr)(userRoutine), uppPPCCompProcInfo, GetCurrentArchitecture())

  Here's how GUSI does it:

    RoutineDescriptor	uADSPCompletion =
		BUILD_ROUTINE_DESCRIPTOR(uppADSPCompletionProcInfo, ADSPCompletion);
		
	pb->ioCompletion	=	ADSPCompletionUPP(&uADSPCompletion);

	void ADSPCompletion(AnnotatedADSPParamBlock * pb)
	{
		....
	}

*/

// Declare some vars to hold the UPP's. Actually created as needed;
static PPCCompUPP  MyInformCompProcUPP;	
static PPCCompUPP  MyAcceptCompProcUPP;
static PPCCompUPP  MyRejectCompProcUPP;
static PPCFilterUPP	MyBrowserPortFilterUPP;
// this is how you prototype MyInformCompProc
pascal void MyInformCompProc(PPCParamBlockPtr);
// Prototypes for user defined routines to accept or reject sessions
void DoPPCAccept(PPCParamBlockPtr);
void DoPPCReject(PPCParamBlockPtr);
// prototype your Reject completion Procedure like this:
pascal void MyRejectCompProc(PPCParamBlockPtr);
// the iocompletion routine is prototyped as follows
pascal void MyAcceptCompProc(PPCParamBlockPtr);



//---------------- HIGH-LEVEL SERVICES --------------------------

Boolean PPCInitialize()
{	
	PBptr = (PPCParamBlockPtr)NewPtr(sizeof(PPCParamBlockRec));
	ReadPBptr = (PPCReadPBPtr) NewPtr(sizeof(PPCReadPBRec));
	WritePBptr = (PPCWritePBPtr) NewPtr(sizeof(PPCWritePBRec));
	
	return PBptr && ReadPBptr && WritePBptr;	
}	

// synchronous read - useful at startup
long ipcSyncRead(PPCSessRefNum sess,char * buffer, long size)
{	OSErr err;
	long len = 0;
	
	err = ppcStartRead(ReadPBptr,sess,size,buffer);
	if (err) 
		return err;
	if (ReadPBptr)
	{
		err = ppcRead(ReadPBptr,sess,size,buffer);
		if (err)
			return err;
		len = ReadPBptr->actualLength;
	}
	return len;
}

// returns number of read bytes ( or < 0 for error)
long ipcRead(PPCSessRefNum sess,char * buffer, long size)
{	OSErr err;
	long len = 0;
	
	if (ReadPBptr)
	{	
		err = ppcRead(ReadPBptr,sess,size,buffer);
		if (err) 
			return err;
			
		len = ReadPBptr->actualLength;
	
		err = ppcStartRead(ReadPBptr,sess,size,buffer);
		if (err)
			return err;
	} 
	return len;
}

// returns number of written bytes ( or < 0 for error)
long ipcWrite(PPCSessRefNum sess,char *buffer, long size)
{
	OSErr err;
	long t;
	long len = 0;
	
	if (WritePBptr)
	{
		err = ppcWrite(WritePBptr,sess,size,buffer);
		if (err) return err;
		
		Delay(60,&t);
		while (! MyWriteComplete(WritePBptr,&err)) 
		{
			if (WaitNextEvent( nullEvent, &noLockEvent, 30, NULL))
			{
			}
		}
		len = WritePBptr->actualLength;
	}
	
	return len;
}

//------------------------- INIT/OPEN SERVICES -----------------------------

OSErr  ppcInit(long *ppcAttributes)
{
	OSErr err;	

	err = Gestalt(gestaltPPCToolboxAttr, ppcAttributes);
	if ( err == noErr )
	{
		// PPC Toolbox is present
		if (  !( *ppcAttributes & gestaltPPCSupportsRealTime) )
		{
			// PPC Toolbox needs initialization
			// initialize the PPC Toolbox and set function result
			err = PPCInit();
			// test the attributes for the PPC Toolbox
			err = Gestalt(gestaltPPCToolboxAttr,ppcAttributes);
		}
	}
	return err;
}

// Opening a PPC port

OSErr ppcOpen(PPCPortRefNum *thePortRefNum, char *portName, Boolean *nbpRegisterFlag)
{
	PPCOpenPBRec	thePPCOpenPBRec;
	PPCPortRec	thePPCPortRec;
	LocationNameRec	theLocationNameRec;
	OSErr	err;

	// nameScript and name should be resources to allow easy localization
	thePPCPortRec.nameScript = smRoman;	// Roman script

	// name is a pascal style string, need to cast it to call strcpy
	strcpy((char *)thePPCPortRec.name,portName);
	// convert back to Pascal string
	CtoPstr((char *)thePPCPortRec.name);

	// the port type should always be hard-coded to allow the
	// application to find ports of a particular type even after
	// the name is localized
	thePPCPortRec.portKindSelector = ppcByString;

	// name is a pascal style string, need to cast it to call strcpy
	strcpy((char *)thePPCPortRec.u.portTypeStr,"wxIPC Appl");
	CtoPstr((char *)thePPCPortRec.u.portTypeStr);

	theLocationNameRec.locationKindSelector = ppcNBPTypeLocation;

	// nbpType is a pascal style string, need to cast it to call strcpy
	strcpy((char *)theLocationNameRec.u.nbpType,portName); // <<<< UNUSUAL
	CtoPstr((char *)theLocationNameRec.u.nbpType);

	thePPCOpenPBRec.serviceType = ppcServiceRealTime;
	thePPCOpenPBRec.resFlag = 0;    // must be 0 for 7.0+
	thePPCOpenPBRec.portName = &thePPCPortRec;
	thePPCOpenPBRec.locationName  = &theLocationNameRec;

	// make this a visible entity on the network
	thePPCOpenPBRec.networkVisible 	= TRUE;

	err  = PPCOpen(&thePPCOpenPBRec, FALSE);	// synchronous
	*thePortRefNum  = thePPCOpenPBRec.portRefNum;
	*nbpRegisterFlag = thePPCOpenPBRec.nbpRegistered;
	return err;
}

//----------------------- START/INFORM SERVICES ---------------------------

// Initiating a session using the PPCStart function

OSErr ppcStart	(PortInfoPtr	thePortInfoPtr,
						LocationNamePtr	theLocationNamePtr,
						PPCPortRefNum	thePortRefNum,
						PPCSessRefNum	*theSessRefNum,
						long		*theUserRefNum,
						long		*theRejectInfo)
{
	PPCStartPBRec	thePPCStartPBRec;
	Str32		userName;
	OSErr		err;

	thePPCStartPBRec.ioCompletion	= NULL;

	thePPCStartPBRec.portRefNum	= thePortRefNum;
												// from PPCOpen function

	thePPCStartPBRec.serviceType	= ppcServiceRealTime;

	thePPCStartPBRec.resFlag	= 0;

	thePPCStartPBRec.portName	= &thePortInfoPtr->name;
												// destination port

	thePPCStartPBRec.locationName	= theLocationNamePtr;
												// destination location

 // application-specific data for PPCInform
 thePPCStartPBRec.userData = 0;

	err = GetDefaultUser(&thePPCStartPBRec.userRefNum, userName);
	if ( err != noErr )
	   thePPCStartPBRec.userRefNum = 0;

	if ( thePortInfoPtr->authRequired && !thePPCStartPBRec.userRefNum )
	   // port selected does not allow guests and you do not have a default
	   // user reference number, so you cannot log on to this port
	   err = authFailErr;
	else
	   // attempt to log on
	   err = PPCStart(&thePPCStartPBRec, FALSE);

	if ( err == noErr ) {
	   *theSessRefNum  = thePPCStartPBRec.sessRefNum;
	   *theUserRefNum = thePPCStartPBRec.userRefNum;
	   gSessionState = sOpen;
	}
	else if ( err == userRejectErr )
		// return rejectInfo from the PPCReject function
		*theRejectInfo = thePPCStartPBRec.rejectInfo;
	return err;
}


// Using the PPCInform function to enable a port to receive sessions

OSErr ppcInform(PPCParamBlockPtr	thePPCParamBlockPtr,
						PPCPortPtr	thePPCPortPtr,
						LocationNamePtr	theLocationNamePtr,
						StringPtr	theUserNamePtr,
						PPCPortRefNum	thePortRefNum
	)
{
	
	MyInformCompProcUPP = NewPPCCompProc(MyInformCompProc);
	thePPCParamBlockPtr->informParam.ioCompletion	= MyInformCompProcUPP;
	
	// from the PPCOpen function
	thePPCParamBlockPtr->informParam.portRefNum 	= thePortRefNum;
	
	// the completion routine handles accepting or rejecting requests
	thePPCParamBlockPtr->informParam.autoAccept = FALSE;
	thePPCParamBlockPtr->informParam.portName = thePPCPortPtr;
	thePPCParamBlockPtr->informParam.locationName = theLocationNamePtr;
	thePPCParamBlockPtr->informParam.userName = theUserNamePtr;
	
	return PPCInform((PPCInformPBPtr)thePPCParamBlockPtr, TRUE);
}


// Completion routine for a PPCInform function

pascal void MyInformCompProc( PPCParamBlockPtr pb)

{

	if ( pb->informParam.ioResult == noErr ) {
		// decide if this session should be accepted or rejected
		// by looking at data supplied by the session requester
		if ( pb->informParam.userData != -1 )
		DoPPCAccept(pb);
	   else
		DoPPCReject(pb);
	}
	else
		// use a global to tell the application that PPCParamBlockRec
		// and the records it points to can be deallocated}
	gPBInUse = FALSE;
}


// Accepting a session request using the PPCAccept function

void DoPPCAccept(PPCParamBlockPtr pb)

{
	OSErr err;
	// accept the session
	MyAcceptCompProcUPP = NewPPCCompProc(MyAcceptCompProc);
	pb->acceptParam.ioCompletion = MyAcceptCompProcUPP;
	// the sessRefNum field is set by the PPCInform function
	err = PPCAccept(&pb->acceptParam, TRUE);  
}


// Completion routine for a PPCAccept function

pascal void MyAcceptCompProc(PPCParamBlockPtr pb)
{
	if ( pb->acceptParam.ioResult == noErr ) 
			// accept completed so the session is completely open
		
		gSessionState = sOpening;
		
			// use a global to tell the application that PPCParamBlockRec
			// and the records it points to can be deallocated
		gPBInUse = FALSE;
}


// Rejecting a session request using the PPCReject function

void DoPPCReject( PPCParamBlockPtr pb)
{
	OSErr err;

			// reject the session}
	MyRejectCompProcUPP = NewPPCCompProc(MyRejectCompProc);
	pb->acceptParam.ioCompletion = MyRejectCompProcUPP;
			// the sessRefNum field is set by the PPCInform function
	pb->rejectParam.rejectInfo = -1;

	err = PPCReject(&pb->rejectParam, TRUE);    
}


// Completion routine for a PPCReject function

pascal void MyRejectCompProc(PPCParamBlockPtr pb)
{
	// use a global to tell the application that PPCParamBlockRec and
	// the records it points to can be deallocated
	gPBInUse = FALSE;
}

//------------------------------ READ/WRITE SERVICES ----------------------

// Using the PPCRead function to read data during a session

OSErr ppcStartRead(PPCReadPBPtr thePPCReadPBPtr,
					PPCSessRefNum	theSessRefNum,
					Size			theBufferLength,
					Ptr			theBufferPtr
	)
{	
	thePPCReadPBPtr->ioCompletion = NULL;
	// from the PPCStart function  or the PPCInform function:
	thePPCReadPBPtr->sessRefNum	= theSessRefNum;
	thePPCReadPBPtr->bufferLength	= theBufferLength;
	thePPCReadPBPtr->bufferPtr	= theBufferPtr;

	return PPCRead(thePPCReadPBPtr, TRUE);  
}

OSErr ppcRead(PPCReadPBPtr	thePPCReadPBPtr,
					PPCSessRefNum	theSessRefNum,
					Size			theBufferLength,
					Ptr			theBufferPtr
	)
{
OSErr err;
	
	while ((err = thePPCReadPBPtr->ioResult) > 0)
	{
		if (WaitNextEvent( nullEvent, &noLockEvent, 30, NULL))
		{
		}
	}
	//if (err < 0) { Error("Read error on completion"); ("Code:%d\n",err); }
	
	return err;
}


// Polling the ioResult field to determine if a PPCRead function has completed

Boolean MyReadComplete(PPCReadPBPtr thePPCReadPBPtr, OSErr *err)
{
    // Error result gets value of ioResult
	*err = thePPCReadPBPtr->ioResult;

	// Return false if error is 1.
	return !*err;
}

/*-------------------------------------------------------------*/

// Using the PPCWrite function to write data during a session

OSErr ppcWrite(	PPCWritePBPtr	thePPCWritePBPtr,
						PPCSessRefNum	theSessRefNum,
						Size			theBufferLength,
						Ptr			theBufferPtr
						  )
{
	thePPCWritePBPtr->ioCompletion	= NULL;

	// from the PPCStart function or the PPCInform function--
	thePPCWritePBPtr->sessRefNum	= theSessRefNum;
	thePPCWritePBPtr->bufferLength	= theBufferLength;
	thePPCWritePBPtr->bufferPtr	= theBufferPtr;
	thePPCWritePBPtr->more		= FALSE;   /* no more data to read */
	thePPCWritePBPtr->userData	= 0;           /* app-specific data */
#ifdef PPCC		// no known reason *why* this should be required !!!  
	memcpy(&thePPCWritePBPtr->blockCreator, "????", 4);
	memcpy(&thePPCWritePBPtr->blockType, "????", 4);
#else
	thePPCWritePBPtr->blockCreator	= '????';  /* app-specific data */
	thePPCWritePBPtr->blockType	= '????';  /* app-specific data */
#endif
	return PPCWrite(thePPCWritePBPtr, TRUE); 
}


// Polling the ioResult field to determine if a PPCWrite function has completed

Boolean MyWriteComplete(PPCWritePBPtr thePPCWritePBPtr,OSErr *err)
{
	// Check ioResult for error
	*err = thePPCWritePBPtr->ioResult;

	// Return false if error is 1
	return !*err;
}

//------------------------ CLOSE SERVICES --------------------------------

// Ending a PPC session using the PPCEnd function

OSErr ppcEnd(PPCSessRefNum theSessRefNum)
{
	PPCEndPBRec thePPCEndPBRec;

	// Set the Session ref number to pass to PPCEnd
	thePPCEndPBRec.sessRefNum = theSessRefNum;

	// Return result of PPCEnd
	return PPCEnd(&thePPCEndPBRec, FALSE);	// synchronous
}


// Closing a PPC port using the PPCClose function

OSErr ppcClose(PPCPortRefNum thePortRefNum)
{
	PPCClosePBRec theClosePBRec;

	// Set the proper portRefNum, which is grabbed from PPCOpen function
	theClosePBRec.portRefNum = thePortRefNum;

	return PPCClose(&theClosePBRec, FALSE);	// synchronous
}

//-------------------------- LOCATE SERVICES ---------------------------------


// Using a port filter function

pascal Boolean MyBrowserPortFilter(LocationNameRec theLocationNameRec,
												PortInfoRec thePortInfoRec)
{
	return TRUE;
	// If port type selector for this port is a string
	// check for the right port, otherwise return FALSE(don't display port)
	if (thePortInfoRec.name.portKindSelector == ppcByString)
		// If string matches port we are looking for, display it
		if  ( !strcmp ((char *)thePortInfoRec.name.u.portTypeStr,
					(char *)"\pwxIPC Appl" ) )
		return TRUE;
	else
		return FALSE;
	else
		return FALSE;
}


// Browsing through dictionary service ports

OSErr ppcBrowser(LocationNameRec *theLocationNameRec,
				   PortInfoRec *thePortInfoRec)
{
	Str255	prompt;              // Prompt to display in dialogue
	Str255 	applListLabel;    // Title for list of applications
	Boolean 	defaultSpecified;
	Str32 	theLocNBPType;

	strcpy ((char *)prompt, "Choose an IPC service to link to:");
	CtoPstr((char *)prompt);
	strcpy((char *)applListLabel,"Services");
	CtoPstr((char *)applListLabel);

	defaultSpecified = TRUE;

	theLocationNameRec->locationKindSelector = ppcNBPLocation;
	strcpy((char *)theLocationNameRec->u.nbpEntity.objStr,"Moof");
	CtoPstr((char *)theLocationNameRec->u.nbpEntity.objStr);
	// typeStr is ignored
	strcpy((char *)theLocationNameRec->u.nbpEntity.zoneStr,"Twilight");
	CtoPstr((char *)theLocationNameRec->u.nbpEntity.zoneStr);

	thePortInfoRec->name.nameScript = smRoman;
	strcpy ((char *)thePortInfoRec->name.name,"server");
	CtoPstr((char *)thePortInfoRec->name.name);
	thePortInfoRec->name.portKindSelector = ppcByString;
	strcpy ((char *)thePortInfoRec->name.u.portTypeStr,"wxIPC Appl");
	CtoPstr((char *)thePortInfoRec->name.u.portTypeStr);

	// when building the list of objects (Macintoshes), show only
	// those with the NBP type "PPC Example"

	strcpy ((char *)theLocNBPType ,"server"); // match this NBP type
	CtoPstr((char *)theLocNBPType);

	MyBrowserPortFilterUPP = NewPPCFilterProc(MyBrowserPortFilter);
	return PPCBrowser(prompt, applListLabel, defaultSpecified,
			theLocationNameRec, thePortInfoRec,
			MyBrowserPortFilterUPP, theLocNBPType);
}


// Using the IPCListPorts function to obtain a list of ports

OSErr ipcListPorts(short	theStartIndex,
	short	theRequestCount, short 	*theActualCount,
	Str32	theObjStr, Str32	theZoneStr,
	PortInfoArrayPtr thePortInfoBufferPtr )

{
	IPCListPortsPBRec	theIPCListPortsPBRec;
	PPCPortRec	thePPCPortRec;
	LocationNameRec	theLocationNameRec;
	OSErr	err;

	// list all PPC ports at the specified location
	thePPCPortRec.nameScript = smRoman;

	// set name to match all names, NOTE call to CtoPstr to convert string
	// back to Pascal style string.
	strcpy((char *)thePPCPortRec.name,"=");
	CtoPstr((char *)thePPCPortRec.name);

	thePPCPortRec.portKindSelector = ppcByString;

	// match all types,  NOTE call to CtoPstr to convert string
	// back to pascal style string after casting for call to strcpy
	strcpy((char *)thePPCPortRec.u.portTypeStr,"=");
	CtoPstr((char *)thePPCPortRec.u.portTypeStr);

	// The application must choose and supply the NBP zone string from
	// the list returned by GetZoneList. Then, the application must
	// choose and supply the NBP object string from the list returned by
	// NBPLookup. This example looks for NBP type "PPC Example". If you
	// don't supply your own NBP type, you should use "PPCToolBox" for
	// the NBP type string.

	theLocationNameRec.locationKindSelector = ppcNBPLocation;

	// copy objstr, note casting to make C type string for strcpy
	// then convert string back to pascal type string
	strcpy ((char *)theLocationNameRec.u.nbpEntity.objStr,
			(char *)theObjStr);
	CtoPstr((char *)theLocationNameRec.u.nbpEntity.objStr);

	// copy typestr, note casting to make C type string for strcpy
	// then convert string back to pascal type string
	strcpy((char *)theLocationNameRec.u.nbpEntity.typeStr,"PPC Example");
	CtoPstr((char *)theLocationNameRec.u.nbpEntity.typeStr);

	// copy zonestr, note casting to make C type string for strcpy
	// then convert string back to pascal type string
	strcpy((char *)theLocationNameRec.u.nbpEntity.zoneStr,
			(char *)theZoneStr);
	CtoPstr((char *)theLocationNameRec.u.nbpEntity.zoneStr);

	theIPCListPortsPBRec.startIndex 	= theStartIndex;
	theIPCListPortsPBRec.requestCount 	= theRequestCount;
	theIPCListPortsPBRec.portName 	= &thePPCPortRec;
	theIPCListPortsPBRec.locationName	= &theLocationNameRec;
	theIPCListPortsPBRec.bufferPtr 	= thePortInfoBufferPtr;

	err = IPCListPorts(&theIPCListPortsPBRec, FALSE);
	*theActualCount = theIPCListPortsPBRec.actualCount;
	return err;
}






