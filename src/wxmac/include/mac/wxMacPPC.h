/*
 * File:	wxMacPPC.h
 * Purpose:	IPC primitives (Macintosh version)
 * Author:	Tomaso Paoletti
 * Created:	1994
 * Updated:	
 * Copyright:	(c) 1993-1994, AIAI, University of Edinburgh
 */

#pragma once
#include <GestaltEqu.h>
#include <PPCToolBox.h>

enum { sClosed = 0, sOpening, sOpen, sListening };

extern short gSessionState;

extern Boolean gPBInUse;
extern Boolean gAsync;

extern PPCReadPBPtr		ReadPBptr;
extern PPCWritePBPtr	WritePBptr;

#ifdef __cplusplus
extern "C" {
#endif

Boolean PPCInitialize(void);
long ipcSyncRead(PPCSessRefNum sess,char * buffer, long size);
long ipcRead(PPCSessRefNum sess,char * buffer, long size);
long ipcWrite(PPCSessRefNum sess,char *buffer, long size);

pascal void MyInformCompProc( PPCParamBlockPtr );
pascal void MyAcceptCompProc(PPCParamBlockPtr);
pascal void MyRejectCompProc(PPCParamBlockPtr);


OSErr  ppcInit(long *ppcAttributes);
OSErr ppcOpen(PPCPortRefNum *, char *, Boolean *);
OSErr ppcStart(PortInfoPtr,  LocationNamePtr,  PPCPortRefNum, 
						PPCSessRefNum *,  long *,  long *);
OSErr ppcInform(PPCParamBlockPtr,  PPCPortPtr,  LocationNamePtr,
						StringPtr,  PPCPortRefNum);

OSErr ppcRead(PPCReadPBPtr,PPCSessRefNum,Size,Ptr);
OSErr ppcStartRead(PPCReadPBPtr,PPCSessRefNum,Size,Ptr);
OSErr ppcWrite(PPCWritePBPtr,PPCSessRefNum,Size,Ptr);

OSErr ppcClose (PPCPortRefNum);
OSErr ppcEnd(PPCSessRefNum);

Boolean MyReadComplete(PPCReadPBPtr thePPCReadPBPtr, OSErr *err);
Boolean MyWriteComplete(PPCWritePBPtr, OSErr *);

void DoPPCAccept(PPCParamBlockPtr);
void DoPPCReject( PPCParamBlockPtr );

pascal Boolean MyBrowserPortFilter(LocationNameRec,PortInfoRec);
OSErr ppcBrowser(LocationNameRec *,PortInfoRec *);
OSErr ipcListPorts(short,short,short *,Str32,Str32,PortInfoArrayPtr);

#ifdef __cplusplus
}
#endif