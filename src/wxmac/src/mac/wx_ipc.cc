/*
 * File:	wx_ipc.cc
 * Purpose:	Interprocess communication implementation (Mac version)
 * Author:		Tomaso Paoletti
 * Created:	1994-1995
 * Updated:	
 * Copyright:	(c) 1993-1995, AIAI, University of Edinburgh
 */
 
/* This implementation is limited to only ONE connection and works in a cooperative way:
      DoIPCIdle() is called periodicallyfrom inside wx_app (right after DoIdle), 
      the connection (if present) is scanned for input, possible requests are worked out
      and the control returns to main app.
     
     A much more elegant solution would be achieved through multiple threads (maybe in the future).
*/

static const char sccsid[] = "%W% %G%";

#include <stdlib.h>
#include <iostream.h>
#include <stdio.h>
#include <math.h>

#include "common.h"
#include "wx_setup.h"


#if USE_IPC

#include "wx_main.h"
#include "wx_frame.h"
#include "wx_utils.h"
#include "wx_ipc.h"
#include "wxMacPPC.h"
#include <Strings.h>

#define ONE_CONNECTION	

extern int errno;
int wx_socket_create(int port);
// Add NULL-terminated string to buffer, returning next start point
int wxAddString(int start, char *info, char *buffer, int size = -1);

// Get next NULL-terminated string from buffer
char *wxGetNextString(char *buffer);

extern Bool wxIPCInitialized;

OSErr err;
wxIPCObject * wxTheIPCObj = NULL; // used for execution of periodic tasks

extern PPCParamBlockPtr	PBptr;
extern PPCWritePBPtr		WritePBptr;
extern PPCReadPBPtr		ReadPBptr;
Boolean gRead = FALSE;

Bool wx_input_ready(char * client);
Bool wx_accept_connection(wxServer * server);

//  the"connection-crunching" routine
void DoIPCIdle ();

/*
 * Initialization
 *
 */

void wxIPCInitialize(void)
{		
Boolean memOK;
long attributes = 0L;

 if (wxIPCInitialized)
 {
 #ifdef DEBUGLOG
	wxLog<<"!!!! Trying to initialize IPC twice !!!!\n";
#endif
    return;
 }	
	memOK = PPCInitialize();
	if (!memOK) 
	{	wxError("IPC not initialized (problem with memory)");	return; };
	
	// the PPC toolbox initialization
	wxIPCInitialized = ( ppcInit(&attributes) == noErr );

	if (! ( attributes & gestaltPPCSupportsOutGoing ))
		wxError("AppleTalk is disabled");
			// it's likely that AppleTalk is disabled, so you may
			// want to tell the user to activate AppleTalk from
			// the Chooser

	if (! (attributes & gestaltPPCSupportsIncoming))
		wxError("Program linking is disabled");
			// it's likely that program linking is disabled, so you
			// may want to tell the user to start program linking
			// from the Sharing Setup control panel
  
  if (!wxIPCInitialized) 
  	wxError("IPC not initialized");
 #ifdef DEBUGLOG
	else	wxLog<<"--- IPC initialization OK ---\n";
#endif

}

wxIPCObject::wxIPCObject(void)
{
  service_name = NULL;
  wxTheIPCObj = this;
}

wxIPCObject::~wxIPCObject(void)
{
  wxTheIPCObj = NULL;
}

/*
 * Server
 *
 */

wxServer::wxServer(void)
{
#ifdef DEBUGLOG
	char msg[100];
	sprintf(msg,"*** wxServer constructor (ptr:%ld) ***\n",(long)wxTheIPCObj);
	wxLog << msg;
#endif
}

Bool wxServer::Create(char *server_name)
{
Boolean NBP_reg = FALSE;
PPCPortPtr			thePPCPortPtr = NULL;
LocationNamePtr	locNamePtr = NULL;
StringPtr	userNamePtr = NULL;

#ifdef DEBUGLOG
	wxLog<< "SERVER: Creating...\n";
#endif
  service_name = copystring(server_name);

  // Under UNIX, server should be an integer inside a string!
  int the_port = 0;
  sscanf(server_name, "%d", &the_port);

  /* Create a socket listening on specified port */
  //server_socket = wx_socket_create(the_port);
	err = ppcOpen(&server_socket,"server",&NBP_reg);
  if (err)
  {	wxError("wxServer cannot open PPC port");
    return FALSE;
	}
	if (!NBP_reg)
	{	wxError("server port has not been NBP-registered");	}
	
	err = ppcInform(PBptr,thePPCPortPtr,locNamePtr,userNamePtr,server_socket);

	if (err)
	{	ppcClose (server_socket);
		wxError("ppcInform failed");
		return FALSE;
	}
#ifdef DEBUGLOG
	wxLog<< "SERVER: Inform succeeded.\n";
#endif

#if 0		// FIXME - what is the on the Mac
	Bool notify_toplevel = FALSE;
  wxConnection *toplevel = this->OnAcceptConnection("STDIO");
  if (toplevel)
  {
    toplevel->input_session = 0;
    toplevel->output_session = 1;
    notify_toplevel = TRUE;
    toplevel->topic_name = copystring("STDIO");
  }
  else toplevel = new wxConnection(NULL, 0); // Create a dummy connection

  toplevel->server = this;
  connections.Append(toplevel);

  /* Register stdin (IF APP ALLOWS IT) and the socket with the notifier */
#endif

#ifdef wx_xview
  // stdin
  if (notify_toplevel)
    notify_set_input_func((Notify_client)toplevel, (Notify_func)&wx_input_ready, 0);

  // Top level port
  notify_set_input_func((Notify_client)toplevel, (Notify_func)&wx_accept_connection, server_socket);
#endif
#ifdef wx_motif
  XtAppAddInput(wxTheApp->appContext, server_socket, (XtPointer *)XtInputReadMask, (XtInputCallbackProc) wx_accept_connection, (XtPointer)toplevel);
#endif
	
  return TRUE;
}


wxServer::~wxServer(void)
{
#ifdef DEBUGLOG
	wxLog<<"SERVER: closing port and destroying...\n";
#endif
	ppcClose(server_socket);
}

/*
 * Client
 *
 */

wxClient::wxClient(void)
{
#ifdef DEBUGLOG
	char msg[100];
	sprintf(msg,"*** wxClient constructor (ptr:%ld) ***\n",(long)wxTheIPCObj);
	wxLog << msg;
#endif
}

wxClient::~wxClient(void)
{
#ifdef DEBUGLOG
	wxLog<<"CLIENT: closing port and destroying...\n";
#endif
	ppcClose(client_socket);
}

Bool wxClient::ValidHost(char *host)
{
#ifdef USE_SOCKET
  if (gethostbyname(host))
    return TRUE;
  else
    return FALSE;
#else
	return TRUE;
#endif
}

wxConnection *wxClient::MakeConnection(char *host, char *server_name, char *topic)
{
Boolean NBP_reg = FALSE;
int port;
long cnt;

#ifdef DEBUGLOG
	wxLog<<"CLIENT: Making connection...\n";
#endif   
	sscanf(server_name, "%d", &port);

  PPCSessRefNum session;
	LocationNameRec locName;
	PortInfoRec portInfo;
	long	userRefNum = 0L;
	long	rejectInfo = 0L;
 
  err = ppcOpen(&client_socket,"client",&NBP_reg);
  if (err)
  {
		wxError("wxClient cannot open PPC port");
		return NULL;
  }
  
	if (!NBP_reg)
	{	wxError("client port has not been NBP-registered");	}

	err = ppcBrowser(&locName,&portInfo);
	if (err)
	{	wxError("wxClient cannot browse PPC ports"); }
	
#ifdef DEBUGLOG
	wxLog<< "CLIENT: Starting PPC session...\n";
#endif
	err = ppcStart(&portInfo,&locName,client_socket,&session,&userRefNum,&rejectInfo);
	if (err)
	{	wxError("Client cannot start session");	}
	else
	{
	// Send topic name, and enquire whether this has succeeded
		char buf[200];
		buf[0] = wxCONNECT;
		strcpy(buf+1, topic);
		
#ifdef DEBUGLOG
		wxLog<<"CLIENT: Sending CONNECT message...\n";
#endif		
		ipcWrite(session, buf, strlen(topic)+2);
		ipcSyncRead(session, buf, 200);

		// OK! Confirmation.
		if (buf[0] == wxCONNECT)
		{
#ifdef DEBUGLOG		  
			wxLog<<"CLIENT: server confirmed connection!\n";
#endif
			wxConnection *connection = OnMakeConnection();
			if (connection)
			{
				connections.Append(connection);
				connection->input_session = session;
				connection->output_session = session;
				connection->client = this;

				// Start async. read
#ifdef DEBUGLOG
				wxLog<<"CLIENT: starting async. read of data\n";
#endif
				err = ppcStartRead(ReadPBptr,session, connection->buf_size, connection->buf_ptr);
				
				// Register with the notifier
				connection->Notify(TRUE);
				return connection;
			}
			else { ppcEnd(session); 
#ifdef DEBUGLOG
				 wxLog<<"CLIENT: connection failed, ending...\n";
#endif
				  return NULL; }
		}
		else
		{
#ifdef DEBUGLOG		  
			wxLog<<"CLIENT: Session ended.\n";
#endif
			ppcEnd(session);
			return NULL;
		}
	}
	return NULL;
}

/*
 * Connection
 */

wxConnection::wxConnection(char *buffer, int size):wxbConnection(buffer, size)
{
  input_session = 0;
  output_session = 0;
}

wxConnection::wxConnection(void)
{
  input_session = 0;
  output_session = 0;
}

wxConnection::~wxConnection(void)
{
OSErr err;
#ifdef wx_motif
  if (xtInputId > 0)
   XtRemoveInput(xtInputId);
#endif
#ifdef wx_xview
  notify_set_input_func((Notify_client)this, (Notify_func)0, input_session);
#endif
  	
  if (input_session != 0)
  {	
		err = ppcEnd(input_session);
	}

  if (server)
    server->connections.DeleteObject(this);
  if (client)
    client->connections.DeleteObject(this);
  if (topic_name)
    delete[] topic_name;
}

// Calls that CLIENT can make
Bool wxConnection::Disconnect(void)
{
OSErr err;
#ifdef DEBUGLOG
	wxLog<<"Sending DISCONNECT msg.\n";
#endif

  buf_ptr[0] = wxDISCONNECT;
  ipcWrite(output_session, buf_ptr, 1);
	
#ifdef wx_motif
  if (xtInputId)
    XtRemoveInput(xtInputId);
  xtInputId = 0;
#endif
#ifdef wx_xview
  notify_set_input_func((Notify_client)this, (Notify_func)0, input_session);
#endif

  if (input_session != 0) // same as output_session
  {	
  	err = ppcEnd(input_session);
	}
  
  input_session = 0;
  output_session = 0;

  return TRUE;
}

Bool wxConnection::Execute(char *data, int size, int format)
{
  if (size < 0)
    size = strlen(data);

  char format_buf[10];
  sprintf(format_buf, "%d", format);

  buf_ptr[0] = wxEXECUTE;
  int pos = wxAddString(1, format_buf, buf_ptr);
  pos = wxAddString(pos, data, buf_ptr, size);

  ipcWrite(output_session, buf_ptr, pos);
	
  return TRUE;
}

char *wxConnection::Request(char *item, int *size, int format)
{
  char format_buf[10];
  sprintf(format_buf, "%d", format);

  buf_ptr[0] = wxREQUEST;
  int pos = wxAddString(1, item, buf_ptr);
  pos = wxAddString(pos, format_buf, buf_ptr);

#ifdef wx_xview
  Notify(FALSE);
#endif

  ipcWrite(output_session, buf_ptr, pos);
  ipcRead(input_session, buf_ptr, buf_size);
	
#ifdef wx_xview
  Notify(TRUE);
#endif

  if (err || buf_ptr[0] == wxFAIL)
    return NULL;
  else
  {
    char *new_item = buf_ptr + 1;
    char *data = wxGetNextString(new_item);
    if (size) *size = data-new_item;
    return data;
  }
}

Bool wxConnection::Poke(char *item, char *data, int size, int format)
{
  char format_buf[10];
  sprintf(format_buf, "%d", format);

  if (size < 0)
    size = strlen(data);

  buf_ptr[0] = wxPOKE;
  int pos = wxAddString(1, item, buf_ptr);
  pos = wxAddString(pos, format_buf, buf_ptr);
  pos = wxAddString(pos, data, buf_ptr, size);

  ipcWrite(output_session, buf_ptr, pos);
  
  return TRUE;
}

Bool wxConnection::StartAdvise(char *item)
{
  buf_ptr[0] = wxADVISE_START;
  int pos = wxAddString(1, item, buf_ptr);

#ifdef wx_xview
  Notify(FALSE);
#endif

#ifdef DEBUGLOG
	char msg[80];
	Ptr p = buf_ptr;
	sprintf(msg,"writing %ld bytes : %d %d %d %d %d %d \n",pos,p[0],p[1],p[2],p[3],p[4],p[5]);
	wxLog << "StartAdvise: "<<msg;
#endif
  ipcWrite(output_session, buf_ptr, pos);
  ipcRead(input_session, buf_ptr, buf_size);
  
#ifdef wx_xview
  Notify(TRUE);
#endif
	
#ifdef DEBUGLOG
	if (buf_ptr[0]==wxFAIL) wxLog << "StartAdvise FAILED\n";
#endif
  return (err == noErr && buf_ptr[0] != wxFAIL);
}

Bool wxConnection::StopAdvise(char *item)
{
  buf_ptr[0] = wxADVISE_STOP;
  int pos = wxAddString(1, item, buf_ptr);

#ifdef wx_xview
  Notify(FALSE);
#endif

  ipcWrite(output_session, buf_ptr, pos);
  ipcRead(input_session, buf_ptr, buf_size);
	
#ifdef wx_xview
  Notify(TRUE);
#endif

  return (err == noErr && buf_ptr[0] != wxFAIL);
}

// Calls that SERVER can make
Bool wxConnection::Advise(char *item, char *data, int size, int format)
{
  char format_buf[10];
  sprintf(format_buf, "%d", format);

  buf_ptr[0] = wxADVISE;
  int pos = wxAddString(1, item, buf_ptr);
  pos = wxAddString(pos, format_buf, buf_ptr);
  pos = wxAddString(pos, data, buf_ptr, size);

  ipcWrite(output_session, buf_ptr, pos);
	
  return TRUE;
}

void wxConnection::Notify(Bool notify)
{
#ifdef wx_motif
  if (!notify)
  {
    if (xtInputId > 0)
    {
      XtRemoveInput(xtInputId);
      xtInputId = 0;
    }
  }
  else
    xtInputId = XtAppAddInput(wxTheApp->appContext, input_session, (XtPointer*)XtInputReadMask, (XtInputCallbackProc) wx_input_ready, this);
#endif
#ifdef wx_xview
  if (notify)
    notify_set_input_func((Notify_client)this, (Notify_func)wx_input_ready, input_session);
  else
    notify_set_input_func((Notify_client)this, NOTIFY_FUNC_NULL, input_session);
#endif
}

#ifdef wx_xview
#define NOTIFY_RETURN_TYPE Notify_value
Notify_value wx_input_ready(Notify_client client, int fd)
#endif
#ifdef wx_motif
#define NOTIFY_RETURN_TYPE Bool
Bool wx_input_ready(XtPointer client, int *fid, XtInputId *id)
#endif

#define NOTIFY_RETURN_TYPE Bool
static Bool wx_input_ready(wxConnection * connection)
{
  char msg[200];
    //wxConnection *connection = (wxConnection *)client;
		long nread;
		
	if (gSessionState != sOpen) 
	{	
		return FALSE;
	}
	
	err = ReadPBptr->ioResult;
	if (err < 0) 
	{	
		gSessionState = sClosed;
#ifdef DEBUGLOG
		sprintf(msg,"Error %d detected\n",err);
		wxLog<<msg;
#endif
	}
	else
	if (err == 0) // yeah, we have data...
	{
#ifdef DEBUGLOG
	sprintf(msg,"Connection %lX (input: %d, output: %d) (buf:%lX size:%ld) prev:%s\n",
		connection,connection->input_session,connection->output_session,connection->buf_ptr,
		connection->buf_size, connection->buf_ptr);
	wxLog<<msg;
#endif

    nread = ipcRead(connection->input_session, connection->buf_ptr, connection->buf_size);  
    if ((nread >= 0) && (nread < connection->buf_size))
      connection->buf_ptr[nread] = 0;

#ifdef DEBUGLOG
		char * p = connection->buf_ptr;
		sprintf(msg,"%ld bytes : %d %d %d %d %d %d %d %d -> '%s' (buffer %lX)\n",nread,p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p+1,(long)p);
		wxLog<<"Receiving data.. "<<msg;
#endif  

		if (nread < 0)
		{
#ifdef DEBUGLOG
			sprintf(msg,"Read error %d\n",nread);
    	wxLog<<msg;
#endif
			connection->Notify(FALSE);
			connection->OnDisconnect();
			gSessionState = sClosed;
			return (NOTIFY_RETURN_TYPE)FALSE;
		}     
		else
		if (nread == 0)
		{
#ifdef DEBUGLOG
			wxLog<<"EOF detected\n";
#endif
			connection->Notify(FALSE);
			connection->OnDisconnect();
			return (NOTIFY_RETURN_TYPE)FALSE;
		}
		switch (connection->buf_ptr[0])
		{
			case wxEXECUTE:
     {
#ifdef DEBUGLOG
     	wxLog<<"wxEXECUTE received\n";
#endif     	
       char *format_buf = connection->buf_ptr + 1;
       char *data = wxGetNextString(format_buf);

       int format = wxCF_TEXT;
       sscanf(format_buf, "%d", &format);

       int size = nread - (data - connection->buf_ptr);
       connection->OnExecute(connection->topic_name, data, size, format);
       break;
     }
     case wxADVISE:
     {	
#ifdef DEBUGLOG
     	wxLog<<"wxADVISE received\n";
#endif     
       char *item = connection->buf_ptr + 1;
       char *format_buf = wxGetNextString(item);;
       char *data = wxGetNextString(format_buf);

       int format = wxCF_TEXT;
       sscanf(format_buf, "%d", &format);

       int size = nread - (data - connection->buf_ptr);
       connection->OnAdvise(connection->topic_name, item, data, size, format);
       break;
     }
     case wxADVISE_START:
     {
#ifdef DEBUGLOG
     	wxLog<<"wxADVISE_START received\n";
#endif         		
       char *item = connection->buf_ptr + 1;
       Bool ok = connection->OnStartAdvise(connection->topic_name, item);
       if (ok)
       {
         connection->buf_ptr[0] = wxADVISE_START;
         connection->buf_ptr[1] = 0;
         ipcWrite(connection->output_session, connection->buf_ptr, 2); 
       }
       else
       {
         connection->buf_ptr[0] = wxFAIL;
         connection->buf_ptr[1] = 0;
         ipcWrite(connection->output_session, connection->buf_ptr, 2);
       }

       break;
     }
     case wxADVISE_STOP:
     {
#ifdef DEBUGLOG
     	wxLog<<"wxADVISE_STOP received\n";
#endif         		
       char *item = connection->buf_ptr + 1;
       Bool ok = connection->OnStopAdvise(connection->topic_name, item);
       if (ok)
       {
         connection->buf_ptr[0] = wxADVISE_STOP;
         connection->buf_ptr[1] = 0;
         ipcWrite(connection->output_session, connection->buf_ptr, 2);
       }
       else
       {
         connection->buf_ptr[0] = wxFAIL;
         connection->buf_ptr[1] = 0;
         ipcWrite(connection->output_session, connection->buf_ptr, 2);
       }

       break;
     }
     case wxPOKE:
     {
#ifdef DEBUGLOG
     	wxLog<<"wxPOKE received\n";
#endif         	
       char *item = connection->buf_ptr + 1;
       char *format_buf = wxGetNextString(item);;
       char *data = wxGetNextString(format_buf);

       int format = wxCF_TEXT;
       sscanf(format_buf, "%d", &format);

       int size = nread - (data - connection->buf_ptr);
       connection->OnPoke(connection->topic_name, item, data, size, format);
       break;
     }
     case wxREQUEST:
     {
#ifdef DEBUGLOG
     	wxLog<<"wxREQUEST received\n";
#endif         		
       char *item = connection->buf_ptr + 1;
       char *format_buf = wxGetNextString(item);;

       int format = wxCF_TEXT;
       sscanf(format_buf, "%d", &format);

       int user_size = -1;
       char *user_data = connection->OnRequest(connection->topic_name, item, &user_size, format);
       if (user_data)
       {
         connection->buf_ptr[0] = wxREQUEST_REPLY;
         int pos = wxAddString(1, item, connection->buf_ptr);
         pos = wxAddString(pos, user_data, connection->buf_ptr, user_size);

         ipcWrite(connection->output_session, connection->buf_ptr, pos);
       }
       else
       {
         connection->buf_ptr[0] = wxFAIL;
         connection->buf_ptr[1] = 0;
         
         ipcWrite(connection->output_session, connection->buf_ptr, 2);
       }
       break;
     }
     default:
#ifdef DEBUGLOG
     	 wxLog<<"Unrecognised command - resp. FAILURE\n";
     	 connection->buf_ptr[0] = wxFAIL;
     	 connection->buf_ptr[1] = 0;
     	 ipcWrite(connection->output_session, connection->buf_ptr, 2);
#endif
       break;
		}
	}
	return TRUE;
}

Bool wx_accept_connection(wxServer * server)
{

  PPCSessRefNum newsess;
  //wxConnection *top_connection = (wxConnection *)client;

  /* Accept the connection, getting a new socket */

  newsess = PBptr->informParam.sessRefNum;
  
  char buf[300];
		
#ifdef DEBUGLOG
	char msg[200];
	long n = ipcSyncRead(newsess, buf, 300);
	sprintf(msg,"%ld bytes long: %d %d %d ... '%s'\n",n,buf[1],buf[2],buf[3],buf+1);
	wxLog<<"SERVER: received msg is "<<msg;
#else
  ipcSyncRead(newsess, buf, 300);
#endif

  if (buf[0] == wxCONNECT)
  {
    char *topic_name = copystring(buf + 1);
		
    // wxConnection *new_connection = top_connection->server->OnAcceptConnection(topic_name);
    wxConnection *new_connection = server->OnAcceptConnection(topic_name);
    if (new_connection)
    {
      // Acknowledge success
      buf[0] = wxCONNECT;
      buf[1] = 0;
      ipcWrite(newsess, buf, 2);
#ifdef DEBUGLOG
			wxLog<<"SERVER: Responding to CONNECT msg.\n";
#endif
      new_connection->input_session = newsess;
      new_connection->output_session = newsess;
      new_connection->server = server;  //top_connection->server;
      new_connection->topic_name = topic_name;
      
#ifdef DEBUGLOG
			wxLog<<"SERVER: starting async. read\n";
#endif
      err = ppcStartRead(ReadPBptr, newsess, new_connection->buf_size, new_connection->buf_ptr);
      
      //top_connection->server->connections.Append(new_connection);
      server->connections.Append(new_connection); 
      
#ifdef wx_xview
      notify_set_input_func((Notify_client)new_connection, (Notify_func)&wx_input_ready, newsock);
#endif
#ifdef wx_motif
      new_connection->xtInputId = XtAppAddInput(wxTheApp->appContext, newsock, (XtPointer*)XtInputReadMask, (XtInputCallbackProc) wx_input_ready, (XtPointer)new_connection);
#endif
    }
    else
    {
      // Send failure message
      buf[0] = wxFAIL;
      buf[1] = 0;
#ifdef DEBUGLOG
      wxLog<<"SERVER: Sending FAILURE msg.\n";
#endif
      ipcWrite(newsess, buf, 2);
    }
  }
  return FALSE;
}

/*
 * Create an internet socket listening on the specified port.
 */

int wx_socket_create(int port)
{
#ifdef USE_SOCKETS
  struct sockaddr_in addr; int sock;

  addr.sin_family = AF_INET; 
  addr.sin_addr.s_addr = htonl(INADDR_ANY); 
  addr.sin_port = htons(port);

  sock = socket(AF_INET, SOCK_STREAM, 0);
  if(bind(sock, (struct sockaddr *)&addr, sizeof(addr)) == -1)
  {
    printf("Error in bind\n");
    return -1;
  }

  listen(sock, 5);

  return sock;
#else
	wxError("wx_socket_create not implemented");
	return 0;
#endif
}

//--------------- new event-loop calls --------------------

void wxServer::PeriodicTask()
{	
	if (gSessionState == sOpening)
	{	
#ifdef DEBUGLOG
		wxLog<<"SERVER: session is opening.\n";
#endif
		gSessionState = sOpen;
		wx_accept_connection(this);
	}
	if (gSessionState == sOpen)
	{
		// LIMIT: works with only one connection
		wxNode *node = connections.First();
#ifdef ONE_CONNECTION
		wxConnection *conn = NULL;

		if (node && (conn = (wxConnection *)node->Data()))
		{	Bool valid = wx_input_ready(conn);
		}
		else
		{	
#ifdef DEBUGLOG
			wxLog<<"SERVER: no connection on this side.";
#endif
			gSessionState = sClosed;
		}
#endif
	}
}

void wxClient::PeriodicTask()
{
	if (gSessionState == sOpen)
	{
		// LIMIT: works with only one connection
		wxNode *node = connections.First();
#ifdef ONE_CONNECTION
		wxConnection *conn = NULL;

		if (node && (conn = (wxConnection *)node->Data()))
		{	Bool valid = wx_input_ready(conn);
		}
		else
		{	
#ifdef DEBUGLOG		
			wxLog<<"CLIENT: no connection on this side.";
#endif
			gSessionState = sClosed;
		}
#endif
	}
}

void DoIPCIdle ()
/* MUST BE CALLED PERIODICALLY, OTHERWISE THERE 'S NO WAY FOR CLIENT/SERVER TO COMMUNICATE */
{
	if (wxTheIPCObj)
		wxTheIPCObj->PeriodicTask();
}


/************** UNUSED STUFF ***********************
//----------- old event-loop calls ---------------

// thread-manager support would help...

void ServerIdle(wxServer *);
void ClientIdle(wxClient *);

void ServerIdle(wxServer * server)
{
Bool valid;
static lastTime = TickCount();
	
	if (gSessionState == sOpening)
	{	
#ifdef DEBUGLOG
		wxLog<<"SERVER: session is opening.\n";
#endif
		gSessionState = sOpen;
		wx_accept_connection(server);
	}
	if (gSessionState == sOpen)
	{
#ifdef DEBUGLOG
		if (TickCount()-lastTime > 120)
		{	char msg[100];
			sprintf(msg,"[slow loop for server : %ld ticks]\n",TickCount()-lastTime);
			wxLog<<msg;
		}
#endif
		// LIMIT: works with only one connection
		wxNode *node = server->connections.First();
#ifdef ONE_CONNECTION
		wxConnection *conn = (wxConnection *)node->Data();
		if (conn)
			valid = wx_input_ready(conn);
		else
		{	
#ifdef DEBUGLOG
			wxLog<<"SERVER: no connection on this side.";
#endif
			gSessionState = sClosed;
		}
#endif
	}
	lastTime = TickCount();
}

void ClientIdle(wxClient * client)
{
Bool valid;
static lastTime = TickCount();
	
	if (gSessionState == sOpen)
	{
#ifdef DEBUGLOG
		if (TickCount()-lastTime > 120)
		{	char msg[100];
			sprintf(msg,"[slow loop for client : %ld ticks]\n",TickCount()-lastTime);
			wxLog<<msg;
		}
#endif

		// LIMIT: works with only one connection
		wxNode *node = client->connections.First();
#ifdef ONE_CONNECTION
		wxConnection *conn = (wxConnection *)node->Data();
		if (conn)
			valid = wx_input_ready(conn);
		else
		{	
#ifdef DEBUGLOG		
			wxLog<<"CLIENT: no connection on this side.";
#endif
			gSessionState = sClosed;
		}
#endif
	}
	lastTime = TickCount();
}
*****************************/

#endif // USE_IPC
