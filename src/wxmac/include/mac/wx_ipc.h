/*
 * File:	wx_ipc.h
 * Purpose:	
 * Author:	Tomaso Paoletti (Macintosh version)
 * Created:	1994
 * Updated:	
 * Copyright:	(c) 1993-1994, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

/*
 * Purpose:  Interprocess communication implementation. Uses DDE under
 *           Windows, sockets to implement DDE subset under UNIX
 *           and PPC Toolbox under Macintosh
 */


#ifndef wx_ipch
#define wx_ipch

#include "common.h"
#include "wx_setup.h"

#if USE_IPC
#include <QuickDraw.h>
#include "wx_frame.h"
#include "wx_utils.h"
#include "wb_ipc.h"
#include <PPCToolbox.h>

/*
 * Mini-DDE implementation

   Most transactions involve a topic name and an item name (choose these
   as befits your application).

   A client can:

   - ask the server to execute commands (data) associated with a topic
   - request data from server by topic and item
   - poke data into the server
   - ask the server to start an advice loop on topic/item
   - ask the server to stop an advice loop

   A server can:

   - respond to execute, request, poke and advice start/stop
   - send advise data to client

   Note that this limits the server in the ways it can send data to the
   client, i.e. it can't send unsolicited information.
 *
 */

#ifdef IN_CPROTO
typedef       void    *wxConnection ;
typedef       void    *wxServer;
typedef       void    *wxClient;
#else

class wxIPCObject;

extern wxIPCObject * wxTheIPCObj;

class wxServer;
class wxClient;
class wxConnection: public wxbConnection
{
 public:
#ifdef wx_motif
  unsigned long xtInputId;
#endif
  PPCSessRefNum input_session;
  PPCSessRefNum output_session;

  wxConnection(char *buffer, int size);
  wxConnection(void);
  ~wxConnection(void);

  // Calls that CLIENT can make
  virtual Bool Execute(char *data, int size = -1, int format = wxCF_TEXT);
  virtual char *Request(char *item, int *size = NULL, int format = wxCF_TEXT);
  virtual Bool Poke(char *item, char *data, int size = -1, int format = wxCF_TEXT);
  virtual Bool StartAdvise(char *item);
  virtual Bool StopAdvise(char *item);

  // Calls that SERVER can make
  virtual Bool Advise(char *item, char *data, int size = -1, int format = wxCF_TEXT);

  // Calls that both can make
  Bool Disconnect(void);
	void Notify(Bool notify);  // Internal use only
};

class wxServer: public wxbServer
{
 public:

  PPCPortRefNum server_socket;

  wxServer(void);
  ~wxServer(void);
  Bool Create(char *server_name); // Returns FALSE if can't create server (e.g. port
                                  // number is already in use)
  void PeriodicTask(void);	 // Mac specific
};

class wxClient: public wxbClient
{
 public:
  PPCPortRefNum client_socket;

  wxClient(void);
  ~wxClient(void);
  Bool ValidHost(char *host);
  virtual wxConnection *MakeConnection(char *host, char *server, char *topic);
                                                // Call this to make a connection.
                                                // Returns NULL if cannot.
 	void PeriodicTask(void);	// Mac specific                                          
};

/*
class wxChild: public wxObject
{
 public:
  int the_pid;
  wxChild(void);
  Bool Create(char *command, char *argv[]);
  virtual wxConnection *OnSpawn(int pid);
  virtual void OnDeath(void);
};
*/

#endif // IN_CPROTO
#endif // USE_IPC
#endif // wx_ipc.h
