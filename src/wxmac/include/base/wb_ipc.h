/*
 * File:	wb_ipc.h
 * Purpose:	
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_ipc.h	1.2 5/9/94" */

/*
 * Purpose:  Interprocess communication implementation. Uses DDE under
 *           Windows, sockets to implement DDE subset under UNIX
 */


#ifndef wxb_ipch
#define wxb_ipch

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_setup.h"

#if USE_IPC

#include "wx_frame.h"
#include "wx_utils.h"
#include "wx_ipcob.h"

// Message codes
enum {
  wxEXECUTE = 1,
  wxREQUEST,
  wxPOKE,
  wxADVISE_START,
  wxADVISE_REQUEST,
  wxADVISE,
  wxADVISE_STOP,
  wxREQUEST_REPLY,
  wxFAIL,
  wxCONNECT,
  wxDISCONNECT
};

// Error codes
#define  wxGENERAL           2
#define  wxBAD_SERVICE_NAME  3

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

// Always call before starting IPC
void wxIPCInitialize(void);

// Always call before exiting
void wxIPCCleanUp(void);

#ifdef IN_CPROTO
typedef       void    *wxbConnection;
typedef       void    *wxbServer;
typedef       void    *wxbClient;
#else

class wxIPCObject;

class wxServer;
class wxClient;
class wxConnection;

class wxbConnection: public wxObject
{
 public:
  char *buf_ptr;
  char *topic_name;
  int buf_size;
  wxServer *server;
  wxClient *client;

  wxbConnection(char *buffer, int size);
  wxbConnection(void);
  ~wxbConnection(void);

  // Callbacks to SERVER - override at will
  virtual Bool OnExecute(char *topic, char *data, int size, int format);
  virtual char *OnRequest(char *topic, char *item, int *size, int format);
  virtual Bool OnPoke(char *topic, char *item, char *data, int size, int format);
  virtual Bool OnStartAdvise(char *topic, char *item);
  virtual Bool OnStopAdvise(char *topic, char *item);

  // Callbacks to CLIENT - override at will
  virtual Bool OnAdvise(char *topic, char *item, char *data, int size, int format);

  // Callbacks to BOTH

  // Default behaviour is to delete connection and return TRUE
  virtual Bool OnDisconnect(void);

  // Calls that CLIENT can make
  virtual Bool Execute(char *data, int size = -1, int format = wxCF_TEXT) = 0;
  virtual char *Request(char *item, int *size = NULL, int format = wxCF_TEXT) = 0;
  virtual Bool Poke(char *item, char *data, int size = -1, int format = wxCF_TEXT) = 0;
  virtual Bool StartAdvise(char *item) = 0;
  virtual Bool StopAdvise(char *item) = 0;

  // Calls that SERVER can make
  virtual Bool Advise(char *item, char *data, int size = -1, int format = wxCF_TEXT) = 0;

  // Calls that both can make
  virtual Bool Disconnect(void) = 0;
  virtual void Notify(Bool notify) = 0;  // Internal use only
};

class wxbServer: public wxIPCObject
{
 public:
  wxbServer(void);
  ~wxbServer(void);
  virtual Bool Create(char *server_name) = 0;
                                  // Returns FALSE if can't create server (e.g. port
                                  // number is already in use)
  virtual wxConnection *OnAcceptConnection(char *topic);
};

class wxbClient: public wxIPCObject
{
 public:
  wxbClient(void);
  ~wxbClient(void);
  virtual Bool ValidHost(char *host) = 0;
  virtual wxConnection *MakeConnection(char *host, char *server, char *topic) = 0;
                                                // Call this to make a connection.
                                                // Returns NULL if cannot.
  virtual wxConnection *OnMakeConnection(void); // Tailor this to return own connection.
};

/*
class wxChild: public wxObject
{
 public:
  int the_pid;
  wxChild(void);
  virtual Bool Create(char *command, char *argv[]);
  virtual wxConnection *OnSpawn(int pid);
  virtual void OnDeath(void);
};
*/

#endif // IN_CPROTO
#ifdef wx_mac
// Some Mac compilers (CodeWarrior) really want to see these decl's
extern int wxAddString(int start, char *info, char *buffer, int size);
extern char *wxGetNextString(char *buffer);

#endif
#endif // USE_IPC
#endif // wxb_ipc.h
