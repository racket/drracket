/*
 * File:	wb_ipc.cc
 * Purpose:	Interprocess communication implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_ipc.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "@(#)wb_ipc.cc	1.2 5/9/94"; */

#ifdef __GNUG__
#pragma implementation
#pragma implementation "wb_ipcob.h"
#pragma implementation "wb_ipc.h"
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_main.h"
#include "wx_frame.h"
#include "wx_stdev.h"
#include "wx_utils.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#if USE_IPC

#include "wx_ipc.h"

#include <stdio.h>

char *wxDefaultIPCBuffer = NULL;
int wxDefaultIPCBufferSize = 4000;

/*
 * CleanUp
 * (manually called, to avoid linking with unuseful module)
 */
void wxIPCCleanUp()
{
  if (wxDefaultIPCBuffer)
    delete [] wxDefaultIPCBuffer ;
}

/*
 * Initialization
 *
 */

Bool wxIPCInitialized;

wxbIPCObject::wxbIPCObject(void)
{
}

wxbIPCObject::~wxbIPCObject(void)
{
  if (service_name)
    delete[] service_name;

  wxNode *node = connections.First();
  while (node)
  {
    wxConnection *connection = (wxConnection *)node->Data();
    wxNode *next = node->Next();
    connection->OnDisconnect(); // May delete the node implicitly
    node = next;
  }

  // If any left after this, delete them
  node = connections.First();
  while (node)
  {
    wxConnection *connection = (wxConnection *)node->Data();
    delete connection;
    node = connections.First();
  }
}

/*
 * Server
 *
 */

wxbServer::wxbServer(void)
{
  __type = wxTYPE_DDE_SERVER;
}

wxbServer::~wxbServer(void)
{
}

// Default behaviour for accepting a connection
wxConnection *wxbServer::OnAcceptConnection(char *topic)
{
  return new wxConnection;
}


/*
 * Client
 *
 */


wxbClient::wxbClient(void)
{
  __type = wxTYPE_DDE_CLIENT;
}

wxbClient::~wxbClient(void)
{
  wxNode *node = connections.First();
  while (node)
  {
    wxConnection *connection = (wxConnection *)node->Data();
    delete connection;  // Deletes the node implicitly (see ~wxConnection)
    node = connections.First();
  }
}

wxConnection *wxbClient::OnMakeConnection(void)
{
  return new wxConnection;
}

/*
 * Connection
 */

wxbConnection::wxbConnection(char *buffer, int size)
{
  __type = wxTYPE_DDE_CONNECTION;

  if (buffer == NULL)
  {
    if (wxDefaultIPCBuffer == NULL)
      wxDefaultIPCBuffer = new char[wxDefaultIPCBufferSize];
    buf_ptr = wxDefaultIPCBuffer;
    buf_size = wxDefaultIPCBufferSize;
  }
  else
  {
    buf_ptr = buffer;
    buf_size = size;
  }

  topic_name = NULL;

  client = NULL;
  server = NULL;
}

wxbConnection::wxbConnection(void)
{
  __type = wxTYPE_DDE_CONNECTION;

  if (wxDefaultIPCBuffer == NULL)
    wxDefaultIPCBuffer = new char[wxDefaultIPCBufferSize];

  buf_ptr = wxDefaultIPCBuffer;
  buf_size = wxDefaultIPCBufferSize;

  topic_name = NULL;
  client = NULL;
  server = NULL;
}

wxbConnection::~wxbConnection(void)
{
}

Bool wxbConnection::OnDisconnect(void)
{
  // Default action is to delete itself
  delete this;
  return TRUE;
}

// Callbacks to SERVER - override at will
Bool wxbConnection::OnExecute(char *topic, char *data, int size, int format)
{
  return FALSE;
}

char *wxbConnection::OnRequest(char *topic, char *item, int *size, int format)
{
  return NULL;
}

Bool wxbConnection::OnPoke(char *topic, char *item, char *data, int size, int format)
{
  return FALSE;
}

Bool wxbConnection::OnStartAdvise(char *topic, char *item)
{
  return TRUE;
}

Bool wxbConnection::OnStopAdvise(char *topic, char *item)
{
  return TRUE;
}


// Callbacks to CLIENT - override at will
Bool wxbConnection::OnAdvise(char *topic, char *item, char *data, int size, int format)
{
  return FALSE;
}

/*
// Pipes
wxChild::wxChild(void)
{
  the_pid = -1;
}

Bool wxChild::Create(char *command, char *argv[])
{
#ifdef wx_motif
  return FALSE;
#endif
#ifdef wx_xview
  int to_subprocess[2], from_subprocess[2];

  pipe(to_subprocess); pipe(from_subprocess);

  int pid = fork();

  the_pid = pid;
  switch(pid)
  {
    case -1:
    return FALSE;

    case 0:			// child

    // Copy pipe descriptors to stdin and stdout
    dup2(to_subprocess[0], 0); 	dup2(from_subprocess[1], 1);

    // Close unwanted descriptors

    close(to_subprocess[0]); 	close(to_subprocess[1]);
    close(from_subprocess[0]); 	close(from_subprocess[1]);

    // Exec new process

//	execlp("prolog", "prolog", (char *)0); execvp(command, argv);

  // If we get here it failed; give up

    perror("exec");
    _exit(1);		// Use _exit() not exit() after failed exec

    default:			// parent

	break;
  }

  // Close unneeded descriptors

  close(to_subprocess[0]); close(from_subprocess[1]);

  (void)notify_set_wait3_func((Notify_client)this, (Notify_func)wx_child_died, pid);

  wxConnection *connection = this->OnSpawn(pid);
  connection->input_fd = from_subprocess[0];
  connection->output_fd = to_subprocess[1];

  if (connection)
  {
    connection->Notify(TRUE);
    return TRUE;
  }
  else
  { close(to_subprocess[1]); 
    close(from_subprocess[0]);
    return FALSE;
  }
#endif
#ifdef wx_msw
  return FALSE;
#endif
  }

void wxChild::OnDeath(void)
{
}

// Default behaviour
wxConnection *wxChild::OnSpawn(int pid)
{
  return new wxConnection;
}
*/

// Add NULL-terminated string to buffer, returning next start point
int wxAddString(int start, char *info, char *buffer, int size)
{
  if (size < 0)
    size = strlen(info);

  int i;
  for (i = start; i < (start + size); i++)
    buffer[i] = info[i-start];
  buffer[i] = 0;
  return i+1;
}

// Get next position of NULL-terminated string from buffer
char *wxGetNextString(char *buffer)
{
  int i = 0;
  int ch = -1;
  Bool flag = FALSE;
  while (!flag)
  {
    ch = (int)buffer[i];
    if (ch == 0)
    {
      flag = TRUE;
    }
    else
    {
      i ++;
    }
  }
  return buffer + i + 1;
}

#endif // USE_IPC
