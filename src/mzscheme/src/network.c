/*
  MzScheme
  Copyright (c) 2000 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* This file implements the TCP interface. */

#include "schpriv.h"

#ifndef NO_TCP_SUPPORT
#ifdef USE_TCP

#ifdef UNISTD_INCLUDE
# include <unistd.h>
#endif
#ifdef USE_ULIMIT
# include <ulimit.h>
#endif
#ifdef FILES_HAVE_FDS
# include <sys/types.h>
# include <sys/time.h>
# ifdef BSTRING_INCLUDE
#  include <bstring.h>
# endif
# ifdef SELECT_INCLUDE
#  include <sys/select.h>
# endif
# ifdef USE_BEOS_SOCKET_INCLUDE
#  include <be/net/socket.h>
# endif
#endif
#ifdef IO_INCLUDE
# include <io.h>
#endif
#ifdef NO_ERRNO_GLOBAL
static int mzerrno = 0;
# define errno mzerrno
#else
# include <errno.h>
#endif
#include "schfd.h"

#ifdef USE_MAC_TCP
# include <MacTCP.h>
# include <dnr.c>
#endif

#ifdef USE_UNIX_SOCKETS_TCP
# include <netinet/in.h>
# include <netdb.h>
# include <sys/socket.h>
# include <fcntl.h>
# define TCP_SOCKSENDBUF_SIZE 32768
#endif

#ifdef USE_WINSOCK_TCP
# include <winsock.h>
struct SOCKADDR_IN {
  short sin_family;
  unsigned short sin_port;
  struct in_addr sin_addr;
  char sin_zero[8];
};
#endif

#ifdef USE_MAC_TCP
# define TCP_BUFFER_SIZE 16384
#else
# define TCP_BUFFER_SIZE 512
#endif

#ifdef USE_UNIX_SOCKETS_TCP
typedef int tcp_t;
# define INVALID_SOCKET (-1)
#define closesocket close
#endif

#ifdef USE_WINSOCK_TCP
typedef SOCKET tcp_t;
#endif

#ifdef USE_SOCKETS_TCP
typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  tcp_t s;
  Scheme_Manager_Reference *mref;
} listener_t;
#endif

#ifdef USE_MAC_TCP
typedef struct tcp_t {
  void *create_pb;
  void *current_pb; /* prevents GC during async call */
  StreamPtr stream;
  int state;
  int async_errid;
  Scheme_Object *lock; /* read lock */
} tcp_t;

typedef struct {
  Scheme_Type type; 
  MZ_HASH_KEY_EX
  int portid;
  int count;
  struct Scheme_Tcp **datas;
  Scheme_Manager_Reference *mref;
} listener_t;
# define htons(x) x
# define htonl(x) x
#endif

typedef struct Scheme_Tcp {
  Scheme_Tcp_Buf b;
  tcp_t tcp;
#ifdef USE_MAC_TCP
  struct TCPiopb *activeRcv;
#endif
} Scheme_Tcp;

#ifdef USE_MAC_TCP
static int num_tcp_send_buffers = 0;
static void **tcp_send_buffers;
#endif

#endif /* USE_TCP */

static Scheme_Object *tcp_connect(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_listen(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_stop(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_accept_ready(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_accept(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_listener_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_addresses(int argc, Scheme_Object *argv[]);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

void scheme_init_network(Scheme_Env *env)
{
  if (scheme_starting_up) {
#ifdef MZ_PRECISE_GC
    register_traversers();
#endif

#ifdef USE_MAC_TCP
    REGISTER_SO(tcp_send_buffers);
#endif    
  }

  scheme_add_global_constant("tcp-connect", 
			     scheme_make_prim_w_arity2(tcp_connect,
						       "tcp-connect", 
						       2, 2,
						       2, 2), 
			     env);
  scheme_add_global_constant("tcp-listen", 
			     scheme_make_prim_w_arity(tcp_listen,
						      "tcp-listen", 
						      1, 2), 
			     env);
  scheme_add_global_constant("tcp-close", 
			     scheme_make_prim_w_arity(tcp_stop,
						      "tcp-close", 
						      1, 1), 
			     env);
  scheme_add_global_constant("tcp-accept-ready?", 
			     scheme_make_prim_w_arity(tcp_accept_ready,
						      "tcp-accept-ready?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("tcp-accept", 
			     scheme_make_prim_w_arity2(tcp_accept,
						       "tcp-accept", 
						       1, 1,
						       2, 2), 
			     env);
  scheme_add_global_constant("tcp-listener?", 
			     scheme_make_folding_prim(tcp_listener_p,
						      "tcp-listener?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("tcp-addresses", 
			     scheme_make_folding_prim(tcp_addresses,
						      "tcp-addresses", 
						      1, 1, 1), 
			     env);
}


/*========================================================================*/
/*                             TCP glue                                   */
/*========================================================================*/


/* These two need o be outside of USE_TCP */
#define PORT_ID_TYPE "exact integer in [1, 65535]"
#define CHECK_PORT_ID(obj) (SCHEME_INTP(obj) && (SCHEME_INT_VAL(obj) >= 1) && (SCHEME_INT_VAL(obj) <= 65535))


#ifdef USE_TCP

#ifdef USE_SOCKETS_TCP
#define MAKE_TCP_ARG tcp_t tcp, 
#else
#define MAKE_TCP_ARG
#endif

#define REGISTER_SOCKET(s) /**/
#define UNREGISTER_SOCKET(s) /**/

#ifdef USE_UNIX_SOCKETS_TCP
typedef struct sockaddr_in tcp_address;
#endif
#ifdef USE_WINSOCK_TCP
typedef struct SOCKADDR_IN tcp_address;
# undef REGISTER_SOCKET
# undef UNREGISTER_SOCKET
# define REGISTER_SOCKET(s) winsock_remember(s)
# define UNREGISTER_SOCKET(s) winsock_forget(s)
#endif

#ifdef USE_WINSOCK_TCP

/******************************* WinSock ***********************************/

static int wsr_size = 0;
static tcp_t *wsr_array;

static void winsock_remember(tcp_t s)
{
  int i, new_size;
  tcp_t *naya;

  for (i = 0; i < wsr_size; i++)
    if (!wsr_array[i]) {
      wsr_array[i] = s;
      return;
    }

  if (!wsr_size) {
    REGISTER_SO(wsr_array);
    new_size = 32;
  } else
    new_size = 2 * wsr_size;

  naya = MALLOC_N_ATOMIC(tcp_t, new_size);
  for (i = 0; i < wsr_size; i++)
    naya[i] = wsr_array[i];

  naya[wsr_size] = s;

  wsr_array = naya;
  wsr_size = new_size;  
}

static void winsock_forget(tcp_t s)
{
  int i;

  for (i = 0; i < wsr_size; i++)
    if (wsr_array[i] == s) {
      wsr_array[i] = (tcp_t)NULL;
      return;
    }
}

static int winsock_done(void)
{
  int i;

  for (i = 0; i < wsr_size; i++)
    if (wsr_array[i]) {
      closesocket(wsr_array[i]);
      wsr_array[i] = (tcp_t)NULL;
    }

  return WSACleanup();
}

static void TCP_INIT(char *name)
{
  static int started = 0;
  
  if (!started) {
    WSADATA data;
    if (!WSAStartup(MAKEWORD(1, 1), &data)) {
      started = 1;
      _onexit(winsock_done);
      return;
    }
  } else
    return;
  
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "%s: not supported on this machine"
		   " (no winsock driver)",
		   name);
}
#else
#ifdef USE_MAC_TCP

/***************************** Mac *******************************************/

/* Much of this is derived from (or at least influenced by) GUSI's TCP
   socket implementation, by Matthias Neeracher, which was derived from
   a library by Charlie Reiman Tom Milligan */

static short tcpDriverId;

#define	SOCK_STATE_NO_STREAM 0 /* Socket doesn't have a MacTCP stream yet */
#define	SOCK_STATE_UNCONNECTED 1 /* Socket is unconnected. */
#define	SOCK_STATE_LISTENING 2 /* Socket is listening for connection. */
#define	SOCK_STATE_CONNECTING 4 /* Socket is initiating a connection. */
#define	SOCK_STATE_CONNECTED 5 /* Socket is connected. */
#define	SOCK_STATE_CLOSING 6 /* Socket is closing */
#define	SOCK_STATE_CLOSED 8 /* Socket closed nicely */

typedef struct TCPiopbX {
  TCPiopb pb;
  Scheme_Tcp *data;
  struct TCPiopbX *next;
} TCPiopbX;

typedef struct {
  MZTAG_IF_REQUIRED
  wdsEntry e[2];
  TCPiopbX *xpb;
} WriteData;

static TCPiopbX *active_pbs;

static pascal void dnr_done(struct hostInfo *, int * done)
{
  *done = true;
}

static ResultUPP u_dnr_done;

static pascal void tcp_notify(StreamPtr stream, unsigned short eventCode,
			      Ptr userDataPtr, unsigned short something,
			      struct ICMPReport *reportPtr)
{
  tcp_t *t = (tcp_t *)userDataPtr;

  switch (eventCode) {
  case TCPClosing:
    t->state = SOCK_STATE_CLOSING;
    break;
    
  case TCPTerminate:
    if (t->state == SOCK_STATE_LISTENING)
      t->state = SOCK_STATE_CLOSED;
    else if (t->state == SOCK_STATE_CLOSING)
      t->state == SOCK_STATE_CLOSED;
    else
      t->state = SOCK_STATE_UNCONNECTED;
    break;
  }
}

static TCPNotifyUPP u_tcp_notify;

static void tcp_connect_done(TCPiopbX *pbx)
{
  if (!pbx->pb.ioResult)
    pbx->data->tcp.state = SOCK_STATE_CONNECTED;
}

static TCPIOCompletionUPP u_tcp_connect_done;

static void tcp_listen_done(TCPiopbX *pbx)
{
  TCPiopb *pb = &pbx->pb;
  Scheme_Tcp *data = pbx->data;

  switch(pb->ioResult) {
  case noErr:
    data->tcp.state = SOCK_STATE_CONNECTED;
    break;
    
  case openFailed:
  case invalidStreamPtr:
  case connectionExists:
  case duplicateSocket:
  case commandTimeout:
  default:
    data->tcp.state = SOCK_STATE_UNCONNECTED;
    data->tcp.async_errid = -pb->ioResult;
    break;
  }
}

static TCPIOCompletionUPP u_tcp_listen_done;

static void tcp_recv_done(TCPiopbX *pbx)
{
  TCPiopb *pb = &pbx->pb;
  Scheme_Tcp *data = pbx->data;
  
  if (!pb->ioResult)
    data->b.bufmax = pb->csParam.receive.rcvBuffLen;
}

static TCPIOCompletionUPP u_tcp_recv_done;

static void tcp_send_done(TCPiopbX *pbx)
{
  TCPiopb *pb = &pbx->pb;
  Scheme_Tcp *data = pbx->data;

  /* mark it free: */
  ((WriteData *)(pb->csParam.send.wdsPtr))->xpb = NULL;

  switch (pb->ioResult) {
  case noErr:
    break;
  case ipNoFragMemErr:
  case connectionClosing:
  case connectionTerminated:
  case connectionDoesntExist:
  case ipDontFragErr:
  case invalidStreamPtr:
  case invalidLength:
  case invalidWDS:
  default:
    data->tcp.state = SOCK_STATE_UNCONNECTED;
    data->tcp.async_errid = -pb->ioResult;
    break;
  }
}

static TCPIOCompletionUPP u_tcp_send_done;

/************** Mac Set-up *****************/

static void tcp_cleanup(void);

static void TCP_INIT(char *name)
{
  ParamBlockRec pb;
  short errNo;
  struct GetAddrParamBlock pbr;
	
  pb.ioParam.ioCompletion = 0L; 
  pb.ioParam.ioNamePtr = (StringPtr) "\p.IPP"; 
  pb.ioParam.ioPermssn = fsCurPerm;
  
  if ((errNo = PBOpenSync(&pb))
      || (errNo = OpenResolver(NULL))) {
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "%s: TCP initialization error (%e)",
		     name, (int)errNo);
  }
		
  tcpDriverId = pb.ioParam.ioRefNum; 
  
  u_dnr_done = NewResultProc(dnr_done);
  u_tcp_notify = NewTCPNotifyProc(tcp_notify);
  u_tcp_connect_done = NewTCPIOCompletionProc(tcp_connect_done);
  u_tcp_listen_done = NewTCPIOCompletionProc(tcp_listen_done);
  u_tcp_recv_done = NewTCPIOCompletionProc(tcp_recv_done);
  u_tcp_send_done = NewTCPIOCompletionProc(tcp_send_done);
  
  REGISTER_SO(active_pbs);
  
  atexit(tcp_cleanup);
}

static int tcp_addr(char *address, struct hostInfo *info)
{
  int tries = 3;
  long *done = MALLOC_ONE_ATOMIC(long);
  
  /* Check for numerical address: */
  {
     unsigned char *s = (unsigned char *)address, n[4];
     int p = 0, v = 0;
     while (*s) {
       if (isdigit(*s)) {
         if (v < 256)
           v = (v * 10) + (*s - '0');
       } else if (*s == '.') {
         if (p < 4) {
           n[p] = v;
           p++;
         }
         v = 0;
       } else
         break;
       s++;
     }
     
     if (p == 3) {
       n[p] = v;
       p++;
     }
     
     if (!*s && (p == 4)
         && (s[0] < 256) && (s[1] < 256)
         && (s[2] < 256) && (s[3] < 256)) {
       /* Numerical address */
       info->addr[0] = *(unsigned long *)n;
       return 0;
     }
  }
  
 try_again:
  *done = 0;
  info->rtnCode = 0;
  if (StrToAddr(address, info, u_dnr_done, (char *)done) == cacheFault) {
    /* FIXME: If we get a break, it's possible that `info' and `done' will be
              GCed before the async call completes. */
    while (!*done) { scheme_process_block(0.25); }
    scheme_current_process->ran_some = 1;
  }
  if (info->rtnCode == cacheFault) {
    if (--tries)
      goto try_again;
  }
  if (info->rtnCode)
    return info->rtnCode;
  if (info->cname[0] == 0)
    return -42;
  
  return 0;
}

/* Forward prototype: */
static Scheme_Tcp *make_tcp_port_data(MAKE_TCP_ARG int refcount);

#define STREAM_BUFFER_SIZE 131072

static TCPiopbX *mac_make_xpb(Scheme_Tcp *data)
{
  TCPiopbX *xpb;

  /* FIXME, precise GC: no GC tag... */
  xpb = (TCPiopbX *)scheme_malloc(sizeof(TCPiopbX));
  
  memcpy(xpb, data->tcp.create_pb, sizeof(TCPiopb));

  xpb->data = data;
  
  data->tcp.current_pb = xpb;

  return xpb;
}

static int mac_tcp_make(TCPiopbX **_xpb, TCPiopb **_pb, Scheme_Tcp **_data)
{
  TCPiopbX *xpb;
  TCPiopb *pb;
  Scheme_Tcp *data;
  int errid;

  data = make_tcp_port_data(2);
  
  /* FIXME, precise GC: no GC tag... */
  xpb = (TCPiopbX *)scheme_malloc(sizeof(TCPiopbX));
  xpb->next = active_pbs;
  active_pbs = xpb;
  
  pb = (TCPiopb *)xpb;

  pb->ioCRefNum = tcpDriverId;
  pb->csCode = TCPCreate;
  pb->csParam.create.rcvBuff = (char *)scheme_malloc_atomic(STREAM_BUFFER_SIZE);
  pb->csParam.create.rcvBuffLen = STREAM_BUFFER_SIZE;
  pb->csParam.create.notifyProc = u_tcp_notify;
  pb->csParam.create.userDataPtr = (char *)&data->tcp;
  
  xpb->data = data;
  
  if ((errid = PBControlSync((ParamBlockRec*)pb)))
    return errid;
	
  data->tcp.create_pb = (void *)pb;
  data->tcp.stream = pb->tcpStream;
  data->tcp.async_errid = -1;

  *_xpb = xpb;
  *_pb = pb;
  *_data = data;

  return 0;
}

static void mac_tcp_close(Scheme_Tcp *data)
{
  TCPiopb *pb;
  
  pb = (TCPiopb *)mac_make_xpb(data);
  
  pb->ioCompletion = NULL;
  pb->csCode = TCPClose;
  pb->csParam.close.validityFlags = timeoutValue | timeoutAction;
  pb->csParam.close.ulpTimeoutValue = 60 /* seconds */;
  pb->csParam.close.ulpTimeoutAction = 1 /* 1:abort 0:report */;
  PBControlSync((ParamBlockRec*)pb);

  pb->csCode = TCPRelease;
  PBControlSync((ParamBlockRec*)pb);

 {
    TCPiopbX *x, *prev = NULL;
    x = active_pbs;
    while (x) {
      if (x->data->tcp.stream == data->tcp.stream) {
	if (!prev)
	  active_pbs = x->next;
	else
	  prev->next = x->next;
	break;
      } else {
        prev = x;
        x = x->next;
      }
    }
  }
}

static int mac_tcp_listen(int id, Scheme_Tcp **_data)
{
  TCPiopbX *xpb;
  TCPiopb *pb;
  Scheme_Tcp *data;
  int errid;
  
  if (!(errid = mac_tcp_make(&xpb, &pb, &data))) {
    data->tcp.state = SOCK_STATE_LISTENING;
    
    xpb = mac_make_xpb(data);
    pb = (TCPiopb *)xpb;

    pb->ioCompletion = u_tcp_listen_done;
    pb->csCode = TCPPassiveOpen;
    pb->csParam.open.validityFlags = timeoutValue | timeoutAction;
    pb->csParam.open.ulpTimeoutValue = 0 /* seconds; 0 = infinity */;
    pb->csParam.open.ulpTimeoutAction = 0 /* 1:abort 0:report */;
    pb->csParam.open.commandTimeoutValue = 0 /* seconds; 0 = infinity */;
    pb->csParam.open.remoteHost = 0;
    pb->csParam.open.remotePort = 0;
    pb->csParam.open.localHost = 0;
    pb->csParam.open.localPort = id;
    pb->csParam.open.dontFrag = 0;
    pb->csParam.open.timeToLive = 0;
    pb->csParam.open.security = 0;
    pb->csParam.open.optionCnt = 0;

    if ((errid = PBControlAsync((ParmBlkPtr)pb))) {
      data->tcp.state = SOCK_STATE_UNCONNECTED;
      mac_tcp_close(data);
      return errid;
    } else {
      *_data = data;
      return 0;
    }
  } else
    return errid;
}

static void tcp_cleanup(void)
{
  while (active_pbs) {
    TCPiopbX *pb = active_pbs;
    active_pbs = active_pbs->next;
    mac_tcp_close(pb->data);
  }
}

#else
#define TCP_INIT(x) /* nothing */
#endif
#endif

/*========================================================================*/
/*                       TCP ports and listeners                          */
/*========================================================================*/

#ifdef USE_SOCKETS_TCP
#define LISTENER_WAS_CLOSED(x) (((listener_t *)(x))->s == INVALID_SOCKET)
#endif
#ifdef USE_MAC_TCP
#define LISTENER_WAS_CLOSED(x) !((listener_t *)(x))->datas
#endif
#ifndef LISTENER_WAS_CLOSED
#define LISTENER_WAS_CLOSED(x) 0
#endif

/* Forward declaration */
static int stop_listener(Scheme_Object *o);

static int tcp_check_accept(Scheme_Object *listener)
{
#ifdef USE_SOCKETS_TCP
  tcp_t s;
  DECL_FDSET(readfds, 1);
  DECL_FDSET(exnfds, 1);
  struct timeval time = {0, 0};

  INIT_DECL_FDSET(readfds, 1);
  INIT_DECL_FDSET(exnfds, 1);

  if (LISTENER_WAS_CLOSED(listener))
    return 1;

  s = ((listener_t *)listener)->s;

  MZ_FD_ZERO(readfds);
  MZ_FD_ZERO(exnfds);
  MZ_FD_SET(s, readfds);
  MZ_FD_SET(s, exnfds);
    
  return select(s + 1, readfds, NULL, exnfds, &time);
#endif
#ifdef USE_MAC_TCP
  int i, count;
  Scheme_Tcp **datas;
  count = ((listener_t *)listener)->count;
  datas = ((listener_t *)listener)->datas;

  if (LISTENER_WAS_CLOSED(listener))
    return 1;

  for (i = 0; i < count; i++)
    if (datas[i] && (datas[i]->tcp.state != SOCK_STATE_LISTENING))
      return 1;

  return 0;
#endif
}

static void tcp_accept_needs_wakeup(Scheme_Object *listener, void *fds)
{
#ifdef USE_SOCKETS_TCP
  tcp_t s = ((listener_t *)listener)->s;
  void *fds2;

  fds2 = MZ_GET_FDSET(fds, 2);

  MZ_FD_SET(s, (fd_set *)fds);
  MZ_FD_SET(s, (fd_set *)fds2);
#endif
}

static int tcp_check_connect(Scheme_Object *connector)
{
#ifdef USE_MAC_TCP
  return ((TCPiopb *)connector)->ioResult != inProgress;
#else
#ifdef USE_SOCKETS_TCP
  tcp_t s;
  DECL_FDSET(writefds, 1);
  DECL_FDSET(exnfds, 1);
  struct timeval time = {0, 0};

  INIT_DECL_FDSET(writefds, 1);
  INIT_DECL_FDSET(exnfds, 1);

  s = (tcp_t)connector;

  MZ_FD_ZERO(writefds);
  MZ_FD_SET(s, writefds);
  MZ_FD_ZERO(exnfds);
  MZ_FD_SET(s, exnfds);
    
  if (!select(s + 1, NULL, writefds, exnfds, &time))
    return 0;
  if (FD_ISSET(s, exnfds))
    return -1;
  else
    return 1;
#else
  return 0;
#endif
#endif
}

static void tcp_connect_needs_wakeup(Scheme_Object *connector, void *fds)
{
#ifdef USE_SOCKETS_TCP
  void *fds1, *fds2;
  tcp_t s = (tcp_t)connector;
  
  fds1 = MZ_GET_FDSET(fds, 1);
  fds2 = MZ_GET_FDSET(fds, 2);

  MZ_FD_SET(s, (fd_set *)fds1);
  MZ_FD_SET(s, (fd_set *)fds2);
#endif
}

#ifdef USE_MAC_TCP
static void tcp_read_needs_wakeup(Scheme_Object *connector, void *fds)
{
}

static int tcp_check_read(Scheme_Object *pb)
{
  return (((TCPiopb *)pb)->ioResult != inProgress);
}
#endif

static int tcp_check_write(Scheme_Object *conn)
{
  Scheme_Tcp *data = (Scheme_Tcp *)conn;

#ifdef USE_SOCKETS_TCP
  tcp_t s;
  DECL_FDSET(writefds, 1);
  DECL_FDSET(exnfds, 1);
  struct timeval time = {0, 0};

  INIT_DECL_FDSET(writefds, 1);
  INIT_DECL_FDSET(exnfds, 1);

  s = data->tcp;

  MZ_FD_ZERO(writefds);
  MZ_FD_SET(s, writefds);
  MZ_FD_ZERO(exnfds);
  MZ_FD_SET(s, exnfds);
  
  return select(s + 1, NULL, writefds, exnfds, &time);
#else
  TCPiopbX *xpb;
  TCPiopb *pb;
  int bytes;

  xpb = mac_make_xpb(data);
  pb = (TCPiopb *)xpb;
    
  pb->csCode = TCPStatus;
  if (PBControlSync((ParamBlockRec*)pb))
    bytes = -1;
  else {
    bytes = pb->csParam.status.sendWindow - pb->csParam.status.amtUnackedData;
    if (bytes < 0)
      bytes = 0;
  }
  
  return !!bytes;
#endif
}

static void tcp_write_needs_wakeup(Scheme_Object *conn, void *fds)
{
#ifdef USE_SOCKETS_TCP
  void *fds1, *fds2;
  tcp_t s = ((Scheme_Tcp *)conn)->tcp;
  
  fds1 = MZ_GET_FDSET(fds, 1);
  fds2 = MZ_GET_FDSET(fds, 2);

  MZ_FD_SET(s, (fd_set *)fds1);
  MZ_FD_SET(s, (fd_set *)fds2);
#endif
}


static Scheme_Tcp *make_tcp_port_data(MAKE_TCP_ARG int refcount)
{
  Scheme_Tcp *data;
  char *bfr;
  
  data = MALLOC_ONE_RT(Scheme_Tcp);
#ifdef MZTAG_REQUIRED
  data->b.type = scheme_rt_tcp;
#endif
#ifdef USE_SOCKETS_TCP
  data->tcp = tcp;
#endif

  bfr = (char *)scheme_malloc_atomic(TCP_BUFFER_SIZE);
  data->b.buffer = bfr;

  data->b.bufpos = 0;
  data->b.bufmax = 0;
  data->b.hiteof = 0;
  data->b.refcount = refcount;

#ifndef USE_MAC_TCP
# ifdef USE_WINSOCK_TCP
  {
    unsigned long ioarg = 1;
    ioctlsocket(tcp, FIONBIO, &ioarg);
  }
# else
  fcntl(tcp, F_SETFL, MZ_NONBLOCKING);
# endif
#endif

  return data;
}

static int tcp_char_ready (Scheme_Input_Port *port)
{
  Scheme_Tcp *data;
#ifdef USE_SOCKETS_TCP
  DECL_FDSET(readfds, 1);
  DECL_FDSET(exfds, 1);
  struct timeval time = {0, 0};

  INIT_DECL_FDSET(readfds, 1);
  INIT_DECL_FDSET(exfds, 1);
#endif

  data = (Scheme_Tcp *)port->port_data;

  if (data->b.hiteof)
    return 1;
  if (data->b.bufpos < data->b.bufmax)
    return 1;

#ifdef USE_SOCKETS_TCP
  MZ_FD_ZERO(readfds);
  MZ_FD_ZERO(exfds);
  MZ_FD_SET(data->tcp, readfds);
  MZ_FD_SET(data->tcp, exfds);
    
  return select(data->tcp + 1, readfds, NULL, exfds, &time);
#endif

#ifdef USE_MAC_TCP
  if (data->tcp.state == SOCK_STATE_CONNECTED) {
    /* socket is connected */
    TCPiopbX *xpb;
    TCPiopb *pb;
    
    xpb = mac_make_xpb(data);
    pb = (TCPiopb *)xpb;
    
    pb->csCode = TCPStatus;
    pb->ioCompletion = NULL;

    if (PBControlSync((ParamBlockRec*)pb))
      return 1;
      
    if (pb->csParam.status.amtUnreadData)
      return 1;
 } else
   return 1;
#endif

  return 0;
}

static int tcp_getc(Scheme_Input_Port *port)
{
  int errid;
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

#ifdef USE_MAC_TCP
 top:
#endif

  if (data->b.hiteof)
    return EOF;

#ifdef USE_MAC_TCP
  if (!data->activeRcv)
#endif
    if (data->b.bufpos < data->b.bufmax)
      /* NOTE: this fast path is also inlined in scheme_get_chars */
      return (unsigned char)data->b.buffer[data->b.bufpos++];

  if (!tcp_char_ready(port)) {
#ifdef USE_SOCKETS_TCP
    scheme_current_process->block_descriptor = PORT_BLOCKED;
    scheme_current_process->blocker = (Scheme_Object *)port;
#endif
    do {
      scheme_process_block((float)0.0);
    } while (!tcp_char_ready(port));
#ifdef USE_SOCKETS_TCP
    scheme_current_process->block_descriptor = NOT_BLOCKED;
    scheme_current_process->blocker = NULL;
#endif
    scheme_current_process->ran_some = 1;
  }

#ifdef USE_SOCKETS_TCP
  data->b.bufmax = recv(data->tcp, data->b.buffer, TCP_BUFFER_SIZE, 0);
  errid = errno;
#endif
#ifdef USE_MAC_TCP
  /* Allow only one read at a time: */
  if (!data->tcp.lock)
    data->tcp.lock = scheme_make_sema(0);
  else {
    if (!scheme_wait_sema(data->tcp.lock, 1)) {
      /* Do it the hard way: */
      scheme_wait_sema(data->tcp.lock, 0);
      scheme_post_sema(data->tcp.lock);
      goto top;
    }
  }
  
  if (data->activeRcv || (data->tcp.state == SOCK_STATE_CONNECTED)) {
    /* socket is connected or an old recv is unfinished */
    TCPiopb *pb;    

    if (data->activeRcv) {
      pb = data->activeRcv;
    } else {
      pb = (TCPiopb *)mac_make_xpb(data);
    
      pb->csCode = TCPRcv;
      pb->ioCompletion = u_tcp_recv_done;
      pb->csParam.receive.commandTimeoutValue = 0; /* seconds, 0 = blocking */
      pb->csParam.receive.rcvBuff = data->b.buffer;
      pb->csParam.receive.rcvBuffLen = TCP_BUFFER_SIZE;
    
      data->activeRcv = pb;

      PBControlAsync((ParamBlockRec*)pb);
    }

    BEGIN_ESCAPEABLE(scheme_post_sema(data->tcp.lock));
    scheme_block_until(tcp_check_read, tcp_read_needs_wakeup, pb, 0);
    END_ESCAPEABLE();

    data->activeRcv = NULL;
    
    switch((errid = pb->ioResult)) {
    case noErr:
    case connectionClosing:
    case connectionTerminated:
      errid = 0;
      break;
    case commandTimeout:
    case connectionDoesntExist:
    case invalidStreamPtr:
    case invalidLength:
    case invalidBufPtr:
    default:
      break;
    }
  } else if (data->tcp.state == SOCK_STATE_CLOSING 
             || data->tcp.state == SOCK_STATE_CLOSED) {
    data->b.bufmax = 0;
    errid = 0;
  } else
    errid = data->tcp.async_errid;
  
  if (errid)
    data->b.bufmax = -1;
    
  scheme_post_sema(data->tcp.lock);
#endif
  
  if (data->b.bufmax == -1) {
    if (scheme_return_eof_for_error()) {
      return EOF;
    } else {
      scheme_raise_exn(MZEXN_I_O_PORT_READ,
		       port,
		       "tcp-read: error reading (%e)",
		       errid);
    }
  } else if (!data->b.bufmax) {
    data->b.hiteof = 1;
    return EOF;
  }

  data->b.bufpos = 1;
    
  return (unsigned char)data->b.buffer[0];
}

static void tcp_need_wakeup(Scheme_Input_Port *port, void *fds)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

#ifdef USE_SOCKETS_TCP
  {
    void *fds2;
  
    fds2 = MZ_GET_FDSET(fds, 2);
    
    MZ_FD_SET(data->tcp, (fd_set *)fds);
    MZ_FD_SET(data->tcp, (fd_set *)fds2);
  }
#endif
}

static void tcp_close_input(Scheme_Input_Port *port)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

  if (--data->b.refcount)
    return;

#ifdef USE_SOCKETS_TCP
  UNREGISTER_SOCKET(data->tcp);
  closesocket(data->tcp);
#endif
#ifdef USE_MAC_TCP
  mac_tcp_close(data);
#endif

  --scheme_file_open_count;
}

/* forward decls: */
static void tcp_write_string(char *s, long d, long len, Scheme_Output_Port *port);
static void tcp_close_output(Scheme_Output_Port *port);

int scheme_tcp_write_nb_string(char *s, long len, long offset, int rarely_block, Scheme_Output_Port *port)
{
  /* TCP writes aren't buffered at all right now. */
  /* If rarely_block is nonzero, it means only write as much as
     can be flushed immediately, blocking only if nothing
     can be written. */

  Scheme_Tcp *data;
  int errid, would_block = 0;
  int sent;

  if (!len)
    return 0;

  data = (Scheme_Tcp *)port->port_data;

 top:

#ifdef USE_SOCKETS_TCP
  if ((sent = send(data->tcp, s + offset, len, 0)) != len) {
#ifdef USE_WINSOCK_TCP
    errid = WSAGetLastError();
# define SEND_BAD_MSG_SIZE(e) (e == WSAEMSGSIZE)
# define SEND_WOULD_BLOCK(e) \
    ((e == WSAEWOULDBLOCK) || (e == WSAEINPROGRESS))
#else
    errid = errno;
# ifdef SEND_IS_NEVER_TOO_BIG
#  define SEND_BAD_MSG_SIZE(errid) 0
# else
#  define SEND_BAD_MSG_SIZE(errid) (errid == EMSGSIZE)
# endif
# define SEND_WOULD_BLOCK(errid) \
   ((errid == EWOULDBLOCK) || (errid == EAGAIN)\
    || (errid == EINPROGRESS) || (errid == EALREADY))
#endif
    if (sent > 0) {
      /* Some data was sent. Return, or recur to handle the rest. */
      if (rarely_block)
	return sent;
      else
	scheme_tcp_write_nb_string(s, len - sent, offset + sent, rarely_block, port);
      errid = 0;
    } else if ((len > 1) && SEND_BAD_MSG_SIZE(errid)) {
      /* split the message and try again: */
      int half = (len / 2);
      sent = scheme_tcp_write_nb_string(s, half, offset, rarely_block, port);
      if (rarely_block)
	return sent;
      sent = scheme_tcp_write_nb_string(s, len - half, offset + half, 0, port);
      sent += half;
      errid = 0;
    } else if (SEND_WOULD_BLOCK(errid)) {
      errid = 0;
      would_block = 1;
    }
  } else
    errid = 0;
#endif
#ifdef USE_MAC_TCP
  errid = 0;
  if (data->tcp.state == SOCK_STATE_CONNECTED) {
    /* socket is connected */
    TCPiopbX *xpb;
    TCPiopb *pb;
    int bytes;

    xpb = mac_make_xpb(data);
    pb = (TCPiopb *)xpb;
    
    pb->csCode = TCPStatus;
    if ((errid = PBControlSync((ParamBlockRec*)pb)))
      bytes = 0;
    else {
      bytes = pb->csParam.status.sendWindow - pb->csParam.status.amtUnackedData;
      if (bytes < 0)
	bytes = 0;
    }
    
    if (bytes >= len) {
      WriteData *wd;
      wdsEntry *e;
      int i;
      
      wd = NULL;
      for (i = 0; i < num_tcp_send_buffers; i++)
	if (!((WriteData *)(tcp_send_buffers[i]))->xpb) {
	  wd = (WriteData *)(tcp_send_buffers[i]);
	  break;
	}
      
      if (!wd) {
	void **naya;
	int nayac;
	
	nayac = (2 * num_tcp_send_buffers) + 1;
	naya = MALLOC_N(void *, nayac);
	memcpy(naya, tcp_send_buffers, sizeof(void *) * num_tcp_send_buffers);
	for (i = num_tcp_send_buffers; i < nayac; i++) {
	  wd = MALLOC_ONE_RT(WriteData);
#ifdef MZTAG_REQUIRED
	  wd->type = scheme_rt_write_data;
#endif
	  wd->xpb = NULL;
	  e = wd->e;
	  e[0].ptr = NULL;
	  e[1].ptr = NULL;
	  e[1].length = 0;
	  naya[i] = (void *)e;
	}

	wd = (WriteData *)naya[num_tcp_send_buffers];
	
	tcp_send_buffers = naya;
	num_tcp_send_buffers = nayac;
      }

      wd->xpb = xpb;
      e = wd->e;

      e[0].ptr = (Ptr)scheme_malloc_atomic(len);
      memcpy(e[0].ptr, s + offset, len);
      e[0].length = len;
      e[1].ptr = NULL;
      e[1].length = 0;

      pb->csCode = TCPSend;
      pb->ioCompletion = u_tcp_send_done;
      pb->csParam.send.validityFlags = timeoutValue | timeoutAction;
      pb->csParam.send.ulpTimeoutValue = 60 /* seconds */;
      pb->csParam.send.ulpTimeoutAction = 1 /* 0:abort 1:report */;
      pb->csParam.send.pushFlag = 1;
      pb->csParam.send.urgentFlag = 0;
      pb->csParam.send.wdsPtr = (Ptr)e;
      pb->csParam.send.sendFree = 0;
      pb->csParam.send.sendLength = 0;
      
      errid = PBControlAsync((ParamBlockRec*)pb);
    } else if (!errid) {
      if (bytes) {
      	/* Do partial write: */
        sent = scheme_tcp_write_nb_string(s, bytes, offset, rarely_block, port);
	if (rarely_block)
	  return sent;
        sent = scheme_tcp_write_nb_string(s, len - bytes, offset + bytes, 0, port);
	sent += bytes;
      } else
        would_block = 1;
    }
  } else
    errid = data->tcp.async_errid;
#endif

  if (would_block) {
    /* Block for writing: */
    scheme_block_until(tcp_check_write, tcp_write_needs_wakeup, data, (float)0.0);

    /* Ok - try again! */
    would_block = 0;
    goto top;
  }

  if (errid)
    scheme_raise_exn(MZEXN_I_O_PORT_WRITE,
		     port,
		     "tcp-write: error writing (%e)",
		     errid);

  return sent;
}

static void tcp_write_string(char *s, long d, long len, Scheme_Output_Port *port)
{
  scheme_tcp_write_nb_string(s, len, d, 0, port);
}

static void tcp_close_output(Scheme_Output_Port *port)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

  if (--data->b.refcount)
    return;

#ifdef USE_UNIX_SOCKETS_TCP
  close(data->tcp);
#endif
#ifdef USE_WINSOCK_TCP
  shutdown(data->tcp, 2);
#endif
#ifdef USE_MAC_TCP
  mac_tcp_close(data);
#endif
}

static Scheme_Object *
make_named_tcp_input_port(void *data, const char *name)
{
  Scheme_Input_Port *ip;

  ip = _scheme_make_input_port(scheme_tcp_input_port_type,
			       data,
			       tcp_getc,
			       NULL,
			       tcp_char_ready,
			       tcp_close_input,
			       tcp_need_wakeup,
			       1);

  ip->name = (char *)name;

  return (Scheme_Object *)ip;
}

static Scheme_Object *
make_tcp_output_port(void *data)
{
  return (Scheme_Object *)scheme_make_output_port(scheme_tcp_output_port_type,
						  data,
						  tcp_write_string,
						  tcp_close_output,
						  1);
}

#endif /* USE_TCP */

/*========================================================================*/
/*                         TCP Scheme interface                           */
/*========================================================================*/

# ifdef PROTOENT_IS_INT
#  define PROTO_P_PROTO PROTOENT_IS_INT
# else
#  define PROTO_P_PROTO proto->p_proto
# endif

# ifndef MZ_PF_INET
#  define MZ_PF_INET PF_INET
# endif

static Scheme_Object *tcp_connect(int argc, Scheme_Object *argv[])
{
  char * volatile address = "", * volatile errmsg = "";
  unsigned short origid, id;
  int errpart = 0, errid = 0;
#ifdef USE_SOCKETS_TCP
  struct hostent *host;
  tcp_address tcp_connect_dest_addr; /* Use a long name for precise GC's xform.ss */
# ifndef PROTOENT_IS_INT
  struct protoent *proto;
# endif
#endif

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("tcp-connect", "string", 0, argc, argv);
  if (!CHECK_PORT_ID(argv[1]))
    scheme_wrong_type("tcp-connect", PORT_ID_TYPE, 1, argc, argv);

#ifdef USE_TCP
  TCP_INIT("tcp-connect");
#endif

  address = SCHEME_STR_VAL(argv[0]);
  origid = (unsigned short)SCHEME_INT_VAL(argv[1]);

#ifdef USE_TCP
  /* Set id in network order: */
  id = htons(origid);
#endif

#ifdef USE_MAC_TCP
  {
    TCPiopbX *xpb;
    TCPiopb *pb;
    Scheme_Tcp *data;
    int errNo;
    struct hostInfo *dest_host;
    Scheme_Object *v[2];
    
    dest_host = MALLOC_ONE_ATOMIC(struct hostInfo);
    if ((errNo = tcp_addr(address, dest_host))) {
      errpart = 1;
      errmsg = "; host not found";
      goto tcp_error;
    }
    
    if ((errNo = mac_tcp_make(&xpb, &pb, &data))) {
      errpart = 2;
      goto tcp_error;
    }

    data->tcp.state = SOCK_STATE_CONNECTING;
    
    pb->ioCompletion = u_tcp_connect_done;
    pb->csCode = TCPActiveOpen;
    pb->csParam.open.validityFlags = timeoutValue | timeoutAction;
    pb->csParam.open.ulpTimeoutValue = 60 /* seconds */;
    pb->csParam.open.ulpTimeoutAction = 1 /* 1:abort 0:report */;
    pb->csParam.open.commandTimeoutValue = 0;
    pb->csParam.open.remoteHost = dest_host->addr[0];
    pb->csParam.open.remotePort = id;
    pb->csParam.open.localHost = 0;
    pb->csParam.open.localPort = 0;
    pb->csParam.open.dontFrag = 0;
    pb->csParam.open.timeToLive = 0;
    pb->csParam.open.security = 0;
    pb->csParam.open.optionCnt = 0;

    if (errNo = PBControlAsync((ParamBlockRec*)pb)) {
      errpart = 3;
      goto tcp_close_and_error;
    }
    
    BEGIN_ESCAPEABLE(mac_tcp_close(data));
    scheme_block_until(tcp_check_connect, tcp_connect_needs_wakeup, pb, 0);
    END_ESCAPEABLE();
    
    if (data->tcp.state != SOCK_STATE_CONNECTED) {
      errpart = 4;
      errNo = pb->ioResult;
      goto tcp_close_and_error;
    }
    
    v[0] = make_named_tcp_input_port(data, address);
    v[1] = make_tcp_output_port(data);
    
    return scheme_values(2, v);
    
  tcp_close_and_error:
    
    mac_tcp_close(data);
    
  tcp_error:
    
    errid = errNo;
  }
#endif

#ifdef USE_SOCKETS_TCP
  host = gethostbyname(address);
  if (host) {
    tcp_connect_dest_addr.sin_family = AF_INET;
    tcp_connect_dest_addr.sin_port = id;
    memset(&tcp_connect_dest_addr.sin_addr, 0, sizeof(tcp_connect_dest_addr.sin_addr));
    memset(&tcp_connect_dest_addr.sin_zero, 0, sizeof(tcp_connect_dest_addr.sin_zero));
    memcpy(&tcp_connect_dest_addr.sin_addr, host->h_addr_list[0], host->h_length); 

#ifndef PROTOENT_IS_INT
    proto = getprotobyname("tcp");
    if (proto)
#endif
    {
      tcp_t s = socket(MZ_PF_INET, SOCK_STREAM, PROTO_P_PROTO);
      if (s != INVALID_SOCKET) {
	int status, inprogress;
#ifdef USE_WINSOCK_TCP
	unsigned long ioarg = 1;
	ioctlsocket(s, FIONBIO, &ioarg);
#else
	int size = TCP_SOCKSENDBUF_SIZE;
	fcntl(s, F_SETFL, MZ_NONBLOCKING);
# ifndef CANT_SET_SOCKET_BUFSIZE
	setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char *)&size, sizeof(int));
# endif
#endif
	status = connect(s, (struct sockaddr *)&tcp_connect_dest_addr, sizeof(tcp_connect_dest_addr));
#ifdef USE_UNIX_SOCKETS_TCP
	if (status)
	  status = errno;
	
	inprogress = (status == EINPROGRESS);
#endif
#ifdef USE_WINSOCK_TCP
	if (status)
	  status = WSAGetLastError();

	inprogress = (status == WSAEWOULDBLOCK);
	errno = status;
#endif

	scheme_file_open_count++;
	
	if (inprogress) {
          BEGIN_ESCAPEABLE(closesocket(s); --scheme_file_open_count);
	  status = scheme_block_until(tcp_check_connect, tcp_connect_needs_wakeup, (void *)s, (float)0.0);
	  END_ESCAPEABLE();
	  if (status == 1) {
#ifdef USE_UNIX_SOCKETS_TCP
	    status = recv(s, NULL, 0, 0); /* test input */
	    if (!status)
	      status = send(s, NULL, 0, 0); /* test output */
#else
	    status = send(s, "", 0, 0); /* test output */
	    if (status)
	      errno = WSAGetLastError();
#endif
	  }
	}
	
	if (!status) {
	  Scheme_Object *v[2];
	  Scheme_Tcp *tcp;

	  tcp = make_tcp_port_data(s, 2);
	  
	  v[0] = make_named_tcp_input_port(tcp, address);
	  v[1] = make_tcp_output_port(tcp);
	  
	  REGISTER_SOCKET(s);

	  return scheme_values(2, v);
	} else {
	  closesocket(s);
	  --scheme_file_open_count;
	  errpart = 4;
	}
      } else
	errpart = 3;
      errid = errno;
    }
#ifndef PROTOENT_IS_INT
    else
      (errid = 1, errpart = 2);
#endif
  } else {
    errpart = 1;
#ifdef USE_WINSOCK_TCP
    errid = WSAGetLastError();
#else
    errid = 0;
#endif
    errmsg = "; host not found";
  }
#endif

#ifdef USE_TCP
  scheme_raise_exn(MZEXN_I_O_TCP,
		   "tcp-connect: connection to %s, port %d failed%s (at step %d: %E)",
		   address, origid, errmsg, errpart, errid);
#else
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "tcp-connect: not supported on this platform");
#endif

  return NULL;
}

static Scheme_Object *
tcp_listen(int argc, Scheme_Object *argv[])
{
  unsigned short id, origid;
  int backlog, errid;
#ifdef USE_SOCKETS_TCP
# ifndef PROTOENT_IS_INT
  struct protoent *proto;
# endif
#endif

  if (!CHECK_PORT_ID(argv[0]))
    scheme_wrong_type("tcp-listen", PORT_ID_TYPE, 0, argc, argv);
  if (argc > 1)
    if (!SCHEME_INTP(argv[1]) || (SCHEME_INT_VAL(argv[1]) < 1))
      scheme_wrong_type("tcp-listen", "small positive integer", 1, argc, argv);
    
#ifdef USE_TCP
  TCP_INIT("tcp-listen");
#endif

  origid = (unsigned short)SCHEME_INT_VAL(argv[0]);
  if (argc > 1)
    backlog = SCHEME_INT_VAL(argv[1]);
  else
    backlog = 4;

#ifdef USE_TCP
  /* Set id in network order: */
  id = htons(origid);
#endif

#ifdef USE_MAC_TCP
  {
    int i;
    Scheme_Tcp **datas, *data;

    datas = MALLOC_N(Scheme_Tcp *, backlog);

    for (i = 0; i < backlog; i++) {
      if ((errid = mac_tcp_listen(id, &data))) {
        /* Close listeners that had succeeded: */
        int j;
        for (j = 0; j < i; j++)
          mac_tcp_close(datas[i]);
	break;
      }
      datas[i] = data;
    }

    if (!errid) {
      listener_t *l = MALLOC_ONE_TAGGED(listener_t);

      l->type = scheme_listener_type;
      l->portid = id;
      l->count = backlog;
      l->datas = datas;
      l->mref = scheme_add_managed(NULL,
				   (Scheme_Object *)l,
				   (Scheme_Close_Manager_Client *)stop_listener,
				   NULL,
				   1);
      
      return (Scheme_Object *)l;
    }
  }
#endif

#ifdef USE_SOCKETS_TCP
# ifndef PROTOENT_IS_INT
  proto = getprotobyname("tcp");
  if (proto)
# endif
  {
    tcp_address tcp_listen_addr; /* Use a long name for precise GC's xform.ss */
    tcp_t s;

    tcp_listen_addr.sin_family = AF_INET;
    tcp_listen_addr.sin_port = id;
    memset(&tcp_listen_addr.sin_addr, 0, sizeof(tcp_listen_addr.sin_addr));
    memset(&tcp_listen_addr.sin_zero, 0, sizeof(tcp_listen_addr.sin_zero));

    s = socket(MZ_PF_INET, SOCK_STREAM, PROTO_P_PROTO);
    if (s != INVALID_SOCKET) {
#ifdef USE_WINSOCK_TCP
      unsigned long ioarg = 1;
      ioctlsocket(s, FIONBIO, &ioarg);
#else
      fcntl(s, F_SETFL, MZ_NONBLOCKING);
#endif
      if (!bind(s, (struct sockaddr *)&tcp_listen_addr, sizeof(tcp_listen_addr)))
	if (!listen(s, backlog)) {
	  listener_t *l;

	  l = MALLOC_ONE_TAGGED(listener_t);
	  l->type = scheme_listener_type;
	  l->s = s;
	  {
	    Scheme_Manager_Reference *mref;
	    mref = scheme_add_managed(NULL,
				      (Scheme_Object *)l,
				      (Scheme_Close_Manager_Client *)stop_listener,
				      NULL,
				      1);
	    l->mref = mref;
	  }

	  scheme_file_open_count++;
	  REGISTER_SOCKET(s);

	  return (Scheme_Object *)l;
	}

      closesocket(s);
    }
  }
  errid = errno;
#endif

#ifdef USE_TCP
  scheme_raise_exn(MZEXN_I_O_TCP,
		   "tcp-listen: listen on %d failed (%e)",
		   origid, errid);
#else
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "tcp-listen: not supported on this platform");
#endif

  return NULL;
}

#ifdef USE_TCP
static int stop_listener(Scheme_Object *o)
{
  int was_closed = 0;

#ifdef USE_MAC_TCP
  { 
    listener_t *l = (listener_t *)o;
    int i, count = l->count;
    Scheme_Tcp **datas = l->datas;
    if (!datas || !l->count)
      was_closed = 1;
    else {
      l->count = 0;
      for (i = 0; i < count; i++)
	if (datas[i])
	  mac_tcp_close(datas[i]);
    }
 }
#endif

#ifdef USE_SOCKETS_TCP
  {
    tcp_t s = ((listener_t *)o)->s;
    if (s == INVALID_SOCKET)
      was_closed = 1;
    else {
      UNREGISTER_SOCKET(s);
      closesocket(s);
      ((listener_t *)o)->s = INVALID_SOCKET;
      --scheme_file_open_count;
    }
  }
#endif

  return was_closed;
}
#endif

static Scheme_Object *
tcp_stop(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  int was_closed;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_type("tcp-close", "tcp-listener", 0, argc, argv);

  TCP_INIT("tcp-close");

  was_closed = stop_listener(argv[0]);

  if (was_closed) {
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "tcp-close: listener was already closed");
    return NULL;
  }

  return scheme_void;
#else
  scheme_wrong_type("tcp-close", "tcp-listener", 0, argc, argv);
  return NULL;
#endif
}

static Scheme_Object *
tcp_accept_ready(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  int ready;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_type("tcp-accept-rady?", "tcp-listener", 0, argc, argv);

  TCP_INIT("tcp-accept-ready?");

  if (LISTENER_WAS_CLOSED(argv[0])) {
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "tcp-accept-ready?: listener is closed");
    return NULL;
  }

  ready = tcp_check_accept(argv[0]);

  return (ready ? scheme_true : scheme_false);
#else
  scheme_wrong_type("tcp-accept-rady?", "tcp-listener", 0, argc, argv);
  return NULL;
#endif
}

static Scheme_Object *
tcp_accept(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  int was_closed = 0, errid;
  Scheme_Object *listener;
# ifdef USE_SOCKETS_TCP
  tcp_t s;
  int l;
  tcp_address tcp_accept_addr; /* Use a long name for precise GC's xform.ss */
# endif
# ifdef USE_MAC_TCP
  listener_t *l;
  int i, count;
  Scheme_Tcp **datas;
# endif

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_type("tcp-accept", "tcp-listener", 0, argc, argv);

  TCP_INIT("tcp-accept?");

  listener = argv[0];

  was_closed = LISTENER_WAS_CLOSED(listener);

  if (!was_closed) {
    if (!tcp_check_accept(listener)) {
      scheme_current_process->block_descriptor = -1;
      scheme_current_process->blocker = listener;
      scheme_current_process->block_check = tcp_check_accept;
      scheme_current_process->block_needs_wakeup = tcp_accept_needs_wakeup;
      do {
	scheme_process_block((float)0.0);
      } while (!tcp_check_accept(listener));
      scheme_current_process->block_descriptor = NOT_BLOCKED;
      scheme_current_process->blocker = NULL;
      scheme_current_process->ran_some = 1;
    }
    was_closed = LISTENER_WAS_CLOSED(listener);
  }

  if (was_closed) {
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "tcp-accept: listener is closed");
    return NULL;
  }
  
# ifdef USE_SOCKETS_TCP
  s = ((listener_t *)listener)->s;

  l = sizeof(tcp_accept_addr);
  s = accept(s, (struct sockaddr *)&tcp_accept_addr, &l);
  if (s != INVALID_SOCKET) {
    Scheme_Object *v[2];
    Scheme_Tcp *tcp;
    
#  ifdef USE_UNIX_SOCKETS_TCP
    int size = TCP_SOCKSENDBUF_SIZE;
#   ifndef CANT_SET_SOCKET_BUFSIZE
    setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char *)&size, sizeof(int));
#   endif
#  endif

    tcp = make_tcp_port_data(s, 2);

    v[0] = make_named_tcp_input_port(tcp, "TCP");
    v[1] = make_tcp_output_port(tcp);

    scheme_file_open_count++;
    REGISTER_SOCKET(s);
    
    return scheme_values(2, v);
  }
  errid = errno;
# endif

# ifdef USE_MAC_TCP
  l = (listener_t *)listener;
  count = l->count;
  datas = l->datas;

  errid = 0;
  for (i = 0; i < count; i++) {
    if (datas[i] && (datas[i]->tcp.state != SOCK_STATE_LISTENING)) {
      Scheme_Object *v[2];
      Scheme_Tcp *data;
      
      v[0] = make_named_tcp_input_port(datas[i], "TCP");
      v[1] = make_tcp_output_port(datas[i]);
      
      if (!(errid = mac_tcp_listen(l->portid, &data))) {
        /* new listener at the end of the queue: */
	memcpy(datas + i, datas + i + 1, sizeof(Scheme_Tcp *) * (count - i - 1));
	datas[count - 1] = data;

	scheme_file_open_count++;
      } else {
      	/* catastophic error; we permanently decrement the listener count */
        datas[i] = NULL;
      }
      return scheme_values(2, v);
    }
  }
# endif

  scheme_raise_exn(MZEXN_I_O_TCP,
		   "tcp-accept: accept from listener failed (%e)", errid);
#else
  scheme_wrong_type("tcp-accept", "tcp-listener", 0, argc, argv);
#endif

  return NULL;
}

static Scheme_Object *tcp_listener_p(int argc, Scheme_Object *argv[])
{
   return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type)
	   ? scheme_true
	   : scheme_false);
}

static Scheme_Object *tcp_addresses(int argc, Scheme_Object *argv[])
{
  Scheme_Tcp *tcp = NULL;
  int closed = 0;
  unsigned long here_a, there_a;
  unsigned char *b;
  Scheme_Object *result[2];
  char sa[20];

  if (SCHEME_OUTPORTP(argv[0])) {
    Scheme_Output_Port *op;
    op = (Scheme_Output_Port *)argv[0];
    if (op->sub_type == scheme_tcp_output_port_type)
      tcp = op->port_data;
    closed = op->closed;
  } else if (SCHEME_INPORTP(argv[0])) {
    Scheme_Input_Port *ip;
    ip = (Scheme_Input_Port *)argv[0];
    if (ip->sub_type == scheme_tcp_input_port_type)
      tcp = ip->port_data;
    closed = ip->closed;
  }

  if (!tcp)
    scheme_wrong_type("tcp-addresses", "tcp-port", 0, argc, argv);

  if (closed)
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		     argv[0],
		     "tcp-addresses: port is closed");

# ifdef USE_SOCKETS_TCP
  {
    /* Use a long name for precise GC's xform.ss: */
    tcp_address tcp_here_addr, tcp_there_addr;
    int l;
    
    l = sizeof(tcp_here_addr);
    if (getsockname(tcp->tcp, (struct sockaddr *)&tcp_here_addr, &l)) {
      scheme_raise_exn(MZEXN_I_O_TCP,
		       "tcp-addresses: could not get local address (%e)",
		       errno);
    }
    l = sizeof(tcp_there_addr);
    if (getpeername(tcp->tcp, (struct sockaddr *)&tcp_there_addr, &l)) {
      scheme_raise_exn(MZEXN_I_O_TCP,
		       "tcp-addresses: could not get peer address (%e)",
		       errno);
    }

    here_a = *(unsigned long *)&tcp_here_addr.sin_addr;
    there_a = *(unsigned long *)&tcp_there_addr.sin_addr;
  }
# endif
# ifdef USE_MAC_TCP
  {
    here_a = ((TCPOpenPB *)tcp->tcp.create_pb)->localHost;
    here_a = ((TCPOpenPB *)tcp->tcp.create_pb)->remoteHost;
  }
# endif

  b = (unsigned char *)&here_a;
  sprintf(sa, "%d.%d.%d.%d", b[0], b[1], b[2], b[3]);
  result[0] = scheme_make_string(sa);

  b = (unsigned char *)&there_a;
  sprintf(sa, "%d.%d.%d.%d", b[0], b[1], b[2], b[3]);
  result[1] = scheme_make_string(sa);

  return scheme_values(2, result);
}

/*========================================================================*/
/*                       precise GC traversers                            */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_NETWORK_C
#include "mzmark.c"

static void register_traversers(void)
{
#ifdef USE_TCP
  GC_REG_TRAV(scheme_rt_tcp, mark_tcp);
# ifdef USE_MAC_TCP
  GC_REG_TRAV(scheme_rt_write_data, mark_write_data);
# endif
#endif
  GC_REG_TRAV(scheme_listener_type, mark_listener);  
}

END_XFORM_SKIP;

#endif

#endif /* !NO_TCP_SUPPORT */

