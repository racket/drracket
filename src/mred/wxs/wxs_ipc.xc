
#if 0

#include "wx_ipc.h"

@INCLUDE wxs.xci

@HEADER

@GLOBAL wxsIPCGlobal

@ "wx:ipc-initialize" : void wxIPCInitialize();

@END

@CLASSBASE wxConnection "wx:connection" : "wx:object"

@CREATOR ();

@ "advise" : bool Advise(string,string,int=-1,int=wxCF_TEXT);
@ "poke" : bool Poke(string,string,int=-1,int=wxCF_TEXT);
@ "execute" : bool Execute(string,int=-1,int=wxCF_TEXT);
@ "request" : string Request(string,int*,int=wxCF_TEXT);
@ "start-advise" : bool StartAdvise(string);
@ "stop-advise" : bool StopAdvise(string);

@ v "on-advise" : bool OnAdvise(string,string,string,int,int);
@ v "on-start-advise" : bool OnStartAdvise(string,string);
@ v "on-stop-advise" : bool OnStopAdvise(string,string);
@ v "on-poke" : bool OnPoke(string,string,string,int,int);
@ v "on-execute" : bool OnExecute(string,string,int,int);
@ v "on-request" : string OnRequest(string,string,int*,int);
@ v "on-disconnect" : bool OnDisconnect();

@ "disconnect" : void Disconnect();

@END

@CLASSBASE wxClient "wx:client" : "wx:object"

@CREATOR ();

@ "make-connection" : wxConnection^ MakeConnection(string,string,string);
@ v "on-make-connection" : wxConnection^ OnMakeConnection()

@END

@CLASSBASE wxServer "wx:server" : "wx:object"

@CREATOR ();

@ "create" : bool Create(string);
@ v "on-accept-connection" : wxConnection^ OnAcceptConnection(string)

@END

#endif
