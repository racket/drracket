// browser.cxx

#include "stdafx.h"

#include <objbase.h>
#include <mshtml.h>
#include <initguid.h>
#include <winnls.h>
#include <exdisp.h>
#include <process.h>

#include "escheme.h"

#include "myssink.h"

#include "myspage.h"

#include "mysterx.h"

HWND browserHwnd;

BROWSER_WINDOW_STYLE_OPTION styleOptions[6] = {
  
  // keep alphabetic for bsearch()
  
  // { symbol,Win32 constant,TRUE=add/FALSE=remove } 
  
  { "iconize",WS_ICONIC,TRUE },
  { "maximize",WS_MAXIMIZE,TRUE },
  { "no-caption",WS_CAPTION,FALSE },
  { "no-system-menu",WS_CAPTION | WS_SYSMENU,FALSE },
  { "no-thick-border",WS_THICKFRAME,FALSE },
  { "scrollbars",WS_HSCROLL | WS_VSCROLL,TRUE },
};

int cmpBwso(char *key,BROWSER_WINDOW_STYLE_OPTION *bwso) {
  return strcmp(key,bwso->name);
}

void assignIntOrDefault(int *pVal,Scheme_Object **argv,int argc,int ndx) {
  if (SCHEME_SYMBOLP(argv[ndx])) {
    *pVal = CW_USEDEFAULT;
    if (strcmpi(SCHEME_SYM_VAL(argv[ndx]),"default") == 0) {
      *pVal = CW_USEDEFAULT;
    }
    else {
      scheme_wrong_type("make-document","int",ndx+1,argc,argv);
    }
  }
  else if (SCHEME_INTP(argv[ndx]) == FALSE) {
    scheme_wrong_type("make-browser","int",ndx+1,argc,argv);
  }
  else {
    *pVal = SCHEME_INT_VAL(argv[ndx]);
  }
}

Scheme_Object *mx_make_browser(int argc,Scheme_Object **argv) {
  HRESULT hr;
  MX_Browser_Object *browser;
  IUnknown *pIUnknown;
  IConnectionPointContainer *pIConnectionPointContainer;
  IConnectionPoint *pIConnectionPoint;
  ISink *pISink;
  IDHTMLPage *pIDHTMLPage;
  IStream *pIStream,*pBrowserStream;
  IWebBrowser2 *pIWebBrowser2;
  IEventQueue *pIEventQueue;
  Scheme_Object *pSyms,*currSym;
  char *currStyleOption;
  BROWSER_WINDOW_INIT browserWindowInit;
  BROWSER_WINDOW_STYLE_OPTION *pBwso;
  DWORD cookie;

  if (SCHEME_STRINGP(argv[0]) == FALSE) {
    scheme_wrong_type("make-browser","string",1,argc,argv);
  }

  browserWindowInit.browserWindow.label = SCHEME_STR_VAL(argv[0]);
  
  assignIntOrDefault(&browserWindowInit.browserWindow.width,argv,argc,1);
  assignIntOrDefault(&browserWindowInit.browserWindow.height,argv,argc,2);
  assignIntOrDefault(&browserWindowInit.browserWindow.x,argv,argc,3);
  assignIntOrDefault(&browserWindowInit.browserWindow.y,argv,argc,4);
  
  if (SCHEME_PAIRP(argv[5]) == FALSE && argv[5] != scheme_null) {
    scheme_wrong_type("make-browser","list of symbols",5,argc,argv);
  }

  pSyms = argv[5];
  browserWindowInit.browserWindow.style = WS_OVERLAPPEDWINDOW;

  while (pSyms != scheme_null) {
    
    currSym = SCHEME_CAR(pSyms);
    
    if (SCHEME_SYMBOLP(currSym) == FALSE) {
      scheme_wrong_type("make-browser","list of symbols",5,argc,argv);
    }
    
    currStyleOption = SCHEME_SYM_VAL(currSym);
    
    pBwso = (BROWSER_WINDOW_STYLE_OPTION *)
              bsearch(currStyleOption,
		      styleOptions,
		      sizeray(styleOptions),
		      sizeof(styleOptions[0]),
		      (int (*)(const void *,const void *))cmpBwso);

    if (pBwso == NULL) {
      scheme_signal_error("Invalid browser window style option: %s",
			  currStyleOption);
    }
    
    if (pBwso->enable) {
      browserWindowInit.browserWindow.style |= pBwso->bits;
    }
    else {
      browserWindowInit.browserWindow.style &= ~(pBwso->bits);
    }

    pSyms = SCHEME_CDR(pSyms);
  }

  // mutex to protect association between new window and pIUnknown pointer to DHTML control
  
  WaitForSingleObject(browserHwndMutex,INFINITE);
  
  browserWindowInit.ppIStream = &pBrowserStream;
  
  // use _beginthread instead of CreateThread
  // because the use of HTMLHelp requires the use of
  // multithreaded C library
  
  _beginthread(browserHwndMsgLoop,0,(void *)&browserWindowInit);
  
  // wait until the window is created
  
  WaitForSingleObject(createHwndSem,INFINITE);
  
  hr = CoGetInterfaceAndReleaseStream(pBrowserStream,IID_IUnknown,(void **)&pIUnknown);
  
  browser = (MX_Browser_Object *)scheme_malloc(sizeof(MX_Browser_Object));
  browser->type = mx_browser_type;
  browser->hwnd = browserHwnd;
  
  ReleaseSemaphore(browserHwndMutex,1,NULL);
  
  if (hr != S_OK || pIUnknown == NULL) {
    DestroyWindow(browserHwnd);
    codedComError("make-browser: Can't get browser IUnknown interface",hr);
  }
  
  pIUnknown->QueryInterface(IID_IDHTMLPage,(void **)&pIDHTMLPage);
  
  pIUnknown->Release();

  if (pIDHTMLPage == NULL) {
    scheme_signal_error("make-browser: Can't get IDHTMLPage interface");
  }
  
  // workaround for inability to use exdisp.idl or mshtml.idl
  
  pIStream = NULL;
  pIDHTMLPage->marshalWebBrowserToStream(&pIStream);
  
  if (pIStream == NULL) {
    scheme_signal_error("make-browser: Can't get pIStream interface for browser");
  }

  hr = CoGetInterfaceAndReleaseStream(pIStream,IID_IWebBrowser2,(void **)&pIWebBrowser2);
  
  if (hr != S_OK || pIWebBrowser2 == NULL) {
    codedComError("make-browser: Can't get IWebBrowser2 interface",hr);
  }

  pIStream = NULL;
  pIDHTMLPage->marshalEventQueueToStream(&pIStream);
  
  pIDHTMLPage->Release();

  if (pIStream == NULL) {
    scheme_signal_error("make-browser: Can't get IStream interface for event queue");
  }
  
  hr = CoGetInterfaceAndReleaseStream(pIStream,IID_IEventQueue,(void **)&pIEventQueue);
  
  if (hr != S_OK || pIEventQueue == NULL) {
    codedComError("make-browser: Can't get event queue interface",hr);
  }

  pIEventQueue->GetReaderSemaphore((int *)&browser->readSem);
  
  if (browser->readSem == 0) {
    scheme_signal_error("make-browser: Error retrieving browser event read semaphore");
  }

  // setup event sink for browser

  hr = pIWebBrowser2->QueryInterface(IID_IConnectionPointContainer,(void **)&pIConnectionPointContainer); 

  if (hr != S_OK || pIConnectionPointContainer == NULL) {
    signalCodedEventSinkError("make-browser: Unable to get browser connection point container",hr);
  }

  hr = pIConnectionPointContainer->FindConnectionPoint(DIID_DWebBrowserEvents2,
						       &pIConnectionPoint);

  if (hr != S_OK || pIConnectionPoint == NULL) {
    signalCodedEventSinkError("make-browser: Unable to get browser connection point",hr);
  }

  pIConnectionPointContainer->Release();

  hr = CoCreateInstance(CLSID_Sink,NULL,
			CLSCTX_LOCAL_SERVER | CLSCTX_INPROC_SERVER,
			IID_IUnknown,(void **)&pIUnknown);
  
  if (hr != S_OK || pIUnknown == NULL) {
    signalCodedEventSinkError("make-browser: Unable to create sink object",hr);
  }

  hr = pIUnknown->QueryInterface(IID_ISink,(void **)&pISink);
  
  if (hr != S_OK || pISink == NULL) {
    signalCodedEventSinkError("make-browser: Unable to find sink interface",hr);
  }
  
  pISink->set_extension_table((int)scheme_extension_table); // COM won't take a function ptr
  
  pISink->set_myssink_table((int)&myssink_table);
  
  hr = pIConnectionPoint->Advise(pIUnknown,&cookie);

  pIUnknown->Release();
  
  if (hr != S_OK) {
    signalCodedEventSinkError("make-browser: Unable to connect sink to connection point",hr);
  }
  
  pIEventQueue->set_extension_table((int)scheme_extension_table); 

  browser->pIWebBrowser2 = pIWebBrowser2;
  browser->pISink = pISink;
  browser->pIEventQueue = pIEventQueue;

  scheme_add_managed((Scheme_Manager *)scheme_get_param(scheme_config,MZCONFIG_MANAGER),
		     (Scheme_Object *)browser,
		     (Scheme_Close_Manager_Client *)scheme_release_browser,
		     NULL,0);

  scheme_register_finalizer(browser,scheme_release_browser,NULL,NULL,NULL);

  return (Scheme_Object *)browser;
}

Scheme_Object *mx_browser_show(int argc,Scheme_Object **argv) {
  MX_Browser_Object *pBrowser;
  
  if (MX_BROWSERP(argv[0]) == FALSE) {
    scheme_wrong_type("browser-show","mx-browser",0,argc,argv);
  }
  
  pBrowser = (MX_Browser_Object *)argv[0];

  ShowWindow(pBrowser->hwnd,argv[1] == scheme_false ? SW_HIDE : SW_SHOW);
  
  return scheme_void;
}

Scheme_Object *mx_navigate(int argc,Scheme_Object **argv) {
  IWebBrowser2 *pIWebBrowser2;
  BSTR url;
  VARIANT vars[4];

  if (MX_BROWSERP(argv[0]) == FALSE) {
    scheme_wrong_type("navigate","mx-browser",0,argc,argv);
  }

  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("navigate","string",1,argc,argv);
  }

  pIWebBrowser2 = MX_BROWSER_VAL(argv[0]);

  url = stringToBSTR(SCHEME_STR_VAL(argv[1]),SCHEME_STRLEN_VAL(argv[1]));

  memset(vars,0,sizeof(vars));

  pIWebBrowser2->Navigate(url,vars,vars+1,vars+2,vars+3);

  SysFreeString(url);

  return scheme_void;
}

Scheme_Object *mx_go_back(int argc,Scheme_Object **argv) {
  IWebBrowser2 *pIWebBrowser2;

  if (MX_BROWSERP(argv[0]) == FALSE) {
    scheme_wrong_type("go-back","mx-browser",0,argc,argv);
  }

  pIWebBrowser2 = MX_BROWSER_VAL(argv[0]);

  pIWebBrowser2->GoBack();

  return scheme_void;
}

Scheme_Object *mx_go_forward(int argc,Scheme_Object **argv) {
  IWebBrowser2 *pIWebBrowser2;

  if (MX_BROWSERP(argv[0]) == FALSE) {
    scheme_wrong_type("go-forward","mx-browser",0,argc,argv);
  }

  pIWebBrowser2 = MX_BROWSER_VAL(argv[0]);

  pIWebBrowser2->GoForward();

  return scheme_void;
}

Scheme_Object *mx_register_navigate_handler(int argc,Scheme_Object **argv) {
  ISink *pISink;

  if (MX_BROWSERP(argv[0]) == FALSE) {
    scheme_wrong_type("register-navigate-handler","mx-browser",0,argc,argv);
  }

  pISink = MX_BROWSER_SINK(argv[0]);

  // register handler for NavigateComplete2 event (memID = 259)

  pISink->register_handler(259,(int)argv[1]);

  return scheme_void;
}

Scheme_Object *mx_unregister_navigate_handler(int argc,Scheme_Object **argv) {
  ISink *pISink;

  if (MX_BROWSERP(argv[0]) == FALSE) {
    scheme_wrong_type("unregister-navigate-handler!","mx-browser",0,argc,argv);
  }

  pISink = MX_BROWSER_SINK(argv[0]);

  // unregister handler for NavigateComplete2 event (memID = 259)

  pISink->unregister_handler(259);

  return scheme_void;
}

Scheme_Object *mx_current_document(int argc,Scheme_Object **argv) {
  HRESULT hr;
  IDispatch *pIDispatch;
  IWebBrowser2 *pIWebBrowser2;
  IHTMLDocument2 *pIHTMLDocument2;
  MX_Document_Object *doc;

  if (MX_BROWSERP(argv[0]) == FALSE) {
    scheme_wrong_type("unregister-navigate-handler!","mx-browser",0,argc,argv);
  }

  pIWebBrowser2 = MX_BROWSER_VAL(argv[0]);

  pIWebBrowser2->get_Document(&pIDispatch);

  if (pIDispatch == NULL) {
    scheme_signal_error("Error retrieving DHTML dispatch interface");
  }
  
  hr = pIDispatch->QueryInterface(IID_IHTMLDocument2,(void **)&pIHTMLDocument2);
  
  if (hr != S_OK || pIHTMLDocument2 == NULL) {
    codedComError("Error retrieving DHTML document2 interface",hr);
  }

  pIDispatch->Release();

  doc = (MX_Document_Object *)scheme_malloc(sizeof(MX_Document_Object));
  doc->type = mx_document_type;
  doc->pIHTMLDocument2 = pIHTMLDocument2;

  scheme_add_managed((Scheme_Manager *)scheme_get_param(scheme_config,MZCONFIG_MANAGER),
		     (Scheme_Object *)doc,
		     (Scheme_Close_Manager_Client *)scheme_release_document,
		     NULL,0);

  scheme_register_finalizer(doc,scheme_release_document,NULL,NULL,NULL);

  return (Scheme_Object *)doc;
}

Scheme_Object *mx_current_url(int argc,Scheme_Object **argv) {
  HRESULT hr;
  IWebBrowser2 *pIWebBrowser2;
  BSTR url;
  Scheme_Object *retval;

  if (MX_BROWSERP(argv[0]) == FALSE) {
    scheme_wrong_type("current-url","mx-browser",0,argc,argv);
  }

  pIWebBrowser2 = MX_BROWSER_VAL(argv[0]);

  hr = pIWebBrowser2->get_LocationURL(&url);

  if (hr != S_OK) {
    codedComError("current-url: Error retrieving URL",hr);
  }

  if (url == NULL) {
    scheme_signal_error("current-url: NULL URL");
  }

  retval = BSTRToSchemeString(url);

  // SysFreeString(url);

  return retval;
}

