// eventqueue.cxx : Implementation of CEventQueue

#include "stdafx.h"
#include <stdio.h>
#include <limits.h>

#include <stdarg.h>

#include "escheme.h"
#include "myspage.h"
#include "dhtmlpage.h"

#include "eventqueue.h"


/////////////////////////////////////////////////////////////////////////////
// CEventQueue

CEventQueue::CEventQueue(void) { 

    queueLength = 0;
    readerNdx = writerNdx = 0;
    
    readSem = CreateSemaphore(NULL,0,LONG_MAX,NULL);  // using MAXQUEUELENGTH doesn't work
    mutex = CreateSemaphore(NULL,1,1,NULL);

    if (readSem == NULL || mutex == NULL) {
      ::failureBox("Error creating event semaphore(s)");
    }
}

CEventQueue::~CEventQueue(void) { 
    if (readSem) {
      CloseHandle(readSem);
    }

    if (mutex) {
      CloseHandle(mutex);
    }
}

STDMETHODIMP CEventQueue::QueueEvent(IEvent *pEvent) {
  BOOL signalReader;

  WaitForSingleObject(mutex,INFINITE); 

  if (queueLength < MAXQUEUELENGTH) {
    queueLength++;
    signalReader = TRUE;
  } 
  else {
    readerNdx = ++readerNdx % MAXQUEUELENGTH;
    signalReader = FALSE;
  }

  theQueue[writerNdx] = pEvent;

  writerNdx = ++writerNdx % MAXQUEUELENGTH;

  ReleaseSemaphore(mutex,1,NULL);

  if (signalReader) {
    ReleaseSemaphore(readSem,1,NULL);
  }

  return S_OK;

}

STDMETHODIMP CEventQueue::GetEvent(IEvent **ppEvent) {
  *ppEvent = NULL;

  WaitForSingleObject(readSem,INFINITE); 

  WaitForSingleObject(mutex,INFINITE);

  *ppEvent = theQueue[readerNdx];

  readerNdx = ++readerNdx % MAXQUEUELENGTH;
  queueLength--;

  ReleaseSemaphore(mutex,1,NULL);

  return S_OK;
}
 
STDMETHODIMP CEventQueue::get_EventAvailable(VARIANT_BOOL *pVal) {
  WaitForSingleObject(mutex,INFINITE);
  
  *pVal = (queueLength == 0) ? 0 : -1; 

  ReleaseSemaphore(mutex,1,NULL);

  return S_OK;
}

STDMETHODIMP CEventQueue::GetReaderSemaphore(int *pReadSem) {
  *pReadSem = (int)readSem;

  return S_OK;
}

STDMETHODIMP CEventQueue::set_extension_table(int p) {
  scheme_extension_table = (Scheme_Extension_Table *)p;
  scheme_register_extension_global(&_Module,sizeof(_Module));
  scheme_register_extension_global(&eventMap,sizeof(eventMap));
  scheme_register_extension_global((void *)&IID_IDHTMLPage,sizeof(IID_IDHTMLPage));
  scheme_register_extension_global((void *)&IID_IDHTMLPageUI,sizeof(IID_IDHTMLPageUI));
  scheme_register_extension_global((void *)&IID_IEvent,sizeof(IID_IEvent));
  scheme_register_extension_global((void *)&IID_IEventQueue,sizeof(IID_IEventQueue));
  scheme_register_extension_global((void *)&LIBID_MYSPAGELib,sizeof(LIBID_MYSPAGELib));
  scheme_register_extension_global((void *)&CLSID_DHTMLPage,sizeof(CLSID_DHTMLPage));
  scheme_register_extension_global((void *)&CLSID_Event,sizeof(CLSID_Event));
  scheme_register_extension_global((void *)&CLSID_EventQueue,sizeof(CLSID_EventQueue));

  return S_OK;
}


