// dhtmlpage.cxx : Implementation of CDHTMLPage

#include "stdafx.h"
#include "myspage.h"
#include "wrapper.h"
#include "dhtmlpage.h"

EVENT_MAP eventMap[11] = {
  L"click",click,
  L"dblclick",dblclick,
  L"error",error,
  L"keydown",keydown,
  L"keypress",keypress,
  L"keyup",keyup,
  L"mousedown",mousedown,
  L"mousemove",mousemove,
  L"mouseout",mouseout,
  L"mouseover",mouseover,
  L"mouseup",mouseup,
};


/////////////////////////////////////////////////////////////////////////////
// CDHTMLPage


void ::failureBox(const char *s) {
  ::MessageBox(NULL, s, "MysterX error", MB_OK);
}

int eventSearchCmp(const void *s1,const void *s2) {
  return wcscmp((const WCHAR *)s1,((const EVENT_MAP *)s2)->name);
}

HRESULT CDHTMLPage::AtAnyEvent(void) {
  HRESULT hr;
  static IHTMLWindow2 *pTopWindow;
  IHTMLEventObj *pIHTMLEventObj;
  IEvent *pEvent;
  EVENT_MAP *eventEntry;
  BSTR eventString;
  BSTR eltName;
  BSTR idAttr;
  IHTMLElement *pSrcElement,*pFromElement,*pToElement;
  VARIANT variant;
  VARIANT_BOOL altPressed,ctrlPressed,shiftPressed;
  long mouseButton;
  long x,y;

  if (pTopWindow == NULL) {
      IDispatch *pDocDispatch;
      IHTMLWindow2 *pIHTMLWindow2;
      IHTMLDocument2 *pIHTMLDocument2;

      ((IWebBrowser2 *)(m_spBrowser))->get_Document(&pDocDispatch);

      if (pDocDispatch == NULL) {
         ::failureBox("Can't get document on event trap");
         return S_OK;
      }
      
      pDocDispatch->QueryInterface(IID_IHTMLDocument2,(void **)&pIHTMLDocument2);

      pIHTMLDocument2->get_parentWindow(&pIHTMLWindow2);

      if (pIHTMLWindow2 == NULL) {
         ::failureBox("Can't get window on event trap");
         return S_OK;
      }

      pIHTMLWindow2->get_top(&pTopWindow);

      if (pTopWindow == NULL) {
         ::failureBox("Can't get top window on event trap");
         return S_OK;
      }
    }

    pTopWindow->get_event(&pIHTMLEventObj);

    if (pIHTMLEventObj == NULL) { // occurs on Refresh
      pTopWindow = NULL;
      return S_OK;
    }

    pEvent = NULL;

    hr = CoCreateInstance(CLSID_Event,NULL,CLSCTX_ALL,IID_IEvent,(void **)&pEvent);

    if (SUCCEEDED(hr) == FALSE || pEvent == NULL) {
      ::failureBox("Can't create event in MysPage");
       return -1;
    }

    pIHTMLEventObj->get_type(&eventString);

    eventEntry = (EVENT_MAP *)bsearch(eventString,eventMap,
                                      sizeray(eventMap),sizeof(*eventMap),
                                      eventSearchCmp);
    
    if (eventEntry == NULL) {
      return S_OK;
    }
    
    pEvent->put_eventType(eventEntry->eventType);

    pIHTMLEventObj->get_x(&x);
    pEvent->put_x(x);

    pIHTMLEventObj->get_y(&y);
    pEvent->put_y(y);

    idAttr = SysAllocString(L"id");

    pIHTMLEventObj->get_srcElement(&pSrcElement);

    if (pSrcElement) {

      hr = pSrcElement->get_tagName(&eltName);

      pEvent->put_srcTag(eltName);

      hr = pSrcElement->getAttribute(idAttr,FALSE,&variant);

      pEvent->put_srcId(variant.bstrVal);

      pSrcElement->Release();

    }

    pIHTMLEventObj->get_altKey(&altPressed);
    pEvent->put_altPressed(altPressed);

    pIHTMLEventObj->get_ctrlKey(&ctrlPressed);
    pEvent->put_ctrlPressed(ctrlPressed);

    pIHTMLEventObj->get_shiftKey(&shiftPressed);
    pEvent->put_shiftPressed(shiftPressed);

    pIHTMLEventObj->get_button(&mouseButton);
    pEvent->put_mouseButton((MOUSE_BUTTON)mouseButton);

    if (eventEntry->eventType == mouseover || eventEntry->eventType == mouseout) {
    
      pIHTMLEventObj->get_fromElement(&pFromElement);

      if (pFromElement) {
  
        pFromElement->get_tagName(&eltName);

        pEvent->put_fromTag(eltName);

        pFromElement->getAttribute(idAttr,FALSE,&variant);

        pEvent->put_fromId(variant.bstrVal);

        pFromElement->Release();
      }

      pIHTMLEventObj->get_toElement(&pToElement);

      if (pToElement) {
  
        pToElement->get_tagName(&eltName);

        pEvent->put_toTag(eltName);

        pToElement->getAttribute(idAttr,FALSE,&variant);

        pEvent->put_toId(variant.bstrVal);

        pToElement->Release();
      }

    }

    SysFreeString(idAttr);

    pIEventQueue->QueueEvent(pEvent); 

    pIHTMLEventObj->Release();

    return S_OK;
}

LRESULT CDHTMLPage::OnCreate(UINT,WPARAM,LPARAM,BOOL&) {
  CAxWindow wnd(m_hWnd);
  CComObject<CWrapperDispatch> *pdispWrapper;
  HRESULT hr;
  BOOL hasScrollBars;

  // low bit set in parent's window style means use scrollbars
  hasScrollBars = 
    ::GetWindowLong(::GetParent(m_hWnd),GWL_STYLE) & 1L;

  if (hasScrollBars) {
    wnd.ModifyStyle(0,WS_HSCROLL|WS_VSCROLL,0);
  }

  hr = wnd.CreateControl(IDH_DHTMLPAGE);

  if (SUCCEEDED(hr) == FALSE) {
    ::failureBox("Can't create DHTML control");
    return -1;
  }

  // Create a wrapper about the external dispatch interface
  // workaround for IE5

  hr = pdispWrapper->CreateInstance(&pdispWrapper);
  if (SUCCEEDED(hr) == FALSE) {
    ::failureBox("Can't create Wrapper dispatch for DHTML control");
    return -1;
  }

  CComPtr<IDHTMLPageUI> pdispExternal = com_cast<IDHTMLPageUI>(GetUnknown());
  if (pdispExternal == NULL) {
   return E_UNEXPECTED;
  }

  pdispWrapper->SetDispatch(pdispExternal);

  hr = wnd.SetExternalDispatch(pdispWrapper);
  if (SUCCEEDED(hr) == FALSE) {
    ::failureBox("Can't set dispatcher for DHTML control");
    return -1;
  }

  hr = wnd.QueryControl(IID_IWebBrowser2, (void**)&m_spBrowser);
  if (SUCCEEDED(hr) == FALSE) {
    ::failureBox("Can't find browser in DHTML control");
    return -1;
  }

  hr = CoCreateInstance(CLSID_EventQueue,NULL,CLSCTX_ALL,
			IID_IEventQueue,(void **)&pIEventQueue);

  if (SUCCEEDED(hr) == FALSE || pIEventQueue == NULL) {
    ::failureBox("Can't create event queue");
    return -1;
  }
  
  return 0;
}

STDMETHODIMP CDHTMLPage::marshalWebBrowserToStream(IStream **ppIStream) {
  HRESULT hr;

  hr = CoMarshalInterThreadInterfaceInStream(IID_IWebBrowser2,m_spBrowser,ppIStream);

  if (hr != S_OK) {
    failureBox("Can't marshall Web browser");
    return hr;
  }
  
	return S_OK;
}

STDMETHODIMP CDHTMLPage::marshalEventQueueToStream(IStream **ppIStream)
{
  HRESULT hr;

  hr = CoMarshalInterThreadInterfaceInStream(IID_IEventQueue,pIEventQueue,ppIStream);

  if (hr != S_OK) {
    failureBox("Can't marshall event queue");
    return hr;
  }
  
  return S_OK;
}
