/* this ALWAYS GENERATED file contains the definitions for the interfaces */


/* File created by MIDL compiler version 5.01.0164 */
/* at Wed Sep 22 12:48:00 1999
 */
/* Compiler settings for myspage.idl:
    Oicf (OptLev=i2), W1, Zp8, env=Win32, ms_ext, c_ext
    error checks: allocation ref bounds_check enum stub_data 
*/
//@@MIDL_FILE_HEADING(  )


/* verify that the <rpcndr.h> version is high enough to compile this file*/
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 440
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error this stub requires an updated version of <rpcndr.h>
#endif // __RPCNDR_H_VERSION__

#ifndef COM_NO_WINDOWS_H
#include "windows.h"
#include "ole2.h"
#endif /*COM_NO_WINDOWS_H*/

#ifndef __myspage_h__
#define __myspage_h__

#ifdef __cplusplus
extern "C"{
#endif 

/* Forward Declarations */ 

#ifndef __IDHTMLPage_FWD_DEFINED__
#define __IDHTMLPage_FWD_DEFINED__
typedef interface IDHTMLPage IDHTMLPage;
#endif 	/* __IDHTMLPage_FWD_DEFINED__ */


#ifndef __IDHTMLPageUI_FWD_DEFINED__
#define __IDHTMLPageUI_FWD_DEFINED__
typedef interface IDHTMLPageUI IDHTMLPageUI;
#endif 	/* __IDHTMLPageUI_FWD_DEFINED__ */


#ifndef __IEvent_FWD_DEFINED__
#define __IEvent_FWD_DEFINED__
typedef interface IEvent IEvent;
#endif 	/* __IEvent_FWD_DEFINED__ */


#ifndef __IEventQueue_FWD_DEFINED__
#define __IEventQueue_FWD_DEFINED__
typedef interface IEventQueue IEventQueue;
#endif 	/* __IEventQueue_FWD_DEFINED__ */


#ifndef __DHTMLPage_FWD_DEFINED__
#define __DHTMLPage_FWD_DEFINED__

#ifdef __cplusplus
typedef class DHTMLPage DHTMLPage;
#else
typedef struct DHTMLPage DHTMLPage;
#endif /* __cplusplus */

#endif 	/* __DHTMLPage_FWD_DEFINED__ */


#ifndef __Event_FWD_DEFINED__
#define __Event_FWD_DEFINED__

#ifdef __cplusplus
typedef class Event Event;
#else
typedef struct Event Event;
#endif /* __cplusplus */

#endif 	/* __Event_FWD_DEFINED__ */


#ifndef __EventQueue_FWD_DEFINED__
#define __EventQueue_FWD_DEFINED__

#ifdef __cplusplus
typedef class EventQueue EventQueue;
#else
typedef struct EventQueue EventQueue;
#endif /* __cplusplus */

#endif 	/* __EventQueue_FWD_DEFINED__ */


/* header files for imported files */
#include "oaidl.h"
#include "ocidl.h"

void __RPC_FAR * __RPC_USER MIDL_user_allocate(size_t);
void __RPC_USER MIDL_user_free( void __RPC_FAR * ); 

/* interface __MIDL_itf_myspage_0000 */
/* [local] */ 

typedef 
enum _event_type_
    {	click	= 0,
	dblclick	= click + 1,
	error	= dblclick + 1,
	keydown	= error + 1,
	keypress	= keydown + 1,
	keyup	= keypress + 1,
	mousedown	= keyup + 1,
	mousemove	= mousedown + 1,
	mouseout	= mousemove + 1,
	mouseover	= mouseout + 1,
	mouseup	= mouseover + 1
    }	EVENT_TYPE;

typedef 
enum _mouse_button_
    {	none	= 0,
	left	= 1,
	middle	= 2,
	right	= 4
    }	MOUSE_BUTTON;



extern RPC_IF_HANDLE __MIDL_itf_myspage_0000_v0_0_c_ifspec;
extern RPC_IF_HANDLE __MIDL_itf_myspage_0000_v0_0_s_ifspec;

#ifndef __IDHTMLPage_INTERFACE_DEFINED__
#define __IDHTMLPage_INTERFACE_DEFINED__

/* interface IDHTMLPage */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IDHTMLPage;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("0E7D148C-8948-11D2-B54E-0060089002FE")
    IDHTMLPage : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE marshalWebBrowserToStream( 
            IStream __RPC_FAR *__RPC_FAR *__MIDL_0015) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE marshalEventQueueToStream( 
            IStream __RPC_FAR *__RPC_FAR *__MIDL_0016) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IDHTMLPageVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *QueryInterface )( 
            IDHTMLPage __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void __RPC_FAR *__RPC_FAR *ppvObject);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *AddRef )( 
            IDHTMLPage __RPC_FAR * This);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *Release )( 
            IDHTMLPage __RPC_FAR * This);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfoCount )( 
            IDHTMLPage __RPC_FAR * This,
            /* [out] */ UINT __RPC_FAR *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfo )( 
            IDHTMLPage __RPC_FAR * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo __RPC_FAR *__RPC_FAR *ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetIDsOfNames )( 
            IDHTMLPage __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR __RPC_FAR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID __RPC_FAR *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *Invoke )( 
            IDHTMLPage __RPC_FAR * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS __RPC_FAR *pDispParams,
            /* [out] */ VARIANT __RPC_FAR *pVarResult,
            /* [out] */ EXCEPINFO __RPC_FAR *pExcepInfo,
            /* [out] */ UINT __RPC_FAR *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *marshalWebBrowserToStream )( 
            IDHTMLPage __RPC_FAR * This,
            IStream __RPC_FAR *__RPC_FAR *__MIDL_0015);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *marshalEventQueueToStream )( 
            IDHTMLPage __RPC_FAR * This,
            IStream __RPC_FAR *__RPC_FAR *__MIDL_0016);
        
        END_INTERFACE
    } IDHTMLPageVtbl;

    interface IDHTMLPage
    {
        CONST_VTBL struct IDHTMLPageVtbl __RPC_FAR *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IDHTMLPage_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IDHTMLPage_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IDHTMLPage_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IDHTMLPage_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IDHTMLPage_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IDHTMLPage_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IDHTMLPage_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IDHTMLPage_marshalWebBrowserToStream(This,__MIDL_0015)	\
    (This)->lpVtbl -> marshalWebBrowserToStream(This,__MIDL_0015)

#define IDHTMLPage_marshalEventQueueToStream(This,__MIDL_0016)	\
    (This)->lpVtbl -> marshalEventQueueToStream(This,__MIDL_0016)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IDHTMLPage_marshalWebBrowserToStream_Proxy( 
    IDHTMLPage __RPC_FAR * This,
    IStream __RPC_FAR *__RPC_FAR *__MIDL_0015);


void __RPC_STUB IDHTMLPage_marshalWebBrowserToStream_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IDHTMLPage_marshalEventQueueToStream_Proxy( 
    IDHTMLPage __RPC_FAR * This,
    IStream __RPC_FAR *__RPC_FAR *__MIDL_0016);


void __RPC_STUB IDHTMLPage_marshalEventQueueToStream_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IDHTMLPage_INTERFACE_DEFINED__ */


#ifndef __IDHTMLPageUI_INTERFACE_DEFINED__
#define __IDHTMLPageUI_INTERFACE_DEFINED__

/* interface IDHTMLPageUI */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IDHTMLPageUI;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("0E7D148E-8948-11D2-B54E-0060089002FE")
    IDHTMLPageUI : public IDispatch
    {
    public:
        virtual HRESULT STDMETHODCALLTYPE AtAnyEvent( void) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IDHTMLPageUIVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *QueryInterface )( 
            IDHTMLPageUI __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void __RPC_FAR *__RPC_FAR *ppvObject);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *AddRef )( 
            IDHTMLPageUI __RPC_FAR * This);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *Release )( 
            IDHTMLPageUI __RPC_FAR * This);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfoCount )( 
            IDHTMLPageUI __RPC_FAR * This,
            /* [out] */ UINT __RPC_FAR *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfo )( 
            IDHTMLPageUI __RPC_FAR * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo __RPC_FAR *__RPC_FAR *ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetIDsOfNames )( 
            IDHTMLPageUI __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR __RPC_FAR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID __RPC_FAR *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *Invoke )( 
            IDHTMLPageUI __RPC_FAR * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS __RPC_FAR *pDispParams,
            /* [out] */ VARIANT __RPC_FAR *pVarResult,
            /* [out] */ EXCEPINFO __RPC_FAR *pExcepInfo,
            /* [out] */ UINT __RPC_FAR *puArgErr);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *AtAnyEvent )( 
            IDHTMLPageUI __RPC_FAR * This);
        
        END_INTERFACE
    } IDHTMLPageUIVtbl;

    interface IDHTMLPageUI
    {
        CONST_VTBL struct IDHTMLPageUIVtbl __RPC_FAR *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IDHTMLPageUI_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IDHTMLPageUI_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IDHTMLPageUI_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IDHTMLPageUI_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IDHTMLPageUI_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IDHTMLPageUI_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IDHTMLPageUI_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IDHTMLPageUI_AtAnyEvent(This)	\
    (This)->lpVtbl -> AtAnyEvent(This)

#endif /* COBJMACROS */


#endif 	/* C style interface */



HRESULT STDMETHODCALLTYPE IDHTMLPageUI_AtAnyEvent_Proxy( 
    IDHTMLPageUI __RPC_FAR * This);


void __RPC_STUB IDHTMLPageUI_AtAnyEvent_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IDHTMLPageUI_INTERFACE_DEFINED__ */


#ifndef __IEvent_INTERFACE_DEFINED__
#define __IEvent_INTERFACE_DEFINED__

/* interface IEvent */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IEvent;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("FED3418C-9505-11D2-B55E-0060089002FE")
    IEvent : public IDispatch
    {
    public:
        virtual /* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE get_eventType( 
            /* [retval][out] */ EVENT_TYPE __RPC_FAR *pVal) = 0;
        
        virtual /* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE put_eventType( 
            /* [in] */ EVENT_TYPE newVal) = 0;
        
        virtual /* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE get_srcTag( 
            /* [retval][out] */ BSTR __RPC_FAR *__MIDL_0017) = 0;
        
        virtual /* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE put_srcTag( 
            /* [in] */ BSTR __MIDL_0018) = 0;
        
        virtual /* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE get_srcId( 
            /* [retval][out] */ BSTR __RPC_FAR *pVal) = 0;
        
        virtual /* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE put_srcId( 
            /* [in] */ BSTR newVal) = 0;
        
        virtual /* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE get_fromTag( 
            /* [retval][out] */ BSTR __RPC_FAR *pVal) = 0;
        
        virtual /* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE put_fromTag( 
            /* [in] */ BSTR newVal) = 0;
        
        virtual /* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE get_fromId( 
            /* [retval][out] */ BSTR __RPC_FAR *pVal) = 0;
        
        virtual /* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE put_fromId( 
            /* [in] */ BSTR newVal) = 0;
        
        virtual /* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE get_toTag( 
            /* [retval][out] */ BSTR __RPC_FAR *pVal) = 0;
        
        virtual /* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE put_toTag( 
            /* [in] */ BSTR newVal) = 0;
        
        virtual /* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE get_toId( 
            /* [retval][out] */ BSTR __RPC_FAR *pVal) = 0;
        
        virtual /* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE put_toId( 
            /* [in] */ BSTR newVal) = 0;
        
        virtual /* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE get_keyCode( 
            /* [retval][out] */ long __RPC_FAR *pVal) = 0;
        
        virtual /* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE put_keyCode( 
            /* [in] */ long newVal) = 0;
        
        virtual /* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE get_altPressed( 
            /* [retval][out] */ VARIANT_BOOL __RPC_FAR *pVal) = 0;
        
        virtual /* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE put_altPressed( 
            /* [in] */ VARIANT_BOOL newVal) = 0;
        
        virtual /* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE get_ctrlPressed( 
            /* [retval][out] */ VARIANT_BOOL __RPC_FAR *pVal) = 0;
        
        virtual /* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE put_ctrlPressed( 
            /* [in] */ VARIANT_BOOL newVal) = 0;
        
        virtual /* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE get_shiftPressed( 
            /* [retval][out] */ VARIANT_BOOL __RPC_FAR *pVal) = 0;
        
        virtual /* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE put_shiftPressed( 
            /* [in] */ VARIANT_BOOL newVal) = 0;
        
        virtual /* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE get_mouseButton( 
            /* [retval][out] */ MOUSE_BUTTON __RPC_FAR *pVal) = 0;
        
        virtual /* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE put_mouseButton( 
            /* [in] */ MOUSE_BUTTON newVal) = 0;
        
        virtual /* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE get_x( 
            /* [retval][out] */ long __RPC_FAR *pVal) = 0;
        
        virtual /* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE put_x( 
            /* [in] */ long newVal) = 0;
        
        virtual /* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE get_y( 
            /* [retval][out] */ long __RPC_FAR *pVal) = 0;
        
        virtual /* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE put_y( 
            /* [in] */ long newVal) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IEventVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *QueryInterface )( 
            IEvent __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void __RPC_FAR *__RPC_FAR *ppvObject);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *AddRef )( 
            IEvent __RPC_FAR * This);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *Release )( 
            IEvent __RPC_FAR * This);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfoCount )( 
            IEvent __RPC_FAR * This,
            /* [out] */ UINT __RPC_FAR *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfo )( 
            IEvent __RPC_FAR * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo __RPC_FAR *__RPC_FAR *ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetIDsOfNames )( 
            IEvent __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR __RPC_FAR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID __RPC_FAR *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *Invoke )( 
            IEvent __RPC_FAR * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS __RPC_FAR *pDispParams,
            /* [out] */ VARIANT __RPC_FAR *pVarResult,
            /* [out] */ EXCEPINFO __RPC_FAR *pExcepInfo,
            /* [out] */ UINT __RPC_FAR *puArgErr);
        
        /* [helpstring][id][propget] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *get_eventType )( 
            IEvent __RPC_FAR * This,
            /* [retval][out] */ EVENT_TYPE __RPC_FAR *pVal);
        
        /* [helpstring][id][propput] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *put_eventType )( 
            IEvent __RPC_FAR * This,
            /* [in] */ EVENT_TYPE newVal);
        
        /* [helpstring][id][propget] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *get_srcTag )( 
            IEvent __RPC_FAR * This,
            /* [retval][out] */ BSTR __RPC_FAR *__MIDL_0017);
        
        /* [helpstring][id][propput] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *put_srcTag )( 
            IEvent __RPC_FAR * This,
            /* [in] */ BSTR __MIDL_0018);
        
        /* [helpstring][id][propget] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *get_srcId )( 
            IEvent __RPC_FAR * This,
            /* [retval][out] */ BSTR __RPC_FAR *pVal);
        
        /* [helpstring][id][propput] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *put_srcId )( 
            IEvent __RPC_FAR * This,
            /* [in] */ BSTR newVal);
        
        /* [helpstring][id][propget] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *get_fromTag )( 
            IEvent __RPC_FAR * This,
            /* [retval][out] */ BSTR __RPC_FAR *pVal);
        
        /* [helpstring][id][propput] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *put_fromTag )( 
            IEvent __RPC_FAR * This,
            /* [in] */ BSTR newVal);
        
        /* [helpstring][id][propget] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *get_fromId )( 
            IEvent __RPC_FAR * This,
            /* [retval][out] */ BSTR __RPC_FAR *pVal);
        
        /* [helpstring][id][propput] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *put_fromId )( 
            IEvent __RPC_FAR * This,
            /* [in] */ BSTR newVal);
        
        /* [helpstring][id][propget] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *get_toTag )( 
            IEvent __RPC_FAR * This,
            /* [retval][out] */ BSTR __RPC_FAR *pVal);
        
        /* [helpstring][id][propput] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *put_toTag )( 
            IEvent __RPC_FAR * This,
            /* [in] */ BSTR newVal);
        
        /* [helpstring][id][propget] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *get_toId )( 
            IEvent __RPC_FAR * This,
            /* [retval][out] */ BSTR __RPC_FAR *pVal);
        
        /* [helpstring][id][propput] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *put_toId )( 
            IEvent __RPC_FAR * This,
            /* [in] */ BSTR newVal);
        
        /* [helpstring][id][propget] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *get_keyCode )( 
            IEvent __RPC_FAR * This,
            /* [retval][out] */ long __RPC_FAR *pVal);
        
        /* [helpstring][id][propput] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *put_keyCode )( 
            IEvent __RPC_FAR * This,
            /* [in] */ long newVal);
        
        /* [helpstring][id][propget] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *get_altPressed )( 
            IEvent __RPC_FAR * This,
            /* [retval][out] */ VARIANT_BOOL __RPC_FAR *pVal);
        
        /* [helpstring][id][propput] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *put_altPressed )( 
            IEvent __RPC_FAR * This,
            /* [in] */ VARIANT_BOOL newVal);
        
        /* [helpstring][id][propget] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *get_ctrlPressed )( 
            IEvent __RPC_FAR * This,
            /* [retval][out] */ VARIANT_BOOL __RPC_FAR *pVal);
        
        /* [helpstring][id][propput] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *put_ctrlPressed )( 
            IEvent __RPC_FAR * This,
            /* [in] */ VARIANT_BOOL newVal);
        
        /* [helpstring][id][propget] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *get_shiftPressed )( 
            IEvent __RPC_FAR * This,
            /* [retval][out] */ VARIANT_BOOL __RPC_FAR *pVal);
        
        /* [helpstring][id][propput] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *put_shiftPressed )( 
            IEvent __RPC_FAR * This,
            /* [in] */ VARIANT_BOOL newVal);
        
        /* [helpstring][id][propget] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *get_mouseButton )( 
            IEvent __RPC_FAR * This,
            /* [retval][out] */ MOUSE_BUTTON __RPC_FAR *pVal);
        
        /* [helpstring][id][propput] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *put_mouseButton )( 
            IEvent __RPC_FAR * This,
            /* [in] */ MOUSE_BUTTON newVal);
        
        /* [helpstring][id][propget] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *get_x )( 
            IEvent __RPC_FAR * This,
            /* [retval][out] */ long __RPC_FAR *pVal);
        
        /* [helpstring][id][propput] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *put_x )( 
            IEvent __RPC_FAR * This,
            /* [in] */ long newVal);
        
        /* [helpstring][id][propget] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *get_y )( 
            IEvent __RPC_FAR * This,
            /* [retval][out] */ long __RPC_FAR *pVal);
        
        /* [helpstring][id][propput] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *put_y )( 
            IEvent __RPC_FAR * This,
            /* [in] */ long newVal);
        
        END_INTERFACE
    } IEventVtbl;

    interface IEvent
    {
        CONST_VTBL struct IEventVtbl __RPC_FAR *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IEvent_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IEvent_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IEvent_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IEvent_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IEvent_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IEvent_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IEvent_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IEvent_get_eventType(This,pVal)	\
    (This)->lpVtbl -> get_eventType(This,pVal)

#define IEvent_put_eventType(This,newVal)	\
    (This)->lpVtbl -> put_eventType(This,newVal)

#define IEvent_get_srcTag(This,__MIDL_0017)	\
    (This)->lpVtbl -> get_srcTag(This,__MIDL_0017)

#define IEvent_put_srcTag(This,__MIDL_0018)	\
    (This)->lpVtbl -> put_srcTag(This,__MIDL_0018)

#define IEvent_get_srcId(This,pVal)	\
    (This)->lpVtbl -> get_srcId(This,pVal)

#define IEvent_put_srcId(This,newVal)	\
    (This)->lpVtbl -> put_srcId(This,newVal)

#define IEvent_get_fromTag(This,pVal)	\
    (This)->lpVtbl -> get_fromTag(This,pVal)

#define IEvent_put_fromTag(This,newVal)	\
    (This)->lpVtbl -> put_fromTag(This,newVal)

#define IEvent_get_fromId(This,pVal)	\
    (This)->lpVtbl -> get_fromId(This,pVal)

#define IEvent_put_fromId(This,newVal)	\
    (This)->lpVtbl -> put_fromId(This,newVal)

#define IEvent_get_toTag(This,pVal)	\
    (This)->lpVtbl -> get_toTag(This,pVal)

#define IEvent_put_toTag(This,newVal)	\
    (This)->lpVtbl -> put_toTag(This,newVal)

#define IEvent_get_toId(This,pVal)	\
    (This)->lpVtbl -> get_toId(This,pVal)

#define IEvent_put_toId(This,newVal)	\
    (This)->lpVtbl -> put_toId(This,newVal)

#define IEvent_get_keyCode(This,pVal)	\
    (This)->lpVtbl -> get_keyCode(This,pVal)

#define IEvent_put_keyCode(This,newVal)	\
    (This)->lpVtbl -> put_keyCode(This,newVal)

#define IEvent_get_altPressed(This,pVal)	\
    (This)->lpVtbl -> get_altPressed(This,pVal)

#define IEvent_put_altPressed(This,newVal)	\
    (This)->lpVtbl -> put_altPressed(This,newVal)

#define IEvent_get_ctrlPressed(This,pVal)	\
    (This)->lpVtbl -> get_ctrlPressed(This,pVal)

#define IEvent_put_ctrlPressed(This,newVal)	\
    (This)->lpVtbl -> put_ctrlPressed(This,newVal)

#define IEvent_get_shiftPressed(This,pVal)	\
    (This)->lpVtbl -> get_shiftPressed(This,pVal)

#define IEvent_put_shiftPressed(This,newVal)	\
    (This)->lpVtbl -> put_shiftPressed(This,newVal)

#define IEvent_get_mouseButton(This,pVal)	\
    (This)->lpVtbl -> get_mouseButton(This,pVal)

#define IEvent_put_mouseButton(This,newVal)	\
    (This)->lpVtbl -> put_mouseButton(This,newVal)

#define IEvent_get_x(This,pVal)	\
    (This)->lpVtbl -> get_x(This,pVal)

#define IEvent_put_x(This,newVal)	\
    (This)->lpVtbl -> put_x(This,newVal)

#define IEvent_get_y(This,pVal)	\
    (This)->lpVtbl -> get_y(This,pVal)

#define IEvent_put_y(This,newVal)	\
    (This)->lpVtbl -> put_y(This,newVal)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE IEvent_get_eventType_Proxy( 
    IEvent __RPC_FAR * This,
    /* [retval][out] */ EVENT_TYPE __RPC_FAR *pVal);


void __RPC_STUB IEvent_get_eventType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE IEvent_put_eventType_Proxy( 
    IEvent __RPC_FAR * This,
    /* [in] */ EVENT_TYPE newVal);


void __RPC_STUB IEvent_put_eventType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE IEvent_get_srcTag_Proxy( 
    IEvent __RPC_FAR * This,
    /* [retval][out] */ BSTR __RPC_FAR *__MIDL_0017);


void __RPC_STUB IEvent_get_srcTag_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE IEvent_put_srcTag_Proxy( 
    IEvent __RPC_FAR * This,
    /* [in] */ BSTR __MIDL_0018);


void __RPC_STUB IEvent_put_srcTag_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE IEvent_get_srcId_Proxy( 
    IEvent __RPC_FAR * This,
    /* [retval][out] */ BSTR __RPC_FAR *pVal);


void __RPC_STUB IEvent_get_srcId_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE IEvent_put_srcId_Proxy( 
    IEvent __RPC_FAR * This,
    /* [in] */ BSTR newVal);


void __RPC_STUB IEvent_put_srcId_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE IEvent_get_fromTag_Proxy( 
    IEvent __RPC_FAR * This,
    /* [retval][out] */ BSTR __RPC_FAR *pVal);


void __RPC_STUB IEvent_get_fromTag_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE IEvent_put_fromTag_Proxy( 
    IEvent __RPC_FAR * This,
    /* [in] */ BSTR newVal);


void __RPC_STUB IEvent_put_fromTag_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE IEvent_get_fromId_Proxy( 
    IEvent __RPC_FAR * This,
    /* [retval][out] */ BSTR __RPC_FAR *pVal);


void __RPC_STUB IEvent_get_fromId_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE IEvent_put_fromId_Proxy( 
    IEvent __RPC_FAR * This,
    /* [in] */ BSTR newVal);


void __RPC_STUB IEvent_put_fromId_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE IEvent_get_toTag_Proxy( 
    IEvent __RPC_FAR * This,
    /* [retval][out] */ BSTR __RPC_FAR *pVal);


void __RPC_STUB IEvent_get_toTag_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE IEvent_put_toTag_Proxy( 
    IEvent __RPC_FAR * This,
    /* [in] */ BSTR newVal);


void __RPC_STUB IEvent_put_toTag_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE IEvent_get_toId_Proxy( 
    IEvent __RPC_FAR * This,
    /* [retval][out] */ BSTR __RPC_FAR *pVal);


void __RPC_STUB IEvent_get_toId_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE IEvent_put_toId_Proxy( 
    IEvent __RPC_FAR * This,
    /* [in] */ BSTR newVal);


void __RPC_STUB IEvent_put_toId_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE IEvent_get_keyCode_Proxy( 
    IEvent __RPC_FAR * This,
    /* [retval][out] */ long __RPC_FAR *pVal);


void __RPC_STUB IEvent_get_keyCode_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE IEvent_put_keyCode_Proxy( 
    IEvent __RPC_FAR * This,
    /* [in] */ long newVal);


void __RPC_STUB IEvent_put_keyCode_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE IEvent_get_altPressed_Proxy( 
    IEvent __RPC_FAR * This,
    /* [retval][out] */ VARIANT_BOOL __RPC_FAR *pVal);


void __RPC_STUB IEvent_get_altPressed_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE IEvent_put_altPressed_Proxy( 
    IEvent __RPC_FAR * This,
    /* [in] */ VARIANT_BOOL newVal);


void __RPC_STUB IEvent_put_altPressed_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE IEvent_get_ctrlPressed_Proxy( 
    IEvent __RPC_FAR * This,
    /* [retval][out] */ VARIANT_BOOL __RPC_FAR *pVal);


void __RPC_STUB IEvent_get_ctrlPressed_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE IEvent_put_ctrlPressed_Proxy( 
    IEvent __RPC_FAR * This,
    /* [in] */ VARIANT_BOOL newVal);


void __RPC_STUB IEvent_put_ctrlPressed_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE IEvent_get_shiftPressed_Proxy( 
    IEvent __RPC_FAR * This,
    /* [retval][out] */ VARIANT_BOOL __RPC_FAR *pVal);


void __RPC_STUB IEvent_get_shiftPressed_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE IEvent_put_shiftPressed_Proxy( 
    IEvent __RPC_FAR * This,
    /* [in] */ VARIANT_BOOL newVal);


void __RPC_STUB IEvent_put_shiftPressed_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE IEvent_get_mouseButton_Proxy( 
    IEvent __RPC_FAR * This,
    /* [retval][out] */ MOUSE_BUTTON __RPC_FAR *pVal);


void __RPC_STUB IEvent_get_mouseButton_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE IEvent_put_mouseButton_Proxy( 
    IEvent __RPC_FAR * This,
    /* [in] */ MOUSE_BUTTON newVal);


void __RPC_STUB IEvent_put_mouseButton_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE IEvent_get_x_Proxy( 
    IEvent __RPC_FAR * This,
    /* [retval][out] */ long __RPC_FAR *pVal);


void __RPC_STUB IEvent_get_x_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE IEvent_put_x_Proxy( 
    IEvent __RPC_FAR * This,
    /* [in] */ long newVal);


void __RPC_STUB IEvent_put_x_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE IEvent_get_y_Proxy( 
    IEvent __RPC_FAR * This,
    /* [retval][out] */ long __RPC_FAR *pVal);


void __RPC_STUB IEvent_get_y_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propput] */ HRESULT STDMETHODCALLTYPE IEvent_put_y_Proxy( 
    IEvent __RPC_FAR * This,
    /* [in] */ long newVal);


void __RPC_STUB IEvent_put_y_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IEvent_INTERFACE_DEFINED__ */


#ifndef __IEventQueue_INTERFACE_DEFINED__
#define __IEventQueue_INTERFACE_DEFINED__

/* interface IEventQueue */
/* [unique][helpstring][dual][uuid][object] */ 


EXTERN_C const IID IID_IEventQueue;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("FED3418D-9505-11D2-B55E-0060089002FE")
    IEventQueue : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetEvent( 
            IEvent __RPC_FAR *__RPC_FAR *__MIDL_0019) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE QueueEvent( 
            IEvent __RPC_FAR *__MIDL_0020) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetReaderSemaphore( 
            int __RPC_FAR *__MIDL_0021) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE set_extension_table( 
            int __MIDL_0022) = 0;
        
        virtual /* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE get_EventAvailable( 
            /* [retval][out] */ VARIANT_BOOL __RPC_FAR *pVal) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IEventQueueVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *QueryInterface )( 
            IEventQueue __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void __RPC_FAR *__RPC_FAR *ppvObject);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *AddRef )( 
            IEventQueue __RPC_FAR * This);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *Release )( 
            IEventQueue __RPC_FAR * This);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfoCount )( 
            IEventQueue __RPC_FAR * This,
            /* [out] */ UINT __RPC_FAR *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfo )( 
            IEventQueue __RPC_FAR * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo __RPC_FAR *__RPC_FAR *ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetIDsOfNames )( 
            IEventQueue __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR __RPC_FAR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID __RPC_FAR *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *Invoke )( 
            IEventQueue __RPC_FAR * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS __RPC_FAR *pDispParams,
            /* [out] */ VARIANT __RPC_FAR *pVarResult,
            /* [out] */ EXCEPINFO __RPC_FAR *pExcepInfo,
            /* [out] */ UINT __RPC_FAR *puArgErr);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetEvent )( 
            IEventQueue __RPC_FAR * This,
            IEvent __RPC_FAR *__RPC_FAR *__MIDL_0019);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *QueueEvent )( 
            IEventQueue __RPC_FAR * This,
            IEvent __RPC_FAR *__MIDL_0020);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetReaderSemaphore )( 
            IEventQueue __RPC_FAR * This,
            int __RPC_FAR *__MIDL_0021);
        
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *set_extension_table )( 
            IEventQueue __RPC_FAR * This,
            int __MIDL_0022);
        
        /* [helpstring][id][propget] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *get_EventAvailable )( 
            IEventQueue __RPC_FAR * This,
            /* [retval][out] */ VARIANT_BOOL __RPC_FAR *pVal);
        
        END_INTERFACE
    } IEventQueueVtbl;

    interface IEventQueue
    {
        CONST_VTBL struct IEventQueueVtbl __RPC_FAR *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IEventQueue_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IEventQueue_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IEventQueue_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IEventQueue_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IEventQueue_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IEventQueue_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IEventQueue_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IEventQueue_GetEvent(This,__MIDL_0019)	\
    (This)->lpVtbl -> GetEvent(This,__MIDL_0019)

#define IEventQueue_QueueEvent(This,__MIDL_0020)	\
    (This)->lpVtbl -> QueueEvent(This,__MIDL_0020)

#define IEventQueue_GetReaderSemaphore(This,__MIDL_0021)	\
    (This)->lpVtbl -> GetReaderSemaphore(This,__MIDL_0021)

#define IEventQueue_set_extension_table(This,__MIDL_0022)	\
    (This)->lpVtbl -> set_extension_table(This,__MIDL_0022)

#define IEventQueue_get_EventAvailable(This,pVal)	\
    (This)->lpVtbl -> get_EventAvailable(This,pVal)

#endif /* COBJMACROS */


#endif 	/* C style interface */



/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IEventQueue_GetEvent_Proxy( 
    IEventQueue __RPC_FAR * This,
    IEvent __RPC_FAR *__RPC_FAR *__MIDL_0019);


void __RPC_STUB IEventQueue_GetEvent_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IEventQueue_QueueEvent_Proxy( 
    IEventQueue __RPC_FAR * This,
    IEvent __RPC_FAR *__MIDL_0020);


void __RPC_STUB IEventQueue_QueueEvent_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IEventQueue_GetReaderSemaphore_Proxy( 
    IEventQueue __RPC_FAR * This,
    int __RPC_FAR *__MIDL_0021);


void __RPC_STUB IEventQueue_GetReaderSemaphore_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IEventQueue_set_extension_table_Proxy( 
    IEventQueue __RPC_FAR * This,
    int __MIDL_0022);


void __RPC_STUB IEventQueue_set_extension_table_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


/* [helpstring][id][propget] */ HRESULT STDMETHODCALLTYPE IEventQueue_get_EventAvailable_Proxy( 
    IEventQueue __RPC_FAR * This,
    /* [retval][out] */ VARIANT_BOOL __RPC_FAR *pVal);


void __RPC_STUB IEventQueue_get_EventAvailable_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IEventQueue_INTERFACE_DEFINED__ */



#ifndef __MYSPAGELib_LIBRARY_DEFINED__
#define __MYSPAGELib_LIBRARY_DEFINED__

/* library MYSPAGELib */
/* [helpstring][version][uuid] */ 


EXTERN_C const IID LIBID_MYSPAGELib;

EXTERN_C const CLSID CLSID_DHTMLPage;

#ifdef __cplusplus

class DECLSPEC_UUID("0E7D148D-8948-11D2-B54E-0060089002FE")
DHTMLPage;
#endif

EXTERN_C const CLSID CLSID_Event;

#ifdef __cplusplus

class DECLSPEC_UUID("44D46F53-9375-11D2-B559-0060089002FE")
Event;
#endif

EXTERN_C const CLSID CLSID_EventQueue;

#ifdef __cplusplus

class DECLSPEC_UUID("44D46F51-9375-11D2-B559-0060089002FE")
EventQueue;
#endif
#endif /* __MYSPAGELib_LIBRARY_DEFINED__ */

/* Additional Prototypes for ALL interfaces */

unsigned long             __RPC_USER  BSTR_UserSize(     unsigned long __RPC_FAR *, unsigned long            , BSTR __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  BSTR_UserMarshal(  unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, BSTR __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  BSTR_UserUnmarshal(unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, BSTR __RPC_FAR * ); 
void                      __RPC_USER  BSTR_UserFree(     unsigned long __RPC_FAR *, BSTR __RPC_FAR * ); 

/* end of Additional Prototypes */

#ifdef __cplusplus
}
#endif

#endif
