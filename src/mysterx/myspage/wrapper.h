#pragma once
#ifndef __WRAPPERDISPATCH_H__
#define __WRAPPERDISPATCH_H__

// based on Chris Sells' code at www.sellsbrothers.com/tools

class CWrapperDispatch :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CWrapperDispatch>,
    public IDispatch
{
public:
    void SetDispatch(IDispatch* pdisp) {
        m_spdisp = pdisp;
    }

DECLARE_NO_REGISTRY()
BEGIN_COM_MAP(CWrapperDispatch)
    COM_INTERFACE_ENTRY(IDispatch)
END_COM_MAP()

// IDispatch

    STDMETHODIMP GetTypeInfoCount(UINT* pctinfo) {
        if( !m_spdisp ) return E_UNEXPECTED;
        return m_spdisp->GetTypeInfoCount(pctinfo);
    }

	STDMETHODIMP GetTypeInfo(UINT itinfo, LCID lcid, ITypeInfo** pptinfo) {
        if( !pptinfo ) return E_POINTER;
        *pptinfo = 0;
        if( !m_spdisp ) return E_UNEXPECTED;
        return m_spdisp->GetTypeInfo(itinfo, lcid, pptinfo);
    }

	STDMETHODIMP GetIDsOfNames(REFIID riid, LPOLESTR* rgszNames, UINT cNames,
							   LCID lcid, DISPID* rgdispid) {
        if( !m_spdisp ) return E_UNEXPECTED;
        return m_spdisp->GetIDsOfNames(riid, rgszNames, cNames, lcid, rgdispid);
    }

	STDMETHODIMP Invoke(DISPID dispidMember, REFIID riid, LCID lcid,
						WORD wFlags, DISPPARAMS* pdispparams,
						VARIANT* pvarResult, EXCEPINFO* pexcepinfo,
						UINT* puArgErr) {
        if( !m_spdisp ) return E_UNEXPECTED;
        return m_spdisp->Invoke(dispidMember, riid, lcid, wFlags, pdispparams, pvarResult, pexcepinfo, puArgErr);
    }

private:
    CComPtr<IDispatch>  m_spdisp;
};

#endif  // __WRAPPERDISPATCH_H__
