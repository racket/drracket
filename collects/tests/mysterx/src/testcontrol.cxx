// TestControl.cpp : Implementation of CTestControl

#include "stdafx.h"
#include "Testobject.h"
#include "TestControl.h"

/////////////////////////////////////////////////////////////////////////////
// CTestControl



STDMETHODIMP CTestControl::AddTest(long n1, long *n2, long *n3)
{
  // note side effect  


  *n3 = n1 + *n2;

  *n2 = n1;

	return S_OK;
}

STDMETHODIMP CTestControl::StringTest(BSTR s1, BSTR s2, BSTR *s3)
{
  int len1,len2;

  len1 = SysStringLen(s1);
	len2 = SysStringLen(s2);

  *s3 = SysAllocStringByteLen(NULL,(len1 + len2 + 1)*2);
  wcsncpy(*s3,s1,len1);
  wcsncpy(*s3 + len1,s2,len2);
  *(*s3 + len1 + len2) = L'\0';

  return S_OK;
}

STDMETHODIMP CTestControl::ShortTest(short n1, short n2, short *n3)
{
  *n3 = n1 * n2;

	return S_OK;
}

STDMETHODIMP CTestControl::FloatTest(double n1, double n2, double *n3)
{
  *n3 = n2 - n1;

	return S_OK;
}
