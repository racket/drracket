// bstr.cxx -- BSTR utility functions

#include <windows.h>

#include "escheme.h"

#include "bstr.h"

Scheme_Object *BSTRToSchemeString(BSTR bstr) {
  char *buff;
  unsigned int len;

  len = SysStringLen(bstr);

  buff = (char *)scheme_malloc(len + 1);

  WideCharToMultiByte(CP_ACP,(DWORD)0,bstr,len,
		      buff,len,
		      NULL,NULL);

  buff[len] = '\0';

  return scheme_make_string(buff);
}

void updateSchemeStringFromBSTR(Scheme_Object *val,BSTR bstr) {
  int len;

  len = SysStringLen(bstr);

  if (len > SCHEME_STRLEN_VAL(val)) {
    scheme_signal_error("String updated with longer string");
  }

  WideCharToMultiByte(CP_ACP,(DWORD)0,
		      bstr,len,
		      SCHEME_STR_VAL(val),SCHEME_STRLEN_VAL(val),
		      NULL,NULL);

  SCHEME_STRLEN_VAL(val) = len;

}

BSTR stringToBSTR(char *s,size_t len) {
  HRESULT hr;
  BSTR bstr;
  WCHAR *unicodeString;

  unicodeString = (WCHAR *)scheme_malloc((len + 1) * sizeof(WCHAR));
  scheme_dont_gc_ptr(unicodeString);

  hr = MultiByteToWideChar(CP_ACP,(DWORD)0,s,len,
			   unicodeString,len + 1);

  if (hr == 0 && len > 0) {
    scheme_signal_error("Error translating string parameter to Unicode");
  }

  unicodeString[len] = '\0';

  bstr = SysAllocString(unicodeString);

  scheme_gc_ptr_ok(unicodeString);

  if (bstr == NULL) {
    scheme_signal_error("Error allocating string parameter");
  }

  return bstr;
}

BSTR schemeStringToBSTR(Scheme_Object *o) {
  return stringToBSTR(SCHEME_STR_VAL(o),SCHEME_STRLEN_VAL(o));
}


