// htmlutil.h

#define elt_style_string_getter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  BSTR bstr; \
  Scheme_Object *retval; \
  if (MX_ELEMENTP(argv[0]) == FALSE) { \
    scheme_wrong_type(scm_name,"mx-element",0,argc,argv); \
  } \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  hr = pIHTMLStyle->dhtml_name(&bstr); \
  retval = BSTRToSchemeString(bstr); \
  SysFreeString(bstr); \
  pIHTMLStyle->Release(); \
  if (hr != S_OK) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return retval; \
}  

#define elt_style_string_setter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  BSTR bstr; \
  if (MX_ELEMENTP(argv[0]) == FALSE) { \
    scheme_wrong_type(scm_name,"mx-element",0,argc,argv); \
  } \
  if (SCHEME_STRINGP(argv[1]) == FALSE) { \
    scheme_wrong_type(scm_name,"string",1,argc,argv); \
  } \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  bstr = schemeStringToBSTR(argv[1]); \
  hr = pIHTMLStyle->dhtml_name(bstr); \
  SysFreeString(bstr); \
  pIHTMLStyle->Release(); \
  if (hr != S_OK) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_void; \
}    

#define elt_style_bool_getter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  VARIANT_BOOL boolVal; \
  if (MX_ELEMENTP(argv[0]) == FALSE) { \
    scheme_wrong_type(scm_name,"mx-element",0,argc,argv); \
  } \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  hr = pIHTMLStyle->dhtml_name(&boolVal); \
  pIHTMLStyle->Release(); \
  if (hr != S_OK) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return (boolVal == 0) ? scheme_false : scheme_true; \
}  

#define elt_style_bool_setter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  VARIANT_BOOL boolVal;	\
  if (MX_ELEMENTP(argv[0]) == FALSE) { \
    scheme_wrong_type(scm_name,"mx-element",0,argc,argv); \
  } \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  boolVal = (argv[1] == scheme_false) ? 0 : -1; \
  hr = pIHTMLStyle->dhtml_name(boolVal); \
  pIHTMLStyle->Release(); \
  if (hr != S_OK) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_void; \
}

#define elt_style_long_getter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  long val; \
  if (MX_ELEMENTP(argv[0]) == FALSE) { \
    scheme_wrong_type(scm_name,"mx-element",0,argc,argv); \
  } \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  hr = pIHTMLStyle->dhtml_name(&val); \
  pIHTMLStyle->Release(); \
  if (hr != S_OK) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_make_integer(val); \
}  

#define elt_style_long_setter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  long val; \
  if (MX_ELEMENTP(argv[0]) == FALSE) { \
    scheme_wrong_type(scm_name,"mx-element",0,argc,argv); \
  } \
  if (SCHEME_INTP(argv[1]) == FALSE) { \
    scheme_wrong_type(scm_name,"integer",1,argc,argv); \
  } \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  if (scheme_get_int_val(argv[1],&val) == 0) { \
      scheme_signal_error("Integer argument won't fit in a long"); \
  } \
  hr = pIHTMLStyle->dhtml_name(val); \
  pIHTMLStyle->Release(); \
  if (hr != S_OK) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_void; \
}

#ifdef MZ_USE_SINGLE_FLOATS
#define elt_style_float_getter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  float val; \
  if (MX_ELEMENTP(argv[0]) == FALSE) { \
    scheme_wrong_type(scm_name,"mx-element",0,argc,argv); \
  } \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  hr = pIHTMLStyle->dhtml_name(&val); \
  pIHTMLStyle->Release(); \
  if (hr != S_OK) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_make_float(val); \
}  

#define elt_style_float_setter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  if (MX_ELEMENTP(argv[0]) == FALSE) { \
    scheme_wrong_type(scm_name,"mx-element",0,argc,argv); \
  } \
  if (SCHEME_FLTP(argv[1]) == FALSE) { \
    scheme_wrong_type(scm_name,"float",1,argc,argv); \
  } \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  hr = pIHTMLStyle->dhtml_name(SCHEME_FLT_VAL(argv[1]); \
  pIHTMLStyle->Release(); \
  if (hr != S_OK) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_void; \
}
#else 
#define elt_style_float_getter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  float val; \
  if (MX_ELEMENTP(argv[0]) == FALSE) { \
    scheme_wrong_type(scm_name,"mx-element",0,argc,argv); \
  } \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  hr = pIHTMLStyle->dhtml_name(&val); \
  pIHTMLStyle->Release(); \
  if (hr != S_OK) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_make_double((double)val); \
}  

#define elt_style_float_setter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  if (MX_ELEMENTP(argv[0]) == FALSE) { \
    scheme_wrong_type(scm_name,"mx-element",0,argc,argv); \
  } \
  if (SCHEME_DBLP(argv[1]) == FALSE) { \
    scheme_wrong_type(scm_name,"double",1,argc,argv); \
  } \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  hr = pIHTMLStyle->dhtml_name((float)SCHEME_DBL_VAL(argv[1])); \
  pIHTMLStyle->Release(); \
  if (hr != S_OK) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_void; \
}
#endif

#define elt_style_variant_getter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  VARIANT variant; \
  if (MX_ELEMENTP(argv[0]) == FALSE) { \
    scheme_wrong_type(scm_name,"mx-element",0,argc,argv); \
  } \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  hr = pIHTMLStyle->dhtml_name(&variant); \
  pIHTMLStyle->Release(); \
  if (hr != S_OK) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return variantToSchemeObject(&variant); \
}  

#define elt_style_variant_setter(fun_name,scm_name,dhtml_name) \
Scheme_Object *fun_name(int argc,Scheme_Object **argv) { \
  HRESULT hr; \
  IHTMLStyle *pIHTMLStyle; \
  VARIANT variant; \
  if (MX_ELEMENTP(argv[0]) == FALSE) { \
    scheme_wrong_type(scm_name,"mx-element",0,argc,argv); \
  } \
  pIHTMLStyle = styleInterfaceFromElement(argv[0]); \
  marshallSchemeValueToVariant(argv[1],&variant); \
  hr = pIHTMLStyle->dhtml_name(variant); \
  pIHTMLStyle->Release(); \
  if (hr != S_OK) { \
    scheme_signal_error(#dhtml_name " failed with code %X",hr); \
  } \
  return scheme_void; \
}

