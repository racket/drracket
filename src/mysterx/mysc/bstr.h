// bstr.h

Scheme_Object *BSTRToSchemeString(BSTR);
void updateSchemeStringFromBSTR(Scheme_Object *,BSTR);
BSTR stringToBSTR(char *,size_t);
BSTR schemeStringToBSTR(Scheme_Object *);
