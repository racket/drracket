/*
 * File:    wxstring.h
 * Purpose: String class
 * Author:  Stefan (steve) Hammes (Julian Smart)
 * Created: 1994
 * Updated: 
 * Copyright:   (c) 1994
 */

#ifdef __GNUG__
#pragma interface
#endif

/* sccsid[] = "%W% %G%" */

#ifndef wx_stringh
#define wx_stringh

#include "common.h"
#include "wx_obj.h"
#if defined(wx_mac) && defined(Length)
#undef Length
#endif

/*
 * String
 *
 */
#define NO_POS ((size_t)(-1)) // undefined position

class wxString 
// #ifndef wxSTRING_TEST
: public wxObject
// #endif
{
private:
  char *stringData_;
  void assertElement(size_t) const;    // Index in range
public:
  void SetData(char *s);
  enum stripType {leading = 0x1, trailing = 0x2, both = 0x3};
  enum caseCompare {exact, ignoreCase};

  wxString();          // Null string
  wxString(char *s); // Copy constructor
  wxString(const wxString& s); // Copy constructor
  wxString(char);
  wxString(const char *fmt, ...); // formatted construction (also possible 'wxString s("hello")'
  ~wxString();

  // formatted construction
  int sprintf(const char *fmt, ...);
  
  // input
//  char *ReadLine(FILE *inputFile);

  // Type conversion:
#define NO_CONST_STRING 1
#if NO_CONST_STRING
#define const /**/ 
#endif
  const char*   Data() const {return stringData_;} // un-CONST in certain environment necessary
  operator const char*() const {return stringData_;} // dito.
#if NO_CONST_STRING
#undef const
#endif
  const char* GetData() const {return stringData_;} // this one is alway const!
  wxString     Copy() const;

  // comparison
  int          CompareTo(const char* cs,      caseCompare cmp = exact) const;
  int          CompareTo(const wxString& st, caseCompare cmp = exact) const;
//  Bool operator ==(const wxString& s2) const { return(CompareTo(s2)==0); }
  Bool operator ==(char *s2) const { return(CompareTo(s2)==0); }
//  Bool operator !=(const wxString& s) const { return(CompareTo(s)!=0); }
  Bool operator !=(char  *s) const { return(CompareTo(s)!=0); }
  Bool operator <(const wxString& s) const { return(CompareTo(s)<0); }
  Bool operator <(const char  *s) const { return(CompareTo(s)<0); }
  Bool operator <=(const wxString& s) const { return(CompareTo(s)<=0); }
  Bool operator <=(const char  *s) const { return(CompareTo(s)<=0); }
  Bool operator >(const wxString& s) const { return(CompareTo(s)>0); }
  Bool operator >(const char  *s) const { return(CompareTo(s)>0); }
  Bool operator >=(const wxString& s) const { return(CompareTo(s)>=0); }
  Bool operator >=(const char  *s) const { return(CompareTo(s)>=0); }
  
  // Assignment:
  wxString&    operator=(const char*);     // Replace string
  wxString&    operator=(const wxString&);    // Replace string
  wxString&    operator+=(const char*);    // Append string.
  wxString&    operator+=(const wxString& s);

  // Indexing operators:
  char&     operator[](int);     // Indexing with bounds checking (must be int!)
  char&     operator()(size_t);     // Indexing with  bounds checking
  wxString  operator()(size_t start, size_t len) const;       // Sub-string operator
  wxString  SubString(const char* pat, size_t start=0) const;     
  wxString  SubString(const wxString& pat, size_t start=0) const;     
  
  // Non-static member functions:
  wxString operator +(const wxString& s) const;
  wxString operator +(const char *s) const;
  wxString&    Append(const char* cs);
  wxString&    Append(const wxString& s);
  wxString&    Append(char c, size_t rep=1);   // Append c rep times
  Bool Contains(const char* pat,      caseCompare cmp = exact) const;
  Bool Contains(const wxString& pat, caseCompare cmp = exact) const;
  size_t    First(char c) const;
  size_t    First(const char* cs) const;
  size_t    First(const wxString& cs) const;
  
  size_t    Index(const char* pat, size_t i=0, caseCompare cmp = exact) const;
  size_t    Index(const wxString& s, size_t i=0, caseCompare cmp = exact) const;
              
  wxString&    Insert(size_t pos, const char*);
  wxString&    Insert(size_t pos, const wxString&);
  
  Bool IsAscii() const;
  Bool IsNumber() const;
  Bool IsWord() const;
  Bool IsNull() const { return(stringData_ == NULL); }
  size_t    Last(char c) const;
  size_t    Last(const char* cs) const;
  size_t    Last(const wxString& cs) const;
  size_t    Length() const;
  
  wxString&    Prepend(const char*);           // Prepend a character string
  wxString&    Prepend(const wxString& s);
  wxString&    Prepend(char c, size_t rep=1);  // Prepend c rep times
  
  wxString&    Remove(size_t pos);         // Remove pos to end of string
  wxString&    Remove(size_t pos, size_t n);       // Remove n chars starting at pos
  wxString&    RemoveLast(void);

  wxString&    Replace(size_t pos, size_t n, const char*);
  wxString&    Replace(size_t pos, size_t n, const wxString&);
  wxString  Strip(stripType s=trailing, char c=' ') const;
  void      LowerCase();              // Change self to lower-case
  void      UpperCase();              // Change self to upper-case

  // Edward Z.'s additions
  wxString SubString(size_t from, size_t to);
//  wxString UpperCase(void);
//  wxString LowerCase(void);
};

#endif // wx_stringh
