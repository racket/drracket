
#include <stdio.h>

#include "wx_dialg.h"
#ifndef OLD_WXWINDOWS
// wxWindows 1.62(f): new include for common dialogs
#include "wx_cmdlg.h"
#endif

#include "wx_medio.h"
#include <string.h>

#ifndef WXME_LSB_FIRST
#ifdef wx_msw
#define WXME_LSB_FIRST 1
#else
#define WXME_LSB_FIRST 0
#endif
#endif

#define TYPESAFE 0

enum {
  st_STRING,
  st_NUMBER,
  st_FLOAT,
  st_FIXED
};

/* 
   Integer format specified by first byte:
     bit 8: 0 - read 7-bit (positive) number
     bit 8: 1 - ...
        bit 7: 0 - read abother byte for 15-bit (positive) number
	bit 7: 1 - negative and long numbers...
	 bit 1: 1 - read another 8-bit (signed) number
	 bit 1: 0 - ...
	   bit 2: 1 - read another 16-bit (signed) number
	   bit 2: 0 - read another 32-bit (signed) number
*/

void wxMediaIOCheckLSB(void)
{
  long v = 1;
  char first;

  first = *(char *)&v;
#if WXME_LSB_FIRST
  first = !first;
#endif
  if (first)
    wxMessageBox("wxMedia was compiled with the wrong WXME_LSB_FIRST flag\nvalue. The correct value is "
#if WXME_LSB_FIRST
		 "FALSE"
#else
		 "TRUE"
#endif
		 ".",
		 "Warning");
}

/****************************************************************/

wxMediaStreamInFileBase::wxMediaStreamInFileBase(FILE *s)
{
  f = s;
}

wxMediaStreamInFileBase::~wxMediaStreamInFileBase()
{
}

long wxMediaStreamInFileBase::Tell(void)
{
  return ftell(f);
}

void wxMediaStreamInFileBase::Seek(long p)
{
  fseek(f, p, 0);
}

void wxMediaStreamInFileBase::Skip(long n)
{
  fseek(f, n, 1);
}

Bool wxMediaStreamInFileBase::Bad(void)
{
  return ferror(f);
}

long wxMediaStreamInFileBase::Read(char *data, long len)
{
  if (len <= 0)
    return 0;

  return fread(data, 1, len, f);
}

/****************************************************************/

wxMediaStreamOutFileBase::wxMediaStreamOutFileBase(FILE *s)
{
  f = s;
}

wxMediaStreamOutFileBase::~wxMediaStreamOutFileBase()
{
}

long wxMediaStreamOutFileBase::Tell(void)
{
  return ftell(f);
}

void wxMediaStreamOutFileBase::Seek(long p)
{
  fseek(f, p, 0);
}

Bool wxMediaStreamOutFileBase::Bad(void)
{
  return ferror(f);
}

void wxMediaStreamOutFileBase::Write(char *data, long len)
{
  if (len <= 0)
    return;

  fwrite(data, 1, len, f);
}

/****************************************************************/

wxMediaStreamInStringBase::wxMediaStreamInStringBase(char *s, long l)
{
  string = s;
  len = l;
  pos = 0;
  bad = FALSE;
}

wxMediaStreamInStringBase::~wxMediaStreamInStringBase()
{
}

long wxMediaStreamInStringBase::Tell(void)
{
  return pos;
}

void wxMediaStreamInStringBase::Seek(long p)
{
  if (p < 0)
    p = 0;
  else if (p < len)
    pos = p;
  else
    pos = len;
}

void wxMediaStreamInStringBase::Skip(long n)
{
  pos += n;
  if (pos > len)
    pos = len;
  else if (pos < 0)
    pos = 0;
}

Bool wxMediaStreamInStringBase::Bad(void)
{
  return bad;
}

long wxMediaStreamInStringBase::Read(char *data, long l)
{
  if (l + pos > len) {
    bad = TRUE;
    l = len - pos;
  }

  memcpy(data, string + pos, l);
  pos += l;

  return l;
}

/****************************************************************/

wxMediaStreamOutStringBase::wxMediaStreamOutStringBase()
{
  alloc = 50;
  len = pos = 0;
  string = new char[alloc];
  bad = FALSE;
}

wxMediaStreamOutStringBase::~wxMediaStreamOutStringBase()
{
}

char *wxMediaStreamOutStringBase::GetString(long *l)
{
  *l = len;
  return string;
}

long wxMediaStreamOutStringBase::Tell(void)
{
  return pos;
}

void wxMediaStreamOutStringBase::Seek(long p)
{
  if (p < 0)
    pos = 0;
  else if (p > len)
    pos = len;
  else
    pos = p;
}

Bool wxMediaStreamOutStringBase::Bad(void)
{
  return bad;
}

void wxMediaStreamOutStringBase::Write(char *data, long l)
{
  if (l + pos > alloc) {
    char *old = string;

    alloc *= 2;
    string = new char[alloc];
    memcpy(string, old, len);
    delete[] old;
  }

  memcpy(string + pos, data, l);
  pos += l;
  if (len < pos)
    len = pos;
}

/****************************************************************/

wxMediaStreamIn::wxMediaStreamIn(wxMediaStreamInBase& s)
{
  f = &s;
  boundalloc = 10;
  boundcount = 0;
  boundaries = new long[boundalloc];
  bad = FALSE;
}

wxMediaStreamIn::~wxMediaStreamIn()
{
  delete[] boundaries;
}

void wxMediaStreamIn::Typecheck(char v)
{
  if (bad)
    return;

  if (boundcount && (f->Tell() >= boundaries[boundcount - 1])) {
    bad = TRUE;
    wxMessageBox("Overread caused by file corruption"
		 " or unknown internal error.", "Error");
    return;
  }

  bad = f->Bad();

  if (bad) {
    wxMessageBox("Unknown stream error.", "Error");
    return;
  }

#if TYPESAFE
  char t;

  f->Read(&t, 1);

  if (bad = f->Bad()) {
    wxMessageBox("Unknown stream error.", "Error");
    return;
  }

  bad = (t != v);

  if (bad)
    wxMessageBox("Type safety error.", "Error");
#endif
}

wxMediaStreamIn& wxMediaStreamIn::GetFixed(long& v)
{
  Typecheck(st_FIXED);

  if (bad) {
    v = 0;
    return *this;
  }

#if !WXME_LSB_FIRST
  f->Read((char *)&v, sizeof(long));
#else
  if (WXME_VERSION_ONE())
    f->Read((char *)&v, sizeof(long));
  else {
    unsigned char bl[4];
    
    f->Read((char *)bl, 4);
    v = (((long)bl[0]) << 24) + (((long)bl[1]) << 16)
      + (((long)bl[2]) << 8) + bl[3];
  }
#endif

  return *this;
}

char *wxMediaStreamIn::GetString(long *n)
{
  long m;
  char *r;

  if (bad) {
    if (n)
      *n = 0;
    return NULL;
  }

  Get(m);

  Typecheck(st_STRING);

  r = new char[m];

  f->Read(r, m);
  if (n)
    *n = m;

  return r;
}

wxMediaStreamIn& wxMediaStreamIn::Get(long *n, char *str)
{
  long m;

  if (bad) {
    *n = 0;
    return *this;
  }

  Get(m);

  Typecheck(st_STRING);

  if (m <= *n)
    f->Read(str, m);
  else {
    f->Read(str, *n);
    f->Skip(m - *n);
  }
  *n = m;

  return *this;
}

wxMediaStreamIn& wxMediaStreamIn::Get(long &v)
{
  char b;
  

  Typecheck(st_NUMBER);

  if (bad) {
    v = 0;
    return *this;
  }

  f->Read((char *)&b, sizeof(char));
  if (b & 0x80) {
    if (b & 0x40) {
      if (b & 0x1) {
	signed char bv;
	f->Read((char *)&bv, 1);
	v = bv;
      } else if (b & 0x2) {
	unsigned char bl[2];
	f->Read((char *)bl, 2);
	v = (((int)((signed char *)bl)[0]) << 8) + bl[1];
      } else {
	unsigned char bl[4];
	f->Read((char *)bl, 4);
	v = (((long)((signed char *)bl)[0]) << 24) 
	  + (((long)bl[1]) << 16)
	  + (((long)bl[2]) << 8) + bl[3];
      }
    } else {
      unsigned char b2;
      f->Read((char *)&b2, sizeof(char));
      v = (((int)(b & 0x3F)) << 8) | b2;
    }
  } else
    v = b;

  return *this;
}

wxMediaStreamIn& wxMediaStreamIn::Get(short &v)
{
  long lv;

  Get(lv);
  v = lv;

  return *this;
}

wxMediaStreamIn& wxMediaStreamIn::Get(int &v)
{
  long lv;

  Get(lv);
  v = lv;

  return *this;
}

wxMediaStreamIn& wxMediaStreamIn::Get(char &v)
{
  long lv;

  Get(lv);
  v = lv;

  return *this;
}

wxMediaStreamIn& wxMediaStreamIn::Get(double &v)
{
  Typecheck(st_FLOAT);

  if (bad) {
    v = 0.0;
    return *this;
  }

#if !WXME_LSB_FIRST
  f->Read((char *)&v, sizeof(double));
#else
  if (WXME_VERSION_ONE())
    f->Read((char *)&v, sizeof(double));
  else {
    char num[sizeof(double)], num2[sizeof(double)];
    int i, j;

    f->Read((char *)num, sizeof(double));
    for (i = 0, j = sizeof(double); i < sizeof(double); )
      num2[i++] = num[--j];

    memcpy((char *)&v, num2, sizeof(double));
  }
#endif

  return *this;
}

wxMediaStreamIn& wxMediaStreamIn::Get(float &v)
{
  double lv;

  Get(lv);
  v = lv;

  return *this;
}

wxMediaStreamIn& wxMediaStreamIn::operator>>(long &v) { return Get(v); }
wxMediaStreamIn& wxMediaStreamIn::operator>>(short &v) { return Get(v); }
wxMediaStreamIn& wxMediaStreamIn::operator>>(int &v) { return Get(v); }
wxMediaStreamIn& wxMediaStreamIn::operator>>(char &v) { return Get(v); }
wxMediaStreamIn& wxMediaStreamIn::operator>>(float &v) { return Get(v); }
wxMediaStreamIn& wxMediaStreamIn::operator>>(double &v) { return Get(v); }

void wxMediaStreamIn::SetBoundary(long n)
{
  if (boundcount == boundalloc) {
    long *old = boundaries;
    boundalloc *= 2;
    boundaries = new long[boundalloc];
    memcpy(boundaries, old, boundcount * sizeof(long));
    delete[] old;
  }

  boundaries[boundcount++] = f->Tell() + n;
}

void wxMediaStreamIn::RemoveBoundary()
{
  --boundcount;
}

void wxMediaStreamIn::Skip(long n)
{
  f->Skip(n);
}

long wxMediaStreamIn::Tell(void)
{
  return f->Tell();
}

void wxMediaStreamIn::JumpTo(long pos)
{
  f->Seek(pos);
}

Bool wxMediaStreamIn::Ok(void)
{
  return !bad;
}

/*********************************************************************/

wxMediaStreamOut::wxMediaStreamOut(wxMediaStreamOutBase& s)
{
  f = &s;
  bad = FALSE;
}

void wxMediaStreamOut::Typeset(char v)
{
  if (bad)
    return;

  bad = f->Bad();

  if (bad) {
    wxMessageBox("Unknown stream error.", "Error");
    return;
  }

#if TYPESAFE
  f->Write(&v, 1);
#endif
}

wxMediaStreamOut& wxMediaStreamOut::PutFixed(long v)
{
  Typeset(st_FIXED);

#if !WXME_LSB_FIRST
  f->Write((char *)&v, sizeof(long));
#else
  char lb[4];

  lb[0] = (v >> 24) & 0xFF;
  lb[1] = (v >> 16) & 0xFF;
  lb[2] = (v >> 8) & 0xFF;
  lb[3] = v & 0xFF;
  f->Write(lb, 4);
#endif

  return *this;
}

wxMediaStreamOut& wxMediaStreamOut::Put(long n, char *str)
{
  Put(n);

  Typeset(st_STRING);

  f->Write(str, n);

  return *this;
}

wxMediaStreamOut& wxMediaStreamOut::Put(char *v)
{
  return Put(strlen(v) + 1, v);
}

wxMediaStreamOut& wxMediaStreamOut::Put(long v)
{
  Typeset(st_NUMBER);

  if (v >= 0) {
    if (v <= 0x7F) {
      char b = v;
      f->Write(&b, 1);
    } else if (v <= 0x1FFF) {
      unsigned char b[2];
      b[0] = (v >> 8) | 0x80;
      b[1] = v & 0xFF;
      f->Write((char *)b, 2);
    } else {
      char markb = 0xC0;
      unsigned char lb[4];
      lb[0] = (v >> 24) & 0xFF;
      lb[1] = (v >> 16) & 0xFF;
      lb[2] = (v >> 8) & 0xFF;
      lb[3] = v & 0xFF;
      f->Write(&markb, 1);
      f->Write((char *)lb, 4);
    }
  } else {
    char b = 0xC0;
    if (v > ((signed char)0x80)) {
      signed char b2 = v;
      b |= 0x1;
      f->Write(&b, 1);
      f->Write((char *)&b2, 1);
    } else {
      unsigned char lb[4];
      f->Write(&b, sizeof(char));
      ((signed char *)lb)[0] = (v >> 24) & 0xFF;
      lb[1] = (v >> 16) & 0xFF;
      lb[2] = (v >> 8) & 0xFF;
      lb[3] = v & 0xFF;
      f->Write((char *)lb, 4);
    }
  }

  return *this;
}

wxMediaStreamOut& wxMediaStreamOut::Put(short v)
{
  return Put((long)v);
}

wxMediaStreamOut& wxMediaStreamOut::Put(int v)
{
  return Put((long)v);
}

wxMediaStreamOut& wxMediaStreamOut::Put(char v)
{
  return Put((long)v);
}

wxMediaStreamOut& wxMediaStreamOut::Put(double v)
{
  Typeset(st_FLOAT);

#if !WXME_LSB_FIRST
  f->Write((char *)&v, sizeof(double));
#else
  char num[sizeof(double)], num2[sizeof(double)];
  int i, j;

  memcpy(num2, (char *)&v, sizeof(double));
  for (i = 0, j = sizeof(double); i < sizeof(double); )
    num[i++] = num2[--j];
  
  f->Write((char *)num, sizeof(double));
#endif

  return *this;
}

wxMediaStreamOut& wxMediaStreamOut::Put(float v)
{
  return Put((double)v);
}

wxMediaStreamOut& wxMediaStreamOut::operator<<(char *str) { return Put(str); }
wxMediaStreamOut& wxMediaStreamOut::operator<<(long v) {return Put(v); }
wxMediaStreamOut& wxMediaStreamOut::operator<<(short v) {return Put(v); }
wxMediaStreamOut& wxMediaStreamOut::operator<<(int v) {return Put(v); }
wxMediaStreamOut& wxMediaStreamOut::operator<<(char v) {return Put(v); }
wxMediaStreamOut& wxMediaStreamOut::operator<<(float v) {return Put(v); }
wxMediaStreamOut& wxMediaStreamOut::operator<<(double v) {return Put(v); }

long wxMediaStreamOut::Tell(void)
{
  return f->Tell();
}

void wxMediaStreamOut::JumpTo(long pos)
{
  f->Seek(pos);
}

Bool wxMediaStreamOut::Ok(void)
{
  return !bad;
}
