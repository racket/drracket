
#include <stdio.h>

#include "wx_dialg.h"
#ifndef OLD_WXWINDOWS
// wxWindows 1.62(f): new include for common dialogs
#include "wx_cmdlg.h"
#endif

#include "wx_media.h"
#include <string.h>

static int lsb_first;

/* For testing and debugging: */
#define TYPESAFE 0

extern void wxmeError(const char *e);

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

  lsb_first = *(char *)&v;
}

/****************************************************************/

wxMediaStream::wxMediaStream()
{
  wxStandardSnipClassList *_scl;
  wxBufferDataClassList *_bdl;

  _scl = &wxTheSnipClassList;
  scl = _scl;
  _bdl = &wxTheBufferDataClassList;
  bdl = _bdl;
}

wxMediaStream::~wxMediaStream()
{
}

int wxMediaStream::ReadingVersion(wxSnipClass *sclass)
{
  wxSnipClassLink *asl;
  
  for (asl = sl; asl; asl = asl->next) {
    if (asl->c == sclass)
      return asl->readingVersion;
  }

  return 0;
}

int wxMediaStream::MapPosition(wxSnipClass *c)
{
  wxSnipClassLink *asl;
  
  for (asl = sl; asl; asl = asl->next) {
    if (asl->c == c)
      return asl->mapPosition;
  }

  return -1;
}

int wxMediaStream::MapPosition(wxBufferDataClass *d)
{
  wxDataClassLink *adl;
  
  for (adl = dl; adl; adl = adl->next) {
    if (adl->d == d)
      return adl->mapPosition;
  }

  return -1;
}

int wxMediaStream::GetHeaderFlag(wxSnipClass *c)
{
  wxSnipClassLink *asl;
  
  for (asl = sl; asl; asl = asl->next) {
    if (asl->c == c)
      return asl->headerFlag;
  }

  return 0;
}

void wxMediaStream::SetHeaderFlag(wxSnipClass *c)
{
  wxSnipClassLink *asl;
  
  for (asl = sl; asl; asl = asl->next) {
    if (asl->c == c) {
      asl->headerFlag = 1;
      return;
    }
  }  
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

    alloc = (alloc * 2) + l;
    string = new char[alloc];
    memcpy(string, old, len);
  }

  memcpy(string + pos, data, l);
  pos += l;
  if (len < pos)
    len = pos;
}

/****************************************************************/

wxMediaStreamIn::wxMediaStreamIn(wxMediaStreamInBase *s)
{
  f = s;
  boundalloc = 10;
  boundcount = 0;
  boundaries = new long[boundalloc];
  bad = FALSE;
}

wxMediaStreamIn::~wxMediaStreamIn()
{
}

void wxMediaStreamIn::Typecheck(char v)
{
  if (bad)
    return;

  if (boundcount && (f->Tell() >= boundaries[boundcount - 1])) {
    bad = TRUE;
    wxmeError("Overread caused by file corruption"
	      " or unknown internal error.");
    return;
  }

  bad = f->Bad();

  if (bad) {
    wxmeError("Unknown stream error.");
    return;
  }

#if TYPESAFE
  char t;

  f->Read(&t, 1);

  if (bad = f->Bad()) {
    wxmeError("Unknown stream error.");
    return;
  }

  bad = (t != v);

  if (bad)
    wxmeError("Type safety error.");
#endif
}

wxMediaStreamIn *wxMediaStreamIn::GetFixed(long *v)
{
  Typecheck(st_FIXED);

  if (bad) {
    *v = 0;
    return this;
  }

  if (!lsb_first) {
    if (f->Read((char *)v, sizeof(long)) != sizeof(long)) {
      *v = 0;
      bad = 1;
    }
  } else {
    if (WXME_VERSION_ONE(this)) {
      if (f->Read((char *)v, sizeof(long)) != sizeof(long)) {
	bad = 1;
	*v = 0;
      }
    } else {
      unsigned char bl[4];
      
      if (f->Read((char *)bl, 4) != 4) {
	bad = 1;
	*v = 0;
      } else {
	*v = ((((long)bl[0]) << 24) + (((long)bl[1]) << 16)
	      + (((long)bl[2]) << 8) + bl[3]);
      }
    }
  }

  return this;
}

extern void *wxMallocAtomicIfPossible(size_t s);

char *wxMediaStreamIn::GetString(long *n)
{
  long m;
  char *r;

  if (bad) {
    if (n)
      *n = 0;
    return NULL;
  }

  Get(&m);

  Typecheck(st_STRING);

  r = (char *)wxMallocAtomicIfPossible(m);
  if (!r) {
    wxmeError("String too large (out of memory) reading stream.");
    bad = 1;
    if (n)
      *n = 0;
    return NULL;
  }

  if (f->Read(r, m) != m) {
    bad = 1;
    m = 0;
  }
  if (n)
    *n = m;

  return r;
}

wxMediaStreamIn *wxMediaStreamIn::Get(long *n, char *str)
{
  long m;

  if (bad) {
    *n = 0;
    return this;
  }

  Get(&m);

  Typecheck(st_STRING);

  if (m <= *n) {
    if (f->Read(str, m) != m) {
      bad = 1;
      m = 0;
    }
  } else {
    int d;
    d = f->Read(str, *n);
    if (d != *n) {
      bad = 1;
      m = 0;
    } else {
      f->Skip(m - *n);
    }
  }
  *n = m;

  return this;
}

wxMediaStreamIn* wxMediaStreamIn::Get(long *v)
{
  char b;
  
  Typecheck(st_NUMBER);

  if (bad) {
    *v = 0;
    return this;
  }

  if (f->Read((char *)&b, sizeof(char)) != sizeof(char)) {
    bad = 1;
    b = 0;
  }

  if (b & 0x80) {
    if (b & 0x40) {
      if (b & 0x1) {
	signed char bv;
	if (f->Read((char *)&bv, 1) != 1) {
	  bad = 1;
	  *v = 0;
	} else
	  *v = bv;
      } else if (b & 0x2) {
	unsigned char bl[2];
	if (f->Read((char *)bl, 2) != 2) {
	  bad = 1;
	  *v = 0;
	} else
	  *v = (((int)((signed char *)bl)[0]) << 8) + bl[1];
      } else {
	unsigned char bl[4];
	if (f->Read((char *)bl, 4) != 4) {
	  bad = 1;
	  *v = 0;
	} else
	  *v = (((long)((signed char *)bl)[0]) << 24) 
	    + (((long)bl[1]) << 16)
	    + (((long)bl[2]) << 8) + bl[3];
      }
    } else {
      unsigned char b2;
      if (f->Read((char *)&b2, sizeof(char)) != sizeof(char)) {
	bad = 1;
	*v = 0;
      } else
	*v = (((int)(b & 0x3F)) << 8) | b2;
    }
  } else
    *v = b;

  return this;
}

wxMediaStreamIn* wxMediaStreamIn::Get(short *v)
{
  long lv;

  Get(&lv);
  *v = lv;

  return this;
}

wxMediaStreamIn* wxMediaStreamIn::Get(int *v)
{
  long lv;

  Get(&lv);
  *v = lv;

  return this;
}

wxMediaStreamIn* wxMediaStreamIn::Get(char *v)
{
  long lv;

  Get(&lv);
  *v = lv;

  return this;
}

wxMediaStreamIn *wxMediaStreamIn::Get(double *v)
{
  Typecheck(st_FLOAT);

  if (bad) {
    *v = 0.0;
    return this;
  }

  if (!lsb_first) {
    if (f->Read((char *)v, sizeof(double)) != sizeof(double)) {
      bad = 1;
      *v = 0.0;
    }
  } else {
    if (WXME_VERSION_ONE(this)) {
      if (f->Read((char *)v, sizeof(double)) != sizeof(double)) {
	bad = 1;
	*v = 0.0;
      }
    } else {
      char num[sizeof(double)], num2[sizeof(double)];
      int i, j;
      
      if (f->Read((char *)num, sizeof(double))  != sizeof(double)) {
	bad = 1;
	*v = 0.0;
      } else {
	for (i = 0, j = sizeof(double); i < (int)sizeof(double); ) {
	  num2[i++] = num[--j];
	}
	
	memcpy((char *)v, num2, sizeof(double));
      }
    }
  }

  return this;
}

wxMediaStreamIn* wxMediaStreamIn::Get(float *v)
{
  double lv;

  Get(&lv);
  *v = lv;

  return this;
}

void wxMediaStreamIn::SetBoundary(long n)
{
  if (boundcount == boundalloc) {
    long *old = boundaries;
    boundalloc *= 2;
    boundaries = new long[boundalloc];
    memcpy(boundaries, old, boundcount * sizeof(long));
  }

  {
    long m;
    m = f->Tell() + n;
    boundaries[boundcount++] = m;
  }
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

wxMediaStreamOut::wxMediaStreamOut(wxMediaStreamOutBase *s)
{
  f = s;
  bad = FALSE;
}

void wxMediaStreamOut::Typeset(char v)
{
  if (bad)
    return;

  bad = f->Bad();

  if (bad) {
    wxmeError("Unknown stream error.");
    return;
  }

#if TYPESAFE
  f->Write(&v, 1);
#endif
}

wxMediaStreamOut *wxMediaStreamOut::PutFixed(long v)
{
  Typeset(st_FIXED);

  if (!lsb_first) {
    f->Write((char *)&v, sizeof(long));
  } else {
    char lb[4];
    
    lb[0] = (v >> 24) & 0xFF;
    lb[1] = (v >> 16) & 0xFF;
    lb[2] = (v >> 8) & 0xFF;
    lb[3] = v & 0xFF;
    f->Write(lb, 4);
  }

  return this;
}

wxMediaStreamOut* wxMediaStreamOut::Put(long n, char *str)
{
  Put(n);

  Typeset(st_STRING);

  f->Write(str, n);

  return this;
}

wxMediaStreamOut *wxMediaStreamOut::Put(char *v)
{
  return Put(strlen(v) + 1, v);
}

wxMediaStreamOut *wxMediaStreamOut::Put(long v)
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

  return this;
}

wxMediaStreamOut* wxMediaStreamOut::Put(short v)
{
  return Put((long)v);
}

wxMediaStreamOut* wxMediaStreamOut::Put(int v)
{
  return Put((long)v);
}

wxMediaStreamOut* wxMediaStreamOut::Put(char v)
{
  return Put((long)v);
}

wxMediaStreamOut* wxMediaStreamOut::Put(double v)
{
  Typeset(st_FLOAT);

  if (!lsb_first) {
    f->Write((char *)&v, sizeof(double));
  } else {
    char num[sizeof(double)], num2[sizeof(double)];
    int i, j;
    
    memcpy(num2, (char *)&v, sizeof(double));
    for (i = 0, j = sizeof(double); i < (int)sizeof(double); ) {
      num[i++] = num2[--j];
    }
    
    f->Write((char *)num, sizeof(double));
  }

  return this;
}

wxMediaStreamOut* wxMediaStreamOut::Put(float v)
{
  return Put((double)v);
}

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
