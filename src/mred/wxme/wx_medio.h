/* Header file for wxMediaStream */

#ifndef wx_medio
#define wx_medio

#include <stdio.h>

class wxMediaStreamInBase : public wxObject
{
 public:
  virtual long Tell(void) = 0;
  virtual void Seek(long) = 0;
  virtual void Skip(long) = 0;
  virtual Bool Bad(void) = 0;
  virtual long Read(char *data, long len) = 0;
};

class wxMediaStreamOutBase : public wxObject
{
 public:
  virtual long Tell(void) = 0;
  virtual void Seek(long) = 0;
  virtual Bool Bad(void) = 0;
  virtual void Write(char *data, long len) = 0;
};

/*******************************************************************/

class wxMediaStreamInFileBase : public wxMediaStreamInBase
{
  FILE *f;
 public:
  wxMediaStreamInFileBase(FILE *s);
  ~wxMediaStreamInFileBase();

  long Tell(void);
  void Seek(long);
  void Skip(long);
  Bool Bad(void);
  long Read(char *data, long len);
};

class wxMediaStreamOutFileBase : public wxMediaStreamOutBase
{
  FILE *f;
 public:
  wxMediaStreamOutFileBase(FILE *s);
  ~wxMediaStreamOutFileBase();

  long Tell(void);
  void Seek(long);
  Bool Bad(void);
  void Write(char *data, long len);
};

/*******************************************************************/

class wxMediaStreamInStringBase : public wxMediaStreamInBase
{
  char *string;
  long len;
  long pos;
  Bool bad;
 public:
  wxMediaStreamInStringBase(char *s, long len);
  ~wxMediaStreamInStringBase();

  long Tell(void);
  void Seek(long);
  void Skip(long);
  Bool Bad(void);
  long Read(char *data, long len);
};

class wxMediaStreamOutStringBase : public wxMediaStreamOutBase
{
  char *string;
  long len, alloc;
  long pos;
  Bool bad;
 public:
  wxMediaStreamOutStringBase();
  ~wxMediaStreamOutStringBase();

  char *GetString(long *len);

  long Tell(void);
  void Seek(long);
  Bool Bad(void);
  void Write(char *data, long len);
};

/*******************************************************************/

#define MRED_START_STR "WXME"
#define MRED_START_STR_LEN 4
#define MRED_FORMAT_STR "01"
#define MRED_FORMAT_STR_LEN 2
#define MRED_VERSION_STR "03"
#define MRED_VERSION_STR_LEN 2

#define WXME_VERSION_ONE(f) (f->read_version[1] == '1')
#define WXME_VERSION_TWO(f) (f->read_version[1] == '2')

class wxStandardSnipClassList;
class wxBufferDataClassList;
class wxSnipClass;
class wxBufferDataClass;
class wxStyleList;
class wxStyle;

class wxSnipClassLink
{
 public:
  wxSnipClass *c;
  char *name;
  Bool headerFlag;
  short mapPosition;
  int readingVersion;
  wxSnipClassLink *next;
};

class wxDataClassLink
{
 public:
  wxBufferDataClass *d;
  char *name;
  int mapPosition;
  wxDataClassLink *next;
};

class wxStyleListLink
{
 public:
  wxStyleList *styleList;
  int listId;
  wxStyle **styleMap;
  int numMappedStyles;
  wxStyle *basic; /* used to detect clearing */

  wxStyleListLink *next;
};

class wxMediaStream : public wxObject
{
 public:
  wxStandardSnipClassList *scl;
  wxBufferDataClassList *bdl;
  char read_format[MRED_FORMAT_STR_LEN + 1];
  char read_version[MRED_VERSION_STR_LEN + 1];
  wxSnipClassLink *sl;
  wxDataClassLink *dl;

  wxStyleListLink *ssl;
  int styleCount;

  wxMediaStream();
  ~wxMediaStream();

  int MapPosition(wxSnipClass *c);
  int MapPosition(wxBufferDataClass *d);

  int GetHeaderFlag(wxSnipClass *c);
  void SetHeaderFlag(wxSnipClass *c);

  int ReadingVersion(wxSnipClass *c);
};

/*******************************************************************/

class wxMediaStreamIn : public wxMediaStream
{
  wxMediaStreamInBase *f;
  long *boundaries;
  int boundalloc, boundcount;
  int bad;

  void Typecheck(char);

 public:
  wxMediaStreamIn(wxMediaStreamInBase *base);
  ~wxMediaStreamIn();
  
  wxMediaStreamIn* Get(long *n, char *str);
  wxMediaStreamIn* Get(long*);
  wxMediaStreamIn* Get(short*);
  wxMediaStreamIn* Get(int*);
  wxMediaStreamIn* Get(char*);
  wxMediaStreamIn* Get(float*);
  wxMediaStreamIn* Get(double*);

  wxMediaStreamIn* GetFixed(long*);

  char *GetString(long *n);

  void SetBoundary(long n);
  void RemoveBoundary();

  void Skip(long n);
  long Tell(void);
  void JumpTo(long pos);

  Bool Ok(void);
};

class wxMediaStreamOut : public wxMediaStream
{
  wxMediaStreamOutBase *f;
  int bad;

  void Typeset(char);

 public:
  wxMediaStreamOut(wxMediaStreamOutBase *s);
  
  wxMediaStreamOut* Put(long n, char *str);
  wxMediaStreamOut* Put(char *);
  wxMediaStreamOut* Put(long);
  wxMediaStreamOut* Put(short);
  wxMediaStreamOut* Put(int);
  wxMediaStreamOut* Put(char);
  wxMediaStreamOut* Put(float);
  wxMediaStreamOut* Put(double);

  wxMediaStreamOut* PutFixed(long);

  long Tell(void);
  void JumpTo(long pos);

  Bool Ok(void);
};

#endif /* wx_medio */

