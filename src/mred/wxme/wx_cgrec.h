
/* WARNING: These classes are not garbage-collected, except in precise GC mode. */

#ifdef MZ_PRECISE_GC
# define CGREC_COLLECTED
#endif

class wxcgList;

class wxChangeRecord 
{
 public:
  wxChangeRecord(void);
  virtual ~wxChangeRecord();
  virtual Bool Undo(wxMediaBuffer *media);
  virtual void DropSetUnmodified(void);
};

class wxSchemeModifyRecord : public wxChangeRecord
{
  void *p;
 public:
  wxSchemeModifyRecord(void *p);
  Bool Undo(wxMediaBuffer *media);
};

class wxUnmodifyRecord : public wxChangeRecord
{
  int ok;
 public:
  wxUnmodifyRecord(void);
  Bool Undo(wxMediaBuffer *media);
  void DropSetUnmodified(void);
};

class wxInsertRecord : public wxChangeRecord
{
 private:
  long start, end;
  Bool continued;

 public:
  wxInsertRecord(long position, long length, Bool cont);

  Bool Undo(wxMediaBuffer *media);
};

class wxInsertSnipRecord : public wxChangeRecord
{
 private:
  wxSnip *snip;
  Bool continued;

 public:
  wxInsertSnipRecord(wxSnip *s, Bool cont);

  Bool Undo(wxMediaBuffer *media);
};

class wxDeleteRecord : public wxChangeRecord
{
 private:
  Bool continued;
  long start, end;
  wxcgList *deletions;
  wxcgList *clickbacks;
  Bool undid;

 public:
  wxDeleteRecord(long start, long end, Bool cont);
  ~wxDeleteRecord();

  void InsertSnip(wxSnip *);
  void AddClickback(wxClickback *);

  Bool Undo(wxMediaBuffer *media);
};

class wxDeleteSnipRecord : public wxChangeRecord
{
 private:
  Bool continued;
  wxcgList *deletions;

 public:
  Bool undid;

  wxDeleteSnipRecord(Bool cont);
  ~wxDeleteSnipRecord();

  void InsertSnip(wxSnip *, wxSnip *, float, float);

  Bool Undo(wxMediaBuffer *media);
};

class wxStyleChangeRecord : public wxChangeRecord
{
 private:
  Bool continued;
  long start, end;
  wxcgList *changes;

 public:
  wxStyleChangeRecord(long start, long end, Bool cont);
  ~wxStyleChangeRecord();

  void AddStyleChange(long start, long end, wxStyle *style);
  Bool Undo(wxMediaBuffer *media);
};

class wxStyleChangeSnipRecord : public wxChangeRecord
{
 private:
  Bool continued;
  wxcgList *changes;

 public:
  wxStyleChangeSnipRecord(Bool cont);
  ~wxStyleChangeSnipRecord();

  void AddStyleChange(wxSnip *, wxStyle *style);
  Bool Undo(wxMediaBuffer *media);
};

class wxMoveSnipRecord : public wxChangeRecord
{
 private:
  Bool continued;
  wxSnip *snip;
  float x, y;
  Bool delta;
 public:
  wxMoveSnipRecord(wxSnip *snip, float x, float y, Bool delta, Bool cont);
  Bool Undo(wxMediaBuffer *media);
};

class wxResizeSnipRecord : public wxChangeRecord
{
 private:
  Bool continued;
  wxSnip *snip;
  float x, y;
 public:
  wxResizeSnipRecord(wxSnip *snip, float x, float y, Bool cont);
  Bool Undo(wxMediaBuffer *media);
};

