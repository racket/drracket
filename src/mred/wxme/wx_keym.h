
#ifndef wx_keym
#define wx_keym

#include "wx_stdev.h"
#include "wx_hash.h"

class wxObject;

typedef Bool (*wxKeyFunction)(wxObject *media, wxKeyEvent &event, 
			      void *data);
typedef Bool (*wxMouseFunction)(wxObject *media, wxMouseEvent &event, 
				void *data);

typedef Bool (*wxGrabKeyFunction)(char *str, class wxKeymap *km,
				  wxObject *media, wxKeyEvent &event, 
				  void *data);
typedef Bool (*wxGrabMouseFunction)(char *str, class wxKeymap *km,
				    wxObject *media, wxMouseEvent &event, 
				    void *data);

typedef void (*wxBreakSequenceFunction)(void *data);

enum {
  wxKEY_FINAL = 0,
  wxKEY_PREFIX
};

typedef void (*wxKeyErrorFunction)(void *data, char *str);

#define WXK_MOUSE_RIGHT         (-1)
#define WXK_MOUSE_LEFT          (-2)
#define WXK_MOUSE_MIDDLE        (-3)
#define WXK_MOUSE_RIGHT_DOUBLE  (-4)
#define WXK_MOUSE_LEFT_DOUBLE   (-5)
#define WXK_MOUSE_MIDDLE_DOUBLE (-6)
#define WXK_MOUSE_RIGHT_TRIPLE  (-7)
#define WXK_MOUSE_LEFT_TRIPLE   (-8)
#define WXK_MOUSE_MIDDLE_TRIPLE (-9)

#define WXK_CLICK_ADDER         (-3)

class wxKeymap : public wxObject
{
  wxHashTable *keyfunctions, *mousefunctions;
  wxHashTable *keys;
  wxKeyErrorFunction err;
  void *errdata;

  long numImpliedShifts, allocedImplies;
  long *impliesShift;
  int usage;

  int chainCount;
  wxKeymap **chainTo;

  long doubleInterval;

  int lastButton;
  int clickCount;
  long lastTime;
  float lastX, lastY;

  class wxKeycode *prefix;

  char *active_mouse_function;

  wxGrabKeyFunction grabKeyFunction;
  void *grabKeyData;
  wxGrabMouseFunction grabMouseFunction;
  void *grabMouseData;

  wxBreakSequenceFunction onBreak;
  void *onBreakData;

  class wxKeycode *FindKey(long, Bool, Bool, Bool, Bool, class wxKeycode *);
  int HandleEvent(long code, Bool shift, Bool ctrl, Bool alt, Bool meta,
		  char **fname);

  Bool CycleCheck(wxKeymap *km);

  int ChainHandleKeyEvent(wxObject *media, wxKeyEvent &event,
			  wxGrabKeyFunction grab, void *grabData,
			  int try_state);
  int ChainHandleMouseEvent(wxObject *media, wxMouseEvent &event,
			    wxGrabMouseFunction grab, void *grabData,
			    int try_state);

  int OtherHandleKeyEvent(wxObject *media, wxKeyEvent &event,
			  wxGrabKeyFunction grab, void *grabData,
			  int try_state);
  int OtherHandleMouseEvent(wxObject *media, wxMouseEvent &event,
			    wxGrabMouseFunction grab, void *grabData,
			    int try_state);

  void Reset(void);

 public:
  wxKeymap();
  ~wxKeymap();
  
  virtual Bool HandleKeyEvent(wxObject *media, wxKeyEvent &event);
  virtual Bool HandleMouseEvent(wxObject *media, wxMouseEvent &event);

  void SetGrabKeyFunction(wxGrabKeyFunction grab, void *grabData);
  void RemoveGrabKeyFunction(void);
  void SetGrabMouseFunction(wxGrabMouseFunction grab, void *grabData);
  void RemoveGrabMouseFunction(void);

  void BreakSequence(void);
  void SetBreakSequenceCallback(wxBreakSequenceFunction f, void *data);

  class wxKeycode *MapFunction(long code, Bool shift, Bool ctrl, 
			       Bool alt, Bool meta, 
			       char *fname, class wxKeycode *prevkey=NULL, 
			       int keytype = wxKEY_FINAL);
  void MapFunction(char *keyname, char *fname);

  void ImpliesShift(char *str);

  void AddKeyFunction(char *name, wxKeyFunction func, void *data);
  void AddMouseFunction(char *name, wxMouseFunction func, void *data);
  Bool CallFunction(char *name, wxObject *media, wxKeyEvent &event, 
		    Bool try_chained = FALSE);
  Bool CallFunction(char *name, wxObject *media, wxMouseEvent &event, 
		    Bool try_chained = FALSE);

  void SetErrorCallback(wxKeyErrorFunction, void *d=NULL);

  void ChainToKeymap(wxKeymap *, Bool prefix);
  void RemoveChainedKeymap(wxKeymap *);

  void AdjustUsage(Bool newUser);
  Bool IsUsed(void);

  long GetDoubleClickInterval();
  void SetDoubleClickInterval(long);
};

#endif
