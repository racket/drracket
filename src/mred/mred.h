
#define BREAKING_REQUIRES_SHIFT 1

#ifdef wx_x
typedef XEvent MrEdEvent;
#else
#ifdef wx_msw
typedef MSG MrEdEvent;
#else
typedef EventRecord MrEdEvent;
#endif
#endif

class wxTimer;

class MrEdContextFrames {
 public:
  wxChildList *list;
  MrEdContextFrames *next, *prev;
};

class MrEdFinalizedContext;
class MrEd_Saved_Modal;

typedef struct MrEdContext {
  Scheme_Type type;
  MZ_HASH_KEY_EX

  Scheme_Process *handler_running;
  int suspended;

  MrEdFinalizedContext *finalized;

  wxChildList *topLevelWindowList;
  wxStandardSnipClassList *snipClassList;
  wxBufferDataClassList *bufferDataClassList;
  wxWindow *modal_window;
  MrEd_Saved_Modal *modal_stack;

  Scheme_Config *main_config;

  short ready_to_go;

  short ready, waiting_for_nested;
  short q_callback;
  wxTimer *timer;
  MrEdEvent event;

  /* Alternate condition for nested event loop pending some condition */
  int (*alternate)(void *);
  void *alt_data;

  /* Used to chain active contexts while reading events: */
  struct MrEdContext *next;

  int busyState;
  int killed;

#ifdef wx_msw
  struct LeaveEvent *queued_leaves;
#endif

  struct Context_Manager_Hop *mr_hop;
  Scheme_Manager_Reference *mref;
} MrEdContext;

class MrEdFinalizedContext {
 public:
#ifdef wx_xt
  Widget toplevel;
#endif
  MrEdContextFrames *frames;
  MrEdContext **real_context; /* atomic ptr to actual context */
};

extern MrEdContext *mred_contexts;

MrEdContext *MrEdGetContext(wxObject *w = NULL);

int MrEdGetNextEvent(int peek, int current_only, MrEdEvent *, MrEdContext **);
void MrEdDispatchEvent(MrEdEvent *);

void MrEdInitFirstContext(MrEdContext *c);
void MrEdInitNewContext(MrEdContext *c);
void MrEdDestroyContext(MrEdFinalizedContext *c);

#ifdef wx_msw
void MrEdMSWSleep(float secs, void *fds);
#endif

#ifdef wx_mac
void MrEdMacSleep(float secs);
#endif

typedef void *(*ForEachFrameProc)(wxObject *, void *);
void *MrEdForEachFrame(ForEachFrameProc fp, void *data);
