
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

typedef struct MrEdContextFrames {
  wxChildList *list;
  struct MrEdContextFrames *next, *prev;
} MrEdContextFrames;

typedef struct MrEdFinalizedContext {
#ifdef wx_xt
  Widget toplevel;
#endif
  MrEdContextFrames *frames;
  struct MrEdContext **real_context; /* atomic ptr to actual context */
} MrEdFinalizedContext;

typedef struct MrEdContext {
  Scheme_Type type;

  Scheme_Process *handler_running;

  MrEdFinalizedContext *finalized;

  wxChildList *topLevelWindowList;
  wxStandardSnipClassList *snipClassList;
  wxBufferDataClassList *bufferDataClassList;
  wxWindow *modal_window;

  Scheme_Config *main_config;

  short ready_to_go;

  short ready, waiting_for_nested, waiting_a_little;
  short sema_callback;
  wxTimer *timer;
  MrEdEvent event;

  /* Alternate condition for nested event loop pending some condition */
  int (*alternate)(void *);
  void *alt_data;

  /* Save current directory across thread creations: */
  char *wd;
  int wdlen; /* size of buffer, not name */

  /* Used to chain active contexts while reading events: */
  struct MrEdContext *next;

  int busyState;

  Scheme_Manager_Reference *mref;
} MrEdContext;

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

