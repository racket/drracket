
#include "wx_mline.h"

#define MAX_COUNT_FOR_SNIP 500

enum {
  wxSTREAK_EXCEPT_DELAYED = 1,
  wxSTREAK_KEY_SEQUENCE
  };

class wxClickback : public wxObject
{
 public:
  long start, end;
  wxClickbackFunc f;
  void *data;
  Bool callOnDown;

  wxStyleDelta *delta;

  Bool hilited;
  wxList *unhilite;
};

#include "wx_timer.h"

class wxMediaFlashTimer : public wxTimer
{
 public:
  wxMediaEdit *media;
  void Notify(void);
};

#if VERSION_F
#include "wx_clipb.h"
#else
#include "wx_gclip.h"
#endif

#include "wx_ptreq.h"
