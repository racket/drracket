/* Main code for MzScheme */

#include <PalmOS.h>
#include "resnum.h"
#include "mzpalm.h"

#include "segmap.h"

void GC_set_stack_base(void *base) SEGOF_GC_set_stack_base; 
void *scheme_basic_env(void) SEGOF_scheme_basic_env;
void *scheme_eval_string(char *s, void *env) SEGOF_scheme_eval_string;

static void StartScheme(void *addr)
{
  void *env;

  if (!setjmp(&exit_buf)) {
    env = scheme_basic_env();
    scheme_eval_string("(display 'hello-world)", env);
  }
}

static Boolean MainFormHandleEvent(EventPtr e)
{
  Boolean handled = false;
  FormPtr frm;
    
  switch (e->eType) {
  case frmOpenEvent:
    frm = FrmGetActiveForm();
    FrmDrawForm(frm);
    handled = true;

    StartScheme(&frm);

    break;

  case menuEvent:
    MenuEraseStatus(NULL);

    switch(e->data.menu.itemID) {
    }

    handled = true;
    break;

  case ctlSelectEvent:
    switch(e->data.ctlSelect.controlID) {
    }
    break;

  default:
    break;
  }

  return handled;
}

static Boolean ApplicationHandleEvent(EventPtr e)
{
  FormPtr frm;
  Int16   formId;
  Boolean handled = false;

  if (e->eType == frmLoadEvent) {
    formId = e->data.frmLoad.formID;
    frm = FrmInitForm(formId);
    FrmSetActiveForm(frm);

    switch(formId) {
    case MainForm:
      FrmSetEventHandler(frm, MainFormHandleEvent);
      break;
    }
    handled = true;
  }

  return handled;
}

/* Get preferences, open (or create) app database */
static Int16 StartApplication(void)
{
  FrmGotoForm(MainForm);

  return 0;
}

/* Save preferences, close forms, close app database */
static void StopApplication(void)
{
  FrmSaveAllForms();
  FrmCloseAllForms();
}

/* The main event loop */
static void EventLoop(void)
{
  Int16 err;
  EventType e;

  do {
    EvtGetEvent(&e, evtWaitForever);
    if (! SysHandleEvent (&e))
      if (! MenuHandleEvent (NULL, &e, &err))
	if (! ApplicationHandleEvent (&e))
	  FrmDispatchEvent (&e);
  } while (e.eType != appStopEvent);
}

/* Main entry point */
UInt32 PilotMain(UInt16 cmd, void *cmdPBP, UInt16 launchFlags)
{
  Int16 err;

  if (cmd == sysAppLaunchCmdNormalLaunch) {

    GC_set_stack_base(&err);

    err = StartApplication();
    if (err) return err;

    EventLoop();
    StopApplication();

  } else {
    return sysErrParamErr;
  }

  return 0;
}
