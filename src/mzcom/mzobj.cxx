// mzobj.cxx : Implementation of CMzObj

#include "resource.h"

#include "stdafx.h"
#include "mzcom.h"
#include "mzobj.h"

static Scheme_Env *env;

static BOOL *pErrorState;
static OLECHAR *wideError;

static HANDLE exitSem;

static Scheme_Object *exn_catching_apply;
static Scheme_Object *exn_p;
static Scheme_Object *exn_message;

static Scheme_Object *_apply_thunk_catch_exceptions(Scheme_Object *f,
						    Scheme_Object **exn) {
  Scheme_Object *v;

  v = _scheme_apply(exn_catching_apply,1,&f);

  /* v is a pair: (cons #t value) or (cons #f exn) */

  if (SCHEME_TRUEP(SCHEME_CAR(v))) {
    return SCHEME_CDR(v);
  }
  else {
    *exn = SCHEME_CDR(v);
    return NULL;
  }
}

static Scheme_Object *extract_exn_message(Scheme_Object *v) {
  if (SCHEME_TRUEP(_scheme_apply(exn_p,1,&v)))
    return _scheme_apply(exn_message,1,&v);
  else
    return NULL; /* Not an exn structure */
}

static Scheme_Object *do_eval(void *s,int,Scheme_Object **) {
  return scheme_eval_string_all((char *)s,env,TRUE);
}

static Scheme_Object *eval_string_or_get_exn_message(char *s) {
  Scheme_Object *v;
  Scheme_Object *exn;

  v = _apply_thunk_catch_exceptions(scheme_make_closed_prim(do_eval,s),&exn);
  /* value */
  if (v) {
    *pErrorState = FALSE;
    return v;
  }

  v = extract_exn_message(exn);
  /* exn */
  if (v) {
    *pErrorState = TRUE;
    return v;
  }

  /* `raise' was called on some arbitrary value */
  return exn;
}

OLECHAR *wideStringFromSchemeObj(Scheme_Object *obj,char *fmt,int fmtlen) {
  char *s;
  OLECHAR *wideString;
  int len;

  s = scheme_format(fmt,fmtlen,1,&obj,NULL);
  len = strlen(s);
  wideString = (OLECHAR *)scheme_malloc((len + 1) * sizeof(OLECHAR));
  MultiByteToWideChar(CP_ACP,(DWORD)0,s,len,wideString,len + 1);
  wideString[len] = '\0';
  return wideString;
}

void exitHandler(int) {
  ReleaseSemaphore(exitSem,1,NULL);
  ExitThread(0);
} 

DWORD WINAPI evalLoop(LPVOID args) {
  UINT len;
  char *narrowInput;
  Scheme_Object *outputObj;
  OLECHAR *outputBuffer;
  mz_jmp_buf saveBuff;
  THREAD_GLOBALS *pTg;
  HANDLE readSem;
  HANDLE writeSem;
  BSTR **ppInput;
  BSTR *pOutput;
  HRESULT *pHr;
  char *wrapper;
  char exeBuff[260];

  // make sure all MzScheme calls in this thread

  GC_use_registered_statics = 1;

  scheme_exit = exitHandler;

  pTg = (THREAD_GLOBALS *)args;

  ppInput = pTg->ppInput;
  pOutput = pTg->pOutput; 
  pHr = pTg->pHr;
  readSem = pTg->readSem;
  writeSem = pTg->writeSem;
  pErrorState = pTg->pErrorState;

  env = scheme_basic_env();

  if (env == NULL) {
    ::MessageBox(NULL,"Can't create Scheme environment","MzCOM",MB_OK);
    ExitThread(0);
  }

  scheme_dont_gc_ptr(env); 

  // set up exception trapping
  
  wrapper = 
    "(#%lambda (thunk) "
    "(#%with-handlers ([#%void (#%lambda (exn) (#%cons #f exn))]) "
    "(#%cons #t (thunk))))";
  
  exn_catching_apply = scheme_eval_string(wrapper,env);
  exn_p = scheme_lookup_global(scheme_intern_symbol("exn?"),env);
  exn_message = scheme_lookup_global(scheme_intern_symbol("exn-message"),env);

  // set up collection paths, based on MzScheme startup

  GetModuleFileName(GetModuleHandle("mzcom.exe"),exeBuff,sizeof(exeBuff));

  scheme_add_global("mzcom-exe",scheme_make_string(exeBuff),env);

  scheme_eval_string("(#%current-library-collection-paths "
		     "(#%path-list-string->path-list "
		     "(#%or (#%getenv \"PLTCOLLECTS\") \"\") "
		     "(#%or "
		     "(#%ormap "
		     "(#%lambda (f) (#%let ([p (f)]) "
		     "(#%and p (#%directory-exists? p) (#%list p)))) "
		     "(#%list"
		     "(#%lambda () (#%let ((v (#%getenv \"PLTHOME\"))) "
		     "(#%and v (#%build-path v \"collects\")))) "
		     "(#%lambda () (#%find-executable-path mzcom-exe \"..\")) "
		     "(#%lambda () \"c:\\plt\\collects\") "
		     ")) #%null)))",
		     env);

  while (1) {

    WaitForSingleObject(readSem,INFINITE);

    len = SysStringLen(**ppInput);

    narrowInput = (char *)scheme_malloc(len + 1);

    scheme_dont_gc_ptr(narrowInput); 

    WideCharToMultiByte(CP_ACP,(DWORD)0,
			**ppInput,len,
			narrowInput,len + 1,
			NULL,NULL);

    narrowInput[len] = '\0';

    outputObj = eval_string_or_get_exn_message(narrowInput);

    scheme_gc_ptr_ok(narrowInput); 

    if (*pErrorState) {
      wideError = wideStringFromSchemeObj(outputObj,"MzScheme error: ~a",18);
      *pOutput = SysAllocString(L"");
      *pHr = E_FAIL;
    }
    else {
      outputBuffer = wideStringFromSchemeObj(outputObj,"~s",2);
      *pOutput = SysAllocString(outputBuffer);
      *pHr = S_OK;
    }

    memcpy(&scheme_error_buf,&saveBuff,sizeof(mz_jmp_buf));

    ReleaseSemaphore(writeSem,1,NULL);

  }

  return 0;
}

void CMzObj::startMzThread(void) {
  static THREAD_GLOBALS tg;

  tg.pHr = &hr;
  tg.ppInput = &globInput;
  tg.pOutput = &globOutput;
  tg.readSem = readSem;
  tg.writeSem = writeSem;
  tg.pErrorState = &errorState;

  threadHandle = CreateThread(NULL,0,evalLoop,(LPVOID)&tg,0,&threadId);
}


CMzObj::CMzObj(void) {

  inputMutex = NULL;
  readSem = NULL;
  threadId = NULL;
  threadHandle = NULL;

  inputMutex = CreateSemaphore(NULL,1,1,NULL);
  if (inputMutex == NULL) {
    MessageBox(NULL,"Can't create input mutex","MzCOM",MB_OK);
    return;
  }

  readSem = CreateSemaphore(NULL,0,1,NULL);

  if (readSem == NULL) {
    MessageBox(NULL,"Can't create read semaphore","MzCOM",MB_OK);
    return; 
  }

  writeSem = CreateSemaphore(NULL,0,1,NULL);

  if (writeSem == NULL) {
    MessageBox(NULL,"Can't create write semaphore","MzCOM",MB_OK);
    return; 
  }

  exitSem = CreateSemaphore(NULL,0,1,NULL);

  if (exitSem == NULL) {
    MessageBox(NULL,"Can't create exit semaphore","MzCOM",MB_OK);
    return; 
  }

  evalSems[0] = writeSem;
  evalSems[1] = exitSem;

  startMzThread();
}

void CMzObj::killMzThread(void) {
  if (threadHandle) {
    DWORD threadStatus;

    GetExitCodeThread(threadHandle,&threadStatus);

    if (threadStatus == STILL_ACTIVE) {
      TerminateThread(threadHandle,0);
    }

    CloseHandle(threadHandle);

    threadHandle = NULL;
  }
}

CMzObj::~CMzObj(void) {

  killMzThread();

  if (readSem) {
    CloseHandle(readSem);
  }

  if (writeSem) {
    CloseHandle(writeSem);
  }

  if (exitSem) {
    CloseHandle(exitSem);
  }

  if (inputMutex) {
    CloseHandle(inputMutex);
  }
}

void CMzObj::RaiseError(const OLECHAR *msg) {
  BSTR bstr;
  bstr = SysAllocString(msg);
  Fire_SchemeError(bstr);
  SysFreeString(bstr);
}

BOOL CMzObj::testThread(void) {
  DWORD threadStatus;

  if (threadHandle == NULL) {
    RaiseError(L"No evaluator");
    return FALSE;
  }

  if (GetExitCodeThread(threadHandle,&threadStatus) == 0) { 
    RaiseError(L"Evaluator may be terminated");
  }

  if (threadStatus != STILL_ACTIVE) {
    RaiseError(L"Evaluator terminated");
    return FALSE;
  }

  return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CMzObj


STDMETHODIMP CMzObj::Eval(BSTR input, BSTR *output) {
  if (!testThread()) {
    return E_ABORT;
  }

  WaitForSingleObject(inputMutex,INFINITE);
  globInput = &input;
  // allow evaluator to read
  ReleaseSemaphore(readSem,1,NULL);
  // wait until evaluator done or eval thread terminated
  if (WaitForMultipleObjects(2,evalSems,FALSE,INFINITE) ==
      WAIT_OBJECT_0 + 1) {
    RaiseError(L"Scheme terminated evaluator");
    return E_FAIL;
  }
  *output = globOutput;
  ReleaseSemaphore(inputMutex,1,NULL);

  if (errorState) {
    RaiseError(wideError);
  }

  return hr;
}

BOOL WINAPI dlgProc(HWND hDlg,UINT msg,WPARAM wParam,LPARAM) {
  switch(msg) {
  case WM_INITDIALOG :
    SetDlgItemText(hDlg,MZCOM_URL,
		   "http://www.cs.rice.edu/CS/PLT/packages/mzcom/");
    return TRUE;
  case WM_COMMAND :
    switch (LOWORD(wParam)) { 
    case IDOK :
    case IDCANCEL :
      EndDialog(hDlg,0);
      return FALSE;
    }
  default :
    return FALSE;
  }
}

STDMETHODIMP CMzObj::About() {
  DialogBox(globHinst,MAKEINTRESOURCE(ABOUTBOX),NULL,dlgProc);
  return S_OK;
}

