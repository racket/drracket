/*
 * File:	wx_obj.h
 * Purpose:	Top level object for wxWindows
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_obj.h	1.2 5/9/94" */

#ifndef wxb_objh
#define wxb_objh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"

#ifdef IN_CPROTO
typedef       void    *wxObject ;
#else

#if WXGARBAGE_COLLECTION_ON
#include "gc_cpp.h"
#define WXGC_IGNORE(ptr) GC_general_register_disappearing_link((void **)&(ptr), NULL)
#define WXGC_ATOMIC (AtomicGC)
#define WXGC_NO_CLEANUP FALSE
class wxObject : public gc_cleanup
#else
#define WXGC_IGNORE(ptr)
#define WXGC_ATOMIC
class wxObject
#endif
{
  public:
  WXTYPE __type;
  wxObject(void);
  wxObject(Bool cleanup);
  wxObject(Bool cleanup, WXTYPE t);
  virtual ~wxObject(void);
};

#endif // IN_CPROTO
#endif // wx_objh
