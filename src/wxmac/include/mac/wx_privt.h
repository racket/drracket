/*
 * File:	wx_privt.h
 * Purpose:	Private class declarations.
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_privt.h	1.2 5/9/94" */

#ifndef wx_privth
#define wx_privth

#ifdef __GNUG__
#pragma interface
#endif

#ifdef wx_motif
#include <Xm/Xm.h>
#include "wx_hash.h"
extern wxHashTable *wxWidgetHashTable;

void wxWidgetResizeProc(Widget w, XConfigureEvent *event, String args[], int *num_args);
// void wxWidgetRepaintProc(Widget w, XtPointer clientData, XtPointer);
#endif

#ifdef wx_xview
#include <xview/server.h>
extern Xv_Server xview_server;
#endif

#define	wxNO_COLORS   0x00
#define wxBACK_COLORS 0x01
#define wxFORE_COLORS 0x02

#ifdef wx_motif
extern XColor itemColors[5] ;

#define wxBACK_INDEX 0
#define wxFORE_INDEX 1
#define wxSELE_INDEX 2
#define wxTOPS_INDEX 3
#define wxBOTS_INDEX 4
#endif

#endif // wx_privth

