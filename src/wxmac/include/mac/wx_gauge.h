/*
 * File:	wx_gauge.h
 * Purpose:	Gauge box (experimental)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_group.h	1.2 5/9/94" */

#ifndef __WX_GAUGE__
#define __WX_GAUGE__

#include "wb_gauge.h"
#include "wxLabelArea.h"

// Group box
class wxGauge: public wxbGauge
{
 private:
  wxLabelArea*	cTitle;
  ControlHandle	cMacControl;
  Rect		valueRect;
  int    range;
  int    value;

 public:
  wxGauge(void);
  wxGauge(wxPanel *panel, char *label, int range, int x = -1, int y = -1,
           int width = -1, int height = -1, long style = 0, char *name = "gauge");
  ~wxGauge(void);

  virtual void SetShadowWidth(int w) {}
  virtual void SetBezelFace(int w) {}
  virtual void SetRange(int r);
  virtual void SetValue(int pos);
  
  virtual void Paint(void);
  virtual void OnClientAreaDSize(int dW, int dH, int dX, int dY);
  virtual char *GetLabel(void);
  virtual void SetLabel(char *);

  virtual void DoShow(Bool s);

  virtual void ChangeToGray(Bool gray);
};

#endif
