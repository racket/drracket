/*
 * File:        wx_madm.cc
 * Purpose:     wxMediaAdmins
 * Author:      Matthew Flatt
 * Created:     1997
 * Copyright:   (c) 1997, Matthew Flatt
 */


#ifndef __WX_MEDIA_ADMIN__
#define __WX_MEDIA_ADMIN__

class wxMediaAdmin : public wxObject
{
 private:
  friend class wxMediaCanvas;
  friend class wxMediaEdit;
  friend class wxCanvasMediaAdmin;
  int standard; /* Used to recognize standard display. Hack. */
  
 public:
  inline wxMediaAdmin();

  /* Usually called by wxMediaBuffer objects: */
  virtual wxDC *GetDC(float *x = NULL, float *y = NULL) = 0;
  virtual void GetView(float *x, float *y, float *w, float *h, 
		       Bool full = FALSE) = 0;
  virtual Bool ScrollTo(float localx, float localy, float w, float h,
			Bool refresh = TRUE, int bias = 0) = 0;
  virtual void GrabCaret(int = wxFOCUS_GLOBAL) = 0;

  virtual void Resized(Bool redraw_now) = 0;
  virtual void NeedsUpdate(float localx, float localy, float w, float h) = 0;

  virtual void UpdateCursor() = 0;

  virtual void GetMaxView(float *x, float *y, float *w, float *h, 
			  Bool full = FALSE);
  virtual Bool DelayRefresh();
};

inline wxMediaAdmin::wxMediaAdmin()
     : wxObject(WXGC_NO_CLEANUP)
{
  standard = 0; 
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_MEDIA_ADMIN; 
#endif
};

class wxCanvasMediaAdmin : public wxMediaAdmin
{
 private:
  friend class wxMediaCanvas;
  friend class os_wxCanvasMediaAdmin;
  friend class wxUpdateCursorTimer;

  wxMediaCanvas *canvas;
  wxCanvasMediaAdmin *nextadmin, *prevadmin;
  Bool resetFlag;
  Bool updateBlock, resizedBlock;
  wxUpdateCursorTimer *updateCursorTimer;

  inline void AdjustStdFlag(void);

  wxCanvasMediaAdmin(wxMediaCanvas *c);

 public:
  ~wxCanvasMediaAdmin();

  /* Usually called by wxMediaBuffer objects: */
  wxDC *GetDC(float *x = NULL, float *y = NULL);
  void GetView(float *x, float *y, float *h, float *w, Bool full = FALSE);
  Bool ScrollTo(float localx, float localy, float, float, 
		Bool refresh = TRUE, int bias = 0);

  void GrabCaret(int = wxFOCUS_GLOBAL);

  void Resized(Bool update);
  void NeedsUpdate(float localx, float localy, float w, float h);

  void UpdateCursor();
  void GetMaxView(float *x, float *y, float *h, float *w, Bool full = FALSE);

  inline wxMediaCanvas *GetCanvas() { return canvas; }
};

/* Used by wxMediaSnipMediaAdmin: */
class wxMSMA_SnipDrawState {
 public:
  Bool drawing;
  float x, y;
  wxDC *dc;
};

class wxMediaSnipMediaAdmin : public wxMediaAdmin
{
  friend class wxMediaSnip;
  friend class os_wxMediaSnipMediaAdmin;
  
  wxMSMA_SnipDrawState *state;

  wxMediaSnip *snip;

  void RestoreState(wxMSMA_SnipDrawState *saved);
  void SaveState(wxMSMA_SnipDrawState *save, wxDC *dc, float x, float y);

  wxMediaSnipMediaAdmin(wxMediaSnip *s);

 public:

  /* Only to make wxs_madm.xc happy */
  inline wxMediaSnipMediaAdmin();

  wxDC *GetDC(float *x = NULL, float *y = NULL);
  void GetView(float *x, float *y, float *h, float *w, Bool full = FALSE);
  Bool ScrollTo(float localx, float localy, float w, float h,
			Bool refresh = TRUE, int bias = 0);
  void GrabCaret(int = wxFOCUS_GLOBAL);

  void Resized(Bool redraw_now);
  void NeedsUpdate(float localx, float localy, float w, float h);

  void UpdateCursor();

  virtual Bool DelayRefresh();

  inline wxMediaSnip* GetSnip() { return snip; }
};

/* Only to make wxs_madm.xc happy */
inline wxMediaSnipMediaAdmin::wxMediaSnipMediaAdmin()
{
  snip = NULL;
}

#endif /* __WX_MEDIA_ADMIN__ */
