
/********************************************************/
/*                       Regions                        */
/********************************************************/

wxRegion::wxRegion(wxDC *_dc, wxRegion *r)
{
  dc = _dc;
  is_ps = wxSubType(dc->__type, wxTYPE_DC_POSTSCRIPT);
  
#ifdef wx_msw
  rgn = NULL;
#endif
#ifdef wx_x
  rgn = NULL;
#endif
#ifdef wx_mac
  rgn = NULL;
#endif
  if (r) Union(r);
}

wxRegion::~wxRegion()
{
  Cleanup();
}

void wxRegion::Cleanup()
{  
#ifdef wx_msw
  if (rgn) {
    DeleteObject(rgn);
    rgn = NULL;
  }
#endif
#ifdef wx_x
  if (rgn) {
    XDestroyRegion(rgn);
    rgn = NULL;
  }
#endif
#ifdef wx_mac
  if (rgn) {
    DisposeRgn(rgn);
    rgn = NULL;
  }
#endif
}

void wxRegion::SetRectangle(float x, float y, float width, float height)
{
  float xw, yh;
  int ix, iy, iw, ih;

  Cleanup();

  xw = x + width;
  yh = y + height;
  x = dc->FLogicalToDeviceX(x);
  y = dc->FLogicalToDeviceY(y);
  width = dc->FLogicalToDeviceX(xw) - x;
  height = dc->FLogicalToDeviceY(yh) - y;

  if (is_ps) {
    wxPSRgn *ra;

    height = -height;

    ra = new wxPSRgn_Atomic("", "rect");
    ps = ra;
    Put(x); Put(" "); Put(y); Put(" moveto\n");
    Put(x + width); Put(" "); Put(y); Put(" lineto\n");
    Put(x + width); Put(" "); Put(y - height); Put(" lineto\n");
    Put(x); Put(" "); Put(y - height); Put(" lineto\n");
    Put("closepath\n");

    /* So bitmap-based region is right */
    y  = -y;
  }

  ix = (int)floor(x);
  iy = (int)floor(y);
  iw = ((int)floor(x + width)) - ix;
  ih = ((int)floor(y + height)) - iy;

#ifdef wx_msw
  rgn = CreateRectRgn(ix, iy, ix + iw, iy + ih);
#endif
#ifdef wx_x
  {
    XRectangle r;
    rgn = XCreateRegion();
    r.x = ix;
    r.y = iy;
    r.width = iw;
    r.height = ih;
    XUnionRectWithRegion(&r, rgn, rgn);
  }
#endif
#ifdef wx_mac
  rgn = NewRgn();
  SetRectRgn(rgn, ix, iy, ix + iw, iy + ih);
#endif
}

void wxRegion::SetRoundedRectangle(float x, float y, float width, float height, float radius)
{
  wxRegion *lt, *rt, *lb, *rb, *w, *h, *r;
  int ix, iy, iw, ih;
  float xw, yh;

  Cleanup();

  // A negative radius value is interpreted to mean
  // 'the proportion of the smallest X or Y dimension'
  if (radius < 0.0) {
    float smallest = 0.0;
    if (width < height)
      smallest = width;
    else
      smallest = height;
    radius = (float)(- radius * smallest);
  } else
    radius = dc->FLogicalToDeviceXRel(radius);

#ifndef wx_x
  if (is_ps) {
#endif

    lt = new wxRegion(dc);
    rt = new wxRegion(dc);
    lb = new wxRegion(dc);
    rb = new wxRegion(dc);
    w = new wxRegion(dc);
    h = new wxRegion(dc);

    lt->SetEllipse(x, y, 2 * radius, 2 * radius);
    rt->SetEllipse(x + width - 2 * radius, y, 2 * radius, 2 * radius);
    rb->SetEllipse(x + width - 2 * radius, y + height - 2 * radius, 2 * radius, 2 * radius);
    lb->SetEllipse(x, y + height - 2 * radius, 2 * radius, 2 * radius);

    w->SetRectangle(x, y + radius, width, height - 2 * radius);
    h->SetRectangle(x + radius, y, width - 2 * radius, height);

    r = lt;
    r->Union(rt);
    r->Union(lb);
    r->Union(rb);
    r->Union(w);
    r->Union(h);

    ps = r->ps;
#ifdef wx_x
    /* A little hack: steal rgn from r: */
    rgn = r->rgn;
    r->rgn = NULL;
#else
  }
#endif

  xw = x + width;
  yh = y + height;
  x = dc->FLogicalToDeviceX(x);
  y = dc->FLogicalToDeviceY(y);
  width = dc->FLogicalToDeviceX(xw) - x;
  height = dc->FLogicalToDeviceY(yh) - y;
#if defined(wx_msw) || defined(wx_mac)
  int xradius = dc->FLogicalToDeviceXRel(radius);
  int yradius = dc->FLogicalToDeviceYRel(radius);
#endif

  ix = (int)floor(x);
  iy = (int)floor(y);
  iw = ((int)floor(x + width)) - ix;
  ih = ((int)floor(y + height)) - iy;

  if (is_ps) {
    height = -height;

    /* So bitmap-based region is right */
    y = -y;
  }

#ifdef wx_msw
  rgn = CreateRoundRectRgn(ix, iy, ix + iw, iy + ih, xradius, yradius);
#endif
#ifdef wx_mac
  rgn = NewRgn();
  OpenRgn();
  Rect r2;
  SetRect(&r2, ix, iy, ix + iw, iy + ih);
  FrameRoundRect(&r2, xradius, yradius);
  CloseRgn(rgn);
#endif
}

void wxRegion::SetEllipse(float x, float y, float width, float height)
{
  float xw, yh;

  Cleanup();

  xw = x + width;
  yh = y + height;
  x = dc->FLogicalToDeviceX(x);
  y = dc->FLogicalToDeviceY(y);
  width = dc->FLogicalToDeviceX(xw) - x;
  height = dc->FLogicalToDeviceY(yh) - y;

  if (is_ps) {
    wxPSRgn *ra;

    height = -height;

    ra = new wxPSRgn_Atomic("", "ellipse");
    ps = ra;
    Put(x + width / 2); Put(" "); Put(y - height / 2); Put(" moveto\n");
    Put(x + width / 2); Put(" "); Put(y - height / 2); Put(" ");
    Put(width / 2); Put(" "); Put(height / 2); Put(" 0 360 ellipse\n");
    Put("closepath\n");

    /* So bitmap-based region is right */
    y = -y;
  }

#if defined(wx_msw) || defined(wx_mac)
  int ix, iy, iw, ih;
  
  ix = (int)floor(x);
  iy = (int)floor(y);
  iw = ((int)floor(x + width)) - ix;
  ih = ((int)floor(y + height)) - iy;
#endif

#ifdef wx_msw
  rgn = CreateEllipticRgn(ix, iy, ix + iw, iy + ih);
#endif
#ifdef wx_mac
  rgn = NewRgn();
  OpenRgn();
  Rect r;
  SetRect(&r, ix, iy, ix + iw, iy + ih);
  FrameOval(&r);
  CloseRgn(rgn);
#endif

#ifdef wx_x
  {
    int iwidth = (int)width + 2;
    int is_odd = iwidth & 0x1;
    int x_extent = (int)((iwidth + 1) / 2) + is_odd, i;
    float w2 = (x_extent - 1) * (x_extent - 1), dx, dy;
    XPoint *p;

#ifdef MZ_PRECISE_GC
    p = (XPoint *)GC_malloc_atomic(sizeof(XPoint) * ((4 * x_extent) - (2 * is_odd)));
#else
    p = new XPoint[(4 * x_extent) - (2 * is_odd)];
#endif

    dx = x + width / 2;
    dy = y + height / 2;
    
    for (i = 0; i < x_extent; i++) {
      float y = (height / width) * sqrt(w2 - (i * i));
      p[i].x = (int)floor(i + dx);
      p[i].y = (int)floor(y + dy);
      p[2 * x_extent - i - 1].x = (int)floor(i + dx);
      p[2 * x_extent - i - 1].y = (int)floor(-y + dy);
      p[2 * x_extent + i - is_odd].x = (int)floor(-i + dx);
      p[2 * x_extent + i - is_odd].y = (int)floor(-y + dy);
      if (i || !is_odd) {
	p[4 * x_extent - i - 1 - 2 * is_odd].x = (int)floor(-i + dx);
	p[4 * x_extent - i - 1 - 2 * is_odd].y = (int)floor(y + dy);
      }
    }
    rgn = XPolygonRegion(p, 4 * x_extent, WindingRule);
  }
#endif
}

#ifdef wx_x
# define POINT XPoint
#endif
#ifdef wx_mac
# define POINT MyPoint
  typedef struct { int x, y; } MyPoint;
#endif

typedef struct { float x, y; } FPoint;

void wxRegion::SetPolygon(int n, wxPoint points[], float xoffset, float yoffset, int fillStyle)
{
  POINT *cpoints;
  FPoint *fpoints;
  int i, v;
  float vf;

  Cleanup();

  if (n < 2)
    return;

  cpoints = new POINT[n];
  fpoints = (is_ps ? new FPoint[n] : (FPoint *)NULL);
  for (i = 0; i < n; i++) {
    v = dc->LogicalToDeviceX(points[i].x + xoffset);
    cpoints[i].x = v;
    v = dc->LogicalToDeviceY(points[i].y + yoffset);
    cpoints[i].y = v;
    if (fpoints) {
      vf = dc->FLogicalToDeviceX(points[i].x + xoffset);
      fpoints[i].x = vf;
      vf = dc->FLogicalToDeviceY(points[i].y + yoffset);
      fpoints[i].y = vf;
    }
  }

  if (is_ps) {
    wxPSRgn *ra;
    ra = new wxPSRgn_Atomic("", "poly");
    ps = ra;
    Put(fpoints[0].x); Put(" "); Put(fpoints[0].y); Put(" moveto\n");
    for (i = 1; i < n; i++) {
      Put(fpoints[i].x); Put(" "); Put(fpoints[i].y); Put(" lineto\n");
    }
    Put("closepath\n");

    /* So bitmap-based region is right */
    for (i = 0; i < n; i++) {
      cpoints[i].y = -cpoints[i].y;
    }
  }

#ifdef wx_msw
  rgn = CreatePolygonRgn(cpoints, n, (fillStyle == wxODDEVEN_RULE) ? ALTERNATE : WINDING);
#endif
#ifdef wx_x
  rgn = XPolygonRegion(cpoints, n, (fillStyle == wxODDEVEN_RULE) ? EvenOddRule : WindingRule);
#endif
#ifdef wx_mac
  rgn = NewRgn();
  OpenRgn();
  MoveTo(cpoints[0].x, cpoints[0].y);
  for (i = 0; i < n; i++)
    LineTo(cpoints[i].x, cpoints[i].y);
  LineTo(cpoints[0].x, cpoints[0].y);
  CloseRgn(rgn);
#endif
}

void wxRegion::SetArc(float x, float y, float w, float h, float start, float end)
{
  wxRegion *r;
  static double pi;
  int saw_start = 0, saw_end = 0, closed = 0;
  float cx, cy;
  wxPoint *a;
  int n;

#ifdef MZ_PRECISE_GC
  a = (wxPoint *)GC_malloc_atomic(sizeof(wxPoint) * 20);
#else
  a = new wxPoint[20];
#endif

  SetEllipse(x, y, w, h);

  if (start == end) return;

  r = new wxRegion(dc);

  if (!pi)
    pi = 2 * asin((double)1.0);

  start = fmod(start, 2*pi);
  end = fmod(end, 2*pi);
  if (start < 0)
    start += 2*pi;
  if (end < 0)
    end += 2*pi;

  cx = x + w/2;
  cy = y + h/2;

  a[0].x = (w / 2) * cos(end) + cx;
  a[0].y = (h / 2) * (-sin(end)) + cy;

  a[1].x = cx;
  a[1].y = cy;

  a[2].x = (w / 2) * cos(start) + cx;
  a[2].y = (h / 2) * (-sin(start)) + cy;

  n = 3;

  if (!saw_start && (start < (pi / 2)))
    saw_start = 1;
  if (!saw_end && (end > start) && (end < (pi / 2)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x + w;
    a[n++].y = y;
  }
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y;
  } else
    closed = saw_start;

  if (!saw_start && (start < pi))
    saw_start = 1;
  if (!saw_end && (end > start) && (end < pi))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x;
    a[n++].y = y;
  }
  if (saw_start && !saw_end) {
    a[n].x = x;
    a[n++].y = cy;
  } else
    closed = saw_start;

  if (!saw_start && (start < (1.5 * pi)))
    saw_start = 1;
  if (!saw_end && (end > start) && (end < (1.5 * pi)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x;
    a[n++].y = y + h;
  }
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y + h;
  } else
    closed = saw_start;

  saw_start = 1;
  saw_end = (end > start);
  
  if (saw_start && !closed) {
    a[n].x = x + w;
    a[n++].y = y + h;
  }
  if (saw_start && !saw_end) {
    a[n].x = x + w;
    a[n++].y = cy;    
  } else
    closed = saw_start;

  if (!saw_end && (end < (pi / 2)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x + w;
    a[n++].y = y;
  }
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y;    
  } else
    closed = saw_start;
  
  if (!saw_end && (end < pi))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x;
    a[n++].y = y;
  }
  if (saw_start && !saw_end) {
    a[n].x = x;
    a[n++].y = cy;    
  } else
    closed = saw_start;

  if (!saw_end && (end < (1.5 * pi)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x;
    a[n++].y = y + h;
  } 
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y + h;
  } else
    closed = saw_start;

  if (!closed) {
    a[n].x = x + w;
    a[n++].y = y + h;
  }

  r->SetPolygon(n, a);

  Intersect(r);
}

void wxRegion::Union(wxRegion *r)
{
  if (r->dc != dc) return;
  if (r->Empty()) return;

  if (is_ps) {
    if (!ps)
      ps = r->ps;
    else {
      wxPSRgn *ru;
      ru = new wxPSRgn_Union(ps, r->ps);
      ps = ru;
    }
  }

#ifdef wx_msw
  if (!rgn) {
    rgn = CreateRectRgn(0, 0, 1, 1);
    CombineRgn(rgn, r->rgn, rgn, RGN_COPY);
  } else
    CombineRgn(rgn, r->rgn, rgn, RGN_OR);
#endif
#ifdef wx_x
  if (!rgn) {
    rgn = XCreateRegion();
  }
  XUnionRegion(rgn, r->rgn, rgn);
#endif
#ifdef wx_mac
  if (!rgn)
    rgn = NewRgn();
  UnionRgn(rgn, r->rgn, rgn);
#endif
}

void wxRegion::Intersect(wxRegion *r)
{
  if (r->dc != dc) return;
  if (r->Empty()) {
    Cleanup();
    ps = NULL;
    return;
  }

#ifdef wx_msw
  if (!rgn) return;
  CombineRgn(rgn, r->rgn, rgn, RGN_AND);
#endif
#ifdef wx_x
  if (!rgn) return;
  XIntersectRegion(rgn, r->rgn, rgn);
#endif
#ifdef wx_mac
  if (!rgn) return;
  SectRgn(rgn, r->rgn, rgn);
#endif

  if (Empty()) {
    Cleanup();
    ps = NULL;
  } else if (is_ps) {
    wxPSRgn *ri;
    ri = new wxPSRgn_Intersect(ps, r->ps);
    ps = ri;
  }
}

void wxRegion::Subtract(wxRegion *r)
{
  if (r->dc != dc) return;
  if (r->Empty()) return;

#ifdef wx_msw
  if (!rgn) return;
  CombineRgn(rgn, rgn, r->rgn, RGN_DIFF);
#endif
#ifdef wx_x
  if (!rgn) return;
  XSubtractRegion(rgn, r->rgn, rgn);
#endif
#ifdef wx_mac
  if (!rgn) return;
  DiffRgn(rgn, r->rgn, rgn);
#endif

  if (Empty()) {
    Cleanup();
    ps = NULL;
  } else if (is_ps) {
    /* wxPSRgn_Diff is only half a subtract; the result must be intersected with the first part */
    wxPSRgn *rd, *ri;
    rd = new wxPSRgn_Diff(ps, r->ps);
    ri = new wxPSRgn_Intersect(ps, rd);
    ps = ri;
  }
}

void wxRegion::BoundingBox(float *x, float *y, float *w, float *h)
{
  if (Empty()) {
    *x = *y = *w = *h = 0;
    return;
  } else {
    float v;
#ifdef wx_msw
    RECT r;

    GetRgnBox(rgn, &r);
  
    *x = r.left;
    *y = r.top;
    *w = r.right - r.left;
    *h = r.bottom - r.top;
#endif
#ifdef wx_x
    XRectangle r;
    
    XClipBox(rgn, &r);
    
    *x = r.x;
    *y = r.y;
    *w = r.width;
    *h = r.height;
#endif
#ifdef wx_mac
    *x = (*rgn)->rgnBBox.left;
    *y = (*rgn)->rgnBBox.top;
    *w = (*rgn)->rgnBBox.right - *x;
    *h = (*rgn)->rgnBBox.bottom - *y;
#endif

    if (is_ps) {
      /* Bitmap-based region is stored upside-down */
      *y = -(*y);
    }
    
    v = dc->DeviceToLogicalX((int)*x);
    *x = v;
    v = dc->DeviceToLogicalY((int)*y);
    *y = v;
    v = dc->DeviceToLogicalXRel((int)*w);
    *w = v;
    v = dc->DeviceToLogicalYRel((int)*h);
    *h = v;
  }
}

Bool wxRegion::Empty()
{
#ifdef wx_msw
  RECT r;
  if (!rgn) return TRUE;

  return (GetRgnBox(rgn, &r) == NULLREGION);
#endif
#ifdef wx_x
  if (!rgn) return TRUE;
  return XEmptyRegion(rgn);
#endif
#ifdef wx_mac
  if (!rgn) return TRUE;
  return EmptyRgn(rgn);
#endif
}

void wxRegion::Put(const char *s)
{
  long l, psl;
  char *naya;

  l = strlen(s);
  psl = strlen(((wxPSRgn_Atomic *)ps)->s);

  naya = new WXGC_ATOMIC char[l + psl + 1];
  memcpy(naya, ((wxPSRgn_Atomic *)ps)->s, psl);
  memcpy(naya + psl, s, l);
  naya[psl + l] = 0;
  
  ((wxPSRgn_Atomic *)ps)->s = naya;
}

void wxRegion::Put(double d)
{
  char s[100];
  sprintf(s, "%f", d);
  Put(s);
}

/***************************************************************************************/

char *wxPSRgn_Union::GetString()
{
  return MakeString("", "", "");
}

char *wxPSRgn_Composite::MakeString(const char *prefix, const char *infix, const char *suffix)
{
  char *sa, *sb;
  int plen, ilen, slen;
  int alen, blen;
  char *sr;

  sa = a->GetString();
  sb = b->GetString();
  plen = strlen(prefix);
  ilen = strlen(infix);
  slen = strlen(suffix);
  alen = strlen(sa);
  blen = strlen(sb);
  sr = new WXGC_ATOMIC char[alen + blen + plen + ilen + slen + 1];

  memcpy(sr, prefix, plen);
  memcpy(sr + plen, sa, alen);
  memcpy(sr + plen + alen, infix, ilen);
  memcpy(sr + plen + alen + ilen, sb, blen);
  memcpy(sr + plen + alen + ilen + blen, suffix, slen);
  sr[plen + alen + ilen + blen + slen] = 0;

  return sr;
}

int wxPSRgn_Composite::FlattenIntersects(wxPSRgn **l, wxPSRgn *r, int i)
{
  if (r->is_intersect)
    return FlattenIntersects(l, ((wxPSRgn_Composite *)r)->b, 
			     FlattenIntersects(l, ((wxPSRgn_Composite *)r)->a, i));
  
  if (l)
    l[i] = r;

  return i + 1;
}


wxPSRgn *wxPSRgn_Union::Lift()
{
  wxPSRgn *la, *lb;
  wxPSRgn *r = NULL, **al, **bl;
  int na, nb, i, j;

  la = a->Lift();
  lb = b->Lift();

  if (!la->is_intersect
      && !lb->is_intersect
      && (a == la) && (b == lb))
    return this;

  /* (A n B) U (C n D) = (A U C) n (A U D) n (B U C) n (B U D) */

  /* count: */
  na = FlattenIntersects(NULL, la, 0);
  nb = FlattenIntersects(NULL, lb, 0);

  al = new wxPSRgn*[na];
  bl = new wxPSRgn*[nb];

  /* flatten: */
  FlattenIntersects(al, la, 0);
  FlattenIntersects(bl, lb, 0);

  for (i = 0; i < na; i++) {
    for (j = 0; j < nb; j++) {
      wxPSRgn *c;
      c = new wxPSRgn_Union(al[i], bl[j]);
      if (r)
	r = new wxPSRgn_Intersect(r, c);
      else
	r = c;
    }
  }

  return r;
}


char *wxPSRgn_Intersect::GetString()
{
  return MakeString("", "clip\nnewpath\n", "");
}

wxPSRgn *wxPSRgn_Intersect::Lift()
{
  wxPSRgn *la, *lb;

  la = a->Lift();
  lb = b->Lift();

  if ((la == a) && (lb == b))
    return this;
  else
    return new wxPSRgn_Intersect(la, lb);
}


char *wxPSRgn_Diff::GetString()
{
  return MakeString("", "reversepath\n", "reversepath\n");
}

wxPSRgn *wxPSRgn_Diff::Lift()
{
  wxPSRgn *la, *lb;
  wxPSRgn *r = NULL, **al, **bl;
  int na, nb, i;

  la = a->Lift();
  lb = b->Lift();

  if (!la->is_intersect
      && !lb->is_intersect
      && (a == la) && (b == lb))
    return this;

  if (lb->is_intersect) {
    /* A \ (B n C) = (A \ B) u (A \ C) */
    nb = FlattenIntersects(NULL, lb, 0);
    bl = new wxPSRgn*[nb];
    FlattenIntersects(bl, lb, 0);
    
    for (i = 0; i < nb; i++) {
      wxPSRgn *s;
      s = new wxPSRgn_Diff(la, bl[i]);
      if (r) {
	r = new wxPSRgn_Union(r, s);
      } else
	r = s;
    }

    return r->Lift(); /* Handles intersections in la */
  } else {
    /* (A n B) - C = (A - C) n (B - C)   [note: C has no intersections] */
    na = FlattenIntersects(NULL, la, 0);
    al = new wxPSRgn*[na];
    FlattenIntersects(al, la, 0);
    
    for (i = 0; i < na; i++) {
      wxPSRgn *s;
      s = new wxPSRgn_Diff(al[i], lb);
      if (r) {
	r = new wxPSRgn_Intersect(r, s);
      } else
	r = s;
    }

    return r;
  }
}
