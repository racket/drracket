/*
 * File:     wx_image.cc
 * Purpose:  
 *
 *                       wxWindows 1.50
 * Copyright (c) 1993 Artificial Intelligence Applications Institute,
 *                   The University of Edinburgh
 *
 *                     Author: Julian Smart
 *                        Date: 7-9-93
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice, author statement and this permission
 * notice appear in all copies of this software and related documentation.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, EXPRESS,
 * IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * IN NO EVENT SHALL THE ARTIFICIAL INTELLIGENCE APPLICATIONS INSTITUTE OR THE
 * UNIVERSITY OF EDINBURGH BE LIABLE FOR ANY SPECIAL, INCIDENTAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT ADVISED OF THE POSSIBILITY OF
 * DAMAGE, AND ON ANY THEORY OF LIABILITY, ARISING OUT OF OR IN CONNECTION WITH
 * THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#ifdef wx_xview
#include <stdlib.h>
#endif

#ifdef wx_motif
#include <stdlib.h>
#endif

#include "wx.h"
#include "wx_image.h"

#ifdef wx_x

#include <X11/X.h>
#ifdef wx_xview
#include <xview/screen.h>
#include <xview/cursor.h>
#include <xview/svrimage.h>
extern Xv_Server xview_server;
#endif
#ifdef wx_motif
#endif
#ifdef wx_mac
#include "wximgfil.h"
#  if USE_XPM_IN_MAC
#    define FOR_MAC
#    include "xpm34.h"
#  endif
#endif

/* file types that can be read */
#define UNKNOWN 0
#define GIF     1
#define PM      2
#define PBM     3
#define XBM     4
#define BMP     5
#define PCX     6

static unsigned long rootbg, rootfg;  /* fg/bg for root border */
static int    autoquit = 0;     /* quit after loading first pic to rootW */
static int    autogamma = 0;    /* perform gamma correction by default */
static int    rootPattern = 0;  /* pattern used for root border */
static char   initpath[500];
#endif

byte           r[256],g[256],b[256];  /* colormap */

#ifdef wx_x
/* used in XResource reading... */
static char *def_str;
static int   def_int;


wxImage::wxImage(void)
{
  cMap = new wxColourMap;
  filetype = wxUNKNOWN;

  int   imap, ctrlmap, gmap, clrroot;
  char *display, *fname, *whitestr, *blackstr, 
       *infogeom, *fgstr, *bgstr, *ctrlgeom, *gamgeom;
  char *rootfgstr, *rootbgstr;

  XColor ecdef;
  nfcols = 0;

  /*****************************************************/
  /*** variable Initialization                       ***/
  /*****************************************************/

  getcwd(initpath, sizeof(initpath));

  /* init internal variables */
  display = fname = whitestr = blackstr = NULL;
  fgstr = bgstr = rootfgstr = rootbgstr = NULL;
  pic = epic = cpic = NULL;
  theImage = NULL;
  LocalCmap = 0;
  InitFSDTables(); // Only need in xvpbm.c and xvpm.c. Defined in xv24to8.c

  /* init gamma curve */
  ghand[0].x =   0;  ghand[0].y =   0;
  ghand[1].x =  64;  ghand[1].y =  64;
  ghand[2].x = 192;  ghand[2].y = 192;
  ghand[3].x = 255;  ghand[3].y = 255;

  /* init command-line options flags */
  infogeom = DEFINFOGEOM;  ctrlgeom = DEFCTRLGEOM;  gamgeom = DEFGAMGEOM;
  expand = 1;  ncols = -1;  noglob = 0;  revvideo = 0;  mono = 0;  
  perfect = 0;  ninstall = 0;  fixedaspect = 0;  
  dEBUG = 0;  bwidth = 2;
  useroot = clrroot = noqcheck = rwcolor = 0;

#ifdef BROKEFREECOLS
  brokeFreeCols = 1;
#else
  brokeFreeCols = 0;
#endif

  defaspect = normaspect = 1.0;

  imap = ctrlmap = gmap = 0;

  /*****************************************************/
  /*** X Resource Initialization                     ***/
  /*****************************************************/

  /* open the display */

  if ( (theDisp=XOpenDisplay(display)) == NULL) {
    fprintf(stderr, "Can't open display.\n");
    exit(1);
  }

  if (rd_str ("infoGeometry"))  infogeom = def_str;
  if (rd_flag("infoMap"))       imap     = def_int;
  if (rd_flag("mono"))          mono     = def_int;
  if (rd_int ("ncols"))         { ncols  = def_int; if (ncols>=0) noglob = 1; }
  if (rd_flag("nglobal"))       noglob   = def_int;
  if (rd_flag("ninstall"))      ninstall = def_int;
  if (rd_flag("noqcheck"))      noqcheck = def_int;
  if (rd_flag("perfect"))       perfect  = def_int;
  if (rd_flag("reverseVideo"))  revvideo = def_int;
  if (rd_str ("rootBackground")) rootbgstr = def_str;
  if (rd_str ("rootForeground")) rootfgstr = def_str;
  if (rd_int ("rootPattern"))    rootPattern = def_int;
  if (rd_flag("rwColor"))       rwcolor  = def_int;
  if (rd_flag("slow24"))        slow24   = def_int;
  if (rd_str ("white"))         whitestr = def_str;
      
  /* if using root, generally gotta map ctrl window, 'cause there won't be
     any way to ask for it.  (no kbd or mouse events from rootW) */
  if (useroot && !autoquit) 
    ctrlmap = 1;    

  /* must not install colormaps on rootW */
  if (useroot) { perfect=0;  noglob = 1; } 


  /*****************************************************/
  /*** X Setup                                       ***/
  /*****************************************************/
  
  theScreen = DefaultScreen(theDisp);
  theCmap   = DefaultColormap(theDisp, theScreen);
  rootW     = RootWindow(theDisp,theScreen);
  theVisual = DefaultVisual(theDisp,theScreen);
  ncells    = DisplayCells(theDisp, theScreen);
  dispWIDE  = DisplayWidth(theDisp,theScreen);
  dispHIGH  = DisplayHeight(theDisp,theScreen);
  dispDEEP  = DisplayPlanes(theDisp,theScreen);

  /* set up white,black colors */
  white = WhitePixel(theDisp,theScreen);
  black = BlackPixel(theDisp,theScreen);
  if (whitestr && XParseColor(theDisp, theCmap, whitestr, &ecdef) &&
      XAllocColor(theDisp, theCmap, &ecdef))  white = ecdef.pixel;
  if (blackstr && XParseColor(theDisp, theCmap, blackstr, &ecdef) &&
      XAllocColor(theDisp, theCmap, &ecdef))  black = ecdef.pixel;

  /* set up fg,bg colors */
  fg = black;   bg = white;
  if (fgstr && XParseColor(theDisp, theCmap, fgstr, &ecdef) &&
      XAllocColor(theDisp, theCmap, &ecdef))  fg = ecdef.pixel;
  if (bgstr && XParseColor(theDisp, theCmap, bgstr, &ecdef) &&
      XAllocColor(theDisp, theCmap, &ecdef))  bg = ecdef.pixel;

  /* set up root fg,bg colors */
  rootfg = white;   rootbg = black;
  if (rootfgstr && XParseColor(theDisp, theCmap, rootfgstr, &ecdef) &&
      XAllocColor(theDisp, theCmap, &ecdef))  rootfg = ecdef.pixel;
  if (rootbgstr && XParseColor(theDisp, theCmap, rootbgstr, &ecdef) &&
      XAllocColor(theDisp, theCmap, &ecdef))  rootbg = ecdef.pixel;

  /* set up infofg,infobg colors */
  infofg = fg;   infobg = bg;


  /* if '-mono' not forced, determine if we're on a b/w or color monitor */
  if (!mono)
  {
    if (!wxColourDisplay())
      mono = 1;
  }
  
  /* if ncols wasn't set, set it to 2^dispDEEP, unless dispDEEP=1, in which
     case ncols = 0;  (ncols = max number of colors allocated.  on 1-bit
     displays, no colors are allocated */

  if (ncols == -1) {
    if (dispDEEP>1) ncols = 1<<dispDEEP;
    else ncols = 0;
  }
  else if (ncols>256) ncols = 256;       /* so program doesn't blow up */

  GenerateGamma();
  GenerateFSGamma();

  /* if we're not on a colormapped display, turn off rwcolor */
/*
  if (!(theVisual->class & 1) && rwcolor) {
    fprintf(stderr,"xv: not a colormapped display.  'rwcolor' turned off.\n");
    rwcolor = 0;
  }
*/
 
}

wxImage::~wxImage(void)
{
  Destroy();
//  delete cMap;
}

Bool wxImage::Load(char *file)
{
  Destroy();
  return openPic(file);
}

Bool wxImage::Destroy(void)
{
  closePic();
  return TRUE;
}

void wxImage::GetSize(int *width, int *height)
{
  *width = eWIDE;
  *height = eHIGH;
}

wxColourMap *wxImage::GetColourMap(void)
{
  SortColormap();

  // save the desired RGB colormap (before gamma-correcting it)
  for (int i=0; i<numcols; i++)
    { rorg[i] = r[i];  gorg[i] = g[i];  borg[i] = b[i]; }

  DoMonoAndRV();
  if (autogamma) GammifyColors();

  if (rwcolor) AllocRWColors();
          else AllocColors();

  if (LocalCmap)
  {
    cMap->cmap = LocalCmap;
    cMap->destroyable = TRUE;
    return cMap;
//    XSetWindowColormap(theDisp,mainW, LocalCmap);
  }
  else return NULL;
}

/*
void wxImage::SetWindow(wxCanvas *can)
{
  canvas = can;
  if (!canvas)
    return;

  SortColormap();

  // save the desired RGB colormap (before gamma-correcting it)
  for (int i=0; i<numcols; i++)
    { rorg[i] = r[i];  gorg[i] = g[i];  borg[i] = b[i]; }

  DoMonoAndRV();
  if (autogamma) GammifyColors();

  if (rwcolor) AllocRWColors();
          else AllocColors();

//  Resize(eWIDE,eHIGH);

  if (LocalCmap) {
//    XSetWindowAttributes xswa;
//    if (!ninstall) XInstallColormap(theDisp,LocalCmap); // Should only be used by WMs!!
//    xswa.colormap = LocalCmap;
//    XChangeWindowAttributes(theDisp,mainW,CWColormap,&xswa);
    XSetWindowColormap(theDisp,mainW, LocalCmap);
  }
}

*/

void wxImage::Draw(wxCanvas *canvas, int x, int y, int width, int height)
{
  if (width == -1)
    width = eWIDE;

  if (height == -1)
    height = eHIGH;

  if (!canvas)
    return;

  wxFlushEvents();

  if (!theImage)
  {
    CreateXImage();

    if (!theImage)
      return;
  }

  wxCanvasDC *dc = canvas->GetDC();
  XPutImage(theDisp,canvas->xwindow,dc->gc,theImage,
              dc->LogicalToDeviceX(x),dc->LogicalToDeviceY(y),
              dc->LogicalToDeviceX(x),dc->LogicalToDeviceY(y),
              dc->LogicalToDeviceXRel(width),dc->LogicalToDeviceYRel(height));
#ifdef wx_motif
  if (canvas->is_retained)
    XPutImage(theDisp,canvas->backingPixmap,canvas->GetDC()->gcBacking,theImage,
      // All Rel(ative) since we don't want to shift the image on the bitmap
      // with scrolling
              dc->LogicalToDeviceXRel(x),dc->LogicalToDeviceYRel(y),
              dc->LogicalToDeviceXRel(x),dc->LogicalToDeviceYRel(y),
              dc->LogicalToDeviceXRel(width),dc->LogicalToDeviceYRel(height));
#endif
}


int wxImage::openPic(char *fullname)
{
  /* tries to load file #filenum (from 'namelist' list)
   * returns 0 on failure (cleans up after itself)
   * if successful, returns 1
   */
  PICINFO pinfo;
  xvbzero((char *) &pinfo, sizeof(PICINFO));

  int   i,okay,freename, nw, nh;
  char *tmp;
  FILE *fp;
  char  filename[256], /* full name of the file to be loaded (could be /tmp) */
        basename[128], /* just the name of the original file. No path */
        magicno[8];    /* first 8 bytes of file */
  
  normaspect = defaspect;

  /* clear any old error messages */

  okay = 0;

  /* set up fullname and basename */

  tmp = rindex(fullname,'/');
  if (!tmp) tmp = fullname; else tmp++;
  strcpy(basename,tmp);
  if (strlen(basename)>2 && strcmp(basename+strlen(basename)-2,".Z")==0) 
    basename[strlen(basename)-2]='\0';     /* chop off .Z, if any */

  /* if fullname doesn't start with a '/' (ie, it's a relative path), 
     (and it's not the special case '<stdin>') prepend 'initpath' to it */
  freename = 0;
  if (fullname[0] != '/' && strcmp(fullname,STDINSTR)!=0) {
    char *tmp;
    tmp = (char *) XpmMallocA(strlen(fullname) + strlen(initpath) + 2);
    if (!tmp) FatalError("malloc 'filename' failed");
    sprintf(tmp,"%s/%s", initpath, fullname);
    fullname = tmp;
    freename = 1;
  }
    
  /* uncompress if it's a .Z file */

  i = strlen(fullname);
  if (i>2 && strcmp(fullname+i-2,".Z")==0) {
    strcpy(filename,"/tmp/xvXXXXXX");
    mktemp(filename);
    sprintf(wxBuffer,"%s -c %s >%s",UNCOMPRESS,fullname,filename);
    if (system(wxBuffer)) {
      goto FAILED;
    }
  }
  else strcpy(filename,fullname);
    

  /* if the file is stdio, write it out to a temp file */
  if (strcmp(filename,STDINSTR)==0) {
    FILE *fp;

    strcpy(filename,"/tmp/xvXXXXXX");
    mktemp(filename);

    fp = fopen(filename,"w");
    if (!fp) FatalError("can't write /tmp/xv****** file");
    
    while ( (i=getchar()) != EOF) putc(i,fp);
    fclose(fp);
  }


  /* now, try to determine what type of file we've got by reading the
     first couple bytes and looking for a Magic Number */

  fp=fopen(filename,"r");
  if (!fp) {
    goto FAILED;
  }

  fread(magicno,8,1,fp);  
  fclose(fp);

  filetype = UNKNOWN;
  if (strncmp(magicno,"GIF87",5)==0) filetype = GIF;

  else if (strncmp(magicno,"VIEW",4)==0 ||
	   strncmp(magicno,"WEIV",4)==0) filetype = PM;

  else if (magicno[0] == 'P' && magicno[1]>='1' && 
	   magicno[1]<='6') filetype = PBM;

  else if (strncmp(magicno,"#define",7)==0) filetype = XBM;

  else if (magicno[0] == 'B' && magicno[1] == 'M') filetype = BMP;

  else if (magicno[0]==0x0a && magicno[1] <= 5) filetype = PCX;

  if (filetype == UNKNOWN) {
    goto FAILED;
  }

  i = 1;
  switch (filetype) {
  case GIF: i = LoadGIF(filename,ncols); break;
  case PM:  i = LoadPM (filename,ncols); break;
  case PBM: i = LoadPBM(filename,ncols); break;
  case XBM: i = LoadXBM(filename,ncols); break;
  case PCX: 
    {
      i = !(LoadPCX(filename, &pinfo));
      pic   = pinfo.pic;
      pWIDE = pinfo.w;
      pHIGH = pinfo.h;
      break;
    }
  case BMP: 
    {
      i = !(LoadBMP(filename, &pinfo));
      pic   = pinfo.pic;
      pWIDE = pinfo.w;
      pHIGH = pinfo.h;
      break;
    }

  }
  cpic = NULL;

  if (i) {
    goto FAILED;
  }

  /* successfully read this picture */

  /* if we read a /tmp file, delete it.  won't be needing it any more */
  if (strcmp(fullname,filename)!=0) unlink(filename);

  normFact = 1;  nw = pWIDE;  nh = pHIGH;

  // expand:  if expansion is negative, treat it as a reciprocal
  if (expand<0) { eWIDE = pWIDE/abs(expand);  eHIGH = pHIGH/abs(expand); }
           else { eWIDE = pWIDE * expand;     eHIGH = pHIGH * expand; }

  cpic = pic;  cWIDE = pWIDE;  cHIGH = pHIGH;  cXOFF = cYOFF = 0;

  if (freename) XpmFree(fullname);

  return 1;

  
 FAILED:
  if (strcmp(fullname,filename)!=0) unlink(filename);   /* kill /tmp file */
  if (freename) XpmFree(fullname);
  return 0;
}




/***********************************/
void wxImage::closePic()
{
  /* kill all resources used for this picture.
     this would include the window, any allocated colors, pic, epic, 
     theImage, etc. */

  int i;

  if (LocalCmap) {
 // Should be destroyed by ~wxColourMap
//    XFreeColormap(theDisp,LocalCmap);
    LocalCmap = 0;
  }
  else if (!brokeFreeCols) {
    for (i=0; i<nfcols; i++) 
      XFreeColors(theDisp, theCmap, &freecols[i], 1, 0L);
  }
  else {
    for (i=0; i<nfcols; i++) {
      int j;
      for (j=0; j<i; j++) {
        if (freecols[i] == freecols[j])   /* already been freed once */
	  break;
      }
      if (j==i)      /* wasn't found in already-freed list */
        XFreeColors(theDisp, theCmap, &freecols[i], 1, 0L);
    }
  }

  if (epic != cpic && epic != NULL) XpmFree(epic);
  if (cpic !=  pic && cpic != NULL) XpmFree(cpic);
  if (pic != NULL) XpmFree(pic);
  if (theImage != NULL) xvDestroyImage(theImage);
  theImage = NULL;
  pic = epic = cpic = NULL;

}

/***********************************/
void wxImage::FixAspect(int grow,int *w,int *h)
{
  /* computes new values of eWIDE and eHIGH which will have aspect ratio
     'normaspect'.  If 'grow' it will preserve aspect by enlarging, 
     otherwise, it will shrink to preserve aspect ratio.  
     Returns these values in 'w' and 'h' */

  float xr,yr,curaspect,a,exp;

  *w = eWIDE;  *h = eHIGH;

  /* xr,yr are expansion factors */
  xr = ((float) eWIDE) / cWIDE;
  yr = ((float) eHIGH) / cHIGH;
  curaspect  = xr / yr;

  /* if too narrow & shrink, shrink height.  too wide and grow, grow height */
  if ((curaspect < normaspect && !grow) || 
      (curaspect > normaspect &&  grow)) {    /* modify height */
    exp = curaspect / normaspect;
    *h = (int) (eHIGH * exp + .5);
  }

  /* if too narrow & grow, grow width.  too wide and shrink, shrink width */
  if ((curaspect < normaspect &&  grow) || 
      (curaspect > normaspect && !grow)) {    /* modify width */
    exp = normaspect / curaspect;
    *w = (int) (eWIDE * exp + .5);
  }


  /* shrink to fit screen without changing aspect ratio */
  if (*w>dispWIDE) {
    int i;
    a = (float) *w / dispWIDE;
    *w = dispWIDE;
    i = (int) (*h / a + .5);        /* avoid freaking some optimizers */
    *h = i;
  }

  if (*h>dispHIGH) {
    a = (float) *h / dispHIGH;
    *h = dispHIGH;
    *w = (int) (*w / a + .5);
  }

  if (*w < 1) *w = 1;
  if (*h < 1) *h = 1;
}

/************************************************************************/
/* following three rd_* functions swiped from xgraph, by David Harrison */
/************************************************************************/

/***********************************/
int wxImage::rd_int(char *name)
{
  /* returns '1' if successful.  result in def_int */

  if (def_str = XGetDefault(theDisp, PROGNAME, name)) {
    if (sscanf(def_str, "%ld", &def_int) == 1) return 1;
    else {
      fprintf(stderr, "wxImage: couldn't read integer value for %s resource\n", 
	      name);
      return 0;
    }
  }
  else return 0;

}

/***********************************/
int wxImage::rd_str(char *name)
{
  /* returns '1' if successful.  result in def_str */
  
  if (def_str = XGetDefault(theDisp, PROGNAME, name)) return 1;
  else return 0;

}

/***********************************/
int wxImage::rd_flag(char *name)
{
  /* returns '1' if successful.  result in def_str */
  
  if (def_str = XGetDefault(theDisp, PROGNAME, name)) {
    def_int = (strcmp(def_str, "on")==0) || 
              (strcmp(def_str, "1")==0) ||
	      (strcmp(def_str, "true")==0) ||
	      (strcmp(def_str, "yes")==0);
    return 1;
    }

  else return 0;
}

/*
 * Get a wxBitmap and wxColourMap
 */
Bool wxLoadIntoBitmap(char *filename, wxBitmap *bitmap, wxColourMap **cmap)
{
  wxImage *tempImage = new wxImage;
  if (FileExists(filename) && tempImage->Load(filename))
  {
    bitmap->Create(tempImage->eWIDE, tempImage->eHIGH, tempImage->dispDEEP);

    wxColourMap *tempColourMap = tempImage->GetColourMap();

    tempImage->Resize(tempImage->eWIDE, tempImage->eHIGH);

    wxFlushEvents();

    Display *dpy = tempImage->theDisp;
    GC gc = XCreateGC(dpy, bitmap->x_pixmap, (unsigned long )NULL, (struct XGCValues* )NULL);
    int err = XPutImage(dpy,bitmap->x_pixmap,gc,tempImage->theImage,
              0,0,
              0,0,
              tempImage->eWIDE,tempImage->eHIGH);

    XFreeGC(dpy, gc);

    bitmap->SetOk(TRUE);

    delete tempImage;

    if (cmap)
      *cmap = tempColourMap;
    else
      delete tempColourMap;
    return TRUE;
  }
  else
    return FALSE;
}

wxBitmap *wxLoadBitmap(char *s, wxColourMap **cmap)
{
  wxBitmap *bitmap = new wxBitmap;
  if (wxLoadIntoBitmap(s, bitmap, cmap))
  {
    return bitmap;
  }
  else
  {
    delete bitmap;
    return NULL;
  }
}
 

#endif // End X implementation

#ifdef wx_msw // Start MSW implementation
wxImage::wxImage(void)
{
}

wxImage::~wxImage(void)
{
}

Bool wxImage::Load(char *file)
{
  return FALSE;
}

Bool wxImage::Destroy(void)
{
  return FALSE;
}

void wxImage::Draw(wxCanvas *canvas, int x, int y, int width, int height)
{
}

void wxImage::Resize(int width, int height)
{
}

void wxImage::GetSize(int *width, int *height)
{
  *width = 0;
  *height = 0;
}

wxColourMap *wxImage::GetColourMap(void)
{
  return NULL;
}

#endif // End MSW implementation

#ifdef wx_mac // Start Macintosh implementation

// Save (device dependent) wxBitmap as a DIB
Bool wxSaveBitmap(char *filename, wxBitmap *bitmap, wxColourMap *colourmap)
{
	return FALSE;
}


Bool wxLoadPICTIntoBitmap(char *, wxBitmap *, wxColourMap **);

Bool wxLoadPICTIntoBitmap(char *fileName, wxBitmap *bm, wxColourMap **pal)
{
	FILE *fp = fopen(fileName,"rb");
	if (fp) {
		// I don't know why we skip 512 bytes. I would have
		// thought fopen only processes the data fork. I suppose
		// it could be the "Mac Draw" header block (IM-V, pg 88)
		fseek(fp, 0, SEEK_END);
		int fsize = ftell(fp) - 512;
		fseek(fp, 512, SEEK_SET);	// 0 didn't work
		PicHandle ph = (PicHandle)NewHandle(fsize);
		CheckMemOK(ph);
		int rsize = fread((char *)*ph, 1, fsize, fp);
		// width = (*ph)->picFrame.right;
		bm->SetWidth((*ph)->picFrame.right);
		// bm->height = (*ph)->picFrame.bottom;
		bm->SetHeight((*ph)->picFrame.bottom);
		// bm->depth = wxDisplayDepth();
		bm->SetDepth(wxDisplayDepth());
		GDHandle savegd;
		CGrafPtr saveport;
		GetGWorld(&saveport, &savegd);
		QDErr err;
		GWorldPtr	newGWorld;
		Rect	bounds = {0, 0, bm->GetHeight(), bm->GetWidth()};
		err = NewGWorld(&bm->x_pixmap, 0, &bounds, NULL, NULL, noNewDevice);
		if (!err) {
		  SetGWorld(bm->x_pixmap, 0);
		  DrawPicture(ph, &bounds);
		  DisposeHandle((Handle)ph);
		  SetGWorld(saveport, savegd);
		  return TRUE;
	    } else {
	      bm->x_pixmap = NULL;
	      return FALSE;
	    }
	} else
		return FALSE;
}


Bool wxLoadXPMIntoBitmap(char *fileName, wxBitmap *bm, wxColourMap **pal)
{
#if USE_XPM_IN_MAC
	return bm->LoadFile(fileName,wxBITMAP_TYPE_XPM);
#else
    wxImage *img = new(wxImage);
  	img->LoadPM(fileName, 0);	// nc arg - what does it do ??
  	// Convert the image to a Bitmap
  	delete img;
#endif
 } 

void Mac_FixupFileName(char * dest, char * src);

void Mac_FixupFileName(char * dest, char * src) {
	char *slpos = strchr(src, '/');
	char *colpos = strchr(src, ':');
	if (colpos) {
		// Assume that any colon indicates a mac path
		strcpy(dest, src);
	} else if (slpos) {
		if (slpos == src) {
			// its an abs path - can't cope that
			*dest = '\0';
			return;
		}
		// create a relative path;
		*dest++ = ':';
		int len = colpos - src;
		strncpy(dest, src, len);
		dest += len;
		*dest++ = ':';
		strcpy(dest, slpos+1);	// CJC - FIXME - really should iterate over the rest 
	} else {
		// probably just a file name
		strcpy(dest, src);
	}
}

// Load into existing bitmap;
//Bool *wxLoadIntoBitmap(char *fileName, wxBitmap *bitmap, wxColourMap **pal);

Bool wxLoadIntoBitmap(char *infile, wxBitmap *bitmap, wxColourMap **pal)
{
  Bool stats = FALSE;
  char fileName[255];
  Mac_FixupFileName(fileName, infile);
  if (wxMatchWild("*.gif",fileName)) {
    stats = wxLoadGifIntoBitmap(fileName, bitmap, pal);
  }
  else if (wxMatchWild("*.pict", fileName)) {
    stats = wxLoadPICTIntoBitmap(fileName, bitmap, pal);
  }
  else if (wxMatchWild("*.xpm", fileName)) {
	stats = wxLoadXPMIntoBitmap(fileName, bitmap, pal);
 }
  if (stats)
    return TRUE;
  else
  {
    delete bitmap;
    return 0;
  }
  return 0;
}

wxBitmap *wxLoadBitmap(char *fileName, wxColourMap **pal)
{
  Bool stats;
  wxBitmap *bitmap = new wxBitmap;

  if (wxLoadIntoBitmap(fileName, bitmap, pal))
  	return bitmap;
  else
  	return NULL;
}

 
wxImage::wxImage(void)
{
	gifpic = NULL;
	dEBUG = 0;
}

wxImage::~wxImage(void)
{
	if (gifpic)
		delete gifpic;
}

Bool wxImage::Load(char *file)
{
  gifpic = new wxGIF(file);
  if (gifpic)
  	return TRUE;
  else
  	return FALSE;
}

Bool wxImage::Destroy(void)
{
  return FALSE;
}

void wxImage::Draw(wxCanvas *canvas, int x, int y, int width, int height)
{
}

void wxImage::Resize(int width, int height)
{
}

void wxImage::GetSize(int *width, int *height)
{
  *width = 0;
  *height = 0;
}

wxColourMap *wxImage::GetColourMap(void)
{
  return NULL;
}

#endif // End Mac implementation
