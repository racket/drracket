/*
 * xvpm.c - load routine for 'pm' format pictures
 *
 * LoadPM(fname, numcols)  -  loads a PM pic, does 24to8 code if nec.
 * WritePM(fp, pic, w, h, r,g,b, numcols, style)
 * WriteRaw(fp, pic, w, h, r,g,b, numcols, style)
 */

/*
 * Copyright 1989, 1990 by the University of Pennsylvania
 *
 * Permission to use, copy, and distribute for non-commercial purposes,
 * is hereby granted without fee, providing that the above copyright
 * notice appear in all copies and that both the copyright notice and this
 * permission notice appear in supporting documentation.
 *
 * The software may be modified for your own purposes, but modified versions
 * may not be distributed.
 *
 * This software is provided "as is" without any express or implied warranty.
 */


#include <stdlib.h>
#include "wx.h"
#include "wx_image.h"
#include "wx_imgx.h"

//#include <sys/types.h>

typedef struct {
	int	pm_id;		// Magic number for pm format files.
	int	pm_np;		// Number of planes. Normally 1.	
	int	pm_nrow;	// Number of rows. 1 - MAXNELM.		
	int	pm_ncol;	// Number of columns. 1 - MAXNELM.	
	int	pm_nband;	// Number of bands.			
	int	pm_form;	// Pixel format.			
	int	pm_cmtsize;	// Number comment bytes. Includes NULL. 
	char	*pm_image;	// The image itself.			
	char	*pm_cmt;	// Transforms performed.		
} pmpic;

pmpic  thePic;

static void flipl(byte *);

#define pm_nelm(p)	((p)->pm_ncol * (p)->pm_nrow)
#define pm_nbelm(p)	(pm_nelm(p) * (p)->pm_nband)
#define pm_psize(p)	(pm_nbelm(p) * (((p)->pm_form)&0xff))
#define pm_isize(p)	((p)->pm_np * pm_psize(p))

#define	PM_MAGICNO	0x56494557		/* Hex for VIEW */
#define PM_MAXNELM	1024
#define PM_MAXNBAND	1024
#define PM_NOSHIFT	0
#define PM_SHIFT	1

#define	PM_A		0x8000
#define	PM_C		0x8001
#define	PM_S		0x8002
#define	PM_I		0x8004
#define PM_F		0xc004

#define PM_RED 0xff
#define PM_GREEN 0xff00
#define PM_BLUE 0xff0000
#define PM_ALPHA 0xff000000
#define PM_BW 0

#define	PM_CMAX		0xff
#define PM_SMAX		0x7fff
#define	PM_IMAX		0x7fffffff
#define PM_FMAX		1.7E38

#define PM_IOHDR_SIZE	(sizeof(pmpic)-(2*sizeof(char*)))

/*

#define	pm_max(pm)	((pm)->pm_form == PM_C ? PM_CMAX :		  \
				(pm)->pm_form == PM_S ? PM_SMAX :	  \
					(pm)->pm_form == PM_I ? PM_IMAX : \
						 PM_FMAX)

#define pm_index(fm)	(((fm)&0xff)-(((fm)>>14)&01))
#define	pm_sel(fm,fn)	(*((fn)[pm_index(fm)]))
#define pm_iindex(a,nc,cr,cc)	((a)+((cr)*(nc))+(cc))
 
#define pm_npix(p)      (pm_nbelm(p) * (p)->pm_np)


typedef	struct {
	int	pm_x;		// X Position.
	int	pm_y;		// Y Position. 
	int	pm_z;		// Z Position. 
	float	pm_ux;		// Uncertainty in x.
	float	pm_uy;		// Uncertainty in y.
	float	pm_uz;		// Uncertainty in z.
} pmxyz;

#define	PM_EBASE	100
#define PM_EMALLOC	101
#define PM_EBADPARAM	102
#define PM_EBADPIC	103
#define PM_EBADFORM	104
#define PM_EBADMAGIC	105
#define PM_ENULLPIC	106    // picture given was NULL
#define PM_EBADPLANES   107  // invalid # of planes chosen for format
#define PM_EBADBANDS 	108 // invalid # of bands chosen for format
#define PM_EBADSIZE 	109 // # of rows/cols and x offsets, y offsets
			   // too big for ikonas
#define PM_EBADCOLORS 	110 // invalid number of colors chosen for format
#define PM_EBADCOLORPLANE 111 // invalid color plane entered


#define PM_NERROR	12
#define PM_ERROR(e)	(((e) < PM_EBASE || (e) > (PM_EBASE + PM_NERROR)) ? \
				0 : (e) - PM_EBASE)

extern char	*pm_errmsg[];


pmpic	*pm_add();
pmpic	*pm_addcmt();
pmpic	*pm_alloc();
pmpic	*pm_and();
pmpic	*pm_bthresh();
pmpic	*pm_cast();
char	*pm_cmt();
pmpic	*pm_conv();
pmpic	*pm_ebadform();
int	pm_getcmt();
pmpic	*pm_ikrd();
pmpic	*pm_ikwr();
pmpic	*pm_neg();
pmpic	*pm_or();
pmpic	*pm_prep();
pmpic	*pm_rdhdr();
pmpic	*pm_read();
pmpic	*pm_scale();
pmpic	*pm_sub();
pmpic	*pm_thresh();
pmpic	*pm_write();

pmxyz	*pm_centroid();
*/

/*******************************************/
int wxImage::LoadPM(char *fname, int nc)
/*******************************************/
{
  FILE  *fp;
  int    isize,i,flipit,w,h,rv;

  rv = 0;
  thePic.pm_image = NULL;

  /* read in the PM picture */
  fp=fopen(fname,"r");
//  if (!fp) return( PMError("unable to open file") );
  if (!fp) return 1;
  
  flipit = 0;
  fread(&thePic,PM_IOHDR_SIZE,1,fp);
  if (thePic.pm_id != PM_MAGICNO) {
    flipl( (byte *) &thePic.pm_id);
    if (thePic.pm_id == PM_MAGICNO) flipit = 1;
    else flipl( (byte *) &thePic.pm_id);
  }
//  if (thePic.pm_id != PM_MAGICNO) return( PMError("not a PM file") );
  if (thePic.pm_id != PM_MAGICNO) return 1;

  if (flipit) {
    flipl((byte *) &thePic.pm_np);      flipl((byte *) &thePic.pm_nrow);
    flipl((byte *) &thePic.pm_ncol);    flipl((byte *) &thePic.pm_nband);
    flipl((byte *) &thePic.pm_form);    flipl((byte *) &thePic.pm_cmtsize);
    }
          
  /* make sure that the input picture can be dealt with */
  if ( thePic.pm_nband!=1 || 
      (thePic.pm_form!=PM_I && thePic.pm_form!=PM_C) ||
      (thePic.pm_form==PM_I && thePic.pm_np>1) ||
      (thePic.pm_form==PM_C && (thePic.pm_np==2 || thePic.pm_np>4)) ) {
    fprintf(stderr,"PM picture not in a displayable format.\n");
    fprintf(stderr,"(ie, 1-plane PM_I, or 1-, 3-, or 4-plane PM_C)\n");
    return 1;
    }	

  w = thePic.pm_ncol;  h = thePic.pm_nrow;

  isize = pm_isize(&thePic);

  if (dEBUG) 
    fprintf(stderr,"wxImage: LoadPM() - loading a %dx%d %s pic, %d planes\n",
	    w, h, (thePic.pm_form==PM_I) ? "PM_I" : "PM_C", 
	    thePic.pm_np);

/*
  SetISTR(ISTR_FORMAT,"PM, %s.  (%d plane %s)  (%d bytes)",
	  (thePic.pm_form==PM_I || thePic.pm_np>1) ? 
	     "24-bit color" : "8-bit greyscale",
	  thePic.pm_np, (thePic.pm_form==PM_I) ? "PM_I" : "PM_C",
	  isize + PM_IOHDR_SIZE + thePic.pm_cmtsize);
*/

  /* allocate memory for picture and read it in */
  thePic.pm_image = (char *) malloc(isize);
  if (thePic.pm_image == NULL)
    return 1;
//    return( PMError("unable to malloc PM picture") );

  if (fread(thePic.pm_image, (unsigned) isize, 1, fp) != 1) 
    return 1;
//    return( PMError("file read error") );
  if (fp!=stdin) fclose(fp);

  if (dEBUG) fprintf(stderr,"loadpm 1\n");

  /* convert PM picture to 'pic' (8 bit) format */
  if (thePic.pm_form == PM_I) {
    int  *intptr;
    byte *pic24, *picptr;

    if ((pic24 = (byte *) malloc(w*h*3))==NULL) 
      return 1;
//      return( PMError("unable to malloc 24-bit picture") );
      
    intptr = (int *) thePic.pm_image;
    picptr = pic24;

    if (flipit) {    /* if flipit, integer is RRGGBBAA instead of AABBGGRR */
      for (i=w*h; i>0; i--, intptr++) {
	*picptr++ = (*intptr>>24) & 0xff;
	*picptr++ = (*intptr>>16) & 0xff;
	*picptr++ = (*intptr>>8)  & 0xff;
      }
    }
    else {
      for (i=w*h; i>0; i--, intptr++) {
	*picptr++ = (*intptr)     & 0xff;
	*picptr++ = (*intptr>>8)  & 0xff;
	*picptr++ = (*intptr>>16) & 0xff;
      }
    }

    if (dEBUG) fprintf(stderr,"loadpm 2\n");

    free(thePic.pm_image);
    rv=Conv24to8(pic24,w,h,nc);
    free(pic24);
  }


  else if (thePic.pm_form == PM_C && thePic.pm_np>1) {
    byte *pic24, *picptr, *rptr, *gptr, *bptr;

    if ((pic24 = (byte *) malloc(w*h*3))==NULL)
      return 1;
//      return( PMError("unable to malloc 24-bit picture") );

    rptr = (byte *) thePic.pm_image;
    gptr = rptr + w*h;
    bptr = rptr + w*h*2;
    picptr = pic24;
    for (i=w*h; i>0; i--) {
      *picptr++ = *rptr++;
      *picptr++ = *gptr++;
      *picptr++ = *bptr++;
    }
    free(thePic.pm_image);
    rv=Conv24to8(pic24,w,h,nc);
    free(pic24);
  }
  
  else if (thePic.pm_form == PM_C && thePic.pm_np==1) {
    /* don't have to convert, just point pic at thePic.pm_image */
    pic = (byte *) thePic.pm_image;
    pWIDE = w;  pHIGH = h;  
    for (i=0; i<256; i++) r[i]=g[i]=b[i]=i;  /* and build mono colortable */
    rv = 0;
  }

  return rv;
}


/*******************************************/
int wxImage::WritePM(FILE *fp, byte *pic, int w, int h, byte *rmap, byte *gmap, byte *bmap, int numcols, int colorstyle)
{
  /* writes a PM file to the already open stream
     'colorstyle' single-handedly determines the type of PM pic written
     if colorstyle==0, (Full Color) a 3-plane PM_C pic is written
     if colorstyle==1, (Greyscal) a 1-plane PM_C pic is written
     if colorstyle==0, (B/W stipple) a 1-plane PM_C pic is written */

  char  foo[256];
  int   i;
  byte *p;

  /* create 'comment' field */
  sprintf(foo,"created by wxImage\n");

  /* fill in fields of a pmheader */
  thePic.pm_id = PM_MAGICNO;
  thePic.pm_np = (colorstyle==0) ? 3 : 1;
  thePic.pm_ncol = w;
  thePic.pm_nrow = h;
  thePic.pm_nband = 1;
  thePic.pm_form  = PM_C;
  thePic.pm_cmtsize = strlen(foo);

  if (fwrite(&thePic, PM_IOHDR_SIZE, 1, fp) != 1) return -1;

  /* write the picture data */
  if (colorstyle == 0) {         /* 24bit RGB, organized as 3 8bit planes */
    for (i=0,p=pic; i<w*h; i++, p++)
      putc(rmap[*p], fp);
    for (i=0,p=pic; i<w*h; i++, p++)
      putc(gmap[*p], fp);
    for (i=0,p=pic; i<w*h; i++, p++)
      putc(bmap[*p], fp);
  }

  else if (colorstyle == 1) {    /* GreyScale: 8 bits per pixel */
    byte rgb[256];
    for (i=0; i<numcols; i++) rgb[i] = MONO(rmap[i],gmap[i],bmap[i]);
    for (i=0, p=pic; i<w*h; i++, p++)
      putc(rgb[*p],fp);
  }

  else /* (colorstyle == 2) */ { /* B/W stipple.  pic is 1's and 0's */
    for (i=0, p=pic; i<w*h; i++, p++)
      putc(*p ? 255 : 0,fp);
  }

  if (fputs(foo,fp)==EOF) return -1;

  return 0;
}


/*****************************/
static void flipl(byte *p)
{
  byte t; 
  t = p[0];  p[0]=p[3];  p[3] = t;
  t = p[1];  p[1]=p[2];  p[2] = t;
}



