/* ppmtobmp.c - read a portable pixmap and write a Microsoft BMP file.
**
** Copyright (C) 1991 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include "ppm.h"
#include "ppmcmap.h"

#define MAXVAL 255
#define MAXCOLORS 256

void
main( argc, argv )
    int argc;
    char* argv[];
    {
    FILE* ifp;
    pixel** pixels;
    register pixel* pP;
    colorhist_vector chv;
    colorhash_table cht;
    int rows, cols, row, colors, i;
    register int col;
    pixval maxval;
int taille ;
char c ;

    ppm_init( &argc, argv );

    if ( argc > 2 )
	pm_usage( "[ppmfile]" );

    if ( argc == 2 )
	ifp = pm_openr( argv[1] );
    else
	ifp = stdin;

    pixels = ppm_readppm( ifp, &cols, &rows, &maxval );
    pm_close( ifp );

    pm_message( "computing colormap..." );
    chv = ppm_computecolorhist( pixels, cols, rows, MAXCOLORS, &colors );
    if ( chv == (colorhist_vector) 0 )
	{
	pm_message(
	    "too many colors - try doing a 'ppmquant %d'", MAXCOLORS );
	exit( 1 );
	}
    pm_message( "%d colors found", colors );

    /* Write BMP header header. */
/***
printf("BITMAPFILEHEADER\n") ;
printf("Filesize 0x%x\n",0x32+colors*4+rows*cols) ;
printf("Index of bits 0x%x\n",0x32+colors*4) ;
printf("\n") ;

printf("BITMAPINFOHEADER\n") ;
printf("Width %d\n",cols) ;
printf("Height %d\n",rows) ;
printf("\n") ;

printf("RGBQUAD\n") ;
***/
printf("BM") ;
taille = 0x36+colors*4+(rows*((cols+7)/8)*8)/2 ;
putchar(taille&0xff) ;
putchar((taille>>8)&0xff) ;
putchar((taille>>16)&0xff) ;
putchar((taille>>24)&0xff) ;
putchar(0) ;
putchar(0) ;
putchar(0) ;
putchar(0) ;
taille = 0x36+colors*4 ;
putchar(taille&0xff) ;
putchar((taille>>8)&0xff) ;
putchar((taille>>16)&0xff) ;
putchar((taille>>24)&0xff) ;

putchar(0x28) ;
putchar(0) ;
putchar(0) ;
putchar(0) ;
taille = cols ;
putchar(taille&0xff) ;
putchar((taille>>8)&0xff) ;
putchar((taille>>16)&0xff) ;
putchar((taille>>24)&0xff) ;
taille = rows ;
putchar(taille&0xff) ;
putchar((taille>>8)&0xff) ;
putchar((taille>>16)&0xff) ;
putchar((taille>>24)&0xff) ;
putchar(1) ;
putchar(0) ;
putchar(4) ;
putchar(0) ;

putchar(0) ;
putchar(0) ;
putchar(0) ;
putchar(0) ;

taille = (rows*cols)/2 ;
putchar(taille&0xff) ;
putchar((taille>>8)&0xff) ;
putchar((taille>>16)&0xff) ;
putchar((taille>>24)&0xff) ;

for (taille=0;taille<4*4;taille++)
	putchar(0) ;

/*
    (void) pm_writebiglong( stdout, cols );
    (void) pm_writebiglong( stdout, rows );
    (void) putchar( (unsigned char) colors );
*/
    if ( maxval > MAXVAL )
	pm_message(
	    "maxval is not %d - automatically rescaling colors", MAXVAL );
#define FOO
#ifdef FOO
    for ( i = 0; i < colors; ++i )
	{
	pixel p;

	p = chv[i].color;
	if ( maxval != MAXVAL )
	    PPM_DEPTH( p, p, maxval, MAXVAL );
/*
	(void) putchar( (unsigned char) PPM_GETR( p ) );
	(void) putchar( (unsigned char) PPM_GETG( p ) );
	(void) putchar( (unsigned char) PPM_GETB( p ) );
printf("R %d G %d B %d\n", PPM_GETR(p),PPM_GETG(p),PPM_GETB(p)) ;
*/
	(void) putchar( (unsigned char) PPM_GETB( p ) );
	(void) putchar( (unsigned char) PPM_GETG( p ) );
	(void) putchar( (unsigned char) PPM_GETR( p ) );
	putchar(0) ;
	}
#else
#define PUT(b,v,r)	putchar(b);putchar(v);putchar(r);putchar(0) 

	PUT(0,0,0) ;
	PUT(0,0,128) ;
	PUT(0,128,0) ;
	PUT(0,128,128) ;
	PUT(128,0,0) ;
	PUT(128,0,128) ;
	PUT(128,128,0) ;
	PUT(128,128,128) ;
	PUT(172,172,172) ;
	PUT(0,0,255) ;
	PUT(0,255,0) ;
	PUT(0,255,255) ;
	PUT(255,0,0) ;
	PUT(255,0,255) ;
	PUT(255,255,0) ;
	PUT(255,255,255) ;
#endif
/*
printf("\n") ;
*/
    /* Convert color vector to color hash table, for fast lookup. */
    cht = ppm_colorhisttocolorhash( chv, colors );
    ppm_freecolorhist( chv );

/*
printf("BITS\n") ;
*/
    /* And write out the data. */
    for ( row = rows-1; row >= 0; --row )
	{
	for ( col = 0, pP = pixels[row]; col < ((cols+7)/8)*8; ++col, ++pP )
	    {
	    register int color;

	if (col<cols)
	{
	    color = ppm_lookupcolor( cht, pP );
	    if ( color == -1 )
		pm_error(
		    "color not found?!?  row=%d col=%d  r=%d g=%d b=%d",
		    row, col, PPM_GETR(*pP), PPM_GETG(*pP), PPM_GETB(*pP) );
	}
	else color = 0 ;
/*
	    (void) putchar( (unsigned char) color );
		printf("%d ",color) ;
*/
		if (col&1)
		{
			c <<= 4 ;
			c |= color ;
		    (void) putchar( (unsigned char) c );
		}
		else
			c=color ;
	    }
/*
printf("\n") ;
*/
	}
/*
printf("\n") ;
*/

    exit( 0 );
    }
