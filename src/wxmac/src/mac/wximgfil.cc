// mostly from imagetest at skye.aiai with some (power) macintrash additions
// platform/compiler changes and other dress up (mistakes) by me
// LjB birk@moonface.com for macintosh version of Majestic, (another HTML viewer
// with a built in search engine capability)
// this stuff is meant to make it a little easier to use graphic files
// along with resources and needs to be used with the modified _gdi, and _dccanx
// files for macs at aiai
// also may require GUSI
// also only works with color and only slightly tested on powermacs
// not responsible for what this might do to your macintosh, use at your own risk
// sorry I do not have time to add more comments, but hope this is of some use

#include "wx_gdi.h"
#include "wx_canvs.h"
#include "wx_dc.h"
#include "wx_mtxt.h"
#include "wx_utils.h"
#include "wx_mac_utils.h"
#ifndef GUSI
#include <unix.h>
#endif
#include <string.h>
//#include <sys/types.h>

#include "wximgfil.h"

#define INTERLACEMASK 0x40

#include <palettes.h>

//void MacFixupPixelData(unsigned char *, int );
void CreateOffScreenPixMap (CGrafPtr *,wxGIF *gif);
CTabHandle XlateColorMap(wxGIF *);

#pragma pack(1) Yes
static struct {           
  char header[6];       
  ushort scrwidth;
  ushort scrheight;
  char pflds;
  char bcindx;
  char pxasrat;
} dscgif;

static struct {
  byte sep;
  ushort l;
  ushort t;
  ushort w;
  ushort h;
  byte  pf;
} image;

static struct { 
  ushort colres;
  ushort sogct;
  rgb paleta[256];
} TabCol;

#pragma pack No


static long Code_Mask[] = {
	  0,
	  0x0001, 0x0003,
	  0x0007, 0x000F,
	  0x001F, 0x003F,
	  0x007F, 0x00FF,
	  0x01FF, 0x03FF,
	  0x07FF, 0x0FFF
	  };

Ptr wxGIF::GetRawImage()
{
  return (Ptr) lpbi;
}

wxGIF::~wxGIF()
{
  delete ColourMap;
}

wxGIF::wxGIF( char * path)
{
  navail_bytes = nbits_left = 0;
  lpbi = NULL;
  for (ushort i=0; i<13; i++) code_mask[i] = Code_Mask[i];
  if (path) {
    fp = fopen(path,"rb");
    if (!fp)
    	return;
    ReadHeader(fp);
    Create(image.w, image.h, 8);
    if (GetRawImage() != 0)
      Extrae_imagen();
  }
  fclose(fp);
}

void wxGIF::Create(ushort width, ushort height, ushort deep)
{
  Width = width; Height = height; Deep = deep;
  lpbi = 0;
  RawImage = 0;
  ColourMap = 0;

  lpbi = NewPtr(width*height); CheckMemOK(lpbi);
  if (lpbi == 0)
    {
      lpbi = 0; // for debug purposes
    }
  RawImage = (char HUGE *) (lpbi);
  EfeWidth = (long)(((long)Width*Deep + 31) / 32) * 4;
}

ushort wxGIF::out_line(uchar *pixels, ushort linelen)
{
  if (ItOK()) {

	 SetRow(linelen, pixels);
	 (void)ItPrev();
	 return 0;
  }
  else return -1;
}


wxColourMap *wxGIF::getColorMap()
{
	byte r[256], g[256], b[256];

	for (ushort i=0; i<TabCol.sogct ; i++) {
	  r[i] =  TabCol.paleta[i].r;
	  g[i] =  TabCol.paleta[i].g;
	  b[i] =  TabCol.paleta[i].b;
	}
	SetColourMap(TabCol.sogct,r,g,b);
	return GetColourMap();
}

BOOL wxGIF::ReadHeader( FILE *fp)
{
	unsigned char tstA[256];
	unsigned char *rgbTable, single;
	ushort widlow, widhigh, hgtlow, hgthi, i;

  fread((char*)&tstA[0],13,1,fp);

// what a standard, numbers need to be read in backwards ????
//  fread((char*)&dscgif,sizeof(dscgif),1,f); // not portable
  ushort index = 0;
  for ( i=0; i<6;i++)
    dscgif.header[i] = tstA[index++];
  widlow = (ushort) tstA[index++];
  widhigh = ((ushort) tstA[index++]) * 256;
  hgtlow = (ushort)  tstA[index++];
  hgthi = ((ushort) tstA[index++]) * 256;
  dscgif.scrwidth = widlow + widhigh;
  dscgif.scrheight = hgtlow + hgthi;
  dscgif.pflds = tstA[index++];
  dscgif.bcindx = tstA[index++];
  dscgif.pxasrat = tstA[index++];

  if (strncmp(dscgif.header,"GIF8",3)!=0)
    return FALSE;

  TabCol.sogct = 1<<((dscgif.pflds & 0x0007) + 0x0001);
  TabCol.colres = (dscgif.pflds>>6) & 7 + 1;
  rgbTable = new unsigned char[3*TabCol.sogct];
  int errcnt = fread((char *)rgbTable,1, 3*TabCol.sogct,fp);
  unsigned char *tp = rgbTable;
  for ( i=0 ; i<TabCol.sogct; i++) {
    TabCol.paleta[i].r = *tp++;
    TabCol.paleta[i].g = *tp++;
    TabCol.paleta[i].b = *tp++;
  }
  delete [] rgbTable;

 single = 0;
  while (1) {
    fread((char *)&single, 1, 1, fp);
	if (single == 0x21) { /* extension */
	  fread(&single,1,1,fp); /* function */
	  while (1) {
        fread(&single,1,1,fp); /* block size */
		if (single)
          fread((char*)&tstA[0],single,1,fp);
		else
		  break;
	  }
	} else
	  break;
  }
 

//	 fread((char*)&image,sizeof(image),1,f); // not portable to other compilers
  fread((char*)&tstA[0],9,1,fp);
  index = 0;
  image.sep = single;
  /* CJC MW7 68K fails
  image.l = tstA[index++] + (256 * tstA[index++]);
  image.t = tstA[index++] + (256 * tstA[index++]);
  image.w = tstA[index++] + (256 * tstA[index++]);
  image.h = tstA[index++] + (256 * tstA[index++]);
  image.pf = tstA[index++];
  */
  image.l = tstA[index++];
  image.l += (256 * tstA[index++]);
  image.t = tstA[index++];
  image.t += (256 * tstA[index++]);
  image.w = tstA[index++];
  image.w += (256 * tstA[index++]);
  image.h = tstA[index++];
  image.h += (256 * tstA[index++]);
  image.pf = tstA[index++];
  return TRUE;
}

BOOL wxGIF::Extrae_imagen()
{
  (void)getColorMap();
  ibf=GIFBUFTAM+1;
  IterImage = RawImage; 
  ItCount = 0; 
  decoder(GetWidth());
//  MacFixupPixelData( (unsigned char *) IterImage, Width*Height);
  return TRUE;
}

wxGIF::wxGIF()
{
}
/* This function initializes the decoder for reading a new image.
 */
ushort wxGIF::init_exp(ushort size)
	{
	curr_size = size + 1;
	top_slot = 1 << curr_size;
	clear = 1 << size;
	ending = clear + 1;
	slot = newcodes = ending + 1;
	navail_bytes = nbits_left = 0;
	return(0);
	}

ushort wxGIF::get_byte() {

	 if (ibf>=GIFBUFTAM) {
		fread(buf,GIFBUFTAM,1,fp);
		ibf = 0;
	 }

	 return buf[ibf++];
  }

/* get_next_code()
 * - gets the next code from the GIF file.  Returns the code, or else
 * a negative number in case of file errors...
 */
ushort wxGIF::get_next_code()
	{
	ushort i, x;
	ulong ret;

	if (nbits_left == 0)
      {
		if (navail_bytes <= 0)
			{

         /* Out of bytes in current block, so read next block
          */
         pbytes = byte_buff;
         if ((navail_bytes = get_byte()) < 0)
            return(navail_bytes);
         else if (navail_bytes)
	    {
				for (i = 0; i < navail_bytes; ++i)
               {
               if ((x = get_byte()) < 0)
						return(x);
					byte_buff[i] = x;
					}
            }
			}
      b1 = *pbytes++;
		nbits_left = 8;
		--navail_bytes;
      }

   ret = b1 >> (8 - nbits_left);
   while (curr_size > nbits_left)
      {
      if (navail_bytes <= 0)
         {

         /* Out of bytes in current block, so read next block
			 */
         pbytes = byte_buff;
			if ((navail_bytes = get_byte()) < 0)
            return(navail_bytes);
			else if (navail_bytes)
            {
            for (i = 0; i < navail_bytes; ++i)
					{
					if ((x = get_byte()) < 0)
                  return(x);
               byte_buff[i] = x;
               }
            }
         }
      b1 = *pbytes++;
      ret |= b1 << nbits_left;
		nbits_left += 8;
		--navail_bytes;
      }
	nbits_left -= curr_size;
	ret &= code_mask[curr_size];
	return((ushort)(ret));
   }

void wxGIF::InitInterlaceRow(int linewidth)
{
  int linepos;
  int count, lpos;
  
  lpos = ItCount;
  
  count = ((Height - 1) / 8);
  if (lpos <= count) {
    IterImage = RawImage + (linewidth * (lpos * 8));
  } else {
    lpos -= (count + 1);
    count = ((Height - 5) / 8);
    if (lpos <= count) {
      IterImage = RawImage + (linewidth * (lpos * 8 + 4));
    } else {
      lpos -= (count + 1);
      count = ((Height - 1) / 4);
      if (lpos <= count) {
        IterImage = RawImage + (linewidth * (lpos * 4 + 2));
      } else {
	lpos -= (count + 1);
        IterImage = RawImage + (linewidth * (lpos * 2 + 1));
      }
    }
  }
}


/* short decoder(linewidth)
 *    short linewidth;               * Pixels per line of image *
 */

ushort  wxGIF::decoder(ushort linewidth)
{
  register uchar *sp, *bufptr;
  uchar *buf;
  register ushort code, fc, oc, bufcnt;
  ushort c, size, ret;

	/* Initialize for decoding a new image...
    */
  if ((size = get_byte()) < 0)
    return(size);
  if (size < 2 || 9 < size)
    return(BAD_CODE_SIZE);
  init_exp(size);
  oc = fc = 0;
  ret = 0;

  if ((buf = new uchar[linewidth + 1]/*(uchar *)malloc(linewidth + 1)*/) == NULL)
    return(OUT_OF_MEMORY);

  sp = stack;
  bufptr = buf;
  bufcnt = linewidth;

  int interlace = image.pf & INTERLACEMASK;

   /* This is the main loop.  For each code we get we pass through the
    * linked list of prefix codes, pushing the corresponding "character" for
    * each code onto the stack.  When the list reaches a single "character"
    * we push that on the stack too, and then start unstacking each
    * character for output in the correct order.  Special handling is
    * included for the clear code, and the whole thing ends when we get
    * an ending code.
    */
  while ((c = get_next_code()) != ending)
  {

      /* If we had a file error, return without completing the decode
		 */
    if (c < 0)
    {
      delete buf;
      return(0);
     }

		/* If the code is a clear code, reinitialize all necessary items.
		 */
    if (c == clear)
    {
       curr_size = size + 1;
       slot = newcodes;
       top_slot = 1 << curr_size;

			/* Continue reading codes until we get a non-clear code
			 * (Another unlikely, but possible case...)
			 */
      while ((c = get_next_code()) == clear) ;

         /* If we get an ending code immediately after a clear code
          * (Yet another unlikely case), then break out of the loop.
          */
      if (c == ending)
        break;

         /* Finally, if the code is beyond the range of already set codes,
          * (This one had better NOT happen...  I have no idea what will
          * result from this, but I doubt it will look good...) then set it
          * to color zero.
          */
      if (c >= slot)
        c = 0;

      oc = fc = c;

         /* And let us not forget to put the char into the buffer... And
			 * if, on the off chance, we were exactly one pixel from the end
          * of the line, we have to send the buffer to the out_line()
          * routine...
          */
      *bufptr++ = c;
      if (--bufcnt == 0)
      {
        if (ItCount < Height)
        {
          if (interlace) InitInterlaceRow(linewidth);
          for (ushort i=0; i<linewidth; i++) *IterImage++ = buf[i];
          ItCount++;
        }
        else
        {
          delete buf;
          return(ret);
        }
        bufptr = buf;
        bufcnt = linewidth;
      }
    }
    else
    {

         /* In this case, it's not a clear code or an ending code, so
          * it must be a code code...  So we can now decode the code into
          * a stack of character codes. (Clear as mud, right?)
          */
      code = c;

         /* Here we go again with one of those off chances...  If, on the
          * off chance, the code we got is beyond the range of those already
          * set up (Another thing which had better NOT happen...) we trick
			 * the decoder into thinking it actually got the last code read.
          * (Hmmn... I'm not sure why this works...  But it does...)
			 */
      if (code >= slot)
      {
        if (code > slot)
          ++bad_code_count;
        code = oc;
        *sp++ = fc;
       }

         /* Here we scan back along the linked list of prefixes, pushing
          * helpless characters (ie. suffixes) onto the stack as we do so.
			 */
        while (code >= newcodes)
        {
          *sp++ = suffix[code];
          code = prefix[code];
        }

         /* Push the last character on the stack, and set up the new
          * prefix and suffix, and if the required slot number is greater
			 * than that allowed by the current bit size, increase the bit
          * size.  (NOTE - If we are all full, we *don't* save the new
          * suffix and prefix...  I'm not certain if this is correct...
          * it might be more proper to overwrite the last code...
			 */
         *sp++ = code;
         if (slot < top_slot)
         {
           suffix[slot] = fc = code;
           prefix[slot++] = oc;
           oc = c;
         }
        if (slot >= top_slot)
          if (curr_size < 12)
          {
            top_slot <<= 1;
            ++curr_size;
          } 

         /* Now that we've pushed the decoded string (in reverse order)
          * onto the stack, lets pop it off and put it into our decode
          * buffer...  And when the decode buffer is full, write another
			 * line...
          */
         while (sp > stack)
         {
            *bufptr++ = *(--sp);
            if (--bufcnt == 0)
            {
              if (ItCount < Height)
              {
                if (interlace) InitInterlaceRow(linewidth);
                for (ushort i=0; i<linewidth; i++) *IterImage++ = buf[i];
                ItCount++;
              }
              else
              {
                delete buf;
                return(ret);
              }
              bufptr = buf;
              bufcnt = linewidth;
            }
          }
        }
      }
  ret = 0;
  if (bufcnt != linewidth)
      if (ItCount < Height)
      {
        if (interlace) InitInterlaceRow(linewidth);
        for (ushort i=0; i<(linewidth - bufcnt); i++) *IterImage++ = buf[i];
        ItCount++;
       }
       else
      {
        delete buf;
        return(ret);
      }
  bufptr = buf;
  bufcnt = linewidth;
  delete buf;
  return(ret);
}

//// these were all inline 

//inline 
void wxGIF::reset() 
{
 IterImage = RawImage; 
 ItCount = 0; 
}

//inline 
void wxGIF::upset()
{
  ItCount = Height-1;
  IterImage = RawImage + EfeWidth*(Height-1);
}

//inline 
Bool wxGIF::ItNext()
{
  if (ItCount++ >= Height) return 0;
  IterImage += EfeWidth;
  return 1;
}

//inline 
Bool wxGIF::ItPrev()
{
  if (ItCount-- < 0) return 0;
  IterImage -= EfeWidth;
  return 1;
}

//inline 
void wxGIF::SetRow(ushort n, byte *buf)
{
  for (ushort i=0; i<n; i++) IterImage[i] = buf[i];
}

//inline 
void wxGIF::GetRow(ushort n, byte *buf)
{
  for (ushort i=0; i<n; i++) buf[i] = IterImage[i];
}

///// end inliners


BOOL wxGIF::SetColourMap(ushort n, byte *r, byte *g, byte *b)
{
  ColourMap = new wxColourMap();
  if (ColourMap)
  {
// temporary fix
    numcmapentries = n;
    for (int i=0; i < n; i++) {
      red[i] = r[i];
      green[i] = g[i];
      blue[i] = b[i];
    }
    //ColourMap->Create(n, r, g, b);
    return TRUE;
  }
  return FALSE;
}




CTabHandle XlateColorMap(wxGIF *gif)
{

CTabHandle		gDirClut;
int i;

	gDirClut = (CTabHandle)NewHandleClear(8 + (256 * 8));
	CheckMemOK(gDirClut);

	if (gDirClut) {

		i = GetCTSeed();
		(*gDirClut)->ctSeed = i;
       (*gDirClut)->ctFlags = (0x8000); // mac-magic
       (*gDirClut)->ctSize = gif->numcmapentries - 1;


		for (i = 0; i < gif->numcmapentries; ++i) {
			(*gDirClut)->ctTable[i].value = i;
			(*gDirClut)->ctTable[i].rgb.red = 256 *gif->red[i];
			(*gDirClut)->ctTable[i].rgb.green = 256 *gif->green[i];
			(*gDirClut)->ctTable[i].rgb.blue = 256 *gif->blue[i];
        }
			
  }
  else
    return 0;
    
  return gDirClut;
// caller must
//	DisposeCTable(gDirClut);
//	gDirClut = 0;

}


//void MacFixupPixelData(unsigned char *buf, int nBytes)
//{
//	while (nBytes--) {
//		*buf += 0;
//		++buf;
//	}
//}

// Load device independent bitmap into device dependent bitmap
//wxBitmap *wxLoadBitmap(char *filename, wxColourMap **colourmap);


Bool wxLoadGifIntoBitmap(char *fileName, wxBitmap *bm, wxColourMap **pal)
{
 
  CGrafPtr  colorPort;

  wxGIF *gifImage  = new wxGIF(fileName);
  if (gifImage && gifImage->GetRawImage() != 0) {
	CreateOffScreenPixMap(&colorPort, gifImage);
	bm->x_pixmap = colorPort;
	//  bm->pixmap = colorPort->portPixMap;
	bm->SetWidth(gifImage->GetWidth());
	bm->SetHeight(gifImage->GetHeight());
	bm->SetDepth(gifImage->GetDeep());
	bm->SetOk(TRUE);
	delete gifImage;
	return TRUE;
  }
  else {
	bm->SetOk(FALSE);
  	return FALSE;
  }
}


void CreateOffScreenPixMap (CGrafPtr *cport, wxGIF *gif)
{
	CTabHandle	thisColorTable;
  GDHandle	gThisGDevice;
  GDHandle	oldDevice;
	CGrafPtr	newCGrafPtr;
	Ptr			theseBits;
	long		sizeOfOff, offRowBytes;
	OSErr		theErr;
	ushort		thisDepth;
    Rect        theRect;
CTabHandle		gDirClut;
PaletteHandle dstPalette;
 	
	gThisGDevice = GetMainDevice();
	oldDevice = GetGDevice();
	SetGDevice(gThisGDevice);
	newCGrafPtr = (CGrafPtr)NewPtrClear(sizeof(CGrafPort));
	if (newCGrafPtr != 0L)
	{
		SetRect(&theRect, 0, 0,gif->GetWidth(), gif->GetHeight());
		OpenCPort(newCGrafPtr);
		thisDepth = (**(*newCGrafPtr).portPixMap).pixelSize;
		//offRowBytes = ((((long)thisDepth * 
				//(long)(theRect.right - theRect.left)) + 15L) >> 4L) << 1L;
	    offRowBytes = gif->GetWidth();
		sizeOfOff = (long)(theRect.bottom - theRect.top) * offRowBytes;
		OffsetRect(&theRect, -theRect.left, -theRect.top);
		if (gif->GetRawImage() != 0L)
		{
			(**(*newCGrafPtr).portPixMap).baseAddr = gif->GetRawImage();
			(**(*newCGrafPtr).portPixMap).rowBytes = (short)offRowBytes | 0x8000; // ****0x8000 | gif->GetWidth();
			(**(*newCGrafPtr).portPixMap).pixelSize = 8;
			(**(*newCGrafPtr).portPixMap).cmpSize = 8;
			(**(*newCGrafPtr).portPixMap).cmpCount = 1;
			(**(*newCGrafPtr).portPixMap).bounds = theRect;
			//SetRect(&(**(*newCGrafPtr).portPixMap).bounds, 0, 0,
			//            gif->GetWidth(), gif->GetHeight()); // *theRect
			
			//thisColorTable = (**(**gThisGDevice).gdPMap).pmTable;
			//theErr = HandToHand((Handle *)&thisColorTable);
			gDirClut = XlateColorMap(gif);
			(**(*newCGrafPtr).portPixMap).pmTable = gDirClut; 
			//dstPalette = NewPalette(256, gDirClut, pmTolerant + pmExplicit, 0);
			//NSetPalette((GrafPort *)newCGrafPtr, dstPalette, pmAllUpdates);
			//ActivatePalette((GrafPort *)newCGrafPtr);
			ClipRect(&theRect);
			RectRgn(newCGrafPtr->visRgn, &theRect);
			ForeColor(blackColor);
			BackColor(whiteColor);
			//EraseRect(&theRect);
		}
		else
		{
			CloseCPort(newCGrafPtr);		
			DisposePtr((Ptr)newCGrafPtr);
			newCGrafPtr = 0L;
		}
	}

  SetGDevice(oldDevice);
  *cport = newCGrafPtr;
}
#ifdef CJC
#include <PictUtils.h>
#include <Strings.h>
#include <Resources.h>

PixMapHandle CreatePixMapFromPict(char *, CGrafPtr *, int *, int *);
PixMapHandle CreatePixMapFromPict(char * kPictID, CGrafPtr *cport, int *w, int *h)
{
  CGrafPtr	myCGrafPtr;
  GrafPtr		myGrafPtr;
  int theDepth;
  long  offRowBytes,sizeOfOff;
  PicHandle	aPict;
  PictInfo	pictInfo;
  OSErr		myErr;
  Rect		bounds;
  PicHandle	drawPict;
  GDHandle	gThisGDevice;
  GDHandle	oldDevice;
  CTabHandle	thisColorTable;
  Ptr  mypiximage0;
  PixMapHandle  pixm;
  StringPtr pstr;
  
  gThisGDevice = GetMainDevice();
  oldDevice = GetGDevice();
  SetGDevice(gThisGDevice);
  pstr = c2pstr(kPictID);
  aPict = (PicHandle) GetNamedResource('PICT', pstr);

  HLock((Handle)aPict);
  bounds = (*aPict)->picFrame;
  HUnlock((Handle)aPict);
  OffsetRect(&bounds, -bounds.left, -bounds.top);
  *w = bounds.right - bounds.left;
  *h = bounds.bottom - bounds.top;

  // color graf port
  // must draw into an offscreen, otherwise it appears on screen	
  myCGrafPtr= (CGrafPtr)NewPtrClear(sizeof(CGrafPort));
  CheckMemOK(myCGrafPtr);
  OpenCPort(myCGrafPtr);
  theDepth=(**(*myCGrafPtr).portPixMap).pixelSize;
  offRowBytes=(((theDepth * (*w))+15)>>5)<<2;
  sizeOfOff=(long)(*h) *offRowBytes;
  mypiximage0=NewPtr(sizeOfOff); CheckMemOK(mypiximage0);
  (**(*myCGrafPtr).portPixMap).rowBytes=offRowBytes+0x8000;
  (**(*myCGrafPtr).portPixMap).bounds=bounds;	
  (**(*myCGrafPtr).portPixMap).baseAddr=mypiximage0;
  
  // color map
  thisColorTable = (**(**gThisGDevice).gdPMap).pmTable;
  myErr = HandToHand((Handle *)&thisColorTable);
  (**(*myCGrafPtr).portPixMap).pmTable = thisColorTable; 
  ClipRect(&bounds);
  RectRgn(myCGrafPtr->visRgn, &bounds);
  ForeColor(blackColor);
  BackColor(whiteColor);
  HLock((Handle)aPict);
  DrawPicture(aPict, &bounds);
  HUnlock((Handle)aPict);
  SetGDevice(oldDevice);
  ReleaseResource((Handle)aPict);

  *cport = myCGrafPtr;
  return myCGrafPtr->portPixMap; // pixm;
  
}

// not used for now
void LoadGraphic (ushort );
void LoadGraphic (ushort thePictID)
{
	Rect		bounds;
	PicHandle	thePicture;
	
	thePicture = GetPicture(thePictID);			// Load graphic from resource fork.
	HLock((Handle)thePicture);					// If we made it this far, lock handle.
	bounds = (*thePicture)->picFrame;			// Get a copy of the picture's bounds.
	HUnlock((Handle)thePicture);				// We can unlock the picture now.
	OffsetRect(&bounds, -bounds.left, -bounds.top);	// Offset bounds rect to (0, 0).
	DrawPicture(thePicture, &bounds);			// Draw picture to current port.
	ReleaseResource((Handle)thePicture);		// Dispose of picture from heap.
}
#endif
