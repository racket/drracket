
#include "simpledrop.h"

#ifdef FOR_MRED
# define DEST_CREATOR 'MrEd'
# define PROGNAME "MrEd"
#else
# define DEST_CREATOR 'MzSc'
# define PROGNAME "MzScheme"
#endif

int strlen(char *s);
int isspace(int c);
char *scheme_build_mac_filename(FSSpec *spec, int given_dir);

static char *protect_arg(char *s)
{
  char *p, *r, *q;
  int l;
  int need_quote = 0;
  int tick_count = 0;

  for (p = s, l = 0; *p; p++, l++) {
    if (*p == '\'')
      tick_count++;
    else if (isspace(*p) || *p == '"')
      need_quote = 1;
  }

  if (!need_quote && !tick_count)
    return s;

  r = NewPtr(l + 4 * tick_count + 3);
  p = r;
  *(p++) = '\'';
  for (q = s; *q; q++) {
    if (*q == '\'') {
      *(p++) = '\'';
      *(p++) = '"';
      *(p++) = '\'';
      *(p++) = '"';
      *(p++) = '\'';
    } else
      *(p++) = *q;
  }
  *(p++) = '\'';
  *p = 0;

  return r;
}

int main(void)
{
  CInfoPBRec pbrec;
  WDPBRec  wdrec;
  FSSpec dir;
  short find_position = 0;
  
  MaxApplZone();
	
  InitGraf(&qd.thePort);		/* initialize Mac stuff */
  InitFonts();
  InitWindows();
  InitMenus();
  TEInit();
  InitDialogs(NULL);
  
  MoreMasters();
  MoreMasters();
  
  wdrec.ioNamePtr = dir.name;
  if (PBHGetVol(&wdrec, 0))
    return 0;
  
  dir.vRefNum = wdrec.ioWDVRefNum;
  dir.parID = wdrec.ioWDDirID;
  
  /* Find app: */
  while (1) {
    pbrec.hFileInfo.ioVRefNum = dir.vRefNum;
    pbrec.hFileInfo.ioDirID = dir.parID;
    pbrec.hFileInfo.ioFDirIndex = find_position + 1;
    pbrec.hFileInfo.ioNamePtr = dir.name;
    if (PBGetCatInfo(&pbrec, 0) || !*dir.name)
      break;
    
    find_position++;

    if (pbrec.hFileInfo.ioFlFndrInfo.fdType == 'APPL') {
      if (pbrec.hFileInfo.ioFlFndrInfo.fdCreator == DEST_CREATOR) {
        /* Found it: */
        LaunchParamBlockRec rec;
        AppParameters *app;
        int i, j, storedlen, givenlen;
        short file;
        char *a, *b, buffer[1024];
        unsigned char *reason;
        short rsrcRef_ignored;
        Handle appParam_ignored;
        Str255 ourName;
        int argc;
        char **argv;
        ProcessSerialNumber psn;
        
        psn.highLongOfPSN = 0;
        psn.lowLongOfPSN = kNoProcess;

	/* Is it already running? */
	while (!GetNextProcess(&psn)) {
	  ProcessInfoRec prec;
          FSSpec got;
          Str32 name;
	  
	  prec.processInfoLength = sizeof(prec);
	  prec.processName = name;
	  prec.processAppSpec = &got;
	  GetProcessInformation(&psn, &prec);
	  if ((got.vRefNum == dir.vRefNum)
	      && (got.parID == dir.parID)
	      && (got.name[0] == dir.name[0])) {
	    int i = dir.name[0];
	    while (i--)
	      if (got.name[i + 1] != dir.name[i + 1])
	        break;
	    if (i < 0) {
	      /* It's already running: */
	      ParamText("\p" PROGNAME " is already running. \r"
                        PROGNAME " is needed to run this program. "
                        "Please quit " PROGNAME " and try again. ",
                        NULL, NULL, NULL);
              Alert(128, NULL);
              return 1;
	    }
	  }
	}
        
        /* Get command-line arguments here */
        Drop_GetArgs(&argc, &argv);

	/* Read our own data fork to get the command line: */
	GetAppParms(ourName, &rsrcRef_ignored, &appParam_ignored);
	if (HOpenDF(dir.vRefNum, dir.parID, ourName, fsRdPerm, &file)) {
	  storedlen = 0;
	} else {
	  long x = 1023;
	  FSRead(file, &x, buffer);
	  buffer[storedlen = x] = 0;
	}
		
	/* Compute size of args from command-line */
	givenlen = 0;
	for (i = 1; i < argc; i++) {
	  argv[i] = protect_arg(argv[i]);
	  givenlen += strlen(argv[i]) + 1; /* add space */
	}

        app = (AppParameters *)NewPtr(sizeof(AppParameters) + storedlen + givenlen);
        app->theMsgEvent.what = kHighLevelEvent;
        app->theMsgEvent.message = 'PLT ';
        *(long *)&app->theMsgEvent.where = 'cmdl';
        app->messageLength = storedlen + givenlen;
        a = ((char *)app) + sizeof(AppParameters);
        b = buffer;
        for (i = 0; i < storedlen; i++)
          a[i] = b[i];
        for (j = 1; j < argc; j++) {
          a[i++] = ' ';
          while (*(argv[j]))
            a[i++] = *(argv[j]++);
        }
        
        rec.launchBlockID = extendedBlock;
        rec.launchEPBLength = extendedBlockLen;
        rec.launchFileFlags = 0;
        rec.launchControlFlags = launchNoFileFlags | launchContinue | launchUseMinimum;
        rec.launchAppSpec = &dir;
        rec.launchAppParameters = app;
        
        switch (LaunchApplication(&rec)) {
	case noErr:
	  return 0;
	case memFullErr:
	  reason = "\pThere is not enough memory available.";
	  break;
	default:
	  reason = "\pIs there enough memory available?";
	  break;
        }
        
        /* Startup error */
        ParamText("\pError starting " PROGNAME ". \r"
                  PROGNAME " is needed to run this program. "
                  PROGNAME " was found but didn't start. ",
                  reason, NULL, NULL);
        Alert(128, NULL);
        return 1;
      }
    }
  }
  
  /* Didn't find it */
  ParamText("\pCould not find " PROGNAME " in the same folder. ("
	    PROGNAME " is needed to run this program.)", 
	    NULL, NULL, NULL);
  Alert(128, NULL);
}

void Drop_Runtime(char **, int)
{
}

void Drop_Quit(void)
{
}


/********** Reimplemented/copied here to save space in the binary: ********/

int strlen(char *s)
{
  int i;
  for (i = 0; s[i]; i++);
  return i;
}

int isspace(int c)
{
  return ((c == ' ') 
	  || (c == '\t')
	  || (c == '\n')
	  || (c == '\r'));
}

char *scheme_build_mac_filename(FSSpec *spec, int given_dir)
{
#define QUICK_BUF_SIZE 256

  CInfoPBRec pbrec;
  char buf[256], qbuf[QUICK_BUF_SIZE], *s;
  int i, j, size = 0, alloced = QUICK_BUF_SIZE - 1;
  int vref = spec->vRefNum;
  long dirID = spec->parID;

  s = qbuf;
  if (!given_dir) {
    for (i = spec->name[0]; i; i--)
      s[size++] = spec->name[i];
  }

  while (1)  {
    pbrec.hFileInfo.ioNamePtr = (unsigned char *)buf;
    pbrec.hFileInfo.ioVRefNum = vref;
    pbrec.hFileInfo.ioDirID = dirID;
    pbrec.hFileInfo.ioFDirIndex = -1;
    if (PBGetCatInfo(&pbrec, 0))
      return NULL;
	
    if (size + buf[0] + 1 > alloced) {
      char *old;
	   
      alloced *= 2;
      old = (char *)NewPtr(alloced + 1);
      BlockMove(s, old, size);
    }

    s[size++] = ':';
    for (i = buf[0]; i; i--)
      s[size++] = buf[i];
	  
    dirID = pbrec.dirInfo.ioDrParID;
    if (dirID == 1)
      break;
  }
  
  if (alloced < QUICK_BUF_SIZE) {
    s = (char *)NewPtr(size + 1);
    for (j = 0, i = size; i--; j++)
      s[j] = qbuf[i];
  } else {
    int save;
  	
    for (i = 0, j = size - 1; i < j; i++, j--) {
      save = s[i];
      s[i] = s[j];
      s[j] = save;
    }
  }
  
  s[size] = 0;
  return s;
}
