/*
   FIXME - This needs substantial work
   10/7/95 - This is just enough to link wxPython v2 and extend - untested and/or stubbed
   
   One day, I'll rename it to wx_utils.cc 
   
   Some (most of these functions could be based on GUSI or Mac-Python. There ought
   to be a choice. (Easier than rebuilding python to use GUSI).
*/
#include "wx.h"
#include "wx_utils.h"
#include "wxstring.h"
#include "wx_list.h"
#include <Strings.h>
#include "wx_main.h"
#include <stdarg.h>
#include <ctype.h>
#if 1
#include <stat.h>
#include <unix.h>
#include <iostream.h>
#include <Files.h>
extern "C" long atol(char *);
extern "C" int atoi(char *);
#else
#if defined(PYLIB)
extern "C" {
//	#include "MacDefs.h"
//	#include "macstat.h"
	#include <stat.h>
	#include <Files.h>
//	#define S_ISDIR(x) (x & S_IFDIR)
	#include "dirent.h"
	#include "nfullpath.h"
//	extern "C" mkdir(const char *);
	extern "C" rmdir(const char *);
	extern "C" chdir(const char *);
	extern "C" sleep(int);
	extern "C" char *getcwd(char *, int);
	extern "C" long atol(char *);
	extern "C" int atoi(char *);
}
#elif defined(GUSI)
#include "GUSI.h"
#else
#error "GUSI or PYLIB required for this Module"
#endif
#endif

#if 0
static DIR *wxDirStream = NULL;
#endif
static char *wxFileSpec = NULL;
static int wxFindFileFlags = 0;

extern "C" {
  extern char *scheme_build_mac_filename(FSSpec *spec, int given_dir);
};

// Get a temporary filename, opening and closing the file.
char *wxGetTempFileName (const char *prefix, char *dest)
{
  static char *temp_folder;
  static int temp_len;
  static short last_temp = 0;	// cache last to speed things a bit
  // At most 1000 temp files to a process! We use a ring count.
  char *buf;

  if (!temp_folder) {
    FSSpec spec;
    if (!FindFolder(kOnSystemDisk, 'temp', kCreateFolder, &spec.vRefNum, &spec.parID))
	  temp_folder = scheme_build_mac_filename(&spec, 1);
	else
	  temp_folder = "";
	temp_len = strlen(temp_folder);
  }
 
  if (!prefix)
    prefix = "";
  else {
    int i;
    for (i = 0; prefix[i]; i++)
      if (prefix[i] == ':') {
        prefix = "";
        break;
      }
    if (i > 15)
      prefix = "";
  }
  
  buf = new WXGC_ATOMIC char[temp_len + strlen(prefix) + 20];

  for (short suffix = last_temp + 1; suffix != last_temp; ++suffix %= 1000)
    {
      sprintf (buf, "%s¥%s%d", temp_folder, prefix, (int) suffix); // CJC FIXME - really should get a temp folder
      if (!wxFileExists (buf))
	{
	  // Touch the file to create it (reserve name)
	  FILE *fd = fopen (buf, "w");
	  if (fd)
	    fclose (fd);
	  last_temp = suffix;
          if (dest)
	    strcpy(dest, buf);
	  else
	    dest = copystring(buf);
	  return dest;
	}
    }
  cerr << "wxWindows: error finding temporary file name.\n";
  if (dest) dest[0] = 0;
  return NULL;
}

Bool 
wxRemoveFile (const char *file)
{
#ifdef PYLIB
  return ((remove ((char*)file) == 0) ? TRUE : FALSE);
#else
  return ((unlink ((char*)file) == 0) ? TRUE : FALSE);
#endif
}

Bool 
wxMkdir (const char *dir)
{
  // give default perms of owner read and write, group read and
  // others read. The interface to this func should be changed
  // to pass the perms info in.
  // Since directory it must also be searchable @@@
  // Added S_IXUSR | S_IXGRP | S_IXOTH
#ifdef PYLIB
  return (mkdir (dir,  0777) == 0);
#elif defined(GUSI)
  return (mkdir (dir, 0777) == 0);
#else
  return (mkdir (dir, 0777) == 0);
#endif
}

Bool 
wxRmdir (const char *dir)
{
  return (rmdir (dir) == 0);
}

/* MATTHEW: [5] Mac (use for all platforms?) */
Bool 
wxDirExists (const char *filename)
{
  struct stat stbuf;

  if (filename && (stat ((char *)filename, &stbuf) == 0))
    return !!(stbuf.st_mode & S_IFDIR);
  return FALSE;
}


// Get first file name matching given wild card.
// Flags are reserved for future use.
#if 1 || (!defined(GUSI) && !defined(PYLIB)) // i.e. - no other support lib available
static short find_vref;
static long find_dir_id;
static short find_position = -1;

char *wxFindFirstFile(const char *spec, int flags)
{
	WDPBRec  wdrec;
	CInfoPBRec pbrec;
	
	wxFindFileFlags = flags;
	
	if (wxFileSpec)
		delete[] wxFileSpec;
	wxFileSpec = copystring(spec);
	
	static char buf[400];
	
	// Find path only so we can concatenate
	// found file onto path
	char *p = wxPathOnly(wxFileSpec);
	char *n = wxFileNameFromPath(wxFileSpec);
	long dirID;
	
	/* Get current working dir */
	wdrec.ioNamePtr = (StringPtr)buf;
	if (PBHGetVol(&wdrec, FALSE))
		return NULL;
	
	find_vref = wdrec.ioWDVRefNum;
	find_dir_id = wdrec.ioWDDirID;
	
	/* p is NULL => Local directory */
	if (!p) {
		// Do nothing; find_wd and dirID are set
	} else {
		if (*p == ':') {
			p++;
		} else {
			char *vol = p, vbuf[256];
			HParamBlockRec hrec;
			/* It's an absolute path: get the Volume Name and vRefNum */
			while (*p && *p != ':')
				p++;
			if (*p)
			  *p++ = '\0';
			strcpy(vbuf, vol);
			strcat(vbuf, ":");
			hrec.volumeParam.ioNamePtr = c2pstr(vbuf);
			hrec.volumeParam.ioVRefNum = 0;
			hrec.volumeParam.ioVolIndex = -1;
			if (PBHGetVInfo(&hrec, FALSE))
				return NULL;
			find_vref = hrec.volumeParam.ioVRefNum;
			find_dir_id = 0;
		}
		while (p && *p) {
			char *next = p;
			while (*next && *next != ':')
				next++;
			if (*next)
				*(next++) = 0;
			else
				next = NULL;
			
			c2pstr(p);
			pbrec.hFileInfo.ioNamePtr = (unsigned char *)p;
			pbrec.hFileInfo.ioVRefNum = find_vref;
			pbrec.hFileInfo.ioDirID = find_dir_id;
			pbrec.hFileInfo.ioFDirIndex = 0;
			if (PBGetCatInfo(&pbrec, FALSE))
				return NULL;
			if (!(pbrec.hFileInfo.ioFlAttrib & 0x10))
				return NULL; // Not a directory
      		find_dir_id = pbrec.hFileInfo.ioDirID;
      
			p = next;
		}
	}
 
	find_position = 0;
 
	return wxFindNextFile();
}

char *wxFindNextFile(void)
{
	CInfoPBRec pbrec;
	static char buf[400];
	char file[256];
	
	/* No more files or not searching: */
	if (find_position < 0)
		return NULL;
	
	// Find path only so we can concatenate
	// found file onto path
	char *p = wxPathOnly(wxFileSpec);
	char *n = wxFileNameFromPath(wxFileSpec);
	
	while (1) {
		pbrec.hFileInfo.ioVRefNum = find_vref;
		pbrec.hFileInfo.ioDirID = find_dir_id;
		pbrec.hFileInfo.ioFDirIndex = find_position + 1;
		pbrec.hFileInfo.ioNamePtr = (unsigned char *)file;
		if (PBGetCatInfo(&pbrec, FALSE) || !*file) {
			find_position = -1;
			return NULL;
		}
		
		find_position++;
	
		// Is this the right kind of thing?
		if ((wxFindFileFlags & (wxDIR + wxFILE)) != (wxDIR + wxFILE))
			if (!!(pbrec.hFileInfo.ioFlAttrib & 0x10) != !!(wxFindFileFlags & wxDIR))
				continue;
		
		p2cstr((unsigned char *)file);
		
		if (wxMatchWild(n, file)) {
			if (p)
				strcpy(buf, p);
			else
		  		buf[0] = 0;
		  
			if (buf[0])
				strcat(buf, ":");
			strcat(buf, file);
	      
			return buf;
	    }
	}
}

#else

char *wxFindFirstFile(const char *spec, int flags)
{
  if (wxDirStream)
    closedir(wxDirStream); // edz 941103: better housekeping

  wxFindFileFlags = flags;

  if (wxFileSpec)
    delete[] wxFileSpec;
  wxFileSpec = copystring(spec);

  static char buf[400];

  // Find path only so we can concatenate
  // found file onto path
  char *p = wxPathOnly(wxFileSpec);
  char *n = wxFileNameFromPath(wxFileSpec);

  if ((wxDirStream=opendir(p))==NULL)
    return NULL;

  // Do the reading
  struct dirent *nextDir;
  for (nextDir = readdir(wxDirStream); nextDir != NULL; nextDir = readdir(wxDirStream))
  {
    if ((strcmp(nextDir->d_name, ".") == 0) ||
        (strcmp(nextDir->d_name, "..") == 0))
    {
      if (flags == wxDIR)
      {
        buf[0] = '\0';
        if (p && *p)
        {
          strcpy(buf, p);
          if (strcmp(p, ":") != 0)
            strcat(buf, ":");
        }
        strcat(buf, nextDir->d_name);
        return buf;
      }
    }
    else if (!wxIsWild(n) || wxMatchWild(n, nextDir->d_name))
    {
      buf[0] = 0;
      if (p && *p)
      {
        strcpy(buf, p);
        if (strcmp(p, ":") != 0)
          strcat(buf, ":");
      }
      strcat(buf, nextDir->d_name);
      // Check for directory
      if (flags == wxDIR)
      {
        if (wxDirExists(buf))
          return buf;
      }
      else
        return buf;
    }
  }
  closedir(wxDirStream);
  wxDirStream = NULL;
  return NULL;
}

char *wxFindNextFile(void)
{
  static char buf[400];

  // Find path only so we can concatenate
  // found file onto path
  char *p = wxPathOnly(wxFileSpec);
  char *n = wxFileNameFromPath(wxFileSpec);

  // Do the reading
  struct dirent *nextDir;
  for (nextDir = readdir(wxDirStream); nextDir != NULL; nextDir = readdir(wxDirStream))
  {
    if ((strcmp(nextDir->d_name, ".") == 0) ||
        (strcmp(nextDir->d_name, "..") == 0))
    {
      if (wxFindFileFlags == wxDIR)
      {
        buf[0] = '\0';
        if (p && *p)
        {
          strcpy(buf, p);
          if (strcmp(p, ":") != 0)
            strcat(buf, ":");
        }
        strcat(buf, nextDir->d_name);
        return buf;
      }
    }
    else if (!wxIsWild(n) || wxMatchWild(n, nextDir->d_name))
    {
      buf[0] = '\0';
      if (p && *p)
      {
        strcpy(buf, p);
        if (strcmp(p, ":") != 0)
          strcat(buf, ":");
      }
      strcat(buf, nextDir->d_name);
      // Check for directory
      if (wxFindFileFlags == wxDIR)
      {
        if (wxDirExists(buf))
          return buf;
      }
      else
        return buf;
    }
  }
  closedir(wxDirStream);
  wxDirStream = NULL;
  return NULL;
}
#endif

// Here's some Tough Ones (tm) - could always try to launch ToolServer/MacPerl/MacPython??
// Execute another program. Returns FALSE if there was an error.
Bool wxExecute(char **argv, Bool Async) 
{
	return FALSE;
}
Bool wxExecute(const char *command, Bool Async)
{
	return FALSE;
}

// Execute a command in an interactive shell window
// If no command then just the shell
Bool wxShell(const char *command)
{
	return FALSE;
}

// Get current working directory.
// If buf is NULL, allocates space using new, else
// copies into buf.
char *wxGetWorkingDirectory(char *buf, int sz)
{
  if (!buf)
    buf = new char[sz+1];
  if (getcwd(buf, sz) == NULL) {
    buf[0] = ':';
    buf[1] = '\0';
  }
  return buf;
}

Bool wxSetWorkingDirectory(char *d)
{
  return (chdir(d) == 0);
}

// Get free memory in bytes, or -1 if cannot determine amount (e.g. on UNIX)
long 
wxGetFreeMemory (void)	// CJC FIXME this could be Macintized.
{
  return -1;
}

// Sleep for nSecs seconds.
// XView implementation according to the Heller manual
void 
wxSleep (int nSecs)
{
  sleep (nSecs);
}

// Read $HOME for what it says is home, if not
// read $USER or $LOGNAME for user name else determine
// the Real User, then determine the Real home dir.
char *wxGetUserHome (const char *user)
{
  char usr[256];
  strcpy(usr, user);
  return getcwd(usr, 1000);
}



// Old cursor
static int wxBusyCursorCount = FALSE;

// Set the cursor to the busy cursor for all windows
void 
wxBeginBusyCursor (wxCursor * cursor)
{
  for (wxChildNode *node = wxTopLevelWindows(NULL)->First(); node; node = node->Next()) {
    wxFrame *f = (wxFrame *)node->Data();
    f->cBusyCursor++;
  }
    
  wxTheApp->AdjustCursor();
}

// Restore cursor to normal
void 
wxEndBusyCursor (void)
{
  for (wxChildNode *node = wxTopLevelWindows(NULL)->First(); node; node = node->Next()) {
    wxFrame *f = (wxFrame *)node->Data();
    if (f->cBusyCursor)
      --f->cBusyCursor;
  }
	
  wxTheApp->AdjustCursor();
}

// TRUE if we're between the above two calls
Bool 
wxIsBusy (void)
{
  return (wxBusyCursorCount > 0);
}

// Some additions from Louis Birk
int AddOrReplaceEntry(const char *section, const char *entry, char *Value, const char *file);
int GetEntry(const char *section, const char *entry, char *Value, const char *file);

wxNode *EntryMember (wxList *, const char *, int );
wxNode *EntryMember (wxList *list, const char *s, int length)
{
  for (wxNode * node = list->First (); node; node = node->Next ())
    {
      const char *s1 = (const char *) ((wxString *)(node->Data()))->GetData();
      if (s == s1 || strncmp (s, s1, length) == 0)
	return node;
    }
  return 0;
}

wxNode *SectionMember (wxList *, const char *, int );
wxNode *SectionMember (wxList *list, const char *s, int length)
{
  for (wxNode * node = list->First (); node; node = node->Next ())
    {
      const char *s1 = (const char *) ((wxString *)(node->Data()))->GetData();
      if (s == s1 || strncmp (s, s1, length) == 0)
	return node;
    }
  return 0;
}

wxList *GetStringList(FILE *fd);
int WriteStringList(FILE *fd, wxList *sList);

static char *wxResourceFile;
static wxList *rmain;

void wxInitResources(char *s);

void wxInitResources(char *s)
{
   wxResourceFile = s;
   
   FILE *fd;
   
   if ( (fd = fopen(s, "r+")) == 0 ) // file note there
     rmain = NULL;
   else {
    // find the section or create it
    rmain = GetStringList(fd);

    fclose(fd);
   }
}

int AddOrReplaceEntry(const char *section, const char *entry, char *Value, const char *file)
{
  FILE *fd;
  wxList *sList;
  wxNode *sNode;
  wxNode *eNode;
  char Section[256];

  if (!file)
    return FALSE;

  if ( (fd = fopen(file, "r+")) == 0 ) {
    // file not there
    if ( (fd = fopen(file, "w")) == 0 )
      return FALSE;
    sList = new wxList();
  } else {
   // find the section or create it
    sList = GetStringList(fd);
  }
  
  // make a section
  sprintf(Section, "[%s]", section);

  if ( sNode = SectionMember(sList, Section, strlen(Section)))
  {
    if ( (eNode = EntryMember(sList, entry, strlen(entry))) )
    {
      // replace with new entry
      sList->Insert(eNode, (new wxString(Value) ));

      // delete the old entry
      sList->DeleteNode(eNode); 
    }
    else // section there, entry missing, add it
    {
      // add the entry
      wxNode *afterSection = sNode->Next();

      // replace with new entry
      sList->Insert(afterSection, (new wxString(Value) ));

    }
  }
  else // section missing, add it
  {
    // add the section
    sList->Append(new wxString(Section) );
    // add the entry
    sList->Append(new wxString(Value) );
  }

  int retval;

  if (WriteStringList(fd, sList))
    retval = TRUE;
  else
    retval = FALSE;

  fclose(fd);

  return retval;
}

int GetEntry(const char *section, const char *entry, char *Value, const char *file)
{
  FILE *fd;
  wxList *sList;
  wxNode *sNode;
  wxNode *eNode;
  char Section[256];

  if (!file) {
    if (!rmain)
  	  return FALSE;
  	sList = rmain;
  } else {
    if ( (fd = fopen(file, "r+")) == 0 ) // file note there
      return FALSE;

    // find the section or create it
    sList = GetStringList(fd);

    fclose(fd);
  }

  // make a section
  sprintf(Section, "[%s]", section);

  if ( sNode = SectionMember(sList, Section, strlen(Section)))
  {
    if ( (eNode = EntryMember(sList, entry, strlen(entry))) )
    {
      const char *s1 = (const char *) ((wxString *)(eNode->Data()))->GetData();
        /* This fixes the blanks/tabs etc. in front of the Value
       	 Thomas Fettig fettig@dfki.uni-sb.de  06-dec-95 */
      int i = 1;
      char *s2 = strstr(s1, "=");
      while (isspace(s2[i]) && (s2[i] !='\0'))
       	i++;
      strcpy(Value,s2+i);
      return TRUE;
    }
   }

  return FALSE;

}

wxList *GetStringList(FILE *fd)
{
  wxList *theList = new wxList();
  char *line;
  char line2[256];

  while ( (line = fgets(line2, sizeof(line2), fd)) != 0)
  {
    int len = strlen(line);
    while (len && (line[len - 1] == '\n')) {
      line[--len] = 0;
    }
    theList->Append(new wxString(line));
  }
  if (theList->Number() > 0)
    return theList;
  else
    return 0;
}

int WriteStringList(FILE *fd, wxList *sList)
{
  int err;

  fseek(fd, 0, SEEK_SET);
  for (wxNode * node = sList->First (); node; node = node->Next ())
    {
      const char *s1 = (const char *) ((wxString *)(node->Data()))->GetData();
      if ( (err = fputs(s1, fd)) != 0)
        return FALSE;
      fputs("\n", fd);
    }
  return TRUE;

}

// Resource additions from Louis Birk - All entry names must be unique!
//

Bool wxWriteResource(const char *section, const char *entry, char *value, const char *file)
{
  char Value[256];

  sprintf(Value, "%s=%s", entry, value);

  if ( AddOrReplaceEntry(section, entry, Value, file) )
    return TRUE;
  else
    return FALSE;
}

Bool wxWriteResource(const char *section, const char *entry, float value, const char *file)
{
  char Value[256];

  sprintf(Value, "%s=%f", entry, value);

  if ( AddOrReplaceEntry(section, entry, Value, file) )
    return TRUE;
  else
    return FALSE;
}

Bool wxWriteResource(const char *section, const char *entry, long value, const char *file)
{
  char Value[256];

  sprintf(Value, "%s=%d", entry, value);

  if ( AddOrReplaceEntry(section, entry, Value, file) )
    return TRUE;
  else
    return FALSE;
}

Bool wxWriteResource(const char *section, const char *entry, int value, const char *file)
{
  char Value[256];

  sprintf(Value, "%s=%d", entry, value);

  if ( AddOrReplaceEntry(section, entry, Value, file) )
    return TRUE;
  else
    return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, char **value, const char *file)
{
  char Value[256];

  if (GetEntry(section, entry, Value, file))
  {
    char *s = copystring(Value);
    value[0] = s;
    return TRUE;
  }
  else
    return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, float *value, const char *file)
{
  char Value[256];

  if (GetEntry(section, entry, Value, file))
  {
    sscanf(Value, "%f", value);
    return TRUE;
  }
  else
    return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, long *value, const char *file)
{
  char Value[256];

  if (GetEntry(section, entry, Value, file))
  {
    value[0] = atol(Value);
    return TRUE;
  }
  else
    return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, int *value, const char *file)
{
  char Value[256];

  if (GetEntry(section, entry, Value, file))
  {
    value[0] = atoi(Value);
    return TRUE;
  }
  else
    return FALSE;
}

void wxBell()
{
	SysBeep(0);
}

int wxGetOsVersion(int *a, int *b)
{
  SysEnvRec sysEnvRec;
  ::SysEnvirons(2, &sysEnvRec);

  *a = sysEnvRec.systemVersion >> 8;
  *b = (sysEnvRec.systemVersion >> 4) & 0xF;

  return wxMACINTOSH;
}

// Output a debug mess., in a system dependent fashion.
void 
wxDebugMsg (const char *fmt...)
{
  va_list ap;
  char buffer[BUFSIZ];

  if (!wxTheApp->wantDebugOutput)
    return ;

  va_start (ap, fmt);

  vsprintf (buffer, fmt, ap);
  cerr << buffer;

  va_end (ap);
}


//------ the following should be in wx_utils.cc ----------------

// Get hostname.
Bool wxGetHostName(char *buf, int maxSize)
{	Bool good = FALSE;

	if (maxSize>9)
	{	strcpy(buf,"Macintosh");
		good = TRUE;
	}
	return good;
}

// Get user ID e.g. jacs
Bool wxGetUserId(char *buf, int maxSize)
{	return wxGetUserName(buf,maxSize); }

// Get user name e.g. Julian Smart
Bool wxGetUserName(char *buf, int maxSize)
{	Bool good = FALSE;
	unsigned long userRef;
	Str32 name;
	
	if (maxSize>32)
	{	good = GetDefaultUser( &userRef, name) == noErr;
	  /* MATTHEW: [5] */
	  if (good) {
#if defined(PPCC) || defined(MrCpp) || defined(GUSI) || defined(PYLIB)
		p2cstr(name);
#else
		PtoCstr(name);
#endif
		strcpy(buf,(char *)name);
	  }
	}
	return good;
}
