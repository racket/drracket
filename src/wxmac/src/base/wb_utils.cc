/*
 * File:        wb_utils.cc
 * Purpose:     Miscellaneous utilities
 * Author:      Julian Smart
 * Created:     1993
 * Updated:     August 1994
 * RCS_ID:      $Id: wb_utils.cc,v 1.5 1994/08/15 21:53:50 edz Exp edz $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_utils.h"
#include "wx_win.h"
#include "wx_menu.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#include <iostream.h>
#include <fstream.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if !defined(wx_mac) || defined(_powerc)
#ifndef __WATCOMC__
#include <errno.h>
#endif
#endif
#include <time.h>
#ifndef wx_mac
#include <sys/types.h>
#include <sys/stat.h>
#else	
// wx_mac needs different headers depending on which library is providing stat()
#   if defined(__SC__) || defined(_THINKC)
#	include <unix.h>
#   elif defined(PYLIB)
#	include <stat.h>
// #   elif defined(GUSI)
// #	include <GUSI.h>
#   elif defined(__MWERKS__) // && !defined(GUSI) && !defined(PYLIB)
#	include <stat.h>	// normal CW stat.h
#   else
#	include "macstat.h"	// MPW probably
#   endif
#endif

// Pattern matching code.
// Yes, this path is deliberate (for Borland compilation)
#ifdef wx_mac /* MATTHEW: [5] Mac doesn't like paths with "/" */
#include "glob.inc"
#else
#include "../base/glob.inc"
#endif

#define _MAXPATHLEN 500

extern char *wxBuffer;

/* MATTHEW: [5] Mac */
#if defined(VMS) 
// we have no strI functions under VMS, therefore I have implemented
// an inefficient but portable version: convert copies of strings to lowercase
// and then use the normal comparison
static void myLowerString(char *s)
{
  while(*s){
    if(isalpha(*s)) *s = (char)tolower(*s);
    s++;
  }
}

int strcasecmp(const char *str_1, const char *str_2)
{
  char *temp1 = new char[strlen(str_1)+1];
  char *temp2 = new char[strlen(str_2)+1];
  strcpy(temp1,str_1);
  strcpy(temp2,str_2);
  myLowerString(temp1);
  myLowerString(temp2);

  int result = strcmp(temp1,temp2);
  delete[] temp1;
  delete[] temp2;

  return(result);
}

int strncasecmp(const char *str_1, const char *str_2, size_t maxchar)
{
  char *temp1 = new char[strlen(str_1)+1];
  char *temp2 = new char[strlen(str_2)+1];
  strcpy(temp1,str_1);
  strcpy(temp2,str_2);
  myLowerString(temp1);
  myLowerString(temp2);

  int result = strncmp(temp1,temp2,maxchar);
  delete[] temp1;
  delete[] temp2;

  return(result);
}
#endif

#ifdef wx_msw

#define   _wxToLower(_c)     (char)lcharmap[(unsigned char)(_c)]

#define strcasecmp stricmp
#define strncasecmp strnicmp

// Lower case filename map
static unsigned char lcharmap[] =
{
  '\000', '\001', '\002', '\003', '\004', '\005', '\006', '\007',
  '\010', '\011', '\012', '\013', '\014', '\015', '\016', '\017',
  '\020', '\021', '\022', '\023', '\024', '\025', '\026', '\027',
  '\030', '\031', '\032', '\033', '\034', '\035', '\036', '\037',
  '\040', '\041', '\042', '\043', '\044', '\045', '\046', '\047',
  '\050', '\051', '\052', '\053', '\054', '\055', '\056', '\057',
  '\060', '\061', '\062', '\063', '\064', '\065', '\066', '\067',
  '\070', '\071', '\072', '\073', '\074', '\075', '\076', '\077',
  '\100', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
  '\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
  '\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
  '\170', '\171', '\172', '\133', '\134', '\135', '\136', '\137',
  '\140', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
  '\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
  '\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
  '\170', '\171', '\172', '\173', '\174', '\175', '\176', '\177',
  '\200', '\201', '\202', '\203', '\204', '\205', '\206', '\207',
  '\210', '\211', '\212', '\213', '\214', '\215', '\216', '\217',
  '\220', '\221', '\222', '\223', '\224', '\225', '\226', '\227',
  '\230', '\231', '\232', '\233', '\234', '\235', '\236', '\237',
  '\240', '\241', '\242', '\243', '\244', '\245', '\246', '\247',
  '\250', '\251', '\252', '\253', '\254', '\255', '\256', '\257',
  '\260', '\261', '\262', '\263', '\264', '\265', '\266', '\267',
  '\270', '\271', '\272', '\273', '\274', '\275', '\276', '\277',
  '\300', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
  '\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
  '\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
  '\370', '\371', '\372', '\333', '\334', '\335', '\336', '\337',
  '\340', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
  '\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
  '\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
  '\370', '\371', '\372', '\373', '\374', '\375', '\376', '\377',
};

#else
// This declaration is missing in SunOS!
// (Yes, I know it is NOT ANSI-C but its in BSD libc)
#if defined(__xlC) || defined(_AIX) || defined(__GNUG__)
extern "C"
{
  int strcasecmp (const char *, const char *);
  int strncasecmp (const char *, const char *, size_t);
}
#endif
#endif				/* wx_msw */
#ifdef wx_mac
int strncasecmp(char *, char *, int);	//these are in src:mac:wx_comparestrings.c
int strcasecmp(char *, char *);
#endif

char *
copystring (const char *s)
{
  if (s == NULL) s = "";
  size_t len = strlen (s) + 1;

  char *news = new char[len];
  memcpy (news, s, len);	// Should be the fastest

  return news;
}

// Id generation
static long wxCurrentId = 100;

long 
wxNewId (void)
{
  return wxCurrentId++;
}

long
wxGetCurrentId(void) { return wxCurrentId; }

void 
wxRegisterId (long id)
{
  if (id >= wxCurrentId)
    wxCurrentId = id + 1;
}

void 
StringToFloat (char *s, float *number)
{
  if (s && *s && number)
    *number = (float) strtod (s, NULL);
}

void 
StringToDouble (char *s, double *number)
{
  if (s && *s && number)
    *number = strtod (s, NULL);
}

char *
FloatToString (float number)
{
  static char buf[20];

  sprintf (buf, "%.2f", number);
  return buf;
}

char *
DoubleToString (double number)
{
  static char buf[20];

  sprintf (buf, "%.2f", number);
  return buf;
}

void 
StringToInt (char *s, int *number)
{
  if (s && *s && number)
    *number = (int) strtol (s, NULL, 10);
}

void 
StringToLong (char *s, long *number)
{
  if (s && *s && number)
    *number = strtol (s, NULL, 10);
}

char *
IntToString (int number)
{
  static char buf[20];

  sprintf (buf, "%d", number);
  return buf;
}

char *
LongToString (long number)
{
  static char buf[20];

  sprintf (buf, "%ld", number);
  return buf;
}


// Match a string INDEPENDENT OF CASE
Bool 
StringMatch (char *str1, char *str2, Bool subString, Bool exact)
{
  if (str1 == NULL || str2 == NULL)
    return FALSE;
  if (str1 == str2)
    return TRUE;

  if (subString)
    {
      int len1 = strlen (str1);
      int len2 = strlen (str2);
      int i;

      // Search for str1 in str2
      // Slow .... but acceptable for short strings
      for (i = 0; i <= len2 - len1; i++)
	{
	  if (strncasecmp (str1, str2 + i, len1) == 0)
	    return TRUE;
	}
    }
  else if (exact)
    {
      if (strcasecmp (str1, str2) == 0)
	return TRUE;
    }
  else
    {
      int len1 = strlen (str1);
      int len2 = strlen (str2);

      if (strncasecmp (str1, str2, min (len1, len2)) == 0)
	return TRUE;
    }

  return FALSE;
}


/****** FILE UTILITIES ******/

#ifdef IMPLEMENT_DYNAMIC_CLASS
IMPLEMENT_DYNAMIC_CLASS(wxPathList, wxList)
#endif

void wxPathList::Add (char *path)
{
  Append ((wxObject *) path);
}

// Add paths e.g. from the PATH environment variable
void wxPathList::AddEnvList (char *envVariable)
{
  static const char PATH_TOKS[] =
#ifdef wx_msw
	" ;"; // Don't seperate with colon in DOS (used for drive)
#else
#ifdef wx_mac /* MATTHEW: [5] Mac */
        ";"; // No colons OR spaces for Mac.
             // (Perhaps function doesn't make sense anyway...)
#else
	" :;";
#endif
#endif

  char *val = getenv (envVariable);
  if (val && *val)
    {
      char *s = copystring (val);
      char *token = strtok (s, PATH_TOKS);

      if (token)
	{
	  Add (copystring (token));
	  while (token)
	    {
	      if ((token = strtok (NULL, PATH_TOKS)) != NULL)
		Add (copystring (token));
	    }
	}
      delete[]s;
    }
}

// Given a full filename (with path), ensure that that file can
// be accessed again USING FILENAME ONLY by adding the path
// to the list if not already there.
void wxPathList::EnsureFileAccessible (char *path)
{
  char *path_only = wxPathOnly (path);
  if (path_only)
    {
      if (!Member (path_only))
	Add (path_only);
    }
}

// BugFIX @@@@ Unix is case sensitive!
Bool wxPathList::Member (char *path)
{
  for (wxNode * node = First (); node != NULL; node = node->Next ())
    {
      char *path2 = (char *) node->Data ();
      if (path2 &&
#if defined(wx_msw) || defined(VMS) || defined(wx_mac) /* MATTHEW: [5] Mac */
      // Case INDEPENDENT
	  strcasecmp (path, path2) == 0
#else
      // Case sensitive File System 
	  strcmp (path, path2) == 0
#endif
	)
	return TRUE;
    }
  return FALSE;
}

char *wxPathList::FindValidPath (char *file)
{
/*
  if (wxFileExists (file))
    return file;
*/
  if (wxFileExists (wxExpandPath(wxBuffer, file)))
    return wxBuffer;

  char buf[_MAXPATHLEN];
  strcpy(buf, wxBuffer);

  char *filename = IsAbsolutePath (buf) ? wxFileNameFromPath (buf) : (char *)buf; /* MATTHEW: BC */

  for (wxNode * node = First (); node; node = node->Next ())
    {
      char *path = (char *) node->Data ();
      strcpy (wxBuffer, path);
#ifdef wx_mac
      strcat (wxBuffer, ":"); /* MATTHEW: [5] Mac */
#else
      strcat (wxBuffer, "/");
#endif
      strcat (wxBuffer, filename);
#ifdef wx_msw
      Unix2DosFilename (wxBuffer);
#endif
      if (wxFileExists (wxBuffer))
	return wxBuffer;	// Found!

    }				// for()

  return NULL;			// Not found

}

Bool 
wxFileExists (const char *filename)
{
  struct stat stbuf;

  // (char *) cast necessary for VMS
  if (filename && stat ((char *)filename, &stbuf) == 0)
    return TRUE;
  return FALSE;
}


Bool 
wxIsAbsolutePath (const char *filename)
{
  if (filename)
    {
#ifdef wx_mac /* MATTHEW: [5] Mac */
      if (*filename == ':')
	return FALSE;
      for (; *filename; filename++)
	if (*filename == ':')
	  return TRUE;
      return FALSE;
#else
      if (*filename == '/'
#ifdef VMS
      || (*filename == '[' && *(filename+1) != '.')
#endif
#ifdef wx_msw
      /* MSDOS */
      || *filename == '\\' || (isalpha (*filename) && *(filename + 1) == ':')
#endif
	)
	return TRUE;
#endif
    }
  return FALSE;
}

/*
 * Strip off any extension (dot something) from end of file,
 * IF one exists. Inserts zero into buffer.
 *
 */
 
void wxStripExtension(char *buffer)
{
  int len = strlen(buffer);
  int i = len-1;
  while (i > 0)
  {
    if (buffer[i] == '.')
    {
      buffer[i] = 0;
      break;
    }
    i --;
  }
}

// CAUSES PROBLEMS FOR GNU COMPILER -- does it? Check.
// #ifndef __GNUG__
// Destructive removal of /./ and /../ stuff
char *wxRealPath (char *path)
{
#ifdef wx_mac /* MATTHEW: [5] Mac */
  char *p, *lastc = NULL;

  /* IF we find a "::", delete up to previous ":".
     Illegal if there is no previous ":" */
  for (p = path; *p; p++)
    if (p[0] == ':') {
      if (p[1] == ':') {
	if (lastc) {
	  char *c;
	  for (c = lastc, p++; *p; )
	    *(c++) = *(p++);
	  *c = 0;
	  p = lastc - 1;
	  for (lastc--; (lastc >= path) && (*lastc != ':'); --lastc);
	  if (lastc < path)
	  	lastc = NULL;
	} else
	  lastc = ++p;
      } else
	lastc = p;
    }
#else
#ifdef wx_msw
  static const char SEP = '\\';
  Unix2DosFilename(path);
#else 
  static const char SEP = '/';
#endif
  if (path[0] && path[1]) {
    /* MATTHEW: special case "/./x" */
    char *p;
    if (path[2] == SEP && path[1] == '.')
      p = &path[0];
    else
      p = &path[2];
    for (; *p; p++)
      {
	if (*p == SEP)
	  {
	    if (p[1] == '.' && p[2] == '.' && (p[3] == SEP || p[3] == '\0'))
	      {
		for (char *q = p - 1; q >= path && *q != SEP; q--);
		if (q[0] == SEP && (q[1] != '.' || q[2] != '.' || q[3] != SEP)
		    && (q - 1 <= path || q[-1] != SEP))
		  {
		    strcpy (q, p + 3);
		    if (path[0] == '\0')
		      {
			path[0] = SEP;
			path[1] = '\0';
		      }
#ifdef wx_msw
		    /* MATTHEW: check that path[2] is NULL! */
		    else if (path[1] == ':' && !path[2])
		      {
			path[2] = SEP;
			path[3] = '\0';
		      }
#endif
		    p = q - 1;
		  }
	      }
	    else if (p[1] == '.' && (p[2] == SEP || p[2] == '\0'))
	      strcpy (p, p + 2);
	  }
      }
  }
#endif
  return path;
}


// Must be destroyed [@@@ new func]
char *wxCopyAbsolutePath(const char *filename)
{
  if (filename == NULL)
    return NULL;

  if (! IsAbsolutePath(wxExpandPath(wxBuffer, filename))) {
    char    buf[_MAXPATHLEN];
    buf[0] = '\0';
    wxGetWorkingDirectory(buf, sizeof(buf)/sizeof(char));
    char ch = buf[strlen(buf) - 1];
#ifdef wx_msw
    if (ch != '\\' && ch != '/')
	strcat(buf, "\\");
#else
#ifdef wx_mac /* MATTHEW: [5] Mac */
    if (ch != ':')
	strcat(buf, ":");
#else
    if (ch != '/')
	strcat(buf, "/");
#endif
#endif
    strcat(buf, wxBuffer);
    return copystring( wxRealPath(buf) );
  }
  return copystring( wxBuffer );
}

/*-
 Handles:
   ~/ => home dir
   ~user/ => user's home dir
   If the environment variable a = "foo" and b = "bar" then:
   Unix:
	$a	=>	foo
	$a$b	=>	foobar
	$a.c	=>	foo.c
	xxx$a	=>	xxxfoo
	${a}!	=>	foo!
	$(b)!	=>	bar!
	\$a	=>	\$a
   MSDOS:
	$a	==>	$a
	$(a)	==>	foo
	$(a)$b	==>	foo$b
	$(a)$(b)==>	foobar
	test.$$	==>	test.$$
 */

/* input name in name, pathname output to buf. */

char *wxExpandPath(char *buf, const char *name)
{
    register char  *d, *s, *nm;
    char            lnm[_MAXPATHLEN];
    Bool            q;

    // Some compilers don't like this line.
//    const char      trimchars[] = "\n \t";

    char      trimchars[4];
    trimchars[0] = '\n';
#ifdef wx_mac
	trimchars[1] = '\r'; /* MATTHEW: [5] Space OK, carriage return is not */
#else
    trimchars[1] = ' ';
#endif
    trimchars[2] = '\t';
    trimchars[3] = 0;

#ifdef wx_msw
     const char     SEP = '\\';
#else 
#ifdef wx_mac /* MATTHEW: [5] Mac */
    const char SEP = ':';
#else
     const char     SEP = '/';
#endif
#endif
    buf[0] = '\0';
    if (name == NULL || *name == '\0')
	return buf;
    nm = copystring(name); // Make a scratch copy
    char *nm_tmp = nm;

    /* Skip leading whitespace and cr */
    while (strchr((char *)trimchars, *nm) != NULL) /* MATTHEW: BC */
	nm++;
    /* And strip off trailing whitespace and cr */
    s = nm + (q = strlen(nm)) - 1;
    while (q-- && strchr((char *)trimchars, *s) != NULL) /* MATTHEW: BC */
	*s = '\0';

#ifdef wx_mac /* MATTHEW: [5] That's all we should do for the Mac */
	strcpy(buf, nm);
    return wxRealPath(buf);
#endif

    s = nm;
    d = lnm;
#ifdef wx_msw
    q = FALSE;
#else
    q = nm[0] == '\\' && nm[1] == '~';
#endif

    /* Expand inline environment variables */
    while (*d++ = *s) {
#ifndef wx_msw
	if (*s == '\\') {
	    if (*(d - 1) = *++s) {
		s++;
		continue;
	    } else
		break;
	} else
#endif
#ifdef wx_msw
	if (*s++ == '$' && (*s == '{' || *s == ')'))
#else
	if (*s++ == '$')
#endif
	{
	    register char  *start = d;
	    register        braces = (*s == '{' || *s == '(');
	    register char  *value;
	    while (*d++ = *s)
		if (braces ? (*s == '}' || *s == ')') : !(isalnum(*s) || *s == '_') )
		    break;
		else
		    s++;
	    *--d = 0;
	    value = getenv(braces ? start + 1 : start);
	    if (value) {
		for (d = start - 1; *d++ = *value++;);
		d--;
		if (braces && *s)
		    s++;
	    }
	}
    }

    /* Expand ~ and ~user */
    nm = lnm;
    s = "";
    if (nm[0] == '~' && !q) {
	/* prefix ~ */
	if (nm[1] == SEP || nm[1] == 0) {	/* ~/filename */
	    if ((s = wxGetUserHome(NULL)) != NULL) {
		if (*++nm)
		    nm++;
	    }
	} else {		/* ~user/filename */
	    register char  *nnm;
	    register char  *home;
	    for (s = nm; *s && *s != SEP; s++);
	    int was_sep; /* MATTHEW: Was there a separator, or NULL? */
	    was_sep = (*s == SEP);
	    nnm = *s ? s + 1 : s;
	    *s = 0;
	    if ((home = wxGetUserHome(nm + 1)) == NULL) {
		if (was_sep) /* MATTHEW: replace only if it was there: */
		  *s = SEP;
		s = "";
	    } else {
		nm = nnm;
		s = home;
	    }
	}
    }

    d = buf;
    if (s && *s) { /* MATTHEW: s could be NULL if user '~' didn't exist */
	/* Copy home dir */
	while ('\0' != (*d++ = *s++))
	  /* loop */;
	// Handle root home
	if (d - 1 > buf && *(d - 2) != SEP)
	  *(d - 1) = SEP;
    }
    s = nm;
    while (*d++ = *s++);

    delete[] nm_tmp; // clean up alloc
    /* Now clean up the buffer */
    return wxRealPath(buf);
}


/* Contract Paths to be build upon an environment variable
   component:

   example: "/usr/openwin/lib", OPENWINHOME --> ${OPENWINHOME}/lib

   The call wxExpandPath can convert these back!
 */
char *
wxContractPath (const char *filename, const char *envname, const char *user)
{
  static char dest[_MAXPATHLEN];

  if (filename == NULL)
    return NULL;

#ifdef wx_mac /* MATTHEW: [5] Do nothing on Mac */
  strcpy(dest, filename);
  return dest;
#endif

  strcpy (dest, filename);
#ifdef wx_msw
  Unix2DosFilename(dest);
#endif

  // Handle environment
  char *val, *tcp;
  if (envname != NULL && (val = getenv (envname)) != NULL &&
     (tcp = strstr (dest, val)) != NULL)
    {
        strcpy (wxBuffer, tcp + strlen (val));
        *tcp++ = '$';
        *tcp++ = '{';
        strcpy (tcp, envname);
        strcat (tcp, "}");
        strcat (tcp, wxBuffer);
    }

  // Handle User's home (ignore root homes!)
  size_t len;
  if ((val = wxGetUserHome (user)) != NULL &&
      (len = strlen(val)) > 2 &&
      strncmp(dest, val, len) == 0)
    {
      strcpy(wxBuffer, "~");
      if (user && *user)
	strcat(wxBuffer, user);
#ifdef wx_msw
//      strcat(wxBuffer, "\\");
#else
//      strcat(wxBuffer, "/");
#endif
      strcat(wxBuffer, dest + len);
      strcpy (dest, wxBuffer);
    }

  return dest;
}

// Return just the filename, not the path
// (basename)
char *
wxFileNameFromPath (char *path)
{
  if (path)
    {
      register char *tcp;

      tcp = path + strlen (path);
      while (--tcp >= path)
	{
	  if (
#ifdef wx_mac /* MATTHEW: [5] Mac */
	      *tcp == ':'
#else
	      *tcp == '/' 
#ifdef wx_msw /* MATTHEW: [5] DOS only */
                    || *tcp == '\\'
#endif
#endif
#ifdef VMS
     || *tcp == ':' || *tcp == ']')
#else
     )
#endif
	    return tcp + 1;
	}			/* while */
#ifdef wx_msw
      if (isalpha (*path) && *(path + 1) == ':')
	return path + 2;
#endif
    }
  return path;
}

// Return just the directory, or NULL if no directory
char *
wxPathOnly (char *path)
{
  if (path && *path)
    {
      static char buf[_MAXPATHLEN];

      // Local copy
      strcpy (buf, path);

      int l = strlen(path);
      Bool done = FALSE;

      int i = l - 1;

      // Search backward for a backward or forward slash
      while (!done && i > -1)
      {
        if (
#ifdef wx_mac /* MATTHEW: [5] Mac */
	    path[i] == ':'
#else
	    path[i] == '/' 
#ifdef wx_msw /* MATTHEW: [5] DOS only */
	    || path[i] == '\\'
#endif
#endif
	    )
        {
          done = TRUE;
          buf[i] = 0;
          return buf;
        }
        else i --;
      }

/* there's a bug here somewhere, so replaced with my original code.
      char *tcp;
      // scan back
      for (tcp = &buf[strlen (buf) - 1]; tcp >= buf; tcp--)
	{
	  // Search for Unix or Dos path sep {'\\', '/'}
	  if (*tcp == '\\' || *tcp == '/')
	    {
	      *tcp = '\0';
	      return buf;
	    }
	}			// for()
*/
#ifdef wx_msw
      // Try Drive specifier
      if (isalpha (buf[0]) && buf[1] == ':')
	{
	  // A:junk --> A:. (since A:.\junk Not A:\junk)
	  buf[2] = '.';
	  buf[3] = '\0';
	  return buf;
	}
#endif
    }

  return NULL;
}

// Utility for converting delimiters in DOS filenames to UNIX style
// and back again - or we get nasty problems with delimiters.
// Also, convert to lower case, since case is significant in UNIX.

void 
wxDos2UnixFilename (char *s)
{
  if (s)
    while (*s)
      {
	if (*s == '\\')
	  *s = '/';
#ifdef wx_msw
	else
	  *s = _wxToLower (*s);	// Case INDEPENDENT
#endif
	s++;
      }
}

void 
wxUnix2DosFilename (char *s)
{
// Yes, I really mean this to happen under DOS only! JACS
#ifdef wx_msw
  if (s)
    while (*s)
      {
	if (*s == '/')
	  *s = '\\';
	s++;
      }
#endif
}

// Return the current date/time
// [volatile]
char *wxNow( void )
{
  time_t now = time(NULL);
  char *date = ctime(&now); 
  date[24] = '\0';
  return date;
}

/* Get Full RFC822 style email address */
Bool
wxGetEmailAddress (char *address, int maxSize)
{
  char host[65];
  char user[65];

  if (wxGetHostName(host, 64) == FALSE)
    return FALSE;
  if (wxGetUserId(user, 64) == FALSE)
    return FALSE;

  char tmp[130];
  strcpy(tmp, user);
  strcat(tmp, "@");
  strcat(tmp, host);

  strncpy(address, tmp, maxSize - 1);
  address[maxSize-1] = '\0';
  return TRUE;
}

// Concatenate two files to form third
Bool 
wxConcatFiles (const char *file1, const char *file2, const char *file3)
{
  char *outfile = wxGetTempFileName("cat");

  FILE *fp1 = NULL;
  FILE *fp2 = NULL;
  FILE *fp3 = NULL;
  // Open the inputs and outputs
  if ((fp1 = fopen (file1, "rb")) == NULL ||
      (fp2 = fopen (file2, "rb")) == NULL ||
      (fp3 = fopen (outfile, "wb")) == NULL)
    {
      if (fp1)
	fclose (fp1);
      if (fp2)
	fclose (fp2);
      if (fp3)
	fclose (fp3);
      return FALSE;
    }

  int ch;
  while ((ch = getc (fp1)) != EOF)
    (void) putc (ch, fp3);
  fclose (fp1);

  while ((ch = getc (fp2)) != EOF)
    (void) putc (ch, fp3);
  fclose (fp2);

  fclose (fp3);
  Bool result = wxRenameFile(outfile, file3);
  delete[] outfile;
  return result;
}

// Copy files
Bool 
wxCopyFile (const char *file1, const char *file2)
{
  FILE *fd1;
  FILE *fd2;
  int ch;

  if ((fd1 = fopen (file1, "rb")) == NULL)
    return FALSE;
  if ((fd2 = fopen (file2, "wb")) == NULL)
    {
      fclose (fd1);
      return FALSE;
    }

  while ((ch = getc (fd1)) != EOF)
    (void) putc (ch, fd2);

  fclose (fd1);
  fclose (fd2);
  return TRUE;
}

Bool 
wxRenameFile (const char *file1, const char *file2)
{
  // Normal system call
  if (0 == rename (file1, file2))
    return TRUE;
  // Try to copy
  if (wxCopyFile(file1, file2)) {
    wxRemoveFile(file1);
    return TRUE;
  }
  // Give up
  return FALSE;
}

/*
 * Strip out any menu codes
 */

char *wxStripMenuCodes (char *in, char *out)
{
  if (!in)
    return NULL;
    
  if (!out)
    out = copystring(in);

  char *tmpOut = out;
  
  while (*in)
    {
      if (*in == '&')
	{
	  // Check && -> &, &x -> x
	  if (*++in == '&')
	    *out++ = *in++;
	}
      else if (*in == '\t')
	{
          // Remove all stuff after \t in X mode, and let the stuff as is
          // in Windows mode.
          // Accelerators are handled in wx_item.cc for Motif, and are not
          // YET supported in XView
	  break;
	}
      else
	*out++ = *in++;
    }				// while

  *out = '\0';

  return tmpOut;
}


/*
 * Window search functions
 *
 */

/*
 * If parent is non-NULL, look through children for a label or title
 * matching the specified string. If NULL, look through all top-level windows.
 *
 */

static wxWindow *wxFindWindowByLabel1 (char *title, wxWindow * parent);

wxWindow *
wxFindWindowByLabel (char *title, wxWindow * parent)
{
  if (parent)
    {
      return wxFindWindowByLabel1 (title, parent);
    }
  else
    {
      for (wxChildNode * node = wxTopLevelWindows(NULL)->First (); node; node = node->Next ())
	{
	  wxWindow *win = (wxWindow *) node->Data ();
	  if (win && node->IsShown()) {
		  wxWindow *retwin = wxFindWindowByLabel1 (title, win);
		  if (retwin)
		    return retwin;
	  }
	}			// for()

    }
  return NULL;
}

// Recursive
static wxWindow *
wxFindWindowByLabel1 (char *title, wxWindow * parent)
{
  if (parent)
    {
      char *lab = parent->GetLabel ();
      if (lab && StringMatch (title, lab))
	return parent;
    }

  if (parent)
    {
      for (wxChildNode * node = parent->GetChildren()->First (); node; node = node->Next ())
	{
	  wxWindow *win = (wxWindow *) node->Data ();
	  wxWindow *retwin = wxFindWindowByLabel1 (title, win);
	  if (retwin)
	    return retwin;
	}			// for()

    }

  return NULL;			// Not found

}

/*
 * If parent is non-NULL, look through children for a name
 * matching the specified string. If NULL, look through all top-level windows.
 *
 */

static wxWindow *wxFindWindowByName1 (char *title, wxWindow * parent);

wxWindow *
wxFindWindowByName (char *title, wxWindow * parent)
{
  if (parent)
    {
      return wxFindWindowByName1 (title, parent);
    }
  else
    {
      for (wxChildNode * node = wxTopLevelWindows(NULL)->First (); node; node = node->Next ())
	{
	  wxWindow *win = (wxWindow *) node->Data ();
	  if (win && node->IsShown()) {
		  wxWindow *retwin = wxFindWindowByName1 (title, win);
		  if (retwin)
		    return retwin;
	  }
	}			// for()

    }
  // Failed? Try by label instead.
  return wxFindWindowByLabel(title, parent);
}

// Recursive
static wxWindow *
wxFindWindowByName1 (char *title, wxWindow * parent)
{
  if (parent)
    {
      char *lab = parent->GetName ();
      if (lab && StringMatch (title, lab))
	return parent;
    }

  if (parent)
    {
      for (wxChildNode * node = parent->GetChildren()->First (); node; node = node->Next ())
	{
	  wxWindow *win = (wxWindow *) node->Data ();
	  wxWindow *retwin = wxFindWindowByName1 (title, win);
	  if (retwin)
	    return retwin;
	}			// for()

    }

  return NULL;			// Not found

}

// Returns menu item id or -1 if none.
int 
wxFindMenuItemId (wxFrame * frame, char *menuString, char *itemString)
{
  wxMenuBar *menuBar = frame->GetMenuBar ();
  if (!menuBar)
    return -1;
  return menuBar->FindMenuItem (menuString, itemString);
}

/*
 * wxDebugStreamBuf
 */
#ifndef wx_mac /* MATTHEW: [5] Not in Mac header? */

wxDebugStreamBuf::wxDebugStreamBuf(void)
{
  if (allocate()) setp(base(),ebuf());
}

int wxDebugStreamBuf::overflow(int WXUNUSED(i))
{
  int len = pptr() - pbase();
  char *txt = new char[len+1];
  strncpy(txt, pbase(), len);
  txt[len] = '\0';
#ifdef wx_msw
  OutputDebugString((LPCSTR)txt);
#else
  fprintf(stderr, txt);
#endif
  setp(pbase(), epptr());
  delete[] txt;
  return EOF;
}

int wxDebugStreamBuf::sync(void)
{
  int len = pptr() - pbase();
  char *txt = new char[len+1];
  strncpy(txt, pbase(), len);
  txt[len] = '\0';
#ifdef wx_msw
  OutputDebugString((LPCSTR)txt);
#else
  fprintf(stderr, txt);
#endif
  setp(pbase(), epptr());
  delete[] txt;
  return 0;
}


#endif

#ifdef wx_mac

/////////////////////////////////////////////////////////////////////////////
//
#ifdef DEBUGLOG
//

#define SHARED	// one or more applications can dump on the same file, useful on IPC testing

wxLogClass::wxLogClass (const char *file)
{
  log_file = copystring (file);
#ifdef SHARED
  the_stream = new ofstream(log_file,ios::out|ios::app); 
#else
  the_stream = new ofstream(log_file);
#endif
  delete the_stream;
}

wxLogClass::~wxLogClass ()
{
 delete log_file;
}

void wxLogClass::Open (void)
{
#ifdef wx_mac
  the_stream = new ofstream(log_file, ios::out|ios::app);
#else
	the_stream = new ofstream(log_file, ios::app);
#endif
}

void wxLogClass::Close (void)
{
  delete the_stream;
}

wxLogClass & wxLogClass::operator <<(char *s)
{
  this->Open ();
  *the_stream << s;
  the_stream->flush ();
  this->Close ();
  return *this;
}

wxLogClass & wxLogClass::operator <<(int i)
{
  this->Open ();
  *the_stream << i;
  the_stream->flush ();
  this->Close ();
  return *this;
}

wxLogClass & wxLogClass::operator <<(double i)
{
  this->Open ();
  *the_stream << i;
  the_stream->flush ();
  this->Close ();
  return *this;
}

wxLogClass wxLog ("log");
#endif /* DEBUGLOG */
/////////////////////////////////////////////////////////////////////////////

#endif
