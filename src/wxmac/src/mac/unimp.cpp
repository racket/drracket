#include "wx.h"
#include "wx_utils.h"
#include "unimp.h"
#include "wxstring.h"
#include "wx_list.h"
#include <Strings.h>

int AddOrReplaceEntry(const char *section, const char *entry, char *Value, const char *file);
int GetEntry(const char *section, const char *entry, char *Value, const char *file);

Bool wxWriteResource(const char *section, const char *entry, char *value, const char *file)
{
  char Value[256];

  sprintf(Value, "%s = %s\n", entry, value);

  if ( AddOrReplaceEntry(section, entry, Value, file) )
    return TRUE;
  else
    return FALSE;
}

Bool wxWriteResource(const char *section, const char *entry, float value, const char *file)
{
  char Value[256];

  sprintf(Value, "%s = %f\n", entry, value);

  if ( AddOrReplaceEntry(section, entry, Value, file) )
    return TRUE;
  else
    return FALSE;
}

Bool wxWriteResource(const char *section, const char *entry, long value, const char *file)
{
  char Value[256];

  sprintf(Value, "%s = %d\n", entry, value);

  if ( AddOrReplaceEntry(section, entry, Value, file) )
    return TRUE;
  else
    return FALSE;
}

Bool wxWriteResource(const char *section, const char *entry, int value, const char *file)
{
  char Value[256];

  sprintf(Value, "%s = %d\n", entry, value);

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
    char *badChar = strstr(Value, "\n");
    if (badChar)
      *badChar = '\0';
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
    char *badChar = strstr(Value, "\n");
    if (badChar)
      *badChar = '\0';
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
    char *badChar = strstr(Value, "\n");
    if (badChar)
      *badChar = '\0';
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
    char *badChar = strstr(Value, "\n");
    if (badChar)
      *badChar = '\0';
    value[0] = atoi(Value);
    return TRUE;
  }
  else
    return FALSE;
}

int wxMessageBox(char *, char *);
int wxMessageBox(char *, char *)
{
  return FALSE;
}

#ifdef somethingnotdefined
//char *output = wxFileSelector("File to print to...", NULL, NULL, NULL, "*.txt");
char *wxFileSelector(char *message, char *default_path,
                     char *default_filename, char *default_extension,
                     char *wildcard, int flags,
                     wxFrame *parent, int x, int y)
{
  StandardFileReply reply;
  SFTypeList typeList;
  OSErr err;
  TFileSpec *tFile;
  char				*name;
  StringPtr pstrmsg;
  StringPtr pstrdefname;

//TFileSpec::TFileSpec(short vRefNum, long parID, ConstStr31Param name, Boolean useAlias)
//extern pascal void StandardPutFile(ConstStr255Param prompt, ConstStr255Param defaultName, StandardFileReply *reply)

  if (flags & wxOPEN)
  {
    typeList[0] = '????';
    StandardGetFile(nil, -1, typeList, &reply); // typeList,
  }
  else if (flags & wxSAVE)
  {
    if (message)
      pstrmsg = c2pstr(message);
    else
      pstrmsg = "\pSave File Name";

    if (default_filename)
      pstrdefname = c2pstr(default_filename);
    else
      pstrdefname = "\pPRINTFILE.TXT";

    StandardPutFile(pstrmsg, pstrdefname, &reply);
  }

  if (reply.sfFile.name)
  {
    tFile = new TFileSpec(reply.sfFile.vRefNum,
                      reply.sfFile.parID,
                      reply.sfFile.name, TRUE);
    name = tFile->FullPath();
    return name;
  }
  else
    return 0;
}

#endif

// Get current working directory.
// If buf is NULL, allocates space using new, else
// copies into buf.
char *wxGetWorkingDirectory(char *buf, int sz)
{

  if (!buf)
    buf = new char[1000];
#ifdef wx_msw
#ifdef __BORLANDC__
  (void)getcwd(buf, sz);
#elif __WATCOMC__
  (void)getcwd(buf, sz);
#else 
  (void)_getcwd(buf, sz);
#endif
#elif defined wx_mac
// mac
  return getcwd(buf, sz);
#endif

  //return buf;
}


// Needed for wx_mac since ANSI stdlib is C compiled

#ifdef wx_mac
extern "C" {
int wx_comparestrings(const void *arg1, const void *arg2);
/*int wx_comparestrings(const void *arg1, const void *arg2);*/
#ifndef GUSI
int strcmp(char* s1, char* s2);
#endif
}

int wx_comparestrings(const void *arg1, const void *arg2)
{
  char **s1 = (char **)arg1;
  char **s2 = (char **)arg2;

  return strcmp(*s1, *s2);
}
#endif


char *wxGetUserHome(const char *user)
{
  char usr[256];
  strcpy(usr, user);
  return getcwd(usr, 1000);
}


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

int AddOrReplaceEntry(const char *section, const char *entry, char *Value, const char *file)
{
  FILE *fd;
  wxList *sList;
  wxNode *sNode;
  wxNode *eNode;
  char Section[256];

  // make a section
  sprintf(Section, "[%s]", section);

  if ( (fd = fopen(file, "r+")) == 0 ) // file note there
    return FALSE;

  // find the section or create it
  sList = GetStringList(fd);
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

  // make a section
  sprintf(Section, "[%s]", section);

  if ( (fd = fopen(file, "r+")) == 0 ) // file note there
    return FALSE;

  // find the section or create it
  sList = GetStringList(fd);

  fclose(fd);

  if ( sNode = SectionMember(sList, Section, strlen(Section)))
  {
    if ( (eNode = EntryMember(sList, entry, strlen(entry))) )
    {
      const char *s1 = (const char *) ((wxString *)(eNode->Data()))->GetData();
      char *s2 = strstr(s1, "=");
      strcpy(Value,s2+1);
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
    }
  return TRUE;

}

#ifdef CJC
//int strcasecmp (const char *, const char *);
//int strncasecmp (const char *, const char *, size_t);

int strcasecmp (char *s1, char *s2)
{

  return strcmp(s1,s2);

}

int strncasecmp (char *s1, char *s2, int len)
{

  return strncmp(s1,s2,len);

}
#endif

//	::InvalRect(&newViewRect); // force redraw of text window







