/*
 * File:	wx_clipb.cc
 * Purpose:	Clipboard implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
 */

static const char sccsid[] = "%W% %G%";

#include "common.h"
#include "wx_setup.h"

#if USE_CLIPBOARD
#include "wx_clipb.h"
#include "wx_list.h"
#include "wx_main.h"
#include "wx_utils.h"
#include <Scrap.h>
#include <TextEdit.h>

static wxList *ClipboardFormats = NULL;

#define CUSTOM_ID_START 100

class ClipboardFormat : public wxObject
{
  public:
   int format;
   char *name;
};

static void InitFormats()
{
	ClipboardFormat *cf;

    ClipboardFormats = new wxList;

 	cf = new ClipboardFormat;
 	cf->name = "TEXT";
  	cf->format = wxCF_TEXT;

    ClipboardFormats->Append(cf);
}

Bool wxOpenClipboard(void)
{
  TEToScrap();
  return TRUE;
}

Bool wxCloseClipboard(void)
{
  TEFromScrap();
  return TRUE;
}

Bool wxEmptyClipboard(void)
{
  return ZeroScrap();
}

Bool wxIsClipboardFormatAvailable(int dataFormat)
{
  long offset;
  long format;

  if (dataFormat == wxCF_OEMTEXT)
    dataFormat = wxCF_TEXT;

  if (!wxGetClipboardFormatName(dataFormat, (char *)&format, 4))
     return FALSE;

  return (GetScrap(NULL, format, &offset) > 0);
}

Bool wxSetClipboardData(int dataFormat, wxObject *obj, int width, int height)
{
  long format, length;

  if (dataFormat == wxCF_OEMTEXT)
    dataFormat = wxCF_TEXT;

  if (!wxGetClipboardFormatName(dataFormat, (char *)&format, 4))
     return FALSE;

  if (format == 'TEXT')
    length = strlen((char *)obj);
  else
    length = (long)width * height;

  return !PutScrap(length, format, (Ptr)obj);
}

wxObject *wxGetClipboardData(int dataFormat, long *size)
{
  Handle h;
  char *result;
  long format, length, offset;

  if (dataFormat == wxCF_OEMTEXT)
    dataFormat = wxCF_TEXT;

  if (!wxGetClipboardFormatName(dataFormat, (char *)&format, 4))
     return NULL;

  h = NewHandle(10);
  CheckMemOK(h);
  length = GetScrap(h, format, &offset);

  if (length < 0)
	return NULL;

  result = new char[length + ((format == 'TEXT') ? 1 : 0)];
  HLock(h);
  memcpy(result, *h, length);
  HUnlock(h);
  DisposeHandle(h);

  if (format == 'TEXT')
    result[length++] = 0;

  if (size)
	*size = length;

  return (wxObject *)result;
}

int  wxEnumClipboardFormats(int dataFormat)
{
  long offset, format;
  wxNode *node;
  ClipboardFormat *cf;

  if (!ClipboardFormats)
    InitFormats();   

  if (!dataFormat)
   node = ClipboardFormats->First();
  else {
	for (node = ClipboardFormats->First(); node; node = node->Next()) {
		cf = (ClipboardFormat *)node->Data();
    	if (cf->format == dataFormat) {
			node = node->Next();
			break;
		}
	}
  }

  for (; node; node = node->Next()) {
	cf = (ClipboardFormat *)node->Data();
    memcpy(&format, cf->name, 4);
    if (GetScrap(NULL, format, &offset) > 0)
       return cf->format;
  }

  return 0;
}

int  wxRegisterClipboardFormat(char *formatName)
{
  wxNode *node;
  ClipboardFormat *cf;

  if (!ClipboardFormats)
	InitFormats();
  
  for (node = ClipboardFormats->First(); node; node = node->Next()) {
	cf = (ClipboardFormat *)node->Data();
    if (!strcmp(cf->name, formatName))
      return cf->format;
  }

  cf = new ClipboardFormat;

  cf->format = ClipboardFormats->Number() + CUSTOM_ID_START;
  cf->name = new char[strlen(formatName)];
  strcpy(cf->name, formatName);

  ClipboardFormats->Append(cf);
 
  return cf->format;
}

Bool wxGetClipboardFormatName(int dataFormat, char *formatName, int maxCount)
{
  wxNode *node;
  ClipboardFormat *cf;

  if (dataFormat == wxCF_OEMTEXT)
    dataFormat = wxCF_TEXT;

  if (!ClipboardFormats)
	InitFormats();
  
  for (node = ClipboardFormats->First(); node; node = node->Next()) {
	cf = (ClipboardFormat *)node->Data();
    if (cf->format == dataFormat) {
      strncpy(formatName, cf->name, maxCount);
	  return TRUE;
    }
  }

  return FALSE;
}

/********************************************************************************/
/*                             Clipboard Classes                                */
/********************************************************************************/

wxClipboard *wxTheClipboard;

void wxInitClipboard(void)
{
  if (!wxTheClipboard)
    wxTheClipboard = new wxClipboard;
}

wxClipboard::wxClipboard()
{
  clipOwner = NULL;
  cbString = NULL;
}

wxClipboard::~wxClipboard()
{
  if (clipOwner)
    clipOwner->BeingReplaced();
  if (cbString)
    delete[] cbString;
}

static int FormatStringToID(char *str)
{
  if (!strcmp(str, "TEXT"))
    return wxCF_TEXT;
  
  return wxRegisterClipboardFormat(str);
}


void wxClipboard::SetClipboardClient(wxClipboardClient *client, long time)
{
  Bool got_selection;

  if (clipOwner)
    clipOwner->BeingReplaced();
  clipOwner = client;
  if (cbString) {
    delete[] cbString;
    cbString = NULL;
  }

  if (wxOpenClipboard()) {
    char **formats, *data;
    int i;
    int ftype;
    long size;

    wxEmptyClipboard();

    formats = clipOwner->formats.ListToArray(FALSE);
    for (i = clipOwner->formats.Number(); i--; ) {
      ftype = FormatStringToID(formats[i]);
      data = clipOwner->GetData(formats[i], &size);
      if (!wxSetClipboardData(ftype, (wxObject *)data, size, 1)) {
	got_selection = FALSE;
	break;
      }
    }

    if (i < 0)
      got_selection = wxCloseClipboard();
  } else
    got_selection = FALSE;
  
  got_selection = FALSE; // Assume another process takes over

  if (!got_selection) {
    clipOwner->BeingReplaced();
    clipOwner = NULL;
  }
}

wxClipboardClient *wxClipboard::GetClipboardClient()
{
  return clipOwner;
}

void wxClipboard::SetClipboardString(char *str, long time)
{
  Bool got_selection;

  if (clipOwner) {
    clipOwner->BeingReplaced();
    clipOwner = NULL;
  }
  if (cbString)
    delete[] cbString;

  cbString = str;

  if (wxOpenClipboard()) {    
    wxEmptyClipboard();
    if (!wxSetClipboardData(wxCF_TEXT, (wxObject *)str))
      got_selection = FALSE;
    else
      got_selection = wxCloseClipboard();
  } else
    got_selection = FALSE;

  got_selection = FALSE; // Assume another process takes over

  if (!got_selection) {
    delete[] cbString;
    cbString = NULL;
  }
}

char *wxClipboard::GetClipboardString(long time)
{
  char *str;
  long length;

  str = GetClipboardData("TEXT", &length, time);
  if (!str) {
    str = new char[1];
    *str = 0;
  }

  return str;
}

char *wxClipboard::GetClipboardData(char *format, long *length, long time)
{
  if (clipOwner)  {
    if (clipOwner->formats.Member(format))
      return clipOwner->GetData(format, length);
    else
      return NULL;
  } else if (cbString) {
    if (!strcmp(format, "TEXT"))
      return copystring(cbString);
    else
      return NULL;
  } else {

    if (wxOpenClipboard()) {
      receivedString = (char *)wxGetClipboardData(FormatStringToID(format), 
						  length);
      wxCloseClipboard();
    } else
      receivedString = NULL;

    return receivedString;
  }
}


#endif
