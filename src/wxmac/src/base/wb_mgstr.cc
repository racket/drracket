/*
 * File:      wb_mgstr.cc
 * Purpose:     simple language support
 * Author:      Julian Smart
 * Created:     1993
 * Updated:     August 1994
 * RCS_ID:      $Id: wb_mgstr.cc,v 1.4 1994/08/15 21:53:50 edz Exp edz $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "@(#)wb_mgstr.cc	1.2 5/9/94"; */

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"
#include "wx_main.h"
#include "wx_utils.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

static const char *msg_english[] =
{
/* wxSTR_ERROR               */ "Error",
/* wxSTR_WARNING             */ "Warning",

/* wxSTR_YES                 */ "Yes",
/* wxSTR_NO                  */ "No",

/* wxSTR_BUTTON_OK           */ "OK",
/* wxSTR_BUTTON_CANCEL       */ "Cancel",
/* wxSTR_BUTTON_REVERT       */ "Revert",
/* wxSTR_BUTTON_UPDATE       */ "Update",
/* wxSTR_CONSTRAINT_VIOLATION*/ "Constraint violation",
/* wxSTR_VIOLATION_FLOAT     */ "%s should be in range %.2f to %.2f",
/* wxSTR_VIOLATION_LONG      */ "%s should be in range %.0f to %.0f",
/* wxSTR_MENU_HELP           */ "Help",
/* wxSTR_BUTTON_HELP         */ "Help",

/* wxSTR_FILE_SELECTOR       */ "File selector",
/* wxSTR_OVERWRITE_FILE      */ "Overwrite existing file %s?",
/* wxSTR_LOAD_FILE           */ "Load %s file",
/* wxSTR_SAVE_FILE           */ "Save %s file",
/* wxSTR_ENTER_FILENAME      */ "Enter %s file name",
/* wxSTR_LABEL_FILENAME      */ "Name",
/* wxSTR_LABEL_PATH          */ "Path",
/* wxSTR_LABEL_FILES         */ "Files",
/* wxSTR_LABEL_DIRS          */ "Directories",

/* wxSTR_HELP_TIMEOUT        */ "Connection to wxHelp timed out in %d seconds",
 NULL
};

static const char *msg_german[] =
{
/* wxSTR_ERROR               */ "Fehler",
/* wxSTR_WARNING             */ "Warnung",

/* wxSTR_YES                 */ "Ja",
/* wxSTR_NO                  */ "Nein",

/* wxSTR_BUTTON_OK           */ "OK",
/* wxSTR_BUTTON_CANCEL       */ "Abbrechen",
/* wxSTR_BUTTON_REVERT       */ "Reset",
/* wxSTR_BUTTON_UPDATE       */ "Aktualisieren",
/* wxSTR_CONSTRAINT_VIOLATION*/ "Wertebereich Verletzung",
/* wxSTR_VIOLATION_FLOAT     */ "%s liegt nicht im Bereich von %.2f bis %.2f",
/* wxSTR_VIOLATION_LONG      */ "%s liegt nicht im Bereich von %.0f bis %.0f",
/* wxSTR_MENU_HELP           */ "Hilfe",
/* wxSTR_BUTTON_HELP         */ "Hilfe",

/* wxSTR_FILE_SELECTOR       */ "Datai Auswahl",
/* wxSTR_OVERWRITE_FILE      */ "Überschriebe datai %s?",
/* wxSTR_LOAD_FILE           */ "Lese %s-Datai",
/* wxSTR_SAVE_FILE           */ "Speiche %s-Datai",
/* wxSTR_ENTER_FILENAME      */ "%s Datainame Auswahl",
/* wxSTR_LABEL_FILENAME      */ "Name",
/* wxSTR_LABEL_PATH          */ "Pfad",
/* wxSTR_LABEL_FILES         */ "Dataien",
/* wxSTR_LABEL_DIRS          */ "Verzeichnis",

/* wxSTR_HELP_TIMEOUT        */ "wxHelp Verbindungsaufbau 'timeout' nach %d sec.",
 NULL
};

// To Do:
//      (0) Use the language of the platform NOT default
//          English
//      (1) extend support to other 8859-1 languages
//      (2) Use UTF for string encodings
//      (3) Add in sub-font mechanism for Unicode
//      (4) Add in support for non 8859 languages

void 
wxSetLanguage (wxlanguage_t language)
{
  switch (language)
    {
    case wxLANGUAGE_DEFAULT:
#if defined(wx_msw) && USE_RESOURCES
      char *lang;
      if (wxGetResource ("intl", "sLanguage", &lang, "WIN.INI") == FALSE)
	lang = copystring ("eng");
      if (StringMatch (lang, "eng"))
	wxSetLanguage (wxLANGUAGE_ENGLISH);
      else if (StringMatch (lang, "ger"))
	wxSetLanguage (wxLANGUAGE_GERMAN);
      else
	wxSetLanguage (wxLANGUAGE_ENGLISH);
      delete[]lang;
#else
      // Default is English
      wxSetLanguage (wxLANGUAGE_ENGLISH);
#endif
      break;
    case wxLANGUAGE_GERMAN:
      wx_msg_str = (char **) &(msg_german[0]);
      break;
    case wxLANGUAGE_ENGLISH:
    default:
      wx_msg_str = (char **) &(msg_english[0]);
      break;
    }
}
