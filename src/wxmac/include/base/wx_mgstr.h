/*
 * File:	wx_mgstr.h
 * Purpose:	simple language support
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_mgstr.h	1.2 5/9/94" */

#ifdef __GNUG__
#pragma interface
#endif

#ifndef wx_mgstr
#define wx_mgstr

typedef enum {
  wxLANGUAGE_DEFAULT,
  wxLANGUAGE_GERMAN,
  wxLANGUAGE_ENGLISH
} wxlanguage_t;

extern char **wx_msg_str;
extern void wxSetLanguage(wxlanguage_t language);

#define wxSTR(X)	wx_msg_str[X]

// Generic Message Box titles
#define wxSTR_ERROR                  wx_msg_str[0]
#define wxSTR_WARNING                wx_msg_str[1]

// wxForm messages:
#define wxSTR_YES                    wx_msg_str[2]
#define wxSTR_NO                     wx_msg_str[3]

#define wxSTR_BUTTON_OK              wx_msg_str[4]
#define wxSTR_BUTTON_CANCEL          wx_msg_str[5]
#define wxSTR_BUTTON_REVERT          wx_msg_str[6]
#define wxSTR_BUTTON_UPDATE          wx_msg_str[7]
#define wxSTR_CONSTRAINT_VIOLATION   wx_msg_str[8]
#define wxSTR_VIOLATION_FLOAT        wx_msg_str[9]
#define wxSTR_VIOLATION_LONG         wx_msg_str[10]

#define wxSTR_MENU_HELP              wx_msg_str[11]
#define wxSTR_BUTTON_HELP            wx_msg_str[12]

// File Dialog Messages
#define wxSTR_FILE_SELECTOR          wx_msg_str[13]
#define wxSTR_OVERWRITE_FILE         wx_msg_str[14]
#define wxSTR_LOAD_FILE              wx_msg_str[15]
#define wxSTR_SAVE_FILE              wx_msg_str[16]
#define wxSTR_ENTER_FILENAME         wx_msg_str[17]
#define wxSTR_LABEL_FILENAME         wx_msg_str[18]
#define wxSTR_LABEL_PATH             wx_msg_str[19]
#define wxSTR_LABEL_FILES            wx_msg_str[20]
#define wxSTR_LABEL_DIRS             wx_msg_str[21]

// Misc Messages
#define wxSTR_HELP_TIMEOUT           wx_msg_str[22]

#endif // wx_mgstr.h
