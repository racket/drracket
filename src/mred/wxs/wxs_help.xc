
#include "wx_help.h"

@INCLUDE wxs.xci

@HEADER

#if 0

@CLASSBASE wxHelpInstance "wx:help-instance" : "wx:client"

@CREATOR ()

@ "initialize" : void Initialize(string,int=-1);
@ "display-block" : bool DisplayBlock(long);
@ "display-contents" : bool DisplayContents();
@ "display-section" : bool DisplaySection(int);
@ "keyword-search" : bool KeywordSearch(string);
@ "load-file" : bool LoadFile(string=NULL);
@ "on-quit" : void OnQuit();
@ "quit" : bool Quit();

@END

#endif
