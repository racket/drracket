# Microsoft Developer Studio Project File - Name="wxwin" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=wxwin - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "wxwin.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wxwin.mak" CFG="wxwin - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wxwin - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "wxwin - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "wxwin - Win32 SGC" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "wxwin - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Release"
# PROP BASE Intermediate_Dir ".\Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\Release"
# PROP Intermediate_Dir ".\Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /YX"wx.h" /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\Debug"
# PROP BASE Intermediate_Dir ".\Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\Debug"
# PROP Intermediate_Dir ".\Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MTd /W3 /Z7 /Od /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /YX"wx.h" /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\wxwin___"
# PROP BASE Intermediate_Dir ".\wxwin___"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\SGC"
# PROP Intermediate_Dir ".\SGC"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Z7 /Od /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\mzscheme\gc" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW" /D WX_NORMALIZED_PS_FONTS=1 /YX"wx.h" /c
# ADD CPP /nologo /MTd /W3 /ZI /Od /I "..\..\mzscheme\sgc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "USE_SENORA_GC" /D "USE_WXOBJECT_TRACE_COUNT" /YX"wx.h" /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "wxwin - Win32 Release"
# Name "wxwin - Win32 Debug"
# Name "wxwin - Win32 SGC"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;for;f90"
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_CANVS.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_CMDLG.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_DATA.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_DC.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_DIALG.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_FRAME.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_GDI.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_HASH.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_ITEM.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_LIST.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_MAIN.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_MF.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_MGSTR.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_OBJ.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_PANEL.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_PRINT.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_PS.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_RES.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_STDEV.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_SYSEV.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_TIMER.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_TYPES.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_UTILS.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_WIN.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_BUTTN.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_CANVS.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_CHECK.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_CHOIC.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_CLIPB.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_CMDLG.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_DC.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_DIALG.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_FRAME.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_GAUGE.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_GDI.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_ITEM.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_LBOX.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_MAIN.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_MENU.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_MESSG.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_MF.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_PANEL.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\wx_pdf.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_RBOX.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_SLIDR.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_TIMER.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_UTILS.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_WIN.cxx
# End Source File
# Begin Source File

SOURCE=..\..\WXWINDOW\SRC\MSW\wximgfil.cxx
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
