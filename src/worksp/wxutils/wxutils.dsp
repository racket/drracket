# Microsoft Developer Studio Project File - Name="wxutils" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=wxutils - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "wxutils.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wxutils.mak" CFG="wxutils - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wxutils - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "wxutils - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "wxutils - Win32 SGC" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "wxutils - Win32 Release"

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
# ADD CPP /nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /FD /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "wxutils - Win32 Debug"

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
# ADD CPP /nologo /MTd /W3 /Z7 /Od /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /FD /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "wxutils - Win32 SGC"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\SGC"
# PROP BASE Intermediate_Dir ".\SGC"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\SGC"
# PROP Intermediate_Dir ".\SGC"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Z7 /Od /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\mred\mzscheme\gc" /I "..\..\wxWindow\contrib\fafa" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW" /D WX_NORMALIZED_PS_FONTS=1 /YX"wx.h" /c
# ADD CPP /nologo /MTd /W3 /ZI /Od /I "..\..\mzscheme\sgc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "USE_SENORA_GC" /D "USE_WXOBJECT_TRACE_COUNT" /FD /c
# SUBTRACT CPP /YX
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

# Name "wxutils - Win32 Release"
# Name "wxutils - Win32 Debug"
# Name "wxutils - Win32 SGC"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;for;f90"
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Button.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Check.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Cont.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crbuffri.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crdatfri.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Create.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crifrbuf.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crifrdat.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Data.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Dialog.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Utils\Dib\DIB.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Draw.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Dumfafa.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Fafa.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Hashtab.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Misc.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Parse.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Rdftodat.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Rdftoi.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Rgb.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Scan.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Simx.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Static.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Wrffrdat.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Wrffri.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Wrffrp.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\wximgxbm.cxx
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Gauge\Zyz3d.c
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Gauge\Zyzgauge.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=..\..\wxwindow\utils\dib\dib.h
# End Source File
# Begin Source File

SOURCE=..\..\wxWindow\contrib\fafa\fafa.h
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h
# End Source File
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Gauge\zyz3d.h
# End Source File
# Begin Source File

SOURCE=..\..\wxwindow\contrib\gauge\zyzgauge.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
