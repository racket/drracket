# Microsoft Developer Studio Project File - Name="wxs" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=wxs - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "wxs.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wxs.mak" CFG="wxs - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wxs - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "wxs - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "wxs - Win32 SGC" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "wxs - Win32 Release"

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
# ADD CPP /nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "NDEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /YX"wx.h" /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

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
# ADD CPP /nologo /MTd /W3 /Gm /ZI /Od /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "__DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /YX"wx.h" /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\wxs___Wi"
# PROP BASE Intermediate_Dir ".\wxs___Wi"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\SGC"
# PROP Intermediate_Dir ".\SGC"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /Zi /Od /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /D "__DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT" /D "__WINDOWS__" /D "WXME_FOR_MRED" /YX /c
# ADD CPP /nologo /MTd /W3 /Gm /ZI /Od /I "..\..\mzscheme\sgc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "__DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "USE_SENORA_GC" /YX"wx.h" /FD /c
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

# Name "wxs - Win32 Release"
# Name "wxs - Win32 Debug"
# Name "wxs - Win32 SGC"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;for;f90"
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_BMAP.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_BUTN.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_CHCE.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_CKBX.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_CNVS.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_DC.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_EVNT.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_FRAM.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_GAGE.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_GDI.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_GLOB.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_ITEM.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_LBOX.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_MADM.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_MEDE.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_MEDI.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_MENU.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_MIO.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_MISC.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_MPB.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_OBJ.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_PANL.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_RADO.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_SLID.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_SNIP.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_STYL.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_WIN.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\WXSCHEME.cxx
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_bmap.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_butn.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_chce.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_ckbx.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_cnvs.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_dc.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_evnt.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_fram.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_gage.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_gdi.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_glob.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_item.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_lbox.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_madm.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_mede.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_medi.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_menu.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_mio.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_misc.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_mpb.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_obj.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_panl.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_rado.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_slid.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_snip.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_styl.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_text.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxs_win.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxs\wxscheme.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
