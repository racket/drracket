# Microsoft Developer Studio Project File - Name="gc" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=gc - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "gc.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "gc.mak" CFG="gc - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "gc - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "gc - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "gc - Win32 Threads" (based on "Win32 (x86) Static Library")
!MESSAGE "gc - Win32 MT DLL" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "gc - Win32 Release"

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
# ADD CPP /nologo /MT /W3 /GX /Zi /O2 /D "NDEBUG" /D "__STDC__" /D "SILENT" /D "OLD_BLOCK_ALLOC" /YX /FD /c
# SUBTRACT CPP /X
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "gc - Win32 Debug"

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
# ADD CPP /nologo /MTd /W3 /GX /ZI /Od /D "_DEBUG" /D "__STDC__" /D "SILENT" /YX /FD /c
# SUBTRACT CPP /X
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "gc - Win32 Threads"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\gc___Win"
# PROP BASE Intermediate_Dir ".\gc___Win"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\Threads"
# PROP Intermediate_Dir ".\Threads"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "XXX_NO_AUTO_STACK_PUSH" /D "SILENT" /YX /c
# ADD CPP /nologo /MT /W3 /GX /Zi /O2 /D "NDEBUG" /D "WIN32_THREADS" /D "__STDC__" /D "SILENT" /YX /FD /c
# SUBTRACT CPP /X
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "gc - Win32 MT DLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "gc___Win32_MT_DLL"
# PROP BASE Intermediate_Dir "gc___Win32_MT_DLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "MTDLL"
# PROP Intermediate_Dir "MTDLL"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /Zi /O2 /D "NDEBUG" /D "__STDC__" /D "XXX_NO_AUTO_STACK_PUSH" /D "SILENT" /YX /FD /c
# SUBTRACT BASE CPP /X
# ADD CPP /nologo /MD /W3 /GX /Zi /O2 /D "__STDC__" /D "SILENT" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /YX /FD /c
# SUBTRACT CPP /X
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

# Name "gc - Win32 Release"
# Name "gc - Win32 Debug"
# Name "gc - Win32 Threads"
# Name "gc - Win32 MT DLL"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;for;f90"
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Allchblk.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Alloc.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Blacklst.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Dyn_load.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Finalize.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Headers.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Mach_dep.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Malloc.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Mallocx.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Mark.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Mark_rts.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Misc.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\New_hblk.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Obj_map.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Os_dep.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Reclaim.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Stubborn.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\gc\win32_threads.c
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
