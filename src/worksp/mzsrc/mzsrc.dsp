# Microsoft Developer Studio Project File - Name="mzsrc" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=mzsrc - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "mzsrc.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "mzsrc.mak" CFG="mzsrc - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mzsrc - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "mzsrc - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "mzsrc - Win32 SGC" (based on "Win32 (x86) Static Library")
!MESSAGE "mzsrc - Win32 Threads" (based on "Win32 (x86) Static Library")
!MESSAGE "mzsrc - Win32 MT DLL" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "mzsrc - Win32 Release"

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
# ADD CPP /nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /YX /FD /Zm1000 /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo /o".\DebugOpt\mzsrc.bsc"
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

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
# ADD CPP /nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /YX /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\mzsrc___"
# PROP BASE Intermediate_Dir ".\mzsrc___"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\SGC"
# PROP Intermediate_Dir ".\SGC"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /Zi /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "MZWINCONSOLE" /YX /c
# ADD CPP /nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /YX /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\mzsrc___"
# PROP BASE Intermediate_Dir ".\mzsrc___"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\Threads"
# PROP Intermediate_Dir ".\Threads"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "MZWINCONSOLE" /YX /c
# ADD CPP /nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /YX /FD /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo /o"DebugOpt/mzsrc.bsc"
# ADD BSC32 /nologo /o".\DebugOpt\mzsrc.bsc"
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "mzsrc___Win32_MT_DLL"
# PROP BASE Intermediate_Dir "mzsrc___Win32_MT_DLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "MTDLL"
# PROP Intermediate_Dir "MTDLL"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "MZWINCONSOLE" /YX /FD /Zm1000 /c
# ADD CPP /nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /YX /FD /Zm1000 /c
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo /o".\DebugOpt\mzsrc.bsc"
# ADD BSC32 /nologo /o".\DebugOpt\mzsrc.bsc"
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "mzsrc - Win32 Release"
# Name "mzsrc - Win32 Debug"
# Name "mzsrc - Win32 SGC"
# Name "mzsrc - Win32 Threads"
# Name "mzsrc - Win32 MT DLL"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;for;f90"
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Bignum.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Bool.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\builtin.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Char.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Complex.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Dynext.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Env.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Error.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Eval.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\File.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Fun.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Hash.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\MZSCHEME\SRC\image.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\List.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\mzsj86.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\network.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\numarith.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Number.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\numcomp.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\numstr.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\objclass.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Object.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Port.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\portfun.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Print.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Process.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Promise.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Rational.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Read.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Regexp.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Salloc.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Sema.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Setjmpup.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\String.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Struct.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Symbol.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Syntax.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Tsymbol.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Type.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Unit.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\unitsig.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Vector.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

# ADD CPP /O2

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

# ADD BASE CPP /O2
# ADD CPP /O2

!ENDIF 

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
