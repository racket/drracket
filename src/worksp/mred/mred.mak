# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=mred - Win32 Release
!MESSAGE No configuration specified.  Defaulting to mred - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "mred - Win32 Release" && "$(CFG)" != "mred - Win32 Debug" &&\
 "$(CFG)" != "mred - Win32 SGC"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "mred.mak" CFG="mred - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mred - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "mred - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE "mred - Win32 SGC" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "mred - Win32 Debug"
CPP=cl.exe
RSC=rc.exe
MTL=mktyplib.exe

!IF  "$(CFG)" == "mred - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=Release
INTDIR=Release

ALL : "$(OUTDIR)\mred.exe"

CLEAN : 
	-@erase "Release\vc40.pdb"
	-@erase "Release\mred.exe"
	-@erase "Release\WX_STYLE.obj"
	-@erase "Release\WX_MEDIA.obj"
	-@erase "Release\WX_MEDAD.obj"
	-@erase "Release\xcglue.obj"
	-@erase "Release\WX_KEYM.obj"
	-@erase "Release\mredgcpp.obj"
	-@erase "Release\WX_MSNIP.obj"
	-@erase "Release\WX_MPRIV.obj"
	-@erase "Release\WX_MPBRD.obj"
	-@erase "Release\WX_SNIP.obj"
	-@erase "Release\WX_CGREC.obj"
	-@erase "Release\WX_MBUF.obj"
	-@erase "Release\MREDMSW.obj"
	-@erase "Release\MRED.obj"
	-@erase "Release\WX_MLINE.obj"
	-@erase "Release\EDJR.obj"
	-@erase "Release\WX_MEDIO.obj"
	-@erase "Release\Mred.res"
	-@erase "Release\mred.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /D "NDEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT" /D "__WINDOWS__" /D "WXME_FOR_MRED" /YX /c
CPP_PROJ=/nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I\
 "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I\
 "..\..\mzscheme\include" /I "..\..\mred\wxme" /I\
 "..\..\mzscheme\utils" /D "NDEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS"\
 /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT" /D "__WINDOWS__" /D "WXME_FOR_MRED"\
 /Fp"$(INTDIR)/mred.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=Release/
CPP_SBRS=
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /i "..\..\wxwindow\include\msw" /i "..\..\wxwindow\contrib\fafa" /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/Mred.res" /i "..\..\wxwindow\include\msw" /i\
 "..\..\wxwindow\contrib\fafa" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/mred.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 ..\wxs\Release\wxs.lib ..\wxutils\Release\wxutils.lib ..\wxwin\Release\wxwin.lib ..\mzsrc\Release\mzsrc.lib ..\gc\Release\gc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo /subsystem:windows /debug /machine:I386 /nodefaultlib:"libc.lib"
# SUBTRACT LINK32 /incremental:yes
LINK32_FLAGS=..\wxs\Release\wxs.lib\
 ..\wxutils\Release\wxutils.lib\
 ..\wxwin\Release\wxwin.lib\
 ..\mzsrc\Release\mzsrc.lib ..\gc\Release\gc.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib\
 winmm.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)/mred.pdb"\
 /debug /machine:I386 /nodefaultlib:"libc.lib" /out:"$(OUTDIR)/mred.exe" 
LINK32_OBJS= \
	"$(INTDIR)/WX_STYLE.obj" \
	"$(INTDIR)/WX_MEDIA.obj" \
	"$(INTDIR)/WX_MEDAD.obj" \
	"$(INTDIR)/xcglue.obj" \
	"$(INTDIR)/WX_KEYM.obj" \
	"$(INTDIR)/mredgcpp.obj" \
	"$(INTDIR)/WX_MSNIP.obj" \
	"$(INTDIR)/WX_MPRIV.obj" \
	"$(INTDIR)/WX_MPBRD.obj" \
	"$(INTDIR)/WX_SNIP.obj" \
	"$(INTDIR)/WX_CGREC.obj" \
	"$(INTDIR)/WX_MBUF.obj" \
	"$(INTDIR)/MREDMSW.obj" \
	"$(INTDIR)/MRED.obj" \
	"$(INTDIR)/WX_MLINE.obj" \
	"$(INTDIR)/EDJR.obj" \
	"$(INTDIR)/WX_MEDIO.obj" \
	"$(INTDIR)/Mred.res"

"$(OUTDIR)\mred.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=Debug

ALL : "$(OUTDIR)\mred.exe" "$(OUTDIR)\mred.bsc"

CLEAN : 
	-@erase "Debug\vc40.pdb"
	-@erase "Debug\vc40.idb"
	-@erase ".\Debug\mred.bsc"
	-@erase "Debug\WX_MEDIA.sbr"
	-@erase "Debug\EDJR.sbr"
	-@erase "Debug\mredgcpp.sbr"
	-@erase "Debug\WX_SNIP.sbr"
	-@erase "Debug\WX_KEYM.sbr"
	-@erase "Debug\WX_MPRIV.sbr"
	-@erase "Debug\WX_MPBRD.sbr"
	-@erase "Debug\WX_MBUF.sbr"
	-@erase "Debug\MREDMSW.sbr"
	-@erase "Debug\WX_CGREC.sbr"
	-@erase "Debug\WX_STYLE.sbr"
	-@erase "Debug\WX_MEDAD.sbr"
	-@erase "Debug\WX_MLINE.sbr"
	-@erase "Debug\MRED.sbr"
	-@erase "Debug\WX_MSNIP.sbr"
	-@erase "Debug\WX_MEDIO.sbr"
	-@erase "Debug\xcglue.sbr"
	-@erase ".\Debug\mred.exe"
	-@erase "Debug\WX_MLINE.obj"
	-@erase "Debug\MRED.obj"
	-@erase "Debug\WX_MSNIP.obj"
	-@erase "Debug\WX_MEDIO.obj"
	-@erase "Debug\xcglue.obj"
	-@erase "Debug\WX_MEDIA.obj"
	-@erase "Debug\EDJR.obj"
	-@erase "Debug\mredgcpp.obj"
	-@erase "Debug\WX_SNIP.obj"
	-@erase "Debug\WX_KEYM.obj"
	-@erase "Debug\WX_MPRIV.obj"
	-@erase "Debug\WX_MPBRD.obj"
	-@erase "Debug\WX_MBUF.obj"
	-@erase "Debug\MREDMSW.obj"
	-@erase "Debug\WX_CGREC.obj"
	-@erase "Debug\WX_STYLE.obj"
	-@erase "Debug\WX_MEDAD.obj"
	-@erase "Debug\Mred.res"
	-@erase ".\Debug\mred.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MTd /W3 /Gm /Zi /Od /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /D "_DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT" /D "__WINDOWS__" /D "WXME_FOR_MRED" /FR /YX /c
CPP_PROJ=/nologo /MTd /W3 /Gm /Zi /Od /I "..\..\mzscheme\gc" /I\
 "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I\
 "..\..\mzscheme\include" /I "..\..\mred\wxme" /I\
 "..\..\mzscheme\utils" /D "_DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS"\
 /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT" /D "__WINDOWS__" /D "WXME_FOR_MRED"\
 /FR"$(INTDIR)/" /Fp"$(INTDIR)/mred.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=Debug/
CPP_SBRS=Debug/
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /i "..\..\wxwindow\include\msw" /i "..\..\wxwindow\contrib\fafa" /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/Mred.res" /i "..\..\wxwindow\include\msw" /i\
 "..\..\wxwindow\contrib\fafa" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/mred.bsc" 
BSC32_SBRS= \
	"$(INTDIR)/WX_MEDIA.sbr" \
	"$(INTDIR)/EDJR.sbr" \
	"$(INTDIR)/mredgcpp.sbr" \
	"$(INTDIR)/WX_SNIP.sbr" \
	"$(INTDIR)/WX_KEYM.sbr" \
	"$(INTDIR)/WX_MPRIV.sbr" \
	"$(INTDIR)/WX_MPBRD.sbr" \
	"$(INTDIR)/WX_MBUF.sbr" \
	"$(INTDIR)/MREDMSW.sbr" \
	"$(INTDIR)/WX_CGREC.sbr" \
	"$(INTDIR)/WX_STYLE.sbr" \
	"$(INTDIR)/WX_MEDAD.sbr" \
	"$(INTDIR)/WX_MLINE.sbr" \
	"$(INTDIR)/MRED.sbr" \
	"$(INTDIR)/WX_MSNIP.sbr" \
	"$(INTDIR)/WX_MEDIO.sbr" \
	"$(INTDIR)/xcglue.sbr"

"$(OUTDIR)\mred.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 libc.lib ..\wxs\Debug\wxs.lib ..\wxutils\Debug\wxutils.lib ..\wxwin\Debug\wxwin.lib ..\mzsrc\Debug\mzsrc.lib ..\gc\Debug\gc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo /subsystem:windows /incremental:no /debug /machine:I386 /nodefaultlib:"libcd.lib"
# SUBTRACT LINK32 /pdb:none
LINK32_FLAGS=libc.lib ..\wxs\Debug\wxs.lib\
 ..\wxutils\Debug\wxutils.lib\
 ..\wxwin\Debug\wxwin.lib ..\mzsrc\Debug\mzsrc.lib\
 ..\gc\Debug\gc.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo\
 /subsystem:windows /incremental:no /pdb:"$(OUTDIR)/mred.pdb" /debug\
 /machine:I386 /nodefaultlib:"libcd.lib" /out:"$(OUTDIR)/mred.exe" 
LINK32_OBJS= \
	"$(INTDIR)/WX_MLINE.obj" \
	"$(INTDIR)/MRED.obj" \
	"$(INTDIR)/WX_MSNIP.obj" \
	"$(INTDIR)/WX_MEDIO.obj" \
	"$(INTDIR)/xcglue.obj" \
	"$(INTDIR)/WX_MEDIA.obj" \
	"$(INTDIR)/EDJR.obj" \
	"$(INTDIR)/mredgcpp.obj" \
	"$(INTDIR)/WX_SNIP.obj" \
	"$(INTDIR)/WX_KEYM.obj" \
	"$(INTDIR)/WX_MPRIV.obj" \
	"$(INTDIR)/WX_MPBRD.obj" \
	"$(INTDIR)/WX_MBUF.obj" \
	"$(INTDIR)/MREDMSW.obj" \
	"$(INTDIR)/WX_CGREC.obj" \
	"$(INTDIR)/WX_STYLE.obj" \
	"$(INTDIR)/WX_MEDAD.obj" \
	"$(INTDIR)/Mred.res"

"$(OUTDIR)\mred.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "mred___W"
# PROP BASE Intermediate_Dir "mred___W"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "SGC"
# PROP Intermediate_Dir "SGC"
# PROP Target_Dir ""
OUTDIR=.\SGC
INTDIR=SGC

ALL : "$(OUTDIR)\mred.exe" "$(OUTDIR)\mred.bsc"

CLEAN : 
	-@erase "SGC\vc40.pdb"
	-@erase "SGC\vc40.idb"
	-@erase ".\SGC\mred.bsc"
	-@erase "SGC\WX_SNIP.sbr"
	-@erase "SGC\WX_KEYM.sbr"
	-@erase "SGC\WX_MBUF.sbr"
	-@erase "SGC\MREDMSW.sbr"
	-@erase "SGC\xcglue.sbr"
	-@erase "SGC\WX_MLINE.sbr"
	-@erase "SGC\WX_STYLE.sbr"
	-@erase "SGC\EDJR.sbr"
	-@erase "SGC\WX_MEDAD.sbr"
	-@erase "SGC\mredgcpp.sbr"
	-@erase "SGC\WX_MSNIP.sbr"
	-@erase "SGC\MRED.sbr"
	-@erase "SGC\WX_MPRIV.sbr"
	-@erase "SGC\WX_MEDIO.sbr"
	-@erase "SGC\WX_MPBRD.sbr"
	-@erase "SGC\WX_MEDIA.sbr"
	-@erase "SGC\WX_CGREC.sbr"
	-@erase ".\SGC\mred.exe"
	-@erase "SGC\MRED.obj"
	-@erase "SGC\WX_MPRIV.obj"
	-@erase "SGC\WX_MEDIO.obj"
	-@erase "SGC\WX_MPBRD.obj"
	-@erase "SGC\WX_MEDIA.obj"
	-@erase "SGC\WX_CGREC.obj"
	-@erase "SGC\WX_SNIP.obj"
	-@erase "SGC\WX_KEYM.obj"
	-@erase "SGC\WX_MBUF.obj"
	-@erase "SGC\MREDMSW.obj"
	-@erase "SGC\xcglue.obj"
	-@erase "SGC\WX_MLINE.obj"
	-@erase "SGC\WX_STYLE.obj"
	-@erase "SGC\EDJR.obj"
	-@erase "SGC\WX_MEDAD.obj"
	-@erase "SGC\mredgcpp.obj"
	-@erase "SGC\WX_MSNIP.obj"
	-@erase "SGC\Mred.res"
	-@erase ".\SGC\mred.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /Zi /Od /I "c:\matthew\wxwindow\include\base" /I "c:\Matthew\wxwindow\include\msw" /I "c:\Matthew\mred\mzscheme\include" /I "c:\Matthew\mred\mzscheme\gc" /I "c:\Matthew\mred\wxme" /I "c:\Matthew\mred\mzscheme\utils" /D "_DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT" /D "__WINDOWS__" /D "WXME_FOR_MRED" /FR /YX /c
# ADD CPP /nologo /MTd /W3 /Gm /Zi /Od /I "..\..\mred\mzscheme\sgc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mred\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mred\mzscheme\utils" /D "_DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT" /D "__WINDOWS__" /D "WXME_FOR_MRED" /D "USE_SENORA_GC" /D "USE_WXOBJECT_TRACE_COUNTER" /FR /YX /c
CPP_PROJ=/nologo /MTd /W3 /Gm /Zi /Od /I "..\..\mred\mzscheme\sgc" /I\
 "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I\
 "..\..\mred\mzscheme\include" /I "..\..\mred\wxme" /I\
 "..\..\mred\mzscheme\utils" /D "_DEBUG" /D "__STDC__" /D "WIN32" /D\
 "_WINDOWS" /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT" /D "__WINDOWS__" /D\
 "WXME_FOR_MRED" /D "USE_SENORA_GC" /D "USE_WXOBJECT_TRACE_COUNTER"\
 /FR"$(INTDIR)/" /Fp"$(INTDIR)/mred.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=SGC/
CPP_SBRS=SGC/
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /i "c:\matthew\wxwindow\include\msw" /i "c:\matthew\wxwindow\contrib\fafa" /d "_DEBUG"
# ADD RSC /l 0x409 /i "..\..\wxwindow\include\msw" /i "..\..\wxwindow\contrib\fafa" /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/Mred.res" /i "..\..\wxwindow\include\msw" /i\
 "..\..\wxwindow\contrib\fafa" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/mred.bsc" 
BSC32_SBRS= \
	"$(INTDIR)/WX_SNIP.sbr" \
	"$(INTDIR)/WX_KEYM.sbr" \
	"$(INTDIR)/WX_MBUF.sbr" \
	"$(INTDIR)/MREDMSW.sbr" \
	"$(INTDIR)/xcglue.sbr" \
	"$(INTDIR)/WX_MLINE.sbr" \
	"$(INTDIR)/WX_STYLE.sbr" \
	"$(INTDIR)/EDJR.sbr" \
	"$(INTDIR)/WX_MEDAD.sbr" \
	"$(INTDIR)/mredgcpp.sbr" \
	"$(INTDIR)/WX_MSNIP.sbr" \
	"$(INTDIR)/MRED.sbr" \
	"$(INTDIR)/WX_MPRIV.sbr" \
	"$(INTDIR)/WX_MEDIO.sbr" \
	"$(INTDIR)/WX_MPBRD.sbr" \
	"$(INTDIR)/WX_MEDIA.sbr" \
	"$(INTDIR)/WX_CGREC.sbr"

"$(OUTDIR)\mred.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 libc.lib ..\wxs\Debug\wxs.lib ..\wxwin\Debug\wxwin.lib ..\mzsrc\DebugOpt\mzsrc.lib ..\gc\DebugOpt\gc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo /subsystem:windows /incremental:no /debug /machine:I386 /nodefaultlib:"libcd.lib"
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 ..\wxs\SGC\wxs.lib ..\wxutils\SGC\wxutils.lib ..\wxwin\SGC\wxwin.lib ..\mzsrc\SGC\mzsrc.lib ..\sgc\Debug\sgc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib libc.lib /nologo /subsystem:windows /incremental:no /debug /machine:I386 /nodefaultlib:"libcd.lib"
# SUBTRACT LINK32 /pdb:none
LINK32_FLAGS=..\wxs\SGC\wxs.lib\
 ..\wxutils\SGC\wxutils.lib ..\wxwin\SGC\wxwin.lib\
 ..\mzsrc\SGC\mzsrc.lib ..\sgc\Debug\sgc.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib\
 winmm.lib libc.lib /nologo /subsystem:windows /incremental:no\
 /pdb:"$(OUTDIR)/mred.pdb" /debug /machine:I386 /nodefaultlib:"libcd.lib"\
 /out:"$(OUTDIR)/mred.exe" 
LINK32_OBJS= \
	"$(INTDIR)/MRED.obj" \
	"$(INTDIR)/WX_MPRIV.obj" \
	"$(INTDIR)/WX_MEDIO.obj" \
	"$(INTDIR)/WX_MPBRD.obj" \
	"$(INTDIR)/WX_MEDIA.obj" \
	"$(INTDIR)/WX_CGREC.obj" \
	"$(INTDIR)/WX_SNIP.obj" \
	"$(INTDIR)/WX_KEYM.obj" \
	"$(INTDIR)/WX_MBUF.obj" \
	"$(INTDIR)/MREDMSW.obj" \
	"$(INTDIR)/xcglue.obj" \
	"$(INTDIR)/WX_MLINE.obj" \
	"$(INTDIR)/WX_STYLE.obj" \
	"$(INTDIR)/EDJR.obj" \
	"$(INTDIR)/WX_MEDAD.obj" \
	"$(INTDIR)/mredgcpp.obj" \
	"$(INTDIR)/WX_MSNIP.obj" \
	"$(INTDIR)/Mred.res"

"$(OUTDIR)\mred.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "mred - Win32 Release"
# Name "mred - Win32 Debug"
# Name "mred - Win32 SGC"

!IF  "$(CFG)" == "mred - Win32 Release"

!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxme\EDJR.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_EDJR_=\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\wxme\edjr.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	

"$(INTDIR)\EDJR.obj" : $(SOURCE) $(DEP_CPP_EDJR_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_EDJR_=\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\wxme\edjr.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\EDJR.obj" : $(SOURCE) $(DEP_CPP_EDJR_) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\EDJR.sbr" : $(SOURCE) $(DEP_CPP_EDJR_) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_EDJR_=\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\wxme\edjr.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	
NODEP_CPP_EDJR_=\
	".\..\..\wxwindow\include\base\gc_cpp.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\EDJR.obj" : $(SOURCE) $(DEP_CPP_EDJR_) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\EDJR.sbr" : $(SOURCE) $(DEP_CPP_EDJR_) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_CGREC.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_WX_CG=\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	

"$(INTDIR)\WX_CGREC.obj" : $(SOURCE) $(DEP_CPP_WX_CG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_WX_CG=\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_CGREC.obj" : $(SOURCE) $(DEP_CPP_WX_CG) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_CGREC.sbr" : $(SOURCE) $(DEP_CPP_WX_CG) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_WX_CG=\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	
NODEP_CPP_WX_CG=\
	".\..\..\wxwindow\include\base\gc_cpp.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_CGREC.obj" : $(SOURCE) $(DEP_CPP_WX_CG) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_CGREC.sbr" : $(SOURCE) $(DEP_CPP_WX_CG) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_KEYM.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_WX_KE=\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	

"$(INTDIR)\WX_KEYM.obj" : $(SOURCE) $(DEP_CPP_WX_KE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_WX_KE=\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_KEYM.obj" : $(SOURCE) $(DEP_CPP_WX_KE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_KEYM.sbr" : $(SOURCE) $(DEP_CPP_WX_KE) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_WX_KE=\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	
NODEP_CPP_WX_KE=\
	".\..\..\wxwindow\include\base\gc_cpp.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_KEYM.obj" : $(SOURCE) $(DEP_CPP_WX_KE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_KEYM.sbr" : $(SOURCE) $(DEP_CPP_WX_KE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MBUF.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_WX_MB=\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_print.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\mred\Wxme\wx_gclip.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	

"$(INTDIR)\WX_MBUF.obj" : $(SOURCE) $(DEP_CPP_WX_MB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_WX_MB=\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_print.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\mred\Wxme\wx_gclip.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MBUF.obj" : $(SOURCE) $(DEP_CPP_WX_MB) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MBUF.sbr" : $(SOURCE) $(DEP_CPP_WX_MB) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_WX_MB=\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_print.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\mred\Wxme\wx_gclip.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	
NODEP_CPP_WX_MB=\
	".\..\..\wxwindow\include\base\gc_cpp.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MBUF.obj" : $(SOURCE) $(DEP_CPP_WX_MB) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MBUF.sbr" : $(SOURCE) $(DEP_CPP_WX_MB) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MEDAD.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_WX_ME=\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\Wxme\wx_gcrct.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	

"$(INTDIR)\WX_MEDAD.obj" : $(SOURCE) $(DEP_CPP_WX_ME) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_WX_ME=\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\Wxme\wx_gcrct.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MEDAD.obj" : $(SOURCE) $(DEP_CPP_WX_ME) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MEDAD.sbr" : $(SOURCE) $(DEP_CPP_WX_ME) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_WX_ME=\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\Wxme\wx_gcrct.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	
NODEP_CPP_WX_ME=\
	".\..\..\wxwindow\include\base\gc_cpp.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MEDAD.obj" : $(SOURCE) $(DEP_CPP_WX_ME) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MEDAD.sbr" : $(SOURCE) $(DEP_CPP_WX_ME) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MEDIA.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_WX_MED=\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\Wxme\wx_mpriv.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\mred\Wxme\wx_mline.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\mred\Wxme\wx_gclip.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	

"$(INTDIR)\WX_MEDIA.obj" : $(SOURCE) $(DEP_CPP_WX_MED) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_WX_MED=\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\Wxme\wx_mpriv.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\mred\Wxme\wx_mline.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\mred\Wxme\wx_gclip.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MEDIA.obj" : $(SOURCE) $(DEP_CPP_WX_MED) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MEDIA.sbr" : $(SOURCE) $(DEP_CPP_WX_MED) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_WX_MED=\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\Wxme\wx_mpriv.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\mred\Wxme\wx_mline.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\mred\Wxme\wx_gclip.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	
NODEP_CPP_WX_MED=\
	".\..\..\wxwindow\include\base\gc_cpp.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MEDIA.obj" : $(SOURCE) $(DEP_CPP_WX_MED) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MEDIA.sbr" : $(SOURCE) $(DEP_CPP_WX_MED) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MEDIO.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_WX_MEDI=\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	

"$(INTDIR)\WX_MEDIO.obj" : $(SOURCE) $(DEP_CPP_WX_MEDI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_WX_MEDI=\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MEDIO.obj" : $(SOURCE) $(DEP_CPP_WX_MEDI) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MEDIO.sbr" : $(SOURCE) $(DEP_CPP_WX_MEDI) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_WX_MEDI=\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	
NODEP_CPP_WX_MEDI=\
	".\..\..\wxwindow\include\base\gc_cpp.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MEDIO.obj" : $(SOURCE) $(DEP_CPP_WX_MEDI) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MEDIO.sbr" : $(SOURCE) $(DEP_CPP_WX_MEDI) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MLINE.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_WX_ML=\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxme\wx_mline.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	

"$(INTDIR)\WX_MLINE.obj" : $(SOURCE) $(DEP_CPP_WX_ML) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_WX_ML=\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxme\wx_mline.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MLINE.obj" : $(SOURCE) $(DEP_CPP_WX_ML) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MLINE.sbr" : $(SOURCE) $(DEP_CPP_WX_ML) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_WX_ML=\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxme\wx_mline.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	
NODEP_CPP_WX_ML=\
	".\..\..\wxwindow\include\base\gc_cpp.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MLINE.obj" : $(SOURCE) $(DEP_CPP_WX_ML) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MLINE.sbr" : $(SOURCE) $(DEP_CPP_WX_ML) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MPBRD.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_WX_MP=\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxme\wx_gcrct.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	

"$(INTDIR)\WX_MPBRD.obj" : $(SOURCE) $(DEP_CPP_WX_MP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_WX_MP=\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxme\wx_gcrct.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MPBRD.obj" : $(SOURCE) $(DEP_CPP_WX_MP) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MPBRD.sbr" : $(SOURCE) $(DEP_CPP_WX_MP) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_WX_MP=\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxme\wx_gcrct.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	
NODEP_CPP_WX_MP=\
	".\..\..\wxwindow\include\base\gc_cpp.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MPBRD.obj" : $(SOURCE) $(DEP_CPP_WX_MP) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MPBRD.sbr" : $(SOURCE) $(DEP_CPP_WX_MP) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MPRIV.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_WX_MPR=\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\Wxme\wx_mpriv.h"\
	".\..\..\mred\Wxme\wx_gcrct.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\mred\Wxme\wx_mline.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\mred\Wxme\wx_gclip.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	

"$(INTDIR)\WX_MPRIV.obj" : $(SOURCE) $(DEP_CPP_WX_MPR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_WX_MPR=\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\Wxme\wx_mpriv.h"\
	".\..\..\mred\Wxme\wx_gcrct.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\mred\Wxme\wx_mline.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\mred\Wxme\wx_gclip.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MPRIV.obj" : $(SOURCE) $(DEP_CPP_WX_MPR) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MPRIV.sbr" : $(SOURCE) $(DEP_CPP_WX_MPR) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_WX_MPR=\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\Wxme\wx_mpriv.h"\
	".\..\..\mred\Wxme\wx_gcrct.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\mred\Wxme\wx_mline.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\mred\Wxme\wx_gclip.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	
NODEP_CPP_WX_MPR=\
	".\..\..\wxwindow\include\base\gc_cpp.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MPRIV.obj" : $(SOURCE) $(DEP_CPP_WX_MPR) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MPRIV.sbr" : $(SOURCE) $(DEP_CPP_WX_MPR) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MSNIP.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_WX_MS=\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxme\wx_mpriv.h"\
	".\..\..\mred\Wxme\wx_gcrct.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\mred\Wxme\wx_mline.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\mred\Wxme\wx_gclip.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	

"$(INTDIR)\WX_MSNIP.obj" : $(SOURCE) $(DEP_CPP_WX_MS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_WX_MS=\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxme\wx_mpriv.h"\
	".\..\..\mred\Wxme\wx_gcrct.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\mred\Wxme\wx_mline.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\mred\Wxme\wx_gclip.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MSNIP.obj" : $(SOURCE) $(DEP_CPP_WX_MS) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MSNIP.sbr" : $(SOURCE) $(DEP_CPP_WX_MS) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_WX_MS=\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxme\wx_mpriv.h"\
	".\..\..\mred\Wxme\wx_gcrct.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\mred\Wxme\wx_mline.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\mred\Wxme\wx_gclip.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	
NODEP_CPP_WX_MS=\
	".\..\..\wxwindow\include\base\gc_cpp.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_MSNIP.obj" : $(SOURCE) $(DEP_CPP_WX_MS) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_MSNIP.sbr" : $(SOURCE) $(DEP_CPP_WX_MS) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_SNIP.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_WX_SN=\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\mred\Wxme\wx_gcrct.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	

"$(INTDIR)\WX_SNIP.obj" : $(SOURCE) $(DEP_CPP_WX_SN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_WX_SN=\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\mred\Wxme\wx_gcrct.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_SNIP.obj" : $(SOURCE) $(DEP_CPP_WX_SN) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_SNIP.sbr" : $(SOURCE) $(DEP_CPP_WX_SN) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_WX_SN=\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\mred\Wxme\wx_gcrct.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	
NODEP_CPP_WX_SN=\
	".\..\..\wxwindow\include\base\gc_cpp.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_SNIP.obj" : $(SOURCE) $(DEP_CPP_WX_SN) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_SNIP.sbr" : $(SOURCE) $(DEP_CPP_WX_SN) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_STYLE.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_WX_ST=\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	

"$(INTDIR)\WX_STYLE.obj" : $(SOURCE) $(DEP_CPP_WX_ST) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_WX_ST=\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_STYLE.obj" : $(SOURCE) $(DEP_CPP_WX_ST) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_STYLE.sbr" : $(SOURCE) $(DEP_CPP_WX_ST) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_WX_ST=\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\Wxme\wx_ptreq.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	
NODEP_CPP_WX_ST=\
	".\..\..\wxwindow\include\base\gc_cpp.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\WX_STYLE.obj" : $(SOURCE) $(DEP_CPP_WX_ST) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\WX_STYLE.sbr" : $(SOURCE) $(DEP_CPP_WX_ST) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\MRED.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_MRED_=\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\wxme\edjr.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\wxs\wxsmred.h"\
	".\..\..\mred\Wxs\wxs_fram.h"\
	".\..\..\mred\wxs\wxs_obj.h"\
	".\..\..\mred\mred.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_MRED_=\
	".\..\..\mred\simpledrop.h"\
	".\..\..\mred\boundary.h"\
	".\..\..\mred\wxscheme.h"\
	".\..\..\mred\wxsmred.h"\
	".\..\..\mred\wxs_fram.h"\
	".\..\..\mred\wxs_obj.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\MRED.obj" : $(SOURCE) $(DEP_CPP_MRED_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_MRED_=\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\wxme\edjr.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\wxs\wxsmred.h"\
	".\..\..\mred\Wxs\wxs_fram.h"\
	".\..\..\mred\wxs\wxs_obj.h"\
	".\..\..\mred\mred.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_MRED_=\
	".\..\..\mred\simpledrop.h"\
	".\..\..\mred\boundary.h"\
	".\..\..\mred\wxscheme.h"\
	".\..\..\mred\wxsmred.h"\
	".\..\..\mred\wxs_fram.h"\
	".\..\..\mred\wxs_obj.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\MRED.obj" : $(SOURCE) $(DEP_CPP_MRED_) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\MRED.sbr" : $(SOURCE) $(DEP_CPP_MRED_) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_MRED_=\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\wxme\edjr.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\wxs\wxsmred.h"\
	".\..\..\mred\Wxs\wxs_fram.h"\
	".\..\..\mred\wxs\wxs_obj.h"\
	".\..\..\mred\mred.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	
NODEP_CPP_MRED_=\
	".\..\..\mred\simpledrop.h"\
	".\..\..\mred\boundary.h"\
	".\..\..\mred\wxscheme.h"\
	".\..\..\mred\wxsmred.h"\
	".\..\..\mred\wxs_fram.h"\
	".\..\..\mred\wxs_obj.h"\
	".\..\..\wxwindow\include\base\gc_cpp.h"\
	".\..\..\MRED\WXS\xcglue.h"\
	".\..\..\MZSCHEME\UTILS\scheme.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\MRED.obj" : $(SOURCE) $(DEP_CPP_MRED_) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\MRED.sbr" : $(SOURCE) $(DEP_CPP_MRED_) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\MREDMSW.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_MREDM=\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\mred\mred.h"\
	".\..\..\mzscheme\src\schwinfd.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	
NODEP_CPP_MREDM=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\MREDMSW.obj" : $(SOURCE) $(DEP_CPP_MREDM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_MREDM=\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\mred\mred.h"\
	".\..\..\mzscheme\src\schwinfd.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	
NODEP_CPP_MREDM=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\MREDMSW.obj" : $(SOURCE) $(DEP_CPP_MREDM) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\MREDMSW.sbr" : $(SOURCE) $(DEP_CPP_MREDM) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_MREDM=\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\mred\mred.h"\
	".\..\..\mzscheme\src\schwinfd.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\Wxme\Wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxme\Wx_snip.h"\
	".\..\..\mred\Wxme\Wx_cgrec.h"\
	".\..\..\mred\Wxme\Wx_medad.h"\
	".\..\..\mred\Wxme\Wx_medpb.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\Wxme\wx_madm.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	
NODEP_CPP_MREDM=\
	".\..\..\mred\scheme.h"\
	".\..\..\wxwindow\include\base\gc_cpp.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\MREDMSW.obj" : $(SOURCE) $(DEP_CPP_MREDM) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\MREDMSW.sbr" : $(SOURCE) $(DEP_CPP_MREDM) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\mredgcpp.cxx

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_MREDG=\
	".\..\..\mzscheme\gc\gc_cpp.cc"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	

"$(INTDIR)\mredgcpp.obj" : $(SOURCE) $(DEP_CPP_MREDG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_MREDG=\
	".\..\..\mzscheme\gc\gc_cpp.cc"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\mredgcpp.obj" : $(SOURCE) $(DEP_CPP_MREDG) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\mredgcpp.sbr" : $(SOURCE) $(DEP_CPP_MREDG) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

NODEP_CPP_MREDG=\
	".\..\..\mred\gc_cpp.cc"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\mredgcpp.obj" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\mredgcpp.sbr" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\Mred.rc
DEP_RSC_MRED_R=\
	".\mred.ico"\
	".\mreddoc.ico"\
	".\..\..\wxwindow\include\msw\wx.rc"\
	

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\Mred.res" : $(SOURCE) $(DEP_RSC_MRED_R) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\Mred.res" : $(SOURCE) $(DEP_RSC_MRED_R) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\Mred.res" : $(SOURCE) $(DEP_RSC_MRED_R) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=\MFLATT\PROJ\MZSCHEME\UTILS\xcglue.c

!IF  "$(CFG)" == "mred - Win32 Release"

DEP_CPP_XCGLU=\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_XCGLU=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\xcglue.obj" : $(SOURCE) $(DEP_CPP_XCGLU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

DEP_CPP_XCGLU=\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_XCGLU=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\xcglue.obj" : $(SOURCE) $(DEP_CPP_XCGLU) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\xcglue.sbr" : $(SOURCE) $(DEP_CPP_XCGLU) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

DEP_CPP_XCGLU=\
	".\..\..\mzscheme\utils\xcglue.h"\
	
NODEP_CPP_XCGLU=\
	".\..\..\MZSCHEME\UTILS\gc.h"\
	".\..\..\MZSCHEME\UTILS\scheme.h"\
	

BuildCmds= \
	$(CPP) $(CPP_PROJ) $(SOURCE) \
	

"$(INTDIR)\xcglue.obj" : $(SOURCE) $(DEP_CPP_XCGLU) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\xcglue.sbr" : $(SOURCE) $(DEP_CPP_XCGLU) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
