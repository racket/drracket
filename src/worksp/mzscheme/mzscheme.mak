# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=mzscheme - Win32 Release
!MESSAGE No configuration specified.  Defaulting to mzscheme - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "mzscheme - Win32 Release" && "$(CFG)" !=\
 "mzscheme - Win32 Debug" && "$(CFG)" != "mzscheme - Win32 SGC" && "$(CFG)" !=\
 "mzscheme - Win32 Threads"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "mzscheme.mak" CFG="mzscheme - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mzscheme - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "mzscheme - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "mzscheme - Win32 SGC" (based on "Win32 (x86) Console Application")
!MESSAGE "mzscheme - Win32 Threads" (based on\
 "Win32 (x86) Console Application")
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
# PROP Target_Last_Scanned "mzscheme - Win32 Debug"
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "mzscheme - Win32 Release"

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

ALL : "$(OUTDIR)\mzscheme.exe"

CLEAN : 
	-@erase "Release\vc40.pdb"
	-@erase "Release\mzscheme.exe"
	-@erase "Release\Main.obj"
	-@erase "Release\mzscheme.res"
	-@erase "Release\mzscheme.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /D "WIN32"\
 /D "NDEBUG" /D "_CONSOLE" /Fp"$(INTDIR)/mzscheme.pch" /YX /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c 
CPP_OBJS=Release/
CPP_SBRS=
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/mzscheme.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/mzscheme.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 ..\mzsrc\Release\mzsrc.lib ..\gc\Release\gc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console /debug /machine:I386
# SUBTRACT LINK32 /pdb:none /incremental:yes
LINK32_FLAGS=..\mzsrc\Release\mzsrc.lib\
 ..\gc\Release\gc.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console\
 /incremental:no /pdb:"$(OUTDIR)/mzscheme.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/mzscheme.exe" 
LINK32_OBJS= \
	"$(INTDIR)/Main.obj" \
	"$(INTDIR)/mzscheme.res"

"$(OUTDIR)\mzscheme.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mzscheme - Win32 Debug"

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
OUTDIR=Debug
INTDIR=Debug

ALL : "$(OUTDIR)\mzscheme.exe"

CLEAN : 
	-@erase "Debug\vc40.pdb"
	-@erase "Debug\vc40.idb"
	-@erase "Debug\mzscheme.exe"
	-@erase "Debug\Main.obj"
	-@erase "Debug\mzscheme.res"
	-@erase "Debug\mzscheme.ilk"
	-@erase "Debug\mzscheme.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "..\..\mzscheme\include" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "..\..\mzscheme\include" /D\
 "WIN32" /D "_DEBUG" /D "_CONSOLE" /Fp"$(INTDIR)/mzscheme.pch" /YX\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=Debug/
CPP_SBRS=
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/mzscheme.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/mzscheme.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 ..\mzsrc\Debug\mzsrc.lib ..\gc\Debug\gc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console /debug /machine:I386
# SUBTRACT LINK32 /pdb:none
LINK32_FLAGS=..\mzsrc\Debug\mzsrc.lib\
 ..\gc\Debug\gc.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console\
 /incremental:yes /pdb:"$(OUTDIR)/mzscheme.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/mzscheme.exe" 
LINK32_OBJS= \
	"$(INTDIR)/Main.obj" \
	"$(INTDIR)/mzscheme.res"

"$(OUTDIR)\mzscheme.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mzscheme - Win32 SGC"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "mzscheme"
# PROP BASE Intermediate_Dir "mzscheme"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "SGC"
# PROP Intermediate_Dir "SGC"
# PROP Target_Dir ""
OUTDIR=SGC
INTDIR=SGC

ALL : "$(OUTDIR)\mzscheme.exe"

CLEAN : 
	-@erase "SGC\vc40.pdb"
	-@erase "SGC\vc40.idb"
	-@erase "SGC\mzscheme.exe"
	-@erase "SGC\Main.obj"
	-@erase "SGC\mzscheme.res"
	-@erase "SGC\mzscheme.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /I "c:\Matthew\mzscheme\include" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "..\..\mzscheme\include" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D SGC_STD_DEBUGGING=1 /YX /c
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "..\..\mzscheme\include" /D\
 "WIN32" /D "_DEBUG" /D "_CONSOLE" /D SGC_STD_DEBUGGING=1\
 /Fp"$(INTDIR)/mzscheme.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=SGC/
CPP_SBRS=
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/mzscheme.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/mzscheme.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 ..\mzsrc\Debug\mzsrc.lib ..\gc\Debug\gc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console /debug /machine:I386
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 ..\mzsrc\SGC\mzsrc.lib ..\sgc\Debug\sgc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /nodefaultlib:"libcd.lib"
# SUBTRACT LINK32 /pdb:none
LINK32_FLAGS=..\mzsrc\SGC\mzsrc.lib\
 ..\sgc\Debug\sgc.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console\
 /incremental:no /pdb:"$(OUTDIR)/mzscheme.pdb" /debug /machine:I386\
 /nodefaultlib:"libcd.lib" /out:"$(OUTDIR)/mzscheme.exe" 
LINK32_OBJS= \
	"$(INTDIR)/Main.obj" \
	"$(INTDIR)/mzscheme.res"

"$(OUTDIR)\mzscheme.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mzscheme - Win32 Threads"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "mzscheme"
# PROP BASE Intermediate_Dir "mzscheme"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Threads"
# PROP Intermediate_Dir "Threads"
# PROP Target_Dir ""
OUTDIR=Threads
INTDIR=Threads

ALL : "$(OUTDIR)\mzscheme.exe"

CLEAN : 
	-@erase "Threads\vc40.pdb"
	-@erase "Threads\mzscheme.exe"
	-@erase "Threads\Main.obj"
	-@erase "Threads\mzscheme.res"
	-@erase "Threads\mzscheme.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "WIN32_THREADS" /YX /c
CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /D "WIN32"\
 /D "NDEBUG" /D "_CONSOLE" /D "WIN32_THREADS" /Fp"$(INTDIR)/mzscheme.pch" /YX\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=Threads/
CPP_SBRS=
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/mzscheme.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/mzscheme.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 ..\mzsrc\Release\mzsrc.lib ..\gc\Release\gc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console /machine:I386
# SUBTRACT BASE LINK32 /pdb:none /incremental:yes /debug
# ADD LINK32 ..\mzsrc\Threads\mzsrc.lib ..\gc\Threads\gc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console /debug /machine:I386
# SUBTRACT LINK32 /pdb:none /incremental:yes
LINK32_FLAGS=..\mzsrc\Threads\mzsrc.lib\
 ..\gc\Threads\gc.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console\
 /incremental:no /pdb:"$(OUTDIR)/mzscheme.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/mzscheme.exe" 
LINK32_OBJS= \
	"$(INTDIR)/Main.obj" \
	"$(INTDIR)/mzscheme.res"

"$(OUTDIR)\mzscheme.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "mzscheme - Win32 Release"
# Name "mzscheme - Win32 Debug"
# Name "mzscheme - Win32 SGC"
# Name "mzscheme - Win32 Threads"

!IF  "$(CFG)" == "mzscheme - Win32 Release"

!ELSEIF  "$(CFG)" == "mzscheme - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzscheme - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzscheme - Win32 Threads"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Main.c

!IF  "$(CFG)" == "mzscheme - Win32 Release"

DEP_CPP_MAIN_=\
	".\..\..\mzscheme\include\scheme.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_MAIN_=\
	".\..\..\Mzscheme\simpledrop.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\Main.obj" : $(SOURCE) $(DEP_CPP_MAIN_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzscheme - Win32 Debug"

DEP_CPP_MAIN_=\
	".\..\..\mzscheme\include\scheme.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	
NODEP_CPP_MAIN_=\
	".\..\..\Mzscheme\simpledrop.h"\
	

"$(INTDIR)\Main.obj" : $(SOURCE) $(DEP_CPP_MAIN_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzscheme - Win32 SGC"

DEP_CPP_MAIN_=\
	".\..\..\mzscheme\include\scheme.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	
NODEP_CPP_MAIN_=\
	".\..\..\Mzscheme\simpledrop.h"\
	

"$(INTDIR)\Main.obj" : $(SOURCE) $(DEP_CPP_MAIN_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzscheme - Win32 Threads"

DEP_CPP_MAIN_=\
	".\..\..\mzscheme\include\scheme.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_MAIN_=\
	".\..\..\Mzscheme\simpledrop.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\Main.obj" : $(SOURCE) $(DEP_CPP_MAIN_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mzscheme.rc
DEP_RSC_MZSCH=\
	".\mzscheme.ico"\
	

!IF  "$(CFG)" == "mzscheme - Win32 Release"


"$(INTDIR)\mzscheme.res" : $(SOURCE) $(DEP_RSC_MZSCH) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzscheme - Win32 Debug"


"$(INTDIR)\mzscheme.res" : $(SOURCE) $(DEP_RSC_MZSCH) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzscheme - Win32 SGC"


"$(INTDIR)\mzscheme.res" : $(SOURCE) $(DEP_RSC_MZSCH) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzscheme - Win32 Threads"


"$(INTDIR)\mzscheme.res" : $(SOURCE) $(DEP_RSC_MZSCH) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
