# Microsoft Developer Studio Generated NMAKE File, Based on mzscheme.dsp
!IF "$(CFG)" == ""
CFG=mzscheme - Win32 Release
!MESSAGE No configuration specified. Defaulting to mzscheme - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "mzscheme - Win32 Release" && "$(CFG)" != "mzscheme - Win32 Debug" && "$(CFG)" != "mzscheme - Win32 SGC" && "$(CFG)" != "mzscheme - Win32 Threads" && "$(CFG)" != "mzscheme - Win32 MT DLL"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "mzscheme.mak" CFG="mzscheme - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mzscheme - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "mzscheme - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "mzscheme - Win32 SGC" (based on "Win32 (x86) Console Application")
!MESSAGE "mzscheme - Win32 Threads" (based on "Win32 (x86) Console Application")
!MESSAGE "mzscheme - Win32 MT DLL" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "mzscheme - Win32 Release"

OUTDIR=.\..\..\..\..\plt
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\..\..\..\..\plt
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\mzscheme.exe"

!ELSE 

ALL : "mzsrc - Win32 Release" "gc - Win32 Release" "$(OUTDIR)\mzscheme.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"gc - Win32 ReleaseCLEAN" "mzsrc - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\Main.obj"
	-@erase "$(INTDIR)\mzscheme.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\mzscheme.exe"
	-@erase "$(OUTDIR)\mzscheme.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /Fp"$(INTDIR)\mzscheme.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\mzscheme.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mzscheme.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=..\mzsrc\Release\mzsrc.lib ..\gc\Release\gc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\mzscheme.pdb" /debug /machine:I386 /out:"$(OUTDIR)\mzscheme.exe" 
LINK32_OBJS= \
	"$(INTDIR)\Main.obj" \
	"$(INTDIR)\mzscheme.res" \
	"$(OUTDIR)\src\worksp\gc\Release\gc.lib" \
	"$(OUTDIR)\src\worksp\mzsrc\Release\mzsrc.lib"

"$(OUTDIR)\mzscheme.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mzscheme - Win32 Debug"

OUTDIR=.\..\..\..\..\plt
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\..\..\..\..\plt
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\mzscheme.exe"

!ELSE 

ALL : "mzsrc - Win32 Debug" "gc - Win32 Debug" "$(OUTDIR)\mzscheme.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"gc - Win32 DebugCLEAN" "mzsrc - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\Main.obj"
	-@erase "$(INTDIR)\mzscheme.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\mzscheme.exe"
	-@erase "$(OUTDIR)\mzscheme.ilk"
	-@erase "$(OUTDIR)\mzscheme.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /I "..\..\mzscheme\include" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /Fp"$(INTDIR)\mzscheme.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\mzscheme.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mzscheme.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=..\mzsrc\Debug\mzsrc.lib ..\gc\Debug\gc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\mzscheme.pdb" /debug /machine:I386 /out:"$(OUTDIR)\mzscheme.exe" 
LINK32_OBJS= \
	"$(INTDIR)\Main.obj" \
	"$(INTDIR)\mzscheme.res" \
	"$(OUTDIR)\src\worksp\gc\Debug\gc.lib" \
	"$(OUTDIR)\src\worksp\mzsrc\Debug\mzsrc.lib"

"$(OUTDIR)\mzscheme.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mzscheme - Win32 SGC"

OUTDIR=.\..\..\..\..\plt
INTDIR=.\SGC
# Begin Custom Macros
OutDir=.\..\..\..\..\plt
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\mzscheme.exe"

!ELSE 

ALL : "mzsrc - Win32 SGC" "$(OUTDIR)\mzscheme.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"mzsrc - Win32 SGCCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\Main.obj"
	-@erase "$(INTDIR)\mzscheme.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\mzscheme.exe"
	-@erase "$(OUTDIR)\mzscheme.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /I "..\..\mzscheme\include" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzscheme.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\mzscheme.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mzscheme.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=..\mzsrc\SGC\mzsrc.lib ..\sgc\Debug\sgc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\mzscheme.pdb" /debug /machine:I386 /nodefaultlib:"libcd.lib" /out:"$(OUTDIR)\mzscheme.exe" 
LINK32_OBJS= \
	"$(INTDIR)\Main.obj" \
	"$(INTDIR)\mzscheme.res" \
	"$(OUTDIR)\src\worksp\mzsrc\SGC\mzsrc.lib"

"$(OUTDIR)\mzscheme.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mzscheme - Win32 Threads"

OUTDIR=.\..\..\..\..\plt
INTDIR=.\Threads
# Begin Custom Macros
OutDir=.\..\..\..\..\plt
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\mzscheme.exe"

!ELSE 

ALL : "mzsrc - Win32 Threads" "gc - Win32 Threads" "$(OUTDIR)\mzscheme.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"gc - Win32 ThreadsCLEAN" "mzsrc - Win32 ThreadsCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\Main.obj"
	-@erase "$(INTDIR)\mzscheme.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\mzscheme.exe"
	-@erase "$(OUTDIR)\mzscheme.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "WIN32_THREADS" /Fp"$(INTDIR)\mzscheme.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\mzscheme.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mzscheme.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=..\mzsrc\Threads\mzsrc.lib ..\gc\Threads\gc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\mzscheme.pdb" /debug /machine:I386 /out:"$(OUTDIR)\mzscheme.exe" 
LINK32_OBJS= \
	"$(INTDIR)\Main.obj" \
	"$(INTDIR)\mzscheme.res" \
	"$(OUTDIR)\src\worksp\gc\Threads\gc.lib" \
	"$(OUTDIR)\src\worksp\mzsrc\Threads\mzsrc.lib"

"$(OUTDIR)\mzscheme.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mzscheme - Win32 MT DLL"

OUTDIR=.\MTDLL
INTDIR=.\MTDLL
# Begin Custom Macros
OutDir=.\MTDLL
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\mzscheme.exe"

!ELSE 

ALL : "mzsrc - Win32 MT DLL" "gc - Win32 MT DLL" "$(OUTDIR)\mzscheme.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"gc - Win32 MT DLLCLEAN" "mzsrc - Win32 MT DLLCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\Main.obj"
	-@erase "$(INTDIR)\mzscheme.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\mzscheme.exe"
	-@erase "$(OUTDIR)\mzscheme.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /D "WIN32" /D "_CONSOLE" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzscheme.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\mzscheme.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mzscheme.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=..\mzsrc\MTDLL\mzsrc.lib ..\gc\MTDLL\gc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\mzscheme.pdb" /debug /machine:I386 /out:"$(OUTDIR)\mzscheme.exe" 
LINK32_OBJS= \
	"$(INTDIR)\Main.obj" \
	"$(INTDIR)\mzscheme.res" \
	"..\gc\MTDLL\gc.lib" \
	"..\mzsrc\MTDLL\mzsrc.lib"

"$(OUTDIR)\mzscheme.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("mzscheme.dep")
!INCLUDE "mzscheme.dep"
!ELSE 
!MESSAGE Warning: cannot find "mzscheme.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "mzscheme - Win32 Release" || "$(CFG)" == "mzscheme - Win32 Debug" || "$(CFG)" == "mzscheme - Win32 SGC" || "$(CFG)" == "mzscheme - Win32 Threads" || "$(CFG)" == "mzscheme - Win32 MT DLL"
SOURCE=..\..\Mzscheme\Main.c

"$(INTDIR)\Main.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\mzscheme.rc

"$(INTDIR)\mzscheme.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


!IF  "$(CFG)" == "mzscheme - Win32 Release"

"gc - Win32 Release" : 
   cd "\Matthew\plt\src\worksp\gc"
   $(MAKE) /$(MAKEFLAGS) /F .\gc.mak CFG="gc - Win32 Release" 
   cd "..\mzscheme"

"gc - Win32 ReleaseCLEAN" : 
   cd "\Matthew\plt\src\worksp\gc"
   $(MAKE) /$(MAKEFLAGS) /F .\gc.mak CFG="gc - Win32 Release" RECURSE=1 CLEAN 
   cd "..\mzscheme"

!ELSEIF  "$(CFG)" == "mzscheme - Win32 Debug"

"gc - Win32 Debug" : 
   cd "\Matthew\plt\src\worksp\gc"
   $(MAKE) /$(MAKEFLAGS) /F .\gc.mak CFG="gc - Win32 Debug" 
   cd "..\mzscheme"

"gc - Win32 DebugCLEAN" : 
   cd "\Matthew\plt\src\worksp\gc"
   $(MAKE) /$(MAKEFLAGS) /F .\gc.mak CFG="gc - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\mzscheme"

!ELSEIF  "$(CFG)" == "mzscheme - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzscheme - Win32 Threads"

"gc - Win32 Threads" : 
   cd "\Matthew\plt\src\worksp\gc"
   $(MAKE) /$(MAKEFLAGS) /F .\gc.mak CFG="gc - Win32 Threads" 
   cd "..\mzscheme"

"gc - Win32 ThreadsCLEAN" : 
   cd "\Matthew\plt\src\worksp\gc"
   $(MAKE) /$(MAKEFLAGS) /F .\gc.mak CFG="gc - Win32 Threads" RECURSE=1 CLEAN 
   cd "..\mzscheme"

!ELSEIF  "$(CFG)" == "mzscheme - Win32 MT DLL"

"gc - Win32 MT DLL" : 
   cd "\Matthew\plt\src\worksp\gc"
   $(MAKE) /$(MAKEFLAGS) /F .\gc.mak CFG="gc - Win32 MT DLL" 
   cd "..\mzscheme"

"gc - Win32 MT DLLCLEAN" : 
   cd "\Matthew\plt\src\worksp\gc"
   $(MAKE) /$(MAKEFLAGS) /F .\gc.mak CFG="gc - Win32 MT DLL" RECURSE=1 CLEAN 
   cd "..\mzscheme"

!ENDIF 

!IF  "$(CFG)" == "mzscheme - Win32 Release"

"mzsrc - Win32 Release" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 Release" 
   cd "..\mzscheme"

"mzsrc - Win32 ReleaseCLEAN" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 Release" RECURSE=1 CLEAN 
   cd "..\mzscheme"

!ELSEIF  "$(CFG)" == "mzscheme - Win32 Debug"

"mzsrc - Win32 Debug" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 Debug" 
   cd "..\mzscheme"

"mzsrc - Win32 DebugCLEAN" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\mzscheme"

!ELSEIF  "$(CFG)" == "mzscheme - Win32 SGC"

"mzsrc - Win32 SGC" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 SGC" 
   cd "..\mzscheme"

"mzsrc - Win32 SGCCLEAN" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 SGC" RECURSE=1 CLEAN 
   cd "..\mzscheme"

!ELSEIF  "$(CFG)" == "mzscheme - Win32 Threads"

"mzsrc - Win32 Threads" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 Threads" 
   cd "..\mzscheme"

"mzsrc - Win32 ThreadsCLEAN" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 Threads" RECURSE=1 CLEAN 
   cd "..\mzscheme"

!ELSEIF  "$(CFG)" == "mzscheme - Win32 MT DLL"

"mzsrc - Win32 MT DLL" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 MT DLL" 
   cd "..\mzscheme"

"mzsrc - Win32 MT DLLCLEAN" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 MT DLL" RECURSE=1 CLEAN 
   cd "..\mzscheme"

!ENDIF 


!ENDIF 

