# Microsoft Developer Studio Generated NMAKE File, Based on MzCOM.dsp
!IF "$(CFG)" == ""
CFG=MzCOM - Win32 Debug
!MESSAGE No configuration specified. Defaulting to MzCOM - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "MzCOM - Win32 Debug" && "$(CFG)" != "MzCOM - Win32 Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "MzCOM.mak" CFG="MzCOM - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "MzCOM - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE "MzCOM - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "MzCOM - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\MzCOM.exe" ".\Debug\regsvr32.trg"


CLEAN :
	-@erase "$(INTDIR)\mzcom.obj"
	-@erase "$(INTDIR)\mzcom.res"
	-@erase "$(INTDIR)\mzcom.tlb"
	-@erase "$(INTDIR)\mzobj.obj"
	-@erase "$(INTDIR)\stdafx.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\MzCOM.exe"
	-@erase "$(OUTDIR)\MzCOM.ilk"
	-@erase "$(OUTDIR)\MzCOM.pdb"
	-@erase ".\Debug\regsvr32.trg"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /Gm /ZI /Od /I "..\..\..\collects\mzscheme\include" /I "..\..\mzcom" /I "." /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_ATL_STATIC_REGISTRY" /Fp"$(INTDIR)\MzCOM.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\mzcom.res" /i ".\..\mzcom" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\MzCOM.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=libcmtd.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib mzsrc.lib gc.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\MzCOM.pdb" /debug /machine:I386 /nodefaultlib:"libcmt.lib" /out:"$(OUTDIR)\MzCOM.exe" /pdbtype:sept /libpath:"..\mzsrc\Release" /libpath:"..\gc\Release" 
LINK32_OBJS= \
	"$(INTDIR)\mzcom.obj" \
	"$(INTDIR)\mzobj.obj" \
	"$(INTDIR)\stdafx.obj" \
	"$(INTDIR)\mzcom.res"

"$(OUTDIR)\MzCOM.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

OutDir=.\Debug
TargetPath=.\Debug\MzCOM.exe
InputPath=.\Debug\MzCOM.exe
SOURCE="$(InputPath)"

"$(OUTDIR)\regsvr32.trg" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	<<tempfile.bat 
	@echo off 
	"$(TargetPath)" /RegServer 
	echo regsvr32 exec. time > "$(OutDir)\regsvr32.trg" 
	echo Server registration done! 
<< 
	

!ELSEIF  "$(CFG)" == "MzCOM - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\MzCOM.exe" "$(OUTDIR)\mzcom.tlb" ".\Release\regsvr32.trg"


CLEAN :
	-@erase "$(INTDIR)\mzcom.obj"
	-@erase "$(INTDIR)\mzcom.res"
	-@erase "$(INTDIR)\stdafx.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\MzCOM.exe"
	-@erase ".\Release\mzcom.tlb"
	-@erase ".\Release\mzobj.obj"
	-@erase ".\Release\regsvr32.trg"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /O1 /I "..\..\..\collects\mzscheme\include" /I "..\..\mzcom" /I "." /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_ATL_STATIC_REGISTRY" /Fp"$(INTDIR)\MzCOM.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\mzcom.res" /i "..\..\mzcom" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\MzCOM.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib mzsrc.lib gc.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\MzCOM.pdb" /machine:I386 /out:"$(OUTDIR)\MzCOM.exe" /libpath:"..\mzsrc\Release" /libpath:"..\gc\Release" 
LINK32_OBJS= \
	"$(INTDIR)\mzcom.obj" \
	".\Release\mzobj.obj" \
	"$(INTDIR)\stdafx.obj" \
	"$(INTDIR)\mzcom.res"

"$(OUTDIR)\MzCOM.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

OutDir=.\Release
TargetPath=.\Release\MzCOM.exe
InputPath=.\Release\MzCOM.exe
SOURCE="$(InputPath)"

"$(OUTDIR)\regsvr32.trg" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	<<tempfile.bat 
	@echo off 
	"$(TargetPath)" /RegServer 
	echo regsvr32 exec. time > "$(OutDir)\regsvr32.trg" 
	echo Server registration done! 
<< 
	

!ENDIF 

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

MTL_PROJ=

!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("MzCOM.dep")
!INCLUDE "MzCOM.dep"
!ELSE 
!MESSAGE Warning: cannot find "MzCOM.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "MzCOM - Win32 Debug" || "$(CFG)" == "MzCOM - Win32 Release"
SOURCE=..\..\mzcom\mzcom.cxx

!IF  "$(CFG)" == "MzCOM - Win32 Debug"

CPP_SWITCHES=/nologo /MT /W3 /Gm /ZI /Od /I "..\..\..\collects\mzscheme\include" /I "..\..\mzcom" /I "." /I "../../mzscheme/include ../worksp/mzcom" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_ATL_STATIC_REGISTRY" /Fp"$(INTDIR)\MzCOM.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

"$(INTDIR)\mzcom.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "MzCOM - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /O1 /I "..\..\..\collects\mzscheme\include" /I "..\..\mzcom" /I "." /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_ATL_STATIC_REGISTRY" /Fp"$(INTDIR)\MzCOM.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\mzcom.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\mzcom\mzcom.idl

!IF  "$(CFG)" == "MzCOM - Win32 Debug"

MTL_SWITCHES=/tlb "$(OUTDIR)\mzcom.tlb" 

"$(INTDIR)\mzcom.tlb" : $(SOURCE) "$(INTDIR)"
	$(MTL) @<<
  $(MTL_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "MzCOM - Win32 Release"

MTL_SWITCHES=/tlb "$(OUTDIR)\mzcom.tlb" 

"$(INTDIR)\mzcom.tlb" : $(SOURCE)
	$(MTL) @<<
  $(MTL_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\mzcom\mzobj.cxx

!IF  "$(CFG)" == "MzCOM - Win32 Debug"


"$(INTDIR)\mzobj.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "MzCOM - Win32 Release"


".\Release\mzobj.obj" : $(SOURCE)
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\mzcom\stdafx.cxx

"$(INTDIR)\stdafx.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\mzcom.rc

"$(INTDIR)\mzcom.res" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\mzcom.tlb"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

