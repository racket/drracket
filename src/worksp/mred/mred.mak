# Microsoft Developer Studio Generated NMAKE File, Based on mred.dsp
!IF "$(CFG)" == ""
CFG=mred - Win32 Release
!MESSAGE No configuration specified. Defaulting to mred - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "mred - Win32 Release" && "$(CFG)" != "mred - Win32 Debug" && "$(CFG)" != "mred - Win32 SGC"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
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

!IF  "$(CFG)" == "mred - Win32 Release"

OUTDIR=.\..\..\..\..\plt
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\..\..\..\..\plt
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\mred.exe"

!ELSE 

ALL : "wxutils - Win32 Release" "wxs - Win32 Release" "gc - Win32 Release" "mzsrc - Win32 Release" "wxwin - Win32 Release" "$(OUTDIR)\mred.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"wxwin - Win32 ReleaseCLEAN" "mzsrc - Win32 ReleaseCLEAN" "gc - Win32 ReleaseCLEAN" "wxs - Win32 ReleaseCLEAN" "wxutils - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\MRED.obj"
	-@erase "$(INTDIR)\Mred.res"
	-@erase "$(INTDIR)\MREDMSW.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\WX_CGREC.obj"
	-@erase "$(INTDIR)\WX_KEYM.obj"
	-@erase "$(INTDIR)\WX_MBUF.obj"
	-@erase "$(INTDIR)\WX_MEDAD.obj"
	-@erase "$(INTDIR)\WX_MEDIA.obj"
	-@erase "$(INTDIR)\WX_MEDIO.obj"
	-@erase "$(INTDIR)\WX_MLINE.obj"
	-@erase "$(INTDIR)\WX_MPBRD.obj"
	-@erase "$(INTDIR)\WX_MPRIV.obj"
	-@erase "$(INTDIR)\WX_MSNIP.obj"
	-@erase "$(INTDIR)\WX_SNIP.obj"
	-@erase "$(INTDIR)\WX_STYLE.obj"
	-@erase "$(INTDIR)\wxGC.obj"
	-@erase "$(INTDIR)\xcglue.obj"
	-@erase "$(OUTDIR)\mred.exe"
	-@erase "$(OUTDIR)\mred.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /D "NDEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /Fp"$(INTDIR)\mred.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\Mred.res" /i "..\..\wxwindow\include\msw" /i "..\..\wxwindow\contrib\fafa" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mred.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=..\wxs\Release\wxs.lib ..\wxutils\Release\wxutils.lib ..\wxwin\Release\wxwin.lib ..\mzsrc\Release\mzsrc.lib ..\gc\Release\gc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\mred.pdb" /debug /machine:I386 /nodefaultlib:"libcd.lib" /out:"$(OUTDIR)\mred.exe" 
LINK32_OBJS= \
	"$(INTDIR)\MRED.obj" \
	"$(INTDIR)\MREDMSW.obj" \
	"$(INTDIR)\WX_CGREC.obj" \
	"$(INTDIR)\WX_KEYM.obj" \
	"$(INTDIR)\WX_MBUF.obj" \
	"$(INTDIR)\WX_MEDAD.obj" \
	"$(INTDIR)\WX_MEDIA.obj" \
	"$(INTDIR)\WX_MEDIO.obj" \
	"$(INTDIR)\WX_MLINE.obj" \
	"$(INTDIR)\WX_MPBRD.obj" \
	"$(INTDIR)\WX_MPRIV.obj" \
	"$(INTDIR)\WX_MSNIP.obj" \
	"$(INTDIR)\WX_SNIP.obj" \
	"$(INTDIR)\WX_STYLE.obj" \
	"$(INTDIR)\wxGC.obj" \
	"$(INTDIR)\xcglue.obj" \
	"$(INTDIR)\Mred.res" \
	"$(OUTDIR)\src\worksp\wxwin\Release\wxwin.lib" \
	"$(OUTDIR)\src\worksp\mzsrc\Release\mzsrc.lib" \
	"$(OUTDIR)\src\worksp\gc\Release\gc.lib" \
	"$(OUTDIR)\src\worksp\wxs\Release\wxs.lib" \
	"$(OUTDIR)\src\worksp\wxutils\Release\wxutils.lib"

"$(OUTDIR)\mred.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

OUTDIR=.\..\..\..\..\plt
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\..\..\..\..\plt
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\mred.exe" "$(OUTDIR)\mred.bsc"

!ELSE 

ALL : "wxutils - Win32 Debug" "wxs - Win32 Debug" "gc - Win32 Debug" "mzsrc - Win32 Debug" "wxwin - Win32 Debug" "$(OUTDIR)\mred.exe" "$(OUTDIR)\mred.bsc"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"wxwin - Win32 DebugCLEAN" "mzsrc - Win32 DebugCLEAN" "gc - Win32 DebugCLEAN" "wxs - Win32 DebugCLEAN" "wxutils - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\MRED.obj"
	-@erase "$(INTDIR)\Mred.res"
	-@erase "$(INTDIR)\MRED.sbr"
	-@erase "$(INTDIR)\MREDMSW.obj"
	-@erase "$(INTDIR)\MREDMSW.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\WX_CGREC.obj"
	-@erase "$(INTDIR)\WX_CGREC.sbr"
	-@erase "$(INTDIR)\WX_KEYM.obj"
	-@erase "$(INTDIR)\WX_KEYM.sbr"
	-@erase "$(INTDIR)\WX_MBUF.obj"
	-@erase "$(INTDIR)\WX_MBUF.sbr"
	-@erase "$(INTDIR)\WX_MEDAD.obj"
	-@erase "$(INTDIR)\WX_MEDAD.sbr"
	-@erase "$(INTDIR)\WX_MEDIA.obj"
	-@erase "$(INTDIR)\WX_MEDIA.sbr"
	-@erase "$(INTDIR)\WX_MEDIO.obj"
	-@erase "$(INTDIR)\WX_MEDIO.sbr"
	-@erase "$(INTDIR)\WX_MLINE.obj"
	-@erase "$(INTDIR)\WX_MLINE.sbr"
	-@erase "$(INTDIR)\WX_MPBRD.obj"
	-@erase "$(INTDIR)\WX_MPBRD.sbr"
	-@erase "$(INTDIR)\WX_MPRIV.obj"
	-@erase "$(INTDIR)\WX_MPRIV.sbr"
	-@erase "$(INTDIR)\WX_MSNIP.obj"
	-@erase "$(INTDIR)\WX_MSNIP.sbr"
	-@erase "$(INTDIR)\WX_SNIP.obj"
	-@erase "$(INTDIR)\WX_SNIP.sbr"
	-@erase "$(INTDIR)\WX_STYLE.obj"
	-@erase "$(INTDIR)\WX_STYLE.sbr"
	-@erase "$(INTDIR)\wxGC.obj"
	-@erase "$(INTDIR)\wxGC.sbr"
	-@erase "$(INTDIR)\xcglue.obj"
	-@erase "$(INTDIR)\xcglue.sbr"
	-@erase "$(OUTDIR)\mred.bsc"
	-@erase "$(OUTDIR)\mred.exe"
	-@erase "$(OUTDIR)\mred.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /ZI /Od /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /D "_DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\mred.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\Mred.res" /i "..\..\wxwindow\include\msw" /i "..\..\wxwindow\contrib\fafa" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mred.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\MRED.sbr" \
	"$(INTDIR)\MREDMSW.sbr" \
	"$(INTDIR)\WX_CGREC.sbr" \
	"$(INTDIR)\WX_KEYM.sbr" \
	"$(INTDIR)\WX_MBUF.sbr" \
	"$(INTDIR)\WX_MEDAD.sbr" \
	"$(INTDIR)\WX_MEDIA.sbr" \
	"$(INTDIR)\WX_MEDIO.sbr" \
	"$(INTDIR)\WX_MLINE.sbr" \
	"$(INTDIR)\WX_MPBRD.sbr" \
	"$(INTDIR)\WX_MPRIV.sbr" \
	"$(INTDIR)\WX_MSNIP.sbr" \
	"$(INTDIR)\WX_SNIP.sbr" \
	"$(INTDIR)\WX_STYLE.sbr" \
	"$(INTDIR)\wxGC.sbr" \
	"$(INTDIR)\xcglue.sbr"

"$(OUTDIR)\mred.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=libcmt.lib ..\wxs\Debug\wxs.lib ..\wxutils\Debug\wxutils.lib ..\wxwin\Debug\wxwin.lib ..\mzsrc\Debug\mzsrc.lib ..\gc\Debug\gc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\mred.pdb" /debug /machine:I386 /nodefaultlib:"libcmtd.lib" /out:"$(OUTDIR)\mred.exe" 
LINK32_OBJS= \
	"$(INTDIR)\MRED.obj" \
	"$(INTDIR)\MREDMSW.obj" \
	"$(INTDIR)\WX_CGREC.obj" \
	"$(INTDIR)\WX_KEYM.obj" \
	"$(INTDIR)\WX_MBUF.obj" \
	"$(INTDIR)\WX_MEDAD.obj" \
	"$(INTDIR)\WX_MEDIA.obj" \
	"$(INTDIR)\WX_MEDIO.obj" \
	"$(INTDIR)\WX_MLINE.obj" \
	"$(INTDIR)\WX_MPBRD.obj" \
	"$(INTDIR)\WX_MPRIV.obj" \
	"$(INTDIR)\WX_MSNIP.obj" \
	"$(INTDIR)\WX_SNIP.obj" \
	"$(INTDIR)\WX_STYLE.obj" \
	"$(INTDIR)\wxGC.obj" \
	"$(INTDIR)\xcglue.obj" \
	"$(INTDIR)\Mred.res" \
	"$(OUTDIR)\src\worksp\wxwin\Debug\wxwin.lib" \
	"$(OUTDIR)\src\worksp\mzsrc\Debug\mzsrc.lib" \
	"$(OUTDIR)\src\worksp\gc\Debug\gc.lib" \
	"$(OUTDIR)\src\worksp\wxs\Debug\wxs.lib" \
	"$(OUTDIR)\src\worksp\wxutils\Debug\wxutils.lib"

"$(OUTDIR)\mred.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

OUTDIR=.\..\..\..\..\plt
INTDIR=.\SGC
# Begin Custom Macros
OutDir=.\..\..\..\..\plt
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\mred.exe" "$(OUTDIR)\mred.bsc"

!ELSE 

ALL : "wxutils - Win32 SGC" "wxs - Win32 SGC" "mzsrc - Win32 SGC" "wxwin - Win32 SGC" "$(OUTDIR)\mred.exe" "$(OUTDIR)\mred.bsc"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"wxwin - Win32 SGCCLEAN" "mzsrc - Win32 SGCCLEAN" "wxs - Win32 SGCCLEAN" "wxutils - Win32 SGCCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\MRED.obj"
	-@erase "$(INTDIR)\Mred.res"
	-@erase "$(INTDIR)\MRED.sbr"
	-@erase "$(INTDIR)\MREDMSW.obj"
	-@erase "$(INTDIR)\MREDMSW.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\WX_CGREC.obj"
	-@erase "$(INTDIR)\WX_CGREC.sbr"
	-@erase "$(INTDIR)\WX_KEYM.obj"
	-@erase "$(INTDIR)\WX_KEYM.sbr"
	-@erase "$(INTDIR)\WX_MBUF.obj"
	-@erase "$(INTDIR)\WX_MBUF.sbr"
	-@erase "$(INTDIR)\WX_MEDAD.obj"
	-@erase "$(INTDIR)\WX_MEDAD.sbr"
	-@erase "$(INTDIR)\WX_MEDIA.obj"
	-@erase "$(INTDIR)\WX_MEDIA.sbr"
	-@erase "$(INTDIR)\WX_MEDIO.obj"
	-@erase "$(INTDIR)\WX_MEDIO.sbr"
	-@erase "$(INTDIR)\WX_MLINE.obj"
	-@erase "$(INTDIR)\WX_MLINE.sbr"
	-@erase "$(INTDIR)\WX_MPBRD.obj"
	-@erase "$(INTDIR)\WX_MPBRD.sbr"
	-@erase "$(INTDIR)\WX_MPRIV.obj"
	-@erase "$(INTDIR)\WX_MPRIV.sbr"
	-@erase "$(INTDIR)\WX_MSNIP.obj"
	-@erase "$(INTDIR)\WX_MSNIP.sbr"
	-@erase "$(INTDIR)\WX_SNIP.obj"
	-@erase "$(INTDIR)\WX_SNIP.sbr"
	-@erase "$(INTDIR)\WX_STYLE.obj"
	-@erase "$(INTDIR)\WX_STYLE.sbr"
	-@erase "$(INTDIR)\wxGC.obj"
	-@erase "$(INTDIR)\wxGC.sbr"
	-@erase "$(INTDIR)\xcglue.obj"
	-@erase "$(INTDIR)\xcglue.sbr"
	-@erase "$(OUTDIR)\mred.bsc"
	-@erase "$(OUTDIR)\mred.exe"
	-@erase "$(OUTDIR)\mred.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /ZI /Od /I "..\..\mred\mzscheme\sgc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mred\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mred\mzscheme\utils" /D "_DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT" /D "__WINDOWS__" /D "WXME_FOR_MRED" /D "USE_SENORA_GC" /D "USE_WXOBJECT_TRACE_COUNTER" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\mred.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\Mred.res" /i "..\..\wxwindow\include\msw" /i "..\..\wxwindow\contrib\fafa" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mred.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\MRED.sbr" \
	"$(INTDIR)\MREDMSW.sbr" \
	"$(INTDIR)\WX_CGREC.sbr" \
	"$(INTDIR)\WX_KEYM.sbr" \
	"$(INTDIR)\WX_MBUF.sbr" \
	"$(INTDIR)\WX_MEDAD.sbr" \
	"$(INTDIR)\WX_MEDIA.sbr" \
	"$(INTDIR)\WX_MEDIO.sbr" \
	"$(INTDIR)\WX_MLINE.sbr" \
	"$(INTDIR)\WX_MPBRD.sbr" \
	"$(INTDIR)\WX_MPRIV.sbr" \
	"$(INTDIR)\WX_MSNIP.sbr" \
	"$(INTDIR)\WX_SNIP.sbr" \
	"$(INTDIR)\WX_STYLE.sbr" \
	"$(INTDIR)\wxGC.sbr" \
	"$(INTDIR)\xcglue.sbr"

"$(OUTDIR)\mred.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=..\wxs\SGC\wxs.lib ..\wxutils\SGC\wxutils.lib ..\wxwin\SGC\wxwin.lib ..\mzsrc\SGC\mzsrc.lib ..\sgc\Debug\sgc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib libc.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\mred.pdb" /debug /machine:I386 /nodefaultlib:"libcd.lib" /out:"$(OUTDIR)\mred.exe" 
LINK32_OBJS= \
	"$(INTDIR)\MRED.obj" \
	"$(INTDIR)\MREDMSW.obj" \
	"$(INTDIR)\WX_CGREC.obj" \
	"$(INTDIR)\WX_KEYM.obj" \
	"$(INTDIR)\WX_MBUF.obj" \
	"$(INTDIR)\WX_MEDAD.obj" \
	"$(INTDIR)\WX_MEDIA.obj" \
	"$(INTDIR)\WX_MEDIO.obj" \
	"$(INTDIR)\WX_MLINE.obj" \
	"$(INTDIR)\WX_MPBRD.obj" \
	"$(INTDIR)\WX_MPRIV.obj" \
	"$(INTDIR)\WX_MSNIP.obj" \
	"$(INTDIR)\WX_SNIP.obj" \
	"$(INTDIR)\WX_STYLE.obj" \
	"$(INTDIR)\wxGC.obj" \
	"$(INTDIR)\xcglue.obj" \
	"$(INTDIR)\Mred.res" \
	"$(OUTDIR)\src\worksp\wxwin\SGC\wxwin.lib" \
	"$(OUTDIR)\src\worksp\mzsrc\SGC\mzsrc.lib" \
	"$(OUTDIR)\src\worksp\wxs\SGC\wxs.lib" \
	"$(OUTDIR)\src\worksp\wxutils\SGC\wxutils.lib"

"$(OUTDIR)\mred.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("mred.dep")
!INCLUDE "mred.dep"
!ELSE 
!MESSAGE Warning: cannot find "mred.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "mred - Win32 Release" || "$(CFG)" == "mred - Win32 Debug" || "$(CFG)" == "mred - Win32 SGC"
SOURCE=..\..\mred\MRED.cxx

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\MRED.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\MRED.obj"	"$(INTDIR)\MRED.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\MRED.obj"	"$(INTDIR)\MRED.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\Mred.rc

"$(INTDIR)\Mred.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=..\..\mred\MREDMSW.cxx

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\MREDMSW.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\MREDMSW.obj"	"$(INTDIR)\MREDMSW.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\MREDMSW.obj"	"$(INTDIR)\MREDMSW.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\mred\Wxme\WX_CGREC.cxx

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\WX_CGREC.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\WX_CGREC.obj"	"$(INTDIR)\WX_CGREC.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\WX_CGREC.obj"	"$(INTDIR)\WX_CGREC.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\mred\Wxme\WX_KEYM.cxx

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\WX_KEYM.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\WX_KEYM.obj"	"$(INTDIR)\WX_KEYM.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\WX_KEYM.obj"	"$(INTDIR)\WX_KEYM.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\mred\Wxme\WX_MBUF.cxx

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\WX_MBUF.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\WX_MBUF.obj"	"$(INTDIR)\WX_MBUF.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\WX_MBUF.obj"	"$(INTDIR)\WX_MBUF.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\mred\Wxme\WX_MEDAD.cxx

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\WX_MEDAD.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\WX_MEDAD.obj"	"$(INTDIR)\WX_MEDAD.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\WX_MEDAD.obj"	"$(INTDIR)\WX_MEDAD.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\mred\Wxme\WX_MEDIA.cxx

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\WX_MEDIA.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\WX_MEDIA.obj"	"$(INTDIR)\WX_MEDIA.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\WX_MEDIA.obj"	"$(INTDIR)\WX_MEDIA.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\mred\Wxme\WX_MEDIO.cxx

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\WX_MEDIO.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\WX_MEDIO.obj"	"$(INTDIR)\WX_MEDIO.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\WX_MEDIO.obj"	"$(INTDIR)\WX_MEDIO.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\mred\Wxme\WX_MLINE.cxx

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\WX_MLINE.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\WX_MLINE.obj"	"$(INTDIR)\WX_MLINE.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\WX_MLINE.obj"	"$(INTDIR)\WX_MLINE.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\mred\Wxme\WX_MPBRD.cxx

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\WX_MPBRD.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\WX_MPBRD.obj"	"$(INTDIR)\WX_MPBRD.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\WX_MPBRD.obj"	"$(INTDIR)\WX_MPBRD.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\mred\Wxme\WX_MPRIV.cxx

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\WX_MPRIV.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\WX_MPRIV.obj"	"$(INTDIR)\WX_MPRIV.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\WX_MPRIV.obj"	"$(INTDIR)\WX_MPRIV.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\mred\Wxme\WX_MSNIP.cxx

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\WX_MSNIP.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\WX_MSNIP.obj"	"$(INTDIR)\WX_MSNIP.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\WX_MSNIP.obj"	"$(INTDIR)\WX_MSNIP.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\mred\Wxme\WX_SNIP.cxx

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\WX_SNIP.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\WX_SNIP.obj"	"$(INTDIR)\WX_SNIP.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\WX_SNIP.obj"	"$(INTDIR)\WX_SNIP.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\mred\Wxme\WX_STYLE.cxx

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\WX_STYLE.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\WX_STYLE.obj"	"$(INTDIR)\WX_STYLE.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\WX_STYLE.obj"	"$(INTDIR)\WX_STYLE.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\wxcommon\wxGC.cxx

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\wxGC.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\wxGC.obj"	"$(INTDIR)\wxGC.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\wxGC.obj"	"$(INTDIR)\wxGC.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\mzscheme\utils\xcglue.c

!IF  "$(CFG)" == "mred - Win32 Release"


"$(INTDIR)\xcglue.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 Debug"


"$(INTDIR)\xcglue.obj"	"$(INTDIR)\xcglue.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mred - Win32 SGC"


"$(INTDIR)\xcglue.obj"	"$(INTDIR)\xcglue.sbr" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

!IF  "$(CFG)" == "mred - Win32 Release"

"wxwin - Win32 Release" : 
   cd "\Matthew\plt\src\worksp\wxwin"
   $(MAKE) /$(MAKEFLAGS) /F .\wxwin.mak CFG="wxwin - Win32 Release" 
   cd "..\mred"

"wxwin - Win32 ReleaseCLEAN" : 
   cd "\Matthew\plt\src\worksp\wxwin"
   $(MAKE) /$(MAKEFLAGS) /F .\wxwin.mak CFG="wxwin - Win32 Release" RECURSE=1 CLEAN 
   cd "..\mred"

!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

"wxwin - Win32 Debug" : 
   cd "\Matthew\plt\src\worksp\wxwin"
   $(MAKE) /$(MAKEFLAGS) /F .\wxwin.mak CFG="wxwin - Win32 Debug" 
   cd "..\mred"

"wxwin - Win32 DebugCLEAN" : 
   cd "\Matthew\plt\src\worksp\wxwin"
   $(MAKE) /$(MAKEFLAGS) /F .\wxwin.mak CFG="wxwin - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\mred"

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

"wxwin - Win32 SGC" : 
   cd "\Matthew\plt\src\worksp\wxwin"
   $(MAKE) /$(MAKEFLAGS) /F .\wxwin.mak CFG="wxwin - Win32 SGC" 
   cd "..\mred"

"wxwin - Win32 SGCCLEAN" : 
   cd "\Matthew\plt\src\worksp\wxwin"
   $(MAKE) /$(MAKEFLAGS) /F .\wxwin.mak CFG="wxwin - Win32 SGC" RECURSE=1 CLEAN 
   cd "..\mred"

!ENDIF 

!IF  "$(CFG)" == "mred - Win32 Release"

"mzsrc - Win32 Release" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 Release" 
   cd "..\mred"

"mzsrc - Win32 ReleaseCLEAN" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 Release" RECURSE=1 CLEAN 
   cd "..\mred"

!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

"mzsrc - Win32 Debug" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 Debug" 
   cd "..\mred"

"mzsrc - Win32 DebugCLEAN" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\mred"

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

"mzsrc - Win32 SGC" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 SGC" 
   cd "..\mred"

"mzsrc - Win32 SGCCLEAN" : 
   cd "\Matthew\plt\src\worksp\mzsrc"
   $(MAKE) /$(MAKEFLAGS) /F .\mzsrc.mak CFG="mzsrc - Win32 SGC" RECURSE=1 CLEAN 
   cd "..\mred"

!ENDIF 

!IF  "$(CFG)" == "mred - Win32 Release"

"gc - Win32 Release" : 
   cd "\Matthew\plt\src\worksp\gc"
   $(MAKE) /$(MAKEFLAGS) /F .\gc.mak CFG="gc - Win32 Release" 
   cd "..\mred"

"gc - Win32 ReleaseCLEAN" : 
   cd "\Matthew\plt\src\worksp\gc"
   $(MAKE) /$(MAKEFLAGS) /F .\gc.mak CFG="gc - Win32 Release" RECURSE=1 CLEAN 
   cd "..\mred"

!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

"gc - Win32 Debug" : 
   cd "\Matthew\plt\src\worksp\gc"
   $(MAKE) /$(MAKEFLAGS) /F .\gc.mak CFG="gc - Win32 Debug" 
   cd "..\mred"

"gc - Win32 DebugCLEAN" : 
   cd "\Matthew\plt\src\worksp\gc"
   $(MAKE) /$(MAKEFLAGS) /F .\gc.mak CFG="gc - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\mred"

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

!ENDIF 

!IF  "$(CFG)" == "mred - Win32 Release"

"wxs - Win32 Release" : 
   cd "\Matthew\plt\src\worksp\wxs"
   $(MAKE) /$(MAKEFLAGS) /F .\wxs.mak CFG="wxs - Win32 Release" 
   cd "..\mred"

"wxs - Win32 ReleaseCLEAN" : 
   cd "\Matthew\plt\src\worksp\wxs"
   $(MAKE) /$(MAKEFLAGS) /F .\wxs.mak CFG="wxs - Win32 Release" RECURSE=1 CLEAN 
   cd "..\mred"

!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

"wxs - Win32 Debug" : 
   cd "\Matthew\plt\src\worksp\wxs"
   $(MAKE) /$(MAKEFLAGS) /F .\wxs.mak CFG="wxs - Win32 Debug" 
   cd "..\mred"

"wxs - Win32 DebugCLEAN" : 
   cd "\Matthew\plt\src\worksp\wxs"
   $(MAKE) /$(MAKEFLAGS) /F .\wxs.mak CFG="wxs - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\mred"

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

"wxs - Win32 SGC" : 
   cd "\Matthew\plt\src\worksp\wxs"
   $(MAKE) /$(MAKEFLAGS) /F .\wxs.mak CFG="wxs - Win32 SGC" 
   cd "..\mred"

"wxs - Win32 SGCCLEAN" : 
   cd "\Matthew\plt\src\worksp\wxs"
   $(MAKE) /$(MAKEFLAGS) /F .\wxs.mak CFG="wxs - Win32 SGC" RECURSE=1 CLEAN 
   cd "..\mred"

!ENDIF 

!IF  "$(CFG)" == "mred - Win32 Release"

"wxutils - Win32 Release" : 
   cd "\Matthew\plt\src\worksp\wxutils"
   $(MAKE) /$(MAKEFLAGS) /F .\wxutils.mak CFG="wxutils - Win32 Release" 
   cd "..\mred"

"wxutils - Win32 ReleaseCLEAN" : 
   cd "\Matthew\plt\src\worksp\wxutils"
   $(MAKE) /$(MAKEFLAGS) /F .\wxutils.mak CFG="wxutils - Win32 Release" RECURSE=1 CLEAN 
   cd "..\mred"

!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

"wxutils - Win32 Debug" : 
   cd "\Matthew\plt\src\worksp\wxutils"
   $(MAKE) /$(MAKEFLAGS) /F .\wxutils.mak CFG="wxutils - Win32 Debug" 
   cd "..\mred"

"wxutils - Win32 DebugCLEAN" : 
   cd "\Matthew\plt\src\worksp\wxutils"
   $(MAKE) /$(MAKEFLAGS) /F .\wxutils.mak CFG="wxutils - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\mred"

!ELSEIF  "$(CFG)" == "mred - Win32 SGC"

"wxutils - Win32 SGC" : 
   cd "\Matthew\plt\src\worksp\wxutils"
   $(MAKE) /$(MAKEFLAGS) /F .\wxutils.mak CFG="wxutils - Win32 SGC" 
   cd "..\mred"

"wxutils - Win32 SGCCLEAN" : 
   cd "\Matthew\plt\src\worksp\wxutils"
   $(MAKE) /$(MAKEFLAGS) /F .\wxutils.mak CFG="wxutils - Win32 SGC" RECURSE=1 CLEAN 
   cd "..\mred"

!ENDIF 


!ENDIF 

