# Microsoft Developer Studio Generated NMAKE File, Based on wxutils.dsp
!IF "$(CFG)" == ""
CFG=wxutils - Win32 Release
!MESSAGE No configuration specified. Defaulting to wxutils - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "wxutils - Win32 Release" && "$(CFG)" != "wxutils - Win32 Debug" && "$(CFG)" != "wxutils - Win32 SGC"
!MESSAGE Invalid configuration "$(CFG)" specified.
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
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "wxutils - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\wxutils.lib"


CLEAN :
	-@erase "$(INTDIR)\Button.obj"
	-@erase "$(INTDIR)\Check.obj"
	-@erase "$(INTDIR)\Cont.obj"
	-@erase "$(INTDIR)\Crbuffri.obj"
	-@erase "$(INTDIR)\Crdatfri.obj"
	-@erase "$(INTDIR)\Create.obj"
	-@erase "$(INTDIR)\Crifrbuf.obj"
	-@erase "$(INTDIR)\Crifrdat.obj"
	-@erase "$(INTDIR)\Data.obj"
	-@erase "$(INTDIR)\Dialog.obj"
	-@erase "$(INTDIR)\DIB.obj"
	-@erase "$(INTDIR)\Draw.obj"
	-@erase "$(INTDIR)\Dumfafa.obj"
	-@erase "$(INTDIR)\Fafa.obj"
	-@erase "$(INTDIR)\Hashtab.obj"
	-@erase "$(INTDIR)\Misc.obj"
	-@erase "$(INTDIR)\Parse.obj"
	-@erase "$(INTDIR)\Rdftodat.obj"
	-@erase "$(INTDIR)\Rdftoi.obj"
	-@erase "$(INTDIR)\Rgb.obj"
	-@erase "$(INTDIR)\Scan.obj"
	-@erase "$(INTDIR)\Simx.obj"
	-@erase "$(INTDIR)\Static.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\Wrffrdat.obj"
	-@erase "$(INTDIR)\Wrffri.obj"
	-@erase "$(INTDIR)\Wrffrp.obj"
	-@erase "$(INTDIR)\wximgxbm.obj"
	-@erase "$(INTDIR)\Zyz3d.obj"
	-@erase "$(INTDIR)\Zyzgauge.obj"
	-@erase "$(OUTDIR)\wxutils.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wxutils.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\wxutils.lib" 
LIB32_OBJS= \
	"$(INTDIR)\Button.obj" \
	"$(INTDIR)\Check.obj" \
	"$(INTDIR)\Cont.obj" \
	"$(INTDIR)\Crbuffri.obj" \
	"$(INTDIR)\Crdatfri.obj" \
	"$(INTDIR)\Create.obj" \
	"$(INTDIR)\Crifrbuf.obj" \
	"$(INTDIR)\Crifrdat.obj" \
	"$(INTDIR)\Data.obj" \
	"$(INTDIR)\Dialog.obj" \
	"$(INTDIR)\DIB.obj" \
	"$(INTDIR)\Draw.obj" \
	"$(INTDIR)\Dumfafa.obj" \
	"$(INTDIR)\Fafa.obj" \
	"$(INTDIR)\Hashtab.obj" \
	"$(INTDIR)\Misc.obj" \
	"$(INTDIR)\Parse.obj" \
	"$(INTDIR)\Rdftodat.obj" \
	"$(INTDIR)\Rdftoi.obj" \
	"$(INTDIR)\Rgb.obj" \
	"$(INTDIR)\Scan.obj" \
	"$(INTDIR)\Simx.obj" \
	"$(INTDIR)\Static.obj" \
	"$(INTDIR)\Wrffrdat.obj" \
	"$(INTDIR)\Wrffri.obj" \
	"$(INTDIR)\Wrffrp.obj" \
	"$(INTDIR)\wximgxbm.obj" \
	"$(INTDIR)\Zyz3d.obj" \
	"$(INTDIR)\Zyzgauge.obj"

"$(OUTDIR)\wxutils.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wxutils - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\wxutils.lib"


CLEAN :
	-@erase "$(INTDIR)\Button.obj"
	-@erase "$(INTDIR)\Check.obj"
	-@erase "$(INTDIR)\Cont.obj"
	-@erase "$(INTDIR)\Crbuffri.obj"
	-@erase "$(INTDIR)\Crdatfri.obj"
	-@erase "$(INTDIR)\Create.obj"
	-@erase "$(INTDIR)\Crifrbuf.obj"
	-@erase "$(INTDIR)\Crifrdat.obj"
	-@erase "$(INTDIR)\Data.obj"
	-@erase "$(INTDIR)\Dialog.obj"
	-@erase "$(INTDIR)\DIB.obj"
	-@erase "$(INTDIR)\Draw.obj"
	-@erase "$(INTDIR)\Dumfafa.obj"
	-@erase "$(INTDIR)\Fafa.obj"
	-@erase "$(INTDIR)\Hashtab.obj"
	-@erase "$(INTDIR)\Misc.obj"
	-@erase "$(INTDIR)\Parse.obj"
	-@erase "$(INTDIR)\Rdftodat.obj"
	-@erase "$(INTDIR)\Rdftoi.obj"
	-@erase "$(INTDIR)\Rgb.obj"
	-@erase "$(INTDIR)\Scan.obj"
	-@erase "$(INTDIR)\Simx.obj"
	-@erase "$(INTDIR)\Static.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\Wrffrdat.obj"
	-@erase "$(INTDIR)\Wrffri.obj"
	-@erase "$(INTDIR)\Wrffrp.obj"
	-@erase "$(INTDIR)\wximgxbm.obj"
	-@erase "$(INTDIR)\Zyz3d.obj"
	-@erase "$(INTDIR)\Zyzgauge.obj"
	-@erase "$(OUTDIR)\wxutils.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Z7 /Od /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wxutils.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\wxutils.lib" 
LIB32_OBJS= \
	"$(INTDIR)\Button.obj" \
	"$(INTDIR)\Check.obj" \
	"$(INTDIR)\Cont.obj" \
	"$(INTDIR)\Crbuffri.obj" \
	"$(INTDIR)\Crdatfri.obj" \
	"$(INTDIR)\Create.obj" \
	"$(INTDIR)\Crifrbuf.obj" \
	"$(INTDIR)\Crifrdat.obj" \
	"$(INTDIR)\Data.obj" \
	"$(INTDIR)\Dialog.obj" \
	"$(INTDIR)\DIB.obj" \
	"$(INTDIR)\Draw.obj" \
	"$(INTDIR)\Dumfafa.obj" \
	"$(INTDIR)\Fafa.obj" \
	"$(INTDIR)\Hashtab.obj" \
	"$(INTDIR)\Misc.obj" \
	"$(INTDIR)\Parse.obj" \
	"$(INTDIR)\Rdftodat.obj" \
	"$(INTDIR)\Rdftoi.obj" \
	"$(INTDIR)\Rgb.obj" \
	"$(INTDIR)\Scan.obj" \
	"$(INTDIR)\Simx.obj" \
	"$(INTDIR)\Static.obj" \
	"$(INTDIR)\Wrffrdat.obj" \
	"$(INTDIR)\Wrffri.obj" \
	"$(INTDIR)\Wrffrp.obj" \
	"$(INTDIR)\wximgxbm.obj" \
	"$(INTDIR)\Zyz3d.obj" \
	"$(INTDIR)\Zyzgauge.obj"

"$(OUTDIR)\wxutils.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wxutils - Win32 SGC"

OUTDIR=.\SGC
INTDIR=.\SGC
# Begin Custom Macros
OutDir=.\SGC
# End Custom Macros

ALL : "$(OUTDIR)\wxutils.lib"


CLEAN :
	-@erase "$(INTDIR)\Button.obj"
	-@erase "$(INTDIR)\Check.obj"
	-@erase "$(INTDIR)\Cont.obj"
	-@erase "$(INTDIR)\Crbuffri.obj"
	-@erase "$(INTDIR)\Crdatfri.obj"
	-@erase "$(INTDIR)\Create.obj"
	-@erase "$(INTDIR)\Crifrbuf.obj"
	-@erase "$(INTDIR)\Crifrdat.obj"
	-@erase "$(INTDIR)\Data.obj"
	-@erase "$(INTDIR)\Dialog.obj"
	-@erase "$(INTDIR)\DIB.obj"
	-@erase "$(INTDIR)\Draw.obj"
	-@erase "$(INTDIR)\Dumfafa.obj"
	-@erase "$(INTDIR)\Fafa.obj"
	-@erase "$(INTDIR)\Hashtab.obj"
	-@erase "$(INTDIR)\Misc.obj"
	-@erase "$(INTDIR)\Parse.obj"
	-@erase "$(INTDIR)\Rdftodat.obj"
	-@erase "$(INTDIR)\Rdftoi.obj"
	-@erase "$(INTDIR)\Rgb.obj"
	-@erase "$(INTDIR)\Scan.obj"
	-@erase "$(INTDIR)\Simx.obj"
	-@erase "$(INTDIR)\Static.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\Wrffrdat.obj"
	-@erase "$(INTDIR)\Wrffri.obj"
	-@erase "$(INTDIR)\Wrffrp.obj"
	-@erase "$(INTDIR)\wximgxbm.obj"
	-@erase "$(INTDIR)\Zyz3d.obj"
	-@erase "$(INTDIR)\Zyzgauge.obj"
	-@erase "$(OUTDIR)\wxutils.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /ZI /Od /I "..\..\mzscheme\sgc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "USE_SENORA_GC" /D "USE_WXOBJECT_TRACE_COUNT" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wxutils.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\wxutils.lib" 
LIB32_OBJS= \
	"$(INTDIR)\Button.obj" \
	"$(INTDIR)\Check.obj" \
	"$(INTDIR)\Cont.obj" \
	"$(INTDIR)\Crbuffri.obj" \
	"$(INTDIR)\Crdatfri.obj" \
	"$(INTDIR)\Create.obj" \
	"$(INTDIR)\Crifrbuf.obj" \
	"$(INTDIR)\Crifrdat.obj" \
	"$(INTDIR)\Data.obj" \
	"$(INTDIR)\Dialog.obj" \
	"$(INTDIR)\DIB.obj" \
	"$(INTDIR)\Draw.obj" \
	"$(INTDIR)\Dumfafa.obj" \
	"$(INTDIR)\Fafa.obj" \
	"$(INTDIR)\Hashtab.obj" \
	"$(INTDIR)\Misc.obj" \
	"$(INTDIR)\Parse.obj" \
	"$(INTDIR)\Rdftodat.obj" \
	"$(INTDIR)\Rdftoi.obj" \
	"$(INTDIR)\Rgb.obj" \
	"$(INTDIR)\Scan.obj" \
	"$(INTDIR)\Simx.obj" \
	"$(INTDIR)\Static.obj" \
	"$(INTDIR)\Wrffrdat.obj" \
	"$(INTDIR)\Wrffri.obj" \
	"$(INTDIR)\Wrffrp.obj" \
	"$(INTDIR)\wximgxbm.obj" \
	"$(INTDIR)\Zyz3d.obj" \
	"$(INTDIR)\Zyzgauge.obj"

"$(OUTDIR)\wxutils.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("wxutils.dep")
!INCLUDE "wxutils.dep"
!ELSE 
!MESSAGE Warning: cannot find "wxutils.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "wxutils - Win32 Release" || "$(CFG)" == "wxutils - Win32 Debug" || "$(CFG)" == "wxutils - Win32 SGC"
SOURCE=..\..\Wxwindow\Contrib\Fafa\Button.c

"$(INTDIR)\Button.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Fafa\Check.c

"$(INTDIR)\Check.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Fafa\Cont.c

"$(INTDIR)\Cont.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crbuffri.c

"$(INTDIR)\Crbuffri.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crdatfri.c

"$(INTDIR)\Crdatfri.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Create.c

"$(INTDIR)\Create.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crifrbuf.c

"$(INTDIR)\Crifrbuf.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crifrdat.c

"$(INTDIR)\Crifrdat.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Data.c

"$(INTDIR)\Data.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Fafa\Dialog.c

"$(INTDIR)\Dialog.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Utils\Dib\DIB.cxx

"$(INTDIR)\DIB.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Fafa\Draw.c

"$(INTDIR)\Draw.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Fafa\Dumfafa.c

"$(INTDIR)\Dumfafa.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Fafa\Fafa.c

"$(INTDIR)\Fafa.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Hashtab.c

"$(INTDIR)\Hashtab.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Misc.c

"$(INTDIR)\Misc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Parse.c

"$(INTDIR)\Parse.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Rdftodat.c

"$(INTDIR)\Rdftodat.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Rdftoi.c

"$(INTDIR)\Rdftoi.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Rgb.c

"$(INTDIR)\Rgb.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Scan.c

"$(INTDIR)\Scan.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Simx.c

"$(INTDIR)\Simx.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Fafa\Static.c

"$(INTDIR)\Static.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Wrffrdat.c

"$(INTDIR)\Wrffrdat.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Wrffri.c

"$(INTDIR)\Wrffri.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Wrffrp.c

"$(INTDIR)\Wrffrp.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\wximgxbm.cxx

"$(INTDIR)\wximgxbm.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Gauge\Zyz3d.c

"$(INTDIR)\Zyz3d.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Contrib\Gauge\Zyzgauge.c

"$(INTDIR)\Zyzgauge.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

