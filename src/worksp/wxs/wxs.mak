# Microsoft Developer Studio Generated NMAKE File, Based on wxs.dsp
!IF "$(CFG)" == ""
CFG=wxs - Win32 Release
!MESSAGE No configuration specified. Defaulting to wxs - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "wxs - Win32 Release" && "$(CFG)" != "wxs - Win32 Debug" && "$(CFG)" != "wxs - Win32 SGC"
!MESSAGE Invalid configuration "$(CFG)" specified.
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
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "wxs - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\wxs.lib"


CLEAN :
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\WXS_BMAP.obj"
	-@erase "$(INTDIR)\WXS_BUTN.obj"
	-@erase "$(INTDIR)\WXS_CHCE.obj"
	-@erase "$(INTDIR)\WXS_CKBX.obj"
	-@erase "$(INTDIR)\WXS_CNVS.obj"
	-@erase "$(INTDIR)\WXS_DC.obj"
	-@erase "$(INTDIR)\WXS_EVNT.obj"
	-@erase "$(INTDIR)\WXS_FRAM.obj"
	-@erase "$(INTDIR)\WXS_GAGE.obj"
	-@erase "$(INTDIR)\WXS_GDI.obj"
	-@erase "$(INTDIR)\WXS_GLOB.obj"
	-@erase "$(INTDIR)\WXS_ITEM.obj"
	-@erase "$(INTDIR)\WXS_LBOX.obj"
	-@erase "$(INTDIR)\WXS_MADM.obj"
	-@erase "$(INTDIR)\WXS_MEDE.obj"
	-@erase "$(INTDIR)\WXS_MEDI.obj"
	-@erase "$(INTDIR)\WXS_MENU.obj"
	-@erase "$(INTDIR)\WXS_MIO.obj"
	-@erase "$(INTDIR)\WXS_MISC.obj"
	-@erase "$(INTDIR)\WXS_MPB.obj"
	-@erase "$(INTDIR)\WXS_OBJ.obj"
	-@erase "$(INTDIR)\WXS_PANL.obj"
	-@erase "$(INTDIR)\WXS_RADO.obj"
	-@erase "$(INTDIR)\WXS_SLID.obj"
	-@erase "$(INTDIR)\WXS_SNIP.obj"
	-@erase "$(INTDIR)\WXS_STYL.obj"
	-@erase "$(INTDIR)\WXS_WIN.obj"
	-@erase "$(INTDIR)\WXSCHEME.obj"
	-@erase "$(OUTDIR)\wxs.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "NDEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /Fp"$(INTDIR)\wxs.pch" /YX"wx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wxs.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\wxs.lib" 
LIB32_OBJS= \
	"$(INTDIR)\WXS_BMAP.obj" \
	"$(INTDIR)\WXS_BUTN.obj" \
	"$(INTDIR)\WXS_CHCE.obj" \
	"$(INTDIR)\WXS_CKBX.obj" \
	"$(INTDIR)\WXS_CNVS.obj" \
	"$(INTDIR)\WXS_DC.obj" \
	"$(INTDIR)\WXS_EVNT.obj" \
	"$(INTDIR)\WXS_FRAM.obj" \
	"$(INTDIR)\WXS_GAGE.obj" \
	"$(INTDIR)\WXS_GDI.obj" \
	"$(INTDIR)\WXS_GLOB.obj" \
	"$(INTDIR)\WXS_ITEM.obj" \
	"$(INTDIR)\WXS_LBOX.obj" \
	"$(INTDIR)\WXS_MADM.obj" \
	"$(INTDIR)\WXS_MEDE.obj" \
	"$(INTDIR)\WXS_MEDI.obj" \
	"$(INTDIR)\WXS_MENU.obj" \
	"$(INTDIR)\WXS_MIO.obj" \
	"$(INTDIR)\WXS_MISC.obj" \
	"$(INTDIR)\WXS_MPB.obj" \
	"$(INTDIR)\WXS_OBJ.obj" \
	"$(INTDIR)\WXS_PANL.obj" \
	"$(INTDIR)\WXS_RADO.obj" \
	"$(INTDIR)\WXS_SLID.obj" \
	"$(INTDIR)\WXS_SNIP.obj" \
	"$(INTDIR)\WXS_STYL.obj" \
	"$(INTDIR)\WXS_WIN.obj" \
	"$(INTDIR)\WXSCHEME.obj"

"$(OUTDIR)\wxs.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\wxs.lib"


CLEAN :
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\WXS_BMAP.obj"
	-@erase "$(INTDIR)\WXS_BUTN.obj"
	-@erase "$(INTDIR)\WXS_CHCE.obj"
	-@erase "$(INTDIR)\WXS_CKBX.obj"
	-@erase "$(INTDIR)\WXS_CNVS.obj"
	-@erase "$(INTDIR)\WXS_DC.obj"
	-@erase "$(INTDIR)\WXS_EVNT.obj"
	-@erase "$(INTDIR)\WXS_FRAM.obj"
	-@erase "$(INTDIR)\WXS_GAGE.obj"
	-@erase "$(INTDIR)\WXS_GDI.obj"
	-@erase "$(INTDIR)\WXS_GLOB.obj"
	-@erase "$(INTDIR)\WXS_ITEM.obj"
	-@erase "$(INTDIR)\WXS_LBOX.obj"
	-@erase "$(INTDIR)\WXS_MADM.obj"
	-@erase "$(INTDIR)\WXS_MEDE.obj"
	-@erase "$(INTDIR)\WXS_MEDI.obj"
	-@erase "$(INTDIR)\WXS_MENU.obj"
	-@erase "$(INTDIR)\WXS_MIO.obj"
	-@erase "$(INTDIR)\WXS_MISC.obj"
	-@erase "$(INTDIR)\WXS_MPB.obj"
	-@erase "$(INTDIR)\WXS_OBJ.obj"
	-@erase "$(INTDIR)\WXS_PANL.obj"
	-@erase "$(INTDIR)\WXS_RADO.obj"
	-@erase "$(INTDIR)\WXS_SLID.obj"
	-@erase "$(INTDIR)\WXS_SNIP.obj"
	-@erase "$(INTDIR)\WXS_STYL.obj"
	-@erase "$(INTDIR)\WXS_WIN.obj"
	-@erase "$(INTDIR)\WXSCHEME.obj"
	-@erase "$(OUTDIR)\wxs.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /ZI /Od /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "__DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /Fp"$(INTDIR)\wxs.pch" /YX"wx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wxs.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\wxs.lib" 
LIB32_OBJS= \
	"$(INTDIR)\WXS_BMAP.obj" \
	"$(INTDIR)\WXS_BUTN.obj" \
	"$(INTDIR)\WXS_CHCE.obj" \
	"$(INTDIR)\WXS_CKBX.obj" \
	"$(INTDIR)\WXS_CNVS.obj" \
	"$(INTDIR)\WXS_DC.obj" \
	"$(INTDIR)\WXS_EVNT.obj" \
	"$(INTDIR)\WXS_FRAM.obj" \
	"$(INTDIR)\WXS_GAGE.obj" \
	"$(INTDIR)\WXS_GDI.obj" \
	"$(INTDIR)\WXS_GLOB.obj" \
	"$(INTDIR)\WXS_ITEM.obj" \
	"$(INTDIR)\WXS_LBOX.obj" \
	"$(INTDIR)\WXS_MADM.obj" \
	"$(INTDIR)\WXS_MEDE.obj" \
	"$(INTDIR)\WXS_MEDI.obj" \
	"$(INTDIR)\WXS_MENU.obj" \
	"$(INTDIR)\WXS_MIO.obj" \
	"$(INTDIR)\WXS_MISC.obj" \
	"$(INTDIR)\WXS_MPB.obj" \
	"$(INTDIR)\WXS_OBJ.obj" \
	"$(INTDIR)\WXS_PANL.obj" \
	"$(INTDIR)\WXS_RADO.obj" \
	"$(INTDIR)\WXS_SLID.obj" \
	"$(INTDIR)\WXS_SNIP.obj" \
	"$(INTDIR)\WXS_STYL.obj" \
	"$(INTDIR)\WXS_WIN.obj" \
	"$(INTDIR)\WXSCHEME.obj"

"$(OUTDIR)\wxs.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

OUTDIR=.\SGC
INTDIR=.\SGC
# Begin Custom Macros
OutDir=.\SGC
# End Custom Macros

ALL : "$(OUTDIR)\wxs.lib"


CLEAN :
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\WXS_BMAP.obj"
	-@erase "$(INTDIR)\WXS_BUTN.obj"
	-@erase "$(INTDIR)\WXS_CHCE.obj"
	-@erase "$(INTDIR)\WXS_CKBX.obj"
	-@erase "$(INTDIR)\WXS_CNVS.obj"
	-@erase "$(INTDIR)\WXS_DC.obj"
	-@erase "$(INTDIR)\WXS_EVNT.obj"
	-@erase "$(INTDIR)\WXS_FRAM.obj"
	-@erase "$(INTDIR)\WXS_GAGE.obj"
	-@erase "$(INTDIR)\WXS_GDI.obj"
	-@erase "$(INTDIR)\WXS_GLOB.obj"
	-@erase "$(INTDIR)\WXS_ITEM.obj"
	-@erase "$(INTDIR)\WXS_LBOX.obj"
	-@erase "$(INTDIR)\WXS_MADM.obj"
	-@erase "$(INTDIR)\WXS_MEDE.obj"
	-@erase "$(INTDIR)\WXS_MEDI.obj"
	-@erase "$(INTDIR)\WXS_MENU.obj"
	-@erase "$(INTDIR)\WXS_MIO.obj"
	-@erase "$(INTDIR)\WXS_MISC.obj"
	-@erase "$(INTDIR)\WXS_MPB.obj"
	-@erase "$(INTDIR)\WXS_OBJ.obj"
	-@erase "$(INTDIR)\WXS_PANL.obj"
	-@erase "$(INTDIR)\WXS_RADO.obj"
	-@erase "$(INTDIR)\WXS_SLID.obj"
	-@erase "$(INTDIR)\WXS_SNIP.obj"
	-@erase "$(INTDIR)\WXS_STYL.obj"
	-@erase "$(INTDIR)\WXS_WIN.obj"
	-@erase "$(INTDIR)\WXSCHEME.obj"
	-@erase "$(OUTDIR)\wxs.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /ZI /Od /I "..\..\mzscheme\sgc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "__DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "USE_SENORA_GC" /Fp"$(INTDIR)\wxs.pch" /YX"wx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wxs.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\wxs.lib" 
LIB32_OBJS= \
	"$(INTDIR)\WXS_BMAP.obj" \
	"$(INTDIR)\WXS_BUTN.obj" \
	"$(INTDIR)\WXS_CHCE.obj" \
	"$(INTDIR)\WXS_CKBX.obj" \
	"$(INTDIR)\WXS_CNVS.obj" \
	"$(INTDIR)\WXS_DC.obj" \
	"$(INTDIR)\WXS_EVNT.obj" \
	"$(INTDIR)\WXS_FRAM.obj" \
	"$(INTDIR)\WXS_GAGE.obj" \
	"$(INTDIR)\WXS_GDI.obj" \
	"$(INTDIR)\WXS_GLOB.obj" \
	"$(INTDIR)\WXS_ITEM.obj" \
	"$(INTDIR)\WXS_LBOX.obj" \
	"$(INTDIR)\WXS_MADM.obj" \
	"$(INTDIR)\WXS_MEDE.obj" \
	"$(INTDIR)\WXS_MEDI.obj" \
	"$(INTDIR)\WXS_MENU.obj" \
	"$(INTDIR)\WXS_MIO.obj" \
	"$(INTDIR)\WXS_MISC.obj" \
	"$(INTDIR)\WXS_MPB.obj" \
	"$(INTDIR)\WXS_OBJ.obj" \
	"$(INTDIR)\WXS_PANL.obj" \
	"$(INTDIR)\WXS_RADO.obj" \
	"$(INTDIR)\WXS_SLID.obj" \
	"$(INTDIR)\WXS_SNIP.obj" \
	"$(INTDIR)\WXS_STYL.obj" \
	"$(INTDIR)\WXS_WIN.obj" \
	"$(INTDIR)\WXSCHEME.obj"

"$(OUTDIR)\wxs.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("wxs.dep")
!INCLUDE "wxs.dep"
!ELSE 
!MESSAGE Warning: cannot find "wxs.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "wxs - Win32 Release" || "$(CFG)" == "wxs - Win32 Debug" || "$(CFG)" == "wxs - Win32 SGC"
SOURCE=..\..\mred\Wxs\WXS_BMAP.cxx

"$(INTDIR)\WXS_BMAP.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_BUTN.cxx

"$(INTDIR)\WXS_BUTN.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_CHCE.cxx

"$(INTDIR)\WXS_CHCE.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_CKBX.cxx

"$(INTDIR)\WXS_CKBX.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_CNVS.cxx

"$(INTDIR)\WXS_CNVS.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_DC.cxx

"$(INTDIR)\WXS_DC.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_EVNT.cxx

"$(INTDIR)\WXS_EVNT.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_FRAM.cxx

"$(INTDIR)\WXS_FRAM.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_GAGE.cxx

"$(INTDIR)\WXS_GAGE.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_GDI.cxx

"$(INTDIR)\WXS_GDI.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_GLOB.cxx

"$(INTDIR)\WXS_GLOB.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_ITEM.cxx

"$(INTDIR)\WXS_ITEM.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_LBOX.cxx

"$(INTDIR)\WXS_LBOX.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_MADM.cxx

"$(INTDIR)\WXS_MADM.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_MEDE.cxx

"$(INTDIR)\WXS_MEDE.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_MEDI.cxx

"$(INTDIR)\WXS_MEDI.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_MENU.cxx

"$(INTDIR)\WXS_MENU.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_MIO.cxx

"$(INTDIR)\WXS_MIO.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_MISC.cxx

"$(INTDIR)\WXS_MISC.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_MPB.cxx

"$(INTDIR)\WXS_MPB.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_OBJ.cxx

"$(INTDIR)\WXS_OBJ.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_PANL.cxx

"$(INTDIR)\WXS_PANL.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_RADO.cxx

"$(INTDIR)\WXS_RADO.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_SLID.cxx

"$(INTDIR)\WXS_SLID.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_SNIP.cxx

"$(INTDIR)\WXS_SNIP.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_STYL.cxx

"$(INTDIR)\WXS_STYL.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXS_WIN.cxx

"$(INTDIR)\WXS_WIN.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxs\WXSCHEME.cxx

"$(INTDIR)\WXSCHEME.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

