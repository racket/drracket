# Microsoft Developer Studio Generated NMAKE File, Based on wxwin.dsp
!IF "$(CFG)" == ""
CFG=wxwin - Win32 Release
!MESSAGE No configuration specified. Defaulting to wxwin - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "wxwin - Win32 Release" && "$(CFG)" != "wxwin - Win32 Debug" && "$(CFG)" != "wxwin - Win32 SGC"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wxwin.mak" CFG="wxwin - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wxwin - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "wxwin - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "wxwin - Win32 SGC" (based on "Win32 (x86) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "wxwin - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\wxwin.lib"


CLEAN :
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\WB_CANVS.obj"
	-@erase "$(INTDIR)\WB_CMDLG.obj"
	-@erase "$(INTDIR)\WB_DATA.obj"
	-@erase "$(INTDIR)\WB_DC.obj"
	-@erase "$(INTDIR)\WB_DIALG.obj"
	-@erase "$(INTDIR)\WB_FRAME.obj"
	-@erase "$(INTDIR)\WB_GDI.obj"
	-@erase "$(INTDIR)\WB_HASH.obj"
	-@erase "$(INTDIR)\WB_ITEM.obj"
	-@erase "$(INTDIR)\WB_LIST.obj"
	-@erase "$(INTDIR)\WB_MAIN.obj"
	-@erase "$(INTDIR)\WB_MF.obj"
	-@erase "$(INTDIR)\WB_MGSTR.obj"
	-@erase "$(INTDIR)\WB_OBJ.obj"
	-@erase "$(INTDIR)\WB_PANEL.obj"
	-@erase "$(INTDIR)\WB_PRINT.obj"
	-@erase "$(INTDIR)\WB_PS.obj"
	-@erase "$(INTDIR)\WB_RES.obj"
	-@erase "$(INTDIR)\WB_STDEV.obj"
	-@erase "$(INTDIR)\WB_SYSEV.obj"
	-@erase "$(INTDIR)\WB_TIMER.obj"
	-@erase "$(INTDIR)\WB_TYPES.obj"
	-@erase "$(INTDIR)\WB_UTILS.obj"
	-@erase "$(INTDIR)\WB_WIN.obj"
	-@erase "$(INTDIR)\WX_BUTTN.obj"
	-@erase "$(INTDIR)\WX_CANVS.obj"
	-@erase "$(INTDIR)\WX_CHECK.obj"
	-@erase "$(INTDIR)\WX_CHOIC.obj"
	-@erase "$(INTDIR)\WX_CLIPB.obj"
	-@erase "$(INTDIR)\WX_CMDLG.obj"
	-@erase "$(INTDIR)\WX_DC.obj"
	-@erase "$(INTDIR)\WX_DIALG.obj"
	-@erase "$(INTDIR)\WX_FRAME.obj"
	-@erase "$(INTDIR)\WX_GAUGE.obj"
	-@erase "$(INTDIR)\WX_GDI.obj"
	-@erase "$(INTDIR)\WX_ITEM.obj"
	-@erase "$(INTDIR)\WX_LBOX.obj"
	-@erase "$(INTDIR)\WX_MAIN.obj"
	-@erase "$(INTDIR)\WX_MENU.obj"
	-@erase "$(INTDIR)\WX_MESSG.obj"
	-@erase "$(INTDIR)\WX_MF.obj"
	-@erase "$(INTDIR)\WX_PANEL.obj"
	-@erase "$(INTDIR)\wx_pdf.obj"
	-@erase "$(INTDIR)\WX_RBOX.obj"
	-@erase "$(INTDIR)\WX_SLIDR.obj"
	-@erase "$(INTDIR)\WX_TIMER.obj"
	-@erase "$(INTDIR)\WX_UTILS.obj"
	-@erase "$(INTDIR)\WX_WIN.obj"
	-@erase "$(INTDIR)\wximgfil.obj"
	-@erase "$(OUTDIR)\wxwin.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /Fp"$(INTDIR)\wxwin.pch" /YX"wx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wxwin.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\wxwin.lib" 
LIB32_OBJS= \
	"$(INTDIR)\WB_CANVS.obj" \
	"$(INTDIR)\WB_CMDLG.obj" \
	"$(INTDIR)\WB_DATA.obj" \
	"$(INTDIR)\WB_DC.obj" \
	"$(INTDIR)\WB_DIALG.obj" \
	"$(INTDIR)\WB_FRAME.obj" \
	"$(INTDIR)\WB_GDI.obj" \
	"$(INTDIR)\WB_HASH.obj" \
	"$(INTDIR)\WB_ITEM.obj" \
	"$(INTDIR)\WB_LIST.obj" \
	"$(INTDIR)\WB_MAIN.obj" \
	"$(INTDIR)\WB_MF.obj" \
	"$(INTDIR)\WB_MGSTR.obj" \
	"$(INTDIR)\WB_OBJ.obj" \
	"$(INTDIR)\WB_PANEL.obj" \
	"$(INTDIR)\WB_PRINT.obj" \
	"$(INTDIR)\WB_PS.obj" \
	"$(INTDIR)\WB_RES.obj" \
	"$(INTDIR)\WB_STDEV.obj" \
	"$(INTDIR)\WB_SYSEV.obj" \
	"$(INTDIR)\WB_TIMER.obj" \
	"$(INTDIR)\WB_TYPES.obj" \
	"$(INTDIR)\WB_UTILS.obj" \
	"$(INTDIR)\WB_WIN.obj" \
	"$(INTDIR)\WX_BUTTN.obj" \
	"$(INTDIR)\WX_CANVS.obj" \
	"$(INTDIR)\WX_CHECK.obj" \
	"$(INTDIR)\WX_CHOIC.obj" \
	"$(INTDIR)\WX_CLIPB.obj" \
	"$(INTDIR)\WX_CMDLG.obj" \
	"$(INTDIR)\WX_DC.obj" \
	"$(INTDIR)\WX_DIALG.obj" \
	"$(INTDIR)\WX_FRAME.obj" \
	"$(INTDIR)\WX_GAUGE.obj" \
	"$(INTDIR)\WX_GDI.obj" \
	"$(INTDIR)\WX_ITEM.obj" \
	"$(INTDIR)\WX_LBOX.obj" \
	"$(INTDIR)\WX_MAIN.obj" \
	"$(INTDIR)\WX_MENU.obj" \
	"$(INTDIR)\WX_MESSG.obj" \
	"$(INTDIR)\WX_MF.obj" \
	"$(INTDIR)\WX_PANEL.obj" \
	"$(INTDIR)\wx_pdf.obj" \
	"$(INTDIR)\WX_RBOX.obj" \
	"$(INTDIR)\WX_SLIDR.obj" \
	"$(INTDIR)\WX_TIMER.obj" \
	"$(INTDIR)\WX_UTILS.obj" \
	"$(INTDIR)\WX_WIN.obj" \
	"$(INTDIR)\wximgfil.obj"

"$(OUTDIR)\wxwin.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\wxwin.lib"


CLEAN :
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\WB_CANVS.obj"
	-@erase "$(INTDIR)\WB_CMDLG.obj"
	-@erase "$(INTDIR)\WB_DATA.obj"
	-@erase "$(INTDIR)\WB_DC.obj"
	-@erase "$(INTDIR)\WB_DIALG.obj"
	-@erase "$(INTDIR)\WB_FRAME.obj"
	-@erase "$(INTDIR)\WB_GDI.obj"
	-@erase "$(INTDIR)\WB_HASH.obj"
	-@erase "$(INTDIR)\WB_ITEM.obj"
	-@erase "$(INTDIR)\WB_LIST.obj"
	-@erase "$(INTDIR)\WB_MAIN.obj"
	-@erase "$(INTDIR)\WB_MF.obj"
	-@erase "$(INTDIR)\WB_MGSTR.obj"
	-@erase "$(INTDIR)\WB_OBJ.obj"
	-@erase "$(INTDIR)\WB_PANEL.obj"
	-@erase "$(INTDIR)\WB_PRINT.obj"
	-@erase "$(INTDIR)\WB_PS.obj"
	-@erase "$(INTDIR)\WB_RES.obj"
	-@erase "$(INTDIR)\WB_STDEV.obj"
	-@erase "$(INTDIR)\WB_SYSEV.obj"
	-@erase "$(INTDIR)\WB_TIMER.obj"
	-@erase "$(INTDIR)\WB_TYPES.obj"
	-@erase "$(INTDIR)\WB_UTILS.obj"
	-@erase "$(INTDIR)\WB_WIN.obj"
	-@erase "$(INTDIR)\WX_BUTTN.obj"
	-@erase "$(INTDIR)\WX_CANVS.obj"
	-@erase "$(INTDIR)\WX_CHECK.obj"
	-@erase "$(INTDIR)\WX_CHOIC.obj"
	-@erase "$(INTDIR)\WX_CLIPB.obj"
	-@erase "$(INTDIR)\WX_CMDLG.obj"
	-@erase "$(INTDIR)\WX_DC.obj"
	-@erase "$(INTDIR)\WX_DIALG.obj"
	-@erase "$(INTDIR)\WX_FRAME.obj"
	-@erase "$(INTDIR)\WX_GAUGE.obj"
	-@erase "$(INTDIR)\WX_GDI.obj"
	-@erase "$(INTDIR)\WX_ITEM.obj"
	-@erase "$(INTDIR)\WX_LBOX.obj"
	-@erase "$(INTDIR)\WX_MAIN.obj"
	-@erase "$(INTDIR)\WX_MENU.obj"
	-@erase "$(INTDIR)\WX_MESSG.obj"
	-@erase "$(INTDIR)\WX_MF.obj"
	-@erase "$(INTDIR)\WX_PANEL.obj"
	-@erase "$(INTDIR)\wx_pdf.obj"
	-@erase "$(INTDIR)\WX_RBOX.obj"
	-@erase "$(INTDIR)\WX_SLIDR.obj"
	-@erase "$(INTDIR)\WX_TIMER.obj"
	-@erase "$(INTDIR)\WX_UTILS.obj"
	-@erase "$(INTDIR)\WX_WIN.obj"
	-@erase "$(INTDIR)\wximgfil.obj"
	-@erase "$(OUTDIR)\wxwin.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Z7 /Od /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /Fp"$(INTDIR)\wxwin.pch" /YX"wx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wxwin.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\wxwin.lib" 
LIB32_OBJS= \
	"$(INTDIR)\WB_CANVS.obj" \
	"$(INTDIR)\WB_CMDLG.obj" \
	"$(INTDIR)\WB_DATA.obj" \
	"$(INTDIR)\WB_DC.obj" \
	"$(INTDIR)\WB_DIALG.obj" \
	"$(INTDIR)\WB_FRAME.obj" \
	"$(INTDIR)\WB_GDI.obj" \
	"$(INTDIR)\WB_HASH.obj" \
	"$(INTDIR)\WB_ITEM.obj" \
	"$(INTDIR)\WB_LIST.obj" \
	"$(INTDIR)\WB_MAIN.obj" \
	"$(INTDIR)\WB_MF.obj" \
	"$(INTDIR)\WB_MGSTR.obj" \
	"$(INTDIR)\WB_OBJ.obj" \
	"$(INTDIR)\WB_PANEL.obj" \
	"$(INTDIR)\WB_PRINT.obj" \
	"$(INTDIR)\WB_PS.obj" \
	"$(INTDIR)\WB_RES.obj" \
	"$(INTDIR)\WB_STDEV.obj" \
	"$(INTDIR)\WB_SYSEV.obj" \
	"$(INTDIR)\WB_TIMER.obj" \
	"$(INTDIR)\WB_TYPES.obj" \
	"$(INTDIR)\WB_UTILS.obj" \
	"$(INTDIR)\WB_WIN.obj" \
	"$(INTDIR)\WX_BUTTN.obj" \
	"$(INTDIR)\WX_CANVS.obj" \
	"$(INTDIR)\WX_CHECK.obj" \
	"$(INTDIR)\WX_CHOIC.obj" \
	"$(INTDIR)\WX_CLIPB.obj" \
	"$(INTDIR)\WX_CMDLG.obj" \
	"$(INTDIR)\WX_DC.obj" \
	"$(INTDIR)\WX_DIALG.obj" \
	"$(INTDIR)\WX_FRAME.obj" \
	"$(INTDIR)\WX_GAUGE.obj" \
	"$(INTDIR)\WX_GDI.obj" \
	"$(INTDIR)\WX_ITEM.obj" \
	"$(INTDIR)\WX_LBOX.obj" \
	"$(INTDIR)\WX_MAIN.obj" \
	"$(INTDIR)\WX_MENU.obj" \
	"$(INTDIR)\WX_MESSG.obj" \
	"$(INTDIR)\WX_MF.obj" \
	"$(INTDIR)\WX_PANEL.obj" \
	"$(INTDIR)\wx_pdf.obj" \
	"$(INTDIR)\WX_RBOX.obj" \
	"$(INTDIR)\WX_SLIDR.obj" \
	"$(INTDIR)\WX_TIMER.obj" \
	"$(INTDIR)\WX_UTILS.obj" \
	"$(INTDIR)\WX_WIN.obj" \
	"$(INTDIR)\wximgfil.obj"

"$(OUTDIR)\wxwin.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

OUTDIR=.\SGC
INTDIR=.\SGC
# Begin Custom Macros
OutDir=.\SGC
# End Custom Macros

ALL : "$(OUTDIR)\wxwin.lib"


CLEAN :
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\WB_CANVS.obj"
	-@erase "$(INTDIR)\WB_CMDLG.obj"
	-@erase "$(INTDIR)\WB_DATA.obj"
	-@erase "$(INTDIR)\WB_DC.obj"
	-@erase "$(INTDIR)\WB_DIALG.obj"
	-@erase "$(INTDIR)\WB_FRAME.obj"
	-@erase "$(INTDIR)\WB_GDI.obj"
	-@erase "$(INTDIR)\WB_HASH.obj"
	-@erase "$(INTDIR)\WB_ITEM.obj"
	-@erase "$(INTDIR)\WB_LIST.obj"
	-@erase "$(INTDIR)\WB_MAIN.obj"
	-@erase "$(INTDIR)\WB_MF.obj"
	-@erase "$(INTDIR)\WB_MGSTR.obj"
	-@erase "$(INTDIR)\WB_OBJ.obj"
	-@erase "$(INTDIR)\WB_PANEL.obj"
	-@erase "$(INTDIR)\WB_PRINT.obj"
	-@erase "$(INTDIR)\WB_PS.obj"
	-@erase "$(INTDIR)\WB_RES.obj"
	-@erase "$(INTDIR)\WB_STDEV.obj"
	-@erase "$(INTDIR)\WB_SYSEV.obj"
	-@erase "$(INTDIR)\WB_TIMER.obj"
	-@erase "$(INTDIR)\WB_TYPES.obj"
	-@erase "$(INTDIR)\WB_UTILS.obj"
	-@erase "$(INTDIR)\WB_WIN.obj"
	-@erase "$(INTDIR)\WX_BUTTN.obj"
	-@erase "$(INTDIR)\WX_CANVS.obj"
	-@erase "$(INTDIR)\WX_CHECK.obj"
	-@erase "$(INTDIR)\WX_CHOIC.obj"
	-@erase "$(INTDIR)\WX_CLIPB.obj"
	-@erase "$(INTDIR)\WX_CMDLG.obj"
	-@erase "$(INTDIR)\WX_DC.obj"
	-@erase "$(INTDIR)\WX_DIALG.obj"
	-@erase "$(INTDIR)\WX_FRAME.obj"
	-@erase "$(INTDIR)\WX_GAUGE.obj"
	-@erase "$(INTDIR)\WX_GDI.obj"
	-@erase "$(INTDIR)\WX_ITEM.obj"
	-@erase "$(INTDIR)\WX_LBOX.obj"
	-@erase "$(INTDIR)\WX_MAIN.obj"
	-@erase "$(INTDIR)\WX_MENU.obj"
	-@erase "$(INTDIR)\WX_MESSG.obj"
	-@erase "$(INTDIR)\WX_MF.obj"
	-@erase "$(INTDIR)\WX_PANEL.obj"
	-@erase "$(INTDIR)\wx_pdf.obj"
	-@erase "$(INTDIR)\WX_RBOX.obj"
	-@erase "$(INTDIR)\WX_SLIDR.obj"
	-@erase "$(INTDIR)\WX_TIMER.obj"
	-@erase "$(INTDIR)\WX_UTILS.obj"
	-@erase "$(INTDIR)\WX_WIN.obj"
	-@erase "$(INTDIR)\wximgfil.obj"
	-@erase "$(OUTDIR)\wxwin.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /ZI /Od /I "..\..\mzscheme\sgc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "USE_SENORA_GC" /D "USE_WXOBJECT_TRACE_COUNT" /Fp"$(INTDIR)\wxwin.pch" /YX"wx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wxwin.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\wxwin.lib" 
LIB32_OBJS= \
	"$(INTDIR)\WB_CANVS.obj" \
	"$(INTDIR)\WB_CMDLG.obj" \
	"$(INTDIR)\WB_DATA.obj" \
	"$(INTDIR)\WB_DC.obj" \
	"$(INTDIR)\WB_DIALG.obj" \
	"$(INTDIR)\WB_FRAME.obj" \
	"$(INTDIR)\WB_GDI.obj" \
	"$(INTDIR)\WB_HASH.obj" \
	"$(INTDIR)\WB_ITEM.obj" \
	"$(INTDIR)\WB_LIST.obj" \
	"$(INTDIR)\WB_MAIN.obj" \
	"$(INTDIR)\WB_MF.obj" \
	"$(INTDIR)\WB_MGSTR.obj" \
	"$(INTDIR)\WB_OBJ.obj" \
	"$(INTDIR)\WB_PANEL.obj" \
	"$(INTDIR)\WB_PRINT.obj" \
	"$(INTDIR)\WB_PS.obj" \
	"$(INTDIR)\WB_RES.obj" \
	"$(INTDIR)\WB_STDEV.obj" \
	"$(INTDIR)\WB_SYSEV.obj" \
	"$(INTDIR)\WB_TIMER.obj" \
	"$(INTDIR)\WB_TYPES.obj" \
	"$(INTDIR)\WB_UTILS.obj" \
	"$(INTDIR)\WB_WIN.obj" \
	"$(INTDIR)\WX_BUTTN.obj" \
	"$(INTDIR)\WX_CANVS.obj" \
	"$(INTDIR)\WX_CHECK.obj" \
	"$(INTDIR)\WX_CHOIC.obj" \
	"$(INTDIR)\WX_CLIPB.obj" \
	"$(INTDIR)\WX_CMDLG.obj" \
	"$(INTDIR)\WX_DC.obj" \
	"$(INTDIR)\WX_DIALG.obj" \
	"$(INTDIR)\WX_FRAME.obj" \
	"$(INTDIR)\WX_GAUGE.obj" \
	"$(INTDIR)\WX_GDI.obj" \
	"$(INTDIR)\WX_ITEM.obj" \
	"$(INTDIR)\WX_LBOX.obj" \
	"$(INTDIR)\WX_MAIN.obj" \
	"$(INTDIR)\WX_MENU.obj" \
	"$(INTDIR)\WX_MESSG.obj" \
	"$(INTDIR)\WX_MF.obj" \
	"$(INTDIR)\WX_PANEL.obj" \
	"$(INTDIR)\wx_pdf.obj" \
	"$(INTDIR)\WX_RBOX.obj" \
	"$(INTDIR)\WX_SLIDR.obj" \
	"$(INTDIR)\WX_TIMER.obj" \
	"$(INTDIR)\WX_UTILS.obj" \
	"$(INTDIR)\WX_WIN.obj" \
	"$(INTDIR)\wximgfil.obj"

"$(OUTDIR)\wxwin.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("wxwin.dep")
!INCLUDE "wxwin.dep"
!ELSE 
!MESSAGE Warning: cannot find "wxwin.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "wxwin - Win32 Release" || "$(CFG)" == "wxwin - Win32 Debug" || "$(CFG)" == "wxwin - Win32 SGC"
SOURCE=..\..\Wxwindow\Src\Base\WB_CANVS.cxx

"$(INTDIR)\WB_CANVS.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_CMDLG.cxx

"$(INTDIR)\WB_CMDLG.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_DATA.cxx

"$(INTDIR)\WB_DATA.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_DC.cxx

"$(INTDIR)\WB_DC.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_DIALG.cxx

"$(INTDIR)\WB_DIALG.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_FRAME.cxx

"$(INTDIR)\WB_FRAME.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_GDI.cxx

"$(INTDIR)\WB_GDI.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_HASH.cxx

"$(INTDIR)\WB_HASH.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_ITEM.cxx

"$(INTDIR)\WB_ITEM.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_LIST.cxx

"$(INTDIR)\WB_LIST.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_MAIN.cxx

"$(INTDIR)\WB_MAIN.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_MF.cxx

"$(INTDIR)\WB_MF.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_MGSTR.cxx

"$(INTDIR)\WB_MGSTR.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_OBJ.cxx

"$(INTDIR)\WB_OBJ.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_PANEL.cxx

"$(INTDIR)\WB_PANEL.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_PRINT.cxx

"$(INTDIR)\WB_PRINT.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_PS.cxx

"$(INTDIR)\WB_PS.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_RES.cxx

"$(INTDIR)\WB_RES.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_STDEV.cxx

"$(INTDIR)\WB_STDEV.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_SYSEV.cxx

"$(INTDIR)\WB_SYSEV.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_TIMER.cxx

"$(INTDIR)\WB_TIMER.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_TYPES.cxx

"$(INTDIR)\WB_TYPES.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_UTILS.cxx

"$(INTDIR)\WB_UTILS.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Base\WB_WIN.cxx

"$(INTDIR)\WB_WIN.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_BUTTN.cxx

"$(INTDIR)\WX_BUTTN.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_CANVS.cxx

"$(INTDIR)\WX_CANVS.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_CHECK.cxx

"$(INTDIR)\WX_CHECK.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_CHOIC.cxx

"$(INTDIR)\WX_CHOIC.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_CLIPB.cxx

"$(INTDIR)\WX_CLIPB.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_CMDLG.cxx

"$(INTDIR)\WX_CMDLG.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_DC.cxx

"$(INTDIR)\WX_DC.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_DIALG.cxx

"$(INTDIR)\WX_DIALG.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_FRAME.cxx

"$(INTDIR)\WX_FRAME.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_GAUGE.cxx

"$(INTDIR)\WX_GAUGE.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_GDI.cxx

"$(INTDIR)\WX_GDI.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_ITEM.cxx

"$(INTDIR)\WX_ITEM.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_LBOX.cxx

"$(INTDIR)\WX_LBOX.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_MAIN.cxx

"$(INTDIR)\WX_MAIN.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_MENU.cxx

"$(INTDIR)\WX_MENU.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_MESSG.cxx

"$(INTDIR)\WX_MESSG.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_MF.cxx

"$(INTDIR)\WX_MF.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_PANEL.cxx

"$(INTDIR)\WX_PANEL.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\wx_pdf.cxx

"$(INTDIR)\wx_pdf.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_RBOX.cxx

"$(INTDIR)\WX_RBOX.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_SLIDR.cxx

"$(INTDIR)\WX_SLIDR.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_TIMER.cxx

"$(INTDIR)\WX_TIMER.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_UTILS.cxx

"$(INTDIR)\WX_UTILS.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Wxwindow\Src\Msw\WX_WIN.cxx

"$(INTDIR)\WX_WIN.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\WXWINDOW\SRC\MSW\wximgfil.cxx

"$(INTDIR)\wximgfil.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

