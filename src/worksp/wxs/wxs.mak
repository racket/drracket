# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

!IF "$(CFG)" == ""
CFG=wxs - Win32 Release
!MESSAGE No configuration specified.  Defaulting to wxs - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "wxs - Win32 Release" && "$(CFG)" != "wxs - Win32 Debug" &&\
 "$(CFG)" != "wxs - Win32 SGC"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
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
################################################################################
# Begin Project
# PROP Target_Last_Scanned "wxs - Win32 Debug"
CPP=cl.exe

!IF  "$(CFG)" == "wxs - Win32 Release"

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

ALL : "$(OUTDIR)\wxs.lib"

CLEAN : 
	-@erase "Release\vc40.pdb"
	-@erase "Release\wxs.lib"
	-@erase "Release\WXS_GAGE.obj"
	-@erase "Release\WXS_SLID.obj"
	-@erase "Release\WXS_BUTN.obj"
	-@erase "Release\WXS_ITEM.obj"
	-@erase "Release\WXS_LBOX.obj"
	-@erase "Release\WXS_MPB.obj"
	-@erase "Release\WXS_DC.obj"
	-@erase "Release\WXS_MIO.obj"
	-@erase "Release\WXS_RADO.obj"
	-@erase "Release\WXSCHEME.obj"
	-@erase "Release\WXS_MENU.obj"
	-@erase "Release\WXS_WIN.obj"
	-@erase "Release\WXS_MISC.obj"
	-@erase "Release\WXS_MADM.obj"
	-@erase "Release\WXS_GDI.obj"
	-@erase "Release\WXS_BMAP.obj"
	-@erase "Release\WXS_CKBX.obj"
	-@erase "Release\WXS_PANL.obj"
	-@erase "Release\WXS_FRAM.obj"
	-@erase "Release\WXS_TEXT.obj"
	-@erase "Release\WXS_GLOB.obj"
	-@erase "Release\WXS_MEDE.obj"
	-@erase "Release\WXS_CHCE.obj"
	-@erase "Release\WXS_STYL.obj"
	-@erase "Release\WXS_OBJ.obj"
	-@erase "Release\WXS_CNVS.obj"
	-@erase "Release\WXS_MEDI.obj"
	-@erase "Release\WXS_EVNT.obj"
	-@erase "Release\WXS_SNIP.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "NDEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT" /D "__WINDOWS__" /D "WXME_FOR_MRED" /YX"wx.h" /c
CPP_PROJ=/nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I\
 "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I\
 "..\..\mzscheme\include" /I "..\..\mred\wxme" /I\
 "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "NDEBUG" /D\
 "__STDC__" /D "WIN32" /D "_WINDOWS" /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT"\
 /D "__WINDOWS__" /D "WXME_FOR_MRED" /Fp"$(INTDIR)/wxs.pch" /YX"wx.h"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=Release/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/wxs.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/wxs.lib" 
LIB32_OBJS= \
	"$(INTDIR)/WXS_GAGE.obj" \
	"$(INTDIR)/WXS_SLID.obj" \
	"$(INTDIR)/WXS_BUTN.obj" \
	"$(INTDIR)/WXS_ITEM.obj" \
	"$(INTDIR)/WXS_LBOX.obj" \
	"$(INTDIR)/WXS_MPB.obj" \
	"$(INTDIR)/WXS_DC.obj" \
	"$(INTDIR)/WXS_MIO.obj" \
	"$(INTDIR)/WXS_RADO.obj" \
	"$(INTDIR)/WXSCHEME.obj" \
	"$(INTDIR)/WXS_MENU.obj" \
	"$(INTDIR)/WXS_WIN.obj" \
	"$(INTDIR)/WXS_MISC.obj" \
	"$(INTDIR)/WXS_MADM.obj" \
	"$(INTDIR)/WXS_GDI.obj" \
	"$(INTDIR)/WXS_BMAP.obj" \
	"$(INTDIR)/WXS_CKBX.obj" \
	"$(INTDIR)/WXS_PANL.obj" \
	"$(INTDIR)/WXS_FRAM.obj" \
	"$(INTDIR)/WXS_TEXT.obj" \
	"$(INTDIR)/WXS_GLOB.obj" \
	"$(INTDIR)/WXS_MEDE.obj" \
	"$(INTDIR)/WXS_CHCE.obj" \
	"$(INTDIR)/WXS_STYL.obj" \
	"$(INTDIR)/WXS_OBJ.obj" \
	"$(INTDIR)/WXS_CNVS.obj" \
	"$(INTDIR)/WXS_MEDI.obj" \
	"$(INTDIR)/WXS_EVNT.obj" \
	"$(INTDIR)/WXS_SNIP.obj"

"$(OUTDIR)\wxs.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

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

ALL : "$(OUTDIR)\wxs.lib"

CLEAN : 
	-@erase "Debug\vc40.pdb"
	-@erase "Debug\vc40.idb"
	-@erase "Debug\wxs.lib"
	-@erase "Debug\WXS_MIO.obj"
	-@erase "Debug\WXS_RADO.obj"
	-@erase "Debug\WXS_MEDI.obj"
	-@erase "Debug\WXS_EVNT.obj"
	-@erase "Debug\WXS_OBJ.obj"
	-@erase "Debug\WXS_SNIP.obj"
	-@erase "Debug\WXS_WIN.obj"
	-@erase "Debug\WXS_GDI.obj"
	-@erase "Debug\WXS_GAGE.obj"
	-@erase "Debug\WXS_SLID.obj"
	-@erase "Debug\WXS_BUTN.obj"
	-@erase "Debug\WXS_ITEM.obj"
	-@erase "Debug\WXS_FRAM.obj"
	-@erase "Debug\WXS_LBOX.obj"
	-@erase "Debug\WXSCHEME.obj"
	-@erase "Debug\WXS_MENU.obj"
	-@erase "Debug\WXS_MISC.obj"
	-@erase "Debug\WXS_MADM.obj"
	-@erase "Debug\WXS_BMAP.obj"
	-@erase "Debug\WXS_DC.obj"
	-@erase "Debug\WXS_CKBX.obj"
	-@erase "Debug\WXS_PANL.obj"
	-@erase "Debug\WXS_TEXT.obj"
	-@erase "Debug\WXS_MPB.obj"
	-@erase "Debug\WXS_GLOB.obj"
	-@erase "Debug\WXS_MEDE.obj"
	-@erase "Debug\WXS_CHCE.obj"
	-@erase "Debug\WXS_STYL.obj"
	-@erase "Debug\WXS_CNVS.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MTd /W3 /Gm /Zi /Od /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "__DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT" /D "__WINDOWS__" /D "WXME_FOR_MRED" /YX"wx.h" /c
CPP_PROJ=/nologo /MTd /W3 /Gm /Zi /Od /I "..\..\mzscheme\gc" /I\
 "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I\
 "..\..\mzscheme\include" /I "..\..\mred\wxme" /I\
 "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "__DEBUG" /D\
 "__STDC__" /D "WIN32" /D "_WINDOWS" /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT"\
 /D "__WINDOWS__" /D "WXME_FOR_MRED" /Fp"$(INTDIR)/wxs.pch" /YX"wx.h"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=Debug/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/wxs.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/wxs.lib" 
LIB32_OBJS= \
	"$(INTDIR)/WXS_MIO.obj" \
	"$(INTDIR)/WXS_RADO.obj" \
	"$(INTDIR)/WXS_MEDI.obj" \
	"$(INTDIR)/WXS_EVNT.obj" \
	"$(INTDIR)/WXS_OBJ.obj" \
	"$(INTDIR)/WXS_SNIP.obj" \
	"$(INTDIR)/WXS_WIN.obj" \
	"$(INTDIR)/WXS_GDI.obj" \
	"$(INTDIR)/WXS_GAGE.obj" \
	"$(INTDIR)/WXS_SLID.obj" \
	"$(INTDIR)/WXS_BUTN.obj" \
	"$(INTDIR)/WXS_ITEM.obj" \
	"$(INTDIR)/WXS_FRAM.obj" \
	"$(INTDIR)/WXS_LBOX.obj" \
	"$(INTDIR)/WXSCHEME.obj" \
	"$(INTDIR)/WXS_MENU.obj" \
	"$(INTDIR)/WXS_MISC.obj" \
	"$(INTDIR)/WXS_MADM.obj" \
	"$(INTDIR)/WXS_BMAP.obj" \
	"$(INTDIR)/WXS_DC.obj" \
	"$(INTDIR)/WXS_CKBX.obj" \
	"$(INTDIR)/WXS_PANL.obj" \
	"$(INTDIR)/WXS_TEXT.obj" \
	"$(INTDIR)/WXS_MPB.obj" \
	"$(INTDIR)/WXS_GLOB.obj" \
	"$(INTDIR)/WXS_MEDE.obj" \
	"$(INTDIR)/WXS_CHCE.obj" \
	"$(INTDIR)/WXS_STYL.obj" \
	"$(INTDIR)/WXS_CNVS.obj"

"$(OUTDIR)\wxs.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "wxs___Wi"
# PROP BASE Intermediate_Dir "wxs___Wi"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "SGC"
# PROP Intermediate_Dir "SGC"
# PROP Target_Dir ""
OUTDIR=SGC
INTDIR=SGC

ALL : "$(OUTDIR)\wxs.lib"

CLEAN : 
	-@erase "SGC\vc40.pdb"
	-@erase "SGC\vc40.idb"
	-@erase "SGC\wxs.lib"
	-@erase "SGC\WXS_DC.obj"
	-@erase "SGC\WXS_MENU.obj"
	-@erase "SGC\WXS_MISC.obj"
	-@erase "SGC\WXS_OBJ.obj"
	-@erase "SGC\WXS_BMAP.obj"
	-@erase "SGC\WXS_SLID.obj"
	-@erase "SGC\WXS_BUTN.obj"
	-@erase "SGC\WXS_ITEM.obj"
	-@erase "SGC\WXS_FRAM.obj"
	-@erase "SGC\WXS_MEDE.obj"
	-@erase "SGC\WXS_STYL.obj"
	-@erase "SGC\WXSCHEME.obj"
	-@erase "SGC\WXS_MEDI.obj"
	-@erase "SGC\WXS_MPB.obj"
	-@erase "SGC\WXS_SNIP.obj"
	-@erase "SGC\WXS_MADM.obj"
	-@erase "SGC\WXS_MIO.obj"
	-@erase "SGC\WXS_CKBX.obj"
	-@erase "SGC\WXS_PANL.obj"
	-@erase "SGC\WXS_GAGE.obj"
	-@erase "SGC\WXS_TEXT.obj"
	-@erase "SGC\WXS_WIN.obj"
	-@erase "SGC\WXS_LBOX.obj"
	-@erase "SGC\WXS_GLOB.obj"
	-@erase "SGC\WXS_CHCE.obj"
	-@erase "SGC\WXS_CNVS.obj"
	-@erase "SGC\WXS_GDI.obj"
	-@erase "SGC\WXS_RADO.obj"
	-@erase "SGC\WXS_EVNT.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /Zi /Od /I "c:\matthew\wxwindow\include\base" /I "c:\Matthew\wxwindow\include\msw" /I "c:\Matthew\mzscheme\include" /I "c:\Matthew\mzscheme\gc" /I "c:\Matthew\mred\wxme" /I "c:\Matthew\mzscheme\utils" /D "__DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT" /D "__WINDOWS__" /D "WXME_FOR_MRED" /YX /c
# ADD CPP /nologo /MTd /W3 /Gm /Zi /Od /I "..\..\mzscheme\sgc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "__DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT" /D "__WINDOWS__" /D "WXME_FOR_MRED" /D "USE_SENORA_GC" /YX"wx.h" /c
CPP_PROJ=/nologo /MTd /W3 /Gm /Zi /Od /I "..\..\mzscheme\sgc" /I\
 "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I\
 "..\..\mzscheme\include" /I "..\..\mred\wxme" /I\
 "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "__DEBUG" /D\
 "__STDC__" /D "WIN32" /D "_WINDOWS" /D "WXS_CANT_ASSIGN_STRUCTURES" /D "WINNT"\
 /D "__WINDOWS__" /D "WXME_FOR_MRED" /D "USE_SENORA_GC" /Fp"$(INTDIR)/wxs.pch"\
 /YX"wx.h" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=SGC/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/wxs.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/wxs.lib" 
LIB32_OBJS= \
	"$(INTDIR)/WXS_DC.obj" \
	"$(INTDIR)/WXS_MENU.obj" \
	"$(INTDIR)/WXS_MISC.obj" \
	"$(INTDIR)/WXS_OBJ.obj" \
	"$(INTDIR)/WXS_BMAP.obj" \
	"$(INTDIR)/WXS_SLID.obj" \
	"$(INTDIR)/WXS_BUTN.obj" \
	"$(INTDIR)/WXS_ITEM.obj" \
	"$(INTDIR)/WXS_FRAM.obj" \
	"$(INTDIR)/WXS_MEDE.obj" \
	"$(INTDIR)/WXS_STYL.obj" \
	"$(INTDIR)/WXSCHEME.obj" \
	"$(INTDIR)/WXS_MEDI.obj" \
	"$(INTDIR)/WXS_MPB.obj" \
	"$(INTDIR)/WXS_SNIP.obj" \
	"$(INTDIR)/WXS_MADM.obj" \
	"$(INTDIR)/WXS_MIO.obj" \
	"$(INTDIR)/WXS_CKBX.obj" \
	"$(INTDIR)/WXS_PANL.obj" \
	"$(INTDIR)/WXS_GAGE.obj" \
	"$(INTDIR)/WXS_TEXT.obj" \
	"$(INTDIR)/WXS_WIN.obj" \
	"$(INTDIR)/WXS_LBOX.obj" \
	"$(INTDIR)/WXS_GLOB.obj" \
	"$(INTDIR)/WXS_CHCE.obj" \
	"$(INTDIR)/WXS_CNVS.obj" \
	"$(INTDIR)/WXS_GDI.obj" \
	"$(INTDIR)/WXS_RADO.obj" \
	"$(INTDIR)/WXS_EVNT.obj"

"$(OUTDIR)\wxs.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
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

# Name "wxs - Win32 Release"
# Name "wxs - Win32 Debug"
# Name "wxs - Win32 SGC"

!IF  "$(CFG)" == "wxs - Win32 Release"

!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_DC.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_D=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_mf.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_dc.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_D=\
	".\..\..\mred\Wxs\wx_dcpr.h"\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_DC.obj" : $(SOURCE) $(DEP_CPP_WXS_D) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_D=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_mf.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_dc.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_D=\
	".\..\..\mred\Wxs\wx_dcpr.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_DC.obj" : $(SOURCE) $(DEP_CPP_WXS_D) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_D=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_mf.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_dc.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_D=\
	".\..\..\mred\Wxs\wx_dcpr.h"\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_DC.obj" : $(SOURCE) $(DEP_CPP_WXS_D) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_BUTN.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_B=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_butn.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_B=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_BUTN.obj" : $(SOURCE) $(DEP_CPP_WXS_B) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_B=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_butn.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
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
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
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
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_B=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_BUTN.obj" : $(SOURCE) $(DEP_CPP_WXS_B) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_B=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_butn.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_B=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_BUTN.obj" : $(SOURCE) $(DEP_CPP_WXS_B) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_CHCE.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_C=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_chce.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_C=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_CHCE.obj" : $(SOURCE) $(DEP_CPP_WXS_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_C=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_chce.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
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
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
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
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_C=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_CHCE.obj" : $(SOURCE) $(DEP_CPP_WXS_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_C=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_chce.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_C=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_CHCE.obj" : $(SOURCE) $(DEP_CPP_WXS_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_CKBX.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_CK=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_ckbx.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_CK=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_CKBX.obj" : $(SOURCE) $(DEP_CPP_WXS_CK) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_CK=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_ckbx.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
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
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
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
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_CK=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_CKBX.obj" : $(SOURCE) $(DEP_CPP_WXS_CK) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_CK=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_ckbx.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_CK=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_CKBX.obj" : $(SOURCE) $(DEP_CPP_WXS_CK) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_CNVS.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_CN=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_cnvs.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_CN=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_CNVS.obj" : $(SOURCE) $(DEP_CPP_WXS_CN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_CN=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_cnvs.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_CN=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_CNVS.obj" : $(SOURCE) $(DEP_CPP_WXS_CN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_CN=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_cnvs.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_CN=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_CNVS.obj" : $(SOURCE) $(DEP_CPP_WXS_CN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_BMAP.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_BM=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\mred\Wxs\wxs_bmt.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_bmap.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_BM=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_BMAP.obj" : $(SOURCE) $(DEP_CPP_WXS_BM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_BM=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\mred\Wxs\wxs_bmt.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_bmap.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_BM=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_BMAP.obj" : $(SOURCE) $(DEP_CPP_WXS_BM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_BM=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\mred\Wxs\wxs_bmt.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_bmap.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_BM=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_BMAP.obj" : $(SOURCE) $(DEP_CPP_WXS_BM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_EVNT.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_E=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_evnt.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\contrib\fafa\fafa.h"\
	".\..\..\wxwindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_E=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_EVNT.obj" : $(SOURCE) $(DEP_CPP_WXS_E) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_E=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_evnt.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\contrib\fafa\fafa.h"\
	".\..\..\wxwindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_E=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_EVNT.obj" : $(SOURCE) $(DEP_CPP_WXS_E) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_E=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_evnt.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_E=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_EVNT.obj" : $(SOURCE) $(DEP_CPP_WXS_E) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_FRAM.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_F=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_fram.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_F=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_FRAM.obj" : $(SOURCE) $(DEP_CPP_WXS_F) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_F=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_fram.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_F=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_FRAM.obj" : $(SOURCE) $(DEP_CPP_WXS_F) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_F=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_fram.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_F=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_FRAM.obj" : $(SOURCE) $(DEP_CPP_WXS_F) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_GAGE.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_G=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_gage.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_G=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_GAGE.obj" : $(SOURCE) $(DEP_CPP_WXS_G) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_G=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_gage.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
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
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
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
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_G=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_GAGE.obj" : $(SOURCE) $(DEP_CPP_WXS_G) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_G=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_gage.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_G=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_GAGE.obj" : $(SOURCE) $(DEP_CPP_WXS_G) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_GDI.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_GD=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\mred\Wxs\wxs_bmt.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_gdi.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_GD=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_GDI.obj" : $(SOURCE) $(DEP_CPP_WXS_GD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_GD=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\mred\Wxs\wxs_bmt.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_gdi.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_GD=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_GDI.obj" : $(SOURCE) $(DEP_CPP_WXS_GD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_GD=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\mred\Wxs\wxs_bmt.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_gdi.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_GD=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_GDI.obj" : $(SOURCE) $(DEP_CPP_WXS_GD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_GLOB.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_GL=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_print.h"\
	".\..\..\wxwindow\include\msw\wx_mf.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_glob.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_GL=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_GLOB.obj" : $(SOURCE) $(DEP_CPP_WXS_GL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_GL=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_print.h"\
	".\..\..\wxwindow\include\msw\wx_mf.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_glob.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
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
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_GL=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_GLOB.obj" : $(SOURCE) $(DEP_CPP_WXS_GL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_GL=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_print.h"\
	".\..\..\wxwindow\include\msw\wx_mf.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_glob.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_GL=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_GLOB.obj" : $(SOURCE) $(DEP_CPP_WXS_GL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_ITEM.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_I=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_item.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_I=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_ITEM.obj" : $(SOURCE) $(DEP_CPP_WXS_I) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_I=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_item.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
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
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_I=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_ITEM.obj" : $(SOURCE) $(DEP_CPP_WXS_I) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_I=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_item.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_I=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_ITEM.obj" : $(SOURCE) $(DEP_CPP_WXS_I) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_LBOX.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_L=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_lbox.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_L=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_LBOX.obj" : $(SOURCE) $(DEP_CPP_WXS_L) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_L=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_lbox.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
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
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
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
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_L=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_LBOX.obj" : $(SOURCE) $(DEP_CPP_WXS_L) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_L=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_lbox.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_L=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_LBOX.obj" : $(SOURCE) $(DEP_CPP_WXS_L) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_MADM.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_M=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_madm.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\mred\Wxs\wxs_obj.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_M=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MADM.obj" : $(SOURCE) $(DEP_CPP_WXS_M) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_M=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_madm.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\mred\Wxs\wxs_obj.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_M=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MADM.obj" : $(SOURCE) $(DEP_CPP_WXS_M) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_M=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_madm.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\mred\Wxs\wxs_obj.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_M=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MADM.obj" : $(SOURCE) $(DEP_CPP_WXS_M) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_MEDE.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_ME=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_mede.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_ME=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MEDE.obj" : $(SOURCE) $(DEP_CPP_WXS_ME) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_ME=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_mede.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
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
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_ME=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MEDE.obj" : $(SOURCE) $(DEP_CPP_WXS_ME) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_ME=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_mede.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_ME=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MEDE.obj" : $(SOURCE) $(DEP_CPP_WXS_ME) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_MEDI.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_MED=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_medi.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_MED=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MEDI.obj" : $(SOURCE) $(DEP_CPP_WXS_MED) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_MED=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_medi.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
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
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_MED=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MEDI.obj" : $(SOURCE) $(DEP_CPP_WXS_MED) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_MED=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_medi.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_MED=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MEDI.obj" : $(SOURCE) $(DEP_CPP_WXS_MED) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_MENU.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_MEN=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_menu.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_MEN=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MENU.obj" : $(SOURCE) $(DEP_CPP_WXS_MEN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_MEN=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_menu.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_MEN=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MENU.obj" : $(SOURCE) $(DEP_CPP_WXS_MEN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_MEN=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_menu.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_MEN=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MENU.obj" : $(SOURCE) $(DEP_CPP_WXS_MEN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_MIO.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_MI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_mio.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_MI=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MIO.obj" : $(SOURCE) $(DEP_CPP_WXS_MI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_MI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_mio.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_MI=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MIO.obj" : $(SOURCE) $(DEP_CPP_WXS_MI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_MI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_mio.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_MI=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MIO.obj" : $(SOURCE) $(DEP_CPP_WXS_MI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_MISC.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_MIS=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_misc.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\msw\wx_mf.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	
NODEP_CPP_WXS_MIS=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MISC.obj" : $(SOURCE) $(DEP_CPP_WXS_MIS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_MIS=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_misc.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\msw\wx_mf.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	
NODEP_CPP_WXS_MIS=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MISC.obj" : $(SOURCE) $(DEP_CPP_WXS_MIS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_MIS=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_misc.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\msw\wx_mf.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	
NODEP_CPP_WXS_MIS=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MISC.obj" : $(SOURCE) $(DEP_CPP_WXS_MIS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_MPB.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_MP=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_mpb.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_MP=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MPB.obj" : $(SOURCE) $(DEP_CPP_WXS_MP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_MP=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_mpb.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
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
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_MP=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MPB.obj" : $(SOURCE) $(DEP_CPP_WXS_MP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_MP=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_mpb.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_MP=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_MPB.obj" : $(SOURCE) $(DEP_CPP_WXS_MP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_OBJ.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_O=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_obj.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_O=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_OBJ.obj" : $(SOURCE) $(DEP_CPP_WXS_O) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_O=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_obj.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_O=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_OBJ.obj" : $(SOURCE) $(DEP_CPP_WXS_O) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_O=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_obj.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_O=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_OBJ.obj" : $(SOURCE) $(DEP_CPP_WXS_O) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_PANL.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_P=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wx_enhdg.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_panl.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_P=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_PANL.obj" : $(SOURCE) $(DEP_CPP_WXS_P) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_P=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wx_enhdg.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_panl.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
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
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_P=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_PANL.obj" : $(SOURCE) $(DEP_CPP_WXS_P) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_P=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wx_enhdg.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_panl.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_P=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_PANL.obj" : $(SOURCE) $(DEP_CPP_WXS_P) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_RADO.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_R=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_rado.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\mred\Wxs\wxs_bmap.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_R=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_RADO.obj" : $(SOURCE) $(DEP_CPP_WXS_R) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_R=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_rado.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\mred\Wxs\wxs_bmap.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
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
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
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
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_R=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_RADO.obj" : $(SOURCE) $(DEP_CPP_WXS_R) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_R=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_rado.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\mred\Wxs\wxs_bmap.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_R=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_RADO.obj" : $(SOURCE) $(DEP_CPP_WXS_R) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_SLID.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_S=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_slid.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_S=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_SLID.obj" : $(SOURCE) $(DEP_CPP_WXS_S) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_S=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_slid.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
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
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
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
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_S=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_SLID.obj" : $(SOURCE) $(DEP_CPP_WXS_S) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_S=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_slid.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_S=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_SLID.obj" : $(SOURCE) $(DEP_CPP_WXS_S) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_SNIP.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_SN=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_snip.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_SN=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_SNIP.obj" : $(SOURCE) $(DEP_CPP_WXS_SN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_SN=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_snip.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
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
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_SN=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_SNIP.obj" : $(SOURCE) $(DEP_CPP_WXS_SN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_SN=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_snip.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_SN=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_SNIP.obj" : $(SOURCE) $(DEP_CPP_WXS_SN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_STYL.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_ST=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_styl.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
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
	
NODEP_CPP_WXS_ST=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_STYL.obj" : $(SOURCE) $(DEP_CPP_WXS_ST) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_ST=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_styl.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_ST=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_STYL.obj" : $(SOURCE) $(DEP_CPP_WXS_ST) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_ST=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_styl.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
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
	
NODEP_CPP_WXS_ST=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_STYL.obj" : $(SOURCE) $(DEP_CPP_WXS_ST) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_TEXT.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_T=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_text.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_T=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_TEXT.obj" : $(SOURCE) $(DEP_CPP_WXS_T) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_T=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_text.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
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
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_T=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_TEXT.obj" : $(SOURCE) $(DEP_CPP_WXS_T) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_T=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_text.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_T=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_TEXT.obj" : $(SOURCE) $(DEP_CPP_WXS_T) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXS_WIN.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXS_W=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_win.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_W=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_WIN.obj" : $(SOURCE) $(DEP_CPP_WXS_W) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXS_W=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_win.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_W=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_WIN.obj" : $(SOURCE) $(DEP_CPP_WXS_W) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXS_W=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\mred\Wxs\wxs_win.h"\
	".\..\..\mred\Wxs\wxscomon.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	"..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	"..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_WXS_W=\
	"..\..\wxwindow\include\msw\fafa.h"\
	"..\..\wxwindow\include\msw\fafapriv.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXS_WIN.obj" : $(SOURCE) $(DEP_CPP_WXS_W) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mred\Wxs\WXSCHEME.cxx

!IF  "$(CFG)" == "wxs - Win32 Release"

DEP_CPP_WXSCH=\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\wxwindow\include\msw\wx_pdf.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\mred\Wxs\wxsmred.h"\
	".\..\..\mred\Wxs\wxs_obj.h"\
	".\..\..\mred\Wxs\wxs_win.h"\
	".\..\..\mred\Wxs\wxs_fram.h"\
	".\..\..\mred\Wxs\wxs_item.h"\
	".\..\..\mred\Wxs\wxs_butn.h"\
	".\..\..\mred\Wxs\wxs_text.h"\
	".\..\..\mred\Wxs\wxs_ckbx.h"\
	".\..\..\mred\Wxs\wxs_chce.h"\
	".\..\..\mred\Wxs\wxs_evnt.h"\
	".\..\..\mred\Wxs\wxs_panl.h"\
	".\..\..\mred\Wxs\wxs_list.h"\
	".\..\..\mred\Wxs\wxs_medi.h"\
	".\..\..\mred\Wxs\wxs_mede.h"\
	".\..\..\mred\Wxs\wxs_madm.h"\
	".\..\..\mred\Wxs\wxs_snip.h"\
	".\..\..\mred\Wxs\wxs_mpb.h"\
	".\..\..\mred\Wxs\wxs_mio.h"\
	".\..\..\mred\Wxs\wxs_styl.h"\
	".\..\..\mred\Wxs\wxs_menu.h"\
	".\..\..\mred\Wxs\wxs_bmap.h"\
	".\..\..\mred\Wxs\wxs_ipc.h"\
	".\..\..\mred\Wxs\wxs_help.h"\
	".\..\..\mred\Wxs\wxs_misc.h"\
	".\..\..\mred\Wxs\wxs_rado.h"\
	".\..\..\mred\Wxs\wxs_slid.h"\
	".\..\..\mred\Wxs\wxs_gage.h"\
	".\..\..\mred\Wxs\wxs_lbox.h"\
	".\..\..\mred\Wxs\wxs_tbar.h"\
	".\..\..\mred\Wxs\wxs_cstr.h"\
	".\..\..\mred\Wxs\wxs_glob.h"\
	".\..\..\mred\Wxs\wxs_gdi.h"\
	".\..\..\mred\Wxs\wxs_dc.h"\
	".\..\..\mred\Wxs\wxs_cnvs.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WXSCH=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXSCHEME.obj" : $(SOURCE) $(DEP_CPP_WXSCH) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 Debug"

DEP_CPP_WXSCH=\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\wxwindow\include\msw\wx_pdf.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\mred\Wxs\wxsmred.h"\
	".\..\..\mred\Wxs\wxs_obj.h"\
	".\..\..\mred\Wxs\wxs_win.h"\
	".\..\..\mred\Wxs\wxs_fram.h"\
	".\..\..\mred\Wxs\wxs_item.h"\
	".\..\..\mred\Wxs\wxs_butn.h"\
	".\..\..\mred\Wxs\wxs_text.h"\
	".\..\..\mred\Wxs\wxs_ckbx.h"\
	".\..\..\mred\Wxs\wxs_chce.h"\
	".\..\..\mred\Wxs\wxs_evnt.h"\
	".\..\..\mred\Wxs\wxs_panl.h"\
	".\..\..\mred\Wxs\wxs_list.h"\
	".\..\..\mred\Wxs\wxs_medi.h"\
	".\..\..\mred\Wxs\wxs_mede.h"\
	".\..\..\mred\Wxs\wxs_madm.h"\
	".\..\..\mred\Wxs\wxs_snip.h"\
	".\..\..\mred\Wxs\wxs_mpb.h"\
	".\..\..\mred\Wxs\wxs_mio.h"\
	".\..\..\mred\Wxs\wxs_styl.h"\
	".\..\..\mred\Wxs\wxs_menu.h"\
	".\..\..\mred\Wxs\wxs_bmap.h"\
	".\..\..\mred\Wxs\wxs_ipc.h"\
	".\..\..\mred\Wxs\wxs_help.h"\
	".\..\..\mred\Wxs\wxs_misc.h"\
	".\..\..\mred\Wxs\wxs_rado.h"\
	".\..\..\mred\Wxs\wxs_slid.h"\
	".\..\..\mred\Wxs\wxs_gage.h"\
	".\..\..\mred\Wxs\wxs_lbox.h"\
	".\..\..\mred\Wxs\wxs_tbar.h"\
	".\..\..\mred\Wxs\wxs_cstr.h"\
	".\..\..\mred\Wxs\wxs_glob.h"\
	".\..\..\mred\Wxs\wxs_gdi.h"\
	".\..\..\mred\Wxs\wxs_dc.h"\
	".\..\..\mred\Wxs\wxs_cnvs.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WXSCH=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXSCHEME.obj" : $(SOURCE) $(DEP_CPP_WXSCH) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxs - Win32 SGC"

DEP_CPP_WXSCH=\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\mred\wxme\wx_media.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\mred\Wxs\wxscheme.h"\
	".\..\..\wxwindow\include\msw\wx_pdf.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\mred\Wxs\wxsmred.h"\
	".\..\..\mred\Wxs\wxs_obj.h"\
	".\..\..\mred\Wxs\wxs_win.h"\
	".\..\..\mred\Wxs\wxs_fram.h"\
	".\..\..\mred\Wxs\wxs_item.h"\
	".\..\..\mred\Wxs\wxs_butn.h"\
	".\..\..\mred\Wxs\wxs_text.h"\
	".\..\..\mred\Wxs\wxs_ckbx.h"\
	".\..\..\mred\Wxs\wxs_chce.h"\
	".\..\..\mred\Wxs\wxs_evnt.h"\
	".\..\..\mred\Wxs\wxs_panl.h"\
	".\..\..\mred\Wxs\wxs_list.h"\
	".\..\..\mred\Wxs\wxs_medi.h"\
	".\..\..\mred\Wxs\wxs_mede.h"\
	".\..\..\mred\Wxs\wxs_madm.h"\
	".\..\..\mred\Wxs\wxs_snip.h"\
	".\..\..\mred\Wxs\wxs_mpb.h"\
	".\..\..\mred\Wxs\wxs_mio.h"\
	".\..\..\mred\Wxs\wxs_styl.h"\
	".\..\..\mred\Wxs\wxs_menu.h"\
	".\..\..\mred\Wxs\wxs_bmap.h"\
	".\..\..\mred\Wxs\wxs_ipc.h"\
	".\..\..\mred\Wxs\wxs_help.h"\
	".\..\..\mred\Wxs\wxs_misc.h"\
	".\..\..\mred\Wxs\wxs_rado.h"\
	".\..\..\mred\Wxs\wxs_slid.h"\
	".\..\..\mred\Wxs\wxs_gage.h"\
	".\..\..\mred\Wxs\wxs_lbox.h"\
	".\..\..\mred\Wxs\wxs_tbar.h"\
	".\..\..\mred\Wxs\wxs_cstr.h"\
	".\..\..\mred\Wxs\wxs_glob.h"\
	".\..\..\mred\Wxs\wxs_gdi.h"\
	".\..\..\mred\Wxs\wxs_dc.h"\
	".\..\..\mred\Wxs\wxs_cnvs.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\mred\wxme\wx_keym.h"\
	".\..\..\mred\wxme\wx_medio.h"\
	".\..\..\mred\wxme\wx_style.h"\
	".\..\..\mred\wxme\wx_mtype.h"\
	".\..\..\mred\wxme\wx_snip.h"\
	".\..\..\mred\wxme\wx_cgrec.h"\
	".\..\..\mred\wxme\wx_medad.h"\
	".\..\..\mred\wxme\wx_medpb.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\mred\wxme\wx_madm.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\mzscheme\utils\xcglue.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\src\stypes.h"\
	".\..\..\mzscheme\src\schexn.h"\
	".\..\..\mzscheme\src\schemef.h"\
	".\..\..\mzscheme\src\schemex.h"\
	".\..\..\mzscheme\src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WXSCH=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\include\stypes.h"\
	".\..\..\mzscheme\include\schexn.h"\
	".\..\..\mzscheme\include\schemef.h"\
	".\..\..\mzscheme\include\schemex.h"\
	".\..\..\mzscheme\include\schemexm.h"\
	

"$(INTDIR)\WXSCHEME.obj" : $(SOURCE) $(DEP_CPP_WXSCH) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
