# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

!IF "$(CFG)" == ""
CFG=wxwin - Win32 Release
!MESSAGE No configuration specified.  Defaulting to wxwin - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "wxwin - Win32 Release" && "$(CFG)" != "wxwin - Win32 Debug" &&\
 "$(CFG)" != "wxwin - Win32 SGC"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "wxutils.mak" CFG="wxwin - Win32 Release"
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
################################################################################
# Begin Project
# PROP Target_Last_Scanned "wxwin - Win32 Debug"
CPP=cl.exe

!IF  "$(CFG)" == "wxwin - Win32 Release"

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

ALL : "$(OUTDIR)\wxutils.lib"

CLEAN : 
	-@erase "Release\vc40.pdb"
	-@erase "Release\wxutils.lib"
	-@erase "Release\Check.obj"
	-@erase "Release\DIB.obj"
	-@erase "Release\Zyz3d.obj"
	-@erase "Release\wximgxbm.obj"
	-@erase "Release\Draw.obj"
	-@erase "Release\Hashtab.obj"
	-@erase "Release\Zyzgauge.obj"
	-@erase "Release\Crifrdat.obj"
	-@erase "Release\Crifrbuf.obj"
	-@erase "Release\Misc.obj"
	-@erase "Release\Crdatfri.obj"
	-@erase "Release\Wrffri.obj"
	-@erase "Release\Cont.obj"
	-@erase "Release\Rdftoi.obj"
	-@erase "Release\Dumfafa.obj"
	-@erase "Release\Data.obj"
	-@erase "Release\Rdftodat.obj"
	-@erase "Release\Crbuffri.obj"
	-@erase "Release\Parse.obj"
	-@erase "Release\Fafa.obj"
	-@erase "Release\Rgb.obj"
	-@erase "Release\Dialog.obj"
	-@erase "Release\Simx.obj"
	-@erase "Release\Button.obj"
	-@erase "Release\Scan.obj"
	-@erase "Release\Wrffrdat.obj"
	-@erase "Release\Wrffrp.obj"
	-@erase "Release\Static.obj"
	-@erase "Release\Create.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MT /W3 /Zi /O2 /I "..\..\mred\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "NDEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW" /D WX_NORMALIZED_PS_FONTS=1 /c
# SUBTRACT CPP /YX
CPP_PROJ=/nologo /MT /W3 /Zi /O2 /I "..\..\plt\mzscheme\gc" /I\
 "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I\
 ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I\
 "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa"\
 /D "NDEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW"\
 /D WX_NORMALIZED_PS_FONTS=1 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=Release/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/wxutils.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/wxutils.lib" 
LIB32_OBJS= \
	"$(INTDIR)/Check.obj" \
	"$(INTDIR)/DIB.obj" \
	"$(INTDIR)/Zyz3d.obj" \
	"$(INTDIR)/wximgxbm.obj" \
	"$(INTDIR)/Draw.obj" \
	"$(INTDIR)/Hashtab.obj" \
	"$(INTDIR)/Zyzgauge.obj" \
	"$(INTDIR)/Crifrdat.obj" \
	"$(INTDIR)/Crifrbuf.obj" \
	"$(INTDIR)/Misc.obj" \
	"$(INTDIR)/Crdatfri.obj" \
	"$(INTDIR)/Wrffri.obj" \
	"$(INTDIR)/Cont.obj" \
	"$(INTDIR)/Rdftoi.obj" \
	"$(INTDIR)/Dumfafa.obj" \
	"$(INTDIR)/Data.obj" \
	"$(INTDIR)/Rdftodat.obj" \
	"$(INTDIR)/Crbuffri.obj" \
	"$(INTDIR)/Parse.obj" \
	"$(INTDIR)/Fafa.obj" \
	"$(INTDIR)/Rgb.obj" \
	"$(INTDIR)/Dialog.obj" \
	"$(INTDIR)/Simx.obj" \
	"$(INTDIR)/Button.obj" \
	"$(INTDIR)/Scan.obj" \
	"$(INTDIR)/Wrffrdat.obj" \
	"$(INTDIR)/Wrffrp.obj" \
	"$(INTDIR)/Static.obj" \
	"$(INTDIR)/Create.obj"

"$(OUTDIR)\wxutils.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

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

ALL : "$(OUTDIR)\wxutils.lib"

CLEAN : 
	-@erase "Debug\wxutils.lib"
	-@erase "Debug\Wrffri.obj"
	-@erase "Debug\Scan.obj"
	-@erase "Debug\Hashtab.obj"
	-@erase "Debug\Rgb.obj"
	-@erase "Debug\Misc.obj"
	-@erase "Debug\Static.obj"
	-@erase "Debug\DIB.obj"
	-@erase "Debug\Rdftoi.obj"
	-@erase "Debug\Create.obj"
	-@erase "Debug\Crbuffri.obj"
	-@erase "Debug\Dialog.obj"
	-@erase "Debug\Data.obj"
	-@erase "Debug\wximgxbm.obj"
	-@erase "Debug\Fafa.obj"
	-@erase "Debug\Button.obj"
	-@erase "Debug\Dumfafa.obj"
	-@erase "Debug\Crifrdat.obj"
	-@erase "Debug\Parse.obj"
	-@erase "Debug\Wrffrp.obj"
	-@erase "Debug\Check.obj"
	-@erase "Debug\Zyz3d.obj"
	-@erase "Debug\Cont.obj"
	-@erase "Debug\Rdftodat.obj"
	-@erase "Debug\Zyzgauge.obj"
	-@erase "Debug\Crifrbuf.obj"
	-@erase "Debug\Simx.obj"
	-@erase "Debug\Draw.obj"
	-@erase "Debug\Crdatfri.obj"
	-@erase "Debug\Wrffrdat.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MTd /W3 /Z7 /Od /I "..\..\mred\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW" /D WX_NORMALIZED_PS_FONTS=1 /c
# SUBTRACT CPP /YX
CPP_PROJ=/nologo /MTd /W3 /Z7 /Od /I "..\..\mzscheme\gc" /I\
 "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I\
 ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I\
 "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa"\
 /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW"\
 /D WX_NORMALIZED_PS_FONTS=1 /Fo"$(INTDIR)/" /c 
CPP_OBJS=Debug/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/wxutils.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/wxutils.lib" 
LIB32_OBJS= \
	"$(INTDIR)/Wrffri.obj" \
	"$(INTDIR)/Scan.obj" \
	"$(INTDIR)/Hashtab.obj" \
	"$(INTDIR)/Rgb.obj" \
	"$(INTDIR)/Misc.obj" \
	"$(INTDIR)/Static.obj" \
	"$(INTDIR)/DIB.obj" \
	"$(INTDIR)/Rdftoi.obj" \
	"$(INTDIR)/Create.obj" \
	"$(INTDIR)/Crbuffri.obj" \
	"$(INTDIR)/Dialog.obj" \
	"$(INTDIR)/Data.obj" \
	"$(INTDIR)/wximgxbm.obj" \
	"$(INTDIR)/Fafa.obj" \
	"$(INTDIR)/Button.obj" \
	"$(INTDIR)/Dumfafa.obj" \
	"$(INTDIR)/Crifrdat.obj" \
	"$(INTDIR)/Parse.obj" \
	"$(INTDIR)/Wrffrp.obj" \
	"$(INTDIR)/Check.obj" \
	"$(INTDIR)/Zyz3d.obj" \
	"$(INTDIR)/Cont.obj" \
	"$(INTDIR)/Rdftodat.obj" \
	"$(INTDIR)/Zyzgauge.obj" \
	"$(INTDIR)/Crifrbuf.obj" \
	"$(INTDIR)/Simx.obj" \
	"$(INTDIR)/Draw.obj" \
	"$(INTDIR)/Crdatfri.obj" \
	"$(INTDIR)/Wrffrdat.obj"

"$(OUTDIR)\wxutils.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "wxwin___"
# PROP BASE Intermediate_Dir "wxwin___"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "SGC"
# PROP Intermediate_Dir "SGC"
# PROP Target_Dir ""
OUTDIR=SGC
INTDIR=SGC

ALL : "$(OUTDIR)\wxutils.lib"

CLEAN : 
	-@erase "SGC\vc40.pdb"
	-@erase "SGC\wxutils.lib"
	-@erase "SGC\Misc.obj"
	-@erase "SGC\Dialog.obj"
	-@erase "SGC\Wrffrdat.obj"
	-@erase "SGC\Button.obj"
	-@erase "SGC\Cont.obj"
	-@erase "SGC\Wrffrp.obj"
	-@erase "SGC\Static.obj"
	-@erase "SGC\Fafa.obj"
	-@erase "SGC\Parse.obj"
	-@erase "SGC\Create.obj"
	-@erase "SGC\Hashtab.obj"
	-@erase "SGC\wximgxbm.obj"
	-@erase "SGC\Rgb.obj"
	-@erase "SGC\Zyz3d.obj"
	-@erase "SGC\Simx.obj"
	-@erase "SGC\DIB.obj"
	-@erase "SGC\Draw.obj"
	-@erase "SGC\Zyzgauge.obj"
	-@erase "SGC\Crifrdat.obj"
	-@erase "SGC\Crifrbuf.obj"
	-@erase "SGC\Scan.obj"
	-@erase "SGC\Crdatfri.obj"
	-@erase "SGC\Dumfafa.obj"
	-@erase "SGC\Wrffri.obj"
	-@erase "SGC\Rdftodat.obj"
	-@erase "SGC\Crbuffri.obj"
	-@erase "SGC\Check.obj"
	-@erase "SGC\Data.obj"
	-@erase "SGC\Rdftoi.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Z7 /Od /I "c:\Matthew\wxwindow\include\base" /I "c:\Matthew\wxwindow\include\msw" /I ":\Matthew\wxwindow\src\base" /I "c:\Matthew\wxwindow\src\msw" /I "c:\Matthew\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "c:\Matthew\mred\mzscheme\gc" /I "c:\Matthew\wxWindow\contrib\fafa" /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW" /D WX_NORMALIZED_PS_FONTS=1 /YX"wx.h" /c
# ADD CPP /nologo /MTd /W3 /Zi /Od /I "..\..\mred\mzscheme\sgc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW" /D WX_NORMALIZED_PS_FONTS=1 /D "USE_SENORA_GC" /D "USE_WXOBJECT_TRACE_COUNT" /c
# SUBTRACT CPP /YX
CPP_PROJ=/nologo /MTd /W3 /Zi /Od /I "..\..\mzscheme\sgc" /I\
 "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I\
 ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I\
 "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa"\
 /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW"\
 /D WX_NORMALIZED_PS_FONTS=1 /D "USE_SENORA_GC" /D "USE_WXOBJECT_TRACE_COUNT"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=SGC/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/wxutils.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/wxutils.lib" 
LIB32_OBJS= \
	"$(INTDIR)/Misc.obj" \
	"$(INTDIR)/Dialog.obj" \
	"$(INTDIR)/Wrffrdat.obj" \
	"$(INTDIR)/Button.obj" \
	"$(INTDIR)/Cont.obj" \
	"$(INTDIR)/Wrffrp.obj" \
	"$(INTDIR)/Static.obj" \
	"$(INTDIR)/Fafa.obj" \
	"$(INTDIR)/Parse.obj" \
	"$(INTDIR)/Create.obj" \
	"$(INTDIR)/Hashtab.obj" \
	"$(INTDIR)/wximgxbm.obj" \
	"$(INTDIR)/Rgb.obj" \
	"$(INTDIR)/Zyz3d.obj" \
	"$(INTDIR)/Simx.obj" \
	"$(INTDIR)/DIB.obj" \
	"$(INTDIR)/Draw.obj" \
	"$(INTDIR)/Zyzgauge.obj" \
	"$(INTDIR)/Crifrdat.obj" \
	"$(INTDIR)/Crifrbuf.obj" \
	"$(INTDIR)/Scan.obj" \
	"$(INTDIR)/Crdatfri.obj" \
	"$(INTDIR)/Dumfafa.obj" \
	"$(INTDIR)/Wrffri.obj" \
	"$(INTDIR)/Rdftodat.obj" \
	"$(INTDIR)/Crbuffri.obj" \
	"$(INTDIR)/Check.obj" \
	"$(INTDIR)/Data.obj" \
	"$(INTDIR)/Rdftoi.obj"

"$(OUTDIR)\wxutils.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
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

# Name "wxwin - Win32 Release"
# Name "wxwin - Win32 Debug"
# Name "wxwin - Win32 SGC"

!IF  "$(CFG)" == "wxwin - Win32 Release"

!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Wrffrp.c
DEP_CPP_WRFFR=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Wrffrp.obj" : $(SOURCE) $(DEP_CPP_WRFFR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Wrffrp.obj" : $(SOURCE) $(DEP_CPP_WRFFR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Wrffrp.obj" : $(SOURCE) $(DEP_CPP_WRFFR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crdatfri.c
DEP_CPP_CRDAT=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Crdatfri.obj" : $(SOURCE) $(DEP_CPP_CRDAT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Crdatfri.obj" : $(SOURCE) $(DEP_CPP_CRDAT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Crdatfri.obj" : $(SOURCE) $(DEP_CPP_CRDAT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Create.c
DEP_CPP_CREAT=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Create.obj" : $(SOURCE) $(DEP_CPP_CREAT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Create.obj" : $(SOURCE) $(DEP_CPP_CREAT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Create.obj" : $(SOURCE) $(DEP_CPP_CREAT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crifrbuf.c
DEP_CPP_CRIFR=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Crifrbuf.obj" : $(SOURCE) $(DEP_CPP_CRIFR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Crifrbuf.obj" : $(SOURCE) $(DEP_CPP_CRIFR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Crifrbuf.obj" : $(SOURCE) $(DEP_CPP_CRIFR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crifrdat.c
DEP_CPP_CRIFRD=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Crifrdat.obj" : $(SOURCE) $(DEP_CPP_CRIFRD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Crifrdat.obj" : $(SOURCE) $(DEP_CPP_CRIFRD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Crifrdat.obj" : $(SOURCE) $(DEP_CPP_CRIFRD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Data.c
DEP_CPP_DATA_=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Data.obj" : $(SOURCE) $(DEP_CPP_DATA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Data.obj" : $(SOURCE) $(DEP_CPP_DATA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Data.obj" : $(SOURCE) $(DEP_CPP_DATA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Hashtab.c
DEP_CPP_HASHT=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Hashtab.obj" : $(SOURCE) $(DEP_CPP_HASHT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Hashtab.obj" : $(SOURCE) $(DEP_CPP_HASHT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Hashtab.obj" : $(SOURCE) $(DEP_CPP_HASHT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Misc.c
DEP_CPP_MISC_=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Misc.obj" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Misc.obj" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Misc.obj" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Parse.c
DEP_CPP_PARSE=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Parse.obj" : $(SOURCE) $(DEP_CPP_PARSE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Parse.obj" : $(SOURCE) $(DEP_CPP_PARSE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Parse.obj" : $(SOURCE) $(DEP_CPP_PARSE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Rdftodat.c
DEP_CPP_RDFTO=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Rdftodat.obj" : $(SOURCE) $(DEP_CPP_RDFTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Rdftodat.obj" : $(SOURCE) $(DEP_CPP_RDFTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Rdftodat.obj" : $(SOURCE) $(DEP_CPP_RDFTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Rdftoi.c
DEP_CPP_RDFTOI=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Rdftoi.obj" : $(SOURCE) $(DEP_CPP_RDFTOI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Rdftoi.obj" : $(SOURCE) $(DEP_CPP_RDFTOI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Rdftoi.obj" : $(SOURCE) $(DEP_CPP_RDFTOI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Rgb.c
DEP_CPP_RGB_C=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\rgbtab.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Rgb.obj" : $(SOURCE) $(DEP_CPP_RGB_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Rgb.obj" : $(SOURCE) $(DEP_CPP_RGB_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Rgb.obj" : $(SOURCE) $(DEP_CPP_RGB_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Scan.c
DEP_CPP_SCAN_=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Scan.obj" : $(SOURCE) $(DEP_CPP_SCAN_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Scan.obj" : $(SOURCE) $(DEP_CPP_SCAN_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Scan.obj" : $(SOURCE) $(DEP_CPP_SCAN_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Simx.c
DEP_CPP_SIMX_=\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Simx.obj" : $(SOURCE) $(DEP_CPP_SIMX_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Simx.obj" : $(SOURCE) $(DEP_CPP_SIMX_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Simx.obj" : $(SOURCE) $(DEP_CPP_SIMX_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Wrffrdat.c
DEP_CPP_WRFFRD=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Wrffrdat.obj" : $(SOURCE) $(DEP_CPP_WRFFRD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Wrffrdat.obj" : $(SOURCE) $(DEP_CPP_WRFFRD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Wrffrdat.obj" : $(SOURCE) $(DEP_CPP_WRFFRD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Wrffri.c
DEP_CPP_WRFFRI=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Wrffri.obj" : $(SOURCE) $(DEP_CPP_WRFFRI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Wrffri.obj" : $(SOURCE) $(DEP_CPP_WRFFRI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Wrffri.obj" : $(SOURCE) $(DEP_CPP_WRFFRI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crbuffri.c
DEP_CPP_CRBUF=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Crbuffri.obj" : $(SOURCE) $(DEP_CPP_CRBUF) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Crbuffri.obj" : $(SOURCE) $(DEP_CPP_CRBUF) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Crbuffri.obj" : $(SOURCE) $(DEP_CPP_CRBUF) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Utils\Dib\DIB.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_DIB_C=\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\..\..\utils\dib\dib.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	

"$(INTDIR)\DIB.obj" : $(SOURCE) $(DEP_CPP_DIB_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_DIB_C=\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\..\..\utils\dib\dib.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	

"$(INTDIR)\DIB.obj" : $(SOURCE) $(DEP_CPP_DIB_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_DIB_C=\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\..\..\utils\dib\dib.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	

"$(INTDIR)\DIB.obj" : $(SOURCE) $(DEP_CPP_DIB_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Gauge\Zyz3d.c
DEP_CPP_ZYZ3D=\
	".\..\..\Wxwindow\Contrib\Gauge\zyz3d.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Zyz3d.obj" : $(SOURCE) $(DEP_CPP_ZYZ3D) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Zyz3d.obj" : $(SOURCE) $(DEP_CPP_ZYZ3D) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Zyz3d.obj" : $(SOURCE) $(DEP_CPP_ZYZ3D) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Gauge\Zyzgauge.c
DEP_CPP_ZYZGA=\
	".\..\..\Wxwindow\Contrib\Gauge\zyz3d.h"\
	".\..\..\wxwindow\include\base\..\..\contrib\gauge\zyzgauge.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Zyzgauge.obj" : $(SOURCE) $(DEP_CPP_ZYZGA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Zyzgauge.obj" : $(SOURCE) $(DEP_CPP_ZYZGA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Zyzgauge.obj" : $(SOURCE) $(DEP_CPP_ZYZGA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Static.c
DEP_CPP_STATI=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Static.obj" : $(SOURCE) $(DEP_CPP_STATI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Static.obj" : $(SOURCE) $(DEP_CPP_STATI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Static.obj" : $(SOURCE) $(DEP_CPP_STATI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Check.c
DEP_CPP_CHECK=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Check.obj" : $(SOURCE) $(DEP_CPP_CHECK) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Check.obj" : $(SOURCE) $(DEP_CPP_CHECK) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Check.obj" : $(SOURCE) $(DEP_CPP_CHECK) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Cont.c
DEP_CPP_CONT_=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Cont.obj" : $(SOURCE) $(DEP_CPP_CONT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Cont.obj" : $(SOURCE) $(DEP_CPP_CONT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Cont.obj" : $(SOURCE) $(DEP_CPP_CONT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Dialog.c
DEP_CPP_DIALO=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Dialog.obj" : $(SOURCE) $(DEP_CPP_DIALO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Dialog.obj" : $(SOURCE) $(DEP_CPP_DIALO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Dialog.obj" : $(SOURCE) $(DEP_CPP_DIALO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Draw.c
DEP_CPP_DRAW_=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Draw.obj" : $(SOURCE) $(DEP_CPP_DRAW_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Draw.obj" : $(SOURCE) $(DEP_CPP_DRAW_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Draw.obj" : $(SOURCE) $(DEP_CPP_DRAW_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Dumfafa.c
DEP_CPP_DUMFA=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Dumfafa.obj" : $(SOURCE) $(DEP_CPP_DUMFA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Dumfafa.obj" : $(SOURCE) $(DEP_CPP_DUMFA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Dumfafa.obj" : $(SOURCE) $(DEP_CPP_DUMFA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Fafa.c
DEP_CPP_FAFA_=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Fafa.obj" : $(SOURCE) $(DEP_CPP_FAFA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Fafa.obj" : $(SOURCE) $(DEP_CPP_FAFA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Fafa.obj" : $(SOURCE) $(DEP_CPP_FAFA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Button.c
DEP_CPP_BUTTO=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Button.obj" : $(SOURCE) $(DEP_CPP_BUTTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Button.obj" : $(SOURCE) $(DEP_CPP_BUTTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Button.obj" : $(SOURCE) $(DEP_CPP_BUTTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\wximgxbm.cxx
DEP_CPP_WXIMG=\
	".\..\..\wxwindow\include\msw\wximgxbm.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\wximgxbm.obj" : $(SOURCE) $(DEP_CPP_WXIMG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\wximgxbm.obj" : $(SOURCE) $(DEP_CPP_WXIMG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\wximgxbm.obj" : $(SOURCE) $(DEP_CPP_WXIMG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
