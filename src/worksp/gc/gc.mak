# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

!IF "$(CFG)" == ""
CFG=gc - Win32 Release
!MESSAGE No configuration specified.  Defaulting to gc - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "gc - Win32 Release" && "$(CFG)" != "gc - Win32 Debug" &&\
 "$(CFG)" != "gc - Win32 Threads"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "gc.mak" CFG="gc - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "gc - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "gc - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "gc - Win32 Threads" (based on "Win32 (x86) Static Library")
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
# PROP Target_Last_Scanned "gc - Win32 Debug"
CPP=cl.exe

!IF  "$(CFG)" == "gc - Win32 Release"

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

ALL : "$(OUTDIR)\gc.lib"

CLEAN : 
	-@erase "Release\vc40.pdb"
	-@erase "Release\gc.lib"
	-@erase "Release\Malloc.obj"
	-@erase "Release\Headers.obj"
	-@erase "Release\Reclaim.obj"
	-@erase "Release\Allchblk.obj"
	-@erase "Release\Mark.obj"
	-@erase "Release\Finalize.obj"
	-@erase "Release\Blacklst.obj"
	-@erase "Release\Os_dep.obj"
	-@erase "Release\winthred.obj"
	-@erase "Release\Obj_map.obj"
	-@erase "Release\Mach_dep.obj"
	-@erase "Release\Stubborn.obj"
	-@erase "Release\Alloc.obj"
	-@erase "Release\Mark_rts.obj"
	-@erase "Release\Dyn_load.obj"
	-@erase "Release\Misc.obj"
	-@erase "Release\Mallocx.obj"
	-@erase "Release\New_hblk.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MT /W3 /GX /Zi /O2 /D "NDEBUG" /D "__STDC__" /D "XXX_NO_AUTO_STACK_PUSH" /D "SILENT" /YX /c
# SUBTRACT CPP /X
CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /D "NDEBUG" /D "__STDC__" /D\
 "XXX_NO_AUTO_STACK_PUSH" /D "SILENT" /Fp"$(INTDIR)/gc.pch" /YX /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c 
CPP_OBJS=Release/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/gc.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/gc.lib" 
LIB32_OBJS= \
	"$(INTDIR)/Malloc.obj" \
	"$(INTDIR)/Headers.obj" \
	"$(INTDIR)/Reclaim.obj" \
	"$(INTDIR)/Allchblk.obj" \
	"$(INTDIR)/Mark.obj" \
	"$(INTDIR)/Finalize.obj" \
	"$(INTDIR)/Blacklst.obj" \
	"$(INTDIR)/Os_dep.obj" \
	"$(INTDIR)/winthred.obj" \
	"$(INTDIR)/Obj_map.obj" \
	"$(INTDIR)/Mach_dep.obj" \
	"$(INTDIR)/Stubborn.obj" \
	"$(INTDIR)/Alloc.obj" \
	"$(INTDIR)/Mark_rts.obj" \
	"$(INTDIR)/Dyn_load.obj" \
	"$(INTDIR)/Misc.obj" \
	"$(INTDIR)/Mallocx.obj" \
	"$(INTDIR)/New_hblk.obj"

"$(OUTDIR)\gc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "gc - Win32 Debug"

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

ALL : "$(OUTDIR)\gc.lib"

CLEAN : 
	-@erase "Debug\vc40.pdb"
	-@erase "Debug\gc.lib"
	-@erase "Debug\Mallocx.obj"
	-@erase "Debug\Malloc.obj"
	-@erase "Debug\Allchblk.obj"
	-@erase "Debug\Finalize.obj"
	-@erase "Debug\Blacklst.obj"
	-@erase "Debug\Headers.obj"
	-@erase "Debug\Mach_dep.obj"
	-@erase "Debug\Stubborn.obj"
	-@erase "Debug\Mark_rts.obj"
	-@erase "Debug\Reclaim.obj"
	-@erase "Debug\Misc.obj"
	-@erase "Debug\Dyn_load.obj"
	-@erase "Debug\Mark.obj"
	-@erase "Debug\New_hblk.obj"
	-@erase "Debug\winthred.obj"
	-@erase "Debug\Alloc.obj"
	-@erase "Debug\Obj_map.obj"
	-@erase "Debug\Os_dep.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MTd /W3 /GX /Zi /Od /D "_DEBUG" /D "__STDC__" /D "XXX_NO_AUTO_STACK_PUSH" /D "SILENT" /YX /c
# SUBTRACT CPP /X
CPP_PROJ=/nologo /MTd /W3 /GX /Zi /Od /D "_DEBUG" /D "__STDC__" /D\
 "XXX_NO_AUTO_STACK_PUSH" /D "SILENT" /Fp"$(INTDIR)/gc.pch" /YX /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c 
CPP_OBJS=Debug/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/gc.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/gc.lib" 
LIB32_OBJS= \
	"$(INTDIR)/Mallocx.obj" \
	"$(INTDIR)/Malloc.obj" \
	"$(INTDIR)/Allchblk.obj" \
	"$(INTDIR)/Finalize.obj" \
	"$(INTDIR)/Blacklst.obj" \
	"$(INTDIR)/Headers.obj" \
	"$(INTDIR)/Mach_dep.obj" \
	"$(INTDIR)/Stubborn.obj" \
	"$(INTDIR)/Mark_rts.obj" \
	"$(INTDIR)/Reclaim.obj" \
	"$(INTDIR)/Misc.obj" \
	"$(INTDIR)/Dyn_load.obj" \
	"$(INTDIR)/Mark.obj" \
	"$(INTDIR)/New_hblk.obj" \
	"$(INTDIR)/winthred.obj" \
	"$(INTDIR)/Alloc.obj" \
	"$(INTDIR)/Obj_map.obj" \
	"$(INTDIR)/Os_dep.obj"

"$(OUTDIR)\gc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "gc - Win32 Threads"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "gc___Win"
# PROP BASE Intermediate_Dir "gc___Win"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Threads"
# PROP Intermediate_Dir "Threads"
# PROP Target_Dir ""
OUTDIR=Threads
INTDIR=Threads

ALL : "$(OUTDIR)\gc.lib"

CLEAN : 
	-@erase "Threads\vc40.pdb"
	-@erase "Threads\gc.lib"
	-@erase "Threads\New_hblk.obj"
	-@erase "Threads\Obj_map.obj"
	-@erase "Threads\Headers.obj"
	-@erase "Threads\Misc.obj"
	-@erase "Threads\Mallocx.obj"
	-@erase "Threads\Reclaim.obj"
	-@erase "Threads\Alloc.obj"
	-@erase "Threads\Allchblk.obj"
	-@erase "Threads\Finalize.obj"
	-@erase "Threads\Blacklst.obj"
	-@erase "Threads\winthred.obj"
	-@erase "Threads\Mark.obj"
	-@erase "Threads\Os_dep.obj"
	-@erase "Threads\Mach_dep.obj"
	-@erase "Threads\Stubborn.obj"
	-@erase "Threads\Mark_rts.obj"
	-@erase "Threads\Malloc.obj"
	-@erase "Threads\Dyn_load.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "XXX_NO_AUTO_STACK_PUSH" /D "SILENT" /YX /c
# ADD CPP /nologo /MT /W3 /GX /Zi /O2 /D "NDEBUG" /D "WIN32_THREADS" /D "__STDC__" /D "XXX_NO_AUTO_STACK_PUSH" /D "SILENT" /YX /c
# SUBTRACT CPP /X
CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /D "NDEBUG" /D "WIN32_THREADS" /D\
 "__STDC__" /D "XXX_NO_AUTO_STACK_PUSH" /D "SILENT" /Fp"$(INTDIR)/gc.pch" /YX\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=Threads/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/gc.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/gc.lib" 
LIB32_OBJS= \
	"$(INTDIR)/New_hblk.obj" \
	"$(INTDIR)/Obj_map.obj" \
	"$(INTDIR)/Headers.obj" \
	"$(INTDIR)/Misc.obj" \
	"$(INTDIR)/Mallocx.obj" \
	"$(INTDIR)/Reclaim.obj" \
	"$(INTDIR)/Alloc.obj" \
	"$(INTDIR)/Allchblk.obj" \
	"$(INTDIR)/Finalize.obj" \
	"$(INTDIR)/Blacklst.obj" \
	"$(INTDIR)/winthred.obj" \
	"$(INTDIR)/Mark.obj" \
	"$(INTDIR)/Os_dep.obj" \
	"$(INTDIR)/Mach_dep.obj" \
	"$(INTDIR)/Stubborn.obj" \
	"$(INTDIR)/Mark_rts.obj" \
	"$(INTDIR)/Malloc.obj" \
	"$(INTDIR)/Dyn_load.obj"

"$(OUTDIR)\gc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
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

# Name "gc - Win32 Release"
# Name "gc - Win32 Debug"
# Name "gc - Win32 Threads"

!IF  "$(CFG)" == "gc - Win32 Release"

!ELSEIF  "$(CFG)" == "gc - Win32 Debug"

!ELSEIF  "$(CFG)" == "gc - Win32 Threads"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Allchblk.c
DEP_CPP_ALLCH=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_ALLCH=\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Allchblk.obj" : $(SOURCE) $(DEP_CPP_ALLCH) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Allchblk.obj" : $(SOURCE) $(DEP_CPP_ALLCH) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Allchblk.obj" : $(SOURCE) $(DEP_CPP_ALLCH) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Alloc.c
DEP_CPP_ALLOC=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_ALLOC=\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Alloc.obj" : $(SOURCE) $(DEP_CPP_ALLOC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Alloc.obj" : $(SOURCE) $(DEP_CPP_ALLOC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Alloc.obj" : $(SOURCE) $(DEP_CPP_ALLOC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Blacklst.c
DEP_CPP_BLACK=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_BLACK=\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Blacklst.obj" : $(SOURCE) $(DEP_CPP_BLACK) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Blacklst.obj" : $(SOURCE) $(DEP_CPP_BLACK) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Blacklst.obj" : $(SOURCE) $(DEP_CPP_BLACK) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Dyn_load.c
DEP_CPP_DYN_L=\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_DYN_L=\
	".\..\..\Mzscheme\Gc\il\PCR_IL.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	".\..\..\Mzscheme\Gc\mm\PCR_MM.h"\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Dyn_load.obj" : $(SOURCE) $(DEP_CPP_DYN_L) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Dyn_load.obj" : $(SOURCE) $(DEP_CPP_DYN_L) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Dyn_load.obj" : $(SOURCE) $(DEP_CPP_DYN_L) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Finalize.c
DEP_CPP_FINAL=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	".\..\..\Mzscheme\Gc\gc_mark.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_FINAL=\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Finalize.obj" : $(SOURCE) $(DEP_CPP_FINAL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Finalize.obj" : $(SOURCE) $(DEP_CPP_FINAL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Finalize.obj" : $(SOURCE) $(DEP_CPP_FINAL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Headers.c
DEP_CPP_HEADE=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_HEADE=\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Headers.obj" : $(SOURCE) $(DEP_CPP_HEADE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Headers.obj" : $(SOURCE) $(DEP_CPP_HEADE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Headers.obj" : $(SOURCE) $(DEP_CPP_HEADE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Mach_dep.c
DEP_CPP_MACH_=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_MACH_=\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Mach_dep.obj" : $(SOURCE) $(DEP_CPP_MACH_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Mach_dep.obj" : $(SOURCE) $(DEP_CPP_MACH_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Mach_dep.obj" : $(SOURCE) $(DEP_CPP_MACH_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Malloc.c
DEP_CPP_MALLO=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_MALLO=\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Malloc.obj" : $(SOURCE) $(DEP_CPP_MALLO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Malloc.obj" : $(SOURCE) $(DEP_CPP_MALLO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Malloc.obj" : $(SOURCE) $(DEP_CPP_MALLO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Mark.c
DEP_CPP_MARK_=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	".\..\..\Mzscheme\Gc\gc_mark.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_MARK_=\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Mark.obj" : $(SOURCE) $(DEP_CPP_MARK_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Mark.obj" : $(SOURCE) $(DEP_CPP_MARK_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Mark.obj" : $(SOURCE) $(DEP_CPP_MARK_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Mark_rts.c
DEP_CPP_MARK_R=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_MARK_R=\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Mark_rts.obj" : $(SOURCE) $(DEP_CPP_MARK_R) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Mark_rts.obj" : $(SOURCE) $(DEP_CPP_MARK_R) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Mark_rts.obj" : $(SOURCE) $(DEP_CPP_MARK_R) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Misc.c
DEP_CPP_MISC_=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_MISC_=\
	".\..\..\Mzscheme\Gc\il\PCR_IL.h"\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Misc.obj" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Misc.obj" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Misc.obj" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\New_hblk.c
DEP_CPP_NEW_H=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_NEW_H=\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\New_hblk.obj" : $(SOURCE) $(DEP_CPP_NEW_H) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\New_hblk.obj" : $(SOURCE) $(DEP_CPP_NEW_H) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\New_hblk.obj" : $(SOURCE) $(DEP_CPP_NEW_H) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Obj_map.c
DEP_CPP_OBJ_M=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_OBJ_M=\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Obj_map.obj" : $(SOURCE) $(DEP_CPP_OBJ_M) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Obj_map.obj" : $(SOURCE) $(DEP_CPP_OBJ_M) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Obj_map.obj" : $(SOURCE) $(DEP_CPP_OBJ_M) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Os_dep.c
DEP_CPP_OS_DE=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_OS_DE=\
	".\..\..\Mzscheme\Gc\il\PCR_IL.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	".\..\..\Mzscheme\Gc\mm\PCR_MM.h"\
	".\..\..\Mzscheme\Gc\vd\PCR_VD.h"\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Os_dep.obj" : $(SOURCE) $(DEP_CPP_OS_DE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Os_dep.obj" : $(SOURCE) $(DEP_CPP_OS_DE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Os_dep.obj" : $(SOURCE) $(DEP_CPP_OS_DE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Reclaim.c
DEP_CPP_RECLA=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_RECLA=\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Reclaim.obj" : $(SOURCE) $(DEP_CPP_RECLA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Reclaim.obj" : $(SOURCE) $(DEP_CPP_RECLA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Reclaim.obj" : $(SOURCE) $(DEP_CPP_RECLA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Stubborn.c
DEP_CPP_STUBB=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_STUBB=\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Stubborn.obj" : $(SOURCE) $(DEP_CPP_STUBB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Stubborn.obj" : $(SOURCE) $(DEP_CPP_STUBB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Stubborn.obj" : $(SOURCE) $(DEP_CPP_STUBB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Mallocx.c
DEP_CPP_MALLOC=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_MALLOC=\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\Mallocx.obj" : $(SOURCE) $(DEP_CPP_MALLOC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\Mallocx.obj" : $(SOURCE) $(DEP_CPP_MALLOC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\Mallocx.obj" : $(SOURCE) $(DEP_CPP_MALLOC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\mzscheme\gc\winthred.c
DEP_CPP_WINTH=\
	".\..\..\Mzscheme\Gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Gc\gc.h"\
	".\..\..\Mzscheme\Gc\config.h"\
	".\..\..\Mzscheme\Gc\gc_hdrs.h"\
	
NODEP_CPP_WINTH=\
	".\..\..\Mzscheme\Gc\th\PCR_Th.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCrSec.h"\
	".\..\..\Mzscheme\Gc\th\PCR_ThCtl.h"\
	

!IF  "$(CFG)" == "gc - Win32 Release"


"$(INTDIR)\winthred.obj" : $(SOURCE) $(DEP_CPP_WINTH) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Debug"


"$(INTDIR)\winthred.obj" : $(SOURCE) $(DEP_CPP_WINTH) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "gc - Win32 Threads"


"$(INTDIR)\winthred.obj" : $(SOURCE) $(DEP_CPP_WINTH) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
