# Microsoft Developer Studio Generated NMAKE File, Based on gc.dsp
!IF "$(CFG)" == ""
CFG=gc - Win32 Release
!MESSAGE No configuration specified. Defaulting to gc - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "gc - Win32 Release" && "$(CFG)" != "gc - Win32 Debug" && "$(CFG)" != "gc - Win32 Threads" && "$(CFG)" != "gc - Win32 MT DLL"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "gc.mak" CFG="gc - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "gc - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "gc - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "gc - Win32 Threads" (based on "Win32 (x86) Static Library")
!MESSAGE "gc - Win32 MT DLL" (based on "Win32 (x86) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "gc - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\gc.lib"


CLEAN :
	-@erase "$(INTDIR)\Allchblk.obj"
	-@erase "$(INTDIR)\Alloc.obj"
	-@erase "$(INTDIR)\Blacklst.obj"
	-@erase "$(INTDIR)\Dyn_load.obj"
	-@erase "$(INTDIR)\Finalize.obj"
	-@erase "$(INTDIR)\Headers.obj"
	-@erase "$(INTDIR)\Mach_dep.obj"
	-@erase "$(INTDIR)\Malloc.obj"
	-@erase "$(INTDIR)\Mallocx.obj"
	-@erase "$(INTDIR)\Mark.obj"
	-@erase "$(INTDIR)\Mark_rts.obj"
	-@erase "$(INTDIR)\Misc.obj"
	-@erase "$(INTDIR)\New_hblk.obj"
	-@erase "$(INTDIR)\Obj_map.obj"
	-@erase "$(INTDIR)\Os_dep.obj"
	-@erase "$(INTDIR)\Reclaim.obj"
	-@erase "$(INTDIR)\Stubborn.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\win32_threads.obj"
	-@erase "$(OUTDIR)\gc.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /D "NDEBUG" /D "__STDC__" /D "SILENT" /D "OLD_BLOCK_ALLOC" /Fp"$(INTDIR)\gc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\gc.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\gc.lib" 
LIB32_OBJS= \
	"$(INTDIR)\Allchblk.obj" \
	"$(INTDIR)\Alloc.obj" \
	"$(INTDIR)\Blacklst.obj" \
	"$(INTDIR)\Dyn_load.obj" \
	"$(INTDIR)\Finalize.obj" \
	"$(INTDIR)\Headers.obj" \
	"$(INTDIR)\Mach_dep.obj" \
	"$(INTDIR)\Malloc.obj" \
	"$(INTDIR)\Mallocx.obj" \
	"$(INTDIR)\Mark.obj" \
	"$(INTDIR)\Mark_rts.obj" \
	"$(INTDIR)\Misc.obj" \
	"$(INTDIR)\New_hblk.obj" \
	"$(INTDIR)\Obj_map.obj" \
	"$(INTDIR)\Os_dep.obj" \
	"$(INTDIR)\Reclaim.obj" \
	"$(INTDIR)\Stubborn.obj" \
	"$(INTDIR)\win32_threads.obj"

"$(OUTDIR)\gc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "gc - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\gc.lib"


CLEAN :
	-@erase "$(INTDIR)\Allchblk.obj"
	-@erase "$(INTDIR)\Alloc.obj"
	-@erase "$(INTDIR)\Blacklst.obj"
	-@erase "$(INTDIR)\Dyn_load.obj"
	-@erase "$(INTDIR)\Finalize.obj"
	-@erase "$(INTDIR)\Headers.obj"
	-@erase "$(INTDIR)\Mach_dep.obj"
	-@erase "$(INTDIR)\Malloc.obj"
	-@erase "$(INTDIR)\Mallocx.obj"
	-@erase "$(INTDIR)\Mark.obj"
	-@erase "$(INTDIR)\Mark_rts.obj"
	-@erase "$(INTDIR)\Misc.obj"
	-@erase "$(INTDIR)\New_hblk.obj"
	-@erase "$(INTDIR)\Obj_map.obj"
	-@erase "$(INTDIR)\Os_dep.obj"
	-@erase "$(INTDIR)\Reclaim.obj"
	-@erase "$(INTDIR)\Stubborn.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\win32_threads.obj"
	-@erase "$(OUTDIR)\gc.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /GX /ZI /Od /D "_DEBUG" /D "__STDC__" /D "SILENT" /Fp"$(INTDIR)\gc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\gc.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\gc.lib" 
LIB32_OBJS= \
	"$(INTDIR)\Allchblk.obj" \
	"$(INTDIR)\Alloc.obj" \
	"$(INTDIR)\Blacklst.obj" \
	"$(INTDIR)\Dyn_load.obj" \
	"$(INTDIR)\Finalize.obj" \
	"$(INTDIR)\Headers.obj" \
	"$(INTDIR)\Mach_dep.obj" \
	"$(INTDIR)\Malloc.obj" \
	"$(INTDIR)\Mallocx.obj" \
	"$(INTDIR)\Mark.obj" \
	"$(INTDIR)\Mark_rts.obj" \
	"$(INTDIR)\Misc.obj" \
	"$(INTDIR)\New_hblk.obj" \
	"$(INTDIR)\Obj_map.obj" \
	"$(INTDIR)\Os_dep.obj" \
	"$(INTDIR)\Reclaim.obj" \
	"$(INTDIR)\Stubborn.obj" \
	"$(INTDIR)\win32_threads.obj"

"$(OUTDIR)\gc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "gc - Win32 Threads"

OUTDIR=.\Threads
INTDIR=.\Threads
# Begin Custom Macros
OutDir=.\Threads
# End Custom Macros

ALL : "$(OUTDIR)\gc.lib"


CLEAN :
	-@erase "$(INTDIR)\Allchblk.obj"
	-@erase "$(INTDIR)\Alloc.obj"
	-@erase "$(INTDIR)\Blacklst.obj"
	-@erase "$(INTDIR)\Dyn_load.obj"
	-@erase "$(INTDIR)\Finalize.obj"
	-@erase "$(INTDIR)\Headers.obj"
	-@erase "$(INTDIR)\Mach_dep.obj"
	-@erase "$(INTDIR)\Malloc.obj"
	-@erase "$(INTDIR)\Mallocx.obj"
	-@erase "$(INTDIR)\Mark.obj"
	-@erase "$(INTDIR)\Mark_rts.obj"
	-@erase "$(INTDIR)\Misc.obj"
	-@erase "$(INTDIR)\New_hblk.obj"
	-@erase "$(INTDIR)\Obj_map.obj"
	-@erase "$(INTDIR)\Os_dep.obj"
	-@erase "$(INTDIR)\Reclaim.obj"
	-@erase "$(INTDIR)\Stubborn.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\win32_threads.obj"
	-@erase "$(OUTDIR)\gc.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /D "NDEBUG" /D "WIN32_THREADS" /D "__STDC__" /D "SILENT" /Fp"$(INTDIR)\gc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\gc.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\gc.lib" 
LIB32_OBJS= \
	"$(INTDIR)\Allchblk.obj" \
	"$(INTDIR)\Alloc.obj" \
	"$(INTDIR)\Blacklst.obj" \
	"$(INTDIR)\Dyn_load.obj" \
	"$(INTDIR)\Finalize.obj" \
	"$(INTDIR)\Headers.obj" \
	"$(INTDIR)\Mach_dep.obj" \
	"$(INTDIR)\Malloc.obj" \
	"$(INTDIR)\Mallocx.obj" \
	"$(INTDIR)\Mark.obj" \
	"$(INTDIR)\Mark_rts.obj" \
	"$(INTDIR)\Misc.obj" \
	"$(INTDIR)\New_hblk.obj" \
	"$(INTDIR)\Obj_map.obj" \
	"$(INTDIR)\Os_dep.obj" \
	"$(INTDIR)\Reclaim.obj" \
	"$(INTDIR)\Stubborn.obj" \
	"$(INTDIR)\win32_threads.obj"

"$(OUTDIR)\gc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "gc - Win32 MT DLL"

OUTDIR=.\MTDLL
INTDIR=.\MTDLL
# Begin Custom Macros
OutDir=.\MTDLL
# End Custom Macros

ALL : "$(OUTDIR)\gc.lib"


CLEAN :
	-@erase "$(INTDIR)\Allchblk.obj"
	-@erase "$(INTDIR)\Alloc.obj"
	-@erase "$(INTDIR)\Blacklst.obj"
	-@erase "$(INTDIR)\Dyn_load.obj"
	-@erase "$(INTDIR)\Finalize.obj"
	-@erase "$(INTDIR)\Headers.obj"
	-@erase "$(INTDIR)\Mach_dep.obj"
	-@erase "$(INTDIR)\Malloc.obj"
	-@erase "$(INTDIR)\Mallocx.obj"
	-@erase "$(INTDIR)\Mark.obj"
	-@erase "$(INTDIR)\Mark_rts.obj"
	-@erase "$(INTDIR)\Misc.obj"
	-@erase "$(INTDIR)\New_hblk.obj"
	-@erase "$(INTDIR)\Obj_map.obj"
	-@erase "$(INTDIR)\Os_dep.obj"
	-@erase "$(INTDIR)\Reclaim.obj"
	-@erase "$(INTDIR)\Stubborn.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\win32_threads.obj"
	-@erase "$(OUTDIR)\gc.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /Zi /O2 /D "__STDC__" /D "SILENT" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\gc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\gc.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\gc.lib" 
LIB32_OBJS= \
	"$(INTDIR)\Allchblk.obj" \
	"$(INTDIR)\Alloc.obj" \
	"$(INTDIR)\Blacklst.obj" \
	"$(INTDIR)\Dyn_load.obj" \
	"$(INTDIR)\Finalize.obj" \
	"$(INTDIR)\Headers.obj" \
	"$(INTDIR)\Mach_dep.obj" \
	"$(INTDIR)\Malloc.obj" \
	"$(INTDIR)\Mallocx.obj" \
	"$(INTDIR)\Mark.obj" \
	"$(INTDIR)\Mark_rts.obj" \
	"$(INTDIR)\Misc.obj" \
	"$(INTDIR)\New_hblk.obj" \
	"$(INTDIR)\Obj_map.obj" \
	"$(INTDIR)\Os_dep.obj" \
	"$(INTDIR)\Reclaim.obj" \
	"$(INTDIR)\Stubborn.obj" \
	"$(INTDIR)\win32_threads.obj"

"$(OUTDIR)\gc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("gc.dep")
!INCLUDE "gc.dep"
!ELSE 
!MESSAGE Warning: cannot find "gc.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "gc - Win32 Release" || "$(CFG)" == "gc - Win32 Debug" || "$(CFG)" == "gc - Win32 Threads" || "$(CFG)" == "gc - Win32 MT DLL"
SOURCE=..\..\Mzscheme\Gc\Allchblk.c

"$(INTDIR)\Allchblk.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Alloc.c

"$(INTDIR)\Alloc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Blacklst.c

"$(INTDIR)\Blacklst.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Dyn_load.c

"$(INTDIR)\Dyn_load.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Finalize.c

"$(INTDIR)\Finalize.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Headers.c

"$(INTDIR)\Headers.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Mach_dep.c

"$(INTDIR)\Mach_dep.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Malloc.c

"$(INTDIR)\Malloc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Mallocx.c

"$(INTDIR)\Mallocx.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Mark.c

"$(INTDIR)\Mark.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Mark_rts.c

"$(INTDIR)\Mark_rts.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Misc.c

"$(INTDIR)\Misc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\New_hblk.c

"$(INTDIR)\New_hblk.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Obj_map.c

"$(INTDIR)\Obj_map.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Os_dep.c

"$(INTDIR)\Os_dep.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Reclaim.c

"$(INTDIR)\Reclaim.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Stubborn.c

"$(INTDIR)\Stubborn.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\gc\win32_threads.c

"$(INTDIR)\win32_threads.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

