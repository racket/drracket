# Microsoft Developer Studio Generated NMAKE File, Based on mzsrc.dsp
!IF "$(CFG)" == ""
CFG=mzsrc - Win32 Release
!MESSAGE No configuration specified. Defaulting to mzsrc - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "mzsrc - Win32 Release" && "$(CFG)" != "mzsrc - Win32 Debug" && "$(CFG)" != "mzsrc - Win32 SGC" && "$(CFG)" != "mzsrc - Win32 Threads" && "$(CFG)" != "mzsrc - Win32 MT DLL"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "mzsrc.mak" CFG="mzsrc - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mzsrc - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "mzsrc - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "mzsrc - Win32 SGC" (based on "Win32 (x86) Static Library")
!MESSAGE "mzsrc - Win32 Threads" (based on "Win32 (x86) Static Library")
!MESSAGE "mzsrc - Win32 MT DLL" (based on "Win32 (x86) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "mzsrc - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\mzsrc.lib"


CLEAN :
	-@erase "$(INTDIR)\Bignum.obj"
	-@erase "$(INTDIR)\Bool.obj"
	-@erase "$(INTDIR)\builtin.obj"
	-@erase "$(INTDIR)\Char.obj"
	-@erase "$(INTDIR)\Complex.obj"
	-@erase "$(INTDIR)\Dynext.obj"
	-@erase "$(INTDIR)\Env.obj"
	-@erase "$(INTDIR)\Error.obj"
	-@erase "$(INTDIR)\Eval.obj"
	-@erase "$(INTDIR)\File.obj"
	-@erase "$(INTDIR)\Fun.obj"
	-@erase "$(INTDIR)\Hash.obj"
	-@erase "$(INTDIR)\image.obj"
	-@erase "$(INTDIR)\List.obj"
	-@erase "$(INTDIR)\mzsj86.obj"
	-@erase "$(INTDIR)\network.obj"
	-@erase "$(INTDIR)\numarith.obj"
	-@erase "$(INTDIR)\Number.obj"
	-@erase "$(INTDIR)\numcomp.obj"
	-@erase "$(INTDIR)\numstr.obj"
	-@erase "$(INTDIR)\objclass.obj"
	-@erase "$(INTDIR)\Object.obj"
	-@erase "$(INTDIR)\Port.obj"
	-@erase "$(INTDIR)\portfun.obj"
	-@erase "$(INTDIR)\Print.obj"
	-@erase "$(INTDIR)\Process.obj"
	-@erase "$(INTDIR)\Promise.obj"
	-@erase "$(INTDIR)\Rational.obj"
	-@erase "$(INTDIR)\Read.obj"
	-@erase "$(INTDIR)\Regexp.obj"
	-@erase "$(INTDIR)\Salloc.obj"
	-@erase "$(INTDIR)\Sema.obj"
	-@erase "$(INTDIR)\Setjmpup.obj"
	-@erase "$(INTDIR)\String.obj"
	-@erase "$(INTDIR)\Struct.obj"
	-@erase "$(INTDIR)\Symbol.obj"
	-@erase "$(INTDIR)\Syntax.obj"
	-@erase "$(INTDIR)\Tsymbol.obj"
	-@erase "$(INTDIR)\Type.obj"
	-@erase "$(INTDIR)\Unit.obj"
	-@erase "$(INTDIR)\unitsig.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\Vector.obj"
	-@erase "$(OUTDIR)\mzsrc.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

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
BSC32_FLAGS=/nologo /o".\DebugOpt\mzsrc.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\mzsrc.lib" 
LIB32_OBJS= \
	"$(INTDIR)\Bignum.obj" \
	"$(INTDIR)\Bool.obj" \
	"$(INTDIR)\Char.obj" \
	"$(INTDIR)\Complex.obj" \
	"$(INTDIR)\Dynext.obj" \
	"$(INTDIR)\Env.obj" \
	"$(INTDIR)\Error.obj" \
	"$(INTDIR)\Eval.obj" \
	"$(INTDIR)\File.obj" \
	"$(INTDIR)\Fun.obj" \
	"$(INTDIR)\Hash.obj" \
	"$(INTDIR)\image.obj" \
	"$(INTDIR)\List.obj" \
	"$(INTDIR)\mzsj86.obj" \
	"$(INTDIR)\Number.obj" \
	"$(INTDIR)\Object.obj" \
	"$(INTDIR)\Port.obj" \
	"$(INTDIR)\Print.obj" \
	"$(INTDIR)\Process.obj" \
	"$(INTDIR)\Promise.obj" \
	"$(INTDIR)\Rational.obj" \
	"$(INTDIR)\Read.obj" \
	"$(INTDIR)\Regexp.obj" \
	"$(INTDIR)\Salloc.obj" \
	"$(INTDIR)\Sema.obj" \
	"$(INTDIR)\Setjmpup.obj" \
	"$(INTDIR)\String.obj" \
	"$(INTDIR)\Struct.obj" \
	"$(INTDIR)\Symbol.obj" \
	"$(INTDIR)\Syntax.obj" \
	"$(INTDIR)\Tsymbol.obj" \
	"$(INTDIR)\Type.obj" \
	"$(INTDIR)\Unit.obj" \
	"$(INTDIR)\Vector.obj" \
	"$(INTDIR)\unitsig.obj" \
	"$(INTDIR)\numarith.obj" \
	"$(INTDIR)\numcomp.obj" \
	"$(INTDIR)\numstr.obj" \
	"$(INTDIR)\objclass.obj" \
	"$(INTDIR)\portfun.obj" \
	"$(INTDIR)\builtin.obj" \
	"$(INTDIR)\network.obj"

"$(OUTDIR)\mzsrc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\mzsrc.lib"


CLEAN :
	-@erase "$(INTDIR)\Bignum.obj"
	-@erase "$(INTDIR)\Bool.obj"
	-@erase "$(INTDIR)\builtin.obj"
	-@erase "$(INTDIR)\Char.obj"
	-@erase "$(INTDIR)\Complex.obj"
	-@erase "$(INTDIR)\Dynext.obj"
	-@erase "$(INTDIR)\Env.obj"
	-@erase "$(INTDIR)\Error.obj"
	-@erase "$(INTDIR)\Eval.obj"
	-@erase "$(INTDIR)\File.obj"
	-@erase "$(INTDIR)\Fun.obj"
	-@erase "$(INTDIR)\Hash.obj"
	-@erase "$(INTDIR)\image.obj"
	-@erase "$(INTDIR)\List.obj"
	-@erase "$(INTDIR)\mzsj86.obj"
	-@erase "$(INTDIR)\network.obj"
	-@erase "$(INTDIR)\numarith.obj"
	-@erase "$(INTDIR)\Number.obj"
	-@erase "$(INTDIR)\numcomp.obj"
	-@erase "$(INTDIR)\numstr.obj"
	-@erase "$(INTDIR)\objclass.obj"
	-@erase "$(INTDIR)\Object.obj"
	-@erase "$(INTDIR)\Port.obj"
	-@erase "$(INTDIR)\portfun.obj"
	-@erase "$(INTDIR)\Print.obj"
	-@erase "$(INTDIR)\Process.obj"
	-@erase "$(INTDIR)\Promise.obj"
	-@erase "$(INTDIR)\Rational.obj"
	-@erase "$(INTDIR)\Read.obj"
	-@erase "$(INTDIR)\Regexp.obj"
	-@erase "$(INTDIR)\Salloc.obj"
	-@erase "$(INTDIR)\Sema.obj"
	-@erase "$(INTDIR)\Setjmpup.obj"
	-@erase "$(INTDIR)\String.obj"
	-@erase "$(INTDIR)\Struct.obj"
	-@erase "$(INTDIR)\Symbol.obj"
	-@erase "$(INTDIR)\Syntax.obj"
	-@erase "$(INTDIR)\Tsymbol.obj"
	-@erase "$(INTDIR)\Type.obj"
	-@erase "$(INTDIR)\Unit.obj"
	-@erase "$(INTDIR)\unitsig.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\Vector.obj"
	-@erase "$(OUTDIR)\mzsrc.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mzsrc.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\mzsrc.lib" 
LIB32_OBJS= \
	"$(INTDIR)\Bignum.obj" \
	"$(INTDIR)\Bool.obj" \
	"$(INTDIR)\Char.obj" \
	"$(INTDIR)\Complex.obj" \
	"$(INTDIR)\Dynext.obj" \
	"$(INTDIR)\Env.obj" \
	"$(INTDIR)\Error.obj" \
	"$(INTDIR)\Eval.obj" \
	"$(INTDIR)\File.obj" \
	"$(INTDIR)\Fun.obj" \
	"$(INTDIR)\Hash.obj" \
	"$(INTDIR)\image.obj" \
	"$(INTDIR)\List.obj" \
	"$(INTDIR)\mzsj86.obj" \
	"$(INTDIR)\Number.obj" \
	"$(INTDIR)\Object.obj" \
	"$(INTDIR)\Port.obj" \
	"$(INTDIR)\Print.obj" \
	"$(INTDIR)\Process.obj" \
	"$(INTDIR)\Promise.obj" \
	"$(INTDIR)\Rational.obj" \
	"$(INTDIR)\Read.obj" \
	"$(INTDIR)\Regexp.obj" \
	"$(INTDIR)\Salloc.obj" \
	"$(INTDIR)\Sema.obj" \
	"$(INTDIR)\Setjmpup.obj" \
	"$(INTDIR)\String.obj" \
	"$(INTDIR)\Struct.obj" \
	"$(INTDIR)\Symbol.obj" \
	"$(INTDIR)\Syntax.obj" \
	"$(INTDIR)\Tsymbol.obj" \
	"$(INTDIR)\Type.obj" \
	"$(INTDIR)\Unit.obj" \
	"$(INTDIR)\Vector.obj" \
	"$(INTDIR)\unitsig.obj" \
	"$(INTDIR)\numarith.obj" \
	"$(INTDIR)\numcomp.obj" \
	"$(INTDIR)\numstr.obj" \
	"$(INTDIR)\objclass.obj" \
	"$(INTDIR)\portfun.obj" \
	"$(INTDIR)\builtin.obj" \
	"$(INTDIR)\network.obj"

"$(OUTDIR)\mzsrc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

OUTDIR=.\SGC
INTDIR=.\SGC
# Begin Custom Macros
OutDir=.\SGC
# End Custom Macros

ALL : "$(OUTDIR)\mzsrc.lib"


CLEAN :
	-@erase "$(INTDIR)\Bignum.obj"
	-@erase "$(INTDIR)\Bool.obj"
	-@erase "$(INTDIR)\builtin.obj"
	-@erase "$(INTDIR)\Char.obj"
	-@erase "$(INTDIR)\Complex.obj"
	-@erase "$(INTDIR)\Dynext.obj"
	-@erase "$(INTDIR)\Env.obj"
	-@erase "$(INTDIR)\Error.obj"
	-@erase "$(INTDIR)\Eval.obj"
	-@erase "$(INTDIR)\File.obj"
	-@erase "$(INTDIR)\Fun.obj"
	-@erase "$(INTDIR)\Hash.obj"
	-@erase "$(INTDIR)\image.obj"
	-@erase "$(INTDIR)\List.obj"
	-@erase "$(INTDIR)\mzsj86.obj"
	-@erase "$(INTDIR)\network.obj"
	-@erase "$(INTDIR)\numarith.obj"
	-@erase "$(INTDIR)\Number.obj"
	-@erase "$(INTDIR)\numcomp.obj"
	-@erase "$(INTDIR)\numstr.obj"
	-@erase "$(INTDIR)\objclass.obj"
	-@erase "$(INTDIR)\Object.obj"
	-@erase "$(INTDIR)\Port.obj"
	-@erase "$(INTDIR)\portfun.obj"
	-@erase "$(INTDIR)\Print.obj"
	-@erase "$(INTDIR)\Process.obj"
	-@erase "$(INTDIR)\Promise.obj"
	-@erase "$(INTDIR)\Rational.obj"
	-@erase "$(INTDIR)\Read.obj"
	-@erase "$(INTDIR)\Regexp.obj"
	-@erase "$(INTDIR)\Salloc.obj"
	-@erase "$(INTDIR)\Sema.obj"
	-@erase "$(INTDIR)\Setjmpup.obj"
	-@erase "$(INTDIR)\String.obj"
	-@erase "$(INTDIR)\Struct.obj"
	-@erase "$(INTDIR)\Symbol.obj"
	-@erase "$(INTDIR)\Syntax.obj"
	-@erase "$(INTDIR)\Tsymbol.obj"
	-@erase "$(INTDIR)\Type.obj"
	-@erase "$(INTDIR)\Unit.obj"
	-@erase "$(INTDIR)\unitsig.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\Vector.obj"
	-@erase "$(OUTDIR)\mzsrc.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mzsrc.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\mzsrc.lib" 
LIB32_OBJS= \
	"$(INTDIR)\Bignum.obj" \
	"$(INTDIR)\Bool.obj" \
	"$(INTDIR)\Char.obj" \
	"$(INTDIR)\Complex.obj" \
	"$(INTDIR)\Dynext.obj" \
	"$(INTDIR)\Env.obj" \
	"$(INTDIR)\Error.obj" \
	"$(INTDIR)\Eval.obj" \
	"$(INTDIR)\File.obj" \
	"$(INTDIR)\Fun.obj" \
	"$(INTDIR)\Hash.obj" \
	"$(INTDIR)\image.obj" \
	"$(INTDIR)\List.obj" \
	"$(INTDIR)\mzsj86.obj" \
	"$(INTDIR)\Number.obj" \
	"$(INTDIR)\Object.obj" \
	"$(INTDIR)\Port.obj" \
	"$(INTDIR)\Print.obj" \
	"$(INTDIR)\Process.obj" \
	"$(INTDIR)\Promise.obj" \
	"$(INTDIR)\Rational.obj" \
	"$(INTDIR)\Read.obj" \
	"$(INTDIR)\Regexp.obj" \
	"$(INTDIR)\Salloc.obj" \
	"$(INTDIR)\Sema.obj" \
	"$(INTDIR)\Setjmpup.obj" \
	"$(INTDIR)\String.obj" \
	"$(INTDIR)\Struct.obj" \
	"$(INTDIR)\Symbol.obj" \
	"$(INTDIR)\Syntax.obj" \
	"$(INTDIR)\Tsymbol.obj" \
	"$(INTDIR)\Type.obj" \
	"$(INTDIR)\Unit.obj" \
	"$(INTDIR)\Vector.obj" \
	"$(INTDIR)\unitsig.obj" \
	"$(INTDIR)\numarith.obj" \
	"$(INTDIR)\numcomp.obj" \
	"$(INTDIR)\numstr.obj" \
	"$(INTDIR)\objclass.obj" \
	"$(INTDIR)\portfun.obj" \
	"$(INTDIR)\builtin.obj" \
	"$(INTDIR)\network.obj"

"$(OUTDIR)\mzsrc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

OUTDIR=.\Threads
INTDIR=.\Threads
# Begin Custom Macros
OutDir=.\Threads
# End Custom Macros

ALL : "$(OUTDIR)\mzsrc.lib"


CLEAN :
	-@erase "$(INTDIR)\Bignum.obj"
	-@erase "$(INTDIR)\Bool.obj"
	-@erase "$(INTDIR)\builtin.obj"
	-@erase "$(INTDIR)\Char.obj"
	-@erase "$(INTDIR)\Complex.obj"
	-@erase "$(INTDIR)\Dynext.obj"
	-@erase "$(INTDIR)\Env.obj"
	-@erase "$(INTDIR)\Error.obj"
	-@erase "$(INTDIR)\Eval.obj"
	-@erase "$(INTDIR)\File.obj"
	-@erase "$(INTDIR)\Fun.obj"
	-@erase "$(INTDIR)\Hash.obj"
	-@erase "$(INTDIR)\image.obj"
	-@erase "$(INTDIR)\List.obj"
	-@erase "$(INTDIR)\mzsj86.obj"
	-@erase "$(INTDIR)\network.obj"
	-@erase "$(INTDIR)\numarith.obj"
	-@erase "$(INTDIR)\Number.obj"
	-@erase "$(INTDIR)\numcomp.obj"
	-@erase "$(INTDIR)\numstr.obj"
	-@erase "$(INTDIR)\objclass.obj"
	-@erase "$(INTDIR)\Object.obj"
	-@erase "$(INTDIR)\Port.obj"
	-@erase "$(INTDIR)\portfun.obj"
	-@erase "$(INTDIR)\Print.obj"
	-@erase "$(INTDIR)\Process.obj"
	-@erase "$(INTDIR)\Promise.obj"
	-@erase "$(INTDIR)\Rational.obj"
	-@erase "$(INTDIR)\Read.obj"
	-@erase "$(INTDIR)\Regexp.obj"
	-@erase "$(INTDIR)\Salloc.obj"
	-@erase "$(INTDIR)\Sema.obj"
	-@erase "$(INTDIR)\Setjmpup.obj"
	-@erase "$(INTDIR)\String.obj"
	-@erase "$(INTDIR)\Struct.obj"
	-@erase "$(INTDIR)\Symbol.obj"
	-@erase "$(INTDIR)\Syntax.obj"
	-@erase "$(INTDIR)\Tsymbol.obj"
	-@erase "$(INTDIR)\Type.obj"
	-@erase "$(INTDIR)\Unit.obj"
	-@erase "$(INTDIR)\unitsig.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\Vector.obj"
	-@erase "$(OUTDIR)\mzsrc.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o".\DebugOpt\mzsrc.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\mzsrc.lib" 
LIB32_OBJS= \
	"$(INTDIR)\Bignum.obj" \
	"$(INTDIR)\Bool.obj" \
	"$(INTDIR)\Char.obj" \
	"$(INTDIR)\Complex.obj" \
	"$(INTDIR)\Dynext.obj" \
	"$(INTDIR)\Env.obj" \
	"$(INTDIR)\Error.obj" \
	"$(INTDIR)\Eval.obj" \
	"$(INTDIR)\File.obj" \
	"$(INTDIR)\Fun.obj" \
	"$(INTDIR)\Hash.obj" \
	"$(INTDIR)\image.obj" \
	"$(INTDIR)\List.obj" \
	"$(INTDIR)\mzsj86.obj" \
	"$(INTDIR)\Number.obj" \
	"$(INTDIR)\Object.obj" \
	"$(INTDIR)\Port.obj" \
	"$(INTDIR)\Print.obj" \
	"$(INTDIR)\Process.obj" \
	"$(INTDIR)\Promise.obj" \
	"$(INTDIR)\Rational.obj" \
	"$(INTDIR)\Read.obj" \
	"$(INTDIR)\Regexp.obj" \
	"$(INTDIR)\Salloc.obj" \
	"$(INTDIR)\Sema.obj" \
	"$(INTDIR)\Setjmpup.obj" \
	"$(INTDIR)\String.obj" \
	"$(INTDIR)\Struct.obj" \
	"$(INTDIR)\Symbol.obj" \
	"$(INTDIR)\Syntax.obj" \
	"$(INTDIR)\Tsymbol.obj" \
	"$(INTDIR)\Type.obj" \
	"$(INTDIR)\Unit.obj" \
	"$(INTDIR)\Vector.obj" \
	"$(INTDIR)\unitsig.obj" \
	"$(INTDIR)\numarith.obj" \
	"$(INTDIR)\numcomp.obj" \
	"$(INTDIR)\numstr.obj" \
	"$(INTDIR)\objclass.obj" \
	"$(INTDIR)\portfun.obj" \
	"$(INTDIR)\builtin.obj" \
	"$(INTDIR)\network.obj"

"$(OUTDIR)\mzsrc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

OUTDIR=.\MTDLL
INTDIR=.\MTDLL
# Begin Custom Macros
OutDir=.\MTDLL
# End Custom Macros

ALL : "$(OUTDIR)\mzsrc.lib"


CLEAN :
	-@erase "$(INTDIR)\Bignum.obj"
	-@erase "$(INTDIR)\Bool.obj"
	-@erase "$(INTDIR)\builtin.obj"
	-@erase "$(INTDIR)\Char.obj"
	-@erase "$(INTDIR)\Complex.obj"
	-@erase "$(INTDIR)\Dynext.obj"
	-@erase "$(INTDIR)\Env.obj"
	-@erase "$(INTDIR)\Error.obj"
	-@erase "$(INTDIR)\Eval.obj"
	-@erase "$(INTDIR)\File.obj"
	-@erase "$(INTDIR)\Fun.obj"
	-@erase "$(INTDIR)\Hash.obj"
	-@erase "$(INTDIR)\image.obj"
	-@erase "$(INTDIR)\List.obj"
	-@erase "$(INTDIR)\mzsj86.obj"
	-@erase "$(INTDIR)\network.obj"
	-@erase "$(INTDIR)\numarith.obj"
	-@erase "$(INTDIR)\Number.obj"
	-@erase "$(INTDIR)\numcomp.obj"
	-@erase "$(INTDIR)\numstr.obj"
	-@erase "$(INTDIR)\objclass.obj"
	-@erase "$(INTDIR)\Object.obj"
	-@erase "$(INTDIR)\Port.obj"
	-@erase "$(INTDIR)\portfun.obj"
	-@erase "$(INTDIR)\Print.obj"
	-@erase "$(INTDIR)\Process.obj"
	-@erase "$(INTDIR)\Promise.obj"
	-@erase "$(INTDIR)\Rational.obj"
	-@erase "$(INTDIR)\Read.obj"
	-@erase "$(INTDIR)\Regexp.obj"
	-@erase "$(INTDIR)\Salloc.obj"
	-@erase "$(INTDIR)\Sema.obj"
	-@erase "$(INTDIR)\Setjmpup.obj"
	-@erase "$(INTDIR)\String.obj"
	-@erase "$(INTDIR)\Struct.obj"
	-@erase "$(INTDIR)\Symbol.obj"
	-@erase "$(INTDIR)\Syntax.obj"
	-@erase "$(INTDIR)\Tsymbol.obj"
	-@erase "$(INTDIR)\Type.obj"
	-@erase "$(INTDIR)\Unit.obj"
	-@erase "$(INTDIR)\unitsig.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\Vector.obj"
	-@erase "$(OUTDIR)\mzsrc.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

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
BSC32_FLAGS=/nologo /o".\DebugOpt\mzsrc.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\mzsrc.lib" 
LIB32_OBJS= \
	"$(INTDIR)\Bignum.obj" \
	"$(INTDIR)\Bool.obj" \
	"$(INTDIR)\Char.obj" \
	"$(INTDIR)\Complex.obj" \
	"$(INTDIR)\Dynext.obj" \
	"$(INTDIR)\Env.obj" \
	"$(INTDIR)\Error.obj" \
	"$(INTDIR)\Eval.obj" \
	"$(INTDIR)\File.obj" \
	"$(INTDIR)\Fun.obj" \
	"$(INTDIR)\Hash.obj" \
	"$(INTDIR)\image.obj" \
	"$(INTDIR)\List.obj" \
	"$(INTDIR)\mzsj86.obj" \
	"$(INTDIR)\Number.obj" \
	"$(INTDIR)\Object.obj" \
	"$(INTDIR)\Port.obj" \
	"$(INTDIR)\Print.obj" \
	"$(INTDIR)\Process.obj" \
	"$(INTDIR)\Promise.obj" \
	"$(INTDIR)\Rational.obj" \
	"$(INTDIR)\Read.obj" \
	"$(INTDIR)\Regexp.obj" \
	"$(INTDIR)\Salloc.obj" \
	"$(INTDIR)\Sema.obj" \
	"$(INTDIR)\Setjmpup.obj" \
	"$(INTDIR)\String.obj" \
	"$(INTDIR)\Struct.obj" \
	"$(INTDIR)\Symbol.obj" \
	"$(INTDIR)\Syntax.obj" \
	"$(INTDIR)\Tsymbol.obj" \
	"$(INTDIR)\Type.obj" \
	"$(INTDIR)\Unit.obj" \
	"$(INTDIR)\Vector.obj" \
	"$(INTDIR)\unitsig.obj" \
	"$(INTDIR)\numarith.obj" \
	"$(INTDIR)\numcomp.obj" \
	"$(INTDIR)\numstr.obj" \
	"$(INTDIR)\objclass.obj" \
	"$(INTDIR)\portfun.obj" \
	"$(INTDIR)\builtin.obj" \
	"$(INTDIR)\network.obj"

"$(OUTDIR)\mzsrc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("mzsrc.dep")
!INCLUDE "mzsrc.dep"
!ELSE 
!MESSAGE Warning: cannot find "mzsrc.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "mzsrc - Win32 Release" || "$(CFG)" == "mzsrc - Win32 Debug" || "$(CFG)" == "mzsrc - Win32 SGC" || "$(CFG)" == "mzsrc - Win32 Threads" || "$(CFG)" == "mzsrc - Win32 MT DLL"
SOURCE=..\..\Mzscheme\Src\Bignum.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Bignum.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Bignum.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Bignum.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Bignum.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Bignum.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Bool.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Bool.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Bool.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Bool.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Bool.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Bool.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\mzscheme\src\builtin.c

"$(INTDIR)\builtin.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Char.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Char.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Char.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Char.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Char.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Char.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Complex.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Complex.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Complex.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Complex.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Complex.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Complex.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Dynext.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Dynext.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Dynext.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Dynext.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Dynext.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Dynext.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Env.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Env.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Env.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Env.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Env.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Env.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Error.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Error.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Error.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Error.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Error.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Error.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Eval.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Eval.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Eval.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Eval.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Eval.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Eval.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\File.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\File.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\File.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\File.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\File.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\File.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Fun.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Fun.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Fun.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Fun.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Fun.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Fun.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Hash.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Hash.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Hash.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Hash.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Hash.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Hash.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\MZSCHEME\SRC\image.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\image.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\image.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\image.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\image.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\image.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\List.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\List.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\List.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\List.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\List.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\List.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\mzsj86.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\mzsj86.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\mzsj86.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\mzsj86.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\mzsj86.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\mzsj86.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\mzscheme\src\network.c

"$(INTDIR)\network.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\src\numarith.c

"$(INTDIR)\numarith.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Number.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Number.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Number.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Number.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Number.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Number.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\mzscheme\src\numcomp.c

"$(INTDIR)\numcomp.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\src\numstr.c

"$(INTDIR)\numstr.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\src\objclass.c

"$(INTDIR)\objclass.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Object.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Object.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Object.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Object.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Object.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Object.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Port.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Port.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Port.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Port.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Port.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Port.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\mzscheme\src\portfun.c

"$(INTDIR)\portfun.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Print.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Print.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Print.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Print.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Print.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Print.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Process.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Process.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Process.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Process.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Process.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Process.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Promise.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Promise.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Promise.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Promise.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Promise.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Promise.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Rational.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Rational.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Rational.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Rational.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Rational.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Rational.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Read.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Read.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Read.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Read.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Read.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Read.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Regexp.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Regexp.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Regexp.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Regexp.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Regexp.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Regexp.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Salloc.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Salloc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Salloc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Salloc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Salloc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Salloc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Sema.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Sema.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Sema.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Sema.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Sema.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Sema.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Setjmpup.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Setjmpup.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Setjmpup.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Setjmpup.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Setjmpup.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Setjmpup.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\String.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\String.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\String.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\String.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\String.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\String.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Struct.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Struct.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Struct.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Struct.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Struct.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Struct.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Symbol.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Symbol.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Symbol.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Symbol.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Symbol.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Symbol.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Syntax.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Syntax.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Syntax.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Syntax.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Syntax.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Syntax.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Tsymbol.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Tsymbol.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Tsymbol.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Tsymbol.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Tsymbol.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Tsymbol.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Type.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Type.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Type.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Type.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Type.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Type.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Mzscheme\Src\Unit.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Unit.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Unit.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Unit.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Unit.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Unit.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\mzscheme\src\unitsig.c

"$(INTDIR)\unitsig.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Vector.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Vector.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Vector.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

CPP_SWITCHES=/nologo /MTd /W3 /GX /ZI /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Vector.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

CPP_SWITCHES=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Vector.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "mzsrc - Win32 MT DLL"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "NDEBUG" /D "USE_MSVC_MD_LIBRARY" /Fp"$(INTDIR)\mzsrc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /Zm1000 /c 

"$(INTDIR)\Vector.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 


!ENDIF 

