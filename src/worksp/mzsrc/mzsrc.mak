# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

!IF "$(CFG)" == ""
CFG=mzsrc - Win32 Release
!MESSAGE No configuration specified.  Defaulting to mzsrc - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "mzsrc - Win32 Release" && "$(CFG)" != "mzsrc - Win32 Debug" &&\
 "$(CFG)" != "mzsrc - Win32 SGC" && "$(CFG)" != "mzsrc - Win32 Threads"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "mzsrc.mak" CFG="mzsrc - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mzsrc - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "mzsrc - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "mzsrc - Win32 SGC" (based on "Win32 (x86) Static Library")
!MESSAGE "mzsrc - Win32 Threads" (based on "Win32 (x86) Static Library")
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
# PROP Target_Last_Scanned "mzsrc - Win32 Debug"
CPP=cl.exe

!IF  "$(CFG)" == "mzsrc - Win32 Release"

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

ALL : "$(OUTDIR)\mzsrc.lib"

CLEAN : 
	-@erase "Release\vc40.pdb"
	-@erase "Release\mzsrc.lib"
	-@erase "Release\Unit.obj"
	-@erase "Release\Syntax.obj"
	-@erase "Release\Object.obj"
	-@erase "Release\Eval.obj"
	-@erase "Release\Regexp.obj"
	-@erase "Release\mzsj86.obj"
	-@erase "Release\Process.obj"
	-@erase "Release\Symbol.obj"
	-@erase "Release\Error.obj"
	-@erase "Release\Bool.obj"
	-@erase "Release\Env.obj"
	-@erase "Release\Complex.obj"
	-@erase "Release\Print.obj"
	-@erase "Release\Fun.obj"
	-@erase "Release\Type.obj"
	-@erase "Release\Hash.obj"
	-@erase "Release\File.obj"
	-@erase "Release\String.obj"
	-@erase "Release\Tsymbol.obj"
	-@erase "Release\Vector.obj"
	-@erase "Release\Bignum.obj"
	-@erase "Release\Struct.obj"
	-@erase "Release\Sema.obj"
	-@erase "Release\Char.obj"
	-@erase "Release\Number.obj"
	-@erase "Release\Promise.obj"
	-@erase "Release\Read.obj"
	-@erase "Release\Salloc.obj"
	-@erase "Release\Setjmpup.obj"
	-@erase "Release\Rational.obj"
	-@erase "Release\Port.obj"
	-@erase "Release\Dynext.obj"
	-@erase "Release\List.obj"
	-@erase "Release\image.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MT /W3 /GX /Zi /O2 /I "..\..\plt\mzscheme\include" /I "..\..\plt\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "MZWINCONSOLE" /YX /c
CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\plt\mzscheme\include" /I\
 "..\..\plt\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D\
 "MZWINCONSOLE" /Fp"$(INTDIR)/mzsrc.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=Release/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo /o"DebugOpt/mzsrc.bsc"
BSC32_FLAGS=/nologo /o"DebugOpt/mzsrc.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/mzsrc.lib" 
LIB32_OBJS= \
	"$(INTDIR)/Unit.obj" \
	"$(INTDIR)/Syntax.obj" \
	"$(INTDIR)/Object.obj" \
	"$(INTDIR)/Eval.obj" \
	"$(INTDIR)/Regexp.obj" \
	"$(INTDIR)/mzsj86.obj" \
	"$(INTDIR)/Process.obj" \
	"$(INTDIR)/Symbol.obj" \
	"$(INTDIR)/Error.obj" \
	"$(INTDIR)/Bool.obj" \
	"$(INTDIR)/Env.obj" \
	"$(INTDIR)/Complex.obj" \
	"$(INTDIR)/Print.obj" \
	"$(INTDIR)/Fun.obj" \
	"$(INTDIR)/Type.obj" \
	"$(INTDIR)/Hash.obj" \
	"$(INTDIR)/File.obj" \
	"$(INTDIR)/String.obj" \
	"$(INTDIR)/Tsymbol.obj" \
	"$(INTDIR)/Vector.obj" \
	"$(INTDIR)/Bignum.obj" \
	"$(INTDIR)/Struct.obj" \
	"$(INTDIR)/Sema.obj" \
	"$(INTDIR)/Char.obj" \
	"$(INTDIR)/Number.obj" \
	"$(INTDIR)/Promise.obj" \
	"$(INTDIR)/Read.obj" \
	"$(INTDIR)/Salloc.obj" \
	"$(INTDIR)/Setjmpup.obj" \
	"$(INTDIR)/Rational.obj" \
	"$(INTDIR)/Port.obj" \
	"$(INTDIR)/Dynext.obj" \
	"$(INTDIR)/List.obj" \
	"$(INTDIR)/image.obj"

"$(OUTDIR)\mzsrc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

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

ALL : "$(OUTDIR)\mzsrc.lib"

CLEAN : 
	-@erase "Debug\vc40.pdb"
	-@erase "Debug\mzsrc.lib"
	-@erase "Debug\Syntax.obj"
	-@erase "Debug\Object.obj"
	-@erase "Debug\Bool.obj"
	-@erase "Debug\Error.obj"
	-@erase "Debug\Type.obj"
	-@erase "Debug\Hash.obj"
	-@erase "Debug\File.obj"
	-@erase "Debug\Regexp.obj"
	-@erase "Debug\mzsj86.obj"
	-@erase "Debug\Setjmpup.obj"
	-@erase "Debug\Symbol.obj"
	-@erase "Debug\Rational.obj"
	-@erase "Debug\Dynext.obj"
	-@erase "Debug\Sema.obj"
	-@erase "Debug\Char.obj"
	-@erase "Debug\Fun.obj"
	-@erase "Debug\String.obj"
	-@erase "Debug\Read.obj"
	-@erase "Debug\Process.obj"
	-@erase "Debug\Vector.obj"
	-@erase "Debug\Print.obj"
	-@erase "Debug\Port.obj"
	-@erase "Debug\Complex.obj"
	-@erase "Debug\List.obj"
	-@erase "Debug\Unit.obj"
	-@erase "Debug\Bignum.obj"
	-@erase "Debug\Struct.obj"
	-@erase "Debug\Promise.obj"
	-@erase "Debug\Number.obj"
	-@erase "Debug\Eval.obj"
	-@erase "Debug\Tsymbol.obj"
	-@erase "Debug\Salloc.obj"
	-@erase "Debug\Env.obj"
	-@erase "Debug\image.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MTd /W3 /GX /Zi /Od /I "..\..\plt\mzscheme\include" /I "..\..\plt\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "MZWINCONSOLE" /YX /c
CPP_PROJ=/nologo /MTd /W3 /GX /Zi /Od /I "..\..\plt\mzscheme\include" /I\
 "..\..\plt\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D\
 "MZWINCONSOLE" /Fp"$(INTDIR)/mzsrc.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=Debug/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/mzsrc.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/mzsrc.lib" 
LIB32_OBJS= \
	"$(INTDIR)/Syntax.obj" \
	"$(INTDIR)/Object.obj" \
	"$(INTDIR)/Bool.obj" \
	"$(INTDIR)/Error.obj" \
	"$(INTDIR)/Type.obj" \
	"$(INTDIR)/Hash.obj" \
	"$(INTDIR)/File.obj" \
	"$(INTDIR)/Regexp.obj" \
	"$(INTDIR)/mzsj86.obj" \
	"$(INTDIR)/Setjmpup.obj" \
	"$(INTDIR)/Symbol.obj" \
	"$(INTDIR)/Rational.obj" \
	"$(INTDIR)/Dynext.obj" \
	"$(INTDIR)/Sema.obj" \
	"$(INTDIR)/Char.obj" \
	"$(INTDIR)/Fun.obj" \
	"$(INTDIR)/String.obj" \
	"$(INTDIR)/Read.obj" \
	"$(INTDIR)/Process.obj" \
	"$(INTDIR)/Vector.obj" \
	"$(INTDIR)/Print.obj" \
	"$(INTDIR)/Port.obj" \
	"$(INTDIR)/Complex.obj" \
	"$(INTDIR)/List.obj" \
	"$(INTDIR)/Unit.obj" \
	"$(INTDIR)/Bignum.obj" \
	"$(INTDIR)/Struct.obj" \
	"$(INTDIR)/Promise.obj" \
	"$(INTDIR)/Number.obj" \
	"$(INTDIR)/Eval.obj" \
	"$(INTDIR)/Tsymbol.obj" \
	"$(INTDIR)/Salloc.obj" \
	"$(INTDIR)/Env.obj" \
	"$(INTDIR)/image.obj"

"$(OUTDIR)\mzsrc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "mzsrc___"
# PROP BASE Intermediate_Dir "mzsrc___"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "SGC"
# PROP Intermediate_Dir "SGC"
# PROP Target_Dir ""
OUTDIR=SGC
INTDIR=SGC

ALL : "$(OUTDIR)\mzsrc.lib"

CLEAN : 
	-@erase "SGC\vc40.pdb"
	-@erase "SGC\mzsrc.lib"
	-@erase "SGC\Dynext.obj"
	-@erase "SGC\Error.obj"
	-@erase "SGC\Print.obj"
	-@erase "SGC\String.obj"
	-@erase "SGC\Vector.obj"
	-@erase "SGC\Process.obj"
	-@erase "SGC\Bignum.obj"
	-@erase "SGC\Struct.obj"
	-@erase "SGC\Complex.obj"
	-@erase "SGC\Port.obj"
	-@erase "SGC\Number.obj"
	-@erase "SGC\Unit.obj"
	-@erase "SGC\Type.obj"
	-@erase "SGC\Env.obj"
	-@erase "SGC\Salloc.obj"
	-@erase "SGC\Eval.obj"
	-@erase "SGC\Fun.obj"
	-@erase "SGC\Syntax.obj"
	-@erase "SGC\Object.obj"
	-@erase "SGC\Setjmpup.obj"
	-@erase "SGC\Sema.obj"
	-@erase "SGC\Rational.obj"
	-@erase "SGC\Char.obj"
	-@erase "SGC\Bool.obj"
	-@erase "SGC\Regexp.obj"
	-@erase "SGC\Promise.obj"
	-@erase "SGC\mzsj86.obj"
	-@erase "SGC\Read.obj"
	-@erase "SGC\Hash.obj"
	-@erase "SGC\File.obj"
	-@erase "SGC\Symbol.obj"
	-@erase "SGC\Tsymbol.obj"
	-@erase "SGC\List.obj"
	-@erase "SGC\image.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Zi /Od /I "c:\Matthew\plt\mzscheme\include" /I "C:\Matthew\plt\mzscheme\gc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "MZWINCONSOLE" /YX /c
# ADD CPP /nologo /MTd /W3 /GX /Zi /Od /I "..\..\plt\mzscheme\include" /I "..\..\plt\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "MZWINCONSOLE" /D SGC_STD_DEBUGGING=1 /YX /c
CPP_PROJ=/nologo /MTd /W3 /GX /Zi /Od /I "..\..\plt\mzscheme\include" /I\
 "..\..\plt\mzscheme\sgc" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D\
 "MZWINCONSOLE" /D SGC_STD_DEBUGGING=1 /Fp"$(INTDIR)/mzsrc.pch" /YX\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=SGC/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/mzsrc.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/mzsrc.lib" 
LIB32_OBJS= \
	"$(INTDIR)/Dynext.obj" \
	"$(INTDIR)/Error.obj" \
	"$(INTDIR)/Print.obj" \
	"$(INTDIR)/String.obj" \
	"$(INTDIR)/Vector.obj" \
	"$(INTDIR)/Process.obj" \
	"$(INTDIR)/Bignum.obj" \
	"$(INTDIR)/Struct.obj" \
	"$(INTDIR)/Complex.obj" \
	"$(INTDIR)/Port.obj" \
	"$(INTDIR)/Number.obj" \
	"$(INTDIR)/Unit.obj" \
	"$(INTDIR)/Type.obj" \
	"$(INTDIR)/Env.obj" \
	"$(INTDIR)/Salloc.obj" \
	"$(INTDIR)/Eval.obj" \
	"$(INTDIR)/Fun.obj" \
	"$(INTDIR)/Syntax.obj" \
	"$(INTDIR)/Object.obj" \
	"$(INTDIR)/Setjmpup.obj" \
	"$(INTDIR)/Sema.obj" \
	"$(INTDIR)/Rational.obj" \
	"$(INTDIR)/Char.obj" \
	"$(INTDIR)/Bool.obj" \
	"$(INTDIR)/Regexp.obj" \
	"$(INTDIR)/Promise.obj" \
	"$(INTDIR)/mzsj86.obj" \
	"$(INTDIR)/Read.obj" \
	"$(INTDIR)/Hash.obj" \
	"$(INTDIR)/File.obj" \
	"$(INTDIR)/Symbol.obj" \
	"$(INTDIR)/Tsymbol.obj" \
	"$(INTDIR)/List.obj" \
	"$(INTDIR)/image.obj"

"$(OUTDIR)\mzsrc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "mzsrc___"
# PROP BASE Intermediate_Dir "mzsrc___"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Threads"
# PROP Intermediate_Dir "Threads"
# PROP Target_Dir ""
OUTDIR=Threads
INTDIR=Threads

ALL : "$(OUTDIR)\mzsrc.lib"

CLEAN : 
	-@erase "Threads\vc40.pdb"
	-@erase "Threads\mzsrc.lib"
	-@erase "Threads\Regexp.obj"
	-@erase "Threads\Sema.obj"
	-@erase "Threads\Char.obj"
	-@erase "Threads\mzsj86.obj"
	-@erase "Threads\Setjmpup.obj"
	-@erase "Threads\Rational.obj"
	-@erase "Threads\Symbol.obj"
	-@erase "Threads\Read.obj"
	-@erase "Threads\File.obj"
	-@erase "Threads\Dynext.obj"
	-@erase "Threads\Port.obj"
	-@erase "Threads\List.obj"
	-@erase "Threads\Unit.obj"
	-@erase "Threads\String.obj"
	-@erase "Threads\Env.obj"
	-@erase "Threads\Vector.obj"
	-@erase "Threads\Fun.obj"
	-@erase "Threads\Bignum.obj"
	-@erase "Threads\Struct.obj"
	-@erase "Threads\Error.obj"
	-@erase "Threads\Print.obj"
	-@erase "Threads\Number.obj"
	-@erase "Threads\Bool.obj"
	-@erase "Threads\Salloc.obj"
	-@erase "Threads\Process.obj"
	-@erase "Threads\Type.obj"
	-@erase "Threads\Hash.obj"
	-@erase "Threads\Complex.obj"
	-@erase "Threads\Syntax.obj"
	-@erase "Threads\Object.obj"
	-@erase "Threads\Promise.obj"
	-@erase "Threads\Eval.obj"
	-@erase "Threads\Tsymbol.obj"
	-@erase "Threads\image.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Zi /O2 /I "..\..\plt\mzscheme\include" /I "..\..\plt\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__STDC__" /D "MZWINCONSOLE" /YX /c
# ADD CPP /nologo /MT /W3 /GX /Zi /O2 /I "..\..\plt\mzscheme\include" /I "..\..\plt\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS" /D "__STDC__" /D "MZWINCONSOLE" /YX /c
CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\plt\mzscheme\include" /I\
 "..\..\plt\mzscheme\gc" /D "NDEBUG" /D "WIN32" /D "WIN32_THREADS" /D "_WINDOWS"\
 /D "__STDC__" /D "MZWINCONSOLE" /Fp"$(INTDIR)/mzsrc.pch" /YX /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c 
CPP_OBJS=Threads/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo /o"DebugOpt/mzsrc.bsc"
# ADD BSC32 /nologo /o"DebugOpt/mzsrc.bsc"
BSC32_FLAGS=/nologo /o"DebugOpt/mzsrc.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/mzsrc.lib" 
LIB32_OBJS= \
	"$(INTDIR)/Regexp.obj" \
	"$(INTDIR)/Sema.obj" \
	"$(INTDIR)/Char.obj" \
	"$(INTDIR)/mzsj86.obj" \
	"$(INTDIR)/Setjmpup.obj" \
	"$(INTDIR)/Rational.obj" \
	"$(INTDIR)/Symbol.obj" \
	"$(INTDIR)/Read.obj" \
	"$(INTDIR)/File.obj" \
	"$(INTDIR)/Dynext.obj" \
	"$(INTDIR)/Port.obj" \
	"$(INTDIR)/List.obj" \
	"$(INTDIR)/Unit.obj" \
	"$(INTDIR)/String.obj" \
	"$(INTDIR)/Env.obj" \
	"$(INTDIR)/Vector.obj" \
	"$(INTDIR)/Fun.obj" \
	"$(INTDIR)/Bignum.obj" \
	"$(INTDIR)/Struct.obj" \
	"$(INTDIR)/Error.obj" \
	"$(INTDIR)/Print.obj" \
	"$(INTDIR)/Number.obj" \
	"$(INTDIR)/Bool.obj" \
	"$(INTDIR)/Salloc.obj" \
	"$(INTDIR)/Process.obj" \
	"$(INTDIR)/Type.obj" \
	"$(INTDIR)/Hash.obj" \
	"$(INTDIR)/Complex.obj" \
	"$(INTDIR)/Syntax.obj" \
	"$(INTDIR)/Object.obj" \
	"$(INTDIR)/Promise.obj" \
	"$(INTDIR)/Eval.obj" \
	"$(INTDIR)/Tsymbol.obj" \
	"$(INTDIR)/image.obj"

"$(OUTDIR)\mzsrc.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
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

# Name "mzsrc - Win32 Release"
# Name "mzsrc - Win32 Debug"
# Name "mzsrc - Win32 SGC"
# Name "mzsrc - Win32 Threads"

!IF  "$(CFG)" == "mzsrc - Win32 Release"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Vector.c
DEP_CPP_VECTO=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_VECTO=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Vector.obj" : $(SOURCE) $(DEP_CPP_VECTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Vector.obj" : $(SOURCE) $(DEP_CPP_VECTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Vector.obj" : $(SOURCE) $(DEP_CPP_VECTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Vector.obj" : $(SOURCE) $(DEP_CPP_VECTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Bool.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

DEP_CPP_BOOL_=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schmach.h"\
	".\..\..\mzscheme\gc\malloc.c"\
	".\..\..\Mzscheme\Src\mzstkchk.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	".\..\..\mzscheme\gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\gc\config.h"\
	".\..\..\mzscheme\gc\gc_hdrs.h"\
	
NODEP_CPP_BOOL_=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\gc\th\PCR_Th.h"\
	".\..\..\mzscheme\gc\th\PCR_ThCrSec.h"\
	".\..\..\mzscheme\gc\th\PCR_ThCtl.h"\
	

"$(INTDIR)\Bool.obj" : $(SOURCE) $(DEP_CPP_BOOL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

DEP_CPP_BOOL_=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schmach.h"\
	".\..\..\mzscheme\gc\malloc.c"\
	".\..\..\Mzscheme\Src\mzstkchk.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	".\..\..\mzscheme\gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\gc\config.h"\
	".\..\..\mzscheme\gc\gc_hdrs.h"\
	
NODEP_CPP_BOOL_=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\gc\th\PCR_Th.h"\
	".\..\..\mzscheme\gc\th\PCR_ThCrSec.h"\
	".\..\..\mzscheme\gc\th\PCR_ThCtl.h"\
	

"$(INTDIR)\Bool.obj" : $(SOURCE) $(DEP_CPP_BOOL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

DEP_CPP_BOOL_=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schmach.h"\
	".\..\..\Mzscheme\Src\mzstkchk.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_BOOL_=\
	".\..\..\mzscheme\include\sconfig.h"\
	

"$(INTDIR)\Bool.obj" : $(SOURCE) $(DEP_CPP_BOOL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

DEP_CPP_BOOL_=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schmach.h"\
	".\..\..\mzscheme\gc\malloc.c"\
	".\..\..\Mzscheme\Src\mzstkchk.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	".\..\..\mzscheme\gc\gc_priv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\gc\config.h"\
	".\..\..\mzscheme\gc\gc_hdrs.h"\
	
NODEP_CPP_BOOL_=\
	".\..\..\mzscheme\include\sconfig.h"\
	".\..\..\mzscheme\gc\th\PCR_Th.h"\
	".\..\..\mzscheme\gc\th\PCR_ThCrSec.h"\
	".\..\..\mzscheme\gc\th\PCR_ThCtl.h"\
	

"$(INTDIR)\Bool.obj" : $(SOURCE) $(DEP_CPP_BOOL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Char.c
DEP_CPP_CHAR_=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_CHAR_=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Char.obj" : $(SOURCE) $(DEP_CPP_CHAR_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Char.obj" : $(SOURCE) $(DEP_CPP_CHAR_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Char.obj" : $(SOURCE) $(DEP_CPP_CHAR_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Char.obj" : $(SOURCE) $(DEP_CPP_CHAR_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Complex.c
DEP_CPP_COMPL=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_COMPL=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Complex.obj" : $(SOURCE) $(DEP_CPP_COMPL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Complex.obj" : $(SOURCE) $(DEP_CPP_COMPL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Complex.obj" : $(SOURCE) $(DEP_CPP_COMPL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Complex.obj" : $(SOURCE) $(DEP_CPP_COMPL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Dynext.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

DEP_CPP_DYNEX=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schvers.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\Mzscheme\aixdlfcn\dlfcn.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemex.inc"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_DYNEX=\
	".\..\..\Mzscheme\Src\sgc.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	

"$(INTDIR)\Dynext.obj" : $(SOURCE) $(DEP_CPP_DYNEX) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

DEP_CPP_DYNEX=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schvers.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\Mzscheme\aixdlfcn\dlfcn.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemex.inc"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_DYNEX=\
	".\..\..\Mzscheme\Src\sgc.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	

"$(INTDIR)\Dynext.obj" : $(SOURCE) $(DEP_CPP_DYNEX) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

DEP_CPP_DYNEX=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schvers.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\mzscheme\sgc\gc.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\Mzscheme\aixdlfcn\dlfcn.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemex.inc"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_DYNEX=\
	".\..\..\mzscheme\include\sconfig.h"\
	

"$(INTDIR)\Dynext.obj" : $(SOURCE) $(DEP_CPP_DYNEX) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

DEP_CPP_DYNEX=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schvers.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\Mzscheme\aixdlfcn\dlfcn.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemex.inc"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_DYNEX=\
	".\..\..\Mzscheme\Src\sgc.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	

"$(INTDIR)\Dynext.obj" : $(SOURCE) $(DEP_CPP_DYNEX) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Env.c
DEP_CPP_ENV_C=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schminc.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\Mzscheme\Src\cmacro.inc"\
	".\..\..\Mzscheme\Src\macro.inc"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_ENV_C=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Env.obj" : $(SOURCE) $(DEP_CPP_ENV_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Env.obj" : $(SOURCE) $(DEP_CPP_ENV_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Env.obj" : $(SOURCE) $(DEP_CPP_ENV_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Env.obj" : $(SOURCE) $(DEP_CPP_ENV_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Error.c
DEP_CPP_ERROR=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_ERROR=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Error.obj" : $(SOURCE) $(DEP_CPP_ERROR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Error.obj" : $(SOURCE) $(DEP_CPP_ERROR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Error.obj" : $(SOURCE) $(DEP_CPP_ERROR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Error.obj" : $(SOURCE) $(DEP_CPP_ERROR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Eval.c
DEP_CPP_EVAL_=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schrunst.h"\
	".\..\..\Mzscheme\Src\schmach.h"\
	".\..\..\Mzscheme\Src\mzstkchk.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_EVAL_=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Eval.obj" : $(SOURCE) $(DEP_CPP_EVAL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Eval.obj" : $(SOURCE) $(DEP_CPP_EVAL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Eval.obj" : $(SOURCE) $(DEP_CPP_EVAL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Eval.obj" : $(SOURCE) $(DEP_CPP_EVAL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\File.c
DEP_CPP_FILE_=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_FILE_=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\File.obj" : $(SOURCE) $(DEP_CPP_FILE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\File.obj" : $(SOURCE) $(DEP_CPP_FILE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\File.obj" : $(SOURCE) $(DEP_CPP_FILE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\File.obj" : $(SOURCE) $(DEP_CPP_FILE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Fun.c
DEP_CPP_FUN_C=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_FUN_C=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Fun.obj" : $(SOURCE) $(DEP_CPP_FUN_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Fun.obj" : $(SOURCE) $(DEP_CPP_FUN_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Fun.obj" : $(SOURCE) $(DEP_CPP_FUN_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Fun.obj" : $(SOURCE) $(DEP_CPP_FUN_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Hash.c
DEP_CPP_HASH_=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_HASH_=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Hash.obj" : $(SOURCE) $(DEP_CPP_HASH_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Hash.obj" : $(SOURCE) $(DEP_CPP_HASH_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Hash.obj" : $(SOURCE) $(DEP_CPP_HASH_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Hash.obj" : $(SOURCE) $(DEP_CPP_HASH_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\List.c
DEP_CPP_LIST_=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_LIST_=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\List.obj" : $(SOURCE) $(DEP_CPP_LIST_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\List.obj" : $(SOURCE) $(DEP_CPP_LIST_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\List.obj" : $(SOURCE) $(DEP_CPP_LIST_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\List.obj" : $(SOURCE) $(DEP_CPP_LIST_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Number.c
DEP_CPP_NUMBE=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\nummacs.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_NUMBE=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Number.obj" : $(SOURCE) $(DEP_CPP_NUMBE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Number.obj" : $(SOURCE) $(DEP_CPP_NUMBE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Number.obj" : $(SOURCE) $(DEP_CPP_NUMBE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Number.obj" : $(SOURCE) $(DEP_CPP_NUMBE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Object.c
DEP_CPP_OBJEC=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schrunst.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_OBJEC=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Object.obj" : $(SOURCE) $(DEP_CPP_OBJEC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Object.obj" : $(SOURCE) $(DEP_CPP_OBJEC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Object.obj" : $(SOURCE) $(DEP_CPP_OBJEC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Object.obj" : $(SOURCE) $(DEP_CPP_OBJEC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Port.c
DEP_CPP_PORT_=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Src\schfd.h"\
	".\..\..\Mzscheme\Src\schwinfd.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_PORT_=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Port.obj" : $(SOURCE) $(DEP_CPP_PORT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Port.obj" : $(SOURCE) $(DEP_CPP_PORT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Port.obj" : $(SOURCE) $(DEP_CPP_PORT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Port.obj" : $(SOURCE) $(DEP_CPP_PORT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Print.c
DEP_CPP_PRINT=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schmach.h"\
	".\..\..\Mzscheme\Src\schcpt.h"\
	".\..\..\Mzscheme\Src\mzstkchk.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_PRINT=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Print.obj" : $(SOURCE) $(DEP_CPP_PRINT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Print.obj" : $(SOURCE) $(DEP_CPP_PRINT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Print.obj" : $(SOURCE) $(DEP_CPP_PRINT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Print.obj" : $(SOURCE) $(DEP_CPP_PRINT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Process.c
DEP_CPP_PROCE=\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\Mzscheme\Src\schpriv.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\Mzscheme\Src\schfd.h"\
	".\..\..\mzscheme\uconfig.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	
NODEP_CPP_PROCE=\
	".\..\..\Mzscheme\Src\sconfig.h"\
	".\..\..\Mzscheme\gc\semaphores.h"\
	".\..\..\Mzscheme\gc\sproc.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Process.obj" : $(SOURCE) $(DEP_CPP_PROCE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Process.obj" : $(SOURCE) $(DEP_CPP_PROCE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Process.obj" : $(SOURCE) $(DEP_CPP_PROCE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Process.obj" : $(SOURCE) $(DEP_CPP_PROCE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Promise.c
DEP_CPP_PROMI=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_PROMI=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Promise.obj" : $(SOURCE) $(DEP_CPP_PROMI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Promise.obj" : $(SOURCE) $(DEP_CPP_PROMI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Promise.obj" : $(SOURCE) $(DEP_CPP_PROMI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Promise.obj" : $(SOURCE) $(DEP_CPP_PROMI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Rational.c
DEP_CPP_RATIO=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_RATIO=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Rational.obj" : $(SOURCE) $(DEP_CPP_RATIO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Rational.obj" : $(SOURCE) $(DEP_CPP_RATIO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Rational.obj" : $(SOURCE) $(DEP_CPP_RATIO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Rational.obj" : $(SOURCE) $(DEP_CPP_RATIO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Read.c
DEP_CPP_READ_=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schmach.h"\
	".\..\..\Mzscheme\Src\schcpt.h"\
	".\..\..\Mzscheme\Src\mzstkchk.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_READ_=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Read.obj" : $(SOURCE) $(DEP_CPP_READ_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Read.obj" : $(SOURCE) $(DEP_CPP_READ_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Read.obj" : $(SOURCE) $(DEP_CPP_READ_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Read.obj" : $(SOURCE) $(DEP_CPP_READ_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Regexp.c
DEP_CPP_REGEX=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_REGEX=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Regexp.obj" : $(SOURCE) $(DEP_CPP_REGEX) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Regexp.obj" : $(SOURCE) $(DEP_CPP_REGEX) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Regexp.obj" : $(SOURCE) $(DEP_CPP_REGEX) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Regexp.obj" : $(SOURCE) $(DEP_CPP_REGEX) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Salloc.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

DEP_CPP_SALLO=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_SALLO=\
	".\..\..\Mzscheme\Src\sgc.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	

"$(INTDIR)\Salloc.obj" : $(SOURCE) $(DEP_CPP_SALLO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

DEP_CPP_SALLO=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_SALLO=\
	".\..\..\Mzscheme\Src\sgc.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	

"$(INTDIR)\Salloc.obj" : $(SOURCE) $(DEP_CPP_SALLO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

DEP_CPP_SALLO=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\mzscheme\sgc\gc.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_SALLO=\
	".\..\..\mzscheme\include\sconfig.h"\
	

"$(INTDIR)\Salloc.obj" : $(SOURCE) $(DEP_CPP_SALLO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

DEP_CPP_SALLO=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_SALLO=\
	".\..\..\Mzscheme\Src\sgc.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	

"$(INTDIR)\Salloc.obj" : $(SOURCE) $(DEP_CPP_SALLO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Sema.c
DEP_CPP_SEMA_=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_SEMA_=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Sema.obj" : $(SOURCE) $(DEP_CPP_SEMA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Sema.obj" : $(SOURCE) $(DEP_CPP_SEMA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Sema.obj" : $(SOURCE) $(DEP_CPP_SEMA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Sema.obj" : $(SOURCE) $(DEP_CPP_SEMA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Setjmpup.c
DEP_CPP_SETJM=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schmach.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_SETJM=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Setjmpup.obj" : $(SOURCE) $(DEP_CPP_SETJM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Setjmpup.obj" : $(SOURCE) $(DEP_CPP_SETJM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Setjmpup.obj" : $(SOURCE) $(DEP_CPP_SETJM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Setjmpup.obj" : $(SOURCE) $(DEP_CPP_SETJM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\String.c
DEP_CPP_STRIN=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schvers.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_STRIN=\
	".\..\..\Mzscheme\Src\schsys.h"\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\String.obj" : $(SOURCE) $(DEP_CPP_STRIN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\String.obj" : $(SOURCE) $(DEP_CPP_STRIN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\String.obj" : $(SOURCE) $(DEP_CPP_STRIN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\String.obj" : $(SOURCE) $(DEP_CPP_STRIN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Struct.c
DEP_CPP_STRUC=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_STRUC=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Struct.obj" : $(SOURCE) $(DEP_CPP_STRUC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Struct.obj" : $(SOURCE) $(DEP_CPP_STRUC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Struct.obj" : $(SOURCE) $(DEP_CPP_STRUC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Struct.obj" : $(SOURCE) $(DEP_CPP_STRUC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Symbol.c
DEP_CPP_SYMBO=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_SYMBO=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Symbol.obj" : $(SOURCE) $(DEP_CPP_SYMBO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Symbol.obj" : $(SOURCE) $(DEP_CPP_SYMBO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Symbol.obj" : $(SOURCE) $(DEP_CPP_SYMBO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Symbol.obj" : $(SOURCE) $(DEP_CPP_SYMBO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Syntax.c
DEP_CPP_SYNTA=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_SYNTA=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Syntax.obj" : $(SOURCE) $(DEP_CPP_SYNTA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Syntax.obj" : $(SOURCE) $(DEP_CPP_SYNTA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Syntax.obj" : $(SOURCE) $(DEP_CPP_SYNTA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Syntax.obj" : $(SOURCE) $(DEP_CPP_SYNTA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Tsymbol.c
DEP_CPP_TSYMB=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_TSYMB=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Tsymbol.obj" : $(SOURCE) $(DEP_CPP_TSYMB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Tsymbol.obj" : $(SOURCE) $(DEP_CPP_TSYMB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Tsymbol.obj" : $(SOURCE) $(DEP_CPP_TSYMB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Tsymbol.obj" : $(SOURCE) $(DEP_CPP_TSYMB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Type.c
DEP_CPP_TYPE_=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_TYPE_=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Type.obj" : $(SOURCE) $(DEP_CPP_TYPE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Type.obj" : $(SOURCE) $(DEP_CPP_TYPE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Type.obj" : $(SOURCE) $(DEP_CPP_TYPE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Type.obj" : $(SOURCE) $(DEP_CPP_TYPE_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Unit.c
DEP_CPP_UNIT_=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schrunst.h"\
	".\..\..\Mzscheme\Src\schminc.h"\
	".\..\..\Mzscheme\Src\cunitsig.inc"\
	".\..\..\Mzscheme\Src\unitsig.inc"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_UNIT_=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Unit.obj" : $(SOURCE) $(DEP_CPP_UNIT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Unit.obj" : $(SOURCE) $(DEP_CPP_UNIT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Unit.obj" : $(SOURCE) $(DEP_CPP_UNIT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Unit.obj" : $(SOURCE) $(DEP_CPP_UNIT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Bignum.c
DEP_CPP_BIGNU=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_BIGNU=\
	".\..\..\mzscheme\include\sconfig.h"\
	

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\Bignum.obj" : $(SOURCE) $(DEP_CPP_BIGNU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\Bignum.obj" : $(SOURCE) $(DEP_CPP_BIGNU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\Bignum.obj" : $(SOURCE) $(DEP_CPP_BIGNU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\Bignum.obj" : $(SOURCE) $(DEP_CPP_BIGNU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Mzscheme\Src\mzsj86.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"


"$(INTDIR)\mzsj86.obj" : $(SOURCE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"


"$(INTDIR)\mzsj86.obj" : $(SOURCE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"


"$(INTDIR)\mzsj86.obj" : $(SOURCE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"


"$(INTDIR)\mzsj86.obj" : $(SOURCE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\MZSCHEME\SRC\image.c

!IF  "$(CFG)" == "mzsrc - Win32 Release"

DEP_CPP_IMAGE=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schmach.h"\
	".\..\..\Mzscheme\Src\schvers.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\gc\config.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_IMAGE=\
	".\..\..\mzscheme\include\sconfig.h"\
	

"$(INTDIR)\image.obj" : $(SOURCE) $(DEP_CPP_IMAGE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Debug"

DEP_CPP_IMAGE=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schmach.h"\
	".\..\..\Mzscheme\Src\schvers.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\gc\config.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_IMAGE=\
	".\..\..\mzscheme\include\sconfig.h"\
	

"$(INTDIR)\image.obj" : $(SOURCE) $(DEP_CPP_IMAGE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 SGC"

DEP_CPP_IMAGE=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schmach.h"\
	".\..\..\Mzscheme\Src\schvers.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\mzscheme\sgc\gc.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\gc\config.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	
NODEP_CPP_IMAGE=\
	".\..\..\mzscheme\include\sconfig.h"\
	

"$(INTDIR)\image.obj" : $(SOURCE) $(DEP_CPP_IMAGE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "mzsrc - Win32 Threads"

DEP_CPP_IMAGE=\
	".\..\..\Mzscheme\Src\schpriv.h"\
	".\..\..\Mzscheme\Src\schmach.h"\
	".\..\..\Mzscheme\Src\schvers.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\mzscheme\gc\config.h"\
	".\..\..\mzscheme\include\scheme.h"\
	".\..\..\mzscheme\sconfig.h"\
	".\..\..\Mzscheme\Src\stypes.h"\
	".\..\..\Mzscheme\Src\schexn.h"\
	".\..\..\Mzscheme\Src\schemef.h"\
	".\..\..\Mzscheme\Src\schemex.h"\
	".\..\..\Mzscheme\Src\schemexm.h"\
	".\..\..\mzscheme\uconfig.h"\
	
NODEP_CPP_IMAGE=\
	".\..\..\mzscheme\include\sconfig.h"\
	

"$(INTDIR)\image.obj" : $(SOURCE) $(DEP_CPP_IMAGE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
