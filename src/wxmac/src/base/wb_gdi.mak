#
# Borland C++ IDE generated makefile
#
.AUTODEPEND


#
# Borland C++ tools
#
IMPLIB  = Implib
BCC32   = Bcc32 +BccW32.cfg 
TLINK32 = TLink32
TLIB    = TLib
BRC32   = Brc32
TASM32  = Tasm32
#
# IDE macros
#


#
# Options
#
IDE_LFLAGS32 =  -LC:\BC4\LIB
IDE_RFLAGS32 =  -IC:\BC4\INCLUDE
LLATW32_wb_gdidexe =  -Tpe -aa
RLATW32_wb_gdidexe =  -ID:\BC4\INCLUDE;C:\WXWIN\INCLUDE\BASE;C:\WXWIN\INCLUDE\MSW -w32
BLATW32_wb_gdidexe = 
LEAT_wb_gdidexe = $(LLATW32_wb_gdidexe)
REAT_wb_gdidexe = $(RLATW32_wb_gdidexe)
BEAT_wb_gdidexe = $(BLATW32_wb_gdidexe)

#
# Dependency List
#
Dep_wb_gdi = \
   wb_gdi.exe

wb_gdi : BccW32.cfg $(Dep_wb_gdi)
  echo MakeNode wb_gdi

Dep_wb_gdidexe = \
   wb_gdi.obj

wb_gdi.exe : $(Dep_wb_gdidexe)
  $(TLINK32) @&&|
 /v $(IDE_LFLAGS32) $(LEAT_wb_gdidexe) +
/v- wb_gdi.obj
$<,$*


|

wb_gdi.obj :  wb_gdi.cc
  $(BCC32) -c $(CEAT_wb_gdidexe) -o$@ wb_gdi.cc

# Compiler configuration file
BccW32.cfg : 
   Copy &&|
-R
-v
-vi
-X-
-H
-IC:\BC4\INCLUDE
-H=wb_gdi.csm
-Dwx_msw;__win32s__;farmalloc=malloc;farfree=free;
-ID:\BC4\INCLUDE;C:\WXWIN\INCLUDE\BASE;C:\WXWIN\INCLUDE\MSW
-W
| $@


