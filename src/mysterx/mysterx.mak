# mysterx.mak

all : mxmain.dll

clean :
        -@erase comtypes.obj
        -@erase htmlevent.obj
        -@erase htmlutil.obj
        -@erase array.obj
        -@erase browser.obj
        -@erase mysterx.obj
	-@erase mxmain.dll

CPP=cl.exe
CPP_FLAGS=/I"../../collects/mzscheme/include" /I"./myspage" /I"./mysc" /I"./myssink" /I"$(SHELL32)\Include" \
	/I"$(HTMLHELP)\include" /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /D"NEWMSHTML=$(NEWMSHTML)" /c 

.cxx.obj::
   $(CPP) $(CPP_FLAGS) $< 

LINK32=$(MZC)
LINK32_LIBS= \
	kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib "$(SHELL32)\LIB\shell32.lib" ole32.lib oleaut32.lib \
	uuid.lib "$(HTMLHELP)\lib\htmlhelp.lib" \
	mysc\mysc.lib 

LINK32_OBJS= \
        mysterx.obj array.obj comtypes.obj htmlevent.obj htmlutil.obj \
        browser.obj

mxmain.dll : $(DEF_FILE) $(LINK32_OBJS) mysc\mysc.lib
	$(LINK32) --ld mxmain.dll $(LINK32_OBJS) $(LINK32_LIBS)
	copy mxmain.dll ..\..\collects\mysterx\dlls
	
comtypes.obj : comtypes.cxx mysterx.h stdafx.h

htmlevent.obj : htmlevent.cxx mysterx.h stdafx.h

htmlutil.obj : htmlutil.cxx mysterx.h stdafx.h

array.obj : array.cxx mysterx.h stdafx.h

browser.obj : browser.cxx mysterx.h stdafx.h

mysterx.obj : mysterx.cxx mysterx.h stdafx.h


