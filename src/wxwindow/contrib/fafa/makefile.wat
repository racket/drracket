
WXDIR = ..\.. 

!include $(WXDIR)\src\makewat.env

LIBTARGET = fafa.lib

OBJECTS = button.obj check.obj cont.obj dialog.obj draw.obj fafa.obj static.obj

all: $(LIBTARGET)

$(LIBTARGET):      $(OBJECTS)
        erase $(LIBTARGET)
        wlib /b /c /n /p=32 $(LIBTARGET) $(OBJECTS)

clean:   .SYMBOLIC
    -erase *.obj *.bak *.err *.pch *.lib *.lbc
