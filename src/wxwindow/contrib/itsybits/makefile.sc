# Makefile : Builds ITSYBITS library
# for Symantec C++

# Set WXDIR for your system
WXDIR = $(WX)
WXLIB = $(WXDIR)\lib\wx.lib
ITSYDIR = $(WXDIR)\contrib\itsybits
ITSYLIB = $(FAFADIR)\itsy.lib
INCDIR = $(WXDIR)\include
MSWINC = $(INCDIR)\msw
BASEINC = $(INCDIR)\base

CC=sc
CFLAGS = -o -ml -W -Dwx_msw

INCLUDE=$(BASEINC);$(MSWINC);$(WXDIR)\contrib\itsybits


OBJS = itsybits.obj

.c.obj:
	*$(CC) -c $(CFLAGS) -I$(INCLUDE) $<

all: $(ITSYLIB)

$(ITSYLIB): $(OBJS)
        -del $(ISTYLIB)
	*lib $(ITSYLIB) y $(OBJS), nul;

itsybits.obj: itsybits.h itsybits.c

clean:
        -del *.obj
	-del $(ITSYLIB)
