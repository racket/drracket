# Makefile : Builds FAFA library
# for Symantec C++

# Set WXDIR for your system
WXDIR = $(WX)
WXLIB = $(WXDIR)\lib\wx.lib
FAFADIR = $(WXDIR)\contrib\fafa
FAFALIB = $(FAFADIR)\fafa.lib
INCDIR = $(WXDIR)\include
MSWINC = $(INCDIR)\msw
BASEINC = $(INCDIR)\base

CC=sc
CFLAGS = -o -ml -W -Dwx_msw

INCLUDE=$(BASEINC);$(MSWINC);$(WXDIR)\contrib\fafa


OBJS = button.obj check.obj cont.obj dialog.obj draw.obj fafa.obj \
static.obj

.c.obj:
	*$(CC) -c $(CFLAGS) -I$(INCLUDE) $<

all: $(FAFALIB)

$(FAFALIB): $(OBJS)
        -del $(FAFALIB)
	*lib $(FAFALIB) y $(OBJS), nul;

button.obj: fafapriv.h button.c

check.obj: fafapriv.h check.c

cont.obj: fafapriv.h cont.c

dialog.obj: fafapriv.h dialog.c

draw.obj: fafapriv.h draw.c

fafa.obj: fafapriv.h fafa.c

static.obj: fafapriv.h static.c

clean:
        -del *.obj
	-del $(FAFALIB)
