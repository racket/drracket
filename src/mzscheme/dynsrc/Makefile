#
# Makefile for mzscheme
#

# dynlink is a script that uses the right set of link commands
DYNLINK = ./dynlink

CFLAGS_NODBG = $(OPT) $(WARN) $(OPTIONS) $(DYNOPTIONS) -I../include
CFLAGS = $(CFLAGS_NODBG) $(DEBUGFLAGS)

SRCS = mzdyn.c dynexmpl.c oe.c

LIBDIR = lib/sys$(MZPLATFORMSUFFIX)
OBJDIR = objects/sys$(MZPLATFORMSUFFIX)

DIRDEPS = objects $(OBJDIR)

dynlib: $(DIRDEPS)
	$(MAKE) ../$(LIBDIR)/mzdyn.o

dynexample: $(DIRDEPS)
	$(MAKE) ../$(OBJDIR)/dynexmpl.so

oe: $(DIRDEPS)
	$(MAKE) ../$(OBJDIR)/oe.so

$(OBJDIR):
	mkdir $(OBJDIR)

objects:
	mkdir objects

HEADERS = ../include/scheme.h ../src/schemef.h ../sconfig.h ../uconfig.h

MZDYNDEP = ../$(LIBDIR)/mzdyn.o ../include/ext.exp ../include/mzscheme.exp

../$(LIBDIR)/mzdyn.o: $(OBJDIR)/mzdyn.o
	cp $(OBJDIR)/mzdyn.o ../$(LIBDIR)/mzdyn.o
$(OBJDIR)/mzdyn.o: mzdyn.c ../src/schvers.h  $(HEADERS)
	$(CC) $(CFLAGS) -c mzdyn.c -o $(OBJDIR)/mzdyn.o

../$(OBJDIR)/dynexmpl.so: $(OBJDIR)/dynexmpl.o $(MZDYNDEP)
	$(DYNLINK) $(OBJDIR)/dynexmpl.o -o ../$(OBJDIR)/dynexmpl.so
$(OBJDIR)/dynexmpl.o: dynexmpl.c  $(HEADERS)
	$(CC) $(CFLAGS) -c dynexmpl.c -o $(OBJDIR)/dynexmpl.o

../$(OBJDIR)/oe.so: $(OBJDIR)/oe.o  $(MZDYNDEP)
	$(DYNLINK) $(OBJDIR)/oe.o -o ../$(OBJDIR)/oe.so
$(OBJDIR)/oe.o: oe.c $(HEADERS)
	$(CC) $(CFLAGS) -c oe.c -o $(OBJDIR)/oe.o

depend:
	makedepend -- $(CFLAGS) -- $(SRCS)

clean:
	/bin/rm -f objects/*/*.o *~ Makefile.bak

exile:
	$(MAKE) clean

# Note: 'make depend' is no longer required. As distributed, the below
#       dependencies have been trimmed to include only dependencies
#       on MzScheme files

# DO NOT DELETE THIS LINE -- make depend depends on it.

mzdyn.o: ../include/escheme.h ../include/scheme.h ../src/stypes.h \
         ../src/schemex.h ../src/schemexm.h ../src/schvers.h

dynexmpl.o: ../include/escheme.h ../include/scheme.h ../src/schemexm.h 

oe.o: ../include/escheme.h ../include/scheme.h ../src/stypes.h \
      ../src/schemex.h ../src/schemexm.h
