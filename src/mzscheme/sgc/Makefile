#
# Makefile for SenoraGC
#

# This must be an ANSI C compiler.
CC=gcc
CXX=gcc
AS=gcc -c -x assembler-with-cpp

# Debugging flags
DEBUGFLAGS = -g

# Other options; mainly used to import other options
OPTIONS =

# Optimization
OPT = -O 

# Warnings
WARN = -Wall 

OBJDIR = objects/sys$(MZPLATFORMSUFFIX)

CFLAGS_NODBG = $(OPT) $(WARN) $(OPTIONS) -I.
CFLAGS = $(DEBUGFLAGS) $(CFLAGS_NODBG)

OBJS =  $(OBJDIR)/sgc.o

SRCS =  sgc.c

wrong:
	# Make from the directory above this one

test: $(OBJS) $(OBJDIR)/test.o $(OBJDIR)/sgc.o
	$(CC) $(CFLAGS_NODBG) -o test $(OBJS) $(OBJDIR)/test.o

$(OBJDIR)/gc.a: objects $(OBJDIR) $(OBJS)
	$(MAKE) gcobjects
	$(AR) ruv $(OBJDIR)/gc.a $(OBJS)

gcobjects: $(OBJS)

c++: $(OBJDIR)/gc_cpp.o
	$(AR) ruv $(OBJDIR)/gc.a gc_cpp.o

$(OBJDIR):
	mkdir $(OBJDIR)
objects:
	mkdir objects

$(OBJDIR)/sgc.o: sgc.c
	$(CC) $(CFLAGS) -c sgc.c -o $(OBJDIR)/sgc.o

$(OBJDIR)/test.o: test.c
	$(CC) $(CFLAGS) -c test.c -o $(OBJDIR)/test.o

$(OBJDIR)/gc_cpp.o: gc_cpp.cc
	$(CXX) $(CFLAGS) -c gc_cpp.cc -o $(OBJDIR)/gc_cpp.o

depend:
	makedepend -- $(CFLAGS) -- $(SRCS)

clean:
	/bin/rm -f $(OBJS) objects/*/* test *~ Makefile.bak

# Note: 'make depend' is no longer required. As distributed, the below
#       dependencies have been trimmed to include only dependencies
#       on MzScheme files

# DO NOT DELETE THIS LINE -- make depend depends on it.

$(OBJDIR)/sgc.o: sgc.h

$(OBJDIR)/test.o: sgc.h
