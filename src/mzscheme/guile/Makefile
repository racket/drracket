
CC = gcc
RANLIB=ranlib

CFLAGS_NODBG = $(OPT) $(WARN) $(OPTIONS) -I../include
CFLAGS = $(DEBUGFLAGS) $(CFLAGS_NODBG)

LIBDIR = lib/sys$(MZPLATFORMSUFFIX)

OBJDIR = objects/sys$(MZPLATFORMSUFFIX)

OBJECTS = $(OBJDIR)/guile.o

HEADERS =  gh.h

# LINKEXTRA for AIX
LINKEXTRA = 
# LINKEXTRA = -lld ../lib/sys/libdl.a 

libguile: ../$(LIBDIR)/libguile.a

../$(LIBDIR)/libguile.a: $(OBJECTS) ../$(LIBDIR)/libmzscheme.a ../$(LIBDIR)/libgc.a 
	@echo repackaging libmzscheme.a and libgc.a into libguile.a...
	cd tmp; $(AR) x ../../$(LIBDIR)/libmzscheme.a
	cd tmp; $(AR) x ../../$(LIBDIR)/libgc.a
	$(AR) ruv ../$(LIBDIR)/libguile.a $(OBJECTS) tmp/*.o
	$(RANLIB) ../$(LIBDIR)/libguile.a
	rm -rf tmp/*.o

test: ../$(LIBDIR)/libguile.a $(OBJDIR)/test.o
	$(CC) -o test $(OBJDIR)/test.o ../$(LIBDIR)/libguile.a -lm $(LINKEXTRA)

$(OBJDIR)/guile.o: objects $(OBJDIR) guile.c guileinc.c wrap.inc $(HEADERS)

$(OBJDIR)/guile.o: guile.c
	$(CC) $(CFLAGS) -c guile.c -o $(OBJDIR)/guile.o

$(OBJDIR)/test.o: test.c $(HEADERS)
	$(CC) $(CFLAGS) -c test.c -o $(OBJDIR)/test.o

guileinc.c: mkghsrc
	mkghsrc

gh.h: ghbase.h mkghsrc
	mkghsrc

wrap.inc: mkghsrc
	mkghsrc

$(OBJDIR):
	mkdir $(OBJDIR)

objects:
	mkdir objects

clean:
	rm -rf *.o *~ #*#
	rm -rf test
	rm -rf tmp/*.o tmp/*.a
