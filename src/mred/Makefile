
include $(WXDIR)/src/make$(MZPLATFORMSUFFIX).env

OBJDIR=objects/sys$(MZPLATFORMSUFFIX)$(GUISUFFIX)
LIBDIR=lib/sys$(MZPLATFORMSUFFIX)
MZLIBDIR=$(LIBDIR)$(MZTHREADSUFFIX)

MZSCHEMELDFLAGS = -L../mzscheme/$(LIBDIR)/
MZSCHEMELDLIBS = -lmzscheme -lgc

WXSCHEMELDFLAGS = -Lwxs/$(OBJDIR)
WXSCHEMELDLIBS = -lwxscheme

WXMELDFLAGS = -Lwxme/$(OBJDIR)
WXMELDLIBS = -lwxme

MREDLDFLAGS = $(LDFLAGS) $(MZSCHEMELDFLAGS) $(WXSCHEMELDFLAGS) $(WXMELDFLAGS)
MREDLDLIBS = $(MZSCHEMELDLIBS) $(WXSCHEMELDLIBS) $(WXMELDLIBS) $(LDLIBS) $(LDNONSTATICLIBS)

MREDOBJECTS = $(OBJDIR)/mred.o $(OBJDIR)/mredx.o

MRED = $(MRBUILDDIR)mred
MZSCHEME = ../mzscheme/$(MZLIBDIR)/libmzscheme.a ../mzscheme/$(MZLIBDIR)/libgc.a
WXSCHEME = wxs/$(OBJDIR)/libwxscheme.a
WXME = wxme/$(OBJDIR)/libwxme.a

INSTALLDEST=/home/scheme/plt

MRSTATIC = 
MRSTATIC_STUB =

MRCHECK =

# For SGIs and gcc 2.7.2, I needed a hack
# (not needed with CC)
SGILINKHACK =
# SGILINKHACK = $(OBJDIR)/sgilinkhack.o

EXPORT = CC='$(CC)' CXX='$(CXX)' AS='$(AS)' RANLIB='$(RANLIB)' DEBUGFLAGS='$(DEBUGFLAGS)' OPTIONS='$(OPTIONS)' THREADFLAGS='$(THREADFLAGS)' WARN='$(WARN)' OPT='$(OPT)' $(MZEXTRAEXPORT)

# Used for distribution
HIDEDIR = /home/mflatt/mredhide
FTPDIR = /home/scheme/FTP/plt/staging

all:  objects $(OBJDIR)
	cd ../mzscheme; $(MAKE) $(EXPORT) mzschemelib++
	cd wxs; $(MAKE)
	cd wxme; $(MAKE)
	cd wxme; $(MAKE) $(OBJDIR)/edjr.o
	$(MAKE) $(MRED) MRCHECK='$(MRCHECK)'

bin:
	$(MAKE) $(MRED)

checked:
	$(MAKE) all MRCHECK='$(OBJDIR)/checkm.o'

$(OBJDIR):
	mkdir $(OBJDIR)
objects:
	mkdir objects

$(MRED) : $(WXME) $(WXSCHEME) $(MZSCHEME) $(MREDOBJECTS) wxme/$(OBJDIR)/edjr.o \
          $(WXDIR)/$(LIBDIR)/libwx$(GUISUFFIX).a \
          $(MRSTATIC_STUB) $(MRCHECK)
	$(LINKER) $(MREDLDFLAGS) $(MRSTATIC) -o $(MRED) $(LDSTARTERS) $(MREDOBJECTS) wxme/$(OBJDIR)/edjr.o $(MRCHECK) $(MREDLDLIBS) $(MRSTATIC_STUB) $(SGILINKHACK) $(LDENDERS)

sunos4-static: $(OBJDIR)/dl_stub.o
	$(MAKE) $(MRED) MRSTATIC=-static  MRSTATIC_STUB='$(OBJDIR)/dl_stub.o' LDNONSTATICLIBS=''

solaris-static:
	$(MAKE) $(MRED) MRSTATIC="-Wl,-Bstatic" LDNONSTATICLIBS='-Wl,-Bdynamic -ldl -Wl,-Bstatic'

$(OBJDIR)/checkm.o: misc/checkm.c
	$(CC) $(CFLAGS) $(LOCALFLAGS) -c misc/checkm.c -o $(OBJDIR)/checkm.o

wx:
	cd $(WXDIR)/src/x; $(MAKE) -f makefile.unx

$(OBJDIR)/mred.o : mred.cxx wxme/wx_media.h wxme/wx_medad.h wxs/wxscheme.h ../mzscheme/include/scheme.h mred.h
	$(CXX) $(CPPFLAGS) $(LOCALFLAGS) -c mred.cxx -o $(OBJDIR)/mred.o

$(OBJDIR)/mredx.o : mredx.cxx ../mzscheme/include/scheme.h mred.h
	$(CXX) $(CPPFLAGS) $(LOCALFLAGS) -c mredx.cxx -o $(OBJDIR)/mredx.o

$(OBJDIR)/dl_stub.o: misc/dl_stub.c
	$(CC) $(CPPFLAGS) $(LOCALFLAGS) -c misc/dl_stub.c -o $(OBJDIR)/dl_stub.o

$(OBJDIR)/sgilinkhack.o:
	$(CXX) $(CPPFLAGS) $(LOCALFLAGS) -c misc/sgilinkhack.cxx -o $(OBJDIR)/sgilinkhack.o

$(WXDIR)/$(LIBDIR)/libwx$(GUISUFFIX).a:
	echo Updating $(WXDIR)/$(LIBDIR)/libwx$(GUISUFFIX).a
	$(MAKE) wx

depend:
	cd wxs; $(MAKE) depend
	cd ../mzscheme; $(MAKE) depend

TILDES=*~ ../collects/mrdemo/*~ demo/turtles/*~ demo/morph/*~ system/*~ collects/mred/*~ tests/*~ misc/*~ windows/*~ ../*~

clean:
	rm -rf objects/*/*.o $(TILDES) core
	cd wxme; $(MAKE) clean
	cd wxs; $(MAKE) clean
	cd ../mzscheme; $(MAKE) clean

exile:
	rm -rf $(TILDES) core
	mkdir $(HIDEDIR)/mred_objects
	mv objects .bin $(HIDEDIR)/mred_objects
	mv mred.exe $(HIDEDIR)/mred_objects
	cd ../mzscheme; $(MAKE) exile
	cd wxs; $(MAKE) HIDEDIR='$(HIDEDIR)' exile
	cd wxme; $(MAKE) HIDEDIR='$(HIDEDIR)' exile
	rm tests/RCS
	rm ../collects/icons
	rm ../collects/make/RCS

repatriate:
	mv $(HIDEDIR)/mred_objects/objects .
	mv $(HIDEDIR)/mred_objects/.bin .
	mv $(HIDEDIR)/mred_objects/mred.exe .
	rmdir $(HIDEDIR)/mred_objects
	cd ../mzscheme; $(MAKE) repatriate
	cd wxs; $(MAKE) HIDEDIR='$(HIDEDIR)' repatriate
	cd wxme; $(MAKE) HIDEDIR='$(HIDEDIR)' repatriate
	/bin/ln -s /home/scheme/plt/tests/mred/RCS tests/RCS
	/bin/ln -s /home/scheme/plt/collects/icons ../collects/icons
	/bin/ln -s /home/scheme/plt/collects/make/RCS ../collects/make/RCS

bundle:
	# $(MAKE) exile
	rm -rf mred
	$(MAKE) bundle-wxme
	$(MAKE) bundle-mzscheme
	$(MAKE) bundle-self

bundle-wxme:
	tar -cf wxmedia.tar wxme
	mv wxmedia.tar ../
	cd ..; gzip wxmedia.tar
	cd ..; mv wxmedia.tar.gz $(FTPDIR)/wxmedia/
	rm -f $(FTPDIR)/wxmedia/wxmedia.zip
	zip -r $(FTPDIR)/wxmedia/wxmedia.zip wxme

bundle-mzscheme:
	cd ..; tar -cf mzscheme.src.tar mzscheme
	cd ..; gzip mzscheme.src.tar
	cd ..; mv mzscheme.src.tar.gz $(FTPDIR)/mzscheme/
	rm -f $(FTPDIR)/mzscheme/mzsrc.zip
	cd ..; zip -r $(FTPDIR)/mzscheme/mzsrc.zip mzscheme

bundle-self:
	cd ..; tar -cf mred.src.tar mred
	cd ..; gzip mred.src.tar
	cd ..; mv mred.src.tar.gz $(FTPDIR)/mred/
	rm -f $(FTPDIR)/mred/mredsrc.zip
	cd ..; zip -r $(FTPDIR)/mred/mredsrc.zip mred

install:
	# cd system; rm -rf *~ core
	# cd system/hyper; rm -rf *~ core
	# cp -r system $(INSTALLDEST)/system
	cp MrEd.ad $(INSTALLDEST)/notes/mred/MrEd.ad
	cp LICENSE COPYING.LIB HISTORY FONTS mred.txt fonts12.mre fontsall.mre openbugs \
            $(INSTALLDEST)/notes/mred
	cp mred.sh $(INSTALLDEST)/bin/mred
	cp mred.ini $(INSTALLDEST)/notes/mred/mred.ini
	cp mred.fnt $(INSTALLDEST)/notes/mred/mred.fnt
	cd tests; ci -l -mrelease *.ss README
	cd $(INSTALLDEST)/tests/mred; co RCS/*
	$(MAKE) install-demo

install-demo:
	cd ../collects/mrdemo; rm -f *~
	rm -rf $(INSTALLDEST)/collects/mrdemo/*
	cp -r ../collects/mrdemo/* $(INSTALLDEST)/collects/mrdemo

asia:
	rdist -m asia

hk:
	rdist -m hk

stan:
	rdist -m stan
