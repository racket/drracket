
PLTSRCDIR = ../..
include ../../wxxt/src/xtmake.env

OBJDIR=objects/sys$(MZPLATFORMSUFFIX)$(GUISUFFIX)

# GCLIBS used only for edjr
# Don't need GC objects unless you're compiling with garbage collection on
GCLIBS =
# GCLIBS = -L../mzscheme/lib/sys -lgc

OBJECTS = $(OBJDIR)/wx_style.o $(OBJDIR)/wx_keym.o $(OBJDIR)/wx_mbuf.o \
          $(OBJDIR)/wx_media.o $(OBJDIR)/wx_mpriv.o $(OBJDIR)/wx_msnip.o \
          $(OBJDIR)/wx_mline.o $(OBJDIR)/wx_mpbrd.o \
          $(OBJDIR)/wx_medad.o $(OBJDIR)/wx_snip.o $(OBJDIR)/wx_cgrec.o \
          $(OBJDIR)/wx_medio.o

ALLHEADERS = wx_ptreq.h wx_mtype.h \
             ../../wxxt/src/DataStructures/Object.h ../../wxcommon/wxGC.h ../../wxcommon/wx_list.h
HEADERS = wx_media.h wx_medad.h wx_cgrec.h wx_medio.h wx_style.h \
          $(ALLHEADERS) ../../wxxt/src/Windows/Window.h ../../wxxt/src/DeviceContexts/DC.h

# Used only when bundling a distribution
HIDEDIR = /home/mflatt/mredhide

$(OBJDIR)/libwxme.a: objects $(OBJDIR) $(OBJECTS)
	$(AR) $(ARFLAGS) $(OBJDIR)/libwxme.a $(OBJECTS)
	$(RANLIB) $(OBJDIR)/libwxme.a

$(OBJDIR): 
	mkdir $(OBJDIR)
objects:
	mkdir objects

edjr: $(OBJDIR) $(OBJDIR)/libwxme.a $(OBJDIR)/edjr.o $(OBJDIR)/edjrmain.o
	$(LINKER) $(CPPFLAGS) $(LDFLAGS) -L$(OBJDIR) -o edjr $(OBJDIR)/edjr.o $(OBJDIR)/edjrmain.o -lwxme $(LDLIBS) $(GCLIBS)

MCC = $(CXX) $(CPPFLAGS) $(LOCALFLAGS) -c

$(OBJDIR)/wx_media.o : wx_media.cxx $(HEADERS) wx_keym.h wx_snip.h \
                       wx_mpriv.h wx_mline.h
	$(MCC) wx_media.cxx -o $(OBJDIR)/wx_media.o

$(OBJDIR)/wx_mpriv.o : wx_mpriv.cxx $(HEADERS) wx_keym.h wx_snip.h wx_mpriv.h \
                       wx_mline.h wx_gcrct.h
	$(MCC) wx_mpriv.cxx -o $(OBJDIR)/wx_mpriv.o

$(OBJDIR)/wx_msnip.o : wx_msnip.cxx $(HEADERS) wx_keym.h wx_snip.h wx_madm.h \
                       wx_mpriv.h wx_gcrct.h
	$(MCC) wx_msnip.cxx -o $(OBJDIR)/wx_msnip.o

$(OBJDIR)/wx_mline.o : wx_mline.cxx wx_mline.h wx_snip.h $(HEADERS)
	$(MCC) wx_mline.cxx -o $(OBJDIR)/wx_mline.o

$(OBJDIR)/wx_mpbrd.o : wx_mpbrd.cxx $(HEADERS) wx_keym.h wx_snip.h wx_medpb.h
	$(MCC) wx_mpbrd.cxx -o $(OBJDIR)/wx_mpbrd.o

$(OBJDIR)/wx_medad.o : wx_medad.cxx $(HEADERS) wx_snip.h
	$(MCC) wx_medad.cxx -o $(OBJDIR)/wx_medad.o

$(OBJDIR)/wx_mbuf.o : wx_mbuf.cxx $(HEADERS) wx_keym.h wx_snip.h
	$(MCC) wx_mbuf.cxx -o $(OBJDIR)/wx_mbuf.o

$(OBJDIR)/wx_snip.o : wx_snip.cxx $(HEADERS) wx_gcrct.h
	$(MCC) wx_snip.cxx -o $(OBJDIR)/wx_snip.o

$(OBJDIR)/wx_cgrec.o : wx_cgrec.cxx $(HEADERS) wx_snip.h
	$(MCC) wx_cgrec.cxx -o $(OBJDIR)/wx_cgrec.o

$(OBJDIR)/wx_keym.o : wx_keym.cxx wx_keym.h $(ALLHEADERS)
	$(MCC) wx_keym.cxx -o $(OBJDIR)/wx_keym.o

$(OBJDIR)/wx_medio.o : wx_medio.cxx wx_medio.h $(ALLHEADERS)
	$(MCC) wx_medio.cxx -o $(OBJDIR)/wx_medio.o

$(OBJDIR)/wx_style.o : wx_style.cxx wx_style.h wx_medio.h $(ALLHEADERS) ../../wxxt/src/DeviceContexts/DC.h
	$(MCC) wx_style.cxx -o $(OBJDIR)/wx_style.o 

$(OBJDIR)/edjr.o : edjr.cxx edjr.h  wx_media.h wx_medad.h
	$(MCC) edjr.cxx -o $(OBJDIR)/edjr.o

$(OBJDIR)/edjrmain.o : edjrmain.cxx edjr.h
	$(MCC) edjrmain.cxx -o $(OBJDIR)/edjrmain.o

clean:
	rm -f objects/*/* *~

exile-common:
	rm -f *~ libwxme.a edjr
	mkdir $(HIDEDIR)/wxme_objects
	mv objects $(HIDEDIR)/wxme_objects

exile:
	$(MAKE) exile-common

soft-exile:
	$(MAKE) exile-common

repatriate:
	mv $(HIDEDIR)/wxme_objects/objects .
	rmdir $(HIDEDIR)/wxme_objects
