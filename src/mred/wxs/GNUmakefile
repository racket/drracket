
PLTSRCDIR = ../..
include ../../wxxt/src/xtmake.env

LOCALFLAGS = 

WXSCFLAGS = $(XINCLUDE) $(INC) $(OPTIONS) $(GUI) $(WARN) $(OPT) $(DEBUGFLAGS)

WXSCHEMEHEADERS = wxscheme.h ../../mzscheme/src/stypes.h ../../mzscheme/include/scheme.h ../../wxxt/src/DataStructures/Object.h ../../wxcommon/wxGC.h ../../wxcommon/wx_list.h
MEDIAHEADERS = ../wxme/wx_media.h ../wxme/wx_medad.h ../wxme/wx_snip.h
WINDOWHEADERS = ../../wxxt/src/Windows/Window.h
MAINDCHEADERS = ../../wxxt/src/DeviceContexts/DC.h
DCHEADERS = $(MAINDCHEADERS) ../../wxxt/src/DeviceContexts/WindowDC.h ../../wxxt/src/DeviceContexts/PSDC.h

OBJDIR=objects/sys$(MZPLATFORMSUFFIX)$(GUISUFFIX)

OBJECTS = $(OBJDIR)/xcglue.o $(OBJDIR)/wxscomon.o $(OBJDIR)/wxs_bmap.o $(OBJDIR)/wxs_butn.o $(OBJDIR)/wxs_chce.o $(OBJDIR)/wxs_ckbx.o $(OBJDIR)/wxs_cnvs.o $(OBJDIR)/wxs_cstr.o $(OBJDIR)/wxs_dc.o $(OBJDIR)/wxs_evnt.o $(OBJDIR)/wxs_fram.o $(OBJDIR)/wxs_gage.o $(OBJDIR)/wxs_gdi.o $(OBJDIR)/wxs_glob.o $(OBJDIR)/wxs_help.o $(OBJDIR)/wxs_ipc.o $(OBJDIR)/wxs_item.o $(OBJDIR)/wxs_lbox.o $(OBJDIR)/wxs_list.o $(OBJDIR)/wxs_madm.o $(OBJDIR)/wxs_mede.o $(OBJDIR)/wxs_medi.o $(OBJDIR)/wxs_menu.o $(OBJDIR)/wxs_mio.o $(OBJDIR)/wxs_misc.o $(OBJDIR)/wxs_mpb.o $(OBJDIR)/wxs_obj.o $(OBJDIR)/wxs_panl.o $(OBJDIR)/wxs_rado.o $(OBJDIR)/wxs_slid.o $(OBJDIR)/wxs_snip.o $(OBJDIR)/wxs_styl.o $(OBJDIR)/wxs_tbar.o $(OBJDIR)/wxs_text.o $(OBJDIR)/wxs_win.o $(OBJDIR)/wxscheme.o

SOURCES = wxscomon.cxx wxs_bmap.cxx wxs_butn.cxx wxs_chce.cxx wxs_ckbx.cxx wxs_cnvs.cxx wxs_cstr.cxx wxs_dc.cxx wxs_evnt.cxx wxs_fram.cxx wxs_gage.cxx wxs_gdi.cxx wxs_glob.cxx wxs_help.cxx wxs_ipc.cxx wxs_item.cxx wxs_lbox.cxx wxs_list.cxx wxs_madm.cxx wxs_mede.cxx wxs_medi.cxx wxs_menu.cxx wxs_mio.cxx wxs_misc.cxx wxs_mpb.cxx wxs_obj.cxx wxs_panl.cxx wxs_rado.cxx wxs_slid.cxx wxs_snip.cxx wxs_styl.cxx wxs_tbar.cxx wxs_text.cxx wxs_win.cxx wxscheme.cxx

SCRIPTS = wxs_bmap.xc wxs_butn.xc wxs_chce.xc wxs_ckbx.xc wxs_cnvs.xc wxs_cstr.xc wxs_dc.xc wxs_evnt.xc wxs_fram.xc wxs_gage.xc wxs_gdi.xc wxs_glob.xc wxs_help.xc wxs_ipc.xc wxs_item.xc wxs_lbox.xc wxs_list.xc wxs_madm.xc wxs_mede.xc wxs_medi.xc wxs_menu.xc wxs_mio.xc wxs_misc.xc wxs_mpb.xc wxs_obj.xc wxs_panl.xc wxs_rado.xc wxs_slid.xc wxs_snip.xc wxs_styl.xc wxs_tbar.xc wxs_text.xc wxs_win.xc

DOCDIR = /home/mflatt/proj/doc/

all: objects $(OBJDIR)
	$(MAKE) $(OBJDIR)/libwxscheme.a

$(OBJDIR):
	mkdir $(OBJDIR)
objects:
	mkdir objects

$(OBJDIR)/libwxscheme.a : $(OBJECTS) 
	$(AR) $(ARFLAGS) $(OBJDIR)/libwxscheme.a $(OBJECTS)
	$(RANLIB) $(OBJDIR)/libwxscheme.a

names:
	$(MAKE) $(DOCDIR)/names.tex

test-file:
	rm -f ../../../tests/mred/tests.ss
	$(MAKE) ../../../tests/mred/tests.ss

../../../tests/mred/tests.ss: $(SCRIPTS) danger.tst
	../../mzscheme/utils/xctotest danger.tst *.xc > ../../../tests/mred/tests.ss

$(DOCDIR)/names.tex: $(SCRIPTS)
	../../mzscheme/utils/xctotex *.xc > $(DOCDIR)/names.tex

ccsources: $(SOURCES)


xdepend:
	makedepend -- $(CFLAGS) -- $(SOURCES)

depend:

clean:
	rm -f objects/*/*

exile-common:
	mkdir $(HIDEDIR)/wxs_objects
	mv objects $(HIDEDIR)/wxs_objects/
	rm -f libwxscheme.a

exile:
	$(MAKE) exile-common
	rm -f *~ testobj

soft-exile:
	$(MAKE) exile-common

repatriate:
	mv $(HIDEDIR)/wxs_objects/objects .
	rmdir $(HIDEDIR)/wxs_objects

XCTOCXX = $(PERL) ../../mzscheme/utils/xctocc -cxx
XCDEPEND = ../../mzscheme/utils/xctocc ../../mzscheme/utils/parse.pl wxs.xci common.txt

CB_DEPEND = cb_start.xci cb_end.xci cbgen_s.xci
LIST_DEPEND = list.xci
DRAW_DEPEND = wxs_draw.xci wxs_drwf.xci wxs_drws.xci $(LIST_DEPEND)
ITEM_DEPEND = wxs_item.xci wxs_win.xci

wxscomon.cxx : wxscomon.xc $(XCDEPEND)
	$(XCTOCXX) wxscomon.xc
wxs_bmap.cxx : wxs_bmap.xc wxs_bmt.xci $(XCDEPEND) $(LIST_DEPEND)
	$(XCTOCXX) wxs_bmap.xc
wxs_butn.cxx : wxs_butn.xc  $(XCDEPEND) $(CB_DEPEND) $(ITEM_DEPEND)
	$(XCTOCXX) wxs_butn.xc
wxs_chce.cxx : wxs_chce.xc  range.xci $(XCDEPEND) $(CB_DEPEND) $(LIST_DEPEND) $(ITEM_DEPEND)
	$(XCTOCXX) wxs_chce.xc
wxs_ckbx.cxx : wxs_ckbx.xc  $(XCDEPEND) $(CB_DEPEND) $(ITEM_DEPEND)
	$(XCTOCXX) wxs_ckbx.xc
wxs_cnvs.cxx : wxs_cnvs.xc wxs_win.xci wxs_cnvs.xci wxs_char.xci $(XCDEPEND)
	$(XCTOCXX) wxs_cnvs.xc
wxs_cstr.cxx : wxs_cstr.xc  $(XCDEPEND)
	$(XCTOCXX) wxs_cstr.xc
wxs_dc.cxx : wxs_dc.xc  wxs_dorf.xci $(XCDEPEND) $(DRAW_DEPEND)
	$(XCTOCXX) wxs_dc.xc
wxs_evnt.cxx : wxs_evnt.xc  $(XCDEPEND)
	$(XCTOCXX) wxs_evnt.xc
wxs_form.cxx : wxs_form.xc  $(XCDEPEND) $(CB_DEPEND)
	$(XCTOCXX) wxs_form.xc
wxs_fram.cxx : wxs_fram.xc wxs_win.xci wxs_fram.xci wxs_espc.xci $(XCDEPEND)
	$(XCTOCXX) wxs_fram.xc
wxs_gage.cxx : wxs_gage.xc  $(XCDEPEND) $(CB_DEPEND) $(ITEM_DEPEND)
	$(XCTOCXX) wxs_gage.xc
wxs_gdi.cxx : wxs_gdi.xc  $(XCDEPEND)
	$(XCTOCXX) wxs_gdi.xc
wxs_glob.cxx : wxs_glob.xc  $(XCDEPEND) $(LIST_DEPEND)
	$(XCTOCXX) wxs_glob.xc
wxs_help.cxx : wxs_help.xc  $(XCDEPEND)
	$(XCTOCXX) wxs_help.xc
wxs_ipc.cxx : wxs_ipc.xc  $(XCDEPEND)
	$(XCTOCXX) wxs_ipc.xc
wxs_item.cxx : wxs_item.xc  $(ITEM_DEPEND) $(XCDEPEND)
	$(XCTOCXX) wxs_item.xc
wxs_lbox.cxx : wxs_lbox.xc  range.xci $(XCDEPEND) $(CB_DEPEND) $(LIST_DEPEND) $(ITEM_DEPEND)
	$(XCTOCXX) wxs_lbox.xc
wxs_list.cxx : wxs_list.xc  $(XCDEPEND)
	$(XCTOCXX) wxs_list.xc
wxs_madm.cxx : wxs_madm.xc wxs_madm.xci wxs_fcs.xci wxs_bkt.xci wxs_win.xci $(XCDEPEND)
	$(XCTOCXX) wxs_madm.xc
wxs_medi.cxx : wxs_medi.xc wxs_mbuf.xci wxs_fcs.xci wxs_eds.xci wxs_cret.xci wxs_eop.xci $(XCDEPEND) $(LIST_DEPEND)
	$(XCTOCXX) wxs_medi.xc
wxs_mede.cxx : wxs_mede.xc wxs_mbuf.xci wxs_fcs.xci wxs_eds.xci wxs_cret.xci wxs_bkt.xci $(XCDEPEND) $(LIST_DEPEND)
	$(XCTOCXX) wxs_mede.xc
wxs_menu.cxx : wxs_menu.xc  $(XCDEPEND) $(CB_DEPEND) $(LIST_DEPEND)
	$(XCTOCXX) wxs_menu.xc
wxs_mio.cxx : wxs_mio.xc  $(XCDEPEND)
	$(XCTOCXX) wxs_mio.xc
wxs_misc.cxx : wxs_misc.xc  wxs_espc.xci $(XCDEPEND) $(CB_DEPEND)
	$(XCTOCXX) wxs_misc.xc
wxs_mpb.cxx : wxs_mpb.xc  wxs_mbuf.xci wxs_eds.xci wxs_cret.xci wxs_fcs.xci $(XCDEPEND)
	$(XCTOCXX) wxs_mpb.xc
wxs_obj.cxx : wxs_obj.xc  $(XCDEPEND)
	$(XCTOCXX) wxs_obj.xc
wxs_panl.cxx : wxs_panl.xc wxs_panl.xci wxs_fram.xci wxs_cnvs.xci wxs_dorf.xci wxs_ifnt.xci wxs_win.xci wxs_espc.xci $(XCDEPEND) $(CB_DEPEND)
	$(XCTOCXX) wxs_panl.xc
wxs_rado.cxx : wxs_rado.xc  range.xci $(XCDEPEND) $(CB_DEPEND) $(LIST_DEPEND) $(ITEM_DEPEND)
	$(XCTOCXX) wxs_rado.xc
wxs_slid.cxx : wxs_slid.xc  $(XCDEPEND) $(CB_DEPEND) $(ITEM_DEPEND)
	$(XCTOCXX) wxs_slid.xc
wxs_snip.cxx : wxs_snip.xc  wxs_snip.xci wxs_bmt.xci wxs_cret.xci wxs_eop.xci $(XCDEPEND)
	$(XCTOCXX) wxs_snip.xc
wxs_styl.cxx : wxs_styl.xc  $(XCDEPEND)
	$(XCTOCXX) wxs_styl.xc
wxs_tbar.cxx : wxs_tbar.xc $(XCDEPEND)
	$(XCTOCXX) wxs_tbar.xc
wxs_text.cxx : wxs_text.xc wxs_win.xci  $(XCDEPEND) $(CB_DEPEND) $(ITEM_DEPEND)
	$(XCTOCXX) wxs_text.xc
wxs_win.cxx : wxs_win.xc wxs_win.xci $(XCDEPEND)
	$(XCTOCXX) wxs_win.xc

WXSCC = $(CXX) $(WXSCFLAGS) $(LOCALFLAGS) -c

$(OBJDIR)/wxscheme.o : wxscheme.cxx $(WXSCHEMEHEADERS) $(MEDIAHEADERS) wxsmred.h wrap.inc cwrap.inc
	$(WXSCC) wxscheme.cxx -o $(OBJDIR)/wxscheme.o 

$(OBJDIR)/xcglue.o : ../../mzscheme/utils/xcglue.c ../../mzscheme/utils/xcglue.h $(WXSCHEMEHEADERS)
	$(CC) $(WXSCFLAGS) $(LOCALFLAGS) -c ../../mzscheme/utils/xcglue.c -o $(OBJDIR)/xcglue.o

$(OBJDIR)/wxscomon.o : wxscomon.cxx $(WXSCHEMEHEADERS)
	$(WXSCC) wxscomon.cxx -o $(OBJDIR)/wxscomon.o
$(OBJDIR)/wxs_bmap.o : wxs_bmap.cxx $(WXSCHEMEHEADERS)
	$(WXSCC) wxs_bmap.cxx -o $(OBJDIR)/wxs_bmap.o
$(OBJDIR)/wxs_butn.o : wxs_butn.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS) ../../wxxt/src/Windows/Button.h
	$(WXSCC) wxs_butn.cxx -o $(OBJDIR)/wxs_butn.o
$(OBJDIR)/wxs_chce.o : wxs_chce.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS)
	$(WXSCC) wxs_chce.cxx -o $(OBJDIR)/wxs_chce.o
$(OBJDIR)/wxs_ckbx.o : wxs_ckbx.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS) ../../wxxt/src/Windows/CheckBox.h
	$(WXSCC) wxs_ckbx.cxx -o $(OBJDIR)/wxs_ckbx.o
$(OBJDIR)/wxs_cnvs.o : wxs_cnvs.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS)
	$(WXSCC) wxs_cnvs.cxx -o $(OBJDIR)/wxs_cnvs.o
$(OBJDIR)/wxs_cstr.o : wxs_cstr.cxx $(WXSCHEMEHEADERS)
	$(WXSCC) wxs_cstr.cxx -o $(OBJDIR)/wxs_cstr.o
$(OBJDIR)/wxs_dc.o : wxs_dc.cxx $(WXSCHEMEHEADERS) $(DCHEADERS)
	$(WXSCC) wxs_dc.cxx -o $(OBJDIR)/wxs_dc.o
$(OBJDIR)/wxs_evnt.o : wxs_evnt.cxx $(WXSCHEMEHEADERS)
	$(WXSCC) wxs_evnt.cxx -o $(OBJDIR)/wxs_evnt.o
$(OBJDIR)/wxs_fram.o : wxs_fram.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS) ../../wxxt/src/Windows/Frame.h
	$(WXSCC) wxs_fram.cxx -o $(OBJDIR)/wxs_fram.o
$(OBJDIR)/wxs_gage.o : wxs_gage.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS)
	$(WXSCC) wxs_gage.cxx -o $(OBJDIR)/wxs_gage.o
$(OBJDIR)/wxs_gdi.o : wxs_gdi.cxx $(WXSCHEMEHEADERS)
	$(WXSCC) wxs_gdi.cxx -o $(OBJDIR)/wxs_gdi.o
$(OBJDIR)/wxs_glob.o : wxs_glob.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS)
	$(WXSCC) wxs_glob.cxx -o $(OBJDIR)/wxs_glob.o
$(OBJDIR)/wxs_help.o : wxs_help.cxx $(WXSCHEMEHEADERS)
	$(WXSCC) wxs_help.cxx -o $(OBJDIR)/wxs_help.o
$(OBJDIR)/wxs_ipc.o : wxs_ipc.cxx $(WXSCHEMEHEADERS)
	$(WXSCC) wxs_ipc.cxx -o $(OBJDIR)/wxs_ipc.o
$(OBJDIR)/wxs_item.o : wxs_item.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS)
	$(WXSCC) wxs_item.cxx -o $(OBJDIR)/wxs_item.o
$(OBJDIR)/wxs_lbox.o : wxs_lbox.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS)
	$(WXSCC) wxs_lbox.cxx -o $(OBJDIR)/wxs_lbox.o
$(OBJDIR)/wxs_list.o : wxs_list.cxx $(WXSCHEMEHEADERS)
	$(WXSCC) wxs_list.cxx -o $(OBJDIR)/wxs_list.o
$(OBJDIR)/wxs_madm.o : wxs_madm.cxx $(WXSCHEMEHEADERS) $(MEDIAHEADERS) $(WINDOWHEADERS)
	$(WXSCC) wxs_madm.cxx -o $(OBJDIR)/wxs_madm.o
$(OBJDIR)/wxs_medi.o : wxs_medi.cxx $(WXSCHEMEHEADERS) $(MEDIAHEADERS) $(WINDOWHEADERS) $(MAINDCHEADERS)
	$(WXSCC) wxs_medi.cxx -o $(OBJDIR)/wxs_medi.o
$(OBJDIR)/wxs_mede.o : wxs_mede.cxx $(WXSCHEMEHEADERS) $(MEDIAHEADERS) $(WINDOWHEADERS) $(MAINDCHEADERS)
	$(WXSCC) wxs_mede.cxx -o $(OBJDIR)/wxs_mede.o
$(OBJDIR)/wxs_menu.o : wxs_menu.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS)
	$(WXSCC) wxs_menu.cxx -o $(OBJDIR)/wxs_menu.o
$(OBJDIR)/wxs_mio.o : wxs_mio.cxx $(WXSCHEMEHEADERS) $(MEDIAHEADERS) $(WINDOWHEADERS)
	$(WXSCC) wxs_mio.cxx -o $(OBJDIR)/wxs_mio.o
$(OBJDIR)/wxs_misc.o : wxs_misc.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS) ../../wxxt/src/Misc/Clipboard.h
	$(WXSCC) wxs_misc.cxx -o $(OBJDIR)/wxs_misc.o
$(OBJDIR)/wxs_mpb.o : wxs_mpb.cxx $(WXSCHEMEHEADERS) $(MEDIAHEADERS) $(WINDOWHEADERS) ../wxme/wx_medpb.h $(MAINDCHEADERS)
	$(WXSCC) wxs_mpb.cxx -o $(OBJDIR)/wxs_mpb.o
$(OBJDIR)/wxs_obj.o : wxs_obj.cxx $(WXSCHEMEHEADERS)
	$(WXSCC) wxs_obj.cxx -o $(OBJDIR)/wxs_obj.o
$(OBJDIR)/wxs_panl.o : wxs_panl.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS)
	$(WXSCC) wxs_panl.cxx -o $(OBJDIR)/wxs_panl.o
$(OBJDIR)/wxs_rado.o : wxs_rado.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS)
	$(WXSCC) wxs_rado.cxx -o $(OBJDIR)/wxs_rado.o
$(OBJDIR)/wxs_slid.o : wxs_slid.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS)
	$(WXSCC) wxs_slid.cxx -o $(OBJDIR)/wxs_slid.o
$(OBJDIR)/wxs_snip.o : wxs_snip.cxx $(WXSCHEMEHEADERS) $(MEDIAHEADERS) $(WINDOWHEADERS) $(MAINDCHEADERS)
	$(WXSCC) wxs_snip.cxx -o $(OBJDIR)/wxs_snip.o
$(OBJDIR)/wxs_styl.o : wxs_styl.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS) ../wxme/wx_style.h $(MAINDCHEADERS)
	$(WXSCC) wxs_styl.cxx -o $(OBJDIR)/wxs_styl.o
$(OBJDIR)/wxs_tbar.o : wxs_tbar.cxx $(WXSCHEMEHEADERS)
	$(WXSCC) wxs_tbar.cxx -o $(OBJDIR)/wxs_tbar.o
$(OBJDIR)/wxs_text.o : wxs_text.cxx $(WXSCHEMEHEADERS)
	$(WXSCC) wxs_text.cxx -o $(OBJDIR)/wxs_text.o
$(OBJDIR)/wxs_win.o : wxs_win.cxx $(WXSCHEMEHEADERS) $(WINDOWHEADERS)
	$(WXSCC) wxs_win.cxx -o $(OBJDIR)/wxs_win.o
