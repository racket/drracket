# Symantec C++ makefile for the base objects
# called from src\makefile.sc

# configuration section (see src\makefile.sc) ###########################

WXDIR = $(WXWIN)
INCDIR = $(WXDIR)\include
MSWINC = $(INCDIR)\msw
BASEINC = $(INCDIR)\base

# default values overridden by src\makefile.sc

CC=sc
CFLAGS = -o -ml -W -Dwx_msw

INCLUDE=$(BASEINC);$(MSWINC);$(WXDIR)\contrib\fafa

OPTIONS=

# end of configuration section ##########################################

.cc.obj:
	*$(CC) -c $(CFLAGS) -I$(INCLUDE) $(OPTIONS) $<

OBJS = wb_win.obj wb_data.obj wb_frame.obj wb_panel.obj wb_utils.obj wb_main.obj \
wb_item.obj wb_list.obj wb_obj.obj wb_text.obj wb_gdi.obj wb_dialg.obj \
wb_canvs.obj wb_dc.obj wb_mf.obj wb_ps.obj wx_enhdg.obj wb_hash.obj \
wb_ipc.obj wb_form.obj wb_timer.obj wb_help.obj wb_sysev.obj wb_stdev.obj \
wb_types.obj wb_mgstr.obj

HDRS = $(BASEINC)\wb_item.h $(BASEINC)\wb_buttn.h \
$(BASEINC)\wb_check.h $(BASEINC)\wb_choic.h $(BASEINC)\wb_menu.h \
$(BASEINC)\wb_messg.h $(BASEINC)\wb_txt.h $(BASEINC)\wb_mtxt.h \
$(BASEINC)\wb_lbox.h $(BASEINC)\wb_slidr.h

all: $(OBJS)

wb_obj.obj: $(BASEINC)\wx_obj.h

wb_win.obj: $(BASEINC)\common.h $(BASEINC)\wb_win.h $(BASEINC)\wx_obj.h

wb_main.obj: $(BASEINC)\common.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wb_frame.h $(BASEINC)\wx_utils.h

wb_frame.obj: $(BASEINC)\common.h $(BASEINC)\wb_win.h \
$(BASEINC)\wx_obj.h $(BASEINC)\wx_utils.h $(BASEINC)\wb_frame.h \
wb_frame.cc $(BASEINC)\wx_stdev.h

wb_panel.obj: $(BASEINC)\common.h $(BASEINC)\wb_win.h \
$(BASEINC)\wx_obj.h $(BASEINC)\wx_utils.h $(BASEINC)\wb_frame.h \
$(BASEINC)\wb_panel.h wb_panel.cc $(BASEINC)\wx_stdev.h

wb_text.obj: $(BASEINC)\common.h $(BASEINC)\wb_win.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_utils.h $(BASEINC)\wb_frame.h $(BASEINC)\wb_text.h \
wb_text.cc $(BASEINC)\wx_stdev.h

wb_canvs.obj: $(BASEINC)\common.h $(BASEINC)\wb_win.h \
$(BASEINC)\wx_obj.h $(BASEINC)\wx_utils.h $(BASEINC)\wb_frame.h \
$(BASEINC)\wb_canvs.h wb_canvs.cc $(BASEINC)\wx_stdev.h \
$(BASEINC)\wb_gdi.h $(BASEINC)\wb_dc.h

wb_dc.obj: $(BASEINC)\common.h $(BASEINC)\wb_win.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_utils.h $(BASEINC)\wb_frame.h $(BASEINC)\wb_canvs.h \
wb_dc.cc $(BASEINC)\wx_stdev.h $(BASEINC)\wb_gdi.h $(BASEINC)\wb_dc.h

wb_ps.obj: $(BASEINC)\common.h $(BASEINC)\wb_win.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_utils.h $(BASEINC)\wb_frame.h $(BASEINC)\wb_canvs.h \
wb_ps.cc $(BASEINC)\wx_stdev.h $(BASEINC)\wb_gdi.h $(BASEINC)\wb_dc.h

wb_mf.obj: $(BASEINC)\common.h $(BASEINC)\wb_win.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_utils.h $(BASEINC)\wb_frame.h $(BASEINC)\wb_canvs.h \
wb_mf.cc $(BASEINC)\wx_stdev.h $(BASEINC)\wb_gdi.h $(BASEINC)\wb_mf.h

wb_item.obj: $(BASEINC)\common.h $(BASEINC)\wb_win.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_utils.h $(BASEINC)\wb_frame.h $(HDRS) wb_item.cc \
$(BASEINC)\wx_stdev.h

wb_utils.obj: $(BASEINC)\common.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_utils.h wb_utils.cc

wb_ipc.obj: $(BASEINC)\common.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_utils.h $(BASEINC)\wb_ipc.h wb_ipc.cc

wb_list.obj: $(BASEINC)\common.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_list.h $(BASEINC)\wx_utils.h wb_list.cc

wb_hash.obj: $(BASEINC)\common.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_hash.h wb_hash.cc $(BASEINC)\wx_list.h \
$(BASEINC)\wx_utils.h

wb_event.obj: $(BASEINC)\common.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_stdev.h $(BASEINC)\wx_utils.h wb_event.cc

wb_gdi.obj: $(BASEINC)\common.h $(BASEINC)\wb_gdi.h \
$(BASEINC)\wx_utils.h wb_gdi.cc

wb_dialg.obj: $(BASEINC)\common.h wb_dialg.cc $(BASEINC)\wb_dialg.h \
$(BASEINC)\wb_win.h $(BASEINC)\wx_utils.h $(BASEINC)\wb_panel.h $(HDRS)

wb_form.obj: $(BASEINC)\common.h wb_form.cc $(BASEINC)\wx_form.h \
$(BASEINC)\wb_win.h $(BASEINC)\wx_utils.h $(BASEINC)\wb_panel.h

wb_timer.obj: $(BASEINC)\common.h wb_timer.cc $(BASEINC)\wb_timer.h

wb_help.obj: $(BASEINC)\common.h wb_help.cc $(BASEINC)\wx_help.h \
$(MSWINC)\wx_ipc.h

wb_types.obj: $(BASEINC)\common.h wb_types.cc $(BASEINC)\wx_types.h

wb_mgstr.obj: $(BASEINC)\common.h $(BASEINC)\wx_mgstr.h wb_mgstr.cc

wb_sysev.obj: $(BASEINC)\common.h wb_sysev.cc $(BASEINC)\wx_sysev.h \
$(BASEINC)\wx_types.h

wb_stdev.obj: $(BASEINC)\common.h wb_stdev.cc $(BASEINC)\wx_stdev.h \
$(BASEINC)\wx_types.h $(BASEINC)\wx_sysev.h

wx_enhdg.obj: $(BASEINC)\common.h wx_enhdg.cc $(BASEINC)\wx_enhdg.h

wb_data.obj: $(BASEINC)\common.h wb_data.cc

clean:
	-del *.obj
