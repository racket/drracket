!ifdef ARGFILE
!include $(ARGFILE)
!else
!include ..\argfile.mkf
!endif

WXMSWINC =	$(WXDIR)\include\msw
WXBASEINC =	$(WXDIR)\include\base

.AUTODEPEND

CPPFLAGS =	@$(CFG) $(DEBUG_FLAGS) $(OPT)

.cc.obj:
	$(CC) $(CPPFLAGS) -c {$< }

OBJECTS =	wb_data.obj wb_win.obj wb_frame.obj wb_panel.obj \
		wb_utils.obj wb_main.obj wb_item.obj wb_list.obj \
		wb_obj.obj wb_ps.obj wb_text.obj wb_gdi.obj \
		wb_dialg.obj wb_canvs.obj wb_dc.obj wb_mf.obj \
		wb_hash.obj wb_ipc.obj wb_form.obj wb_timer.obj \
		wb_help.obj wb_sysev.obj wb_stdev.obj wb_types.obj \
		wb_mgstr.obj wxstring.obj wx_enhdg.obj

all:    $(CFG) $(OBJECTS)

clean:
	-erase *.obj
	-erase *.bak
	-erase *.csm

nuke:	clean
	echo Nothing to nuke
