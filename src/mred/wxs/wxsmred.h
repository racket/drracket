
extern int mred_eventspace_param;
extern int mred_event_dispatch_param;
extern int mred_ps_setup_param;

extern Scheme_Type mred_eventspace_type;

extern Scheme_Object *MrEdGetFrameList(void);
extern int MrEdCheckForBreak(void);
extern Scheme_Object *MrEdEventspaceConfig(Scheme_Object *);

extern Scheme_Object *MrEdMakeEventspace(Scheme_Config *c);
extern int wxsIsContextShutdown(void *cx);

extern Scheme_Object *wxsBundlePSSetup(wxPrintSetupData *d);
extern wxPrintSetupData *wxsUnbundlePSSetup(Scheme_Object *s);

extern void MrEd_add_q_callback(char *who, int argc, Scheme_Object **argv);
extern Scheme_Object *MrEd_mid_queue_key;

extern Scheme_Object *wxs_app_file_proc;
extern Scheme_Object *wxs_app_quit_proc;

extern Bool wxSchemeYield(void *sema);

extern wxWindow *wxLocationToWindow(int x, int y);
