// mysterx.h

#define sizeray(x) (sizeof(x)/sizeof(*x))

#define MX_PRIM_DECL(f) Scheme_Object *f(int,Scheme_Object **)

#define MX_DEFAULT_WIDTH  (400)
#define MX_DEFAULT_HEIGHT (400)

#define DOCHWND_TRIES 40
#define DOCDISPATCH_TRIES 60

#define MAXARRAYDIMS 32

// temp !!! until dont_gc_ptr fixed

#ifdef scheme_dont_gc_ptr
#undef scheme_dont_gc_ptr
#endif

#ifdef scheme_gc_ptr_ok
#undef scheme_gc_ptr_ok
#endif

#define scheme_dont_gc_ptr(p)
#define scheme_gc_ptr_ok(p)

// end temp !!!!!!!!!!!!!!!!!!!!!

#define NORETVALINDEX (-1)
#define UNICODE_BUFFER_SIZE 256
#define TYPE_TBL_SIZE 1019

/* extends INVOKEKIND enum in OAIDL.H */
#define INVOKE_EVENT 16

typedef struct _MX_prim_ {
  Scheme_Object *(*c_fun)(int argc,Scheme_Object **);
  char *name;
  short minargs;
  short maxargs;
} MX_PRIM;

typedef struct _scheme_com_obj_ { 
  Scheme_Type type;
  BOOL released;
  IDispatch *pIDispatch;
  ITypeInfo *pITypeInfo;
  ITypeInfo *pEventTypeInfo;
  IConnectionPoint *pIConnectionPoint;
  DWORD connectionCookie;
  ISink *pISink;
} MX_COM_Object;

typedef struct _scheme_com_type_ { 
  Scheme_Type type;
  BOOL released;
  ITypeInfo *pITypeInfo;
} MX_COM_Type;

typedef struct _scheme_mx_event_ { 
  Scheme_Type type;
  BOOL released;
  IEvent *pEvent;
} MX_Event;

typedef enum _mx_desckind_ {
  funcDesc,varDesc
} MX_DESCKIND;

typedef struct _method_desc_ {
  Scheme_Type type;
  BOOL released;
  MEMBERID memID;
  ITypeInfo *pITypeInfo;
  MX_DESCKIND descKind;
  union {
    FUNCDESC *pFuncDesc;
    VARDESC *pVarDesc;
  };
} MX_TYPEDESC;

typedef struct _mx_com_data_ {
  Scheme_Type type;
  BOOL released;
  union { // MS representations
    DATE date; 
    CY cy;    
    SCODE scode;
    IUnknown *pIUnknown;
  }; 
} MX_COM_Data_Object;

typedef struct _com_document_ {
  Scheme_Type type;
  BOOL released;
  HWND hwnd;
  IHTMLDocument2 *pIHTMLDocument2;
  IEventQueue *pIEventQueue;
  HANDLE readSem;
} MX_Document_Object;

typedef struct _mx_element_ {
  Scheme_Type type;
  BOOL released;
  BOOL valid;
  IHTMLElement *pIHTMLElement;
} MX_Element;

typedef struct _date_ {
  Scheme_Type type;
} MX_Date_Object;

typedef struct _mx_omit_ {
  Scheme_Type type;
} MX_OMIT;

typedef struct _mx_type_tbl_entry_ {
  IDispatch *pIDispatch;
  char *name;
  INVOKEKIND invKind;
  MX_TYPEDESC *pTypeDesc;
  struct _mx_type_tbl_entry_ *next;
} MX_TYPE_TBL_ENTRY;

typedef enum _mx_html_where_ {
  insert,append
} MX_HTML_WHERE;
  
typedef struct _document_window_ { // parameters a la MrEd frame% class
  char *label;
  int width;
  int height;
  int x;
  int y;
  DWORD style;
} DOCUMENT_WINDOW;

typedef struct _document_window_init_ {
  DOCUMENT_WINDOW docWindow;
  IStream **ppIStream; // for passing COM interface back to main thread
} DOCUMENT_WINDOW_INIT;

typedef struct _document_window_style_option {
  char *name;
  DWORD bits;
  BOOL enable;
} DOCUMENT_WINDOW_STYLE_OPTION;

// dummy type for "subtyping"
// a managed object has a Scheme_Type, followed by a released flag

typedef struct _managed_obj_ {
  Scheme_Type type;
  BOOL released;
} MX_MANAGED_OBJ;

#define MX_MANAGED_OBJ_RELEASED(o) (((MX_MANAGED_OBJ *)o)->released)

#define MX_COM_OBJP(o) (!SCHEME_INTP(o) && o->type == mx_com_object_type)
#define MX_COM_OBJ_VAL(o) (((MX_COM_Object *)o)->pIDispatch)
#define MX_COM_OBJ_CONNECTIONPOINT(o) (((MX_COM_Object *)o)->pIConnectionPoint)
#define MX_COM_OBJ_TYPEINFO(o) (((MX_COM_Object *)o)->pITypeInfo)
#define MX_COM_OBJ_EVENTTYPEINFO(o) (((MX_COM_Object *)o)->pEventTypeInfo)
#define MX_COM_OBJ_EVENTSINK(o) (((MX_COM_Object *)o)->pISink)

#define MX_COM_TYPEP(o) (!SCHEME_INTP(o) && o->type == mx_com_type_type)
#define MX_COM_TYPE_VAL(o) (((MX_COM_Type *)o)->pITypeInfo)

#define MX_DOCUMENTP(o) (!SCHEME_INTP(o) && o->type == mx_document_type)
#define MX_DOCUMENT_VAL(o) (((MX_Document_Object *)o)->pIHTMLDocument2)
#define MX_DOCUMENT_EVENTQUEUE(o) (((MX_Document_Object *)o)->pIEventQueue)

#define MX_ELEMENTP(o) (!SCHEME_INTP(o) && o->type == mx_element_type)
#define MX_ELEMENT_VALIDITY(o) (((MX_Element *)o)->valid)
#define MX_ELEMENT_VAL(o) (((MX_Element *)o)->pIHTMLElement)

#define MX_EVENTP(o) (!SCHEME_INTP(o) && o->type == mx_event_type)
#define MX_EVENT_VAL(o) (((MX_Event *)o)->pEvent)

#define MX_CYP(o) (!SCHEME_INTP(o) && o->type == mx_com_cy_type)
#define MX_CY_VAL(o) (((MX_COM_Data_Object *)o)->cy)

#define MX_DATEP(o) (!SCHEME_INTP(o) && o->type == mx_com_date_type)
#define MX_DATE_VAL(o) (((MX_COM_Data_Object *)o)->date)

#define MX_SCODEP(o) (!SCHEME_INTP(o) && o->type == mx_com_scode_type)
#define MX_SCODE_VAL(o) (((MX_COM_Data_Object *)o)->scode)

#define MX_IUNKNOWNP(o) (!SCHEME_INTP(o) && o->type == mx_com_iunknown_type)
#define MX_IUNKNOWN_VAL(o) (((MX_COM_Data_Object *)o)->pIUnknown)

extern Scheme_Type mx_com_object_type; 
extern Scheme_Type mx_com_type_type; 
extern Scheme_Type mx_document_type;
extern Scheme_Type mx_element_type;
extern Scheme_Type mx_event_type;
extern Scheme_Type mx_com_cy_type;
extern Scheme_Type mx_com_date_type;
extern Scheme_Type mx_com_boolean_type;
extern Scheme_Type mx_com_scode_type;
extern Scheme_Type mx_com_variant_type;
extern Scheme_Type mx_com_hresult_type;
extern Scheme_Type mx_com_iunknown_type;
extern Scheme_Type mx_com_pointer_type;
extern Scheme_Type mx_com_array_type;
extern Scheme_Type mx_com_omit_type;
extern Scheme_Type mx_com_typedesc_type;

extern Scheme_Object *hash_table_get;
extern Scheme_Object *hash_table_put;
extern Scheme_Object *hash_table_remove;
extern Scheme_Object *make_hash_table;

Scheme_Object *mx_make_cy(CY *);
Scheme_Object *mx_make_date(DATE *);
Scheme_Object *mx_make_bool(unsigned);
Scheme_Object *mx_make_scode(SCODE);
Scheme_Object *mx_make_idispatch(IDispatch *);
Scheme_Object *mx_make_iunknown(IUnknown *);

BOOL mx_cy_pred(Scheme_Object *);
BOOL mx_date_pred(Scheme_Object *);
BOOL mx_scode_pred(Scheme_Object *);
BOOL mx_comobj_pred(Scheme_Object *);
BOOL mx_iunknown_pred(Scheme_Object *);

CY mx_cy_val(Scheme_Object *);
DATE mx_date_val(Scheme_Object *);
SCODE mx_scode_val(Scheme_Object *);
IDispatch *mx_comobj_val(Scheme_Object *);
IUnknown *mx_iunknown_val(Scheme_Object *);

MX_PRIM_DECL(mx_com_terminate);
MX_PRIM_DECL(mx_com_invoke);
MX_PRIM_DECL(mx_com_set_property);
MX_PRIM_DECL(mx_com_get_property);
MX_PRIM_DECL(mx_com_methods);
MX_PRIM_DECL(mx_com_get_properties);
MX_PRIM_DECL(mx_com_set_properties);
MX_PRIM_DECL(mx_com_events);
MX_PRIM_DECL(mx_com_method_type);
MX_PRIM_DECL(mx_com_get_property_type);
MX_PRIM_DECL(mx_com_set_property_type);
MX_PRIM_DECL(mx_com_event_type);
MX_PRIM_DECL(mx_cocreate_instance);
MX_PRIM_DECL(mx_com_object_eq);
MX_PRIM_DECL(mx_com_object_pred);
MX_PRIM_DECL(mx_com_register_object);
MX_PRIM_DECL(mx_com_release_object);
MX_PRIM_DECL(mx_com_get_object_type);
MX_PRIM_DECL(mx_com_is_a);
MX_PRIM_DECL(mx_com_help);
MX_PRIM_DECL(mx_com_register_event_handler);
MX_PRIM_DECL(mx_com_unregister_event_handler);
MX_PRIM_DECL(mx_all_controls);
MX_PRIM_DECL(mx_all_coclasses);
MX_PRIM_DECL(mx_find_element);
MX_PRIM_DECL(mx_document_objects);
MX_PRIM_DECL(mx_coclass_to_html);
MX_PRIM_DECL(mx_insert_html);
MX_PRIM_DECL(mx_append_html);
MX_PRIM_DECL(mx_replace_html);
MX_PRIM_DECL(mx_get_event);
MX_PRIM_DECL(mx_document_pred);

// elements

MX_PRIM_DECL(mx_element_insert_html);
MX_PRIM_DECL(mx_element_append_html);
MX_PRIM_DECL(mx_element_replace_html);
MX_PRIM_DECL(mx_element_insert_text);
MX_PRIM_DECL(mx_element_append_text);
MX_PRIM_DECL(mx_element_attribute);
MX_PRIM_DECL(mx_element_set_attribute);
MX_PRIM_DECL(mx_element_click);
MX_PRIM_DECL(mx_element_tag);
MX_PRIM_DECL(mx_element_font_family);
MX_PRIM_DECL(mx_element_set_font_family);
MX_PRIM_DECL(mx_element_font_style);
MX_PRIM_DECL(mx_element_set_font_style);
MX_PRIM_DECL(mx_element_font_variant);
MX_PRIM_DECL(mx_element_set_font_variant);
MX_PRIM_DECL(mx_element_font_weight);
MX_PRIM_DECL(mx_element_set_font_weight);
MX_PRIM_DECL(mx_element_font);
MX_PRIM_DECL(mx_element_set_font);
MX_PRIM_DECL(mx_element_background);
MX_PRIM_DECL(mx_element_set_background);
MX_PRIM_DECL(mx_element_background_image);
MX_PRIM_DECL(mx_element_set_background_image);
MX_PRIM_DECL(mx_element_background_repeat);
MX_PRIM_DECL(mx_element_set_background_repeat);
MX_PRIM_DECL(mx_element_background_position);
MX_PRIM_DECL(mx_element_set_background_position);
MX_PRIM_DECL(mx_element_text_decoration);
MX_PRIM_DECL(mx_element_set_text_decoration);
MX_PRIM_DECL(mx_element_text_transform);
MX_PRIM_DECL(mx_element_set_text_transform);
MX_PRIM_DECL(mx_element_text_align);
MX_PRIM_DECL(mx_element_set_text_align);
MX_PRIM_DECL(mx_element_margin);
MX_PRIM_DECL(mx_element_set_margin);
MX_PRIM_DECL(mx_element_padding);
MX_PRIM_DECL(mx_element_set_padding);
MX_PRIM_DECL(mx_element_border);
MX_PRIM_DECL(mx_element_set_border);
MX_PRIM_DECL(mx_element_border_top);
MX_PRIM_DECL(mx_element_set_border_top);
MX_PRIM_DECL(mx_element_border_bottom);
MX_PRIM_DECL(mx_element_set_border_bottom);
MX_PRIM_DECL(mx_element_border_left);
MX_PRIM_DECL(mx_element_set_border_left);
MX_PRIM_DECL(mx_element_border_right);
MX_PRIM_DECL(mx_element_set_border_right);
MX_PRIM_DECL(mx_element_border_color);
MX_PRIM_DECL(mx_element_set_border_color);
MX_PRIM_DECL(mx_element_border_width);
MX_PRIM_DECL(mx_element_set_border_width);
MX_PRIM_DECL(mx_element_border_style);
MX_PRIM_DECL(mx_element_set_border_style);
MX_PRIM_DECL(mx_element_border_top_style);
MX_PRIM_DECL(mx_element_set_border_top_style);
MX_PRIM_DECL(mx_element_border_bottom_style);
MX_PRIM_DECL(mx_element_set_border_bottom_style);
MX_PRIM_DECL(mx_element_border_left_style);
MX_PRIM_DECL(mx_element_set_border_left_style);
MX_PRIM_DECL(mx_element_border_right_style);
MX_PRIM_DECL(mx_element_set_border_right_style);
MX_PRIM_DECL(mx_element_style_float);
MX_PRIM_DECL(mx_element_set_style_float);
MX_PRIM_DECL(mx_element_clear);
MX_PRIM_DECL(mx_element_set_clear);
MX_PRIM_DECL(mx_element_display);
MX_PRIM_DECL(mx_element_set_display);
MX_PRIM_DECL(mx_element_visibility);
MX_PRIM_DECL(mx_element_set_visibility);
MX_PRIM_DECL(mx_element_list_style_type);
MX_PRIM_DECL(mx_element_set_list_style_type);
MX_PRIM_DECL(mx_element_list_style_position);
MX_PRIM_DECL(mx_element_set_list_style_position);
MX_PRIM_DECL(mx_element_list_style_image);
MX_PRIM_DECL(mx_element_set_list_style_image);
MX_PRIM_DECL(mx_element_list_style);
MX_PRIM_DECL(mx_element_set_list_style);
MX_PRIM_DECL(mx_element_whitespace);
MX_PRIM_DECL(mx_element_set_whitespace);
MX_PRIM_DECL(mx_element_position);
MX_PRIM_DECL(mx_element_overflow);
MX_PRIM_DECL(mx_element_set_overflow);
MX_PRIM_DECL(mx_element_pagebreak_before);
MX_PRIM_DECL(mx_element_set_pagebreak_before);
MX_PRIM_DECL(mx_element_pagebreak_after);
MX_PRIM_DECL(mx_element_set_pagebreak_after);
MX_PRIM_DECL(mx_element_css_text);
MX_PRIM_DECL(mx_element_set_css_text);
MX_PRIM_DECL(mx_element_cursor);
MX_PRIM_DECL(mx_element_set_cursor);
MX_PRIM_DECL(mx_element_clip);
MX_PRIM_DECL(mx_element_set_clip);
MX_PRIM_DECL(mx_element_filter);
MX_PRIM_DECL(mx_element_set_filter);
MX_PRIM_DECL(mx_element_style_string);
MX_PRIM_DECL(mx_element_text_decoration_none);
MX_PRIM_DECL(mx_element_set_text_decoration_none);
MX_PRIM_DECL(mx_element_text_decoration_underline);
MX_PRIM_DECL(mx_element_set_text_decoration_underline);
MX_PRIM_DECL(mx_element_text_decoration_overline);
MX_PRIM_DECL(mx_element_set_text_decoration_overline);
MX_PRIM_DECL(mx_element_text_decoration_linethrough);
MX_PRIM_DECL(mx_element_set_text_decoration_linethrough);
MX_PRIM_DECL(mx_element_text_decoration_blink);
MX_PRIM_DECL(mx_element_set_text_decoration_blink);
MX_PRIM_DECL(mx_element_pixel_top);
MX_PRIM_DECL(mx_element_set_pixel_top);
MX_PRIM_DECL(mx_element_pixel_left);
MX_PRIM_DECL(mx_element_set_pixel_left);
MX_PRIM_DECL(mx_element_pixel_width);
MX_PRIM_DECL(mx_element_set_pixel_width);
MX_PRIM_DECL(mx_element_pixel_height);
MX_PRIM_DECL(mx_element_set_pixel_height);
MX_PRIM_DECL(mx_element_pos_top);
MX_PRIM_DECL(mx_element_set_pos_top);
MX_PRIM_DECL(mx_element_pos_left);
MX_PRIM_DECL(mx_element_set_pos_left);
MX_PRIM_DECL(mx_element_pos_width);
MX_PRIM_DECL(mx_element_set_pos_width);
MX_PRIM_DECL(mx_element_pos_height);
MX_PRIM_DECL(mx_element_set_pos_height);
MX_PRIM_DECL(mx_element_font_size);
MX_PRIM_DECL(mx_element_set_font_size);
MX_PRIM_DECL(mx_element_color);
MX_PRIM_DECL(mx_element_set_color);
MX_PRIM_DECL(mx_element_background_color);
MX_PRIM_DECL(mx_element_set_background_color);
MX_PRIM_DECL(mx_element_background_position_x);
MX_PRIM_DECL(mx_element_set_background_position_x);
MX_PRIM_DECL(mx_element_background_position_y);
MX_PRIM_DECL(mx_element_set_background_position_y);
MX_PRIM_DECL(mx_element_word_spacing);
MX_PRIM_DECL(mx_element_set_word_spacing);
MX_PRIM_DECL(mx_element_letter_spacing);
MX_PRIM_DECL(mx_element_set_letter_spacing);
MX_PRIM_DECL(mx_element_vertical_align);
MX_PRIM_DECL(mx_element_set_vertical_align);
MX_PRIM_DECL(mx_element_text_indent);
MX_PRIM_DECL(mx_element_set_text_indent);
MX_PRIM_DECL(mx_element_line_height);
MX_PRIM_DECL(mx_element_set_line_height);
MX_PRIM_DECL(mx_element_margin_top);
MX_PRIM_DECL(mx_element_set_margin_top);
MX_PRIM_DECL(mx_element_margin_bottom);
MX_PRIM_DECL(mx_element_set_margin_bottom);
MX_PRIM_DECL(mx_element_margin_left);
MX_PRIM_DECL(mx_element_set_margin_left);
MX_PRIM_DECL(mx_element_margin_right);
MX_PRIM_DECL(mx_element_set_margin_right);
MX_PRIM_DECL(mx_element_padding_top);
MX_PRIM_DECL(mx_element_set_padding_top);
MX_PRIM_DECL(mx_element_padding_bottom);
MX_PRIM_DECL(mx_element_set_padding_bottom);
MX_PRIM_DECL(mx_element_padding_left);
MX_PRIM_DECL(mx_element_set_padding_left);
MX_PRIM_DECL(mx_element_padding_right);
MX_PRIM_DECL(mx_element_set_padding_right);
MX_PRIM_DECL(mx_element_border_top_color);
MX_PRIM_DECL(mx_element_set_border_top_color);
MX_PRIM_DECL(mx_element_border_bottom_color);
MX_PRIM_DECL(mx_element_set_border_bottom_color);
MX_PRIM_DECL(mx_element_border_left_color);
MX_PRIM_DECL(mx_element_set_border_left_color);
MX_PRIM_DECL(mx_element_border_right_color);
MX_PRIM_DECL(mx_element_set_border_right_color);
MX_PRIM_DECL(mx_element_border_top_width);
MX_PRIM_DECL(mx_element_set_border_top_width);
MX_PRIM_DECL(mx_element_border_bottom_width);
MX_PRIM_DECL(mx_element_set_border_bottom_width);
MX_PRIM_DECL(mx_element_border_left_width);
MX_PRIM_DECL(mx_element_set_border_left_width);
MX_PRIM_DECL(mx_element_border_right_width);
MX_PRIM_DECL(mx_element_set_border_right_width);
MX_PRIM_DECL(mx_element_width);
MX_PRIM_DECL(mx_element_set_width);
MX_PRIM_DECL(mx_element_height);
MX_PRIM_DECL(mx_element_set_height);
MX_PRIM_DECL(mx_element_top);
MX_PRIM_DECL(mx_element_set_top);
MX_PRIM_DECL(mx_element_left);
MX_PRIM_DECL(mx_element_set_left);
MX_PRIM_DECL(mx_element_z_index);
MX_PRIM_DECL(mx_element_set_z_index);

// HTML events

MX_PRIM_DECL(mx_event_keypress_pred);
MX_PRIM_DECL(mx_event_keydown_pred);
MX_PRIM_DECL(mx_event_keyup_pred);
MX_PRIM_DECL(mx_event_mousedown_pred);
MX_PRIM_DECL(mx_event_mouseover_pred);
MX_PRIM_DECL(mx_event_mousemove_pred);
MX_PRIM_DECL(mx_event_mouseout_pred);
MX_PRIM_DECL(mx_event_mouseup_pred);
MX_PRIM_DECL(mx_event_click_pred);
MX_PRIM_DECL(mx_event_dblclick_pred);
MX_PRIM_DECL(mx_event_error_pred);
MX_PRIM_DECL(mx_event_tag);
MX_PRIM_DECL(mx_event_id);
MX_PRIM_DECL(mx_event_from_tag);
MX_PRIM_DECL(mx_event_from_id);
MX_PRIM_DECL(mx_event_to_tag);
MX_PRIM_DECL(mx_event_to_id);
MX_PRIM_DECL(mx_event_keycode);
MX_PRIM_DECL(mx_event_shiftkey);
MX_PRIM_DECL(mx_event_ctrlkey);
MX_PRIM_DECL(mx_event_altkey);
MX_PRIM_DECL(mx_event_x);
MX_PRIM_DECL(mx_event_y);
MX_PRIM_DECL(mx_event_pred);
MX_PRIM_DECL(mx_event_keypress_pred);
MX_PRIM_DECL(mx_event_keydown_pred);
MX_PRIM_DECL(mx_event_keyup_pred);
MX_PRIM_DECL(mx_event_mousedown_pred);
MX_PRIM_DECL(mx_event_mouseover_pred);
MX_PRIM_DECL(mx_event_mouseout_pred);
MX_PRIM_DECL(mx_event_mouseup_pred);
MX_PRIM_DECL(mx_event_click_pred);
MX_PRIM_DECL(mx_event_dblclick_pred);
MX_PRIM_DECL(mx_event_error_pred);
MX_PRIM_DECL(mx_block_until_event);
MX_PRIM_DECL(mx_process_win_events);
MX_PRIM_DECL(mx_make_document);
MX_PRIM_DECL(mx_document_show);
MX_PRIM_DECL(mx_release_type_table);
  
void mx_register_com_object(Scheme_Object *,IDispatch *);
void mx_register_simple_com_object(Scheme_Object *,IUnknown *);
void codedComError(char *,HRESULT);
IHTMLElementCollection *getBodyElementsWithTag(IHTMLElement *,char *);
IDispatch *getElementInCollection(IHTMLElementCollection *,int);
IDispatch *getObjectInCollection(IHTMLElementCollection *,int);
BSTR schemeStringToBSTR(Scheme_Object *);
BSTR stringToBSTR(char *,size_t);
Scheme_Object *BSTRToSchemeString(BSTR);
Scheme_Object *variantToSchemeObject(VARIANTARG *);
void marshallSchemeValueToVariant(Scheme_Object *,VARIANTARG *);
void initEventNames(void);
IHTMLElement *findBodyElement(IHTMLDocument2 *,char *,char *);
CLSID getCLSIDFromString(const char *);
ITypeInfo *eventTypeInfoFromComObject(MX_COM_Object *);

// array procedures

Scheme_Object *safeArrayToSchemeVector(SAFEARRAY *);
SAFEARRAY *schemeVectorToSafeArray(Scheme_Object *);
