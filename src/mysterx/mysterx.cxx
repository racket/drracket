// mysterx.cxx : COM/ActiveX/DHTML extension for MzScheme
// Author: Paul Steckler

#include "stdafx.h"

#include <stdio.h>
#include <malloc.h>
#include <float.h>
#include <limits.h>
#include <io.h>

#define _WIN32_DCOM

#include <objbase.h>
#include <mshtml.h>
#include <initguid.h>
#include <winnls.h>
#include <exdisp.h>
#include <shellapi.h>
#include <htmlhelp.h>

#include "resource.h"

#include "escheme.h"

#include "bstr.h"

// ATL support

#include <atlbase.h>
extern CComModule _Module;
#include <atlcom.h>
#include <atlhost.h>
CComModule _Module;

// end ATL support

#include "myspage.h"
#include "myspage_i.c"
#include "myssink.h"
#include "myssink_i.c"

#include "mysterx.h"

static WNDPROC AtlWndProc;

HINSTANCE hInstance;
HICON hIcon;
HANDLE browserHwndMutex;
HANDLE createHwndSem;
HANDLE eventSinkMutex;

const CLSID emptyClsId;

static Scheme_Unit *mx_unit;  /* the unit returned by the extension */

static Scheme_Object *mx_omit_obj; /* omitted argument placeholder */

static MX_TYPE_TBL_ENTRY *typeTable[TYPE_TBL_SIZE];

MYSSINK_TABLE myssink_table;

static char *objectAttributes[] = { "InprocServer", "InprocServer32",
				    "LocalServer", "LocalServer32", NULL };
static char *controlAttributes[] = { "Control", NULL };

static MX_PRIM mxPrims[] = {

  // COM reflection
  
  { mx_com_invoke,"com-invoke",2,-1}, 
  { mx_com_set_property,"com-set-property!",2,-1 },
  { mx_com_get_property,"com-get-property",2, -1 },
  { mx_com_methods,"com-methods",1,1 },
  { mx_com_get_properties,"com-get-properties",1,1 },
  { mx_com_set_properties,"com-set-properties", 1, 1 },
  { mx_com_events,"com-events",1,1 },
  { mx_com_method_type,"com-method-type",2,2 },
  { mx_com_get_property_type,"com-get-property-type",2,2 },
  { mx_com_set_property_type,"com-set-property-type",2,2 },
  { mx_com_event_type,"com-event-type",2,2 },
  { mx_com_help,"com-help",1,2 },

  // COM types

  { mx_com_get_object_type,"com-object-type",1,1 },
  { mx_com_is_a,"com-is-a?",2,2 },
  
  // COM events
  
  { mx_com_register_event_handler,"com-register-event-handler",3,3 },
  { mx_com_unregister_event_handler,"com-unregister-event-handler",2,2 },
  
  // coclasses
  
  { mx_all_coclasses,"com-all-coclasses",0,0 },
  { mx_all_controls,"com-all-controls",0,0 },
  { mx_coclass_to_html,"coclass->html",3,4 },
  
  // COM objects
  
  { mx_cocreate_instance_from_coclass,"cocreate-instance-from-coclass",1,2 },
  { mx_cocreate_instance_from_progid,"cocreate-instance-from-progid",1,2 },
  { mx_coclass,"coclass",1,1 },
  { mx_coclass_as_progid,"coclass-as-progid",1,1 },
  { mx_set_coclass,"set-coclass!",2,2 },
  { mx_set_coclass_from_progid,"set-coclass-from-progid!",2,2 },
  { mx_com_object_pred,"com-object?",1,1 },
  { mx_com_object_eq,"com-object-eq?",2,2 },
  { mx_com_register_object,"com-register-object",1,1 },  
  { mx_com_release_object,"com-release-object",1,1 },  

  // COM termination

  { mx_com_terminate,"com-terminate",0,0 },  

  // browsers

  { mx_make_browser,"make-browser",6,6},
  { mx_browser_show,"browser-show",2,2},
  { mx_navigate,"navigate",2,2 },
  { mx_go_back,"go-back",1,1 },
  { mx_go_forward,"go-forward",1,1 },
  { mx_current_url,"current-url",1,1 },
  { mx_register_navigate_handler,"register-navigate-handler",2,2 },
  { mx_unregister_navigate_handler,"unregister-navigate-handler",1,1 },
  { mx_current_document,"current-document",1,1 },

  // documents 
  
  { mx_document_pred,"document?",1,1 },
  { mx_insert_html,"document-insert-html",2,2 },
  { mx_append_html,"document-append-html",2,2 },
  { mx_replace_html,"document-replace-html",2,2 },
  { mx_find_element,"document-find-element",3,3 },
  { mx_find_element_by_id_or_name,"document-find-element-by-id-or-name",2,2 },
  { mx_elements_with_tag,"document-elements-with-tag",2,2 },
  { mx_document_objects,"document-objects",1,1 },
  
  // elements
  
  { mx_element_insert_html,"element-insert-html",2,2 },
  { mx_element_append_html,"element-append-html",2,2 },
  { mx_element_insert_text,"element-insert-text",2,2 },
  { mx_element_append_text,"element-append-text",2,2 },
  { mx_element_replace_html,"element-replace-html",2,2 },
  { mx_element_focus,"element-focus",1,1 },
  { mx_element_selection,"element-selection",1,1 },
  { mx_element_set_selection,"element-set-selection!",2,2 },
  { mx_element_attribute,"element-attribute",2,2 },
  { mx_element_set_attribute,"element-set-attribute!",3,3 },
  { mx_element_click,"element-click",1,1 },
  { mx_element_tag,"element-tag",1,1 },
  { mx_element_font_family,"element-font-family",1,1 },
  { mx_element_set_font_family,"element-set-font-family!",2,2 },
  { mx_element_font_style,"element-font-style",1,1 },
  { mx_element_set_font_style,"element-set-font-style!",2,2 },
  { mx_element_font_variant,"element-font-variant",1,1 },
  { mx_element_set_font_variant,"element-set-font-variant!",2,2 },
  { mx_element_font_weight,"element-font-weight",1,1 },
  { mx_element_set_font_weight,"element-set-font-weight!",2,2 },
  { mx_element_font,"element-font",1,1 },
  { mx_element_set_font,"element-set-font!",2,2 }, 
  { mx_element_background,"element-background",1,1 },
  { mx_element_set_background,"element-set-background!",2,2 }, 
  { mx_element_background_attachment,"element-background-attachment",1,1 },
  { mx_element_set_background_attachment,"element-set-background-attachment!",2,2 },
  { mx_element_background_image,"element-background-image",1,1 },
  { mx_element_set_background_image,"element-set-background-image!",2,2 },
  { mx_element_background_repeat,"element-background-repeat",1,1 },
  { mx_element_set_background_repeat,"element-set-background-repeat!",2,2 },
  { mx_element_background_position,"element-background-position",1,1 },
  { mx_element_set_background_position,"element-set-background-position!",2,2 }, 
  { mx_element_text_decoration,"element-text-decoration",1,1 },
  { mx_element_set_text_decoration,"element-set-text-decoration!",2,2 }, 
  { mx_element_text_transform,"element-text-transform",1,1 },
  { mx_element_set_text_transform,"element-set-text-transform!",2,2 }, 
  { mx_element_text_align,"element-text-align",1,1 },
  { mx_element_set_text_align,"element-set-text-align!",2,2 }, 
  { mx_element_margin,"element-margin",1,1 },
  { mx_element_set_margin,"element-set-margin!",2,2 }, 
  { mx_element_padding,"element-padding",1,1 },
  { mx_element_set_padding,"element-set-padding!",2,2 }, 
  { mx_element_border,"element-border",1,1 },
  { mx_element_set_border,"element-set-border!",2,2 }, 
  { mx_element_border_top,"element-border-top",1,1 },
  { mx_element_set_border_top,"element-set-border-top!",2,2 }, 
  { mx_element_border_bottom,"element-border-bottom",1,1 },
  { mx_element_set_border_bottom,"element-set-border-bottom!",2,2 }, 
  { mx_element_border_left,"element-border-left",1,1 },
  { mx_element_set_border_left,"element-set-border-left!",2,2 }, 
  { mx_element_border_right,"element-border-right",1,1 },
  { mx_element_set_border_right,"element-set-border-right!",2,2 }, 
  { mx_element_border_color,"element-border-color",1,1 },
  { mx_element_set_border_color,"element-set-border-color!",2,2 }, 
  { mx_element_border_width,"element-border-width",1,1 },
  { mx_element_set_border_width,"element-set-border-width!",2,2 }, 
  { mx_element_border_style,"element-border-style",1,1 },
  { mx_element_set_border_style,"element-set-border-style!",2,2 }, 
  { mx_element_border_top_style,"element-border-top-style",1,1 },
  { mx_element_set_border_top_style,"element-set-border-top-style!",2,2 }, 
  { mx_element_border_bottom_style,"element-border-bottom-style",1,1 },
  { mx_element_set_border_bottom_style,"element-set-border-bottom-style!",2,2 }, 
  { mx_element_border_left_style,"element-border-left-style",1,1 },
  { mx_element_set_border_left_style,"element-set-border-left-style!",2,2 }, 
  { mx_element_border_right_style,"element-border-right-style",1,1 },
  { mx_element_set_border_right_style,"element-set-border-right-style!",2,2 }, 
  { mx_element_style_float,"element-style-float",1,1 },
  { mx_element_set_style_float,"element-set-style-float!",2,2 }, 
  { mx_element_clear,"element-clear",1,1 },
  { mx_element_set_clear,"element-set-clear!",2,2 }, 
  { mx_element_display,"element-display",1,1 },
  { mx_element_set_display,"element-set-display!",2,2 }, 
  { mx_element_visibility,"element-visibility",1,1 },
  { mx_element_set_visibility,"element-set-visibility!",2,2 }, 
  { mx_element_list_style_type,"element-list-style-type",1,1 },
  { mx_element_set_list_style_type,"element-set-list-style-type!",2,2 }, 
  { mx_element_list_style_position,"element-list-style-position",1,1 },
  { mx_element_set_list_style_position,"element-set-list-style-position!",2,2 }, 
  { mx_element_list_style_image,"element-list-style-image",1,1 },
  { mx_element_set_list_style_image,"element-set-list-style-image!",2,2 }, 
  { mx_element_list_style,"element-list-style",1,1 },
  { mx_element_set_list_style,"element-set-list-style!",2,2 }, 
  { mx_element_position,"element-position",1,1 },
  { mx_element_overflow,"element-overflow",1,1 },
  { mx_element_set_overflow,"element-set-overflow!",2,2 }, 
  { mx_element_pagebreak_before,"element-pagebreak-before",1,1 },
  { mx_element_set_pagebreak_before,"element-set-pagebreak-before!",2,2 }, 
  { mx_element_pagebreak_after,"element-pagebreak-after",1,1 },
  { mx_element_set_pagebreak_after,"element-set-pagebreak-after!",2,2 }, 
  { mx_element_css_text,"element-css-text",1,1 },
  { mx_element_set_css_text,"element-set-css-text!",2,2 }, 
  { mx_element_cursor,"element-cursor",1,1 },
  { mx_element_set_cursor,"element-set-cursor!",2,2 }, 
  { mx_element_clip,"element-clip",1,1 },
  { mx_element_set_clip,"element-set-clip!",2,2 }, 
  { mx_element_filter,"element-filter",1,1 },
  { mx_element_set_filter,"element-set-filter!",2,2 }, 
  { mx_element_style_string,"element-style-string",1,1 },
  { mx_element_text_decoration_none,"element-text-decoration-none",1,1 },
  { mx_element_set_text_decoration_none,"element-set-text-decoration-none!",2,2 }, 
  { mx_element_text_decoration_underline,"element-text-decoration-underline",1,1 },
  { mx_element_set_text_decoration_underline,"element-set-text-decoration-underline!",2,2 }, 
  { mx_element_text_decoration_overline,"element-text-decoration-overline",1,1 },
  { mx_element_set_text_decoration_overline,"element-set-text-decoration-overline!",2,2 }, 
  { mx_element_text_decoration_linethrough,"element-text-decoration-linethrough",1,1 },
  { mx_element_set_text_decoration_linethrough,"element-set-text-decoration-linethrough!",2,2 }, 
  { mx_element_text_decoration_blink,"element-text-decoration-blink",1,1 },
  { mx_element_set_text_decoration_blink,"element-set-text-decoration-blink!",2,2 }, 
  { mx_element_pixel_top,"element-pixel-top",1,1 },
  { mx_element_set_pixel_top,"element-set-pixel-top!",2,2 }, 
  { mx_element_pixel_left,"element-pixel-left",1,1 },
  { mx_element_set_pixel_left,"element-set-pixel-left!",2,2 }, 
  { mx_element_pixel_width,"element-pixel-width",1,1 },
  { mx_element_set_pixel_width,"element-set-pixel-width!",2,2 }, 
  { mx_element_pixel_height,"element-pixel-height",1,1 },
  { mx_element_set_pixel_height,"element-set-pixel-height!",2,2 }, 
  { mx_element_pos_top,"element-pos-top",1,1 },
  { mx_element_set_pos_top,"element-set-pos-top!",2,2 }, 
  { mx_element_pos_left,"element-pos-left",1,1 },
  { mx_element_set_pos_left,"element-set-pos-left!",2,2 }, 
  { mx_element_pos_width,"element-pos-width",1,1 },
  { mx_element_set_pos_width,"element-set-pos-width!",2,2 }, 
  { mx_element_pos_height,"element-pos-height",1,1 },
  { mx_element_set_pos_height,"element-set-pos-height!",2,2 }, 
  { mx_element_font_size,"element-font-size",1,1 },
  { mx_element_set_font_size,"element-set-font-size!",2,2 }, 
  { mx_element_color,"element-color",1,1 },
  { mx_element_set_color,"element-set-color!",2,2 }, 
  { mx_element_background_color,"element-background-color",1,1 },
  { mx_element_set_background_color,"element-set-background-color!",2,2 }, 
  { mx_element_background_position_x,"element-background-position-x",1,1 },
  { mx_element_set_background_position_x,"element-set-background-position-x!",2,2 }, 
  { mx_element_background_position_y,"element-background-position-y",1,1 },
  { mx_element_set_background_position_y,"element-set-background-position-y!",2,2 }, 
  { mx_element_letter_spacing,"element-letter-spacing",1,1 },
  { mx_element_set_letter_spacing,"element-set-letter-spacing!",2,2 }, 
  { mx_element_vertical_align,"element-vertical-align",1,1 },
  { mx_element_set_vertical_align,"element-set-vertical-align!",2,2 }, 
  { mx_element_text_indent,"element-text-indent",1,1 },
  { mx_element_set_text_indent,"element-set-text-indent!",2,2 }, 
  { mx_element_line_height,"element-line-height",1,1 },
  { mx_element_set_line_height,"element-set-line-height!",2,2 }, 
  { mx_element_margin_top,"element-margin-top",1,1 },
  { mx_element_set_margin_top,"element-set-margin-top!",2,2 }, 
  { mx_element_margin_bottom,"element-margin-bottom",1,1 },
  { mx_element_set_margin_bottom,"element-set-margin-bottom!",2,2 }, 
  { mx_element_margin_left,"element-margin-left",1,1 },
  { mx_element_set_margin_left,"element-set-margin-left!",2,2 }, 
  { mx_element_margin_right,"element-margin-right",1,1 },
  { mx_element_set_margin_right,"element-set-margin-right!",2,2 }, 
  { mx_element_padding_top,"element-padding-top",1,1 },
  { mx_element_set_padding_top,"element-set-padding-top!",2,2 }, 
  { mx_element_padding_bottom,"element-padding-bottom",1,1 },
  { mx_element_set_padding_bottom,"element-set-padding-bottom!",2,2 }, 
  { mx_element_padding_left,"element-padding-left",1,1 },
  { mx_element_set_padding_left,"element-set-padding-left!",2,2 }, 
  { mx_element_padding_right,"element-padding-right",1,1 },
  { mx_element_set_padding_right,"element-set-padding-right!",2,2 }, 
  { mx_element_border_top_color,"element-border-top-color",1,1 },
  { mx_element_set_border_top_color,"element-set-border-top-color!",2,2 }, 
  { mx_element_border_bottom_color,"element-border-bottom-color",1,1 },
  { mx_element_set_border_bottom_color,"element-set-border-bottom-color!",2,2 }, 
  { mx_element_border_left_color,"element-border-left-color",1,1 },
  { mx_element_set_border_left_color,"element-set-border-left-color!",2,2 }, 
  { mx_element_border_right_color,"element-border-right-color",1,1 },
  { mx_element_set_border_right_color,"element-set-border-right-color!",2,2 }, 
  { mx_element_border_top_width,"element-border-top-width",1,1 },
  { mx_element_set_border_top_width,"element-set-border-top-width!",2,2 }, 
  { mx_element_border_bottom_width,"element-border-bottom-width",1,1 },
  { mx_element_set_border_bottom_width,"element-set-border-bottom-width!",2,2 }, 
  { mx_element_border_left_width,"element-border-left-width",1,1 },
  { mx_element_set_border_left_width,"element-set-border-left-width!",2,2 }, 
  { mx_element_border_right_width,"element-border-right-width",1,1 },
  { mx_element_set_border_right_width,"element-set-border-right-width!",2,2 }, 
  { mx_element_width,"element-width",1,1 },
  { mx_element_set_width,"element-set-width!",2,2 }, 
  { mx_element_height,"element-height",1,1 },
  { mx_element_set_height,"element-set-height!",2,2 }, 
  { mx_element_top,"element-top",1,1 },
  { mx_element_set_top,"element-set-top!",2,2 }, 
  { mx_element_left,"element-left",1,1 },
  { mx_element_set_left,"element-set-left!",2,2 }, 
  { mx_element_z_index,"element-z-index",1,1 },
  { mx_element_set_z_index,"element-set-z-index!",2,2 }, 
  
  // events
  
  { mx_event_pred,"event?",1,1 },
  { mx_get_event,"get-event",1,1 },
  { mx_event_tag,"event-tag",1,1},
  { mx_event_id,"event-id",1,1},
  { mx_event_from_tag,"event-from-tag",1,1},
  { mx_event_from_id,"event-from-id",1,1},
  { mx_event_to_tag,"event-to-tag",1,1},
  { mx_event_to_id,"event-to-id",1,1},
  { mx_event_keycode,"event-keycode",1,1},
  { mx_event_shiftkey,"event-shiftkey",1,1},
  { mx_event_shiftkey,"event-ctrlkey",1,1},
  { mx_event_altkey,"event-altkey",1,1},
  { mx_event_x,"event-x",1,1},
  { mx_event_y,"event-y",1,1},
  { mx_event_keypress_pred,"event-keypress?",1,1},
  { mx_event_keydown_pred,"event-keydown?",1,1},
  { mx_event_keyup_pred,"event-keyup?",1,1},
  { mx_event_mousedown_pred,"event-mousedown?",1,1},
  { mx_event_mousemove_pred,"event-mousemove?",1,1},
  { mx_event_mouseover_pred,"event-mouseover?",1,1},
  { mx_event_mouseout_pred,"event-mouseout?",1,1},
  { mx_event_mouseup_pred,"event-mouseup?",1,1},
  { mx_event_click_pred,"event-click?",1,1},
  { mx_event_dblclick_pred,"event-dblclick?",1,1},
  { mx_event_error_pred,"event-error?",1,1},
  { mx_block_until_event,"block-until-event",1,1},
  { mx_process_win_events,"process-win-events",0,0},

  // type table

  { mx_release_type_table,"release-type-table",0,0},
};

BOOL isEmptyClsId(CLSID clsId) {
  if (memcmp(&clsId,&emptyClsId,sizeof(CLSID)) == 0) {
    return TRUE;
  }
  else {
    return FALSE;
  }
}

void scheme_release_typedesc(void *p,void *) {
  MX_TYPEDESC *pTypeDesc;
  ITypeInfo *pITypeInfo;

  pTypeDesc = (MX_TYPEDESC *)p;

  if (MX_MANAGED_OBJ_RELEASED(pTypeDesc)) {
    return;
  }

  pITypeInfo = pTypeDesc->pITypeInfo;

  if (pTypeDesc->descKind == funcDesc) {
    pITypeInfo->ReleaseFuncDesc(pTypeDesc->pFuncDesc);
  }
  else if (pTypeDesc->descKind == varDesc) {
    pITypeInfo->ReleaseVarDesc(pTypeDesc->pVarDesc);
  }

  pITypeInfo->Release();

  MX_MANAGED_OBJ_RELEASED(pTypeDesc) = TRUE;
}

void scheme_release_com_object(void *comObject,void *pIDispatch) {
  ITypeInfo *pITypeInfo;
  ITypeInfo *pEventTypeInfo;
  IConnectionPoint *pIConnectionPoint;
  ISink *pISink;

  if (MX_MANAGED_OBJ_RELEASED(comObject)) {
    return;
  }

  // when COM object GC'd, release associated interfaces

  pITypeInfo = MX_COM_OBJ_TYPEINFO(comObject);

  pEventTypeInfo = MX_COM_OBJ_EVENTTYPEINFO(comObject);
  pIConnectionPoint = MX_COM_OBJ_CONNECTIONPOINT(comObject);
  pISink = MX_COM_OBJ_EVENTSINK(comObject);

  if (pITypeInfo) {
    pITypeInfo->Release();
  }

  if (pEventTypeInfo) {
    pEventTypeInfo->Release();
  }

  if (pIConnectionPoint) {
    pIConnectionPoint->Release();
  }

  if (pISink) {
    pISink->Release();
  }

  if (pIDispatch) {
    ((IDispatch *)pIDispatch)->Release();
  }

  MX_MANAGED_OBJ_RELEASED(comObject) = TRUE;
}

void mx_register_com_object(Scheme_Object *obj,IDispatch *pIDispatch) {
  scheme_register_finalizer(obj,scheme_release_com_object,pIDispatch,NULL,NULL);
  scheme_add_managed((Scheme_Manager *)scheme_get_param(scheme_config,MZCONFIG_MANAGER),
		     (Scheme_Object *)obj,
		     (Scheme_Close_Manager_Client *)scheme_release_com_object,
		     pIDispatch,0);
}

Scheme_Object *mx_com_register_object(int argc,Scheme_Object **argv) {
  if (MX_COM_OBJP(argv[0]) == FALSE) {
    scheme_wrong_type("com-register-com-object","com-object",0,argc,argv);
  }

  mx_register_com_object(argv[0],MX_COM_OBJ_VAL(argv[0]));

  return scheme_void;
}

void scheme_release_simple_com_object(void *comObject,void *pIUnknown) {

  if (MX_MANAGED_OBJ_RELEASED(comObject)) {
    return;
  }

  if (pIUnknown) {
    ((IUnknown *)pIUnknown)->Release();
  }

  MX_MANAGED_OBJ_RELEASED(comObject) = TRUE;
}

void mx_register_simple_com_object(Scheme_Object *obj,IUnknown *pIUnknown) {
  scheme_register_finalizer(obj,scheme_release_simple_com_object,pIUnknown,NULL,NULL);
  scheme_add_managed((Scheme_Manager *)scheme_get_param(scheme_config,MZCONFIG_MANAGER),
		     (Scheme_Object *)obj,
		     (Scheme_Close_Manager_Client *)scheme_release_simple_com_object,
		     pIUnknown,0);
}

void scheme_release_browser(void *wb,void *) {

  if (MX_MANAGED_OBJ_RELEASED(wb)) {
    return;
  }

  if (((MX_Browser_Object *)wb)->pIWebBrowser2) {
    ((MX_Browser_Object *)wb)->pIWebBrowser2->Release();
  }

  if (((MX_Browser_Object *)wb)->pISink) {
    ((MX_Browser_Object *)wb)->pISink->Release();
  }

  if (((MX_Browser_Object *)wb)->pIEventQueue) {
    ((MX_Browser_Object *)wb)->pIEventQueue->Release();
  } 

  MX_MANAGED_OBJ_RELEASED(wb) = TRUE;
}

void scheme_release_document(void *doc,void *) {

  if (MX_MANAGED_OBJ_RELEASED(doc)) {
    return;
  }

  if (((MX_Document_Object *)doc)->pIHTMLDocument2) {
    ((MX_Document_Object *)doc)->pIHTMLDocument2->Release();
  }

  MX_MANAGED_OBJ_RELEASED(doc) = TRUE;
}

Scheme_Object *mx_com_release_object(int argc,Scheme_Object **argv) {
  if (MX_COM_OBJP(argv[0]) == FALSE) {
    scheme_wrong_type("com-release-object","com-object",0,argc,argv);
  }

  scheme_release_com_object((void *)argv[0],MX_COM_OBJ_VAL(argv[0]));

  return scheme_void;
}

char *inv_kind_string(INVOKEKIND invKind) {
  switch (invKind) {
  case INVOKE_FUNC :
    return "method";
  case INVOKE_PROPERTYGET :
  case INVOKE_PROPERTYPUT :
    return "property";
  case INVOKE_EVENT :
    return "event";
  }
  
  return NULL;
}

char *mx_fun_string(INVOKEKIND invKind) {
  switch (invKind) {
  case INVOKE_FUNC :
    return "com-invoke";
  case INVOKE_PROPERTYGET :
    return "com-get-property";
  case INVOKE_PROPERTYPUT :
    return "com-set-property!";
  }
  
  return NULL;
}

unsigned short getHashValue(IDispatch *pIDispatch,INVOKEKIND invKind,
			    char *name) {
  char *p;
  unsigned short hashVal;
  
  hashVal = (unsigned short)pIDispatch + invKind;
  
  p = name;
  while (*p) {
    hashVal += (unsigned short)(*p);
    p++;
  }
  
  return hashVal % TYPE_TBL_SIZE;
  
}

void addTypeToTable(IDispatch *pIDispatch,char *name,
		    INVOKEKIND invKind,
		    MX_TYPEDESC *pTypeDesc) {
  unsigned short hashVal;
  MX_TYPE_TBL_ENTRY *pEntry,*p;

  // we don't call AddRef() for the IDispatch pointer
  // because it's not used as an interface, only its
  // pointer value is used, for hashing

  pEntry = (MX_TYPE_TBL_ENTRY *)scheme_malloc(sizeof(MX_TYPE_TBL_ENTRY));
  scheme_dont_gc_ptr(pEntry);
  pEntry->pTypeDesc = pTypeDesc;
  pEntry->pIDispatch = pIDispatch;
  pEntry->invKind = invKind;
  pEntry->name = name;
  pEntry->next = NULL;

  hashVal = getHashValue(pIDispatch,invKind,name);
  
  p = typeTable[hashVal];
  
  if (p == NULL) {
    typeTable[hashVal] = pEntry;
  }
  else {
    while (p->next != NULL) {
      p = p->next;
    }
    p->next = pEntry; 
  }
}

MX_TYPEDESC *lookupTypeDesc(IDispatch *pIDispatch,char *name,
			    INVOKEKIND invKind) {
  unsigned short hashVal;
  MX_TYPE_TBL_ENTRY *p;
  
  hashVal = getHashValue(pIDispatch,invKind,name);
  
  p = typeTable[hashVal];
  
  while (p) {
    if (p->pIDispatch == pIDispatch && 
	p->invKind == invKind &&
	strcmp(p->name,name) == 0) {
      return p->pTypeDesc;
    }
    p = p->next;
  }
  
  return NULL;
}

Scheme_Object *mx_release_type_table(int argc,Scheme_Object **argv) {
  int i;
  MX_TYPE_TBL_ENTRY *p,*psave;

  for (i = 0; i < sizeray(typeTable); i++) {
    p = typeTable[i];
  
    while (p) {
      scheme_release_typedesc((void *)p->pTypeDesc,NULL);
      psave = p;
      p = p->next;
      scheme_gc_ptr_ok(psave);
    }
  }

  return scheme_void;
}

Scheme_Object *mx_unit_init(Scheme_Object **boxes,Scheme_Object **anchors,
			    Scheme_Unit *m,void *debug_request) {
  int i;

  for (i = 0; i < sizeray(mxPrims); i++) {
    SCHEME_ENVBOX_VAL(boxes[i]) = scheme_make_prim_w_arity(mxPrims[i].c_fun,
							   mxPrims[i].name,
							   mxPrims[i].minargs,
							   mxPrims[i].maxargs);
    anchors[i] = boxes[i];
  }
  
  mx_omit_obj = (Scheme_Object *)scheme_malloc(sizeof(MX_OMIT));
  mx_omit_obj->type = mx_com_omit_type;
  
  SCHEME_ENVBOX_VAL(boxes[sizeray(mxPrims)]) = mx_omit_obj;
  anchors[sizeray(mxPrims)] = boxes[sizeray(mxPrims)];

  return scheme_void;
}

void codedComError(char *s,HRESULT hr) {
  char buff[1024];
  char finalBuff[2048];

  if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,
		    0,hr,0,buff,sizeof(buff),NULL) > 0) {
    sprintf(finalBuff,"%s, code = %X: %s",s,hr,buff);
  }
  else {
    sprintf(finalBuff,"%s, code = %X",s,hr);    
  }

  scheme_signal_error(finalBuff);
}

Scheme_Object *do_cocreate_instance(CLSID clsId,char *name,char *location,
				    char *machine) {
  HRESULT hr;
  IDispatch *pIDispatch;
  MX_COM_Object *com_object;
  
  if (stricmp(location,"local") == 0) {
    hr = CoCreateInstance(clsId,NULL,
			  CLSCTX_LOCAL_SERVER | CLSCTX_INPROC_SERVER,
			  IID_IDispatch,(void **)&pIDispatch);
  }
  else if (stricmp(location,"remote") == 0) {
    COSERVERINFO csi;
    MULTI_QI mqi;
    OLECHAR machineBuff[1024];

    if (machine) {
      int len;
      int count;

      csi.dwReserved1 = 0;
      csi.dwReserved2 = 0;
      csi.pAuthInfo = NULL;

      len = strlen(machine);
      count = MultiByteToWideChar(CP_ACP,(DWORD)0,
				  machine,len,
				  machineBuff,
				  sizeray(machineBuff) - 1);
      machineBuff[len] = '\0';  

      if (count < len) {
	scheme_signal_error("cocreate-instance-from-{coclass,progid}: "
			    "Unable to translate machine name to Unicode");
      }

      csi.pwszName = machineBuff;
      
    }

    mqi.pIID = &IID_IDispatch;
    mqi.pItf = NULL;
    mqi.hr = 0; 

    hr = CoCreateInstanceEx(clsId,NULL,
			    CLSCTX_REMOTE_SERVER,
			    machine ? &csi : NULL,
			    1,&mqi);
			    
    pIDispatch = (IDispatch *)(mqi.pItf);
  
    if (mqi.hr != S_OK || pIDispatch == NULL) {
      codedComError("cocreate-instance-from-{coclass,progid}: "
		    "Unable to obtain IDispatch interface for remote server",
		    hr);
    }

  }
  else {
    scheme_signal_error("cocreate-instance-from-{coclass,progid}: "
			"Expected 'local, 'remote, or machine name for 2nd argument, "
			"got '%s",location); 
  }

  if (hr != ERROR_SUCCESS) {
    char errBuff[2048];
    sprintf(errBuff,"cocreate-instance-from-{coclass,progid}: Unable to create instance of %s",
	    name);
    codedComError(errBuff,hr);
  }
  
  com_object = (MX_COM_Object *)scheme_malloc(sizeof(MX_COM_Object));
  
  com_object->type = mx_com_object_type; 
  com_object->pIDispatch = pIDispatch;
  com_object->pITypeInfo = NULL;
  com_object->clsId = clsId;
  com_object->pEventTypeInfo = NULL;
  com_object->pIConnectionPoint = NULL;
  com_object->pISink = NULL;
  com_object->connectionCookie = (DWORD)0;
  com_object->released = FALSE;
  
  mx_register_com_object((Scheme_Object *)com_object,pIDispatch);
  
  return (Scheme_Object *)com_object;
}

void bindCocreateLocation(int argc,Scheme_Object **argv,
			  char **pLocation,char **pMachine,
			  char *f) {
  if (argc == 2) {
    if (SCHEME_SYMBOLP(argv[1])) {
      *pLocation = SCHEME_SYM_VAL(argv[1]);
      *pMachine = NULL;
    }
    else if (SCHEME_STRINGP(argv[1])) {
      *pLocation = "remote";
      *pMachine = SCHEME_STR_VAL(argv[1]);
    }
    else {
      scheme_wrong_type(f,"symbol or string",0,argc,argv);
    }
  }
  else {
    *pLocation = "local";
    *pMachine = NULL;
  }
}

Scheme_Object *mx_cocreate_instance_from_coclass(int argc,Scheme_Object **argv) {
  char *coclass;
  CLSID clsId;
  char *location;
  char *machine;

  if (SCHEME_STRINGP(argv[0]) == FALSE) {
    scheme_wrong_type("cocreate-instance-from-coclass","string",0,argc,argv);
  }

  bindCocreateLocation(argc,argv,&location,&machine,
		       "cocreate-instance-from-coclass");

  coclass = SCHEME_STR_VAL(argv[0]);
  clsId = getCLSIDFromCoClass(coclass);

  return do_cocreate_instance(clsId,coclass,location,machine);
}  

CLSID schemeProgIdToCLSID(Scheme_Object *obj,char *fname) {
  HRESULT hr;
  char *progId;
  CLSID clsId;
  BSTR wideProgId;

  progId = SCHEME_STR_VAL(obj);
  
  wideProgId = schemeStringToBSTR(obj);

  hr = CLSIDFromProgID(wideProgId,&clsId);

  if (hr != S_OK) {
    char errBuff[2048];
    sprintf(errBuff,"%s: Error retrieving CLSID from ProgID %s",
	    fname,progId);
    codedComError(errBuff,hr);
  }

  return clsId;
}

Scheme_Object *mx_cocreate_instance_from_progid(int argc,
						Scheme_Object **argv) {
  CLSID clsId;
  char *location;
  char *machine;

  if (SCHEME_STRINGP(argv[0]) == FALSE) {
    scheme_wrong_type("cocreate-instance-from-progid","string",0,argc,argv);
  }

  bindCocreateLocation(argc,argv,&location,&machine,
		       "cocreate-instance-from-progid");

  clsId = schemeProgIdToCLSID(argv[0],"cocreate-instance-from-progid");

  return do_cocreate_instance(clsId,SCHEME_STR_VAL(argv[0]),
			      location,machine);
}  

Scheme_Object *mx_set_coclass(int argc,Scheme_Object **argv) {
  CLSID clsId;

  if (MX_COM_OBJP(argv[0]) == FALSE) {
    scheme_wrong_type("set-coclass!","com-object",0,argc,argv);
  }

  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("set-coclass!","string",1,argc,argv);
  }

  clsId = getCLSIDFromCoClass(SCHEME_STR_VAL(argv[1]));

  MX_COM_OBJ_CLSID(argv[0]) = clsId;

  return scheme_void;
}

Scheme_Object *mx_coclass(int argc,Scheme_Object **argv) {
  HRESULT hr;
  HKEY hkey,hsubkey;
  LONG result;
  FILETIME fileTime;
  unsigned long keyIndex;
  TCHAR clsIdBuffer[256];
  OLECHAR oleClsIdBuffer[256];
  DWORD clsIdBufferSize;
  DWORD dataType;
  BYTE dataBuffer[256];
  DWORD dataBufferSize;
  CLSID clsId,registryClsId;
  int count;
  Scheme_Object *retval;
  
  if (MX_COM_OBJP(argv[0]) == FALSE) {
    scheme_wrong_type("coclass","com-object",0,argc,argv);
  }

  clsId = MX_COM_OBJ_CLSID(argv[0]);

  if (isEmptyClsId(clsId)) {
    scheme_signal_error("coclass: No coclass for object");
  }

  // use CLSID to rummage through Registry to find coclass
  
  result = RegOpenKeyEx(HKEY_CLASSES_ROOT,
			"CLSID",
			(DWORD)0,
			KEY_READ,
			&hkey);
  
  
  if (result != ERROR_SUCCESS) {
    scheme_signal_error("Error while searching Windows registry");
  }	    
  
  // enumerate subkeys until we find the one we want
  
  // really, should call RegQueryInfoKey to find size needed for buffers
  
  keyIndex = 0;
  
  retval = NULL;

  while (1) {
    
    // get next subkey
    
    clsIdBufferSize = sizeof(clsIdBuffer);
    
    result = RegEnumKeyEx(hkey,keyIndex++,
			  clsIdBuffer,
			  &clsIdBufferSize,
			  0,NULL,NULL,
			  &fileTime);
    
    if (result == ERROR_NO_MORE_ITEMS) {
      break;
    }		
    
    if (result != ERROR_SUCCESS) {
      scheme_signal_error("Error enumerating subkeys in Windows registry");
    }
    
    if (strlen(clsIdBuffer) != CLSIDLEN) { // not a CLSID -- bogus entry
      continue;
    }

    count = MultiByteToWideChar(CP_ACP,(DWORD)0,
				clsIdBuffer,strlen(clsIdBuffer),
				oleClsIdBuffer,sizeray(oleClsIdBuffer));
	    
    if (count == 0) {
      scheme_signal_error("Error translating CLSID to Unicode");
    }

    oleClsIdBuffer[CLSIDLEN] = '\0';
	    
    hr = CLSIDFromString(oleClsIdBuffer,&registryClsId);

    if (hr != NOERROR) {
      scheme_signal_error("coclass: Error obtaining coclass CLSID");
    }

    if (registryClsId != clsId) {
      continue;
    }
      
    // open subkey
    
    result = RegOpenKeyEx(hkey,clsIdBuffer,
			  (DWORD)0,
			  KEY_READ,&hsubkey);
    
    if (result != ERROR_SUCCESS) {
      scheme_signal_error("coclass: Error obtaining coclass value");
    }	    
    
    dataBufferSize = sizeof(dataBuffer);
    
    RegQueryValueEx(hsubkey,"",0,&dataType,dataBuffer,&dataBufferSize);
    
    RegCloseKey(hsubkey);

    if (dataType == REG_SZ) {
      retval = scheme_make_string((char *)dataBuffer);
      break;
    }
  }

  RegCloseKey(hkey);  

  if (retval == NULL) {
    scheme_signal_error("coclass: object's coclass not found in Registry");
  }

  return retval;
}

Scheme_Object *mx_coclass_as_progid(int argc,Scheme_Object **argv) {
  HRESULT hr;
  LPOLESTR wideProgId;
  CLSID clsId;
  char *buff;
  unsigned int len;

  if (MX_COM_OBJP(argv[0]) == FALSE) {
    scheme_wrong_type("coclass-as-progid","com-object",0,argc,argv);
  }

  clsId = MX_COM_OBJ_CLSID(argv[0]);

  if (isEmptyClsId(clsId)) {
    scheme_signal_error("coclass-as-progid: No coclass for object");
  }

  hr = ProgIDFromCLSID(clsId,&wideProgId);
  
  if (hr != S_OK) {
    scheme_signal_error("coclass-as-progid: Error finding coclass");
  }

  len = wcslen(wideProgId);

  buff = (char *)scheme_malloc(len + 1);

  WideCharToMultiByte(CP_ACP,(DWORD)0,
		      wideProgId,len,
		      buff,len + 1,
		      NULL,NULL);

  buff[len] = '\0';

  return scheme_make_string(buff);
}

Scheme_Object *mx_set_coclass_from_progid(int argc,Scheme_Object **argv) {
  CLSID clsid;

  if (MX_COM_OBJP(argv[0]) == FALSE) {
    scheme_wrong_type("set-coclass-from-progid!","com-object",0,argc,argv);
  }

  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("set-coclass-from-progid!","string",1,argc,argv);
  }

  clsid = schemeProgIdToCLSID(argv[1],"set-coclass-from-progid!");

  MX_COM_OBJ_CLSID(argv[0]) = clsid;

  return scheme_void;
}

ITypeInfo *typeInfoFromComObject(MX_COM_Object *obj) {
  HRESULT hr;
  ITypeInfo *pITypeInfo;
  IDispatch *pIDispatch;
  unsigned int count;

  pITypeInfo = obj->pITypeInfo;

  if (pITypeInfo) {
    return pITypeInfo;
  }

  pIDispatch = obj->pIDispatch;

  pIDispatch->GetTypeInfoCount(&count);
  
  if (count == 0) {
    scheme_signal_error("COM object does not expose type information");
  }
  
  hr = pIDispatch->GetTypeInfo(0,LOCALE_SYSTEM_DEFAULT,&pITypeInfo);
  
  if (hr != S_OK || pITypeInfo == NULL) {
    codedComError("Error getting COM type information",hr);
  }

  obj->pITypeInfo = pITypeInfo;

  return pITypeInfo;
}

Scheme_Object *mx_com_get_object_type(int argc,Scheme_Object **argv) {
  ITypeInfo *pITypeInfo;
  MX_COM_Type *retval;
  MX_COM_Object *obj;

  if (MX_COM_OBJP(argv[0]) == FALSE) {
    scheme_wrong_type("com-object-type","com-object",0,argc,argv);
  }

  obj = (MX_COM_Object *)argv[0];
  pITypeInfo = typeInfoFromComObject(obj);

  retval = (MX_COM_Type *)scheme_malloc(sizeof(MX_COM_Type));

  retval->type = mx_com_type_type;
  retval->released = FALSE;
  retval->pITypeInfo = pITypeInfo;
  retval->clsId = obj->clsId;

  pITypeInfo->AddRef();

  mx_register_simple_com_object((Scheme_Object *)retval,pITypeInfo);
  
  return (Scheme_Object *)retval;
}

BOOL typeInfoEq(ITypeInfo *pITypeInfo1,ITypeInfo *pITypeInfo2) {
  HRESULT hr;
  TYPEATTR *pTypeAttr1,*pTypeAttr2;
  BOOL retval;

  // intensional equality

  if (pITypeInfo1 == pITypeInfo2) {
    return TRUE;
  }

  hr = pITypeInfo1->GetTypeAttr(&pTypeAttr1);

  if (hr != S_OK || pTypeAttr1 == NULL) {
    codedComError("Error getting type attributes",hr);
  }

  hr = pITypeInfo2->GetTypeAttr(&pTypeAttr2);

  if (hr != S_OK || pTypeAttr2 == NULL) {
    codedComError("Error getting type attributes",hr);
  }

  retval = (pTypeAttr1->guid == pTypeAttr2->guid);

  pITypeInfo1->ReleaseTypeAttr(pTypeAttr1);
  pITypeInfo2->ReleaseTypeAttr(pTypeAttr2);

  return retval;
}

Scheme_Object *mx_com_is_a(int argc,Scheme_Object **argv) {
  ITypeInfo *pITypeInfo1,*pITypeInfo2;

  if (MX_COM_OBJP(argv[0]) == FALSE) {
    scheme_wrong_type("com-is-a?","com-object",0,argc,argv);
  }

  if (MX_COM_TYPEP(argv[1]) == FALSE) {
    scheme_wrong_type("com-is-a?","com-type",1,argc,argv);
  }

  pITypeInfo1 = typeInfoFromComObject((MX_COM_Object *)argv[0]);
  pITypeInfo2 = MX_COM_TYPE_VAL((MX_COM_Type *)argv[1]);

  return typeInfoEq(pITypeInfo1,pITypeInfo2) ? scheme_true : scheme_false;
}

Scheme_Object *mx_com_help(int argc,Scheme_Object **argv) {
  HRESULT hr;
  ITypeInfo *pITypeInfo;
  BSTR helpFileName;
  char buff[MAX_PATH];
  int len;
  
  if (MX_COM_OBJP(argv[0]) == FALSE && MX_COM_TYPEP(argv[0]) == FALSE) {
    scheme_wrong_type("com-help","com-object or com-type",0,argc,argv);
  }
  
  if (argc == 2 && SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("com-help","string",1,argc,argv);
  }

  if (MX_COM_OBJP(argv[0])) {
    pITypeInfo = typeInfoFromComObject((MX_COM_Object *)argv[0]);
  }
  else {
    pITypeInfo = MX_COM_TYPE_VAL(argv[0]);
  }

  hr = pITypeInfo->GetDocumentation(MEMBERID_NIL,NULL,NULL,NULL,
				    &helpFileName);
  
  if (hr != S_OK) {
    codedComError("Can't get help",hr); 
  }
  else if (helpFileName == NULL || wcscmp(helpFileName,L"") == 0) {
    scheme_signal_error("No help available"); 
  }
  
  WideCharToMultiByte(CP_ACP,(DWORD)0,helpFileName,SysStringLen(helpFileName),
		      buff,sizeof(buff) - 1,
		      NULL,NULL);
  
  SysFreeString(helpFileName);

  buff[sizeof(buff)-1] = '\0';
  
  len = strlen(buff);
  
  if (stricmp(buff + len - 4,".CHM") == 0) {
    HWND hwnd;
    
    if (argc >= 2) {
      hwnd = HtmlHelp(NULL,buff,
		      HH_DISPLAY_INDEX,(DWORD)SCHEME_STR_VAL(argv[1]));
    }
    else {
      hwnd = HtmlHelp(NULL,buff,HH_DISPLAY_TOPIC,0);
    }
    
    if (hwnd) {
      SetForegroundWindow(hwnd);
    }
  }
  else if (stricmp(buff + len - 4,".HLP") == 0) {
    if (argc >= 2) {
      WinHelp(NULL,buff,HELP_KEY,(DWORD)(SCHEME_STR_VAL(argv[1])));
    }
    else {
      WinHelp(NULL,buff,HELP_FINDER,0);
    }
  }
  else {
    scheme_signal_error("Unknown help file type: %s",buff);
  }
  
  return scheme_void;
}

void signalCodedEventSinkError(char *s,HRESULT hr) {
  ReleaseSemaphore(eventSinkMutex,1,NULL);
  codedComError(s,hr);
}

void connectComObjectToEventSink(MX_COM_Object *obj) {
  HRESULT hr;
  IUnknown *pIUnknown;
  IDispatch *pIDispatch;
  ITypeInfo *pITypeInfo;
  IConnectionPointContainer *pIConnectionPointContainer;
  IConnectionPoint *pIConnectionPoint;
  ISink *pISink;
  DWORD cookie;
  TYPEATTR *pTypeAttr;
  
  if (obj->pIConnectionPoint) {
    return;
  }
  
  WaitForSingleObject(eventSinkMutex,INFINITE);
  
  pIDispatch = obj->pIDispatch;
  
  hr = pIDispatch->QueryInterface(IID_IConnectionPointContainer,(void **)&pIConnectionPointContainer); 
  
  if (hr != S_OK || pIConnectionPointContainer == NULL) {
    signalCodedEventSinkError("cocreate-instance-from-{coclass,progid}: "
			      "Unable to get COM object connection point "
			      "container",hr);
  }
  
  pITypeInfo = eventTypeInfoFromComObject(obj);
  
  if (pITypeInfo == NULL) {
    ReleaseSemaphore(eventSinkMutex,1,NULL);
    scheme_signal_error("cocreate-instance-from-{coclass,progid}: "
			"Unable to get type information for events");
  }
  
  hr = pITypeInfo->GetTypeAttr(&pTypeAttr);
  
  if (hr != S_OK || pTypeAttr == NULL) {
    signalCodedEventSinkError("cocreate-instance-from-{coclass,progid}: "
			      "Unable to get type attributes for events",hr);
  }
  
  hr = pIConnectionPointContainer->FindConnectionPoint(pTypeAttr->guid,&pIConnectionPoint);
  
  pITypeInfo->ReleaseTypeAttr(pTypeAttr);
  pIConnectionPointContainer->Release();
  
  if (hr != S_OK || pIConnectionPoint == NULL) {
    signalCodedEventSinkError("cocreate-instance-from-{coclass,progid}: "
			      "Unable to find COM object connection point",hr);
  }
  
  hr = CoCreateInstance(CLSID_Sink,NULL,CLSCTX_LOCAL_SERVER | CLSCTX_INPROC_SERVER,
			IID_IUnknown,(void **)&pIUnknown);
  
  if (hr != S_OK || pIUnknown == NULL) {
    signalCodedEventSinkError("cocreate-instance-from-{coclass,progid}: "
			      "Unable to create sink object",hr);
  }
  
  hr = pIUnknown->QueryInterface(IID_ISink,(void **)&pISink);
  
  if (hr != S_OK || pISink == NULL) {
    signalCodedEventSinkError("cocreate-instance-from-{coclass,progid}: "
			      "Unable to find sink interface",hr);
  }
  
  pISink->set_extension_table((int)scheme_extension_table); // COM won't take a function ptr
  
  pISink->set_myssink_table((int)&myssink_table);
  
  hr = pIConnectionPoint->Advise(pIUnknown,&cookie);

  pIUnknown->Release();
  
  if (hr != S_OK) {
    signalCodedEventSinkError("cocreate-instance-from-{coclass,progid}: "
			      "Unable to connect sink to connection point",hr);
  }
  
  obj->pEventTypeInfo = pITypeInfo;
  obj->pIConnectionPoint = pIConnectionPoint;
  obj->connectionCookie = cookie;
  obj->pISink = pISink;
  
  ReleaseSemaphore(eventSinkMutex,1,NULL);
}

FUNCDESC *getFuncDescForEvent(LPOLESTR name,ITypeInfo *pITypeInfo) {        
  HRESULT hr;
  TYPEATTR *pTypeAttr;
  FUNCDESC *pFuncDesc;
  BSTR bstr;
  UINT bstrCount;
  unsigned short numFuncDescs;
  int i;

  hr = pITypeInfo->GetTypeAttr(&pTypeAttr);
  
  if (hr != S_OK || pTypeAttr == NULL) {
    codedComError("Unable to get type attributes for events",hr);
  }

  numFuncDescs = pTypeAttr->cFuncs;
  
  pITypeInfo->ReleaseTypeAttr(pTypeAttr);
  
  for (i = 0; i < numFuncDescs; i++) {
    
    hr = pITypeInfo->GetFuncDesc(i,&pFuncDesc);		
    
    if (hr != S_OK) {
      codedComError("Error getting event method type description",hr);
    }
    
    // rely on name of event
    
    hr = pITypeInfo->GetNames(pFuncDesc->memid,&bstr,1,&bstrCount);
    
    if (hr != S_OK) {
      codedComError("Error getting event method name",hr);
    }
    
    if (wcscmp(name,bstr) == 0) {
      SysFreeString(bstr);
      return pFuncDesc;
    }
    
    SysFreeString(bstr);
    
    pITypeInfo->ReleaseFuncDesc(pFuncDesc);
  }

  return NULL;
}

Scheme_Object *mx_com_register_event_handler(int argc,Scheme_Object **argv) {
  char *eventName;
  ITypeInfo *pITypeInfo;
  ISink *pISink;
  FUNCDESC *pFuncDesc;
  BSTR unicodeName;
  
  if (MX_COM_OBJP(argv[0]) == FALSE) {
    scheme_wrong_type("com-register-event-handler","com-object",0,argc,argv);
  }
  
  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("com-register-event-handler","string",1,argc,argv);
  }
  
  if (SCHEME_PROCP(argv[2]) == FALSE) {
    scheme_wrong_type("com-register-event-handler","procedure",2,argc,argv);
  }
  
  eventName = SCHEME_STR_VAL(argv[1]);
  
  connectComObjectToEventSink((MX_COM_Object *)argv[0]);
  
  pITypeInfo = MX_COM_OBJ_EVENTTYPEINFO(argv[0]);
  pISink = MX_COM_OBJ_EVENTSINK(argv[0]);

  unicodeName = schemeStringToBSTR(argv[1]);

  pFuncDesc = getFuncDescForEvent(unicodeName,pITypeInfo);

  SysFreeString(unicodeName);
  
  if (pFuncDesc == NULL) {				   
    scheme_signal_error("Can't find event %s in type description",eventName);
  }

  pISink->register_handler(pFuncDesc->memid,(int)argv[2]);

  pITypeInfo->ReleaseFuncDesc(pFuncDesc);

  return scheme_void;
}

Scheme_Object *mx_com_unregister_event_handler(int argc,Scheme_Object **argv) {
  char *eventName;
  ITypeInfo *pITypeInfo;
  ISink *pISink;
  FUNCDESC *pFuncDesc;
  BSTR unicodeName;
  
  if (MX_COM_OBJP(argv[0]) == FALSE) {
    scheme_wrong_type("com-unregister-event-handler","com-object",0,argc,argv);
  }
  
  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("com-unregister-event-handler","string",1,argc,argv);
  }
  
  eventName = SCHEME_STR_VAL(argv[1]);
  
  pITypeInfo = MX_COM_OBJ_EVENTTYPEINFO(argv[0]);

  if (pITypeInfo == NULL) {
    scheme_signal_error("No event type information for object");
  }

  pISink = MX_COM_OBJ_EVENTSINK(argv[0]);

  if (pISink == NULL) { // no events registered
    return scheme_void;
  }
  
  unicodeName = schemeStringToBSTR(argv[1]);

  pFuncDesc = getFuncDescForEvent(unicodeName,pITypeInfo);

  SysFreeString(unicodeName);

  if (pFuncDesc == NULL) {				   
    scheme_signal_error("Can't find event %s in type description",eventName);
  }

  pISink->unregister_handler(pFuncDesc->memid);
  
  pITypeInfo->ReleaseFuncDesc(pFuncDesc);
  
  return scheme_void;
}

MX_TYPEDESC *typeDescFromTypeInfo(char *name,INVOKEKIND invKind,
				  ITypeInfo *pITypeInfo) {
  HRESULT hr;
  TYPEATTR *pTypeAttr;
  FUNCDESC *pFuncDesc;
  VARDESC *pVarDesc;
  MEMBERID memID;
  MX_DESCKIND descKind;
  MX_TYPEDESC *pTypeDesc;
  BSTR unicodeName;
  BSTR bstr;
  UINT nameCount;
  BOOL foundDesc;
  int i;

  unicodeName = stringToBSTR(name,strlen(name));
  
  hr = pITypeInfo->GetTypeAttr(&pTypeAttr);
  
  if (hr != S_OK) {
    codedComError("Error getting type attributes for function",hr);
  }
  
  foundDesc = FALSE;
  
  for (i = 0; i < pTypeAttr->cFuncs; i++) {
    
    hr = pITypeInfo->GetFuncDesc(i,&pFuncDesc);		
    
    if (hr != S_OK) {
      codedComError("Error getting type description",hr);
    }
    
    pITypeInfo->GetNames(pFuncDesc->memid,&bstr,1,&nameCount);
    
    // see if this FUNCDESC is the one we want
    
    if (wcscmp(bstr,unicodeName) == 0 &&
	(invKind == INVOKE_EVENT || pFuncDesc->invkind == invKind)) {
      
      foundDesc = TRUE;
      descKind = funcDesc;			
      SysFreeString(bstr);
      memID = pFuncDesc->memid;
      
      break;
    }
    
    // if not, throw it back
    
    SysFreeString(bstr);
    pITypeInfo->ReleaseFuncDesc(pFuncDesc);
    
  }
  
  if (invKind == INVOKE_PROPERTYGET ||
      invKind == INVOKE_PROPERTYPUT ||
      invKind == INVOKE_PROPERTYPUTREF) {
    
    for (i = 0; i < pTypeAttr->cVars; i++) {
      hr = pITypeInfo->GetVarDesc(i,&pVarDesc);		
      if (hr != S_OK) {
	codedComError("Error getting type description",hr);
      }
      
      // see if this VARDESC is the one we want
      
      pITypeInfo->GetNames(pVarDesc->memid,&bstr,1,&nameCount);
      
      if (wcscmp(bstr,unicodeName)) {
	foundDesc = TRUE;
	descKind = varDesc;
	memID = pVarDesc->memid;
	
	break;
      }
      
      // if not, throw it back
      
      pITypeInfo->ReleaseVarDesc(pVarDesc);
      
    }
  }
  
  SysFreeString(unicodeName);
  
  pITypeInfo->ReleaseTypeAttr(pTypeAttr);

  if (foundDesc == FALSE) {
    scheme_signal_error("Error finding type description for \"%s\"",name);
  }
  
  pTypeDesc = (MX_TYPEDESC *)scheme_malloc(sizeof(MX_TYPEDESC));
  
  pTypeDesc->type = mx_com_typedesc_type;
  pTypeDesc->released = FALSE;

  pTypeDesc->memID = memID;

  pTypeDesc->pITypeInfo = pITypeInfo;
  pITypeInfo->AddRef();

  pTypeDesc->descKind = descKind;

  if (descKind == funcDesc) {
    pTypeDesc->pFuncDesc = pFuncDesc;
  }
  else {
    pTypeDesc->pVarDesc = pVarDesc;
  }

  scheme_add_managed((Scheme_Manager *)scheme_get_param(scheme_config,MZCONFIG_MANAGER),
		     (Scheme_Object *)pTypeDesc,
		     (Scheme_Close_Manager_Client *)scheme_release_typedesc,
		     NULL,0);
  scheme_register_finalizer(pTypeDesc,scheme_release_typedesc,NULL,NULL,NULL);

  return pTypeDesc;
}
  
MX_TYPEDESC *getMethodType(MX_COM_Object *obj,char *name,INVOKEKIND invKind) {
  IDispatch *pIDispatch;
  MX_TYPEDESC *pTypeDesc;
  ITypeInfo *pITypeInfo;
  
  // need Unicode version of name to please ITypeInfo::GetIDsOfNames
  // note that we need string length + 1
  
  pIDispatch = obj->pIDispatch;
  
  // check in hash table to see if we already have the type information
  
  pTypeDesc = lookupTypeDesc(pIDispatch,name,invKind);
  
  if (pTypeDesc) {
    return pTypeDesc;
  }
  
  if (invKind == INVOKE_EVENT) {
    pITypeInfo = eventTypeInfoFromComObject(obj);
    
    if (pITypeInfo == NULL) {
      scheme_signal_error("Can't find event type information");
    }
  }
  else {
    pITypeInfo = typeInfoFromComObject(obj);
  }

  pTypeDesc = typeDescFromTypeInfo(name,invKind,pITypeInfo);
  
  addTypeToTable(pIDispatch,name,invKind,pTypeDesc);
  
  return pTypeDesc;
}

Scheme_Object *mx_do_get_methods(int argc,Scheme_Object **argv,INVOKEKIND invKind) {
  ITypeInfo *pITypeInfo;
  BSTR bstr;
  HRESULT hr;
  TYPEATTR *pTypeAttr;
  FUNCDESC *pFuncDesc;
  VARDESC *pVarDesc;
  Scheme_Object *retval;
  char buff[256];
  unsigned int len;
  unsigned int count;
  int i;
  
  if (MX_COM_OBJP(argv[0]) == FALSE && MX_COM_TYPEP(argv[0]) == FALSE) {
    scheme_wrong_type("com-methods","com-object or com-type",0,argc,argv);
  }
  
  if (MX_COM_OBJP(argv[0])) {
    pITypeInfo = typeInfoFromComObject((MX_COM_Object *)argv[0]);
  }  
  else {
    pITypeInfo = MX_COM_TYPE_VAL(argv[0]);
  }

  hr = pITypeInfo->GetTypeAttr(&pTypeAttr);
  
  if (hr != S_OK || pTypeAttr == NULL) {
    codedComError("Error getting type attributes",hr);
  }

  retval = scheme_null;
  
  // properties can appear in list of functions
  // or in list of variables
  
  for (i = 0; i < pTypeAttr->cFuncs; i++) {
    pITypeInfo->GetFuncDesc(i,&pFuncDesc);		
    if (pFuncDesc->invkind == invKind) {
      pITypeInfo->GetNames(pFuncDesc->memid,&bstr,1,&count);
      len = SysStringLen(bstr);
      WideCharToMultiByte(CP_ACP,(DWORD)0,bstr,len,
			  buff,sizeof(buff) - 1,
			  NULL,NULL);
      buff[len] = '\0';
      retval = scheme_make_pair(scheme_make_string(buff),retval);
      SysFreeString(bstr);
    }
    pITypeInfo->ReleaseFuncDesc(pFuncDesc);
  }
  
  if (invKind == INVOKE_FUNC) { // done, if not a property
    return retval;
  }
  
  for (i = 0; i < pTypeAttr->cVars; i++) {
    pITypeInfo->GetVarDesc(i,&pVarDesc);		
    pITypeInfo->GetNames(pVarDesc->memid,&bstr,1,&count);
    len = SysStringLen(bstr);
    WideCharToMultiByte(CP_ACP,(DWORD)0,bstr,len,
			buff,sizeof(buff) - 1,
			NULL,NULL);
    buff[len] = '\0';
    retval = scheme_make_pair(scheme_make_string(buff),retval);
    SysFreeString(bstr);
    pITypeInfo->ReleaseVarDesc(pVarDesc);
  } 

  return retval;
}

Scheme_Object *mx_com_methods(int argc,Scheme_Object **argv) {
  return mx_do_get_methods(argc,argv,INVOKE_FUNC);
}

Scheme_Object *mx_com_get_properties(int argc,Scheme_Object **argv) {
  return mx_do_get_methods(argc,argv,INVOKE_PROPERTYGET);
}

Scheme_Object *mx_com_set_properties(int argc,Scheme_Object **argv) {
  return mx_do_get_methods(argc,argv,INVOKE_PROPERTYPUT);
}

ITypeInfo *coclassTypeInfoFromTypeInfo(ITypeInfo *pITypeInfo,CLSID clsId) {
  HRESULT hr;
  ITypeLib *pITypeLib;
  ITypeInfo *pCoclassTypeInfo;
  ITypeInfo *pCandidateTypeInfo;
  TYPEATTR *pTypeAttr;
  TYPEKIND typeKind;
  HREFTYPE hRefType;
  UINT ndx;
  UINT typeInfoCount;
  UINT coclassCount;
  UINT typeCount;
  UINT coclassNdx;
  UINT i,j;

  hr = pITypeInfo->GetContainingTypeLib(&pITypeLib,&ndx);

  if (hr != S_OK) {
    scheme_signal_error("Can't get dispatch type library");
  }

  // first try using explicit clsId

  if (!isEmptyClsId(clsId)) {
    hr = pITypeLib->GetTypeInfoOfGuid(clsId,&pCoclassTypeInfo);

    pITypeLib->Release();

    if (hr != S_OK || pCoclassTypeInfo == NULL) {
      codedComError("Error getting type info for coclass",hr);
      return NULL;
    }
  
    return pCoclassTypeInfo;
  } 

  // if no CLSID, search for coclass implementing supplied 
  // interface

  typeInfoCount = pITypeLib->GetTypeInfoCount();

  coclassCount = 0;

  // check for ambiguity

  for (i = 0; i < typeInfoCount; i++) {
    
    pITypeLib->GetTypeInfoType(i,&typeKind);

    if (typeKind == TKIND_COCLASS) {

      hr = pITypeLib->GetTypeInfo(i,&pCoclassTypeInfo);

      if (hr != S_OK || pCoclassTypeInfo == NULL) {
	pITypeLib->Release();
	codedComError("Error getting type info for coclass",hr);
      }

      hr = pCoclassTypeInfo->GetTypeAttr(&pTypeAttr);
	
      if (hr != S_OK || pTypeAttr == NULL) {
	pCoclassTypeInfo->Release();
	pITypeLib->Release();
	codedComError("Error getting coclass type attributes",hr);
      }
  
      typeCount = pTypeAttr->cImplTypes; 
  
      pCoclassTypeInfo->ReleaseTypeAttr(pTypeAttr);
  
      for (j = 0; j < typeCount; j++) {
	hr = pCoclassTypeInfo->GetRefTypeOfImplType(j,&hRefType);
	  
	if (hr != S_OK) {
	  pCoclassTypeInfo->Release();
	  pITypeLib->Release();
	  codedComError("Error retrieving type info handle",hr);
	}
  
	hr = pCoclassTypeInfo->GetRefTypeInfo(hRefType,&pCandidateTypeInfo);

	if (hr != S_OK || pCandidateTypeInfo == NULL) {
	  pCoclassTypeInfo->Release();
	  pITypeLib->Release();
	  codedComError("Error retrieving candidate type info",hr);
	}

	if (typeInfoEq(pCandidateTypeInfo,pITypeInfo)) {
	  coclassNdx = i;
	  if (++coclassCount >= 2) {
	    pCandidateTypeInfo->Release();
	    pCoclassTypeInfo->Release();
	    pITypeLib->Release();
	    scheme_signal_error("Ambiguous coclass for object");
	  }
	}

	pCandidateTypeInfo->Release();

      }

      pCoclassTypeInfo->Release();

    }
  }

  if (coclassCount == 0) {
    pITypeLib->Release();
    return NULL;
  }

  hr = pITypeLib->GetTypeInfo(coclassNdx,&pCoclassTypeInfo);

  pITypeLib->Release();

  if (hr != S_OK || pCoclassTypeInfo == NULL) {
    codedComError("Error getting type info for coclass",hr);
  }

  return pCoclassTypeInfo;
}

ITypeInfo *eventTypeInfoFromCoclassTypeInfo(ITypeInfo *pCoclassTypeInfo) {
  HRESULT hr;
  ITypeInfo *pEventTypeInfo;
  TYPEATTR *pTypeAttr;
  HREFTYPE hRefType;
  UINT typeCount;
  UINT eventTypeInfoNdx;
  int typeFlags;
  UINT i;
 
  hr = pCoclassTypeInfo->GetTypeAttr(&pTypeAttr);
  
  if (hr != S_OK || pTypeAttr == NULL) {
    codedComError("Error getting type attributes",hr);
  }
  
  typeCount = pTypeAttr->cImplTypes; 
  
  pCoclassTypeInfo->ReleaseTypeAttr(pTypeAttr);
  
  eventTypeInfoNdx = -1;
  
  for (i = 0; i < typeCount; i++) {
    
    hr = pCoclassTypeInfo->GetImplTypeFlags(i,&typeFlags);
    
    if (hr != S_OK) {
      codedComError("Error retrieving type flags",hr);
    }
    
    // look for [source, default]
    
    if ((typeFlags & IMPLTYPEFLAG_FSOURCE) &&
	(typeFlags & IMPLTYPEFLAG_FDEFAULT)) {
      eventTypeInfoNdx = i;
      break;
    }
  }
  
  if (eventTypeInfoNdx == -1) {
    return NULL;
  }
  
  hr = pCoclassTypeInfo->GetRefTypeOfImplType(eventTypeInfoNdx,&hRefType);
  
  if (hr != S_OK) {
    codedComError("Error retrieving type info handle",hr);
  }
  
  hr = pCoclassTypeInfo->GetRefTypeInfo(hRefType,&pEventTypeInfo);

  if (hr != S_OK) {
    codedComError("Error retrieving event type info",hr);
  }

  return pEventTypeInfo;
}

ITypeInfo *eventTypeInfoFromComObject(MX_COM_Object *obj) {
  HRESULT hr;
  IDispatch *pIDispatch;
  ITypeInfo *pCoclassTypeInfo,*pEventTypeInfo;
  IProvideClassInfo *pIProvideClassInfo;

  pEventTypeInfo = obj->pEventTypeInfo;
  
  if (pEventTypeInfo) {
    return pEventTypeInfo;
  }
  
  pIDispatch = obj->pIDispatch;

  /* preferred mechanism for finding coclass ITypeInfo */

  hr = pIDispatch->QueryInterface(IID_IProvideClassInfo,
				  (void **)&pIProvideClassInfo);
  
  if (hr == S_OK && pIProvideClassInfo != NULL) {

    hr = pIProvideClassInfo->GetClassInfo(&pCoclassTypeInfo);

    if (hr != S_OK || pCoclassTypeInfo == NULL) {
      scheme_signal_error("Error getting coclass type information");
    }
  }
  else if (hr == E_NOINTERFACE) {
    ITypeInfo *pDispatchTypeInfo;

    /* alternate mechanism */

    hr = pIDispatch->GetTypeInfo(0,LOCALE_SYSTEM_DEFAULT,&pDispatchTypeInfo);

    if (hr != S_OK) {
      codedComError("Can't get dispatch type information",hr);
    }

    pCoclassTypeInfo = coclassTypeInfoFromTypeInfo(pDispatchTypeInfo,
						   obj->clsId);
    pDispatchTypeInfo->Release();

    if (pCoclassTypeInfo == NULL) {
      scheme_signal_error("Error getting coclass type information");
    }
  }

  else {
    codedComError("Error getting COM event type information",hr);
  }

  // have type info for coclass
  // event type info is one of the "implemented" interfaces

  pEventTypeInfo = eventTypeInfoFromCoclassTypeInfo(pCoclassTypeInfo);
 
  pCoclassTypeInfo->Release();
  
  if (pEventTypeInfo == NULL) {
    scheme_signal_error("Error retrieving event type info");
  }
  
  obj->pEventTypeInfo = pEventTypeInfo;
  
  return pEventTypeInfo;
}

ITypeInfo *eventTypeInfoFromComType(MX_COM_Type *obj) {
  ITypeInfo *pCoclassTypeInfo,*pEventTypeInfo;

  pCoclassTypeInfo = coclassTypeInfoFromTypeInfo(obj->pITypeInfo,
						 obj->clsId);

  if (pCoclassTypeInfo == NULL) {
    scheme_signal_error("Error getting coclass type information");
  }

  // have type info for coclass
  // event type info is one of the "implemented" interfaces

  pEventTypeInfo = eventTypeInfoFromCoclassTypeInfo(pCoclassTypeInfo);
 
  pCoclassTypeInfo->Release();
  
  if (pEventTypeInfo == NULL) {
    scheme_signal_error("Error retrieving event type info");
  }
  
  return pEventTypeInfo;
}

Scheme_Object *mx_com_events(int argc,Scheme_Object **argv) {
  HRESULT hr;
  ITypeInfo *pEventTypeInfo;
  TYPEATTR *pEventTypeAttr;
  FUNCDESC *pFuncDesc;
  Scheme_Object *retval;
  char buff[256];
  UINT nameCount;
  BSTR bstr;
  unsigned int len;
  UINT i;
  
  if (MX_COM_OBJP(argv[0]) == FALSE && MX_COM_TYPEP(argv[0]) == FALSE) {
    scheme_wrong_type("com-methods","com-object or com-type",0,argc,argv);
  }

  if (MX_COM_OBJP(argv[0]) && MX_COM_OBJ_VAL(argv[0])== NULL) {
    scheme_signal_error("NULL COM object");
  }

  // query for outbound interface info
  
  if (MX_COM_OBJP(argv[0])) {
    pEventTypeInfo = eventTypeInfoFromComObject((MX_COM_Object *)argv[0]);
  }
  else {
    pEventTypeInfo = eventTypeInfoFromComType((MX_COM_Type *)argv[0]);
  }

  if (pEventTypeInfo == NULL) {
    scheme_signal_error("Can't find event type information");
  }
  
  hr = pEventTypeInfo->GetTypeAttr(&pEventTypeAttr);
  
  if (hr != S_OK || pEventTypeAttr == NULL) {
    codedComError("Error retrieving event type attributes",hr);
  }
  
  retval = scheme_null;
  
  for (i = 0; i < pEventTypeAttr->cFuncs; i++) {
    pEventTypeInfo->GetFuncDesc(i,&pFuncDesc);		
    pEventTypeInfo->GetNames(pFuncDesc->memid,&bstr,1,&nameCount);
    len = SysStringLen(bstr);
    WideCharToMultiByte(CP_ACP,(DWORD)0,bstr,len,
			buff,sizeof(buff) - 1,
			NULL,NULL);
    buff[len] = '\0';
    retval = scheme_make_pair(scheme_make_string(buff),retval);
    SysFreeString(bstr);
  }
  
  pEventTypeInfo->ReleaseFuncDesc(pFuncDesc);
  pEventTypeInfo->ReleaseTypeAttr(pEventTypeAttr);
  
  return retval;
}


VARTYPE getVarTypeFromElemDesc(ELEMDESC *pElemDesc) {
  unsigned short flags;
  
  flags = pElemDesc->paramdesc.wParamFlags;
  
  if ((flags & PARAMFLAG_FOPT) && (flags & PARAMFLAG_FHASDEFAULT))  {
    
    // use type of default value
    
    return pElemDesc->paramdesc.pparamdescex->varDefaultValue.vt;
  }
  
  if (pElemDesc->tdesc.vt == VT_PTR) {
    return pElemDesc->tdesc.lptdesc->vt | VT_BYREF;
  }
  
  return pElemDesc->tdesc.vt;
  
}

Scheme_Object *elemDescToSchemeType(ELEMDESC *pElemDesc,BOOL ignoreByRef,BOOL isOpt) {
  static char buff[256];
  char *s;
  BOOL isBox;
  VARTYPE vt;
  
  vt = getVarTypeFromElemDesc(pElemDesc);
  
  if (ignoreByRef) {
    vt &= ~VT_BYREF;
  }
  
  isBox = FALSE;
  
  switch(vt) {
    
  case VT_HRESULT :
  case VT_NULL :
    
    s = "void";
    break;
    
  case VT_UI1 :
    
    s = "char";
    break;
    
  case VT_UI1 | VT_BYREF :
    
    s = "char";
    isBox = TRUE;
    break;
    
  case VT_I2 :
    
    s = "short-int";
    break;
    
  case VT_I2 + VT_BYREF :
    
    s = "short-int";
    isBox = TRUE;
    break;
    
  case VT_I4 :
    
    s = "int";
    break;
    
  case VT_I4 | VT_BYREF:
    
    s = "int";
    isBox = TRUE;
    break;
    
  case VT_R4 :
    
    s = "float";
    break;
    
  case VT_R4 | VT_BYREF :
    
    s = "float";
    isBox = TRUE;
    break;
    
  case VT_R8 :
    
    s = "double";
    break;
    
  case VT_R8 | VT_BYREF :
    
    s = "double";
    isBox = TRUE;
    break;
    
  case VT_BSTR :
    
    s = "string";
    break;
    
  case VT_BSTR | VT_BYREF :
    
    s = "string";
    isBox = TRUE;
    break;
    
  case VT_CY :
    
    s = "mx-currency";
    break;
    
  case VT_CY | VT_BYREF :
    
    s = "mx-currency";
    isBox = TRUE;
    break;
    
  case VT_DATE :
    
    s = "mx-date";
    break;
    
  case VT_DATE | VT_BYREF :
    
    s = "mx-date";
    isBox = TRUE;
    break;
    
  case VT_BOOL :
    
    s = "boolean";
    break;
    
  case VT_BOOL | VT_BYREF :
    
    s = "boolean";
    isBox = FALSE;
    break;
    
  case VT_ERROR :
    
    s = "mx-scode";
    break;
    
  case VT_ERROR | VT_BYREF:
    
    s = "mx-scode";
    isBox = TRUE;
    break;
    
  case VT_UNKNOWN :
    
    s = "mx-unknown-com-object";
    break;
    
  case VT_UNKNOWN | VT_BYREF :
    
    s = "mx-unknown-com-object";
    isBox = TRUE;
    break;
    
  case VT_DISPATCH :
    
    s = "com-object";
    break;
    
  case VT_DISPATCH | VT_BYREF :
    
    s = "com-object";
    isBox = TRUE;
    break;
    
  case VT_VARIANT : 
    
    s = "mx-any";
    break;
    
  case VT_VARIANT | VT_BYREF : 
    
    s = "mx-any";
    isBox = TRUE;
    break;
    
  case VT_USERDEFINED :
    
    s = "user-defined";
    break;
    
  default :
    
    { 
      char defaultBuff[32];
      sprintf(defaultBuff,"COM-0x%X",vt);
      return scheme_intern_symbol(defaultBuff);
    }
  }
  
  if (isBox) {
    if (isOpt) {
      sprintf(buff,"%s-box-opt",s);
    }
    else {
      sprintf(buff,"%s-box",s);
    }
  }
  else {
    if (isOpt) {
      sprintf(buff,"%s-opt",s);
    }
    else {
      strcpy(buff,s);
    }
  }
  
  return scheme_intern_exact_symbol(buff,strlen(buff));
}


Scheme_Object *mx_make_function_type(Scheme_Object *paramTypes,
				     Scheme_Object *returnType) {
  Scheme_Object *arrow;
  
  arrow = scheme_intern_symbol("->");
  
  return scheme_append(paramTypes,
		       scheme_make_pair(arrow,
					scheme_make_pair(returnType,
							 scheme_null)));
}

BOOL isDefaultParam(FUNCDESC *pFuncDesc,short int i) {
  unsigned short flags;
  
  if (pFuncDesc->lprgelemdescParam == NULL) {
    return FALSE;
  }
  
  flags = pFuncDesc->lprgelemdescParam[i].paramdesc.wParamFlags;
  return ((flags & PARAMFLAG_FOPT) && (flags & PARAMFLAG_FHASDEFAULT));
}

BOOL isOptionalParam(FUNCDESC *pFuncDesc,short int i) {
  unsigned short flags;
  
  if (pFuncDesc->lprgelemdescParam == NULL) {
    return FALSE;
  }
  
  flags = pFuncDesc->lprgelemdescParam[i].paramdesc.wParamFlags;
  return (flags & PARAMFLAG_FOPT);
}

short getOptParamCount(FUNCDESC *pFuncDesc,short hi) {
  short i;
  short numOptParams;
  
  numOptParams = 0;
  
  for (i = hi; i >= 0; i--) { 
    if (isOptionalParam(pFuncDesc,i)) {
      numOptParams++;
    }
  }
  return numOptParams;
}	

Scheme_Object *mx_do_get_method_type(int argc,Scheme_Object **argv,
				     INVOKEKIND invKind) {
  MX_TYPEDESC *pTypeDesc;
  ITypeInfo* pITypeInfo;
  FUNCDESC *pFuncDesc;
  VARDESC *pVarDesc;
  Scheme_Object *s,*paramTypes,*returnType;
  char *name;
  short int numActualParams;
  short int numOptParams;
  short int firstOptArg;
  short int hiBound;
  BOOL lastParamIsRetval;
  int i;

  if (MX_COM_OBJP(argv[0]) == FALSE && MX_COM_TYPEP(argv[0]) == FALSE) {
    scheme_wrong_type("com-method-type","com-object or com-type",0,argc,argv);
  }

  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("com-method-type","string",1,argc,argv);
  }
  
  if (MX_COM_OBJ_VAL(argv[0]) == NULL) {
    scheme_signal_error("NULL COM object");
  }
  
  name = SCHEME_STR_VAL(argv[1]);
  
  if (MX_COM_OBJP(argv[0])) {
    pTypeDesc = getMethodType((MX_COM_Object *)argv[0],name,invKind);
  }
  else {
    if (invKind == INVOKE_EVENT) {
      pITypeInfo = eventTypeInfoFromComType((MX_COM_Type *)argv[0]);
    }
    else {
      pITypeInfo = MX_COM_TYPE_VAL(argv[0]);
    }
    pTypeDesc = typeDescFromTypeInfo(name,invKind,pITypeInfo);
  }
  
  if (pTypeDesc->descKind == funcDesc) {
    pFuncDesc = pTypeDesc->pFuncDesc;
    
    paramTypes = scheme_null;
    
    numActualParams = pFuncDesc->cParams;
    
    if (pFuncDesc->cParamsOpt == -1) { // all args > pFuncDesc->cParams - 1 get packaged into SAFEARRAY
      
      // this branch is untested
      
      lastParamIsRetval = FALSE;
      paramTypes = scheme_make_pair(scheme_intern_symbol("..."),paramTypes);
      for (i = numActualParams - 1; i >= 0; i--) { 
	s = elemDescToSchemeType(&pFuncDesc->lprgelemdescParam[i],FALSE,FALSE);
	paramTypes = scheme_make_pair(s,paramTypes);
      }
    }
    else {
      
      if (numActualParams > 0 && 
	  (invKind == INVOKE_FUNC || invKind == INVOKE_PROPERTYGET)) {
	lastParamIsRetval = 
	  (pFuncDesc->lprgelemdescParam[numActualParams-1].paramdesc.wParamFlags & PARAMFLAG_FRETVAL);
      }
      else {
	lastParamIsRetval = FALSE;
      }
      
      if (lastParamIsRetval) {
	hiBound = numActualParams - 2;
      }
      else {
	hiBound = numActualParams - 1;
      }
      
      // parameters that are optional with a default value in IDL are not
      // counted in pFuncDesc->cParamsOpt, so look for default bit flag
      
      numOptParams = getOptParamCount(pFuncDesc,hiBound);
      
      firstOptArg = hiBound - numOptParams + 1;
      
      for (i = hiBound; i >= 0; i--) { 
	s = elemDescToSchemeType(&pFuncDesc->lprgelemdescParam[i],FALSE,i >= firstOptArg);
	paramTypes = scheme_make_pair(s,paramTypes);
      }
    }
  }
  
  // if not a function type, distinguish varDesc's 
  // by invKind
  
  else if (invKind == INVOKE_PROPERTYGET) {
    pVarDesc = pTypeDesc->pVarDesc;
    paramTypes = scheme_null;
    numActualParams = 0;
  }
  else if (invKind == INVOKE_PROPERTYPUT) {
    pVarDesc = pTypeDesc->pVarDesc;
    paramTypes = 
      scheme_make_pair(elemDescToSchemeType(&pVarDesc->elemdescVar,FALSE,FALSE),
		       scheme_null);
    numActualParams = 1;
  }
  
  switch(invKind) {
    
  case INVOKE_FUNC :
    
    // if final parameter is marked as retval, use its type
    
    if (lastParamIsRetval) {
      returnType = elemDescToSchemeType(&pFuncDesc->lprgelemdescParam[numActualParams-1],TRUE,FALSE);
    }
    else {
      returnType = elemDescToSchemeType(&pFuncDesc->elemdescFunc,TRUE,FALSE);
    }
    
    break;
    
  case INVOKE_EVENT :
  case INVOKE_PROPERTYPUT :
    
    returnType = scheme_intern_symbol("void");
    
    break;
    
  case INVOKE_PROPERTYGET :
    
    // pTypeDesc->descKind may be either funcDesc or varDesc
    
    if (pTypeDesc->descKind == funcDesc) {
      
      if (lastParamIsRetval == FALSE || pFuncDesc->cParams == 0) {
	returnType = elemDescToSchemeType(&pFuncDesc->elemdescFunc,TRUE,FALSE);
      }
      else {
	returnType = elemDescToSchemeType(&pFuncDesc->lprgelemdescParam[numActualParams-1],TRUE,FALSE);
      }
    }
    else { // pTypeDesc->descKind == varDesc
      returnType = elemDescToSchemeType(&pVarDesc->elemdescVar,TRUE,FALSE);
    }
    
    break;
  }

  return mx_make_function_type(paramTypes,returnType);
  
}


Scheme_Object *mx_com_method_type(int argc,Scheme_Object **argv) {
  return mx_do_get_method_type(argc,argv,INVOKE_FUNC);
}

Scheme_Object *mx_com_get_property_type(int argc,Scheme_Object **argv) {
  return mx_do_get_method_type(argc,argv,INVOKE_PROPERTYGET);
}

Scheme_Object *mx_com_set_property_type(int argc,Scheme_Object **argv) {
  return mx_do_get_method_type(argc,argv,INVOKE_PROPERTYPUT);
}

Scheme_Object *mx_com_event_type(int argc,Scheme_Object **argv) {
  return mx_do_get_method_type(argc,argv,(INVOKEKIND)INVOKE_EVENT);
}

BOOL schemeValueFitsVarType(Scheme_Object *val,VARTYPE vt) {
  long int longInt;
  
  switch (vt) {
    
  case VT_NULL :
    
    return SCHEME_VOIDP(val);
    
  case VT_UI1 :
    
    return SCHEME_CHARP(val);
    
  case VT_I2 :
    
    return SCHEME_INTP(val) && 
      scheme_get_int_val(val,&longInt) && 
      longInt <= SHRT_MAX && longInt >= SHRT_MIN;
    
  case VT_I4 :
    
    return SCHEME_EXACT_INTEGERP(val) && 
      scheme_get_int_val(val,&longInt);
    
  case VT_R4 :
    
    return SCHEME_FLTP(val) || 
      (SCHEME_DBLP(val) &&
       SCHEME_DBL_VAL(val) >= FLT_MIN &&
       SCHEME_DBL_VAL(val) <= FLT_MAX);
    
  case VT_R8 :
    
    return SCHEME_DBLP(val);
    
  case VT_BSTR :
    
    return SCHEME_STRINGP(val);
    
  case VT_CY :
    
    return MX_CYP(val);
    
  case VT_DATE :
    
    return MX_DATEP(val);
    
  case VT_BOOL :
    
    return TRUE; // ain't Scheme great
    
  case VT_ERROR :
    
    return MX_SCODEP(val);
    
  case VT_UNKNOWN :
    
    return MX_IUNKNOWNP(val);
    
  case VT_DISPATCH :
    
    return MX_COM_OBJP(val);
    
  case VT_VARIANT : // we can package anything into a VARIANTARG
    
    return TRUE;
    
  case VT_USERDEFINED : 
    
    return TRUE;
    
  default :
    
    return FALSE;
    
  }
}


BOOL subArrayFitsVarType(Scheme_Object *val,
			 unsigned short numDims,SAFEARRAYBOUND *bounds,
			 VARTYPE vt) {
  Scheme_Object **els;
  unsigned long len;
  
  if (SCHEME_VECTORP(val) == FALSE) {
    return FALSE;
  }
  
  len = SCHEME_VEC_SIZE(val);
  
  if (len != bounds->cElements) {
    return FALSE;
  }
  
  els = SCHEME_VEC_ELS(val);
  
  if (numDims == 1) { // innermost vector
    for (unsigned long i = 0; i < len; i++) {
      if (schemeValueFitsVarType(els[i],vt) == FALSE) {
	return FALSE;
      }
    }
  }
  else {
    for (unsigned long i = 0; i < len; i++) {
      
      // recursion, the programmer's best friend
      
      if (subArrayFitsVarType(els[i],numDims - 1,bounds + 1,vt) == FALSE) {
	return FALSE;
      }
    } 
  }
  
  return TRUE;
}

BOOL schemeValueFitsElemDesc(Scheme_Object *val,ELEMDESC *pElemDesc) {
  unsigned short flags;
  
  // if default available, check value has appropriate type
  
  flags = pElemDesc->paramdesc.wParamFlags;
  if (flags & PARAMFLAG_FOPT) {
    if (val == mx_omit_obj) {
      return TRUE;
    }
    if (flags & PARAMFLAG_FHASDEFAULT)  {
      return schemeValueFitsVarType(val,pElemDesc->paramdesc.pparamdescex->varDefaultValue.vt);
    }
  }
  
  // if array, check we have a vector of proper dimension and contained types 
  
  if (pElemDesc->tdesc.vt & VT_ARRAY) {
    return subArrayFitsVarType(val,
			       pElemDesc->tdesc.lpadesc->cDims,
			       pElemDesc->tdesc.lpadesc->rgbounds,
			       pElemDesc->tdesc.lpadesc->tdescElem.vt);
    
  }
  
  
  // if box, check the contained value
  
  if (pElemDesc->tdesc.vt == VT_PTR) {
    if (SCHEME_BOXP(val) == FALSE) {
      return FALSE;
    }
    
    return schemeValueFitsVarType(SCHEME_BOX_VAL(val),pElemDesc->tdesc.lptdesc->vt);
  }
  
  // not array or box or default value
  
  return schemeValueFitsVarType(val,pElemDesc->tdesc.vt);
}

VARIANT_BOOL schemeValToBool(Scheme_Object *val) {
  return SCHEME_FALSEP(val) ? 0 : 0xFFFF;
}

VARTYPE schemeValueToVarType(Scheme_Object *obj) {
  
  // test for global constants
  
  if (SCHEME_FALSEP(obj)) {
    return VT_BOOL;
  }
  
  if (SCHEME_VOIDP(obj)) {
    return VT_NULL;
  }
  
  // handle fixnums

  if (SCHEME_INTP(obj)) {
    return VT_I4;
  }
  
  // otherwise, dispatch on value type
  
  switch(obj->type) {
  case scheme_char_type :
    return VT_UI1;
  case scheme_integer_type :
    return VT_I4;
  case scheme_float_type :
    return VT_R4;
  case scheme_double_type :
    return VT_R8;
  case scheme_string_type :
    return VT_BSTR;
  case scheme_vector_type : // may need to specify elt type
    return VT_ARRAY;
  }
  
  scheme_signal_error("Unable to coerce value to VARIANT");
  
  return 0; // keep compiler happy
}

void *allocParamMemory(size_t n) {
  void *retval;
  
  // do we need a semaphore here?
  
  retval = scheme_malloc(n);
  scheme_dont_gc_ptr(retval);
  return retval;
}

void marshallSchemeValueToVariant(Scheme_Object *val,VARIANTARG *pVariantArg) {
  
  // called when COM type spec allows any VARIANT
  
  if (SCHEME_CHARP(val)) {
    pVariantArg->vt = VT_UI1;
    pVariantArg->bVal = SCHEME_CHAR_VAL(val);
    return;
  }
  
  if (SCHEME_EXACT_INTEGERP(val)) {
    pVariantArg->vt = VT_I4;
    scheme_get_int_val(val,&pVariantArg->lVal);
    return;
  }
  
#ifdef MZ_USE_SINGLE_FLOATS    
  if (SCHEME_FLTP(val)) {
    pVariantArg->vt = VT_R4;
    pVariantArg->fltVal = SCHEME_FLT_VAL(val);
    return;
  }
#endif
  
  if (SCHEME_DBLP(val)) {
    pVariantArg->vt = VT_R8;
    pVariantArg->dblVal = SCHEME_DBL_VAL(val);
    return;
  }
  
  if (SCHEME_STRINGP(val)) {
    pVariantArg->vt = VT_BSTR;
    pVariantArg->bstrVal = schemeStringToBSTR(val);
    return;
  }
  
  if (SCHEME_SYMBOLP(val)) {
    char *s = SCHEME_SYM_VAL(val);
    pVariantArg->vt = VT_BSTR;
    pVariantArg->bstrVal = stringToBSTR(s,strlen(s));
    return;
  }
  
  if (MX_CYP(val)) {
    pVariantArg->vt = VT_CY;
    pVariantArg->cyVal = MX_CY_VAL(val);
    return;
  }
  
  if (MX_DATEP(val)) {
    pVariantArg->vt = VT_DATE;
    pVariantArg->date = MX_DATE_VAL(val);
    return;
  }
  
  if (val == scheme_false) {
    pVariantArg->vt = VT_BOOL;
    pVariantArg->boolVal = 0;
    return;
  }
  
  if (val == scheme_true) {
    pVariantArg->vt = VT_BOOL;
    pVariantArg->boolVal = -1;
    return;
  }
  
  if (MX_SCODEP(val)) {
    pVariantArg->vt = VT_ERROR;
    pVariantArg->scode = MX_SCODE_VAL(val);
    return;
  }
  
  if (MX_COM_OBJP(val)) {
    pVariantArg->pdispVal = MX_COM_OBJ_VAL(val);
    pVariantArg->vt = VT_DISPATCH;
    return;
  }
  
  if (MX_IUNKNOWNP(val)) {
    pVariantArg->vt = VT_UNKNOWN;
    pVariantArg->punkVal = MX_IUNKNOWN_VAL(val);
    return;
  }
  
  if (SCHEME_VECTORP (val)) {
    pVariantArg->vt = VT_ARRAY | VT_VARIANT;
    pVariantArg->parray = schemeVectorToSafeArray(val);
    return;
  }
  
  scheme_signal_error("Unable to inject Scheme value into VARIANT");
  
}

void marshallSchemeValue(Scheme_Object *val,VARIANTARG *pVariantArg) {
  char errBuff[128];

  if (pVariantArg->vt & VT_ARRAY) {
    pVariantArg->parray = schemeVectorToSafeArray(val);
  }
  
  switch (pVariantArg->vt) {
    
  case VT_NULL :
    
    break;
    
  case VT_UI1 :
    
    pVariantArg->bVal = SCHEME_CHAR_VAL(val);
    break;
    
  case VT_UI1 | VT_BYREF :
    
    pVariantArg->pbVal = (unsigned char *)allocParamMemory(sizeof(unsigned char));
    *pVariantArg->pbVal = (unsigned char)SCHEME_CHAR_VAL(SCHEME_BOX_VAL(val));
    break;
    
  case VT_I2 :
    
    pVariantArg->iVal = (short)SCHEME_INT_VAL(val);
    break;
    
  case VT_I2 | VT_BYREF :
    
    pVariantArg->piVal = (short *)allocParamMemory(sizeof(short));
    *pVariantArg->piVal = (short)SCHEME_INT_VAL(SCHEME_BOX_VAL(val));
    
    break;
    
  case VT_I4 :
    
    pVariantArg->lVal = SCHEME_INT_VAL(val);
    break;
    
  case VT_I4 | VT_BYREF :
    
    pVariantArg->plVal = (long *)allocParamMemory(sizeof(long));
    *pVariantArg->plVal = (long)SCHEME_INT_VAL(SCHEME_BOX_VAL(val));
    break;
    
  case VT_R4 :
    
    pVariantArg->fltVal = (float)SCHEME_DBL_VAL(val);
    break;
    
  case VT_R4 | VT_BYREF :
    
    pVariantArg->pfltVal = (float *)allocParamMemory(sizeof(float));
    *pVariantArg->pfltVal = (float)SCHEME_DBL_VAL(SCHEME_BOX_VAL(val));
    break;
    
  case VT_R8 :
    
    pVariantArg->dblVal = SCHEME_DBL_VAL(val);
    break;
    
  case VT_R8 | VT_BYREF :
    
    pVariantArg->pdblVal = (double *)allocParamMemory(sizeof(double));
    *pVariantArg->pdblVal = (double)SCHEME_DBL_VAL(SCHEME_BOX_VAL(val));
    break;
    
  case VT_BSTR :
    pVariantArg->bstrVal = schemeStringToBSTR(val);
    break;
    
  case VT_BSTR | VT_BYREF :
    
    pVariantArg->pbstrVal = (BSTR *)allocParamMemory(sizeof(BSTR));
    *pVariantArg->pbstrVal = schemeStringToBSTR(val);
    break;
    
  case VT_CY :
    
    pVariantArg->cyVal = MX_CY_VAL(val);
    break;
    
  case VT_CY | VT_BYREF :
    
    pVariantArg->pcyVal = (CY *)allocParamMemory(sizeof(CY));
    *pVariantArg->pcyVal = (CY)MX_CY_VAL(val);
    break;
    
  case VT_DATE :
    
    pVariantArg->date = MX_DATE_VAL(val);
    break;
    
  case VT_DATE | VT_BYREF :
    
    pVariantArg->pdate = (DATE *)allocParamMemory(sizeof(DATE));
    *pVariantArg->pdate = (DATE)MX_DATE_VAL(val);
    break;
    
  case VT_BOOL :
    
    pVariantArg->boolVal = schemeValToBool(val);
    break;
    
  case VT_BOOL | VT_BYREF :
    
    pVariantArg->pboolVal = (VARIANT_BOOL *)allocParamMemory(sizeof(VARIANT_BOOL));
    *pVariantArg->pboolVal = schemeValToBool(val);
    break;
    
  case VT_ERROR :
    
    pVariantArg->scode = MX_SCODE_VAL(val);
    break;
    
  case VT_ERROR | VT_BYREF :
    
    pVariantArg->pscode = (SCODE *)allocParamMemory(sizeof(SCODE));
    *pVariantArg->pscode = MX_SCODE_VAL(SCHEME_BOX_VAL(val));
    break;
    
  case VT_DISPATCH :
    
    pVariantArg->pdispVal = MX_COM_OBJ_VAL(val);
    break;
    
  case VT_DISPATCH | VT_BYREF :
    
    pVariantArg->ppdispVal = (IDispatch **)allocParamMemory(sizeof(IDispatch *));
    *pVariantArg->ppdispVal = MX_COM_OBJ_VAL(SCHEME_BOX_VAL(val));
    break;
    
  case VT_VARIANT | VT_BYREF :
    
    // pass boxed value of almost-arbitrary type

    Scheme_Object *boxedVal;

    boxedVal = SCHEME_BOX_VAL(val);
    
    pVariantArg->pvarVal = (VARIANTARG *)allocParamMemory(sizeof(VARIANTARG));
    pVariantArg->pvarVal->vt = schemeValueToVarType(boxedVal);
    marshallSchemeValue(boxedVal,pVariantArg->pvarVal);
    break;
    
  case VT_UNKNOWN :
    pVariantArg->punkVal = MX_IUNKNOWN_VAL(val);
    break;
    
  case VT_UNKNOWN | VT_BYREF :
    pVariantArg->ppunkVal = (IUnknown **)allocParamMemory(sizeof(IUnknown *));
    *pVariantArg->ppunkVal = MX_IUNKNOWN_VAL(SCHEME_BOX_VAL(val));
    break;
    
  case VT_USERDEFINED :
  case VT_VARIANT :
    marshallSchemeValueToVariant(val,pVariantArg);
    break;
    
  default :
    sprintf(errBuff,"Unable to marshall Scheme value into VARIANT: 0x%X",
	    pVariantArg->vt);
    scheme_signal_error(errBuff);

  }
}

Scheme_Object *variantToSchemeObject(VARIANTARG *pVariantArg) {
  char errBuff[128];

  if (pVariantArg->vt & VT_ARRAY) {
    return safeArrayToSchemeVector(pVariantArg->parray);
  }
  
  switch(pVariantArg->vt) {
    
  case VT_EMPTY :
  case VT_NULL :
    
    return scheme_void;
    
  case VT_UI1 :
    
    return scheme_make_character((char)(pVariantArg->bVal));
    
  case VT_I2 :
    
    return scheme_make_integer(pVariantArg->iVal);
    
  case VT_I4 :
    
    return scheme_make_integer(pVariantArg->lVal);
    
  case VT_R4 :
    
#ifdef MZ_USE_SINGLE_FLOATS
    return scheme_make_float(pVariantArg->fltVal);
#else
    return scheme_make_double((double)(pVariantArg->fltVal));
#endif
    
  case VT_R8 :
    
    return scheme_make_double(pVariantArg->dblVal);
    
  case VT_BSTR :
    
    return BSTRToSchemeString(pVariantArg->bstrVal);
    
  case VT_CY :
    
    return mx_make_cy(&pVariantArg->cyVal);
    
  case VT_DATE :
    
    return mx_make_date(&pVariantArg->date);
    
  case VT_BOOL :
    
    return mx_make_bool(pVariantArg->boolVal);
    
  case VT_ERROR :
    
    return mx_make_scode(pVariantArg->scode);
    
  case VT_DISPATCH :
    
    return mx_make_idispatch(pVariantArg->pdispVal);
    
  case VT_UNKNOWN :
    
    return mx_make_iunknown(pVariantArg->punkVal);
    
  default :
    
    sprintf(errBuff,"Can't make Scheme value from VARIANT 0x%X",
	    pVariantArg->vt);
    scheme_signal_error(errBuff);
    
  }
  
  return NULL;
}

void unmarshallVariant(Scheme_Object *val,VARIANTARG *pVariantArg) {
  
  switch(pVariantArg->vt) {
    
  case VT_UI1 | VT_BYREF :
    
    SCHEME_BOX_VAL(val) = scheme_make_character((char)(*pVariantArg->pbVal));
    scheme_gc_ptr_ok(pVariantArg->pbVal);
    break;
    
  case VT_I2 | VT_BYREF :
    
    SCHEME_BOX_VAL(val) = scheme_make_integer(*pVariantArg->piVal);
    scheme_gc_ptr_ok(pVariantArg->piVal);
    break;
    
  case VT_I4 | VT_BYREF :
    
    SCHEME_BOX_VAL(val) = scheme_make_integer(*pVariantArg->plVal);
    scheme_gc_ptr_ok(pVariantArg->plVal);
    break;
    
  case VT_R4 | VT_BYREF :
    
#ifdef MZ_USE_SINGLE_FLOATS
    SCHEME_BOX_VAL(val) = scheme_make_float(*pVariantArg->pfltVal);
#else
    SCHEME_BOX_VAL(val) = scheme_make_double((double)(*pVariantArg->pfltVal));
#endif
    scheme_gc_ptr_ok(pVariantArg->pfltVal);
    break;
    
  case VT_R8 | VT_BYREF :
    
    SCHEME_BOX_VAL(val) = scheme_make_double(*pVariantArg->pdblVal);
    scheme_gc_ptr_ok(pVariantArg->pdblVal);
    break;
    
  case VT_CY | VT_BYREF :
    
    SCHEME_BOX_VAL(val) = mx_make_cy(pVariantArg->pcyVal);
    scheme_gc_ptr_ok(pVariantArg->pcyVal);
    break;
    
  case VT_DATE | VT_BYREF :
    
    SCHEME_BOX_VAL(val) = mx_make_date(pVariantArg->pdate);
    scheme_gc_ptr_ok(pVariantArg->pdate);
    break;
    
  case VT_BOOL | VT_BYREF :
    
    SCHEME_BOX_VAL(val) = mx_make_bool(*pVariantArg->pboolVal);
    scheme_gc_ptr_ok(pVariantArg->pboolVal);
    break;
    
  case VT_ERROR | VT_BYREF :
    
    SCHEME_BOX_VAL(val) = mx_make_scode(*pVariantArg->pscode);
    scheme_gc_ptr_ok(pVariantArg->pscode);
    break;
    
  case VT_DISPATCH | VT_BYREF :
    
    SCHEME_BOX_VAL(val) = mx_make_idispatch(*pVariantArg->ppdispVal);
    scheme_gc_ptr_ok(pVariantArg->ppdispVal);
    break;
    
  case VT_UNKNOWN | VT_BYREF :
    
    SCHEME_BOX_VAL(val) = mx_make_iunknown(*pVariantArg->ppunkVal);
    scheme_gc_ptr_ok(pVariantArg->ppunkVal);
    break;
    
  case VT_VARIANT | VT_BYREF :
    
    SCHEME_BOX_VAL(val) = variantToSchemeObject(pVariantArg->pvarVal);
    scheme_gc_ptr_ok(pVariantArg->pvarVal);
    break;
    
  case VT_BSTR :
    
    updateSchemeStringFromBSTR(val,pVariantArg->bstrVal);
    SysFreeString(pVariantArg->bstrVal);
    break;
    
  case VT_BSTR | VT_BYREF :
    
    SCHEME_BOX_VAL(val) = BSTRToSchemeString(*pVariantArg->pbstrVal);
    SysFreeString(*pVariantArg->pbstrVal);
    scheme_gc_ptr_ok(pVariantArg->pbstrVal);
    break;
    
  default :
    
    ;   
    
    // no unmarshalling or cleanup needed
    
  }
}

short int buildMethodArgumentsUsingFuncDesc(FUNCDESC *pFuncDesc,
					    INVOKEKIND invKind,
					    int argc,Scheme_Object **argv,
					    DISPPARAMS *methodArguments) {
  char errBuff[256];
  short int numParamsPassed;
  short int numOptParams;
  BOOL lastParamIsRetval;
  int i,j,k;
  static DISPID dispidPropPut = DISPID_PROPERTYPUT;

  numParamsPassed = pFuncDesc->cParams;
  
  if (pFuncDesc->cParams > 0 && 
      ((invKind == INVOKE_PROPERTYGET || invKind == INVOKE_FUNC) && 
       (pFuncDesc->lprgelemdescParam[numParamsPassed-1].paramdesc.wParamFlags 
	& PARAMFLAG_FRETVAL))) {
    // last parameter is retval
    numParamsPassed--;
    lastParamIsRetval = TRUE;
  }
  else {
    lastParamIsRetval = FALSE;
  }

  if (pFuncDesc->cParamsOpt == -1) {  // last args get packaged into SAFEARRAY
    
    // this branch is untested
    
    // optional parameters with default values not counted in pFuncDesc->cParamsOpt
    
    numOptParams = getOptParamCount(pFuncDesc,numParamsPassed - 1);

    if (argc < numParamsPassed + 2 - 1) {
      sprintf(errBuff,"%s (%s \"%s\")",
	      mx_fun_string(invKind),
	      inv_kind_string(invKind),
	      SCHEME_STR_VAL(argv[1]));
      scheme_wrong_count(errBuff,numParamsPassed+1,-1,argc,argv);
    }
  }
  else {
    
    // optional parameters with default values not counted in pFuncDesc->cParamsOpt
    
    numOptParams = getOptParamCount(pFuncDesc,numParamsPassed - 1);
    
    if (argc < numParamsPassed - numOptParams + 2 ||  // too few
	argc > numParamsPassed + 2) {  // too many
      sprintf(errBuff,"%s (%s \"%s\")",
	      mx_fun_string(invKind),
	      inv_kind_string(invKind),
	      SCHEME_STR_VAL(argv[1]));
      scheme_wrong_count(errBuff,numParamsPassed-numOptParams+2,numParamsPassed+2,argc,argv);
    }
  }
  
  // compare types of actual arguments to prescribed types
  
  for (i = 0,k = 2; i < argc - 2; i++,k++) {
    
    // i = index of ELEMDESC's
    // k = index of actual args in argv
    
    if (schemeValueFitsElemDesc(argv[k],&pFuncDesc->lprgelemdescParam[i]) == FALSE) {
      sprintf(errBuff,"%s (%s \"%s\")",mx_fun_string(invKind),
	      inv_kind_string(invKind),SCHEME_STR_VAL(argv[1]));
      scheme_wrong_type(errBuff,
			SCHEME_SYM_VAL(elemDescToSchemeType(&(pFuncDesc->lprgelemdescParam[i]),FALSE,FALSE)),
			k,argc,argv);
    }
  }

  switch(invKind) {
    
  case INVOKE_PROPERTYPUT :
    
    // Named argument represents the assigned value
    
    methodArguments->rgdispidNamedArgs = &dispidPropPut;
    methodArguments->cNamedArgs = methodArguments->cArgs = 1;
    break;
    
  case INVOKE_PROPERTYGET :
    
    methodArguments->rgdispidNamedArgs = NULL;
    methodArguments->cNamedArgs = 0;
    methodArguments->cArgs = numParamsPassed;
    break;
    
  default :
    
    methodArguments->rgdispidNamedArgs = NULL;
    methodArguments->cNamedArgs = 0;
    methodArguments->cArgs = numParamsPassed;
    break;
  }
  
  if (numParamsPassed > 0) {
    methodArguments->rgvarg = 
      (VARIANTARG *)scheme_malloc(numParamsPassed * sizeof(VARIANTARG));
    scheme_dont_gc_ptr(methodArguments->rgvarg);
  }
  
  // marshall Scheme argument list into COM argument list
  // arguments are in reverse order in rgvarg
  
  for (i = 0,j = numParamsPassed - 1,k = 2; i < argc - 2; i++,j--,k++) {
    
    // i = index of ELEMDESC's
    // j = index of VARIANTARG's
    
    VariantInit(&methodArguments->rgvarg[j]);
    
    if (argv[k] == mx_omit_obj) { // omitted argument
      methodArguments->rgvarg[j].vt = VT_ERROR;
      methodArguments->rgvarg[j].lVal = DISP_E_PARAMNOTFOUND;
    }
    else {
      methodArguments->rgvarg[j].vt = 
	getVarTypeFromElemDesc(&pFuncDesc->lprgelemdescParam[i]);
      marshallSchemeValue(argv[k],&methodArguments->rgvarg[j]);
    }
  }
  
  // omitted optional arguments
  // supply default if available
  
  if (numOptParams > 0) {
    for (i = argc - 2,j = numParamsPassed - 1 - (argc - 2); j >= 0; i++,j--) {
      if (isDefaultParam(pFuncDesc,i)) {
	methodArguments->rgvarg[j] =
	  pFuncDesc->lprgelemdescParam[i].paramdesc.pparamdescex->varDefaultValue;
      }
      else {
	VariantInit(&methodArguments->rgvarg[j]);
	methodArguments->rgvarg[j].vt = VT_ERROR;
	methodArguments->rgvarg[j].lVal = DISP_E_PARAMNOTFOUND;
      }
    }
  }
  
  return numParamsPassed;
}

short int buildMethodArgumentsUsingVarDesc(VARDESC *pVarDesc,
					   INVOKEKIND invKind,
					   int argc,Scheme_Object **argv,
					   DISPPARAMS *methodArguments) {
  char errBuff[256];
  short int numParamsPassed;
  int i,j,k;
  static DISPID dispidPropPut = DISPID_PROPERTYPUT;

  if (invKind == INVOKE_PROPERTYGET) {
    numParamsPassed = 0;
  }
  else if (invKind == INVOKE_PROPERTYPUT) {
    numParamsPassed = 1;
  }
  
  if (argc != numParamsPassed + 2) {
    sprintf(errBuff,"%s (%s \"%s\")",
	    mx_fun_string(invKind),
	    inv_kind_string(invKind),
	    SCHEME_STR_VAL(argv[1]));
    scheme_wrong_count(errBuff,
		       numParamsPassed + 2,numParamsPassed + 2,
		       argc,argv);
  }
  
  switch(invKind) {
    
  case INVOKE_PROPERTYPUT :
    
    methodArguments->rgdispidNamedArgs = &dispidPropPut;
    methodArguments->cNamedArgs = methodArguments->cArgs = 1;
    break;
    
  case INVOKE_PROPERTYGET :
    
    methodArguments->rgdispidNamedArgs = NULL;
    methodArguments->cNamedArgs = 0;
    methodArguments->cArgs = numParamsPassed;
    break;
    
  }
  
  if (numParamsPassed > 0) {
    methodArguments->rgvarg = 
      (VARIANTARG *)scheme_malloc(numParamsPassed * sizeof(VARIANTARG));
    scheme_dont_gc_ptr(methodArguments->rgvarg);
  }
  
  // marshall Scheme argument list into COM argument list
  
  for (i = 0,j = numParamsPassed - 1,k = 2; i < numParamsPassed; i++,j--,k++) {
    
    // i = index of ELEMDESC's
    // j = index of VARIANTARG's
    
    VariantInit(&methodArguments->rgvarg[j]);
    methodArguments->rgvarg[j].vt = 
      getVarTypeFromElemDesc(&pVarDesc->elemdescVar);
    
    marshallSchemeValue(argv[k],&methodArguments->rgvarg[j]);
  }

  return numParamsPassed;
}

short int buildMethodArguments(MX_TYPEDESC *pTypeDesc,
			       INVOKEKIND invKind,
			       int argc,Scheme_Object **argv,
			       DISPPARAMS *methodArguments) {
  if (pTypeDesc->descKind == funcDesc) {
    return buildMethodArgumentsUsingFuncDesc(pTypeDesc->pFuncDesc,
					     invKind,argc,argv,
					     methodArguments);
  }
  
  return buildMethodArgumentsUsingVarDesc(pTypeDesc->pVarDesc,
					  invKind,argc,argv,
					  methodArguments);
  
}

static Scheme_Object *mx_make_call(int argc,Scheme_Object **argv,
				   INVOKEKIND invKind) {
  MX_TYPEDESC *pTypeDesc;
  DISPPARAMS methodArguments; 
  VARIANT methodResult;
  EXCEPINFO exnInfo;
  unsigned int errorIndex;
  IDispatch *pIDispatch;
  char *name;
  short numParamsPassed;
  int i,j;
  HRESULT hr;
  
  if (MX_COM_OBJP(argv[0]) == FALSE) {
    scheme_wrong_type(mx_fun_string(invKind),"com-object",0,argc,argv);
  }
  
  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type(mx_fun_string(invKind),"string",1,argc,argv);
  }
  
  pIDispatch = MX_COM_OBJ_VAL(argv[0]);
  
  if (pIDispatch == NULL) {
    scheme_signal_error("NULL COM object");
  }
  
  name = SCHEME_STR_VAL(argv[1]);
  
  // check arity, types of method arguments
  
  pTypeDesc = getMethodType((MX_COM_Object *)argv[0],name,invKind);

  numParamsPassed = buildMethodArguments(pTypeDesc,
					 invKind,
					 argc,argv,
					 &methodArguments);
  
  if (invKind != INVOKE_PROPERTYPUT) {
    VariantInit(&methodResult);
  }
  
  // invoke requested method
  
  hr = pIDispatch->Invoke(pTypeDesc->memID,IID_NULL,LOCALE_SYSTEM_DEFAULT,
			  invKind,
			  &methodArguments,
			  (invKind == INVOKE_PROPERTYPUT) ? NULL : &methodResult,
			  &exnInfo,
			  &errorIndex);
  
  if (hr == DISP_E_EXCEPTION) {
    char errBuff[2048];
    char description[1024];
    BOOL hasErrorCode;
    BOOL hasDescription;
    
    hasErrorCode = (exnInfo.wCode > 0); 
    hasDescription = (exnInfo.bstrDescription != NULL);
    
    if (hasDescription) {
      unsigned int len;

      len = SysStringLen(exnInfo.bstrDescription);
      WideCharToMultiByte(CP_ACP,(DWORD)0,
			  exnInfo.bstrDescription,len,
			  description,sizeof(description)-1,
			  NULL,NULL);
      description[len] = '\0';
    }
    
    if (hasErrorCode) {
      sprintf(errBuff,
	      "COM object exception, error code 0x%X%s%s",
	      exnInfo.wCode,
	      hasDescription ? "\nDescription: " : "" ,
	      hasDescription ? description : "");
      scheme_signal_error(errBuff);
    }
    else {
      sprintf(errBuff,
	      "COM object exception%s%s",
	      hasDescription ? "\nDescription: " : "" ,
	      hasDescription ? description : "");
      codedComError(errBuff,exnInfo.scode);
    }
  }

  if (hr != S_OK) {
    char buff[2048];
    sprintf(buff,"\"%s\" (%s) failed",
	    SCHEME_STR_VAL(argv[1]),inv_kind_string(invKind));
    codedComError(buff,hr);
  }
  
  // unmarshall data passed by reference, cleanup

  for (i = 2,j = numParamsPassed - 1; i < argc; i++,j--) {
    unmarshallVariant(argv[i],&methodArguments.rgvarg[j]);
  }
  
  if (numParamsPassed > 0) {
    scheme_gc_ptr_ok(methodArguments.rgvarg);
  }  
  
  if (invKind == INVOKE_PROPERTYPUT) {
    return scheme_void;
  }
  
  // unmarshall return value
  
  return variantToSchemeObject(&methodResult);
  
}

BOOL _stdcall drawContinue(DWORD data) {
  return TRUE;
}

Scheme_Object *mx_com_invoke(int argc,Scheme_Object **argv) {
  return mx_make_call(argc,argv,INVOKE_FUNC);
}

Scheme_Object *mx_com_get_property(int argc,Scheme_Object **argv) {
  return mx_make_call(argc,argv,INVOKE_PROPERTYGET); 
}

Scheme_Object *mx_com_set_property(int argc,Scheme_Object **argv) {
  return mx_make_call(argc,argv,INVOKE_PROPERTYPUT); 
}

Scheme_Object *mx_all_clsid(int argc,Scheme_Object **argv,char **attributes) {
  LONG result;
  Scheme_Object *retval;
  HKEY hkey,hsubkey;
  FILETIME fileTime;
  unsigned long keyIndex;
  TCHAR clsidBuffer[256];
  DWORD clsidBufferSize;
  DWORD dataType;
  BYTE dataBuffer[256];
  DWORD dataBufferSize;
  BOOL loopFlag;
  char **p;
  
  retval = scheme_null;
  
  result = RegOpenKeyEx(HKEY_CLASSES_ROOT,
			"CLSID",
			(DWORD)0,
			KEY_READ,
			&hkey);
  
  if (result != ERROR_SUCCESS) {
    return retval;
  }	    
  
  // enumerate subkeys until we find the one we want
  
  keyIndex = 0;
  
  while (1) {
    
    // get next subkey
    
    clsidBufferSize = sizeray(clsidBuffer);
    
    result = RegEnumKeyEx(hkey,keyIndex++,
			  clsidBuffer,
			  &clsidBufferSize,
			  0,NULL,NULL,
			  &fileTime);
    
    if (result == ERROR_NO_MORE_ITEMS) {
      break;
    }		
    
    if (strlen(clsidBuffer) != CLSIDLEN) { // not a CLSID -- bogus entry
      continue;
    }
    
    // open subkey
    
    result = RegOpenKeyEx(hkey,clsidBuffer,
			  (DWORD)0,
			  KEY_READ,&hsubkey);
    
    if (result != ERROR_SUCCESS) {
      scheme_signal_error("Error while searching Windows registry");
    }	    
    
    dataBufferSize = sizeof(dataBuffer);
    
    RegQueryValueEx(hsubkey,"",0,&dataType,dataBuffer,&dataBufferSize);
    
    if (dataType == REG_SZ) {
      int subkeyIndex;
      TCHAR subkeyBuffer[256];
      DWORD subkeyBufferSize;
      
      subkeyIndex = 0;
      
      loopFlag = TRUE;
      
      while (loopFlag) {
	
	subkeyBufferSize = sizeray(subkeyBuffer);
	
	result = RegEnumKeyEx(hsubkey,subkeyIndex++,
			      subkeyBuffer,
			      &subkeyBufferSize,
			      0,NULL,NULL,
			      &fileTime);
	
	if (result == ERROR_NO_MORE_ITEMS) {
	  break;
	}
	
	p = attributes;
	
	while (*p) {
	  if (stricmp(subkeyBuffer,*p) == 0) {
	    retval = scheme_make_pair(scheme_make_string((char *)dataBuffer),
				      retval);
	    loopFlag = FALSE;
	    break; // *p loop
	  }
	  p++;
	}
      }
    }
    
    RegCloseKey(hsubkey);  
  } 
  
  RegCloseKey(hkey);  
  
  return retval;
}

Scheme_Object *mx_all_controls(int argc,Scheme_Object **argv) {
  return mx_all_clsid(argc,argv,controlAttributes);
}

Scheme_Object *mx_all_coclasses(int argc,Scheme_Object **argv) {
  return mx_all_clsid(argc,argv,objectAttributes);
}

Scheme_Object *mx_com_object_eq(int argc,Scheme_Object **argv) {
  IUnknown *pIUnknown1,*pIUnknown2;
  IDispatch *pIDispatch1,*pIDispatch2;
  Scheme_Object *retval;
  
  for (int i = 0; i < 2; i++) {
    if (MX_COM_OBJP(argv[i]) == FALSE) {
      scheme_wrong_type("com-object-eq?","com-object",i,argc,argv);
    }
  }
  
  pIDispatch1 = MX_COM_OBJ_VAL(argv[0]);
  pIDispatch2 = MX_COM_OBJ_VAL(argv[1]);
  
  // these should never fail
  
  pIDispatch1->QueryInterface(IID_IUnknown,(void **)&pIUnknown1);
  pIDispatch2->QueryInterface(IID_IUnknown,(void **)&pIUnknown2);
  
  retval = (pIUnknown1 == pIUnknown2) ? scheme_true : scheme_false;
  
  pIUnknown1->Release();
  pIUnknown2->Release();
  
  return retval;
}

Scheme_Object *mx_com_object_pred(int argc,Scheme_Object **argv) {
  return MX_COM_OBJP(argv[0]) ? scheme_true : scheme_false;
}
  
Scheme_Object *mx_document_objects(int argc,Scheme_Object **argv) {
  HRESULT hr;  
  IHTMLDocument2 *pDocument;
  IHTMLElement *pBody;
  IHTMLElementCollection *pObjectsCollection;
  long numObjects;
  Scheme_Object *retval;
  int i;
  IDispatch *pObjectDispatch;
  MX_COM_Object *com_object;
  
  if (MX_DOCUMENTP(argv[0]) == FALSE) {
    scheme_wrong_type("document_objects","mx-document",0,argc,argv);
  }
  
  pDocument = MX_DOCUMENT_VAL(argv[0]); 
  
  hr = pDocument->get_body(&pBody);
  
  if (hr != S_OK || pBody == NULL) {
    codedComError("document-objects: Can't find document BODY",hr);
  }
  
  pObjectsCollection = getBodyElementsWithTag(pBody,"OBJECT");
  
  pBody->Release();
  
  pObjectsCollection->get_length(&numObjects);
  
  retval = scheme_null;
  
  for (i = numObjects - 1; i >= 0; i--) {
    
    pObjectDispatch = getObjectInCollection(pObjectsCollection,i);
    
    com_object = (MX_COM_Object *)scheme_malloc(sizeof(MX_COM_Object));
    
    com_object->type = mx_com_object_type; 
    com_object->pIDispatch = pObjectDispatch;
    com_object->clsId = emptyClsId;
    com_object->pITypeInfo = NULL;
    com_object->pEventTypeInfo = NULL;
    com_object->pIConnectionPoint = NULL;
    com_object->pISink = NULL;
    com_object->connectionCookie = (DWORD)0;
    com_object->released = FALSE;

    mx_register_com_object((Scheme_Object *)com_object,pObjectDispatch);
    
    retval = scheme_make_pair((Scheme_Object *)com_object,retval);
  }
  
  pObjectsCollection->Release();
  
  return retval;
}

Scheme_Object *mx_elements_with_tag(int argc,Scheme_Object **argv) {
  HRESULT hr;
  IHTMLDocument2 *pDocument;
  IHTMLElement *pBody,*pIHTMLElement;
  IHTMLElementCollection *pCollection;
  long numObjects;
  Scheme_Object *retval;
  MX_Element *elt;
  IDispatch *pDispatch;
  int i;
  
  if (MX_DOCUMENTP(argv[0]) == FALSE) {
    scheme_wrong_type("document-elements-with-tag","mx-document",0,argc,argv);
  }
  
  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("document-elements-with-tag","string",1,argc,argv);
  }
  
  pDocument = MX_DOCUMENT_VAL(argv[0]); 
  
  pDocument->get_body(&pBody);
  
  if (pBody == NULL) {
    scheme_signal_error("Can't find document BODY");
  }
  
  pCollection = getBodyElementsWithTag(pBody,SCHEME_STR_VAL(argv[1]));
  
  pBody->Release();
  
  pCollection->get_length(&numObjects);
  
  retval = scheme_null;
  
  for (i = numObjects - 1; i >= 0; i--) {
    
    pDispatch = getElementInCollection(pCollection,i);

    hr = pDispatch->QueryInterface(IID_IHTMLElement,(void **)&pIHTMLElement);
    
    if (hr != S_OK || pIHTMLElement == NULL) {
      codedComError("document-elements-with-tag: Can't get IHTMLElement interface",hr);
    }

    elt = (MX_Element *)scheme_malloc(sizeof(MX_Element));
  
    elt->type = mx_element_type;
    elt->released = FALSE;
    elt->valid = TRUE;
    elt->pIHTMLElement = pIHTMLElement;

    pIHTMLElement->AddRef();  

    mx_register_simple_com_object((Scheme_Object *)elt,pIHTMLElement);
  
    retval = scheme_make_pair((Scheme_Object *)elt,retval);
  }
  
  pCollection->Release();
  
  return retval;
}

CLSID getCLSIDFromCoClass(const char *name) { 
  HKEY hkey,hsubkey;
  LONG result;
  FILETIME fileTime;
  unsigned long keyIndex;
  TCHAR clsIdBuffer[256];
  OLECHAR oleClsIdBuffer[256];
  DWORD clsIdBufferSize;
  DWORD dataType;
  BYTE dataBuffer[256];
  DWORD dataBufferSize;
  CLSID clsId;
  BOOL loopFlag;
  int count;
  unsigned int len;
  char **p;
  
  // dummy entry
  
  clsId = emptyClsId;
  
  // get HKEY to Interfaces listing in Registry
  
  result = RegOpenKeyEx(HKEY_CLASSES_ROOT,
			"CLSID",
			(DWORD)0,
			KEY_READ,
			&hkey);
  
  
  if (result != ERROR_SUCCESS) {
    scheme_signal_error("Error while searching Windows registry");
  }	    
  
  // enumerate subkeys until we find the one we want
  
  // really, should call RegQueryInfoKey to find size needed for buffers
  
  keyIndex = 0;
  
  while (1) {
    
    // get next subkey
    
    clsIdBufferSize = sizeof(clsIdBuffer);
    
    result = RegEnumKeyEx(hkey,keyIndex++,
			  clsIdBuffer,
			  &clsIdBufferSize,
			  0,NULL,NULL,
			  &fileTime);
    
    if (result == ERROR_NO_MORE_ITEMS) {
      break;
    }		
    
    if (result != ERROR_SUCCESS) {
      scheme_signal_error("Error enumerating subkeys in Windows registry");
    }
    
    if (strlen(clsIdBuffer) != CLSIDLEN) { // not a CLSID -- bogus entry
      continue;
    }
    
    // open subkey
    
    result = RegOpenKeyEx(hkey,clsIdBuffer,
			  (DWORD)0,
			  KEY_READ,&hsubkey);
    
    if (result != ERROR_SUCCESS) {
      return clsId;
    }	    
    
    dataBufferSize = sizeof(dataBuffer);
    
    RegQueryValueEx(hsubkey,"",0,&dataType,dataBuffer,&dataBufferSize);
    
    if (dataType == REG_SZ && strcmp(name,(char *)dataBuffer) == 0) {
      int subkeyIndex;
      TCHAR subkeyBuffer[256];
      DWORD subkeyBufferSize;
      
      // confirm this is a COM object
      
      subkeyIndex = 0;
      
      loopFlag = TRUE;
      
      while (loopFlag) {
	
	subkeyBufferSize = sizeray(subkeyBuffer);
	
	result = RegEnumKeyEx(hsubkey,subkeyIndex++,
			      subkeyBuffer,
			      &subkeyBufferSize,
			      0,NULL,NULL,
			      &fileTime);
	
	if (result == ERROR_NO_MORE_ITEMS) {
	  break;
	}
	
	if (result != ERROR_SUCCESS) {
	  scheme_signal_error("Error enumerating subkeys in Windows registry");
	}
	
	p = objectAttributes;
	
	while (*p) {
	  if (stricmp(subkeyBuffer,*p) == 0) {
	    len = strlen(clsIdBuffer);
	    count = MultiByteToWideChar(CP_ACP,(DWORD)0,
					clsIdBuffer,len,
					oleClsIdBuffer,
					sizeray(oleClsIdBuffer) - 1);
	    oleClsIdBuffer[len] = '\0';  
	    
	    if (count == 0) {
	      scheme_signal_error("Error translating CLSID to Unicode",name);
	    }
	    
	    CLSIDFromString(oleClsIdBuffer,&clsId);
	    loopFlag = FALSE;
	    break; // *p loop
	  }
	  p++;
	}
      }
    }
    
    RegCloseKey(hsubkey);
    
  }
  
  RegCloseKey(hkey);  

  if (isEmptyClsId(clsId)) {
    scheme_signal_error("Coclass %s not found",name);
  }
  
  return clsId;
}

Scheme_Object *mx_find_element(int argc,Scheme_Object **argv) {
  IHTMLElement *pIHTMLElement;
  MX_Element *retval;
  
  if (MX_DOCUMENTP(argv[0]) == FALSE) { 
    scheme_wrong_type("document-find-element","mx-document",0,argc,argv);
  }
  
  if (SCHEME_STRINGP(argv[1]) == FALSE) { 
    scheme_wrong_type("document-find-element","string",1,argc,argv);
  }
  
  if (SCHEME_STRINGP(argv[2]) == FALSE) {
    scheme_wrong_type("document-find-element","string",2,argc,argv);
  }
  
  pIHTMLElement = findBodyElement(MX_DOCUMENT_VAL(argv[0]),
				  SCHEME_STR_VAL(argv[1]),
				  SCHEME_STR_VAL(argv[2]));
  
  if (pIHTMLElement == NULL) {
    scheme_signal_error("HTML element with tag = %s, id = %s not found",
			SCHEME_STR_VAL(argv[1]),SCHEME_STR_VAL(argv[2]));
  }
  
  retval = (MX_Element *)scheme_malloc(sizeof(MX_Element));
  
  retval->type = mx_element_type;
  retval->released = FALSE;
  retval->valid = TRUE;
  retval->pIHTMLElement = pIHTMLElement;

  /* IE seems to give a reference count of 1 for elements */
  /* but releasing them appears to cause an error */
  
  pIHTMLElement->AddRef();  

  mx_register_simple_com_object((Scheme_Object *)retval,pIHTMLElement);
  
  return (Scheme_Object *)retval;
}

Scheme_Object *mx_find_element_by_id_or_name(int argc,Scheme_Object **argv) {
  HRESULT hr;
  IHTMLElement *pIHTMLElement;
  IHTMLElementCollection *pIHTMLElementCollection;
  IHTMLDocument2 *pIHTMLDocument2;
  VARIANT name,index;
  BSTR bstr;
  IDispatch *pEltDispatch;
  MX_Element *retval;
  
  if (MX_DOCUMENTP(argv[0]) == FALSE) { 
    scheme_wrong_type("document-find-element-by-id-or-name","mx-document",0,argc,argv);
  }
  
  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("document-find-element-by-id-or-name","string",1,argc,argv);
  }
  
  pIHTMLDocument2 = MX_DOCUMENT_VAL(argv[0]);

  hr = pIHTMLDocument2->get_all(&pIHTMLElementCollection);
  
  if (hr != S_OK || pIHTMLElementCollection == NULL) {
    scheme_signal_error("find-element-by-id-or-name: "
			"Couldn't retrieve element collection "
			"from HTML document");
  }

  bstr = stringToBSTR(SCHEME_STR_VAL(argv[1]),SCHEME_STRLEN_VAL(argv[1]));

  name.vt = VT_BSTR;
  name.bstrVal = bstr;

  index.vt = VT_I4;
  index.lVal = 0;

  pIHTMLElementCollection->item(name,index,&pEltDispatch);

  SysFreeString(bstr);
  
  pIHTMLElementCollection->Release();

  if (pEltDispatch == NULL) {
    scheme_signal_error("find-element-by-id-or-name: "
			"Couldn't find element with id = %s", 
			SCHEME_STR_VAL(argv[1]));
  }

  hr = pEltDispatch->QueryInterface(IID_IHTMLElement,(void **)&pIHTMLElement);

  if (hr != S_OK || pIHTMLElement == NULL) {
    scheme_signal_error("find-element-by-id-or-name: "
			"Couldn't retrieve element interface "
			"for element with id = %s",SCHEME_STR_VAL(argv[1]));
  }

  retval = (MX_Element *)scheme_malloc(sizeof(MX_Element));
  
  retval->type = mx_element_type;
  retval->released = FALSE;
  retval->valid = TRUE;
  retval->pIHTMLElement = pIHTMLElement;

  /* IE seems to give a reference count of 1 for elements */
  /* but releasing them appears to cause an error */
  
  pIHTMLElement->AddRef();  

  mx_register_simple_com_object((Scheme_Object *)retval,pIHTMLElement);
  
  return (Scheme_Object *)retval;
}

Scheme_Object *mx_coclass_to_html(int argc,Scheme_Object **argv) {
  char *controlName;
  LPOLESTR clsIdString;
  char widthBuff[20];
  char heightBuff[20];
  char buff[512];
  CLSID clsId;
  char *format; 
  int i;

  if (SCHEME_STRINGP(argv[0]) == FALSE) {
    scheme_wrong_type("coclass->html","string",0,argc,argv);
  }
  
  for (i = 1; i <= 2; i++) { 
    if (SCHEME_INTP(argv[i]) == FALSE) {
      scheme_wrong_type("coclass->html","int",i,argc,argv);
    }
  }

  format = "%u";

  if (argc > 3) {
    char *symString;

    if (SCHEME_SYMBOLP(argv[3]) == FALSE) {
      scheme_wrong_type("coclass->html","symbol",3,argc,argv);
    }

    symString = SCHEME_SYM_VAL(argv[3]);

    if (stricmp(symString,"percent") == 0) {
      format = "%u%%";
    }
    else if (stricmp(symString,"pixels")) {
      scheme_signal_error("Invalid size specifier '%s: "
			  "must be either 'pixels or 'percent",symString);
    }
  } 

  controlName = SCHEME_STR_VAL(argv[0]);
  
  sprintf(widthBuff,format,SCHEME_INT_VAL(argv[1]));
  sprintf(heightBuff,format,SCHEME_INT_VAL(argv[2]));

  clsId = getCLSIDFromCoClass(controlName);
  
  if (isEmptyClsId(clsId)) {
    scheme_signal_error("Control not found");  
  }
  
  StringFromCLSID(clsId,&clsIdString);
  
  *(clsIdString + wcslen(clsIdString) - 1) = L'\0'; 
  
  if (clsIdString == NULL) {
    scheme_signal_error("Can't convert control CLSID to string");
  }
  
  sprintf(buff,
	  "<OBJECT ID=\"%s\" WIDTH=\"%s\" HEIGHT=\"%s\" CLASSID=\"clsid:%S\">\n"
	  "</OBJECT>",
	  controlName,		    
	  widthBuff,heightBuff,
	  clsIdString + 1);
  
  return (Scheme_Object *)scheme_make_string(buff);
}

Scheme_Object *mx_stuff_html(int argc,Scheme_Object **argv,
			     WCHAR *oleWhere,char *scheme_name) {
  IHTMLDocument2 *pDocument; 
  IHTMLElement *pBody;
  BSTR where,html;

  if (MX_DOCUMENTP(argv[0]) == FALSE) {
    scheme_wrong_type(scheme_name,"mx-document",0,argc,argv);
  }
  
  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type(scheme_name,"string",1,argc,argv);
  }
  
  pDocument = MX_DOCUMENT_VAL(argv[0]);
  
  html = schemeStringToBSTR(argv[1]);
  pDocument->get_body(&pBody);
  
  if (pBody == NULL) {
    scheme_signal_error("Can't find document BODY");
  }
  
  where = SysAllocString(oleWhere);
  
  pBody->insertAdjacentHTML(where,html);
  
  SysFreeString(where);			    
  SysFreeString(html);			    
  
  return scheme_void;
  
}

Scheme_Object *mx_insert_html(int argc,Scheme_Object **argv) {
  return mx_stuff_html(argc,argv,L"AfterBegin","doc-insert-html");
}

Scheme_Object *mx_append_html(int argc,Scheme_Object **argv) {
  return mx_stuff_html(argc,argv,L"BeforeEnd","doc-append-html");
}

Scheme_Object *mx_replace_html(int argc,Scheme_Object **argv) {
  IHTMLDocument2 *pDocument; 
  IHTMLElement *pBody;
  BSTR html;
  
  if (MX_DOCUMENTP(argv[0]) == FALSE) {
    scheme_wrong_type("replace-html","mx-document",0,argc,argv);
  }
  
  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("replace-html","string",1,argc,argv);
  }
  
  pDocument = MX_DOCUMENT_VAL(argv[0]);
  html = schemeStringToBSTR(argv[1]);
  
  pDocument->get_body(&pBody);
  
  if (pBody == NULL) {
    scheme_signal_error("Can't find document body");
  }
  
  pBody->put_innerHTML(html);
  
  SysFreeString(html);			    
  
  return scheme_void;
}

static BOOL win_event_available(void *) {
  MSG msg;

  return PeekMessage(&msg,NULL,0x400,0x400,PM_NOREMOVE);
}

static void win_event_sem_fun(MX_Document_Object *doc,void *fds) {
  static HANDLE dummySem;

  if (!dummySem) {
    dummySem = CreateSemaphore(NULL,0,1,NULL); 
    if (!dummySem) {
      scheme_signal_error("Error creating Windows event semaphore");
    }
  }

  scheme_add_fd_eventmask(fds,QS_ALLEVENTS);
  scheme_add_fd_handle(dummySem,fds,TRUE); 
}

Scheme_Object *mx_process_win_events(int argc,Scheme_Object **argv) {
  MSG msg;

  scheme_block_until((int (*)(Scheme_Object *))win_event_available,
  		     (void (*)(Scheme_Object *,void *))win_event_sem_fun,
  		     NULL,0.0F);

  while (PeekMessage(&msg,NULL,0x400,0x400,PM_REMOVE)) {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
  }

  return scheme_void;
}

void initMysSinkTable(void) {
  myssink_table.pmake_cy = mx_make_cy;
  myssink_table.pmake_date = mx_make_date;
  myssink_table.pmake_bool = mx_make_bool;
  myssink_table.pmake_scode = mx_make_scode;
  myssink_table.pmake_idispatch = mx_make_idispatch;
  myssink_table.pmake_iunknown = mx_make_iunknown;
  
  myssink_table.pcy_pred = mx_cy_pred;
  myssink_table.pdate_pred = mx_date_pred;
  myssink_table.pscode_pred = mx_scode_pred;
  myssink_table.pcomobj_pred = mx_comobj_pred;
  myssink_table.piunknown_pred = mx_iunknown_pred;
  
  myssink_table.pcy_val = mx_cy_val;
  myssink_table.pdate_val = mx_date_val;
  myssink_table.pscode_val = mx_scode_val;
  myssink_table.pcomobj_val = mx_comobj_val;
  myssink_table.piunknown_val = mx_iunknown_val;
}

Scheme_Object *scheme_initialize(Scheme_Env *env) {
  HRESULT hr;
  int i;

  // globals in mysterx.cxx

  scheme_register_extension_global(&mx_unit,sizeof(mx_unit));
  scheme_register_extension_global(&mx_omit_obj,sizeof(mx_omit_obj));

  mx_com_object_type = scheme_make_type("<com-object>");
  mx_com_type_type = scheme_make_type("<com-type>");
  mx_browser_type = scheme_make_type("<mx-browser>");
  mx_document_type = scheme_make_type("<mx-document>");
  mx_element_type = scheme_make_type("<mx-element>");
  mx_event_type = scheme_make_type("<mx-event>");
  mx_com_cy_type = scheme_make_type("<com-currency>");
  mx_com_date_type = scheme_make_type("<com-date>");
  mx_com_boolean_type = scheme_make_type("<com-bool>");
  mx_com_scode_type = scheme_make_type("<com-scode>");
  mx_com_variant_type = scheme_make_type("<com-variant>");
  mx_com_iunknown_type = scheme_make_type("<com-iunknown>");
  mx_com_pointer_type = scheme_make_type("<com-pointer>");
  mx_com_array_type = scheme_make_type("<com-array>");
  mx_com_omit_type = scheme_make_type("<com-omit>");
  mx_com_typedesc_type = scheme_make_type("<com-typedesc>");

  hr = CoInitialize(NULL);
  
  // S_OK means success, S_FALSE means COM already loaded
  
  if (hr != S_OK && hr != S_FALSE) {
    return scheme_false;
  }		
  
  // export prims + omit value
  
  mx_unit = (Scheme_Unit *)scheme_malloc(sizeof(Scheme_Unit));
  mx_unit->type = scheme_unit_type;
  mx_unit->num_imports = 0;
  mx_unit->num_exports = sizeray(mxPrims) + 1;
  mx_unit->exports = (Scheme_Object **)
    scheme_malloc((sizeray(mxPrims) + 1) * sizeof(Scheme_Object *));
  mx_unit->export_debug_names = NULL;
  mx_unit->init_func = mx_unit_init;
  
  for (i = 0; i < sizeray(mxPrims); i++) {
    mx_unit->exports[i] = scheme_intern_symbol(mxPrims[i].name);
  }
  
  mx_unit->exports[sizeray(mxPrims)] = scheme_intern_symbol("com-omit");

  initEventNames();
  
  initMysSinkTable();

  if (isatty(fileno(stdin))) {
    fputs("MysterX extension for MzScheme, "
	  "Copyright (c) 1999-2000 Rice PLT (Paul Steckler)",stderr);
  }
  
  return (Scheme_Object *)mx_unit;
}

Scheme_Object *scheme_reload(Scheme_Env *env) {
  return (Scheme_Object *)mx_unit;
}

Scheme_Object *mx_com_terminate(int argc,Scheme_Object **argv) {
  CoUninitialize();
  return scheme_void;
}

// for some reason, couldn't put ATL stuff in browser.cxx
// so we leave the Win message loop here

void browserHwndMsgLoop(LPVOID p) {
  HRESULT hr;
  MSG msg;
  HWND hwnd;
  IUnknown *pIUnknown;
  BROWSER_WINDOW_INIT *pBrowserWindowInit;
  LONG hasScrollBars;
  
  pBrowserWindowInit = (BROWSER_WINDOW_INIT *)p;

  // set apparently-unused low bit in style to inform
  // DHTMLPage object that we want scrollbars 
  if (pBrowserWindowInit->browserWindow.style & (WS_HSCROLL|WS_VSCROLL)) {
    hasScrollBars = 1L;
  }
  else {
    hasScrollBars = 0L;
  }

  hwnd = CreateWindow("AtlAxWin","myspage.DHTMLPage.1",
		      WS_VISIBLE | hasScrollBars | 
		      (pBrowserWindowInit->browserWindow.style & ~(WS_HSCROLL|WS_VSCROLL)),
		      pBrowserWindowInit->browserWindow.x,pBrowserWindowInit->browserWindow.y,
		      pBrowserWindowInit->browserWindow.width,pBrowserWindowInit->browserWindow.height,
		      NULL,NULL,hInstance,NULL);
  
  if (hwnd == NULL) {
    scheme_signal_error("make-browser: Can't create browser window");
  }

  browserHwnd = hwnd;
  
  if (hasScrollBars) {
    // clear spurious low bit to avoid trouble
    SetWindowLong(hwnd,GWL_STYLE,
		  GetWindowLong(hwnd,GWL_STYLE) & ~1L);
  } 

  SetClassLong(hwnd,GCL_HICON,(LONG)hIcon);
  
  SetWindowText(hwnd,pBrowserWindowInit->browserWindow.label);

  ShowWindow(hwnd,SW_SHOW);
  SetForegroundWindow(hwnd);

  pIUnknown = NULL;
  
  while (IsWindow(hwnd)) {
    
    if (pIUnknown == NULL) {
      AtlAxGetControl(hwnd,&pIUnknown);
      if (pIUnknown) {
	
	hr = CoMarshalInterThreadInterfaceInStream(IID_IUnknown,pIUnknown,
						   pBrowserWindowInit->ppIStream);
	
	if (hr != S_OK) {
	  DestroyWindow(hwnd);
	  ReleaseSemaphore(createHwndSem,1,NULL);
	  codedComError("Can't marshal document interface",hr);
	}
	
	ReleaseSemaphore(createHwndSem,1,NULL);
      }
    }

    while (GetMessage(&msg,NULL,0,0)) {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
  }
}

BOOL APIENTRY DllMain(HANDLE hModule,DWORD reason,LPVOID lpReserved) {
  
  if (reason == DLL_PROCESS_ATTACH) {
    
    hInstance = (HINSTANCE)hModule;
    
    browserHwndMutex = CreateSemaphore(NULL,1,1,NULL);
    createHwndSem = CreateSemaphore(NULL,0,1,NULL);
    eventSinkMutex = CreateSemaphore(NULL,1,1,NULL);
    
    hIcon = (HICON)LoadImage(hInstance,
			     MAKEINTRESOURCE(MYSTERX_ICON),
			     IMAGE_ICON,0,0,0);
    
    _Module.Init(NULL,hInstance);
    AtlAxWinInit();
    
  }
  else if (reason == DLL_PROCESS_DETACH) {
    _Module.Term();
  }
  
  return TRUE;
}
