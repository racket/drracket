/*
 * File:	wx_form.h
 * Purpose:	
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

/*
 * Purpose:  Declaration of the wxForm object and related functions, for
 *           creating forms with constrained user-editable items.
 */


#ifndef wxb_formh
#define wxb_formh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_setup.h"

#if USE_FORM

#include "wx_item.h"

// Data types
typedef enum { 
 wxFORM_SHORT = 1,
 wxFORM_LONG,
 wxFORM_STRING,
 wxFORM_BOOL,
 wxFORM_FLOAT,
 wxFORM_LIST_OF_LONGS,
 wxFORM_LIST_OF_STRINGS
} form_data_types_t;

// Editor types
typedef enum {
 wxFORM_DEFAULT = 1, // wxFORM_DEFAULT: use whatever is most suitable
 wxFORM_SINGLE_LIST,
 wxFORM_MULTI_LIST,
 wxFORM_CHOICE,
 wxFORM_CHECKBOX,
 wxFORM_TEXT,
 wxFORM_MULTITEXT,
 wxFORM_SLIDER,
 wxFORM_MESSAGE,
 wxFORM_NEWLINE,
 wxFORM_BUTTON,
 wxFORM_DUMB_MESSAGE,
 wxFORM_RADIOBOX
} form_editor_types_t;

// Editable or not editable
typedef enum {
 wxFORM_NOT_EDITABLE,
 wxFORM_EDITABLE
} form_edit_t;

// Constraint types
typedef enum {
 wxFORM_CONSTRAINT_ONE_OF  =  1,
 wxFORM_CONSTRAINT_RANGE,
 wxFORM_CONSTRAINT_FUNCTION,
 wxFORM_CONSTRAINT_IS_INT,
 wxFORM_CONSTRAINT_IS_STRING,
 wxFORM_CONSTRAINT_IS_REAL,
 wxFORM_CONSTRAINT_IS_BOOL
} form_constraint_types_t;

// Form buttons
#define wxFORM_BUTTON_OK          1
#define wxFORM_BUTTON_CANCEL      2
#define wxFORM_BUTTON_UPDATE      4
#define wxFORM_BUTTON_REVERT      8
#define wxFORM_BUTTON_HELP        16
#define wxFORM_BUTTON_ALL (wxFORM_BUTTON_OK|wxFORM_BUTTON_CANCEL|wxFORM_BUTTON_UPDATE|wxFORM_BUTTON_REVERT)

// Form buttons placement
#define wxFORM_BUTTON_AT_TOP      1
#define wxFORM_BUTTON_AT_BOTTOM   2

// Returns FALSE if constraint violated, print error to msg_buffer.
// Cast value to POINTER to appropriate value.
typedef Bool (*wxConstraintFunction) (int type, char *value, char *label, char *msg_buffer);

// Edits a value with a user-supplied editor, placing result in
// variable pointed to by value.
typedef Bool (*wxEditFunction) (int type, char *current_value, char *variable);

#ifdef IN_CPROTO
typedef       void    *wxRealRange ;
typedef       void    *wxFormItemConstraint;
typedef       void    *wxFormItem;
typedef       void    *wxForm;
#else

class wxRealRange: public wxObject
{
 public:
  float lo;
  float hi;
  wxRealRange(float lo, float hi);
};

class wxFormItemConstraint: public wxObject
{
 public:
  int Type;
  Bool localList ; // For remembering to delete (or not) OneOf.
  union
  {
    wxList *OneOf; // List of strings or longs
    wxConstraintFunction ConstraintFunc;
    wxRealRange *Range;
  } Constraint;

  wxFormItemConstraint(int type);
  ~wxFormItemConstraint(void);
};

// A form item contains information about the C++ variable
// containing the data, the type, the constraints, error messages, help
// information.
class wxForm;
class wxFormItem: public wxObject
{
 public:
  long Id;
  int Type;
  int ItemType;
  int Width;
  int Height;
  int style;
  wxForm *Form;
  wxItem *PanelItem;
  wxList *Constraints;
//  wxEditFunction CustomEditor;
  char *HelpString;
  char *Label;
  wxFunction ButtonFunc;

  // Temporary value before user clicks Ok on form
  union
  {
    float FloatValue;
    long LongIntValue;
    int ShortIntValue;
    char *StringValue;
    Bool BoolValue;
    wxList *ListValue;
    wxFunction ButtonFunction;
  } Value;

  // Pointer to (for example) some C++ object member
  union
  {
    float *FloatValuePtr;
    long *LongIntValuePtr;
    int *ShortIntValuePtr;
    char **StringValuePtr;
    Bool *BoolValuePtr;
    wxList **ListValuePtr;
  } ValuePtr;

   wxFormItem(int type, int item_type);
   ~wxFormItem(void);

   void MakePanelItem(wxPanel *panel);

   // Checking functions
   Bool CheckLongValue(long val);
   Bool CheckBoolValue(Bool val);
   Bool CheckStringValue(char *val);
   Bool CheckFloatValue(float val);

   // Set C++ variable to currently edited value(s) if doesn't
   // violate any constraint. Returns TRUE if successful.
   Bool UpdateValue(void);

   // Display current C++ variable value
   void RevertValue(void);

   // Get panel item (only available AFTER association with
   // panel/dialog)
   inline wxItem *GetPanelItem(void) { return PanelItem; }
};

// A form is independent from an actual panel or dialog,
// but can be associated with one
class wxForm: public wxObject
{
  int buttonUse;
  int buttonPlace;
 public:
  wxList FormItems;
  wxPanel *wx_form_panel;
  Bool wx_editable;
  wxForm(int button_use = wxFORM_BUTTON_ALL, int button_place = wxFORM_BUTTON_AT_TOP);
  ~wxForm(void);
  void Add(wxFormItem *item, long id = -1);
  void MakeAndPlaceButtons(wxPanel *panel);
  wxNode *FindItem(long id);
  Bool Set(long id, wxFormItem *item);
  Bool Delete(long id);
  void AssociatePanel(wxPanel *panel);
  void DisassociatePanel(void);
  Bool UpdateValues(void);
  void RevertValues(void);
  inline void SetEditable(Bool editable)
    { wx_editable = editable; }
  inline Bool IsEditable(void)
    { return wx_editable; }

  // Default behaviour for OnOk/OnCancel - delete form and panel/dialog box
  virtual void OnOk(void);
  virtual void OnCancel(void);
  virtual void OnRevert(void);
  virtual void OnUpdate(void);
  virtual void OnHelp(void);
};

// Functions for making wxFormItems
wxFormItem *wxMakeFormButton(char *label, wxFunction fun);

wxFormItem *wxMakeFormMessage(char *label);

wxFormItem *wxMakeFormNewLine(void);

wxFormItem *wxMakeFormLong(char *label, long *var,
  int item_type = wxFORM_DEFAULT, wxList *constraints = NULL,
  char *help_string = NULL, int style = 0,
  int width = -1, int height = -1);

wxFormItem *wxMakeFormShort(char *label, int *var,
  int item_type = wxFORM_DEFAULT, wxList *constraints = NULL,
  char *help_string = NULL, int style = 0,
  int width = -1, int height = -1);

wxFormItem *wxMakeFormFloat(char *label, float *var,
  int item_type = wxFORM_DEFAULT, wxList *constraints = NULL,
  char *help_string = NULL, int style = 0,
  int width = -1, int height = -1);

wxFormItem *wxMakeFormBool(char *label, Bool *var,
  int item_type = wxFORM_DEFAULT, wxList *constraints = NULL,
  char *help_string = NULL, int style = 0,
  int width = -1, int height = -1);

wxFormItem *wxMakeFormString(char *label, char **var,
  int item_type = wxFORM_DEFAULT, wxList *constraints = NULL,
  char *help_string = NULL, int style = 0,
  int width = -1, int height = -1);

/* NOT IMPLEMENTED
// List of strings
wxFormItem *wxMakeFormStringList(char *label, wxList **var,
  int item_type = wxFORM_DEFAULT, wxList *constraints = NULL,
  char *help_string = NULL, int style = 0,
  int width = -1, int height = -1);

// List of longs
wxFormItem *wxMakeFormLongList(char *label, wxList **var,
  int item_type = wxFORM_DEFAULT, wxList *constraints = NULL,
  char *help_string = NULL, int style = 0,
  int width = -1, int height = -1);
*/

// Functions for making wxFormItemConstraints
wxFormItemConstraint *wxMakeConstraintStrings(wxList *list);
wxFormItemConstraint *wxMakeConstraintStrings(char *first ...);
// wxFormItemConstraint *wxMakeConstraintLongs(wxList *list);
wxFormItemConstraint *wxMakeConstraintFunction(wxConstraintFunction func);
wxFormItemConstraint *wxMakeConstraintRange(float lo, float hi);

#endif // IN_CPROTO
#endif // USE_FORM
#endif // wxb_formh
