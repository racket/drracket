
@INCLUDE prefix.xci

#include "wx_menu.h"

@HEADER

@INCLUDE wxs.xci

#ifdef wx_msw
# define XTMAC_UNUSED(x) x
#else
# define XTMAC_UNUSED(x) /**/
#endif

static void menuSelect(wxMenu *XTMAC_UNUSED(m))
{
#ifdef wx_msw
  m->SelectMenu();
#endif
}

// @CLASSBASE wxMenuItem "menu-item" : "object"
// @END

// wxMenu is really derived from wxItem
//  but that makes no sense. Enforce different hierarchy here
@CLASSBASE wxMenu "menu" : "object"

@SET CALLBACK_CLASS = wxMenu
@SET CALLBACK_CLASS_USER = METHODNAME("menu%","initialization")
@INCLUDE cb_start.xci

@MACRO CHECKNEG[pos.result] = if (x<pos> < 0) return <result>;
@MACRO CHECKNEGVOID[pos] = $$CHECKNEG[<pos>.scheme_void]
@MACRO CHECKNEGNULL[pos] = $$CHECKNEG[<pos>.XC_SCHEME_NULL]
@MACRO CHECKNEGFALSE[pos] = $$CHECKNEG[<pos>.scheme_false]

@CREATOR (nstring=NULL,wxFunction=NULL/bCallback/ubCallback/cCallback//spCallback/nopush); : : ubCallbackSetup///ubCallbackCreatorFinish

@ "append" : void Append(ExactLong,string,wxMenu!,nstring=NULL); : : <> submenu
@ "append" : void Append(ExactLong,string,nstring=NULL,bool=FALSE); : : <> string item
@ "delete" : bool Delete(ExactLong); : :
@ "delete-by-position" : bool DeleteByPosition(int); : :
@ "append-separator" : void AppendSeparator();
@ "checked?" : bool Checked(ExactLong); : :
@ "check" : void Check(ExactLong,bool); : :
@ "enable" : void Enable(ExactLong,bool); : :
@ "number" : int Number()

@ "set-help-string" : void SetHelpString(ExactLong, nstring); : :
@ "set-label" : void SetLabel(ExactLong, string); : :
@ "set-title" : void SetTitle(string);

@ m "select" : void menuSelect();

@END

@INCLUDE cb_end.xci

// wxMenuBar is really derived from wxItem
//  but that makes no sense. Enforce different hierarchy here
@CLASSBASE wxMenuBar "menu-bar" : "object"

@SET TYPE = string
@SET NOTEST = 1
@INCLUDE list.xci

@SET TYPE = wxMenu
@SET POINTERS = 1
@INCLUDE list.xci

@MACRO CHECKSAMELENGTH = if (scheme_proper_list_length(p[0]) != scheme_proper_list_length(p[1])) scheme_arg_mismatch(METHODNAME("menu-bar%","initialization"), "list size mismatch: ", p[0]);

@MACRO spMenuList = (listof wxMenu-object)

@CREATOR (); <> no argument
@CREATOR (-int, wxMenu*[]/bList/ubList/cList//spMenuList/push, string[]/bList/ubList/cList///push); : : CHECKSAMELENGTH/glueListSet[wxMenu.0.1.0.METHODNAME("menu-bar%","initialization")] | glueListSet[string.1.2.0.METHODNAME("menu-bar%","initialization")]// <> menu% list

@ "append" : void Append(wxMenu!,string);
@ "delete" : bool Delete(wxMenu^,int=0);
@ "enable-top" : void EnableTop(int,bool); : : /CHECKNEGVOID[0]
@ "number" : int Number()

@ "set-label-top" : void SetLabelTop(int, string); : : /CHECKNEGVOID[0]

@END


class wxsMenuItem : public wxObject
{
public:
  wxsMenuItem(void) {
  }

  ExactLong Id(void) {
    return (ExactLong)this;
  }
};

wxsMenuItem* wxsIdToMenuItem(ExactLong id)
{
  return (wxsMenuItem *)id;
}

@CLASSBASE wxsMenuItem "menu-item" : "object"

@CREATOR ()

@ "id" : ExactLong Id();

@END


@GLOBAL wxsMenuItemGlobal

@ "id-to-menu-item" : wxsMenuItem! wxsIdToMenuItem(ExactLong);

@END
