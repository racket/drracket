
@INCLUDE prefix.xci

#include "wx_media.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxSnip "wx:snip":"wx:object"

@CREATOR ();

@CLASSID wxTYPE_SNIP

@CONSTANT "wx:const-snip-can-append" : int wxSNIP_CAN_APPEND
@CONSTANT "wx:const-snip-newline" : int wxSNIP_NEWLINE
@CONSTANT "wx:const-snip-hard-newline" : int wxSNIP_HARD_NEWLINE
@CONSTANT "wx:const-snip-is-text" : int wxSNIP_IS_TEXT
@CONSTANT "wx:const-snip-invisible" : int wxSNIP_INVISIBLE
@CONSTANT "wx:const-snip-handles-events" : int wxSNIP_HANDLES_EVENTS
@CONSTANT "wx:const-snip-width-depends-on-x" : int wxSNIP_WIDTH_DEPENDS_ON_X
@CONSTANT "wx:const-snip-height-depends-on-x" : int wxSNIP_HEIGHT_DEPENDS_ON_X
@CONSTANT "wx:const-snip-width-depends-on-y" : int wxSNIP_WIDTH_DEPENDS_ON_Y
@CONSTANT "wx:const-snip-height-depends-on-y" : int wxSNIP_HEIGHT_DEPENDS_ON_Y
@CONSTANT "wx:const-snip-anchored" : int wxSNIP_ANCHORED
@CONSTANT "wx:const-snip-uses-buffer-path" : int wxSNIP_USES_BUFFER_PATH

@IVAR r "count" : long count
@IVAR r "flags" : long flags
@IVAR r "style" : wxStyle! style
@IVAR "snipclass" : wxSnipClass^ snipclass

@ "get-admin" : wxSnipAdmin! GetAdmin();

@ "set-count" : void SetCount(long);
@ "set-flags" : void SetFlags(long);

@ "set-style" : void SetStyle(wxStyle!)

@ "is-owned?" : bool IsOwned();
@ "release-from-owner" : bool ReleaseFromOwner();

@SETMARK s = v
@INCLUDE wxs_snip.xci

@ "next" : wxSnip^ Next();
@ "previous" : wxSnip^ Previous();

@END


@CLASSBASE wxTextSnip "wx:text-snip":"wx:snip"

@CREATOR (long=0);

@CLASSID wxTYPE_TEXT_SNIP

@SETMARK s = d
@INCLUDE wxs_snip.xci

@ "insert" : void Insert(string,long,long=0);
@ "read" : void Read(long,wxMediaStreamIn%);

@END


@CLASSBASE wxTabSnip "wx:tab-snip":"wx:text-snip"

@CREATOR ();

@CLASSID wxTYPE_TAB_SNIP

@SETMARK s = d
@INCLUDE wxs_snip.xci

@END


@CLASSBASE wxImageSnip "wx:image-snip":"wx:snip"

@CREATOR (nstring=NULL,long=0,bool=FALSE,bool=TRUE);

@CLASSID wxTYPE_IMAGE_SNIP

@SETMARK s = d
@INCLUDE wxs_snip.xci

@ "load-file" : void LoadFile(nstring,long,bool=FALSE,bool=TRUE);

@ "get-filename" : nstring GetFilename(bool?);
@ "get-filetype" : long GetFiletype();

@ "set-bitmap" : void SetBitmap(wxBitmap!);

@ "set-offset" : void SetOffset(float, float);

@END


#define Get_This_Media GetThisMedia

@CLASSBASE wxMediaSnip "wx:media-snip" : "wx:snip"

@CREATOR (wxMediaBuffer^=NULL,bool=TRUE,int=wxMSNIPBOX_XMARGIN,int=wxMSNIPBOX_YMARGIN,int=wxMSNIPBOX_XMARGIN,int=wxMSNIPBOX_YMARGIN,int=wxMSNIPBOX_XINSET,int=wxMSNIPBOX_YINSET,int=wxMSNIPBOX_XINSET,int=wxMSNIPBOX_YINSET,int=-1,int=-1,int=-1,int=-1);

@CLASSID wxTYPE_MEDIA_SNIP

@ "get-this-media" : wxMediaBuffer^ Get_This_Media();
@ "get-media" : wxMediaBuffer^ GetThisMedia();
@ "set-media" : void SetMedia(wxMediaBuffer^);

@CONSTANT "wx:const-msnipbox-xmargin" : int wxMSNIPBOX_XMARGIN
@CONSTANT "wx:const-msnipbox-ymargin" : int wxMSNIPBOX_YMARGIN
@CONSTANT "wx:const-msnipbox-xinset" : int wxMSNIPBOX_XINSET
@CONSTANT "wx:const-msnipbox-yinset" : int wxMSNIPBOX_YINSET

@SETMARK s = d
@INCLUDE wxs_snip.xci

@ "set-max-width" : void SetMaxWidth(float);
@ "set-max-height" : void SetMaxHeight(float);
@ "get-max-width" : float GetMaxWidth();
@ "get-max-height" : float GetMaxHeight();
@ "set-min-width" : void SetMinWidth(float);
@ "set-min-height" : void SetMinHeight(float);
@ "get-min-width" : float GetMinWidth();
@ "get-min-height" : float GetMinHeight();

@ "show-border" : void ShowBorder(bool);
@ "border-visible?" : bool BorderVisible();

@ "set-margin" : void SetMargin(int,int,int,int);
@ "get-margin" :void GetMargin(int*,int*,int*,int*);
@ "set-inset" :void SetInset(int,int,int,int);
@ "get-inset" :void GetInset(int*,int*,int*,int*);

@END

@MACRO rZERO = return 0;
@MACRO rNULL = return NULL;

@CLASSBASE wxBufferDataClass "wx:buffer-data-class" : "wx:object"

@CREATOR ()

@CLASSID wxTYPE_BUFFER_DATA_CLASS

@IVAR "classname" : string classname
@IVAR "required" : bool required

@ V "read" : wxBufferData^ Read(wxMediaStreamIn%); : : : : rNULL

@END

@CLASSBASE wxBufferDataClassList "wx:buffer-data-class-list" : "wx:object"

@CREATOR ()

@CLASSID wxTYPE_BUFFER_DATA_CLASS_LIST

@ "find" : wxBufferDataClass^ Find(string);
@ "find-position" : short FindPosition(wxBufferDataClass!);
@ "add" : void Add(wxBufferDataClass!);
@ "number" : int Number();
@ "nth" : wxBufferDataClass^ Nth(int);

@END

@CLASSBASE wxBufferData "wx:buffer-data" : "wx:object"

@CREATOR ()

@CLASSID wxTYPE_BUFFER_DATA

@IVAR "dataclass" : wxBufferDataClass^ dataclass
@IVAR "next" : wxBufferData^ next

@ V "write" : bool Write(wxMediaStreamOut%); : : : : rZERO

@END

