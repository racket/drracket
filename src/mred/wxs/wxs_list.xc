
#include "wx_list.h"
#include "wx_hash.h"
#include "wx_utils.h"

#if 0

@INCLUDE wxs.xci

@HEADER

#if defined(wx_mac)
#define NODE_FIX os_wxNode() : wxNode(NULL, NULL, NULL, NULL) {}
#else
#define NODE_FIX 
#endif

#if 1
#define NODE_DATA_GC_IGNORED(r) r->DataGCIgnored()
#else
#define NODE_DATA_GC_IGNORED(r) WXGC_IGNORED(r->data)
#endif

@MACRO ThisNodeDataIgnored = NODE_DATA_GC_IGNORED(((wxNode *)((Scheme_Class_Object *)obj)->primdata));


@CLASSBASE wxNode "wx:node":"wx:object"

@VAR NODE_FIX

@ "data" : wxObject! Data();
@ "next" : wxNode^ Next();
@ "previous" : wxNode^ Previous();
@ "set-data" : void SetData(wxObject!); : : ///ThisNodeDataIgnored

@END

#ifdef wx_mac
#define _KEY_TYPE KeyType
#else
#define _KEY_TYPE unsigned
#endif
@MACRO bInt = objscheme_bundle_integer((int){x})
@MACRO ubIntKey = (_KEY_TYPE)objscheme_unbundle_integer({x}, "wxList")
@MACRO tInt = objscheme_istype_number({x}, NULL)

@MACRO CHECKVALIDKEYTYPE = if ((x0 != wxKEY_NONE) && (x0 != wxKEY_INTEGER) && (x0 != wxKEY_STRING)) scheme_wrong_type("make-wx:list%", "wx:const-key-none, wx:const-key-integer, or wx:const-key-string", 0, 1, p);

@MACRO CHECKVALIDHKEYTYPE = if ((x0 != wxKEY_INTEGER) && (x0 != wxKEY_STRING)) scheme_wrong_type("make-wx:hash-table", "wx:const-key-integer or wx:const-key-string", 0, 2, p);

@MACRO CHECKKEYTYPE[cl.t.k.w.s] = if (((<cl> *)((Scheme_Class_Object *)obj)->primdata)->key_type != <t>) scheme_signal_error("wx:" <k> "%%::" <w> ": list does not have " <s> " keys");

@MACRO NodeDataGCIgnored = NODE_DATA_GC_IGNORED(r);


@CLASSBASE wxList "wx:list" : "wx:object"

@CREATOR ()
@CREATOR (_KEY_TYPE/bInt/ubIntKey/tInt) : : /CHECKVALIDKEYTYPE
// @CREATOR (int, wxObject*[]);

@ "append" : wxNode! Append(wxObject!); : : CHECKKEYTYPE[wxList.wxKEY_NONE."list"."append"."empty"]///NodeDataGCIgnored
@ "append" : wxNode! Append(long,wxObject!); : : CHECKKEYTYPE[wxList.wxKEY_INTEGER."list"."append"."integer"]///NodeDataGCIgnored
@ "append" : wxNode! Append(string,wxObject!); : : CHECKKEYTYPE[wxList.wxKEY_STRING."list"."append"."string"]///NodeDataGCIgnored

@ "clear" : void Clear();
// UNSAFE -- @ "set-delete-contents" : void DeleteContents(bool);
@ "delete-node" : bool DeleteNode(wxNode!);
@ "delete-object" : bool DeleteObject(wxObject!);
@ "find" : wxNode^ Find(long); : : CHECKKEYTYPE[wxList.wxKEY_INTEGER."list"."find"."integer"]
@ "find" : wxNode^ Find(string); : : CHECKKEYTYPE[wxList.wxKEY_STRING."list"."find"."string"]
@ "first" : wxNode^ First();
@ "insert" : wxNode! Insert(wxNode!, wxObject!); : : CHECKKEYTYPE[wxList.wxKEY_NONE."list"."insert"."empty"]///NodeDataGCIgnored
@ "insert" : wxNode! Insert(wxObject!); : : CHECKKEYTYPE[wxList.wxKEY_NONE."list"."insert"."empty"]///NodeDataGCIgnored
@ "last" : wxNode^ Last();
@ "member" : wxNode^ Member(wxObject!);
@ "nth" : wxNode^ Nth(int);
@ "number" : int Number();

@END


@CLASSBASE wxHashTable "wx:hash-table" : "wx:list"

@CREATOR (int,int=1000) : : /CHECKVALIDHKEYTYPE|NOZERO[1]

@ "begin-find" : void BeginFind();
@ "clear" : void Clear();
@ "delete" : void Delete(long); : : CHECKKEYTYPE[wxHashTable.wxKEY_INTEGER."hash-table"."delete"."integer"]
@ "delete" : void Delete(string); : : CHECKKEYTYPE[wxHashTable.wxKEY_STRING."hash-table"."delete"."string"]
@ "get" : wxObject^ Get(long); : : CHECKKEYTYPE[wxHashTable.wxKEY_INTEGER."hash-table"."get"."integer"]
@ "get" : wxObject^ Get(string); : : CHECKKEYTYPE[wxHashTable.wxKEY_STRING."hash-table"."get"."string"]
@ "make-key" : long MakeKey(string);
@ "next" : wxNode^ Next();
@ "put" : void Put(long, wxObject!); : : CHECKKEYTYPE[wxHashTable.wxKEY_INTEGER."hash-table"."put"."integer"]
@ "put" : void Put(string, wxObject!); : : CHECKKEYTYPE[wxHashTable.wxKEY_STRING."hash-table"."put"."string"]

@CONSTANT "wx:const-key-integer" : int wxKEY_INTEGER
@CONSTANT "wx:const-key-string" : int wxKEY_STRING

@END


@CLASSBASE wxPathList "wx:path-list" : "wx:list"

@CREATOR ();

@ "add-env-list" : void AddEnvList(string);
@ "add" : void Add(string)
@ "find-valid-path" : string FindValidPath(string);
@ "member?" : bool Member(string);

@END


@CLASSBASE wxStringList "wx:string-list" : "wx:list"

@CREATOR ();

@ "add" : void Add(string);
@ "delete" : void Delete(string);
@ "sort" : void Sort();
@ "member?" : bool Member(string);

@END


#endif