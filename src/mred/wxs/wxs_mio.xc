
@INCLUDE prefix.xci

#include "wx_obj.h"
#include "wx_medio.h"

@INCLUDE wxs.xci

@HEADER

@MACRO rZERO = return 0;

@SET TYPE = char
@SET NOTEST = 1
@SET SIZEISLONG = 1
@INCLUDE list.xci

@CLASSBASE wxMediaStreamInBase "wx:media-stream-in-base" : "wx:object"

static char *VectorToArray(char *r, Scheme_Object *vec, long *len)
{
  long c, i;
  Scheme_Object **a;

  if (!SCHEME_VECTORP(vec))
    scheme_wrong_type("wx:media-stream-in-base::read", 
		      "character vector", -1, 0, &vec);

  c = *len = SCHEME_VEC_SIZE(vec);

  if (!r)
    r = (char *)scheme_malloc_atomic(c);

  for (a = SCHEME_VEC_ELS(vec), i = 0; i < c; i++) {
    if (!SCHEME_CHARP(a[i]))
      scheme_wrong_type("wx:media-stream-in-base::read", 
			"character vector", -1, 0, &vec);
    r[i] = SCHEME_CHAR_VAL(a[i]);
  }

  return r;
}

static Scheme_Object *ArrayToVector(char *r, Scheme_Object *vec, long len)
{
  long i;
  Scheme_Object **a;

  if (!vec)
    vec = scheme_make_vector(len, scheme_make_char(0));
  else if (!SCHEME_VECTORP(vec))
    scheme_wrong_type("wx:media-stream-in-base::read", 
		      "character vector", -1, 0, &vec);
  
  for (a = SCHEME_VEC_ELS(vec), i = 0; i < len; i++)
    a[i] = scheme_make_char(r[i]);

  return vec;
}

@MACRO setNULL = NULL
@MACRO arrayToVector = p[0] = ArrayToVector(x0, NULL, x1);
@MACRO copyArrayToVector = ArrayToVector(x0, p[0], x1);
@MACRO vectorToArray = x0 = VectorToArray(NULL, p[0], &x1);
@MACRO copyVectorToArray = VectorToArray(x0, p[0], &x1);

@CREATOR ();

@ V "tell" : long Tell(); : : : rZERO
@ V "seek" : void Seek(long);
@ V "skip" : void Skip(long);
@ V "bad?" : bool Bad(); : : : rZERO
@ V "read" : long Read(char[]/setNULL/setNULL,-long); : /arrayToVector/copyVectorToArray : /vectorToArray/copyArrayToVector : rZERO

@END

@CLASSBASE wxMediaStreamOutBase "wx:media-stream-out-base" : "wx:object"

@CREATOR ();

@ V "tell" : long Tell(); : : : rZERO
@ V "seek" : void Seek(long);
@ V "bad?" : bool Bad(); : : : rZERO
@ V "write" : void Write(char[]/bList/ubList/cList,-long); : /methListSet[char.0.0.1] : /glueListSet[char.0.0.1."wx:media-stream-out-base%::write"]

@END


@CLASSBASE wxMediaStreamInStringBase "wx:media-stream-in-string-base" : "wx:media-stream-in-base"

@MACRO setStringSize[ss.cn] = x<cn> = SCHEME_STRTAG_VAL(p[<ss>]);

@CREATOR (string,-long); : : /setStringSize[0.1]

@END

@CLASSBASE wxMediaStreamOutStringBase "wx:media-stream-out-string-base" : "wx:media-stream-out-base"

@CREATOR ()

@MACRO makeSizedString = (r ? scheme_make_sized_string(r, _x0 - 1, 0) : scheme_null)

@ "get-string" : nstring/makeSizedString GetString(-long*);

@END

static long GetExact(wxMediaStreamIn *s)
{
  long l;
  s->Get(l);
  return l;
}
static double GetInexact(wxMediaStreamIn *s)
{
  double d;
  s->Get(d);
  return d;
}

@CLASSBASE wxMediaStreamIn "wx:media-stream-in" : "wx:object"

@CREATOR (wxMediaStreamInBase%);
  
// This is too dangerous: it is going away
// @ "get" : wxMediaStreamIn% Get(long*////long,string);

@ "get" : wxMediaStreamIn% Get(Long+////long); <> exact number
@ "get" : wxMediaStreamIn% Get(Double+); <> inexact number
// @ "get" : wxMediaStreamIn% Get(short+);
// @ "get" : wxMediaStreamIn% Get(int+);
// @ "get" : wxMediaStreamIn% Get(char+);
// @ "get" : wxMediaStreamIn% Get(float+);

@MACRO alwaysPassPtr = x0 = &_x0;

@ "get-string" : nstring/makeSizedString GetString(long?=NULL); : : /alwaysPassPtr/
@ "get-fixed" : wxMediaStreamIn% GetFixed(long+);

@ m "get-exact" : long GetExact();
@ m "get-inexact" : double GetInexact();

@ ">>" : wxMediaStreamIn% operator>>(Long+); <> exact
@ ">>" : wxMediaStreamIn% operator>>(Double+); <> inexact
// @ ">>" : wxMediaStreamIn% operator>>(short+);
// @ ">>" : wxMediaStreamIn% operator>>(int+);
// @ ">>" : wxMediaStreamIn% operator>>(char+);
// @ ">>" : wxMediaStreamIn% operator>>(float+);

@ "set-boundary" : void SetBoundary(long);
@ "remove-boundary" : void RemoveBoundary();

@ "skip" : void Skip(long);
@ "tell" : long Tell();
@ "jump-to" : void JumpTo(long);

@ "ok?" : bool Ok();

@END


@CLASSBASE wxMediaStreamOut "wx:media-stream-out" : "wx:object"

@CREATOR (wxMediaStreamOutBase%);

@ "put" : wxMediaStreamOut% Put(long////long,string); <> length and string
@ "put" : wxMediaStreamOut% Put(string); <> string without length
@ "put" : wxMediaStreamOut% Put(Long////long); <> exact number
@ "put" : wxMediaStreamOut% Put(Double); <> inexact number
// @ "put" : wxMediaStreamOut% Put(short);
// @ "put" : wxMediaStreamOut% Put(int);
// @ "put" : wxMediaStreamOut% Put(char);
// @ "put" : wxMediaStreamOut% Put(float);

@ "put-fixed" : wxMediaStreamOut% PutFixed(long);

@ "<<" : wxMediaStreamOut% operator<<(string); <> string
@ "<<" : wxMediaStreamOut% operator<<(Double); <> inexact number
@ "<<" : wxMediaStreamOut% operator<<(Long); <> exact number
// @ "<<" : wxMediaStreamOut% operator<<(short);
// @ "<<" : wxMediaStreamOut% operator<<(int);
// @ "<<" : wxMediaStreamOut% operator<<(byte);
// @ "<<" : wxMediaStreamOut% operator<<(float);

@ "tell" : long Tell();
@ "jump-to" : void JumpTo(long);

@ "ok?" : bool Ok();

@END

