
@TEST

@SETMARK X = 

@CLASSBASE x "x":"y"
@SETMARK Q = H
@SETMARK q = v

@MACRO NULL = NULL
@ v "wx:get-os-version" : int wxGetOsVersion(int?,int?); : : : NULL
@ v "wx:get-resource" : bool wxGetResource(string,string,string*,string=NULL);

@ V "scroll" : void Scroll(int,int,bool);
@ V "scroll" : void Scroll(int,int);
@ Q "append" : void Append(int,string,string=NULL);
@ Q "append" : void Append(int,string,wxMenu!,string=NULL);

@MACRO blah[sam.i.am] = blah({x} + <sam> + <i> + <am>)

@SET temp = 1
@IFDEFINE HM = temp : A : B

// @ V "test" : int& Test(int*,int/blah[a.b.c], -int,int*);

@IVAR "i" : float i

@CONSTANT "one" : int 1

@END
