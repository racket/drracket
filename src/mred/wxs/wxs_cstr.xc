
#if 0

#include "wx_win.h"

#if USE_CONSTRAINTS

#include "wx_lay.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxLayoutConstraints "wx:layout-constraints":"wx:object"

@CREATOR ()

@IVAR "bottom" : wxIndividualLayoutConstraint% bottom
@IVAR "top" : wxIndividualLayoutConstraint% top
@IVAR "right" : wxIndividualLayoutConstraint% right
@IVAR "left" : wxIndividualLayoutConstraint% left
@IVAR "width" : wxIndividualLayoutConstraint% width
@IVAR "height" : wxIndividualLayoutConstraint% height
@IVAR "centreX" : wxIndividualLayoutConstraint% centreX
@IVAR "centreY" : wxIndividualLayoutConstraint% centreY

@END

@CLASSBASE wxIndividualLayoutConstraint "wx:inidividual-layout-constraint":"wx:object"

@CONSTANT "wx:const-left" : int wxLeft
@CONSTANT "wx:const-top" : int wxTop
@CONSTANT "wx:const-right" : int wxRight
@CONSTANT "wx:const-bottom" : int wxBottom
@CONSTANT "wx:const-centre-x" : int wxCentreX
@CONSTANT "wx:const-centre-y" : int wxCentreY

@CONSTANT "wx:const-unconstrained" : int wxUnconstrained
@CONSTANT "wx:const-as-is" : int wxAsIs
@CONSTANT "wx:const-above" : int wxAbove
@CONSTANT "wx:const-below" : int wxBelow
@CONSTANT "wx:const-left-of" : int wxLeftOf
@CONSTANT "wx:const-right-of" : int wxRightOf
@CONSTANT "wx:const-same-as" : int wxSameAs
@CONSTANT "wx:const-percent-of" : int wxPercentOf
@CONSTANT "wx:const-absolute" : int wxAbsolute

@ "above" : void Above(wxWindow!,int=0);
@ "absolute" : void Absolute(int);
@ "as-is" : void AsIs();
@ "below" : void Below(wxWindow!,int=0);
@ "left-of" : void LeftOf(wxWindow!,int=0);
@ "right-of" : void RightOf(wxWindow!,int=0);

@MACRO bInt = objscheme_bundle_integer((int){x})
@MACRO ubIntE = (wxEdge)objscheme_unbundle_integer({x}, "constraint")
@MACRO ubIntR = (wxRelationship)objscheme_unbundle_integer({x}, "constraint")
@MACRO tInt = objscheme_istype_number({x}, NULL)

@ "percent-of" : void PercentOf(wxWindow!,wxEdge/bInt/ubIntE/tInt,int=0);
@ "same-as" : void SameAs(wxWindow!,wxEdge/bInt/ubIntE/tInt,int=0);

@ "set" : void Set(wxRelationship/bInt/ubIntR/tInt,wxWindow!,wxEdge/bInt/ubIntE/tInt,int=0,int=0);

@END

#endif

#endif
