
@INCLUDE prefix.xci

#include "wx_slidr.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxSlider "wx:slider" : "wx:item"

@SET CALLBACK_CLASS = wxSlider
@SET CALLBACK_CLASS_USER = "wx:slider%::initialization"
@INCLUDE cb_start.xci

@MACRO PROGRESS = if (x3 < x4 || x5 < x3) scheme_signal_error("wx:slider%%::initialization: minimum, value, and maximum must be increasing");
@MACRO NOZEROX[p] = if (x<p> <= 0) x<p> = 1;

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback,nstring,int,int,int,int,int=-1,int=-1,long=wxHORIZONTAL,string="slider"); : : ubCallbackSetup/PROGRESS|NOZEROX[6]//ubCallbackCreatorFinish

@INCLUDE wxs_item.xci

@ "get-value" : int GetValue();
@ "set-value" : void SetValue(int);

@END

@INCLUDE cb_end.xci
