
@INCLUDE prefix.xci

#include "wx_stdev.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxEvent "event":"object"

@CREATOR ()

@IVAR "time-stamp" : ExactLong timeStamp

@END

@BEGINSYMBOLS actionType > ONE > PRED
@SYM "button" : wxEVENT_TYPE_BUTTON_COMMAND
@SYM "check-box" : wxEVENT_TYPE_CHECKBOX_COMMAND
@SYM "choice" : wxEVENT_TYPE_CHOICE_COMMAND
@SYM "list-box" : wxEVENT_TYPE_LISTBOX_COMMAND
@SYM "list-box-dclick" : wxEVENT_TYPE_LISTBOX_DCLICK_COMMAND
@SYM "text-field" : wxEVENT_TYPE_TEXT_COMMAND
@SYM "slider" : wxEVENT_TYPE_SLIDER_COMMAND
@SYM "radio-box" : wxEVENT_TYPE_RADIOBOX_COMMAND
@SYM "text-field-enter" : wxEVENT_TYPE_TEXT_ENTER_COMMAND
@SYM "menu" : wxEVENT_TYPE_MENU_SELECT
@SYM "menu-popdown" : wxEVENT_TYPE_MENU_POPDOWN
@SYM "menu-popdown-none" : wxEVENT_TYPE_MENU_POPDOWN_NONE
@ENDSYMBOLS

@CLASSBASE wxCommandEvent "control-event":"event"

@CREATOR (SYM[actionType])

@IVAR "event-type" : SYM[actionType] eventType

@END


@CLASSBASE wxPopupEvent "popup-event":"control-event"

@CREATOR ()

@CLASSID wxTYPE_POPUP_EVENT

@IVAR "menu-id" : ExactLong menuId

@END

@BEGINSYMBOLS scrollMoveType > ONE > PRED
@SYM "top" : wxEVENT_TYPE_SCROLL_TOP
@SYM "bottom" : wxEVENT_TYPE_SCROLL_BOTTOM
@SYM "line-up" : wxEVENT_TYPE_SCROLL_LINEUP
@SYM "line-down" : wxEVENT_TYPE_SCROLL_LINEDOWN
@SYM "page-up" : wxEVENT_TYPE_SCROLL_PAGEUP
@SYM "page-down" : wxEVENT_TYPE_SCROLL_PAGEDOWN
@SYM "thumb" : wxEVENT_TYPE_SCROLL_THUMBTRACK
@ENDSYMBOLS

@INCLUDE wxs_ornt.xci

@CLASSBASE wxScrollEvent "scroll-event":"event"

@CREATOR ()

@IVAR "event-type" : SYM[scrollMoveType] moveType
@IVAR "direction" : SYM[orientation] direction
@IVAR "position" : rint[0|10000] pos

@END


@BEGINSYMBOLS keyCode > ONE/CHAR > PRED
@SYM "escape" : WXK_ESCAPE
@SYM "start" : WXK_START
@SYM "cancel" : WXK_CANCEL
@SYM "clear" : WXK_CLEAR
@SYM "shift" : WXK_SHIFT
@SYM "control" : WXK_CONTROL
@SYM "menu" : WXK_MENU
@SYM "pause" : WXK_PAUSE
@SYM "capital" : WXK_CAPITAL
@SYM "prior" : WXK_PRIOR
@SYM "next" : WXK_NEXT
@SYM "end" : WXK_END
@SYM "home" : WXK_HOME
@SYM "left" : WXK_LEFT
@SYM "up" : WXK_UP
@SYM "right" : WXK_RIGHT
@SYM "down" : WXK_DOWN
@SYM "select" : WXK_SELECT
@SYM "print" : WXK_PRINT
@SYM "execute" : WXK_EXECUTE
@SYM "snapshot" : WXK_SNAPSHOT
@SYM "insert" : WXK_INSERT
@SYM "help" : WXK_HELP
@SYM "numpad0" : WXK_NUMPAD0
@SYM "numpad1" : WXK_NUMPAD1
@SYM "numpad2" : WXK_NUMPAD2
@SYM "numpad3" : WXK_NUMPAD3
@SYM "numpad4" : WXK_NUMPAD4
@SYM "numpad5" : WXK_NUMPAD5
@SYM "numpad6" : WXK_NUMPAD6
@SYM "numpad7" : WXK_NUMPAD7
@SYM "numpad8" : WXK_NUMPAD8
@SYM "numpad9" : WXK_NUMPAD9
@SYM "numpad-enter" : 3
@SYM "multiply" : WXK_MULTIPLY
@SYM "add" : WXK_ADD
@SYM "separator" : WXK_SEPARATOR
@SYM "subtract" : WXK_SUBTRACT
@SYM "decimal" : WXK_DECIMAL
@SYM "divide" : WXK_DIVIDE
@SYM "f1" : WXK_F1
@SYM "f2" : WXK_F2
@SYM "f3" : WXK_F3
@SYM "f4" : WXK_F4
@SYM "f5" : WXK_F5
@SYM "f6" : WXK_F6
@SYM "f7" : WXK_F7
@SYM "f8" : WXK_F8
@SYM "f9" : WXK_F9
@SYM "f10" : WXK_F10
@SYM "f11" : WXK_F11
@SYM "f12" : WXK_F12
@SYM "f13" : WXK_F13
@SYM "f14" : WXK_F14
@SYM "f15" : WXK_F15
@SYM "f16" : WXK_F16
@SYM "f17" : WXK_F17
@SYM "f18" : WXK_F18
@SYM "f19" : WXK_F19
@SYM "f20" : WXK_F20
@SYM "f21" : WXK_F21
@SYM "f22" : WXK_F22
@SYM "f23" : WXK_F23
@SYM "f24" : WXK_F24
@SYM "numlock" : WXK_NUMLOCK
@SYM "scroll" : WXK_SCROLL
@ENDSYMBOLS

@CLASSBASE wxKeyEvent "key-event":"event"

@MACRO SETX0 = x0=wxEVENT_TYPE_CHAR;
@CREATOR (-int=wxEVENT_TYPE_CHAR); : : /SETX0

@IVAR "key-code" : SYM[keyCode] keyCode
@IVAR "shift-down" : bool shiftDown
@IVAR "control-down" : bool controlDown
@IVAR "meta-down" : bool metaDown
@IVAR "alt-down" : bool altDown

@IVAR "x" : float x
@IVAR "y" : float y

@END

@BEGINSYMBOLS mouseEventType > ONE > PRED
@SYM "left-down" : wxEVENT_TYPE_LEFT_DOWN
@SYM "left-up" : wxEVENT_TYPE_LEFT_UP
@SYM "middle-down" : wxEVENT_TYPE_MIDDLE_DOWN
@SYM "middle-up" : wxEVENT_TYPE_MIDDLE_UP
@SYM "right-down" : wxEVENT_TYPE_RIGHT_DOWN
@SYM "right-up" : wxEVENT_TYPE_RIGHT_UP
@SYM "motion" : wxEVENT_TYPE_MOTION
@SYM "enter" : wxEVENT_TYPE_ENTER_WINDOW
@SYM "leave" : wxEVENT_TYPE_LEAVE_WINDOW
@ENDSYMBOLS

#define NEGATIVE_ONE (-1)
@BEGINSYMBOLS buttonId > ONE > PRED BUNDLE
@SYM "any" : NEGATIVE_ONE
@SYM "left" : 1
@SYM "middle" : 2
@SYM "right" : 3
@ENDSYMBOLS

@CLASSBASE wxMouseEvent "mouse-event":"event"

@CREATOR (SYM[mouseEventType]);

@ "button-changed?" : bool Button(SYM[buttonId]=-1);
@ "button-down?" : bool ButtonDown(SYM[buttonId]=-1);
@ "button-up?" : bool ButtonUp(SYM[buttonId]=-1);
@ "dragging?" : bool Dragging();
@ "entering?" : bool Entering();
@ "leaving?" : bool Leaving();
@ "moving?" : bool Moving();

@IVAR "event-type" : SYM[mouseEventType] eventType
@IVAR "left-down" : bool leftDown
@IVAR "middle-down" : bool middleDown
@IVAR "right-down" : bool rightDown
@IVAR "shift-down" : bool shiftDown
@IVAR "control-down" : bool controlDown
@IVAR "meta-down" : bool metaDown
@IVAR "alt-down" : bool altDown
@IVAR "x" : float x
@IVAR "y" : float y

@END
