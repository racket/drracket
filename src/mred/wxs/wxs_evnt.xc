
@INCLUDE prefix.xci

#include "wx_stdev.h"

@INCLUDE wxs.xci

@HEADER

#if !defined(wx_mac)
#define NEW_EVENT_IDS 1
#else
#define NEW_EVENT_IDS 0
#endif

@CLASSBASE wxEvent "wx:event":"wx:object"

// @CREATOR ()

// These are not ready to be used:
// @ H "write-event" : bool WriteEvent(ostream%);
// @ H "read-event" : bool ReadEvent(istream%);

@IVAR "event-class" : long eventClass
@IVAR "event-type" : long eventType
@IVAR "event-object" : wxObject! eventObject

// Don't know where else to put these:
@CONSTANT "wx:const-event-type-scroll-top" : int wxEVENT_TYPE_SCROLL_TOP ## NEW_EVENT_IDS
@CONSTANT "wx:const-event-type-scroll-bottom" : int wxEVENT_TYPE_SCROLL_BOTTOM ## NEW_EVENT_IDS
@CONSTANT "wx:const-event-type-scroll-lineup" : int wxEVENT_TYPE_SCROLL_LINEUP ## NEW_EVENT_IDS
@CONSTANT "wx:const-event-type-scroll-linedown" : int wxEVENT_TYPE_SCROLL_LINEDOWN ## NEW_EVENT_IDS
@CONSTANT "wx:const-event-type-scroll-pageup" : int wxEVENT_TYPE_SCROLL_PAGEUP ## NEW_EVENT_IDS
@CONSTANT "wx:const-event-type-scroll-pagedown" : int wxEVENT_TYPE_SCROLL_PAGEDOWN ## NEW_EVENT_IDS
@CONSTANT "wx:const-event-type-scroll-thumbtrack" : int wxEVENT_TYPE_SCROLL_THUMBTRACK ## NEW_EVENT_IDS

@CONSTANT "wx:const-type-key-event" : int wxTYPE_KEY_EVENT
@CONSTANT "wx:const-type-command-event" : int wxTYPE_COMMAND_EVENT
@CONSTANT "wx:const-type-mouse-event" : int wxTYPE_MOUSE_EVENT

@END


@CLASSBASE wxCommandEvent "wx:command-event":"wx:event"

@CREATOR (int)

@ "get-selection" : int GetSelection();
@ "get-string" : string GetString();
@ "checked?" : bool Checked();
@ "is-selection?" : bool IsSelection();

@IVAR "extra-long" : long extraLong
@IVAR "command-int" : int commandInt
@IVAR "command-string" : string commandString

@CONSTANT "wx:const-event-type-button-command" : int wxEVENT_TYPE_BUTTON_COMMAND
@CONSTANT "wx:const-event-type-checkbox-command" : int wxEVENT_TYPE_CHECKBOX_COMMAND
@CONSTANT "wx:const-event-type-choice-command" : int wxEVENT_TYPE_CHOICE_COMMAND
@CONSTANT "wx:const-event-type-listbox-command" : int wxEVENT_TYPE_LISTBOX_COMMAND
@CONSTANT "wx:const-event-type-text-command" : int wxEVENT_TYPE_TEXT_COMMAND
@CONSTANT "wx:const-event-type-multitext-command" : int wxEVENT_TYPE_MULTITEXT_COMMAND
@CONSTANT "wx:const-event-type-menu-command" : int wxEVENT_TYPE_MENU_COMMAND
@CONSTANT "wx:const-event-type-slider-command" : int wxEVENT_TYPE_SLIDER_COMMAND
@CONSTANT "wx:const-event-type-radiobox-command" : int wxEVENT_TYPE_RADIOBOX_COMMAND
@CONSTANT "wx:const-event-type-text-enter-command" : int wxEVENT_TYPE_TEXT_ENTER_COMMAND
@CONSTANT "wx:const-event-type-set-focus" : int wxEVENT_TYPE_SET_FOCUS
@CONSTANT "wx:const-event-type-kill-focus" : int wxEVENT_TYPE_KILL_FOCUS
@CONSTANT "wx:const-event-type-scrollbar-command" : int wxEVENT_TYPE_SCROLLBAR_COMMAND   ## NEW_EVENT_IDS
@CONSTANT "wx:const-event-type-virt-listbox-command" : int wxEVENT_TYPE_VIRT_LISTBOX_COMMAND   ## NEW_EVENT_IDS

@END


@CLASSBASE wxKeyEvent "wx:key-event":"wx:event"

@CREATOR (int);

// @ "control-down?" : bool ControlDown();
// @ "shift-down?" : bool ShiftDown();
@ "key-code" : long KeyCode();

@IVAR "key-code" : long keyCode
@IVAR "shift-down" : bool shiftDown
@IVAR "control-down" : bool controlDown
@IVAR "meta-down" : bool metaDown
@IVAR "alt-down" : bool altDown
@IVAR "time-stamp" : long timeStamp

@IVAR "x" : float x
@IVAR "y" : float y

@CONSTANT "wx:const-event-type-char" : int wxEVENT_TYPE_CHAR

@CONSTANT "wx:const-k-back" : int WXK_BACK
@CONSTANT "wx:const-k-tab" : int WXK_TAB
@CONSTANT "wx:const-k-return" : int WXK_RETURN
@CONSTANT "wx:const-k-escape" : int WXK_ESCAPE
@CONSTANT "wx:const-k-space" : int WXK_SPACE
@CONSTANT "wx:const-k-delete" : int WXK_DELETE
@CONSTANT "wx:const-k-start" : int WXK_START
@CONSTANT "wx:const-k-lbutton" : int WXK_LBUTTON
@CONSTANT "wx:const-k-rbutton" : int WXK_RBUTTON
@CONSTANT "wx:const-k-cancel" : int WXK_CANCEL
@CONSTANT "wx:const-k-mbutton" : int WXK_MBUTTON
@CONSTANT "wx:const-k-clear" : int WXK_CLEAR
@CONSTANT "wx:const-k-shift" : int WXK_SHIFT
@CONSTANT "wx:const-k-control" : int WXK_CONTROL
@CONSTANT "wx:const-k-menu" : int WXK_MENU
@CONSTANT "wx:const-k-pause" : int WXK_PAUSE
@CONSTANT "wx:const-k-capital" : int WXK_CAPITAL
@CONSTANT "wx:const-k-prior" : int WXK_PRIOR
@CONSTANT "wx:const-k-next" : int WXK_NEXT
@CONSTANT "wx:const-k-end" : int WXK_END
@CONSTANT "wx:const-k-home" : int WXK_HOME
@CONSTANT "wx:const-k-left" : int WXK_LEFT
@CONSTANT "wx:const-k-up" : int WXK_UP
@CONSTANT "wx:const-k-right" : int WXK_RIGHT
@CONSTANT "wx:const-k-down" : int WXK_DOWN
@CONSTANT "wx:const-k-select" : int WXK_SELECT
@CONSTANT "wx:const-k-print" : int WXK_PRINT
@CONSTANT "wx:const-k-execute" : int WXK_EXECUTE
@CONSTANT "wx:const-k-snapshot" : int WXK_SNAPSHOT
@CONSTANT "wx:const-k-insert" : int WXK_INSERT
@CONSTANT "wx:const-k-help" : int WXK_HELP
@CONSTANT "wx:const-k-numpad0" : int WXK_NUMPAD0
@CONSTANT "wx:const-k-numpad1" : int WXK_NUMPAD1
@CONSTANT "wx:const-k-numpad2" : int WXK_NUMPAD2
@CONSTANT "wx:const-k-numpad3" : int WXK_NUMPAD3
@CONSTANT "wx:const-k-numpad4" : int WXK_NUMPAD4
@CONSTANT "wx:const-k-numpad5" : int WXK_NUMPAD5
@CONSTANT "wx:const-k-numpad6" : int WXK_NUMPAD6
@CONSTANT "wx:const-k-numpad7" : int WXK_NUMPAD7
@CONSTANT "wx:const-k-numpad8" : int WXK_NUMPAD8
@CONSTANT "wx:const-k-numpad9" : int WXK_NUMPAD9
@CONSTANT "wx:const-k-multiply" : int WXK_MULTIPLY
@CONSTANT "wx:const-k-add" : int WXK_ADD
@CONSTANT "wx:const-k-separator" : int WXK_SEPARATOR
@CONSTANT "wx:const-k-subtract" : int WXK_SUBTRACT
@CONSTANT "wx:const-k-decimal" : int WXK_DECIMAL
@CONSTANT "wx:const-k-divide" : int WXK_DIVIDE
@CONSTANT "wx:const-k-f1" : int WXK_F1
@CONSTANT "wx:const-k-f2" : int WXK_F2
@CONSTANT "wx:const-k-f3" : int WXK_F3
@CONSTANT "wx:const-k-f4" : int WXK_F4
@CONSTANT "wx:const-k-f5" : int WXK_F5
@CONSTANT "wx:const-k-f6" : int WXK_F6
@CONSTANT "wx:const-k-f7" : int WXK_F7
@CONSTANT "wx:const-k-f8" : int WXK_F8
@CONSTANT "wx:const-k-f9" : int WXK_F9
@CONSTANT "wx:const-k-f10" : int WXK_F10
@CONSTANT "wx:const-k-f11" : int WXK_F11
@CONSTANT "wx:const-k-f12" : int WXK_F12
@CONSTANT "wx:const-k-f13" : int WXK_F13
@CONSTANT "wx:const-k-f14" : int WXK_F14
@CONSTANT "wx:const-k-f15" : int WXK_F15
@CONSTANT "wx:const-k-f16" : int WXK_F16
@CONSTANT "wx:const-k-f17" : int WXK_F17
@CONSTANT "wx:const-k-f18" : int WXK_F18
@CONSTANT "wx:const-k-f19" : int WXK_F19
@CONSTANT "wx:const-k-f20" : int WXK_F20
@CONSTANT "wx:const-k-f21" : int WXK_F21
@CONSTANT "wx:const-k-f22" : int WXK_F22
@CONSTANT "wx:const-k-f23" : int WXK_F23
@CONSTANT "wx:const-k-f24" : int WXK_F24
@CONSTANT "wx:const-k-numlock" : int WXK_NUMLOCK
@CONSTANT "wx:const-k-scroll" : int WXK_SCROLL

@END

@CLASSBASE wxMouseEvent "wx:mouse-event":"wx:event"

@CREATOR (int);

@ "button?" : bool Button(int);
@ "button-d-click?" : bool ButtonDClick(int=-1);
@ "button-down?" : bool ButtonDown(int=-1);
@ "button-up?" : bool ButtonUp(int=-1);
// @ "control-down?" : bool ControlDown();
// @ "shift-down?" : bool ShiftDown();
@ "dragging?" : bool Dragging();
@ "entering?" : bool Entering();
@ "leaving?" : bool Leaving();
@ "is-button?" : bool IsButton();
// @ "left-down?" : bool LeftDown();
// @ "left-is-down?" : bool LeftIsDown();
// @ "left-up?" : bool LeftUp();
// @ "middle-down?" : bool MiddleDown();
// @ "middle-is-down?" : bool MiddleIsDown();
// @ "middle-up?" : bool MiddleUp();
@ "moving?" : bool Moving();
// @ "right-down?" : bool RightDown();
// @ "right-is-down?" : bool RightIsDown();
// @ "right-up?" : bool RightUp();
// @ "position" : void Position(float*,float*);

@IVAR "left-down" : bool leftDown
@IVAR "middle-down" : bool middleDown
@IVAR "right-down" : bool rightDown
@IVAR "shift-down" : bool shiftDown
@IVAR "control-down" : bool controlDown
@IVAR "meta-down" : bool metaDown
@IVAR "alt-down" : bool altDown
@IVAR "x" : float x
@IVAR "y" : float y
@IVAR "time-stamp" : long timeStamp

@CONSTANT "wx:const-event-type-left-down" : int wxEVENT_TYPE_LEFT_DOWN
@CONSTANT "wx:const-event-type-left-up" : int wxEVENT_TYPE_LEFT_UP
@CONSTANT "wx:const-event-type-middle-down" : int wxEVENT_TYPE_MIDDLE_DOWN
@CONSTANT "wx:const-event-type-middle-up" : int wxEVENT_TYPE_MIDDLE_UP
@CONSTANT "wx:const-event-type-right-down" : int wxEVENT_TYPE_RIGHT_DOWN
@CONSTANT "wx:const-event-type-right-up" : int wxEVENT_TYPE_RIGHT_UP
@CONSTANT "wx:const-event-type-motion" : int wxEVENT_TYPE_MOTION
@CONSTANT "wx:const-event-type-enter-window" : int wxEVENT_TYPE_ENTER_WINDOW
@CONSTANT "wx:const-event-type-leave-window" : int wxEVENT_TYPE_LEAVE_WINDOW
@CONSTANT "wx:const-event-type-left-dclick" : int wxEVENT_TYPE_LEFT_DCLICK
@CONSTANT "wx:const-event-type-middle-dclick" : int wxEVENT_TYPE_MIDDLE_DCLICK
@CONSTANT "wx:const-event-type-right-dclick" : int wxEVENT_TYPE_RIGHT_DCLICK

@END
