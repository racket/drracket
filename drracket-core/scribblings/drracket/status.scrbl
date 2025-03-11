#lang scribble/doc
@(require "common.rkt")

@title[#:tag "drracket-status"]{Status Information}

The DrRacket window contains a number of different
pieces of status information that tries to expose
what DrRacket is doing in a relatively unobtrusive way.

In the bottom right corner of the DrRacket window
there are a number of pieces of information. From
left to right:
@itemize[
 @item{       
  The name of the language that DrRacket is currently using.
  This is also a popup menu that you can use to change
  the language from a list of recently used languages.
 }
 @item{
  Further over to the right are some numbers
  showing the line and column or the character
  offset of the insertion point (where characters
  will appear when you next type).
 }
 
 @item{
  The current heap size that DrRacket is using
  internally. The number is updated infrequently;
  to see the accurate number, click on it, which also
  triggers a major garbage collection.
 }
 @item{
  An small square that is blank unless DrRacket
  is doing a garbage collection and a recycle
  icon when DrRacket is doing garbage collection.
 }
 @item{
  A narrow blank space or a pair of parentheses.
  If DrRacket is working to determine the
  colors and where the matching parentheses
  are in the definitions, that will be a
  pair of parentheses and if not, it will be blank.
       }
 @item{
  A small person that indicates if DrRacket is
  running the program in the interactions window or
  not. (The person will run when DrRacket is running
  your program.)
 }
 @item{
  A circle that indicates the status of DrRacket's online
  expansion (only available when using ``Determine language
  from source''). If it is red, there is an error in the
  program or the online expansion has been disabled.
  If it is blue, DrRacket is working to expand your program.
  If it is (dark or light) purple, DrRacket is processing
  the result of the expansion to compute where the arrows
  go and which how renaming any given variable affects
  any of the others. If it is green, DrRacket's online
  expansion has finished. If it is a spiky green ball
  (instead of a round green ball), then DrRacket will use
  the result of the online expansion to speed up the
  @onscreen{Run} button. That is, when the green spiky ball
  is present, DrRacket will not
  expand or compile your program, but just run it directly.
 }
 ]
