#lang scribble/doc
@(require "common.rkt"
          scribble/struct scribble/bnf
          racket/list racket/runtime-path racket/port
          mrlib/tex-table
          (for-label drracket/tool-lib))

@(define (keybinding key . desc)
   (let* ([keys (if (string? key) (list key) key)]
          [key-str (apply string-append (add-between keys " "))])
     (apply item @index[(map (lambda (x) (format "~a keybinding" x)) keys) key-str] " : " desc)))

@(define-syntax-rule (def-mod-beg id)
   (begin
     (require (for-label racket/base))
     (define id @racket[#%module-begin])))
@(def-mod-beg mz-mod-begin)

@title[#:tag "Keyboard Shortcuts"]{Keyboard Shortcuts}

@index['("keybindings")]{Most} key presses simply insert a character
into the editor, such as @litchar{a}, @litchar{3}, or
@litchar{(}. Other keys and key combinations act as keyboard shortcuts
that move the blinking caret, delete a line, copy the selection,
etc. Keyboard shortcuts are usually trigger by key combinations using
the Control, Meta, or Command key.

The keyboard shortcuts are displayed using the same string-encoded notation
for modifier keys as @xmethod[keymap% map-function] and the Keybindings view;

@itemize[

 @item{@litchar{s:} --- All platforms: Shift}

 @item{@litchar{c:} --- All platforms: Control}

 @item{@litchar{a:} --- Mac OS: Option}

 @item{@litchar{m:} --- Windows: Alt; Unix: Meta; Mac OS: Command, when
 @racket[map-command-as-meta-key] produces @racket[#t]}

 @item{@litchar{d:} --- Mac OS: Command}

 @item{@litchar{l:} --- All platforms: Caps Lock}

 @item{@litchar{g:} --- Windows: Control plus Alt as AltGr;
                        see @xmethod[key-event% get-control+meta-is-altgr]}

 @item{@litchar{?:} --- All platforms: allow match to character produced by opposite 
                  use of Shift, AltGr/Option, and/or Caps Lock, when available; see
                  @xmethod[key-event% get-other-shift-key-code]}
]

If a modifier key is not specified, it matches whether that modifier is
 pressed or not pressed. A @litchar{~} prior to a modifier indicates the
 keybinding only activates if that modifier is not pressed.

A keyboard shortcut that begins with @litchar{:} only activates if the modifiers
 Shift, Control, Option, Alt, Meta, or Command are not pressed.

@margin-note{Many of the key-binding actions can also be performed
with menu items.}

c:@nonterm{key} means press the Control key, hold it down and then
press @nonterm{key} and then release them both. For example: c:e
(Control-E) moves the blinking caret to the end of the current line.

m:@nonterm{key} is the same as c:@nonterm{key}, except with the Meta
key.  Depending on your keyboard, Meta may be called ``Left,''
``Right,'' or have a diamond symbol, but it's usually on the bottom
row next to the space bar. m:@nonterm{key} can also be performed as a
two-character sequence: first, strike and release the Escape key, then
strike @nonterm{key}. On Mac OS, Meta is, by default,
available only through the Escape key. But the preferences dialog
(in the @onscreen{General Editing} sub-panel of the @onscreen{Editing} panel)
has check boxes that adjust the handling of the Alt key or the Command
key to be meta.

@litchar{delete} is the Delete key.

@litchar{space} is the Space bar.
  
On most keyboards, ``<'' and ``>'' are shifted characters. So, to
get m:>, you actually have to type Meta-Shift->. That is, press and
hold down both the Meta and Shift keys, and then strike ``>''.

On Windows (and sometimes under Unix)
some of these keybindings are actually standard menu
items.  Those keybindings will behave according to the menus, unless
the @onscreen{Enable keybindings in menus} preference is unchecked.
For example, the c:e keybinding mentioned above actually toggles
the visibility of the interactions window.

@index['("Emacs keybindings")]{If} you are most familiar with
Emacs-style key bindings (especially on windows or some linux installations
where the control key is, by default, for the menu shortcuts), 
you should uncheck the @onscreen{Enable
keybindings in menus} preference. Many of the keybindings below are
inspired by Emacs. See also @secref["defining-shortcuts"] for suggestions
on how to bind keys to menu items on a selective basis.

@margin-note{Some keybindings are not available if @onscreen{Enable
keybindings in menus} is enabled.}

And finally, the authoritative source for keybindings 
is the @onscreen{Edit} menu's @onscreen{Show Active Keybindings}
menu item. Keybindings in DrRacket are often sensitive to
the window that has the keyboard focus, so the contents
of the window that @onscreen{Show Active Keybindings} opens
will depend where the keyboard focus was when the menu was
selected.

@section{Moving Around}

@itemize[
@keybinding["c:f"]{move forward one character}
@keybinding["c:b"]{move backward one character}
@keybinding["m:f"]{move forward one word}
@keybinding["m:b"]{move backward one word}
@keybinding["c:v"]{move forward one page}
@keybinding["m:v"]{move backward one page}
@keybinding["m:<"]{move to beginning of file}
@keybinding["m:>"]{move to end of file}

@keybinding["c:a"]{move to beginning of line (left)}
@keybinding["c:e"]{move to end of line (right)}
@keybinding["c:n"]{move to next line (down)}
@keybinding["c:p"]{move to previous line (up)}

@keybinding["m:c:f"]{move forward one S-expression}
@keybinding["m:c:b"]{move backward one S-expression}
@keybinding["m:c:u"]{move up out of an S-expression}
@keybinding["m:c:d"]{move down into a nested S-expression}
@keybinding["m:c:space"]{select forward S-expression}
@keybinding["m:c:p"]{match parentheses backward}

@keybinding["m:c:left"]{move backwards to the nearest editor box}
@keybinding["A-c:left"]{move backwards to the nearest editor box}
@keybinding["m:c:right"]{move forward to the nearest editor box}
@keybinding["A-c:right"]{move forward to the nearest editor box}
@keybinding["m:c:up"]{move up out of an embedded editor}
@keybinding["A-c:up"]{move up out of an embedded editor}
@keybinding["m:c:down"]{move down into an embedded editor}
@keybinding["A-c:down"]{move down into an embedded editor}

@keybinding["c:c;c:z"]{move the cursor to the interactions window}
@keybinding["c:F6"]{move the cursor between different windows (usually
                    the interactions and definitions windows, but also the
                    search window and other editable portions of DrRacket).
                    Also, search for ``shift-focus'' in the
                    @onscreen{Show Active Keybindings} menu's window for more, 
                    platform-specific keybindings that have this functionality}
]

@section{Editing Operations}

@itemize[
@keybinding["c:_"]{undo}
@keybinding["c:+"]{redo}
@keybinding["c:x;u"]{undo}

@keybinding["c:d"]{delete forward one character}
@keybinding["c:h"]{delete backward one character}
@keybinding["m:d"]{delete forward one word}
@keybinding["m:delete"]{delete backward one word}
@keybinding["c:k"]{delete forward to end of line}
@keybinding["m:c:k"]{delete forward one S-expression}

@keybinding["m:w"]{copy selection to clipboard}
@keybinding["c:w"]{delete selection to clipboard (cut)}
@keybinding["c:y"]{paste from clipboard (yank)}

@keybinding["c:t"]{transpose characters}
@keybinding["m:t"]{transpose words}
@keybinding["m:c:t"]{transpose sexpressions}

@keybinding["m:c:m"]{toggle dark green marking of matching parenthesis}
@keybinding["m:c:k"]{cut complete sexpression}

@keybinding["m:("]{wrap selection in parentheses}
@keybinding["m:["]{wrap selection in square brackets}
@keybinding["m:{"]{wrap selection in curly brackets}
@keybinding["m:s:l"]{wrap selection in @litchar{(lambda () }...@litchar{)}
                     and put the insertion point in the argument list of the lambda}

@keybinding["c:c;c:o"]{the sexpression following the
  insertion point is put in place of its containing sexpression}
@keybinding["c:c;c:e"]{the first and last characters (usually parentheses)
  of the containing expression are removed}
@keybinding["c:cc;:l"]{wraps a let around the
  sexpression following the insertion point and puts a printf in at
  that point (useful for debugging).}

@keybinding["m:o"]{toggle @as-index{overwrite mode}}

@keybinding["c:x;r;a"]{Adjust nearby ASCII art rectangles 
                       (that use @litchar{+}, @litchar{-}, or @litchar{|})
                       to use Unicode characters.
                       
                       For example, if the insertion point is next to this rectangle:
                       @tabular[(list (list @litchar{+-+})
                                      (list @litchar{| |})
                                      (list @litchar{+-+}))]
                       then the keystroke will turn it into this one:
                       @tabular[(list (list @litchar{╔═╗})
                                      (list @litchar{║ ║})
                                      (list @litchar{╚═╝}))]
                       Similarly, if the rectangle near the insertion point has
                       mixed Unicode and ASCII, it will all be converted to 
                       the Unicode characters.
                       }

@keybinding["c:x;r;w"]{Widen the nearby ASCII art rectangles.
                       
                       For example, if the insertion point is just to the left of
                       the middle line of this rectangle:
                       @tabular[(list (list @litchar{╔═╦══╗})
                                      (list @litchar{║ ║  ║})
                                      (list @litchar{╠═╬══╣})
                                      (list @litchar{║ ║  ║})
                                      (list @litchar{╚═╩══╝}))]
                       then the keystroke will turn it into this one:
                       @tabular[(list (list @litchar{╔══╦══╗})
                                      (list @litchar{║  ║  ║})
                                      (list @litchar{╠══╬══╣})
                                      (list @litchar{║  ║  ║})
                                      (list @litchar{╚══╩══╝}))]
                       }
@keybinding["c:x;r;v"]{Make the nearby ASCII art rectangles taller.
                       
                       For example, if the insertion point is just above the
                       the middle line of this rectangle:
                       @tabular[(list (list @litchar{╔══╦══╗})
                                      (list @litchar{║  ║  ║})
                                      (list @litchar{╠══╬══╣})
                                      (list @litchar{║  ║  ║})
                                      (list @litchar{║  ║  ║})
                                      (list @litchar{╚══╩══╝}))]
                       then the keystroke will turn it into this one:
                       @tabular[(list (list @litchar{╔══╦══╗})
                                      (list @litchar{║  ║  ║})
                                      (list @litchar{║  ║  ║})
                                      (list @litchar{╠══╬══╣})
                                      (list @litchar{║  ║  ║})
                                      (list @litchar{║  ║  ║})
                                      (list @litchar{╚══╩══╝}))]
                       }
@keybinding["c:x;r;c"]{Centers the contents of the current line inside the enclosing
                       cell of the enclosing ASCII art rectangle.}

 @keybinding["c:x;r;o"]{
  Toggles the ASCII art rectangle editing mode. When the mode is enabled,
  key strokes that would normally break the rectangles instead enlarge them.
  Specifically:
  @itemlist[@item{Return and enter add a line to the enclosing rectangle
               and put the insertion point at the first column of the
               enclosing cell.}
             @item{When in overwrite mode, if a key would overwrite one
                   of the walls of the cell, the wall is first moved over
                   to accomodate the new key}
             @item{When not in overwrite mode, inserting a character will always
                   widen the containing cell}]
 }
]

@section{File Operations}

@itemize[
@keybinding["c:x;c:s"]{save file}
@keybinding["c:x;c:w"]{save file under new name}
]

@section{Search}

@itemize[
@keybinding["c:s"]{search for string forward}
@keybinding["c:r"]{search for string backward}
]

@section{Evaluation}

@itemize[
@keybinding["f5"]{Run}
]

@section{Documentation}
@itemize[
  @keybinding["f1"]{Search in the documentation for the words near the insertion point}
  @keybinding["f2"]{Reveal the signature box for the identifier at the insertion point (requires
                    background check syntax to be enabled or normal check syntax to have been
                    run).}
]

@section{Interactions}

The @tech{interactions window} has all of the same keyboard shortcuts
as the @tech{definitions window} plus a few more:

@itemize[
@keybinding["m:p"]{bring the previously entered expression down to the prompt}
@keybinding["m:n"]{bring the expression after the current expression in the
  expression history down to the prompt}
@keybinding["m:h"]{Show the current expression history in a separate window}
]

Also, in the interactions window, the return key is treated
specially, in particular to determine if the expression
that's been typed at the prompt is ready to be evaluated or
not. There are three cases:
@itemlist[

 @item{When shift-return is typed: the expression is not
  submitted and instead a new line is started}

 @item{When alt-return or control return is typed: the
  expression is submitted for evaluation}

 @item{When return with no modified is typed: the language
  is consulted to determine which of the two previous actions
  makes the most sense. Typically the language's parser is
  consulted to see if the expression looks complete. If so,
  the expression is submitted and, if not, a new line is
  started. (See also @(elemref "drracket:submit-predicate" @racket['drracket:submit-predicate]).)
  }
 ]

@section{LaTeX and TeX inspired keybindings}

@itemize[
@keybinding['("c:\\" "m:\\" "c:x;l")]{traces backwards from the insertion
point, looking for a backslash followed by a @index["LaTeX"]{LaTeX} 
macro name or a prefix of such a name. If a macro name is found,
it replaces the backslash and the name with the corresponding key in
the table below; if a (proper) prefix @math{p} is found, it replaces @math{p} 
with the longest common prefix of all macro names that have @math{p} as a 
prefix (unless there is only one such name, in which case it behaves as if 
@math{p} were a complete macro name).

These are the currently supported macro names and the keys they map into:
@(let ()
   (define pairs
     (for/list ([line (in-list tex-shortcut-table)]
                #:unless (regexp-match? #rx"^b.$" (list-ref line 0)))
       (define macro (list-ref line 0))
       (define char (list-ref line 1))
       (when (equal? char "\f")
         (set! char "^L"))
       (unless (equal? (regexp-replace* #px"[[:cntrl:]]" char " ") char)
         (error 'keybindings.scrbl "cannot render a non-printing character"))
       (list (make-paragraph (list (index (format "\\~a keyboard shortcut" macro))
                                   (tt (format " \\~a" macro))))
             (make-paragraph (list (hspace 1) char)))))
   (define middle-columns (list (hspace 2) (hspace 1)))
   (tabular
    #:column-properties '(left left right-border left left left)
    (let loop ([pairs pairs])
      (cond
        [(null? pairs) '()]
        [(null? (cdr pairs))
         (list (append (car pairs) middle-columns (list (make-flow '()) (make-flow '()))))]
        [else
         (cons (append (car pairs) middle-columns (cadr pairs))
               (loop (cddr pairs)))]))))

  Additionally, all of the digits, lowercase letters, and uppercase
  letters can be turned into blackboard bold characters with
  the shortcut @tt{\b} followed by the character. For example, @tt{\bN}
  turns into ℕ and @tt{\bZ} turns into ℤ.
}
]

@section[#:tag "defining-shortcuts"]{Defining Custom Shortcuts}

 The @onscreen{Add User-defined Keybindings...} menu item in the
 @onscreen{Keybindings} sub-menu of @onscreen{Edit} selects a file
 containing Racket definitions of keybindings. The file must contain a
 module that uses a special keybindings language,
 @racket[framework/keybinding-lang]. To do so, begin your file with
 this line:

@racketmod[
s-exp framework/keybinding-lang
]

The @racket[framework/keybinding-lang] languages provides all of the bindings
from @racketmodname[racket], @racketmodname[racket/class], and
@racketmodname[drracket/tool-lib], 
except that it adjusts @|mz-mod-begin| to
introduce a @racketidfont{keybinding} form:

@specform[#:literals (keybindings)
          (keybinding string-expr proc-expr)]{

Declares a keybinding, where @racket[string-expr] must produce a
suitable first argument for @xmethod[keymap% map-function], and the
@racket[proc-expr] must produce a suitable second argument for
@xmethod[keymap% add-function].}

For example, this remaps the key combination ``control-a'' key to ``!''.

@racketmod[
s-exp framework/keybinding-lang
(keybinding "c:a" (λ (editor evt) (send editor insert "!")))
]

Since the file contains plain Racket code, you can write keybindings
files that use DrRacket's @seclink[#:doc '(lib
"scribblings/tools/tools.scrbl") "implementing-tools"]{Extension API}.
For example, the following file binds ``control-t'' and ``control-='' to
a execute the program and open a new tab respectively, as they were used
before version 5.2.

@racketmod[
s-exp framework/keybinding-lang

(define modifiers
  (apply string-append
         (map (λ (p)
                (case p
                  [(ctl) "c:"] [(cmd) "d:"] [(alt meta) "~c:m:"]
                  [(shift) "s:"] [(option) "a:"]))
              (get-default-shortcut-prefix))))

(define-syntax-rule (frame-key key command)
  (keybinding
   (string-append modifiers key)
   (λ (ed evt)
     (when (is-a? ed text:basic<%>)
       (define fr (send ed get-top-level-window))
       @code:comment{note: fr could be #f}
       (when fr (send fr command))))))

(frame-key "t" execute-callback)
(frame-key "=" create-new-tab)
]

Another example, this file rebinds ``control-w'' to delete the word
behind the insertion point, but it does it by setting a new key to 
be an existing keyboard shortcut. If you see a key in the 
@onscreen{Show Active Keybindings} dialog (in the @onscreen{Keybindings}
submenu of the @onscreen{Edit} menu), then you can use its
name with the new keystroke you want, like this:

@racketmod[
s-exp framework/keybinding-lang

(define (rebind key command)
  (keybinding
   key
   (λ (ed evt)
     (send (send ed get-keymap) call-function
           command ed evt #t))))

(rebind "c:w" "backward-kill-word")
]

This example shows how to bind a menu item (based on its name) to a particular key.
The call at the end of the example binds ``control-a'' to the @onscreen{Run}
menu item.

@racketmod[
s-exp framework/keybinding-lang

(define (menu-bind key menu-item)
  (keybinding
   key
   (λ (ed evt)
     (define canvas (send ed get-canvas))
     (when canvas
       (define menu-bar (find-menu-bar canvas))
       (when menu-bar
         (define item (find-item menu-bar menu-item))
         (when item
           (define menu-evt
             (new control-event% 
                  [event-type 'menu]
                  [time-stamp 
                   (send evt get-time-stamp)]))
           (send item command menu-evt)))))))

(define/contract (find-menu-bar c)
  (-> (is-a?/c area<%>) (or/c #f (is-a?/c menu-bar%)))
  (let loop ([c c])
    (cond
      [(is-a? c frame%) (send c get-menu-bar)]
      [(is-a? c area<%>) (loop (send c get-parent))]
      [else #f])))

(define/contract (find-item menu-bar label)
  (-> (is-a?/c menu-bar%)
      string?
      (or/c (is-a?/c selectable-menu-item<%>) #f))
  (let loop ([o menu-bar])
    (cond
      [(is-a? o selectable-menu-item<%>)
       (and (equal? (send o get-plain-label) label)
            o)]
      [(is-a? o menu-item-container<%>)
       (for/or ([i (in-list (send o get-items))])
         (loop i))]
      [else #f])))

(menu-bind "c:a" "Run")]

Note that DrRacket does not reload keybindings files automatically when you
make changes, so you'll need to restart DrRacket to see changes to
the file.

@section{Sending Program Fragments to the REPL}

@index['("Emacs keybindings")]Users comfortable with Emacs and the conventional Lisp/Scheme-style
of interaction with an ``inferior process'' commonly request
keybindings in DrRacket that send program fragments to be evaluated
at the prompt. This style of interaction is fraught with difficulty, 
especially for beginners, and so DrRacket, by default, does not support
it. Instead, clicking DrRacket's ``Run'' button starts with a clean slate
and sends the entire contents of the definitions window, ensuring that
the state in the REPL matches what you would expect by reading
the source code of the program.

Based on years of experience with Emacs modes, some of the authors 
consider this mode of interaction also appropriate for experienced 
programmers. Indeed, they go through great effort to mimic this 
behavior in Emacs. 

That said, some people may wish to use such incremental keystroke modes
anyway. Therefore the remainder of this section illustrates how to add such
an incremental mode for your personal use with an example keybindings
file. Specifically, the file shows how to add the ability to send
expressions piecemeal to the interactions window. It also demonstrates how
to pull together a bunch of pieces of DrRacket's implementation and its
libraries to implement keystrokes. 

@(define-runtime-path incremental-keybindings.rkt "incremental-keybindings.rkt")
@(let ([sp (open-output-string)])
   (call-with-input-file incremental-keybindings.rkt
     (λ (port)
       (copy-port port sp)))
   (codeblock (get-output-string sp)))

Others may wish to use the above example to invent other keystrokes for
making work in DrRacket convenient. 
