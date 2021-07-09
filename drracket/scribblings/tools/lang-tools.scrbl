#lang scribble/doc
@(require "common.rkt")

@title[#:tag "lang-languages-customization"]{
 DrRacket support for @tt{#lang}-based Languages
}

The simplest and best way to extend DrRacket with support
for a new language is to implement the language via @tt{
 #lang} (see
@secref["hash-languages" #:doc '(lib "scribblings/guide/guide.scrbl")]
for more details). DrRacket will then use @racket[read-language] to
find code and values that it uses to customize itself to the language.

If the call to @racket[read-language] raises an error,
DrRacket logs the error at the @racket[_debug] level to a
logger with the name @racket['drracket-language] (see
@secref["logging" #:doc '(lib "scribblings/reference/reference.scrbl")]
for more about how to follow specific loggers).

With the exception of the
@racket['definitions-text-surrogate], if there is an error
during a use of one of these extensions, DrRacket notices
the error and removes all of the extensions for the
language. It also shows the error at the bottom of the
DrRacket frame (prefixed by @tt{#lang}). Note that this
applies only to errors that occur during the dynamic extent
of a use of one of these extensions. If an extension were to,
for example, create a new thread that (eventually) caused an
error, DrRacket would not notice that error and would not
remove the extensions.

When experimenting with changes to these extensions, use the
@menuitem["Racket" "Reload #lang Extensions"] menu item to
cause DrRacket to remove the extensions and reload the
implementations from the files on disk.

DrRacket calls the language's @racket[read-language]'s
@racket[_get-info] procedure with the following
@racket[_key] arguments:

@itemize[@item{@language-info-ref[drracket:default-filters]}
         @item{@language-info-ref[drracket:default-extension]}
         @item{@language-info-ref[drracket:indentation]}
         @item{@language-info-ref[drracket:range-indentation]}
         @item{@language-info-ref[drracket:grouping-position]}
         @item{@language-info-ref[drracket:keystrokes]}
         @item{@language-info-ref[drracket:show-big-defs/ints-labels]}
         @item{@language-info-ref[drracket:opt-out-toolbar-buttons]}
         @item{@language-info-ref[drracket:opt-in-toolbar-buttons]}
         @item{@language-info-ref[drracket:submit-predicate]}
         @item{@language-info-ref[drracket:toolbar-buttons]}
         @item{@language-info-ref[color-lexer]}
         @item{@language-info-ref[definitions-text-surrogate]}]

@section{Syntax Coloring}

@language-info-def[color-lexer]{
  When a language's @racket[_get-info] procedure responds to @racket['color-lexer], it
  is expected to return a procedure suitable to pass as the @racket[_get-token]
  argument to @method[color:text<%> start-colorer].
}

The recognized token styles (specified implicitly via @method[color:text<%> start-colorer]'s
@racket[_token-sym->style] argument) are:
@itemize[@item{@indexed-racket['symbol]}
          @item{@indexed-racket['keyword]}
          @item{@indexed-racket['comment]}
          @item{@indexed-racket['string]}
          @item{@indexed-racket['constant]}
          @item{@indexed-racket['parenthesis]}
          @item{@indexed-racket['error]}
          @item{@indexed-racket['other]}]
These precise colors for these identifiers are controlled by the preferences dialog in DrRacket.

@section{Indentation}

@language-info-def[drracket:indentation]{
 When a language's @racket[_get-info] procedure responds to @racket['drracket:indentation],
 it is expected to return a function with this contract:
 @racketblock[(-> (is-a?/c racket:text<%>)
                  exact-nonnegative-integer?
                  (or/c #f exact-nonnegative-integer?))]
 The function is used
 to indent lines in DrRacket. It is called with the position containing the line to be
 indented. It is expected to return the number of spaces that should appear at the beginning
 of the line or @racket[#f]. If @racket[#f] is returned,
 DrRacket uses the standard s-expression indentation rules.

 @history[#:added "1.3"]
}

@language-info-def[drracket:range-indentation]{
 When a language's @racket[_get-info] procedure responds to @racket['drracket:range-indentation],
 it is expected to return a function with this contract:
 @racketblock[(-> (is-a?/c racket:text<%>)
                  exact-nonnegative-integer?
                  exact-nonnegative-integer?
                  (or/c #f (listof (list/c exact-nonnegative-integer? string?))))]
 The function is used to indent a range that potentially spans multiple lines in DrRacket.
 It is called with the start and ending position of the range. The function is expected to return
 either @racket[#f] or a list with an item for each line in the range. Returning @racket[#f]
 falls back to iterating indentation over every line in the range (using @racket['drracket:indentation],
 if available). Returning a list indicates an update for each corresponding line, where
 a line update takes the form @racket[(list _delete-amount _insert-string)]:
 first delete @racket[_delete-amount] items from the start of the line, and then
 insert @racket[_insert-string] at the start of the line. If the returned list has fewer
 items then the range of lines to indent, the list is effectively padded with @racket[(list 0 "")]
 no-op items. If the list has more items than the range of lines to indent, the extra items
 are ignored. Note that returning an empty list causes no lines to be updated, as opposed to
 returning @racket[#f] to trigger a different indentation mechanism.

 When both @racket['drracket:indentation] and @racket['drracket:range-indentation]
 are available, the function for @racket['drracket:range-indentation] is called first---except
 in the case of an implicit indentation from creating a newline, in which case only
 @racket['drracket:indentation] is used.

 @history[#:added "1.10"]
}

@section{Keystrokes}

@language-info-def[drracket:keystrokes]{
  When a language's @racket[_get-info] procedure responds to
 @racket['drracket:keystrokes], it is expected
 to return a list of keybindings and callbacks matching this contract:
 @racketblock[(listof (list/c string?
                              (-> (is-a?/c text%)
                                  (is-a?/c event%)
                                  any)))]
 Each element of the list is a different keybinding, where the
 string indicates the keystroke (see the documentation for
 @method[keymap% map-function] for the precise contents of the
 string and how it maps to particular keystrokes) and the procedure
 is called when the user types that keystroke in the definitions
 window.

 The procedure's first argument will be the definitions text, the
 second will be the event object supplied from the GUI system
 and the result of the procedure is ignored.
}

@language-info-def[drracket:grouping-position]{

 When a language's @racket[_get-info] procedure responds to
 @racket['drracket:keystrokes], it is expected to return a
 function that determines where positions relevant to the
 nesting structure of the program appear. This function is
 used for a number of motion and selection operations in the
 editor, as well as determining where to flash to for closing
 parentheses.

 Specifically the result must be a function matching this
 contract:
 @racket[(-> natural?
             natural?
             (or/c 'up 'down 'backward 'forward)
             (or/c #f #t natural?))]

 Consider first the first and third argument. The first
 argument indicates a position in the editor to start from
 and the third argument is a direction to look. The result
 return the position for the corresponding direction, where
 the nesting structure of the program is viewed as a tree.
 That is, if the third argument is @racket['up], the function
 should return the position that goes up one layer in the
 tree from the given position to the parent. Similarly
 @racket['down] should return the position going on layer
 deeper into that tree, going down to the first child. The
 @racket['backward] and @racket['forward] arguments
 correspond to position where we stay at the same level in
 the tree, moving between siblings. The result should be
 @racket[#f] when there is no corresponding position to move
 to, e.g., when the current position has no children, no
 parents, or no siblings in the corresponding direction.

 The second argument is a limit. Positions smaller than the
 limit should be ignored, so if the corresponding position
 appears to be before the limit, return @racket[#f].

 Finally, return @racket[#t] to get the default behavior,
 namely motion in Racket-style s-expressions.

 @history[#:added "1.11"]
}

@section{Filename Extensions}

@language-info-def[drracket:default-filters]{
  When a language's @racket[_get-info] procedure responds to
  @racket['drracket:default-filters], it is expected to return
  @racket[(listof (list/c string? string?))].

  These results are added as a prefix to @racket[finder:default-filters],
  extending the default that DrRacket normally uses, namely:
  @racketblock[`(["Racket Sources" "*.rkt;*.scrbl;*.rktl;*.rktd;*.ss;*.scm"]
                 ["Any" "*.*"])]
  @history[#:added "1.2"]
}

@language-info-def[drracket:default-extension]{
  When a language's @racket[_get-info] procedure responds to @racket['drracket:default-extension],
  it is expected to return @racket[(and/c string? (not/c #rx"[.]"))]; the result is used
  as the default extension when saving files by setting @racket[finder:default-extension].

  @history[#:added "1.2"]
}

@section{REPL Submit Predicate}

@language-info-def[drracket:submit-predicate]{
 When using the language
 declared in the source, DrRacket queries that language via
 @racket[read-language] to determine if an expression in the
 interactions window is ready to be submitted to the
 evaluator (when the user types return). The info procedure
 is passed @racket['drracket:submit-predicate] and
 should return a function matching this contract:
@racketblock[(-> input-port?
                 boolean?
                 boolean?)]
 This function's first argument is a port that contains the
 interactions window's data, starting just after the prompt
 and continuing to the end of the editor. The second argument
 is a boolean indicating if the insertion point is followed
 only by whitespace. The results should be a boolean
 indicating if the expression should be evaluated. }

For backwards compatibility reasons, DrRacket also queries
the result of @racket[module->language-info] for
@racket['drracket:submit-predicate]. It does this during
the evaluation of the definitions (so the
@menuitem["Racket" "Reload #lang extensions"] menu item
does not trigger a re-load). If the submit predicate is
specified both ways, then the predicate supplied via
@racket[read-language] takes precedence.

@history[#:changed "1.5" @elem{Look for @racket[drracket:submit-predicate] via
          @racket[read-language].}]

@section{Show Big ``Definitions'' and ``Interactions'' Labels}

@language-info-def[drracket:show-big-defs/ints-labels]{
  If the @racket[read-language] predicate returns @racket[#t]
 for @racket['drracket:show-big-defs/ints-labels], then DrRacket
 shows the words “Definitions” and “Interactions” in a large
 font in the corresponding windows. This is intended as a help
 for students who are reading instructions about where to type
 their programs but might not have internalized this particular
 bit of DrRacket terminology.
}

@section{Opting Out of Standard Toolbar Buttons}

@language-info-def[drracket:opt-out-toolbar-buttons]{
 Some of the built-in buttons in the DrRacket button bar at the top of the
 window can be disabled on a per-language basis.
 DrRacket will invoke the @racket[_get-info] proc returned by @racket[read-language] with
 @racket['drracket:opt-out-toolbar-buttons]
 (and @indexed-racket['drscheme:opt-out-toolbar-buttons] for backwards compatibility).

 If the result is a list of symbols, the
 listed symbols are opted out. If the result is @racket[#f], all buttons are opted
 out. The default is the empty list, meaning that all opt-out buttons appear.

 The Check Syntax button uses the symbol @racket['drracket:syncheck]; the debugger
 uses the symbol @racket['debug-tool] and the macro stepper uses @racket['macro-stepper].

 Plugins may add more opt-out buttons via
 @racket[drracket:module-language-tools:add-opt-out-toolbar-button].

}

@section{Opting In to Language-Specific Toolbar Buttons}

@language-info-def[drracket:opt-in-toolbar-buttons]{
 Like @language-info-ref[drracket:opt-out-toolbar-buttons], but for languages to opt in to
 buttons that are not enabled by default.

 Plugins may add more opt-out buttons via
 @racket[drracket:module-language-tools:add-opt-in-toolbar-button].
 @history[#:added "1.6"]
}

@section{Adding New Toolbar Buttons}

@language-info-def[drracket:toolbar-buttons]{
DrRacket queries the result of @racket[read-language] 
to determine if there are any new toolbar
buttons to be used when editing files in this language.
}

Specifically, DrRacket will pass @racket['drracket:toolbar-buttons]
to the function and expect back a value matching this contract:
@racketblock[(or/c (listof (list/c string?
                                   (is-a?/c bitmap%)
                                   (-> (is-a?/c drracket:unit:frame<%>) any)
                                   (or/c real? #f)))
                   #f)]
which is then used to create new toolbar buttons, one for each element
in the result list. The string is the label on the button; the bitmap is the icon 
(it should be 16x16 pixels); the function is called when the button is clicked;
and the number is passed as the @racket[#:number] argument to
@method[drracket:unit:frame<%> register-toolbar-button].

If the result is @racket[#f], then no toolbar buttons are created.

To implement functionality similar to the Run button, call the
@method[drracket:unit:frame% execute-callback] method. You may also
want to use the @racket[drracket:rep:after-expression] parameter.

If @racket['drracket:toolbar-buttons] is not recognized, DrRacket will also
pass @indexed-racket['drscheme:toolbar-buttons]; this is for backwards
compatibility and new code should not use it.
Similarly, if the fourth element from the list (the argument to @racket[#:number])
is not present, then it is treated as @racket[#f].

@section[#:tag "sec:definitions-text-surrogate"]{Definitions Text Surrogate}

Using a @tt{#lang}-specific definitions text surrogate is a
very powerful way to flexibly control DrRacket's behavior
when a new language is installed. It is also easy to cause
DrRacket to completely misbehave with this form of
extension. It is here only when one of the other forms of
extension listed above are not sufficient for the kind of
extension your language requires. And even in that case, it
is preferable to add something to this list that is more
easily controlled in the case of errors, using the
definitions text surrogate only until that more easily
controlled extension has been added to DrRacket.

@language-info-def[definitions-text-surrogate]{ DrRacket
 calls @racket[read-language]'s @racket[get-info] procedure
 with @racket['definitions-text-surrogate] and expects it to
 return a value matching the contract
 @racket[(or/c #f module-path?)], which is then passed to
 @racket[dynamic-require] together with @racket['surrogate%].
 The result is expected to be a class implementing the
 interface @racket[racket:text-mode<%>] (presumably
 derived from @racket[racket:text-mode%]. That mode is
 installed into the definitions text, where it can change its
 behavior by changing how is responds to any of the methods
 in the mode. }

One consequence of this power is that errors that happen
during the dynamic extent of calls into the mode are not
trapped (much as errors that occur on newly created threads
are not trapped, as described in the introduction to this
section).
