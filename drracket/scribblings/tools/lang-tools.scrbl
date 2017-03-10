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
@menuitem["Racket" "Reload #lang extensions"] menu item to
cause DrRacket to remove the extensions and reload the
implementations from the files on disk.

DrRacket calls the language's @racket[read-language]'s
@racket[_get-info] procedure with the following
@racket[_key] arguments:

@itemize[@item{@language-info-ref[drracket:default-filters]}
         @item{@language-info-ref[drracket:default-extension]}
         @item{@language-info-ref[drracket:indentation]}
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

@section{Show big “Definitions” and “Interactions” labels}

@language-info-def[drracket:show-big-defs/ints-labels]{
  If the @racket[read-language] predicate returns @racket[#t]
 for @racket['drracket:show-big-defs/ints-labels], then DrRacket
 shows the words “Definitions” and “Interactions” in a large
 font in the corresponding windows. This is intended as a help
 for students who are reading instructions about where to type
 their programs but might not have internalized this particular
 bit of DrRacket terminology.
}

@section{Opting out of Standard Toolbar Buttons}

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

@section{Opting in to Language-Specific Toolbar Buttons}

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
