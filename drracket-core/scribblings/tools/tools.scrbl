#lang scribble/doc
@(begin
   (require scribble/manual
            "common.rkt"
            scribble/racket
            (for-label racket/syntax)
            (for-syntax racket/base
                        "example-src.rkt"))
   
   (define (File x) @tt[x])
   (define (FileFirst x) @tt[x]) ;; indexing missing
   
   (define-syntax-rule (item/cap x . ys)
     (item (indexed-racket x) ": " . ys)))

@title{DrRacket Plugins}

@author["Robert Bruce Findler"]

@defmodule*[(drracket/tool-lib drracket/tool drscheme/tool-lib drscheme/tool)]

This manual describes DrRacket's plugins interface. It assumes
familiarity with 
Racket, as described in the
@(other-manual '(lib "scribblings/guide/guide.scrbl")),
and the
@(other-manual '(lib "scribblings/reference/reference.scrbl")),
DrRacket, as described in
@(other-manual '(lib "scribblings/drracket/drracket.scrbl"))
and the GUI library, as described in
@(other-manual '(lib "scribblings/gui/gui.scrbl")).
The Framework, as described in
@(other-manual '(lib "scribblings/framework/framework.scrbl")),
may also come in handy.

The @racketmodname[drscheme/tool-lib] and @racketmodname[drscheme/tool]
libraries are for backward compatibility; they export all of the bindings of
their @tt{drracket} counterpart.

@table-of-contents[]

@bold{Thanks}

Thanks to PLT and the early adopters of the 
tools interface for
their feedback and help.

A special thanks to
Eli Barzilay, 
John Clements, 
Matthias Felleisen,
Cormac Flanagan,
Matthew Flatt, 
Max Hailperin, 
Philippe Meunier, and
Christian Queinnec for their
help being early clients for DrRacket plugins.

@include-section["lang-tools.scrbl"]

@section[#:tag "implementing-tools"]{Implementing DrRacket Plugins}

Plugins are designed for major extensions in DrRacket's
functionality.  To extend the appearance
or the functionality the DrRacket window (say, to annotate
programs in certain ways or to add buttons to the DrRacket
frame) use a
tool. The Macro Stepper, the Syntax Checker, the Stepper,
and the teaching languages are all implemented as tools.

When DrRacket starts up, it looks for tools by reading
fields in the @File{info.rkt} file of each collection and the
newest version of each PLaneT package installed on the
system.  (Technically, DrRacket looks in a cache of the
@filepath{info.rkt} files contents created by @tt{raco setup}. Be sure to
re-run @tt{raco setup} if you change the contents of
the @File{info.rkt} files).  DrRacket checks for these
fields:
@itemize[
@item/cap[drracket-tools]{
  @racket[(listof (listof string[subcollection-name]))]
}
@item/cap[drracket-tool-names]{@racket[(listof (or/c #f string))]}
@item/cap[drracket-tool-icons]{
@racketblock[(listof (or/c #f
                           string[relative-pathname] 
                           (cons string[filename] 
                                 (listof string[collection-name]))))]
}
@item/cap[drracket-tool-urls]{
@racket[(listof (or/c #f string[url]))]
}]

The @racket[drracket-tools] field names a list of tools in this
collection. Each tool is specified as a collection path,
relative to the collection where the @File{info.rkt} file
resides. As an example, if there is only one tool named
@File{tool.rkt}, this suffices:
@racketblock[
(define drracket-tools (list (list "tool.rkt")))
]
If the @racket[drracket-tool-icons] or @racket[drracket-tool-names] fields are
present, they must be the same length as @racket[drracket-tools]. The
@racket[drracket-tool-icons] field specifies the path to an icon for each
tool and the name of each tool. If it is @racket[#f], no
tool is shown. If it is a relative pathname, it must refer
to a bitmap and if it is a list of strings, it is treated
the same as the arguments to @racket[lib], inside
@racket[require].

This bitmap and the name show up in the about box, the
bug report form, and the splash screen as the tool is
loaded at DrRacket's startup.

Each of the @racket[drracket-tools] files must contain a module that
@racket[provide]s @racket[tool@], which must be bound to a
@racket[unit]. The unit
must import the @racket[drracket:tool^] signature, which is
provided by the @racketmodname[drracket/tool] library. 
The @as-index{@racket[drracket:tool^]}
signature contains all of the names listed in this manual.
The unit must export the @racket[drracket:tool-exports^]
signature. 

If the tool raises an error as it is loaded, invoked, or as
the @sigelem[drracket:tool-exports^ phase1] or
@sigelem[drracket:tool-exports^ phase2] thunks are called,
DrRacket catches the error and displays a message box. Then,
DrRacket continues to start up, without the tool.

For example, if the @File{info.rkt} file in a collection
contains:
@racketmod[
info
(define drracket-tool-names (list "Tool Name"))
(define drracket-tools (list (list "tool.rkt")))
]
then the same collection would be expected to contain a
@File{tool.rkt} file. It might contain something like this:
@racketmod[
racket/gui
(require drracket/tool)

(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define (phase1) (message-box "tool example" "phase1"))
    (define (phase2) (message-box "tool example" "phase2"))
    (message-box "tool example" "unit invoked")))
]
This tool just opens a few windows to indicate that it has
been loaded and that the @sigelem[drracket:tool-exports^ phase1]
and @sigelem[drracket:tool-exports^ phase2]
functions have been called.

Finally, here is a more involved example. This 
module defines a plugin that adds a button to the DrRacket
frame that, when clicked, reverses the contents of the definitions
window. It also adds an easter egg. Whenever the definitions text is
modified, it checks to see if the definitions text contains the
text ``egg''. If so, it adds ``easter '' just before.

@(let ()
 
   (define-syntax-rule (define-linked-method name interface)
     (define-syntax name 
       (make-element-id-transformer
        (lambda (stx)
          #'(method interface name)))))
   
   (define-linked-method begin-edit-sequence editor<%>)
   (define-linked-method end-edit-sequence editor<%>)
   (define-linked-method find-first-snip editor<%>)
   (define-linked-method on-insert text%)
   (define-linked-method on-delete text%)
   (define-linked-method after-insert text%)
   (define-linked-method after-delete text%)
   
   (define-linked-method insert text%)
   (define-linked-method get-text text%)
   (define-linked-method split-snip text%)
   
   (define-linked-method next snip%)
   (define-linked-method release-from-owner snip%)
   
   (define-linked-method change-children area-container<%>)
   
   (define-linked-method get-button-panel drracket:unit:frame%)
   (define-linked-method register-toolbar-button drracket:unit:frame<%>)
   (define-linked-method get-definitions-text drracket:unit:frame<%>)
   
   (define-linked-method erase dc<%>)
   (define-linked-method set-smoothing dc<%>)
   (define-linked-method set-pen dc<%>)
   (define-linked-method set-brush dc<%>)
   (define-linked-method draw-ellipse dc<%>)
   (define-linked-method set-bitmap bdc%)
   
   (define-syntax (get-src stx)
     (define file (list-ref files 1))
     #`(racketmod
        #,@(let loop ([sw (list-ref file 1)])
             (cond
               [(src-wrap? sw)
                (datum->syntax #'here
                               (loop (src-wrap-obj sw))
                               (src-wrap-srcloc sw))]
               [(pair? sw)
                (cons (loop (car sw)) (loop (cdr sw)))]
               [else
                sw]))))
   
   (get-src))



@section[#:tag "adding-languages"]{Adding Languages to DrRacket}
@index{adding languages to DrRacket}

@subsection{Adding Module-based Languages to DrRacket}

For backwards compatibility, DrRacket also supports
and 
@File{info.rkt} file-based method for specifying
such languages. Include these definitions:
@itemize[
@item/cap[drscheme-language-modules]{
  This must be bound to a
  list of collection path specifications or strings, one for
  each language in the collection. Each collection path
  specification is the quoted form of what might appear as
  an argument to @racket[require], using the
  @tt{lib} argument (but without the @tt{lib}). The
  strings represent relative paths starting at the directory
  containing the @File{info.rkt} file.  They are interpreted
  like string arguments to @racket[require].
}
@item/cap[drscheme-language-positions]{
This must be bound to a
  list of language positions. Each language position
  corresponds to the position of the language in language
  dialog. Each language position is a list of strings whose
  length must be at least two.
  
  If the first string is the same as 
  @racket[(string-constant teaching-languages)], then
  it is put into the ``Teaching Languages'' section
  of the dialog. Otherwise, it goes into the ``Other Languages''
  section of the dialog.
}
@item/cap[get-drscheme-language-positions]{
This must be bound to a list that contains a module path followed
  by a symbol. The module path and symbol are combined with
  @racket[dynamic-require] to obtain a list that is appended
  to the one from @racket[drscheme-language-positions], which
  allows access to @racketmodname[string-constants] to specify
  language positions.
}
@item/cap[drscheme-language-numbers]{
This is optional. If
  present, it must be a list of a list of numbers. Each list
  corresponds to a single language from this collection.
  Each number indicates a sorting order in the language
  dialog for the corresponding string in 
  @racket[drscheme-language-positions]. If absent, it defaults
  to a list of zeros that has the same length as
  @racket[drscheme-language-positions]. This will rarely be correct.
}
  
@item/cap[drscheme-language-one-line-summaries]{
This is
  optional. If present, it must be a list of strings. Each
  string is displayed at the bottom of the language dialog
  when the corresponding language is selected.
}
@item/cap[drscheme-language-urls]{
This is
  optional. If present, it must be a list whose elements are
  either strings or @racket[#f].
  Clicking the corresponding language's name in
  the interactions window opens a web browser to the url.
}
@item/cap[drscheme-language-readers]{
This is optional. If
  present, it must be bound to a quoted list of module
  specifications (that is, a quoted version of the argument
  to @racket[require]). Each
  specification must be a module that exports a function
  named @racket[read-syntax].  Each of these
  @racket[read-syntax] functions must match Racket's
  @racket[read-syntax] primitive's contract, but may
  read different concrete syntax.

  If the module specification is a plain string, it 
  represents a relative path starting at the directory
  containing the @File{info.rkt} file.  It is interpreted
  like the string arguments to @racket[require].
}]
The lists must have the same length.

As an example, the @italic{Essentials of Programming Languages}
language specification's @File{info.rkt} used to look like this:
@racketmod[
info
(require string-constants)
(define name "EoPL Support")
(define drscheme-language-modules
  (list "eopl-lang.rkt"))
(define drscheme-language-positions
  (list (list (string-constant teaching-languages)
              "Essentials of Programming Languages")))
]
This @File{info.rkt} file indicates that there is a single
language in this collection. The module that implements the
language is the @File{eopl-lang.rkt} file in the same directory as
the @File{info.rkt} file. Additionally, the language dialog will contain
@tt{Essentials of Programming Languages} as a potential
language. The use of the string constant
@racket[teaching-languages] ensures that EoPL's language is
placed properly in foreign language versions of DrRacket.

For collections that define multiple (related) languages, if
the language-positions contain multiple strings, the
languages whose leading strings match are grouped together.
That is, if two languages have strings:
@racketblock[
  '("My Text" "First Language")
]
and
@racketblock[
  '("My Text" "Second Language")
]
the two languages will be grouped together in the language
dialog.

@subsection{Adding Arbitrary Languages to DrRacket}
With some additional work, any language that can be compiled
to Racket is supported by the tools interface,
not just those that use standard configurations and
@racket[module].

Each language is a class that implement the
@racket[drracket:language:language<%>] interface.  DrRacket also
  provides two simpler interfaces:
  @racket[drracket:language:module-based-language<%>] and
  @racket[drracket:language:simple-module-based-language<%>],
  and 
  @racket[mixin]s
  @racket[drracket:language:simple-module-based-language->module-based-language-mixin]
  and
  @racket[drracket:language:module-based-language->language-mixin]
  that build implementations of @racket[drracket:language:language<%>]s from these simpler interfaces.

Once you have an implementation of the
@racket[drracket:language:language<%>] interface, call
@racket[drracket:language-configuration:add-language] to add the language
to DrRacket.

Each language comes with its own type, called
@tt{settings}. This can be any type the language
designer chooses, but to aid documentation, we call it
@tt{settings} here. The settings type is expected to
contain parameters of the language, such as case
sensitivity, etc. The implementor of the language provides a
GUI so the user can configure the settings and all of the
language's operations accept a setting. DrRacket maintains
the current settings for each language.

@subsection{Language Extensions}

Some tools may require additional functionality from the
@racket[drracket:language:language<%>] interface. The
@racket[drracket:language:extend-language-interface]
function and the
@racket[drracket:language:get-default-mixin]
mixin make this possible.

For example, the MrFlow tool expands a program, analyzes it
and then displays sets of values for each program point.
These sets of values should be rendered in the syntax of the
language that MrFlow analyzes. Since MrFlow doesn't 
know which languages are available, it can call
@racket[drracket:language:extend-language-interface]
to extend the @racket[drracket:language:language<%>]
interface with a method for rendering sets of values and
provide a default implementation of that method. Tools that
know about MrFlow can then override the value rendering
method to provide a language-specific implementation of
value rendering.  Additionally, since the
@racket[drracket:language:get-default-mixin]
adds the default implementation for the value-set rendering
method, all languages at least have some form of value-set
rendering.

In some cases, it is important for one tool to avoid
depending on another in the manner above. For example, if a
tool that provides a new language provides an implementation
for the MrFlow-specific method, that tool may fail to load
if MrFlow is not present (Indeed, with the tool manager,
this can happen to any tool that depends on another in this
manner.)

To avoid this problem, consider writing your tool to first
check to see if the base method is available before
extending it. For example, if the MrFlow tool provides the
@tt{render-value<%>} interface, then a tool that overrides
that method can first test to see if the superclass
implements that method before overriding it:
@racketblock[
(define (my-language-mixin %)
  (if (implementation? % mrflow:render-value<%>)
      (class % 
        (define/override ...)
        (super-new))
      %))
]

To help test your tool, use the 
@seclink["environment-variables" #:doc '(lib "scribblings/drracket/drracket.scrbl")]{@tt{PLTONLYTOOL}}
environment variable to load it in isolation.

@section{Creating New Kinds of DrRacket Frames}

Each frame in DrRacket has certain menus and functionality,
most of which is achieved by using the framework.
Additionally, there is one mixin that DrRacket provides to
augment that. It is @racket[drracket:frame:basics-mixin].
Be sure to mix it into any new frame class that you add to
DrRacket.

@section{Extending the Existing DrRacket Classes}

Each of the names:
@itemize[
@item{@racket[drracket:get/extend:extend-interactions-text]}
@item{@racket[drracket:get/extend:extend-definitions-text]}
@item{@racket[drracket:get/extend:extend-interactions-canvas]}
@item{@racket[drracket:get/extend:extend-definitions-canvas]}
@item{@racket[drracket:get/extend:extend-unit-frame]}
@item{@racket[drracket:get/extend:extend-tab]}]
is bound to an extender function. In order to change the
behavior of DrRacket, you can derive new classes from the
standard classes for the frame, texts, canvases. Each
extender accepts a function as input. The function it
accepts must take a class as its argument and return a
classes derived from that class as its result. For example:

@racketblock[
(drracket:get/extend:extend-interactions-text
  (lambda (super%)
    (class super%
      (define/public (method1 x) ...)
      (super-new))))
]
extends the interactions text class with a method named @tt{method1}.

@section[#:tag "Expanding and Breaking"]{Expanding the User's Program Text and Breaking}
@index{expanding user programs}
@index{breaking}
@index{break button}

Macro-expanding a program may involve arbitrary computation
and requires the setup of the correct language. To aid this,
DrRacket's tool interface provides
@racket[drracket:eval:expand-program] to help. Use
this method to extract the fully expanded program text in a
particular language.

Because expanding the user's program may require DrRacket to
evaluate arbitrary code that the user wrote, tools that
expand the user's program should also allow the user to break
the expansion. To help with this, the tools interfaces
provides these methods:
@method[drracket:rep:context<%> enable-evaluation]
and
@method[drracket:rep:context<%> disable-evaluation].
Since your tool will be expanding the program text, you
should be both overriding 
@method[drracket:rep:context<%> enable-evaluation]
and
@method[drracket:rep:context<%> disable-evaluation]
to disable your tool and calling them
to ensure that only one expansion is happening
at a time.

Finally, DrRacket provides the
@method[drracket:rep:context<%> set-breakables]
method. This method controls what behavior the Break button
has.

@section{Editor Modes}
@index{modes}
@index{scheme mode}
@index{racket mode}

@subsection{Color Schemes}

DrRacket uses the framework's color schemes to colorize
source text and other aspects of itself. See 
@racket[color-prefs:register-info-based-color-schemes] for
details on how to add new color schemes via @filepath{info.rkt}
files.

@subsection{General-purpose Modes}

Plugins can register modes via
@racket[drracket:modes:add-mode]. Each mode is visible in
the @onscreen{Modes} submenu of the @onscreen{Edit} menu.
Initially, DrRacket only supports two modes: Racket mode and
text mode. (The Racket mode consults the language in the
@tt{#lang} line; see
@secref["sec:definitions-text-surrogate"] for more details.)

DrRacket automatically selects a mode for each open
file based on the file's extension (and the language chosen
as described above). If the file ends with
@File{.txt}, DrRacket uses text mode. Otherwise, DrRacket
uses Racket mode.

@section{Plugin Capabilities}

DrRacket's capability interface provides a mechanism for
tools to allow languages to hide their GUI interface, if the
tool does not apply to the language. Tools register
capabilities keyed with symbols via.
@racket[drracket:language:register-capability]. Once
registered, a tool can query a language, via the 
@method[drracket:language:language<%> capability-value]
method. The result from this method controls whether or not
the tool shows this part of the GUI for DrRacket.

See @racket[drracket:language:register-capability]
for a list of the capabilities registered by default.

@section{Check Syntax}

Check Syntax is a part of the DrRacket collection, but is implemented via the 
plugin API. See also @racketmodname[drracket/check-syntax #:indirect].

@subsection{Check Syntax Button}

@defmodule[drracket/syncheck-drracket-button]

@defthing[syncheck-drracket-button
          (list/c 
           string?
           (is-a?/c bitmap%)
           (-> (is-a?/c
                top-level-window<%>)
               any))]{
   This is meant to be used with the @racket['drracket:toolbar-buttons] 
   argument to the info proc returned
   from @racket[read-language].
}

@defidform[syncheck:button-callback]{
  This is defined with @racket[define-local-member-name] and
  is bound to a method of no arguments of the DrRacket frame that runs Check
  Syntax.
}
          
@defthing[syncheck-bitmap (is-a?/c bitmap%)]{
  The bitmap in the Check Syntax button on the DrRacket frame.
}

@subsection{Syntax Properties that Check Syntax Looks For}

Check Syntax collects the values of the 
@indexed-racket[syntax-property]s named
@indexed-racket['disappeared-use],
@indexed-racket['disappeared-binding],
@indexed-racket['identifiers-as-disappeared-uses?],
@indexed-racket['identifier-as-keyword],
@indexed-racket['sub-range-binders], and
@indexed-racket['mouse-over-tooltips] and uses them to add
control which arrows are added to the program text. These properties are
intended for use when a macro discards or manufactures identifiers that,
from the programmers perspective, should be binding each other, or when
there are identifiers that are intended to be used more in the spirit of keywords,
and thus should be ignored.

For example, here is program with a macro that discards its arguments, but
adds properties to the result syntax object so that the two occurrences of @racket[_a]
are treated as a binding/bound pair by Check Syntax.

@racketblock[
  (define-syntax (m stx)
    (syntax-case stx ()
      [(_ id1 id2)
       (and (identifier? #'id1) (identifier? #'id2))
       (syntax-property
        (syntax-property
         #'1
         'disappeared-use (list (syntax-local-introduce #'id1)))
        'disappeared-binding (list (syntax-local-introduce #'id2)))]))

  (m a a)]

Check Syntax draws arrows only between identifiers that are
@racket[free-identifier=?]. They must be
@racket[syntax-original?] or have the
@racket[syntax-property]
@indexed-racket['original-for-check-syntax] set to
@racket[#t]. See also
@racket[current-recorded-disappeared-uses].

Another approach for situations where identifiers are discarded by a
macro is to introduce a @racket[let] expression that doesn't contribute
to the result of the computation, but does signal to Check Syntax that
there are some arrows to draw. For example, Check Syntax is unable to
draw the arrows between the introductions and uses of @racket[a], @racket[b],
and @racket[c] for this code:
@codeblock{
#lang racket
(require (for-syntax syntax/parse))

(define-syntax (depths stx)
  (syntax-parse stx
    [(_ id expr)
     (define table (make-hash))
     (let loop ([stx #'expr] [depth 0])
       (cond
         [(syntax->list stx)
          =>
          (λ (lst)
            (for ([ele (in-list lst)])
              (loop ele (+ depth 1))))]
         [(identifier? stx)
          (hash-set! table (syntax-e stx) depth)]))
     #`(define-syntax id #,table)]))

(define-syntax (depth-of stx)
  (syntax-parse stx
    [(_ id1 id2)
     (datum->syntax
      #'here
      (hash-ref (syntax-local-value #'id1)
                (syntax-e #'id2)))]))

(depths my-sexp ((a) b (((((((c)))))))))

(depth-of my-sexp a)
(depth-of my-sexp b)
(depth-of my-sexp c)
}

Extending these macro to cooperate with Check syntax
requires more information to be collected on the definition
side and then used at the use side:

@codeblock{
#lang racket
(require (for-syntax syntax/parse))

(define-syntax (depths stx)
  (syntax-parse stx
    [(_ id expr)
     (define table (make-hash))
     (let loop ([stx #'expr] [depth 0])
       (cond
         [(syntax->list stx)
          =>
          (λ (lst)
            (for ([ele (in-list lst)])
              (loop ele (+ depth 1))))]
         [(identifier? stx)
          (hash-set! table (syntax-e stx)
                     (cons (vector (syntax-source stx)
                                   (syntax-line stx)
                                   (syntax-column stx)
                                   (syntax-position stx)
                                   (syntax-span stx))
                           depth))]))
     #`(define-syntax id #,table)]))

(define-syntax (depth-of stx)
  (syntax-parse stx
    [(_ id1 id2)
     (define pr
       (hash-ref (syntax-local-value #'id1)
                 (syntax-e #'id2)))
     (define fake-binder
       (datum->syntax #'id2 (syntax-e #'id2) (car pr) #'id2))
     #`(begin
         (void (let ([#,fake-binder 1]) id2))
         #,(cdr pr))]))

(depths my-sexp ((a) b (((((((c)))))))))

(depth-of my-sexp a)
(depth-of my-sexp b)
(depth-of my-sexp c)
}

For each syntax object that appears in the fully expanded
program, DrRacket traverses it looking for identifiers and
connecting them to likely binding occurrences. When it finds
such identifiers it draws an arrow with a large question
mark near the head of the arrow. But, if the syntax object
has the property @racket['identifiers-as-disappeared-uses?],
then the arrows are the normal arrow color.

The value of the @racket['sub-range-binders] property is expected
to be a tree of @racket[cons] pairs (in any configuration) whose leaves
are either ignored or are vectors with either of these shapes:
@racketblock[(or/c (vector/c identifier? natural? natural?
                             identifier? natural? natural?)

                   (vector/c identifier?
                             natural? natural?
                             (real-in 0 1) (real-in 0 1)

                             identifier?
                             natural? natural?
                             (real-in 0 1) (real-in 0 1)))]

Each vector is interpreted as a single arrow. The first
identifier in the vector is the start of the arrow and the
second identifier in the vector is the destination of the
arrow. The two natural numbers that follow each identifier adjust
the precise starting and ending ranges for the arrows,
however. They are interpreted as offsets into the position of
each corresponding identifier, making the arrows start and
end on just a portion of the identifier, instead of the
entire identifier.

If the vector has 8 elements, then the two real numbers are
treated as the precise location where the arrow starts and
ends, inside the rectangle that corresponds to the start and
end of the identifier. The first real number is for the x
direction and the second one is for the y direction. For
example, if some identifier has a position and span of 100
and 10, and the offset are 1 and 5, then the rectangle that
bounds the corresponding end of the arrow would be from
position 101 to 105. This entire range gets highlighted when
the mouse moves over it. The arrow itself, however, will
start from some specific point inside that editor range,
normally in the center and corresponds to the situation
where the two real numbers are both @racket[0.5]. If,
however the two reals are both @racket[1/3], then the arrow
will start one third of the way from the top to the bottom
and one third of the way from the left to the right.

The property is looked for in expression positions and on
binding identifiers.

Here's an example:

@codeblock{#lang racket/base
           (require (for-syntax racket/base))
           (define-syntax (define/hyphen stx)
             (syntax-case stx ()
               [(_ id1 id2 rhs-expr)
                (let ()
                  (define first-part (symbol->string (syntax-e #'id1)))
                  (define second-part (symbol->string (syntax-e #'id2)))
                  (define first-len (string-length first-part))
                  (define second-len (string-length second-part))
                  (define hyphenated-id 
                    (datum->syntax
                     #'id1
                     (string->symbol (string-append first-part "-" second-part))))
                  (syntax-property
                   #`(define #,hyphenated-id rhs-expr)
                   'sub-range-binders
                   (list
                    (vector (syntax-local-introduce hyphenated-id)
                            0 first-len 0.5 0.5
                            (syntax-local-introduce #'id1)
                            0 first-len 0.5 0.5)
                    (vector (syntax-local-introduce hyphenated-id)
                            (+ first-len 1) second-len 0.5 0
                            (syntax-local-introduce #'id2)
                            0 second-len 0.5 1))))]))
           
           (define/hyphen big generator
             11)
           
           (+ big-generator big-generator)}

After putting this code in the DrRacket window, mouse over the words ``big'' and 
``generator'' to see arrows pointing to the individual pieces of the identifier
@racket[_big-generator]. The four @racket[.5]s in the first vector put the arrows
on @racket[_big] in the center of the identifiers; the @racket[.5 0] and the
@racket[.5 1] in the second vector put the arrows at the top and bottom
center for @racket[_generator].


The value of the @racket['mouse-over-tooltips] property is expected to be 
to be a tree of @racket[cons] pairs (in any configuration) whose leaves
are either ignored or are vectors of the shape
@racketblock[(vector/c syntax?
                       exact-nonnegative-integer?
                       exact-nonnegative-integer?
                       (or/c string? (-> string?)))]
Each vector's content indicates where to show a tooltip. The first three components are
a syntax object whose @racket[syntax-source] field indicates which file the tooltip goes in,
the start and end position in the editor where mouseovers will show the tooltip, 
and the content of the tooltip. Note that editor positions count from zero, while syntax
object positions count from one, so use @racket[sub1] to convert between them. If the
tooltip content is a procedure, this procedure is called by Check Syntax to compute the
string used for the tooltip, as Check Syntax traverses the syntax objects looking for
properties.

For example, here's a macro that shows the span of itself in a tooltip on mouseover:
@codeblock{
#lang racket
(define-syntax (char-span stx)
  (syntax-case stx ()
    [(_ a)
     (syntax-property
      #'a
      'mouse-over-tooltips
      (vector
       stx
       (sub1 (syntax-position stx))
       (sub1 (+ (syntax-position stx)
             (syntax-span stx)))
       (format "this expression\nspans ~a chars"
               (syntax-span stx))))]))

(char-span (+ 1 2))}

If the syntax property @racket['identifier-as-keyword] is any value
except @racket[#f] and appears on an identifier, then Check Syntax
ignores the identifier, not drawing any arrows to it.

@history[#:changed "1.3" @list{
          Looks for @racket['sub-range-binders]
          on binding identifiers (not just in expression positions).}
         #:changed "1.5" @list{
          Looks for @racket['identifier-as-keyword] on identifiers.}]

@section{Cooperating with Background Check Syntax}
@section-index["online-check-syntax logger"]

DrRacket's continuous, background check syntax runs each time 
an edit to the definitions text happens. In some cases, that
expansion process fails, but there is still a well-formed syntax
object that check syntax can use to display information to the user.
In order to communicate that syntax object to check syntax, 
send a log message with the name @racket['online-check-syntax],
e.g.
@racketblock[(define-logger online-check-syntax)
             (log-message online-check-syntax-logger
                          'info
                          "ignored message"
                          list-of-syntax-objects)]
The fourth argument to @racket[log-message] should be a list 
of syntax objects; these are processed as if they were the result
of expansion. 

The syntax objects whose @racket[syntax-source] field does
not match the source of the file that is currently being
expanded are ignored. That is, sometimes a macro may log a
syntax object to be used by DrRacket in this fashion, but
the macro may not be from the file that DrRacket's
expanding, but one from one that is required by it; hence
this check is in place to skip them.

Note: the identifiers in these objects should be @racket[syntax-original?]
or else they will be ignored by check syntax.

@section{Teaching Languages}

The teaching language are implemented via the tools interface and thus
not part of DrRacket proper, but one helper library is documented here.

@defmodule[lang/htdp-langs-save-file-prefix]

@defthing[htdp-save-file-prefix (listof string?)]{
  These strings are used as the prefix in a file saved while using the teaching
  languages. Each string is on a separate line in the saved file.
}
@defproc[(htdp-file-prefix? [ip input-port?]) boolean?]{
  Determines if the contents of @racket[ip] is one of the possible prefixes that
  DrRacket saves at the beginning of a teaching language file.
  
  In the case that this function returns @racket[#t], it consumes the entire prefix
  from @racket[ip] (and discards it). In the case that this function returns
  @racket[#f], it does not consume anything from @racket[ip].
}

@section{Signatures}

@defsignature[drracket:tool^ ()]{
  This signature includes all of the names in this manual that begin
  with @tt{drracket:} (except these two signatures).
}
@defsignature[drracket:tool-exports^ ()]{
The @racket[drracket:tool-exports^] signature contains two
names: @sigelem[drracket:tool-exports^ phase1] and 
@sigelem[drracket:tool-exports^ phase2]. 
After all of the tools are loaded, all of
the @tt{phase1} functions are called and then all of the
@tt{phase2} functions are called. Certain primitives can
only be called during the dynamic extent of those calls.

This mechanism is designed to support DrRacket's
@racket[drracket:language:language<%>] extension
capabilities. That is, this mechanism enables two tools to
cooperate via new capabilities of languages. The first phase
is used for adding functionality that each language must
support and the second is used for creating instances of
languages. As an example, a tool may require certain
specialized language-specific information. It uses phase1 to
extend the @racket[drracket:language:language<%>] interface
and supply a default implementation of the interface
extension. Then, other languages that are aware of the
extension can supply non-default implementations of the
additional functionality.

@defproc[(phase1) void?]{
These functions can be called only in the dynamic extent
of a call to @sigelem[drracket:tool-exports^ phase1] (see
above for details).
@itemize[
@item{@racket[drracket:language:extend-language-interface]}
@item{@racket[drracket:unit:add-to-program-editor-mixin]}
]
}

@defproc[(phase2) void?]{
These functions can be called only in the dynamic extent
of a call to @sigelem[drracket:tool-exports^ phase2] (see
above for details).
@itemize[
@item{@racket[drracket:language-configuration:add-language]}
@item{@racket[drracket:language:get-default-mixin]}
@item{@racket[drracket:language:get-language-extensions]}
]
}
}

@include-section["get-slash-extend.scrbl"]
@include-section["unit.scrbl"]
@include-section["language.scrbl"]
@include-section["language-configuration.scrbl"]
@include-section["debug.scrbl"]
@include-section["rep.scrbl"]
@include-section["frame.scrbl"]
@include-section["help-desk.scrbl"]
@include-section["eval.scrbl"]
@include-section["modes.scrbl"]
@include-section["module-language-tools.scrbl"]
@include-section["module-language.scrbl"]
@include-section["tracing.scrbl"]
@include-section["init.scrbl"]

@section{Backwards Compatibility}

This section lists the bindings that begin with @tt{drscheme:} provided by the tools
library; they are here for backwards compatibility and to provide links to the
@tt{drracket:} versions of the names.

@(require drracket/private/drsig
          (for-syntax racket/base
                      racket/unit-exptime))
@(define-syntax (drs-compat stx)
   (let-values ([(drs-parent drs-vars drs-var-defs-in-sig drs-stx-defs-in-sig)
                 (signature-members #'drscheme:tool-cm^ #'here)]
                [(drr-parent drr-vars drr-var-defs-in-sig drr-stx-defs-in-sig)
                 (signature-members #'drracket:tool-cm^ #'here)])
     (with-syntax ([(drs-id ...) (append '(drscheme:tool^ drscheme:tool-exports^) drs-vars)]
                   [(drr-id ...) (append '(drracket:tool^ drracket:tool-exports^) drr-vars)])
       #'(begin
           (defthing drs-id any/c
             "This is provided for backwards compatibility; new code should use "
             (racket drr-id) " instead.") 
           ...))))
@drs-compat[]

@(tools-include/drs "debug")
@(tools-include/drs "eval")
@(tools-include/drs "frame")
@(tools-include/drs "get/extend")
@(tools-include/drs "help-desk")
@(tools-include/drs "language-configuration")
@(tools-include/drs "language")
@(tools-include/drs "modes")
@(tools-include/drs "module-language-tools")
@(tools-include/drs "module-language")
@(tools-include/drs "rep")
@(tools-include/drs "unit")
@(tools-include/drs "tracing")
@(tools-include/drs "init")

@index-section[]
