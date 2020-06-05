#lang scribble/doc
@(require "common.rkt"
          (for-label compiler/cm setup/parallel-build racket/promise))

@(define racodoc '(lib "scribblings/raco/raco.scrbl"))

@title[#:tag "extending-drracket"]{Extending DrRacket}

DrRacket supports four forms of extension to the programming
 environment: keybindings, teachpacks, scripts and plugins.

@itemize[
  
@item{The @deftech{Keybindings} menu item allows the addition of user 
  defined keybindings. For information on creating user defined 
  keybindings, see @secref["defining-shortcuts" #:doc 
  '(lib "scribblings/drracket/drracket.scrbl")].}

@item{The @deftech{Scripts} menu provided by the Quickscript plugin, makes
  it easy to extend DrRacket with small Racket scripts that can be used
  in the definition (or interaction) window, or to graphically interact
  with the user. For information on creating @tech{scripts}, see 
  @other-doc['(lib "quickscript/scribblings/quickscript.scrbl")].}

@item{@index['("languages" "extending")]{@index['("DrRacket
  Teachpacks")]{A @deftech{teachpack}}} extends the set of procedures
  that are built into a language in DrRacket.  For example, a
  teachpack might extend the Beginning Student language with a
  procedure for playing sounds.

  Teachpacks are particularly useful in a classroom setting, where an
  instructor can provide a teachpack that is designed for a specific
  exercise. To use the teachpack, each student must download the
  teachpack file and select it through the @menuitem["Language" "Add
  Teachpack..."]  menu item.

  See @secref["teachpacks"] for information in creating teachpacks.}

@item{A @deftech{plugin} extends the set of utilities within the
  DrRacket environment. For example, DrRacket's @onscreen{Check
  Syntax} button starts a syntax-checking plugin. For information on
  creating @tech{plugin}s, see @other-manual['(lib
  "scribblings/tools/tools.scrbl")].}
  
]

Customization of the DrRacket environment is available via @deftech{Preferences}
menu item, along the the facility to define new 
@secref["color-scheme" #:doc '(lib "scribblings/drracket/drracket.scrbl")].
Existing colorschemes packages are  
@hyperlink["https://pkgd.racket-lang.org/pkgn/search?tags=colorscheme"]{@italic{tagged 
`colorscheme'}}.


@; ----------------------------------------

@section[#:tag "teachpacks"]{Teachpacks}

Teachpacks are designed to supplement student programs with code that 
cannot be expressed in a teaching language. For
example, to enable students to play hangman, we supply a teachpack that

@itemize[

 @item{implements the random choosing of a word,}

 @item{maintains the state variable of how many guesses have gone wrong, and}

 @item{manages the GUI.}

]

All these tasks are beyond students in the third week and/or impose
memorization of currently useless knowledge on students. The essence
of the hangman game, however, is not. The use of teachpacks enables
the students to implement the interesting part of this exercise and
still be able to enjoy today's graphics without the useless
memorization.

A single Racket source file defines a teachpack (although the file may
access other files via @racket[require]). The file must contain a
module (see @secref[#:doc '(lib "scribblings/guide/guide.scrbl")
"modules"]). Each exported syntax definition or value definition from
the module is provided as a new primitive form or primitive operation
to the user, respectively.

As an example, the following teachpack provides a lazy cons
implementation. To test it, save the following in a file and
add the file as a teachpack (or use @racket[require]).

@racketmod[
racket

(provide (rename-out [:lcons lcons]) lcar lcdr)

(define-struct lcons (hd tl))

(define-syntax (:lcons stx)
  (syntax-case stx ()
    [(_ hd-exp tl-exp)
     (syntax (make-lcons 
               (delay hd-exp)
               (delay tl-exp)))]))

(define (lcar lcons) (force (lcons-hd lcons)))
(define (lcdr lcons) (force (lcons-tl lcons)))
]

Then, in this program:

@racketblock[
(define (lmap f l)
  (lcons
   (f (lcar l))
   (lmap f (lcdr l))))

(define all-nums (lcons 1 (lmap add1 all-nums)))
]

the list @racket[all-nums] is bound to an infinite list
 of ascending numbers.

For more examples, see the @filepath{htdp} sub-collection in the
@filepath{teachpack} collection of the PLT installation.

@subsection{Adding Your Own Teachpacks to the Teachpack Dialog}

The @onscreen{Language|Add Teachpack...} dialog is extensible
in two ways. First, users can add teachpacks to the third column
by clicking the button at the bottom of the column. These additions
are stored in the preferences file, so one way to add site-specific
teachpacks is to provide a default preferences file.

The first two columns are also extensible. When a collection has
an @filepath{info.rkt} file 
(see @secref[#:doc racodoc "info.rkt"])
that defines @racket[htdp-teachpacks] or @racket[2htdp-teachpacks],
then they are expected to be either a list of (collection-relative)
paths containing teachpacks to add to the dialog, or the symbol
@racket['all], which means that all of the (top-level) files in the collection
that end with a module suffix (including @filepath{.rkt}, @filepath{.ss}, or @filepath{.scm})
are teachpacks (except @filepath{info.rkt} or @filepath{info.ss}).

@subsection{Extending Help Desk Search Context}

In @|HtDP| Teaching Languages,
the search context for the @onscreen{Search in Help Desk for ...}
item in the pop-up menu can be extended by defining the
@racket[2htdp:teachpack-modules] binding in the @tt{info.rkt} file
(see @secref[#:doc racodoc "info.rkt"]).
The @racket[2htdp:teachpack-modules] binding should evaluate to a list of
@tech[#:key "symbol" #:doc '(lib "scribblings/reference/reference.scrbl")]{symbols}
representing the
@tech[#:key "module path" #:doc '(lib "scribblings/reference/reference.scrbl")]{module paths}
to be included in the search context.

For example, the following @tt{info.rkt} file

@racketmod[
 info

 (define scribblings '(("scribblings/intro101.scrbl")))

 (define 2htdp:teachpack-modules
   '(intro101/file-operations intro101/iterated))
 ]

includes the modules
@tt{intro101/file-operations} and @tt{intro101/iterated}
in the help desk search context.

@; ----------------------------------------------------------------------

@section[#:tag "environment-variables"]{Environment Variables}

Several environment variables can affect DrRacket's behavior:

@itemize[

 @item{@indexed-envvar{PLTNOTOOLS} : When this environment variable is
       set, DrRacket doesn't load any tools.}

 @item{@indexed-envvar{PLTONLYTOOL} : When this environment variable
       is set, DrRacket only loads the tools in the collection named
       by the value of the environment variable. If the variable is
       bound to a parenthesized list of collections, only the tools in
       those collections are loaded (The contents of the environment
       variable are @racket[read] and expected to be a single symbol
       or a list of symbols).}
 
 @item{@indexed-envvar{PLTDRREPL} : When this environment variable is
        set, DrRacket starts a read-eval-print loop with all of
        the forms and functions from the @racketmodname[racket]
        module and those described in @other-doc['(lib "scribblings/tools/tools.scrbl")].
        
        If it is not set to @litchar{-q}, then @racket[(find-system-path 'init-file)]
        is loaded as well.
        
        A new thread is created to evaluate REPL expressions, so be 
        sure to use @racket[queue-callback] to evaluate expressions
        that mutate the GUI (to avoid race-conditions).
        }

 @item{@indexed-envvar{PLTDRCM} : When this environment variable is
       set, DrRacket installs the compilation manager before starting
       up, which means that the @filepath{.zo} files are automatically
       kept up to date, as DrRacket's (or a tool's) source is modified.

       If the variable is set to @litchar{trace} then the files that are
       actually recompiled are shown.}

 @item{@indexed-envvar{PLTDRPAR} : When this environment variable is
       set, DrRacket uses @racket[parallel-compile-files] to compile
       the framework and the drracket collections in parallel and then
       installs the compilation manager before starting
       up, which means that the @filepath{.zo} files are automatically
       kept up to date, as DrRacket's (or a tool's) source is modified.

       If the variable is set to @litchar{trace} then the files that are
       actually recompiled are shown.}

 @item{@indexed-envvar{PLTDRDEBUG} : When this environment variable is
       set, DrRacket starts up with errortrace enabled. If the
       variable is set to @litchar{profile}, DrRacket also records
       profiling information about itself.}

 @item{@indexed-envvar{PLTDRPROFILE} : When this environment variable is
       set, DrRacket uses the @racketmodname[profile] library (with
       a little GUI) to collect profiling information about itself.}

 @item{@indexed-envvar{PLTDRBREAK} : When this environment variable is
       set, DrRacket creates a window with a break button, during
       startup. Clicking the button breaks DrRacket's eventspace's
       main thread. This works well in combination with
       @envvar{PLTDRDEBUG} since the source locations are reported for
       the breaks.}

 @item{@indexed-envvar{PLTSTRINGCONSTANTS} : When this environment
       variable is set, DrRacket prints out the string constants that
       have not yet been translated. If it is set to a particular
       language (corresponding to one of the files in
       @filepath{string-constants} collection) it only shows the unset
       string constants matching that language.

       This environment variable must be set when @filepath{.zo} files
       are made. To ensure that you see its output properly, run
       @exec{raco setup} with the @Flag{c} flag, set the environment
       variable, and then run @exec{raco setup} again.}

 @item{@indexed-envvar{PLTDRXREFDELAY} : When this environment variable
        is set, DrRacket uses an ordinary @racket[delay] (instead of
        @racket[delay/idle]) to delay the computation of the searching
        indices. This means that Check Syntax will start more slowly
        the first time, but that the startup performance is more
        predictable. In addition, when the environment variable is
        set, DrRacket will print out that it is set, and will print
        when the index is started loading and when it finishes loading.}

 @item{@indexed-envvar{PLTDREASTERSECONDS} : When this environment variable
        is set, DrRacket pretends that the result of @racket[current-seconds]
        is actually this environment variable's value, for the purposes
        of easter eggs. For example, setting it to 1339390801 would simulate
        King Kamehameha day 2012 and show the corresponding easter egg splash 
        screen.}
]
