#lang scribble/manual
@(require (for-label racket
                     help/search
                     help/bug-report
                     help/help-utils)
          (only-in scribble/core
                   style
                   paragraph
                   make-background-color-property))

@title{Bug Reporting}
@defmodule[help/bug-report]

@(define yellow-style (style #f (list (make-background-color-property "yellow"))))

@paragraph[yellow-style
           @elem{To report a Racket bug, please
                 @hyperlink["https://github.com/racket/racket/issues"]{create a new issue here}.}]

The @racketmodname[help/bug-report] library is provided for backward
compatibility, but the server that it attempts to contact has been
disabled, so no bug submissions through this interface will succeed.

@defproc[(help-desk:report-bug [this-bug-id #f (or/c #f exact-positive-integer?)] 
                               [#:frame-mixin 
                                frame-mixin
                                (make-mixin-contract frame%)
                                values])
         void?]{
  Opens a bug report window to edit the bug report identified by @racket[this-bug-id].
  If @racket[this-bug-id] is @racket[#f], then creates a new bug ID and uses that one.
  
  The @racket[frame-mixin] argument is passed the frame class before creating the window.
}

@defproc[(saved-bug-report-titles/ids) (listof brinfo?)]{
  Returns a list of the saved bug reports.
}

@defproc[(discard-all-saved-bug-reports) void?]{
  Deletes all of the saved bug reports, except those currently
  open in frames.
}

@defproc[(unsave-bug-report [bug-id exact-positive-integer?]) void?]{
  Deletes the saved bug report identified by @racket[bug-id].
}

@defstruct[brinfo ([title label-string?]
                   [id number?]) #:transparent]{
  A record representing a saved bug report. The @racket[id] field is suitable
  for use with @racket[help-desk:report-bug], and the @racket[label] field
  is suitable for use in a GUI control.
}
