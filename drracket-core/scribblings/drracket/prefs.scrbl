#lang scribble/doc
@(require "common.rkt" scribble/bnf scribble/struct
          "indentation-table.rkt")

@(define (PrefItem . s) (apply onscreen s))

@title[#:tag "prefs-explanation"]{Preferences}

The preferences dialog consists of several panels.


@section{@onscreen{Font}}

This panel controls the main font used by DrRacket.

@section[#:tag "sec:colors"]{@onscreen{Colors}}

The @onscreen{Colors} panel has several sub-panels that let you
configure the colors that DrRacket uses.
@itemize[
 @item{@onscreen{Color Schemes}

  DrRacket comes with a number of different color schemes
  that you can choose from. The @onscreen{Classic} and
  @onscreen{White on Black} schemes are actually a matched
  pair that, under Mac OS and Linux will be chosen based on
  the operating system's dark-mode configuration. Similarly
  for the two Tol's color-blind safe modes.}

 @item{@onscreen{Background}

  This section of the preferences configures the default
  background color and the colors used behind matching
  parentheses. The @onscreen{Parenthesis color scheme} menu
  also includes options that shade nested parentheses darker
  and lighter, based on the nesting depth.}]

The remaining panels inside the @onscreen{Colors} panel
allow the configuration of many different colors that
DrRacket uses for specific parts of its interface.

@section{@onscreen{Editing}}

The @onscreen{Editing} panel consists of several sub-panels:

@itemize[

@item{@onscreen{Indenting}

      This panel controls which keywords DrRacket recognizes for
      indenting, and how each keyword is treated.
      
      To decide how a particular line is indented, DrRacket starts
      at the beginning of the line and looks
      for an enclosing s-expression. It finds the first symbol
      after that open parenthesis, and uses that to determine 
      how to indent the line.
      
      If the symbol is not listed in any of the lists in the preferences
      dialog, then it is indented like a function call, e.g.:
      @verbatim[app-example1]
      @verbatim[app-example2]
      
      If the symbol is listed in the @onscreen{Begin-like Keywords} section,
      it gets one extra space when the first sub-expression is on
      a different line and otherwise it looks like an application.
      @verbatim[begin-example1]
      @verbatim[begin-example2]
      
      If the symbol is listed in the @onscreen{Define-like Keywords} section,
      then all of the subexpressions are indented one extra space, 
      no matter what happens on the first line:
      @verbatim[define-example1]
      @verbatim[define-example2]
      @verbatim[define-example3]
      
      If the symbol is listed in the @onscreen{Lambda-like Keywords} section,
      then it is indented like define, except that there is three extra spaces
      of indentation added when the first sub-expression is not on the 
      same line as the keyword:
      @verbatim[lambda-example1]
      @verbatim[lambda-example2]
      
      If the symbol is listed in the @onscreen{For/fold-like Keywords} section,
      it is indented just like the define section, except that the first two
      sub-expressions are treated specially:
      @verbatim[for/fold-example]

      If the symbol is not listed in any of the list boxes in the various
      sections, the contents of the
      @onscreen{Extra Regexp} panel are consulted. They are
      @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{regular expressions}
      that are matched against the printed representation of the symbol, and
      if they match, the corresponding indentation style is used.

      }

@item{@onscreen{Square bracket}

      This panel controls which keywords DrRacket uses to determine when
      to rewrite @litchar["["] to @litchar["("].  For @racket[cond]-like
      keywords, the number in parenthesis indicates how many
      sub-expressions are skipped before square brackets are started.

      See @secref["editor"] for details on how the entries in the
      columns behave.}

@item{@onscreen{General Editing}

      @itemize[
      @item{@PrefItem{Map delete to backspace} --- If checked, the editor
            treats the Delete key like the Backspace key.}
      @item{@PrefItem{Color syntax interactively} --- If checked,
            DrRacket colors your syntax as you type.}
      @item{@PrefItem{Wrap words in editor buffers} --- If checked,
            DrRacket editors auto-wrap text lines by default.  Changing
            this preference affects new windows only.}
      @item{@PrefItem{Search using anchors} --- If checked, DrRacket's
            searching mode will jump directly to the first search hit,
            using an ``anchor'' to determine where to search if the
            search string changes.}
      @item{@PrefItem{Disable caret blinking} --- If checked, the insertion
             point in the definitions and interactions windows (and other
             editors) does not blink.}
      @item{@PrefItem{Normalize pasted strings} --- If checked, DrRacket
            adjusts strings that are pasted into the editor to avoid
            confusion.  For example, non-breaking spaces look just like
            spaces but are not considered separators like ordinary
            spaces are.  If this is checked DrRacket will automatically
            turn those non-breaking spaces into regular spaces.
            Similarly with other (less common) characters.}
      @item{@PrefItem{Treat alt key as meta} --- If checked,
            DrRacket will use the alt (option) key for some Emacs-like
            keybindings, instead of using it for menu shortcuts. This
            option is only available on Mac OS.}
      @item{@PrefItem{Enable overwrite mode keybindings} --- If checked,
            DrRacket enables the insert keybinding to swap into
            overwrite mode}
      @item{@PrefItem{Treat command key as meta} --- If checked,
            DrRacket will use the command key for some Emacs-like
            keybindings, instead of using it for menu shortcuts. This
            option is only available on Mac OS.}
      @item{@PrefItem{Always use the platform-specific linefeed convention} ---
             If checked, DrRacket always saves files with CRLF line terminators. 
             If unchecked, DrRacket looks at each file as it is opened and if every
             line is terminated with CRLF (and there is at least one line), 
             then it saves the file with CRLF terminators
             and otherwise it is saved with LF terminators (following the
             Mac OS and Linux convention). When a file is going to be saved
             with CRLF terminators, then the status line at the bottom of the
             DrRacket window shows ``CRLF''.
             
             This option is available only under Windows. On other operating
             systems, all files are always saved with LF line terminators.}
      @item{@PrefItem{Enable keybindings in menus} --- If checked, some
            DrRacket menu items have keybindings.  Otherwise, no menu
            items have key bindings.  This preference is designed for
            people who are comfortable editing in Emacs and find the
            standard menu keybindings interfere with the Emacs
            keybindings.}
      @item{@PrefItem{Enable automatic parentheses, square brackets, and quotes} --- If checked,
             typing an open parenthesis, curly brace, square bracket, double quote,
             or vertical bar character automatically inserts a matching one.
             Additionally, backspace will automatically remove matched empty pairs
             of such characters when the caret is between them in many cases.}
      @item{@PrefItem{Add one pixel of extra space between lines} ---
             If checked, then an extra pixel of whitespace is added
             between lines in the editor. The default value is platform-specific;
             some fonts (notably those with @tt{╔══╗} characters) only look right with
             this unchecked.}
 
      @item{@PrefItem{Maximum character width guide} --- If checked, DrRacket
             draws a vertical line in files that exceed the given maximum
             width. The vertical line shows where the given maximum width is.}
      @item{@PrefItem{Show line numbers} --- If checked, DrRacket shows
            line numbers for the file being edited in the left-hand
            column}
      @item{@PrefItem{Show definitions/interactions labels} --- If checked,
             then the teaching languages show big friendly labels indicating
             which window is the definitions window and which is the interactions
             window.]}
 @item{@PrefItem{Startup Open Files} --- If the @PrefItem{Restore files from previous session} is selected,
    DrRacket will reopen the files that were open when DrRacket last exited, as it starts up.
    When @PrefItem{Open a blank window} is selected, DrRacket starts with a new, fresh window each time.
    When @PrefItem{Ask me each time} is selected and some files were open in the previous session,
    DrRacket will open a dialog box asking if you would like to open the same files as last time.
    If DrRacket is started from the command-line (or via Mac OS's @tt{open} command) and files are
    supplied, then this preference is ignored, and only those files are opened.}
 @item{@PrefItem{Automatic @tt{#lang} line} --- When new files are opened in DrRacket,
        it will use this for the first line of the buffer.}
 ]}

@item{@onscreen{Racket}

      @itemize[
      @item{@PrefItem{Highlight between matching parens} --- If checked,
            the editor marks the region between matching parenthesis
            with a gray background (in color) or a stipple pattern (in
            monochrome) when the blinking caret is next to a
            parenthesis.}
      @item{@PrefItem{Automatically adjust closing parens} --- If
            checked, the editor automatically converts a typed
            @litchar[")"] to @litchar["]"] to match @litchar["["], or it
            converts a typed @litchar["]"] to @litchar[")"] to match
            @litchar["("].}
      @item{@PrefItem{Automatically adjust opening square brackets} If
            checked, the editor changes typed @litchar["["] to match the
            context (as explained in @secref["editor"]).}
      @item{@PrefItem{Flash paren match} --- If checked, typing a
            closing parenthesis, square bracket, or quotation mark
            flashes the matching open parenthesis/bracket/quote.}]}

]


@section{@onscreen{Warnings}}

@itemize[

@item{@PrefItem{Ask before changing save format} --- If checked,
      DrRacket consults the user before saving a file in non-text format
      (see @secref["drracket-file-formats"]).}

@item{@PrefItem{Verify exit} --- If checked, DrRacket consults the
      user before exiting.}

@item{@PrefItem{Ask about normalizing strings} --- If checked, DrRacket
      consults the user before normalizing a string pasted into the
      editor.}

@item{@PrefItem{Only warn once when executions and interactions are not
      synchronized} --- If checked, DrRacket warns the user on the first
      interaction after the definitions window, language, or teachpack
      is changed without a corresponding click on @onscreen{Run}.
      Otherwise, the warning appears on every interaction.}

@item{@PrefItem{Ask about clearing test coverage} --- If checked, when
      test coverage annotations are displayed DrRacket prompts about
      removing them.  This setting only applies to the PLT languages.
      DrRacket never asks in the teaching languages.}

@item{@PrefItem{Check for newer Racket versions} --- If checked,
      DrRacket periodically polls a server to determine whether a newer
      version of DrRacket is available.}

]


@section{@onscreen{General}}

@itemize[

@item{@PrefItem{Number of recent items} --- controls the length of the
      @onscreen{Open Recent} menu (in the @onscreen{File} menu).}

 @item{@PrefItem{Make backups for unsaved files} --- If checked, the editor generates
      copies of open files (see @secref["drracket-autosave-files"]) for files
      that have not been saved after thirty seconds.}

@item{@PrefItem{Create first-change files} --- If checked, when saving a file for the
      first time in each editing session, the original copy of the file
      is copied to a new file in the same directory.  The new
      files has the same name as the original, except that it ends in
      either @indexed-file{.bak} or @indexed-file{~}.}

@item{@PrefItem{Show status-line} --- If checked, DrRacket shows a
      status line at the bottom of each window.}

@item{@PrefItem{Count column numbers from one} --- If checked, the
      status line's column counter counts from one.  Otherwise, it
      counts from zero.}

@item{@PrefItem{Display line numbers in buffer; not character offsets}
      --- If checked, the status line shows a
      @nonterm{line}:@nonterm{column} display for the current selection
      rather than the character offset into the text.}

@item{@PrefItem{Automatically print to PostScript file} --- If checked,
      printing will automatically save PostScript files.  If not,
      printing will use the standard printing mechanisms for your
      computer.}

@item{@PrefItem{Open files in separate tabs (not separate windows)} ---
      If checked, DrRacket will use tabs in the front-most window to
      open new files, rather than creating new windows for new files.}

@item{@PrefItem{Automatically open interactions window when running a
      program} --- If checked, DrRacket shows the interactions window
      (if it is hidden) when a program is run.}

@item{@PrefItem{Automatically switch to the module language when opening
      a module} --- If checked, DrRacket will recognize files that have
      a @tt{#lang} line and adjust the language setting automatically.}

@item{@PrefItem{Put the interactions window beside the definitions
      window} --- If checked, DrRacket puts the interactions window to
      the right of the definitions window.  By default, the interactions
      window is below the definitions window.}

@item{@PrefItem{Always show the #lang line in the Module language} ---
      If checked, the module language always shows the @hash-lang[] line
      (even when it would ordinarily be scrolled off of the page),
      assuming that the @hash-lang[] line is the first line in the file.}

 @item{@PrefItem{Show test coverage results summary} --- If
  checked, DrRacket shows a little note indicating that all
  tests were covered in addition to not changing the colors.
  If not checked, DrRacket remains silent (unchanged) when
  test coverage is enabled but all of the code is covered.}

 @item{@PrefItem{Save files whenever switching tabs or windows} ---
  If checked, DrRacket will automatically save any unsaved files
  when switching between windows or tabs.}
 
 @item{@PrefItem{Don't ask and don't save files when
   clicking Run with unsaved tabs or windows} --- If checked,
  DrRacket will not show a warning dialog about unsaved tabs
  when clicking the @onscreen{Run} button, nor will it save
  the files automatically. If this is checked, the previous
  checkbox's value is ignored.}

]


@section{@onscreen{Profiling}}

This preference panel configures the profiling report.  The band of
color shows the range of colors that profiled functions take on.  Colors
near the right are used for code that is not invoked often and colors on
the right are used for code that is invoked often.

If you are interested in more detail at the low end, choose the
@onscreen{Square root} check box.  If you are interested in more detail
at the upper end, choose the @onscreen{Square} check box.


@section{@onscreen{Browser}}

This preferences panel allows you to configure your HTTP proxy.  Contact
your system administrator for details.


@section{@onscreen{Tools}}

This preference panel allows you to configure the currently active
plugins.
