#lang at-exp racket/base

#|
This first time this is loaded, it loads all of DrRacket and invokes
the main unit, starting up DrRacket. After that, it just provides
all of the names in the tools library, for use defining keybindings
|#

(module test racket/base)

(require racket/class
         racket/gui/base
         racket/unit 
         racket/contract
         racket/class
         racket/struct-info
         
         ;; these have to be absolute requires for `include-extracted'
         ;; to work with this file.
         drracket/private/link
         drracket/private/drsig
         drracket/private/language-object-contract
         drracket/private/local-member-names
         
         framework
         framework/splash
         
         mrlib/switchable-button
         scribble/srcdoc
         
         net/url)

(require (for-syntax racket/base racket/list racket/struct-info))

(generate-delayed-documents) ; avoids a distribution dependency on `scribblings/tools/doc-util'
(require (for-doc drracket/private/ts
                  racket/base scribble/manual
                  scribblings/tools/doc-util
                  (for-syntax racket/base)
                  (for-label errortrace/errortrace-key
                             racket/place
                             racket/pretty 
                             mzlib/pconvert
                             syntax/toplevel
                             drracket/tool-lib
                             string-constants)))

;; these two declarations produce all of the struct names
;; but with "drscheme" in front instead of drracket
(begin
  (module drr-structs racket/base
    (require drracket/private/local-member-names)
    (provide 
     (combine-out
      (struct-out drracket:language-configuration:language-settings)
      (struct-out drracket:unit:teachpack-callbacks)
      (struct-out drracket:language:text/pos) 
      (struct-out drracket:language:simple-settings)
      (struct-out drracket:modes:mode))))
  (require racket/require
           (filtered-in 
            (λ (x) (regexp-replace #rx"drracket:" x "drscheme:"))
            (submod "." drr-structs))))


;; would like to use except here, but that
;; is allowed only on imports,
;; so just rename to something unused.
(define-syntax (dv/iu/i/drop-struct stx)
  (syntax-case stx ()
    [(_ sig stuff ...)
     (let ()
       (define (get-ids prefix sid-flds)
         (syntax-case sid-flds ()
           [(struct-id constructor? fields ...)
            (let ()
              (define include-constructor? (syntax-e #'constructor?))
              (unless (boolean? include-constructor?)
                (raise-syntax-error #f "expected boolean?" stx #'constructor?))
              (define (fmt str . args)
                (define orig
                  (string->symbol
                    (apply format str (map (λ (x) (if (syntax? x) (syntax-e x) x))
                                           args))))
                `(,(datum->syntax #'sig (string->symbol (format "___~a" orig)))
                  ,(datum->syntax #'sig orig)))
              (append (list (fmt "~a~a?" prefix #'struct-id))
                      (if include-constructor?
                          (list (fmt "~a~a" prefix #'struct-id))
                          '())
                      (for/list ([fld (in-list (syntax->list #'(fields ...)))])
                        (fmt "~a~a-~a" prefix #'struct-id fld))))]))
     #`(define-values/invoke-unit/infer 
         (export (rename sig
                         #,@(apply
                             append
                             (for/list ([sid-flds (syntax->list #'(stuff ...))])
                               (append (get-ids 'drracket: sid-flds)
                                       (get-ids 'drscheme: sid-flds))))))
         drracket@))]))
(dv/iu/i/drop-struct 
 drscheme/drracket:tool^
 (language-configuration:language-settings #t language settings)
 (unit:teachpack-callbacks #t get-names add remove remove-all)
 (language:text/pos #t text start end)
 (language:simple-settings #t case-sensitive printing-style
                           fraction-style show-sharing
                           insert-newlines annotations)
 (modes:mode #t name surrogate repl-submit matches-language intended-to-edit-programs?))
 
(provide-signature-elements drracket:tool-cm^) ;; provide all of the classes & interfaces
(provide-signature-elements drscheme:tool-cm^) ;; provide the classes & interfaces w/ drscheme: prefix

(provide drracket:unit:program-editor-mixin)
(define-syntax (drracket:unit:program-editor-mixin stx)
  (syntax-case stx ()
    [(_ a ...)
     #'((drracket:unit:get-program-editor-mixin) a ...)]
    [_ #'(drracket:unit:get-program-editor-mixin)]))

(language-object-abstraction drracket:language:object/c #t)

(define-syntax (provide/dr/doc stx)
  (define (munge-id stx)
    (datum->syntax
     stx
     (string->symbol
      (regexp-replace #rx"drracket:" (symbol->string (syntax-e stx)) "drscheme:"))
     stx))
  (define (handle-one id ctc)
    (with-syntax ([id id]
                  [ctc ctc]
                  [mid (munge-id id)])
      #`(thing-doc 
         mid ctc 
         ("This binding provided for backwards compatibility; new code should use " 
          (racket id)
          " instead."))))
  (define (remove-src-locs stx)
    (let loop ([x stx])
      (cond
        [(syntax? x) (datum->syntax x (loop (syntax-e x)) #f)]
        [(pair? x) (cons (loop (car x)) (loop (cdr x)))]
        [else x])))
  
  (define (handle-struct a fld/ctc omit-constructor?)
    (with-syntax ([([fld ctc] ...) fld/ctc])
      (define (ds fmt . args)
        (datum->syntax a
                       (string->symbol (apply format fmt args))))
      (let* ([flds (syntax->list #'(fld ...))]
             [ctcs (syntax->list #'(ctc ...))]
             [st-name (syntax-e a)]
             [pid (ds "~a?" st-name)])
        #`(combine-out
           #,(handle-one pid (remove-src-locs #`(-> any/c boolean?)))
           #,(if omit-constructor?
                 #'(combine-out)
                 (handle-one (ds "make-~a" st-name) (remove-src-locs #`(-> #,@ctcs #,pid))))
           #,@(for/list ([fld (in-list flds)]
                         [ctc (in-list ctcs)])
                (define id (ds "~a-~a" st-name (syntax-e fld)))
                (handle-one id (remove-src-locs #`(-> #,pid #,ctc))))))))
  
  (define defthings
    (syntax-case stx ()
      [(_ case ...)
       (for/list ([case (in-list (syntax->list #'(case ...)))])
         (syntax-case case (thing-doc
                            proc-doc/names proc-doc 
                            parameter-doc parameter/c
                            struct-doc)
           [(proc-doc/names id ctc . stuff)
            (identifier? #'id)
            (handle-one #'id #'ctc)]
           [(proc-doc id ctc . stuff)
            (identifier? #'id)
            (handle-one #'id #'ctc)]
           [(parameter-doc id (parameter/c ctc) arg-id . stuff)
            (and (identifier? #'id)
                 (identifier? #'arg-id))
            (handle-one #'id (remove-src-locs #`(parameter/c ctc)))]
           [(thing-doc id ctc . stuff)
            (identifier? #'id)
            (handle-one #'id #'ctc)]
           [(struct-doc a ([fld ctc] ...) #:omit-constructor stuff)
            (handle-struct #'a #'([fld ctc] ...) #t)]
           [(struct-doc a ([fld ctc] ...) stuff)
            (handle-struct #'a #'([fld ctc] ...) #f)]
           [_
            (raise-syntax-error 'provide/dr/doc "unknown clause" case)]))]))
  (syntax-case stx ()
    [(_  rst ...)
     #`(provide #,@defthings rst ...)]))

(provide/dr/doc
 
 (proc-doc/names
  drracket:module-language-tools:add-opt-out-toolbar-button
  (->* ((-> (is-a?/c top-level-window<%>)
            (is-a?/c area-container<%>) 
            (is-a?/c switchable-button%))
        symbol?)
       (#:number (or/c real? #f))
       void?)
  ((make-button id) ((number #f)))
  @{Call this function to add another button to DrRacket's toolbar. When buttons are added this way,
    DrRacket monitors the @tt{#lang} line at the top of the file; when it changes DrRacket queries
    the language to see if this button should be included.
    These buttons are ``opt out'', meaning that if the language doesn't explicitly ask to not
    have this button (or all such buttons), the button will appear. See
    @language-info-ref[drracket:opt-out-toolbar-buttons] for more information.
    
    The @racket[number] argument is the same as the @racket[number] argument
    to @method[drracket:unit:frame<%> register-toolbar-button].})

 (proc-doc/names
  drracket:module-language-tools:add-opt-in-toolbar-button
  (->* ((-> (is-a?/c top-level-window<%>)
            (is-a?/c area-container<%>) 
            (is-a?/c switchable-button%))
        symbol?)
       (#:number (or/c real? #f))
       void?)
  ((make-button id) ((number #f)))
  @{Like @racket[drracket:module-language-tools:add-opt-out-toolbar-button], but
    for buttons that should not be enabled by default, but instead explicitly
    opted in by languages via @language-info-ref[drracket:opt-in-toolbar-buttons].
    @history[#:added "1.6"]})
 
 (proc-doc/names
  drracket:module-language-tools:add-online-expansion-handler 
  (-> path-string? symbol? (-> (is-a?/c drracket:unit:definitions-text<%>)
                               any/c
                               any)
      void?)
  (mod-path id local-handler)
  @{Registers a pair of procedures with DrRacket's online expansion machinery. 
    (See also @racket[drracket:module-language-tools:add-online-expansion-monitor].)
    
    The procedure @racket[id] from @racket[mod-path] is loaded by
    @racket[dynamic-require] into a specially designed separate @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{place}.
    When DrRacket detects that
    the editor has been modified, it sends the contents of the editor over to
    that separate place, @racket[expand]s the program there, and then supplies
    the fully expanded object to that first procedure. (The procedure is called 
    in the same context as the expansion process.)

    If the expansion raises an exception, then that exception is supplied as the 
    first argument instead of the syntax object. If a non-@racket[exn?] is raised, 
    or if the expansion process is terminated (e.g. via @racket[custodian-shutdown-all]
    called during expansion), then the expansion monitor is not notified.
    
    The contract for that procedure is
    @racketblock[(-> (or/c syntax? exn?) path? any/c custodian? 
                     any)]
    There are three other arguments:
    @itemize[
      @item{
    The @racket[path?] argument is the path that was the @racket[current-directory]
    when the code was expanded. This directory should be used as the 
    @racket[current-directory] when resolving module paths obtained from
    the syntax object.}
    
      @item{
    The third argument is the source object used in the syntax objects that
    come from the definitions window in DrRacket. It may be a path (if the file
    was saved), but it also might not be. Use @racket[equal?] to compare it
    with the @racket[syntax-source] field of syntax objects to determine if
    they come from the definitions window.}
    
      @item{ Note that the thread that calls this procedure may be
    killed at any time: DrRacket may kill it when the user types in the buffer
    (in order to start a new expansion), but bizarro code may also create a separate
    thread during expansion that lurks around and then mutates arbitrary things.
    
    Some code, however, should be longer running, surviving such custodian
    shutdowns. To support this, the procedure called in the separate place is
    supplied with a more powerful custodian that is not shut down. }]
    
    The result of the procedure is expected to be something that can be sent
    across a @racket[place-channel], which is then sent back to the original
    place where DrRacket itself is running and passed to the @racket[local-handler]
    argument. At this point, the only code running is trusted code (DrRacket itself
    and other tools), but any long running computations may freeze DrRacket's GUI, 
    since this procedure is invoked on DrRacket's eventspace's handler thread.})
 
 (proc-doc/names
  drracket:module-language-tools:add-online-expansion-monitor
  (-> path-string? symbol?
      (-> (is-a?/c drracket:unit:definitions-text<%>)
          (or/c drracket:module-language-tools:start?
                any/c)
          any)
      void?)
  (mod-path id local-handler)
  @{Registers a pair of procedures with DrRacket's online expansion machinery. 
    
    Like @racket[drracket:module-language-tools:add-online-expansion-handler], 
    the first two arguments specify a procedure that is called in the separate place
    designated for expansion.
    
    The procedure is called before expansion starts and once it returns, expansion
    begins. The procedure should match this contract:
    @racketblock[(-> (-> any/c void?)
                     path? any/c custodian? 
                     any)]
    The first argument is a function that transmits its argument back to the
    DrRacket place, send it to the @racket[local-handler] argument.
    The other three arguments are the same as the corresponding procedure used
    by @racket[drracket:module-language-tools:add-online-expansion-handler].
    
    The expectation is that this procedure creates a thread and monitors the 
    expansion process, sending back information to the main place while
    expansion is progressing.
    
    The @racket[local-handler] procedure is called each time the 
    @racket[(-> any/c void?)] procedure (described just above) is called.
    It is also called each time an expansion starts; it receives a value
    that returns @racket[#t] from @racket[drracket:module-language-tools:start?]
    in that case.

    To help with debugging, DrRacket logs progress and recovers from some
    errors that happen when running the handler procedures. To monitor
    its progress, monitor the @racket['debug] level of the
    @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{logger}
    with the topic @indexed-racket['drracket-background-compilation].

 })

 (proc-doc/names
  drracket:module-language-tools:start?
  (-> any/c boolean?)
  (val)
  @{Returns @racket[#t] if this is a special (unique) value, used as
    discussed in @racket[drracket:module-language-tools:add-online-expansion-monitor].
    Returns @racket[#f] otherwise.})
 
 (proc-doc/names
  drracket:module-language-tools:register-online-expansion-pref
  (-> (-> (is-a?/c vertical-panel%) void?) void?)
  (func)
  @{Registers @racket[func] so that it is called while building the
    preferences panel. The function is passed a panel that contains 
    other configuration controls for online expansion.})
 
 (proc-doc/names
  drracket:module-language-tools:done?
  (-> any/c boolean?)
  (val)
  @{Returns @racket[#t] for @racket[drracket:module-language-tools:done]
            and @racket[#f] otherwise.})
 
 (thing-doc
  drracket:module-language-tools:done
  drracket:module-language-tools:done?
  @{Used to inform a monitor-based handler that the online expansion has finished.})
  
 
 (proc-doc/names
  drracket:module-language:add-module-language
  (-> any)
  ()
  @{Adds the module language to DrRacket. This is called during DrRacket's startup.})
 
 (proc-doc/names
  drracket:module-language:module-language-put-file-mixin
  (-> (implementation?/c text:basic<%>) (implementation?/c text:basic<%>))
  (super%)
  @{Extends @racket[super%] by overriding the @method[editor<%> put-file] method
    to use a default name from the buffer, if the buffer contains something like
    @tt{(module name ...)}.})
  
  
 
 ;                           
 ;                           
 ;                           
 ;                        ;  
 ;                        ;  
 ;                        ;  
 ;    ;;;  ;     ; ;;;    ;  
 ;   ;   ;  ;   ; ;   ;   ;  
 ;  ;    ;  ;   ;     ;   ;  
 ;  ;;;;;;   ; ;   ;;;;   ;  
 ;  ;        ; ;  ;   ;   ;  
 ;   ;        ;   ;   ;   ;  
 ;    ;;;;    ;    ;;;;;  ;  
 ;                           
 ;                           
 ;                           
 
 
 (proc-doc/names
  drracket:eval:set-basic-parameters
  (->* ((listof (is-a?/c snip-class%)))
       (#:gui-modules? boolean?)
       void?)
  ((snipclasses)
   ((gui-modules #t)))
  @{Sets the parameters that are shared between the repl's
    initialization and @racket[drracket:eval:build-user-eventspace/custodian].
    
    Specifically, it sets these parameters:
    @itemize[
             @item{@racket[current-namespace] has been set to a newly
                    created empty namespace. This namespace has the following modules 
                    shared (with @racket[namespace-attach-module])
                    from DrRacket's original namespace:
                    @itemize[@item{@racketmodname[racket/base]} 
                              @item{@racketmodname['#%foreign]}
                              @item{@racketmodname[mzlib/pconvert-prop]}
                              @item{@racketmodname[planet/terse-info]}]
                    If the @racket[gui-modules?] parameter is a true value, then
                    these modules are also shared:
                    @itemize[@item{@racketmodname[mred/mred]} 
                              @item{@racketmodname[mrlib/cache-image-snip]}
                              @item{@racketmodname[mrlib/image-core]}
                              @item{@racketmodname[mrlib/matrix-snip]}]
                    }
              @item{@racket[read-curly-brace-as-paren]
                     is @racket[#t]; }
              @item{@racket[read-square-bracket-as-paren]
                     is @racket[#t];}
              @item{@racket[error-print-width] is set to 250;}
              @item{@racket[current-ps-setup]
                     is set to a newly created
                     @racket[ps-setup%]
                     object;}
              @item{the @racket[exit-handler] is set to
                        a parameter that kills the user's custodian; and}
              @item{the snip-class-list, returned by
                    @racket[get-the-snip-class-list]
                    is initialized with all of the snipclasses in DrRacket's eventspace's 
                    snip-class-list.}]})
 
 (proc-doc/names
  drracket:eval:get-snip-classes
  (-> (listof (is-a?/c snip-class%)))
  ()
  @{Returns a list of all of the snipclasses in the current eventspace.})
 
 (proc-doc/names
  drracket:eval:expand-program
  (->* ((or/c input-port? drracket:language:text/pos?)
        drracket:language-configuration:language-settings?
        boolean?
        (-> void?)
        (-> void?)
        (-> (or/c eof-object? syntax? (cons/c string? any/c))
            (-> any)
            any))
       (#:gui-modules? boolean?)
       void?)
  ((input language-settings eval-compile-time-part? init kill-termination iter)
   ((gui-modules? #t)))
  
  @{Use this function to expand the contents of the definitions
    window for use with external program processing tools.
    
    This function uses
    @racket[drracket:eval:build-user-eventspace/custodian]
    to build the user's environment.
    The arguments @racket[language-settings], @racket[init], 
    @racket[kill-termination], and @racket[gui-modules?] are passed to
    @racket[drracket:eval:build-user-eventspace/custodian].
    
    The @racket[input] argument specifies the source of the program.
    
    The @racket[eval-compile-time-part?] argument indicates if
    @racket[expand]
    is called or if
    @racket[expand-top-level-with-compile-time-evals]
    is called when the program is expanded.
    Roughly speaking, if your tool will evaluate each expression
    itself by calling
    @racket[eval]
    then pass @racket[#f]. Otherwise, if your tool
    just processes the expanded program, be sure to pass
    @racket[#t].
    
    This function calls
    @method[drracket:language:language<%> front-end/complete-program]
    to expand the program. Unlike when the @onscreen{Run} is clicked,
    however, it does not call 
    @method[drracket:language:language<%> front-end/finished-complete-program].
    
    
    The first argument to @racket[iter] is the expanded program
    (represented as syntax) or eof.
    The @racket[iter] argument is called for each expression in the
    expanded program and once more with eof, unless an error is
    raised during expansion.
    It is called from the user's thread.
    If an exception is raised during expansion of the
    user's program, @racket[iter] is not called.
    Consider setting the exception-handler during @racket[init] to
    handle this situation.
    
    The second argument to @racket[iter] is a thunk that
    continues expanding the rest of the contents of the
    definitions window. If the first argument to @racket[iter] was
    eof, this argument is just the primitive
    @racket[void].
    
    See also
    @racket[drracket:eval:expand-program/multiple].})
 
 (proc-doc/names
  drracket:eval:traverse-program/multiple
  (->* (drracket:language-configuration:language-settings?
        (-> void?)
        (-> void?))
       (#:gui-modules? boolean?)
       (-> (or/c input-port? drracket:language:text/pos?)
           (-> (or/c eof-object? syntax? (cons/c string? any/c))
               (-> any)
               any)
           boolean?
           void?))
  ((language-settings init kill-termination)
   ((gui-modules #t)))
  
  @{This function is similar to
    @racket[drracket:eval:expand-program/multiple]
    The only difference is that it does not
    expand the program in the editor; instead
    the processing function can decide how to
    expand the program.})
 
 (proc-doc/names
  drracket:eval:expand-program/multiple
  (->* (drracket:language-configuration:language-settings?
        boolean?
        (-> void?)
        (-> void?))
       (#:gui-modules? boolean?)
       (-> (or/c input-port? drracket:language:text/pos?)
           (-> (or/c eof-object? syntax? (cons/c string? any/c))
               (-> any)
               any)
           boolean?
           void?))
  ((language-settings eval-compile-time-part? init kill-termination)
   ((gui-modules? #t)))
  
  @{This function is just like
    @racket[drracket:eval:expand-program]
    except that it is curried and the second application
    can be used multiple times.
    Use this function if you want to initialize the user's
    thread (and namespace, etc) once but have program text
    that comes from multiple sources.
    
    The extra boolean argument to the result function
    determines if
    @racket[drracket:language:language front-end/complete-program<%>]
    or
    @racket[drracket:language:language front-end/interaction<%>]
    is called.})
 
 (proc-doc/names
  drracket:eval:build-user-eventspace/custodian
  (->* (drracket:language-configuration:language-settings?
        (-> void?)
        (-> void?))
       (#:gui-modules? boolean?)
       (values eventspace? custodian?))
  ((language-settings init kill-termination) 
   ((gui-modules? #t)))
  
  @{This function creates a custodian and an eventspace (on the
    new custodian) to expand the user's program. It does not
    kill this custodian, but it can safely be shutdown (with
    @racket[custodian-shutdown-all]) after the
    expansion is finished.
    
    It initializes the
    user's eventspace's main thread with several parameters:
    @itemize[
             @item{ @racket[current-custodian] is set to a new custodian.
                    }@item{
                           In addition, it calls
                           @racket[drracket:eval:set-basic-parameters],
                           passing the @racket[#:gui-modules?] parameter along.
                           }]
    
    The @racket[language-settings] argument is the current
    language and its settings. See
    @racket[drracket:language-configuration:language-settings]
    for details on that structure.
    
    If the program is associated with a DrRacket
    frame, get the frame's language settings from the
    @method[drracket:unit:definitions-text<%> get-next-settings]
    method of 
    @racket[drracket:unit:definitions-text<%>].  Also, the most recently chosen language in
    the language dialog is saved via the framework's
    preferences. Apply
    @racket[preferences:get]
    to
    @racket[drracket:language-configuration:get-settings-preferences-symbol]
    for that @racket[language-settings].
    
    The @racket[init] argument is called after the user's parameters
    are all set, but before the program is run. It is called on
    the user's thread. The
    @racket[current-directory] and
    @racket[current-load-relative-directory]
    parameters are not set, so if there are appropriate directories,
    the @racket[init] argument is a good place to set them.
    
    The @racket[kill-termination] argument is called when the main thread of
    the eventspace terminates, no matter if the custodian was
    shutdown, or the thread was killed. This procedure is also
    called when the thread terminates normally. This procedure is
    called from a new, dedicated thread (@italic{i. e.}, not the thread
    created to do the expansion, nor the thread that
    @racket[drracket:eval:build-user-eventspace/custodian] was called from.)})
 
 
 
 ;                                         
 ;                                         
 ;                                         
 ;       ;          ;                      
 ;       ;          ;                      
 ;       ;          ;                      
 ;    ;; ;    ;;;   ; ;;    ;   ;    ;; ;  
 ;   ;  ;;   ;   ;  ;;  ;   ;   ;   ;  ;;  
 ;  ;    ;  ;    ;  ;    ;  ;   ;  ;    ;  
 ;  ;    ;  ;;;;;;  ;    ;  ;   ;  ;    ;  
 ;  ;    ;  ;       ;    ;  ;   ;  ;    ;  
 ;   ;  ;;   ;      ;;  ;   ;  ;;   ;  ;;  
 ;    ;; ;    ;;;;  ; ;;     ;; ;    ;; ;  
 ;                                      ;  
 ;                                 ;    ;  
 ;                                  ;;;;   
 
 (proc-doc/names
  drracket:debug:error-display-handler/stacktrace
  (->* (string? any/c)
       ((or/c false/c (listof srcloc?))
        #:definitions-text (or/c #f (is-a?/c drracket:unit:definitions-text<%>))
        #:interactions-text (or/c #f (is-a?/c drracket:rep:text<%>))
        )
       any/c)
  ((msg exn) ((stack #f)
              (defs #f)
              (ints #f)))
  @{Displays the error message represented by the string, adding
    embellishments like those that appears in the DrRacket REPL,
    specifically a clickable icon for the stack trace (if the srcloc location is not empty),
    and a clickable icon for the source of the error (read & syntax errors show their source
    locations and otherwise the first place in the stack trace is shown).
    
    If @racket[stack] is false, then the stack traces embedded in the @racket[exn] argument (if any)
    are used. Specifically, this function looks for a stacktrace via
    @racket[errortrace-key] in the continuation marks of @racket[exn] and
    @racket[continuation-mark-set->context].
    
    If @racket[stack] is not false, that stack is added to the stacks already in the exception.
    
    This should be called in the same eventspace and on the same thread as the error.})
 
 (proc-doc/names
  drracket:debug:make-debug-error-display-handler
  (-> (-> string? (or/c any/c exn?) any)
      (-> string? (or/c any/c exn?) any))
  
  (oedh)
  
  @{This function implements an error-display-handler in terms
    of another error-display-handler.
    
    See also Racket's
    @racket[error-display-handler]
    parameter.
    
    If the current-error-port is the definitions window in
    DrRacket, this error handler inserts some debugging
    annotations, calls @racket[oedh], and then highlights the
    source location of the runtime error.
    
    It looks for both stack trace information in the continuation
    marks both via the
    @racketmodname[errortrace/errortrace-key] 
    module and via 
    @racket[continuation-mark-set->context].
    
    })
 
 (proc-doc/names
  drracket:debug:hide-backtrace-window
  (-> void?)
  ()
  @{Hides the backtrace window.})
 
 (proc-doc/names
  drracket:debug:add-prefs-panel
  (-> void?)
  ()
  @{Adds the profiling preferences panel.})
 
 (proc-doc/names
  drracket:debug:make-debug-compile-handler
  (-> (-> any/c boolean? compiled-expression?)
      (-> any/c boolean? compiled-expression?))
  (oc)
  @{Returns a function suitable for use with @racket[current-compile].

    The result function first adds debugging information to
    its argument and then passes it to @racket[oc].})

 (proc-doc/names
  drracket:debug:make-debug-eval-handler
  (-> (-> any/c any) (-> any/c any))
  (oe)
  @{Returns a function suitable for use with 
    @racket[current-eval].
                                             
    The result function first adds debugging information to
    its argument and then passes it to @racket[oe].})
 
 (parameter-doc
  drracket:debug:test-coverage-enabled
  (parameter/c boolean?)
  enabled?
  @{Determines if the test-coverage annotation is added
    by the result of @racket[drracket:debug:make-debug-eval-handler].})
 
 (thing-doc
  drracket:debug:test-coverage-on-style-name
  string?
  @{The name of the 
    @racket[style%] object (in @racket[editor:get-standard-style-name])
    used to indicate a covered region of code.})
 
 (thing-doc
  drracket:debug:test-coverage-off-style-name
  string?
  @{The name of the 
    @racket[style%] object (in @racket[editor:get-standard-style-name])
    used to indicate a region of code that tests (or any code, really)
    didn't cover.})
 
 (parameter-doc
  drracket:debug:profiling-enabled
  (parameter/c boolean?)
  enabled?
  @{Determines if the profiling annotation is added
    by the result of @racket[drracket:debug:make-debug-eval-handler].})
 
 (proc-doc/names
  drracket:debug:bug-info->ticket-url
  (-> (listof (cons/c symbol? (or/c #f string?)))
      url?)
  (query)
  @{Builds a url that goes to the trac report system. 
    The @racket[query] argument is used as the 
    url's query field.})
 
 (thing-doc
  drracket:debug:small-planet-bitmap
  (is-a?/c bitmap%)
  @{The icon used in the DrRacket REPL when an exception is 
    raised that includes blame information blaming a PLaneT
    package. (Clicking the icon connects to the PLaneT bug
    report form.)})
 
 (proc-doc/names
  drracket:debug:open-and-highlight-in-file
  (->* ((or/c srcloc? (listof srcloc?)))
       ((or/c #f (cons/c (λ (x) (and (weak-box? x)
                                     (let ([v (weak-box-value x)])
                                       (or (not v)
                                           (is-a? v editor<%>)))))
                         number?)))
       void?)
  ((debug-info)
   ((edition-pair #f)))
  @{This function opens a DrRacket to display
    @racket[debug-info]. Only the src the position
    and the span fields of the srcloc are considered.
    
    The @racket[edition-pair] is used to determine if a
    warning message is shown when before opening the file.
    If the @racket[edition-pair] is not @racket[#f], it is compared
    with the result of @method[text:basic<%> get-edition-number]
    of the editor that is loaded to determine if the file has been
    edited since the source location was recorded. If so, it 
    puts up a warning dialog message to that effect.})
 
 (proc-doc/names
  drracket:debug:show-backtrace-window/edition-pairs
  (-> string?
      (listof srcloc?)
      (listof 
       (or/c 
        #f
        (cons/c (λ (x) 
                  (and (weak-box? x)
                       (let ([v (weak-box-value x)])
                         (or (not v)
                             (is-a? v editor<%>)))))
                number?)))
      (or/c #f (is-a?/c drracket:unit:definitions-text<%>))
      (or/c #f (is-a?/c drracket:rep:text<%>))
      void?)
  (error-message dis editions-pairs defs ints)
  @{Same as @racket[drracket:debug:show-backtrace-window/edition-pairs/two],
            where the @racket[_dis2] and @racket[_editions-pairs2] arguments
            are both @racket['()]})
 
 (proc-doc/names
  drracket:debug:show-backtrace-window/edition-pairs/two
  (-> string?
      (listof srcloc?)
      (listof 
       (or/c 
        #f
        (cons/c (λ (x) 
                  (and (weak-box? x)
                       (let ([v (weak-box-value x)])
                         (or (not v)
                             (is-a? v editor<%>)))))
                number?)))
      (listof srcloc?)
      (listof 
       (or/c 
        #f
        (cons/c (λ (x) 
                  (and (weak-box? x)
                       (let ([v (weak-box-value x)])
                         (or (not v)
                             (is-a? v editor<%>)))))
                number?)))
      (or/c #f (is-a?/c drracket:unit:definitions-text<%>))
      (or/c #f (is-a?/c drracket:rep:text<%>))
      void?)
  (error-message dis1 editions-pairs1 dis2 editions-pairs2 defs ints)
  @{Shows the backtrace window you get when clicking on the bug in
    DrRacket's REPL.
    
    The @racket[error-message] argument is the text of the error,
    @racket[dis1] and @racket[dis2] are the stacktrace information, 
    extracted from the
    continuation mark in the exception record, using
    @racket[errortrace-key] and using
    @racket[continuation-mark-set->context].
    
    The @racket[editions1] and @racket[editions2] arguments indicate
    the editions of any editors
    that are open editing the files corresponding to the source locations.
    The lists must have the same length as @racket[dis1] and @racket[dis2].
    
    The @racket[defs] argument should be non-@racket[#f] if there are 
    possibly stacktrace frames that contain unsaved versions of the 
    definitions window from DrRacket. Similarly, the @racket[ints] argument
    should be non-@racket[#f] if there are possibly stacktrace frames that contain
    unsaved versions of the interactions window.
    
    Use
    @racket[drracket:rep:current-rep] to get the rep during evaluation of a program.
    
    })
 
 (proc-doc
  drracket:debug:get-error-color
  (-> (is-a?/c color%))
  @{Returns the background color used to highlight errors in the definitions window
    (and other places, possibly). See also @racket[drracket:debug:get-error-color-name].
    
    The result depends on the @racket['framework:white-on-black?] preference
    setting.})

 (proc-doc
  drracket:debug:get-error-color-name
  (-> color-prefs:color-scheme-color-name?)
  @{Returns the name of the background color used to
    highlight errors in the definitions window
    (and other places, possibly).})
 
 (proc-doc/names
  drracket:debug:show-backtrace-window
  (->* (string?
        (or/c exn? 
              (listof srcloc?)))
       ((or/c #f (is-a?/c drracket:rep:text<%>))
        (or/c #f (is-a?/c drracket:unit:definitions-text<%>)))
       void?)
  ((error-message dis)
   ((rep #f)
    (defs #f)))
  @{Shows the backtrace window you get when clicking on the bug in
    DrRacket's REPL.

 If @racket[dis] is a list of @racket[srcloc?], then this function simply
 calls @racket[drracket:debug:show-backtrace-window/edition-pairs],
 passing @racket[error-message], @racket[dis], and a list of @racket[#f]
 that is as long as @racket[dis].

 If @racket[dis] is an @racket[exn:fail?], then this function calls
 @racket[drracket:debug:show-backtrace-window/edition-pairs/two], extracting
 the builtin stack trace (via @racket[continuation-mark-set->context])
 and an errortrace stack trace from
 the continuation marks in @racket[exn].

    })
 
 
 
 ;                                                              
 ;                                                              
 ;                                                              
 ;   ;;             ;;                  ;;;               ;;    
 ;   ;;             ;;                   ;;               ;;    
 ;   ;;             ;;                   ;;               ;;    
 ;   ;;;;;;  ;;;;   ;;  ;;;;;         ;;;;;  ;;;;    ;;;; ;;  ;;
 ;   ;;  ;; ;;; ;;  ;;  ;;  ;;       ;; ;;; ;;; ;;  ;;  ; ;; ;  
 ;   ;;  ;; ;;;;;;  ;;  ;;  ;;  ;;;;;;;  ;; ;;;;;;  ;;;;  ;;;;  
 ;   ;;  ;; ;;      ;;  ;;  ;;  ;;;;;;;  ;; ;;      ;;;;  ;;;;; 
 ;   ;;  ;; ;;;  ;  ;;  ;;  ;;       ;;  ;; ;;;  ; ;  ;;  ;; ;; 
 ;   ;; ;;;  ;;;;   ;;  ;;;;;        ;;;;;;  ;;;;  ;;;;   ;; ;;;
 ;                      ;;                                      
 ;                      ;;                                      
 ;                      ;;                                      
 
 (proc-doc/names
  drracket:help-desk:help-desk
  (->* ()
       ((or/c #f string?)
        (or/c #f string? (list/c string? string?))
        (or/c (is-a?/c frame%) (is-a?/c dialog%) #f))
       any)
  (()
   ((search-key #f)
    (search-context #f)
    (parent #f)))
  @{if @racket[search-key] is a string, performs a search in the docs with
 @racket[search-key] and @racket[search-context].
 Otherwise, calls @racket[send-main-page] with no arguments.

 The search may involve asking the user a question, in which case the
 dialog with the question uses @racket[parent] as its parent.
 })
 
 (proc-doc
  drracket:help-desk:goto-plt-license
  (-> void?)
  @{Opens the user's web browser and points it at the license for PLT software.})
 
 
 ;                           
 ;                           
 ;                           
 ;                   ;       
 ;                           
 ;                       ;   
 ;   ;   ;   ; ;;    ;  ;;;; 
 ;   ;   ;   ;;  ;   ;   ;   
 ;   ;   ;   ;   ;   ;   ;   
 ;   ;   ;   ;   ;   ;   ;   
 ;   ;   ;   ;   ;   ;   ;   
 ;   ;  ;;   ;   ;   ;   ;   
 ;    ;; ;   ;   ;   ;    ;; 
 ;                           
 ;                           
 ;                           
 
 
 (proc-doc/names
  drracket:unit:get-program-editor-mixin
  (-> ((subclass?/c text%) . -> . (subclass?/c text%)))
  ()
  @{Returns a mixin that must be mixed in to any
    @racket[text%] object that might contain
    program text (and thus can be in the source
    field of some syntax object).
    
    See also
    @racket[drracket:unit:add-to-program-editor-mixin].})
 
 (proc-doc/names
  drracket:unit:add-to-program-editor-mixin
  (((subclass?/c text%) . -> . (subclass?/c text%)) . -> . void?)
  (mixin)
  @{@phase[1]
     
     Adds @racket[mixin] to the result of
     @racket[drracket:unit:get-program-editor-mixin].})
 
 (proc-doc/names
  drracket:unit:open-drscheme-window
  (->* ()
       ((or/c string? #f) #:show? boolean?)
       (is-a?/c drracket:unit:frame%))
  (() ((filename #f) (show? #t)))
  
  @{Opens a DrRacket frame that displays 
    @racket[filename],
    or, if @racket[filename] is @racket[#f], an empty file.

 If @racket[show?] is @racket[#t], then the @method[top-level-window<%> show] is not
 invoked before the function returns; otherwise it is.})
 
 (proc-doc/names
  drracket:unit:add-search-help-desk-menu-item
  (->* ((is-a?/c text%) (is-a?/c menu-item-container<%>) exact-nonnegative-integer?) ((-> any)) void?)
  ((text menu position)
   ((add-sep void)))
  @{Adds a menu item to @racket[menu] that searches in Help Desk
  for the word around @racket[position] in @racket[text].
  
  If there is only whitespace around @racket[position],
  then no @racket[menu-item%]s are added, and
  @racket[add-sep] is not called. If there is something to be
  added, then @racket[add-sep] is called before the menu item is
  created.
  })
 
 (struct-doc 
  drracket:unit:teachpack-callbacks
  ([get-names (-> any/c (listof string?))]
   [add (-> any/c path-string? any/c)]
   [remove (-> path-string? any/c any/c)]
   [remove-all (-> any/c any/c)])
  @{Holds callbacks for teachpack operations. DrRacket invokes
    these functions in response to GUI operations being triggered.
    
    Each of the @racket[any/c]s that appear in the field
    contracts are actually the settings of a language.
    
    The @racket[get-names] field returns the names
    of the teachpacks in the given settings; @racket[add]
    returns a new settings that includes the @racket[path-string?]
    argument as a new teachpack; @racket[remove] removes the
    given teachpack and @racket[remove-all] removes them all.})
 
 (thing-doc
  drracket:unit:struct:teachpack-callbacks
  struct-type?
  @{This is an alias for @racket[struct:drracket:unit:teachpack-callbacks].})
 
 (thing-doc
  drracket:unit:make-teachpack-callbacks
  procedure?
  @{This is an alias for @racket[make-drracket:unit:teachpack-callbacks].})
 
 
 (proc-doc/names
  drracket:unit:find-symbol
  (-> (is-a?/c text%) exact-nonnegative-integer? string?)
  (text pos)
  @{returns a string that corresponds to the a
    symbol surrounding @racket[pos] (in @racket[text]).
    
    This is intended to be used with the ``f1'' keybinding
    for searching in the documentation, so the result is
    not always a symbol, but instead a best effort to find 
    something that is likely to be useful to search for around
    a point in the @racket[text].})
  

  
 
 ;                                            
 ;                                            
 ;                                            
 ;                           ;                
 ;                           ;                
 ;                           ;                
 ;   ; ;;  ;;     ;;;     ;; ;    ;;;    ;;;  
 ;   ;;  ;;  ;   ;   ;   ;  ;;   ;   ;  ;     
 ;   ;   ;   ;  ;     ; ;    ;  ;    ;  ;;    
 ;   ;   ;   ;  ;     ; ;    ;  ;;;;;;   ;;   
 ;   ;   ;   ;  ;     ; ;    ;  ;          ;  
 ;   ;   ;   ;   ;   ;   ;  ;;   ;         ;  
 ;   ;   ;   ;    ;;;     ;; ;    ;;;;  ;;;   
 ;                                            
 ;                                            
 ;                                            
 
 
 (proc-doc/names
  drracket:modes:add-mode
  (->* (string?
        (or/c #f (is-a?/c mode:surrogate-text<%>))
        (-> (is-a?/c drracket:rep:text%) number? boolean?)
        (-> (or/c #f (listof string?)) boolean?))
       (#:intended-to-edit-programs? boolean?)
       drracket:modes:mode?)
  ((name surrogate repl-submit matches-language)
   ([intended-to-edit-programs? #t]))
  @{Adds a mode to DrRacket. Returns a mode value
    that identifies the mode.
    
    The first argument, @racket[name], is the name
    of the mode, used in DrRacket's GUI to allow
    the user to select this mode.
    
    The @racket[surrogate] argument is set to the
    definitions text and the interactions text
    (via the
    @racket[mode:host-text set-surrogate<%>]
    method) whenever this mode is enabled.
    
    The @racket[repl-submit] procedure is called
    whenever the user types a return in the interactions
    window. It is passed the interactions editor
    and the position where the last prompt occurs.
    If it 
    returns @racket[#t], the text after the last
    prompt is treated as a program fragment and
    evaluated, according to the language settings.
    If it returns @racket[#f], the text is
    assumed to be an incomplete program fragment, and
    the keystroke is not treated specially.
    
    The @racket[matches-language] predicate is called whenever
    the language changes. If it returns @racket[#t]
    this mode is installed. It is passed the list of strings
    that correspond to the names of the language in the
    language dialog.
    
    The @racket[intended-to-edit-programs?] boolean indicates
    if this mode is intended to be for editing programs 
    (as opposed to some other kind of file content). If it is
    @racket[#f], online expansion is disabled and DrRacket
    won't look for @tt{(module} at the front of the buffer
    to try to guess the intended filename.
    
    Modes are tested in the opposite order that they are
    added. That is, the last mode to be added gets tested
    first when the filename changes or when the language
    changes.
    
    See also
    @racket[drracket:modes:get-modes].
    
    @history[#:changed "1.1" @list{Added the @racket[intended-to-edit-programs?] argument.}]
    })
 
 (struct-doc 
  drracket:modes:mode
  ([name string?]
   [surrogate (or/c #f (is-a?/c mode:surrogate-text<%>))]
   [repl-submit (-> (is-a?/c drracket:rep:text%) number? boolean?)]
   [matches-language (-> (or/c #f (listof string?)) boolean?)]
   [intended-to-edit-programs? boolean?])
  #:omit-constructor
  @{See @racket[drracket:modes:add-mode] for details on modes.
        
    @history[#:changed "1.1" @list{Added the @racket[intended-to-edit-programs?] field.}]})
 
 (thing-doc
  drracket:modes:struct:mode
  struct-type?
  @{An alias for @racket[struct:drracket:modes:mode].})

 (proc-doc/names
  drracket:modes:get-modes
  (-> (listof drracket:modes:mode?))
  ()
  @{Returns all of the modes currently added to DrRacket.
    
    Note that the @racket[_surrogate] field of the
    mode corresponding to the module language does not
    take into account the 
    @language-info-ref[definitions-text-surrogate], so it
    may not be the actual class used directly in DrRacket,
    even when the mode is active.
    
    See also
    @racket[drracket:modes:add-mode].})
 
 
 ;                      
 ;                      
 ;                      
 ;                      
 ;                      
 ;                      
 ;   ; ;   ;;;   ; ;;   
 ;   ;;   ;   ;  ;;  ;  
 ;   ;   ;    ;  ;    ; 
 ;   ;   ;;;;;;  ;    ; 
 ;   ;   ;       ;    ; 
 ;   ;    ;      ;;  ;  
 ;   ;     ;;;;  ; ;;   
 ;               ;      
 ;               ;      
 ;               ;      
 
 
 (proc-doc/names
  drracket:rep:get-welcome-delta 
  (-> (is-a?/c style-delta%))
  ()
  @{Returns a style delta that matches the style and color of the 
    phrase ``Welcome to'' in the beginning of the interactions window.})
    
 
 (proc-doc/names
  drracket:rep:get-dark-green-delta
  (-> (is-a?/c style-delta%))
  ()
  @{Returns a style delta that matches the style and color of the 
    name of a language in the interactions window.})
 
 (proc-doc
  drracket:rep:get-error-delta 
  (-> (is-a?/c style-delta%))
  @{Returns a style delta that matches the style and color of 
    errors that get shown in the interactions window.})
 
 (proc-doc/names
  drracket:rep:get-drs-bindings-keymap
  (-> (is-a?/c keymap%))
  ()
  @{Returns a keymap that binds various DrRacket-specific
    keybindings. This keymap is used in the definitions
    and interactions window.

    By default, binds C-x;o to a function that switches
    the focus between the definitions and interactions
    windows. Also binds f5 to Execute and f1 to Help Desk.})
 
 (proc-doc/names
  drracket:rep:current-rep
  (-> (or/c false/c (is-a?/c drracket:rep:text%)))
  ()
  
  @{This is a parameter whose value should not be set by tools.
    It is initialized to the repl that controls this evaluation
    in the user's thread.
    
    It only returns @racket[#f] if the program not running
    in the context of a repl (eg, the test suite window).})
 
 (proc-doc/names
  drracket:rep:current-value-port
  (-> (or/c false/c port?))
  ()
  @{This is a parameter whose value is a port that
    prints in the REPL in blue. It is used to print
    the values of toplevel expressions in the REPL.
    
    It is only initialized on the user's thread.})
 
 (parameter-doc
  drracket:rep:after-expression
  (parameter/c (or/c #f (-> any)))
  top-level-expression
  @{This parameter is used by @method[drracket:rep:text% evaluate-from-port].
    When it is a thunk, then DrRacket invokes the thunk on the user's thread
    as the last thing it does (before cleaning up).})
 
 (parameter-doc
  drracket:rep:current-language-settings
  (parameter/c drracket:language-configuration:language-settings?)
  language-settings
  @{This parameter is set (on the user's thread) to the 
    @racket[drracket:language-configuration:language-settings]
    for the currently running language.})

 (parameter-doc
  drracket:rep:module-language-initial-run
  (parameter/c boolean?)
  initial-run?
  @{The value of this parameter is @racket[#t] in the
 dynamic extent of the call to @method[drracket:rep:text% evaluate-from-port]
 that sets up the initial read-eval-print loop (which doesn't run the user's program)})
  
 
 
 ;                                                                        
 ;                                                                        
 ;                                                                        
 ;                          ;                                          ;  
 ;                          ;                                          ;  
 ;                  ;      ;                   ;                       ;  
 ;    ;; ;    ;;;  ;;;;    ;     ;;;  ;     ; ;;;;   ;;;   ; ;;     ;; ;  
 ;   ;  ;;   ;   ;  ;      ;    ;   ;  ;   ;   ;    ;   ;  ;;  ;   ;  ;;  
 ;  ;    ;  ;    ;  ;      ;   ;    ;   ; ;    ;   ;    ;  ;   ;  ;    ;  
 ;  ;    ;  ;;;;;;  ;     ;    ;;;;;;    ;     ;   ;;;;;;  ;   ;  ;    ;  
 ;  ;    ;  ;       ;     ;    ;        ; ;    ;   ;       ;   ;  ;    ;  
 ;   ;  ;;   ;      ;     ;     ;      ;   ;   ;    ;      ;   ;   ;  ;;  
 ;    ;; ;    ;;;;   ;;  ;       ;;;; ;     ;   ;;   ;;;;  ;   ;    ;; ;  
 ;       ;               ;                                                
 ;  ;    ;               ;                                                
 ;   ;;;;                                                                 
 
 
 (proc-doc
  drracket:get/extend:extend-unit-frame
  (->i ([mixin (make-mixin-contract drracket:unit:frame%)])
       ([before boolean?]
        #:name-for-changes [name-for-changes (or/c #f symbol?)])
       [result void?])
  (#t #f)
  
  @{Extends the class that is used for the frame that implements the main DrRacket window.
    
    The @racket[before] argument controls if the mixin is applied before or
    after already installed mixins.
    
    If @racket[name-for-changes] is a symbol and @racket[drracket:get/extend:allow-re-extension!]
    has been called (without a subsequent call to @racket[drracket:get/extend:disallow-re-extension!])
    then calling this function replaces any earlier mixins that have been added
    that have the same name. Otherwise, calling this with the same name
    twice is an error and calling it once @racket[drracket:get/extend:get-frame] has been
    called is an error.})
 
 (proc-doc
  drracket:get/extend:get-unit-frame
  (-> (subclass?/c drracket:unit:frame%))
  
  @{Returns a class whose objects are used for the DrRacket frames.
    
    Once this function is called, 
    @racket[drracket:get/extend:extend-unit-frame]
    raises an error, disallowing any more extensions.
    
    See also @racket[drracket:get/extend:allow-re-extension!].})

 (proc-doc
  drracket:get/extend:extend-tab
  (->i ([mixin (make-mixin-contract drracket:unit:tab<%>)])
       ([before boolean?]
        #:name-for-changes [name-for-changes (or/c #f symbol?)])
       [result void?])
  (#t #f)
  @{Like @racket[drracket:get/extend:extend-unit-frame], except it extends the class 
    that implements the tabs in DrRacket. One is created for each tab
    in a frame (each frame always has at least one tab, even if the tab bar is not shown).})
 
 (proc-doc
  drracket:get/extend:get-tab
  (-> (implementation?/c drracket:unit:tab<%>))
  
  @{Like @racket[drracket:get/extend:get-unit-frame], except it
    returns the class used for tabs.})
 
 (proc-doc
  drracket:get/extend:extend-definitions-text
  (->i ([mixin (make-mixin-contract drracket:unit:definitions-text<%>
                                    editor:standard-style-list<%>
                                    editor:info<%>
                                    racket:text<%>
                                    text:all-string-snips<%>
                                    text:file<%>
                                    text:info<%>
                                    text:wide-snip<%>)])
       ([before boolean?]
        #:name-for-changes [name-for-changes (or/c #f symbol?)])
       [result void?])
  (#t #f)
  
  @{Like @racket[drracket:get/extend:extend-unit-frame], except
         this text is used in the top window of DrRacket frames.})
 
 (proc-doc
  drracket:get/extend:get-definitions-text
  (-> (and/c (implementation?/c drracket:unit:definitions-text<%>)
             (implementation?/c editor:standard-style-list<%>)
             (implementation?/c editor:info<%>)
             (implementation?/c racket:text<%>)
             (implementation?/c text:all-string-snips<%>)
             (implementation?/c text:file<%>)
             (implementation?/c text:info<%>)
             (implementation?/c text:wide-snip<%>)))
  
  @{Like @racket[drracket:get/extend:get-unit-frame], except
         for the text that is used in the top window of DrRacket frames.})

 
 (proc-doc
  drracket:get/extend:extend-interactions-text
  (->i ([mixin (make-mixin-contract drracket:rep:text<%>)])
       ([before boolean?]
        #:name-for-changes [name-for-changes (or/c #f symbol?)])
       [result void?])
  (#t #f)
  @{Like @racket[drracket:get/extend:extend-unit-frame], except it extends the class 
    that implements the the editor in the interactions window.})
 
 (proc-doc
  drracket:get/extend:get-interactions-text
  (-> (implementation?/c drracket:rep:text<%>))
  
  @{Like @racket[drracket:get/extend:get-unit-frame] except it returns
         the class that implements the editor in the interactions window.})
 
  
 (proc-doc
  drracket:get/extend:extend-definitions-canvas
  (->i ([mixin (make-mixin-contract drracket:unit:definitions-canvas%)])
       ([before boolean?]
        #:name-for-changes [name-for-changes (or/c #f symbol?)])
       [result void?])
  (#t #f)
  
  @{Like @racket[drracket:get/extend:extend-unit-frame], except it extends the class 
    that implements the definitions window's @racket[editor-canvas%].})
 
 (proc-doc
  drracket:get/extend:get-definitions-canvas
  (-> (subclass?/c drracket:unit:definitions-canvas%))
  
  @{Like @racket[drracket:get/extend:get-unit-frame] except it returns
         the class that implements the definitions window's @racket[editor-canvas%].})
 
 (proc-doc
  drracket:get/extend:extend-interactions-canvas
  (->i ([mixin (make-mixin-contract drracket:unit:interactions-canvas%)])
       ([before boolean?]
        #:name-for-changes [name-for-changes (or/c #f symbol?)])
       [result void?])
  (#t #f)
  
  @{Like @racket[drracket:get/extend:extend-unit-frame], except it extends the class 
    that implements the interactions window's @racket[editor-canvas%].})
 
 (proc-doc
  drracket:get/extend:get-interactions-canvas
  (-> (subclass?/c drracket:unit:interactions-canvas%))
  
  @{Like @racket[drracket:get/extend:get-unit-frame] except it returns
         the class that implements the definitions window's @racket[editor-canvas%].})
 
 
 (proc-doc
  drracket:get/extend:disallow-re-extension!
  (-> void?)
  @{Once this is called, re-extension of the mixins described in this
    section is not allowed. This is the default state of mixin extension,
    but it can be changed by @racket[drracket:get/extend:allow-re-extension!].})
 
 (proc-doc
  drracket:get/extend:allow-re-extension!
  (-> void?)
  @{Once this is called, re-extension of the mixins described in this
    section are now allowed (see @racket[drracket:get/extend:extend-unit-frame]
    for details of how to effect a re-extension).
    
    This mode is intended to support a faster development cycle, not for production code.
    Specifically, the issue is that replacing mixins in this manner does not affect any
    objects that have already been create and thus 
    there can, in general, be a mixture of old and new objects in a single DrRacket.
    If some kind of systematic change to the classes is wanted, consider instead using
    the @racketmodname[racket/surrogate] library.
    
    Once an extension happens, newly created objects will use the new mixins. 
    Mostly, however, creating a new frame will create a new set of all of the objects
    that are extended in this section, so that can be used to experiment more quickly
    with changes.
    })
 
 ;                                                
 ;                                                
 ;                                                
 ;                                                
 ;    ;                       ;;;                 
 ;  ;;;                                           
 ;  ;;;; ;;; ;;;;;;;    ;;;   ;;; ;;; ;;   ;; ;;; 
 ;  ;;;; ;;;;;;;;;;;;  ;;;;;  ;;; ;;;;;;; ;;;;;;; 
 ;  ;;;  ;;;  ;;  ;;; ;;;  ;; ;;; ;;; ;;; ;;; ;;; 
 ;  ;;;  ;;;    ;;;;; ;;;     ;;; ;;; ;;; ;;; ;;; 
 ;  ;;;  ;;;  ;;; ;;; ;;;  ;; ;;; ;;; ;;; ;;; ;;; 
 ;  ;;;; ;;;  ;;; ;;;  ;;;;;  ;;; ;;; ;;; ;;;;;;; 
 ;   ;;; ;;;   ;;;;;;   ;;;   ;;; ;;; ;;;  ;; ;;; 
 ;                                            ;;; 
 ;                                        ;;;;;;  
 ;                                                
 ;                                                
 
 (proc-doc/names
  drracket:tracing:annotate
  (-> syntax? syntax?)
  (stx)
  @{Call this function to add tracing annotations to the a fully-expanded
    expression. When the program runs, DrRacket will pop open the tracing
    window to display the trace.})


;                             
;                             
;                             
;                             
;                             
;                             
;   ;;;            ;;;    ;   
;   ;;;            ;;;  ;;;   
;                       ;;;   
;   ;;;  ;;; ;;;   ;;; ;;;;;; 
;   ;;;  ;;;;;;;;  ;;; ;;;;;; 
;   ;;;  ;;;  ;;;  ;;;  ;;;   
;   ;;;  ;;;  ;;;  ;;;  ;;;   
;   ;;;  ;;;  ;;;  ;;;  ;;;   
;   ;;;  ;;;  ;;;  ;;;  ;;;   
;   ;;;  ;;;  ;;;  ;;;  ;;;;; 
;   ;;;  ;;;  ;;;  ;;;   ;;;; 
;                             
;                             
;                             
;                             
;                             

 
 (proc-doc/names
  drracket:init:original-error-display-handler
  (-> string? any/c any)
  (message exn)
  @{This is the @racket[error-display-handler] installed
 at the time that DrRacket starts up.

 DrRacket sets the @racket[error-display-handler] to one that
 shows an ``Internal Error'' dialog box.})
 
 ;                                                           
 ;                                                           
 ;                                                           
 ;   ;                                                       
 ;   ;                                                       
 ;   ;                                                       
 ;   ;   ;;;    ; ;;     ;; ;   ;   ;   ;;;     ;; ;    ;;;  
 ;   ;  ;   ;   ;;  ;   ;  ;;   ;   ;  ;   ;   ;  ;;   ;   ; 
 ;   ;      ;   ;   ;  ;    ;   ;   ;      ;  ;    ;  ;    ; 
 ;   ;   ;;;;   ;   ;  ;    ;   ;   ;   ;;;;  ;    ;  ;;;;;; 
 ;   ;  ;   ;   ;   ;  ;    ;   ;   ;  ;   ;  ;    ;  ;      
 ;   ;  ;   ;   ;   ;   ;  ;;   ;  ;;  ;   ;   ;  ;;   ;     
 ;   ;   ;;;;;  ;   ;    ;; ;    ;; ;   ;;;;;   ;; ;    ;;;; 
 ;                          ;                      ;         
 ;                     ;    ;                 ;    ;         
 ;                      ;;;;                   ;;;;          
 ;                                                                                       
 ;                                                                                       
 ;                                                                                       
 ;                           ;;; ;                                    ;                  
 ;                          ;                                                            
 ;                          ;                                    ;                       
 ;    ;;;    ;;;    ; ;;   ;;;;  ;    ;; ;   ;   ;   ; ;  ;;;   ;;;;  ;    ;;;    ; ;;   
 ;   ;   ;  ;   ;   ;;  ;   ;    ;   ;  ;;   ;   ;   ;;  ;   ;   ;    ;   ;   ;   ;;  ;  
 ;  ;      ;     ;  ;   ;   ;    ;  ;    ;   ;   ;   ;       ;   ;    ;  ;     ;  ;   ;  
 ;  ;      ;     ;  ;   ;   ;    ;  ;    ;   ;   ;   ;    ;;;;   ;    ;  ;     ;  ;   ;  
 ;  ;      ;     ;  ;   ;   ;    ;  ;    ;   ;   ;   ;   ;   ;   ;    ;  ;     ;  ;   ;  
 ;   ;   ;  ;   ;   ;   ;   ;    ;   ;  ;;   ;  ;;   ;   ;   ;   ;    ;   ;   ;   ;   ;  
 ;    ;;;    ;;;    ;   ;   ;    ;    ;; ;    ;; ;   ;    ;;;;;   ;;  ;    ;;;    ;   ;  
 ;                                       ;                                               
 ;                                  ;    ;                                               
 ;                                   ;;;;                                                
 
 (proc-doc/names
  drracket:language-configuration:get-languages
  (-> (listof (is-a?/c drracket:language:language<%>)))
  ()
  @{This can only be called after all of the tools initialization phases have completed.
    
    Returns the list of all of the languages installed in DrRacket.})
 
 (proc-doc/names
  drracket:language-configuration:add-language
  (->* ((and/c (is-a?/c drracket:language:language<%>)
               drracket:language:object/c))
       (#:allow-executable-creation? boolean?)
       void?)
  ((language) ((allow-executable-creation? #f)))
  
  @{@phase[2]
     
     Adds @racket[language] to the languages offered by DrRacket.
     
     If @racket[allow-executable-creation?] is @racket[#f], then
     choosing the @onscreen{Create Executable...} menu item results
     in a dialog box saying that executable creation is disabled.
     If it is @racket[#t], then the 
     @method[drracket:language:language<%> create-executable]
     is called when that menu item is selected (after checking
     to make sure the file is saved).})
 
 (proc-doc/names
  drracket:language-configuration:get-settings-preferences-symbol
  (-> symbol?)
  ()
  @{Returns the symbol that is used to store the user's language
    settings. Use as an argument to either
    @racket[preferences:get]
    or
    @racket[preferences:set].})
 
 (struct-doc
  drracket:language-configuration:language-settings
  ([language (or/c (is-a?/c drracket:language:language<%>) 
                   drracket:language:object/c)]
   [settings any/c])
  @{This struct pairs together a language and some specific settings
    for the language.
    
    The settings is a language-specific record that holds a
    value describing a parameterization of the language.})
 
 (thing-doc
  drracket:language-configuration:struct:language-settings
  struct-type?
  @{An alias for @racket[struct:drracket:language-configuration:language-settings].})
 
 (thing-doc
  drracket:language-configuration:make-language-settings
  procedure?
  @{An alias for @racket[make-drracket:language-configuration:language-settings].})
 
 (proc-doc/names
  drracket:language-configuration:language-dialog
  (->* (boolean? drracket:language-configuration:language-settings?)
       ((or/c false/c (is-a?/c top-level-window<%>)))
       (or/c false/c drracket:language-configuration:language-settings?))
  ((show-welcome? language-settings-to-show)
   ((parent #t)))
  @{Opens the language configuration dialog.
    See also
    @racket[drracket:language-configuration:fill-language-dialog].
    
    The @racket[show-welcome?] argument determines if
    if a ``Welcome to DrRacket'' message and some
    natural language buttons are shown.
    
    The @racket[language-settings-to-show] argument
    must be some default language settings that the dialog
    is initialized to.
    If unsure of a default, the currently set language
    in the user's preferences can be obtained via:
    @racketblock[
      (preferences:get 
       (drracket:language-configuration:get-settings-preferences-symbol))]
    
    The @racket[parent] argument is used as the parent
    to the dialog.
    
    The result if @racket[#f] when the user cancells the dialog, and
    the selected language if they hit ok.})
 
 (proc-doc/names
  drracket:language-configuration:fill-language-dialog
  (->*
   ((is-a?/c vertical-panel%)
    (is-a?/c area-container<%>)
    drracket:language-configuration:language-settings?)
   ((or/c false/c (is-a?/c top-level-window<%>))
    (-> symbol? void?))
   (values (-> (is-a?/c drracket:language:language<%>))
           (-> any/c)
           (-> any/c (is-a?/c mouse-event%) any)))
  ((panel button-panel language-setting)
   ((re-center #f)
    (ok-handler void)))
  @{This procedure accepts two parent panels and
    fills them with the contents of the language dialog.
    It is used to include language configuration controls
    in some larger context in another dialog.
    
    The @racket[panel] argument is the main panel where the
    language controls will be placed.
    The function adds buttons to the @racket[button-panel]
    to revert a language to its default settings and to
    show the details of a language.
    
    The @racket[language-setting] is the default
    language to show in the dialog.
    
    The @racket[re-center] argument is used when the @onscreen{Show Details}
    button is clicked. If that argument is a @racket[top-level-window<%>],
    the @onscreen{Show Details} callback will recenter the window each time
    it is clicked. Otherwise, the argument is not used.
    
    @racket[ok-handler] is a function that is in charge of interfacing the OK
    button. It should accept a symbol message: @racket['enable] and
    @racket['disable] to toggle the button, and @racket['execute] to run
    the desired operation. (The language selection dialog also uses an
    internal @racket['enable-sync] message.)
    
    The first two results of the function return a language object
    and a settings for that language, as chosen by the user using the dialog.
    The final function should be called when keystrokes are typed in the
    enclosing frame. It is used to implement the shortcuts that choose the
    two radio buttons in the language dialog.
    })
 
 (proc-doc
  drracket:language:register-capability
  (->i ([s symbol?]
        [the-contract contract?]
        [default (the-contract) the-contract])
       ()
       [res void?])
  @{Registers a new capability with a default value for each language
    and a contract on the values the capability might have.
    
    By default, these capabilities are registered as DrRacket starts up:
    @(let-syntax ([cap
                   (λ (stx)
                     (syntax-case stx ()
                       [(_ block? key contract default desc ...)
                        (begin
                          (unless (boolean? (syntax-e #'block?))
                            (raise-syntax-error 'cap "expected a boolean" stx #'block?))
                          #`(item
                             @index[(list "drracket capability" (symbol->string 'key))]{@racket['key]}
                             " " @racket[:]
                             #,(if (syntax-e #'block?)
                                   #'@racketblock[contract]
                                   #'(list " " @racket[contract] " "))
                             @racket[= default]
                             " --- " desc ...))]))])
       (itemize
        @cap[#f drracket:check-syntax-button boolean? #t]{controls the visiblity of
                                                       the check syntax button}
        @cap[#f drracket:language-menu-title string?
             (string-constant scheme-menu-name)]{
          controls the name of the menu just to the right of the language
          menu (named ``Racket'' by default)}
        @cap[#t drscheme:define-popup
             (or/c #f
                   (list/c string? string? string?)
                   (non-empty-listof (list/c string? string? string?))
                   (non-empty-listof (list/c string? string? string?
                                             (listof (or/c 'case-insensitive 'delimited))))
                   (cons/c string? string?))
             (list "(define" "(define ...)" "δ")]{
          specifies the prefix that the define popup should look for and what
          label it should have, or @racket[#f] if it should not appear at all.
          Text is found only when it is not in a comment or string literal.
          
          If the list of three strings alternative is used, the first string is
          the prefix that is looked for when finding definitions. The second
          and third strings are used as the label of the control, in horizontal
          and vertical mode, respectively.

          If it is a list of lists, then multiple prefixes are used
          for the definition pop-up. The name of the popup menu is based only on the
          first element of the list. When a nested list contains a list of symbols,
          the symbols refine the matching strategy: @scheme['case-insensitive] for
          case-insensitive matching, and @scheme['delimited] to indicate that the
          matched text's edges must coincide with forward and backward expression
          nagivation.
          
          The pair of strings alternative is deprecated. If it is used, 
          the pair @racket[(cons a-str b-str)] is the same as @racket[(list a-str b-str "δ")].
         }
        @cap[#f drscheme:help-context-term (or/c false/c string?) #f]{
          specifies a context query for documentation searches that are
          initiated in this language, can be @racket[#f] (no change to the
          user's setting) or a string to be used as a context query (note: the
          context is later maintained as a cookie, @racket[""] is different
          from @racket[#f] in that it clears the stored context)}
        @cap[#f drscheme:special:insert-fraction boolean? #t]{
          determines if the insert fraction menu item in the special menu is
          visible}
        @cap[#f drscheme:special:insert-lambda boolean? #t]{
          determines if the insert lambda menu item in the special menu is
          visible}
        @cap[#f drscheme:special:insert-large-letters boolean? #t]{
          determines if the insert large letters menu item in the special menu
          is visible}
        @cap[#f drscheme:special:insert-image boolean? #t]{
          determines if the insert image menu item in the special menu is
          visible}
        @cap[#f drscheme:special:insert-comment-box boolean? #t]{
          determines if the insert comment box menu item in the special menu
          is visible}
        @cap[#f drscheme:special:insert-gui-tool boolean? #t]{
          determines if the insert gui menu item in the special menu is
          visible}
        @cap[#f drscheme:special:slideshow-menu-item boolean? #t]{
          determines if the insert pict box menu item in the special menu is
          visible}
        @cap[#f drscheme:special:insert-text-box boolean? #t]{
          determines if the insert text box menu item in the special menu is
          visible}
        @cap[#f drscheme:special:xml-menus boolean? #t]{
          determines if the insert scheme box, insert scheme splice box, and
          the insert xml box menu item in the special menu are visible}
        @cap[#f drscheme:autocomplete-words (listof string?) '()]{
          determines the list of words that are used when completing words in
          this language}
        @cap[#t drscheme:tabify-menu-callback
             (or/c (-> (is-a?/c text%)
                       number?
                       number?
                       void?)
                   #f)
             (λ (t a b) (send t tabify-selection a b))]{
          is used as the callback when the ``Reindent'' or ``Reindent All''
          menu is selected. The first argument is the editor, and the second
          and third are a range in the editor.}
          ))})
 
 (proc-doc/names
  drracket:language:capability-registered? 
  (-> symbol? boolean?)
  (s)
  @{Indicates if
    @racket[drracket:language:register-capability]
    has been called with @racket[s].})
 (proc-doc
  drracket:language:get-capability-default
  (->i ([s (and/c symbol? drracket:language:capability-registered?)])
       ()
       [res (s) (drracket:language:get-capability-contract s)])
  @{Returns the default for a particular capability.})
 (proc-doc/names
  drracket:language:get-capability-contract
  (-> (and/c symbol? drracket:language:capability-registered?)
      contract?)
  (s)
  @{Returns the contract for a given capability, which was specified
    when @racket[drracket:language:register-capability] was called.})
 
 
 ;                                                           
 ;                                                           
 ;                                                           
 ;   ;                                                       
 ;   ;                                                       
 ;   ;                                                       
 ;   ;   ;;;    ; ;;     ;; ;   ;   ;   ;;;     ;; ;    ;;;  
 ;   ;  ;   ;   ;;  ;   ;  ;;   ;   ;  ;   ;   ;  ;;   ;   ; 
 ;   ;      ;   ;   ;  ;    ;   ;   ;      ;  ;    ;  ;    ; 
 ;   ;   ;;;;   ;   ;  ;    ;   ;   ;   ;;;;  ;    ;  ;;;;;; 
 ;   ;  ;   ;   ;   ;  ;    ;   ;   ;  ;   ;  ;    ;  ;      
 ;   ;  ;   ;   ;   ;   ;  ;;   ;  ;;  ;   ;   ;  ;;   ;     
 ;   ;   ;;;;;  ;   ;    ;; ;    ;; ;   ;;;;;   ;; ;    ;;;; 
 ;                          ;                      ;         
 ;                     ;    ;                 ;    ;         
 ;                      ;;;;                   ;;;;          
 
 
 (proc-doc/names
  drracket:language:add-snip-value
  (->* ((-> any/c boolean?)
        (-> any/c (is-a?/c snip%)))
       ((-> any/c))
       void?)
  ((test-value convert-value)
   ((setup-thunk void)))
  @{Registers a handler to convert values into snips as they are printed in the REPL.
    
    The @racket[test-snip] argument is called to determine if this handler can convert the value 
    and the @racket[convert-value] argument is called to build a snip. 
    The (optional) @racket[setup-thunk] is called just after the user's namespace and other 
    setings are built, but before any of the user's code is evaluated.
    
    All three functions are called on the user's thread and with the user's settings.})
 
 (proc-doc/names
  drracket:language:extend-language-interface
  (-> interface?
      (make-mixin-contract drracket:language:language<%>)
      void?)
  (interface default-implementation)
  
  @{@phase[1]
     
     Each language added passed to
     @racket[drracket:language-configuration:add-language]
     must implement @racket[interface]. 
     
     The @racket[default-implementation] is a mixin
     that provides a default implementation of 
     @racket[interface]. Languages that are unaware of
     the specifics of @racket[extension] use
     @racket[default-implementation] via
     @racket[drracket:language:get-default-mixin].})
 
 (proc-doc
  drracket:language:get-default-mixin
  (-> (make-mixin-contract drracket:language:language<%>))
  
  @{@phase[2]
     
     The result of this function is the composite of all of the 
     @racket[default-implementation] arguments passed
     to
     @racket[drracket:language:extend-language-interface].})
 
 (proc-doc/names
  drracket:language:get-language-extensions
  (-> (listof interface?))
  ()
  @{@phase[2]
     
     Returns a list of the interfaces passed to
     @racket[drracket:language:extend-language-interface].})
 
 (proc-doc/names
  drracket:language:put-executable
  ((is-a?/c top-level-window<%>) 
   path? 
   (or/c boolean? 'launcher 'standalone 'distribution)
   boolean? 
   string? 
   . -> . (or/c false/c path?))
  (parent program-filename mode mred? title)
  @{Calls the GRacket primitive
    @racket[put-file]
    with arguments appropriate for creating an executable
    from the file @racket[program-filename]. 
    
    The arguments @racket[mred?] and @racket[mode] indicates
    what type of executable this should be (and the dialog
    may be slightly different on some platforms, depending
    on these arguments). For historical reasons, @racket[#f]
    is allowed for @racket[mode] as an alias for @racket['launcher], and
    @racket[#t] is allowed for @racket[mode] as an alias for @racket['stand-alone].
    
    The @racket[title] argument is used as the title to the primitive
    @racket[put-file]
    or
    @racket[get-directory]
    primitive.})
 
 (proc-doc/names
  drracket:language:create-executable-gui
  ((or/c #f (is-a?/c top-level-window<%>))
   (or/c #f string?)
   (or/c #t 'launcher 'standalone 'distribution)
   (or/c #t 'mzscheme 'mred)
   . -> .
   (or/c #f
         (list/c (or/c 'no-show 'launcher 'stand-alone 'distribution)
                 (or/c 'no-show 'mred 'mzscheme)
                 string?
                 (listof (cons/c symbol? any/c)))))
  (parent program-name show-type show-base)
  @{Opens a dialog to prompt the user about their choice of executable.
    If @racket[show-type] is @racket[#t], the user is prompted about
    a choice of executable: stand-alone,
    launcher, or distribution; otherwise, the symbol determines the type.
    If @racket[show-base]
    is @racket[#t], the user is prompted about a choice of base
    binary: mzscheme or mred; otherwise the symbol determines the base.
    
    The @racket[program-name] argument is used to construct the default
    executable name in a platform-specific manner.
    
    The @racket[parent] argument is used for the parent of the dialog.
    
    The result of this function is @racket[#f] if the user cancel's
    the dialog and a list of three items indicating what options
    they chose. If either @racket[show-type] or @racket[show-base]
    was not @racket[#t], the corresponding result will be @racket['no-show],
    otherwise it will indicate the user's choice.})
 
 (proc-doc/names
  drracket:language:create-module-based-stand-alone-executable 
  ((or/c path? string?)
   (or/c path? string?) any/c any/c any/c boolean? boolean?
   . -> .
   void?)
  (program-filename
   executable-filename
   module-language-spec
   transformer-module-language-spec
   init-code
   gui?
   use-copy?)
  
  @{This procedure creates a stand-alone executable in the file
    @racket[executable-filename] that runs the program
    @racket[program-filename]. 
    
    The arguments
    @racket[module-language-spec] and
    @racket[transformer-module-language-spec] specify the 
    settings of the initial namespace, both the transformer
    portion and the regular portion. Both may be @racket[#f]
    to indicate there are no initial bindings.
    
    The @racket[init-code] argument is an s-expression representing
    the code for a module. This module is expected to provide
    the identifier @racket[init-code], bound to a procedure of no
    arguments. That module is required and the @racket[init-code]
    procedure is executed to initialize language-specific
    settings before the code in @racket[program-filename] runs.
    
    The @racket[gui?] argument indicates if a GRacket or Racket
    stand-alone executable is created.
    
    The @racket[use-copy?] argument indicates if the initial
    namespace should be populated with
    @racket[namespace-require/copy] or
    @racket[namespace-require]. })
 
 (proc-doc/names
  drracket:language:create-module-based-distribution
  ((or/c path? string?)
   (or/c path? string?) any/c any/c any/c boolean? boolean?
   . -> .
   void?)
  (program-filename
   distribution-filename
   module-language-spec
   transformer-module-language-spec
   init-code
   gui?
   use-copy?)
  
  @{Like
    @racket[drracket:language:create-module-based-stand-alone-executable], 
    but packages the stand-alone executable into a distribution.})
 
 (proc-doc/names
  drracket:language:create-distribution-for-executable
  ((or/c path? string?) 
   boolean?
   (-> path? void?)
   . -> .
   void?)
  (distribution-filename
   gui?
   make-executable)
  
  @{Creates a distribution where the given
    @racket[make-executable] procedure
    creates the stand-alone executable to be distributed. 
    The @racket[make-executable] procedure is given the name of the 
    executable to create. The @racket[gui?] argument is needed in case the
    executable's name (which @racket[drracket:language:create-distribution-for-executable] 
    must generate) depends on the type of executable. During the distribution-making 
    process, a progress dialog is shown to the user, and the user can click an 
    @onscreen{Abort} button that sends a break to the current thread.})
 
 (proc-doc/names
  drracket:language:create-module-based-launcher
  ((or/c path? string?) (or/c path? string?) any/c any/c any/c boolean? boolean?
                        . -> .
                        void?)
  (program-filename
   executable-filename
   module-language-spec
   transformer-module-language-spec
   init-code
   gui?
   use-copy?)
  
  @{This procedure is identical to 
    @racket[drracket:language:create-module-based-stand-alone-executable], 
    except that it creates a launcher instead of a
    stand-alone executable.})
 
 (proc-doc/names
  drracket:language:simple-module-based-language-convert-value
  (-> any/c drracket:language:simple-settings? any)
  (value settings)
  @{The result can be either one or two values. The first result is
    the converted value. The second result is @racket[#t] if the converted
    value should be printed with @racket[write] (or @racket[pretty-write]),
    @racket[#f] if the converted result should be printed with
    @racket[print] (or @racket[pretty-print]); the default second
    result is @racket[#t].

    The result of this function depends on the 
    @racket[simple-settings-printing-style] field of @racket[settings].
    If it is @racket['print], the
    result is @racket[(values value #f)]. If it is @racket['write] or @racket['trad-write],
    the result is just @racket[value]. Otherwise, the result is produce by 
    adjusting the @racket[constructor-style-printing] and @racket[show-sharing] 
    parameters based on @racket[settings], setting @racket[current-print-convert-hook]
    to ignore snips, and then applying @racket[print-convert] to @racket[value].})
 
 (proc-doc/names
  drracket:language:setup-printing-parameters
  (-> (-> any) drracket:language:simple-settings? (or/c number? 'infinity) any)
  (thunk settings width)
  @{Equivalent to @racket[(drracket:language:make-setup-printing-parameters)].})
 
 (proc-doc/names
  drracket:language:make-setup-printing-parameters
  (-> (-> (-> any) drracket:language:simple-settings? (or/c number? 'infinity) any))
  ()
  @{Returns a procedure that accepts three arguments: a thunk, settings, and 
    a pretty-print width. The result procedure, when invoked sets all of the 
    @racket[pretty-print] and @racket[print-convert] parameters
    either to the defaults to values based on @racket[settings]
    and then invokes @racket[thunk], returning what it returns.
    
    When @racket[drracket:language:make-setup-printing-parameters] is invoked,
    it @racket[dynamic-require]s @racketmodname[pict/convert] and
    closes over the results, using them to convert values when the resulting
    procedure is invoked.
    })
 
 (struct-doc
  drracket:language:text/pos
  ([text (is-a?/c text%)]
   [start exact-nonnegative-integer?]
   [end exact-nonnegative-integer?])
  @{A record that tracks a @racket[text%] object and a range inside it.})
 
 (thing-doc
  drracket:language:make-text/pos
  procedure?
  @{An alias for @racket[make-drracket:language:text/pos].})
 
 (thing-doc
  drracket:language:struct:text/pos
  struct-type?
  @{An alias for @racket[struct:drracket:language:text/pos].})
 
 (struct-doc
  drracket:language:simple-settings
  ([case-sensitive boolean?]
   [printing-style (or/c 'constructor
                         'quasiquote
                         'write
                         'trad-write
                         'print)]
   [fraction-style (or/c 'mixed-fraction
                         'mixed-fraction-e
                         'repeating-decimal
                         'repeating-decimal-e)]
   [show-sharing boolean?]
   [insert-newlines boolean?]
   [annotations (or/c 'none 'debug 'debug/profile 'test-coverage)])
  @{A struct that tracks commonly used settings for a language.})
 
 (thing-doc
  drracket:language:make-simple-settings
  procedure?
  @{An alias for @racket[make-drracket:language:simple-settings].})
 
 (thing-doc
  drracket:language:struct:simple-settings
  struct-type?
  @{An alias for @racket[struct:drracket:language:simple-settings].})
 
 (proc-doc/names
  drracket:language:simple-settings->vector
  (drracket:language:simple-settings? . -> . vector?)
  (simple-settings)
  
  @{Constructs a vector whose elements are the fields of @racket[simple-settings].}))
