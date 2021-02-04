#lang racket/base

(require errortrace/errortrace-key
         racket/unit
         racket/contract
         errortrace/stacktrace
         racket/class
         racket/path
         racket/bool
         racket/gui/base
         string-constants
         framework
         framework/private/srcloc-panel
         "embedded-snip-utils.rkt"
         drracket/private/drsig
         "bindings-browser.rkt"
         "stack-checkpoint.rkt"
         (prefix-in : (only-in "stack-checkpoint.rkt" srcloc->edition/pair get-editions))
         "ellipsis-snip.rkt"
         "local-member-names.rkt"
         "eval-helpers-and-pref-init.rkt"
         net/sendurl
         net/url
         racket/match
         mrlib/include-bitmap
         mrlib/close-icon
         images/compile-time
         pkg/lib
         pkg/gui
         (for-syntax images/icons/misc images/icons/style images/icons/control images/logos)
         (for-syntax racket/base)
         (submod "frame.rkt" install-pkg))

(define orig (current-output-port))
(define (oprintf . args) (apply fprintf orig args))

(define base-phase
  (variable-reference->module-base-phase (#%variable-reference)))

(provide debug@)
(define-unit debug@
  (import [prefix drracket:rep: drracket:rep^]
          [prefix drracket:frame: drracket:frame^]
          [prefix drracket:unit: drracket:unit/int^]
          [prefix drracket:language: drracket:language^]
          [prefix drracket:language-configuration: drracket:language-configuration/internal^]
          [prefix drracket:init: drracket:init^]
          [prefix drracket: drracket:interface^])
  (export drracket:debug/int^)
  
  ;                                                          
  ;                                                          
  ;   ;                                                      
  ;   ;                             ;                        
  ;   ;                                                      
  ;   ; ;;   ;   ;   ;; ;         ;;;     ;;;;   ;;;   ; ;;  
  ;   ;;  ;  ;   ;  ;  ;;           ;    ;      ;   ;  ;;  ; 
  ;   ;   ;  ;   ;  ;   ;           ;    ;      ;   ;  ;   ; 
  ;   ;   ;  ;   ;  ;   ;           ;    ;      ;   ;  ;   ; 
  ;   ;   ;  ;   ;  ;   ;           ;    ;      ;   ;  ;   ; 
  ;   ;;  ;  ;  ;;  ;  ;;           ;    ;      ;   ;  ;   ; 
  ;   ; ;;    ;; ;   ;; ;           ;     ;;;;   ;;;   ;   ; 
  ;                     ;                                    
  ;                  ;;;                                     
  ;                                                          

  (define (cms->srclocs cms)
    (map 
     errortrace-stack-item->srcloc
     (continuation-mark-set->list cms errortrace-key)))
  
  ;; type debug-source = (union symbol (instanceof editor<%>))
  
  ;; original-output-port : output-port
  ;; for debugging -- be sure to print to here, not the current output port
  (define original-output-port (current-output-port))
  
  ;; error-delta : (instanceof style-delta%)
  (define error-delta (make-object style-delta% 'change-style 'italic))
  (send error-delta set-delta-foreground (make-object color% 255 0 0))
  
  ;; get-error-color : -> (instanceof color%)
  (define (get-error-color)
    (color-prefs:lookup-in-color-scheme
     'drracket:error-background-highlighting))
  
  (define arrow-cursor (make-object cursor% 'arrow))
  (define (clickable-snip-mixin snip%)
    (class snip%
      (init-rest args)
      (inherit get-flags set-flags get-admin get-extent)
      
      (define callback void)
      (define/public (set-callback cb) (set! callback cb))
      (define/public (get-callback) callback)
      
      (define grabbed? #f)
      (define in-bounds? #f)
      
      (define/private (set-clicked new-grabbed? new-in-bounds? dc)
        (define needs-invalidate?
          (or (not (equal? grabbed? new-grabbed?))
              (not (equal? new-in-bounds? in-bounds?))))
        (set! grabbed? new-grabbed?)
        (set! in-bounds? new-in-bounds?)
        (when needs-invalidate?
          (invalidate dc)))
      
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (super draw dc x y left top right bottom dx dy draw-caret)
        (when (and in-bounds? grabbed?)
          (let ([brush (send dc get-brush)]
                [pen (send dc get-pen)])
            (let-values ([(w h) (get-w/h dc)])
              (send dc set-brush (send the-brush-list find-or-create-brush "black" 'hilite))
              (send dc set-pen (send the-pen-list find-or-create-pen "white" 1 'transparent))
              (send dc draw-rectangle x y w h)
              (send dc set-pen pen)
              (send dc set-brush brush)))))
      
      (define/override (on-event dc x y editorx editory evt)
        (define-values (w h) (get-w/h dc))
        (define in-bounds? (and (<= (- (send evt get-x) x) w)
                                (<= (- (send evt get-y) y) h)))
        (cond
          [(send evt button-down? 'left)
           (set-clicked #t in-bounds? dc)]
          [(send evt button-up? 'left)
           (let ([admin (send this get-admin)])
             (when admin
               (send (send admin get-editor) set-caret-owner #f 'global)))
           (when (and grabbed? in-bounds?)
             (callback this))
           (set-clicked #f in-bounds? dc)]
          [else
           (set-clicked grabbed? in-bounds? dc)]))
      
      (define/private (invalidate dc)
        (let ([admin (get-admin)])
          (when admin
            (let-values ([(w h) (get-w/h dc)])
              (send admin needs-update this 0 0 w h)))))
      
      (define/private (get-w/h dc)
        (let ([wb (box 0)]
              [hb (box 0)])
          ;; know that the snip is the same size everywhere, 
          ;; so just use (0,0) for its position
          (get-extent dc 0 0 wb hb #f #f #f #f)
          (values (unbox wb)
                  (unbox hb))))
      
      (define/override (adjust-cursor dc x y editorx editory event)
        arrow-cursor)
      
      (apply super-make-object args)
      (set-flags (cons 'handles-events (get-flags)))))
  
  (define clickable-image-snip%  (clickable-snip-mixin image-snip%))
  (define clickable-string-snip%
    (class (clickable-snip-mixin snip%)
      (define/override (get-extent dc x y wb hb db sb lb rb)
        (define-values (w h d a) (send dc get-text-extent str))
        (set-box/f wb w)
        (set-box/f hb h)
        (set-box/f db d)
        (set-box/f sb a)
        (set-box/f lb 0)
        (set-box/f rb 0))
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (define font (send dc get-font))
        (send dc set-font (send the-font-list find-or-create-font
                                (send font get-point-size)
                                (send font get-face)
                                (send font get-family)
                                (send font get-style)
                                (send font get-weight)
                                #t
                                (send font get-smoothing)
                                #f
                                (send font get-hinting)))
        (send dc draw-text str x y)
        (send dc set-font font))
      
      (inherit get-callback set-callback)
      (init-field str)
      (define/override (copy)
        (let ([n (new clickable-string-snip% [str str])])
          (send n set-callback (get-callback))
          n))
      (define/override (write f)
        (define bts (string->bytes/utf-8 str))
        (send f put (bytes-length bts) bts))
      (super-new)
      (inherit set-snipclass)
      (set-snipclass clickable-string-snipclass)))
  (define (set-box/f b v) (when (box? b) (set-box! b v)))
  (define clickable-string-snipclass
    (new (class snip-class%
           (define/override (read f)
             (define str (bytes->string/utf-8 (or (send f get-unterminated-bytes) #"")))
             (new clickable-string-snip% [str str]))
           (super-new))))
  (send clickable-string-snipclass set-classname "drclickable-string-snipclass")
  (send clickable-string-snipclass set-version 0)
  (send (get-the-snip-class-list) add clickable-string-snipclass)

  (define (srcloc->edition/pair defs ints srcloc [port-name-matches-cache #f])
    (define (is-drracket-frame? x) (is-a? x drracket:unit:frame<%>))
    (:srcloc->edition/pair defs ints srcloc is-drracket-frame? port-name-matches-cache))
  (define get-editions :get-editions)
  
  ;; make-note% : string -> (union class #f)
  (define (make-note% filename bitmap)
    (and (send bitmap ok?)
         (letrec ([note%
                   (class clickable-image-snip%
                     (inherit get-callback)
                     (define/public (get-image-name) filename)
                     (define stack1 #f)
                     (define stack2 #f)
                     (define/public (set-stacks s1 s2) (set! stack1 s1) (set! stack2 s2))
                     (define/public (get-stacks) (values stack1 stack2))
                     (define/override (copy) 
                       (let ([n (new note%)])
                         (send n set-callback (get-callback))
                         (send n set-stacks stack1 stack2)
                         n))
                     (super-make-object bitmap))])
           note%)))
  
  (define file-note%
    (make-note% "stop-22x22.png" (compiled-bitmap (stop-sign-icon #:color halt-icon-color))))
  (define bug-note%
    (make-note% "stop-multi.png" (compiled-bitmap (stop-signs-icon #:color halt-icon-color))))
  
  (define mf-note% (make-note% "mf.gif" (include-bitmap (lib "icons/mf.gif") 'gif)))
  (define small-planet-bitmap (compiled-bitmap (planet-logo #:height (default-icon-height))))
  (define planet-note% (make-note% "small-planet.png" small-planet-bitmap))
  (define install-note% 
    (class clickable-image-snip%
      (inherit get-callback)
      (define/override (copy) 
        (let ([n (new install-note%)])
          (send n set-callback (get-callback))
          n))
      (super-new)))
  
  ;; display-stats : (syntax -> syntax)
  ;; count the number of syntax expressions & number of with-continuation-marks in an 
  ;; expanded expression ... except that it counts keywords, too.
  ;; returns its argument.
  ;(define (display-stats stx)
  ;  (let ([exps 0]
  ;        [wcms 0])
  ;    (let loop ([stx stx])
  ;      (kernel-syntax-case stx ()
  ;        [(#%with-continuation-mark key mark body)
  ;         (set! wcms (+ wcms 1))
  ;         (loop #`body)]
  ;        [(subexps ...)
  ;         (set! exps (+ exps 1))
  ;         (for-each loop (syntax->list stx))]
  ;        [exp
  ;         (set! exps (+ exps 1))]))
  ;    (eprintf "exps: ~v\nwcms: ~v\n" exps wcms))
  ;  stx)
  
  ;; make-debug-eval-handler : (sexp -> value) -> sexp -> value
  ;; adds debugging information to `sexp' and calls `oe'
  (define (make-debug-eval-handler oe)
    (let ([debug-tool-eval-handler
           (λ (orig-exp)
             (if (compiled-expression? (if (syntax? orig-exp)  
                                           (syntax-e orig-exp)  
                                           orig-exp))
                 (oe orig-exp)
                 (let loop ([exp (if (syntax? orig-exp)
                                     orig-exp
                                     (namespace-syntax-introduce
                                      (datum->syntax #f orig-exp)))])
                   (let ([top-e (expand-syntax-to-top-form exp)]) 
                     (syntax-case top-e (begin) 
                       [(begin expr ...)
                        ;; Found a `begin', so expand/eval each contained 
                        ;; expression one at a time 
                        (let i-loop ([exprs (syntax->list #'(expr ...))]
                                     [last-one (list (void))])
                          (cond
                            [(null? exprs) 
                             (apply values last-one)]
                            [else 
                             (i-loop (cdr exprs)
                                     (call-with-values 
                                      (λ () 
                                        (call-with-continuation-prompt
                                         (λ () (loop (car exprs)))
                                         (default-continuation-prompt-tag)
                                         (λ args
                                           (apply
                                            abort-current-continuation 
                                            (default-continuation-prompt-tag)
                                            args))))
                                      list))]))]
                       [_else 
                        ;; Not `begin', so proceed with normal expand and eval 
                        (oe (errortrace-annotate top-e #f))])))))])
      debug-tool-eval-handler))

  (define (make-debug-compile-handler orig)
    (make-debug-compile-handler/errortrace-annotate orig errortrace-annotate))
  
  ;; make-debug-error-display-handler : 
  ;;    (string (union TST exn) -> void) -> string (union TST exn) -> void
  ;; adds in the bug icon, if there are contexts to display
  (define (make-debug-error-display-handler orig-error-display-handler)
    (define (debug-error-display-handler msg exn)
      
      (call-with-exception-handler
       (λ (exn)
         (printf "-- ~s\n" exn)
         (when (exn? exn)
           (for ([e (in-list (continuation-mark-set->context
                              (exn-continuation-marks exn)))])
             (printf "  ~s\n" e)))
         exn)
       (λ ()
         (let ([rep (drracket:rep:current-rep)])
           (cond
             [rep
              (error-display-handler/stacktrace msg exn)]
             [else 
              (orig-error-display-handler msg exn)])))))
    debug-error-display-handler)
  
  ;; error-display-handler/stacktrace : string any (or/c #f viewable-stack? (listof srcloc)) -> void
  ;; =User=
  (define (error-display-handler/stacktrace 
           msg exn 
           [pre-stack #f]
           #:interactions-text [ints (drracket:rep:current-rep)]
           #:definitions-text [defs (let ([rep (drracket:rep:current-rep)])
                                      (and rep
                                           (send rep get-definitions-text)))])
    (define stack1
      (cond
        [(viewable-stack? pre-stack) pre-stack]
        [(list? pre-stack) (srclocs->viewable-stack pre-stack (filter values (list ints defs)))]
        [(and (exn? exn)
              (continuation-mark-set? (exn-continuation-marks exn)))
         (cms->errortrace-viewable-stack (exn-continuation-marks exn)
                                         (filter values (list ints defs)))]
        [else (empty-viewable-stack)]))
    (define stack2
      (if (exn? exn)
          (cms->builtin-viewable-stack (exn-continuation-marks exn)
                                       (filter values (list ints defs))
                                       #:share-cache stack1)
          (empty-viewable-stack)))
    (define port-name-matches-cache (make-hasheq))
    (define src-locs (get-exn-source-locs defs exn stack1 stack2))

    (print-planet-icon-to-stderr exn)
    (unless (exn:fail:user? exn)
      (unless (exn:fail:syntax? exn)
        (unless (and (empty-viewable-stack? stack1) (empty-viewable-stack? stack2))
          (unless (zero? (error-print-context-length))
            (print-bug-to-stderr msg stack1 stack2))))
      (display-srclocs-in-error src-locs stack1))
    (display-error-message exn msg)
    (when (exn:fail:syntax? exn)
      (unless (error-print-source-location)
        (show-syntax-error-context (current-error-port) exn)))
    (print-pkg-icon-to-stderr exn)
    (newline (current-error-port))
    (flush-output (current-error-port))
    (when (and ints
               (eq? (current-error-port) 
                    (send ints get-err-port)))
      (parameterize ([current-eventspace drracket:init:system-eventspace])
        (queue-callback
         (λ ()
           ;; need to make sure that the user's eventspace is still the same
           ;; and still running here?
           (send ints highlight-errors src-locs
                 (viewable-stack->red-arrows-backtrace-srclocs
                  (if (empty-viewable-stack? stack1)
                      stack2
                      stack1)))
             
           (when (and ints (exn:fail:out-of-memory? exn))
             (define frame (send ints get-top-level-window))
             (send ints no-user-evaluation-dialog frame #f #t #f)))))))
  
  (define (render-message lines)
    (define collected (collect-hidden-lines lines))
    (for ([x (in-list collected)]
          [i (in-naturals)])
      (unless (zero? i) (newline (current-error-port)))
      (cond
        [(string? x)
         (display x (current-error-port))]
        [(pair? x)
         (define line (list-ref x 0))
         (define to-show-later (list-ref x 1))
         (write-string line (current-error-port) 0 (- (string-length line) 4))
         (write-special (new ellipsis-snip% [extra to-show-later]) (current-error-port))
         (display ":" (current-error-port))])))
  
  (define (display-error-message exn msg)
    (cond
      [(exn:fail? exn)
       (define lines (regexp-split #rx"\n" msg))
       (cond
         [(ellipsis-candidate? lines)
          (render-message lines)]
         [else
          (display msg (current-error-port))])]
      [else 
       (display msg (current-error-port))]))
    
  (define (ellipsis-candidate? lines)
    (and ((length lines) . > . 1)
         (for/or ([x (in-list (cdr lines))])
           (regexp-match ellipsis-error-message-field x))))
    
  ;; =User=
  (define (print-planet-icon-to-stderr exn)
    (when (exn:fail:contract:blame? exn)
      (let ([table (parse-gp exn
                             (blame-positive
                              (exn:fail:contract:blame-object exn)))])
        (when table
          (let ([gp-url (bug-info->ticket-url table)])
            (when planet-note%
              (when (port-writes-special? (current-error-port))
                (let ([note (new planet-note%)])
                  (send note set-callback (λ (snp) 
                                            ;; =Kernel= =Handler=
                                            (drracket:unit:forget-saved-bug-report table)
                                            (send-url (url->string gp-url))))
                  (parameterize ([current-eventspace drracket:init:system-eventspace])
                    (queue-callback
                     (λ ()
                       (drracket:unit:record-saved-bug-report table))))
                  (write-special note (current-error-port))
                  (display #\space (current-error-port))))))))))
  
  
  ;; =Kernel= =User=
  (define (bug-info->ticket-url table)
    (make-url 
     "http"
     #f
     "planet.racket-lang.org"
     #f
     #t
     (list (make-path/param "trac" '())
           (make-path/param "newticket" '()))
     table
     #f))
  
  ;; =User=
  (define (parse-gp exn gp)
    (match gp
      [`(planet ,fn (,user ,package ,planet-version ...))
       (list (cons 'component (format "~a/~a" user package))
             (cons 'keywords "contract violation")
             (cons 'pltversion (version))
             (cons 'planetversion
                   (cond
                     [(null? planet-version) ""]
                     [(null? (cdr planet-version))
                      (format "~s" `(,(car planet-version) ?))]
                     [else
                      (format "~s" `(,(car planet-version) ,(cadr planet-version)))]))
             (cons 'description (exn->trace exn)))]
      [else #f]))
  
  ;; =User=
  (define (print-pkg-icon-to-stderr exn)
    (when (exn:missing-module? exn)
      (define mod ((exn:missing-module-accessor exn) exn))
      (define pkgs (pkg-catalog-suggestions-for-module mod))
      (define update-pkgs-node (new clickable-string-snip% [str "[update catalog]"]))
      (define (get-tlw snp)
        (define admin (send snp get-admin))
        (define canvas (and admin (send (send admin get-editor) get-canvas)))
        (and canvas (send canvas get-top-level-window)))
      (send update-pkgs-node set-callback 
            (λ (snp)
              (pkg-catalog-update-local/simple-status-dialog
               #:parent (get-tlw snp))))
      (cond
        [(null? pkgs)
         (when (port-writes-special? (current-error-port))
           (display "\n  no package suggestions are available " (current-error-port))
           (write-special update-pkgs-node (current-error-port)))]
        [else
         (display "\n  packages that provide the missing module:" (current-error-port))
         (when (port-writes-special? (current-error-port))
           (display " " (current-error-port))
           (write-special update-pkgs-node (current-error-port)))
         (for ([pkg (in-list pkgs)])
           (eprintf "\n    ~a" pkg)
           (when (port-writes-special? (current-error-port))
             (define note (new clickable-string-snip% [str "[install]"]))
             (send note set-callback 
                   (λ (snp) 
                     ;; =Kernel= =Handler=
                     (define tlw (get-tlw snp))
                     (install-pkg
                      tlw
                      (lambda (thunk)
                        (parameterize ([error-display-handler 
                                        drracket:init:original-error-display-handler])
                          (thunk)))
                      #:package-to-offer pkg)))
             (eprintf "  ")
             (write-special note (current-error-port))))])))
  
  ;; =User=
  (define (exn->trace exn)
    (let ([sp (open-output-string)])
      (parameterize ([current-error-port sp])
        (drracket:init:original-error-display-handler (exn-message exn) exn))
      (get-output-string sp)))
  
  ;; =User=
  (define (print-bug-to-stderr msg viewable-stack1 viewable-stack2)
    (when (port-writes-special? (current-error-port))
      (define note (make-note-to-print-to-stderr msg viewable-stack1 viewable-stack2))
      (when note
        (write-special note (current-error-port))
        (display #\space (current-error-port)))))

  ;; =Kernel= =User=
  (define (make-note-to-print-to-stderr msg viewable-stack1 viewable-stack2)
    (cond
      [bug-note%
       (define note (new bug-note%))
       (send note set-stacks viewable-stack1 viewable-stack2)
       (send note set-callback
             (λ (snp)
               (show-backtrace-window/viewable-stacks msg
                                                      viewable-stack1
                                                      viewable-stack2)))
       note]
      [else #f]))

  ;; display-srclocs-in-error : (listof src-loc) viewable-stack? -> void
  ;; prints out the src location information for src-to-display
  ;; as it would appear in an error message
  (define (display-srclocs-in-error srcs-to-display a-viewable-stack)
    (unless (null? srcs-to-display)
      (define src-to-display (car srcs-to-display))
      (match-define (srcloc src line col pos _span) src-to-display)
      (define (do-icon)
        (when file-note%
          (when (port-writes-special? (current-error-port))
            (define note (new file-note%))
            (send note set-callback 
                  (λ (snp) (open-and-highlight-in-file srcs-to-display a-viewable-stack)))
            (write-special note (current-error-port))
            (display #\space (current-error-port)))))
      (define (do-src)
        (cond
          [(path? src)
           (define-values (n-cd n-src)
             (with-handlers ([exn:fail? (λ (x) (values (current-directory) src))])
               (values (normalize-path (current-directory)) (normalize-path src))))
           (display (path->string (find-relative-path n-cd n-src))
                    (current-error-port))]
          [else
           (define name
             (cond
               [(string? src) src]
               [(symbol? src) (symbol->string src)]
               [else "<unsaved editor>"]))
           (display name (current-error-port))]))
      (define (do-line/col) (eprintf ":~a:~a" line col))
      (define (do-pos) (eprintf "::~a" pos))
      (define src-loc-in-defs/ints?
        (let ([rep (drracket:rep:current-rep)])
          (and rep
               (is-a? rep drracket:rep:text<%>)
               (let ([defs (send rep get-definitions-text)])
                 (or (send rep port-name-matches? src)
                     (eq? rep src)
                     (send defs port-name-matches? src)
                     (eq? defs src))))))
      (cond
        [(and src line col)
         (do-icon)
         (unless src-loc-in-defs/ints?
           (do-src)
           (do-line/col)
           (display ": " (current-error-port)))]
        [(and src pos)
         (do-icon)
         (unless src-loc-in-defs/ints?
           (do-src)
           (do-pos)
           (display ": " (current-error-port)))])))
  
  ;; find-src-to-display : exn (union #f (listof srcloc))
  ;;                    -> (listof srclocs)
  ;; finds the source location to display, choosing between
  ;; the stack trace and the exception record.
  (define (find-src-to-display exn cms)
    (let ([has-info?
           (λ (srcloc)
             (ormap (λ (f) (f srcloc))
                    (list srcloc-column
                          srcloc-line
                          srcloc-position
                          srcloc-source
                          #;srcloc-span)))])  ;; don't consider span alone to count as `info'
      (cond
        [(and (exn:srclocs? exn)
              (ormap has-info? ((exn:srclocs-accessor exn) exn)))
         ((exn:srclocs-accessor exn) exn)]
        [(pair? cms) (list (car cms))]
        [else '()])))
  
  ;; show-syntax-error-context : 
  ;; display the source information associated with a syntax error (if present)
  (define (show-syntax-error-context port exn)
    (let ([error-text-style-delta (make-object style-delta%)]
          [send-out
           (λ (msg f) 
             (if (port-writes-special? (current-error-port))
                 (let ([snp (make-object string-snip% msg)])
                   (f snp)
                   (write-special snp (current-error-port)))
                 (display msg (current-error-port))))])
      (send error-text-style-delta set-delta-foreground (make-object color% 200 0 0))
      (define (show-one expr)
        (display " " (current-error-port))
        (send-out (format "~s" (syntax->datum expr))
                  (λ (snp)
                    (send snp set-style
                          (send (editor:get-standard-style-list) find-or-create-style
                                (send (editor:get-standard-style-list) find-named-style "Standard")
                                error-text-style-delta)))))
      (define exprs (exn:fail:syntax-exprs exn))
      (define (show-in)
        (send-out " in:"
                  (λ (snp)
                    (send snp set-style
                          (send (editor:get-standard-style-list) find-named-style
                                (editor:get-default-color-style-name))))))
      (cond
        [(null? exprs) (void)]
        [(null? (cdr exprs))
         (show-in)
         (show-one (car exprs))]
        [else
         (show-in)
         (for-each (λ (expr)
                     (display "\n " (current-error-port))
                     (show-one expr))
                   exprs)])))
  
  
  ;; insert/clickback : (instanceof text%) (union string (instanceof snip%)) (-> void)
  ;; inserts `note' and a space at the end of `rep'
  ;; also sets a clickback on the inserted `note' (but not the space).
  (define (insert/clickback rep note clickback)
    (let ([before (send rep last-position)])
      (send rep insert (if (string? note)
                           note
                           (send note copy))
            before before)
      (let ([after (send rep last-position)])
        (send rep insert #\space after after)
        (send rep set-clickback before after
              (λ (txt start end)
                (clickback))))))
  
  ;; Note that this is not necessarily the same format used by `make-st-mark`
  ;; which is unspecified.
  (define (special-source-handling-for-drr src)
    (define rep (drracket:rep:current-rep))
    (cond
      [rep
       (define defs (send rep get-definitions-text))
       (cond
         [(send rep port-name-matches? src)
          (send rep get-port-name)]
         [(send defs port-name-matches? src)
          (send defs get-port-name)]
         [else #f])]
      [(is-a? src editor<%>) src]
      [else #f]))
  (define with-mark (make-with-mark special-source-handling-for-drr))
  
  ;; current-backtrace-window : (union #f (instanceof frame:basic<%>))
  ;; the currently visible backtrace window, or #f, if none
  (define current-backtrace-window #f)
  
  ;; reset-backtrace-window : -> void
  ;; effect: updates current-backtrace-window
  ;; closes the current backtrace window and creates a new (unshown) one
  (define (reset-backtrace-window)
    (when current-backtrace-window
      (send current-backtrace-window close)
      (set! current-backtrace-window #f))
    
    (set! current-backtrace-window 
          (make-object backtrace-frame%
            (string-constant backtrace-window-title)
            #f
            (preferences:get 'drracket:backtrace-window-width)
            (preferences:get 'drracket:backtrace-window-height)
            (preferences:get 'drracket:backtrace-window-x)
            (preferences:get 'drracket:backtrace-window-y))))
  
  ;; hide-backtrace-window : -> void
  (define (hide-backtrace-window)
    (when current-backtrace-window
      (send current-backtrace-window close)
      (set! current-backtrace-window #f)))
  
  ;; backtrace-frame% : (extends frame:basic<%>)
  (define backtrace-frame%
    (class (drracket:frame:basics-mixin (frame:standard-menus-mixin frame:basic%))
      (define/override (on-size x y)
        (preferences:set 'drracket:backtrace-window-width x)
        (preferences:set 'drracket:backtrace-window-height y)
        (super on-size x y))
      (define/override (on-move x y)
        (preferences:set 'drracket:backtrace-window-x x)
        (preferences:set 'drracket:backtrace-window-y y)
        (super on-move x y))
      (define/override (edit-menu:between-find-and-preferences edit-menu) (void))
      (define/override (edit-menu:between-select-all-and-find edit-menu) (void))
      (define/override (file-menu:between-save-as-and-print file-menu) (void))
      (define/augment (on-close) 
        (set! current-backtrace-window #f)
        (inner (void) on-close))
      (super-new)))
  
  ;; show-backtrace-window : string
  ;;                         (listof srcloc?)
  ;;                         -> 
  ;;                         void
  (define (show-backtrace-window error-text dis/exn [ints #f] [defs #f])
    (cond
      [(exn? dis/exn)
       (define a-viewable-stack
         (cms->builtin-viewable-stack (exn-continuation-marks dis/exn)
                                      (filter values (list ints defs))))
       (show-backtrace-window/viewable-stacks
        error-text
        a-viewable-stack
        (cms->errortrace-viewable-stack (exn-continuation-marks dis/exn)
                                        (filter values (list ints defs))
                                        #:share-cache a-viewable-stack))]
      [else
       (define (is-drracket-frame? x) (is-a? x drracket:unit:frame<%>))
       (show-backtrace-window/viewable-stacks
        error-text
        (dis+edition->viewable-stack
         dis/exn
         (get-editions (make-hasheq)
                       defs ints dis/exn
                       is-drracket-frame?)
         (list defs ints))
        (empty-viewable-stack))]))
  
  (define (show-backtrace-window/edition-pairs error-text dis editions defs ints)
    (show-backtrace-window/viewable-stacks
     error-text
     (dis+edition->viewable-stack dis editions (list defs ints))
     (dis+edition->viewable-stack '() '() '())
     defs ints))
  
  (define (show-backtrace-window/edition-pairs/two error-text dis1 editions1 dis2 editions2 defs ints)
    (show-backtrace-window/viewable-stacks
     error-text
     (dis+edition->viewable-stack dis1 editions1 (list defs ints))
     (dis+edition->viewable-stack dis2 editions2 (list defs ints))))

  (define (show-backtrace-window/viewable-stacks error-text orig-viewable-stack1 orig-viewable-stack2)
    (define viewable-stack1 (copy-viewable-stack orig-viewable-stack1))
    (define viewable-stack2 (copy-viewable-stack orig-viewable-stack2))
    (reset-backtrace-window)
    (define both-non-empty?
      (and (not (empty-viewable-stack? viewable-stack1))
           (not (empty-viewable-stack? viewable-stack2))))
    
    (define tab-panel 
      (if both-non-empty?
          (new tab-panel% 
               [choices (list "Errortrace" "Builtin")]
               [parent (send current-backtrace-window get-area-container)]
               [callback
                (λ (a b) 
                  (send tab-panel change-children
                        (λ (l) (if (zero? (send tab-panel get-selection))
                                   (list ec1)
                                   (list ec2)))))])
          (new-vertical-panel% [parent (send current-backtrace-window get-area-container)])))
    (define ec1 (add-ec/text viewable-stack1 tab-panel error-text))
    (define ec2 (add-ec/text viewable-stack2 tab-panel error-text))
    (when both-non-empty?
      (send tab-panel change-children (λ (l) (list ec1)))))

  (define (add-ec/text viewable-stack tab-panel error-text)
    (cond
      [(empty-viewable-stack? viewable-stack) #f]
      [else
       (define text1 (new (text:wide-snip-mixin
                           (editor:standard-style-list-mixin
                            text:hide-caret/selection%))))
       (define ec1 (new (canvas:color-mixin canvas:wide-snip%)
                        [parent tab-panel]
                        [editor text1]))
       (add-one-set-to-frame text1 ec1 error-text viewable-stack)
       ec1]))

  (define (add-one-set-to-frame text ec error-text a-viewable-stack)
    (define (show-next-dis)
      (define start-pos (send text get-start-position))
      (define end-pos (send text get-end-position))
      (send text begin-edit-sequence)
      (send text set-position (send text last-position))
      (define-values (items more-to-show?) (viewable-stack-get-next-items! a-viewable-stack))
      (for ([item (in-list items)])
        (show-frame ec text
                    (car item) (cdr item)
                    a-viewable-stack))
                  
      ;; add 'more frames' link
      (when more-to-show?
        (define end-of-current (send text last-position))
        (send text insert #\newline)
        (define hyper-start (send text last-position))
        (send text insert (string-constant more-stack-frames))
        (define hyper-end (send text last-position))
        (send text change-style (gui-utils:get-clickback-delta
                                 (preferences:get 'framework:white-on-black?))
              hyper-start hyper-end)
        (send text set-clickback
              hyper-start hyper-end
              (λ x
                (send text begin-edit-sequence)
                (send text lock #f)
                (send text delete end-of-current (send text last-position))
                (show-next-dis)
                (send text set-position 
                      (send text last-position)
                      (send text last-position))
                (send text lock #t)
                (send text end-edit-sequence)))
                          
        (send text insert #\newline)
        (send text set-paragraph-alignment (send text last-paragraph) 'center))
                  
      (send text set-position start-pos end-pos)
      (send text end-edit-sequence))
    (send current-backtrace-window set-alignment 'center 'center)
    (send current-backtrace-window reflow-container)
    (send text auto-wrap #t)
    (send text set-autowrap-bitmap #f)
    (send text insert error-text)
    (send text insert "\n\n")
    (send text change-style error-delta 0 (- (send text last-position) 1))
    (show-next-dis)
    (send text set-position 0 0)
    (send text lock #t)
    (send text hide-caret #t)
    (send current-backtrace-window show #t))

  (define (change-regular-style text start-pos end-pos)
    (send text change-style
          (send (send text get-style-list) find-named-style
                (editor:get-default-color-style-name))
          start-pos
          end-pos))

  ;; show-frame : (instanceof editor-canvas%)
  ;;              (instanceof text%) 
  ;;              st-mark   // see format description at `with-mark`
  ;;              def ints  // definitions and interactions texts
  ;;              viewable-stack?
  ;;              -> 
  ;;              void 
  ;; shows one frame of the continuation
  (define (show-frame editor-canvas text di skip-count a-viewable-stack)
    (match-define (srcloc debug-source line column start span) di)
    (define fn (get-filename debug-source))
    (define start-pos (send text last-position))
      
    ;; make hyper link to the file
    (send text insert (format "~a:~a:~a" fn line column))
    (define end-pos (send text last-position))
    (send text insert " ")
    (change-regular-style text start-pos end-pos)
    (send text change-style 
          (gui-utils:get-clickback-delta (preferences:get 'framework:white-on-black?))
          start-pos 
          end-pos)
    (send text set-clickback
          start-pos end-pos
          (λ (ed start end)
            (open-and-highlight-in-file (list di) a-viewable-stack)))

    (unless (zero? skip-count)
      (define before (send text last-position))
      (send text insert " skipped ")
      (send text insert (number->string skip-count))
      (send text insert " duplicate frame")
      (unless (= skip-count 1)
        (send text insert "s"))
      (change-regular-style text before (send text last-position)))
    (send text insert #\newline)
      
    (when (and start span)
      (insert-context editor-canvas text a-viewable-stack di)
      (send text insert #\newline)))
  
  ;; insert-context : (instanceof editor-canvas%)
  ;;                  (instanceof text%)
  ;;                  debug-info
  ;;                  number
  ;;                  -> 
  ;;                  void
  (define (insert-context editor-canvas text a-viewable-stack a-srcloc)
    (match-define (srcloc file line col start span) a-srcloc)
    (define-values (from-text close-text)
      (cond
        [(viewable-stack-matching-editor a-viewable-stack a-srcloc)
         =>
         (λ (txt) (values txt void))]
        [(path? file)
         (define normalized-file
           (with-handlers ((exn:fail? (λ (x) #f)))
             (normal-case-path (normalize-path file))))
         (cond
           [(not normalized-file) (values #f void)]
           [(send (group:get-the-frame-group)
                  locate-file
                  normalized-file)
            =>
            (λ (frame)
              (cond
                [(is-a? frame drracket:unit:frame%)
                 (let loop ([tabs (send frame get-tabs)])
                   (cond
                     [(null? tabs) (values #f void)]
                     [else
                      (let* ([tab (car tabs)]
                             [defs (send tab get-defs)])
                        (if (with-handlers ((exn:fail? (λ (x) #f)))
                              (equal? 
                               (normalize-path (normal-case-path (send defs get-filename)))
                               normalized-file))
                            (values defs void)
                            (loop (cdr tabs))))]))]
                [(is-a? frame frame:editor<%>)
                 (values (send frame get-editor) void)]
                [else (values #f void)]))]
           [(path? normalized-file)
            (define text (new (editor:standard-style-list-mixin text:basic%)))
            (cond
              [(send text load-file normalized-file)
               (change-regular-style text 0 (send text last-position))
               (values text
                       (λ () (send text on-close)))]
              [else
               (values #f (λ () (void)))])]
           [else
            (values #f void)])]
        [(is-a? file editor<%>)
         (values file void)]
        [else (values #f void)]))
    (when from-text
      (let* ([finish (+ start span -1)]
             [context-text (copy/highlight-text from-text start finish)])
        (send context-text lock #t)
        (send context-text hide-caret #t)
        (send text insert "  ")
        (let ([snip (make-object editor-snip% context-text)])
          (send snip use-style-background #t)
          (send editor-canvas add-wide-snip snip)
          (let ([p (send text last-position)])
            (send text insert snip p p)
            (send text insert #\newline)
            (change-regular-style text p (add1 p)))))
      (close-text)))

  ;; copy/highlight-text : text number number -> text
  ;; copies the range from `start' to `finish', including the entire paragraph at
  ;; each end and highlights the characters corresponding the original range,
  ;; in the resulting text
  (define (copy/highlight-text from-text start finish)
    (let* ([to-text (new text:standard-style-list%)]
           [para-start-pos (send from-text paragraph-start-position 
                                 (send from-text position-paragraph start))]
           [para-end-pos (send from-text paragraph-end-position
                               (send from-text position-paragraph 
                                     finish))]
           [from-start (- start para-start-pos)]
           [from-end (+ from-start (- finish start))])
      (send from-text split-snip para-start-pos)
      (send from-text split-snip para-end-pos)
      (let loop ([snip (send from-text find-snip para-start-pos 'after-or-none)])
        (when (and snip
                   (< (send from-text get-snip-position snip) para-end-pos))
          (send to-text insert (send snip copy))
          (loop (send snip next))))
      (send to-text highlight-range (max 0 (- from-start 1)) from-end (get-error-color) #f 'high)
      to-text))
  
  ;; get-filename : debug-source -> string
  (define (get-filename file)
    (cond
      [(symbol? file) (symbol->string file)]
      [(path? file) (path->string file)]
      [(is-a? file editor<%>)
       (get-filename-from-editor file)]))
  
  ;; get-filename-from-editor : (is-a?/c editor<%>) -> string
  (define (get-filename-from-editor editor)
    (let* ([untitled (string-constant unknown-debug-frame)]
           [canvas (send editor get-canvas)]
           [frame (and canvas (send canvas get-top-level-window))])
      (if (is-a? frame drracket:unit:frame%)
          (let ([filename (send (send frame get-definitions-text) get-filename)])
            (cond
              [(and filename (eq? editor (send frame get-interactions-text)))
               (format (string-constant files-interactions) filename)]
              [(eq? editor (send frame get-interactions-text))
               (string-constant current-interactions)]
              [filename filename]
              [else (string-constant current-definitions)]))
          (or (send editor get-filename) 
              untitled))))
  
  ;; open-and-highlight-in-file : (or/c srcloc (listof srcloc))
  ;;                              (or/c #f (cons/c weak-box nat) viewable-stack?)
  ;;                           -> void
  (define (open-and-highlight-in-file raw-srcloc [edition-pair #f])
    (define srclocs (if (srcloc? raw-srcloc) (list raw-srcloc) raw-srcloc))
    (define sources (filter values (map srcloc-source srclocs)))
    (unless (null? sources)
      (define debug-source (car sources))
      (define same-src-srclocs
        (filter (λ (x) (eq? debug-source (srcloc-source x)))
                srclocs))
      (define frame
        (cond
          [(path? debug-source) (handler:edit-file debug-source)]
          [(and (symbol? debug-source)
                (text:lookup-port-name debug-source))
           =>
           (lambda (editor)
             (get-enclosing-editor-frame editor))]
          [else #f]))
      (define editor
        (cond
          [(path? debug-source)
           (cond
             [(and frame (is-a? frame drracket:unit:frame%))
              (send frame get-definitions-text)]
             [(and frame (is-a? frame frame:editor<%>))
              (send frame get-editor)]
             [else #f])]
          [(and (symbol? debug-source)
                (text:lookup-port-name debug-source))
           =>
           values]
          [else #f]))
      (define rep (and (is-a? frame drracket:unit:frame%)
                       (send frame get-interactions-text)))
      (when frame
        (send frame show #t))
      (define out-of-date?
        (cond
          [(not edition-pair) #f]
          [(pair? edition-pair)
           (define wbv (weak-box-value (car edition-pair)))
           (and wbv
                (eq? editor wbv)
                (not (= (cdr edition-pair)
                        (send editor get-edition-number))))]
          [else
           (viewable-stack-out-of-date-editor? edition-pair
                                               (car same-src-srclocs))]))
                                               
      (when out-of-date?
        (message-box (string-constant drscheme)
                     (string-constant editor-changed-since-srcloc-recorded)
                     frame
                     '(ok caution)
                     #:dialog-mixin frame:focus-table-mixin))
      (when (and rep editor)
        (when (is-a? editor text:basic<%>)
          (send rep highlight-errors same-src-srclocs '())
          (send editor set-caret-owner #f 'global)))))
  
  
  
  
  ;                                                                                      
  ;                                                                                      
  ;                                                                                      
  ;                                                                                      
  ;                                                                                      
  ;   ;                  ;                                                               
  ;  ;;;;   ;;;    ;;;  ;;;;       ;;;    ;;;   ;     ;  ;;;   ; ;  ;;;     ;; ;    ;;;  
  ;   ;    ;   ;  ;      ;        ;   ;  ;   ;   ;   ;  ;   ;  ;;  ;   ;   ;  ;;   ;   ; 
  ;   ;   ;    ;  ;;     ;       ;      ;     ;  ;   ; ;    ;  ;       ;  ;    ;  ;    ; 
  ;   ;   ;;;;;;   ;;    ;       ;      ;     ;   ; ;  ;;;;;;  ;    ;;;;  ;    ;  ;;;;;; 
  ;   ;   ;          ;   ;       ;      ;     ;   ; ;  ;       ;   ;   ;  ;    ;  ;      
  ;   ;    ;         ;   ;        ;   ;  ;   ;     ;    ;      ;   ;   ;   ;  ;;   ;     
  ;    ;;   ;;;;  ;;;     ;;       ;;;    ;;;      ;     ;;;;  ;    ;;;;;   ;; ;    ;;;; 
  ;                                                                            ;         
  ;                                                                       ;    ;         
  ;                                                                        ;;;;          
  
  
  (define test-coverage-enabled (make-parameter #f))
  
  (define current-test-coverage-info (make-thread-cell #f))
  
  (define (test-coverage-point body expr phase)
    (cond
      [(and (test-coverage-enabled)
            (zero? phase)
            (should-annotate? expr phase))
       ;; initialize the hash holding test coverage results
       (unless (hash? (thread-cell-ref current-test-coverage-info))
         (define rep (drracket:rep:current-rep))
         (when rep
           (define ut (eventspace-handler-thread (send rep get-user-eventspace)))
           (when (eq? ut (current-thread))
             (define ht (make-hasheq))
             (thread-cell-set! current-test-coverage-info ht)
             (send rep set-test-coverage-info ht))))
       (define ht (thread-cell-ref current-test-coverage-info))
       (cond
         [(hash? ht) ;; the initialization may have failed, give up in that case
          (define v (mcons #f #f))
          (hash-set! ht expr v) ;; record as point that might get executed
          (define update-coverage #`(#%plain-app set-mcar! #,v #t))
          (syntax-case expr (#%plain-module-begin)
            [(_mod _name _init-import (#%plain-module-begin . _body))
             (drop-in-sequence body '(tl tl tl hd tl) update-coverage)]
            [_else
             #`(begin #,update-coverage #,body)])]
         [else body])]
      [else body]))

  (define (drop-in-sequence stx path to-add)
    (let loop ([stx stx]
               [path path])
      (cond
        [(null? path)
         (cons to-add stx)]
        [(syntax? stx)
         (define dstx (disarm stx))
         (syntax-rearm
          (datum->syntax
           dstx
           (loop (syntax-e dstx) path)
           dstx
           dstx)
          stx)]
        [(pair? stx)
         (case (car path)
           [(hd) (cons (loop (car stx) (cdr path)) (cdr stx))]
           [(tl) (cons (car stx) (loop (cdr stx) (cdr path)))])])))

  (define (disarm orig) (syntax-disarm orig drracket:init:system-inspector))
  
  (define test-coverage-interactions-text<%>
    (interface ()
      set-test-coverage-info
      get-test-coverage-info))
  
  (define test-coverage-tab<%>
    (interface ()
      show-test-coverage-annotations ;; hash-table (union #f style) (union #f style) boolean -> void
      get-test-coverage-info-visible?
      ask-about-clearing-test-coverage?))

  (define test-coverage-frame<%>
    (interface ()))

  (define test-coverage-frame-mixin
    (mixin (drracket:unit:frame<%>) (test-coverage-frame<%>)
      (define test-coverage-entirely-covered-message-state #f)
      (define/augment (on-tab-change from-tab to-tab)
        (inner (void) on-tab-change from-tab to-tab)
        (update-test-coverage-entirely-covered-message to-tab))
      (inherit get-current-tab
               begin-container-sequence
               end-container-sequence)
      (define/public (update-test-coverage-entirely-covered-message [tab (get-current-tab)])
        (define new-val (and (preferences:get 'drracket:coverage-show-overview-bar)
                             (send tab get-test-coverage-entirely-covered-message-state)))
        (unless (equal? new-val test-coverage-entirely-covered-message-state)
          (set! test-coverage-entirely-covered-message-state new-val)
          (begin-container-sequence)
          (create-entirely-covered-panel-gui)
          (cond
            [test-coverage-entirely-covered-message-state
             (send entirely-covered-parent-panel
                   change-children
                   (λ (l) (append (remove entirely-covered-panel l)
                                  (list entirely-covered-panel))))]
            [else
             (send entirely-covered-parent-panel
                   change-children
                   (λ (l) (remove entirely-covered-panel l)))])
          (end-container-sequence)))

      (define (update-test-coverage-callback p v)
        (update-test-coverage-entirely-covered-message))
      (preferences:add-callback 'drracket:coverage-show-overview-bar
                                update-test-coverage-callback
                                #t)

      (define entirely-covered-parent-panel #f)
      (define entirely-covered-panel #f)
      (define entirely-covered-checkbox #f)
      (define entirely-covered-checkbox-callback
        ;; must not be a private method -- need the frame
        ;; to hold onto a pointer to this function
        (λ (p v)
          (send entirely-covered-checkbox set-value v)))
      (define/private (create-entirely-covered-panel-gui)
        (unless entirely-covered-panel
          (set! entirely-covered-panel (new-horizontal-panel%
                                            [stretchable-height #f]
                                            [parent entirely-covered-parent-panel]))
          (new message%
               [label (string-constant test-coverage-entirely-covered)]
               [parent entirely-covered-panel]
               [stretchable-width #t])
          (set! entirely-covered-checkbox
                (new check-box%
                     [label (string-constant test-coverage-next-time-check-box)]
                     [parent entirely-covered-panel]
                     [callback
                      (λ (cb v)
                        (preferences:set 'drracket:coverage-show-overview-bar
                                         (send cb get-value)))]))
          (define hide-button
            (new close-icon%
                 [callback
                  (λ ()
                    (send (get-current-tab) set-test-coverage-entirely-covered-message-state #f)
                    (update-test-coverage-entirely-covered-message))]
                 [parent entirely-covered-panel]))
          (send entirely-covered-checkbox set-value
                (preferences:get 'drracket:coverage-show-overview-bar))
          (preferences:add-callback 'drracket:coverage-show-overview-bar
                                    entirely-covered-checkbox-callback
                                    #t)))
      
      (define/override (make-root-area-container cls parent)
        (set! entirely-covered-parent-panel
              (super make-root-area-container vertical-pane% parent))
        (make-object cls entirely-covered-parent-panel))
      (super-new)))
  
  (define test-coverage-interactions-text-mixin
    (mixin (drracket:rep:text<%> text:basic<%>) (test-coverage-interactions-text<%>)
      (inherit get-context)
      (field [test-coverage-info #f]
             [test-coverage-on-style #f]
             [test-coverage-off-style #f]
             [ask-about-reset? #f]
             [test-coverage-all-covered-message-visible? #f])
      (define/public set-test-coverage-info
        (λ (ht [on-style #f] [off-style #f] [ask? #t])
          (set! test-coverage-info ht)
          (set! test-coverage-on-style on-style)
          (set! test-coverage-off-style off-style)
          (set! ask-about-reset? ask?)))
      (define/public (get-test-coverage-info) 
        test-coverage-info)
      
      (inherit get-top-level-window)
      (define/augment (after-many-evals)
        (when (and test-coverage-info
                   (not (drracket:rep:module-language-initial-run)))
          (send (get-context) show-test-coverage-annotations
                test-coverage-info
                test-coverage-on-style
                test-coverage-off-style
                ask-about-reset?))
        (inner (void) after-many-evals))

      (define/augment (on-execute rout)
        (inner (void) on-execute rout)
        (set-test-coverage-info #f))
      
      (super-new)))
  
  (define test-coverage-definitions-text-mixin
    (mixin ((class->interface text%) drracket:unit:definitions-text<%>) ()
      (inherit get-canvas get-tab)
      
      (define/private (clear-test-coverage?)
        (if (preferences:get 'drracket:test-coverage-ask-about-clearing?)
            (let ([msg-box-result
                   (message-box/custom
                    (string-constant drscheme)
                    (string-constant test-coverage-clear?)
                    (string-constant yes)
                    (string-constant no)
                    (string-constant test-coverage-clear-and-do-not-ask-again)
                    (send (get-canvas) get-top-level-window)
                    '(default=1)
                    2
                    #:dialog-mixin frame:focus-table-mixin)])
              (case msg-box-result
                [(1) #t]
                [(2) #f]
                [(3)
                 (preferences:set 'drracket:test-coverage-ask-about-clearing? #f)
                 #t]))
            #t))
      
      (define/public (clear-test-coverage)
        (let ([tab (get-tab)])
          (when (send tab get-test-coverage-info-visible?)
            (send tab clear-test-coverage-display)
            (let ([it (send tab get-ints)])
              (when (is-a? it test-coverage-interactions-text<%>)
                (send it set-test-coverage-info #f))))))
      
      (define/private (can-clear-coverage?)
        (let ([tab (get-tab)])
          (or (not tab)
              (not (send tab get-test-coverage-info-visible?))
              (not (send tab ask-about-clearing-test-coverage?))
              (clear-test-coverage?))))
      
      (define/augment (can-insert? x y)
        (and (inner #t can-insert? x y)
             (can-clear-coverage?)))
      
      (define/augment (can-delete? x y)
        (and (inner #t can-delete? x y)
             (can-clear-coverage?)))
      
      (define/augment (after-insert x y)
        (inner (void) after-insert x y)
        (clear-test-coverage))
      
      (define/augment (after-delete x y)
        (inner (void) after-delete x y)
        (clear-test-coverage))
      
      (super-new)))
  
  (define test-coverage-on-style-name "plt:module-language:test-coverage-on")
  (define test-coverage-off-style-name "plt:module-language:test-coverage-off")
  
  (define erase-test-coverage-style-delta (make-object style-delta% 'change-normal-color))
  (send erase-test-coverage-style-delta set-transparent-text-backing-on #t)
  
  (define test-coverage-tab-mixin
    (mixin (drracket:rep:context<%> drracket:unit:tab<%>) (test-coverage-tab<%>)
      (inherit get-frame)
      
      (field [internal-clear-test-coverage-display #f]
             [test-coverage-entirely-covered-message-state #f])

      (define/public (get-test-coverage-entirely-covered-message-state)
        test-coverage-entirely-covered-message-state)
      (define/public (set-test-coverage-entirely-covered-message-state t)
        (set! test-coverage-entirely-covered-message-state t))
      
      (define/public (clear-test-coverage-display)
        (when internal-clear-test-coverage-display
          (internal-clear-test-coverage-display)
          (set! internal-clear-test-coverage-display #f)))
      
      (field [ask-about-reset? #t])
      (define/public (ask-about-clearing-test-coverage?) ask-about-reset?)
      
      (define/public (get-test-coverage-info-visible?)
        (not (not internal-clear-test-coverage-display)))
      
      (define/public (show-test-coverage-annotations ht on-style off-style ask?)
        (set! ask-about-reset? ask?)
        (let* ([edit-sequence-ht (make-hasheq)]
               [locked-ht (make-hasheq)]
               [already-frozen-ht (make-hasheq)]
               [actions-ht (make-hash)]
               
               [port-name-cache (make-hasheq)]
               
               ;; can-annotate : (listof (list boolean srcloc))
               ;; boolean is #t => code was run
               ;;            #f => code was not run
               ;; remove those that cannot be annotated
               [can-annotate
                (filter values
                        (hash-map ht
                          (λ (stx covered?)
                            (and (syntax? stx)
                                 (let ([src (syntax-source stx)]
                                       [pos (syntax-position stx)]
                                       [span (syntax-span stx)])
                                   (and pos
                                        span
                                        (hash-ref! port-name-cache src
                                                   (λ () (send (get-defs) port-name-matches? src)))
                                        (list (mcar covered?)
                                              (make-srcloc (get-defs) #f #f pos span))))))))]
               
               ;; filtered : (listof (list boolean srcloc))
               ;; remove redundant expressions
               [filtered
                (let (;; actions-ht : (list src number number) -> (list boolean syntax)
                      [actions-ht (make-hash)])
                  (for-each
                   (λ (pr)
                     (let* ([on? (list-ref pr 0)]
                            [key (list-ref pr 1)]
                            [old (hash-ref actions-ht key 'nothing)])
                       (cond
                         [(eq? old 'nothing) (hash-set! actions-ht key on?)]
                         [old ;; recorded as executed
                          (void)]
                         [(not old) ;; recorded as unexected
                          (when on?
                            (hash-set! actions-ht key #t))])))
                   can-annotate)
                  (hash-map actions-ht (λ (k v) (list v k))))])

          (set! test-coverage-entirely-covered-message-state
                (and (andmap car filtered)
                     (not (get-test-coverage-info-visible?))))
          (define fr (get-frame))
          (when (object=? this (send fr get-current-tab))
            (send (get-frame) update-test-coverage-entirely-covered-message))
          ;; if everything is covered *and* no coloring has been done, do no coloring.
          (unless test-coverage-entirely-covered-message-state
            (let (;; sorted : (listof (list boolean srcloc))
                  ;; sorting predicate:
                  ;;  x < y if
                  ;;    x's span is bigger than y's (ie, do larger expressions first)
                  ;;    unless x and y are the same source location.
                  ;;    in that case, color red first and then green
                  [sorted
                   (sort
                    filtered
                    (λ (x y)
                      (let* ([x-on (list-ref x 0)]
                             [y-on (list-ref y 0)]
                             [x-srcloc (list-ref x 1)]
                             [y-srcloc (list-ref y 1)]
                             [x-pos (srcloc-position x-srcloc)]
                             [y-pos (srcloc-position y-srcloc)]
                             [x-span (srcloc-span x-srcloc)]
                             [y-span (srcloc-span y-srcloc)])
                        (cond
                          [(and (= x-pos y-pos)
                                (= x-span x-span))
                           (or y-on
                               (not x-on))]
                          [else (>= x-span y-span)]))))])
              
              ;; turn on edit-sequences in all editors to be touched by new annotations
              ;; also fill in the edit-sequence-ht
              (for-each
               (λ (pr)
                 (let ([src (srcloc-source (list-ref pr 1))])
                   (hash-ref 
                    edit-sequence-ht
                    src
                    (λ ()
                      (hash-set! edit-sequence-ht src #f)
                      (send src begin-edit-sequence #f #f)
                      (when (send src is-locked?)
                        (hash-set! locked-ht src #t)
                        (send src lock #f))))))
               sorted)
              
              ;; clear out old annotations (and thaw colorers)
              (when internal-clear-test-coverage-display
                (internal-clear-test-coverage-display)
                (set! internal-clear-test-coverage-display #f))
              
              ;; freeze the colorers, but avoid a second freeze (so we can avoid a second thaw)
              (hash-for-each
               edit-sequence-ht
               (λ (src _)
                 (if (send src is-frozen?)
                     (hash-set! already-frozen-ht src #t)
                     (send src freeze-colorer))))
              
              ;; set new annotations
              (for-each
               (λ (pr)
                 (let ([on? (list-ref pr 0)]
                       [srcloc (list-ref pr 1)])
                   (let* ([src (srcloc-source srcloc)]
                          [pos (srcloc-position srcloc)]
                          [span (srcloc-span srcloc)])
                     (send src change-style
                           (if on?
                               (or on-style 
                                   (send (editor:get-standard-style-list)
                                         find-named-style 
                                         test-coverage-on-style-name))
                               (or off-style 
                                   (send (editor:get-standard-style-list)
                                         find-named-style 
                                         test-coverage-off-style-name)))
                           (- pos 1)
                           (+ (- pos 1) span)
                           #f))))
               sorted)
              
              ;; relock editors
              (hash-for-each 
               locked-ht
               (λ (txt _) (send txt lock #t)))
              
              ;; end edit sequences
              (hash-for-each 
               edit-sequence-ht
               (λ (txt _) (send txt end-edit-sequence)))
              
              ;; save thunk to reset these new annotations
              (set! internal-clear-test-coverage-display
                    (λ ()
                      (hash-for-each
                       edit-sequence-ht
                       (λ (txt _) 
                         (send txt begin-edit-sequence #f #f)))
                      (hash-for-each
                       edit-sequence-ht
                       (λ (txt _) 
                         (let ([locked? (send txt is-locked?)])
                           (when locked? (send txt lock #f))
                           (send txt change-style 
                                 erase-test-coverage-style-delta
                                 0
                                 (send txt last-position)
                                 #f)
                           (when locked? (send txt lock #t)))))
                      (hash-for-each
                       edit-sequence-ht
                       (λ (txt _) 
                         (unless (hash-ref already-frozen-ht txt #f)
                           (let ([locked? (send txt is-locked?)])
                             (when locked? (send txt lock #f))
                             (send txt thaw-colorer)
                             (when locked? (send txt lock #t))))
                         (send txt end-edit-sequence)))))))))
      
      (inherit get-defs)
      (define/augment (clear-annotations)
        (inner (void) clear-annotations)
        (send (get-defs) clear-test-coverage))
      
      (super-new)))
  
  
  
  
  
  ;                                                                 
  ;                                                                 
  ;                           ;;;        ;;;                        
  ;                          ;      ;      ;      ;                 
  ;                          ;             ;                        
  ;   ; ;;   ; ;;;   ;;;   ;;;;;; ;;;      ;    ;;;    ; ;;    ;; ; 
  ;   ;;  ;  ;;  ;  ;   ;    ;      ;      ;      ;    ;;  ;  ;  ;; 
  ;   ;   ;  ;      ;   ;    ;      ;      ;      ;    ;   ;  ;   ; 
  ;   ;   ;  ;      ;   ;    ;      ;      ;      ;    ;   ;  ;   ; 
  ;   ;   ;  ;      ;   ;    ;      ;      ;      ;    ;   ;  ;   ; 
  ;   ;;  ;  ;      ;   ;    ;      ;      ;      ;    ;   ;  ;  ;; 
  ;   ; ;;   ;       ;;;     ;      ;      ;      ;    ;   ;   ;; ; 
  ;   ;                                                           ; 
  ;   ;                                                        ;;;  
  ;                                                                 
  
  
  (define profile-key (gensym))
  
  ;; prof-info =
  ;; (make-prof-info
  ;;    boolean ;; protect against nested calls
  ;;    number[number of calls]
  ;;    number[time spent in all calls]
  ;;   (union #f symbol) 
  ;;   expression)
  (define-struct prof-info (nest num time name expr) #:mutable)
  
  ;; copy-prof-info : prof-info -> prof-info
  (define (copy-prof-info prof-info)
    (make-prof-info (prof-info-nest prof-info)
                    (prof-info-num prof-info)
                    (prof-info-time prof-info)
                    (prof-info-name prof-info)
                    (prof-info-expr prof-info)))
  
  ;; any-info? : prof-info -> boolean
  (define (any-info? prof-info)
    (or (not (zero? (prof-info-num prof-info)))
        (not (zero? (prof-info-time prof-info)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; profiling runtime support
  
  ;; parameter
  ;; imported into errortrace
  (define profiling-enabled (make-parameter #f))
  
  ;; holds a hash-table for the profiling information
  (define current-profile-info (make-thread-cell #f))
  
  
  ;; initialize-profile-point : sym syntax syntax -> void
  ;; called during compilation to register this point as
  ;; a profile point. 
  ;; =User=
  ;; imported into errortrace
  (define (initialize-profile-point key name expr)
    (unless (thread-cell-ref current-profile-info)
      (let ([rep (drracket:rep:current-rep)])
        (when rep
          (let ([ut (eventspace-handler-thread (send rep get-user-eventspace))])
            (when (eq? ut (current-thread))
              (let ([ht (make-hasheq)])
                (thread-cell-set! current-profile-info ht)
                (send (send rep get-context) add-profile-info ht)))))))
    (let ([profile-info (thread-cell-ref current-profile-info)])
      (when profile-info
        (hash-set! profile-info
                   key 
                   (make-prof-info #f 0 0 (and (syntax? name) (syntax-e name)) expr))))
    (void))
  
  ;; register-profile-start : sym -> (union #f number)
  ;; =User=
  ;; imported into errortrace
  (define (register-profile-start key)
    (let ([ht (thread-cell-ref current-profile-info)])
      (when ht
        (let ([info (hash-ref ht key)])
          (set-prof-info-num! info (+ (prof-info-num info) 1))
          (if (prof-info-nest info)
              #f
              (begin
                (set-prof-info-nest! info #t)
                (current-process-milliseconds)))))))
  
  ;; register-profile-done : sym (union #f number) -> void
  ;; =User=
  ;; imported into errortrace
  (define (register-profile-done key start)
    (when start
      (let ([ht (thread-cell-ref current-profile-info)])
        (when ht
          (let ([info (hash-ref ht key)])
            (set-prof-info-nest! info #f)
            (set-prof-info-time! info
                                 (+ (- (current-process-milliseconds) start)
                                    (prof-info-time info)))))))
    (void))
  
  (define (get-color-value/pref val max-val 
                                drracket:profile:low-color
                                drracket:profile:high-color
                                drracket:profile:scale)
    (let* ([adjust
            (case drracket:profile:scale
              [(sqrt) sqrt]
              [(square) (λ (x) (* x x))]
              [(linear) (λ (x) x)])]
           [factor (adjust (if (zero? max-val) 0 (/ val max-val)))]
           [get-rgb-value
            (λ (sel)
              (let ([small (sel drracket:profile:low-color)]
                    [big (sel drracket:profile:high-color)])
                (inexact->exact (floor (+ (* factor (- big small)) small)))))])
      (make-object color% 
        (get-rgb-value (λ (x) (send x red)))
        (get-rgb-value (λ (x) (send x green)))
        (get-rgb-value (λ (x) (send x blue))))))
  
  ;; get-color-value : number number -> (is-a?/c color%)
  ;; returns the profiling color
  ;; for `val' if `max-val' is the largest
  ;; of any profiling amount.
  (define (get-color-value val max-val)
    (get-color-value/pref val 
                          max-val
                          (preferences:get 'drracket:profile:low-color)
                          (preferences:get 'drracket:profile:high-color)
                          (preferences:get 'drracket:profile:scale)))
  
  ;; extract-maximum : (listof prof-info) -> number
  ;; gets the maximum value of the currently preferred profiling info.
  (define (extract-maximum infos)
    (let ([max-value 0]
          [sel (if (eq? (preferences:get 'drracket:profile-how-to-count) 'time)
                   prof-info-time
                   prof-info-num)])
      (for-each
       (λ (val)
         (set! max-value (max max-value (sel val))))
       infos)
      max-value))
  
  ;; profile-definitions-mixin : mixin
  (define profile-definitions-text-mixin
    (mixin ((class->interface text%) drracket:unit:definitions-text<%>) ()
      (inherit get-canvas get-tab)
      
      (define/augment (can-insert? x y)
        (and (inner #t can-insert? x y)
             (can-reset-profile?)))
      
      (define/augment (can-delete? x y)
        (and (inner #t can-delete? x y)
             (can-reset-profile?)))
      
      (define/augment (on-insert x y)
        (inner (void) on-insert x y)
        (do-reset-profile))
      
      (define/augment (on-delete x y)
        (inner (void) on-delete x y)
        (do-reset-profile))
      
      (define/private (can-reset-profile?)
        (let ([canvas (get-canvas)])
          (or (not canvas)
              (let ([frame (send canvas get-top-level-window)])
                (or (not (send frame get-profile-info-visible?))
                    (eq? (message-box (string-constant drscheme)
                                      (string-constant profiling-clear?)
                                      frame
                                      '(yes-no)
                                      #:dialog-mixin frame:focus-table-mixin)
                         'yes))))))
      
      (define/private (do-reset-profile)
        (send (get-tab) reset-profile))
      
      (super-new)))
  
  (define profile-interactions-tab<%>
    (interface ()
      add-profile-info))
  
  (define-local-member-name 
    
    ;; tab methods
    reset-profile  ;; erases profile display & information
    hide-profile  ;; hides profiling info, but it is still here to be shown again
    show-profile  ;; shows the profile info, if there is any to show
    refresh-profile ;; shows current info in profile window
    get-profile-info-text
    can-show-profile?
    get-sort-mode ;; indicates if the results are currently shown sorted by time, or not
    set-sort-mode ;; updates the sort mode flag (only called by the gui control callback)
    get-test-coverage-entirely-covered-message-state
    update-test-coverage-entirely-covered-message
    
    ;; frame methods
    hide-profile-gui
    show-profile-gui
    
    ;; frame and tab methods
    get-profile-info-visible?
    ; on frame, indicates if the gui stuff shows up currently
    ; on tab, indicates if the user has asked for the gui to show up.
    )
  
  (define profile-tab-mixin
    (mixin (drracket:unit:tab<%>) (profile-interactions-tab<%>)
      (define profile-info-visible? #f)
      (define/public (get-profile-info-visible?) profile-info-visible?)
      
      (define sort-mode (preferences:get 'drracket:profile-how-to-count))
      (define/public (get-sort-mode) sort-mode)
      (define/public (set-sort-mode mode) (set! sort-mode mode))
      
      (inherit get-frame is-current-tab? get-defs)
      ;; profile-info : (listof hashtable[symbol -o> prof-info])
      (define profile-info '())
      (define/public (add-profile-info ht) (set! profile-info (cons ht profile-info)))
      
      (define/public (show-profile)
        (unless profile-info-visible?
          (set! profile-info-visible? #t)
          (send (get-frame) show-profile-gui)))
      
      (define/public (hide-profile)
        (when profile-info-visible?
          (set! profile-info-visible? #f)
          (send profile-info-text clear-profile-display)
          (when (is-current-tab?)
            (send (get-frame) hide-profile-gui))))
      
      (define/public (reset-profile)
        (hide-profile)
        (set! profile-info '()))
      
      (define/public (refresh-profile) 
        (send profile-info-text refresh-profile profile-info (get-defs)))
      
      ;; can-show-profile? : -> boolean
      ;; indicates if there is any profiling information to be shown.
      (define/public (can-show-profile?)
        (let/ec esc-k
          (for-each
           (λ (ht)
             (hash-for-each
              ht
              (λ (key v)
                (when (any-info? v)
                  (esc-k #t)))))
           profile-info)
          #f))
      
      (define profile-info-text (new profile-text% (tab this)))
      (define/public (get-profile-info-text) profile-info-text)
      
      (define/augment (clear-annotations)
        (inner (void) clear-annotations)
        (reset-profile))
      
      (super-new)))
  
  ;; profile-unit-frame-mixin : mixin
  ;; adds profiling to the unit frame
  (define profile-unit-frame-mixin
    (mixin (drracket:unit:frame<%> drracket:frame:<%>) ()
      
      (inherit get-interactions-text get-current-tab set-show-menu-sort-key)
      
      ;; update-shown : -> void
      ;; updates the state of the profile item's show menu
      (define/override (update-shown)
        (super update-shown)
        (send show-profile-menu-item set-label
              (if profile-info-visible?
                  (string-constant profiling-hide-profile)
                  (string-constant profiling-show-profile))))
      
      ;; add-show-menu-items : menu -> void
      ;; adds the show profile menu item
      (define/override (add-show-menu-items show-menu)
        (super add-show-menu-items show-menu)
        (set! show-profile-menu-item 
              (instantiate menu:can-restore-menu-item% ()
                (label (string-constant profiling-hide-profile))
                (parent show-menu)
                (callback
                 (λ (x y)
                   (show-profile-menu-callback)))))
        (set-show-menu-sort-key show-profile-menu-item 207))
      
      (define show-profile-menu-item #f)
      (define profile-gui-constructed? #f)
      
      ;; get-profile-info-visible? : -> boolean
      ;; returns #t when the profiling information is visible in the frame.
      (define/public (get-profile-info-visible?) profile-info-visible?)
      
      (field [profile-info-outer-panel #f])
      (define/override (make-root-area-container % parent)
        (set! profile-info-outer-panel
              (super make-root-area-container
                     panel:vertical-dragable%
                     parent))
        (make-object % profile-info-outer-panel))
      
      (define/private (show-profile-menu-callback)
        (cond
          [profile-info-visible?
           (send (get-current-tab) hide-profile)]
          [(send (get-current-tab) can-show-profile?)
           (send (get-current-tab) show-profile)
           (send (get-current-tab) refresh-profile)]
          [else
           (message-box (string-constant drscheme)
                        (string-constant profiling-no-information-available)
                        #:dialog-mixin frame:focus-table-mixin)]))
      
      (define/public (hide-profile-gui)
        (when profile-gui-constructed?
          (when profile-info-visible?
            (send profile-info-outer-panel change-children
                  (λ (l)
                    (remq profile-info-panel l)))
            (set! profile-info-visible? #f)
            (update-shown))))
      
      (define/public (show-profile-gui)
        (unless profile-info-visible?
          (construct-profile-gui)
          (send profile-info-outer-panel change-children
                (λ (l)
                  (append (remq profile-info-panel l)
                          (list profile-info-panel))))
          (set! profile-info-visible? #t)
          (send profile-info-editor-canvas set-editor (send (get-current-tab) get-profile-info-text))
          (send (get-current-tab) refresh-profile)
          ;; set to a small percentage so it gets the minimum height
          (send profile-info-outer-panel set-percentages '(9/10 1/10))
          (update-shown)))
      
      (field (profile-info-visible? #f))
      
      (define/augment (on-tab-change from-tab to-tab)
        (inner (void) on-tab-change from-tab to-tab)
        (cond
          [(and (not profile-info-visible?)
                (send to-tab get-profile-info-visible?))
           (show-profile-gui)]
          [(and profile-info-visible?
                (not (send to-tab get-profile-info-visible?)))
           (hide-profile-gui)])
        (when profile-choice
          (send profile-choice set-selection
                (profile-mode->selection
                 (send to-tab get-sort-mode))))
        (when profile-info-editor-canvas
          (send profile-info-editor-canvas set-editor 
                (and (send to-tab can-show-profile?)
                     (send to-tab get-profile-info-text)))))
      
      (super-new)
      
      (define profile-info-panel #f)
      (define profile-info-editor-canvas #f)
      (define profile-choice #f)
      
      (inherit begin-container-sequence end-container-sequence)
      (define/private (construct-profile-gui)
        (unless profile-gui-constructed?
          (set! profile-gui-constructed? #t)
          (begin-container-sequence)
          (set! profile-info-panel (instantiate horizontal-panel% ()
                                     (parent profile-info-outer-panel)
                                     (stretchable-height #f)))
          (define profile-left-side (new-vertical-panel% [parent profile-info-panel]))
          (set! profile-info-editor-canvas 
                (new canvas:color%
                     (parent profile-info-panel)
                     (editor (send (get-current-tab) get-profile-info-text))))
          (define profile-message (instantiate message% ()
                                    (label (string-constant profiling))
                                    (parent profile-left-side)))
          (set! profile-choice (new radio-box%
                                    (label #f)
                                    (parent profile-left-side)
                                    (callback
                                     (λ (x y)
                                       (define mode
                                         (profile-selection->mode 
                                          (send profile-choice get-selection)))
                                       (preferences:set 'drracket:profile-how-to-count mode)
                                       (send (get-current-tab) set-sort-mode mode)
                                       (send (get-current-tab) refresh-profile)))
                                    (choices (list (string-constant profiling-time)
                                                   (string-constant profiling-number)))))
          (send profile-choice set-selection
                (case (preferences:get 'drracket:profile-how-to-count)
                  [(time) 0]
                  [(count) 1]))
          (define update-profile-button
            (instantiate button% ()
              (label (string-constant profiling-update))
              (parent profile-left-side)
              (callback
               (λ (x y)
                 (send (get-current-tab) refresh-profile)))))
          (define hide-profile-button 
            (instantiate button% ()
              (label (string-constant profiling-hide-profile))
              (parent profile-left-side)
              (callback
               (λ (x y)
                 (send (get-current-tab) hide-profile)))))
          (send profile-choice set-selection 
                (profile-mode->selection (preferences:get 'drracket:profile-how-to-count)))
          
          (send profile-left-side stretchable-width #f)
          
          (let ([wid (max (send update-profile-button get-width)
                          (send hide-profile-button get-width)
                          (send profile-choice get-width)
                          (send profile-message get-width))])
            (send update-profile-button min-width wid)
            (send hide-profile-button min-width wid)
            (send profile-choice min-width wid))
          (send profile-left-side set-alignment 'left 'center)
            
          ;; hide profiling info initially, but reflow the container
          ;; so that the invisible children get the right size.
          (send this reflow-container)
          (send profile-info-outer-panel change-children
                (λ (l)
                  (remq profile-info-panel l)))
          (end-container-sequence)))))
  
  (define (profile-selection->mode sel)
    (case sel
      [(0) 'time]
      [(1) 'count]))
  
  (define (profile-mode->selection mode)
    (case mode
      [(time) 0]
      [(count) 1]))
  
  ;; profile-text% : extends text:basic%
  ;; this class keeps track of a single thread's
  ;; profiling information. these methods are not
  ;; to be called directly, but only by the frame class, since
  ;; they do not completely implement the abstraction for the
  ;; GUI. They only manage the profiling information reported
  ;; in the bottom window
  (define profile-text% 
    (class (text:foreground-color-mixin
            (editor:standard-style-list-mixin
             text:line-spacing%))
      (init-field tab)
      
      ;; clear-profile-display : -> void
      ;; clears out the GUI showing the profile results
      (define/public (clear-profile-display)
        (begin-edit-sequence #t #f)
        (let ([locked? (is-locked?)])
          (lock #f)
          (clear-old-results)
          (erase)
          (lock locked?)
          (end-edit-sequence)))
      
      (inherit lock is-locked?
               get-canvas hide-caret get-snip-location
               begin-edit-sequence end-edit-sequence 
               erase insert get-start-position
               change-style get-style-list default-style-name)
      
      ;; clear-old-results : -> void
      ;; removes the profile highlighting
      (field [clear-old-results void])
      
      ;; refresh-profile : (listof hashtable[...]) text% -> void
      ;; does the work to erase any existing profile info
      ;; and make new profiling info.
      (define/public (refresh-profile profile-info definitions-text)
        (begin-edit-sequence #t #f)
        (lock #f)
        (erase)
        (clear-old-results)
        (define infos '())
        (let loop ([profile-info profile-info])
          (cond
            [(null? profile-info) (void)]
            [else 
             (let ([ht (car profile-info)])
               (hash-for-each
                ht
                (λ (key val)
                  (when (any-info? val)
                    (set! infos (cons (copy-prof-info val) infos))))))
             (loop (cdr profile-info))]))
        
        ;; each editor that gets some highlighting is put
        ;; into this table and an edit sequence is begun for it.
        ;; after all ranges are updated, the edit sequences are all closed.
        (define in-edit-sequence (make-hasheq))
        (define clear-highlight void)
        (define max-value (extract-maximum infos))
        
        (define port-name-matches-cache (make-hasheq))
        (define (show-highlight info)
          (define expr (prof-info-expr info))
          (define src 
            (and (syntax-source expr)
                 definitions-text
                 (hash-ref! port-name-matches-cache
                            (syntax-source expr)
                            (λ () (send definitions-text port-name-matches? (syntax-source expr))))))
          (define pos (syntax-position expr))
          (define span (syntax-span expr))
          (when (and (is-a? src text:basic<%>)
                     (number? pos)
                     (number? span))
            (unless (hash-ref in-edit-sequence src (λ () #f))
              (hash-set! in-edit-sequence src #t)
              (send src begin-edit-sequence #t #f))
            (let* ([color (get-color-value 
                           (if (eq? (preferences:get 'drracket:profile-how-to-count) 'time)
                               (prof-info-time info)
                               (prof-info-num info))
                           max-value)]
                   [clr (send src highlight-range (- pos 1) (+ pos span -1) color)])
              (let ([old-thnk clear-highlight])
                (set! clear-highlight
                      (λ ()
                        (clr)
                        (old-thnk)))))))
        (define (smaller-range? x y)
          (let ([x-span (syntax-span (prof-info-expr x))]
                [y-span (syntax-span (prof-info-expr y))])
            (if (and x-span y-span)
                (< x-span y-span)
                #f)))
        
        (define (show-line info newline? highlight-line?)
          (let* ([expr (prof-info-expr info)]
                 [expr-src (syntax-source expr)]
                 [count (prof-info-num info)]
                 [time (prof-info-time info)]
                 [name (prof-info-name info)])
            (when newline? (send src-loc-editor insert "\n"))
            (when highlight-line? (small-blank-line src-loc-editor))
            (let ([before (send src-loc-editor last-position)])
              (insert-profile-src-loc src-loc-editor expr name)
              (let ([after (send src-loc-editor last-position)])
                (cond
                  [(string? expr-src)
                   (send src-loc-editor change-style
                         (gui-utils:get-clickback-delta (preferences:get 'framework:white-on-black?))
                         before after)
                   (let ([after (send src-loc-editor last-position)])
                     (send src-loc-editor set-clickback 
                           before after 
                           (λ (text start end)
                             (open-file-and-goto-position expr-src (syntax-position expr)))))]
                  [(is-a? expr-src editor:basic<%>)
                   (send src-loc-editor change-style
                         (gui-utils:get-clickback-delta (preferences:get 'framework:white-on-black?))
                         before after)
                   (send src-loc-editor set-clickback
                         before after
                         (λ (text start end)
                           (let ([window (send expr-src get-top-level-window)]
                                 [pos (syntax-position expr)])
                             (when window (send window show #t))
                             (when pos (send expr-src set-position (- pos 1)))
                             (send expr-src set-caret-owner #f 'global))))]
                  [else (void)])))
            
            (when newline? (send time-editor insert "\n"))
            (when highlight-line? (small-blank-line time-editor))
            (send time-editor insert (format "~a" time))
            (send time-editor set-paragraph-alignment (send time-editor last-paragraph) 'right)
            
            (when newline? (send count-editor insert "\n")) 
            (when highlight-line? (small-blank-line count-editor))
            (send count-editor insert (format "~a" count))
            (send count-editor set-paragraph-alignment (send count-editor last-paragraph) 'right)))
        
        (define (bigger-value? x y)
          (let ([sel (if (eq? 'count (preferences:get 'drracket:profile-how-to-count))
                         prof-info-num
                         prof-info-time)])
            (> (sel x) (sel y))))
        
        (define (cleanup-editor ed)
          (let* ([ed-admin (send ed get-admin)]
                 [snip (send ed-admin get-snip)]
                 [bl (box 0)]
                 [br (box 0)])
            (get-snip-location snip bl #f #f)
            (get-snip-location snip br #f #t)
            (let ([w (+ (- (unbox br) (unbox bl)) 4)])
              (send ed set-max-width w)
              (send ed set-min-width w)))
          (send ed hide-caret #t)
          (send ed lock #t))
        
        (define top-infos (top 100 (sort infos bigger-value?)))
        (for-each show-highlight top-infos)
        (initialize-editors)
        (let loop ([infos top-infos]
                   [newline? #f]
                   [highlight-counter 0])
          (cond
            [(null? infos) (void)]
            [else 
             (show-line (car infos) newline? (and newline? (zero? highlight-counter)))
             (loop (cdr infos) #t (modulo (+ highlight-counter 1) 2))]))
        (cleanup-editor count-editor)
        (cleanup-editor time-editor)
        (cleanup-editor src-loc-editor)
        
        (hash-for-each
         in-edit-sequence
         (λ (key val)
           (send key end-edit-sequence)))
        (set! clear-old-results 
              (λ ()
                (hash-for-each
                 in-edit-sequence
                 (λ (key val) (send key begin-edit-sequence #t #f)))
                (clear-highlight)
                (hash-for-each
                 in-edit-sequence
                 (λ (key val) (send key end-edit-sequence)))
                (set! clear-old-results void)))
        (lock #t)
        (end-edit-sequence)
        (let ([canvas (get-canvas)])
          (when canvas
            (send canvas scroll-to 0 0 1 1 #t 'start))))
      
      
      ;; top : number (listof X) -> (listof X)
      ;; extracts the first `n' elements from a list.
      (define/private (top n lst)
        (let loop ([n n]
                   [lst lst])
          (cond
            [(null? lst) null]
            [(= 0 n) null]
            [else (cons (car lst) (loop (- n 1) (cdr lst)))])))
      
      (field (src-loc-editor #f)
             (time-editor #f)
             (count-editor #f))
      (define/private (clear-editors)
        (set! src-loc-editor #f)
        (set! time-editor #f)
        (set! count-editor #f))
      (define/private (initialize-editors)
        (set! src-loc-editor (new profile-content-text%))
        (set! time-editor (new profile-content-text%))
        (set! count-editor (new profile-content-text%))
        (send src-loc-editor set-styles-sticky #f)            
        (send time-editor set-styles-sticky #f)
        (send count-editor set-styles-sticky #f)
        (define (insert-es e)
          (define es (new editor-snip% [editor e]))
          (send es use-style-background #t)
          (define before (get-start-position))
          (insert es)
          (change-style (send (get-style-list) find-named-style (default-style-name))
                        before
                        (+ before 1)))
        (insert-es time-editor)
        (insert-es count-editor)
        (insert-es src-loc-editor)
        (insert-title (string-constant profiling-col-function) src-loc-editor)
        (insert-title (string-constant profiling-col-time-in-msec) time-editor)
        (insert-title (string-constant profiling-col-calls) count-editor))
      
      (define/private (insert-title str txt)
        (send txt insert str)
        (send txt insert "\n")
        (send txt change-style bold-delta 0 (- (send txt last-position) 1))
        (send txt set-paragraph-alignment 0 'center))
      
      (super-new)
      (hide-caret #t)))

  (define profile-content-text% (text:foreground-color-mixin text:standard-style-list%))
  
  ;; format-percentage : number[0 <= n <= 1] -> string
  ;; formats the number as a percentage string with trailing zeros,
  ;; to 3 decimal places.
  (define (format-percentage n)
    (let* ([number-of-places 3]
           [whole-part (floor (* n 100))]
           [decimal-part (- (* n 100) whole-part)]
           [truncated/moved-decimal-part (floor (* (expt 10 number-of-places) decimal-part))]
           [pad
            (λ (str)
              (if ((string-length str) . < . number-of-places)
                  (string-append (make-string (- number-of-places (string-length str)) #\0) 
                                 str)
                  str))])
      (string-append (format "~a" whole-part)
                     "."
                     (pad (format "~a" truncated/moved-decimal-part)))))
  
  (define (small-blank-line txt)
    (let ([before (send txt last-position)])
      (send txt insert "\n")
      (let ([after (send txt last-position)])
        (send txt change-style small-font-style before after))))
  
  (define small-font-style (make-object style-delta% 'change-size 6))
  
  ;; bold-delta : style-delta
  (define bold-delta (make-object style-delta% 'change-bold))
  
  ;; insert-profile-src-loc : syntax name -> string
  (define (insert-profile-src-loc editor stx name)
    (cond
      [name
       (let ([before (send editor last-position)])
         (send editor insert (format "~a" name)))]
      [else
       (let* ([src (syntax-source stx)]
              [filename 
               (cond
                 [(not src) (string-constant profiling-unknown-src)]
                 [(is-a? src editor<%>) (get-filename-from-editor src)]
                 [else (format "~a" src)])]
              [col (syntax-column stx)]
              [line (syntax-line stx)]
              [pos (syntax-position stx)]
              [span (syntax-span stx)]
              [src
               (cond
                 [(and col line)
                  (format "~a: ~a.~a" filename line col)]
                 [pos
                  (format "~a: ~a" filename pos)]
                 [else 
                  filename])])
         (send editor insert src))]))
  
  ;; open-file-and-goto-position : string (union #f number) -> void
  (define (open-file-and-goto-position filename pos)
    (let ([frame (handler:edit-file filename)])
      (when (and frame
                 pos
                 (is-a? frame drracket:unit:frame%))
        (let ([defs (send frame get-definitions-text)])
          (send defs set-position (- pos 1))))))
  
  ;; get-src-filename : tst -> (union #f string)
  (define (get-src-filename src)
    (cond
      [(string? src) src]
      [(is-a? src text%)
       (send src get-filename)]
      [else #f]))
  
  ;; get-src-loc : syntax -> string
  (define (get-src-loc expr)
    (cond
      [(and (number? (syntax-line expr))
            (number? (syntax-column expr))
            (number? (syntax-span expr)))
       (format " ~a.~a [~a]" 
               (syntax-line expr) 
               (syntax-column expr)
               (syntax-span expr))]
      [(and (number? (syntax-position expr))
            (number? (syntax-span expr)))
       (format " ~a-~a" 
               (syntax-position expr)
               (syntax-span expr))]
      [else ""]))
  
  (define (add-prefs-panel)
    (preferences:add-panel
     (string-constant profiling)
     (λ (s-parent)
       (letrec ([parent (new-vertical-panel% [parent s-parent])]
                [msg (make-object message% 
                       (string-constant profiling-color-config) 
                       parent)]
                [hp (make-object horizontal-pane% parent)]
                [low (make-object button% (string-constant profiling-low-color) hp 
                       (λ (x y) (color-callback #t)))]
                [color-bar (make-object color-bar% hp)]
                [high (make-object button% (string-constant profiling-high-color) hp
                        (λ (x y) (color-callback #f)))]
                
                [scale (instantiate radio-box% ()
                         (label (string-constant profiling-scale))
                         (parent parent)
                         (callback (λ (x y) (scale-callback)))
                         (choices
                          (list (string-constant profiling-sqrt)
                                (string-constant profiling-linear)
                                (string-constant profiling-square))))]
                
                [color-callback
                 (λ (low?)
                   (let ([color (get-color-from-user 
                                 (if low?
                                     (string-constant profiling-choose-low-color)
                                     (string-constant profiling-choose-high-color))
                                 #f
                                 (preferences:get
                                  (if low?
                                      'drracket:profile:low-color
                                      'drracket:profile:high-color)))])
                     (when color
                       (preferences:set 
                        (if low? 'drracket:profile:low-color 'drracket:profile:high-color)
                        color))))]
                [scale-callback
                 (λ ()
                   (preferences:set 
                    'drracket:profile:scale
                    (case (send scale get-selection)
                      [(0) 'sqrt]
                      [(1) 'linear]
                      [(2) 'square])))])
         (preferences:add-callback
          'drracket:profile:scale
          (λ (p v)
            (send scale set-selection
                  (case v
                    [(sqrt) 0]
                    [(linear) 1]
                    [(square) 2]))))
         (send parent set-alignment 'left 'center)
         (send hp stretchable-height #f)
         parent))))
  
  (define color-bar%
    (class canvas%
      (inherit get-client-size get-dc)
      (field [pen (make-object pen% "black" 1 'solid)]
             [in-on-paint? #f])
      (define/override (on-paint)
        (set! in-on-paint? #t)
        (let* ([dc (get-dc)]
               [dummy-pen (send dc get-pen)]
               [drracket:profile:low-color (preferences:get 'drracket:profile:low-color)]
               [drracket:profile:high-color (preferences:get 'drracket:profile:high-color)]
               [drracket:profile:scale (preferences:get 'drracket:profile:scale)])
          (let-values ([(w h) (get-client-size)])
            (let loop ([n 0])
              (when (n . <= . w)
                (send pen set-color 
                      (get-color-value/pref n w 
                                            drracket:profile:low-color
                                            drracket:profile:high-color
                                            drracket:profile:scale))
                (send dc set-pen pen)
                (send dc draw-line n 0 n h)
                (send dc set-pen dummy-pen)
                (loop (+ n 1))))
            (let-values ([(tw th ta td) (send dc get-text-extent 
                                              (string-constant profiling-example-text))])
              (send dc draw-text
                    (string-constant profiling-example-text)
                    (floor (- (/ w 2) (/ tw 2)))
                    (floor (- (/ h 2) (/ th 2)))))))
        (set! in-on-paint? #f))
      
      ;; queue callbacks here so that the preferences
      ;; values are actually set by the time on-paint
      ;; is called.
      (preferences:add-callback
       'drracket:profile:scale
       (λ (p v)
         (unless in-on-paint?
           (queue-callback
            (λ ()
              (on-paint))))))
      (preferences:add-callback
       'drracket:profile:low-color
       (λ (p v)
         (unless in-on-paint?
           (queue-callback
            (λ ()
              (on-paint))))))
      (preferences:add-callback
       'drracket:profile:high-color
       (λ (p v)
         (unless in-on-paint?
           (queue-callback
            (λ ()
              (on-paint))))))
      
      (super-instantiate ())))

  (define-values/invoke-unit/infer stacktrace/errortrace-annotate@))

(define ellipsis-error-message-field #rx"  [^ ].*[.][.][.]:$")

(define (collect-hidden-lines lines)
  (let loop ([lines lines]
             [ellipsis-line #f]
             [collection #f])
    (cond
      [(null? lines)
       (cond
         [ellipsis-line
          (list (list ellipsis-line collection))]
         [else
          '()])]
      [else
       (define line (car lines))
       (cond
         [ellipsis-line
          (cond
            [(regexp-match #rx"^   " line)
             (loop (cdr lines) ellipsis-line (cons line collection))]
            [else
             (cons (list ellipsis-line collection)
                   (loop lines #f #f))])]
         [else
          (cond
            [(regexp-match ellipsis-error-message-field line)
             (loop (cdr lines) line '())]
            [else
             (cons line (loop (cdr lines) #f #f))])])])))

(module+ test
  (require rackunit)
  (check-equal?
   (collect-hidden-lines
    '("car: arity mismatch;"
      " the expected number of arguments does not match the given number"
      "  expected: 1"
      "  given: 3"
      "  arguments...:"
      "   1"
      "   2"
      "   3"))
   '("car: arity mismatch;"
     " the expected number of arguments does not match the given number"
     "  expected: 1"
     "  given: 3"
     ("  arguments...:" ("   3" "   2" "   1"))))
  
  (check-equal?
   (collect-hidden-lines
    '("car: arity mismatch;"
      " the expected number of arguments does not match the given number"
      "  expected: 1"
      "  given: 3"
      "  arguments...:"
      "   1"
      "   2"
      "   3"
      "  another field:"))
   '("car: arity mismatch;"
     " the expected number of arguments does not match the given number"
     "  expected: 1"
     "  given: 3"
     ("  arguments...:" ("   3" "   2" "   1"))
     "  another field:"))
  
  (check-equal?
   (collect-hidden-lines
    '("car: arity mismatch;"
      " the expected number of arguments does not match the given number"
      "  expected: 1"
      "  given: 3"
      "  arguments...:"
      "   1"
      "   2"
      "   3"
      "  arguments...:"
      "   1"
      "   2"
      "   3"))
   '("car: arity mismatch;"
     " the expected number of arguments does not match the given number"
     "  expected: 1"
     "  given: 3"
     ("  arguments...:" ("   3" "   2" "   1"))
     ("  arguments...:" ("   3" "   2" "   1"))))
  
  (check-equal?
   (collect-hidden-lines
    '("car: arity mismatch;"
      " the expected number of arguments does not match the given number"
      "  expected: 1"
      "  given: 3"
      "  arguments...:"
      "   1"
      "   2"
      "   3"
      "  a different field:"
      "  arguments...:"
      "   1"
      "   2"
      "   3"))
   '("car: arity mismatch;"
     " the expected number of arguments does not match the given number"
     "  expected: 1"
     "  given: 3"
     ("  arguments...:" ("   3" "   2" "   1"))
     "  a different field:"
     ("  arguments...:" ("   3" "   2" "   1")))))

