#lang racket/base

(require compiler/module-suffix
         drracket/private/rectangle-intersect
         framework/gui-utils
         framework/preferences
         mrlib/graph
         mrlib/panel-wob
         pkg/path
         racket/async-channel
         racket/class
         racket/contract
         racket/gui/base
         racket/list
         racket/match
         racket/port
         racket/promise
         racket/set
         racket/unit
         setup/dirs
         string-constants
         syntax/moddep
         syntax/toplevel)

(define oprintf
  (let ([op (current-output-port)])
    (λ args
      (apply fprintf op args))))

(provide standalone-module-overview/file
         module-overview/file
         make-module-overview-pasteboard
         module-browser-pkg-set-choice%
         module-browser-submod-set-choice%)

(preferences:set-default 'drracket:module-overview:label-font-size 12 number?)
(preferences:set-default 'drracket:module-overview:window-height 500 number?)
(preferences:set-default 'drracket:module-overview:window-width 500 number?)
(preferences:set-default 'drracket:module-browser:name-length 1 
                         (λ (x) (memq x '(0 1 2 3))))

(preferences:set-default 'drracket:module-browser:visible-submodules
                         '(())
                         (listof (listof symbol?)))

(define-struct req (r-mpi))
;; type req = (make-req [result from resolve-module-path-index]
;;                   -- except only when it has a path
;;                   -- and with the filename inside cleaned up
;;                      (simplified and .ss/.rkt matching the filesystem)

(provide process-program-unit
         process-program-import^
         process-program-export^
         (struct-out req))

(define adding-file (string-constant module-browser-adding-file))
(define unknown-module-name "? unknown module name")

(define pkg-constant "pkg: ~a")
(define sc-main-collects (string-constant module-browser-main-collects))
(define sc-unknown-pkg (string-constant module-browser-unknown-pkg))
(define sc-visible-pkgs (string-constant module-browser-visible-pkgs))
(define sc-visible-submodules (string-constant module-browser-visible-submodules))
(define sc-top-level-module (string-constant module-browser-top-level-module))
(define filename-constant (string-constant module-browser-filename-format))
(define font-size-gauge-label (string-constant module-browser-font-size-gauge-label))
(define progress-label (string-constant module-browser-progress-label))
(define open-file-format (string-constant module-browser-open-file-format))

(define (set-box/f b v) (when (box? b) (set-box! b v)))

(define (find-label-font size)
  (send the-font-list find-or-create-font size 'decorative 'normal 'normal #f))

(define module-overview-pasteboard<%>
  (interface ()
    set-label-font-size
    get-label-font-size
    set-name-length
    get-name-length
    get-pkgs
    get-main-file-pkg
    restrict-files-to-pkgs
    get-pkg-restriction
    get-submods
    restrict-files-to-submods
    get-submod-restriction))

(define boxed-word-snip<%>
  (interface ()
    get-filename
    get-word
    get-lines
    set-found!))

(define/contract (render-phases s)
  (-> (set/c (or/c exact-integer? #f)) string?)
  (define for-doc (set-member? s #f))
  (define lst (sort (filter number? (set->list s)) <))
  (define joined-lst
    (cond
      [(null? lst) '()]
      [else
       (let loop ([lst (cdr lst)]
                  [pending-low (car lst)]
                  [pending-high (car lst)])
         (cond
           [(null? lst) (list (cons pending-low pending-high))]
           [else
            (define fst (car lst))
            (cond
              [(equal? (- fst 1) pending-high)
               (loop (cdr lst)
                     pending-low
                     fst)]
              [else
               (cons (cons pending-low pending-high)
                     (loop (cdr lst) fst fst))])]))]))
  (cond
    [(and (null? joined-lst) (not for-doc))
     ""]
    [(and (null? joined-lst) for-doc)
     " (phase: for-label)"]
    [else
     (define plural? (not (= 1 (set-count s))))
     (define strings 
       (append (for/list ([joined (in-list joined-lst)])
                 (cond
                   [(= (car joined) (cdr joined))
                    (format "~a" (car joined))]
                   [else
                    (format "~a–~a" (car joined) (cdr joined))]))
               (if for-doc
                   '("for-label")
                   '())))
     (apply string-append
            (if plural? " (phases: " " (phase: ")
            (append (add-between strings ", ")
                    '(")")))]))

(module+ test
  (require rackunit)
  (check-equal? (render-phases (set)) "")
  (check-equal? (render-phases (set #f)) " (phase: for-label)")
  (check-equal? (render-phases (set 1)) " (phase: 1)")
  (check-equal? (render-phases (set -1 1)) " (phases: -1, 1)")
  (check-equal? (render-phases (set -3 -2 -1 0 1 2 3)) " (phases: -3–3)")
  (check-equal? (render-phases (set -3 -2 -1)) " (phases: -3–-1)")
  (check-equal? (render-phases (set 1 3)) " (phases: 1, 3)")
  (check-equal? (render-phases (set 1 2 3)) " (phases: 1–3)")
  (check-equal? (render-phases (set 1 2 3 7 8 9)) " (phases: 1–3, 7–9)")
  (check-equal? (render-phases (set #f 1 2 3 7 8 9)) " (phases: 1–3, 7–9, for-label)")
  (check-equal? (render-phases (set 1 3 5 7 9)) " (phases: 1, 3, 5, 7, 9)")
  (check-equal? (render-phases (set #f 1 3 5 7 9)) " (phases: 1, 3, 5, 7, 9, for-label)"))

;                                                                                    
;                                                                                    
;                                                                                    
;                                                                                    
;                                                                                    
;                                                                                    
;   ; ;;    ; ;   ;;;     ;;;    ;;;    ;;;    ;;;       ; ;;    ; ;   ;;;     ;; ;  
;   ;;  ;   ;;   ;   ;   ;   ;  ;   ;  ;      ;          ;;  ;   ;;   ;   ;   ;  ;;  
;   ;    ;  ;   ;     ; ;      ;    ;  ;;     ;;         ;    ;  ;   ;     ; ;    ;  
;   ;    ;  ;   ;     ; ;      ;;;;;;   ;;     ;;        ;    ;  ;   ;     ; ;    ;  
;   ;    ;  ;   ;     ; ;      ;          ;      ;       ;    ;  ;   ;     ; ;    ;  
;   ;;  ;   ;    ;   ;   ;   ;  ;         ;      ;       ;;  ;   ;    ;   ;   ;  ;;  
;   ; ;;    ;     ;;;     ;;;    ;;;;  ;;;    ;;;        ; ;;    ;     ;;;     ;; ;  
;   ;                                                    ;                        ;  
;   ;                                                    ;                   ;    ;  
;   ;                                                    ;                    ;;;;   


(define-signature process-program-import^
  (progress-channel connection-channel))

(define-signature process-program-export^
  (add-connections))

(define-unit process-program-unit
  (import process-program-import^)
  (export process-program-export^)
  
  (define visited-hash-table (make-hash))
  
  ;; add-connections : (union syntax string[filename]) -> (union #f string)
  ;; recursively adds a connections from this file and
  ;; all files it requires
  ;; returns a string error message if there was an error compiling
  ;; the program
  (define (add-connections filename/stx)
    (cond
      [(path-string? filename/stx)
       (add-module-code-connections/with-submods filename/stx (get-module-code filename/stx))]
      [(syntax? filename/stx)
       (add-syntax-connections filename/stx)]))
  
  ;; add-syntax-connections : syntax -> void
  (define (add-syntax-connections stx)
    (define module-codes (map compile (expand-syntax-top-level-with-compile-time-evals/flatten stx)))
    (for ([module-code (in-list module-codes)]
          #:when (compiled-module-expression? module-code))
      (define name (extract-module-name stx))
      (define base
        (build-module-filename
         (if (regexp-match #rx"^," name)
             (substring name 1 (string-length name))
             (build-path (or (current-load-relative-directory) (current-directory)) name))
         #f))
      (add-module-code-connections/with-submods base module-code)))

  (define (add-module-code-connections/with-submods base module-code)
    (add-module-code-connections base module-code)
    (let loop ([module-code module-code])
      (for ([submod-code (in-list (append (module-compiled-submodules module-code #f)
                                          (module-compiled-submodules module-code #t)))])
        (add-module-code-connections
         (match (module-compiled-name submod-code)
           [`(,_main-module-name ,submod-names ...) `(submod ,base ,@submod-names)]
           [the-name the-name])
         submod-code)
        (loop submod-code))))

  (define module-suffixes (delay (map (lambda (s) (bytes-append #"." s))
                                      (get-module-suffixes))))

  (define (build-module-filename pth remove-extension?)
    (define (try ext)
      (define tst (bytes->path (bytes-append 
                                (if remove-extension?
                                    (regexp-replace #rx"[.][^.]*$" (path->bytes pth) #"")
                                    (path->bytes pth))
                                ext)))
      (and (file-exists? tst)
           tst))
    (or (ormap try (force module-suffixes))
        (try #"")
        pth))
  
  ;; add-submod/filename-connections : string -> void
  (define (add-submod/filename-connections fn/submod)
    (match fn/submod
      [(? path?) (add-module-code-connections fn/submod (get-module-code fn/submod))]
      [`(submod ,filename ,sub-mods ...)
       (add-module-code-connections 
        fn/submod
        (get-module-code filename #:submodule-path sub-mods))]))
  
  (define (add-module-code-connections module-name module-code)
    (unless (hash-ref visited-hash-table module-name #f)
      (async-channel-put progress-channel (format adding-file module-name))
      (hash-set! visited-hash-table module-name #t)
      (define import-assoc (module-compiled-imports module-code))
      (for ([line (in-list import-assoc)])
        (define level (car line))
        (define mpis (cdr line))
        (define requires (extract-filenames mpis module-name))
        (for ([require (in-list requires)])
          (add-connection module-name
                          (req-r-mpi require)
                          level)
          (add-submod/filename-connections (req-r-mpi require))))))
  
  ;; add-connection : string string (union symbol #f) number -> void
  ;; name-original and name-require and the identifiers for those paths and
  ;; original-filename? and require-filename? are booleans indicating if the names
  ;; are filenames.
  (define (add-connection name-original name-require require-depth)
    (async-channel-put connection-channel
                       (list name-original name-require require-depth)))
  
  (define (extract-module-name stx)
    (syntax-case stx ()
      [(module m-name rest ...)
       (and (eq? (syntax-e (syntax module)) 'module)
            (identifier? (syntax m-name)))
       (format "~a" (syntax->datum (syntax m-name)))]
      [else unknown-module-name]))

  ;; extract-filenames :
  ;;   (listof (union symbol module-path-index)) 
  ;;   result-of-resolve-module-path-index/but-with-simplified-paths
  ;;   -> (listof req)
  (define (extract-filenames direct-requires base/submod)
    (define base
      (match base/submod
        [`(submod ,p ,_ ...) p]
        [else base/submod]))
    (for*/list ([dr (in-list direct-requires)]
                [r-mpi (in-value (and (module-path-index? dr)
                                      (resolve-module-path-index dr base)))]
                #:when (or (path? r-mpi) (pair? r-mpi)))
      (define r-mpi-filename
        (match r-mpi
          [(? path-string?) r-mpi]
          [`(submod ,p ,_ ...) p]))
      (build-module-filename r-mpi-filename #t)
      (define true-filename #f)
      (get-module-path r-mpi-filename
                       #:choose (λ (src _1 _2) (set! true-filename src) #f)
                       )
      (make-req (match r-mpi
                  [(? path?) (simplify-path true-filename)]
                  [`(submod ,p ,submods ...) `(submod ,(simplify-path true-filename) ,@submods)]))))

  (define (to-path r-mpi)
    (match r-mpi
      [(? path? p) p]
      [`(submod ,(? path? p) ,_ ...) p]
      [(? symbol?) #f])))

(define (standalone-module-overview/file filename)
  (module-overview/file filename #f standalone-fill-pasteboard
                        frame% editor-canvas% pasteboard%))
  
(define (module-overview/file filename parent fill-pasteboard 
                              overview-frame% overview-editor-canvas% overview-pasteboard%
                              #:on-boxed-word-double-click [on-boxed-word-double-click void])
  (define progress-eventspace (make-eventspace))
  (define progress-frame (parameterize ([current-eventspace progress-eventspace])
                           (new frame% (parent parent) (label progress-label) (width 600))))
  (define progress-message (new message% (label "") (stretchable-width #t) (parent progress-frame)))
  
  (define thd 
    (thread
     (λ ()
       (sleep 2)
       (parameterize ([current-eventspace progress-eventspace])
         (queue-callback
          (λ ()
            (send progress-frame show #t)))))))
  
  
  (define update-label void)
  (define callback-queued?-sema (make-semaphore 1))
  (define timer #f)
  (define (show-status str)
    (semaphore-wait callback-queued?-sema)
    (cond
      [timer
       (semaphore-post callback-queued?-sema)]
      [else
       (set! timer (new timer%
                        [notify-callback
                         (λ ()
                           (semaphore-wait callback-queued?-sema)
                           (set! timer #f)
                           (semaphore-post callback-queued?-sema)
                           (send progress-message set-label (gui-utils:trim-string str 200)))]
                        [interval 50]
                        [just-once? #t]))
       (semaphore-post callback-queued?-sema)]))
  
  (define pasteboard (make-module-overview-pasteboard 
                      #f
                      (λ (x) (update-label x))
                      overview-pasteboard%))
  
  (define success? (fill-pasteboard pasteboard filename show-status void))
  (kill-thread thd)
  (parameterize ([current-eventspace progress-eventspace])
    (queue-callback (λ () (send progress-frame show #f))))
  (when success?
    (define frame
      (new overview-frame%
           [label (string-constant module-browser)]
           [width (preferences:get 'drracket:module-overview:window-width)]
           [height (preferences:get 'drracket:module-overview:window-height)]
           [alignment '(left center)]))
    (define vp
      (new vertical-panel%
           [parent
            (if (method-in-interface? 'get-area-container (object-interface frame))
                (send frame get-area-container)
                frame)]
           [alignment '(left center)]))
    (define root-message
      (instantiate message% ()
        [label (format (string-constant module-browser-root-filename) filename)]
        [parent vp]
        [stretchable-width #t]))
    (define label-message
      (instantiate message% ()
        [label ""]
        [parent vp]
        [stretchable-width #t]))
    (define font/label-panel (new horizontal-panel% [parent vp] [stretchable-height #f]))
    (define pkg-choice
      (new module-browser-pkg-set-choice% [parent font/label-panel] [pasteboard pasteboard]))
    (define submod-choice
      (new module-browser-submod-set-choice% [parent font/label-panel] [pasteboard pasteboard]))
    (define font-size-gauge
      (instantiate slider% ()
        [label font-size-gauge-label]
        [min-value 1]
        [max-value 72]
        [init-value (preferences:get 'drracket:module-overview:label-font-size)]
        [parent font/label-panel]
        [callback (λ (x y) (send pasteboard set-label-font-size (send font-size-gauge get-value)))]))
    (define module-browser-name-length-choice
      (new choice%
           (parent font/label-panel)
           (label (string-constant module-browser-name-length))
           (choices (list (string-constant module-browser-name-long)
                          (string-constant module-browser-name-very-long)))
           (selection (case (preferences:get 'drracket:module-browser:name-length)
                        [(0) 0]
                        [(1) 0]
                        [(2) 0]
                        [(3) 1]))
           (callback
            (λ (x y)
              ;; note: the preference drracket:module-browser:name-length is also used for
              ;; the View|Show Module Browser version of the module browser
              ;; here we just treat any pref value except '3' as if it were for the long names.
              (let ([selection (send module-browser-name-length-choice get-selection)])
                (preferences:set 'drracket:module-browser:name-length (+ 2 selection))
                (send pasteboard
                      set-name-length
                      (case selection
                        [(0) 'long]
                        [(1) 'very-long])))))))
    (send pkg-choice set-string-selection (send pasteboard get-main-file-pkg))
    
    (define ec (make-object overview-editor-canvas% vp pasteboard))
    
    (define search-hp (new horizontal-panel% [parent vp] [stretchable-height #f]))
    (define search-tf
      (new text-field%
           [label (string-constant module-browser-highlight)]
           [parent search-hp]
           [callback
            (λ (tf evt)
              (define val (send tf get-value))
              (define reg (and (not (string=? val "")) (regexp (regexp-quote (send tf get-value)))))
              (update-found-and-search-hits reg))]))
    (define search-hits (new message% [parent search-hp] [label ""] [auto-resize #t]))
    (define (update-found-and-search-hits reg)
      (send pasteboard begin-edit-sequence)
      (define count 0)
      (define phases (set))
      (let loop ([snip (send pasteboard find-first-snip)])
        (when snip
          (when (is-a? snip boxed-word-snip<%>)
            (define fn (send snip get-filename))
            (define found? (and reg fn (regexp-match reg (path->string fn))))
            (when (or (not reg) found?)
              (for ([phase (in-list (send snip get-require-phases))])
                (set! phases (set-add phases phase)))
              (set! count (+ count 1)))
            (send snip set-found! found?))
          (loop (send snip next))))
    
      (send search-hits
            set-label
            (string-append (if reg
                               (format "~a found" count)
                               (format "~a total" count))
                           (render-phases phases)))
      (send pasteboard end-edit-sequence))
    (update-found-and-search-hits #f) ;; only to initialize search-hits
    
    (set! update-label
          (λ (s)
            (if (and s (not (null? s)))
                (let* ([currently-over (car s)]
                       [fn (send currently-over get-filename)]
                       [lines (send currently-over get-lines)])
                  (when (and fn lines)
                    (define label (format filename-constant fn lines))
                    (define pkg (send currently-over get-pkg))
                    (when pkg
                      (set! label (string-append (format pkg-constant pkg) "  " label)))
                    (send label-message set-label label)))
                (send label-message set-label ""))))
    
    (send pasteboard
          set-name-length
          (case (preferences:get 'drracket:module-browser:name-length)
            [(0) 'long]
            [(1) 'long]
            [(2) 'long]
            [(3) 'very-long]))
    ;; shouldn't be necessary here -- need to find callback on editor
    (send pasteboard render-snips)
    
    (send frame show #t)))

(define module-browser-choice%
  (class canvas%
    (init-field pasteboard)
    (init-field label get-choices get-selected-choices update-selected-choices
                choice->string choice-comparison-for-sorting)
    (define/private (choice->label-string choice)
      (define str (choice->string choice))
      (cond
        [(<= (string-length str) 200) str]
        [else (string-append (substring str 0 197) "...")]))
    (define choices '())
    (define selected (make-hash))
    (define in? #f)
    (super-new [style '(transparent)])
    (when pasteboard (choices-refreshed))
    (inherit get-client-size popup-menu refresh
             min-width min-height get-dc
             stretchable-width stretchable-height)
    (let ()
      (send (get-dc) set-font normal-control-font)
      (send (get-dc) set-smoothing 'smoothed)
      (define-values (tw th _1 _2) (send (get-dc) get-text-extent label))
      (min-width (+ menu-based-set-choice-inset (inexact->exact (ceiling tw)) menu-based-set-choice-inset))
      (min-height (+ menu-based-set-choice-inset (inexact->exact (ceiling th)) menu-based-set-choice-inset))
      (stretchable-width #f)
      (stretchable-height #f))

    (define/public (set-pasteboard _pb)
      (set! pasteboard _pb)
      (choices-refreshed))
    (define/public (choices-refreshed)
      (unless pasteboard (error 'choices-refreshed "pasteboard hasn't been set yet"))
      (set! selected (make-hash))
      (set! choices (sort (set->list (get-choices pasteboard)) choice-comparison-for-sorting))
      (for ([choice (in-list choices)])
        (hash-set! selected choice #f))
      (for ([choice (in-set (get-selected-choices pasteboard))])
        (hash-set! selected choice #t)))
    (define/private (update-the-pasteboard)
      (define new-choices
        (for/set ([selection (in-list (get-selections))])
          (list-ref choices selection)))
      (update-selected-choices pasteboard new-choices))

    (define/override (on-paint)
      (define dc (get-dc))
      (define-values (cw ch) (get-client-size))
      (define-values (tw th _1 _2) (send dc get-text-extent label))
      (when in?
        (define color (if (white-on-black-panel-scheme?) 0.5 0.2))
        (send dc set-pen "black" 1 'transparent)
        (send dc set-brush (get-label-foreground-color) 'solid)
        (define alpha (send dc get-alpha))
        (send dc set-alpha color)
        (send dc draw-rounded-rectangle 0 0 cw ch)
        (send dc set-alpha alpha))
      (send dc draw-text
            label
            (- (/ cw 2) (/ tw 2))
            (- (/ ch 2) (/ th 2))))
    (define/override (on-event evt)
      (super on-event evt)
      (cond
        [(send evt entering?)
         (set-in? #t)]
        [(send evt leaving?)
         (set-in? #f)]
        [(send evt button-down?)
         (unless pasteboard
           (error 'module-browser-choice%
                  "pasteboard hasn't been set yet but we got a button down event"))
         (define-values (cw ch) (get-client-size))
         (define menu (new popup-menu%))
         (for ([choice (in-list choices)])
           (define item
             (new checkable-menu-item%
                  [parent menu]
                  [label (choice->label-string choice)]
                  [callback (λ (item evt)
                              (hash-update! selected choice (λ (v) (not v)))
                              (update-the-pasteboard))]))
           (send item check (hash-ref selected choice)))
         (popup-menu menu 0 ch)]))

    (define/private (set-in? _in?)
      (unless (equal? in? _in?)
        (set! in? _in?)
        (refresh)))

    (define/public (set-string-selection s)
      (for ([choice (in-list choices)])
        (hash-set! selected choice (equal? s choice))))
    (define/public (get-selections)
      (filter
       values
       (for/list ([choice (in-list choices)]
                  [i (in-naturals)])
         (and (hash-ref selected choice) i))))))

(define module-browser-pkg-set-choice%
  (class module-browser-choice%
    (super-new [label sc-visible-pkgs]
               [get-choices (λ (pasteboard) (send pasteboard get-pkgs))]
               [get-selected-choices (λ (pasteboard) (send pasteboard get-pkg-restriction))]
               [update-selected-choices
                (λ (pasteboard pkgs)
                  (send pasteboard restrict-files-to-pkgs pkgs))]
               [choice->string (λ (x) x)]
               [choice-comparison-for-sorting string<?])))

(define module-browser-submod-set-choice%
  (class module-browser-choice%
    (super-new [label sc-visible-submodules]
               [get-choices (λ (pasteboard) (send pasteboard get-submods))]
               [get-selected-choices (λ (pasteboard) (send pasteboard get-submod-restriction))]
               [update-selected-choices
                (λ (pasteboard submods)
                  (send pasteboard restrict-files-to-submods submods))]
               [choice->string
                (λ (l)
                  (match l
                    ['() sc-top-level-module]
                    [(cons x xs)
                     (string-append
                      "(submod ... "
                      (apply string-append (add-between (map symbol->string l) " "))
                      ")")]))]
               [choice-comparison-for-sorting
                (λ (x y)
                  (cond
                    [(= (length x) (length y))
                     (string<? (format "~s" x) (format "~s" y))]
                    [else
                     (< (length x) (length y))]))])))

(define menu-based-set-choice-inset 4)
;; make-module-overview-pasteboard : boolean
;;                                   ((union #f snip) -> void)
;;                                -> (union string pasteboard)
;; string as result indicates an error message
;; pasteboard as result is the pasteboard to show
(define (make-module-overview-pasteboard vertical? mouse-currently-over 
                                         overview-pasteboard%
                                         #:on-boxed-word-double-click
                                         [on-boxed-word-double-click #f])
  
  (define level-ht (make-hash))
  
  ;; snip-table : hash-table[sym -o> snip]
  (define snip-table (make-hash))
  (define label-font (find-label-font (preferences:get 'drracket:module-overview:label-font-size)))
  (define text-color "blue")
  
  (define search-result-text-color "white")
  (define search-result-background "forestgreen")
  
  (define dark-syntax-pen (send the-pen-list find-or-create-pen "darkorchid" 1 'solid))
  (define dark-syntax-brush (send the-brush-list find-or-create-brush "darkorchid" 'solid))
  (define light-syntax-pen (send the-pen-list find-or-create-pen "plum" 1 'solid))
  (define light-syntax-brush (send the-brush-list find-or-create-brush "plum" 'solid))
  
  (define dark-template-pen (send the-pen-list find-or-create-pen "seagreen" 1 'solid))
  (define dark-template-brush (send the-brush-list find-or-create-brush "seagreen" 'solid))
  (define light-template-pen (send the-pen-list find-or-create-pen "springgreen" 1 'solid))
  (define light-template-brush (send the-brush-list find-or-create-brush "springgreen" 'solid))
  
  (define dark-pen (send the-pen-list find-or-create-pen "blue" 1 'solid))
  (define dark-brush (send the-brush-list find-or-create-brush "blue" 'solid))
  (define light-pen (send the-pen-list find-or-create-pen "light blue" 1 'solid))
  (define light-brush (send the-brush-list find-or-create-brush "light blue" 'solid))
  
  (define (module-overview-pasteboard-mixin %)
    (class* % (module-overview-pasteboard<%>)
      
      (inherit get-snip-location
               begin-edit-sequence
               end-edit-sequence
               insert
               move-to
               find-first-snip
               dc-location-to-editor-location
               find-snip
               get-canvas)
      
      ;; require-depth-ht : hash[(list snip snip) -o> (listof integer)]
      ;; maps parent/child snips (ie, those that match up to modules 
      ;; that require each other) to phase differences
      (define require-depth-ht (make-hash))

      (define original-plain-links (make-hash))
      (define original-for-syntax-links (make-hash))
      (define roots '())

      ;; (or/c #f (set/c string?)) -- #f when uninitialized
      (define pkg-restriction #f)
      (define/public (get-pkg-restriction) pkg-restriction)
      (define/public (restrict-files-to-pkgs pkgs)
        (unless (equal? pkgs pkg-restriction)
          (set! pkg-restriction pkgs)
          (remove-and-re-add-all)))

      ;; (set/c (listof symbol?))
      (define submod-restriction (apply set (preferences:get 'drracket:module-browser:visible-submodules)))
      (define/public (get-submod-restriction) submod-restriction)
      (define/public (restrict-files-to-submods submods)
        (preferences:set 'drracket:module-browser:visible-submodules
                         (sort (set->list submods)
                               string<?
                               #:key (λ (x) (format "~s" x))))
        (unless (equal? submods submod-restriction)
          (set! submod-restriction submods)
          (remove-and-re-add-all)))

      (define/private (remove-and-re-add-all)
        (begin-edit-sequence)
        (remove-currrently-inserted)
        (add-all)
        (end-edit-sequence)
        (render-snips))

      (define path->pkg-cache (make-hash))
      (define all-pkgs #f)
      (define/public (get-pkgs)
        (unless all-pkgs (error 'get-pkgs "not yet computed"))
        all-pkgs)
      (define main-file-pkg #f)
      (define/public (get-main-file-pkg)
        (unless main-file-pkg (error 'get-main-file-pkg "not yet computed"))
        main-file-pkg)

      (define all-submods #f)
      (define/public (get-submods)
        (unless all-submods (error 'get-submods "not yet computed"))
        all-submods)
      
      (define name-length 'long)
      (define/public (set-name-length nl)
        (unless (eq? name-length nl)
          (set! name-length nl)
          (begin-edit-sequence)
          (remove-currrently-inserted)
          (add-all)
          (end-edit-sequence)
          (render-snips)))
      (define/public (get-name-length) name-length)
      
      (field [max-lines #f])
      
      ;; controls if the snips should be moved
      ;; around when the font size is changed.
      ;; set to #f if the user ever moves a
      ;; snip themselves.
      (define dont-move-snips #f)
      
      (field (label-font-size (preferences:get 'drracket:module-overview:label-font-size)))
      (define/public (get-label-font-size) label-font-size)
      (define/private (get-snip-hspace) (if vertical?
                                            2
                                            (* 2 label-font-size)))
      (define/private (get-snip-vspace) (if vertical?
                                            30
                                            2))
      (define snip-height #f)
      
      (define font-label-size-callback-running? #f)
      (define new-font-size #f)
      (define/public (set-label-font-size size-to-set)
        (set! new-font-size size-to-set)
        (unless font-label-size-callback-running?
          (set! font-label-size-callback-running? #t)
          (queue-callback
           (λ ()
             (set! label-font-size new-font-size)
             (preferences:set 'drracket:module-overview:label-font-size 
                              new-font-size)
             (set! label-font (find-label-font label-font-size))
             (begin-edit-sequence)
             (let loop ([snip (find-first-snip)])
               (when snip
                 (let ([admin (send snip get-admin)])
                   (when admin
                     (send admin resized snip #t)))
                 (loop (send snip next))))
             (unless dont-move-snips
               (render-snips))
             (end-edit-sequence)
             (set! new-font-size #f)
             (set! font-label-size-callback-running? #f))
           #f)))
      
      (define/public (begin-adding-connections init-filename)
        (set! main-file-pkg (path->pkg-as-string init-filename path->pkg-cache))
        (when max-lines
          (error 'begin-adding-connections
                 "already in begin-adding-connections/end-adding-connections sequence"))
        (set! max-lines 0)
        (begin-edit-sequence)
        (let loop ()
          (define s (find-first-snip))
          (when s
            (send s release-from-owner)
            (loop)))
        (set! level-ht (make-hasheq))
        (set! snip-table (make-hash))
        (set! roots '())
        (set! original-plain-links (make-hash))
        (set! original-for-syntax-links (make-hash)))
      
      (define/public (end-adding-connections)
        (unless max-lines
          (error 'end-adding-connections
                 "not in begin-adding-connections/end-adding-connections sequence"))
        
        (unless (zero? max-lines)
          (define-values (all-the-pkgs all-the-submods)
            (let loop ([snip (find-first-snip)]
                       [all-pkgs (set main-file-pkg)]
                       [all-submods (set)])
              (cond
                [(not snip) (values all-pkgs all-submods)]
                [(is-a? snip word-snip/lines%)
                 (send snip normalize-lines max-lines)
                 (define pkg (send snip get-pkg))
                 (loop (send snip next)
                       (set-add all-pkgs pkg)
                       (set-add all-submods (send snip get-submods)))]
                [else
                 (loop (send snip next)
                       all-pkgs
                       all-submods)])))
          (set! all-pkgs all-the-pkgs)
          (set! all-submods all-the-submods))
        
        (set! max-lines #f)
        (compute-snip-require-phases)
        ;; setting pkg-restriction ensures that
        ;; restrict-files-to-pkgs moves snips properly
        (set! pkg-restriction #f)
        (restrict-files-to-pkgs (set main-file-pkg))
        (end-edit-sequence))
      
      (define/private (compute-snip-require-phases)
        (define ht (make-hash)) ;; avoid infinite loops
        (for ([snip (in-list (get-top-most-snips))])
          (let loop ([parent snip]
                     [depth 0]) ;; depth is either an integer or #f (indicating for-label)
            (unless (hash-ref ht (cons parent depth) #f)
              (hash-set! ht (cons parent depth) #t)
              (send parent add-require-phase depth)
              (for* ([child (in-list (send parent get-children))]
                     [delta-depth (in-list (hash-ref require-depth-ht (list parent child)))])
                (loop child (and depth delta-depth (+ delta-depth depth))))))))
      
      ;; add-connection : path/string/submod path/string/submod (union symbol #f) number -> void
      ;; name-original and name-require and the identifiers for those paths and
      ;; original-filename? and require-filename? are booleans indicating if the names
      ;; are filenames.
      (define/public (add-connection name-original name-require require-depth)
        (unless max-lines
          (error 'add-connection "not in begin-adding-connections/end-adding-connections sequence"))
        (define original-snip (find/create-snip name-original))
        (define require-snip (find/create-snip name-require))
        (set! roots (remove require-snip roots))
        (let ([require-depth-key (list original-snip require-snip)])
          (hash-update! require-depth-ht require-depth-key (λ (v) (cons require-depth v)) '()))
        (define table-to-add-to (if (equal? require-depth 0) original-plain-links original-for-syntax-links))
        (define previous-children (hash-ref table-to-add-to original-snip '()))
        (unless (member require-snip previous-children)
          (hash-set! table-to-add-to original-snip (cons require-snip previous-children))))

      (define/private (add-regular-link original-snip require-snip)
        (add-links original-snip require-snip
                   dark-pen light-pen
                   dark-brush light-brush))

      (define/private (add-for-syntax-link original-snip require-snip)
        (add-links original-snip require-snip
                   dark-syntax-pen light-syntax-pen
                   dark-syntax-brush light-syntax-brush))

      (define/private (fix-snip-level-after-linking original-snip require-snip)
        (define original-level (send original-snip get-level))
        (if original-level
            (fix-snip-level require-snip (+ original-level 1))
            (fix-snip-level original-snip 0)))
      
      ;; fix-snip-level : snip number -> void
      ;; moves the snip (and any children) to at least `new-level'
      ;; doesn't move them if they are already past that level
      (define/private (fix-snip-level snip new-min-level)
        (define cycle-guard (make-hash))
        (let loop ([snip snip]
                   [new-min-level new-min-level])
          (unless (hash-ref cycle-guard snip #f)
            (hash-set! cycle-guard snip #t)
            (define current-level (send snip get-level))
            (when (or (not current-level)
                      (new-min-level . > . current-level))
              (send snip set-level new-min-level)
              (for ([child (in-list (send snip get-children))])
                (loop child (+ new-min-level 1)))))))
      
      ;; find/create-snip : (union path string) boolean? -> word-snip/lines
      ;; finds the snip with this key, or creates a new
      ;; ones. For the same key, always returns the same snip.
      ;; uses snip-table as a cache for this purpose.
      (define/private (find/create-snip name)
        (hash-ref
         snip-table
         name
         (λ ()
           (define-values (filename submods)
             (match name
               [(? path-string?) (values (and (file-exists? name) name) '())]
               [`(submod ,p ,submods ...)
                (values (and (file-exists? p) p)
                        submods)]
               [else (values #f '())]))
           (define snip
             (new word-snip/lines%
                  [lines (and filename (count-lines filename))]
                  [word 
                   (cond
                     [filename
                      (define short-name
                        (let-values ([(_1 name _2) (split-path filename)])
                          (path->string name)))
                      (match name
                        [(? path-string?) short-name]
                        [`(submod ,p ,submods ...) (format "~s" `(submod ,short-name ,@submods))])]
                     [else (format "~a" name)])]
                  [pb this]
                  [filename filename]
                  [pkg (cond
                         [(and filename (path->pkg filename #:cache path->pkg-cache))
                          => values]
                         [(and filename (is-in-main-collects? filename))
                          sc-main-collects]
                         [else sc-unknown-pkg])]
                  [submods submods]))
           (insert snip)
           (hash-set! snip-table name snip)
           (set! roots (cons snip roots))
           snip)))
      
      ;; count-lines : string[filename] -> (union #f number)
      ;; effect: updates max-lines
      (define/private (count-lines filename)
        (define lines
          (call-with-input-file filename
                                (λ (port)
                                  (let loop ([n 0])
                                    (define l (read-line port))
                                    (if (eof-object? l)
                                        n
                                        (loop (+ n 1)))))
                                #:mode 'text))
        (set! max-lines (max lines max-lines))
        lines)
      
      ;; get-snip-width : snip -> number
      ;; exracts the width of a snip
      (define/private (get-snip-width snip)
        (define lb (box 0))
        (define rb (box 0))
        (get-snip-location snip lb #f #f)
        (get-snip-location snip rb #f #t)
        (- (unbox rb) (unbox lb)))
      
      ;; get-snip-height : snip -> number
      ;; exracts the width of a snip
      (define/private (get-snip-height snip)
        (define tb (box 0))
        (define bb (box 0))
        (get-snip-location snip #f tb #f)
        (get-snip-location snip #f bb #t)
        (- (unbox bb) (unbox tb)))

      (define/private (remove-currrently-inserted)
        (let loop ([snip (find-first-snip)])
          (when snip
            (for ([child (in-list (send snip get-children))])
              (remove-links snip child))
            (loop (send snip next))))
        (let loop ()
          (define snip (find-first-snip))
          (when snip
            (send snip release-from-owner)
            (loop))))

      (define/private (add-all)
        (define visited (make-hash))
        (reset-levels)
        (for ([root (in-list roots)])
          (when (show-this-one? root)
            (insert root)
            (send root set-level 0))
          (let loop ([parent-to-link (and (show-this-one? root) root)]
                     [parent root]
                     [through-for-syntax? #f])
            (unless (hash-ref visited parent #f)
              (hash-set! visited parent #t)
              (define (continue child for-syntax-child?)
                (cond
                  [(show-this-one? child)
                   (insert child)
                   (cond
                     [parent-to-link
                      (if (or for-syntax-child? through-for-syntax?)
                          (add-for-syntax-link parent-to-link child)
                          (add-regular-link parent-to-link child))
                      (fix-snip-level-after-linking parent-to-link child)]
                     [else (fix-snip-level child 0)])
                   (loop child child #f)]
                  [else
                   (loop parent-to-link child (or through-for-syntax? for-syntax-child?))]))
              (for ([child (in-list (hash-ref original-plain-links parent '()))])
                (continue child #f))
              (for ([child (in-list (hash-ref original-for-syntax-links parent '()))])
                (continue child #t))))))

      (define/private (show-this-one? word-ship/lines)
        (and (set-member? submod-restriction (send word-ship/lines get-submods))
             (set-member? pkg-restriction (send word-ship/lines get-pkg))))

      (define/private (reset-levels)
        (for* ([(level snips) (in-hash level-ht)]
               [snip (in-list snips)])
          (send snip reset-level))
        (set! level-ht (make-hash)))
      
      (define/private (get-top-most-snips) (hash-ref level-ht 0 '()))
      
      ;; render-snips : -> void
      (define/public (render-snips)
        (begin-edit-sequence)
        (let ([max-minor 0])
          
          ;; major-dim is the dimension that new levels extend along
          ;; minor-dim is the dimension that snips inside a level extend along
          
          (hash-for-each
           level-ht
           (λ (n v)
             (set! max-minor (max max-minor (apply + (map (if vertical?
                                                              (λ (x) (get-snip-width x))
                                                              (λ (x) (get-snip-height x)))
                                                          v))))))
          
          (define levels (sort (hash-map level-ht list) (λ (x y) (<= (car x) (car y)))))
          (let loop ([levels levels]
                     [major-dim 0])
            (cond
              [(null? levels) (void)]
              [else
               (let* ([level (car levels)]
                      [n (car level)]
                      [this-level-snips (cadr level)]
                      [this-minor (apply +
                                         (map (if vertical?
                                                  (λ (x) (get-snip-width x))
                                                  (λ (x) (get-snip-height x)))
                                              this-level-snips))]
                      [this-major (apply max
                                         0
                                         (map (if vertical?
                                                  (λ (x) (get-snip-height x))
                                                  (λ (x) (get-snip-width x)))
                                              this-level-snips))])
                 (let loop ([snips this-level-snips]
                            [minor-dim (/ (- max-minor this-minor) 2)])
                   (unless (null? snips)
                     (let* ([snip (car snips)]
                            [new-major-coord (+ major-dim
                                                (floor (- (/ this-major 2)
                                                          (/ (if vertical?
                                                                 (get-snip-height snip)
                                                                 (get-snip-width snip))
                                                             2))))])
                       (if vertical?
                           (move-to snip minor-dim new-major-coord)
                           (move-to snip new-major-coord minor-dim))
                       (loop (cdr snips)
                             (+ minor-dim
                                (if vertical?
                                    (get-snip-hspace)
                                    (get-snip-vspace))
                                (if vertical?
                                    (get-snip-width snip)
                                    (get-snip-height snip)))))))
                 (loop (cdr levels)
                       (+ major-dim
                          (if vertical?
                              (get-snip-vspace)
                              (get-snip-hspace))
                          this-major)))])))
        (end-edit-sequence))
      
      (define/override (on-mouse-over-snips snips)
        (mouse-currently-over snips))
      
      (define/override (on-double-click snip event)
        (cond
          [(is-a? snip boxed-word-snip<%>)
           (when on-boxed-word-double-click
             (on-boxed-word-double-click (send snip get-filename)))]
          [else (super on-double-click snip event)]))
      
      (define/override (on-event evt)
        (cond
          [(and on-boxed-word-double-click (send evt button-down? 'right))
           (let ([ex (send evt get-x)]
                 [ey (send evt get-y)])
             (let-values ([(x y) (dc-location-to-editor-location ex ey)])
               (let ([snip (find-snip x y)]
                     [canvas (get-canvas)])
                 (let ([right-button-menu (make-object popup-menu%)])
                   (when (and snip
                              (is-a? snip boxed-word-snip<%>)
                              canvas
                              (send snip get-filename))
                     (new menu-item%
                          [label 
                           (trim-string
                            (format open-file-format
                                    (path->string (send snip get-filename)))
                            200)]
                          [parent right-button-menu]
                          [callback
                           (λ (x y)
                             (on-boxed-word-double-click
                              (send snip get-filename)))]))
                   (new menu-item%
                        [label (string-constant module-browser-open-all)]
                        [parent right-button-menu]
                        [callback
                         (λ (x y)
                           (let loop ([snip (find-first-snip)])
                             (when snip
                               (when (is-a? snip boxed-word-snip<%>)
                                 (let ([filename (send snip get-filename)])
                                   (on-boxed-word-double-click filename)))
                               (loop (send snip next)))))])
                   (send canvas popup-menu
                         right-button-menu
                         (+ (send evt get-x) 1)
                         (+ (send evt get-y) 1))))))]
          [else (super on-event evt)]))
      
      (super-new)))
  
  (define (trim-string str len)
    (cond
      [(<= (string-length str) len) str]
      [else (substring str (- (string-length str) len) (string-length str))]))
  
  (define (level-mixin %)
    (class %
      (define level #f)
      (define/public (reset-level) (set! level #f))
      (define/public (get-level) level)
      (define/public (set-level _l) 
        (when level
          (hash-set! level-ht level
                     (for/list ([snip (in-list (hash-ref level-ht level))]
                                #:unless (object=? snip this))
                       snip)))
        (set! level _l)
        (hash-update! level-ht level (λ (v) (cons this v)) '()))
      
      (super-new)))
  
  (define (boxed-word-snip-mixin %)
    (class* % (boxed-word-snip<%>)
      (init-field word
                  filename
                  lines
                  pb
                  pkg      ;; string?; might be a pkg but might be some other descriptive string
                  submods) ;; (listof symbol?); the empty list if it isn't a submodule

      (unless (string? pkg) (error 'pkg "is not a string"))
      
      (inherit get-admin)
      
      (define require-phases '())
      (define/public (get-require-phases) require-phases)
      (define/public (add-require-phase d)
        (unless (member d require-phases)
          (set! last-name #f)
          (set! last-size #f)
          (set! require-phases (sort (cons d require-phases) < #:key (λ (x) (or x +inf.0))))))
      
      (define/public (get-filename) filename)
      (define/public (get-word) word)
      (define/public (get-lines) lines)

      (define/public (get-pkg) pkg)
      (define/public (get-submods) submods)
      
      (field (lines-brush #f))
      (define/public (normalize-lines n)
        (cond
          [lines
           (define grey (inexact->exact (floor (- 255 (* 255 (sqrt (/ lines n)))))))
           (set!
            lines-brush
            (send the-brush-list find-or-create-brush (make-object color% grey grey grey) 'solid))]
          [else (set! lines-brush (send the-brush-list find-or-create-brush "salmon" 'solid))]))
      
      (define snip-width 0)
      (define snip-height 0)
      
      (define/override (get-extent dc x y wb hb descent space lspace rspace)
        (cond
          [(equal? (name->label) "")
           (set! snip-width 15)
           (set! snip-height 15)]
          [else
           (define-values (w h a d) (send dc get-text-extent (name->label) label-font))
           (set! snip-width (+ w 5))
           (set! snip-height (+ h 5))])
        (set-box/f wb snip-width)
        (set-box/f hb snip-height)
        (set-box/f descent 0)
        (set-box/f space 0)
        (set-box/f lspace 0)
        (set-box/f rspace 0))
      
      (define/public (set-found! fh?) 
        (unless (eq? (and fh? #t) found-highlight?)
          (set! found-highlight? (and fh? #t))
          (define admin (get-admin))
          (when admin
            (send admin needs-update this 0 0 snip-width snip-height))))
      (define found-highlight? #f)
      
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (define old-font (send dc get-font))
        (define old-text-foreground (send dc get-text-foreground))
        (define old-brush (send dc get-brush))
        (define old-pen (send dc get-pen))
        (send dc set-font label-font)
        (cond
          [found-highlight? (send dc set-brush search-result-background 'solid)]
          [lines-brush (send dc set-brush lines-brush)])
        (when (rectangles-intersect? left top right bottom x y (+ x snip-width) (+ y snip-height))
          (send dc draw-rectangle x y snip-width snip-height)
          (send dc
                set-text-foreground
                (send the-color-database
                      find-color
                      (if found-highlight? search-result-text-color text-color)))
          (send dc draw-text (name->label) (+ x 2) (+ y 2)))
        (send dc set-pen old-pen)
        (send dc set-brush old-brush)
        (send dc set-text-foreground old-text-foreground)
        (send dc set-font old-font))
      
      ;; name->label : path -> string
      ;; constructs a label for the little boxes in terms
      ;; of the filename.
      
      (define last-name #f)
      (define last-size #f)
      
      (define/private (name->label)
        (define this-size (send pb get-name-length))
        (cond
          [(eq? this-size last-size) last-name]
          [else
           (set! last-size this-size)
           (set!
            last-name
            (case last-size
              [(short)
               (if (string=? word "")
                   ""
                   (string (string-ref word 0)))]
              [(medium)
               (let ([m (regexp-match #rx"^(.*)\\.[^.]*$" word)])
                 (let ([short-name (if m
                                       (cadr m)
                                       word)])
                   (if (string=? short-name "")
                       ""
                       (let ([ms (regexp-match* #rx"-[^-]*" short-name)])
                         (cond
                           [(null? ms) (substring short-name 0 (min 2 (string-length short-name)))]
                           [else
                            (apply string-append
                                   (cons (substring short-name 0 1)
                                         (map (λ (x) (substring x 1 2)) ms)))])))))]
              [(long) word]
              [(very-long) (string-append word ": " (format "~s" require-phases))]))
           last-name]))
      
      (super-new)))
  
  (define word-snip/lines% (level-mixin (boxed-word-snip-mixin (graph-snip-mixin snip%))))
  
  (define draw-lines-pasteboard% (module-overview-pasteboard-mixin
                                  (graph-pasteboard-mixin
                                   overview-pasteboard%)))
  (new draw-lines-pasteboard% [cache-arrow-drawing? #t]))

(define (path->pkg-as-string filename path->pkg-cache)
  (cond
    [(not filename) sc-unknown-pkg]
    [(path->pkg filename #:cache path->pkg-cache)
     => values]
    [(is-in-main-collects? filename)
     sc-main-collects]
    [else sc-unknown-pkg]))

(define (is-in-main-collects? path)
  (define exploded (explode-path path))
  (for/or ([search-dir-path (in-list (get-main-collects-search-dirs))])
    (define search-dir-exploded (explode-path search-dir-path))
    (and (>= (length exploded)
             (length search-dir-exploded))
         (for/and ([exploded (in-list exploded)]
                   [search-dir-exploded (in-list search-dir-exploded)])
           (equal? exploded
                   search-dir-exploded)))))

(define (standalone-fill-pasteboard pasteboard filename show-status _void)
  (define progress-channel (make-async-channel))
  (define connection-channel (make-async-channel))
  (struct done (s))
  
  (define-values/invoke-unit process-program-unit
    (import process-program-import^)
    (export process-program-export^))
  
  (define user-custodian (make-custodian))
  (define error-str #f)
  
  (define init-dir (get-init-dir filename))
  
  (define (swallow-specials port)
    (define-values (in out) (make-pipe-with-specials))
    (thread
     (λ ()
       (let loop ()
         (define c (read-char-or-special in))
         (cond
           [(char? c)
            (display c out)
            (loop)]
           [(eof-object? c)
            (close-output-port out)
            (close-input-port in)]
           [else
            (loop)]))))
    out)
  
  (define user-thread
    (parameterize ([current-custodian user-custodian])
      
      ;; not that we expect anyone to really use this, but
      ;; we make it here to avoid the expanded code from interfereing
      ;; with the module browser
      (parameterize ([current-eventspace (make-eventspace)])
        
        (thread
         (λ ()
           (moddep-current-open-input-file
            (λ (filename)
              (let* ([p (open-input-file filename)]
                     [wxme? (regexp-match-peek #rx#"^WXME" p)])
                (if wxme?
                    (let ([t (new text%)])
                      (close-input-port p)
                      (send t load-file filename)
                      (let ([prt (open-input-text-editor t)])
                        (port-count-lines! prt)
                        prt))
                    p))))
           (current-load-relative-directory #f)
           (define relative? (eq? init-dir 'relative))
           (unless relative? ; already there
             (current-directory init-dir))
           (define file-path (if (string? filename)
                                 (string->path filename)
                                 filename))
           (add-connections
            (if relative? (build-path (current-directory) file-path) file-path))
           (define wait-until-done-sema (make-semaphore))
           (async-channel-put connection-channel (done wait-until-done-sema))
           (semaphore-wait wait-until-done-sema))))))
  
  (send pasteboard begin-adding-connections filename)
  (let ([evt
         (choice-evt
          (handle-evt progress-channel (λ (x) (cons 'progress x)))
          (handle-evt connection-channel
                      (λ (x)
                        (match x
                          [(done c) (cons 'done c)]
                          [else (cons 'connect x)])))
          (handle-evt user-thread (λ (x) (cons 'died #f))))])
    (let loop ()
      (define evt-value (yield evt))
      (define key (car evt-value))
      (define val (cdr evt-value))
      (case key
        [(done) (semaphore-post val)]
        [(died) (exit)]
        [(progress) 
         (show-status val)
         (loop)]
        [(connect)
         (define name-original (list-ref val 0))
         (define path-key (list-ref val 1))
         (define require-depth (list-ref val 2))
         (send pasteboard add-connection name-original path-key require-depth)
         (loop)])))
  (send pasteboard end-adding-connections)
  
  (custodian-shutdown-all user-custodian)
  
  (cond
    [error-str
     (message-box 
      (string-constant module-browser)
      (format (string-constant module-browser-error-expanding)
              error-str))
     #f]
    [else
     #t]))

;; get-init-dir : (or/c path? #f) -> path?
;; returns the initial directory for a program
;; that is saved in 'path/f' (with #f indicating
;; an unsaved file)
(define (get-init-dir path/f)
  (cond
    [path/f
     (let-values ([(base name dir?) (split-path path/f)])
       base)]
    [else
     (find-system-path 'home-dir)]))

