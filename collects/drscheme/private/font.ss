(module font mzscheme
  (require (lib "unitsig.ss")
           (lib "class.ss")
           "drsig.ss"
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "string-constant.ss" "string-constants"))
  
  (define sc-smoothing-label (string-constant font-smoothing-label))
  (define sc-smoothing-none (string-constant font-smoothing-none))
  (define sc-smoothing-some (string-constant font-smoothing-some))
  (define sc-smoothing-all (string-constant font-smoothing-all))
  
  (provide font@)
  
  (define font@
    (unit/sig drscheme:font^
      (import [drscheme:language-configuration : drscheme:language-configuration/internal^])
      
      (define get-fixed-faces
        (cond
          [(eq? (system-type) 'unix) 
           (lambda () (get-face-list))]
          [else
           (lambda ()
             (let* ([canvas (make-object canvas% (make-object frame% "bogus"))]
                    [dc (send canvas get-dc)]
                    [ans
                     (let loop ([faces (get-face-list)])
                       (cond
                         [(null? faces) null]
                         [else (let* ([face (car faces)]
                                      [font (make-object font% 12 face 'default 'normal 'normal #f)])
                                 (let*-values ([(wi _1 _2 _3) (send dc get-text-extent "i" font)]
                                               [(ww _1 _2 _3) (send dc get-text-extent "w" font)])
                                   (if (and (= ww wi) 
                                            (not (zero? ww)))
                                       (cons face (loop (cdr faces)))
                                       (loop (cdr faces)))))]))])
               (set! get-fixed-faces (lambda () ans))
               ans))]))
      
      (define (get-default-font-name)
        (get-family-builtin-face 'modern))
      
      (define (set-font-size size)
        (update-standard-style
         (lambda (scheme-delta)
           (send scheme-delta set-size-mult 0)
           (send scheme-delta set-size-add size))))
      
      (define (set-font-name name)
        (update-standard-style
         (lambda (scheme-delta)
           (send scheme-delta set-delta-face name)
           (send scheme-delta set-family 'modern))))
      
      (define (set-font-smoothing sym)
        (update-standard-style
         (lambda (scheme-delta)
           (send scheme-delta set-smoothing-on sym))))
      
      (define (update-standard-style cng-delta)
        (let* ([scheme-standard (send (editor:get-standard-style-list) find-named-style "Standard")]
               [scheme-delta (make-object style-delta%)])
          (send scheme-standard get-delta scheme-delta)
          (cng-delta scheme-delta)
          (send scheme-standard set-delta scheme-delta)))
        
      
      (define (setup-preferences)
        (set-font-size (preferences:get 'drscheme:font-size))
        (set-font-name (preferences:get 'drscheme:font-name))
        (set-font-smoothing (preferences:get 'drscheme:font-smoothing))
        (preferences:add-callback 'drscheme:font-size (lambda (p v) (set-font-size v)))
        (preferences:add-callback 'drscheme:font-name (lambda (p v) (set-font-name v)))
        (preferences:add-callback 'drscheme:font-smoothing (lambda (p v) (set-font-smoothing v)))
        
        (unless (member (preferences:get 'drscheme:font-name)
                        (get-fixed-faces))
          (preferences:set 'drscheme:font-name (get-default-font-name)))
        
        (preferences:add-panel
         (list (string-constant editor-prefs-panel-label) 
               (string-constant font-prefs-panel-title))
         (lambda (panel)
           (let* ([main (make-object vertical-panel% panel)]
                  [options-panel (make-object horizontal-panel% main)]
                  [size (make-object slider% (string-constant font-size) 1 72 options-panel
                          (lambda (size evt)
                            (preferences:set 'drscheme:font-size (send size get-value)))
                          (preferences:get 'drscheme:font-size))]
                  
                  [choice-panel (new vertical-panel% (parent options-panel))]
                  [font-name-control
                   (case (system-type)
                     [(windows macos macosx)
                      (let ([choice
                             (new choice%
                                  (label (string-constant font-name))
                                  (choices (get-fixed-faces))
                                  (parent choice-panel)
                                  (stretchable-width #t)
                                  (callback
                                   (lambda (font-name evt)
                                     (preferences:set 
                                      'drscheme:font-name
                                      (send font-name get-string-selection)))))])
                        (send choice set-string-selection (preferences:get 'drscheme:font-name))
                        choice)]
                     [(unix)
                      (make-object button%
                        (string-constant set-font)
                        choice-panel
                        (lambda xxx
                          (let* ([faces (get-fixed-faces)]
                                 [init-choices
                                  (let ([init (preferences:get 'drscheme:font-name)])
                                    (let loop ([faces faces]
                                               [num 0])
                                      (cond
                                        [(null? faces) null]
                                        [else
                                         (let ([face (car faces)])
                                           (if (equal? init face)
                                               (list num)
                                               (loop (cdr faces)
                                                     (+ num 1))))])))]
                                 [choice (get-choices-from-user
                                          (string-constant select-font-name)
                                          (string-constant select-font-name)
                                          (get-fixed-faces)
                                          #f
                                          init-choices)])
                            (when choice
                              (preferences:set 
                               'drscheme:font-name 
                               (list-ref (get-fixed-faces) (car choice)))))))]
                     [else (error 'font-name-control "unknown system type: ~s~n" (system-type))])]
                  [smoothing-contol
                   (new choice%
                        (label sc-smoothing-label)
                        (choices (list sc-smoothing-none
                                       sc-smoothing-some
                                       sc-smoothing-all))
                        (parent choice-panel)
                        (stretchable-width #t)
                        (selection (case (preferences:get 'drscheme:font-smoothing)
                                     [(unsmoothed) 0]
                                     [(partly-smoothed) 1]
                                     [(smoothed) 2]))
                        (callback (lambda (x y) 
                                    (preferences:set 
                                     'drscheme:font-smoothing
                                     (case (send x get-selection)
                                       [(0) 'unsmoothed]
                                       [(1) 'partly-smoothed]
                                       [(2) 'smoothed])))))]
                  
                  [text (make-object (editor:standard-style-list-mixin text%))]
                  [ex-panel (make-object horizontal-panel% main)]
                  [msg (make-object message% (string-constant example-text) ex-panel)]
                  [canvas (make-object editor-canvas% main text)]
                  [update-text
                   (lambda (setting)
                     (send text begin-edit-sequence)
                     (send text lock #f)
                     (send text erase)
                     (send text insert 
                           (format
                            ";; howmany : list-of-numbers -> number~
                           \n;; to determine how many numbers are in `a-lon'~
                           \n(define (howmany a-lon)~
                           \n  (cond~
                           \n    [(empty? a-lon) 0]~
                           \n    [else (+ 1 (howmany (rest a-lon)))]))~
                           \n~
                           \n;; examples as tests~
                           \n(howmany empty)~
                           \n=~
                           \n0~
                           \n~
                           \n(howmany (cons 1 (cons 2 (cons 3 empty))))
                           \n\"should be\"~
                           \n3"))
                     (send text set-position 0 0)
                     (send text lock #t)
                     (send text end-edit-sequence))])
             
             (preferences:add-callback
              'drscheme:font-size
              (lambda (p v) (send size set-value v)))
             (preferences:add-callback
              drscheme:language-configuration:settings-preferences-symbol
              (lambda (p v)
                (update-text v)))
             (update-text (preferences:get drscheme:language-configuration:settings-preferences-symbol))
             (send ex-panel set-alignment 'left 'center)
             (send ex-panel stretchable-height #f)
             (send canvas allow-tab-exit #t)
             (send options-panel stretchable-height #f)
             (send options-panel set-alignment 'center 'top)
             (send text lock #t)
             main)))))))