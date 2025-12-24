#lang racket/unit

(require racket/gui/base
         browser/external
         framework
         racket/class
         net/url
         setup/dirs
         setup/materialize-user-docs
         help/search
         drracket/private/drsig
         string-constants
         setup/dirs
         "local-member-names.rkt")

(import [prefix drracket:frame: drracket:frame^]
        [prefix drracket:language-configuration: drracket:language-configuration/internal^]
        [prefix drracket:init: drracket:init/int^])
(export drracket:help-desk^)

(define (-add-help-desk-font-prefs b) '(add-help-desk-font-prefs b))

;; : -> string
(define (get-computer-language-info)
  (let* ([language/settings (preferences:get 
                             drracket:language-configuration:settings-preferences-symbol)]
         [language (drracket:language-configuration:language-settings-language
                    language/settings)]
         [settings (drracket:language-configuration:language-settings-settings
                    language/settings)])
    (format
     "~s"
     (list
      (send language get-language-position)
      (send language marshall-settings settings)))))

(define lang-message%
  (class canvas%
    (init-field button-release font)
    (define/override (on-event evt)
      (when (send evt button-up?)
        (button-release)))
    (field [msg ""])
    (define/public (set-msg l) (set! msg l) (on-paint))
    (inherit get-dc get-client-size)
    (define/override (on-paint)
      (let ([dc (get-dc)]
            [dots "..."])
        (let-values ([(tw th _1 _2) (send dc get-text-extent msg #f 'grapheme)]
                     [(dw dh _3 _4) (send dc get-text-extent dots)]
                     [(cw ch) (get-client-size)])
          (send dc set-brush (send the-brush-list find-or-create-brush (get-panel-background) 'panel))
          (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
          (send dc set-font font)
          (send dc draw-rectangle 0 0 cw ch)
          (cond
            [(tw . <= . cw)
             (send dc draw-text msg 0 (- (/ ch 2) (/ th 2)) 'grapheme)]
            [(cw . <= . dw)  ;; just give up if there's not enough room to draw the dots
             (void)]
            [else
             (send dc set-clipping-rect 0 0 (- cw dw 2) ch)
             (send dc draw-text msg 0 (- (/ ch 2) (/ th 2)) 'grapheme)
             (send dc set-clipping-region #f)
             (send dc draw-text dots (- cw dw) (- (/ ch 2) (/ th 2)))]))))
    (super-new)))

(define (goto-plt-license)
  (send-main-page #:sub "license/index.html"))

(define (maybe-try-to-materialize-docs parent)
  ;; under mac os x, the basic installation is put into a 'com.apple.quarantine'
  ;; which has the strange effect of making osascript drop the query parameters
  ;; for urls when they are sent to the browser. To work around this,
  ;; we materialize the documentation indicies in a user-specific place the
  ;; first time someone tries to read the docs with a specific query
  ;; the 'drracket:materialized-user-docs-versions pref is initialized to #f
  ;; on non-mac os x platforms so that we don't try at all there.
  (define materialize-pref (preferences:get 'drracket:materialized-user-docs-versions))
  (when materialize-pref
    (define prev-doc-dir (hash-ref materialize-pref (version) #f))
    (define might-take-a-long-time? (directory-exists? (find-user-doc-dir)))
    (define looks-like-we-need-to-do-something?
      (or (not prev-doc-dir)
          (not (directory-exists? (bytes->path prev-doc-dir)))))
    (when (and looks-like-we-need-to-do-something?
               (if might-take-a-long-time?
                   (user-okays-it? prev-doc-dir parent)
                   #t))
      (define error-output-or-false #t)
      (cond
        [might-take-a-long-time?
         (define d (new dialog% [parent parent] [label (string-constant drracket)]))
         (define m (new message%
                        [label (string-constant help-desk-materializing-user-docs...)]
                        [parent d]))
         (define c (new button%
                        [label (string-constant cancel)]
                        [parent d]
                        [callback
                         (λ (_1 _2)
                           (break-thread t)
                           (send d show #f))]))
         (define t
           (thread
            (λ ()
              (define _error-output-or-false (do-materialize))
              (queue-callback (λ ()
                                (send d show #f)
                                (set! error-output-or-false _error-output-or-false))))))
         (send d show #t)]
        [else (set! error-output-or-false (do-materialize))])
      (preferences:set 'drracket:materialized-user-docs-versions
                       (hash-set materialize-pref (version)
                                 (path->bytes (find-doc-dir))))
      (when error-output-or-false
        (message-box (string-constant drracket)
                     (string-append
                      "Attempting to materialize user docs failed:\n\n"
                      error-output-or-false))))))

(define (do-materialize)
  (define sp (open-output-string))
  (define succeeded? #t)
  (materialize-user-docs
   (λ (go)
     (set! succeeded? #f)
     (parameterize ([current-output-port sp]
                    [current-error-port sp])
       (set! succeeded? (go))))
   #:skip-user-doc-check? #t)
  (if succeeded?
      #f
      (get-output-string sp)))

(define (user-okays-it? prev-doc-dir parent)
  (define choice
    (message-box/custom
     (string-constant drracket)
     (if prev-doc-dir
         (format (string-constant help-desk-materialize-docs-something-changed)
                 prev-doc-dir)
         (string-constant help-desk-materialize-docs-first-time))
     (string-constant help-desk-materialize-user-docs)
     (string-constant help-desk-do-nothing)
     #f
     parent
     '(default=1)))
  (equal? choice 1))

(define (help-desk [key #f] [context #f] [parent #f]
                   #:language-family [language-family #f])
  (when key (maybe-try-to-materialize-docs parent))
  (cond
    [key
     (perform-search key context #:language-family language-family)]
    [language-family
     (send-language-family-page language-family)]
    [else
     (send-main-page)]))
