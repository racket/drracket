
(module language mzscheme
  (require (lib "unitsig.ss")
           (lib "hierlist.ss" "hierlist")
           (lib "class.ss")
           (lib "class100.ss")
           "drsig.ss"
	   (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "file.ss")
           (lib "pconvert.ss")
           (prefix basis: (lib "basis.ss" "userspce")))
  
  (provide language@)
  
  (define language@
    (unit/sig drscheme:language^
      (import [drscheme:unit : drscheme:unit^]
              [drscheme:language-tower : drscheme:language-tower^])
      
      (define settings-preferences-symbol 
        (string->symbol (format "drscheme:~a-settings" (version:version))))
      
      (define re:mred (regexp "MrEd"))
      
      ;; (union #f (listof (instanceof language<%>)))
      ;; #f indicates uninitialized
      (define available-languages #f)
      
      (define (calculate-available-languages)
        (let ([make-simple
               (lambda (lang ps)
                 (make-object drscheme:language-tower:module-based-language->language%
		   (make-object drscheme:language-tower:simple-module-based-language->module-based-language%
		     (make-object drscheme:language-tower:simple-module-based-language%
		       `(lib ,lang "langs")
		       ps))))])
          (list (make-simple "full-mred.ss" '("Full" "Graphical (MrEd)"))
                (make-simple "full-mzscheme.ss" '("Full" "Textual (MzScheme)")))))
      
      (define (get-available-languages)
        (unless available-languages
          (set! available-languages (calculate-available-languages)))
        available-languages)
      
      ;; type language-setting = (make-language-setting (instanceof language<%>) setting)
      (define-struct language-setting (language setting))
       
      ;; language-dialog : (language-setting -> language-setting)
      ;;                   (language-setting (union #f (instanceof top-level-window%)) -> language-setting
      ;; allows the user to configure their language. The input language-setting is used
      ;; as the defaults in the dialog and the output language setting is the user's choice
      (define language-dialog
        (opt-lambda (settings-to-show [parent #f])
          
          ;; hier-list items that implement this interface correspond to
	  ;; actual language selections
	  (define hieritem-language<%>
	    (interface (hierarchical-list-item<%>)
	      show-details))

	  ;; language-container-mixin : (implements hierlist<%>) -> (implements hierlist<%>)
	  ;; a mixin that delegates item clicks to the item clicked on, if it
	  ;; is a language.
	  (define selectable-hierlist%
            (class hierarchical-list%
              (init parent)
              (override on-select)
              (rename [super-on-select on-select])
              (define (on-select i)
                (when (is-a? i hieritem-language<%>)
                  (send i show-details)))
              (super-instantiate (parent))))

          (define dialog (make-object dialog% (string-constant language-dialog-title) parent #f #f #f #f '(resize-border)))
          (define outermost-panel (make-object horizontal-panel% dialog))
          (define languages-hier-list (make-object selectable-hierlist% outermost-panel))
	  (define details-panel (make-object panel:single% outermost-panel))
          (define languages-table (make-hash-table))
          (define languages (get-available-languages))

	  ;; details-panels-associations : (listof (list language panel (-> settings)))
	  (define details-panels-associations
	    (map (lambda (language)
		   (let ([panel (make-object vertical-panel% details-panel)])
		     (list
		      language
		      panel
		      (send language config-panel panel))))
		 languages))

	  ;; language-mixin : (implements language<%>) -> ((implements hierlist<%>) -> (implements hierlist<%>))
	  ;; a mixin that responds to language selections and updates the details-panel
	  (define (language-mixin language)
            (lambda (%)
              (class* % (hieritem-language<%>)
                (init-rest args)
                (public show-details)
                (define (show-details)
                  (send details-panel active-child
                        (second (assoc language details-panels-associations))))
                (apply super-make-object args))))

	  ;; add-language : (instanceof language<%>) -> void
	  ;; adds the language to the dialog
          (define (add-language language)
            (let add-sub-language ([ht languages-table]
                                   [hier-list languages-hier-list]
                                   [lng (send language get-language-position)])
              (cond
                [(null? (cdr lng))
		 (let ([item (send hier-list new-item (language-mixin language))])
		   (send (send item get-editor) insert (car lng)))]
                [else (let ([sub-lng (car lng)]
                            [sub-ht/sub-hier-list
                             (hash-table-get
                              ht
                              (string->symbol (car lng))
                              (lambda ()
                                (let* ([new-list (send hier-list new-list)]
				       [x (cons (make-hash-table) new-list)])
				  (send (send new-list get-editor) insert (car lng))
                                  (hash-table-put! ht (string->symbol (car lng)) x)
                                  x)))])
                        (add-sub-language (car sub-ht/sub-hier-list)
                                          (cdr sub-ht/sub-hier-list)
                                          (cdr lng)))])))
          
          (for-each add-language languages)
          (send dialog show #t)))
 
      
      '(define language-dialog
        (case-lambda
         [(original-settings) (language-dialog original-settings #f)]
         [(original-settings parent) 
          (error 'language-dialog "not yet implemented")
          '(letrec
              ([settings (basis:copy-setting original-settings)]
               [language-levels (map basis:setting-name (basis:get-settings))]
               
               [dialog%
                (class100 dialog% args
                  (override
                    [on-close
                     (lambda ()
                       (set! settings #f))])
                  (sequence
                    (apply super-init args)))]
               
               [f (make-object dialog% "Language" parent)]
               [main (make-object vertical-pane% f)]
               [language-panel (make-object horizontal-panel% main '(border))]
               [language-choice-panel (make-object vertical-pane% language-panel)]
               [customization-panel (make-object horizontal-panel% main)]
               [customization-left-panel (make-object vertical-pane% customization-panel)]
               [customization-right-panel (make-object vertical-pane% customization-panel)]
               [when-message (make-object message%
                               "Language changes effective after click on Execute"
                               main)]
               [make-sub-panel
                (lambda (name panel)
                  (let* ([p (make-object vertical-pane% panel)]
                         [message (make-object message% name p)])
                    (make-object vertical-panel% p '(border))))]
               [input-syntax-panel (make-sub-panel "Input Syntax" customization-left-panel)]
               [dynamic-panel (make-sub-panel "Dynamic Properties" customization-left-panel)]
               [output-syntax-panel (make-sub-panel "Output Syntax" customization-right-panel)]
               
               [specifics-shown? #f]
               [show-specifics
                (lambda (bool)
                  (set! specifics-shown? bool)
                  (send ok-panel change-children
                        (lambda (l)
                          (cons (if bool hide-button show-button)
                                (remq hide-button
                                      (remq show-button l)))))
                  (send main change-children
                        (lambda (l)
                          (if bool
                              (list language-panel customization-panel when-message ok-panel)
                              (list language-panel when-message ok-panel)))))]
               [full-scheme-panel (let ([p (make-object panel% language-panel)])
                                    (send p set-label-position 'vertical)
                                    (send p stretchable-width #f)
                                    (send p stretchable-height #t)
                                    p)]
               [full-scheme-radio-box-label-map
                (let ([re (regexp "Full Scheme ")])
                  (let loop ([settings (cdr (cdr (cdr (basis:get-settings))))] ;; remove teaching languages
                             [n 0])
                    (cond
                      [(null? settings) null]
                      [else
                       (let ([name (basis:setting-name (car settings))])
                         (cons (list (regexp-replace re name "") name n)
                               (loop (cdr settings)
                                     (+ n 1))))])))]
               [full-scheme-radio-box #f]
               [full-scheme-radio-box-callback
                (lambda ()
                  (set! settings
                        (basis:copy-setting
                         (basis:find-setting-named
                          (cadr
                           (list-ref
                            full-scheme-radio-box-label-map
                            (send full-scheme-radio-box get-selection))))))
                  (update-to settings))]
               [close-full-scheme-radio-box
                (lambda ()
                  (when full-scheme-radio-box
                    (send full-scheme-panel change-children (lambda (x) null))))]
               [open-full-scheme-radio-box
                (lambda (setting)
                  (cond
                    [full-scheme-radio-box
                     (send full-scheme-panel change-children
                           (lambda (x) (list full-scheme-radio-box)))]
                    [else
                     (set! full-scheme-radio-box
                           (make-object radio-box% 
                             "Full Scheme Variant"
                             (map car full-scheme-radio-box-label-map)
                             full-scheme-panel
                             (lambda x (full-scheme-radio-box-callback))))
                     (let ([ele (assoc (basis:setting-name setting)
                                       (map cdr full-scheme-radio-box-label-map))])
                       (when ele
                         (send full-scheme-radio-box set-selection (cadr ele))))]))]
               [full-scheme "Full Scheme"]
               [language-choice-choices (list (first language-levels)
                                              (second language-levels)
                                              (third language-levels)
                                              full-scheme)]
               [compatible-space1 (make-object vertical-panel% language-choice-panel)]
               [language-choice
                (make-object choice%
                  "Language"
                  language-choice-choices
                  language-choice-panel
                  (lambda (choice evt)
                    (cond
                      [(string=? full-scheme (send choice get-string-selection))
                       (open-full-scheme-radio-box settings)
                       (full-scheme-radio-box-callback)]
                      [else
                       (close-full-scheme-radio-box)
                       (set! settings
                             (basis:copy-setting
                              (basis:number->setting
                               (send choice get-selection))))
                       (update-to settings)])))]
               [compatible-space2 (make-object vertical-panel% language-choice-panel)]
               [compatible-with-student-languages
                (make-object check-box%
                  "Compatible with student languages" language-choice-panel
                  (lambda xxx
                    (let ([v (send compatible-with-student-languages get-value)])
                      (basis:set-setting-teaching-primitives-and-syntax?! settings v)
                      (basis:set-setting-case-sensitive?! settings v)
                      (basis:set-setting-unmatched-cond/case-is-error?! settings v)
                      (basis:set-setting-signal-undefined! settings v)
                      (update-to settings))))]
               [compatible-space3 (make-object vertical-panel% language-choice-panel)]
               
               [custom-message (make-object message% "Custom" language-panel)]
               [right-align
                (opt-lambda (mo panel)
                  (let* ([hp (make-object horizontal-pane% panel)])
                    (begin0
                      (mo hp)
                      (make-object horizontal-pane% hp))))]
               [make-check-box
                (lambda (set-setting! setting name panel)
                  (right-align
                   (lambda (hp)
                     (let ([cb (make-object check-box%
                                 name
                                 hp
                                 (lambda (check-box evt)
                                   (let ([i (send check-box get-value)])
                                     (set-setting! settings i)
                                     (update-to settings))))])
                       (send cb set-value (setting settings))
                       cb))
                   panel))]
               
               [case-sensitive? (make-check-box basis:set-setting-case-sensitive?!
                                                basis:setting-case-sensitive?
                                                (string-constant case-sensitive?-label)
                                                input-syntax-panel)]
               [teaching-primitives-and-syntax?
                (make-check-box basis:set-setting-teaching-primitives-and-syntax?!
                                basis:setting-teaching-primitives-and-syntax?
                                (string-constant teaching-primitives-and-syntax?-label)
                                dynamic-panel)]
               [printer-number->symbol
                (lambda (which)
                  (case which
                    [(0) 'constructor-style]
                    [(1) 'quasi-style]
                    [(2) 'r4rs-style]
                    [else 'constructor-style]))]
               [printing
                (right-align
                 (lambda (main)
                   (make-object radio-box%
                     (string-constant output-style-label)
                     (list (string-constant contructor-printing-style)
                           (string-constant quasiquote-printing-style)
                           (string-constant write-printing-style))
                     main
                     void))
                 output-syntax-panel)]
               [sharing-printing?
                (make-check-box basis:set-setting-sharing-printing?!
                                basis:setting-sharing-printing?
                                "Show sharing in values"
                                output-syntax-panel)]
               [whole/fractional-exact-numbers
                (make-check-box basis:set-setting-whole/fractional-exact-numbers!
                                basis:setting-whole/fractional-exact-numbers
                                "Print rationals in whole/part notation"
                                output-syntax-panel)]
               [booleans-as-true/false
                (make-check-box basis:set-setting-print-booleans-as-true/false!
                                basis:setting-print-booleans-as-true/false
                                "Print booleans as true and false"
                                output-syntax-panel)]
               [use-pretty-printer?
                (make-check-box basis:set-setting-use-pretty-printer?!
                                basis:setting-use-pretty-printer?
                                "Insert newlines in printed values"
                                output-syntax-panel)]
               [ok-panel (make-object horizontal-pane% main)]
               [hide-button (make-object button%
                              "Hide Details"
                              ok-panel
                              (lambda (button evt) (show-specifics #f)))]
               [show-button (make-object button%
                              "Show Details"
                              ok-panel
                              (lambda (button evt) (show-specifics #t)))]
               [_3 (make-object horizontal-pane% ok-panel)]
               [cancel-button (make-object button%
                                "Cancel"
                                ok-panel
                                (lambda (button evt) 
                                  (set! settings #f)
                                  (send f show #f)))]
               [ok-button (make-object button%
                            "OK"
                            ok-panel
                            (lambda (button evt) 
                              (send f show #f))
                            '(border))]
               [compare-setting-to-gui
                (lambda (setting)
                  (let* ([compare-check-box
                          (lambda (check-box selector)
                            (let* ([cbv (send check-box get-value)]
                                   [ss (selector setting)])
                              (equal? (not cbv)
                                      (not ss))))])
                    (and (compare-check-box case-sensitive? basis:setting-case-sensitive?)
                         (compare-check-box unmatched-cond/case-is-error? basis:setting-unmatched-cond/case-is-error?)
                         (compare-check-box signal-undefined basis:setting-signal-undefined)
                         (compare-check-box teaching-primitives-and-syntax? basis:setting-teaching-primitives-and-syntax?)
                         (compare-check-box sharing-printing? basis:setting-sharing-printing?)
                         (compare-check-box whole/fractional-exact-numbers basis:setting-whole/fractional-exact-numbers)
                         (compare-check-box booleans-as-true/false basis:setting-print-booleans-as-true/false)
                         (compare-check-box use-pretty-printer? basis:setting-use-pretty-printer?)
                         (eq? (printer-number->symbol (send printing get-selection))
                              (basis:setting-printing setting)))))]
               [reset-choice
                (lambda ()
                  (send language-panel
                        change-children
                        (lambda (l)
                          (let ([not-custom?
                                 (compare-setting-to-gui
                                  (basis:number->setting
                                   (send language-choice get-selection)))])
                            (if not-custom?
                                (list language-choice-panel full-scheme-panel)
                                (list language-choice-panel full-scheme-panel custom-message))))))]
               [update-to
                (lambda (v)
                  (let ([zodiac? (basis:zodiac-vocabulary? v)])
                    
                    (cond
                      [(member (basis:setting-name v) language-choice-choices)
                       (send teaching-primitives-and-syntax? enable #f)
                       (send language-choice-panel change-children
                             (lambda (l) (list language-choice)))
                       (send language-choice set-string-selection (basis:setting-name v))
                       (close-full-scheme-radio-box)]
                      [else
                       (send language-choice set-string-selection full-scheme)
                       
                       (let ([teaching-ok?
                              (regexp-match re:mred (basis:setting-name v))])
                         (send teaching-primitives-and-syntax? enable teaching-ok?)
                         (send compatible-with-student-languages
                               set-value
                               (and zodiac?
                                    teaching-ok?
                                    (basis:setting-case-sensitive? v)
                                    (basis:setting-unmatched-cond/case-is-error? v)
                                    (basis:setting-signal-undefined v)
                                    (basis:setting-teaching-primitives-and-syntax? v)))
                         (send compatible-with-student-languages enable
                               (and teaching-ok? zodiac?)))
                       
                       (send language-choice-panel change-children
                             (lambda (l)
                               (list compatible-space1
                                     language-choice
                                     compatible-space2
                                     compatible-with-student-languages
                                     compatible-space3)))
                       (open-full-scheme-radio-box v)
                       (let ([map (assoc (basis:setting-name v)
                                         full-scheme-radio-box-label-map)])
                         (when map
                           (send full-scheme-radio-box set-selection (caddr map))))])
                    
                    (send printing set-selection
                          (get-printer-style-number (basis:setting-printing v)))
                    (let ([r4rs-style? (eq? 'r4rs-style (basis:setting-printing v))])
                      (send booleans-as-true/false enable (not r4rs-style?))
                      (when r4rs-style?
                        (basis:set-setting-print-booleans-as-true/false! v #f)))
                    
                    (for-each
                     (lambda (get check-box) (send check-box set-value (get v)))
                     (list basis:setting-case-sensitive?
                           basis:setting-sharing-printing?
                           basis:setting-use-pretty-printer?
                           basis:setting-whole/fractional-exact-numbers
                           basis:setting-print-booleans-as-true/false
                           basis:setting-unmatched-cond/case-is-error?
                           basis:setting-signal-undefined
                           basis:setting-teaching-primitives-and-syntax?)
                     (list case-sensitive? 
                           sharing-printing?
                           use-pretty-printer?
                           whole/fractional-exact-numbers
                           booleans-as-true/false
                           unmatched-cond/case-is-error?
                           signal-undefined
                           teaching-primitives-and-syntax?))
                    
                    (send printing enable 1
                          (not (eq? (basis:setting-vocabulary-symbol v) 'beginner)))
                    (send signal-undefined enable zodiac?)
                    
                    (reset-choice)))])
            (send language-choice-panel stretchable-width #f)
            (send f stretchable-width #f)
            (send f stretchable-height #f)
            (send language-choice stretchable-width #f)
            (send printing stretchable-width #t)
            (update-to settings)
            (show-specifics (not (ormap compare-setting-to-gui (basis:get-settings))))
            (for-each (lambda (x) (send x stretchable-height #f))
                      (list language-panel ok-panel main))
            (send language-panel set-alignment 'center 'center)
            (send ok-button min-width (send cancel-button get-width))
            (send f center 'both)
            (send f show #t)
            settings)]))
      
      (define teachpack-directory 
        (let ([lib-dir (build-path 
                        (collection-path "mzlib")
                        'up 'up "teachpack")])
          (if (directory-exists? lib-dir)
              lib-dir
              #f)))
      
      ;; add-new-teachpack : (instanceof frame%) -> void
      ;; querys the user for the name of a teachpack and adds it to the
      ;; current teachpacks. Uses the argument as the parent to the dialog
      (define (add-new-teachpack frame)
        (error 'add-new-teachpack "not yet implemented")
        '(let ([lib-file
                (parameterize ([finder:dialog-parent-parameter frame])
                  (finder:get-file 
                   teachpack-directory
                  "Select a Teachpack"
                  ".*\\.(ss|scm)$"))])
          (when lib-file
            (parameterize ([basis:teachpack-error-display
                            (lambda (message)
                              (message-box "Invalid Teachpack" message))])
              (let* ([old-pref (preferences:get 'drscheme:teachpack-file)]
                     [new-item (normalize-path lib-file)])
                (if (member (normal-case-path new-item) (map normal-case-path old-pref))
                    (message-box "DrScheme Teachpacks"
                                 (format "Already added ~a Teachpack"
                                         new-item)
                                 frame)
                    (when (basis:teachpack-ok? lib-file)
                      (preferences:set
                       'drscheme:teachpack-file
                       (append old-pref (list lib-file))))))
              (set! teachpack-directory (path-only lib-file))))))
      
      ;; clear-all-teachpacks : -> void
      ;; clears all of the teachpack settings
      (define (clear-all-teachpacks)
        (preferences:set 'drscheme:teachpack-file null))
      
      ;; choose-language : (instance frame%) -> void
      ;; queries the user for a new language setting
      ;; and sets it. Uses the argument for the parent
      ;; to the dialog
      (define (choose-language frame)
        (let ([new-settings (language-dialog
                             (preferences:get settings-preferences-symbol)
                             frame)])
          (when new-settings
            '(preferences:set
	      settings-preferences-symbol
	      new-settings))))
      
      (define (fill-language-menu frame language-menu)
        (make-object menu:can-restore-menu-item%
          "Choose Language..."
          language-menu
          (lambda (_1 _2)
            (choose-language frame))
          #\l)
        (make-object separator-menu-item% language-menu)
        (make-object menu:can-restore-menu-item%
          "Add Teachpack..."
          language-menu
          (lambda (_1 _2)
            (add-new-teachpack frame)))
        (make-object (class100 menu:can-restore-menu-item% args
                       (inherit enable)
                       (rename [super-on-demand on-demand])
                       (override
                         [on-demand
                          (lambda ()
                            (enable (not (null? (preferences:get 'drscheme:teachpack-file))))
                            (super-on-demand))])
                       (sequence (apply super-init args)))
          "Clear All Teachpacks"
          language-menu
          (lambda (_1 _2) (clear-all-teachpacks)))))))
