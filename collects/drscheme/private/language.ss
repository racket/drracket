
(module language mzscheme
  (require (lib "unitsig.ss")
           (lib "class.ss")
           (lib "class100.ss")
           "drsig.ss"
           "mred-wrap.ss"
           (lib "framework.ss" "framework")
           (prefix drscheme:unit: "unit.ss")
           (prefix zodiac: (lib "zodiac.ss" "syntax"))
           (prefix basis (lib "basis.ss" "userspce"))
           (lib "list.ss")
           (lib "file.ss")
           (lib "pconvert.ss"))
  
  (provide language@)

  (define settings-preferences-symbol 
    (string->symbol (format "drscheme:~a-settings" (version))))

  (define (get-printer-style-number printing-setting)
    (case printing-setting
      [(constructor-style) 0]
      [(quasi-style) 1]
      [(quasi-read-style) 1]
      [(r4rs-style) 2]
      [else (error 'drscheme:language:update-to "got: ~a as printing style"
		   printing-setting)]))

  (define re:mred (regexp "MrEd"))

  (define language-dialog
    (case-lambda
     [(original-settings) (language-dialog original-settings #f)]
     [(original-settings parent) 
      (letrec
	  ([settings (basis:copy-setting original-settings)]
	   [language-levels (map basis:setting-name basis:settings)]

	   [dialog%
	    (class100 mred:dialog% args
	      (override
	       [on-close
		(lambda ()
		  (set! settings #f))])
	      (sequence
		(apply super-init args)))]
	   
	   [f (make-object dialog% "Language" parent)]
	   [main (make-object mred:vertical-pane% f)]
	   [language-panel (make-object mred:horizontal-panel% main '(border))]
           [language-choice-panel (make-object mred:vertical-pane% language-panel)]
	   [customization-panel (make-object mred:horizontal-panel% main)]
	   [customization-left-panel (make-object mred:vertical-pane% customization-panel)]
	   [customization-right-panel (make-object mred:vertical-pane% customization-panel)]
	   [when-message (make-object mred:message%
			   "Language changes effective after click on Execute"
			   main)]
	   [make-sub-panel
	    (lambda (name panel)
	      (let* ([p (make-object mred:vertical-pane% panel)]
		     [message (make-object mred:message% name p)])
		(make-object mred:vertical-panel% p '(border))))]
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
	   [full-scheme-panel (let ([p (make-object mred:panel% language-panel)])
				(send p set-label-position 'vertical)
                                (send p stretchable-width #f)
				(send p stretchable-height #t)
				p)]
	   [full-scheme-radio-box-label-map
	    (let ([re (regexp "Full Scheme ")])
	      (let loop ([settings (cdr (cdr (cdr basis:settings)))] ;; remove teaching languages
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
		      (make-object mred:radio-box% 
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
	   [compatible-space1 (make-object mred:vertical-panel% language-choice-panel)]
	   [language-choice
	    (make-object mred:choice%
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
	   [compatible-space2 (make-object mred:vertical-panel% language-choice-panel)]
           [compatible-with-student-languages
	    (make-object mred:check-box%
	      "Compatible with student languages" language-choice-panel
	      (lambda xxx
		(let ([v (send compatible-with-student-languages get-value)])
		  (basis:set-setting-teaching-primitives-and-syntax?! settings v)
		  (basis:set-setting-case-sensitive?! settings v)
		  (basis:set-setting-unmatched-cond/case-is-error?! settings v)
		  (basis:set-setting-signal-undefined! settings v)
		  (update-to settings))))]
	   [compatible-space3 (make-object mred:vertical-panel% language-choice-panel)]

	   [custom-message (make-object mred:message% "Custom" language-panel)]
	   [right-align
	    (opt-lambda (mo panel)
	      (let* ([hp (make-object mred:horizontal-pane% panel)])
		(begin0
		 (mo hp)
		 (make-object mred:horizontal-pane% hp))))]
	   [make-check-box
	    (lambda (set-setting! setting name panel)
	      (right-align
	       (lambda (hp)
		 (let ([cb (make-object mred:check-box%
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
					    "Case sensitive"
					    input-syntax-panel)]
	   [unmatched-cond/case-is-error?
	    (make-check-box basis:set-setting-unmatched-cond/case-is-error?!
			    basis:setting-unmatched-cond/case-is-error?
			    "Unmatched cond/case is an error"
			    dynamic-panel)]
	   [signal-undefined
	    (make-check-box basis:set-setting-signal-undefined!
			    basis:setting-signal-undefined
			    "Signal undefined variables when first referenced"
			    dynamic-panel)]
	   [teaching-primitives-and-syntax?
	    (make-check-box basis:set-setting-teaching-primitives-and-syntax?!
			    basis:setting-teaching-primitives-and-syntax?
			    "Teaching language primitives and syntax"
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
	       (make-object mred:radio-box%
		 "Output Style"
		 (list "Constructor"
		       "Quasiquote"
		       "write")
		 main
		 (lambda (box evt)
		   (let* ([which (send box get-selection)]
			  [symbol-which (printer-number->symbol which)])
		     (basis:set-setting-printing! settings symbol-which)
		     (update-to settings)))))
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
	   [ok-panel (make-object mred:horizontal-pane% main)]
	   [hide-button (make-object mred:button%
			  "Hide Details"
			  ok-panel
			  (lambda (button evt) (show-specifics #f)))]
	   [show-button (make-object mred:button%
			  "Show Details"
			  ok-panel
			  (lambda (button evt) (show-specifics #t)))]
	   [_3 (make-object mred:horizontal-pane% ok-panel)]
	   [cancel-button (make-object mred:button%
			    "Cancel"
			    ok-panel
			    (lambda (button evt) 
			      (set! settings #f)
			      (send f show #f)))]
	   [ok-button (make-object mred:button%
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
	(show-specifics (not (ormap compare-setting-to-gui basis:settings)))
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

  (define (fill-language-menu frame language-menu)
    (make-object menu:can-restore-menu-item%
      "Choose Language..."
      language-menu
      (lambda (_1 _2)
	(let ([new-settings (language-dialog
			     (preferences:get settings-preferences-symbol)
			     frame)])
	  (when new-settings
	    (preferences:set
	     settings-preferences-symbol
	     new-settings))))
      #\l)
    (make-object mred:separator-menu-item% language-menu)
    (make-object menu:can-restore-menu-item%
      "Add Teachpack..."
      language-menu
      (lambda (_1 _2)
	(let ([lib-file
	       (parameterize ([finder:dialog-parent-parameter frame])
		 (finder:get-file 
		  teachpack-directory
		  "Select a Teachpack"
		  ".*\\.(ss|scm)$"))])
	  (when lib-file
            (parameterize ([basis:teachpack-error-display
                            (lambda (message)
                              (mred:message-box "Invalid Teachpack" message))])
              (let* ([old-pref (preferences:get 'drscheme:teachpack-file)]
                     [new-item (normalize-path lib-file)])
                (if (member (normal-case-path new-item) (map normal-case-path old-pref))
                    (mred:message-box "DrScheme Teachpacks"
                                      (format "Already added ~a Teachpack"
                                              new-item)
                                      frame)
                    (when (basis:teachpack-ok? lib-file)
                      (preferences:set
                       'drscheme:teachpack-file
                       (append old-pref (list lib-file))))))
              (set! teachpack-directory (path-only lib-file)))))))
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
      (lambda (_1 _2) (preferences:set 'drscheme:teachpack-file null)))))

