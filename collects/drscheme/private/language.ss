
(module language mzscheme
  (require (lib "unitsig.ss")
           (lib "hierlist.ss" "hierlist")
           (lib "class.ss")
           (lib "class100.ss")
           "drsig.ss"
           "string-constant.ss"
	   (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "file.ss")
           (lib "pconvert.ss"))
  
  (provide language@)
  
  (define language@
    (unit/sig drscheme:language^
      (import [drscheme:unit : drscheme:unit^]
              [drscheme:language-tower : drscheme:language-tower^]
              [drscheme:rep : drscheme:rep^])
      
      ;; settings-preferences-symbol : symbol
      ;; the preferences key for the language settings.
      ;; depends on the version number, so people can use multiple versions
      ;; of drscheme and maintain separate language settings for each
      ;; of them.
      (define settings-preferences-symbol 
        (string->symbol (format "drscheme:~a-settings" (version:version))))
      
      ;; default-language-position : (listof string)
      ;; if a language is registered with this position, it is
      ;; considered the default language
      (define default-language-position '("HtDP" "Beginning Student"))

      ;; languages : (listof (instanceof language<%>))
      ;; all of the languages supported in DrScheme
      (define languages null)
      
      ;; can-still-add-languages? : boolean
      ;; invariant: addition of languages is only
      ;;            allowed when this is #t
      (define can-still-add-languages? #t)

      ;; add-language : (instanceof language%) -> void
      ;; checks can-still-add-languages? before adding the language
      ;; effect: updates `languages'
      (define (add-language language)
	(unless can-still-add-languages?
	  (error 'add-language "too late to add a new language: ~e" language))
	(set! languages (cons language languages)))
      
      ;; get-languages : -> (listof languages)
      ;; effect: sets can-still-add-languages? to #f
      (define (get-languages)
	(set! can-still-add-languages? #f)
        languages)
      
      ;; get-default-language-settings : -> language-settings
      ;; uses `default-language-position' to find the default language.
      ;; if that language is not available, just takes the first language.
      ;; if there are no languages defined yet, signal an error -- drscheme is screwed.
      (define (get-default-language-settings)
	(when (null? languages)
	  (error 'get-default-language-settings "no languages registered!"))
	(let ([lang (or (ormap (lambda (x)
				 (and (equal? (send x get-language-position)
					      default-language-position)
				      x))
			       (get-languages))
			(first (get-languages)))])
	  (make-language-settings lang (send lang default-settings))))

      ;; type language-settings = (make-language-settings (instanceof language<%>) settings)
      (define-struct language-settings (language settings))
       
      ;; language-dialog : (language-setting -> language-setting)
      ;;                   (language-setting (union #f (instanceof top-level-window%))
      ;;                    -> language-setting)
      ;; allows the user to configure their language. The input language-setting is used
      ;; as the defaults in the dialog and the output language setting is the user's choice
      (define language-dialog
        (opt-lambda (language-settings-to-show [parent #f])
	  (define language-to-show (language-settings-language language-settings-to-show))
	  (define settings-to-show (language-settings-settings language-settings-to-show))
          
          ;; hier-list items that implement this interface correspond to
	  ;; actual language selections
	  (define hieritem-language<%>
	    (interface (hierarchical-list-item<%>)
	      selected))

	  ;; language-container-mixin : (implements hierlist<%>) -> (implements hierlist<%>)
	  ;; a mixin that delegates item clicks to the item clicked on, if it
	  ;; is a language.
	  (define selectable-hierlist%
            (class hierarchical-list%
              (init parent)
              (override on-select)
              (rename [super-on-select on-select])
              (define (on-select i)
                (cond
		  [(and i (is-a? i hieritem-language<%>))
		   (send i selected)]
		  [else
		   (nothing-selected)]))
              (super-instantiate (parent))))

          (define dialog (make-object dialog% (string-constant language-dialog-title)
                           parent #f #f #f #f '(resize-border)))
          (define outermost-panel (make-object horizontal-panel% dialog))
          (define languages-hier-list (make-object selectable-hierlist% outermost-panel))
	  (define details-panel (make-object panel:single% outermost-panel))
	  (define button-panel (make-object horizontal-panel% dialog))

	  (define no-details-panel (make-object vertical-panel% details-panel))

          (define languages-table (make-hash-table))
          (define languages (get-languages))

	  ;; selected-language : (union (instanceof language<%>) #f)
	  ;; invariant: selected-language and get-selected-language-settings
	  ;;            match the user's selection in the language-hier-list.
	  ;;            or #f if the user is not selecting a language.
	  (define selected-language #f)
	  ;; get-selected-language-settings (union #f (-> settings))
	  (define get-selected-language-settings #f)

	  ;; language-mixin : (implements language<%>) -> ((implements hierlist<%>) -> (implements hierlist<%>))
	  ;; a mixin that responds to language selections and updates the details-panel
	  (define (language-mixin language language-details-panel get-settings)
            (lambda (%)
              (class* % (hieritem-language<%>)
                (init-rest args)
                (public selected)
                (define (selected)
                  (send details-panel active-child language-details-panel)
		  (send ok-button enable #t)
		  (set! get-selected-language-settings get-settings)
		  (set! selected-language language))
                (apply super-make-object args))))

	  ;; nothing-selected : -> void
	  ;; updates the GUI and selected-language and get-selected-language-settings
	  ;; for when no language is selected.
	  (define (nothing-selected)
	    (send ok-button enable #f)
	    (send details-panel active-child no-details-panel)
	    (set! get-selected-language-settings #f)
	    (set! selected-language #f))

	  ;; add-language : (instanceof language<%>) -> void
	  ;; adds the language to the dialog
	  ;; opens all of the turn-down tags
          (define (add-language language)
            (let add-sub-language ([ht languages-table]
                                   [hier-list languages-hier-list]
                                   [lng (send language get-language-position)])
              (cond
                [(null? (cdr lng))
		 (let-values ([(language-details-panel get-settings)
			       (make-details-panel language)])
		   (let ([item
			  (send hier-list new-item
				(language-mixin language language-details-panel get-settings))])
		   (send (send item get-editor) insert (car lng))))]
                [else (let ([sub-lng (car lng)]
                            [sub-ht/sub-hier-list
                             (hash-table-get
                              ht
                              (string->symbol (car lng))
                              (lambda ()
                                (let* ([new-list (send hier-list new-list)]
				       [x (cons (make-hash-table) new-list)])
				  (send new-list open)
				  (send (send new-list get-editor) insert (car lng))
                                  (hash-table-put! ht (string->symbol (car lng)) x)
                                  x)))])
                        (add-sub-language (car sub-ht/sub-hier-list)
                                          (cdr sub-ht/sub-hier-list)
                                          (cdr lng)))])))
          
	  ;; make-details-panel : ((instanceof language<%>) -> (values panel (-> settings)))
	  ;; adds a details panel for `language', using
	  ;; the language's default settings, unless this is
	  ;; the to-show language.
	  (define (make-details-panel language)
	    (let ([panel (make-object vertical-panel% details-panel)])
	      (values
	       panel
	       (send language config-panel
		     panel
		     (if (eq? language language-to-show)
			 settings-to-show
			 (send language default-settings))))))

	  ;; close-all-languages : -> void
	  ;; closes all of the tabs in the language hier-list.
	  (define (close-all-languages)
	    (define (close-children list)
	      (for-each close-this-one (send list get-items)))
	    (define (close-this-one item)
	      (cond
		[(is-a? item hierarchical-list-compound-item<%>)
		 (send item close)
		 (close-children item)]
		[else (void)]))
	    (close-children languages-hier-list))

	  ;; open-current-language : -> void
	  ;; opens the tabs that lead to the current language
	  ;; and selects the current language
	  (define (open-current-language)
	    (let loop ([hi languages-hier-list]
		       [first-pos (car (send language-to-show get-language-position))]
		       [position (cdr (send language-to-show get-language-position))])
		 (let ([child
			;; know that this `car' is okay by construction of the dialog
			(car 
			 (filter (lambda (x)
				   (equal? (send (send x get-editor) get-text)
					   first-pos))
				 (send hi get-items)))])
		   (cond
		     [(null? position)
		      (send child select #t)]
		     [else
		      (send child open)
		      (loop child (car position) (cdr position))]))))

	  (define (ok-callback)
	    (preferences:set settings-preferences-symbol
			     (make-language-settings
			      selected-language
			      (get-selected-language-settings)))
	    (send dialog show #f))
	  (define (cancel-callback)
	    (send dialog show #f))

	  (define cancel-button (make-object button% "Cancel" button-panel (lambda (x y) (cancel-callback))))
	  (define ok-button (make-object button% "OK" button-panel (lambda (x y) (ok-callback))))
          (define grow-box-spacer (make-object grow-box-spacer-pane% button-panel))
          
	  (send button-panel set-alignment 'right 'center)
	  (send button-panel stretchable-height #f)

          (for-each add-language languages)
	  (send dialog reflow-container)
	  (send languages-hier-list stretchable-width #f)
	  (send languages-hier-list min-client-width
		(text-width (send languages-hier-list get-editor)))
	  (close-all-languages)
	  (open-current-language)
          (send dialog show #t)))
 
      ;; text-width : (isntanceof text%) -> exact-integer
      ;; calculates the width of widest line in the
      ;; editor. This only makes sense if auto-wrap
      ;; is turned off. Otherwise, you could just use
      ;; the admin's width.
      (define (text-width text)
	(let loop ([n (+ (send text last-line) 1)]
		   [current-max-width 0])
	  (cond
	    [(zero? n)
	     (+
	      10 ;; this should be some magic small constant (hopefully less than 10 on all platforms)
	      (floor (inexact->exact current-max-width)))]
	    [else (let* ([line-number (- n 1)]
			 [box (box 0.0)]
			 [eol-pos (send text line-end-position line-number)]
			 [eol-snip (send text find-snip eol-pos 'before)])
		    (when eol-snip
		      (send text get-snip-location eol-snip box #f #t))
		    (loop (- n 1)
			  (max current-max-width (unbox box))))])))

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
