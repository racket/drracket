;; WARNING: printf is rebound in this module to always use the 
;;          original stdin/stdout of drscheme, instead of the 
;;          user's io ports, to aid any debugging printouts.
;;          (esp. useful when debugging the users's io)

(module language mzscheme
  (require "drsig.ss"
           (lib "string-constant.ss" "string-constants")
           (lib "pconvert.ss")
           (lib "pretty.ss")
           (lib "etc.ss")
	   (lib "unitsig.ss")
	   (lib "class.ss")
           (lib "file.ss")
           (lib "list.ss")
           (lib "embed.ss" "compiler")
           (lib "launcher.ss" "launcher")
	   (lib "mred.ss" "mred")
	   (lib "framework.ss" "framework"))

  (provide language@)

  (define language@
    (unit/sig drscheme:language^
      (import [drscheme:rep : drscheme:rep^]
              [drscheme:snip : drscheme:snip^]
              [drscheme:debug : drscheme:debug^]
              [drscheme:teachpack : drscheme:teachpack^]
              [drscheme:tools : drscheme:tools^])
      
      (define original-output-port (current-output-port))
      (define (printf . args) (apply fprintf original-output-port args)) 
      
      (define-struct text/pos (text start end))
      ;; text/pos = (make-text/pos (instanceof text% number number))
      ;; this represents a portion of a text to be processed.
      
      (define language<%>
	(interface ()
	  marshall-settings
          unmarshall-settings
          default-settings
	  default-settings?
          
          front-end
	  config-panel
	  on-execute
          render-value/format
          render-value
          
          create-executable
          
          get-language-position
          get-language-name
          get-style-delta
          get-language-numbers
          get-one-line-summary))
      
      (define module-based-language<%>
	(interface ()
	  marshall-settings
          unmarshall-settings
          default-settings
	  default-settings?

          get-module
          get-transformer-module
          use-namespace-require/copy?
	  config-panel

          get-reader
	  
          on-execute
          get-init-code
          use-mred-launcher?
          
          render-value/format
          render-value
          
          get-language-position
          get-language-numbers
          get-one-line-summary))
      
      (define simple-module-based-language<%>
	(interface ()
          get-module
          get-language-position
          get-language-numbers
          get-one-line-summary
          get-reader))
      
      
                                          
          ;                  ;;;          
                               ;          
                               ;          
  ;;;   ;;;   ;;; ;  ; ;;;     ;     ;;;  
 ;   ;    ;    ; ; ;  ;   ;    ;    ;   ; 
  ;;;     ;    ; ; ;  ;   ;    ;    ;;;;; 
     ;    ;    ; ; ;  ;   ;    ;    ;     
 ;   ;    ;    ; ; ;  ;   ;    ;    ;   ; 
  ;;;   ;;;;; ;; ; ;; ;;;;   ;;;;;;  ;;;  
                      ;                   
                      ;                   
                     ;;;                  

      
      (define simple-module-based-language%
        (class* object% (simple-module-based-language<%>)
          (init-field module
                      language-position
                      (language-numbers (map (lambda (x) 0) language-position))
                      (one-line-summary "")
                      (documentation-reference #f)
                      (reader read-syntax))
          (define/public (get-module) module)
	  (define/public (get-language-position) language-position)
          (define/public (get-language-numbers) language-numbers)
          (define/public (get-one-line-summary) one-line-summary)
	  (define/public (get-reader) reader)
          (super-instantiate ())))
      

                                                                                                  
                                ;;         ;;;                 ;;                              ;; 
                                 ;           ;                  ;                               ; 
         ;                       ;           ;                  ;                               ; 
          ;   ;;; ;    ;;;    ;;;; ;;  ;;    ;     ;;;          ;;;;   ;;;;    ;;;    ;;;    ;;;; 
           ;   ; ; ;  ;   ;  ;   ;  ;   ;    ;    ;   ;         ;   ;      ;  ;   ;  ;   ;  ;   ; 
 ;;;;;      ;  ; ; ;  ;   ;  ;   ;  ;   ;    ;    ;;;;;  ;;;;;  ;   ;   ;;;;   ;;;   ;;;;;  ;   ; 
           ;   ; ; ;  ;   ;  ;   ;  ;   ;    ;    ;             ;   ;  ;   ;      ;  ;      ;   ; 
          ;    ; ; ;  ;   ;  ;   ;  ;   ;    ;    ;   ;         ;   ;  ;   ;  ;   ;  ;   ;  ;   ; 
         ;    ;; ; ;;  ;;;    ;;; ;  ;;; ; ;;;;;;  ;;;         ; ;;;    ;;; ;  ;;;    ;;;    ;;; ;
                                                                                                  
                                                                                                  
                                                                                                  
                                                                                    
                                                                                    

      
      ;; simple-module-based-language->module-based-language : module-based-language<%>
      ;; transforms a simple-module-based-language into a module-based-language<%>
      (define simple-module-based-language->module-based-language-mixin
	(mixin (simple-module-based-language<%>) (module-based-language<%>)
	  (define/public (get-transformer-module) 'mzscheme)
          (define/public (use-namespace-require/copy?) #f)
          (define/public (use-mred-launcher?) #t)
          
          (inherit get-module)
          (define/public (marshall-settings settings)
	    (simple-settings->vector settings))
          (define/public (unmarshall-settings printable)
            (and (vector? printable)
                 (= (vector-length printable)
                    (procedure-arity make-simple-settings))
                 (boolean? (vector-ref printable 0))
                 (memq (vector-ref printable 1) '(constructor quasiquote write))
                 (memq (vector-ref printable 2) 
                       '(mixed-fraction 
                         mixed-fraction-e
                         repeating-decimal 
                         repeating-decimal-e))
                 (boolean? (vector-ref printable 3))
                 (boolean? (vector-ref printable 4))
                 (memq (vector-ref printable 5) '(none debug debug/profile))
                 (apply make-simple-settings (vector->list printable))))
          (define/public (default-settings) 
            (make-simple-settings #f 'write 'mixed-fraction-e #f #t 'debug))
          (define/public (default-settings? x)
	    (equal? (simple-settings->vector x)
		    (simple-settings->vector (default-settings))))
          (define/public (config-panel parent)
	    (simple-module-based-language-config-panel parent))
	  
          (define/public (on-execute setting run-in-user-thread)
	    (initialize-simple-module-based-language setting run-in-user-thread))
          (define/public (get-init-code setting)
            (simple-module-based-language-get-init-code setting))
          
          (define/public (render-value/format value settings port put-snip width)
            (simple-module-based-language-render-value/format value settings port put-snip width))
          (define/public (render-value value settings port put-snip)
            (simple-module-based-language-render-value value settings port put-snip))
	  (super-instantiate ())))

      ;; settings for a simple module based language
      (define-struct simple-settings (case-sensitive 
                                      printing-style
                                      fraction-style
                                      show-sharing
                                      insert-newlines
                                      annotations))
      ;;  case-sensitive  : boolean
      ;;  printing-style  : (union 'write 'constructor 'quasiquote)
      ;;  fraction-style  : (union 'mixed-fraction 'mixed-fraction-e 'repeating-decimal 'repeating-decimal-e)
      ;;  show-sharing    : boolean
      ;;  insert-newlines : boolean
      ;;  annotations     : (union 'none 'debug 'debug/profile)
      (define simple-settings->vector (make-->vector simple-settings))

      ;; simple-module-based-language-config-panel : parent -> (case-> (-> settings) (settings -> void))
      (define (simple-module-based-language-config-panel _parent)
	(let* ([parent (make-object vertical-panel% _parent)]
	       
	       [input-msg (make-object message% (string-constant input-syntax) parent)]
	       [input-panel (instantiate vertical-panel% ()
                              (parent parent)
                              (style '(border))
                              (alignment '(left center)))]
               
               [dynamic-msg (make-object message% (string-constant dynamic-properties) parent)]
	       [dynamic-panel (instantiate vertical-panel% ()
                                (parent parent)
                                (style '(border))
                                (alignment '(left center)))]
	       
	       [output-msg (make-object message% (string-constant output-syntax) parent)]
	       [output-panel (instantiate vertical-panel% ()
                               (parent parent)
                               (style '(border))
                               (alignment '(left center)))]
               
	       [case-sensitive (make-object check-box%
				 (string-constant case-sensitive-label)
				 input-panel
				 void)]
               [debugging (instantiate radio-box% ()
                            (label #f)
                            (choices 
                             (list (string-constant no-debugging-or-profiling)
                                   (string-constant debugging)
                                   (string-constant debugging-and-profiling)))
                            (parent dynamic-panel)
                            (callback void))]
	       [output-style (make-object radio-box%
			       (string-constant output-style-label)
			       (list (string-constant constructor-printing-style)
				     (string-constant quasiquote-printing-style)
				     (string-constant write-printing-style))
			       output-panel
			       void)]
               [fraction-style
                (make-object check-box% (string-constant decimal-notation-for-rationals)
                  output-panel
                  void)]
	       [show-sharing (make-object check-box%
			       (string-constant sharing-printing-label)
			       output-panel
			       void)]
	       [insert-newlines (make-object check-box%
				  (string-constant use-pretty-printer-label)
				  output-panel
				  void)])
	  
	  ;; set the characteristics of the GUI
          (send _parent set-alignment 'center 'center)
	  (send parent stretchable-height #f)
	  (send parent stretchable-width #f)
          
	  (case-lambda
           [()
	    (make-simple-settings
	     (send case-sensitive get-value)
	     (case (send output-style get-selection)
	       [(0) 'constructor]
	       [(1) 'quasiquote]
	       [(2) 'write])
             (if (send fraction-style get-value)
                 'repeating-decimal-e
                 'mixed-fraction-e)
	     (send show-sharing get-value)
	     (send insert-newlines get-value)
             (case (send debugging get-selection)
               [(0) 'none]
               [(1) 'debug]
               [(2) 'debug/profile]))]
           [(settings)
            (send case-sensitive set-value (simple-settings-case-sensitive settings))
            (send output-style set-selection
                  (case (simple-settings-printing-style settings)
                    [(constructor) 0]
                    [(quasiquote) 1]
                    [(write) 2]))
            (send fraction-style set-value (eq? (simple-settings-fraction-style settings)
                                                'repeating-decimal-e))
            (send show-sharing set-value (simple-settings-show-sharing settings))
            (send insert-newlines set-value (simple-settings-insert-newlines settings))
            (send debugging set-selection
                  (case (simple-settings-annotations settings)
                    [(none) 0]
                    [(debug) 1]
                    [(debug/profile) 2]))])))

      ;; simple-module-based-language-render-value/format : TST settings port (union #f (snip% -> void)) number -> void
      (define (simple-module-based-language-render-value/format value settings port put-snip width)
        (parameterize ([current-inspector drscheme-inspector])
          (let ([converted-value
                 (simple-module-based-language-convert-value value settings)])
            (parameterize ([print-graph
                            ;; only turn on print-graph when using `write' printing 
                            ;; style because the sharing is being taken care of
                            ;; by the print-convert sexp construction when using
                            ;; other printing styles.
                            (and (eq? (simple-settings-printing-style settings) 'write)
                                 (simple-settings-show-sharing settings))]
                           [drscheme:rep:which-number-snip
                            (lambda (n)
                              (simple-settings-fraction-style settings))])
              (cond
                [(simple-settings-insert-newlines settings)
                 (if (number? width)
                     (parameterize ([pretty-print-columns width])
                       (pretty-print converted-value port))
                     (pretty-print converted-value port))]
                [else
                 (parameterize ([pretty-print-columns 'infinity])
                   (pretty-print converted-value port))
                 (newline port)])))))
      
      ;; simple-module-based-language-render-value : TST settings port (union #f (snip% -> void)) -> void
      (define (simple-module-based-language-render-value value settings port put-snip)
        (parameterize ([pretty-print-columns 'infinity]
                       [current-inspector drscheme-inspector]
                       [drscheme:rep:use-number-snip
                        (lambda (x)
                          (if (and (number? x)
                                   (exact? x)
                                   (real? x)
                                   (not (integer? x)))
                              (if (memq (simple-settings-fraction-style settings)
                                        '(repeating-decimal repeating-decimal-e))
                                  (simple-settings-fraction-style settings)
                                  #t)
                              #f))])
          (pretty-print (simple-module-based-language-convert-value value settings) port)))
      
      ;; drscheme-inspector : inspector
      (define drscheme-inspector (current-inspector))
      
      ;; simple-module-based-language-convert-value : TST settings -> TST
      (define (simple-module-based-language-convert-value value settings)
        (case (simple-settings-printing-style settings)
          [(write) value]
          [(constructor)
           (parameterize ([constructor-style-printing #t]
                          [show-sharing (simple-settings-show-sharing settings)]
			  [current-print-convert-hook leave-snips-alone-hook])
             (print-convert value))]
          [(quasiquote)
           (parameterize ([constructor-style-printing #f]
                          [show-sharing (simple-settings-show-sharing settings)]
			  [current-print-convert-hook leave-snips-alone-hook])
             (print-convert value))]))
      
      ;; leave-snips-alone-hook : any? (any? -> printable) any? -> printable
      (define (leave-snips-alone-hook expr basic-convert sub-convert)
	(if (is-a? expr snip%)
	    expr
	    (basic-convert expr)))

      ;; initialize-simple-module-based-language : setting ((-> void) -> void)
      (define (initialize-simple-module-based-language setting run-in-user-thread)
        (run-in-user-thread
         (lambda ()
           (let ([annotations (simple-settings-annotations setting)])
             (when (memq annotations '(debug debug/profile))
               (current-eval 
                (drscheme:debug:make-debug-eval-handler
                 (current-eval)))
               (error-display-handler 
                (drscheme:debug:make-debug-error-display-handler
                 (error-display-handler))))
             (drscheme:debug:profiling-enabled (eq? annotations 'debug/profile)))
           (current-inspector (make-inspector))
           (read-case-sensitive (simple-settings-case-sensitive setting)))))
      
      ;; simple-module-based-language-get-init-code : setting -> sexp[module]
      (define (simple-module-based-language-get-init-code setting)
        `(module mod-name mzscheme
           (require (lib "pconvert.ss")
                    (lib "pretty.ss"))
           
           (provide init-code)
           
           (define root-inspector (current-inspector))
           
           (define (executable-error-value->string-handler val size)
             (let ([o (open-output-string)])
               (render-value val o)
               (let ([s (get-output-string o)])
                 (if ((string-length s) . <= . size)
                     s
                     (string-append
                      (substring s 0 (- size 3))
                      "...")))))
           
           (define (render-value value port)
             (parameterize ([pretty-print-columns 'infinity]
                            [current-inspector root-inspector])
               (pretty-print (convert-value value) port)))
           
           (define (convert-value value)
             ,(case (simple-settings-printing-style setting)
                [(write) `value]
                [(constructor)
                 `(parameterize ([constructor-style-printing #t]
                                 [show-sharing ,(simple-settings-show-sharing setting)])
                    (print-convert value))]
                [(quasiquote)
                 `(parameterize ([constructor-style-printing #f]
                                 [show-sharing ,(simple-settings-show-sharing setting)])
                    (print-convert value))]))
           
           ,(if (memq (simple-settings-annotations setting) '(debug debug/profile))
                `(require (lib "errortrace.ss" "errortrace"))
                `(void))

           (define (init-code)
             ,(drscheme:teachpack:launcher-init-code (preferences:get 'drscheme:teachpacks))
             (current-inspector (make-inspector))
             (error-value->string-handler executable-error-value->string-handler)
             (read-case-sensitive ,(simple-settings-case-sensitive setting)))))
      


                                                                      
               ;;;                                                    
                 ;                                                    
         ;       ;                                                    
          ;      ;    ;;;;  ; ;;;    ;;; ;;;  ;;  ;;;;    ;;; ;  ;;;  
           ;     ;        ;  ;;  ;  ;   ;  ;   ;      ;  ;   ;  ;   ; 
 ;;;;;      ;    ;     ;;;;  ;   ;  ;   ;  ;   ;   ;;;;  ;   ;  ;;;;; 
           ;     ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;     
          ;      ;    ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ;  ;   ; 
         ;     ;;;;;;  ;;; ;;;;  ;;  ;;;;   ;;; ;  ;;; ;  ;;;;   ;;;  
                                        ;                    ;        
                                        ;                    ;        
                                     ;;;                  ;;;         

      
      
      ;; module-based-language->language : module-based-language -> language<%>
      ;; given a module-based-language, implements a language
      (define module-based-language->language-mixin
	(mixin (module-based-language<%>) (language<%>)
	  (inherit get-module get-transformer-module use-namespace-require/copy?
                   get-init-code use-mred-launcher? get-reader)
	  (rename [super-on-execute on-execute])

          (inherit get-language-position)
          (define/public (get-language-name)
            (let ([pos (get-language-position)])
              (if (null? pos)
                  "<<unknown>>"
                  (car (last-pair pos)))))

          (define/public (get-style-delta) #f)
	  (define/override (on-execute setting run-in-user-thread)
	    (super-on-execute setting run-in-user-thread)
            (initialize-module-based-language (use-namespace-require/copy?)
                                              (get-module)
                                              (get-transformer-module)
                                              run-in-user-thread))
          (define/public (front-end input settings)
            (module-based-language-front-end input (get-reader)))
          (define/public (create-executable setting parent program-filename executable-filename)
            (create-module-based-language-executable parent 
                                                     program-filename
                                                     executable-filename
                                                     (get-module)
                                                     (get-transformer-module)
                                                     (get-init-code setting)
                                                     (use-mred-launcher?)
                                                     (use-namespace-require/copy?)))
          (super-instantiate ())))

      ;; create-module-based-language-executable :  ... -> void (see docs)
      (define (create-module-based-language-executable parent
                                                       program-filename
                                                       executable-filename
                                                       module-language-spec
                                                       transformer-module-language-spec
                                                       init-code
                                                       gui?
                                                       use-copy?)
        (let ([create-executable
               (if (use-stand-alone-executable? parent)
                   create-module-based-stand-alone-executable 
                   create-module-based-launcher)])
          (create-executable
           program-filename
           executable-filename 
           module-language-spec
           transformer-module-language-spec
           init-code
           gui?
           use-copy?)))
      
      ;; use-stand-alone-executable? : (union (instanceof frame%) (instanceof dialog%)) -> boolean
      (define (use-stand-alone-executable? parent)
	(let* ([raw-message
		(format (string-constant inline-saved-program-in-executable?)
			(case (system-type)
			  [(unix) (system-library-subpath)]
			  [else (system-type)]))]
	       [message
		(case (system-type)
		  [(windows)
		   (string-append
		    raw-message
		    "\n\n"
		    (format
		     (string-constant inline-saved-program-in-executable/windows/path)
		     (normalize-path (build-path (collection-path "mzlib") 'up 'up))))]
		  [else raw-message])])
	  (gui-utils:get-choice
	   message
	   (string-constant yes)
	   (string-constant no)
	   (string-constant drscheme)
	   parent)))
      
      ;; create-module-based-stand-alone-executable : ... -> void (see docs)
      (define (create-module-based-stand-alone-executable program-filename 
                                                          executable-filename
                                                          module-language-spec
                                                          transformer-module-language-spec
                                                          init-code
                                                          gui?
                                                          use-copy?)
        
        (with-handlers ([not-break-exn?
                         (lambda (exn)
                           (message-box 
                            (string-constant drscheme)
                            (if (exn? exn)
                                (exn-message exn)
                                (format "~s" exn))))])
          (define init-code-tmp-filename (make-temporary-file "drs-standalone-exectable-init~a"))
          (define bootstrap-tmp-filename (make-temporary-file "drs-standalone-exectable-bootstrap~a"))
          
          (call-with-output-file bootstrap-tmp-filename
            (lambda (port)
              (write `(begin
                        (,(if use-copy? 'namespace-require/copy 'namespace-require) ',module-language-spec)
                        (namespace-transformer-require ',transformer-module-language-spec)
                        ((dynamic-require '(file ,init-code-tmp-filename) 'init-code)))
                     port))
            'truncate
            'text)
          
          (let-values ([(_1 init-code-mod-name _2) (split-path init-code-tmp-filename)])
            (let ([new-init-code 
                   (list*
                    (car init-code)
                    (string->symbol init-code-mod-name)
                    (cddr init-code))])
              (call-with-output-file init-code-tmp-filename
                (lambda (port)
                  (write new-init-code port))
                'truncate 'text)))
          
          (let* ([pre-to-be-embedded-module-specs0
                  (if (equal? module-language-spec transformer-module-language-spec)
                      (list module-language-spec)
                      (list module-language-spec
                            transformer-module-language-spec))]
                 [pre-to-be-embedded-module-specs1
                  (if gui?
                      (cons '(lib "mred.ss" "mred")
                            pre-to-be-embedded-module-specs0)
                      pre-to-be-embedded-module-specs0)]
                 [pre-to-be-embedded-module-specs2
                  (cons `(file ,init-code-tmp-filename)
                        pre-to-be-embedded-module-specs1)]
                 [pre-to-be-embedded-module-specs3
                  (append (drscheme:teachpack:launcher-modules-to-embed
                           (preferences:get 'drscheme:teachpacks))
                          pre-to-be-embedded-module-specs2)]
                 [pre-to-be-embedded-module-specs4
                  (filter (lambda (x) (not (eq? x 'mzscheme)))
                          pre-to-be-embedded-module-specs3)]
                 [to-be-embedded-module-specs
                  (map (lambda (x) (list #f x))
                       pre-to-be-embedded-module-specs4)])
            
            (make-embedding-executable 
             executable-filename
             gui?
             #f ;; verbose?
             to-be-embedded-module-specs
             (list 
              bootstrap-tmp-filename
              program-filename)
             #f
             (if gui?
                 (list "-mvqZ")
                 (list "-mvq"))))))

      (define (condense-scheme-code-string s)
        (let ([i (open-input-string s)]
              [o (open-output-string)])
          (let loop ()
            (let ([c (read-char i)])
              (unless (eof-object? c)
                (let ([next (lambda ()
                              (display c o)
                              (loop))])
                  (case c
                    [(#\space)
                     (if (char=? #\( (peek-char i))
                         (loop)
                         (next))]
                    [(#\))
                     (if (eq? #\space (peek-char i))
                         (begin
                           (display #\) o)
                           (read-char i)
                           (loop))
                         (next))]
                    [(#\\)
                     (begin
                       (display #\\ o)
                       (display (read-char i) o)
                       (loop))]
                    [(#\" #\|)
                     (display c o)
                     (let loop ()
                       (let ([v (read-char i)])
                         (cond
                           [(eq? v c) (next)]
                           [(eq? v #\\)
                            (display v o)
                            (display v (read-char i))
                            (loop)]
                           [else (display v o)
                                 (loop)])))]
                    [else (next)])))))
          (get-output-string o)))
      
      (define (create-module-based-launcher program-filename 
                                            executable-filename
                                            module-language-spec
                                            transformer-module-language-spec
                                            init-code
                                            gui?
                                            use-copy?)

        (with-handlers ([not-break-exn?
                         (lambda (exn)
                           (message-box 
                            (string-constant drscheme)
                            (if (exn? exn)
                                (exn-message exn)
                                (format "~s" exn))))])
          
          ((if gui? make-mred-launcher make-mzscheme-launcher)
           (list
            "-qmvt"
            (build-path (collection-path "drscheme" "private") 
                        "launcher-bootstrap.ss")
            "--"
            (condense-scheme-code-string (format "~s" init-code))
            program-filename
            (format "~s" module-language-spec)
            (format "~s" transformer-module-language-spec)
            (format "~s" use-copy?)
            (format "~s" (if gui?  
                             (list 'mzscheme '(lib "mred.ss" "mred"))
                             (list 'mzscheme))))
           executable-filename)))
      
      ;; initialize-module-based-language : boolean module-spec module-spec ((-> void) -> void)
      (define (initialize-module-based-language use-copy?
                                                module-spec
                                                transformer-module-spec
                                                run-in-user-thread)
        (run-in-user-thread
         (lambda ()
           (with-handlers ([(lambda (x) #t)
                            (lambda (x)
                              (display (exn-message x))
                              (newline))])
	     (if use-copy?
		 (namespace-require/copy module-spec)
		 (namespace-require module-spec))
	     (namespace-transformer-require transformer-module-spec)))))

      ;; module-based-language-front-end : (input reader -> (-> (union sexp syntax eof)))
      ;; type reader = type-spec-of-read-syntax (see mz manual for details)
      (define (module-based-language-front-end input reader)
        (let-values ([(port source offset line col)
                      (cond
                        [(string? input)
                         (let ([skip-first-line? 
                                (let* ([tmp (open-input-file input)]
                                       [c1 (read-char tmp)]
                                       [c2 (read-char tmp)])
                                  (begin0
                                    (and (char=? c1 #\#)
                                         (char=? c2 #\!))
                                    (close-input-port tmp)))])
                           (let ([port (open-input-file input)])
                             (port-count-lines! port)
                             (if skip-first-line?
                                 (begin (read-line port 'any)
                                        (values port input (file-position port) 1 0))
                                 (values port input 0 0 0))))]
                        [else 
                         (let* ([text (text/pos-text input)]
                                [pre-start (text/pos-start input)]
                                [start (if (= pre-start 0)
                                           (get-post-hash-bang-start text)
                                           pre-start)]
                                [end (text/pos-end input)]
                                [start-line (send text position-paragraph start)]
                                [start-col (- start (send text paragraph-start-position start-line))])
                           (let ([port (open-input-text text start end)])
                             (port-count-lines! port)
                             (values port
                                     text
                                     start
                                     start-line
                                     start-col)))])])
          (let ([closed? #f])
            (lambda ()
              (if closed?
                  eof
                  (let ([result (reader source port (list line col offset))])
                    (if (eof-object? result)
                        (begin
                          (set! closed? #t)
                          (close-input-port port)
                          eof)
                        result)))))))
      
      (define-struct loc (source position line column span))

      ;; read-syntax-original : ... -> syntax
      ;; arguments are the same as to read-syntax.
      ;; adds the 'original annotations to the syntax that comes 
      ;; from applying read-syntax to the arguments
      (define (read-syntax-original . x)
        (let ([res (apply read-syntax x)])
          (let loop ([stx res])
            (cond
              [(syntax? stx)
               (let ([obj (syntax-e stx)])
                 (cond
                   [(pair? obj)
                    (with-syntax ([fst (loop (car obj))]
                                  [rst (loop (cdr obj))])
                      (printf "adding to: ~s\n" (syntax-object->datum stx))
                      (syntax-property
                       (syntax (fst . rst))
                       'original
                       (stx->loc stx)))]
                   [else
                    (syntax-property stx 'original (stx->loc stx))]))]
              [(pair? stx)
               (cons (loop (car stx))
                     (loop (cdr stx)))]
              [else stx]))))
      
      ;; stx->loc : syntax -> loc
      ;; adds the loc that corresponds to this syntax as the
      ;; 'original property to the input syntax.
      (define (stx->loc stx)
        (make-loc (syntax-source stx)
                  (syntax-position stx)
                  (syntax-line stx)
                  (syntax-column stx)
                  (syntax-span stx)))
      
      
                                                                                                         
                                      ;                                                                  
                                                                 ;             ;                    ;    
                                                                 ;             ;                    ;    
  ;;;  ; ;;;    ;;;  ; ;;;          ;;;   ; ;;;  ; ;;;  ;;  ;;  ;;;;;         ;;;;;   ;;;  ;;; ;;; ;;;;; 
 ;   ;  ;   ;  ;   ;  ;;  ;           ;    ;;  ;  ;   ;  ;   ;   ;             ;     ;   ;   ; ;    ;    
 ;   ;  ;   ;  ;;;;;  ;   ;  ;;;;;    ;    ;   ;  ;   ;  ;   ;   ;     ;;;;;   ;     ;;;;;    ;     ;    
 ;   ;  ;   ;  ;      ;   ;           ;    ;   ;  ;   ;  ;   ;   ;             ;     ;       ; ;    ;    
 ;   ;  ;   ;  ;   ;  ;   ;           ;    ;   ;  ;   ;  ;   ;   ;   ;         ;   ; ;   ;  ;   ;   ;   ;
  ;;;   ;;;;    ;;;  ;;;  ;;        ;;;;; ;;;  ;; ;;;;    ;;; ;   ;;;           ;;;   ;;;  ;;   ;;   ;;; 
        ;                                         ;                                                      
        ;                                         ;                                                      
       ;;;                                       ;;;                                                     

      
      ;; open-input-text : (instanceof text%) num num boolean -> input-port
      ;; creates a user port whose input is taken from the text%,
      ;; starting at position `start-in' (taking into account #!)
      ;; and ending at position `end'.
      (define (open-input-text text start end)
        (send text split-snip start)
        (send text split-snip end)
        (let* ([snip (send text find-snip start 'after-or-none)]
               [str #f]
               [pos 0]
               [update-str-to-snip
                (lambda ()
                  (cond
                    [(not snip)
                     (set! str #f)]
                    [((send text get-snip-position snip) . >= . end)
                     (set! str #f)]
                    [(is-a? snip string-snip%)
                     (set! str (send snip get-text 0 (send snip get-count)))]
                    [else
                     (set! str 'snip)]))]
               [next-snip
                (lambda ()
                  (set! snip (send snip next))
                  (set! pos 0)
                  (update-str-to-snip))]
               [read-char (lambda ()
                            (cond
                              [(not str) eof]
                              [(string? str)
                               (begin0 (string-ref str pos)
                                       (set! pos (+ pos 1))
                                       (when ((string-length str) . <= . pos)
                                         (next-snip)))]
                              [(eq? str 'snip)
                               (begin0
                                 (let ([the-snip snip])
                                   (lambda (file line col pos)
                                     (if (is-a? the-snip drscheme:snip:special<%>)
                                         (send the-snip read-special file line col pos)
                                         (values (send the-snip copy) 1))))
                                 (next-snip))]))]
               [char-ready? (lambda () #t)]
               [close (lambda () (void))]
               ;; We create a slow port for now; in the future, try
               ;; grabbing more characters:
               [port (make-custom-input-port 
                      #f
                      (lambda (s)
                        (let ([c (read-char)])
                          (if (char? c)
                              (begin
                                (string-set! s 0 c)
                                1)
                              c)))
                      #f ; no peek
                      close)])
          (update-str-to-snip)
          (port-count-lines! port)
          port))
      
      ;; get-post-hash-bang-start : text -> number
      ;; returns the beginning of the line after #! if there
      ;; is a #! or `pos'.
      (define (get-post-hash-bang-start text)
        (if (1 . < . (send text last-position))
            (let ([c1 (send text get-character 0)]
                  [c2 (send text get-character 1)])
              (if (and (char=? c1 #\#)
                       (char=? c2 #\!))
                  (if (1 . <= . (send text last-paragraph))
                      (send text paragraph-start-position 1)
                      (send text last-position))
                  0))
            0))

                                                                      
                                             ;                        
                ;                                                     
                ;                                                     
  ;;;  ;;; ;;; ;;;;;   ;;;  ; ;;;    ;;;   ;;;     ;;;  ; ;;;    ;;;  
 ;   ;   ; ;    ;     ;   ;  ;;  ;  ;   ;    ;    ;   ;  ;;  ;  ;   ; 
 ;;;;;    ;     ;     ;;;;;  ;   ;   ;;;     ;    ;   ;  ;   ;   ;;;  
 ;       ; ;    ;     ;      ;   ;      ;    ;    ;   ;  ;   ;      ; 
 ;   ;  ;   ;   ;   ; ;   ;  ;   ;  ;   ;    ;    ;   ;  ;   ;  ;   ; 
  ;;;  ;;   ;;   ;;;   ;;;  ;;;  ;;  ;;;   ;;;;;   ;;;  ;;;  ;;  ;;;  
                                                                      
                                                                      

      (define language-extensions null)
      (define (get-language-extensions) 
        (drscheme:tools:only-in-phase
         'drscheme:language:get-default-mixin
         'phase2)
        language-extensions)

      (define (default-mixin x) x)
      (define (get-default-mixin)
        (drscheme:tools:only-in-phase
         'drscheme:language:get-default-mixin
         'phase2)
        default-mixin)
      
      (define (extend-language-interface extension<%> default-impl)
        (drscheme:tools:only-in-phase
         'drscheme:language:extend-language-interface
         'phase1)
        (set! default-mixin (compose default-impl default-mixin))
        (set! language-extensions (cons extension<%> language-extensions))))))

