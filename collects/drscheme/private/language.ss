;; WARNING: printf is rebound in this module to always use the 
;;          original stdin/stdout of drscheme, instead of the 
;;          user's io ports, to aid any debugging printouts.
;;          (esp. useful when debugging the users's io)

(module language mzscheme
  (require "drsig.ss"
           (lib "string-constant.ss" "string-constants")
           (lib "pconvert.ss")
           (lib "pretty.ss")
	   (lib "macro.ss" "userspce")
           (lib "etc.ss")
	   (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "mred.ss" "mred")
	   (lib "framework.ss" "framework"))

  (provide language@)

  (define language@
    (unit/sig drscheme:language^
      (import [drscheme:rep : drscheme:rep^]
              [drscheme:snip : drscheme:snip^])
      
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
          get-teachpack-names
          render-value/format
          render-value
          
          get-language-position))
      
      (define module-based-language<%>
	(interface ()
	  marshall-settings
          unmarshall-settings
          default-settings
	  default-settings?

          get-module
	  config-panel
	  on-execute
          get-teachpack-names
          render-value/format
          render-value
          
          get-language-position))
      
      (define simple-module-based-language<%>
	(interface ()
	  get-module
          get-language-position
          get-teachpack-names))
      
      (define simple-module-based-language%
        (class* object% (simple-module-based-language<%>)
          (init-field module language-position teachpack-names)
          (public get-module get-language-position get-teachpack-names)
          (define (get-module) module)
	  (define (get-language-position) language-position)
          (define (get-teachpack-names) teachpack-names)
	  (super-instantiate ())))
      
      ;; simple-module-based-language->module-based-language : module-based-language<%>
      ;; transforms a simple-module-based-language into a module-based-language<%>
      (define simple-module-based-language->module-based-language-mixin
	(mixin (simple-module-based-language<%>) (module-based-language<%>)
	  (public config-panel on-execute 
                  render-value/format render-value
                  default-settings? default-settings marshall-settings unmarshall-settings)
          (inherit get-module)
          (define (marshall-settings settings)
	    (simple-settings->vector settings))
          (define (unmarshall-settings printable)
	    (and (vector? printable)
		 (= (vector-length printable)
		    (+ (procedure-arity make-simple-settings) 1))
		 (boolean? (vector-ref printable 1))
		 (memq (vector-ref printable 2) '(constructor quasiquote write))
		 (boolean? (vector-ref printable 3))
		 (boolean? (vector-ref printable 4))
		 (apply make-simple-settings (cdr (vector->list printable)))))
          (define (default-settings) 
            (make-simple-settings/parse
             '((case-sensitive #f)
               (printing-style write)
               (show-sharing #f)
               (insert-newlines #t))))
          (define (default-settings? x)
	    (equal? (simple-settings->vector x)
		    (simple-settings->vector (default-settings))))
          (define (config-panel parent)
	    (simple-module-based-language-config-panel parent))
	  (define (on-execute setting run-in-user-thread)
	    (initialize-simple-module-based-language setting run-in-user-thread))
          (define (render-value/format value settings port put-snip)
            (simple-module-based-language-render-value/format value settings port put-snip))
          (define (render-value value settings port put-snip)
            (simple-module-based-language-render-value value settings port put-snip))
	  (super-instantiate ())))

      ;; settings for a simple module based language
      ;;  case-sensitive  : boolean
      ;;  printing-style  : (union 'write 'constructor 'quasiquote)
      ;;  show-sharing    : boolean
      ;;  insert-newlines : boolean
      (define-struct/parse simple-settings
                           (case-sensitive printing-style show-sharing insert-newlines))

      ;; simple-module-based-language-config-panel : parent -> (case-> (-> settings) (settings -> void))
      (define (simple-module-based-language-config-panel _parent)
	(let* ([parent (make-object vertical-panel% _parent)]
	       
	       [input-msg (make-object message% (string-constant input-syntax) parent)]
	       [input-panel (make-object vertical-panel% parent '(border))]
	       
	       [output-msg (make-object message% (string-constant output-syntax) parent)]
	       [output-panel (make-object vertical-panel% parent '(border))]
               
	       [case-sensitive (make-object check-box%
				 (string-constant case-sensitive-label)
				 input-panel
				 void)]
	       [output-style (make-object radio-box%
			       (string-constant output-style-label)
			       (list (string-constant constructor-printing-style)
				     (string-constant quasiquote-printing-style)
				     (string-constant write-printing-style))
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
          (send parent set-alignment 'center 'center)
	  (send output-panel stretchable-width #f)
	  (send output-panel set-alignment 'left 'center)
          
          (send parent reflow-container)
	  (let*-values ([(iw) (send input-panel get-width)]
                        [(ow) (send output-panel get-width)]
                        [(w) (max iw ow)])
            (send input-panel min-width w)
            (send output-panel min-width w))
          
	  (case-lambda
           [()
	    (make-simple-settings
	     (send case-sensitive get-value)
	     (case (send output-style get-selection)
	       [(0) 'constructor]
	       [(1) 'quasiquote]
	       [(2) 'write])
	     (send show-sharing get-value)
	     (send insert-newlines get-value))]
           [(settings)
            (send case-sensitive set-value (simple-settings-case-sensitive settings))
            (send output-style set-selection
                  (case (simple-settings-printing-style settings)
                    [(constructor) 0]
                    [(quasiquote) 1]
                    [(write) 2]))
            (send show-sharing set-value (simple-settings-show-sharing settings))
            (send insert-newlines set-value (simple-settings-insert-newlines settings))])))

      ;; simple-module-based-language-render-value/format : TST settings port (union #f (snip% -> void)) -> void
      (define (simple-module-based-language-render-value/format value settings port put-snip)
        (let ([converted-value
               (simple-module-based-language-convert-value value settings)])
	  (parameterize ([print-graph
			  ;; only turn on print-graph when using `write' printing style
			  ;; because the sharing is being taken care of by the print-convert
			  ;; sexp construction when using other printing styles.
			  (and (eq? (simple-settings-printing-style settings) 'write)
			       (simple-settings-show-sharing settings))])
	    (cond
	      [(simple-settings-insert-newlines settings)
	       (pretty-print converted-value port)]
	      [else
               (parameterize ([pretty-print-columns 'infinity])
                 (pretty-print converted-value port))
               (newline port)]))))
      
      ;; simple-module-based-language-render-value : TST settings port (union #f (snip% -> void)) -> void
      (define (simple-module-based-language-render-value value settings port put-snip)
        (parameterize ([pretty-print-columns 'infinity])
          (pretty-print (simple-module-based-language-convert-value value settings) port)))
      
      ;; simple-module-based-language-convert-value : TST settings -> TST
      (define (simple-module-based-language-convert-value value settings)
        (case (simple-settings-printing-style settings)
          [(write) value]
          [(constructor)
           (parameterize ([constructor-style-printing #t]
                          [show-sharing (simple-settings-show-sharing settings)])
             (print-convert value))]
          [(quasiquote)
           (parameterize ([constructor-style-printing #f]
                          [show-sharing (simple-settings-show-sharing settings)])
             (print-convert value))]))
      
    ;; initialize-simple-module-based-language : setting ((-> void) -> void)
    (define (initialize-simple-module-based-language setting run-in-user-thread)
      (run-in-user-thread
       (lambda ()
	 (read-case-sensitive (simple-settings-case-sensitive setting)))))
      
      ;; module-based-language->language : language<%>
      ;; given a module-based-language, implements a language
      (define module-based-language->language-mixin
	(mixin (module-based-language<%>) (language<%>)
	  (public front-end)
	  (inherit get-module)
	  (override on-execute)
	  (rename [super-on-execute on-execute])
	  (define (on-execute setting run-in-user-thread)
	    (super-on-execute setting run-in-user-thread)
	    (initialize-module-based-language (get-module) run-in-user-thread))
          (define (front-end input settings)
            (module-based-language-front-end input))
          (super-instantiate ())))
      
      ;; initialize-simple-module-based-language : module-spec ((-> void) -> void)
      (define (initialize-module-based-language module-spec run-in-user-thread)
        ;; must call the resolver before setting the namespace
        (dynamic-require module-spec #f)
        (let ([orig-namespace (current-namespace)]
              [lang-name (if (symbol? module-spec)
			     module-spec
			     ((current-module-name-resolver) module-spec #f #f))])
          (run-in-user-thread
	   (let ([o (current-output-port)])
	     (lambda ()
	       (namespace-attach-module orig-namespace lang-name)
	       (namespace-require module-spec))))))

      ;; module-based-language-front-end : (input settings -> (-> (union sexp syntax eof)))
      (define (module-based-language-front-end input)
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
                             (if skip-first-line?
                                 (begin (read-line port 'any)
                                        (values port input (file-position port) 1 0))
                                 (values port input 0 0 0))))]
                        [else 
                         (let* ([text (text/pos-text input)]
                                [start (text/pos-start input)]
                                [end (text/pos-end input)]
                                [start-line (send text position-paragraph start)]
                                [start-col (- start (send text paragraph-start-position start-line))])
                           (send text split-snip start)
                           (send text split-snip end)
                           (values (open-input-text text start end)
                                   text
                                   start
                                   start-line
                                   start-col))])])
          (lambda ()
            (read-syntax source port (list line col offset)))))
      
      ;; open-input-text : (instanceof text%) num num -> input-port
      ;; creates a user port whose input is taken from the text%,
      ;; starting at position `start' and ending at position `end'
      (define (open-input-text text start end)
        (send text split-snip start)
        (send text split-snip end)
        (let* ([snip (send text find-snip start 'after-or-none)]
               [str #f]
               [pos 0]
               [read-special-object #f]
               [read-special-length 1]
               [read-special-proc
                (lambda ()
                  (values read-special-object 
                          read-special-length))]
               [update-str-to-snip
                (lambda ()
                  (cond
                    [(not snip)
                     (set! str #f)]
                    [((send text get-snip-position snip) . >= . end)
                     (set! str #f)]
                    [(is-a? snip drscheme:snip:special<%>)
                     (let-values ([(_read-special-object _read-special-length)
                                   (send snip read-special)])
                       (set! read-special-object _read-special-object)
                       (set! read-special-length _read-special-length)
                       (set! str " #$ "))]
                    [(is-a? snip string-snip%)
                     (set! str (send snip get-text 0 (send snip get-count)))]
                    [else
                     (set! read-special-object (send snip copy))
                     (set! read-special-length 1)
                     (set! str " #$ ")]))]
               [next-snip
                (lambda ()
                  (set! snip (send snip next))
                  (set! pos 0)
                  (update-str-to-snip))]
               [read-char (lambda () 
                            (when (and str
                                       ((string-length str) . <= . pos))
                              (next-snip))
                            (cond
                              [(not str) eof]
                              [else
                               (begin0
                                 (string-ref str pos)
                                 (set! pos (+ pos 1)))]))]
               [char-ready? (lambda () #t)]
               [close (lambda () (void))]
               [peek-char #f])
          (update-str-to-snip)
          (make-input-port read-char 
                           char-ready?
                           close
                           peek-char 
                           read-special-proc))))))
