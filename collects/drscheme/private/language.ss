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
	   (lib "mred.ss" "mred")
	   (lib "framework.ss" "framework"))

  (provide language@)

  (define language@
    (unit/sig drscheme:language^
      (import [drscheme:rep : drscheme:rep^]
              [drscheme:snip : drscheme:snip^]
              [drscheme:debug : drscheme:debug^])
      
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
          
          get-language-position
          get-style-delta
          get-language-numbers))
      
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
	  on-execute
          render-value/format
          render-value
          
          get-language-position
          get-language-numbers))
      
      (define simple-module-based-language<%>
	(interface ()
          get-module
          get-language-position
          get-language-numbers))
      
      (define simple-module-based-language%
        (class* object% (simple-module-based-language<%>)
          (init-field module
                      language-position
                      (language-numbers (map (lambda (x) 0) language-position)))
          (define/public (get-module) module)
	  (define/public (get-language-position) language-position)
          (define/public (get-language-numbers) language-numbers)
	  (super-instantiate ())))
      
      ;; simple-module-based-language->module-based-language : module-based-language<%>
      ;; transforms a simple-module-based-language into a module-based-language<%>
      (define simple-module-based-language->module-based-language-mixin
	(mixin (simple-module-based-language<%>) (module-based-language<%>)
	  (public config-panel on-execute 
                  render-value/format render-value
                  default-settings? default-settings marshall-settings unmarshall-settings)

          (define/public (get-transformer-module) 'mzscheme)
          (define/public (use-namespace-require/copy?) #f)

          (inherit get-module)
          (define (marshall-settings settings)
	    (simple-settings->vector settings))
          (define (unmarshall-settings printable)
            (and (vector? printable)
                 (= (vector-length printable)
                    (procedure-arity make-simple-settings))
                 (boolean? (vector-ref printable 0))
                 (memq (vector-ref printable 1) '(constructor quasiquote write))
                 (boolean? (vector-ref printable 2))
                 (boolean? (vector-ref printable 3))
                 (boolean? (vector-ref printable 4))
                 (apply make-simple-settings (vector->list printable))))
          (define (default-settings) 
            (make-simple-settings #f 'write #f #t #t))
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
      (define-struct simple-settings (case-sensitive printing-style show-sharing insert-newlines debugging))
      ;;  case-sensitive  : boolean
      ;;  printing-style  : (union 'write 'constructor 'quasiquote)
      ;;  show-sharing    : boolean
      ;;  insert-newlines : boolean
      ;;  debugging       : boolean
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
               [debugging (instantiate check-box% ()
                            (label (string-constant debugging))
                            (parent dynamic-panel)
                            (callback void))]
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
          
	  (case-lambda
           [()
	    (make-simple-settings
	     (send case-sensitive get-value)
	     (case (send output-style get-selection)
	       [(0) 'constructor]
	       [(1) 'quasiquote]
	       [(2) 'write])
	     (send show-sharing get-value)
	     (send insert-newlines get-value)
             (send debugging get-value))]
           [(settings)
            (send case-sensitive set-value (simple-settings-case-sensitive settings))
            (send output-style set-selection
                  (case (simple-settings-printing-style settings)
                    [(constructor) 0]
                    [(quasiquote) 1]
                    [(write) 2]))
            (send show-sharing set-value (simple-settings-show-sharing settings))
            (send insert-newlines set-value (simple-settings-insert-newlines settings))
            (send debugging set-value (simple-settings-debugging settings))])))

      ;; simple-module-based-language-render-value/format : TST settings port (union #f (snip% -> void)) -> void
      (define (simple-module-based-language-render-value/format value settings port put-snip)
        (parameterize ([current-inspector drscheme-inspector])
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
                 (newline port)])))))
      
      ;; simple-module-based-language-render-value : TST settings port (union #f (snip% -> void)) -> void
      (define (simple-module-based-language-render-value value settings port put-snip)
        (parameterize ([pretty-print-columns 'infinity]
                       [current-inspector drscheme-inspector])
          (pretty-print (simple-module-based-language-convert-value value settings) port)))
      
      ;; drscheme-inspector : inspector
      (define drscheme-inspector (current-inspector))
      
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
         (when (simple-settings-debugging setting)
           (current-eval 
            (drscheme:debug:make-debug-eval-handler
             (current-eval)))
           (error-display-handler 
            (drscheme:debug:make-debug-error-display-handler
             (error-display-handler))))
         (current-inspector (make-inspector))
	 (read-case-sensitive (simple-settings-case-sensitive setting)))))
      
      ;; module-based-language->language : module-based-language -> language<%>
      ;; given a module-based-language, implements a language
      (define module-based-language->language-mixin
	(mixin (module-based-language<%>) (language<%>)
	  (inherit get-module get-transformer-module use-namespace-require/copy?)
	  (rename [super-on-execute on-execute])
          (define/public (get-style-delta) #f)
	  (define/override (on-execute setting run-in-user-thread)
	    (super-on-execute setting run-in-user-thread)
            (initialize-module-based-language (use-namespace-require/copy?)
                                              (get-module)
                                              (get-transformer-module)
                                              run-in-user-thread))
          (define/public (front-end input settings)
            (module-based-language-front-end input))
          (super-instantiate ())))

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
                             (port-count-lines! port)
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
                  (let ([result (read-syntax source port (list line col offset))])
                    (if (eof-object? result)
                        (begin
                          (set! closed? #t)
                          (close-input-port port)
                          eof)
                        result)))))))
      
      ;; open-input-text : (instanceof text%) num num -> input-port
      ;; creates a user port whose input is taken from the text%,
      ;; starting at position `start' and ending at position `end'
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
               [peek-char #f]
               [port
                (make-input-port read-char 
                                 char-ready?
                                 close
                                 peek-char)])
          (update-str-to-snip)
          (port-count-lines! port)
          port)))))
