;;;
(with-handlers ([void void])
  ; Might not be there if this is MrEd-less DrJr distribution
  (require-relative-library "userspcs.ss"))
(require-relative-library "ricedefs.ss")
(require-library "ariess.ss" "cogen")
(require-library "cores.ss")
(require-library "pconvers.ss")
(require-library "zsigs.ss" "zodiac")
(require-library "sigs.ss" "zodiac")

;; extend structs with a parsing constructor
(define-macro define-struct/parse
  (lambda (str fields)
    (unless (symbol? str)
      (error 'define-struct/parse "no super structs allowed"))
    (let* ([first car]
	   [second cadr]
	   [second-name 'cadr]
	   [third caddr]
	   [defn (expand-defmacro `(#%define-struct ,str ,fields))]
	   [_ (unless (and (pair? defn)
			   (eq? (car defn) '#%define-values))
		(error 'define-struct/parse "expand-defmacro didn't return expected value: ~s~n" defn))]
	   [bindings (second defn)]
	   [exp (third defn)]
	   [make-parse (string->symbol (string-append "make-" (symbol->string str) "/parse"))]
	   [maker-name (second bindings)]
	   [parser
	    `(lambda (inits)
	       (apply ,maker-name
		      (map (lambda (field)
			     (let ([m (assq field inits)])
			       (unless m
				 (error ',make-parse "no binding for: ~a" field))
			       (unless (= (length m) 2)
				 (error ',make-parse "malformed binding: ~a" m))
			       (,second-name m)))
			   ',fields)))])
      `(define-values ,(cons make-parse bindings)
	 (call-with-values (lambda () ,exp)
			   (lambda bindings (apply values (cons ,parser bindings))))))))

(define-signature userspace:basis^
  (initialize-parameters
   settings
   get-default-setting
   get-default-setting-name

   zodiac-vocabulary?
   
   error-display/debug-handler
   current-vocabulary
   current-setting
   intermediate-values-during-load
   bottom-escape-handler

   drscheme-print

   initial-line
   initial-column
   initial-offset
   
   format-source-loc
   
   primitive-eval
   primitive-load
   syntax-checking-primitive-eval

   process/zodiac
   process/no-zodiac

   process-file/zodiac
   process-file/no-zodiac
   process-sexp/zodiac
   process-sexp/no-zodiac

   (struct process-finish (error?))

   setting-name->number
   number->setting
   (struct setting (name
		    vocabulary-symbol
		    extra-definitions-unit-name
		    macro-libraries
		    case-sensitive?
		    allow-set!-on-undefined?
		    unmatched-cond/case-is-error?
		    allow-improper-lists?
		    sharing-printing?
		    abbreviate-cons-as-list?
		    signal-undefined
		    signal-not-boolean
		    eq?-only-compares-symbols?
		    <=-at-least-two-args
		    disallow-untagged-inexact-numbers
		    print-tagged-inexact-numbers
		    whole/fractional-exact-numbers
                    print-booleans-as-true/false
		    printing))
   make-setting/parse

   find-setting-named
   add-setting
   copy-setting

   r4rs-style-printing?))

(define-signature drscheme:interface^ 
  ((open zodiac:interface^)
   set-zodiac-phase))

(define-signature userspace:basis-import^
  (in-mzscheme?))
