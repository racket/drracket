(require-relative-library "ricedefs.ss")
(require-library "sig.ss" "stepper")
(require-library "cores.ss")
(require-library "pconvers.ss")
(require-library "zsigs.ss" "zodiac")
(require-library "sigs.ss" "zodiac")
(require-library "coreflats.ss")
(require-relative-library "ricedefs.ss")
(require-library "sig.ss" "mred")
(require-library "turtles.ss" "graphics")

(define-signature plt:beginner-extras^
  ((struct posn (x y) -setters)
   (open mzlib:core-flat^)))

(define-signature plt:intermediate-extras^
  plt:beginner-extras^)

(begin-construction-time
 (if (defined? 'mred@)
     `(define-signature plt:userspace^
	((open mred^)
	 (open mzlib:core-flat^)
	 (open turtle^)
	 (struct posn (x y))))
     `(define-signature plt:userspace^
	((open mzlib:core-flat^)
	 (struct posn (x y))))))

(begin-construction-time
 (if (defined? 'mred@)
     `(define-signature plt:advanced-extras^
	((struct posn (x y))
	 (open mzlib:core-flat^)
	 (open turtle^)))
     `(define-signature plt:advanced-extras^
	((struct posn (x y))
	 (open mzlib:core-flat^)))))

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

(define-signature plt:init-params^
  (initialize-parameters
   settings
   get-default-setting
   get-default-setting-name

   drscheme-load-handler

   zodiac-vocabulary?
   beginner-language?
   intermediate-language?
   advanced-language?
   full-language?
   
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
		    printing
		    use-pretty-printer?))
   make-setting/parse

   find-setting-named
   add-setting
   copy-setting

   r4rs-style-printing?))

(define-signature plt:init-namespace^
  (init-namespace
   teachpack-ok?
   teachpack-changed))

(define-signature plt:basis^
  ((open plt:init-params^)
   (open plt:init-namespace^)))

(define-signature drscheme:interface^ 
  ((open zodiac:interface^)
   (struct exn:zodiac-syntax (link-tag))
   (struct exn:zodiac-read (link-tag))
   set-zodiac-phase))

(define-signature plt:basis-import^
  (invalid-teachpack
   in-mzscheme?))
