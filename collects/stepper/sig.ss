(define-signature xml-reconstruct^
  (patch-up-sexp))

(define-signature stepper:zodiac-client-procs^
  (never-undefined-getter
   never-undefined-setter
   read-getter
   read-setter))

(define-signature stepper:error^
  (static-error dynamic-error internal-error))

(define-signature stepper:model-input^
  (text-stream settings image? receive-result))

(define-signature stepper:model^
  (check-pre-defined-var
   check-global-defined
   global-lookup
   constructor-style-printing?
   abbreviate-cons-as-list?
   user-cons?
   user-vector?
   image?
   print-convert))

(define-signature stepper:shared^
  ((struct before-after-result (finished-exprs exp redex post-exp reduct))
   (struct before-error-result (finished-exprs exp redex err-msg))
   (struct error-result (finished-exprs err-msg))
   (struct finished-result (finished-exprs))
   list-take
   (struct closure-record (name mark constructor?))
   create-bogus-bound-varref
   create-bogus-top-level-varref
   *unevaluated* 
   no-sexp
   if-temp
   struct-flag
   highlight-placeholder
   get-arg-symbol
   top-level-exp-gensym-source
   expr-read
   set-expr-read!
   flatten-take
   closure-table-put!
   closure-table-lookup))

(define-signature stepper:annotate^
  (initial-env-package
   annotate))

(define-signature stepper:reconstruct^
  (expose-mark-list
   reconstruct-completed
   reconstruct-current
   final-mark-list?
   skip-result-step?
   skip-redex-step?))

  