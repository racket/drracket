#lang racket
(require gui-debugger/annotator  gui-debugger/load-sandbox syntax/parse
         gui-debugger/marks)
(provide run-code-with-annotator break/test)

;; Syntax -> Any
;; run the given code with the debugging annotator
;; WARNING: the code *must* have src-locs to be annotated
;; the debugging annotator skips code with no source
(define (run-code-with-annotator code)
  (parameterize ([current-namespace (make-base-namespace)])
    (define stx (expand-syntax code))
    (eval/annotations
     stx
     (const #t)
     the-annotator)
    (syntax-parse stx
      #:literal-sets (kernel-literals)
      [(module name . a)
       (eval `(require ',(syntax->datum #'name)))]
      [_ (void)])))

(define the-annotator
  (lambda (stx)
    (define source (syntax-source stx))
    (define-values (annotated break-posns)
      (annotate-for-single-stepping
       (expand-syntax stx)
       ; always trigger breaks
       (const (const #t))
       ; don't interpose on returned values
       (const #f)
       ; if we are not in tail position don't interpose on returned values
       (lambda (_ __ . vals) (apply values vals))
       ; record-bound-identifier (do nothing at annotation time)
       void
       ; record-top-level-identifier (do nothing at runtime)
       void
       source))
    annotated))

(define ((break/test  id) [marks #f])
  (define debug-marks
    (or marks
        (continuation-mark-set->list (current-continuation-marks) debug-key)))
  (map mark-binding-value (lookup-all-bindings (const #t) debug-marks)))
