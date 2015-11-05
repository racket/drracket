#lang racket
(require gui-debugger/annotator  gui-debugger/load-sandbox syntax/parse)
(provide run-code-with-annotator)

(define (run-code-with-annotator code)
  (parameterize ([current-namespace (make-base-namespace)])
    (define stx(expand-syntax code))
    (eval/annotations
     stx
     #;
     (expand-syntax-to-top-form
      (namespace-syntax-introduce
       (expand
        code)))
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
       (const (const #t))
       (const #f)
       (lambda (_ __ . vals) (apply values vals))
       ; record-bound-identifier
       void
       ; record-top-level-identifier
       void
       source))
    annotated))
