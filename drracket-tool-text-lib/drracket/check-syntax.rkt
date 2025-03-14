#lang racket/base
(require racket/contract
         racket/class
         racket/path
         syntax/modread
         "private/syncheck/traversals.rkt"
         "private/syncheck/syncheck-intf.rkt"
         "private/syncheck/syncheck-local-member-names.rkt")

(provide
 (contract-out
  [show-content (->* ((or/c path-string?
                            (and/c syntax?
                                   has-path-string-source?)))
                     (#:fully-expanded? any/c
                      #:namespace (or/c #f namespace?))
                     (listof vector?))]
  [make-traversal 
   (-> namespace?
       (or/c path-string? #f)
       (values (->* (syntax?) ((-> any/c void?)) void?)
               (-> void?)))]
  
  [current-max-to-send-at-once
   (parameter/c (or/c +inf.0 (and/c exact-integer? (>=/c 2))))]
  [syncheck-annotations<%>
   interface?]
  [current-annotations 
   (parameter/c (or/c #f (is-a?/c syncheck-annotations<%>)))]
  [annotations-mixin 
   (and/c mixin-contract
          (-> any/c (implementation?/c syncheck-annotations<%>)))]))

(define (has-path-string-source? stx)
  (path-string? (syntax-source stx)))

;; methods in syncheck-annotations<%>
(provide 
 syncheck:find-source-object
 syncheck:add-text-type
 syncheck:add-background-color
 syncheck:add-require-open-menu
 syncheck:add-docs-menu
 syncheck:add-id-set
 syncheck:add-rename-menu
 syncheck:add-arrow
 syncheck:add-arrow/name-dup
 syncheck:add-arrow/name-dup/pxpy
 syncheck:add-tail-arrow
 syncheck:add-mouse-over-status
 syncheck:add-jump-to-definition
 syncheck:add-jump-to-definition/phase-level+space
 syncheck:add-definition-target
 syncheck:add-definition-target/phase-level+space
 syncheck:unused-binder
 syncheck:add-prefixed-require-reference
 syncheck:add-unused-require
 syncheck:color-range)

(define (show-content file-or-stx
                      #:fully-expanded? [fully-expanded? #f]
                      #:namespace [_namespace #f])
  (define expand? (or (not fully-expanded?) (not (syntax? file-or-stx))))
  (define ns (or _namespace (make-base-namespace)))
  (define src
    (cond
      [(path-string? file-or-stx)
       file-or-stx]
      [(syntax? file-or-stx)
       (syntax-source file-or-stx)]))
  (define o (new build-trace% [src src]))
  (define user-dir (path-only (simple-form-path src)))
  
  (parameterize ([current-annotations o])
    (define-values (expanded-expression expansion-completed) 
      (make-traversal ns user-dir))
    (cond
      [(path-string? file-or-stx)
       (parameterize ([current-namespace ns]
                      [current-load-relative-directory user-dir])
         (expanded-expression
          (expand
           (call-with-input-file file-or-stx
             (λ (port)
               (port-count-lines! port)
               (with-module-reading-parameterization
                (λ ()
                  (read-syntax file-or-stx port))))))))]
      [else 
       (parameterize ([current-namespace ns]
                      [current-load-relative-directory user-dir])
         (expanded-expression
          (if expand?
              (expand file-or-stx)
              file-or-stx)))])
    (expansion-completed))
  
  (send o get-trace))
