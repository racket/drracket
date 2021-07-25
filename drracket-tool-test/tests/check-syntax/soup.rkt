#lang racket/base
(require (for-syntax racket/base))

(provide (for-space soup kettle) kettle
         soup-ref)

(define-syntax (define-soup stx)
  (syntax-case stx ()
    [(_ id rhs)
     #`(define #,((make-interned-syntax-introducer 'soup)
                  #'id)
         rhs)]))

(define-soup kettle 'soup)
(define kettle 'default)

(define-syntax (soup-ref stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     ((make-interned-syntax-introducer 'soup) #'id)]))
