#lang racket/base
(require setup/xref 
         scribble/xref
         racket/match)
(provide get-index-entry-info
         flush-index-entry-cache)

(define xref (load-collections-xref))

(define req-chan (make-channel))

(define thd
  (thread
   (λ ()
     (define cache (make-hash))
     (define (lookup-binding-info binding-info)
       (let/ec escape
         (define (fail) (escape #f))
         (define definition-tag (xref-binding->definition-tag xref binding-info #f))
         (unless definition-tag (fail))
         (define-values (path tag) (xref-tag->path+anchor xref definition-tag))
         (unless path (fail))
         (define index-entry (xref-tag->index-entry xref definition-tag))
         (and index-entry
              (list (entry-desc index-entry)
                    path
                    definition-tag
                    tag))))

     (let loop ()
       (match (channel-get req-chan)
         [(list binding-info cd resp-chan nack-evt)
          (define resp
            (parameterize ([current-directory cd])
              (and xref
                   (hash-ref! cache binding-info (λ () (lookup-binding-info binding-info))))))
          (thread
           (λ ()
             (sync (channel-put-evt resp-chan resp)
                   nack-evt)))]
         [#f (set! cache (make-hash))])
       (loop)))))

;; these functions are called from a thread that might be killed
;; (but the body of this module is run in a context where it is
;; guaranteed that that custodian doesn't get shut down)
(define (get-index-entry-info binding-info)
  (and (not (thread-dead? thd))
       (sync
        (nack-guard-evt
         (λ (nack-evt)
           (define resp-chan (make-channel))
           (channel-put req-chan (list binding-info (current-directory) resp-chan nack-evt))
           resp-chan)))))

(define (flush-index-entry-cache)
  (unless (thread-dead? thd)
    (thread (λ () (channel-put-evt req-chan #f)))
    (void)))
