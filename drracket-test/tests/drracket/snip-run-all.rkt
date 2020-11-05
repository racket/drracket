#lang racket/base
(require racket/gui/base)
(require racket/runtime-path)
(define-runtime-path here "snip")

(define known-wxme-failures '("collapsed.rkt" "run-collapsed.rkt"))

(define (record-failure exn)
  (parameterize ([current-error-port (current-output-port)])
    (set! failures (+ failures 1))
    ((error-display-handler) (exn-message exn) exn)))

(define failures 0)
(define tried 0)

(parameterize ([current-directory here]
               #;[use-compiled-file-paths '()])
  ;; Setting the use-compiled-file-paths is extra safe to
  ;; so we don't "cheat" by using the wxme version to compile
  ;; the file and then just avoid using the GUI version at all.
  ;; But the "info.rkt" file ensures that
  ;; the modules in this directory are not compiled, and
  ;; disabling all compiled files makes the test very slow.
  
  (for ([f (in-list (sort (directory-list) string<=? 
                          #:key path->string))]
        #:when (regexp-match? #rx"[.]rkt$" (path->bytes f))
        #:unless (regexp-match? #rx#"/info.rkt$" (path->bytes f)))

    (define gui-namespace (make-gui-namespace))
    (define base-namespace (make-base-namespace))
    (define eventspace (make-eventspace))
    (define done (make-semaphore 0))
    
    ;; need to be on a fresh eventspace to make
    ;; sure we get a fresh snipclass list
    (parameterize ([current-eventspace eventspace])
      (queue-callback
       (λ ()
         (parameterize ([current-namespace gui-namespace])
           (set! tried (+ tried 1))
           (printf "=== trying ~a with gui-namespace\n" f)
           (with-handlers ((exn:fail? record-failure))
             (dynamic-require (build-path here f) #f))
           (semaphore-post done)))))
    (semaphore-wait done)
        
    (unless (member (path->string f) known-wxme-failures)
      (parameterize ([current-namespace base-namespace])
        (set! tried (+ tried 1))
        (printf "=== trying ~a with base-namespace\n" f)
        (with-handlers ((exn:fail? record-failure))
          (dynamic-require (build-path here f) #f))))))

(printf "tried ~a files\n" tried)
(unless (zero? failures)
  (eprintf "~a attempt~a failed\n"
           failures
           (if (= failures 1) "" "s")))
