;; set things up so that the load-handler opens files into
;; a text when the file begins with WXME so that mred saved
;; files still load properly.

(require-library "errortrace.ss" "errortrace")
(require-library "core.ss" "drscheme-jr")

(define main-unit
  (let ([settings settings]
	[teachpacks teachpacks]
	[filename filename]
	[mred@ mred@])
    (unit/sig drscheme-jr:settings^
      (import [prims : prims^]
	      [basis : plt:basis^]
	      [mzlib : mzlib:core^]
	      mred^)
      
      (basis:teachpack-changed teachpacks)

      (define show-banner? #f)
      (define repl? #f)
      (define (run-in-new-user-thread thunk)
	(parameterize ([current-eventspace (make-eventspace)])
	  (let ([thread #f]
		[sema (make-semaphore 0)])
	    (queue-callback (lambda ()
			      (set! thread (current-thread))
			      (semaphore-post sema)))
	    (semaphore-wait sema)
	    (queue-callback
	     thunk)
	    thread)))
      (define (initialize-userspace)

        ;; add mred to the namespace
	(global-define-values/invoke-unit/sig mred^ mred@))

      (define setting (apply basis:make-setting (cdr (vector->list settings))))
      (define startup-file filename))))

(define go 
  (make-go
   (compound-unit/sig
     (import [prims : prims^]
	     [basis : plt:basis^]
	     [mzlib : mzlib:core^])
     (link [mred : mred^ (mred@)]
	   [main : drscheme-jr:settings^ (main-unit prims basis mzlib mred)])
     (export (open main)))))

(go)
