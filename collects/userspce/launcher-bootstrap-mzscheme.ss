(let ([settings settings]
      [teachpacks teachpacks]
      [filename filename])
  (unit/sig drscheme-jr:settings^
    (import [prims : prims^]
	    [basis : plt:basis^]
	    [mzlib : mzlib:core^])
    
    (basis:teachpack-changed teachpacks)

    (define show-banner? #f)
    (define repl? #f)
    (define (run-in-new-user-thread thunk)
      (thread thunk))
    
    (define (load-and-repl-done)
      (exit))

    (define (initialize-userspace) (void))

    (define setting (apply basis:make-setting (cdr (vector->list settings))))
    (define startup-file filename)))
