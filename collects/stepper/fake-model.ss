; this is an icky hack: the annotater wants to know whether procedures are primitives;
; if they are, it wraps their applications with return-value breaks.  For the purposes
; of the debugger, it doesn't matter, since no breaks are really inserted anyway.
; So this unit is a complete farce.

(unit/sig stepper:model^
  (import)
  
  (define check-pre-defined-var
    (lambda (ignored) #f))
  
  (define check-global-defined 'fake)
  (define global-lookup 'fake)
  (define constructor-style-printing? 'fake)
  (define abbreviate-cons-as-list? 'fake)
  (define user-cons? 'fake)
  (define user-vector? 'fake)
  (define image? 'fake)
  (define print-convert 'fake))