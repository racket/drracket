(module drracket-errortrace-key '#%kernel

  ;; as with errortrace/errortrace-key:
  ;; Defining `errortrace-key' as a function is a performance hack:
  ;; the compiler can track function constants, and in particular the
  ;; fact that it's not an impersonated/chaperoned mark key, so that a
  ;; `with-continuation-mark' using this key can be dropped if the
  ;; body expression is simple.
  (define-values (drracket-errortrace-key) (lambda () 'anything))
  
  (#%provide drracket-errortrace-key))
