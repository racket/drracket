(unit/sig plt:userspace:params^
  (import)
  (define <=-at-least-two-args (make-parameter #t))
  (define allow-improper-lists (make-parameter #t))
  (define eq?-only-compares-symbols (make-parameter #f)))
