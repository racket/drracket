
(read-case-sensitive #t)
(require-library "core.ss")
(load "listlib.ss")
(load "veclib.ss")
(load "tic-func.ss")

(let loop ()
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (dump-memory-stats)
  (time (tic-tac-toe 1 1))
  '(loop))
