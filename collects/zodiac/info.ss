
(lambda (request failure)
  (case request
    [(name) "zodiac"]
    [(compile-prefix) '(begin
			 (require-library "refer.ss")
			 (require-library "zsigs.ss" "zodiac")
			 (require-library "sigs.ss" "zodiac"))]
    [(compile-omit-files)
     (list "sigs.ss" "zsigs.ss" "scm-hanc.ss" "quasi.ss")]
    [(compile-elaboration-zos)
     (list "zsigs.ss" "sigs.ss")]
    [else (failure)]))
