(lambda (sym fail)
  (let ([elab (list "cgis.ss" "mails.ss" "nntps.ss" "pop3s.ss" "urls.ss"
		    "smtps.ss" "heads.ss" "imaps.ss" "dnss.ss" "base64s.ss")])
    (case sym
      [(name) "Net"]
      [(compile-prefix) `(begin ,@(map (lambda (x) `(require-library ,x "net")) elab))]
      [(compile-omit-files) elab]
      [(compile-elaboration-zos) elab]
      [else (fail)])))