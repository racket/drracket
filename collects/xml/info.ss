(lambda (sym fail)
  (let* ([sig "xmls.ss"]
         [signatures (list sig)])
    (case sym
      [(name) "XML"]
      [(compile-prefix) `(require-library ,sig "xml")]
      [(compile-omit-files) signatures]
      [(compile-elaboration-zos) signatures]
      ;[(compile-subcollections) (list (list "xml" "xt3d"))]
      [else (fail)])))
