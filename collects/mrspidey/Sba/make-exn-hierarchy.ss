
(define src "/home/mflatt/proj/mred/mzscheme/src/exnsrc.ss")
(define dest "/home/cormac/plt/mrspidey/Sba/exn-hierarchy.ss")
(when (file-exists? dest) (delete-file dest))

(with-input-from-file src
  (lambda ()
    (let ([x (read)])
      (with-output-to-file dest
        (lambda ()
          (letrec
            ([trav
               (lambda (tree parent)
                 (match tree
                   [(name fields+strings . subtrees)
                     (let* ( [fields (filter symbol? fields+strings)]
                             [s (symbol->string name)]
                             [name (if (memq (string-ref s 0) '(#\* #\+))
                                     (string->symbol
                                       (substring s 1 (string-length s)))
                                     name)]
                             [name (if parent (symbol-append parent ': name) name)])
                       (display 
                         `(define-struct
                            ,(if parent 
                               `(,name ,(symbol-append 'struct: parent))
                               name)
                            ,fields))
                       (newline)
                       (for-each
                         (lambda (subtree) (trav subtree name))
                         subtrees))]
                   ['- (void)]
                   [(? string?) (void)]))])
            (trav x #f)))))))
