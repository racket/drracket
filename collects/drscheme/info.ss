(module info (lib "infotab.ss" "setup")
  (define name "DrScheme")
  (define mred-launcher-libraries (list "drscheme.ss"))
  (define mred-launcher-names (list "DrScheme"))
  (define doc-sub-collections (list "tools"))
  (define compile-omit-files
    (list "rep-new.ss" "phooks.ss" "toy.ss" ; should these files be deleted?
          "rload.ss" "rrequire.ss"
          "getcoll.ss" "tmp.ss" ;; these files are tmp files in robby's directory, not in cvs
          "launcher-bootstrap.ss"
          "rrequire.ss"))
  (define compile-subcollections
    (map (lambda (x) (list "drscheme" "tools" x))
         (filter
          (lambda (x) 
            (and (not (string-ci=? "CVS" x))
                 (directory-exists? (build-path (collection-path "drscheme" "tools") x))
                 (file-exists? 
                  (build-path 
                   (collection-path "drscheme" "tools" x)
                   "info.ss"))))
          (directory-list (collection-path "drscheme" "tools"))))))
  