
;; Library functions

(require-library "file.ss")

;; path-list->path-list-string : (listof String) -> String
(define (path-list->path-list-string paths)
  (cond
    [(null? paths) ""]
    [else
     ;; loop : (cons String (listof String)) -> String
     (let loop ([paths paths])
       (cond
         [(null? (cdr paths)) (car paths)]
         [else (string-append (car paths) ":" (loop (cdr paths)))]))]))

;; delete-file-maybe : String -> Void
(define (delete-file-maybe path)
  (when (file-exists? path)
    (delete-file path)))

;; delete-directory*-maybe : String -> Void
(define (delete-directory*-maybe dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

;; clobber-file : String String -> Void
(define (clobber-file from to)
  (delete-file-maybe to)
  (copy-file from to))

;; find-executable : String -> (u String #f)
(define find-executable
  (let ([path (path-list-string->path-list (getenv "PATH") '("."))])
    (lambda (name)
      (ormap
       (lambda (p)
         (let ([guess (build-path p name)])
           (and (file-exists? guess) guess)))
       path))))