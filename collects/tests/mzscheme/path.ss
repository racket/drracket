(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'PATH)

(test #f relative-path? (current-directory))
(test #t relative-path? "down")
(test #t relative-path? (build-path 'up "down"))
(test #t relative-path? (build-path 'same "down"))
(test #t relative-path? (build-path 'same "down" "deep"))
(test #f relative-path? (build-path (current-directory) 'up "down"))
(test #f relative-path? (build-path (current-directory) 'same "down"))
(test #f relative-path? (build-path (current-directory) 'same "down" "deep"))
(test #f relative-path? (string #\a #\nul #\b))

(arity-test relative-path? 1 1)
(error-test '(relative-path? 'a))

(test #t absolute-path? (current-directory))
(test #f absolute-path? (build-path 'up))
(test #f absolute-path? (string #\a #\nul #\b))

(arity-test absolute-path? 1 1)
(error-test '(absolute-path? 'a))

(test #t complete-path? (current-directory))
(test #f complete-path? (build-path 'up))
(test #f complete-path? (string #\a #\nul #\b))

(arity-test complete-path? 1 1)
(error-test '(complete-path? 'a))

(call-with-output-file "tmp6" void 'replace)
(define existant "tmp6")

(test #t file-exists? existant)

(test #t make-directory "down")
(test #f make-directory "down")
(test #t directory-exists? "down")
(test #f file-exists? "down")

(define deepdir (build-path "down" "deep"))
(test #t make-directory deepdir)
(test #f make-directory deepdir)
(test #t directory-exists? deepdir)
(test #f file-exists? deepdir)

(test #t file-exists? (build-path "down" 'up existant))
(test #t file-exists? (build-path deepdir 'up 'up existant))
(test #t file-exists? (build-path 'same deepdir 'same 'up 'same 'up existant))

(test #f file-exists? (build-path "down" existant))
(test #f file-exists? (build-path deepdir 'up existant))
(test #f file-exists? (build-path 'same deepdir 'same 'same 'up existant))

(delete-file "tmp6")

(test #f file-exists? (build-path "down" 'up "badfile"))
(test #f file-exists? (build-path deepdir 'up 'up "badfile"))
(test #f file-exists? (build-path 'same deepdir 'same 'up 'same 'up "badfile"))

(error-test '(open-output-file (build-path "wrong" "down" "tmp8"))
	    exn:i/o:filesystem:file?)
(error-test '(open-output-file (build-path deepdir "wrong" "tmp7"))
	    exn:i/o:filesystem:file?)

(define start-time (current-seconds))
(close-output-port (open-output-file "tmp5" 'replace))
(close-output-port (open-output-file (build-path "down" "tmp8") 'replace))
(close-output-port (open-output-file (build-path deepdir "tmp7") 'replace))
(define end-time (current-seconds))

(map
 (lambda (f)
   (let ([time (seconds->date (file-modify-seconds f))]
	 [start (seconds->date start-time)]
	 [end (seconds->date end-time)])
     (test #t = (date-year start) (date-year time) (date-year end))
     (test #t = (date-month start) (date-month time) (date-month end))
     (test #t = (date-day start) (date-day time) (date-day end))
     (test #t = (date-week-day start) (date-week-day time) (date-week-day end))
     (test #t = (date-year-day start) (date-year-day time) (date-year-day end))
     (test #t = (date-hour start) (date-hour time) (date-hour end))
     (test #t <= (date-minute start) (date-minute time) (date-minute end))
     (test #t <= (date-second start) (date-second time) (date-second end))))
 (list "tmp5"
       (build-path "down" "tmp8")
       (build-path deepdir "tmp7")))

(test #t file-exists? "tmp5")
(test #t file-exists? (build-path "down" "tmp8"))
(test #t file-exists? (build-path deepdir "tmp7"))

(test #t rename-file "tmp5" "tmp5x")
(test #f rename-file "tmp5" "tmp5x")
(test #t rename-file (build-path "down" "tmp8") (build-path "down" "tmp8x"))
(test #f rename-file (build-path "down" "tmp8") (build-path "down" "tmp8x"))
(test #t rename-file (build-path deepdir "tmp7") (build-path deepdir "tmp7x"))
(test #f rename-file (build-path deepdir "tmp7") (build-path deepdir "tmp7x"))

(test #t rename-file (build-path deepdir "tmp7x") "tmp7x")
(test #f rename-file (build-path deepdir "tmp7x") "tmp7x")
(test #t rename-file "tmp7x" (build-path deepdir "tmp7x"))
(test #f rename-file "tmp7x" (build-path deepdir "tmp7x"))

(test #f not (member "tmp5x" (directory-list)))
(test #t 'directory-list 
      (let ([l (directory-list "down")])
	(or (equal? l '("deep" "tmp8x"))
	    (equal? l '("tmp8x" "deep")))))
(test '("tmp7x") directory-list deepdir)

(test #f delete-directory deepdir)
(test #f delete-directory "down")

(test #t delete-file (build-path deepdir "tmp7x"))
(test #f delete-file (build-path deepdir "tmp7x"))
(test #t delete-file (build-path "down" "tmp8x"))
(test #f delete-file (build-path "down" "tmp8x"))
(test #t delete-file "tmp5x")
(test #f delete-file "tmp5x")

(test #f delete-directory "down")
(test #t delete-directory deepdir)
(test #f delete-directory deepdir)
(test #t delete-directory "down")
(test #f delete-directory "down")

; Redefine these per-platform
(define drives null)
(define nondrive-roots (list "/"))
(define a (list "a"))
(define a/b (list "a/b" "a//b"))
(define a/b/c (list "a/b/c" "a//b/c"))
(define /a/b (list "/a/b"))
(define a/../b (list "a/../b"))
(define a/./b (list "a/./b"))
(define a/../../b (list "a/../../b"))
(define trail-sep "/")

(define add-slashes
  (lambda (l)
    (if (null? l)
	null
	(let loop ([s (car l)][rest (add-slashes (cdr l))])
	  (let ([naya (regexp-replace "/" s "\\")])
	    (if (string=? naya s)
		(cons s rest)
		(loop naya (cons s rest))))))))

(when (eq? (system-type) 'windows)
      (set! drives (list "c:" "c:/" "//hello/start" "//hello/start/"))
      (set! nondrive-roots null)
      (for-each
       (lambda (var)
	 (eval `(set! ,var (add-slashes ,var))))
       '(a a/b a/b/c /a/b a/../b a/./b a/../../b)))


(when (eq? (system-type) 'macos)
      (set! drives null)
      (set! nondrive-roots (filesystem-root-list))
      (set! a (list ":a"))
	  (set! a/b (list ":a:b"))
	  (set! a/b/c (list ":a:b:c"))
      (set! /a/b (list "a:b"))
      (set! a/../b (list ":a::b"))
      (set! a/./b null)
	  (set! a/../../b (list ":a:::b"))
	  (set! trail-sep ":"))

(define roots (append drives nondrive-roots))

(define a/ (map (lambda (s) (string-append s trail-sep)) a))
(define a/b/ (map (lambda (s) (string-append s trail-sep)) a/b))
(define a/b/c/ (map (lambda (s) (string-append s trail-sep)) a/b/c))
(define /a/b/ (map (lambda (s) (string-append s trail-sep)) /a/b))

(define absols (append roots /a/b /a/b/))
(define nondrive-absols (append nondrive-roots /a/b /a/b/))
(define rels (append a a/ a/b a/b/ a/b/c a/b/c/ a/../b a/./b a/../../b))

(define i (lambda (x) x))

(test #f ormap i (map relative-path? roots))
(test #t andmap i (map relative-path? a/b))
(test #f ormap i (map relative-path? /a/b))

(test #t andmap i (map absolute-path? roots))
(test #f ormap i (map absolute-path? a/b))

(test #t andmap i (map complete-path? drives))
(test #t andmap i (map complete-path? nondrive-roots))
(test #f ormap i (map complete-path? a/b))

(for-each
 (lambda (abs)
   (for-each
    (lambda (rel)
      (test #t string? (build-path abs rel))
      (for-each
       (lambda (rel2)
	 (test #t string? (build-path abs rel rel2)))
       rels))
    rels))
 absols)

(for-each
 (lambda (drive)
   (for-each
    (lambda (root)
      (test #t string? (build-path drive root))
      (for-each
       (lambda (rel)
	 (test #t string? (build-path drive root rel)))
       rels))
    nondrive-absols))
 drives)

(for-each
 (lambda (rel)
   (test (build-path (current-directory) rel)
	 path->complete-path rel))
 rels)

(define (test-path expect f . args)
  (test (normal-case-path (expand-path expect))
	normal-case-path (expand-path (apply f args))))

(for-each
 (lambda (absol)
   (let ([cabsol (path->complete-path absol)])
     (for-each
      (lambda (rel)
	(test-path (build-path cabsol rel) path->complete-path rel cabsol)
	(test-path (build-path cabsol rel rel) path->complete-path rel (build-path cabsol rel))
	(error-test `(path->complete-path ,rel ,rel) exn:i/o:filesystem:path?))
      rels)))
 absols)

(for-each
 (lambda (drive)
   (for-each
    (lambda (rel)
      (unless (relative-path? rel)
	      (test-path (build-path (current-drive) rel)
			 path->complete-path rel))
      (test-path (build-path drive rel) path->complete-path rel drive)
      (test-path (if (relative-path? rel)
		     (build-path drive rel rel)
		     (build-path drive rel))
		 path->complete-path rel (build-path drive rel)))
    (append rels nondrive-absols)))
 drives)

(for-each
 (lambda (drive)
   (test drive path->complete-path drive)
   (test drive path->complete-path drive drive))
 drives)

(unless (eq? (system-type) 'macos)
 (for-each
  (lambda (abs1)
    (for-each
     (lambda (abs2)
       (error-test `(build-path ,abs1 ,abs2) exn:i/o:filesystem:path?))
     absols))
  nondrive-roots))

(for-each
 (lambda (root)
   (let-values ([(base name dir?) (split-path root)])
      (test #f 'split-path base)
      (test #t 'split-path dir?)))
 roots)

(let ([check-a/b
       (lambda (a/b end/?)
	 (for-each
	  (lambda (path)
	    (let*-values ([(base name dir?) (split-path path)]
			  [(base2 name2 dir?2) (split-path base)])
	       (test "b" substring name 0 1)
	       (test end/? 'split-path dir?)
	       (test "a" substring name2 0 1)
	       (test 'relative 'split-path base2)
	       (test #t 'split-path dir?2)
	       (for-each 
		(lambda (root)
		  (let ([bigpath (build-path root path)])
		    (let*-values ([(base name dir?) (split-path bigpath)]
				  [(base2 name2 dir?2) (split-path base)]
				  [(base3 name3 dir?3) (split-path base2)])
		       (test #f 'split-path base3)
		       (test #t 'split-path dir?3))))
		roots)))
	  a/b))])
  (check-a/b a/b #f)
  (check-a/b a/b/ #t))

(arity-test split-path 1 1)

(arity-test path->complete-path 1 2)
(error-test '(path->complete-path 1))
(error-test '(path->complete-path "a" 1))

(map
 (lambda (f)
   (error-test `(,f (string #\a #\nul #\b)) exn:i/o:filesystem:path?))
 '(build-path split-path file-exists? directory-exists?
	      delete-file directory-list make-directory delete-directory
	      file-modify-seconds file-or-directory-permissions 
	      expand-path resolve-path path->complete-path
	      open-input-file open-output-file))
(map 
 (lambda (f)
   (error-test `(,f (string #\a #\nul #\b) "a") exn:i/o:filesystem:path?)
   (error-test `(,f "a" (string #\a #\nul #\b)) exn:i/o:filesystem:path?))
 '(rename-file path->complete-path))

; normal-case-path doesn't check for pathness:
(test #t string? (normal-case-path (string #\a #\nul #\b)))

(report-errs)
