#!/bin/sh -f
string=? ; if [ "$PLTHOME" = "" ] ; then
string=? ;  echo Please define PLTHOME 
string=? ;  exit -1
string=? ; fi
string=? ; exec ${PLTHOME}/bin/mzscheme -qr $0 "$@"

(require-library "make.ss" "make")
(require-library "link.ss" "dynext")
(require-library "compile.ss" "dynext")
(require-library "file.ss" "dynext")
(require-library "file.ss")

(require-library "functio.ss")

(define header (build-path (collection-path "mzscheme" "include") "scheme.h"))
(define version-header (build-path (collection-path "mzscheme" "include") "schvers.h"))

(define dir (build-path "compiled" "native" (system-library-subpath)))
(define mzrl.so (build-path dir "mzrl.so"))
(define mzrl.o (build-path dir "mzrl.o"))

(define (add-flags fp flags)
  (fp (append (fp) flags)))

(define (files dir regexp)
  (let loop ([l (directory-list dir)])
    (cond
     [(null? l) null]
     [(regexp-match regexp (car l)) (cons (build-path dir (car l))
					  (loop (cdr l)))]
     [else (cdr l)])))

(define mach-id (string->symbol (system-library-subpath)))

(when (and (eq? mach-id 'i386-linux)
	   (file-exists? "/usr/lib/libreadline.so"))
      (set! mach-id 'i386-linux/readline))

(define readline-in-/usr/local/gnu?
  (directory-exists? "/usr/local/gnu/include/readline"))

; Compiler flags
(case mach-id
  [(sparc-solaris i386-solaris i386-linux)
   (add-flags current-extension-compiler-flags
	      (list "-I/home/mflatt/proj/readline-2.1"))]
  [(rs6k-aix)
   (add-flags current-extension-compiler-flags
	      (cons "-DNEEDS_SELECT_H"
		    (if readline-in-/usr/local/gnu?
			(list "-I/usr/local/gnu/include")
			null)))]
  [else (void)])

; Linker flags
(case mach-id
  [(sparc-solaris i386-solaris)
   (add-flags current-extension-linker-flags
	      (list "-ltermcap"
		    "-u" "rl_readline_name"
		    (format "/home/mflatt/proj/readline-2.1/~asolaris/libreadline.a"
			    (if (eq? mach-id 'i386-solaris)
				"i386-"
				""))))]
  [(i386-linux)
   (add-flags current-extension-linker-flags
	      (list "--whole-archive"
		    "-L/home/mflatt/proj/readline-2.1/linux/"
		    "-lreadline"))]
  [else (add-flags current-extension-linker-flags 
		   (list (if readline-in-/usr/local/gnu?
			     "-L/usr/local/gnu/lib"
			     "-L/usr/local/lib")
			 "-lreadline"))
	(when (eq? mach-id 'rs6k-aix)
	      (add-flags current-extension-linker-flags 
			 (list "-lc")))])

(add-flags current-extension-linker-flags (list "-lcurses"))

(define (delete/continue x)
  (with-handlers ([(lambda (x) #t) void])
    (delete-file x)))

(make 
 ((mzrl.so (mzrl.o dir)
	   (link-extension #f (list mzrl.o) mzrl.so))
  
  (mzrl.o ("mzrl.c" header version-header dir)
	  (compile-extension #f "mzrl.c" mzrl.o ()))

  ("clean" () (begin (delete/continue mzrl.o) (delete/continue mzrl.so)))
  
  (dir ()
       (make-directory* dir)))

 argv)
