#!/bin/sh -f
string=? ; if [ "$PLTHOME" = "" ] ; then
string=? ;  echo Please define PLTHOME 
string=? ;  exit -1
string=? ; fi
string=? ; exec ${PLTHOME}/bin/mzscheme -qr $0 "$@"

;;; This program attempts to compile and link mzrl.c.
;;; See doc.txt for more information.

(define mach-id (string->symbol (system-library-subpath)))

;; Is the readline library in /usr/local/gnu ?

;; We look for the readline library and includes in the 
;;  following places:
(define search-path
  (list "/usr"
	"/usr/local/gnu"
	;; Hack for the author's convenience:
	(format "/home/mflatt/proj/readline-2.1/~a" mach-id)))

(define rl-path
  (ormap (lambda (x)
	   (and (directory-exists? (build-path x "include" "readline"))
		(or (file-exists? (build-path x "lib" "libreadline.a"))
		    (file-exists? (build-path x "lib" "libreadline.so")))
		x))
	 search-path))

(unless rl-path
  (error 'readline-installer
	 "can't find readline include files and/or library; try editing `search-path' in mzmake.ss"))

(require-library "make.ss" "make")
(require-library "link.ss" "dynext")
(require-library "compile.ss" "dynext")
(require-library "file.ss" "dynext")

(require-library "file.ss")
(require-library "functio.ss")

(make-print-checking #f)

;; Used as make dependencies:
(define header (build-path (collection-path "mzscheme" "include") "scheme.h"))
(define version-header (build-path (collection-path "mzscheme" "include") "schvers.h"))

(define dir (build-path "compiled" "native" (system-library-subpath)))
(define mzrl.so (build-path dir (append-extension-suffix "mzrl")))
(define mzrl.o (build-path dir (append-object-suffix "mzrl")))

;; Function used to add a command-line flag:
(define (add-flags fp flags)
  (fp (append (fp) flags)))

;; Add -I to compiler command-line
(add-flags current-extension-compiler-flags
	   (list (format "-I~a/include" rl-path)))

;; More platform-specific compiler flags.
(case mach-id
  [(rs6k-aix)
   (add-flags current-extension-compiler-flags
	      (list "-DNEEDS_SELECT_H"))]
  [else (void)])

;; If we don't have a .so file, we need to make the linker
;;   use the whole archive:
(when (not (file-exists? (build-path rl-path "lib" "libreadline.so")))
  (case mach-id
    [(sparc-solaris i386-solaris)
     (add-flags current-extension-linker-flags
		(list "-u" "rl_readline_name"))]
    [(i386-linux i386-freebsd)
     (add-flags current-extension-linker-flags
		(list "--whole-archive"))]
    [else (fpritnf (current-error-port)
		   "mzmake.ss Warning: trying to use .a library, but don't know how to force inclusion;~
                  ~n   result may have undefined references~n")]))

;; Add -L and -l for readline:
(add-flags current-extension-linker-flags 
	   (list (format "-L~a/lib" rl-path)
		 "-lreadline"))

; More platform-specific linker flags.
(case mach-id
  [(sparc-solaris i386-solaris)
   (add-flags current-extension-linker-flags
	      (list "-ltermcap"))]
  [(rs6k-aix)
   (add-flags current-extension-linker-flags 
	      (list "-lc"))]
  [else (void)])

;; Add the -lcurses flag:
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
