#! /bin/sh

string=? ; exec mzscheme -f "$0" "$@"

(load-relative "file-utils.ss")

;; Tools

;; gen-external : String -> (String* -> Boolean)
(define (gen-external name)
  (let ([name
         (if (absolute-path? name)
             name
             (find-executable name))])
  (lambda args
    (printf "~a " name)
    (for-each (lambda (arg) (printf "~a " arg)) args)
    (printf "~n~n")
    (apply system* name args))))

(define compile-c (gen-external "gcc"))
(define ld (gen-external "ld"))
(define chmod (gen-external "chmod"))
(define compile-java (gen-external "javac"))
(define jar (gen-external "jar"))

(define install-dir (collection-path "drjava"))
(define my-libs (build-path install-dir "compiled" "native" (system-library-subpath)))

(define mzscheme-I
  (list "-I" (build-path (collection-path "mzscheme") "include")))

;; jar-path : String -> String
(define (jar-path jar)
  (build-path (collection-path "drjava") "jars" jar))

(define drjava-jar (jar-path "DrJava.jar"))
(define jars (list (jar-path "gjc.jar") drjava-jar))

;;;;;;; begin jvm-dependent

;;;; begin kaffe stuff
(define kaffe-base "/home/ptg/.bin/kaffe-7-22-1999AD")
(define kaffe-I-flags
  (list* "-I" (build-path kaffe-base "include" "kaffe") mzscheme-I))
(define kaffe-libs (build-path kaffe-base "lib"))

(define kaffe-so-path
  (path-list->path-list-string
   (list
    my-libs
    (build-path kaffe-libs "kaffe")
    kaffe-libs
    (build-path "/usr" "site" "gcc-2.8.1" "lib" "gcc-lib" "sparc-sun-solaris2.5.1" "2.8.1"))))

(define kaffe-c-flags (cons (format "-DKAFFE=~s" kaffe-base) kaffe-I-flags))
(define kaffe-ld-flags '("-lkaffevm" "-lsocket" "-lc" "-lnative" "-lgcc" "-lio"))

(define kaffe-classpath
  (path-list->path-list-string
    (cons (jar-path "Klasses.jar") jars)))

;;;; end kaffe stuff

;;;; begin reference 1.2.1 stuff

(define ref-base "/home/ptg/jdk1.2.1")
(define ref-jdk-libs (build-path ref-base "jre" "lib" "sparc"))
(define ref-so-path
  (path-list->path-list-string
   (list
    my-libs
    ref-jdk-libs
    (build-path ref-jdk-libs "native_threads")
    (build-path ref-jdk-libs "classic"))))

(define ref-c-flags
  (let ([include (build-path ref-base "include")])
    (list* "-I" include "-I" (build-path include "solaris") mzscheme-I)))

(define ref-ld-flags '("-ljava" "-ljvm" "-lthread" "-lc"))
(define ref-classpath
  (path-list->path-list-string
   (let ([lib (build-path ref-base "jre" "lib")])
     (list* (build-path lib "rt.jar")
	    (build-path lib "i18n.jar")
	    jars))))

;;;; end reference 1.2.1 stuff

;; select jvm here  (Yes, this should use units.)
;(define so-path kaffe-so-path)
;(define c-flags kaffe-c-flags)
;(define ld-flags kaffe-ld-flags)
;(define classpath kaffe-classpath)

(define so-path ref-so-path)
(define c-flags ref-c-flags)
(define ld-flags ref-ld-flags)
(define classpath ref-classpath)
;;;;;;; end jvm-dependent

(define mzdyn-dot-o
  (build-path (collection-path "mzscheme") "lib" (system-library-subpath) "mzdyn.o"))


(define tmp-dir "classes")

; Targets and Sources
(define mzjvm "libmzjvm.so")
(define embed-obj "hello.o")
(define embed-src "hello.c")

(define scheme-val-obj "SchemeValue.o")
(define scheme-val "libSchemeValue.so")
(define scheme-val-src "SchemeValue.c")

(define objs (list embed-obj scheme-val-obj))
(define sos (list mzjvm scheme-val))

(define launcher-name (build-path (collection-path "drjava") 'up 'up "bin" "drjava"))

;; build : -> Void
(define (build)
  (clobber)
  (build-sos)
  (build-classfiles))

;; install assumes build has run
;; install : -> Void
(define (install)
  (unless (directory-exists? my-libs)
    (make-directory* my-libs))
  (for-each (lambda (so) (clobber-file so (build-path my-libs so))) sos)
  (parameterize ([current-directory tmp-dir])
    (apply jar "-0cvf"
	   drjava-jar
	   (directory-list)))
  (install-launcher))

;; build-classfiles : -> Void
(define (build-classfiles)
  (make-directory tmp-dir)
  (let ([src-dir (build-path "java" "edu" "rice" "cs" "drj")])
    (apply compile-java "-d" "classes"
	   (filter file-exists?
		   (map (lambda (f) (build-path src-dir f))
			(directory-list src-dir))))))

;; build-sos : -> Void
(define (build-sos)
  (load-relative "gen-wrappers.ss")
  (build-so embed-src embed-obj "-o" mzjvm)
  (build-so scheme-val-src scheme-val-obj "-lmzjvm" "-o" scheme-val))

;; build-so : String String (listof String) -> Void
(define (build-so src obj . rest)
  (apply compile-c (list* "-c" src c-flags))
  (apply ld
	 (list* "-G" obj mzdyn-dot-o "-R" so-path "-Y"
		(string-append "P,.:/usr/lib:" so-path)
		(append ld-flags rest))))

;; nuke : -> Void
(define (nuke) (uninstall) (clobber))

;; clean : -> Void
(define (clean)
  (for-each delete-file-maybe
	    (list* "generated.c" "SchemeValue-gen.c" objs)))

;; clobber : -> Void
(define (clobber)
  (clean)
  (for-each delete-file-maybe sos)
  (delete-directory*-maybe tmp-dir))

;; uninstall : -> Void
(define (uninstall)
  (for-each delete-file-maybe
	    (list* drjava-jar launcher-name
		   (map (lambda (so) (build-path my-libs so)) sos)))
  (delete-directory*-maybe (build-path (collection-path "drjava") "compiled")))
  
;; install-launcher
(define (install-launcher)
  (let ([out (open-output-file launcher-name 'truncate)])
    (fprintf out
"#! /bin/sh

string=? ; LD_LIBRARY_PATH=~a export LD_LIBRARY_PATH ; exec $PLTHOME/bin/mred -g -q -l core.ss -v -m -f \"$0\" -e '(yield (make-semaphore))' < /dev/null > /dev/null 2>&1

(define libpath (getenv \"LD_LIBRARY_PATH\"))

(define classpath ~s)

(putenv \"CLASSPATH\" classpath)
(putenv \"GJC_PATH\" classpath)

(require-library \"start.ss\" \"drjava\")
"
     so-path classpath)
    (close-output-port out))
  (chmod "755" launcher-name))
