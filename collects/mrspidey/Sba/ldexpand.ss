; ldexpand.ss - loads and macro expands source files
; ----------------------------------------------------------------------
; Copyright (C) 1995-97 Cormac Flanagan
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; version 2 as published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
; ----------------------------------------------------------------------

(define (open-code-file filename)
  (let ([filename (normalize-path filename)]
         [cd (current-directory)])
    (dynamic-let 
      ([current-directory (path-only filename)])
      (unless (file-exists? filename)
        (mrspidey:error (format "Can't open file ~s, current directory ~s"
                          filename (current-directory))))
      (let* ( [p (open-input-file filename 'text)]
              [p (system-expand-if-necy p)])
        p))))

(define (zodiac:read* port filename)
  (let* ( [default-loc (zodiac:make-location 1 1 0 filename)]                      
            
         [reader (if (st:fake-reader)
                     (fake-zodiac-reader port default-loc)
                     (zodiac:read port default-loc))]
         [sexps
          (recur loop ()
            (let* ([expr (reader)])
              (if (or (zodiac:eof? expr) (eof-object? expr))
                  '()
                  (begin
                    (when (zodiac:zodiac? expr)
                      (mrspidey:zprogress "Reading" 
                                          (zodiac:zodiac-start expr)))
                    (cons expr (loop))))))])
    (unless (null? sexps)
      (mrspidey:zprogress "Reading" (zodiac:zodiac-finish (rac sexps))))
    (close-input-port port)
    (when debugging-front
      (printf "~n--Loaded file---------------------~n")
      (for-each (lambda (sexp) (pretty-print (zodiac:stripper sexp))) sexps)
      (printf "----------------------------------~n"))
    sexps))

(define mrspidey:zprogress
  (let ( [cur-phase '()]
         [cur-line -100]
         [cur-file ""])
    (lambda (phase loc)
      (let ( [file (zodiac:location-file loc)]
             [line (zodiac:location-line loc)])
        (unless (and 
                  (equal? file cur-file)
                  (eq? phase cur-phase)
                  ;;(>= line cur-line)
                  (< line (+ cur-line 10)))
          (set! cur-phase phase)
          (set! cur-line line)
          (set! cur-file file)
          (mrspidey:progress
            (format "~a ~a: "
              (padr phase 10)
              (file-name-from-path file))
            line))))))

; ----------------------------------------------------------------------

(define fake-zodiac-reader
  (case-lambda
   [(p) (fake-zodiac-reader p (zodiac:make-location 1 1 0 "filename"))]
   [(p loc)
    (lambda ()
      (let ([x (read p)])
        (recur loop ([x x])
          (match x
            [(? string?)  (zodiac:make-string loc loc loc x)]
            [(? boolean?) (zodiac:make-boolean loc loc loc x)]
            [(? number?)  (zodiac:make-number loc loc loc x)]
            [(? symbol?)  (zodiac:make-symbol loc loc loc x x x)]
            [(? char?)    (zodiac:make-char loc loc loc x)]
            [(? vector?)  (zodiac:make-vector loc loc loc
                                              (map loop (vector->list x))
                                              (vector-length x))]
            [(? list?)    (zodiac:make-list loc loc loc
                                            (map loop x)
                                            (length x)
                                            'marks)]
            [(? pair?)    (zodiac:make-improper-list 
                           loc loc loc
                           (recur loop2 ([x x])
                             (match x
                               [(a . d) (cons (loop a) (loop2 d))]
                               [r (list (loop r))]))
                           0
                           'period 'marks)]
            [_ (if (eof-object? x)
                   (zodiac:make-eof loc)
                   (mrspidey:internal-error 
                    'fake-zodiac-reader
                    "Bad object ~s" x))]))))]))

; ----------------------------------------------------------------------

(define (expand-zexp->port exp)
  (let* ( [exp (zodiac:stripper exp)]
          [s (format "~s" exp)]
          [p (open-input-string s)]
          [p (system-expand-if-necy p)])
    p))

; ----------------------------------------------------------------------

(define (system-expand-if-necy p)
  (if (st:system-expand)
      (system-expand-port p)
      p))

(define system-macros
  '(;; --- r4rs
    case cond do recur rec let* letrec let and or define
    ;; --- Chez things
         parameterize fluid-let case-lambda let-values #%let-values
    ;; --- Rice things
    match match-lambda match-lambda* match-let match-let*
    ;; --- My things
    define-module global assert for 
    ;; --- Misc
    defmacro
    ;; Units w/ signatures
    define-signature 
    unit-with-signature unit/sig 
    compound-unit/sig compound-unit-with-signature
    invoke-unit/sig invoke-unit-with-signature
    unit->unit/sig
    ))


(define (expander-eval e)
  (parameterize ([current-namespace expander-namespace])
                (eval e)))
(when (st:system-expand)
  (expander-eval '(load "/home/cormac/Spidey/Code/Sba/expander-boot.ss")))
(define (my-expand e) (expander-eval `(expand-defmacro ',e)))
(define (my-expand-once e) (expander-eval `(expand-defmacro-once ',e)))

(define unchanged-list
  '( define-constructor
     define-type
     primitive:
     type:))

(define system-expanded-exp (void))

(define (system-expand-exp exp)
  (set! system-expanded-exp exp)
  (match exp
    [((? (lambda (x) (memq x unchanged-list))) . _) 
     exp]
    [_ (match (my-expand exp)
         [(and e ((or '#%define-expansion-time '#%define-macro) . _))
          (expander-eval e)
          '(void)]
         [e e])]))

(define (remove-signature-stuff e)
  (recur loop ([e e])
    (match e
      [('#%make-unit-with-signature x _ _) (loop x)]
      [('#%verify-signed-compound-sub-units . _) '(void)]
      [('#%verify-linkage-signature-match . _) '(void)]
      [('#%unit-with-signature-unit x) x]
      [('#%invoke-open-unit expr name-specifier . imports)
        `(#%invoke-unit ,(loop expr) ,@imports)]
      [('#%invoke-open-unit expr)
        `(#%invoke-unit ,(loop expr))]
      [(('#%global-defined-value ('#%quote match:error)) . args)
        '(error 'match "Match error")]
      [(a . d) (cons (loop a) (loop d))]
      [x x])))

(define (system-expand-port p)
  (pretty-debug `(system-expand-port ,p ,(current-directory)))
  (let* ([o (open-output-string)])
    (parameterize 
      ([pretty-print-depth #f])
      (recur loop ([p p])
        (let ([e (read p)])
          (printf ".") (flush-output)
          (unless (eof-object? e)
            (recur process ([e e])
              (match (system-expand-exp e)
                [('#%begin . e*)
                  (for-each process e*)]
                [e
                  (let* ([e (remove-signature-stuff e)])
                    (match e
                      [((or 'load '#%load '#%load/cd 'load/cd) exp)
                        (let ([filename (normalize-path (expander-eval exp))])
                          (unless (file-exists? filename)
                            (mrspidey:error (format "Can't load ~s" filename)))
                          (dynamic-let 
                            ([current-directory 
                               (if (memq (car e) '(load/cd #%load/cd))
                                 (path-only filename)
                                 (current-directory))])
                            (let* ([p (open-input-file filename 'text)])
                              (printf "[File:~s " (file-name-from-path filename))
                              (loop p)
                              (printf "done]")
                              (close-input-port p))))]
                      [('load-recent s) 
                        (process `(load ,(string-append s ".ss")))]
                      [(or '(void) (? void?)) (void)]
                      [e (pretty-print (strip-hash-percent e) o)]))]))
            (loop p)))))
    (begin0
      (open-input-string (get-output-string o))
      (close-output-port o)
      (close-input-port p))))

(define (strip-hash-percent expr)
  (recur loop ([expr expr])
    (match expr
      [('|#primitive| prim) prim]
      [(a . d) (cons (loop a) (loop d))]
      [(? symbol? x)
       (let* ([s (symbol->string x)]
              [l (string-length s)])
         (if (and (> l 2)
                  (string=? (substring s 0 2) "#%"))
             (string->symbol (substring s 2 l))
             x))]
      [x x])))

; ----------------------------------------------------------------------

(define expand-file
  (case-lambda
    [(infile)
      (let ([infile (normalize-path infile)])
        (let ([outfile (regexp-replace ".ss$" infile ".expanded")])
          (when (eq? outfile infile)  
            (error 'expand-file "Bad suffix on ~s" infile))
          (expand-file infile outfile)))]
    [(infile outfile)
      (when (file-exists? outfile) (delete-file outfile))
      (dynamic-let ( [st:system-expand #t]
                     [current-directory (path-only  (normalize-path infile))])
        (let* ( [p (open-input-file infile 'text)]
                [p2 (system-expand-if-necy p)]
                [p3 (open-output-file outfile 'text)])
          (printf "~nCopying:")
          (recur loop ()
            (let ([e (read p2)])
              (unless (eof-object? e)
                (pretty-print e p3)
                (printf ".") (flush-output)
                (loop))))
          (newline)
          (close-input-port p2)
          (close-output-port p3)
          outfile))]))

; ----------------------------------------------------------------------

(define file-time-cache '())

(define (clear-file-time-cache!)
  (set! file-time-cache '()))

(define (extend-file-time-cache! file n)
  (set! file-time-cache (cons (cons file n) file-time-cache)))

(define (file-time file)
  (or (lookup-or-#f file-time-cache file)
      (let* ([n (file-modify-seconds file)])
        (unless (number? n) 
          (error 'file-time "file-modify-seconds failed on ~a" file))
        (extend-file-time-cache! file n)
        n)))

(define (zodiac-time x)
  (let* ([start (zodiac:zodiac-start x)]
         [file (zodiac:location-file start)])
    (file-time file)))

(define (zodiac-time* x)
  ;; ## should be either current time, or file time of any imported file
  ;; is in right directory
  (let* ([t (zodiac-time x)]
          [fn (lambda (exp cl-fn)
                (match exp
                  [($ zodiac:reference-unit-form _ _ _ _ file cd)
                    (let*-vals
                      ( [_ (unless (zodiac:string? file)
                             (mrspidey:error
                               (format "reference-unit requires a string argument, given ~s" file)))]
                        [file (zodiac:read-object file)]
                        [file (if (relative-path? file)
                                (build-path cd file)
                                file)])
                      (when (file-exists? file)
                        (pretty-debug `(zodiac:time* includes ,file))
                        (set! t (max t (file-time file))))
                      #f)]
                  [_ #f]))])
    ((zodiac:compat fn) x)
    t))

;; ======================================================================

(define attributes #f)
(define expander-namespace #f)

(define (init-expand!)
  (set! attributes (zodiac:make-attributes))
  (set! expander-namespace (make-expander-namespace)))

;; ----------------------------------------------------------------------

(define g:prog #f)

(define (my-scheme-expand-program defs)
  ;;(printf "my-scheme-expand-program cd=~s~n" (cd))
  (let* ( [p (make-parameterization)]
          [_ (with-parameterization p
               (lambda () 
                 (current-namespace 
                   expander-namespace
                                        ;(make-expander-namespace)
                   ) 
                 (reference-library "core.ss")
                 (reference-library "macro.ss")
                 '(reference 
                    (begin-elaboration-time
                      (build-path
                        mred:plt-home-directory "mred" "system" "sig.ss")))
                 '(eval '(unit/sig () (import  mred^) 1))
                 ;;(printf "np=~s~n" normalize-path)
                 ))]
          ;          [defs2 (zodiac:expand-program
          ;                   defs attributes zodiac:mrspidey-vocabulary p)]
          [defs2 (call/nal
                   zodiac:expand-program/nal
                   zodiac:expand-program
                   (expressions: defs)
                   (attributes: attributes)
                   (vocabulary:  zodiac:mrspidey-vocabulary))]
                   ; (parameterization: p))]
          [defs2 (zodiac:inline-begins defs2)]
          [_ (zodiac:initialize-mutated defs2)]
          [free (zodiac:free-vars-defs defs2)])
    (set! g:prog defs2)
    ;;(pretty-print defs2)
    '(when debugging
       (pretty-print (map zodiac:stripper defs2)))
    (values defs2 free)))

;; ----------------------------------------------------------------------

