;; za.ss
;; Reads and writes .za files
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

;; ======================================================================
;; za handling stuff
;; ======================================================================

;; ----------------------------------------------------------------------
;; read-unit-za
;; Returns (values ftype tref-env tdef-env tbang-env)

(define (read-za za)
  (let*-vals
   ([(Tvar envs lookup-Tvar) (read-constraint-set za)]
    [cvt-env (match-lambda
              [(sym num) (cons sym (lookup-Tvar num))])])
   (match envs
     [('ftype sexp
              'tref-env tref-env
              'tdef-env tdef-env
              'tbang-env tbang-env)
      (values
       (recur sexp->ftype ([sexp sexp])
         ;;(pretty-print `(sexp->ftype ,sexp))
         (match sexp
           [(? number? n) (lookup-Tvar n)]
           [('schema n n*)
            (create-fo-FlowType
             (make-schema (lookup-Tvar n)
                          (map lookup-Tvar n*)
                          '()))]
           [('atunit imports exports result)
            (create-fo-FlowType
             (make-atunit (map (match-lambda
                                [(sym . n*)
                                 (cons sym (map lookup-Tvar n*))])
                               imports)
                          (map (match-lambda
                                [(sym sexp)
                                 (cons sym (sexp->ftype sexp))])
                               exports)
                          (sexp->ftype result)
                          #f))]
           [('atstruct A B C D E F)
             (create-fo-FlowType (make-atstruct A B C D E F))]
           [('prim sym type attrs)
             (create-fo-FlowType (apply primitive->atprim sym type attrs))]
           [x (mrspidey:internal-error 'read-za 
                "Bad ftype-sexp in za: ~s" x)]))
       (map cvt-env tref-env)
       (map cvt-env tdef-env)
       (map cvt-env tbang-env))])))

;; ----------------------------------------------------------------------
;; write-za

(define (write-za za tvar-live ftype tref-env tdef-env tbang-env)
  (write-constraint-set 
   za tvar-live
   (lambda (Tvar->nu-num)
     (let ([cvt-env
            (match-lambda
             [(sym . tvar) (list sym (Tvar->nu-num tvar))])])
       (list 
        'ftype
        (recur ftype->sexp ([ftype ftype])
          (pretty-debug-unit `(ftype->sexp ,(FlowType->pretty ftype)))
          (if (Tvar? ftype)
              (Tvar->nu-num ftype)
              (match (fo-FlowType-def ftype)
                [($ schema tvar tvar* '()) 
                 (list 'schema
                       (Tvar->nu-num tvar) 
                       (map Tvar->nu-num tvar*))]
                [($ atunit imports exports result exp)
                 (list 'atunit
                       (map (match-lambda
                             [(sym . tvar*)
                              (cons sym (map Tvar->nu-num tvar*))])
                            imports)
                       (map (match-lambda
                             [(sym . ftype)
                              (list sym (ftype->sexp ftype))])
                            exports)
                       (ftype->sexp result))]
                [($ atstruct A B C D E F) `(atstruct ,A ,B ,C ,D ,E ,F)]
                [($ atprim sym type _ _ attrs orig-type-schema)
                  (list 'prim sym orig-type-schema attrs)]
                [x (mrspidey:internal-error 'ftype->sexp "Bad Atype ~s" x)])))

        'tref-env (map cvt-env tref-env)
        'tdef-env (map cvt-env tdef-env)      
        'tbang-env (map cvt-env tbang-env))))))

;; ======================================================================

(define (write-constraint-set file Tvar-live mk-envs)
  
  (with-handlers
    ([void 
       (lambda (exn) 
         (when (file-exists? file) (delete-file file))
         (raise exn))])

    (let*-vals
      ( [s (format "Saving ~a" (file-name-from-path file))]
        [_ (mrspidey:progress s '...)]
        [_ (when (file-exists? file) (delete-file file))]
        [p (open-output-file file 'text)]
        [disp (lambda (x) (write x p) (newline p))]
        [num-Tvar-live (length Tvar-live)]
        [(get-Tvar-ndx set-Tvar-ndx!) (alloc-Tvar-field)]
        [Tvar-ndx
          (lambda (tvar)
            (let ([ndx (get-Tvar-ndx tvar)])
              '(unless (number? ndx)
                 (error 'write-constraint-set::Tvar-ndx
                   "Tvar ~s not live ~s"
                   (Tvar-name tvar) (map Tvar-name Tvar-live)))
              ndx))]
        [(AV-ndx set-AV-ndx!) (alloc-AV-field)]
        [num-nu-AV 0])

      ;; --- assign number to each live Tvar
      (for-each-with-n
        (lambda (Tvar n)
          (assert (eq? (Tvar-ndx Tvar) #f))
          (set-Tvar-ndx! Tvar n))
        Tvar-live)

      ;; --- write version
      (disp `(st:version ,(st:version)))

      ;; --- write configuration
      (pretty-print (mrspidey:control-fn) p)

      ;; --- write num Tvars
      (disp `(num-Tvar ,num-Tvar-live))

      ;; --- write constructor-env
      (disp `START-constructor-env)
      (hash-table-for-each
        constructor-env
        (lambda (sym template)
          (unless 
            (hash-table-get saved-constructor-env sym (lambda () #f))
            (match template
              [($ template type n+ n- ref assign super-templates)
                (disp (list type n+ n- ref assign 
                        (map template-type super-templates)))]))))
        ;; reverse it so defns in file occur before references ???
      (disp 'END-constructor-env)

      ;; --- write AV
      (disp `START-AV)
      (for-each
        (lambda (Tvar)
          ;;(pretty-print (Tvar-name Tvar))
          (for-each
            (match-lambda 
              [(and AV ($ AV num ($ template type) misc fields+ fields-))
                (unless (AV-ndx AV)
                  (set-AV-ndx! AV num-nu-AV)
                  (set! num-nu-AV (add1 num-nu-AV))
                  (disp (list type
                          (if (or (number? misc) (char? misc) (symbol? misc)
                                (pair? misc))
                            misc
                            '())
                          (map Tvar-ndx (vector->list fields+))
                          (map Tvar-ndx (vector->list fields-)))))])
            (Tvar-objs Tvar)))
        Tvar-live)
      (disp 'END-AV)

      ;; --- write Tvar
      (disp 'START-Tvar)
      (for-each
        (lambda (Tvar)
          (disp 
            (append (map AV-ndx (Tvar-objs Tvar))
              (map 
                (match-lambda
                  [($ con _ ($ template type) field-no Tvar sign)
                    (list type field-no (Tvar-ndx Tvar) sign)]
                  [($ con-filter _ ($ filter sign (($ template types) ...)) 
                     Tvar)
                    (list sign types (Tvar-ndx Tvar))])
                (Tvar-constraints Tvar))
              (let ([edgeto (Tvar-edgeto Tvar)])
                (if (null? edgeto)
                  '()
                  (cons 'T (map Tvar-ndx edgeto)))))))
        Tvar-live)
      (disp 'END-Tvar)

      ;; --- write in-env, out-env
      (let ([rep-envs (mk-envs Tvar-ndx)])
        (pretty-print `(envs  ,@rep-envs)  p)
        ;; --- all done
        (close-output-port p))
      (mrspidey:progress s 'done))))

;; ----------------------------------------------------------------------

(define (read-constraint-set file)
  (match-let*
    ([p (open-input-file file 'text)]
      [('st:version save-st:version) (read p)]
      [configuration (read p)]
      [('num-Tvar save-num-Tvar) (read p)]
      [vec-Tvar (make-vector save-num-Tvar #f)]
      [vec-AV  (void)]
      [lookup-Tvar
        (lambda (n)
          (if (number? n)
            (vector-ref vec-Tvar n)
            (let ([Tvar (mk-Tvar 'load-empty)])
              Tvar)))]
      [lookup-AV
        (lambda (n)
          (let ([AV (vector-ref vec-AV n)])
            (assert (AV? AV) 'lookup-AV)
            AV))])

    (unless (equal? save-st:version (st:version))
      (mrspidey:error ".za file from previous MrSpidey release"))

    (for i 0 save-num-Tvar (vector-set! vec-Tvar i (mk-Tvar 'load)))

    ;; --- constructors

    (match (read p) ['START-constructor-env (void)])
    (recur loop ()
      (match (read p)
        [(type n+ n- ref assign super)
          (let ([t (make-template type n+ n- ref assign '() eqv?)])
            (for-each
              (lambda (s) (record-super-constructor-of-template! s t))
              (reverse super))
            (extend-constructor-env! t)
            (loop))]
        ['END-constructor-env (void)]))

    ;; --- AV's

    (match (read p) ['START-AV (void)])
    (set! vec-AV
      (list->vector
        (recur loop ()
          (match (read p)
            [(C misc tvar-num+* tvar-num-*)
              (cons 
                (create-AV (lookup-template C)
                  misc
                  (list->vector (map lookup-Tvar tvar-num+*))
                  (list->vector (map lookup-Tvar tvar-num-*)))
                (loop))]
            ['END-AV '()]))))

    ;; --- Tvar's

    (match (read p) ['START-Tvar (void)])
    (let ([new-AV!  add-AV!]
           [new-con! add-con!]
           [new-edge! add-edge!])
      (recur loop ([n 0])
        (match (read p)
          ['END-Tvar (void)]
          [in 
            (let ([Tvar (lookup-Tvar n)])
              (recur loop2 ([in in])
                (match in
                  [() (void)]
                  [((? integer? n) . rest)
                    (new-AV! Tvar (lookup-AV n))
                    (loop2 rest)]
                  [(((? boolean? sign) C* tvar-num) . rest)
                    (new-con! 
                      Tvar
                      (create-con-filter
                        (create-filter sign (map lookup-template C*))
                        (lookup-Tvar tvar-num)))
                    (loop2 rest)]
                  [((C n tvar-num sign) . rest)
                    (new-con! 
                      Tvar
                      (create-con 
                        (lookup-template C) n 
                        (lookup-Tvar tvar-num) sign))
                    (loop2 rest)]
                  [('T . edgeto*)
                    (for-each
                      (lambda (tvar-num)
                        (new-edge! Tvar (lookup-Tvar tvar-num)))
                      edgeto*)])))
            (loop (add1 n))])))

    ;; --- in-env and out-env

    (match-let*
      ([('envs  . envs)  (read p)])

      (close-input-port p)
      
      (values
        (vector->list vec-Tvar)
        envs
        lookup-Tvar))))

;; ----------------------------------------------------------------------

