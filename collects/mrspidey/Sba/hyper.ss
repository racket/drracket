; hyper.ss - interface to GUI
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
;;----------------------------------------------------------------------

(define (st:analyze-and-make-annotations filename)
  (analysis-set-arrow-filter! #f)
  (record-analyzed-file
   filename
   (lambda () (open-code-file filename))
   (lambda () (analyze-program filename))))

(define (calc-annotations defs)
  (match-let*
      ([type-annotations (calc-type-annotations defs)]
       [(check-annotations uncheck-annotations) (calc-checks defs)]
       [all-annotations 
        (vector type-annotations check-annotations uncheck-annotations)])
    ;;(pretty-debug `(links ,all-annotations))
    all-annotations))
	          
;;----------------------------------------------------------------------
    
(define arrow-filter #f)          ; if not #f, then a list of templates
; only show pred with matching AV

(define (analysis-set-arrow-filter! C)
  (set! arrow-filter
      (if C 
          (map lookup-template (list C))
          #f)))

(define (analysis-filter-on?)
  (and arrow-filter (symbol->string (template-type (car arrow-filter)))))

(define (analysis-get-filters)
  (cons 
    (cons "No filter" #f)
    (map 
      (lambda (C) (cons (symbol->string C) C))
      (append
        `( nil num sym str void undefined true false 
           box cons vec eof iport oport
           promise unit thread hash-table regexp parameterization semaphore)
        (filter
          (lambda (x) x)
          (hash-table-map
            constructor-env
            (lambda (c template)
              (and
                (memq template-structure (template-super-templates template))
                c))))))))

;; ------------------------------

(define (objs-contain-filter? objs)
  (or 
    (and
      (eq? arrow-filter #f)
      (not (null? objs)))
    (ormap 
      (lambda (AV)
        (memq (AV-template AV) arrow-filter))
      objs)))

(define (tvar-contain-filter-mvalues? get-Tvar-objs tvar)
  (or
    (objs-contain-filter? (get-Tvar-objs tvar))
    (ormap
      (match-lambda
        [($ AV _ (? (eqc? template-mvalues)) _ #(tvar-mvlist))
          ;;(pretty-debug `(tvar-mvlist ,(Tvar-name tvar-mvlist)))
          (ormap
            (match-lambda
              [($ AV _ (? (eqc? template-cons)) _ #(tvar-car-mvlist _))
                (objs-contain-filter? (get-Tvar-objs tvar-car-mvlist))]
              [_ #f])
            (get-Tvar-objs tvar-mvlist))]
        [_ #f])
      (get-Tvar-objs tvar))))

(define (FlowType-contains-filter? ftype)
  ;(pretty-debug `(FlowType-contains-filter? ,(FlowType-name ftype)))
  (or
    (not arrow-filter)
    (match (FlowType->Atype ftype)
      [(or (? atconst?) (? atprim?) (? Tvar?))
        (tvar-contain-filter-mvalues? Tvar-objs (FlowType->Tvar ftype))]
      [($ atvalues (val1 . _))
        (FlowType-contains-filter? val1)]
      ;; other atypes are atschema atthunk, atstruct, atunit, atlunit
      ;; all are too expensive to convert.
      [x
        (pretty-debug  `(FlowType-contains-filter? unhandled-atype x))
        #f])))

(define (FlowType-orig-objs-contains-filter? ftype)
  ;;(pretty-debug `(FlowType-orig-contains-filter? ,(FlowType-name ftype)))
  (match (FlowType->Atype ftype)
    [(or (? Tvar?))
      (tvar-contain-filter-mvalues? Tvar-orig-objs (FlowType->Tvar ftype))]
    [($ atvalues (val1 . _))
      (FlowType-orig-objs-contains-filter? val1)]
    ;; other atypes are atschema atthunk, atstruct, atunit, atlunit
    ;; all are too expensive to convert.
    [x
      (pretty-debug  `(FlowType-orig-objs-contains-filter? unhandled-atype x))
      #f]))

;;----------------------------------------------------------------------
;; Abstracts over parents and children

(define analysis-walk1
  (lambda (start in-get-edges file-visable?)
    (assert (FlowType? start))
    (pretty-debug `(analysis-walk1 ,(FlowType-name start)))
    (if (string? (FlowType-type-annotation start))
      ;; start from .za file, show no edges
      '()
      (letrec*
        ( [traversed '()]               ; list of FlowType's
          [preds '()]                   ; visable FlowType's
          [get-edges
            (lambda (ftype)
              (let ([r (in-get-edges ftype)])
                (pretty-debug `(get-edges ,(map FlowType-name (cons ftype r))))
                r))]
          [traverse 
            (lambda (ftype)
              (pretty-debug `(traverse ,(FlowType-name ftype)))
              (when 
                (and
                  (FlowType-contains-filter? ftype)
                  (not (memq ftype traversed)))
                (set! traversed (cons ftype traversed))
                (let* 
                  ( [ftype-w/-ta (get-ftype-w/-ta ftype)]
                    [ta (and ftype-w/-ta 
                          (FlowType-type-annotation ftype-w/-ta))])
                  (pretty-debug
                    `(traverse-ftype-w/-ta 
                       ,(and ftype-w/-ta (FlowType-name ftype-w/-ta))))
                 
                  (cond
                    [(not ta)
                      ;; invisable
                       ;; Go back to parents
                      (for-each traverse (get-edges ftype))]

                    [(string? ta)
                      (if (file-visable? ta)
                        
                        ;; ftype from .za file, source file is loaded
                        ;; can follow this edge unless dest is from same file
                        (for-each
                          (lambda (ftype2)
                            (let ([ta2 (FlowType-type-annotation ftype2)])
                              (unless (and (string? ta2) (string=? ta ta2))
                                (traverse ftype2))))
                          (get-edges ftype))
                        
                        ;; .za file, source file not loaded
                        (set! preds (cons ftype-w/-ta preds)))]

                    [(and 
                       (not (string? ta))
                       (not (eq? ftype-w/-ta start)))

                      ;; visable
                      ;; add to preds unless src and dest are single value
                      (set! preds (cons ftype-w/-ta preds))]

                    [else
                      ;; Invisable
                      ;; Go back to parents
                      (for-each traverse (get-edges ftype))]))))])

        (for-each traverse (get-edges start))
               
        (pretty-debug `(traversed ,(map FlowType-name traversed)))
        (pretty-debug 
          `(preds ,(map
                     (lambda (i) (and (FlowType? i) (FlowType-name i)))
                     preds)))
        
        (remq start preds)))))

(define (get-ftype-w/-ta ftype)
  (if (FlowType-type-annotation ftype)
      ftype
      (and (FlowType-values-ftype ftype)
           (get-ftype-w/-ta (FlowType-values-ftype ftype)))))

(define (single-value-ftype ftype)
  (let ([r
          (match (FlowType->Atype ftype)
            [(? Tvar? tvar)
                (ormap
                  (lambda (AV) (not (eq? (AV-template AV) template-mvalues)))
                  (get-Tvar-objs tvar))]
            [($ atvalues) #f]
            [_ #t])])
    (pretty-debug `(single-value-ftype ,(FlowType-name ftype) ,r))
    r))

;;----------------------------------------------------------------------
;; Returns the preds in the Tvar graph of a given ftype
;; format is a list of Tvars of predecessors that are visable

(define analysis-parents
  (lambda (ftype file-visable?)
    (pretty-debug `(analysis-parents ,(FlowType-name ftype) ,(print-struct)))
    (let ([r (analysis-walk1 ftype get-arrowfrom file-visable?)])
    (pretty-debug `(analysis-parents-returns ,(map FlowType-name r)))
      r)))

(define (get-arrowfrom to)
  (append
    (if (Tvar? to)
      (append
        (get-mvalue-components to)
        (Tvar-edgefrom to))
      '())
    (match (FlowType->Atype to)
      [($ atvalues ftypes)
        ftypes]
      [_ '()])
    (FlowType-arrowfrom to)))

(define (get-mvalue-components tvar)
  '(pretty-debug `(get-mvalue-components ,(Tvar-name tvar)))
  (filter-map
    (lambda (AV)
      (if (eq? (AV-template AV) template-mvalues)
        ;; get-nth
        (let ( [p (mk-Tvar 'p)]
               [l (mk-Tvar 'l)])
          (new-edge! (vector-ref (AV-fields+ AV) 0) p)
          (new-con! p (make-con-cdr p))
          (new-con! p (make-con-car l))
          '(pretty-debug
             `(get-mvalue-components
                ,(Tvar-name tvar) ,(Tvar-name p) ,(Tvar-name l)))
          l)
        #f))
    (get-Tvar-objs tvar))) 

;; ----------------------------------------------------------------------

(define analysis-children
  (lambda (ftype file-visable?)
    (analysis-walk1
      ftype
      FlowType-alledgeto
      file-visable?)))

(define FlowType-alledgeto
  (lambda (ftype)
    (append
      ;; Direct successors
      (FlowType-arrowto ftype)
      (if (FlowType-values-ftype ftype)
        (list (FlowType-values-ftype ftype))
        '())
      ;; For filter successors, make sure some same values
      (if (Tvar? ftype)
        (append
          (Tvar-edgeto ftype)
          (filter-map
            (match-lambda
              [($ con-filter _ _ tvar) tvar]
              [_ #f])
            (Tvar-constraints ftype)))
        '()))))

;;----------------------------------------------------------------------
;; Returns all ancestor/descendant arrows of a given ftype
;; as (listof (list from to))

(define analysis-walkn
  (lambda (ftype get-edges file-visable?)
    (assert (FlowType? ftype))
    (pretty-debug `(analysis-walkn ,(FlowType-name ftype)))
    ;;(error)
    (let ( [done '()]                   ; list of FlowType's
           [arrows '()])                ; visable arrows
      (recur traverse ([ftype ftype])
        (unless (memq ftype done)
          (set! done (cons ftype done))
          (let ([parents (analysis-walk1 ftype get-edges  file-visable?)])
            (for-each 
              (lambda (p)
                (set! arrows (cons (list p ftype) arrows))
                (traverse p))
              parents))))

      arrows)))

;;----------------------------------------------------------------------
;; Returns all ancestor arrows of a given ftype
;; as (listof (list from to))

(define analysis-ancestors
  (lambda (ftype file-visable?)
    (analysis-walkn ftype get-arrowfrom file-visable?)))

(define analysis-descendants
  (lambda (ftype file-visable?)
    (map reverse
      (analysis-walkn ftype FlowType-alledgeto file-visable?))))

;;----------------------------------------------------------------------
;; Calcs the shortest path to a source FlowType with
;; orig-objs matching filter via breadth-first search
;; Returns a list of FlowTypes
;; or nil if no path to visable source
;; or #f if ftype does not contain filter

(define analysis-shortest-path
  (lambda (ftype file-visable?)
    (assert (FlowType? ftype))
    (if (FlowType-contains-filter? ftype)
      (let ([visited (list ftype)])
        (letrec 
          ([traverse
             (lambda (list-paths)
               (pretty-debug 
                 `(list-paths
                    ,(map (lambda (p) (map FlowType-name p)) list-paths)
                    ,(map FlowType-name visited)))
               (match list-paths
                 [() 
                   ;; No paths -> return the empty path
                   '()]
                 [((and path (ftype . rest-path)) . rest-paths)
                   (pretty-debug 
                     `(path ,(map FlowType-name path)
                        rest-paths 
                        ,(map (lambda (p) (map FlowType-name p)) rest-paths)))
                   (let* ( [parents1 (analysis-parents ftype file-visable?)]
                           [parents2
                             (filter
                               (lambda (p)
                                 (if (memq p visited) 
                                   #f
                                   (begin
                                     (set! visited (cons p visited))
                                     #t)))
                               parents1)])
                     (set! visited (cons ftype visited))
                     (if (and (null? parents2)
                           (or (null? parents1)
                             (null? rest-paths)))
                       ;; either this path has terminated,
                       ;; or went to cycle and is only one left.
                       ;; either way, return it
                       (reverse path)
                       (traverse
                         (append rest-paths
                           (filter-map
                             (lambda (parent) (cons parent path))
                             parents2)))))]))])

          (let ([path (traverse
                        (map list (analysis-parents ftype file-visable?)))])

            (pretty-debug `(traverse-path ,(map FlowType-name path)))

            path)))
      #f)))

'(define analysis-shortest-path
  (lambda (ftype file-visable?)
    (assert (FlowType? ftype))
    (if (FlowType-contains-filter? ftype)
      (let ([visited '()])
        (letrec 
          ([traverse
             (lambda (list-paths)
               (pretty-debug 
                 `(list-paths
                    ,(map (lambda (p) (map FlowType-name p)) list-paths)
                    ,(map FlowType-name visited)))
               (match list-paths
                 [() 
                   ;; No paths -> return the empty path
                   '()]
                 [((and path (ftype . rest-path)) . rest-paths)
                   (pretty-debug 
                     `(path ,(map FlowType-name path)
                        rest-paths 
                        ,(map (lambda (p) (map FlowType-name p)) rest-paths)))
                   (if (memq ftype visited)
                     ;; going around a loop
                     (traverse rest-paths)
                     (let* ( [parents (analysis-parents ftype file-visable?)])
                       (set! visited (cons ftype visited))
                       (if (and (null? parents) (null? rest-paths))
                         ;; this path has terminated, and is the only one left, 
                         ;; and hence the longest, so return it
                         (reverse path)
                         (traverse
                           (append rest-paths
                             (filter-map
                               (lambda (parent) (cons parent path))
                               parents))))))]))])

          (let ([path (traverse
                        (map list (analysis-parents ftype file-visable?)))])

            (pretty-debug `(traverse-path ,(map FlowType-name path)))

            path)))
      #f)))



'(define analysis-shortest-path
  (lambda (ftype file-visable?)
    (assert (FlowType? ftype))
    (let ([visited '()])
      (letrec 
        ([traverse
           (lambda (list-paths)
             (pretty-debug 
               `(list-paths
                  ,(map (lambda (p) (map FlowType-name p)) list-paths)
                  ,(map FlowType-name visited)))
             (match list-paths
               [() 
                 ;; No paths -> return the empty path
                 '()]
               [((and path (ftype . rest-path)) . rest-paths)
                 (if (memq ftype visited)

                   ;; Visited on a shorter path
                   (traverse rest-paths)

                   (begin
                     (pretty-print 
                       `(path ,(map FlowType-name path)
                          rest-paths 
                          ,(map (lambda (p) (map FlowType-name p)) rest-paths)))
                     (set! visited (cons ftype visited))
                     (cond
                       [(FlowType-orig-objs-contains-filter? ftype)
                         ;; Has orig-objs that match the filter 
                         ;; => return this path
                         (reverse path)]
                       [(FlowType-contains-filter? ftype)
                         ;; Has propogated objs that match the filter
                         ;; => go back to parent
                         (let* ([new-paths
                                  (map 
                                    (lambda (parent) (cons parent path))
                                    (get-arrowfrom ftype))])
                           (pretty-print 
                             `(rest-paths ,(map (lambda (p) (map FlowType-name p)) rest-paths)
                                new-paths
                                ,(map (lambda (p) (map FlowType-name p)) new-paths)))
                           (traverse (append rest-paths new-paths)))]
                       [else
                         ;; This FlowType no good
                         (traverse rest-paths)])))]))])
        (let ([path (traverse (list (list ftype)))])

          (pretty-print `(traverse-path ,(map FlowType-name path)))
          ;; Drop first FlowType - it is us
          ;; Remove invisable FlowType's
          ;; NO LONGER Add '() as last tag if last FlowType invisable
          (assert (or (null? path) (eq? (car path) ftype)))
          (if (null? path)
            '()
            (recur loop ([path (cdr path)])
              (match path
                [(ftype . rest)
                  (cond
                    [(FlowType-type-annotation ftype) (cons ftype (loop rest))]
                    [(null? rest) '()]
                    [else (loop rest)])]
                [()
                  ;; No path
                  '()]))))))))

;; ======================================================================
;; Returns a string representation of the the type
      
(define (FlowType->SDL-text ftype)
  (let ([type-port (open-output-string)])
    (dynamic-let
     ([pretty-print-columns (st:pretty-type-width)])
     (pretty-print (FlowType->SDL ftype) type-port))
    (begin0
     (get-output-string type-port)
     (close-output-port type-port))))
      
(define analysis-callback
  (lambda (ftype)
    (assert (FlowType? ftype) 'analysis-callback ftype)
    (pretty-debug `(analysis-callback ,(FlowType->pretty ftype)))
    (FlowType->SDL-text ftype)))
      
;;----------------------------------------------------------------------

'(define test-hyper
   (lambda (files)
     (initialize-analysis!)
     (match-let* 
       ([(defs in out) 
          (sba-analyze-file (files->file-thunk* files))])
       (set! global-in-env in-env)
       (set! global-out-env out-env)
       (report-unbound-vars)
       (hyper defs)
       (printf "Testing parents~n")
       (pretty-print (mapLR analysis-parents list-ftype))
       (printf "Testing analysis-shortest-path~n")
       (pretty-print (mapLR analysis-shortest-path list-ftype))
       (printf "Testing children~n")
       (pretty-print (mapLR analysis-children list-ftype list-ftype))
       (void))))

;; (trace analysis-parents analysis-shortest-path analysis-children)

;;----------------------------------------------------------------------
;; zodiac-based source-correlating pretty-printer

'(define (correlate-pretty-print sexp)
   (let* ([p (open-output-string)]
           [_ (pretty-print sexp p)]
           [s (get-output-string p)]
           [_ (close-output-port p)]
           [p (open-input-string p)]
           [r (zodiac:read p)]
           [zexp (r)])
     (values s
       (recur loop ([sexp sexp][zexp zexp])
         (match (list sexp zexp)
           [((? list? sl) ($ zodiac:list _ s f (? list? zl)))
             (list
               (zodiac:location-offset s)
               (zodiac:location-offset f)
               (map loop sl zl))]
           [(sexp ($ zodiac:zodiac _ s f))
             (list
               (zodiac:location-offset s)
               (zodiac:location-offset f)
               sexp)])))))

;; ----------------------------------------------------------------------

'(define analysis-get-pred-filter-list
   (lambda ()
     (cons (cons "All sources" #f)
       (reverse 
         (map
           (match-lambda
             [(con . constructor) (cons (symbol->string con) (list con))])
           (append constructor-env default-constructor-env))))))

(define analysis-get-param
  (lambda (param)
    ((eval param))))

(define analysis-set-param!
  (lambda (param val)
    ((eval param) val)))

;; ----------------------------------------------------------------------

;; ----------

'(define (contains-some-same-vals? ftype value-from-ftypes)
   ;; Returns #t if ftype contains at least some values from
   ;; each of value-from-ftypes
   (andmap
     (lambda (value-from-ftype)
       (match (list
                (FlowType->Atype ftype)
                (FlowType->Atype value-from-ftype))
         [((? fo-Atype? x) (? fo-Atype? y)) (eq? x y)]
         [((? Tvar? x) (? Tvar? y))
           (ormap 
             (lambda (AV) (memq AV (get-Tvar-objs y)))
             (get-Tvar-objs x))]
         [_ #t]))
     value-from-ftypes))
