; ======================================================================
;; NFA->DFA
;; roots: (list nt)
;; final: (list nt)
;; returns (list (list AVS) (list AVS))

(define (NFA->DFA for-each-prod roots final)
  ;; n is an old nt
  ;; d is new nt
  (letrec
      ([n*->d '()]                    ; Maps set of NFA nts to a DFA nt
       [n*<=
        (lambda (n1* n2*)
          (andmap (lambda (n1) (mem-nt? n1 n2*)) n1*))]        
       [n*=
        (lambda (n1* n2*)
          (and (n*<= n1* n2*) (n*<= n2* n1*)))]
       [lookup
        (lambda (n*)
          (recur loop ([l n*->d])
            (cond
             [(null? l) #f]
             [(n*= n* (caar l)) (cdar l)]
             [else (loop (cdr l))])))]
       [traverse
        (lambda (n*)
          (or (lookup n*)
              ;; Need to traverse
              ;; think about epsilon-closure
              (let* ([rhs* '()]
                     [_ (for-each
                         (lambda (n)
                           (for-each-prod
                            (lambda (rhs) (set! rhs* (cons rhs rhs*)))
                            n))
                         n*)]
                     [d (mk-AVS-nolist 'dfa)])
                (set! n*->d (cons (cons n* d) n*->d))
                (recur loop ([rhs* rhs*])
                  (match rhs*
                    [() (void)]
                    [(($ rhs grsym nt) . _)
                     ;; Merge all with same grsym 
                     (match-let* 
                         ([(nt* . rest) 
                           (filter-map-split
                            (match-lambda
                             [($ rhs grsym2 nt2)
                              (if (grsym-eq? grsym grsym2)
                                  nt2
                                  #f)])
                            rhs*)])
                       (add-prod! d (make-rhs grsym (traverse nt*)))
                       (loop rest))]))
                d)))])
    (traverse (list AVS))
    (list (map (lambda (r) (lookup (list r))) roots)
          (map (lambda (r) (lookup (list r))) final))))

