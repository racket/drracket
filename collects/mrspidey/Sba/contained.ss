;; contained.ss
;;
;; Check if one Tvar is contained in another
;; Doesn't work for contravariant fields
;; ----------------------------------------------------------------------
;; NOTE: Tvar2 must be tidy!!!

(define stack '())
(define fail-stack '())

(define Tvar-containment?
  (lambda (tvar1 tvar2)
    (let*-vals 
      ( [calc-reached
          (lambda (tvar)
            (let*-vals 
              ([(reached? set-reached! get-reached) 
                 (field->set alloc-Tvar-field)])
              (recur traverse ([tvar tvar])
                (unless (reached? tvar)
                  (set-reached! tvar)
                  (for-each
                    (match-lambda
                      [($ AV _ ($ template _ _ ref) _ fields)
                        (for i 0 (vector-length ref)
                          (traverse 
                            (vector-ref fields (vector-ref ref i))))])
                    (get-Tvar-objs tvar))))
              (get-reached)))]
        [list-reached-1 (calc-reached tvar1)]
        [list-reached-2 (calc-reached tvar2)]
        [num-reached-1 (length list-reached-1)]
        [(get-num set-num!) (alloc-Tvar-field)]
        [(get-reached-vec set-reached-vec!) (alloc-Tvar-field)]
        )

        (for-each-with-n (lambda (tvar n) (set-num! tvar n))
          list-reached-1)

      (for-each
        (lambda (tvar) (set-reached-vec! tvar (make-vector num-reached-1 #f)))
        list-reached-2)

      ;; Tidyness check
      '(for-each
        (lambda (tvar) 
          (let* ([objs (get-Tvar-objs tvar)]
                 [templates (map AV-template objs)]
                 [types (map template-type templates)]
                 [types2 (list->set types)])
            (unless (= (length types) (length types2))
              (mrspidey:error
               (format "Upper bound of containment is not tidy, types are ~s"
                       types)))))
        list-reached-2)       

      (begin0
       (let/cc 
        fail

        (recur ensure-contained ([tvar1 tvar1][tvar2 tvar2])
          (fluid-let ([stack (cons (cons tvar1 tvar2) stack)])
            (let ([n (get-num tvar1)]
                  [reached (get-reached-vec tvar2)])
              (unless (vector-ref reached n)
                ;; Need to search - record true to detect loops
                (vector-set! reached n #t)
                (for-each
                 (match-lambda
                  [($ AV _ (and template ($ template _ _ ref)) _ fields)
                   (or 

                    ;; More than one matching => not tidy => say contained.
                    (> (count (match-lambda
                               [($ AV _ template2 _ fields2)
                                (eq? (template-type template)
                                     (template-type template2))])
                              (get-Tvar-objs tvar2))
                       1)

                    (ormap
                     (match-lambda
                      [($ AV _ template2 _ fields2)
                       (and             ;(eq? template template2)
                        (eq? (template-type template)
                             (template-type template2))
                        (begin
                          (for i 0 (vector-length ref)
                               (let ([r (vector-ref ref i)])
                                 (ensure-contained
                                  (vector-ref fields r)
                                  (vector-ref fields2 r))))
                          #t))])
                     (get-Tvar-objs tvar2))
                    ;; No match
                    (begin
                      (printf
                       "~s ~s not in ~s ~s~n"
                       (map (lambda (AV) (template-type (AV-template AV)))
                            (get-Tvar-objs tvar1))
                       (Tvar-name tvar1)
                       (map (lambda (AV) (template-type (AV-template AV)))
                            (get-Tvar-objs tvar2))
                       (Tvar-name tvar2))
                      (set! fail-stack stack)
                      (fail #f)))])
                 (get-Tvar-objs tvar1))))))

        ;; Did not fail => succeed
        #t)

        ))))

'(define (show-fail-stack-sdl)
   (map
    (match-lambda
     [(a . d)
      (list (Tvar->SDL a)
            (Tvar->SDL d))])
    fail-stack))

'(define (show-fail-stack)
   (for-each
    (match-lambda
     [(a . d)
      (printf "============~n")
      (show-Tvar a)
      (show-Tvar d)])
    fail-stack))
