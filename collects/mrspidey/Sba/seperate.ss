;; seperate.ss
;; Handles seperate compilation part
;; ----------------------------------------------------------------------

(define (seperately-analyze-file file-thunk* out-file)
  ;; Prepare language
  (when (eq? (select-language) 'none) (mrspidey:error "No language selected"))
  (init-input-type-expander!)
  (init-current-constructor-env!)

  (match-let*
      ([defs1 (load-parse-expand file-thunk*)]
       [_ (set! defs-expanded defs1)]
       [_ (init-kernel!)]
       [_ (init-misc-analysis)]
       [((out-env* in-env*) ...) 
        (map-with-n
         (lambda (def n)
           (mrspidey:progress 'analyze (/ (add1 n) (length defs1)))
           (traverse-def def))
         defs1)]
       [instantiate-env*
        (lambda (env*)
          (apply append 
                 (map (lambda (env)
                        (map
                         (match-lambda
                          [(and b (_ . (? AVS? AVS))) b]
                          [(sym . (? procedure? thunk)) (cons sym (thunk))])
                         env))
                      env*)))]
       [out-env (instantiate-env* out-env*)]
       [in-env  (instantiate-env* in-env*)]
       [extract-AVS (lambda (env) (map cdr env))]
       [AVS-out (extract-AVS out-env)]
       [AVS-in  (extract-AVS in-env)]
       [(AVS-live AVS->nu) (minimize-constraints AVS-out AVS-in)]
       [convert-env
        (lambda (env) 
          (map
           (match-lambda
            [(sym . AVS) (cons sym (AVS->nu AVS))])
           env))]
       [out-env (convert-env out-env)]
       [in-env  (convert-env in-env)])

    (write-constraint-set out-file AVS-live out-env in-env)))

;; ----------------------------------------------------------------------

(define (write-constraint-set file AVS-live out-env in-env)
  (let* ([p (open-output-port file)]
         [disp (lambda (x) (display x p) (newline p))]
         [tag (gensym)]
         [rep-constraint
          (match-lambda
           [($ con _ ($ template type) field-no ($ AVS num))
            (list 'con type field-no num)]
           [($ con-filter _ ($ filter sign (($ template types) ...)) 
               ($ AVS num))
            (list 'con-filter sign types num)])])

    ;; --- write AV and AVS's
    (disp num-AVS)
    (disp num-AV) 
    ;; --- write constructor-env
    (disp constructor-env)
    ;; --- write AV
    (for-each
     (lambda (AVS)
       (for-each
        (match-lambda 
         [(and AV ($ AV num ($ template type) misc fields))
          (unless (eq? (AV-aux AV) tag)
            (disp (list 'AV num type misc (map AVS-num fields)))
            (set-AV-aux! AV tag))])
        (AVS-orig-objs AVS)))
     list-AVS)
    ;; --- write AVS
    (for-each
     (match-lambda 
      [($ AVS num orig-objs _ constraints edgeto)
       (disp (list `AVS num
                   (map AVS-num orig-objs)
                   (map rep-constraint constraints)
                   (map AVS-num edgeto)))])
     AVS-live)
    ;; --- write out-env, in-env
    (disp `(out-env ,@(map AVS-nu out-env)))
    (disp `(in-env  ,@(map AVS-nu in-env)))
    ;; --- all done
    (close-output-port p)))

;; ----------------------------------------------------------------------

(define (read-constraint-set file)
  ;; returns AVS-live out-env in-env
1
)

;; ----------------------------------------------------------------------

(define (ts file)
  (parameterize 
   ([mrspidey:progress-handler (mrspidey:text-progress)]
    [mrspidey:error-handler    mrspidey:text-error])
   (seperately-analyze-file (files->file-thunk* file) "test/out.za")))

              
       
