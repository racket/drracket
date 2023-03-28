#lang racket/base
(require "drracket-errortrace-key.rkt"
         racket/class
         racket/contract
         racket/gui/base
         racket/math
         racket/match
         framework
         "interface.rkt")
(module+ test (require (rename-in rackunit [check r:check]) racket/list racket/bool))

(define oprintf
  (let ([op (current-output-port)])
    (λ args (apply fprintf op args))))

;; current plan: keep interesting editors in viewable stacks and
;; add the ability to, given a srcloc, return an editor plus the
;; edition it was on. --- beware that looking at the normalize path'd
;; version is expensive ... so cache the mapping (for a given stack, not
;; across multiple stacks)

;; todo:
;; - does the stack chopping (to hide parts of DrRacket's implementation
;;   actually still work?)
;; - add arrows for the ran-out-of-memory case
;; - the definition of src-locs in error-display-handler/stacktrace needs
;;   to be factored out so that it can be called in the "program ran out of memory" case

(provide with-stack-checkpoint
         empty-viewable-stack?
         srclocs->viewable-stack
         viewable-stack?
         ;; provided only for backwards compatibility (exported via debug unit)
         srcloc->edition/pair
         get-editions
         errortrace-stack-item->srcloc
         copy-viewable-stack)

(provide
 (contract-out
  [cut-stack-at-checkpoint (-> continuation-mark-set? (listof srcloc?))]
  [empty-viewable-stack (-> viewable-stack?)]
  [viewable-stack->red-arrows-backtrace-srclocs
   (-> viewable-stack? (listof srcloc?))]
  [cms->errortrace-viewable-stack
   (->* (continuation-mark-set? (listof (is-a?/c text%)))
        (#:share-cache (or/c #f viewable-stack?))
        viewable-stack?)]
  [cms->builtin-viewable-stack
   (->* (continuation-mark-set? (listof (is-a?/c text%)))
        (#:share-cache (or/c #f viewable-stack?))
        viewable-stack?)]
  [dis+edition->viewable-stack
   (->* ((listof srcloc?)
         (listof (or/c #f (cons/c weak-box? natural?)))
         (listof (is-a?/c text%)))
        (#:share-cache (or/c #f viewable-stack?))
        viewable-stack?)]

  [viewable-stack-out-of-date-editor? (-> viewable-stack? srcloc? boolean?)]
  [viewable-stack-matching-editor (-> viewable-stack? srcloc? (or/c #f (is-a?/c text%)))]

  [viewable-stack-get-next-items! (-> viewable-stack?
                                      (values
                                       ;; the natural tells how many repeats were elided
                                       ;; if it is zero, then we have this srcloc, exactly once.
                                       ;; if it is two, then we have this srcloc three times
                                       (listof (cons/c srcloc? natural?))

                                       ;; #t => there is more to come
                                       ;; #f => there is no more to come
                                       boolean?))]

  [get-exn-source-locs (-> (is-a?/c text%)
                           any/c ;; use this if it is an exn, otherwise ignore it
                           viewable-stack?
                           viewable-stack?
                           (listof srcloc?))]))

;; run a proc, and if an exception is raised, make it possible to cut the
;; stack so that the surrounding context is hidden
(define checkpoints (make-weak-hasheq))
(define (call-with-stack-checkpoint proc)
  (define checkpoint #f)
  (call-with-exception-handler
   (λ (exn)
     (when checkpoint ; just in case there's an exception before it's set
       (define key (if (exn? exn) (exn-continuation-marks exn) exn))
       (unless (hash-has-key? checkpoints key)
         (hash-set! checkpoints key checkpoint)))
     exn)
   (λ () (proc (λ (ccm) (set! checkpoint ccm))))))
;; returns the stack of the input exception, cutting off any tail that was
;; registered as a checkpoint
(define (cut-stack-at-checkpoint cont-marks)
  (define stack (continuation-mark-set->context cont-marks))
  (define checkpoint
    (cond [(hash-ref checkpoints cont-marks #f) => continuation-mark-set->context]
          [else #f]))
  (define stack-with-gaps-and-extra-info
    (if checkpoint
        (let loop ([st stack]
                   [sl (length stack)]
                   [cp checkpoint]
                   [cl (length checkpoint)])
          (cond [(sl . > . cl) (cons (car st) (loop (cdr st) (sub1 sl) cp cl))]
                [(sl . < . cl) (loop st sl (cdr cp) (sub1 cl))]
                [(equal? st cp) '()]
                [else (loop st sl (cdr cp) (sub1 cl))]))
        stack))
  (map cdr (filter cdr stack-with-gaps-and-extra-info)))

(define-syntax-rule (with-stack-checkpoint expr)
  (call-with-stack-checkpoint (λ (ccm-receiver)
                                (ccm-receiver (current-continuation-marks))
                                expr)))

(module+ test

  (define test-suite-start-line-number (syntax-line #'here))
  (define test-suite-filename (syntax-source #'here))

  (let ()
    (define (non-tail-context x) x)
    (define (a x) (non-tail-context (f x)))
    (set! a a)

    (define (f x) (non-tail-context (g x)))
    (set! f f)

    (define (g x) (with-stack-checkpoint (h x)))
    (set! g g)

    (define (h x) (non-tail-context (i x)))
    (set! h h)

    (define (i x)
      (+ #f #t))
    (set! i i)

    (define cms
      (with-handlers ([exn:fail? exn-continuation-marks])
        (a 0)))

    (define (strict-prefix-of? l1 l2)
      (let loop ([l1 l1] [l2 l2])
        (cond
          [(and (null? l1) (null? l2)) #f]
          [(and (pair? l1) (null? l2)) #f]
          [(and (null? l1) (pair? l2)) #t]
          [(and (pair? l1) (pair? l2))
           (and (equal? (car l1) (car l2))
                (loop (cdr l1) (cdr l2)))])))

    (define all-context (map cdr (filter cdr (continuation-mark-set->context cms))))
    (define cut-context (cut-stack-at-checkpoint cms))
    (check-pred pair? all-context)
    (check-not-equal? all-context cut-context)
    ;; ensure that there are no source locations from the implementation
    ;; of cut-stack-at-checkpoint in the stack that comes out
    (check-true (for/and ([a-srcloc (in-list cut-context)])
                  (implies (equal? (srcloc-source a-srcloc) test-suite-filename)
                           (test-suite-start-line-number . < . (srcloc-line a-srcloc)))))
    (r:check strict-prefix-of? cut-context all-context)))

(struct viewable-stack (stack-items
                        stack-item->srcloc
                        interesting-editor-editions
                        port-name-matches-cache
                        [env #:mutable]))

(define (cms->errortrace-viewable-stack cms interesting-editors
                                        #:share-cache [a-viewable-stack #f])
  (build-viewable-stack (continuation-mark-set->list cms drracket-errortrace-key)
                        errortrace-stack-item->srcloc
                        interesting-editors
                        a-viewable-stack))

(define (errortrace-stack-item->srcloc x)
  (make-srcloc (vector-ref x 0)
               (vector-ref x 1)
               (vector-ref x 2)
               (vector-ref x 3)
               (vector-ref x 4)))

(define (cms->builtin-viewable-stack cms interesting-editors
                                     #:share-cache [a-viewable-stack #f])
  (build-viewable-stack (cut-stack-at-checkpoint cms)
                        values
                        interesting-editors
                        a-viewable-stack))

(define (srclocs->viewable-stack srclocs interesting-editors
                                 #:share-cache [a-viewable-stack #f])
  (build-viewable-stack srclocs values interesting-editors a-viewable-stack))

(define (dis+edition->viewable-stack dis editions interesting-editors
                                     #:share-cache [a-viewable-stack #f])
  (define interesting-editor-editions (get-interesting-editions interesting-editors))
  (add-editions-to-interesting-editors editions interesting-editor-editions)
  (viewable-stack dis values interesting-editor-editions
                  (get-port-name-matches-cache a-viewable-stack)
                  dis))

(define (empty-viewable-stack)
  (build-viewable-stack '() values '() #f))

(define (build-viewable-stack srclocs f interesting-editors a-viewable-stack)
  (viewable-stack srclocs
                  f
                  (get-interesting-editions interesting-editors)
                  (get-port-name-matches-cache a-viewable-stack)
                  srclocs))

(define (get-port-name-matches-cache a-viewable-stack)
  (if a-viewable-stack
      (viewable-stack-port-name-matches-cache a-viewable-stack)
      (make-weak-hasheq)))

(define (get-interesting-editions interesting-editors)
  (define interesting-editor-editions (make-weak-hash))
  (for ([editor (in-list interesting-editors)])
    (hash-set! interesting-editor-editions editor (send editor get-edition-number)))
  interesting-editor-editions)

(define (add-editions-to-interesting-editors editions interesting-editor-editions)
  (for ([edition (in-list editions)])
    (when edition
      (match-define (cons wb edition-number) edition)
      (define ed (weak-box-value wb))
      (when ed
        (hash-set! interesting-editor-editions ed edition)))))

(define (empty-viewable-stack? a-viewable-stack)
  (match-define (viewable-stack stack-items _ _ _ _)
    a-viewable-stack)
  (null? stack-items))

(define (viewable-stack-out-of-date-editor? a-viewable-stack a-srcloc)
  (define txt+edition (viewable-stack-matching-editor+edition a-viewable-stack a-srcloc))
  (cond
    [txt+edition
     (match-define (cons txt edition) txt+edition)
     (not (= (send txt get-edition-number) edition))]
    [else #f]))

(define (viewable-stack-matching-editor a-viewable-stack a-srcloc)
  (define txt+edition (viewable-stack-matching-editor+edition a-viewable-stack a-srcloc))
  (and txt+edition (car txt+edition)))

(define (viewable-stack-matching-editor+edition a-viewable-stack a-srcloc)
  (match-define (viewable-stack _ _ interesting-editor-editions port-name-matches-cache _)
    a-viewable-stack)
  (match-define (srcloc source line col position span) a-srcloc)
  (for/or ([(txt edition) (in-hash interesting-editor-editions)])
    (and (port-name-matches?/use-cache txt source port-name-matches-cache)
         (cons txt edition))))

(define (remove-adjacent-duplicates things thing->di)
  (cond
    [(null? things) (values '() '())]
    [else
     (let loop ([di (thing->di (car things))]
                [things (cdr things)])
       (cond
         [(null? things) (values (list di) (list 0))]
         [else
          (define di2 (thing->di (car things)))
          (define-values (res-dis skip-counts) (loop di2 (cdr things)))
          (if (equal? di di2)
              (values res-dis (cons (+ (car skip-counts) 1) (cdr skip-counts)))
              (values (cons di res-dis)
                      (cons 0 skip-counts)))]))]))

(module+ test
  (define (check dis-in dis-expected skip-expected)
    (define-values (dis-got skip-got) (remove-adjacent-duplicates dis-in values))
    (unless (and (equal? dis-got dis-expected)
                 (equal? skip-got skip-expected))
      (eprintf "~s =\n  ~s, but expected\n  ~s\n\n"
               `(remove-adjacent-duplicates ',dis-in values)
               `(values ',dis-got ',skip-got)
               `(values ',dis-expected ',skip-expected))))
  (check '() '() '())
  (check '(1) '(1) '(0))
  (check '(1 2 3) '(1 2 3) '(0 0 0))
  (check '(1 1) '(1) '(1))
  (check '(1 2) '(1 2) '(0 0))
  (check '(1 2 2 3 4 4 3)
         '(1 2 3 4 3) '(0 1 0 1 0))
  (check '(1 2 2 2 2 2 3)
         '(1 2 3) '(0 4 0)))

(define (get-editions port-name-matches-cache defs ints stack
                      is-drracket-frame?)
  (for/list ([x (in-list stack)])
    (srcloc->edition/pair defs ints x
                          is-drracket-frame?
                          port-name-matches-cache)))

(define (srcloc->edition/pair defs ints srcloc is-drracket-frame?
                              [port-name-matches-cache #f])
  (define src (srcloc-source srcloc))
  (cond
    [(and (or (symbol? src)
              (path? src))
          ints
          (port-name-matches?/use-cache ints src port-name-matches-cache))
     (cons (make-weak-box ints) (send ints get-edition-number))]
    [(and (or (symbol? src)
              (path? src))
          defs
          (port-name-matches?/use-cache defs src port-name-matches-cache))
     (cons (make-weak-box defs) (send defs get-edition-number))]
    [(path? src)
     (define frame (send (group:get-the-frame-group) locate-file src))
     (and frame
          (is-drracket-frame? frame)
          (cons (make-weak-box (send frame get-definitions-text))
                (send (send frame get-definitions-text) get-edition-number)))]
    [else #f]))

(define (port-name-matches?/use-cache txt src port-name-matches-cache)
  (cond
    [port-name-matches-cache
     (define txt-cache (hash-ref! port-name-matches-cache txt (λ () (make-weak-hash))))
     (hash-ref! txt-cache src
                (λ () (send txt port-name-matches? src)))]
    [else
     (send txt port-name-matches? src)]))

(define (viewable-stack->red-arrows-backtrace-srclocs a-viewable-stack)
  (match-define (viewable-stack stack-items stack-item->srcloc interesting-editor-editions port-name-matches-cache env)
    a-viewable-stack)
  (for/list ([stack-item (in-list stack-items)]
             [i (in-range backtrace-stack-limit)])
    (stack-item->srcloc stack-item)))

;; look only at the first N stack items (not counting duplicates) when trying to figure out where to draw arrows
(define backtrace-stack-limit 1000)

(define (pick-first-defs defs a-viewable-stack)
  (match-define (viewable-stack stack-items stack-item->srcloc interesting-editor-editions port-name-matches-cache env)
    a-viewable-stack)
  (for/or ([stack-item (in-list stack-items)])
    (define srcloc (stack-item->srcloc stack-item))
    (and (srcloc? srcloc)
         (port-name-matches?/use-cache defs (srcloc-source srcloc) port-name-matches-cache)
         srcloc)))

(define (viewable-stack-first-srcloc a-viewable-stack)
  (match-define (viewable-stack stack-items stack-item->srcloc interesting-editor-editions port-name-matches-cache env)
    a-viewable-stack)
  (cond
    [(pair? stack-items)
     (define stack-item (car stack-items))
     (define srcloc (stack-item->srcloc stack-item))
     (and (srcloc? srcloc)
          srcloc)]
    [else #f]))

(define (copy-viewable-stack s)
  (struct-copy viewable-stack s))

(define (viewable-stack-get-next-items! a-viewable-stack)
  (match-define (viewable-stack stack-items stack-item->srcloc interesting-editor-editions port-name-matches-cache stack-next-items)
    a-viewable-stack)

  (unless stack-next-items
    (set! stack-next-items stack-items)
    (set-viewable-stack-env! a-viewable-stack stack-next-items))

  (define how-many-at-once 15)
  (cond
    [(null? stack-next-items)
     (values '() #f)]
    [else
     (define items
       (let loop ([fst-item (car stack-next-items)]
                  [fst-srcloc (stack-item->srcloc (car stack-next-items))]
                  [fst-item-count 0]
                  [rst-items (cdr stack-next-items)]
                  [max-stack-size how-many-at-once]
                  [max-items-to-consume 50000])
         (cond
           [(zero? max-stack-size)
            (set-viewable-stack-env! a-viewable-stack (cons fst-item rst-items))
            '()]
           [(or (zero? max-items-to-consume) (null? rst-items))
            (set-viewable-stack-env! a-viewable-stack rst-items)
            (list (cons fst-srcloc fst-item-count))]
           [else
            (define snd-item (car rst-items))
            (define snd-srcloc (stack-item->srcloc snd-item))
            (cond
              [(equal? snd-srcloc fst-srcloc)
               (loop fst-item fst-srcloc (+ fst-item-count 1) (cdr rst-items)
                     max-stack-size
                     (- max-items-to-consume 1))]
              [else
               (cons (cons fst-srcloc fst-item-count)
                     (loop snd-item snd-srcloc 0 (cdr rst-items)
                           (- max-stack-size 1)
                           (- max-items-to-consume 1)))])])))
     (define more-to-show? (pair? (viewable-stack-env a-viewable-stack)))
     (values items more-to-show?)]))

(module+ test

  (define (viewable-stack-get-next-items!/lst . args)
    (call-with-values (λ () (apply viewable-stack-get-next-items! args))
                      list))

  (let ()
    (define vs1
      (srclocs->viewable-stack '() '()))
    (check-equal? (viewable-stack-get-next-items!/lst vs1)
                  (list '() #f)))

  (let ()
    (define a (srcloc (string->path "/tmp/x.rkt") 1 1 1 1))
    (define b (srcloc (string->path "/tmp/x.rkt") 2 2 2 2))
    (define vs1 (srclocs->viewable-stack (list a b) '()))
    (check-equal? (viewable-stack-get-next-items!/lst vs1)
                  (list (list (cons a 0)
                              (cons b 0))
                        #f)))

  (let ()
    (define a (srcloc (string->path "/tmp/x.rkt") 1 1 1 1))
    (define vs1
      (srclocs->viewable-stack (for/list ([i (in-range 2)]) a) '()))
    (check-equal? (viewable-stack-get-next-items!/lst vs1)
                  (list (list (cons a 1))
                        #f)))

  (let ()
    (define a (srcloc (string->path "/tmp/x.rkt") 1 1 1 1))
    (define vs1
      (srclocs->viewable-stack (for/list ([i (in-range 50)]) a) '()))
    (check-equal? (viewable-stack-get-next-items!/lst vs1)
                  (list (list (cons a 49))
                        #f)))

  (let ()
    (define a (srcloc (string->path "/tmp/x.rkt") 1 1 1 1))
    (define b (srcloc (string->path "/tmp/x.rkt") 2 2 2 2))
    (define vs1
      (srclocs->viewable-stack (append (for/list ([i (in-range 50)]) a)
                                       (for/list ([i (in-range 50)]) b))
                               '()))
    (define (viewable-stack-get-next-items!/lst . args)
      (call-with-values (λ () (apply viewable-stack-get-next-items! args))
                        list))
    (check-equal? (viewable-stack-get-next-items!/lst vs1)
                  (list (list (cons a 49)
                              (cons b 49))
                        #f)))

  (let ()
    (define a (srcloc (string->path "/tmp/x.rkt") 1 1 1 1))
    (define b (srcloc (string->path "/tmp/x.rkt") 2 2 2 2))
    (define vs1
      (srclocs->viewable-stack (flatten (for/list ([i (in-range 20)]) (list a b)))
                               '()))
    (check-equal? (viewable-stack-get-next-items!/lst vs1)
                  (list (list (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0)
                              (cons a 0))
                        #t))

    (check-equal? (viewable-stack-get-next-items!/lst vs1)
                  (list (list (cons b 0)
                              (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0))
                        #t))

    (check-equal? (viewable-stack-get-next-items!/lst vs1)
                  (list (list (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0)
                              (cons a 0)
                              (cons b 0))
                        #f)))

  (let ()
    (define a (srcloc (string->path "/tmp/x.rkt") 1 1 1 1))
    (define b (srcloc (string->path "/tmp/x.rkt") 2 2 2 2))
    (define c (srcloc (string->path "/tmp/x.rkt") 3 3 3 3))
    (define vs1
      (srclocs->viewable-stack (flatten (for/list ([i (in-range 50)]) (list a b c)))
                               '()))
    (check-equal? (viewable-stack-get-next-items!/lst vs1)
                  (list (list (cons a 0)
                              (cons b 0)
                              (cons c 0)
                              (cons a 0)
                              (cons b 0)
                              (cons c 0)
                              (cons a 0)
                              (cons b 0)
                              (cons c 0)
                              (cons a 0)
                              (cons b 0)
                              (cons c 0)
                              (cons a 0)
                              (cons b 0)
                              (cons c 0))
                        #t)))

  (let ()
    (define a (srcloc (string->path "/tmp/x.rkt") 1 1 1 1))
    (define b (srcloc (string->path "/tmp/x.rkt") 2 2 2 2))
    (define c (srcloc (string->path "/tmp/x.rkt") 3 3 3 3))
    (define vs1
      (srclocs->viewable-stack (flatten (for/list ([i (in-range 5)]) (list a b c)))
                               '()))
    (check-equal? (viewable-stack-get-next-items!/lst vs1)
                  (list (list (cons a 0)
                              (cons b 0)
                              (cons c 0)
                              (cons a 0)
                              (cons b 0)
                              (cons c 0)
                              (cons a 0)
                              (cons b 0)
                              (cons c 0)
                              (cons a 0)
                              (cons b 0)
                              (cons c 0)
                              (cons a 0)
                              (cons b 0)
                              (cons c 0))
                        #f)))

  (let ()
    (define a (srcloc (string->path "/tmp/x.rkt") 1 1 1 1))
    (define b (srcloc (string->path "/tmp/x.rkt") 2 2 2 2))
    (define c (srcloc (string->path "/tmp/x.rkt") 3 3 3 3))
    (define vs1
      (srclocs->viewable-stack (list a a a a a
                                     b b b b
                                     c c c)
                               '()))
    (check-equal? (viewable-stack-get-next-items!/lst vs1)
                  (list (list (cons a 4)
                              (cons b 3)
                              (cons c 2))
                        #f)))

  (let ()
    (define a (srcloc (string->path "/tmp/x.rkt") 1 1 1 1))
    (define b (srcloc (string->path "/tmp/x.rkt") 2 2 2 2))
    (define c (srcloc (string->path "/tmp/x.rkt") 3 3 3 3))
    (define vs1
      (srclocs->viewable-stack (list a a a a a
                                     b b b b
                                     c c c
                                     b
                                     a a a a a a a a)
                               '()))
    (check-equal? (viewable-stack-get-next-items!/lst vs1)
                  (list (list (cons a 4)
                              (cons b 3)
                              (cons c 2)
                              (cons b 0)
                              (cons a 7))
                        #f))))

(define (get-exn-source-locs defs exn a-viewable-stack1 a-viewable-stack2)
  (cond
    [(exn:srclocs? exn) ((exn:srclocs-accessor exn) exn)]
    [(pick-first-defs defs a-viewable-stack1) => list]
    [(pick-first-defs defs a-viewable-stack2) => list]
    [(viewable-stack-first-srcloc a-viewable-stack1) => list]
    [(viewable-stack-first-srcloc a-viewable-stack2) => list]
    [else '()]))
