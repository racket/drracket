;; (list-of (list string (list-of (weak-box TST))))
(send-sexp-to-mred '(define mem-boxes null))

(define mem-count 10)
(define mem-cutoff 1)

(define (test-allocate tag open close)
  (send-sexp-to-mred
   `(let ([new-boxes
           (let loop ([n ,mem-count])
             (cond
               [(zero? n) null]
               [else
                (let* ([o (,open)]
                       [b (make-weak-box o)])
                  (,close o)
                  (cons b (loop (- n 1))))]))])
      (collect-garbage)
      (set! mem-boxes (cons (list ,tag new-boxes) mem-boxes)))))

(define (done)
  (send-sexp-to-mred
   `(begin
      (collect-garbage)
      (collect-garbage)
      (collect-garbage)
      (collect-garbage)
      (let ([f (make-object dialog% "Results")]
            [anything? #f])
        (for-each
         (lambda (boxl)
           (let* ([tag (first boxl)]
                  [boxes (second boxl)]
                  [res
                   (lambda ()
                     (foldl (lambda (b n) (if (weak-box-value b) (+ n 1) n))
                            0
                            boxes))])
             (unless (<= (calc-results) ,mem-cutoff)
               (collect-garbage))
             (let ([res (calc-results)])
               (when (<= res ,mem-cutoff)
                 (set! anything? #t)
                 (make-object message% (format "~a: ~a of ~a~n" tag res ,mem-count) f)))))
         mem-boxes)
        (cond
          [anything? (make-object button% "Close" f (lambda x (send f show #f)))]
          [else (make-object button% "NOTHING!" f (lambda x (send f show #f)))])
        (send f show #t)))))

(define (test-frame-allocate name %)
  (send-sexp-to-mred '(preferences:set 'framework:exit-when-no-frames #f))
  (test-allocate name 
                 `(lambda () (let ([f (make-object ,% ,name)])
			       (send f show #t)
			       f))
                 `(lambda (f) (send f close)))
  (send-sexp-to-mred '(preferences:set 'framework:exit-when-no-frames #t)))

(test-allocate "frame%"
               '(lambda () (let ([f (make-object frame% "test frame")])
                            (send f show #t)
                            f))
	       '(lambda (f) (send f show #f)))


(test-frame-allocate "frame:basic%" 'frame:basic%)
(test-frame-allocate "frame:standard-menus%" 'frame:standard-menus%)
(test-frame-allocate "frame:text%" 'frame:text%)
(test-frame-allocate "frame:searchable%" 'frame:searchable%)
(test-frame-allocate "frame:text-info%" 'frame:text-info%)
(test-frame-allocate "frame:text-info-file%" 'frame:text-info-file%)
(test-frame-allocate "frame:pasteboard%" 'frame:pasteboard%)
(test-frame-allocate "frame:pasteboard-info%" 'frame:pasteboard-info%)
(test-frame-allocate "frame:pasteboard-info-file%" 'frame:pasteboard-info-file%)
(done)

