
(module drscheme-normal mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss"))
  
  ;; this used to be done by mred, but
  ;; since drscheme uses the -Z flag now,
  ;; we have to do it explicitly.
  (current-load text-editor-load-handler)
  
  (define texas-independence-day?
    (let ([date (seconds->date (current-seconds))])
      (and (= 3 (date-month date))
           (= 2 (date-day date)))))
  
  (define high-color? ((get-display-depth) . > . 8))
  (define larval-state? #f)
  (define larval-bitmap #f)
  (define (get-larval-bitmap)
    (unless larval-bitmap
      (set! larval-bitmap (make-object bitmap% (build-path (collection-path "icons") "PLTylarval.png"))))
    larval-bitmap)
  (define no-larval-bitmap #f)
  (define (get-no-larval-bitmap)
    (unless no-larval-bitmap
      (set! no-larval-bitmap  (make-object bitmap% (build-path (collection-path "icons") "PLTnolarval.png"))))
    no-larval-bitmap)
  
  ;; reversed version of the actual string we care about
  (define magic-string (list->string (reverse (string->list "larval"))))
  
  (define key-codes null)
  (define (add-key-code new-code)
    (unless (eq? 'release new-code)
      (set! key-codes 
            (let loop ([n (string-length magic-string)]
                       [l (cons new-code key-codes)])
              (cond
                [(zero? n) null]
                [(null? l) null]
                [else (cons (car l) (loop (- n 1) (cdr l)))])))))
  
  (let ([set-splash-bitmap (dynamic-require '(lib "splash.ss" "framework") 'set-splash-bitmap)])
    ((dynamic-require '(lib "splash.ss" "framework") 'set-splash-char-observer)
     (lambda (evt)
       (add-key-code (send evt get-key-code))
       
       ;; as soon as something is typed, load the bitmaps
       (get-larval-bitmap)
       (get-no-larval-bitmap)
       
       (when (and (andmap char? key-codes)
                  (equal? (apply string key-codes) magic-string))
         (set! key-codes null)
         (set! larval-state? (not larval-state?))
         (set-splash-bitmap
          (if larval-state?
              (get-larval-bitmap)
              (get-no-larval-bitmap)))))))
  
  ((dynamic-require '(lib "splash.ss" "framework") 'start-splash)
   (build-path (collection-path "icons") 
               (cond
                 [texas-independence-day?
                  "texas-plt-bw.gif"]
                 [high-color? "PLTnolarval.png"]
                 [(= (get-display-depth) 1)
                  "pltbw.gif"]
                 [else
                  "plt-flat.gif"]))
   "DrScheme"
   99)
  
  (dynamic-require '(lib "start.ss" "drscheme" "private") #f))
