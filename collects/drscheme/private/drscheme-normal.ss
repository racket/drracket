
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
  (define special-state #f)
  (define normal-bitmap #f)
  (define (get-normal-bitmap)
    (unless normal-bitmap
      (set! normal-bitmap  (make-object bitmap% (build-path (collection-path "icons") "PLT-206.png"))))
    normal-bitmap)
  
  (define-struct magic-image (string filename bitmap)) 
  
  (define (magic-img str img)
    (make-magic-image (list->string (reverse (string->list str))) img #f))
  
  ;; magic strings and their associated images. Any string in this list that's a prefix of any other
  ;; is not going to be reachable; you could change that by removing the (set! key-codes null) line
  ;; where the match occurs
  (define magic-images
    (list 
     (magic-img "larval" "PLT-206-larval.png")
     (magic-img "mars" "PLT-206-mars.jpg")))
  
  (define (load-magic-images)
    (get-normal-bitmap)
    (for-each 
     (lambda (magic-image)
       (unless (magic-image-bitmap magic-image)
         (set-magic-image-bitmap! 
          magic-image 
          (make-object bitmap% (build-path (collection-path "icons") (magic-image-filename magic-image))))))
     magic-images))
  
  (define longest-magic-string (apply max (map (lambda (s) (string-length (magic-image-string s))) magic-images)))
  
  (define key-codes null)
  (define key-codes-len 0)
  
  (define (add-key-code new-code)
    (unless (eq? 'release new-code)
      (set! key-codes
            (let loop ([n longest-magic-string]
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
       (load-magic-images)
       
       (when (andmap char? key-codes)
         (let ((s (apply string key-codes)))
           (cond 
             [(ormap (lambda (m)
                       (if (string=? s (magic-image-string m))
                           m
                           #f)) magic-images)
              => 
              (lambda (match)
                (set! key-codes null)
                (set-splash-bitmap
                 (if (and special-state (string=? special-state (magic-image-string match)))
                     (begin
                       (set! special-state #f)
                       (get-normal-bitmap))
                     (begin
                       (set! special-state (magic-image-string match))
                       (magic-image-bitmap match)))))]
             [else (void)])))

         )))
  
  ((dynamic-require '(lib "splash.ss" "framework") 'start-splash)
   (build-path (collection-path "icons") 
               (cond
                 [texas-independence-day?
                  "texas-plt-bw.gif"]
                 [high-color? "PLT-206.png"]
                 [(= (get-display-depth) 1)
                  "pltbw.gif"]
                 [else
                  "plt-flat.gif"]))
   "DrScheme"
   99)
  
  (dynamic-require '(lib "start.ss" "drscheme" "private") #f))
