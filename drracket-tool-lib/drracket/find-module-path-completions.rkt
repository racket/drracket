#lang racket/base

(require racket/contract/base
         racket/system
         racket/port
         racket/contract
         racket/list
         pkg/lib
         compiler/module-suffix)

(define current-library-collection-links-info/c
  (listof (or/c #f
                (and/c path? complete-path?)
                (hash/c (or/c (and/c symbol? module-path?) #f)
                        (listof (and/c path? complete-path?))
                        #:flat? #t))))
(define current-library-collection-paths-info/c
  (listof (and/c path? complete-path?)))

(define pkg-dirs-info/c
  (listof (list/c string? (and/c path? complete-path?))))

(provide
 current-library-collection-links-info/c
 current-library-collection-paths-info/c
 pkg-dirs-info/c
 (contract-out
  [find-module-path-completions
   (-> path-string? (-> string? (listof (list/c string? path?))))]
  [alternate-racket-clcl/clcp (-> path-string?
                                  (box/c (or/c #f pkg-dirs-info/c))
                                  (values current-library-collection-links-info/c 
                                          current-library-collection-paths-info/c
                                          pkg-dirs-info/c))]
  [find-module-path-completions/explicit-cache
   (->* (string?
         path-string?
         #:pkg-dirs-cache
         (box/c (or/c #f pkg-dirs-info/c)))
        (#:alternate-racket
         (or/c #f
               path-string?
               (list/c current-library-collection-links-info/c 
                       current-library-collection-paths-info/c
                       pkg-dirs-info/c)))
        (listof (list/c string? path?)))]))

(define (find-module-path-completions the-current-directory)
  (define pkg-dirs-cache (box #f))
  (λ (str)
    (find-module-path-completions/explicit-cache
     str the-current-directory
     #:pkg-dirs-cache pkg-dirs-cache)))

(define (ignore? module-suffix-regexp x-str is-a-dir?)
  (or (member x-str '("compiled"))
      (if is-a-dir?
          #f
          (not (regexp-match? module-suffix-regexp x-str)))
      (if (equal? (system-type) 'windows)
          (regexp-match? #rx"[.]bak$" x-str)
          (regexp-match? #rx"~$" x-str))))

;; these functions just hide filesystem permission errors, but still check
;; that their arguments match the contracts they are supposed to
(define/contract (safe-directory-list d)
  (-> path-string? (listof path?))
  (with-handlers ([exn:fail? (λ (x) '())])
    (directory-list d)))
(define/contract (safe-directory-exists? d)
  (-> path-string? boolean?)
  (with-handlers ([exn:fail? (λ (x) #f)])
    (directory-exists? d)))

(define (find-module-path-completions/explicit-cache str the-current-directory
                                                     #:alternate-racket [alternate-racket #f]
                                                     #:pkg-dirs-cache pkgs-dirs-cache)
  (cond
    [(and (not (equal? str "")) (equal? (string-ref str 0) #\"))
     (define no-quotes-string
       (cond
         [(equal? (string-ref str (- (string-length str) 1)) #\")
          (define no-last-quote (substring str 0 (- (string-length str) 1)))
          (cond
            [(equal? "" no-last-quote)
             no-last-quote]
            [else
             (substring no-last-quote 1 (string-length no-last-quote))])]
         [else
          (substring str 1 (string-length str))]))
     (define segments (regexp-split #rx"/" no-quotes-string))
     (find-completions/internal segments
                                (list (list "" the-current-directory))
                                safe-directory-list
                                safe-directory-exists?
                                #t)]
    [else
     (find-completions-collection/internal str
                                           (find-all-collection-dirs alternate-racket
                                                                     pkgs-dirs-cache)
                                           safe-directory-list
                                           safe-directory-exists?)]))

(define (find-completions-collection/internal string collection-dirs dir->content is-dir?)
  (define segments (regexp-split #rx"/" string))
  (define first-candidates
    (cond
      [(null? segments) '()]
      [else
       (define reg (regexp (string-append "^" (regexp-quote (car segments)))))
       (filter (λ (line) (regexp-match reg (list-ref line 0)))
               collection-dirs)]))
  (find-completions/internal (cdr segments) first-candidates dir->content is-dir? #f))

(define (find-completions/internal segments first-candidates dir->content is-dir? allow-dot-dot?)
  (define module-suffix-regexp (get-module-suffix-regexp))
  (define unsorted
    (let loop ([segments segments]
               [candidates first-candidates])
      (cond
        [(null? segments) candidates]
        [else
         (define segment (car segments))
         (cond
           [(and allow-dot-dot? (equal? segment ".."))
            (define nexts
              (for*/list ([key+candidate (in-list candidates)]
                          [candidate (in-value (list-ref key+candidate 1))]
                          #:when (is-dir? candidate)
                          [ent (in-value (simplify-path (build-path candidate 'up)))]
                          [ent-str (in-value (path->string ent))]
                          #:unless (ignore? module-suffix-regexp
                                            ent-str
                                            (is-dir? (build-path candidate ent))))
                (list ent-str ent)))
            (loop (cdr segments) nexts)]
           [else
            (define reg (regexp (string-append "^" (regexp-quote segment))))
            (define nexts
              (for*/list ([key+candidate (in-list candidates)]
                          [candidate (in-value (list-ref key+candidate 1))]
                          #:when (is-dir? candidate)
                          [ent (in-list (dir->content candidate))]
                          [ent-str (in-value (path->string ent))]
                          #:unless (ignore? module-suffix-regexp
                                            ent-str
                                            (is-dir? (build-path candidate ent)))
                          #:when (regexp-match reg ent-str))
                (list ent-str (build-path candidate ent))))
            (loop (cdr segments) nexts)])])))
  (sort unsorted string<=? #:key (λ (x) (path->string (list-ref x 1)))))

;; -> (listof (list string? path?))
;; returns a list of all of the directories that are being treated as collections,
;; (together with the names of the collections)
(define (find-all-collection-dirs alternate-racket pkgs-dirs-cache)
  (define-values (library-collection-links library-collection-paths pkg-paths)
    (cond
      [(list? alternate-racket)
       (values (list-ref alternate-racket 0)
               (list-ref alternate-racket 1)
               (list-ref alternate-racket 2))]
      [else
       (alternate-racket-clcl/clcp alternate-racket pkgs-dirs-cache)]))
  ;; link-content : (listof (list (or/c 'root 'static-root string?) path?))
  (define link-content
    (apply
     append
     pkg-paths
     (for/list ([link (in-list library-collection-links)])
       (cond
         [link
          (define-values (base name dir?) (split-path link))
          (cond
            [(file-exists? link)
             (define link-ents (with-handlers ([exn:fail? (λ (x) '())])
                                 (call-with-input-file link read)))
             (for/list ([link-ent (if (list? link-ents)
                                      link-ents
                                      '())]
                        #:when (if (and (list? link-ent) (= 3 (length link-ent)))
                                   (and (regexp? (list-ref link-ent 2))
                                        (regexp-match (list-ref link-ent 2) (version)))
                                   #t))
               `(,(list-ref link-ent 0)
                 ,(simplify-path
                   (if (relative-path? (list-ref link-ent 1))
                       (build-path base (list-ref link-ent 1))
                       (list-ref link-ent 1)))))]
            [else '()])]
         [else
          (for/list ([clp (in-list library-collection-paths)])
            `(root ,(simplify-path clp)))]))))

  (remove-duplicates
   (apply
    append
    (for/list ([just-one (in-list link-content)])
      (define-values (what pth) (apply values just-one))
      (cond
        [(string? what)
         (list just-one)]
        [else
         (cond
           [(safe-directory-exists? pth)
            (for/list ([dir (in-list (safe-directory-list pth))]
                       #:when (safe-directory-exists? (build-path pth dir)))
              (list (path->string dir) (build-path pth dir)))]
           [else '()])])))))

(define-syntax-rule (thunk-and-quote e)
  (values (λ () e) 'e))

(define-values (compute-pkg-dirs compute-pkg-dirs-code)
  (thunk-and-quote
   (let ([h (make-hash)])
     (for*/list ([scope (in-list (get-all-pkg-scopes))]
                 [name (in-list (installed-pkg-names #:scope scope))])
       (list name (simplify-path (pkg-directory name #:cache h)))))))

(define (alternate-racket-clcl/clcp alternate-racket pkgs-dirs-cache)
  (define (use-current-racket n)
    (define pkgs-dirs
      (cond
        [(unbox pkgs-dirs-cache) => values]
        [else
         (define pkgs-dirs (compute-pkg-dirs))
         (set-box! pkgs-dirs-cache pkgs-dirs)
         pkgs-dirs]))
    (values (current-library-collection-links)
            (current-library-collection-paths)
            pkgs-dirs))
  (cond
    [alternate-racket
     (cond
       [(file-exists? alternate-racket)
        (define result-port (open-output-string))
        (define success?
          (parameterize ([current-output-port result-port]
                         [current-error-port (open-output-nowhere)]
                         [current-input-port (open-input-string "")])
            (define get-info-expression
              `(write
                (let loop ([exp
                            (list (current-library-collection-links)
                                  (current-library-collection-paths)
                                  ,compute-pkg-dirs-code)])
                  (cond
                    [(pair? exp) (cons (loop (car exp)) (loop (cdr exp)))]
                    [(hash? exp) (for/hash ([(k v) (in-hash exp)])
                                   (values (loop k)
                                           (loop v)))]
                    [(path? exp) `#s(pth ,(path->string exp))]
                    [else exp]))))
            (system* alternate-racket
                     "-l" "racket/base"
                     "-l" "pkg/lib"
                     "-e" (format "~s" get-info-expression))))
        (cond
          [success?
           (struct pth (p) #:prefab)
           (define (convert-back exp)
             (let loop ([exp exp])
               (cond
                 [(pth? exp) (string->path (pth-p exp))]
                 [(pair? exp) (cons (loop (car exp)) (loop (cdr exp)))]
                 [(hash? exp) (for/hash ([(k v) (in-hash exp)])
                                (values (loop k)
                                        (loop v)))]
                 [else exp])))
           (define ip (open-input-string (get-output-string result-port)))
           (define links/paths
             (convert-back
              (with-handlers ([exn:fail:read? (λ (x) #f)])
                (read ip))))
           (define okay-values? (list/c current-library-collection-links-info/c
                                        current-library-collection-paths-info/c
                                        pkg-dirs-info/c))
           (cond
             [(okay-values? links/paths)
              (values (list-ref links/paths 0)
                      (list-ref links/paths 1)
                      (list-ref links/paths 2))]
             [else (use-current-racket 0)])]
          [else (use-current-racket 1)])]
       [else (use-current-racket 2)])]
    [else (use-current-racket 3)]))

(module+ test
  (require rackunit
           racket/list
           racket/contract
           racket/match)
  
  (define/contract find-completions/c
    (-> string? (listof (list/c string? path?)) (-> path? (listof path?)) (-> path? boolean?)
        (listof (list/c string? path?)))
    find-completions-collection/internal)
  
  (define coll-table
    `(("racket" ,(string->path "/plt/pkgs/compatibility-pkgs/compatibility-lib/racket"))
      ("racket" ,(string->path "/plt/pkgs/draw-pkgs/draw-lib/racket"))
      ("racket" ,(string->path "/plt/racket/collects/racket"))
      ("rackunit" ,(string->path "plt/pkgs/gui-pkgs/gui-lib/rackunit"))))
  
  (define (dir-list d)
    (match (path->string d)
      ["/plt/racket/collects/racket"
       (map string->path '("list.rkt" "info.rkt" "include.rkt" "init.rkt" "gui"))]
      ["/plt/racket/collects/racket/gui"
       (map string->path '("dynamic.rkt"))]
      ["/plt/pkgs/draw-pkgs/draw-lib/racket"
       (map string->path '("gui"))]
      ["/plt/pkgs/draw-pkgs/draw-lib/racket/gui"
       (map string->path '("draw.rkt"))]
      ["plt/pkgs/gui-pkgs/gui-lib/rackunit"
       (map string->path '("something.rkt"))]
      [_ '()]))
  
  (define (dir-exists? d)
    (and (member (path->string d)
                 '("/plt/racket/collects/racket"
                   "/plt/racket/collects/racket/gui"
                   "/plt/pkgs/draw-pkgs/draw-lib/racket"
                   "/plt/pkgs/draw-pkgs/draw-lib/racket/gui"
                   "plt/pkgs/gui-pkgs/gui-lib/rackunit"))
         #t))
  
  (check-equal?
   (find-completions/c "rack/" coll-table dir-list dir-exists?)
   `(("gui" ,(string->path "/plt/pkgs/draw-pkgs/draw-lib/racket/gui"))
     ("gui" ,(string->path "/plt/racket/collects/racket/gui"))
     ("include.rkt" ,(string->path "/plt/racket/collects/racket/include.rkt"))
     ("info.rkt" ,(string->path "/plt/racket/collects/racket/info.rkt"))
     ("init.rkt" ,(string->path "/plt/racket/collects/racket/init.rkt"))
     ("list.rkt" ,(string->path "/plt/racket/collects/racket/list.rkt"))
     ("something.rkt" ,(string->path "plt/pkgs/gui-pkgs/gui-lib/rackunit/something.rkt"))))
  
  (check-equal?
   (find-completions/c "racke/" coll-table dir-list dir-exists?)
   `(("gui" ,(string->path "/plt/pkgs/draw-pkgs/draw-lib/racket/gui"))
     ("gui" ,(string->path "/plt/racket/collects/racket/gui"))
     ("include.rkt" ,(string->path "/plt/racket/collects/racket/include.rkt"))
     ("info.rkt" ,(string->path "/plt/racket/collects/racket/info.rkt"))
     ("init.rkt" ,(string->path "/plt/racket/collects/racket/init.rkt"))
     ("list.rkt" ,(string->path "/plt/racket/collects/racket/list.rkt"))))
  
  (check-equal?
   (find-completions/c "rack" coll-table dir-list dir-exists?)
   coll-table)
  
  (check-equal?
   (find-completions/c "racku" coll-table dir-list dir-exists?)
   (list (last coll-table)))
  
  (check-equal?
   (find-completions/c "racket/i" coll-table dir-list dir-exists?)
   (list (list "include.rkt" (string->path "/plt/racket/collects/racket/include.rkt"))
         (list "info.rkt" (string->path "/plt/racket/collects/racket/info.rkt"))
         (list "init.rkt" (string->path "/plt/racket/collects/racket/init.rkt"))))
  
  (check-equal?
   (find-completions/c "racket/" coll-table dir-list dir-exists?)
   (list (list "gui" (string->path "/plt/pkgs/draw-pkgs/draw-lib/racket/gui"))
         (list "gui" (string->path "/plt/racket/collects/racket/gui"))
         (list "include.rkt" (string->path "/plt/racket/collects/racket/include.rkt"))
         (list "info.rkt" (string->path "/plt/racket/collects/racket/info.rkt"))
         (list "init.rkt" (string->path "/plt/racket/collects/racket/init.rkt"))
         (list "list.rkt" (string->path "/plt/racket/collects/racket/list.rkt"))))
  
  (check-equal?
   (find-completions/c "racket/g" coll-table dir-list dir-exists?)
   (list (list "gui" (string->path "/plt/pkgs/draw-pkgs/draw-lib/racket/gui"))
         (list "gui" (string->path "/plt/racket/collects/racket/gui"))))
  
  (check-equal?
   (find-completions/c "racket/gui/d" coll-table dir-list dir-exists?)
   (list (list "draw.rkt" (string->path "/plt/pkgs/draw-pkgs/draw-lib/racket/gui/draw.rkt"))
         (list "dynamic.rkt" (string->path "/plt/racket/collects/racket/gui/dynamic.rkt"))))

  (check-equal?
   (find-completions/c "r/g/d" coll-table dir-list dir-exists?)
   (list (list "draw.rkt" (string->path "/plt/pkgs/draw-pkgs/draw-lib/racket/gui/draw.rkt"))
         (list "dynamic.rkt" (string->path "/plt/racket/collects/racket/gui/dynamic.rkt")))))
