;; program.ss
;; Handles cormac's program first-order structure for units
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
;; Global environment stuff

(define global-def-env '())
(define global-result (void))
(define global-defs-parsed (void))

;; ----------------------------------------------------------------------

(define (analyze-program file)
  ;; file should be normalized
  (let ([file (normalize-path file)])
    (pretty-debug `(analyze-program ,file , (path-only (normalize-path file))))
    (init-constructor-env!)
    (initialize-language!)
    (init-expand!)
    (clear-file-time-cache!)
    (let*-vals 
      ( [sexps (zodiac:read* (open-code-file file) file)]
        [_ (when debugging-front
             (printf "~n--Loaded program------------------~n")
             (for-each 
               (lambda (sexp) (pretty-print-debug (zodiac:stripper sexp)))
               sexps)
             (printf "----------------------------------~n"))]
        [(defs free-names)
          (parameterize ([current-load-relative-directory (path-only file)])
            (my-scheme-expand-program sexps))]
        [_ (set! global-defs-parsed defs)]
        [_ (when debugging-front
             (printf "~n--Parsed program------------------~n")
             (for-each
               (lambda (def) (pretty-print-debug (zodiac:stripper def)))
               defs)
             (printf "----------------------------------~n"))]
        [init-env (get-default-bindings free-names)])

      (pretty-debug `(init-env ,(atenv->pretty init-env)))
      ;; Now do analyis 

      (let-values ([(env refs result) (top-level-traverse-defs defs init-env)])

        (pretty-debug '(fixing up envs))
        (set! global-def-env env)
        (set! global-result result)
        ;;(assert (null? refs) refs (map zodiac:binding-var (map car refs)))
        '(for-each
           (lambda (ref)
             (mrspidey:warning 
               (format "Unbound variable ~s" (zodiac:binding-var (car ref)))))
           refs)

        ;; --- Fix up type variables
        ;;(set! global-def-env (append global-def-env global-tdef-env))
        (report-duplicate-defs global-tdef-env)
        (report-unbound-vars 
          (connect-refs-defs global-tref-env global-tdef-env))
        (connect-bangs-defs global-tbang-env global-tdef-env)

        (when (not (st:whole-program))
          (put-context-top-s defs env result))

        ;;(show-stat-small)

        defs))))

;; ----------------------------------------------------------------------

(define (put-context-top-s defs env result)
  (mrspidey:progress "Partial program analysis")
  ;;(show-stat-small)
  (when result (new-leq-top-s! (FlowType->Tvar result)))
  (when (and defs env)
    (for-each
      (match-lambda
        [($ zodiac:define-values-form _ _ _ _ refs)
          (for-each
            (lambda (ref)
              (new-leq-top-s! 
                (FlowType->Tvar
                  (atenv:lookup env (zodiac:varref-binding ref)))))
            refs)]
        [_ (void)])
      defs)))

;; ---------------------------------------------------------------------- 
;; initialize-analysis!

(define (initialize-analysis!)
  (pretty-debug '(initialize-analysis!))
  (init-constructor-env!)
  (let ([old-kernel (save-kernel-state)])
    (init-kernel!)
    ;; First free old stuff
    (when (st:zero-old-constraint-sets) (free-kernel-state! old-kernel)))
  (init-global-tenv! '() '() '())
  (init-common-AV!)
  (pretty-debug '(analysis-initialized)))

;; ======================================================================

(define (show-global)
  (list 'DEF (atenv->pretty global-def-env)
        ;;'TDEF (atenv->pretty global-tdef-env)
    ))

;; ----------------------------------------------------------------------



