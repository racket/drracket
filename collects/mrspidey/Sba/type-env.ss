;; type-env.ss
;; Handles the type environment (env for type inference)
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
;; ----------------------------------------------------------------------

;; ======================================================================
;; Global type env

(define global-tref-env '())
(define global-tdef-env '())
(define global-tbang-env '())

(define (add-global-tref! sym tvar)
  (set! global-tref-env (cons (cons sym tvar) global-tref-env)))
(define (add-global-tdef! sym tvar)
  (set! global-tdef-env (cons (cons sym tvar) global-tdef-env)))
(define (add-global-tbang! sym tvar)
  (set! global-tbang-env (cons (cons sym tvar) global-tbang-env)))
(define (init-global-tenv! r d b)
  (set! global-tref-env r)
  (set! global-tdef-env d)
  (set! global-tbang-env b))

;; ----------------------------------------------------------------------
;; find-matching-def

(define (find-matching-def sym defs)
  (ormap (match-lambda
          [(sym2 . def) (and (eq? sym sym2) def)])
         defs))

;; ----------------------------------------------------------------------
;; report-duplicate-defs
;; Report duplicate definitions

(define (report-duplicate-defs env)
  (pretty-debug `(report-duplicate-defs ,@(map car env)))
  (recur loop ([env env][dupls '()])
    (match env
      [() (void)]
      [((sym1 . _) . rest)
       (if (and (find-matching-def sym1 rest)
                (not (memq sym1 dupls)))
           (begin
             (mrspidey:warning
              (format "Duplicate definition of variable ~s" sym1))
             (loop rest (cons sym1 dupls)))
           (loop rest dupls))])))

;;----------------------------------------------------------------------
;; report-unbound-vars-env

(define report-unbound-vars
  (case-lambda
   [(ref-env) (report-unbound-vars ref-env "")]
   [(ref-env msg)
    (for-each
     (match-lambda
      [(sym . _) 
        (mrspidey:warning (format "Unbound type variable ~s~a" sym msg))])
     ref-env)]))

;; ----------------------------------------------------------------------
;; connect-refs-defs
;; Connects refs to defs, returns list of unbound refs

(define (connect-refs-defs ref-env def-env)
  (pretty-debug
   `(connect-refs-defs ,(map car ref-env) ,(map car def-env)))
  (filter
   (match-lambda
    [(and r (sym . tvar))
     (match (find-matching-def sym def-env)
       [#f #t]
       [ftype
         (new-edge! (FlowType->Tvar ftype) tvar)
        #f])])
   ref-env))

;; ----------------------------------------------------------------------
;; combine-def-bang
;; Connects each bang to def, and reports bangs to undefd vars

(define (connect-bangs-defs bang-env def-env)
  (pretty-debug 
   `(connect-bangs-defs ,(map car bang-env) ,(map car def-env)))
  (for-each
   (match-lambda
    [(s2 . tvar2)
     (match (find-matching-def s2 def-env)
       [#f (mrspidey:warning
            (format "Assignment to undefined variable ~s" s2))]
       [tvar1
         (unless (Tvar? tvar1) 
           (mrspidey:error "Assignment to annotated type"))
         (assert (Tvar? tvar2))
         (new-edge! tvar2 tvar1)])])
   bang-env))



