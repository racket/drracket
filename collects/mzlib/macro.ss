
(begin-elaboration-time 
 (require-library "invoke.ss"))

(begin-elaboration-time
 (define-values/invoke-unit (class-asi
			     class*-asi
			     opt-lambda
			     send*
			     local
			     recur
			     rec
			     evcase
			     nor
			     nand
			     signature->symbols)
   (require-library "macror.ss"))
 (define-values/invoke-unit (let+)
   (require-library "letplusr.ss")))

(define-macro class-asi class-asi)
(define-macro class*-asi class*-asi)
(define-macro opt-lambda opt-lambda)
(define-macro let+ let+)
(define-macro send* send*)
(define-macro local local)
(define-macro recur recur)
(define-macro rec rec)
(define-macro evcase evcase)
(define-macro nor nor)
(define-macro nand nand)
(define-macro signature->symbols signature->symbols)

