
(begin-elaboration-time
 (require-library "refer.ss"))

(begin-elaboration-time 
 (require-library "invoke.ss"))

(begin-elaboration-time
 (define-values/invoke-unit (send*
			     local
			     recur
			     rec
			     evcase
			     nor
			     nand
			     signature->symbols)
   (require-library "macroxr.ss")))

