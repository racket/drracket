
(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'unit/sig)

(undefine 'a)
(undefine 'b)

(syntax-test '(define-signature))
(syntax-test '(define-signature))
(syntax-test '(define-signature 8))
(syntax-test '(define-signature . x))
(syntax-test '(define-signature x))
(syntax-test '(define-signature 8))
(syntax-test '(define-signature x (8)))
(syntax-test '(define-signature x (a . 8)))
(syntax-test '(define-signature x (a . y)))
(syntax-test '(define-signature x (y y)))
(syntax-test '(define-signature x ((y))))
(syntax-test '(define-signature x ((struct))))
(syntax-test '(define-signature x ((struct y))))
(syntax-test '(define-signature x ((struct . y))))
(syntax-test '(define-signature x ((struct y . x))))
(syntax-test '(define-signature x ((struct y x))))
(syntax-test '(define-signature x ((struct y (x)) . x)))
(syntax-test '(define-signature x ((unit))))
(syntax-test '(define-signature x ((unit y))))
(syntax-test '(define-signature x ((unit . y))))
(syntax-test '(define-signature x ((unit y : a))))
(define-signature a ())
(syntax-test '(define-signature x ((unit y a))))
(syntax-test '(define-signature x ((unit y . a))))
(syntax-test '(define-signature x ((unit y : . a))))
(syntax-test '(define-signature x ((unit y a) . x)))
(syntax-test '(define-signature x (y (unit y a))))

(syntax-test '(unit/sig))
(syntax-test '(unit/sig 8))
(syntax-test '(unit/sig b))
(define-signature b (x y))
(syntax-test '(unit/sig (a)))
(syntax-test '(unit/sig a (impLort)))
(syntax-test '(unit/sig a (impLort) 5))
(syntax-test '(unit/sig a import 5))
(syntax-test '(unit/sig a (import . x) . 5))
(syntax-test '(unit/sig a (import (x) 8) 5))
(syntax-test '(unit/sig a (import (x) . i) 5))
(syntax-test '(unit/sig a (import (i : a) . b) 5))
(syntax-test '(unit/sig b (import (i : a)) 5))
(syntax-test '(unit/sig a (import (i : a x)) 5))
(syntax-test '(unit/sig a (import (i : a) x) 5))
(syntax-test '(unit/sig b (import (i : a)) (define x 7)))
(syntax-test '(unit/sig b (import (i : a)) (define x 7) (define i:y 6)))
(syntax-test '(unit/sig blah (import) (define x 7)))

(syntax-test '(unit/sig () (import) (begin)))
(syntax-test '(unit/sig () (import) (begin 1 . 2)))
(syntax-test '(unit/sig () (import) (begin (define x 5)) (define x 5)))

(define b@ (unit/sig b (import) (define x 9) (define y 9)))
(define b2@ (unit/sig b (import (i : a)) (define x 9) (define y 9)))
(define b3@ (unit/sig b (import (i : ())) (define x 9) (define y 9)))
(define b3u@ (unit/sig b (import ()) (define x 9) (define y 9)))
(define b3u2@ (unit/sig b (import a) (define x 9) (define y 9)))
(define-signature >b ((unit b@ : b)))
(define b3u3@ (unit/sig b (import (i : >b)) (define x 9) (define y 9)))

(define >b@ (compound-unit/sig (import) (link [b@ : b (b@)]) (export (unit b@))))

(syntax-test '(compound-unit/sig))
(syntax-test '(compound-unit/sig 8))
(syntax-test '(compound-unit/sig b))
(syntax-test '(compound-unit/sig (import) (link) (export (var (U x)))))
(syntax-test '(compound-unit/sig (import a) (link) (export)))
(syntax-test '(compound-unit/sig (import 5) (link) (export)))
(syntax-test '(compound-unit/sig (import . i) (link) (export)))
(syntax-test '(compound-unit/sig (import (i : a)) (link ()) (export)))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@)) (export)))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ b)) (export)))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b)) (export)))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b ())) (export)))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@ 5))) (export)))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@ . i))) (export)))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@ (i . a)))) (export)))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@ (i a a)))) (export)))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@ c@))) (export)))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@ (c@ a)))) (export)))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export . b@)))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export b@)))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit))))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit c@))))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit b@ : c))))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit b@ (b@)))))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit b@ : (b@)))))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var))))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (open))))
(error-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@ (i : a)))) (export)) exn:unit:signature:arity?)
(error-test '(compound-unit/sig (import (i : a)) (link (b@ : b (5 (i : a)))) (export)) exn:unit:signature:non-signed-unit?)
(error-test '(compound-unit/sig (import (i : b)) (link (b@ : b (b3@ (i : b)))) (export)) exn:unit:signature:match:extra?)
(error-test '(compound-unit/sig (import (i : b)) (link (b@ : b (b3u@ (i : b)))) (export)) exn:unit:signature:match:extra?)
(error-test '(compound-unit/sig (import (i : b)) (link (b@ : b (b3u2@ (i : b)))) (export)) exn:unit:signature:match:extra?)
(error-test '(compound-unit/sig (import (i : >b)) (link (b@ : b (b3@ (i : >b)))) (export)) exn:unit:signature:match:extra?)
(error-test '(compound-unit/sig (import (i : ((open a) x))) (link (b@ : b (b3@ (i : ((open a) x))))) (export)) exn:unit:signature:match:extra?)
(error-test '(compound-unit/sig (import (i : ((unit b@ : ((open b) w))))) (link (b@ : b (b3u3@ i))) (export)) exn:unit:signature:match:extra?)
(error-test '(compound-unit/sig (import (i : a)) (link (b@ : (w) (b@))) (export)) exn:unit:signature:match:missing?)
(error-test '(compound-unit/sig (import (i : ())) (link (b@ : b (b3u3@ i))) (export)) exn:unit:signature:match:missing?)
(error-test '(compound-unit/sig (import (i : ((unit b@ : ())))) (link (b@ : b (b3u3@ i))) (export)) exn:unit:signature:match:missing?)
(error-test '(compound-unit/sig (import (i : (b@))) (link (b@ : b (b3u3@ i))) (export)) exn:unit:signature:match:kind?)
(error-test '(compound-unit/sig (import (i : ((unit b@ : (x (unit y : ())))))) (link (b@ : b (b3u3@ i))) (export)) exn:unit:signature:match:kind?)
(syntax-test '(compound-unit/sig (import) (link [b@ : b (0 5)]) (export)))
(syntax-test '(compound-unit/sig (import) (link [b@ : b (0 ())]) (export)))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : a (5 (i : b)))) (export)))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var b@))))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var (b@)))))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var (b@ x y)))))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var (5 x)))))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var (b@ 5)))))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var ((b@ w) 5)))))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var ((b@ 7) 5)))))
(syntax-test '(compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var (b@ x . a)))))

(syntax-test '(compound-unit/sig (import) (link (A : () (0 A))) (export))) ; self-import
(syntax-test '(compound-unit/sig (import) (link (A : (x) (0 A))) (export))) ; self-import

(test #t unit/sig? (unit/sig a (import)))
(test #t unit/sig? (unit/sig b (import) (define x 1) (define y 2)))
(test #t unit/sig? (unit/sig a (import (i : b)) i:x))
(test 5 (lambda (f) (invoke-unit/sig f ())) (unit/sig a (import ()) 5))
(test #t unit/sig? (unit/sig (x) (import) (begin (define x 5))))
(test #t unit/sig? (unit/sig (x) (import) (define a 14) (begin (define x 5) (define y 10)) (define z 12)))
(test #t unit/sig? (compound-unit/sig (import) (link) (export)))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export)))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b2@ (i : a)))) (export)))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b2@ ((i) : a)))) (export)))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b2@ ((i) : ())))) (export)))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var (b@ x)))))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var (b@ x) w))))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (var ((b@) x) w))))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit b@))))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit (b@)))))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (unit b@ b@))))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (open b@))))
(test #t unit/sig? (compound-unit/sig (import (i : a)) (link (b@ : b (b@))) (export (open (b@ : b)))))

; Include:

(define i1@
  (unit/sig 
   ()
   (import)
   
   (include "uinc.ss")))

(test 9 'include (invoke-unit/sig i1@))

(define i2@
  (unit/sig 
   ()
   (import)
   
   (include "uinc.ss")
   (include "uinc2.ss")
   (include "uinc.ss")
   (+ x 2)))

(test 10 'include (invoke-unit/sig i2@))

; Simple:

(define-signature m1^
  (x y a? set-a-b!))

(define m1@
  (unit/sig 
   m1^
   (import)
   
   (define-struct a (b c))

   (define x 7)
   (define z 8)
   (define y (lambda () (* z x)))

   (list x y z)))

(test #t apply (lambda (x y z) (and (= x 7) (= z 8) (procedure? y) (= 0 (arity y))))
      (invoke-unit/sig m1@))

(test #t apply
      (lambda (x y-val a? set-a-b!)
	(and (= x 7) (= y-val 56)
	     (= 1 (arity a?))
	     (= 2 (arity set-a-b!))))
      (invoke-unit/sig
       (compound-unit/sig 
	(import)
	(link [M@ : m1^ (m1@)]
	      [N@ : () ((unit/sig
			 ()
			 (import (i@ : m1^))
			 (list i@:x (i@:y) i@:a? i@:set-a-b!))
			M@)])
	(export (open M@)))))

; More:

(define-signature m2-1-lite^
  (x struct:a v y))

(define-signature m2-1^
  (a?
   (open m2-1-lite^)))

(define-signature m2-2^
  (x? make-x x-z both))

(define m2-1@
  (unit/sig 
   m2-1^
   (import)

   (define x 5)
   (define-struct a (b c))
   (define v (make-a 5 6))
   (define (y v) (a? v))))

(define m2-2@
  (unit/sig
   m2-2^
   (import m2-1^)

   (define-struct (x struct:a) (y z))
   (define both (lambda (v)
		  (and (a? v) (x? v))))))

(define-signature m2-3^
  (simple))

(let-signature m2-3^
  ((unit one@ : m2-1-lite^)
   (unit two@ : m2-2^)
   a?-again)

  (define m2-3@
    (compound-unit/sig
     
     (import)
     (link [O@ : m2-1^ (m2-1@)]
	   [T@ : m2-2^ (m2-2@ O@)])
     (export (unit (O@ : m2-1-lite^) one@)
	     (unit T@ two@)
	     (var (O@ a?) a?-again))))

  (let ([p (open-output-string)]
	[filter (lambda (v)
		  (if (procedure? v)
		      `(proc: ,(inferred-name v))
		      v))])
    (invoke-unit/sig
     (compound-unit/sig
      (import)
      (link [M@ : m2-3^ (m2-3@)]
	    [N@ : () ((unit/sig
		       ()
		       (import (i : m2-3^))
		       (display (map
				 filter
				 (list i:one@:x i:one@:v i:one@:struct:a i:one@:y 
				       i:two@:make-x i:two@:x? i:two@:x-z i:two@:both
				       i:a?-again))
				p)
		       (let ([v2 (i:two@:make-x 1 2 3 4)])
			 (display (map
				   filter
				   (list i:one@:x (struct-type? i:one@:struct:a)
					 i:one@:v (i:one@:y i:one@:v) (i:one@:y i:one@:x)
					 v2
					 (i:one@:y v2)
					 (i:two@:x? v2)
					 (i:two@:both i:one@:v)
					 (i:two@:both v2)))
				  p)))
		      M@)])
      (export)))
    (test (string-append "(5 #(struct:a 5 6) #<struct-type> (proc: y)"
			 " (proc: make-x) (proc: x?)"
			 " (proc: x-z) (proc: both) (proc: a?))"
			 "(5 #t #(struct:a 5 6) #t #f #(struct:x 1 2 3 4) #t #t #f #t)")
	  get-output-string p)))

(test 5 'let-sig
      (invoke-unit/sig
       (unit/sig
	m2-3^
	(import)
	(define simple 5)
	simple)))

(define-signature big^
  (a b c))
(define-signature little^
  (a b c))

(test 11
      'link-restrict 
      (invoke-unit/sig
       (compound-unit/sig
	(import)
	(link [a@ : big^ ((unit/sig big^ (import) (define a 5) (define b 6) (define c 7)))]
	      [b@ : () ((unit/sig () (import (i : little^)) (+ i:a i:b)) 
			(a@ : little^))])
	(export))))

(define-signature just-a^
  (a))
(define-signature >just-a^
  ((unit s@ : just-a^)))

; Test a path for linking: root is a constiuent
(test 12
      'link-path
      (invoke-unit/sig
       (compound-unit/sig
	(import)
	(link [a@ : >just-a^ ((compound-unit/sig
			       (import)
			       (link [i@ : just-a^ ((unit/sig 
						     just-a^ 
						     (import) 
						     (define a 5)))])
			       (export (unit i@ s@))))]
	      [r@ : () ((unit/sig 
			 () 
			 (import (i : just-a^))
			 (+ i:a 7))
			(a@ s@))])
	(export))))

; Test a path for linking: root is an import
(test 12
      'import-path
      (invoke-unit/sig
       (compound-unit/sig
	(import)
	(link [a@ : >just-a^ ((compound-unit/sig
			       (import)
			       (link [i@ : just-a^ ((unit/sig 
						     just-a^ 
						     (import) 
						     (define a 5)))])
			       (export (unit i@ s@))))]
	      [u@ : () ((compound-unit/sig
			 (import (a@ : >just-a^))
			 (link [r@ : () ((unit/sig 
					  () 
					  (import (i : just-a^))
					  (+ i:a 7))
					 (a@ s@))])
			 (export))
			a@)])
	(export))))

; Export var from embedded unit:

(define-signature e ((unit w : (embedded-v))))
(invoke-open-unit/sig
 (compound-unit/sig
  (import)
  (link [E : e ((compound-unit/sig
		 (import)
		 (link [w : (embedded-v) ((unit/sig (embedded-v)
						    (import)
						    (define embedded-v 0)))])
		 (export (unit w))))])
  (export (var ((E w) embedded-v)))))
(test 0 'embedded-v embedded-v)

; Signature ordering

(define o1 (unit/sig (num sym) (import) (define num 5) (define sym 'a)))
(define o2 (unit/sig () (import (sym num)) (list sym (+ num))))

(test (list 'a 5)
      'order
      (invoke-unit/sig
       (compound-unit/sig
	(import)
	(link [one : (num sym) (o1)]
	      [two : () (o2 one)])
	(export))))

; unit->unit/sig, etc.

(define-signature s1
  (a b c))
(define-signature s2
  (+))

(define us1 
  (unit
   (import +)
   (export a b c)
   
   (define a 1)
   (define b 2)
   (define c 3)
   (+ a b c)))
   
(test 6 'u->s (invoke-unit us1 +))
(test 6 'u->s (invoke-unit/sig (unit->unit/sig us1 (s2) s1) s2))

; Exporting a name twice:

(syntax-test
 '(compound-unit/sig
   (import)
   (link [A : (a) ((unit/sig (a) (import) (define a 1)))])
   (export (var (A a)) (open A))))

(syntax-test
 '(compound-unit/sig
   (import)
   (link [A : (a) ((unit/sig (a) (import) (define a 1)))]
	 [B : (b) ((unit/sig (b) (import) (define b 2)))])
   (export (unit A x) (unit B x))))

(syntax-test
 '(compound-unit/sig
   (import)
   (link [A : (a) ((unit/sig (a) (import) (define a 1)))]
	 [B : (b) ((unit/sig (b) (import) (define b 2)))])
   (export (unit A) (unit B A))))

; Shadowed syntax definitions:

(test 8 'unit/sig (invoke-unit/sig (unit/sig () (import) (define lambda 8) lambda)))
(test 9 'unit/sig (invoke-unit/sig (unit/sig () (import) (begin (define lambda 9) (define lambda2 lambda)) lambda2)))

(report-errs)

