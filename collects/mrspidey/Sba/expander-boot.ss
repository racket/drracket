;; expander-boot.ss
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
(printf "Loading expander-boot.ss~n")

(define __keep-mrspidey-annotations #t)
(load "~cormac/scheme/mzschemerc.ss")
(match:set-error-control 'error)

(defmacro assert args '(void))
(defmacro begin-test-case args  `(begin ,@args))
;(defmacro type: args `(type:-quote (quote ,args)))
(defmacro primitive: args `(type: ,@args))

(defmacro include-exp (f) 
  (let ([p (open-input-file f)])
    (begin0 
      (read p)
      (close-input-port p))))

;; ---------- structures --------------------------------------------------
;; field = (: sym type) | (! sym type) | sym
;; field-name = sym

(define (struct-expander-fn struct: const?)
  (#%let ([make-exn make-exn:syntax]
          [debug debug-info-handler])
  (#%lambda body
     (#%let* ([syntax-error
               (#%lambda (s)
                         (#%raise
                          (make-exn
                           (#%format "define-struct: ~a" s)
                           ((debug))
                           (#%cons 'define-struct body))))]
              [field->field-name
               (match-lambda
                [((or ': '!) sym _) sym]
                [(? symbol? sym) sym]
                [x (syntax-error (format "Bad field ~s" x))])]
              [field-mutable?
               (match-lambda
                [('! sym _) #t]
                [_ (not const?)])]
              [build-struct-names
               (#%lambda (name fields)
                         (#%let ([name (#%symbol->string name)]
                                 [+ #%string-append])
                                (#%map #%string->symbol
                                       (#%append
                                        (#%list 
                                         (+ "struct:" name)
                                         (+ "make-" name)
                                         (+ name "?"))
                                        (#%apply
                                         #%append
                                         (#%map
                                          (#%lambda (field) 
                                                    (let ([f
                                                           (symbol->string
                                                            (field->field-name field))])
                                                      (cons
                                                       (+ name "-" f)
                                                       (if (field-mutable? field)
                                                           (list (+ "set-" name "-" f "!"))
                                                           '()))))
                                          fields))))))])
	    (#%or (#%pair? body)
		  (syntax-error "empty declaration"))
	    (#%or (#%= 2 (#%length body))
		  (syntax-error "wrong number of parts"))
	    (#%or (#%symbol? (#%car body))
		  (#%and (#%pair? (#%car body))
			 (#%symbol? (#%caar body))
			 (#%pair? (#%cdar body))
			 (#%null? (#%cddar body)))
		  (syntax-error "first part must be an identifier or identifier-expression pair"))
	    (#%or (#%list? (#%cadr body))
		  (syntax-error "improper field list"))
	    (#%let* ([name (#%if (#%symbol? (#%car body))
			      (#%car body)
			      (#%caar body))]
		  [fields (#%cadr body)])
	      `(#%define-values ,(build-struct-names name fields)
                                (,struct: ,@body)))))))

;(#%define-macro define-const-typed-structure (struct-expander-fn '#%const-typed-structure #t))
(#%define-macro define-const-typed-structure (struct-expander-fn '#%typed-structure #f))
(#%define-macro define-typed-structure (struct-expander-fn '#%typed-structure #f))

(printf "expander-boot.ss done~n")
