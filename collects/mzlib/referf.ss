
 (unit
  (import)
  (export (make-require-unit require-unit) (make-require reference-file))
  
  (define make-require-unit
   ; require-unit, etc.
   (lambda (must-string? require? reqrel? sig? sname)
     (lambda names
       (let ([len (length names)]
	     [expect (if require? +inf.0 1)])
	 (unless (and (positive? len) (<= len expect))
		 ((raise-syntax-error sname
				      (format "expected ~a names; given ~a"
					      (if (= +inf.0 expect)
						  "some"
						  expect)
					      len)
				      (list* sname names)))))
       (let ([names (if must-string?
			(map local-expand-defmacro names)
			names)])
	 (unless (or (not must-string?))
		 (for-each
		  (lambda (s)
		    (unless s
			    (raise-syntax-error sname
						"name is not a string"
						(list* sname names)
						s)))
		  names))
	 `(#%let ([result (,(if require?
				(if reqrel? 
				    '#%require-relative-library/proc
				    '#%require-library/proc)
				'#%load/use-compiled) ,@names)])
		 (#%unless (,(if sig?
				 '#%unit/sig?
				 '#%unit?)
			    result)
			   (#%raise
			    (#%make-exn:unit
			     ,(format "~s: result from ~s is not a ~aunit"
				      sname names (if sig? "signed " ""))
			     (#%current-continuation-marks))))
		 result)))))

  (define make-require
   ; require
   (lambda (must-string? require? reqrel?)
     (lambda names
       (let ([sname (if require? 
			(if reqrel?
			    'require-relative-library
			    'require-library)
			'require)]
	     [len (length names)]
	     [expect (if require? +inf.0 1)])
	 (unless (and (positive? len) (<= len expect))
		 ((raise-syntax-error sname
				      (format "expected ~a names; given ~a"
					      expect len)
				      (list* sname names))))
	 (let ([names (if must-string?
			  (map local-expand-defmacro names)
			  names)])
	   (unless (or (not must-string?) (map string? names))
		   (raise-syntax-error sname
				       "filename is not a string"
				       (list* sname names)))
	   `(,(if require? 
		  (if reqrel?
		      'require-relative-library/proc
		      'require-library/proc)
		  '#%load/use-compiled) 
	     ,@names)))))))
