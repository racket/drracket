
#|

Initial symbols are struct types. A non-initial symbol is a struct
type without fields or subtypes. Square brackets are struct fields, *
indicates especially MzScheme-specific (the star is not in the name),
strings are types/comments.

|#

(exn [message "immutable-string" "error message" 
	      continuation-marks "mark-set" "value returned by \\scmfirst{current-continuation-marks} immediately after the error is detected"] 
     -
     (user [] "raised by calling \\scmfirst{error}")
     
     (variable [id "symbol" "the unbound variable's global identifier"]
	       "unbound global variable at run-time"
	       (*keyword [] "attempt to change the binding of a global keyword"))

     (application [value "value" "the error-specific inappropriate value"] -
		  (arity [expected "arity" "the correct procedure arity as returned by \\scmfirst{arity}"]
			 "application with the wrong number of arguments")
		  (type [expected "symbol" "name of the expected type"]
			"wrong argument type to a procedure, not including divide-by-zero")
		  (mismatch [] "bad argument combination (e.g., out-of-range index for a vector) or platform-specific integer range error")
		  (divide-by-zero [] "divide by zero; \\scm{application-value} is always zero")
		  (*continuation [] "attempt to cross a continuation boundary or apply another thread's continuation"))
     
     (*else [] "fall-through in \\scmfirst{cond} or \\scmfirst{case}")

     (*struct [] "the supertype expression in a \\scmfirst{struct} form returned a value that was not a structure type value")

     (*object [] "object-, class-, or interface-specific error")
     
     (*unit [] "unit- or unit/sig-specific error")

     (syntax [expr "S-expression" "illegal expression (or \\scm{\\#f} if unknown)"]
	     "syntax error, but not a \\scmfirst{read} error")
     
     (read [port "input-port" "port being read"] "\\scm{read} parsing error"
	   (eof [] "unexpected end-of-file"))
     
     (i/o [] -
	  (port [port "port" "port for attempted operation"] -
		(read [] "error reading from a port")
		(write [] "error writing to a port")
		(closed [] "attempt to operate on a closed port")
		(*user [] "user-defined input port returned a non-character from the character-getting procedure"))
	  (filesystem [pathname "path" "file or directory pathname"
				detail "symbol or false" "\\SymbolFirst{ill-formed-path}, \\SymbolFirst{already-exists}, or \\SymbolFirst{wrong-version}, indicating the reason for the exception (if available), or \\scm{\\#f}"] 
		      "illegal pathname or error manipulating a filesystem object")
	  (*tcp [] "TCP errors"))
     
     (thread [] "raised by \\scmfirst{call-with-custodian}")
     
     (misc [] "low-level or MzScheme-specific error"
	   (unsupported [] "unsupported feature")
	   (user-break [continuation "continuation" "a continuation that resumes from the break"] "asynchronous thread break")
	   (out-of-memory [] "out of memory")))
