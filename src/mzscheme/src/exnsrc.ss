
#|

Initial symbols are struct types. A non-initial symbol is a struct
type without fields or subtypes. Square brackets are struct fields, *
indicates MzScheme-specific (the star is not in the name), strings are
types/comments.  Except for exn:misc, only terminal types are
instantiated.

|#

(exn [message "string" "error message" 
	      debug-info "anything" "value returned by the current debug info handler (called immediately after the error is detected)"] -
     (user [] "raised by calling \\scmfirst{error}")
     
     (syntax [expr "S-expression" "illegal expression (or \\scm{\\#f} if unknown)"]
	     "all syntax errors, but not \\scmfirst{read} errors")

     (variable [id "identifier" "the unbound variable's global identifier"]
	       "unbound global variable at run-time")

     (application [value "error-specific" "the error-specific inappropriate value"] -
		  (non-procedure [] "application of a non-procedure")
		  (arity [expected 
			  "arity"
			  "the correct procedure arity as returned by \\scmfirst{arity}"]
			 "application with the wrong number of arguments")
		  (type [expected "symbol" "name of the expected type"]
			"wrong argument type to a procedure, not including divide-by-zero or platform-specific integer range errors")
		  (range [] -
			 (bounds [min "integer" "minimum legal index" 
				       max "integer" "maximum legal index" ] -
				       (vector [] "illegal vector index; raised by \\scmfirst{vector-ref}, \\scm{vector-set!}\\scmindex{vector-set\"!}")
				       (string [] "illegal string index; raised by \\scmfirst{string-ref}, \\scm{string-set!}\\scmindex{string-set\"!}, \\scmfirst{substring}")
				       (struct [] "illegal struct index; raised by \\scmfirst{struct-ref}"))
			 (list [] "illegal list index; raised by \\scmfirst{list-ref}, \\scmfirst{list-tail}"))
		  (list-sizes [] "mismatched list sizes; raised by \\scmfirst{map}, \\scmfirst{for-each}, \\scmfirst{make-struct-case}")
		  (map-arity [provided "non-negative integer" "the number of lists provided for mapping"]
			     "arity of supplied procedure does not match the number of lists provided to \\scmfirst{map}, \\scmfirst{for-each}, \\scmfirst{andmap}, \\scmfirst{ormap}")
		  (integer []
			   "integer out of machine-specific range; raised by \\scmfirst{make-semaphore}, \\scm{set-thread-weight!}\\scmindex{set-thread-weight\"!}, \\scmfirst{seconds->date}")
		  (list []
			"encountered a non-pair in a value expected to be a proper list, or a non-pair item in a value expected to be an association list; raised by \\scmfirst{member}, \\scmfirst{memv}, \\scmfirst{memq}, \\scmfirst{assoc}, \\scmfirst{assv}, \\scmfirst{assq}\\footnote{This exception is raised instead of a type exception because an error will not be signalled if a result is determined before processing the entire list.}")
#|
		  (*struct-type-conflict [conflicts-with "struct predicate procedure" "previously-specified entailing procedure"] 
					 "error in the struct predicate list: it cannot be checked in the order given due to sub-typing constraints, or the same predicate is specified twice; raised by \\scmfirst{make-struct-case}")
|#
		  (math [] -
			(zero [] "divide by zero, etc.; raised by \\scmfirst{/}, \\scmfirst{quotient}, \\scmfirst{remainder}, \\scmfirst{modulo}, \\scmfirst{angle}; \\scm{application-value} is always zero")
			(infinity [] "infinity overflows; never raised by MzScheme")
			(negative [] "illegal negative input (e.g. \\scmfirst{sqrt}); never raised by MzScheme")
			(radix [] "unsupported radix (positive fixnum); raised by \\scmfirst{number->string}"))
		  (*mode-conflict 
		   [filename "string" "filename being opened"]
		   "incompatible mode flags; raised by \\scmfirst{open-output-file}, \\scmfirst{call-with-output-file}, \\scmfirst{with-output-to-file}")
		  (*file-position [] "position was provided, but the port was not a file port; raised by \\scmfirst{file-position}; \\scm{application-value} is the port")
		  (*fprintf [] -
			    (*extra-arguments 
			     [extras "list of values" "arguments without formatting tags"]
			     "arguments without corresponding formatting tags; \\scm{application-value} is the format string")
			    (*no-argument 
			     [] 
			     "formatting tags without corresponding arguments; \\scm{application-value} is the format string")
			    (*argument-type
			     [expected "symbol" "type expected by formatting tag"]
			     "argument does not match type expected by formatting tag; \\scm{application-value} is the bad value")))

     (*else [] "fall-through in \\scmfirst{cond} or \\scmfirst{case}")

     (*struct [] -
	     (*struct-type [value "anything except structure type" "the given supertype value"]
			   "the supertype expression in a \\scmfirst{struct} form returned a value that was not a structure type value"))

     (*object [] -
	     (*class-type [value "anything except class" "the value provided as a superclass"]
			  "the superclass expression in \\scmfirst{class*} returned a non-class")
	     (*interface-type [value "anything except interface" "the value provided as an interface"]
			 "an interface expression in \\scmfirst{class*} or \scmfirst{interface} returned a non-interface")
	     (*generic [object "object" "the object"]
		       "the object was not an instance of the generic's class or interface; raised by generic procedures")
	     (*inherit [ivar "identifier" "instance variable name"] 
		      "inherited ivar not found; raised by \\scmfirst{class*}")
	     (*implement [ivar "identifier" "instance variable name"] 
			 "ivar required by an interface not found in the implementation; raised by \\scmfirst{class*}")
	     (*class-ivar [class  "class" "the class"
				 ivar "identifier" "instance variable name"] 
			 "instance variable not found in class; raised by \\scmfirst{uq-make-generic}")
	     (*interface-ivar [interface  "interface" "the interface"
				 ivar "identifier" "instance variable name"] 
			 "instance variable not found in interface; raised by \\scmfirst{uq-make-generic}")
	     (*ivar [object "object" "the object" 
			   ivar "identifier" "instance variable name"]
		   "instance variable not found; raised by \\scmfirst{uq-ivar}")
	     (*private-class [class "class" "the class"] 
			    "cannot instantiate or derive from this primitive class; raised by \\scmfirst{make-object}, \\scmfirst{class*}")
	     (*init [object "object" "the object"
			    class "class" "the class that called a superclass initializer too much or not enough"] -
		   (*multiple [] "the superclass intialization proc (e.g., \\scmfirst{super-init}) called twice for the class")
		   (*never [] "the superclass intialization proc (e.g., \\scmfirst{super-init}) never called for the class")))
     
     (*unit [] -
	      (*non-unit [value "anything" "non-unit value"] "attempt to link a non-unit; raised by \\scmfirst{compound-unit}, \\scmfirst{invoke-unit}, \\scmfirst{invoke-open-unit}")
	      (*arity [unit "unit" "badly linked unit"] 
		      "attempt to link a unit with the wrong number of imported variables; raised by \\scmfirst{compound-unit}")
	      (*import [in-unit "unit" "importing unit"
			     out-unit "unit" "source unit"
			     in-tag "identifier" "importing unit tag"
			     out-tag "identifier" "source unit tag"
			     name "identifier" "import variable name"] 
		       "cannot find variable to import; raised by \\scmfirst{compound-unit}")
	      (*export [unit "unit" "source unit"
			     tag "identifier" "source unit tag"
			     name "identifier" "source variable name"] 
		       "cannot find variable to export for compound unit; raised by \\scmfirst{compound-unit}")
	      (*invoke [] -
		       (*variable [name "identifier" "bad identifier"]
				  "cannot invoke with import of an undefined global variable"))
	      (*signature [] -
			  (*non-signed-unit [value "anything" "non-signed-unit value"]
					    "attempt to link a non-signed-unit; raised by \\scmfirst{compound-unit/sig}, \\scmfirst{invoke-unit/sig}, \\scmfirst{invoke-open-unit/sig}")
			  (*arity [unit "signed-unit" "signed-unit"] "attempt to link signed unit with the wrong number of imported units; raised by \\scmfirst{compound-unit/sig}")
			  (*match [dest-context "string" "destination context"
						src-context "string" "source context"
						variable "string" "mismatch variable name"] -
				  (*missing [] "signature mismatch (missing variable) detected by \\scmfirst{verify-signature-match}")
				  (*extra [] "signature mismatch (extra variable) detected by \\scmfirst{verify-signature-match}")
				  (*kind [] "signature mismatch (mismatched kinds) detected by \\scmfirst{verify-signature-match}"))))

     (read [port "port" "port being read"] -
	   (+paren [] "unexpected close paren")
	   (+number [input "string" "input number string"] "bad number syntax")
	   (+char [input "string" "input character string without \\scmch{}"] 
		 "bad character constant")
	   (+eof [expected "string" "expected input"] 
		"unexpected end of file")
	   (+dot [] "dot at top-level or illegal use of dot; e.g., \\scm{( .\\ 1)}")
	   (+unsupported [input "string" "input string without \\scm{\\#}"] 
			"unsupported \\scm{\\#} tag")
	   (*vector-length [input "string" "vector length string"]
			   "bad vector size in vector constant")
	   (*compiled  [] "illegal MzScheme compiled code")
	   (*graph [] "bad graph syntax"))
     
     (i/o [] -
	  (read [port "input port" "port with read error"]
		"error reading from a port")
	  (write [port "output port" "port with write error"]
		 "error writing to a port")
	  (filesystem [pathname "string" "pathname"] -
		      (+path [] "bad pathname; raised by all file procedures that take a pathname"
			     (+username [] "bad username (Unix only); raised by all file procedures that take a pathname"))
		      (+file [] "file not found; raised by procedures that open files")
		      (+directory [] "directory not found; raised by \\scmfirst{current-directory}, \\scmfirst{current-load-directory}, \\scmfirst{directory-list}, \\scmfirst{load-with-cd}")
		      (+collection [] "collection not found; raised by \\scmfirst{require-library}, \\scmfirst{require-relative-library}")
		      (+file-exists [] "cannot overwrite file; raised by \\scmfirst{open-output-file}"))
	  (port-closed [port "port" "port for attempted operation"]
		       "attempt to operate on a closed port; raised by \\scmfirst{read}, \\scmfirst{write}, \\scmfirst{display}, \\scmfirst{print}, \\scmfirst{char-ready?}")
	  (*user-port [port "input port" "user-defined input port"] 
		      "user-defined input port returned a non-character from the character-getting procedure")
	  (*tcp [] "TCP errors"
		(*connect [address "string" "address to connect to"
				   port-id "integer" "port to connect to"]
			  "tcp-connect low-level failure")
		(*listen [port-id "integer" "port to listen on"]
			 "tcp-listen low-level failure")
		(*accept [listener "listener" "listener to accept from"]
			 "tcp-accept low-level failure")
		(*listener-closed [listener "listener" "listener that has been closed"]
				  "listener closed for \\scmfirst{tcp-accept}, \\scmfirst{tcp-accept-ready?}, \\scmfirst{tcp-stop}")))

     (misc [] "low-level errors"
	   (unsupported [] "unsupported feature")
	   (user-break [] "asynchronous thread break")
	   (out-of-memory [] "out of memory")
	   (*parameterization [value "anything except a parameterization" "bad value"]
			      "the parameterization branch handler or sharing procedure returned a non-parameterization; raised by \\scmfirst{thread}, \\scmfirst{make-parameterization-with-sharing}")
	   (*defmacro [value "anything except a procedure" "the given macro handler"] 
		      "macro handler is not a procedure")
	   (*expansion-time [] "\\scmfirst{local-expansion-time-value}, \\scmfirst{global-expansion-time-value}, \\scmfirst{local-expand-defmacro}, or \\scmfirst{local-expand-body-expression} called at run-time")
	   (*constant [id "identifier" "constant identifier"] "attempt to change a constant global; raised by \\scm{set!}\\scmindex{set\"!}, \\scmfirst{define}, \\scmfirst{undefine}, \\scmfirst{invoke-open-unit}")
	   (*continuation [] "attempt to cross a continuation boundary or apply another thread's continuation")
	   (*thread [] -
		    (*kill [] "cannot kill the thread; raised by \\scmfirst{kill-thread}"))
	   (*semaphore [] "a semaphore's maximum internal count has been exceeded")
	   (*hash-table [key "anything" "key for failed lookup"] "failed hash table lookup")
	   (*regexp [] "all regular expression errors")
	   (*process [] "error executing an operating system process")
	   (*dynamic-extension [name "string" "dynamic extension pathname"] -
			       (*open [] "cannot open dynamic extension")
			       (*version [] "dynamic extension is wrong version")
			       (*initialize [] "cannot initialize dynamic extension"))
	   (*image [name "string" "image pathname"]
		   "error dumping or restoring an image; raised by \\scmfirst{write-image-to-file}, \\scmfirst{read-image-from-file}")))


