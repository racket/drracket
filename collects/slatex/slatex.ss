(require-library "file.ss")

(define slatex
  (let ([ns (make-namespace)])
    (parameterize ([current-namespace ns])
      (require-library "slatexsrc.ss" "slatex")
      (global-defined-value 'slatex::*texinputs* #f)
      (global-defined-value 'slatex::*texinputs-list* #f))
    (lambda (file)
      (unless (or (file-exists? file) (file-exists? (string-append file ".tex")))
	(error 'slatex "~e does not exist" file))
      (let ([file (normalize-path file)])
	(let-values ([(base name dir?) (split-path file)])
	  (parameterize ([current-namespace ns]
			 [current-directory
			  (if (string? base)
			      base
			      (current-directory))])
	    (eval `(slatex::process-main-tex-file ,name))))
	(case (system-type)
	  [(macos)
	   (system "OTEX")

           ;; boy, wouldn't it be great if the "actv" appleevent worked for OTEX?
	   ;(send-event "OTEX" "misc" "acvt")
           (let ([oztex-location (build-path (car (filesystem-root-list))
                                             "Applications"
                                             "OzTeX"
                                             "OzTeX")])
             (when (file-exists? oztex-location)
               (with-handlers ([void void]) ;; mzscheme cannot handle result
                 (send-event "MACS" "aevt" "odoc" (vector 'file oztex-location)))))
             
	   (send-event "OTEX" "aevt" "odoc" (vector 'file file))]
	   [(windows unix)
	    (system (format "latex ~a" file))])))))
