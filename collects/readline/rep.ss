
(current-prompt-read 
 (let ([read (require-library "pread.ss" "readline")]
       [orig-read (current-prompt-read)]
       [orig-input (current-input-port)])
   (lambda ()
     (if (eq? (current-input-port) orig-input)
	 (read (lambda (n) (if (zero? n) "> " "  ")))
	 (orig-read)))))

