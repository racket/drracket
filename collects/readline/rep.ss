
(current-prompt-read 
 (let ([read (require-library "pread.ss" "readline")])
   (lambda ()
     (read (lambda (n) (if (zero? n) "> " "  "))))))
