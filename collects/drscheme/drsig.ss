(define-signature drscheme:export^
  (console))

(define-signature drscheme^
  ((open mred:application^)))

(define-signature drscheme:load/link-tool^
  (load/link-tool))

(define-signature drscheme:frame^
  (scheme-project-frame%))

(define-signature drscheme:edit^
  (mzscheme-console-edit%))

(define-signature drscheme:project^
  (scheme-project-frame%
   (open mred:application^)))

(define-signature drscheme:check^
  (advance-check%
   beginner-check%))

(define-signature drscheme:setup^
  (do-setup))

(define-signature drscheme:spawn^
  (spawned-process-console-frame%
   spawned-process-console-edit%))

(define-signature mrspidey^
  (create-menu-bar-hook))
(define-signature donkey^
  (create-menu-bar-hook))

;; for the checker -- the checker doesn't work right now
'(define-signature drscheme:aries^
  (sequence-contents lexeme-object lexeme? sequence? lexeme-type
		     lexeme-start-column lexeme-start-line
		     paren-column paren-line sequence-start-paren
		     sequence-end-paren
		     parse-error set!-parse-error
		     set!-top-level parse-place
		     set!-checking? read))
