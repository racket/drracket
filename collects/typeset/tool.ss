(unit/sig ()
  (import mred^
	  framework^
	  [drscheme : drscheme:export^]
	  [zodiac : zodiac:system^])

(define read/snips (lambda x (error x)))

(define (snipize obj)
  (if (is-a? obj snip%)
      obj
      (make-string-snip obj)))

(define (make-string-snip obj)
  (let* ([str (format "~a" obj)]
	 [sn (make-object string-snip% (string-length str))])
    (send sn insert str (string-length str) 0)
    sn))

(define void-snip%
  (class snip% () 
    (inherit get-style)
    (override
      [copy
       (lambda ()
         (let ([ans (make-object void-snip%)])
	   (send ans set-style (get-style))
	   ans))])
    (sequence (super-init))))

(define (make-delta family)
  (let ([d (make-object style-delta% 'change-family family)])
    (send d set-size-mult 0)
    (send d set-size-add (preferences:get 'drscheme:font-size))
    ;(send d set-delta-foreground "BLACK")
    d))

(define renderable-editor-snip%
  (class editor-snip% (family color)
    (inherit get-editor get-style)

    (private
      [pen (send the-pen-list find-or-create-pen color 1 'solid)]
      [brush (send the-brush-list find-or-create-brush "BLACK" 'transparent)])

    (inherit get-extent get-inset)
    (rename [super-draw draw])
    (override
     [draw
      (lambda (dc x y left top right bottom dx dy draw-caret)
	(let ([bl (box 0)]
	      [br (box 0)]
	      [bt (box 0)]
	      [bb (box 0)]
	      [bw (box 0)]
	      [bh (box 0)])
	  (get-extent dc x y bw bh #f #f #f #f)
	  (get-inset bl br bt bb)
	  (super-draw dc x y left top right bottom dx dy draw-caret)
          (let ([old-pen (send dc get-pen)]
		[old-brush (send dc get-brush)])
	    (send dc set-pen pen)
	    (send dc set-brush brush)
	    (send dc draw-rectangle
		  (+ x (unbox bl))
		  (+ y (unbox bt))
		  (- (unbox bw) (unbox bl) (unbox br))
		  (- (unbox bh) (unbox bt) (unbox bb)))
	    (send dc set-pen old-pen)
	    (send dc set-brush old-brush))))])

    (override
      [write
       (lambda (stream-out)
	 (send (get-editor) write-to-file stream-out 0 'eof))])
    (override
      [copy
       (lambda ()
         (let ([snip (make-snip)])
           (send snip set-editor (send (get-editor) copy-self))
	   (send snip set-style (get-style))
           snip))])
    (public
      [make-snip (lambda () (error 'make-snip "abstract method"))])

    (public
      [make-editor
       (lambda ()
	 (make-object (drscheme:unit:program-editor-mixin plain-text%) (make-delta family)))])

    (sequence
      (super-init (make-editor) #f))))

(define constant-snip%
  (class* renderable-editor-snip% (zodiac:expands<%>) (family)
    (inherit get-editor)

    (public
      [expand
       (lambda (obj)
	 (zodiac:structurize-syntax
	  `(,replace-in-template
	    ',family
	    ;,this
	    ;,(make-object editor-snip% (get-editor))
	    ,(make-object editor-snip% (send (get-editor) copy-self))
	    ,@(let loop ([snip (send (get-editor) find-first-snip)])
		(cond
		 [(not snip) `()]
		 [(transformable? snip)
		  `(,snip
		    .
		    ,(loop (send snip next)))]
		 [else (loop (send snip next))])))
	  obj))])

    (public
      [get-family (lambda () family)])

    (override
      [write
       (lambda (stream-out)
         (send stream-out << (symbol->string family))
         (send (get-editor) write-to-file stream-out 0 'eof))]
      [make-snip (lambda () (make-object constant-snip% family))])

    (inherit show-border set-snipclass)
    (sequence
      (super-init family "BLUE")
      (show-border #t)
      (set-snipclass constant-snipclass))))

(define constant-snipclass%
  (class snip-class% ()
    (override
      [read
       (lambda (stream-in)
	 (let* ([family (string->symbol (send stream-in get-string))]
                [snip (make-object constant-snip% (if (member family '(roman modern))
                                                      family
                                                      'modern))])
	   (send (send snip get-editor) read-from-file stream-in)
           snip))])
    (sequence (super-init))))
(define constant-snipclass (make-object constant-snipclass%))
(send constant-snipclass set-version 1)
(send constant-snipclass set-classname "robby:constant-snip")
(send (get-the-snip-class-list) add constant-snipclass)

(define evaluated-snip%
  (class* renderable-editor-snip% (zodiac:expands<%>) ()
    (inherit get-editor)

    (public
      [expand
       (lambda (obj)
	 (let ([text (get-editor)])
	   (let* ([loc (zodiac:make-location 0 0 0 text)]
		  [read
		   (zodiac:read
		    (gui-utils:read-snips/chars-from-text text 0 (send text last-position))
		    loc
		    #t 1)])
	     (zodiac:structurize-syntax
	      `(,snipize ,(read))
	      (zodiac:make-zodiac #f loc loc)))))])


;; MATTHEW
;; cannot do this because the styles information in the saved texts screws up.
   (override
     [make-editor
      (lambda ()
	(make-object (drscheme:unit:program-editor-mixin (scheme:text-mixin text:basic%))))])

    (override
      [make-snip (lambda () (make-object evaluated-snip%))])
    
    (inherit show-border set-snipclass)
    (sequence
      (super-init 'modern "RED")
      (show-border #t)
      (set-snipclass evaluated-snipclass))))

(define evaluated-snipclass%
  (class snip-class% ()
    (override
      [read
       (lambda (stream-in)
	 (let* ([snip (make-object evaluated-snip%)]
                [editor (send snip get-editor)])
	   (send editor read-from-file stream-in)
           snip))])
    (sequence (super-init))))

(define evaluated-snipclass (make-object evaluated-snipclass%))
(send evaluated-snipclass set-version 1)
(send evaluated-snipclass set-classname "robby:evaluated-snip")
(send (get-the-snip-class-list) add evaluated-snipclass)

(define plain-text%
  (class text:keymap% ([delta (make-object style-delta%)])
    (inherit change-style copy-self-to)
    (rename [super-after-insert after-insert]
            [super-on-insert on-insert])
    (inherit begin-edit-sequence end-edit-sequence)
    (override
     [copy-self
      (lambda ()
	(let ([t (make-object plain-text% delta)])
	  (copy-self-to t)
	  t))]
     [on-insert
      (lambda (x y)
	(super-on-insert x y)
	(begin-edit-sequence))]
     [after-insert 
      (lambda (x y)
	(super-after-insert x y)
	(change-style delta x (+ x y))
	(end-edit-sequence))])
    (inherit set-styles-sticky)
    (sequence
      (super-init)
      (set-styles-sticky #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                     ;;;
;;;                      EVALUATION                     ;;;
;;;                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (transformable? snip)
  (or (is-a? snip constant-snip%)
      (is-a? snip evaluated-snip%)))

(define typeset-size
  (let ([value (preferences:get 'drscheme:font-size)])
    (case-lambda
     [() value]
     [(x)
      (unless (and (exact? x)
		   (integer? x)
		   (> x 0))
	(error 'typeset-size
	       "expected an exact integer strictly greater than zero"))
      (set! value x)])))

(define (replace-in-template family template-snip . replacements)
  (let* ([delta (make-delta family)]
	 [_ (begin (send delta set-delta-foreground "BLACK")
		   (send delta set-size-mult 0)
		   (send delta set-size-add (typeset-size)))]
	 [text (make-object plain-text% delta)])
    (let loop ([replacements replacements]
	       [snip (send (send template-snip get-editor) find-first-snip)])
      (cond
       [(not snip)
	(unless (null? replacements)
	  (error 'replace-in-template "found end without doing all replacements: ~s" replacements))
	(void)]
       
       [(transformable? snip)
	(when (null? replacements)
	  (error 'replace-in-template "found replacable without replacement"))
	(let ([replacement (car replacements)]
	      [pos (send text get-snip-position snip)])
	  (send text insert (if (is-a? replacement snip%)
				(send replacement copy)
				(make-string-snip replacement))
		(send text last-position) (send text last-position))
	  (loop (cdr replacements)
		(send snip next)))]

       [else
	(send text insert (send snip copy) (send text last-position) (send text last-position))
	(loop replacements (send snip next))]))

    (let ([snip (make-object editor-snip% text #f
			     0 0 0 0
			     0 0 0 0)])
      (send text hide-caret #t)
      (send snip set-tight-text-fit #t)
      (send snip set-align-top-line #t)
      snip)))

(define (typeset-frame-extension super%)
  (class/d super% args
    ((inherit get-editor get-menu-bar get-edit-target-object))

    (apply super-init args)

    (let* ([mb (get-menu-bar)]
	   [menu (make-object menu% "Typeset" mb)]
	   [insert-snip
	    (lambda (make-obj)
	      (let ([editor (get-edit-target-object)])
		(when editor
		  (let loop ([editor editor])
		    (let ([focused (send editor get-focus-snip)])
		      (if (and focused
			       (is-a? focused editor-snip%))
			  (loop (send focused get-editor))
			  (let ([snip (make-obj)])
			    (send editor insert snip)
			    (send editor set-caret-owner snip 'display))))))))])
      (make-object menu-item% "Modern Constant Snip" menu
		   (lambda (menu evt)
		     (insert-snip
		      (lambda () (make-object constant-snip% 'modern))))
		   #\m)
      (make-object menu-item% "Roman Constant Snip" menu
		   (lambda (menu evt)
		     (insert-snip 
		      (lambda () (make-object constant-snip% 'roman))))
		   #\r)
      (make-object menu-item% "Evaluated Snip" menu
		   (lambda (menu evt)
		     (insert-snip 
		      (lambda () (make-object evaluated-snip%))))))

    (frame:reorder-menus this)))

(define utils (invoke-unit/sig (require-library "utils.ss" "typeset")
			       mred^ framework^
			       typeset:utils-input^))

(define (typeset-rep-extension super-text%)
  (class/d super-text% args
    ((override reset-console)
     (rename [super-reset-console reset-console])
     (inherit user-namespace))

    (define (reset-console)
      (super-reset-console)
      (parameterize ([current-namespace user-namespace])
	(global-define-values/invoke-unit/sig typeset:utils^ utils)))

    (apply super-init args)))

(drscheme:get/extend:extend-unit-frame typeset-frame-extension)
(drscheme:get/extend:extend-interactions-text typeset-rep-extension)


)
