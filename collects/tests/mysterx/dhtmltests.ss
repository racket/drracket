;;; dhtmltests.ss -- DHTML tests for MysterX

(require-library "mysterx.ss" "mysterx")

(define wb (make-object mx-browser% "DHTML tests" 300 300 
			'default 'default '(maximize)))

(define doc (send wb current-document))

(send doc insert-html "<P id=text>This is some text</P>")

(define txt (send doc find-element "P" "text"))

(define (test-prop getter setter expected)
  (printf "Checking ~a~n" getter)
  ((ivar/proc txt setter) expected)
  (let ([got ((ivar/proc txt getter))])
    (unless (equal? got expected)
	    (printf "~a: Expected ~a, got ~a~n" 
		    getter expected got))))

(define tests
 `((font-family set-font-family! ("monospace" "fantasy"))
   (font-size set-font-size! xx-large)
   (font-style set-font-style! oblique)
   (font-variant set-font-variant! small-caps)
   (font-weight set-font-weight! bolder)
   (background-attachment set-background-attachment! fixed)
   (background-image 
    set-background-image! 
    "http://www.cs.rice.edu/CS/PLT/packages/drscheme/logo.gif")
   (background-repeat set-background-repeat! no-repeat)
   (background-position set-background-position! (right bottom))
   (background-position-x set-background-position-x! 
			  ,(make-css-length 42 'em))
   (background-position-y set-background-position-y! 
	      ,(make-css-percentage 95))
   (letter-spacing set-letter-spacing! normal)
   (letter-spacing set-letter-spacing! 
		   ,(make-css-length 20 'pt))
   (vertical-align set-vertical-align! super)
   (text-decoration set-text-decoration! (underline line-through))
   (text-decoration-underline set-text-decoration-underline! #t)
   (text-decoration-overline set-text-decoration-overline! #t)
   (text-decoration-linethrough set-text-decoration-linethrough! #t)
   (text-decoration-blink set-text-decoration-blink! #t)
   (color set-color! red)
   (background-color set-background-color! orange)
   (pixel-top set-pixel-top! 27)
   (pixel-left set-pixel-left! 99)
   (pixel-width set-pixel-width! 99)
   (pixel-height set-pixel-height! 199)
   (overflow set-overflow! scroll)
   (pos-top set-pos-top! 13.0)
   (pos-left set-pos-left! 17.0)
   (pos-width set-pos-width! 188.0)
   (text-transform set-text-transform! uppercase)
   (text-align set-text-align! justify)
   (text-indent set-text-indent! ,(make-css-length 50 'pt))
   (line-height set-line-height! ,(make-css-percentage 200))
   (margin set-margin! (auto ,(make-css-length 70 'pt) auto auto))
   (margin-top set-margin-top! ,(make-css-length 70 'pt))
   (margin-bottom set-margin-bottom! auto)
   (margin-left set-margin-left! auto)
   (margin-right set-margin-right! ,(make-css-percentage 200))
   (pagebreak-before set-pagebreak-before! always)
   (pagebreak-after set-pagebreak-after! always)
   (cursor set-cursor! help)
   (padding set-padding! ,(list (make-css-length 70 'pt) (make-css-percentage 300)))
   (padding-top set-padding-top! ,(make-css-length 30 'em))
   (padding-bottom set-padding-bottom! ,(make-css-length 3 'cm))
   (padding-left set-padding-left! ,(make-css-length 3 'ex))
   (padding-right set-padding-right! ,(make-css-length 70 'mm))
   (border set-border! (blue ,(make-css-length 6 'pt) solid))
   (border-top set-border-top! (red ,(make-css-length 8 'pt) dashed))
   (border-bottom set-border-bottom! (green ,(make-css-length 4 'pt) dotted))
   (border-left set-border-left! (pink thick dotted))
   (border-right set-border-right! (black thin dashed))
   (border-color set-border-color! orange)
   (border-top-color set-border-top-color! cyan)
   (border-bottom-color set-border-bottom-color! darkseagreen)
   (border-left-color set-border-left-color! goldenrod)
   (border-right-color set-border-right-color! purple)
   (border-width set-border-width! ,(make-css-length 20 'pt))
   (border-top-width set-border-top-width! ,(make-css-length 15 'pt))
   (border-bottom-width set-border-bottom-width! ,(make-css-length 15 'pt))
   (border-left-width set-border-left-width! ,(make-css-length 15 'pt))
   (border-right-width set-border-right-width! ,(make-css-length 15 'pt))
   (border-bottom-width set-border-bottom-width! ,(make-css-length 30 'pt))
   (border-left-width set-border-left-width! ,(make-css-length 30 'em))
   (border-right-width set-border-right-width! ,(make-css-length 1 'in))
   (border-style set-border-style! solid)
   (border-top-style set-border-top-style! none)
   (border-bottom-style set-border-bottom-style! dashed)
   (border-left-style set-border-left-style! dotted)
   (border-right-style set-border-right-style! none)
   (style-float set-style-float! left)
   (display set-display! list-item)
   (list-style-type set-list-style-type! lower-roman)
   (list-style-position set-list-style-position! inside)
   (visibility set-visibility! hidden)
   (clip set-clip! 
	(,(make-css-length 2 'cm) auto 
	,(make-css-length 5 'in) auto))
   (clip set-clip! 
	(,(make-css-length 2 'cm) auto 
	,(make-css-length 5 'in) auto))
   (style-float set-style-float! left)	
   (clear set-clear! both)
   (width set-width! ,(make-css-percentage 50))
   (height set-height! ,(make-css-percentage 50))
   (top set-top! auto)
   (left set-left! auto)
   (z-index set-z-index! 4)))

(for-each
 (lambda (t)
   (apply test-prop t))
 tests)

; filter test

(define filter-spec
  '(glow (strength 99) (enabled #t) (color "#ff00ff")))

(apply (ivar/proc txt 'set-filter!) filter-spec)

(let ([result (send txt filter)]) 
  (if (equal? result filter-spec)
      (printf "Checking filter~n")
      (error (format "filter test: Expected ~a, got ~a~n"
		     filter-spec result))))

