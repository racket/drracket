;; prefs.ss - loads preferences
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

(define parameter-radio-boxes
  (lambda (name param sym p major-dim direction)
    (mred:set-preference-default sym (param) 
      (lambda (x)
        (with-handlers ((exn? (lambda (exn) #f)))
          (param x)
          #t)))
    (param (mred:get-preference sym))
    (let* ([o
             (make-object
               mred:radio-box% p 
               (lambda (bx event)
                 ;;(printf "~s~n" (param '?))
                 (match
                   (list-ref (param '?)
                     (send event get-command-int))
                   [(tag . _) 
                     (param tag)
                     (mred:set-preference sym tag)]))
               name 
               -1 -1 -1 -1 
               (map cadr (param '?))
               major-dim
               direction)]
            [pairs
              (map
                (match-lambda [(tag name . _) (cons name tag)])
                (param '?))]
            [default-ndx
              (recur loop ([n 0][pairs pairs])
                (cond 
                  [(null? pairs)
                    (error 'make-parameter-menu 
                      "Can't find para in pairs ~s" (param))]
                  [(equal? (param) (cdar pairs)) n]
                  [else (loop (add1 n) (cdr pairs))]))])
      (send o stretchable-in-x #t)
      (send o set-selection default-ndx)
      o
      )))

(define parameter-check-box
  (lambda (name param sym p)
    (mred:set-preference-default sym (param) 
      (lambda (x)
        (with-handlers ((exn? (lambda (exn) #f)))
          (param x)
          #t)))
    (param (mred:get-preference sym))
    (let* ( [hp (make-object mred:horizontal-panel% p)]
            [o
              (make-object
                mred:check-box% hp 
                (lambda (bx event)
                  ;;(printf "~s~n" (param '?))
                  (match
                    (list-ref (param '?)
                      (send event get-command-int))
                    [(tag . _) 
                      (param tag)
                      (mred:set-preference sym tag)]))
                name 
                -1 -1 -1 -1)]
            [_ (make-object mred:horizontal-panel% hp)])
      (send o set-value (param))
      o
      )))

;; ======================================================================
;; MrSpidey Type Display

(define callbacks-sdl-alg-changed '())

(define (add-callback-sdl-alg-changed! fn)
  (set! callbacks-sdl-alg-changed (cons fn callbacks-sdl-alg-changed)))

(define (remq-callback-sdl-alg-changed! fn)
  (set! callbacks-sdl-alg-changed (remq fn callbacks-sdl-alg-changed)))

(define (sdl-alg-changed)
  (for-each 
    (lambda (f) (f)) 
    callbacks-sdl-alg-changed))

(define (param-ctrls-sdl-alg param)
  (lambda args
    (sdl-alg-changed)
    (apply param args)))

;; ======================================================================

(mred:set-preference-default 'st:const-merge-size (st:const-merge-size) 
  (lambda (x)
    (with-handlers ((exn? (lambda (exn) #f)))
      (st:const-merge-size x)
      #t)))
(st:const-merge-size (mred:get-preference 'st:const-merge-size))

(define mrspidey-mk-analysis-pref-panel
  (lambda (panel)
    (let*
      ( [p (make-object mred:vertical-panel% panel)]
        
        [vp (make-object mred:vertical-panel% p -1 -1 -1 -1)]
        [_ (parameter-check-box
             "Accurate constant types"
             st:constants 'st:constants
             vp)]
        [g (make-object mred:slider% vp 
             (lambda (slider event)
               (st:const-merge-size (send event get-command-int))
               (mred:set-preference 'st:const-merge-size 
                 (send event get-command-int)))
             "Constant merge size"
             (st:const-merge-size)
             1 100 
             100)]
        [_ (send g enable #t)]
        [_ (parameter-check-box
             "If splitting"
             st:if-split 'st:if-split
             vp)]
        [_ (parameter-check-box
             "Flow sensitivity"
             st:flow-sensitive 'st:flow-sensitive
             vp)]
        [_ (parameter-check-box
             "Accurate analysis of numeric operations"
             st:numops 'st:numops
             vp)]
        [_2 (parameter-radio-boxes
              "Polymorphism:"
              st:polymorphism
              'st:polymorphism
              p 0 wx:const-horizontal)]

        [vp (make-object mred:vertical-panel% p -1 -1 -1 -1)]
        [vphp (make-object mred:horizontal-panel% vp)]
        [_0 (make-object mred:message% vphp
              "Polymorphism simplification algorithms:")]
        [vphphp (make-object mred:horizontal-panel% vphp)]
        [_1 (parameter-radio-boxes
              "        "
              st:constraint-simplification-poly
              'st:constraint-simplification-poly
              vp 0 wx:const-vertical)]

        [_ (parameter-radio-boxes
             "Save .za files in:"
             st:save-za-in
             'st:save-za-in
             p
             0 wx:const-horizontal)]
        )
        
      p)))

(mred:add-preference-panel
  "MrSpidey Analysis"
  mrspidey-mk-analysis-pref-panel)

(mrspidey-mk-analysis-pref-panel
  (make-object mred:horizontal-panel%
    (make-object mred:frame% '() "dummy")))

;; ======================================================================

;(mred:set-preference-default 'st:sdl-size-k (st:sdl-size-k))
;(st:sdl-size-k (mred:get-preference 'st:sdl-size-k))

(define (indented-vertical-radio-box p name param sym)
  (let*
    ( [vp (make-object mred:vertical-panel% p -1 -1 -1 -1)]
      [vphp1 (make-object mred:horizontal-panel% vp)]
      [_0 (make-object mred:message% vphp1 name)]
      [vphp2 (make-object mred:horizontal-panel% vp)]
      [spc (make-object mred:horizontal-panel% vphp2)]
      [_ (send spc user-min-width 20)]
      [_ (send spc stretchable-in-x #f)]
      [radio-box
        (parameter-radio-boxes
          '()
          param sym
          vphp2 0 wx:const-vertical)]
      [_ (send radio-box stretchable-in-x #t)]
      [_ (make-object mred:horizontal-panel% vphp2)])
    (void)))

(define mrspidey-mk-type-display-prefs-panel
  (lambda (panel)
    (let*
      ( [p (make-object mred:vertical-panel% panel )])

      (let* 
        (
          [sdl-fo-container-panel
            (make-object mred:horizontal-panel% p)]
          [sdl-fo-sub-panel
            (make-object mred:horizontal-panel% p)]
          [spc (make-object mred:horizontal-panel% sdl-fo-sub-panel)]
          [_ (send spc user-min-width 20)]
          [_ (send spc stretchable-in-x #f)]
          [sdl-fo-sub-sub-panel
            (make-object mred:vertical-panel% sdl-fo-sub-panel)]
          [see-ivars-panel
            (parameter-check-box
              "Show instance variables"
              (param-ctrls-sdl-alg st:sdl-fo-ivars)
              'st:sdl-fo-ivars
              sdl-fo-sub-sub-panel)]
          [see-struct-fields-panel
            (parameter-check-box
              "Show structure fields"
              (param-ctrls-sdl-alg st:sdl-fo-struct-fields)
              'st:sdl-fo-struct-fields
              sdl-fo-sub-sub-panel)] 
          [_ (parameter-radio-boxes
                "Show types as:"
               (match-lambda*
                 [('?) (st:sdl-fo '?)]
                 [() (st:sdl-fo)]
                 [(x)
                   (sdl-alg-changed)
                   (let ([enable-sub-controls (eq? x 'basic-types)])
                     (for-each
                       (lambda (control)
                         (send control enable enable-sub-controls))
                       (list 
                         see-ivars-panel
                         see-struct-fields-panel)))
                   (st:sdl-fo x)])
               'st:sdl-fo
                sdl-fo-container-panel
               0 wx:const-horizontal)])
        (void))
        
      (indented-vertical-radio-box p 
        "Constraint simplification algorithms:" 
        (param-ctrls-sdl-alg st:sdl-constraint-simplification)
        'st:sdl-constraint-simplification)

      (parameter-radio-boxes
        "Type naming:"
        (param-ctrls-sdl-alg st:naming-strategy)
        'st:naming-strategy
        p 0 wx:const-horizontal)
      (parameter-radio-boxes
        "Primitive types:"
        (param-ctrls-sdl-alg st:primitive-types)
        'st:primitive-types
        p 0 wx:const-horizontal)

      (let* 
        (
          [st:expand-output-type-container-panel
            (make-object mred:horizontal-panel% p)]
          [st:expand-output-type-sub-panel
            (make-object mred:horizontal-panel% p)]
          [spc (make-object mred:horizontal-panel% 
                 st:expand-output-type-sub-panel)]
          [_ (send spc user-min-width 20)]
          [_ (send spc stretchable-in-x #f)]
          [sdl-tidy-object
            (parameter-check-box
              "Uses equivalences that make types tidy"
              (param-ctrls-sdl-alg st:sdl-tidy)
              'st:sdl-tidy
              st:expand-output-type-sub-panel)] 
          [_ (parameter-check-box
               "Use equivalences to simplify types"
               (match-lambda*
                 [('?) (st:expand-output-type '?)]
                 [() (st:expand-output-type)]
                 [(x)
                   (sdl-alg-changed)
                   (send sdl-tidy-object enable x)
                   (st:expand-output-type x)])
               'st:expand-output-type
               st:expand-output-type-container-panel)])
        (void))

      p)))

(mred:add-preference-panel
  "MrSpidey Type Display"
  mrspidey-mk-type-display-prefs-panel)

(mrspidey-mk-type-display-prefs-panel
  (make-object mred:horizontal-panel%
    (make-object mred:frame% '() "dummy")))

;; ======================================================================

'(define (make-parameter-menu parameter)
   (let* ([pairs
            (map
              (match-lambda [(tag name . _) (cons name tag)])
              (parameter '?))]
           [default-ndx
             (recur loop ([n 0][pairs pairs])
               (cond 
                 [(null? pairs)
                   (error 'make-parameter-menu "Can't find para in pairs")]
                 [(equal? (parameter) (cdar pairs)) n]
                 [else (loop (add1 n) (cdr pairs))]))])
     (let ([menu (make-object mred:menu%)])
       (send menu append-check-set pairs parameter default-ndx)
       ;;(parameter (cdar pairs))
       menu)))



