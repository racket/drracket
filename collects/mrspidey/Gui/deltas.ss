; deltas.ss
; Loads configuration from .Xresources
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

;; Given a resource name, this returns a style delta corresponding to the 
;; attributes of the file or the default if there are none.

(define (get-resource-maybe s2 default)
  (let ([val-box (box default)])
    (wx:get-resource 
     (if (wx:colour-display?) "mrspidey" "mrspidey-bw")
     s2 
     val-box)
    (unbox val-box)))

(define delta-eval-namespace (make-namespace))
(parameterize ([current-namespace delta-eval-namespace])
    (invoke-open-unit/sig wx@ wx))

(define delta-add-string! 
  (lambda (delta string)
  (let ([p (open-input-string string)])
    (recur loop ()
      (let ([e (read p)])
        (unless (eof-object? e)
          (apply (uq-ivar delta (car e)) (eval `(list ,@(cdr e)) delta-eval-namespace))
          (loop)))))))

(define set-resource-delta
  (lambda (name default-string default-string-bw delta)
    (delta-add-string! delta 
                       (if (wx:colour-display?) 
                           default-string 
                           default-string-bw))
    (delta-add-string! delta (get-resource-maybe name ""))))

(define make-resource-delta
  (lambda (name default-string default-string-bw)
    (let ([delta (make-object wx:style-delta% 
                              wx:const-change-alignment wx:const-align-top)])
      '(set-resource-delta "base-delta"
                          "(set-delta wx:const-change-normal) \
                           (set-delta wx:const-change-family wx:const-modern) \
                           (set-alignment-on wx:const-align-top) "
                          "(set-delta wx:const-change-normal) \
                           (set-delta wx:const-change-family wx:const-modern) \
                           (set-alignment-on wx:const-align-top)"
                          delta)
      (set-resource-delta name default-string default-string-bw delta)
      delta)))

;; ----------------------------------------------------------------------
;; These deltas are defined when the application starts, either from defaults
;; or from resources, their names and their strings agree.

(define base-delta
  (make-resource-delta "base-delta" 
                       "(set-delta wx:const-change-normal) \
                           (set-delta wx:const-change-family wx:const-modern) \
                           (set-alignment-on wx:const-align-top) "
                          "(set-delta wx:const-change-normal) \
                           (set-delta wx:const-change-family wx:const-modern) \
                           (set-alignment-on wx:const-align-top)"))

(define normal-delta
  (make-resource-delta "normal-delta"
                       "" ""))

(define type-link-delta 
  (make-resource-delta "type-link-delta"
                       "(set-delta wx:const-change-bold)"
                       "(set-delta wx:const-change-bold)"))
(define type-delta
  (make-resource-delta "type-link-delta"
                       ""
                       ""))

(define check-delta 
  (make-resource-delta "check-delta"
                       "(set-delta-foreground \"RED\")"
                       "(set-delta wx:const-change-underline 1)"))
(define uncheck-delta
  (make-resource-delta "uncheck-delta"
                       "(set-delta-foreground \"FORESTGREEN\")"
                       ""))

(define check-link-delta
  (make-resource-delta "check-link-delta" 
                       "(set-delta-foreground \"BLUE\") \
                        (set-delta wx:const-change-underline 1)"
                       "(set-delta wx:const-change-underline 1)"))

;; ----------------------------------------------------------------------
