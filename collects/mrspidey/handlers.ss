; handlers.ss 
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

;; (printf "Loading handlers.ss~n")

(define mrspidey:error
  (lambda args
    (apply (mrspidey:error-handler) args)))

(define mrspidey:error-handler
  (make-parameter
    (case-lambda
      [(message object)
        (unless (zodiac:zodiac? object)
          (printf "Bad object in mrspidey:error-handler ~s~n" object)
          ((mrspidey:error-handler) message))
        (let* ([loc (zodiac:zodiac-start object)])
          (unless (zodiac:location? loc)
            (printf "Bad location in mrspidey:error-handler ~s~n" loc)
            ((mrspidey:error-handler) message))
          (error 'MrSpidey "~a"
            (format "~a at ~s line ~s, column ~s~n"
              message
              (file-name-from-path (zodiac:location-file loc))
              (zodiac:location-line loc)
              (zodiac:location-column loc))))]
      [(message)
        (error 'MrSpidey "~a" (format "~a~n" message))])
    (lambda (x) x)))

(define mrspidey:internal-error error)

;; ----------------------------------------------------------------------

(define mrspidey:add-summary
  (lambda line
    ;; line is: 
    ;; str
    ;; str loc word-no
    (apply (mrspidey:add-summary-handler) line)))

(define mrspidey:add-summary-handler
  (make-parameter
    (match-lambda*
      [(str loc word-no) 
        (cond
          [(zodiac:location? loc)
            (printf "~a at line ~s, file ~s~n" 
              str 
              (zodiac:location-line loc)
              (file-name-from-path (zodiac:location-file loc)))]
          [(zodiac:zodiac? loc)
            (mrspidey:add-summary str (zodiac:zodiac-start loc) word-no)]
          [else
            (printf "Bad location in mrspidey:add-summary-handler ~s~n" loc)
            (mrspidey:add-summary str)])]
      [(str . _) (printf "~a~n" str)])
    (lambda (x) x)))

(define (mrspidey:warning . line)
  (apply (mrspidey:warning-handler) line))

(define mrspidey:warning-handler
  (make-parameter
   (match-lambda*
    [(str loc word-no) 
     (mrspidey:add-summary (format "Warning: ~a" str) loc (add1 word-no))]
    [(str) 
     (mrspidey:add-summary (format "Warning: ~a" str))])
   (lambda (x) x)))

;; ----------------------------------------------------------------------

(define mrspidey:progress
  (lambda args
    (apply (mrspidey:progress-handler) args)))

(define default-mrspidey:progress-handler
  (let ([current ()]
        [fresh-line #t])
    (letrec 
        ([f (match-lambda*
             [((? string? name) line)
              (unless (equal? name current)
                (f 'fresh-line)
                (set! current name)
                (mrspidey:progress-output name))
              (mrspidey:progress-output (format "[~s]" line))
              (flush-output)
              (set! fresh-line #f)]
             [((? string? str)) 
              (f 'fresh-line)
              (mrspidey:progress-output str)
              (f #\newline)]
             [(#\newline)
              (mrspidey:progress-output (format "~n"))
              (set! fresh-line #t)]
             [('fresh-line)
              (unless fresh-line (f #\newline))])])
      f)))

(define mrspidey:progress-handler
  (make-parameter default-mrspidey:progress-handler (lambda (x) x)))

;; ----------------------------------------------------------------------
;; Don't really use the following flexibility, but might as well keep.

(define (mrspidey:progress-output  str)
  ((mrspidey:progress-output-handler) str))

(define mrspidey:progress-output-handler
  (make-parameter (lambda (str) (display str) (flush-output)) 
                  (lambda (x) x)))

;; ----------------------------------------------------------------------

(define record-analyzed-file-hook
  (make-parameter
   (lambda (filename . _)
     (printf "Record-analyzed-file ~s~n" filename))
   (lambda (x) x)))

(define (record-analyzed-file . args)
  (apply (record-analyzed-file-hook) args))

;; ----------------------------------------------------------------------

;(trace mrspidey:warning)
;(trace mrspidey:error)
;(trace mrspidey:internal-error)
