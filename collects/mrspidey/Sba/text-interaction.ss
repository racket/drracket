;; text-interaction
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
;; ----------------------------------------------------------------------

   (define (mrspidey:error msg . obj-list)
     (error 'Mrspidey msg))
   (define mrspidey:warning
     (case-lambda
      [(str . _) (printf "Warning: ~a~n" str)]
      ;[(str loc . _) (printf "Warning: file ~s, line ~s: ~a~n" 
      ;                       (file-name-from-path (zodiac:location-file loc))
     ;                        (zodiac:location-line loc)
       ;                      str)]
      ))
   (define mrspidey:internal-error error)
   (define (mrspidey:add-summary str . _)
     (printf "~a~n" str))
   (define mrspidey:progress
     (let ([current ()]
           [fresh-line #t])
       (letrec 
           ([f (match-lambda*
                [((? symbol? name) (? number? fraction))
                 (unless (eq? name current)
                   (f 'fresh-line)
                   (set! current name)
                   (mrspidey:progress-output (format "~s: " name)))
                 (mrspidey:progress-output ".") 
                 (flush-output)
                 (set! fresh-line #f)
                 (when (= fraction 1) (f #\newline))]
                [((? string? str)) 
                 (f 'fresh-line)
                 (mrspidey:progress-output str)
                 (when (char=? (string-ref str (sub1 (string-length str)))
                               #\newline)
                   (set! fresh-line #t))]
                [(#\newline)
                 (mrspidey:progress-output (format "~n"))
                 (set! fresh-line #t)]
                [('fresh-line)
                 (unless fresh-line (f #\newline))])])
         f)))
   (define mrspidey:progress-output
     (lambda (str) (display str) (flush-output)))
