;; macros.ss
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

(reference-library "defstruc.ss")

(eval-at-compile-time
  (if (defined? '__NO_DEBUGGING)
    (begin
      (printf "Debugging off~n")
      (defmacro pretty-debug args           '(void))
      (defmacro pretty-debug-traverse args  '(void))
      (defmacro pretty-debug-object args    '(void))
      (defmacro pretty-debug-front args     '(void))
      (defmacro pretty-debug-min args       '(void))
      (defmacro pretty-debug-min2 args      '(void))
      (defmacro pretty-debug-check args     '(void))
      (defmacro pretty-debug-atenv args     '(void))
      (defmacro pretty-debug-atype args     '(void))
      (defmacro pretty-debug-few args       '(void))
      (defmacro pretty-debug-gram args      '(void))
      (defmacro pretty-debug-sdl args       '(void))
      (defmacro pretty-debug-sdl2 args      '(void))
      (defmacro pretty-debug-dfa-min args   '(void))
      (defmacro pretty-debug-min-table args '(void))
      (defmacro pretty-debug-traverse-small args '(void))
      (defmacro pretty-debug-unit args      '(void))
      (defmacro pretty-debug-gui args       '(void))
      (defmacro assert args                 '(void))
    
      )
  
    (begin
      (defmacro pretty-debug args
        `(when debugging (pretty-print-debug ,@args)))
      (defmacro pretty-debug-traverse args
        `(when debugging-traverse (pretty-print-debug ,@args)))
      (defmacro pretty-debug-object args
        `(when debugging-object (pretty-print-debug ,@args)))
      (defmacro pretty-debug-front args
        `(when debugging-front (pretty-print-debug ,@args)))
      (defmacro pretty-debug-min args
        `(when debugging-min (pretty-print-debug ,@args)))
      (defmacro pretty-debug-min2 args
        `(when debugging-min2 (pretty-print-debug ,@args)))
      (defmacro pretty-debug-check args
        `(when debugging-check (pretty-print-debug ,@args)))
      (defmacro pretty-debug-atenv args
        `(when debugging-atenv (pretty-print-debug ,@args)))
      (defmacro pretty-debug-atype args
        `(when debugging-atype (pretty-print-debug ,@args)))
      (defmacro pretty-debug-few args
        `(when debugging-few (pretty-print-debug ,@args)))
      (defmacro pretty-debug-gram args 
        `(when debugging-gram (pretty-print-debug ,@args)))
      (defmacro pretty-debug-sdl args
        `(when debugging-sdl (pretty-print-debug ,@args)))
      (defmacro pretty-debug-sdl2 args
        `(when (or debugging-sdl2 debugging-sdl) (pretty-print-debug ,@args)))
      (defmacro pretty-debug-dfa-min args
        `(when debugging-dfa-min (pretty-print-debug ,@args)))
      (defmacro pretty-debug-min-table args
        `(when debugging-min-table (pretty-print ,@args)))
      (defmacro pretty-debug-gui args
        `(when debugging-gui (pretty-print ,@args)))

      (defmacro pretty-debug-traverse-small args
        `(when debugging-traverse 
           (dynamic-let ([pretty-print-depth 4]) (pretty-print-debug ,@args))))

      (defmacro pretty-debug-unit args
        (match args
          [(arg) `(when debugging-unit (pretty-print-debug ,arg))]
          [(arg depth)
            `(when debugging-unit
               (dynamic-let ([pretty-print-depth ,depth]) (pretty-print-debug ,arg)))]))
      )))

;; ----------------------------------------------------------------------


(defmacro trace-time-lambda args
  (match args
    [(timer args . body)
     `(lambda ,args (record-time ,timer (lambda () ,@body)))]))

;; ----------------------------------------------------------------------
