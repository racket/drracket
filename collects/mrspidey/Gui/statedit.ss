; statedit.ss
; Defines spidey:static-edit%, a subclass of mred:media-edit%
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

(define spidey:static-edit%
  (class mred:searching-edit% init-args
    (inherit 
      insert delete flash-on flash-off get-text
      set-clickback change-style lock
      set-position load-file
      get-keymap scroll-to-position
      last-position get-style-list
      set-mode
      begin-edit-sequence end-edit-sequence
      )

    (public
      [begin-edit-sequence-and-unlock
        (lambda ()
          (wx:begin-busy-cursor)
          (lock #f)
          (begin-edit-sequence))]
      [end-edit-sequence-and-lock
        (lambda ()
          (end-edit-sequence)
          (lock #t)        
          (wx:end-busy-cursor))]
      [edit-sequence
        (lambda (thunk)
          (dynamic-wind
            begin-edit-sequence-and-unlock
            thunk
            end-edit-sequence-and-lock))]
      [match-paren-forward
        (lambda (source-start)
          (mred:scheme-forward-match
            this source-start (last-position)))]
      )
	      
    (sequence
		
      (apply super-init init-args)

      (set-mode (make-object mred:scheme-mode%))
      ;; disable paste for errant right mouse clicks
      (let ([ k (get-keymap)])
        (send k add-mouse-function "nothing" (lambda l (void)))
        (send k map-function "middlebutton" "nothing")
        (send k map-function "rightbutton" "nothing"))

      ;; make snips go down instead of up
      ;; oops - can't do this :-(
      '(let ([stan (send (get-style-list) find-named-style "Standard")])
         (when stan (send stan set-delta normal-delta)))

      (lock #t)
      )))
