; ======================================================================
; find-nonempty-tvars
;
; Input:  lower : (listof Tvar) with non-empty Tvar_L
;         upper : (listof Tvar) with non-empty Tvar_U
;
; Finds non-empty non-terminals
; returns (listof Tvar)
; ======================================================================
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

(define (find-nonempty-tvars lower upper)
  (pretty-debug-min
    `(find-nonempty-tvars ,(map Tvar-name lower) ,(map Tvar-name upper)))

  (let*-vals ( [(reached-U? set-reached-U!) (alloc-Tvar-field)]
               [(reached-L? set-reached-L!) (alloc-Tvar-field)]
               [(reached-AV set-reached-AV!) (alloc-AV-field)]
               [num-AV 0]
               [list-tvar '()])
    (letrec
      ( [add-Tvar!
          (lambda (tvar)
            (set! list-tvar (cons tvar list-tvar)))]
        [walk-AV
          (lambda (AV)
            (unless (reached-AV AV)
              ;; Walk it
              (set-reached-AV! AV #t)
              (match AV
                [($ AV _ template _ fields+ fields-)
                  (vector-for-each walk-U fields+)
                  (when (follow-antimono-fields template)
                    (pretty-debug-min
                      `(walking-U 
                         ,(eq? template template-lam)
                         ,(st:minimize-respect-assignable-part-of-fields)))
                    (vector-for-each walk-U fields-))])))]
        [walk-U
          (lambda (tvar)
            (unless (reached-U? tvar)
              (unless (reached-L? tvar) (add-Tvar! tvar))
              (set-reached-U! tvar #t)
              (pretty-debug-min `(walk-U ,(Tvar-name tvar)))
              (for-each walk-AV (get-Tvar-objs tvar))
              (for-each walk-U (Tvar-edgefrom tvar))))]
        [walk-L
          (lambda (tvar)
            (unless (reached-L? tvar)
              (unless (reached-U? tvar) (add-Tvar! tvar))
              (set-reached-L! tvar #t)
              (pretty-debug-min `(walk-L ,(Tvar-name tvar)))
              ;; Have to_L -> tvar_L
              (for-each walk-L (Tvar-edgeto tvar))

              (for-each
                (match-lambda
                  [($ con _ _ field-no tvar2 sign misc)
                    ;; Have tvar2_L -> rng(tvar_L)
                    ;; or   tvar2_U -> dom(tvar_L)
                    (if sign
                      (walk-L tvar2)
                      (walk-U tvar2))]
                  [($ con-filter _ filter tvar2)
                    ;; Have tvar2_L -> tvar_L
                    (walk-L tvar2)])
                (Tvar-constraints tvar))))])
      (for-each walk-U upper)
      (for-each walk-L lower)
      '(printf "walk-reachable-constraints allocated ~s tvar, ~s AV~n"
         (length list-tvar) num-AV)

      (values list-tvar reached-L? reached-U?))))

; ======================================================================

(define (copy-nonempty-tvars lower upper)
  (let*-vals
    ( [(live-tvars _ _) (find-nonempty-tvars lower upper)]
      [(tvar-live? set-tvar-live!) (alloc-Tvar-field)])
    (for-each mk-Tvar-NTs! live-tvars)
    (for-each (lambda (tvar) (set-tvar-live! tvar #t)) live-tvars)
    (copy-constraints-equiv! 
      (append lower upper live-tvars)
      tvar-live?
      (append 
        (map Tvar-L live-tvars)
        (map Tvar-U live-tvars))
      (lambda (tvar) (list tvar))
      (lambda (AV) (list AV)))))
