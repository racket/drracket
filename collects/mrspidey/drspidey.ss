;; drspidey.ss
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

;; The code to be loaded from DrScheme

;;(printf "loading drspidey.ss (cd ~s)~n" (current-directory))

(reference "pltrc-co.ss")
(reference "macros.ss")

(begin-elaboration-time
  (unless (getenv "MREDCOMPILE") 
    (match:set-error-control 'match)))


#| MATTHEW: use require-library instead
(load/use-compiled
  (let ([p (build-path (current-load-relative-directory) "Zodiac" "load.ss")])
    (if (file-exists? p)
      p
      (build-path (current-load-relative-directory) 'up "zodiac" "load.ss"))))
|#
(require-library "load.ss" "zodiac")

(load/use-compiled (build-path "Sba" "loadu.ss"))
(load/use-compiled (build-path "Gui" "loadu.ss"))

(define mrspidey:interaction@
  (unit/sig mrspidey:interaction^
    (import 
      [mred : mred^]
      [zodiac : mrspidey:zodiac^]
      mzlib:unprefixed-core^)
    (include "handlers.ss")

    (mrspidey:error-handler
      (case-lambda
        [(message object)
          (unless (zodiac:zodiac? object)
            (printf "Bad object in mrspidey:error-handler ~s~n" object)
            ((mrspidey:error-handler) message))
          (let* ([loc (zodiac:zodiac-start object)])
            (unless (zodiac:location? loc)
              (printf "Bad location in mrspidey:error-handler ~s~n" loc)
              ((mrspidey:error-handler) message))
            ((mrspidey:error-handler)
              (format "~a at ~s line ~s, column ~s~n"
                message
                (file-name-from-path (zodiac:location-file loc))
                (zodiac:location-line loc)
                (zodiac:location-column loc))))]
        [(message)
          (mred:message-box
            (format "~a~n" message)
            "MrSpidey Error")
          (raise 'mrspidey-raise)]))
    ))

;; ----------------------------------------------------------------------

(define mrspidey-tool@
  (unit/sig ()
    (import
      [mred : mred^]
      mrspidey-gui^
      mzlib:unprefixed-core^)
    (mred:add-version-spec 'sd 1)
    (lambda (frame)
      (let* ( [edit (ivar frame definitions-edit)]
              [name (send edit get-filename)])
        (if (string? name)
          (when
            (or (not (send edit modified?))
              (let ([action (mred:unsaved-warning name "Analyze" #t)])
                (case action
                  [(save) (send edit save-file)]
                  [(continue) #t]
                  [else #f])))
            (with-handlers
              ([ (lambda (x) (eq? x 'mrspidey-raise))
                 (lambda (x) (void))])
              (send spidey run-mrspidey (send edit get-filename))))
          (mred:message-box
            "MrSpidey can only process programs that are saved to a file"
            "MrSpidey Error"))))))

;; ----------------------------------------------------------------------

(define tool@
  (let ( [mrspidey:sba@ mrspidey:sba@]
         [mrspidey:interaction@ mrspidey:interaction@]
         [mrspidey-gui@ mrspidey-gui@]
         [mrspidey-tool@ mrspidey-tool@])
    (unit/sig ()
      (import 
        [WX : wx^]
        [MRED : mred^]
        [MZLIB : mzlib:core^]
        [PCONVERT : mzlib:print-convert^]
        [DRSCHEME : drscheme:export^]
        [ZODiac : zodiac:system^])

      (invoke-unit/sig
        (unit->unit/sig
          (unit/sig->unit
            (unit/sig ()
              (import
                mzlib:pretty-print^
                mzlib:file^
                mzlib:function^
                mzlib:compat^
                mzlib:string^)
              (invoke-unit/sig
                (unit->unit/sig
                  (unit/sig->unit
                    (compound-unit/sig 
                      (import 
		        [WX : wx^]
                        [MZLIB : mzlib:unprefixed-core^]
                        [MRED : mred^])
                      (link 
                        [INTERACTION : mrspidey:interaction^
                          (mrspidey:interaction@ MRED (SBA zodiac) MZLIB)]
                        [SBA : mrspidey:sba^
                          (mrspidey:sba@ INTERACTION MZLIB WX)]
                        [GUI : mrspidey-gui^
                          (mrspidey-gui@ WX MRED MZLIB SBA INTERACTION)]
                        [TOOL : () 
                          (mrspidey-tool@ MRED GUI MZLIB)])
                        (export)))
                  (wx^ mzlib:unprefixed-core^ mred^)
                  ()
                  )
		[WX : wx^]
                mzlib:unprefixed-core^
                [MRED : mred^])))
          (((unit pretty-print@ : mzlib:pretty-print^))
            ((unit file@ : mzlib:file^))
            ((unit function@ : mzlib:function^))
            ((unit compat@ : mzlib:compat^))
            ((unit string@ : mzlib:string^)))
          ())
        (MZLIB : ((unit pretty-print@ : mzlib:pretty-print^)))
        (MZLIB : ((unit file@ : mzlib:file^)))
        (MZLIB : ((unit function@ : mzlib:function^)))
        (MZLIB : ((unit compat@ : mzlib:compat^)))
        (MZLIB : ((unit string@ : mzlib:string^)))))))

;;(printf "tool@ defined~n")

;; ----------------------------------------------------------------------
