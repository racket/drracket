(module browser-unit mzscheme
  (require (lib "unitsig.ss")
           (lib "mred-sig.ss" "mred")
           (lib "plt-installer-sig.ss" "setup")
           (lib "tcp-sig.ss" "net")
           (lib "url-sig.ss" "net")
           (lib "url-unit.ss" "net")
           "browser-sig.ss"
           "private/bullet.ss"
           "private/html.ss"
           "private/hyper.ss"
           "private/sig.ss")
  
  (provide browser@)
  
  (define browser@
    (compound-unit/sig
      (import (plt-installer : setup:plt-installer^)
              (mred : mred^)
              (tcp : net:tcp^))
      (link [url : net:url^ (url@ tcp)]
            [bullet : bullet^ (bullet@ mred)]
            [html : html^ (html@ bullet mred url)]
            [hyper : browser^ (hyper@ html bullet mred plt-installer url)])
      (export (open hyper)
              (open (html : html-export^))))))
