(module browser-unit mzscheme
  (require (lib "unitsig.ss")
           (lib "mred-sig.ss" "mred")
           (lib "plt-installer-sig.ss" "setup")
           "private/btree.ss"
           "private/bullet.ss"
           "private/html.ss"
           "private/hyper.ss"
           "private/sig.ss")
  
  (provide browser@)
  
  (define browser@
    (compound-unit/sig
      (import (plt-installer : setup:plt-installer^)
              (mred : mred^))
      (link [btree : relative-btree^ (btree@)]
            [bullet : bullet^ (bullet@ mred)]
            [html : html^ (html@ btree bullet mred)]
            [hyper : browser^ (hyper@ html bullet mred plt-installer)])
      (export (open hyper)))))
