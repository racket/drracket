(load "slaconfg.lsp")
(load "batconfg.lsp")

(cond ((fboundp 'bye) (bye))
      ((fboundp 'exit) (exit))
      ((fboundp 'quit) (quit))
      (t (format t "~&You may exit CL now!~%")))
