(module drscheme mzscheme
  (define debugging? (getenv "PLTDRDEBUG"))
  
  (define install-cm? (and (not debugging?)
                           (getenv "PLTDRCM")))
  
  (define cm-trace? (and install-cm?
                         (equal? (getenv "PLTDRCM") "trace")))
  
  (when debugging?
    (printf "PLTDRDEBUG: installing CM to load/create errortrace zos\n")
    (use-compiled-file-paths (list (build-path "compiled" "errortrace")))
    (error-display-handler (dynamic-require '(lib "errortrace-lib.ss" "errortrace")
                                            'errortrace-error-display-handler))
    (let-values ([(current-managed-zo-compile
                   make-compilation-manager-load/use-compiled-handler
                   manager-trace-handler)
                  (parameterize ([current-namespace (make-namespace)])
                    (values
                     (dynamic-require '(lib "cm.ss") 'current-managed-zo-compile)
                     (dynamic-require '(lib "cm.ss") 'make-compilation-manager-load/use-compiled-handler)
                     (dynamic-require '(lib "cm.ss") 'manager-trace-handler)))])
      (current-managed-zo-compile
       (dynamic-require '(lib "zo-compile.ss" "errortrace") 'zo-compile))
      (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler))
      (when cm-trace?
        (manager-trace-handler
         (lambda (x) (display x) (newline))))))
  
  (when install-cm?
    (printf "PLTDRCM: installing compilation manager\n")
    (let-values ([(make-compilation-manager-load/use-compiled-handler
                   manager-trace-handler)
                  (parameterize ([current-namespace (make-namespace)])
                    (values
                     (dynamic-require '(lib "cm.ss") 'make-compilation-manager-load/use-compiled-handler)
                     (dynamic-require '(lib "cm.ss") 'manager-trace-handler)))])
      (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler))
      (when cm-trace?
        (manager-trace-handler
         (lambda (x) (display x) (newline))))))

  (dynamic-require '(lib "drscheme-normal.ss" "drscheme" "private") #f))
