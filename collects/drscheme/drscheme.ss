(module drscheme mzscheme
  (define debugging? (getenv "PLTDRDEBUG"))
  
  (define profiling? (and debugging?
                          (equal? (getenv "PLTDRDEBUG") "profile")))
  
  (define install-cm? (and (not debugging?)
                           (getenv "PLTDRCM")))
  
  (define cm-trace? (and install-cm?
                         (equal? (getenv "PLTDRCM") "trace")))
  
  (when debugging?
    (printf "PLTDRDEBUG: installing errortrace\n")
    (dynamic-require '(lib "errortrace.ss" "errortrace") #f)
    (when profiling?
      ((dynamic-require '(lib "errortrace.ss" "errortrace") 'profiling-enabled) #t)
      (let ([enable-initially?
             (with-handlers ([not-break-exn? (lambda (x) #f)])
               (display "PLTDRDEBUG: Turn on profiling during startup? [y/N] ")
               (flush-output)
               (let ([l (read-line)])
                 (and (string? l)
                      (regexp-match "[yY]" l))))])
        (printf "PLTDRDEBUG: turning on profiling ")
        (if enable-initially?
            (printf "(initially recording)\n")
            (printf "(not during startup)\n"))
        ((dynamic-require '(lib "errortrace.ss" "errortrace") 'profiling-record-enabled) enable-initially?))))
  
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

  (cond
    [debugging? 
     (dynamic-require '(lib "drscheme-debug.ss" "drscheme" "private") #f)]
    [else
     (dynamic-require '(lib "drscheme-normal.ss" "drscheme" "private") #f)]))
