; stepper.ss

(compound-unit/sig 
  (import (core : mzlib:core^)
          (framework : framework^)
          (print-convert : mzlib:print-convert^)
          (mred : mred^)
          (drscheme : drscheme:export^)
          (zodiac : zodiac:system^)
          (zcp : stepper:zodiac-client-procs^))
  (link [pretty : mzlib:pretty-print^ ((require-library-unit/sig "prettyr.ss"))]
        [error : stepper:error^ ((unit/sig stepper:error^
                                   (import)
                                   (define default-error-handler
                                     (lambda (keyword)
                                       (lambda (where fmt-spec . args)
                                         ; (printf "Error at: ~s~n" where)
                                         (apply error keyword fmt-spec args))))
                                   (define internal-error
                                     (default-error-handler 'internal-error))
                                   (define static-error
                                     (default-error-handler 'syntax-error))
                                   (define dynamic-error
                                     (default-error-handler 'runtime-syntax-error))))]
        [shared : stepper:shared^ ((require-library-unit/sig "sharedr.ss" "stepper")
                                   zodiac
                                   error
                                   zcp)]
        [xml-support : xml-reconstruct^ 
                      ((require-library-unit/sig "xml-support.ss" "stepper")
                       error)]
        [stepper-view-controller : ()
                 ((require-library-unit/sig "stepper-view-controller.ss" "stepper")
                  core
                  error
                  zodiac
                  zcp
                  pretty
                  mred
                  drscheme
                  print-convert
                  framework
                  xml-support
                  shared)])       
      (export))
