(compound-unit/sig 
  (import (core : mzlib:core^)
          (zodiac : zodiac:system^)
          (error : zodiac:interface^))
  (link [pretty : mzlib:pretty-print^ ((require-library-unit/sig "prettyr.ss"))]
        [client-procs : stepper:client-procs^
                      ((require-library-unit/sig "client-procs.ss" "stepper")
                       zodiac)]
        [marks : stepper:marks^
               ((require-library-unit/sig "marks.ss" "stepper")
                zodiac
                client-procs
                (core function))]
        [utils : stepper:cogen-utils^ 
               ((require-library-unit/sig "utils.ss" "stepper")
                zodiac
                error)]        
        [shared : stepper:shared^ ((require-library-unit/sig "sharedr.ss" "stepper")
                                   zodiac
                                   error
                                   client-procs)]
        [fake-stepper : stepper:model^
                     ((require-library-unit/sig "fake-model.ss" "stepper"))]
        [annotate : stepper:annotate^
                  ((require-library-unit/sig "annotater.ss" "stepper")
                   zodiac
                   (core function)
                   error
                   utils
                   marks
                   fake-stepper
                   shared
                   client-procs)]
        [debug-wrapper : plt:aries-no-break^
                          ((require-library-unit/sig "debug-wrapper.ss" "stepper")
                           zodiac
                           utils
                           marks
                           annotate)]
        [break : (break)
               ((unit/sig (break) (import) (define break (lambda () #f))))])       
      (export (open debug-wrapper) (open break)))