; link.ss

(compound-unit/sig 
  (import (core : mzlib:core^)
          (framework : framework^)
          (print-convert : mzlib:print-convert^)
          (mred : mred^)
          (drscheme : drscheme:export^)
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
               ((require-library-unit/sig "break.ss" "stepper")
                mred
                marks
                annotate)]
        [stepper-view-controller : (stepper-go)
                 ((require-library-unit/sig "view-controller.ss" "stepper")
                  core
                  error
                  zodiac
                  client-procs
                  pretty
                  mred
                  drscheme
                  print-convert
                  framework
                  shared
                  utils
                  marks)]
        [stepper-startup : ()
                         ((require-library-unit/sig "startup.ss" "stepper")
                          core
                          mred
                          framework
                          drscheme
                          stepper-view-controller)])       
      (export (open debug-wrapper) (open break)))
