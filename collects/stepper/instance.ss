; stepper-instance

(compound-unit/sig 
  (import (model-input : stepper:model-input^)
          (core : mzlib:core^)
          (error : zodiac:interface^)
          (print-convert : mzlib:print-convert^)
          (drscheme : drscheme:export^)
          (zodiac : zodiac:system^)
          (zcp : stepper:client-procs^)
          (shared : stepper:shared^)
          (mred : mred^)
          (utils : stepper:cogen-utils^)
          (marks : stepper:marks^))
  (link [stepper-annotate : stepper:annotate^
                          ((require-library-unit/sig "annotater.ss" "stepper")
                           zodiac
                           (core function)
                           error
                           utils
                           marks
                           stepper
                           shared
                           zcp)]
        [reconstruct : stepper:reconstruct^ 
                     ((require-library-unit/sig "reconstructr.ss" "stepper")
                      zodiac
                      (core function)
                      error
                      utils
                      (drscheme basis)
                      marks
                      stepper
                      shared)]
        [stepper : stepper:model^
                 ((require-library-unit/sig "model.ss" "stepper")
                  model-input
                  mred
                  zodiac
                  drscheme
                  print-convert
                  error
                  stepper-annotate
                  reconstruct
                  shared)])       
      (export))
