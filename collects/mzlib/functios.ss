
(begin-elaboration-time
 (require-relative-library "spidey.ss"))

(define-signature mzlib:function^
  (true
   false
   
   first
   second
   third
   fourth
   fifth
   sixth
   seventh
   eighth

   rest

   cons?
   empty
   empty?

   boolean=?
   symbol=?

   identity
   compose
   foldl
   foldr

   last-pair

   remv
   remq
   remove
   remv*
   remq*
   remove*

   assf
   memf

   filter

   build-string
   build-vector
   build-list

   quicksort

   loop-until

   ignore-errors))
