(compound-unit/sig
  (import (FUN : mzlib:function^))
  (link
   (S : xml-structs^ ((require-library "structures.ss" "xml")))
   (R : reader^ ((require-library "reader.ss" "xml") S FUN))
   (U : writer^ ((require-library "writer.ss" "xml") S FUN))
   (T : xexpr^ ((require-library "xexpr.ss" "xml") S U FUN))
   (W : space^ ((require-library "space.ss" "xml") S FUN)))
  (export (open S) (open R) (open U) (open T) (open W)))
