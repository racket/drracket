README.TXT

This is the 'alpha-5' release of wxMac161d. Many things have changed since 'a4' so 
please read the status documents and various README's.

Incompatibilities for existing wxMac Applications:
   1. Your Canvas method Paint() must be changed to OnPaint().
   2. Any calls on GetLabel return a ptr to a static global buffer instead of the
      previous method of a new string. Your code may have to change. I believe
	  this is the standard wxWindows method.
   3. wxText::GetValue also returns in the static global buffer.

New features
   1. wxDirecorySelector().
   2. all patches to 'a4' releases  - far too many to list here.
   3. Fixed many memory leaks.
   4. A New directory 'Porting', contains many notes and all the wxMac
      documentation.
	  
Good Luck
Cecil Coupe - ccoupe@simplot.com
