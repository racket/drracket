gcc -c -O2 -I../include mzdyn.c
gcc -c init.cc
gcc -c fixup.c
dlltool --def mzdyn.def --output-exp mzdyn.exp
mkdir ..\..\..\collects\mzscheme\lib
mkdir ..\..\..\collects\mzscheme\lib\win32
mkdir ..\..\..\collects\mzscheme\lib\win32\i386
mkdir ..\..\..\collects\mzscheme\lib\win32\i386\gcc
copy mzdyn.def ..\..\..\collects\mzscheme\lib\win32\i386\gcc
copy mzdyn.exp ..\..\..\collects\mzscheme\lib\win32\i386\gcc
copy mzdyn.o ..\..\..\collects\mzscheme\lib\win32\i386\gcc
copy init.o ..\..\..\collects\mzscheme\lib\win32\i386\gcc
copy fixup.o ..\..\..\collects\mzscheme\lib\win32\i386\gcc
