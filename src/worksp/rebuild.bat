echo off
rem ************* MZSCHEME ******************
cd mzscheme
nmake -f mzscheme.mak
cd ..

rem ************* MRED ******************
cd mred
nmake -f mred.mak
cd ..

rem ************* MZSTART ******************
cd mzstart
nmake -f mzstart.mak
cd ..

rem ************* MRSTART ******************
cd mrstart
nmake -f mrstart.mak
cd ..

cd ..

rem ************* MYSTERX ******************
rem the next complain about the missing file f:\SBN\lib\shell32.lib
rem cd mysterx
rem nmake
rem cd ..

rem ************* DYNSRC *******************
cd  mzscheme\dynsrc
call mkmzdyn.bat
cd ..\..\..

rem ************* SETUP *******************
mzscheme.exe -mvqL- setup.ss setup
rem errors:
rem readline-installer: can't find readline include files and/or library;
rem   try editing 'search-path' in mzmake.ss

