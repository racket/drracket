cl -O2 -Ic:/msdev/include -I../include /c %1.c
set LIB=c:/msdev/lib
link /dll -out:%1.dll mzdyn.obj %1.obj mzdyn.exp
