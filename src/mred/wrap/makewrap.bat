
..\..\..\mzscheme.exe -qmvqr mkwrap.ss
perl ../../mzscheme/src/sstoinc < wrap.ss > ..\wxs\wrap.inc
..\..\..\mzscheme.exe -gqrna ../../mzscheme/src/sstoinc.ss < wrap.ss > ..\wxs\cwrap.inc
