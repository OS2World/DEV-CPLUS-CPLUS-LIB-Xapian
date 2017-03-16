@echo off
REM *** make dll
copy .libs\xapian.a  .libs\xapian.lib

rem -g -nolxlite
dllar -o xapian10 -omf -libdata "MULTIPLE" -libf "INITINSTANCE TERMINSTANCE" .libs\xapian.lib -Zhigh-mem -Zbin-files -Zsym -Zmap -lz

REM *** copy import libs
copy xapian10.a .libs\xapian.a
copy xapian10.lib .libs\xapian.lib

REM *** delete executables
dir /s/b *.exe > make_dll.rsp
rm @make_dll.rsp 2>&1 > nul

REM *** make executables again
make
