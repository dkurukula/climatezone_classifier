echo OFF
set doedir=c:\doe22
set weathr.tmp=%doedir%\weather\%1.bin
set output=%doedir%\weather\%1.lst
echo LIST >input.tmp
echo PACKED  -999  -999    %2    %3 >>input.tmp
%doedir%\exent\doewth
set weathr.tmp=
set output=
del input.tmp
del stout.tmp
del newth.tmp
echo ON
