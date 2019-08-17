echo OFF
set doedir=c:\doe22
set weathr.tmp=%doedir%\weather\%1.bin
set output=%doedir%\weather\%1.sta
echo STAT >input.tmp
%doedir%\exent\doewth
set weathr.tmp=
set output=
del input.tmp
del stout.tmp
del newth.tmp
echo ON
