echo OFF
set doedir=c:\doe22
set weathr.tmp=%doedir%\weather\%1.tpe
set output=%doedir%\weather\%1.lst
echo LIST >input.tmp
echo TRY     -999 14819    %2    %3 >>input.tmp
%doedir%\exent\wth
set weathr.tmp=
set output=
del input.tmp
del stout.tmp
del newth.tmp
echo ON
