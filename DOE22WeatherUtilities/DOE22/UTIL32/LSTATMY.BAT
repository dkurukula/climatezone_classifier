echo OFF
set doedir=c:\doe22
set weathr.tmp=%doedir%\weather\%1.dat
set output=%doedir%\weather\%1.lst
echo LIST >input.tmp
echo TMY     -999 14819    %2    %3     6 41.80 87.80>>input.tmp
%doedir%\exent\doewth
set weathr.tmp=
set output=
del input.tmp
del stout.tmp
del newth.tmp
echo ON
