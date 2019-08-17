echo off
echo This procedure file uses the DOE-2 weather processor to pack
echo a standard format weather file into a packed DOE-2 weather file
echo format.  The parameter is the name of the input file located in
echo the unpacked directory.  The file has an extension of .tpe (but
echo this extension is not on the parameter.)  The input data file to
echo the weather processor is located in the input directory with the
echo name and the extension .inp (extension is again not on the parameter.)
pause
set doedir=c:\doe22
if exist %doedir%\weather\%1.inp goto :doit1
echo ERROR: missing wth processor input command file - %doedir%\weather\%1.inp
goto :end
:doit1
if exist %doedir%\weather\%1.tpe goto :doit2
echo ERROR: missing wth processor weather data file - %doedir%\weather\%1.tpe 
goto :end
:doit2
if not exist %doedir%\weather\%1.bin goto :okay
echo **** %1.bin already exists as a packed file ****
echo press any key to overwrite or CTRL C to abort
goto :end
:okay
set weathr.tmp=%doedir%\weather\%1.tpe
set input.tmp=%doedir%\weather\%1.inp
set output=%doedir%\weather\%1.out
set newth.tmp=%doedir%\weather\%1.bin
%doedir%\exent\doewth
set input.tmp=
set weathr.tmp=
set output=
set newth.tmp=
del stout.tmp
:end
echo on
