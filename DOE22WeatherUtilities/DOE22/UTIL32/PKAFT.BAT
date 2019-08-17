echo off
echo This shell file packs an unpacked DOE-2 weather file.
echo The file was unpacked wth wthfmt2 and has an .ft extension
echo (which is not included in the call to this procedure.)
echo Param 1 is file name.  Param 2 is S or NS for solar or nonsolar.
echo S for TMY, WYEC; NS for TRY.
echo If Param 2 is not specified the type found on the .ft file will be used.
echo A WTH stat run is also made on the file and all output
echo is moved into the INPUT directory.
pause
set doedir=c:\doe22
if exist %doedir%\weather\%1.ft goto :ok
echo ERROR: missing input data file - %doedir%\weather\%1.ft 
goto :end
:ok
if not exist %doedir%\weather\%1.bin goto :okay
echo **** %1.bin already exists as a packed file ****
echo press any key to overwrite or CTRL C to abort
pause
:okay
set WEATHER.FMT=%doedir%\weather\%1.ft 
REM              1234567890123123456789012345678
if "%2"=="" goto :notype
if %2 == S  echo             2                 3 >> INPUT.DAT
if %2 == NS echo             2                 2 >> INPUT.DAT
:notype
set WEATHER.BIN=%doedir%\weather\%1.bin
txt2bin
set WEATHER.BIN=
set WEATHER.FMT=
del INPUT.DAT
set WEATHR.tmp=%doedir%\weather\%1.bin
set output=%doedir%\weather\%1.sta
echo STAT >input.tmp
%doedir%\exent\doewth
set WEATHR.tmp=
set weathr.tmp=
set output=
del input.tmp
del stout.tmp
del newth.tmp
:end
echo on
