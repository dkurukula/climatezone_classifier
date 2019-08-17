
JJ Hirsch PC DOE-2 weather utilities

The DOE-2 32-bit weather utilities can run under Windows 95/98/NT as
a console application (like in a DOS box); they cannot run inWindows 3.XX
or non-32bit DOS.
The weather utilities consist of:

  doewth.exe   The DOE-2 weather processor as documented in the DOE-2.2
               reference manual (this program is in C:\DOE22\EXENT)
  bin2txt.exe  A utility that reads a DOE-2 binary weather file and writes
               a text file with all the data
  txt2bin.exe  A utility that read a text file (of the format written by
               bin2txt - documented on fmtwth.fmt) and writes a DOE-2
               binary weather file.
  mkdlst.exe   A utility that read the file dirin.dir, which contains a
               listing of DOE-2 packed weather files (one name per line
               with no extension) and creates the file DIROUT.DIR (as well 
               as writing to the screen) a description line for each file

The DOE21E directory contains several subdirectories as follows:

The C:\DOE22\UTIL32 directory contains several programs and batch
procedure files as follows:

CVTWTH16 DOC  Documentation on the CVTWTH program
CVTWTH16 EXE  Program to convert between various vendors DOE-2 binary files
BIN2TXT  EXE  Program to create a text format file from a DOE-2 binary file
BIN2TXT  FMT  Documentation on the text file format created by BIN2TXT
LSTABIN  BAT  Batch file to use DOEWTH.EXE to list a binary file
              useage:  LSTABIN filename startmonth endmonth
              filename is DOE-2 binary (filename.bin) in PACKED (no extension)
              filename.lst is placed in WEATHER directory
LSTATMY  BAT  Batch file to list a NOAA TMY format text file using DOEWTH.EXE
              (must be edited to work with a specific file)
LSTATRY  BAT  Batch file to list a NOAA TRY format text file using DOEWTH.EXE
              (must be edited to work with a specific file)
MKAFT    BAT  Batch file to create a text file from a binary file using BIN2TXT
              useage:  MKAFT filename
              filename is DOE-2 binary (filename.bin) in WEATHER (no extension)
              filename.ft is placed in WEATHER directory
PKAFT    BAT  Batch file to create a binary file from a text file using TXT2BIN
              useage:  MKAFT filename
              filename is BIN2TXT text (filename.ft) in WEATHER (no extension)
              filename.bin is placed in WEATHER directory
PKATPE   BAT  Batch file to process a NOAA text file into a DOE-2 file using 
              DOEWTH.EXE
              useage:  PKATPE filename
              filename is NOAA "raw" text (*.tpe) in WEATHER directory (no 
              extension) and matching DOEWTH.EXE input (*.inp) in WEATHER 
              (see EXAMPLES directory for sample files)
              filename.bin is placed in WEATHER directory
              filename.out is placed in WEATHER directory
STAT     BAT  Batch file to create a weather file statistical summary from a
              DOE-2 binary file
              useage:  STAT filename
              filename is binary (*.bin) in WEATHER directory (no extension)
              filename.sta is placed in WEATHER directory
TXT2BIN  EXE  Program to process a BIN2TXT format text file into a DOE-2 binary

When you have problems you should examine the procedure files to see if any
alteration is required to perform as you desire.
