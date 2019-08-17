import numpy as np
import pandas as pd
from pathlib import Path
import subprocess
import os

p = Path.cwd()

#store .bin files here
PATH_TO_WEATHER = p.joinpath('weather_data')
#this is where the conversion EXE is
PATH_TO_UTIL32 = p.joinpath('DOE22WeatherUtilities', 'DOE22', 'UTIL32')
#this is a the bat file that will run the EXE with correct arguments 
PATH_TO_BAT = PATH_TO_UTIL32.joinpath('MKAFT.BAT')

fl = list(PATH_TO_WEATHER.glob('*.bin'))

#modify bat file to include correct paths
bat_file = f"echo OFF \n\
set doedir={PATH_TO_UTIL32} \n\
set weather.bin={PATH_TO_WEATHER}\%1.bin \n\
set WEATHER.FMT={PATH_TO_WEATHER}\%1.ft \n\
bin2txt \n\
echo ON"

#set weather.bin=%doedir%\weather\%1.bin \n\
#set WEATHER.FMT=%doedir%\weather\%1.ft \n\

#write bat file where conversion EXE is
with open(PATH_TO_BAT,'w') as f:
    print(bat_file, file = f)

#run bat for each weather file
os.chdir(PATH_TO_UTIL32)
for wf in fl:
    weather_file = wf.stem
    print(weather_file)
    print(PATH_TO_BAT)
    subprocess.call([PATH_TO_BAT._str, weather_file], shell = True)
os.chdir(p)

#remove bat file with system path info 
PATH_TO_BAT.unlink()



