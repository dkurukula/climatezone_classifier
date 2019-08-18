import pandas as pd
import numpy as np
from pathlib import Path
p = Path.cwd()
PATH_TO_WEATHER = p.joinpath('weather_data')

fl =list(PATH_TO_WEATHER.glob('*.ft'))

def get_HDD(df):
    '''takes df and returns HDD based on max min temp and 65 F'''
    date_str = '1/1/2019' #any non-leap year
    sdate = pd.to_datetime(date_str)
    df.index = pd.date_range(sdate, periods=8760, freq='H')
    df_Tmax = df.resample('D').max()[2].copy()
    df_Tmin = df.resample('D').min()[2].copy()
    df_HDD = 65-((df_Tmax + df_Tmin)/2)
    HDD = int(round(df_HDD.sum()))
    return HDD

def get_cz(HDD):
    '''takes HDD and return cz from 4-8
       based on ASHRAE 90.1-2010 Table B-4 International Climate Zone Definitions'''
    if (HDD>3600) & (HDD<=5400):
        cz = 4
    elif (HDD>5400) & (HDD<=7200):
        cz = 5
    elif (HDD>7200) & (HDD<=9000):
        cz = 6
    elif (HDD>9000) & (HDD<=12600):
        cz = 7
    elif (HDD>12600):
        cz = 8
    else:
        print('HDD outside expected range')
        cz = 0
    return cz

l = []
for sf in fl:
    d = {}
    df = pd.read_fwf(sf, skiprows = 3, header = None, infer_nrows = 8760)
    HDD = get_HDD(df)
    cz = get_cz(HDD)
    d['City'] = sf.stem
    d['HDD'] = HDD
    d['Climate Zone'] = cz
    print(d)
    l.append(d)

df = pd.DataFrame(l)
print(df)
fn = 'ClimateZones.csv'
df.to_csv(fn, index = False)
print(f'\n\n Saved as {fn}')
