'''
input: the daily data in mseed files
output: the pyasdf (h5) data set that includes the station and waveform
---------------------------
to run MPI code:
mpiexec -n 4 python prepro-dailygroup-mpi.py
4: number of cores
---------------------------

NOTE: process
this part is the most important in preprocess step:
- demean
- detrend
- resample
- add 0 value for missing data
- remove the trace have less data then inc_hours
- save processed data to asdf file
- the asdf file can:
  + have less storage (compress) and used for Noise Module
  + save the inventory
'''
##############################################################
############################ CODE ############################
##############################################################

import os
from obspy import UTCDateTime, read, read_inventory
import noiseben             # Import code from NoisePy
from glob import glob           # Dealing with file name
from mpi4py import MPI
from time import time
from parameters import *

# path for input and otput data
pathin_waveform = './waveforms-daily-group/'    # path to the waveform(s)
pathin_station  = './stations/'                 # path to the station(s)    
pathout_asdf    = './waveforms-clean/' # output for asdf
sac = '.sac'

processingtime1 = time() # for processing time

starttime = UTCDateTime(starttime_str)
endtime   = UTCDateTime(endtime_str)

# name of station
station_list = [os.path.basename(x) for x in glob(pathin_waveform+'/*')]
station_list.sort()                             # remember to sort after reading from glob
for i in range(len(station_list)):
    station_list[i] = station_list[i][0:len(station_list[i])-6]

# --------------------------- MPI processing ---------------------------
comm = MPI.COMM_WORLD
rank = comm.Get_rank() # order of cores
size = comm.Get_size() # number of cores

# broad cast the data
# divided the process into segment of time
# deal with data in that segment

if rank == 0:
    all_chunk = noiseben.get_timelist(starttime, endtime, chunklength)
    if len(all_chunk) < 1:
        raise ValueError("Abort! no data chunk between %s and %s" % (starttime, endtime))
    splits = len(all_chunk) - 1
else:
    # save none for other ranks for broad-casting
    splits, all_chunk = [None for _ in range(2)]

# broadcast the variables
splits = comm.bcast(splits, root=0)
all_chunk = comm.bcast(all_chunk, root=0)

# MPI: loop through each time chunk
for ick in range(rank, splits, size): # it isn't in the increasing order 1,2,3,4,...
    currenttime = UTCDateTime(all_chunk[ick])           # step in duration
    # convert time to string - have the same format with file name
    currenttime_str = currenttime.strftime('%y%m%d')
    # create folder
    noiseben.makefolder(pathout_asdf+currenttime_str)
    for station in station_list:
        waveformname_group = glob(pathin_waveform+station+'-group/'+station+currenttime_str+'/*')
        waveformname_group.sort()
        if waveformname_group: # if the waveformname is not empty, continue
            try: # dealing with wrong data
                tr = read(pathin_waveform+station+'-group/'+station+currenttime_str+'/*')
            except:
                continue
            date_info = {'starttime':currenttime,'endtime':currenttime + chunklength}
            inv = read_inventory(pathin_station+station+'.xml')
            try:
                tr = noiseben.preprocess_raw(tr, inv, para, date_info)
                if len(tr) == 0:
                    print(+station+'-'+currenttime_str)
                    continue
            except:
                print('cannot run preprocess_raw module: '+station+'-'+currenttime_str)
                continue
            tr.write(pathout_asdf+currenttime_str+'/'
                     +station+currenttime_str+sac)
processingtime2 = time()
comm.barrier()
if rank == 0:
    print(f'total processing time {round((processingtime2-processingtime1)/60)} minutes')