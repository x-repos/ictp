import os
from obspy import UTCDateTime, read
import noiseben
from glob import glob           # Dealing with file name
import obspy
import numpy as np
import matplotlib.pyplot as plt
from mpi4py import MPI          # for parallel programing
from time import time
from parameters import *
from obspy import Stream
import warnings
warnings.filterwarnings("ignore")

'''
Input: the daily waveform.sac
./waveforms-clean/*
Output: the .sac
./time-norm/*

TODO:
to run MPI code:
mpiexec -n 4 python norm-white-mpi.py
4: number of cores
'''

# calculating the processing time

pathin  = './waveforms-clean/'
pathout_timenorm = './time-norm/'
pathout_freqwhitened = './freq-white/'
sac = '.sac'

processingtime1 = time() # for processing time

# --------------------------- MPI processing ---------------------------
comm = MPI.COMM_WORLD
rank = comm.Get_rank() # order of cores
size = comm.Get_size() # number of cores

# broad cast the data
# divided the process into segment of time
# deal with data in that segment
starttime = UTCDateTime(starttime_str)
endtime   = UTCDateTime(endtime_str)

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

corr_streams=Stream()  # stream of daily correlograms
# MPI: loop through each time chunk
for ick in range(rank, splits, size): # it isn't in the increasing order 1,2,3,4,...
    currenttime = UTCDateTime(all_chunk[ick])        # step in duration
    # convert time to string - have the same format with file name
    currenttime_str = currenttime.strftime('%y%m%d')

    noiseben.makefolder(pathout_timenorm+currenttime_str)
    noiseben.makefolder(pathout_freqwhitened+currenttime_str)

    station = [os.path.basename(x) for x in glob(pathin+currenttime_str+'/*')]
    station.sort() # xD
    nstation = len(station)
    if nstation < 2:
        continue
    for i in range(nstation):
        station[i] = station[i][0:len(station[i])-10]
    # create empty data set to speed up the cross correlation
    st = obspy.Stream()
    outloop = False
    for i in range(nstation):
        try:
            tr = read(pathin+currenttime_str+'/'+station[i]+currenttime_str+sac)[0]
            if len(tr) == 0:
                outloop = True
        except:
            outloop = True
            continue
        print('normalizing & whitening | '+currenttime_str+' | ' + tr.stats.station)
        tr_norm = noiseben.time_domain_normalization_bigdata(tr, para)
        tr_norm.write(pathout_timenorm+currenttime_str+'/'+station[i]+currenttime_str+sac)
        freq_whitened = noiseben.new_spectral_whitening_bigdata(tr_norm,para)
        np.save(pathout_freqwhitened+currenttime_str+'/'+station[i]+currenttime_str+'.npy',freq_whitened)
    if outloop:
        continue

processingtime2 = time()
comm.barrier() # end the parallel

if rank == 0:
    print(f'total processing time {round((processingtime2-processingtime1)/60)} minutes')