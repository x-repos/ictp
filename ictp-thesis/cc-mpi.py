import os
from obspy import UTCDateTime, read, read_inventory
import noiseben
from glob import glob           # Dealing with file name
import obspy
import numpy as np
import matplotlib.pyplot as plt
from mpi4py import MPI          # for parallel programing
from time import time
from parameters import *
from obspy import Trace, Stream
from scipy.signal import fftconvolve, hilbert


'''
Input: the daily waveform.h5
./waveforms-processed-asdf/*
Output: the cc.h5
./cc-asdf/*

TODO:
to run MPI code:
mpiexec -n 4 python cc-mpi.py
4: number of cores
'''

# calculating the processing time

pathin     = './freq-white/'
pathout_cc = './cc-daily/'
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

    noiseben.makefolder(pathout_cc+currenttime_str)
    station = [os.path.basename(x) for x in glob(pathin+currenttime_str+'/*')]
    station.sort() # xD
    nstation = len(station)
    if nstation < 2:
        continue
    for i in range(nstation):
        station[i] = station[i][0:len(station[i])-10]
    # create empty data set to speed up the cross correlation
    freq_whitened = []
    outloop = False
    for i in range(nstation):
        try:
            freq_whitened_sub = np.load(pathin+currenttime_str
                                        +'/'+station[i]
                                        +currenttime_str+'.npy')

            if len(freq_whitened_sub) == 0:
                outloop = True
        except:
            outloop = True
            continue
        freq_whitened.append(freq_whitened_sub)
    if outloop:
        continue

    # Cross correlation
    for i in range(nstation-1): # i for source
        for j in range(i+1, nstation): # j for receiver

            x_corr = noiseben.cross_correlation_bigdata(freq_whitened[i],
                                                        freq_whitened[j],
                                                        para)
            if len(x_corr) > 0:
                x_corr.write(pathout_cc+currenttime_str+'/'+station[i]
                             +'_'+station[j]+'_'+currenttime_str+sac, format='SAC')
            print('cross correlating between | '+currenttime_str+' | '
                    +station[i]+'-'+station[j])

processingtime2 = time()
comm.barrier() # end the parallel

if rank == 0:
    print(f'total processing time {round((processingtime2-processingtime1)/60)} minutes')
