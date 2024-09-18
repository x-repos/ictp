import os
from glob import glob
import noiseben
import numpy as np
import matplotlib.pyplot as plt
from obspy import read, Trace
from parameters import *
from time import time
import pandas as pd

processingtime1 = time()

pathin  = './cc/'
pathout = './cc-stack/'
sac     = '.sac'
pathout_metadata = './metadata'

noiseben.makefolder(pathout_metadata)
noiseben.makefolder(pathout)

ccpair_path = glob(pathin+'*')
ccpair_path.sort()
ccpair_name = [os.path.basename(x) for x in glob(pathin+'*')]
ccpair_name.sort()

stackorder = 0
count = []
for i in range(len(ccpair_path)):
    print('stacking: '+ccpair_name[i])
    tr = Trace()
    data = []
    ccpair_date_path = glob(ccpair_path[i]+'/*')
    ccpair_date_path.sort()
    count_sub = 0
    for cpath in ccpair_date_path:
        st = read(cpath)
        st.filter('bandpass',freqmin=freqmin_rma,freqmax=freqmax_rma,corners=2,zerophase=True)
        data_sub = st[0].data
        if not np.isnan(data_sub[0]):
            count_sub = count_sub + 1
            data.append(data_sub)
    count.append(count_sub)
    stack = noiseben.data_stack(data, stackorder)
    tr.data = stack
    tr.write(pathout+ccpair_name[i]+sac)

dist_count = {
    'ccname': ccpair_name,
    'cc-count': count
}

df = pd.DataFrame(dist_count)
df.to_csv(pathout_metadata+'/'+'cc-count.csv', index=False)

processingtime2 = time()
print(f'total processing time {round((processingtime2-processingtime1)/60)} minutes')