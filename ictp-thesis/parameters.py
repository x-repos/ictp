# Set up the parameters

# pre-process
chunklength     = 86400             # chunk for daily data (24*60*60)
rm_resp_out     = 'DISP'             # choose remove response output = VEL
rm_resp         = 'inv'             # use inventory to remove response
freqmin         = 1./100            # pre filtering frequency bandwidth
freqmax         = 1./5               # used for filter + remove response
samp_freq       = 1.0               # from preprocessing step - resample refrequency
max_files       = 10                # max number of trace per day
portion_data    = 0.95  
# xcorr
lc_stack        = 1./100
hc_stack        = 1./5
smooth_N        = 50
maxlag          = 2000
freqmin_rma     = 1./35
freqmax_rma     = 1./15

para            = {'chunklength':chunklength,'samp_freq':samp_freq, 'freqmin':freqmin,'freqmax':freqmax,
                  'rm_resp':rm_resp, 'rm_resp_out':rm_resp_out, 'max_files':max_files,
                  'portion_data':portion_data, 'lc_stack':lc_stack,'hc_stack':hc_stack,'smooth_N':smooth_N,
                  'maxlag':maxlag, 'freqmin_rma':freqmin_rma, 'freqmax_rma':freqmax_rma}

# time
starttime_str   = '2014-12-01T00:00:00'          
endtime_str     = '2016-12-31T00:00:00'


