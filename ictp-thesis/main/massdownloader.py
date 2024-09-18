import obspy
from obspy.clients.fdsn.mass_downloader import RectangularDomain, \
    Restrictions, MassDownloader
from obspy.clients.fdsn import Client
from obspy import read, read_inventory
import cartopy
import shutil
import os

if os.path.isdir("stations"):
    shutil.rmtree("stations")
    shutil.rmtree("waveforms")
domain = RectangularDomain(minlatitude=-19.0982, maxlatitude=2.11118,
                           minlongitude=113.1521, maxlongitude=141.1678)
starttime=obspy.UTCDateTime("2014-04-01 00:00:00")
endtime  =obspy.UTCDateTime("2014-04-01 00:00:10")
restrictions = Restrictions(
    starttime=starttime,
    endtime=endtime,
    # Chunk it to have one file per day.
    chunklength_in_sec=86400,
    # If the location code is specified, the location priority list is not
    # used; the same is true for the channel argument and priority list.
    # network="*", station="*", location="", channel="*Z",
    network="*", station="*", location_priorities=["", "00", "10"], channel_priorities=["BHZ", "SHZ", "HHZ"],
    reject_channels_with_gaps=False,
    minimum_length=0.0,
    minimum_interstation_distance_in_m=100.0, )
mdl = MassDownloader(providers=["IRIS", "GFZ"])
mdl.download(domain, restrictions, mseed_storage="waveforms",
             stationxml_storage="stations");