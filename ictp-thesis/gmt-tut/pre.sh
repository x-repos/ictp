gmt begin
	# Convert depth positive down to bathymetry positive up and decimate to a 1m lattice
	gmt convert -Em10 @geologists.txt -i0,1,2+s-1 -s | gmt blockmedian -R158:00W/156:58W/18:00N/19:40N -I1m > geo.txt
	# Create all cumulative and incremental grids, with misfit summary
	gmt greenspline geo.txt -Sc -C+c+i -Ggeo.grd -Z2 -Emisfit.txt
	# Create CPTs and prepare background map with static image and data
	gmt makecpt -Cturbo -T-5000/-1000 -H > z.cpt
	gmt makecpt -Cpolar -T-100/100 -H > dz.cpt
	gmt grdimage -JM8c -X0 -Y0 -R158:00W/156:58W/18:00N/19:40N @earth_relief_01m.grd -Cz.cpt -B0 -Ei -I+d
	gmt grdcontour @earth_relief_01m.grd -C200 -S8
	gmt plot geo.txt -Ss2p -Gblack
	gmt colorbar -Cz.cpt -DjTL+w4c+h+o11p/8p -Bxaf -By+l"km" -F+gwhite+p0.5p -W0.001
gmt end
