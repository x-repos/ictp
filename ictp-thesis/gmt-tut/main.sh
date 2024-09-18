gmt begin
	# Plot current solution with contors in the right panel
	gmt grdimage -JM8c -X8c -Y0 geo_cum_${MOVIE_ITEM}.grd -Cz.cpt -B0 -Ei -I+d
	gmt grdcontour geo_cum_${MOVIE_ITEM}.grd -C200 -S8
	gmt plot geo.txt -Ss2p -Cz.cpt
	# Plot the increments in the center panel and add color bar at bottom
	gmt grdimage -X8c geo_inc_${MOVIE_ITEM}.grd -Cdz.cpt -B0 -Ei
	gmt colorbar -Cdz.cpt -DjTR+w4c+h+o19p/8p+e -Bxaf -By+l"m" -F+gwhite+p0.5p
gmt end
