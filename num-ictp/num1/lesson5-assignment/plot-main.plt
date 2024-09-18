# plot.plt
set multiplot layout 1,3 columnsfirst scale 1.1,0.9
set xlabel "x"
set ylabel "y"

set title "Empirical Cumulative Distribution Function"

plot    "empiricalCDF.dat"  using 1:2 with lines lt rgb "blue"  

set title "Histogram"
plot    "histogram.dat" using 1:2 with  boxes lt rgb "pink"

set title "Kernel Density Function"
plot    "kernel.dat" using 1:2 with  lines lt rgb "red"

