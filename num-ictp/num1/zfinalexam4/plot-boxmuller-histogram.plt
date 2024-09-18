# plot.plt

set multiplot layout 1,2 columnsfirst


set xlabel "x"
set ylabel "y"

set title "Empirical Cumulative Distribution Function"

plot    "cumul.dat"  using 1:2 with lines lt rgb "blue"  

set title "Histogram"


plot    "histogram.dat"  using 1:2 with boxes
# plot    "boxmuller-histogram-y.dat"  using 1:2 with boxes
