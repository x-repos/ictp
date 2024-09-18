# plot.plt

set multiplot layout 2,1 rowsfirst
set title "Histogram"


set xlabel "x"
set ylabel "y"

plot    "boxmuller-histogram-x.dat"  using 1:2 with boxes
plot    "boxmuller-histogram-y.dat"  using 1:2 with boxes
