# plot.plt
set title "Histogram"
set xlabel "x"
set ylabel "y"

plot    "histogram.dat" using 1:2 with boxes
