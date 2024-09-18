# plot.plt
set title "Histogram"
set xlabel "x"
set ylabel "y"

plot    "histogram-transformation.dat" using 1:2 with boxes
