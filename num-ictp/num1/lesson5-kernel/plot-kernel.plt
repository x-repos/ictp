# plot.plt
set title "Kernel Density Function"
set xlabel "x"
set ylabel "y"

plot    "kernel.dat" using 1:2 with  lines
