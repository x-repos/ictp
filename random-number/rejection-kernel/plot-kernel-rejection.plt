# plot.plt
set title "Kernel"
set xlabel "x"
set ylabel "y"

plot    "kernel-rejection.dat" using 1:2 with lines
