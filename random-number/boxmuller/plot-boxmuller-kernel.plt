# plot.plt

set multiplot layout 2,1 rowsfirst
set title "Kernel Density Function"


set xlabel "x"
set ylabel "y"

plot    "boxmuller-kernel-x.dat"  using 1:2 with lines
plot    "boxmuller-kernel-y.dat"  using 1:2 with lines
