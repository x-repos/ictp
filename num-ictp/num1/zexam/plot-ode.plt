# plot.plt
set title "Ordinary Differential Eqs"
set grid
set xlabel "x"
set ylabel "y"
plot    "eulerMidpointPoint.dat" with linespoints,\
        "eulerMidpointInterval.dat" with linespoints

