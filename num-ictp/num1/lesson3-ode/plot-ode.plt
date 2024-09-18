# plot.plt
set title "Ordinary Differential Eqs"
set grid
set xlabel "x"
set ylabel "y"
plot    "euler.dat" with linespoints,\
        "eulerMidpoint.dat" with linespoints,\
        "analyticFunc.dat" with linespoints
