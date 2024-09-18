# plot.plt
set title "Ordinary Differential Eqs"
set grid
set xlabel "x"
set ylabel "y"
plot    "preypredator.dat" using 1:2 with linespoints,\
        "preypredator.dat" using 1:3 with linespoints
