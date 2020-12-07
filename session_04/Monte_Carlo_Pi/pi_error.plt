# Plot with gnuplot -p plot.plt

set terminal png
set title "Relative error of pi over number of random points"
set logscale y
set logscale x
set xlabel "Number of random points"
set ylabel "Relative error of pi"
set output "error_pi.png"
m="error_pi.dat"

set style data linespoints
set key right bottom

plot m using 1:3 title "Deviation of the Monte Carlo value for pi from pi"