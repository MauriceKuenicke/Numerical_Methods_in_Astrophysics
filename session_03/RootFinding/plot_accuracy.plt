# Plot with gnuplot -p plot_accuracy.plt

set terminal png
set title "Convergence of root finding algorithms"
set xlabel "# Iterations"
set ylabel "Error Margin"
set output "convergence_newton_bisection.png"

set logscale y

newton="data/accuracy_newton.dat"
bisection="data/accuracy_bisection.dat"

set style data linespoints
set key right top

plot newton using 1:2 title "Newton-Rhapson", \
bisection using 1:2  title "Bisection"