# Plotting the error margin over the number of iterations
# Plot with gnuplot -p plot_convergence.plt

set terminal png
set title "Function Evaluations vs. Accuracy"
set xlabel "# Evaluations"
set ylabel "Relative Error"
set output "accuracy.png"

set logscale y
set logscale x

trapez="data/accuracy_trapez.dat"
simpson="data/accuracy_simpson.dat"

set style data linespoints
set key right top

plot trapez using 1:2 title "Trapez", \
simpson using 1:2  title "Simpson"