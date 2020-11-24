# Plot with gnuplot -p plot.plt

set terminal png
set title "Time Complexity Comparison"
set logscale y
set logscale x
set xlabel "Number of elements"
set ylabel "Time in s"
set output "timecomplexity.png"
m="time_complexity.dat"

set style data linespoints
set key right bottom

plot m using 3:4 title "Insertion Sort", \
m using 1:2  title "Quick Sort"