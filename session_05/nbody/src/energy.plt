# Plot with gnuplot -p plot.plt

set terminal png
set title "System Energy"
set xlabel "Timestep"
set ylabel "System Energy"
set output "plots/system_energy.png"
set xrange [0 to 10]
set yrange [-0.05 to 0.05]
m="out.dat"

set style data linespoints
set key top

plot m using 1:7 title "System Energy"