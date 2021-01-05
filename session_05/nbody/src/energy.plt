# Plot with gnuplot -p plot.plt

set terminal png
set xlabel "Timestep"
set ylabel "Partial Energy Error"
set output "plots/energy_difference.png"
m="out.dat"

set nokey

set yrange[0 to 0.000000000000002]

plot m every 100000 using 1:12 with lines lw 2 title "Partial Energy Error"
