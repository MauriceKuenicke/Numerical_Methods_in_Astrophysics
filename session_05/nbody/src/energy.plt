# Plot with gnuplot -p plot.plt

set terminal png
set title "Energydifference"
set xlabel "Timestep"
set ylabel "Energydifference"
set output "plots/energy_difference.png"
m="out.dat"

set style data linespoints
set key top

plot m using 1:9 title "Difference"



set title "System Energy"
set xlabel "Timestep"
set ylabel "System Energy"
set output "plots/system_energy.png"

set style data linespoints
set key top

plot m using 1:8 title "System Energy"
