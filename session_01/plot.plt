set terminal x11 0
set title "Orbital periods of planets around the Sun"
set nokey
set logscale y
set logscale x
set xlabel "distance (au)"
set ylabel "orbital period (years)"
set output "distancePeriod.png"
m="period.dat"
plot m using 1:2 with linespoints