set terminal x11 0
set title "Orbital Period of Planets around the Sun in years"
set nokey
set logscale y
set logscale x
set xlabel "distance (au)"
set ylabel "orbital period (years)"
set output "distancePeriod.png"
m="period.dat"
plot m using 1:2 with linespoints