#!/bin/bash
#
#  this script plots a map for 3d data from a root finding program 
#  the data should be in the format
#
#      x       y       irr         k
#
#  where irr=1 whenever a real root was found (0 for complex root)
#  and k is number of iterations erquired when starting at point
#  z = complex(x, y)
# 
#  for high-resolution data files (N>1000000) plot with dots
#  (commented out below)
#

# data file name can be given as command-line option, otherwise
# default is used
datafile=${1:-data/complexnewton.dat}

# run gnuplot to create plot
gnuplot <<EOF
set size ratio 1
set border 31 lw 2

set xrange [-1.0 to 1.0]
set yrange [-1.0 to 1.0]
set zrange [0.5 to 1.5]

set xlabel 'x'
set ylabel 'y'

unset key
set view map


set term png font "/usr/share/fonts/truetype/freefont/FreeSans.ttf" 16 size 600, 600
set out 'real_root.png'

splot '$datafile' using 1:2:3 with points pointtype 7 pointsize 0.5
#splot '$datafile' using 1:2:3 with dots

set autoscale z
set cbtics 10
set out 'convergence.png'

splot '$datafile' using 1:2:4 with points palette pt 7 ps 1
#splot '$datafile' using 1:2:4 with dots palette 


 
EOF