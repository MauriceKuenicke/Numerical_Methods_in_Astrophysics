#!/bin/bash
# run with bash plotDist.sh data/datafilename


# data file name can be given as command-line option, otherwise
# default is used
datafile=${1:-data/LCG_DATA.dat}

# run gnuplot to create plot
gnuplot -p <<EOF


splot[0:1][0:1][0:1]'$datafile'


 
EOF