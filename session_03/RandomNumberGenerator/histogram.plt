# Plot with gnuplot -p histogram.plt
#
#The code is taken and adapted from: http://gnuplot-surprising.blogspot.com/2011/09/statistic-analysis-and-histogram.html
#

set terminal png

n=40 #number of intervals
max=4. #max value
min=-4. #min value
width=(max-min)/n #interval width

#function used to map a value to the intervals
hist(x,width)=width*floor(x/width)+width/2.0
set boxwidth width*0.9
set style fill solid 0.5 # fill style

set output "random_normal_histogram.png"

#count and plot
plot "data/random_normal_DATA.dat" u (hist($1,width)):(1.0) smooth freq w boxes lc rgb"blue" notitle