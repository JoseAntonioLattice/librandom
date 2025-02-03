reset

min = 0.0
max =  1.0

n = 50#floor(sqrt(10000))
width = (max - min)/n

hist(x,width) =  width*(floor((x-min)/width)+0.5) + min
set boxwidth width
set style fill solid 0.5 # fill style

p 'random.dat' u (hist($1,width)):(1.0/(width*10**5)) smooth freq w boxes lc rgb "red", 1.0#exp(-x**2/2.0)/sqrt(2*pi)
