f = function(x){ 
  2^x 
}

plot_f = function(a,b) {
  plot(f, a, b, col="red", lwd=2, main = paste("f(x)=2^x, x in [", a,",",b,"]"))
}

plot_f(0,3)