simple_regression = function(m, a, b, xmin, xmax, sigma) {
  x = runif(m, min = xmin, max = xmax)
  epsilon = rnorm(m, mean = 0, sd = sigma)
  y = c( a + b * x + epsilon)
  observations = mapply(cbind, x, y, SIMPLIFY=T)
  
  return (data.frame(x=observations[1,], y=observations[2,]))
}


coef_estimator = function(observations, m, a, b, sigma) {
  coef = lm(y~x, data=observations)
  confint(coef)
  print(confint(coef, level=0.95))
  x=observations$x
  cat("\n")
  cat("Confidence intervals for a:",confint(coef, level=0.95)[1,],"\n")
  cat("Confidence intervals for b:",confint(coef, level=0.95)[2,],"\n")
  
  standard_error_b= (sigma/sqrt(m)) * (1/sd(x))
  standard_error_a= sd(x)*(sqrt(1/m + (mean(x)*mean(x))/sum(x*x)))
  
  cat("Confidence intervals for a:", coef(coef)[1] - standard_error_a, " ",coef(coef)[1] + standard_error_a,"\n")
  cat("Confidence intervals for b:", coef(coef)[2] - standard_error_b, " ", coef(coef)[2] + standard_error_b,"\n")
  
  return(coef)
}


plotting = function(map_plot, a, b, coef) {
  pdf(paste("plot_regression", map_plot[1,1], ".pdf"), width=5, height=5, paper='special')
  
  plot(y~x, map_plot, col="grey")  
  abline(a = a, b = b, col="red")
  abline(coef, col="blue")
  
  dev.off ();
  
  plot(y~x, map_plot, col="grey")  
  abline(a = a, b = b, col="red")
  abline(coef, col="blue")
}


full_regression = function(m, a, b, xmin, xmax, sigma) {
  regression = simple_regression(m, a, b, xmin, xmax, sigma)
  coef = coef_estimator(regression, m, a, b, sigma)
  plotting(regression, a, b, coef)
}


full_regression(100, 3, 5, -200, 200, 1.5)
full_regression(10, 3, 5, -5, 5, 1)
full_regression(10000, 3, 5, -5, 5, 1)
full_regression(10, 3, 5, 5, 5.2, 1)
full_regression(10000, 3, 5, 5, 5.2, 1)
full_regression(10000, 3, 5, 5, 5.2, 0)
full_regression(10, 3, 5, 5, 5.2, 0.01)


