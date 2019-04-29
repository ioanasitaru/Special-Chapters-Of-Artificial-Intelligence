simple_regression = function(m, a, b, xmin, xmax, sigma) {
  x = runif(m, min = xmin, max = xmax)
  epsilon = rnorm(m, mean = 0, sd = sigma)
  y = c( a + b * x + epsilon)
  observations = mapply(cbind, x, y, SIMPLIFY=T)
  
  return (data.frame(x=observations[1,], y=observations[2,]))
}


coef_estimator = function(observations, m, a, b, sigma) {
  x=observations$x
  y=observations$y
  b_estim = sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))*(x-mean(x)))
  a_estim = mean(y) - b_estim * mean(x)
  y_estim = c( a_estim + b_estim * x)
  
  coef = lm(y~x, data=observations)
  print(y)
  print(y_estim)
  cat("\n")
 
  standard_error_b= (sigma/sqrt(m)) * (1/sd(x))
  standard_error_a= sd(x)*(sqrt(1/m + (mean(x)*mean(x))/sum(x*x)))
  
  cat("Confidence intervals for a:", a_estim - standard_error_a, " ",a_estim + standard_error_a,"\n")
  cat("Confidence intervals for b:", b_estim - standard_error_b, " ", b_estim + standard_error_b,"\n")
  pdf(paste("plot_regression", observations[1,1], ".pdf"), width=5, height=5, paper='special')
  
  plot(y~x, observations, col="grey")  
  abline(a = a, b = b, col="red")
  abline(coef, col="blue")
  abline(a= a_estim, b = b_estim, col="black")
  
  dev.off ();
  
  plot(y~x, observations, col="grey")  
  abline(a = a, b = b, col="red")
  abline(coef, col="blue")
  cat("Intercept")
  print(a_estim)
  cat("Slope")
  print(b_estim)
  abline(a= a_estim, b = b_estim, col="black")
  
}


full_regression = function(m, a, b, xmin, xmax, sigma) {
  regression = simple_regression(m, a, b, xmin, xmax, sigma)
  coefs = coef_estimator(regression, m, a, b, sigma)
}


full_regression(100, 3, 5, -200, 200, 1.5)
full_regression(10, 3, 5, -5, 5, 1)
full_regression(10000, 3, 5, -5, 5, 1)
full_regression(10, 3, 5, 5, 5.2, 1)
full_regression(10000, 3, 5, 5, 5.2, 1)
full_regression(10000, 3, 5, 5, 5.2, 0)
full_regression(10, 3, 5, 5, 5.2, 0.01)


