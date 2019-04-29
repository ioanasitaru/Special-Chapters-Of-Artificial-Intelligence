for (sigma in c(0.1, 1, 10)) {
  x = seq(-20,20)
  plot(x, dnorm(x, mean = 0, sd = sigma),type = "b", ylab = "dnorm density")
  dev.copy(png, filename=paste("plot_norm",sigma,".png"));
  dev.off ();
}
