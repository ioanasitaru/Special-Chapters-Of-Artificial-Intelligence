for (p in seq(0.1, 0.9, 0.1)) {
  x = seq(0,20)
  plot(x, dbinom(x, 20, p),type = "b", ylab = "dbinom density")
  dev.copy(png, filename=paste("plot_binom",p,".png"));
  dev.off ();
}