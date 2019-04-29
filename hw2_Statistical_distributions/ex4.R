CLT = function(n) {
  sample_vector = c()
  for (i in seq(1000)) {
    sample_vector = c(sample_vector, mean(runif(n, min = -10, max = 10)))
  }
  sample_vector
}

for (n_value in c(1, 5, 10, 100)) {
  hist(CLT(n_value),col="gray", main="CLT: valoare medie vs nr. esantioane", breaks = 20)
  dev.copy(png, filename=paste("hist",n_value,".png"));
  dev.off ();
}