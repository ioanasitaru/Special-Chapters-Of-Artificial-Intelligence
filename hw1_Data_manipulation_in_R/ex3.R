x = scan(nmax=6, what="numeric")
max(x)
min(x)
mean(x)
median(x)
sd(x)
sort(x)

#standardizare
y = (x-mean(x))/sd(x)
mean(y)
sd(y)
