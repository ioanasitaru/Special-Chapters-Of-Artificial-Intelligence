data_source = data.frame(read.csv("swiss.dat", header=T))
X = data_source[-1]

pca = prcomp(X,center = TRUE,scale = TRUE)
pca
summary(pca)
cor(X)

biplot(pca, scale = 0)


#Value variance explained
screeplot(pca, 6, type = c("lines"))

#Percentage variance explained
varr = pca$sdev^2

prop_var_explained = varr/sum(varr)

plot(prop_var_explained, xlab = "Principal Component",
     ylab = "Proportion Variance",
     type = "b")


#Percentage cumulative variance explained
plot(cumsum(prop_var_explained), xlab = "Principal Component",
     ylab = "Cumulative variance",
     type = "b")