employ <- read.csv(file.choose())
emfact <- factanal(employ[3:11],4,scores="regression")
print(emfact, loadings=T, cutoff=0)
round(emfact$uniqueness,4)
round(rowSums(emfact$loadings^2),4) #Communalities
emfact <- factanal(employ[3:11],4,scores="regression",rotation="none")
print(emfact,loadings=T,cutoff=0)
plot(c(-4,4),c(-4,4),xlab="social serv./transportation/communication, not ag",
     ylab="manufacturing but not mining",type="n")
text(emfact$scores[,1],emfact$scores[,2],employ$Country,cex=0.8) 
plot(c(-4,4),c(-4,4),xlab="service and construction, not agriculture", 
     ylab = "finance and service, not manufacturing and mining",type="n")
text(emfact$scores[,3],emfact$scores[,4],employ$Country,cex=0.8)
euro <- read.csv(file.choose())
X <- scale(euro[,3:11])
colnames(X) <- colnames(euro[,3:11])
rownames(X) <- euro[,1]
round(X, 2)
distances <- dist(X,diag=TRUE,upper=TRUE)
plot(hclust(as.dist(distances),method="single"),ylab="Euclidean distance")

# k-means clustering
#
employ <- read.csv("Euroemp.csv",header=TRUE,row.names=1)
attach(employ)
# Standardizing the data with scale()
matstd.employ <- scale(employ[,2:10])
# K-means, k=2, 3, 4, 5, 6
# Centers (k's) are numbers thus, 10 random sets are chosen
(kmeans2.employ <- kmeans(matstd.employ,2,nstart = 10))
# Computing the percentage of variation accounted for. Two clusters
perc.var.2 <- round(100*(1 - kmeans2.employ$betweenss/kmeans2.employ$totss),1)
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2
# Computing the percentage of variation accounted for. Three clusters
(kmeans3.employ <- kmeans(matstd.employ,3,nstart = 10))
perc.var.3 <- round(100*(1 - kmeans3.employ$betweenss/kmeans3.employ$totss),1)
names(perc.var.3) <- "Perc. 3 clus"
perc.var.3
# Computing the percentage of variation accounted for. Four clusters
(kmeans4.employ <- kmeans(matstd.employ,4,nstart = 10))
perc.var.4 <- round(100*(1 - kmeans4.employ$betweenss/kmeans4.employ$totss),1)
names(perc.var.4) <- "Perc. 4 clus"
perc.var.4
# Computing the percentage of variation accounted for. Five clusters
(kmeans5.employ <- kmeans(matstd.employ,5,nstart = 10))
perc.var.5 <- round(100*(1 - kmeans5.employ$betweenss/kmeans5.employ$totss),1)
names(perc.var.5) <- "Perc. 5 clus"
perc.var.5
(kmeans6.employ <- kmeans(matstd.employ,6,nstart = 10))
# Computing the percentage of variation accounted for. Six clusters
perc.var.6 <- round(100*(1 - kmeans6.employ$betweenss/kmeans6.employ$totss),1)
names(perc.var.6) <- "Perc. 6 clus"
perc.var.6
#
# Saving four k-means clusters in a list
clus1 <- matrix(names(kmeans4.employ$cluster[kmeans4.employ$cluster == 1]), 
                ncol=1, nrow=length(kmeans4.employ$cluster[kmeans4.employ$cluster == 1]))
colnames(clus1) <- "Cluster 1"
clus2 <- matrix(names(kmeans4.employ$cluster[kmeans4.employ$cluster == 2]), 
                ncol=1, nrow=length(kmeans4.employ$cluster[kmeans4.employ$cluster == 2]))
colnames(clus2) <- "Cluster 2"
clus3 <- matrix(names(kmeans4.employ$cluster[kmeans4.employ$cluster == 3]), 
                ncol=1, nrow=length(kmeans4.employ$cluster[kmeans4.employ$cluster == 3]))
colnames(clus3) <- "Cluster 3"
clus4 <- matrix(names(kmeans4.employ$cluster[kmeans4.employ$cluster == 4]), 
                ncol=1, nrow=length(kmeans4.employ$cluster[kmeans4.employ$cluster == 4]))
colnames(clus4) <- "Cluster 4"
list(clus1,clus2,clus3,clus4)
#
detach(employ)
#
# End of script