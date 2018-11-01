######################################################################################################
# D05b Unsupervised Learning
# By William Yu, UCLA Anderson Forecast
# 10/20/2018, updated 10/29/2018
##################################################################################################### 

# Chapter 10 Lab 1: Principal Components Analysis

setwd("~/Documents/DataScience/UCLADatascience/Project 5")

?USArrests
USArrests=USArrests
str(USArrests)
head(USArrests)
states=row.names(USArrests)
states
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
?prcomp
pr.out=prcomp(USArrests, scale=TRUE)
# Scale=T means standardize or normalize the different scales of variables
summary(pr.out)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation  #loading factor
dim(pr.out$x)
biplot(pr.out, scale=0)
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
a=c(1,2,8,-3)
cumsum(a)

# Chapter 10 Lab 2: Clustering

# K-Means Clustering

set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
plot(x)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
plot(x)
km.out=kmeans(x,2,nstart=20)
km.out$cluster
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)
set.seed(4)
km.out=kmeans(x,3,nstart=20)
km.out
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)
set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss

# Hierarchical Clustering

hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)
par(mfrow=c(1,1))
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)
xsc=scale(x)
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")
x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")

# Chapter 10 Lab 3: NCI60 Data Example

# The NCI60 data

library(ISLR)
?NCI60
# 64 by 6830
# 64 cancer cells from 64 patients for 6830 genes
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)
nci.labs[1:10]
table(nci.labs)

# PCA on the NCI60 Data
pr.out=prcomp(nci.data, scale=TRUE)
summary(pr.out)
biplot(pr.out, scale=0)

Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,xlab="PC1",ylab="PC2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,xlab="PC1",ylab="PC3")
par(mfrow=c(1,1))
summary(pr.out)
plot(pr.out)
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")
par(mfrow=c(1,1))

# Clustering the Observations of the NCI60 Data
sd.data=scale(nci.data)
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs,  main="Single Linkage", xlab="", sub="",ylab="")
par(mfrow=c(1,1))
hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out,4)
table(hc.clusters,nci.labs)
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")
hc.out
set.seed(2)
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
table(km.clusters,hc.clusters)
hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out,4), nci.labs)


# PCA on CA County employment
setwd("~/Documents/DataScience/UCLADatascience/Project 6")

library(readxl)  
library(dplyr)
cacounty <- data.frame(read_excel("W06a_cacounty.xlsx"))  

cacounty = cacounty %>% filter(!grepl("Annual", Month)) # remove those rows that are annaul summary

cacounty1=cacounty[,-1]             # remove the time date column
cacounty2=data.frame(t(cacounty1))  # transpose the data
apply(cacounty2,1,mean)

# K-means clustering on CA County employment
sd.data=scale(cacounty2)
data.dist=dist(sd.data)
plot(hclust(data.dist, method="average"), main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="complete"), main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), main="Single Linkage", xlab="", sub="",ylab="")

cacounty3=log(cacounty2)
apply(cacounty3,1,mean)

sd.data=scale(cacounty3)
data.dist=dist(sd.data)
plot(hclust(data.dist, method="average"), main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="complete"), main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), main="Single Linkage", xlab="", sub="",ylab="")

# CA County employment growth data
cacountyg <- data.frame(read_excel("W06a_cacounty.xlsx", sheet="growth"))  

cacountyg = cacountyg %>% filter(!grepl("Annual", Month)) # remove those rows that are annaul summary
cacountyg1=cacountyg[,-1]             # remove the time date column
cacountyg2=data.frame(t(cacountyg1))  # transpose the data
apply(cacountyg2,1,mean)

sd.data=scale(cacountyg2)
data.dist=dist(sd.data)
plot(hclust(data.dist, method="average"), main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="complete"), main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), main="Single Linkage", xlab="", sub="",ylab="")

## Average Linkage: proximity between two clusters is the arithmetic mean 
## Complete Linkage: proximity between two clusters is between their two most distant objects
## Single Linkage: proximity between two clusters is between their two closest objects



