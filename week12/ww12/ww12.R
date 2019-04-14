obs = c("A","B","C","D","E","F")
x1 = c(2,1,0,5,6,4)
x2 = c(4,5,4,2,2,1)
p3data = data.frame(obs,x1,x2)
attach(p3data)
plot(x2~x1)
text(x2~x1, labels=obs,data=p3data, cex=0.9, font=2)


set.seed(15); clust = sample(1:2,6, replace=T)
clust

cent1 = apply(cbind(x1,x2)[clust==1,],2,mean); cent1
cent2 = apply(cbind(x1,x2)[clust==2,],2,mean); cent2


dist(rbind(cbind(x1,x2),cent1,cent2))


clust = c(2,2,2,1,1,2)
cent1 = apply(cbind(x1,x2)[clust==1,],2,mean); cent1
cent2 = apply(cbind(x1,x2)[clust==2,],2,mean); cent2


dist(rbind(cbind(x1,x2),cent1,cent2))


#p4
x1c = c(-1,-2,-3,2,3,1)
x2c = c(1,2,1,-1,-1,-2)
x3c = c(-2,5,4,-1,-3,-3)
x = matrix(c(x1c,x2c,x3c),ncol=3)
pc.info = prcomp(x)


pc.info$rotation[,1]

pc.info$sdev
vjs = pc.info$sdev^2
pve = vjs/sum(vjs)
pve

pc1scores = pc.info$x[,1]  # first principal component score vector
pc2scores = pc.info$x[,2]  # second principal component score vector
pc1scores
pc2scores

#p5
library(MASS)
x = UScereal[1:7,c(2:10)]
x.scale = scale(x)
dim(x)
n = 7
p = 9

dist.x.scale = dist(x.scale, method="euclidean")
hc.complete = hclust(dist.x.scale,method="complete")
plot(hc.complete,cex=0.5)

nclust=2
set.seed(12)
memb = kmeans(x.scale,nclust)$cluster
memb

pc.info = prcomp(x,center=T,scale=T)
pc.info$rotation  #loadings

min(n-1,p)
summary(pc.info)

pc.info$sdev
vjs = pc.info$sdev^2
pve = vjs/sum(vjs)
cumsum(pve)
