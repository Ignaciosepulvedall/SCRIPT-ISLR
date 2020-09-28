#UNSUPERVISED LEARNING
#PCA
fix(USArrests)
apply(USArrests,2,mean)
pr_out=prcomp(USArrests,scale =TRUE)
biplot(pr.out,scale=0)
#VEAMOS LA VARIANZA EXPLICADA
pr_var=pr_out$sdev**2
pr_var
#computo the variance for each component
pve=pr_var/sum(pr_var)
pve
##plot
plot(pve,xlab="Principal Component",ylab=" Proportion of
Variance Explained ", ylim=c(0,1) ,type='b')
plot(cumsum(pve),xlab="Principal Component ", ylab ="Cumulative Proportion of Variance Explained",ylim=c(0,1),type='b')
#K-MEANS CLUSTERING
set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
km_out=kmeans(x,2,nstart=20)
#plotting
plot(x,col=(km_out$cluster+1),main="K-Means Clustering Results with K=2",xlab ="",ylab="", pch=20, cex=2)
km_out =kmeans(x,3,nstart=20)
km_out
#nstart larges nos dan seguridad ante local optimun
#HIERACHICAL CLUSTERING
hc_complete=hclust(dist(x),method="complete")
hc_average=hclust(dist(x),method ="average")
hc_single=hclust (dist(x), method ="single")
par(mfrow=c(1,3))
plot(hc_complete,main="Complete Linkage",xlab="",sub ="",cex =.9)
plot(hc_average , main="Average Linkage",xlab="",sub ="",cex =.9)
plot(hc_single , main="Single Linkage", xlab="", sub ="",cex =.9)
xsc=scale(x)
plot(hclust(dist(xsc),method="complete"),main="Hierarchical Clustering with Scaled Features")
#
x=matrix(rnorm(30*3),ncol =3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd,method ="complete"), main=" Complete Linkage with Correlation -Based Distance ",xlab="",sub="")
######NIC60 DATA EXAMPLE####
library(ISLR)
nci_labs=NCI60$labs
nci_data=NCI60$data
nci_labs[1:4]
table(nci_labs)
#PCA
pr_out=prcompl(nci_data,scale=TRUE)
#plot
Cols=function(vec){
    cols=rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}
par(mfrow =c(1,2))
plot(pr_out$x [,1:2], col =Cols(nci_labs),pch=19,
       xlab ="Z1",ylab="Z2")
plot(pr_out$x[,c(1,3)],col=Cols(nci_labs),pch=19,
       xlab="Z1",ylab="Z3")
summary(pr_out)
plot(pr_out)
#PVE
pve=100*pr_out$sdev**2/sum(pr_out$sdev**2)
par(mfrow=c(1,2))
plot(pve,type ="o", ylab="PVE ", xlab=" Principal Component ",
       col =" blue")
plot(cumsum(pve),type="o",ylab ="Cumulative PVE",xlab="Principal Component",col="brown3")
#Clustering the observations data
sd_data=scale(nci_data)
par(mfrow=c(1,3))
data_dist=dist(sd_data)
plot(hclust(data_dist),labels=nci_labs,main="Complete Linkage ",xlab ="", sub ="", ylab ="")
plot(hclust(data_dist,method="average"),labels=nci_labs,main="Average Linkage",xlab ="", sub ="", ylab ="")
plot(hclust (data_dist , method ="single"),labels=nci_labs,main="Single Linkage",xlab="",sub ="",ylab ="")
#PATRONES
hc_out=hclust(dist(sd_data))
hc_clusters=cutree(hc_out ,4)
table(hc_clusters,nci_labs)
#y estos sugieren que
par(mfrow =c(1,1))
plot(hc_out,labels=nci_labs)
abline(h=139,col="red")
set.seed (2)
km_out =kmeans(sd_data,4,nstart =20)
km_clusters=km_out$cluster
table(km_clusters,hc_clusters)
#
hc_out=hclust(dist(pr_out$x[,1:5]))
plot(hc_out,labels=nci_labs,main="Hier. Clust . on First
Five Score Vectors ")
table(cutree(hc_out,4),nci_labs)