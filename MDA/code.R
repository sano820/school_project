#데이터 불러오기
pitching <- read.csv("C:/PBLGYU/123.csv",header=TRUE,fileEncoding="euc-kr")
head(pitching)
summary(pitching)
colnames(x = pitching)

#주성분 분석
pitching.Xa <- pitching[-1:-2]
pitching.Xb <- pitching.Xa[-12:-14]
pitching.Xc <- pitching.Xb[-2]
pitching.Xd <- pitching.Xc[-14]
pitching.X <- pitching.Xd[-11]
pitching.X.cor <- cor(pitching.X)
print(pitching.X.cor)
pitching.cor.prcomp <- prcomp(pitching.X,center=TRUE,scale=TRUE)
print(pitching.cor.prcomp)
pitching.cor.prcomp$sdev^2
summary(pitching.cor.prcomp)
screeplot(pitching.cor.prcomp,type="l")
pitching.cor.score <- cbind(pitching,pitching.cor.prcomp$x[,1:4])

pitching.X.cor <- as.matrix(pitching.X.cor)
pitching.X.princomp <- princomp(covmat=pitching.X.cor)
summary(pitching.X.princomp,loadings=T,cutoff=0.0001)

#계보적 군집분석
pitching.Y <- pitching.cor.prcomp$x[, 1:4]
pitching.Z <- scale(pitching.Y,center=TRUE,scale=TRUE)
pitching.Z.dist <-dist(pitching.Z,method="euclidean")
pitching.Z.hclust <- hclust(pitching.Z.dist,method="ward.D")
plot(pitching.Z.hclust)
rect.hclust(pitching.Z.hclust,k=4,border="red")

hcluster <- cutree(pitching.Z.hclust,k=4)
pitching.Y.hclust <- data.frame(pitching.Y,hcluster)
options(max.print = 1000000)
pitching.Y.hclust

#Kmeans 군집분석
pitching.Z <- scale(pitching.Y,center=TRUE,scale=TRUE)
pitching.Z.kmeans <- kmeans(pitching.Z,centers=3,nstart=30)
pitching.Z.kmeans$iter
pitching.Z.kmeans$size
pitching.Z.kmeans$centers
kcluster <- pitching.Z.kmeans$cluster
pitching.Y.kclust <- data.frame(pitching.Y,kcluster)
table(hcluster)


library(factoextra)
fviz_nbclust(pitching.Y,kmeans,method="wss",k.max=10)
fviz_nbclust(pitching.Y,kmeans,method="silhouette",k.max=10)
fviz_nbclust(pitching.Y,kmeans,method="gap_stat",nboot=500)
library(NbClust)
NbClust(data=pitching.Y,distance = "euclidean",min.nc=2,max.nc=15,method="kmeans")
NbClust(data=pitching.Y,distance = "euclidean",min.nc=2,max.nc=15,method="kmeans",index="ccc")
summary(pitching.Y.hclust)



write.csv(pitching.Y.hclust, "clustering1.csv", row.names = TRUE)
