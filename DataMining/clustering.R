#clustering

data <- read.csv("modified_data1.csv", header=TRUE, fileEncoding = "euc-kr")
head(data)
summary(data)
colnames(x = data)


#주성분 분석
str(data.X)

library(psych)

colnames(x=data.X)
data.X.cor <- cor(data.X)
print(data.X.cor)
data.cor.prcomp <- prcomp(data.X,center=TRUE,scale=TRUE)
print(data.cor.prcomp)
data.cor.prcomp$sdev^2
summary(data.cor.prcomp)
scree(data.X,hline=1)


data.Y <- data.cor.prcomp$x[, 1:9]

library(factoextra)
library(cluster)
fviz_nbclust(data.Y,kmeans,method="wss",k.max=20)
fviz_nbclust(data.Y,kmeans,method="silhouette")
fviz_nbclust(data.Y,kmeans,method="gap_stat")



#Kmeans 군집분석
data.Z <- scale(data.Y,center=TRUE,scale=TRUE)

data.Z.kmeans <- kmeans(data.Z,centers=3)
data.Z.kmeans$iter
data.Z.kmeans$size
data.Z.kmeans$centers
kcluster <- data.Z.kmeans$cluster
data.Y.kclust <- data.frame(data.Y,kcluster)
table(kcluster)


write.csv(data.Y.kclust, "clustering1.csv", row.names = TRUE)

getwd()
