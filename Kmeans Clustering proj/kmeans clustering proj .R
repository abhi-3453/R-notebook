
df1 <- read.csv("winequality-red.csv",sep = ";")
df2 <- read.csv("winequality-white.csv",sep = ";")
head(df2)

df1$label <- c("red")
df2$label <- c("white")

wine <- bind_rows(df1,df2)
head(wine)
str(wine)

wine$label <- factor(wine$label)

pl1 <- ggplot(wine,aes(residual.sugar)) + geom_histogram(aes(fill=label),color="black",bins=50)
pl2 <- ggplot(wine,aes(citric.acid)) + geom_histogram(aes(fill=label),color="black",bins=50)
pl3 <- ggplot(wine,aes(alcohol)) + geom_histogram(aes(fill=label),color="black",bins=50)

splot1 <- ggplot(wine,aes(citric.acid,residual.sugar)) + geom_point(aes(color=label)) + theme_dark()
splot2 <- ggplot(wine,aes(volatile.acidity,residual.sugar)) + geom_point(aes(color=label)) + theme_dark()


cdata <- wine[,-13]
head(cdata)

clus <- kmeans(cdata,2,nstart = 20)
clusplot(wine,clus$cluster,color = T,shade=T)
print(clus)
table(clus$cluster,wine$label)

######
##     red white
## 1   85  3604
## 2 1514  1294
