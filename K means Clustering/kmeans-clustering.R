library(ISLR)
df <- iris
head(df)
library(ggplot2)
pl <-ggplot(df,aes(Petal.Length,Petal.Width,color=Species))
print(pl + geom_point(size=4))


library(cluster)
set.seed(101)

clus <- kmeans(df[,1:4],3, nstart=2)
print(clus)
table(clus$cluster,iris$Species)


pl2 <- clusplot(df,clus$cluster,color = T,shade = T)