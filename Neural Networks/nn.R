install.packages("MASS")
library(MASS)

df <- Boston

head(df)
str(df)

maxs <- apply(df, 2, max)
mins <- apply(df, 2, min)

sdf <- scale(df,center= mins,scale = maxs-mins)
sdf <- as.data.frame(sdf)

library(caTools)


sample = sample.split(sdf$medv, SplitRatio = 0.7)
train = subset(sdf, sample== TRUE)
test = subset(sdf , sample == FALSE)

library(neuralnet)

n <- names(train)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))

nn <- neuralnet(formula = f,data=train,hidden= c(5,5), linear.output = T)
plot(nn)

pred <- compute(nn,test[1:13])

tpred <- pred$net.result*(max(df$medv)-min(df$medv))+min(df$medv)
testr <- test$medv*(max(df$medv)-min(df$medv))+min(df$medv)

MSE.nn <- sum((testr - tpred)^2)/nrow(test)
MSE.nn
edf <- data.frame(testr,tpred)
head(edf)
library(plotly)
pl <- ggplot(edf,aes(x=testr,y=tpred)) + geom_point() + stat_smooth()
pl
