install.packages("ISLR")
library(ISLR)

str(Caravan)
summary(Caravan$Purchase)
any(is.na(Caravan))

purchase <- Caravan[,86]

caravan <- scale(Caravan[,-86])

library(caTools)

sample = sample.split(Caravan$Purchase,SplitRatio=0.7)
train= subset(caravan,sample==TRUE)
test = subset(caravan,sample==FALSE)
tp <- subset(purchase,sample==TRUE)
tep <- subset(purchase,sample==FALSE)
library(class)

ppurchase <- knn(train,test,tp,k=5)
merror <- mean(tep != ppurchase)


ppurchase <- NULL
error <- NULL

for (i in 1:20) {
  set.seed(101)
  ppurchase <- knn(train,test,tp,k=i)
  error[i] <- mean(tep != ppurchase)
  print(error[i])
  
  
}

df <- data.frame(error,1:20)
library(ggplot2)
library(plotly)
pl <- ggplot(df,aes(y=error,x=X1.20)) + geom_point() + geom_line(lty="dotted",color="red")
ggplotly(pl)
