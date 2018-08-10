

df <- read.csv('Social_Network_Ads.csv')
head(df)
df <- df[-1:-2]

library(ggplot2)
pl1 <- ggplot(df,aes(y=EstimatedSalary,x=Age)) + geom_point(aes(color=factor(Purchased)))

library(caTools)
set.seed(101)
sample = sample.split(df$Purchased, SplitRatio = 0.7)
train = subset(df, sample == T)
test = subset(df,sample == F)

train[-3]=scale(train[-3])
test[-3]=scale(test[-3])

library("kernlab")
gpca <- kpca(~. ,data = train[-3], kernel = 'rbfdot', features = 2)

train_pca = as.data.frame(predict(gpca,train))
train_pca$Purchased = train$Purchased
test_pca = as.data.frame(predict(gpca,test))
test_pca$Purchased = test$Purchased

df1 <- rbind(train_pca,test_pca)
pl <- ggplot(df1,aes(y=V2,x=V1)) + geom_point(aes(color=factor(Purchased)))


model <- glm(Purchased ~. , data = train_pca , family = binomial('logit') )
pred <- predict(model , newdata= test_pca[-3], type = 'response')
xpred <- as.data.frame(pred) 
ypred <- ifelse(pred>0.5,1,0)
yypred <- as.data.frame(ypred)
head(ypred)
cm = table(test_pca[,3], ypred)
cm
