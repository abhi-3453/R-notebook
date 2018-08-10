
df <- read.csv("loan_data.csv")

head(df)
any(is.na(df))
summary(df)
str(df)

df$inq.last.6mths <- factor(df$inq.last.6mths)
df$delinq.2yrs <- factor(df$delinq.2yrs)
df$pub.rec <- factor(df$pub.rec)
df$not.fully.paid <- factor(df$not.fully.paid)


pl <-  ggplot(df, aes(x=fico)) + geom_histogram(aes(fill= factor(not.fully.paid)),color="black",bins = 50)
pl1 <- ggplot(df, aes(x=factor(purpose))) + geom_bar(aes(fill= factor(not.fully.paid)),color="black",position = "dodge")
pl2 <- ggplot(df, aes(x=int.rate, y=fico)) + geom_point(aes(color=factor(not.fully.paid)),alpha=0.5)

library(caTools)
set.seed(101)
sample = sample.split(df$not.fully.paid, SplitRatio = 0.7)
train = subset(df,sample== TRUE)
test = subset(df,sample == FALSE)

library(e1071)
model <- svm(not.fully.paid ~ .,data = train)
summary(model)

pred <- predict(model,test)
table(pred == test$not.fully.paid)
## Accuracy = 83.98% 

tpred <- tune(svm,train.x = not.fully.paid~.,kernel='radial', ranges = list(cost = c(1,10), gamma= c(0.1,1)),data = train)


tmodel <- svm(not.fully.paid ~ .,data = train,cost= 10,gamma=0.1)
tpreds <- predict(tmodel,test[1:13])
table(tpreds == test$not.fully.paid)

