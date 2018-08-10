library("ISLR")

df <- College
head(df)

library(ggplot2)

pl <- ggplot(df, aes(x=Room.Board,y=Grad.Rate)) + geom_point(aes(color=factor(df$Private)))
pl1 <- ggplot(df, aes(x=F.Undergrad)) + geom_histogram(aes(fill=df$Private),color= "black", bins=50)
pl2 <- ggplot(df, aes(x=Grad.Rate)) + geom_histogram(aes(fill=df$Private),color="black",bins=50)


any(is.na(df))
g <- df$Grad.Rate
gr <- NULL

subset(df,Grad.Rate>100)
df['Cazenovia College','Grad.Rate'] <- 100

library(caTools)
set.seed(101)
sample = sample.split(df$Private, SplitRatio = 0.7)
train = subset(df, sample == TRUE)
test =  subset(df, sample== FALSE)


library(rpart)
model <- rpart(Private ~ . , data = train , method = 'class')

tree.preds <- predict(model,newdata = test)
tree.preds <- as.data.frame(tree.preds)

library(rpart.plot)
prp(model)
# Lots of ways to do this
joiner <- function(x){
  if (x>=0.5){
    return('Yes')
  }else{
    return("No")
  }
}
tree.preds$Private <- sapply(tree.preds$Yes,joiner)
table(tree.preds$Private == test$Private)

##ACCURACY = 93.13%

rfmodel <- randomForest(Private~.,data=train,importance= TRUE)
pred <- predict(rfmodel,test)
table(test$Private == pred)

## ACCURACY = 94.4% 