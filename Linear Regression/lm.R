df <- read.csv('student-mat.csv',sep = ";")
head(df)

num.cols <- sapply(df, is.numeric)
cor.data <- cor(df[,num.cols])

corrplot(cor.data,method = "square")
corrgram(df,order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)

set.seed(101)

sample <- sample.split(df$G3, SplitRatio = 0.7)
train = subset(df, sample == TRUE )
test = subset(df,sample == FALSE)


to_zero <- function(x){
  if(x<0){
    return(0)
  }
  else return(x)
}


#Model0
model <- lm(G3 ~ . , data = train)
print(summary(model))
res <- residuals(model)
plot(model)

pred <- predict(model,test)
result <- cbind(pred,test$G3)
result <- as.data.frame(result)
colnames(result) <- c('predicted','actual')
result$predicted <- sapply(result$predicted, to_zero)
head(result)

mse <- mean( (result$actual- result$predicted) ^ 2)
rmse <- mse^0.5
SSE <- sum( (result$actual- result$predicted) ^ 2 )
SST <- sum( (mean(df$G3)- result$actual)^2 )
r2 <- 1 - SSE/SST

#Model1
model1 <- lm(G3 ~ age+ famrel+ absences+G1+G2, data = train)
print(summary(model1))
res1 <- residuals(model1)
plot(model1)

pred1 <- predict(model1,test)
result1 <- cbind(pred1,test$G3)
result1 <- as.data.frame(result1)
colnames(result1) <- c('predicted','actual')
head(result1)
result1$predicted <- sapply(result1$predicted, to_zero)

mse1 <- mean( (result1$actual- result1$predicted) ^ 2)
rmse1 <- mse1^0.5
SSE1 <- sum( (result1$actual- result1$predicted) ^ 2 )
SST1 <- sum( (mean(df$G3)- result1$actual)^2 )
r21 <- 1 - SSE1/SST1

