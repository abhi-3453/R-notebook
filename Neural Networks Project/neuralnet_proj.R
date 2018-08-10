df <- read.csv('bank_note_data.csv')
head(df)
str(df)

library(caTools)
sample= sample.split(df$Class, SplitRatio = 0.7)
train = subset(df, sample== TRUE)
test = subset(df, sample== FALSE)

library(neuralnet)
nn <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy ,data = train, hidden = 10, linear.output = F  )

pred <- compute(nn,test[1:4])
pred$net.result <- round(pred$net.result)

table(pred$net.result,test$Class)


## RANDOM FOREST ##

rdf <- df
rdf$Class <- factor(rdf$Class)

library(caTools)
rsample= sample.split(rdf$Class, SplitRatio = 0.7)
rtrain = subset(rdf, rsample== TRUE)
rtest = subset(rdf, rsample== FALSE)

library(randomForest)
model <- randomForest(Class~ . , data = rtrain , importance = T)
rpred <- predict(model,test)

table( rpred ,test$Class)


