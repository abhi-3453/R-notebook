library(ISLR)
library(caTools)
install.packages('')

df <- iris

sample = sample.split(df$Species, SplitRatio = 0.7)
train = subset(df,sample==TRUE)
test= subset(df,sample==FALSE)

model <- svm(Species~ . , data = train)

pred <- predict(model,iris)
pred
table(pred == iris$Species)

smodel <- tune(svm,train.x = train[1:4],train.y = train[,5],kernel("radial"),ranges = list(cost= c(0.5,1,1.1,1.2,1.3,1.4,1.5,2),gamma= c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))
summary(smodel)

## BEST RESULTS ##
## cost = 1.1 ##
## gamma = 0.2 ##
## best performance: 0.02818182 ##

tmodel <- svm(Species~., data= train, cost = 1.1, gamma = 0.2)
tpred <- predict(tmodel,iris)
table(tpred == iris$Species)
