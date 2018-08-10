library(ISLR)
install.packages("rpart")
library(rpart)
df <- kyphosis

head(df)
tree <- rpart(Kyphosis ~ . ,method = 'class' , data = df )

printcp(tree)

install.packages("rpart.plot")
library(rpart.plot)
prp(tree)

