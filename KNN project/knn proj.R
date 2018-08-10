### IMPORTING THE DATASET ###
df <- iris
species <- iris$Species

### EXPLORING THE DATASET ###
head(df)
str(df)
any(is.na(df))

### SCALING THE DATASET ###
df <- scale(df[,-5])
df <- cbind(df,iris[5])

### SPLITTING THE DATASET ###
library(caTools)
sample = sample.split(df$Species, SplitRatio = 0.7)
train = subset(df ,sample == TRUE)
test = subset(df, sample == FALSE)

### APPLYING k-NN NEAREST NEIGHBOR ###

library(class)
pspec <- knn(train[1:4],test[1:4],train$Species,k=1)
merror = mean(test$Species != pspec)

### CHOOSING THE BEST K-VALUE ###
pspec <- NULL
merror <- NULL
for (i in 1:10) {
  set.seed(101)
  pspec  <- knn(train[1:4],test[1:4],train$Species,k=i)
  merror[i] <- mean(test$Species != pspec)
  print(merror[i])
}

### VISUALIZING THE BEST K-VALUE ###
library(plotly)
sno <- seq(1:10)
error <- cbind(merror,sno)
error <- data.frame(error)
pl <- ggplot(error,aes(x=sno,y=merror)) + geom_point() + geom_line(lty="dotted",color="red")
ggplotly(pl)

## k = 8 optimized value
