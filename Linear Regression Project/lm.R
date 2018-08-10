#loading the dataset
bikeshare <- read.csv("bikeshare.csv")
head(bikeshare)

#creating scatterplot
pl <- ggplot(bikeshare,aes(x=temp,y=count)) + geom_point(aes(color=temp), alpha=0.5)
ggplotly(pl)

#converting datetime
bikeshare$datetime <- as.POSIXct(bikeshare$datetime)

#Plot b/w datetime and count
pl1 <- ggplot(bikeshare,aes(x=datetime,y=count)) +geom_point(aes(color=temp), alpha=0.5) + scale_color_gradient(low = "blue",high = "orange")
ggplotly(pl1)

#correlation b/w temp and count
cor.data<- cor(bikeshare$temp,bikeshare$count)

#box plot of season vs count
pl2 <- ggplot(bikeshare,aes(x=factor(season),y=count))+ geom_boxplot(aes(color=factor(season)))
ggplotly(pl2)

#adding an hour column 
bikeshare$hour <- sapply(bikeshare$datetime,function(x){format(x,"%H")})

#scatterplot of count v/s hour, on working days
pl3 <- ggplot(filter(bikeshare,workingday==1), aes(hour,count))
pl3 <- pl3 + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
pl3 <- pl3 + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl3 + theme_bw()

#scatterplot of count v/s hour, on non working days
pl4 <- ggplot(filter(bikeshare,workingday==0), aes(hour,count))
pl4 <- pl4 + geom_point(position=position_jitter(w=0.5, h=0),aes(color=temp),alpha=0.5)
pl4 <- pl4 + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
ggplotly(pl4) 


#BUILDING THE MODEL
temp.model <- lm(count~temp,bikeshare)
summary(temp.model)

#model2
bikeshare$hour <- sapply(bikeshare$hour,as.numeric)
model <- lm(count ~ . -casual - registered -datetime -atemp,bikeshare )
summary(model)
temp.test <- data.frame(temp=c(25))
predict(model,temp.test)
