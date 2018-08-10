
df = read.csv("Economist_Assignment_Data.csv")
head(df)
library(ggplot2)

pl <- ggplot(df,aes(x=CPI,y=HDI,color=Region)) 
pl1 <- pl+ geom_point(size=5,shape=1) 
pl1

pl2 <- pl1+ geom_smooth(aes(group=1),method = lm, formula = y ~ log(x),se=FALSE,color='red')

pl2+ geom_text(aes(label=Country))
pl2

pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

pl3 <- pl2 + geom_text(aes(label = Country), color = "gray20", 
                       data = subset(df , df$Country %in% pointsToLabel),check_overlap = TRUE)

pl3 + theme_bw()
