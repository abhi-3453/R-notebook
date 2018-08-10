library(ggplot2)
library(ggplot2movies)
df <- movies
pl <- ggplot(movies,aes(x=rating))
pl1 <- pl+ geom_histogram(binwidth = 0.1,aes(fill=..count..))
print(pl1)
pl1+ xlab('Rating') + ylab('Count')
