library(ggplot2)
head(df)
df <- mpg

pl <- ggplot(df,aes(x=class))
pl1 <- pl + geom_bar(aes(fill=drv),position = "dodge")
print(pl1)

#percentage

pl1 <- pl + geom_bar(aes(fill=drv),position = "fill")
print(pl1)
