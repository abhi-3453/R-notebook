df <- mtcars
head(df)

pl <- ggplot(df,aes(x=factor(cyl),y=mpg))
pl1 <- pl +geom_boxplot(aes(fill=factor(cyl)))
print(pl1)

